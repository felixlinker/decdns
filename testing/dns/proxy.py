"""Transparent IPv4 DNS recorder. DNS parsing and upstream I/O use dnspython."""
import argparse
import hashlib
import json
import os
import socket
import sqlite3
import struct
import threading
import time
from concurrent.futures import ThreadPoolExecutor

import dns.exception
import dns
import dns.flags
import dns.message
import dns.opcode
import dns.query
import dns.rcode

IP_TRANSPARENT = 19
IP_ORIGDSTADDR = 20


def event(action, **fields):
    print(json.dumps(dict(time=time.time(), action=action, **fields)), flush=True)


def normalized_query(wire):
    query = dns.message.from_wire(wire)
    if (len(query.question) != 1 or query.flags & dns.flags.QR
            or query.opcode() != dns.opcode.QUERY or query.had_tsig):
        raise ValueError("only unsigned, single-question DNS queries are supported")
    query.id = 0
    query.question[0].name = query.question[0].name.canonicalize()
    return query


class Recorder:
    def __init__(self, path, mode, timeout=3):
        self.mode = mode
        self.timeout = timeout
        self.lock = threading.Lock()
        self.db = sqlite3.connect(path, check_same_thread=False)
        self.db.execute("PRAGMA journal_mode=WAL")
        self.db.execute("""CREATE TABLE IF NOT EXISTS responses (
            key TEXT PRIMARY KEY, server TEXT NOT NULL, port INTEGER NOT NULL,
            transport TEXT NOT NULL, question TEXT NOT NULL, request BLOB NOT NULL,
            response BLOB, outcome TEXT NOT NULL, recorded_at REAL NOT NULL)""")
        self.db.execute("""CREATE TABLE IF NOT EXISTS metadata (
            key TEXT PRIMARY KEY, value TEXT NOT NULL)""")
        version = self.db.execute("SELECT value FROM metadata WHERE key='format'").fetchone()
        if version and version[0] != '1':
            raise ValueError("unsupported recording format")
        self.db.execute("INSERT OR IGNORE INTO metadata VALUES ('format', '1')")
        self.db.execute("INSERT OR IGNORE INTO metadata VALUES ('created_at', ?)", (str(time.time()),))
        self.db.execute("INSERT OR IGNORE INTO metadata VALUES ('dnspython', ?)", (dns.__version__,))
        self.db.commit()

    def answer(self, wire, destination, transport):
        query = normalized_query(wire)
        request = query.to_wire()
        server, port = destination
        key = hashlib.sha256(json.dumps([server, port, transport]).encode() + request).hexdigest()
        fields = dict(key=key, server=server, transport=transport,
                      question=query.question[0].to_text())
        # Serialize misses, including concurrent requests, so only one upstream
        # exchange can ever populate a key. This harness favors low public load.
        with self.lock:
            row = self.db.execute("SELECT response, outcome FROM responses WHERE key=?", (key,)).fetchone()
            if row:
                response, outcome = row
                event("hit", outcome=outcome, **fields)
            elif self.mode == 'replay':
                event("replay_miss", **fields)
                reply = dns.message.make_response(dns.message.from_wire(wire))
                reply.set_rcode(dns.rcode.SERVFAIL)
                return reply.to_wire()
            else:
                query.id = struct.unpack('!H', os.urandom(2))[0]
                event("upstream", **fields)
                try:
                    exchange = dns.query.udp if transport == 'udp' else dns.query.tcp
                    reply = exchange(query, server, port=port, timeout=self.timeout,
                                     one_rr_per_rrset=True)
                    response, outcome = reply.to_wire(max_size=65535), 'response'
                except (dns.exception.Timeout, OSError) as error:
                    response, outcome = None, 'timeout'
                    event("upstream_error", error=str(error), **fields)
                self.db.execute("INSERT INTO responses VALUES (?,?,?,?,?,?,?,?,?)",
                                (key, server, port, transport, fields['question'], request,
                                 response, outcome, time.time()))
                self.db.commit()
                event("recorded", outcome=outcome, **fields)
        if response is None:
            # Replay an upstream timeout by sending nothing, rather than
            # inventing a DNS answer or retrying against the public server.
            return None
        reply = dns.message.from_wire(response, one_rr_per_rrset=True)
        original = dns.message.from_wire(wire)
        reply.id = original.id
        reply.question = original.question
        return reply.to_wire(max_size=65535)


def transparent_socket(kind):
    sock = socket.socket(socket.AF_INET, kind)
    sock.setsockopt(socket.SOL_IP, IP_TRANSPARENT, 1)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    return sock


def receive_exact(sock, length):
    data = bytearray()
    while len(data) < length:
        chunk = sock.recv(length - len(data))
        if not chunk:
            raise EOFError()
        data.extend(chunk)
    return bytes(data)


def serve(recorder):
    udp = transparent_socket(socket.SOCK_DGRAM)
    udp.setsockopt(socket.SOL_IP, IP_ORIGDSTADDR, 1)
    udp.bind(('0.0.0.0', 5300))
    tcp = transparent_socket(socket.SOCK_STREAM)
    tcp.bind(('0.0.0.0', 5300))
    tcp.listen(32)
    pool = ThreadPoolExecutor(max_workers=16)

    def handle_udp(wire, destination, client):
        try:
            reply = recorder.answer(wire, destination, 'udp')
            if reply is not None:
                with transparent_socket(socket.SOCK_DGRAM) as sender:
                    sender.bind(destination)
                    sender.sendto(reply, client)
        except Exception as error:
            event("error", transport='udp', error=repr(error))

    def udp_loop():
        while True:
            wire, ancillary, _, client = udp.recvmsg(65535, 128)
            for level, kind, address in ancillary:
                if level == socket.SOL_IP and kind == IP_ORIGDSTADDR:
                    destination = (socket.inet_ntoa(address[4:8]), struct.unpack('!H', address[2:4])[0])
                    pool.submit(handle_udp, wire, destination, client)
                    break
            else:
                event("error", error="missing original UDP destination")

    def handle_tcp(connection):
        with connection:
            connection.settimeout(15)
            destination = connection.getsockname()
            try:
                while True:
                    length = struct.unpack('!H', receive_exact(connection, 2))[0]
                    reply = recorder.answer(receive_exact(connection, length), destination, 'tcp')
                    if reply is not None:
                        connection.sendall(struct.pack('!H', len(reply)) + reply)
            except (EOFError, socket.timeout):
                pass
            except Exception as error:
                event("error", transport='tcp', error=repr(error))

    threading.Thread(target=udp_loop, daemon=True).start()
    event("ready", mode=recorder.mode)
    while True:
        connection, _ = tcp.accept()
        pool.submit(handle_tcp, connection)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--mode', choices=['record', 'replay'], default='record')
    parser.add_argument('--database', default='/recordings/dns.sqlite')
    parser.add_argument('--timeout', type=float, default=3)
    args = parser.parse_args()
    serve(Recorder(args.database, args.mode, args.timeout))
