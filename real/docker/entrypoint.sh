#!/bin/sh
set -eu
case "$1" in
  proxy)
    if [ "${DNS_RESET_CACHE:-0}" = 1 ]; then
      rm -f /recordings/dns.sqlite /recordings/dns.sqlite-wal /recordings/dns.sqlite-shm
    fi
    ip rule add fwmark 1 lookup 100
    ip route add local 0.0.0.0/0 dev lo table 100
    for protocol in udp tcp; do
      iptables -t mangle -A OUTPUT -m owner --uid-owner 10000 -p "$protocol" --dport 53 -j MARK --set-mark 1
      iptables -t mangle -A PREROUTING -m mark --mark 1 -p "$protocol" --dport 53 \
        -j TPROXY --on-port 5300 --tproxy-mark 1/1
      iptables -A OUTPUT -m owner --uid-owner 10000 -p "$protocol" --dport 53 -j ACCEPT
    done
    iptables -A OUTPUT -m owner --uid-owner 10000 -j REJECT
    iptables -P FORWARD DROP
    if [ "${DNS_MODE:-record}" = replay ]; then
      iptables -A OUTPUT -p udp --dport 53 -j REJECT
      iptables -A OUTPUT -p tcp --dport 53 -j REJECT
    fi
    exec python3 -u proxy.py --mode "${DNS_MODE:-record}" --database /recordings/dns.sqlite
    ;;
  client)
    shift
    exec "$@"
    ;;
  *) exec "$@" ;;
esac
