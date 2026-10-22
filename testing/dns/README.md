# Shared DNS recording and replay

This Docker environment intercepts direct DNS queries from both `dig +trace`
and `dns-regressor`. It preserves authoritative-server destinations and saves
complete DNS responses in SQLite. Repeated matching queries receive the first
recorded response, including referrals, negative answers, DNSSEC data, and
TTL-zero answers. Recordings survive container restarts and are never expired
or refreshed automatically.

Run these commands from the repository root. Docker Desktop on macOS works;
the host does not need Linux routing tools, Python DNS packages, or Haskell.
Docker Compose v2 with `up --wait` and a Linux kernel supporting TPROXY is
required.

## Record queries

```sh
# Start the persistent recorder, then trace a public name.
sh testing/dns/run.sh record
sh testing/dns/run.sh dig @198.41.0.4 iana.org. A +trace
sh testing/dns/run.sh stats
sh testing/dns/run.sh logs --tail=30
```

The `dig` wrapper runs `dig -4` and appends `+norecurse`, `+dnssec`,
`+nocookie`, `+noadflag`, and `+bufsize=1232` after the user arguments.
These match the current Haskell client's relevant wire settings. Cookies must
be disabled **after** `+trace`, which otherwise enables them again. Arbitrary
DNS flags can be tested directly:

```sh
docker compose -f testing/dns/compose.yaml exec -T client dig ...
```

## Then include querying mode

```sh
# Separate opt-in build: GHC 9.8.4, the repository's Stack snapshot and sources.
sh testing/dns/run.sh build-tool

# Uses the SAME proxy/database as dig. Defaults to 60 seconds and two RTS workers.
sh testing/dns/run.sh query root-addresses root-keys A iana.org.

# Rebuild the namespace in offline mode, retaining its database.
sh testing/dns/run.sh replay
sh testing/dns/run.sh query root-addresses root-keys A iana.org.
sh testing/dns/run.sh logs --tail=30

# Stop everything, keeping recordings.
sh testing/dns/run.sh down
```

`DNS_QUERY_LIMIT_SECONDS` changes the tool's CLI timeout (default 60).
`DNS_QUERY_TIMEOUT` changes the outer process timeout (default 65 seconds).
The tool container is limited to two CPUs and 2 GiB of memory. Builds occur
outside the intercepted runtime network and may download Haskell dependencies.
The `replay` command reuses existing images without building or pulling them.
No Haskell source changes are needed.

Paths passed to `query` are relative to the read-only repository mounted at
`/work`. Preserve the root hints and trust anchors used for each evaluation
alongside its recordings. `DNS_RECORDINGS=/absolute/path` selects a different
database directory; set it consistently for record, replay, and other commands.
The default is `testing/dns/recordings/dns.sqlite`. Use a new directory for a
fresh capture rather than deleting or editing a database while it is running.

## What is held constant

The key contains the original destination IPv4 address and port, UDP versus
TCP, and the DNS query normalized only for transaction ID and question-name
case. QTYPE, QCLASS, header flags, EDNS version, DO, advertised buffer size,
and EDNS option contents remain significant. Source ports do not affect the
key. All upstream requests use the proxy's network vantage point. Concurrent
misses are serialized, so retries cannot duplicate an upstream exchange.

Responses keep their original TTLs and sections; the reply transaction ID and
question case are adapted to the current client. DNSSEC signatures are retained.
The database also records normalized requests, capture timestamps, outcomes,
format version, and dnspython version. Timeouts and network failures are frozen
as silence; they do not trigger later public retries. JSON container logs mark
`upstream`, `recorded`, `hit`, `replay_miss`, and errors.

In replay mode a missing key produces SERVFAIL and a `replay_miss` log entry.
**Treat any replay miss as an incomplete dataset, not as a resolver failure.**
The proxy never forwards a miss; firewall rules also reject its outbound UDP
and TCP port 53 traffic. Clients cannot send non-DNS network traffic.

Both clients must populate the dataset: the tool fetches DNSKEYs and other
records that dig may not need. Dig also randomly selects nameservers, so a
later trace can encounter an unrecorded server. Record the additional path
before evaluating it, or issue directed dig queries to the recorded server.
Logs include the server and cache key, making shared answers verifiable.

Different authoritative servers intentionally have separate entries. Merging
them would hide server-dependent answers and could confuse parent referrals
with child answers. This setup guarantees repeatability for matching exchanges,
not identical final answers when clients follow different paths. Compare
canonical RRsets rather than raw dig output or record order.

## Network implementation and limits

The proxy, dig client, and optional tool share a Docker network namespace.
Clients run as UID 10000 with all capabilities dropped. UID-specific OUTPUT
rules mark their DNS traffic; policy routing delivers it through loopback to
TPROXY in PREROUTING. TPROXY leaves the original destination intact. The Python
proxy uses transparent sockets, dnspython for DNS parsing and upstream I/O,
and SQLite for persistence. Its own upstream sockets are unmarked, avoiding
interception loops. Only the proxy receives `NET_ADMIN`; no host networking,
published ports, or privileged container is used for normal operation.

A fixed resolver file ensures dig's system-resolver bootstrap lookups also
pass through the proxy instead of Docker's embedded DNS.

This harness currently supports IPv4 DNS over UDP/TCP on destination port 53.
IPv6 is disabled consistently. It is for functional comparisons, not timing,
retry-latency, IPv6, DNS Cookies, or DNS-over-HTTPS/TLS measurements. It accepts
unsigned, single-question QUERY messages; TSIG is unsupported. Transport errors
are replayed as no answer rather than reproducing their exact ICMP/TCP behavior.
The recorder serializes upstream exchanges to limit public load.

TTL expiration is deliberately suspended within a fixture dataset. **Wall-clock
time is not frozen:** DNSSEC signatures can expire, and responses captured over
a long recording session are not an atomic snapshot of all public zones. Use
short capture sessions and refresh datasets when necessary. Recordings belong
only in this isolated test environment, not in a production caching resolver.
