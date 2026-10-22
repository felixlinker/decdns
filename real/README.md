# Evaluate live DNS queries

**NOTE: This directory is mostly Agent-maintained.**

Run from the repository root:

```sh
sh real/run.sh start
sh real/run.sh status
sh real/run.sh logs
sh real/run.sh stop
python3 real/compare_outputs.py
```

`start` builds the Linux images and launches evaluation in the background.
Docker Desktop on macOS and Docker Engine on Linux are supported; Docker Compose
v2 and a Linux kernel supporting TPROXY are required. Haskell, dig, and the DNS
proxy run inside Docker. Builds may download compiler dependencies.

Every `start` stops the previous evaluation and **empties the proxy DNS cache**
before launching the worker. Existing tool and dig results are retained.
`stop` terminates the current command and its children before stopping the proxy.
It retains the containers, so `status` and `logs` also work after stopping.
Pressing Ctrl-C while following `logs` only stops following the log.

The worker shuffles `100k_mixed.txt` and visits entries in random order without
replacement. For each entry:

1. If **both** `tool/<line>.txt` and `dig/<line>.txt` exist, skip it.
2. Otherwise discard the lone old result, run the tool, then run `dig +trace`
   against the same shared proxy, and save both outputs under those filenames.
3. Continue until all entries have paired results, or the requested limit is met.

Line numbers are one-based, preserving compatibility with `compare_outputs.py`.
Stdout, stderr, GNU time resource measurements, command failures, and timeouts
are recorded in the respective output files. Result files are published only
after a command finishes. A stop between commands leaves an incomplete pair;
both commands will be rerun on the next start. A logged timeout or nonzero exit
is still a result and will not be retried when both result files exist. Delete
one member of a pair to request a rerun.

Use optional bounds for smaller evaluations:

```sh
sh real/run.sh start --limit 10 --timeout 30 --dig-timeout 30
```

`--limit` counts newly evaluated pairs, not sampled entries that already have
both outputs. Defaults: all 100k entries, 30 seconds for the tool, 30 seconds
for the complete dig trace. The tool has a further five-second process cleanup
allowance and uses two RTS workers. The evaluation container has two CPUs and
2 GiB of memory. Evaluation exits when no pending entries remain; use `stop`
to also stop the proxy.

`EVALUATION_OUTPUT=/absolute/path` selects a separate directory containing
`tool/` and `dig/`. `DNS_RECORDINGS=/absolute/path` changes the cache directory
(default `real/recordings`). These directories are created on demand. The
launcher grants the host user's group write access to the output directories;
new result files remain writable to that group. Only results and the proxy
cache are writable mounts; the source repository is mounted read-only.

## Shared DNS cache

The proxy intercepts IPv4 UDP/TCP port 53 requests from both programs, including
dig's nameserver-address bootstrap lookups. It saves complete responses in
`recordings/dns.sqlite`, including referrals, negative answers, DNSSEC records,
and TTL-zero responses. Each query key contacts the public server at most once
per evaluation session. Concurrent misses are serialized. Cache entries never
expire within a session; timeouts and network failures are also retained.

The key includes the original destination server, transport, QNAME, QTYPE,
QCLASS, header flags, and EDNS parameters, ignoring only transaction ID and
question-name case. The worker aligns dig's flags with the tool:
`+trace +norecurse +dnssec +nocookie +noadflag +bufsize=1232`. Cookies are disabled
**after** `+trace`, which would otherwise enable them again.

Different authoritative servers intentionally have separate entries. Both
programs receive the same response for matching exchanges, but they can choose
different servers and obtain different final answers. DNSSEC wall-clock time is
not frozen, so signatures can expire during long evaluation sessions. IPv6 is
disabled consistently. This setup measures functional behavior, not network
latency, TTL aging, DNS Cookies, or DNS-over-HTTPS/TLS.

```sh
sh real/run.sh stats
# Inspect individual cache hits and upstream requests:
docker compose -f real/docker/compose.yaml logs proxy
```

The proxy and evaluation worker share a Docker network namespace. The worker
runs as UID 10000 with all capabilities dropped. UID-specific routing and TPROXY
rules preserve the intended authoritative-server addresses. Only the proxy
receives `NET_ADMIN`; its own upstream requests bypass interception. Non-DNS
network traffic from the worker is blocked. No privileged containers, published
ports, or host networking are required.

The Docker support files live in `docker/`; `run_queries.py` implements the
resumable sampling loop and `compare_outputs.py` compares existing paired
answers. Regression tests for sampling, resumption, failures, and timeouts run
without Docker or public DNS:

```sh
python3 -m unittest discover -s real -p 'test_*.py'
```
