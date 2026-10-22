FROM haskell:9.8.4-slim AS builder
WORKDIR /build
COPY stack.yaml stack.yaml.lock package.yaml dns-regressor.cabal ./
COPY src ./src
COPY app ./app
COPY test ./test
RUN stack --system-ghc --no-install-ghc build --copy-bins --local-bin-path /out --jobs 2

FROM debian:bookworm-slim AS harness
RUN apt-get update && apt-get install -y --no-install-recommends \
    python3 python3-dnspython bind9-dnsutils iproute2 iptables ca-certificates libgmp10 libnuma1 time \
    && rm -rf /var/lib/apt/lists/*
COPY real/docker /opt/dns-test
WORKDIR /opt/dns-test
ENTRYPOINT ["/bin/sh", "/opt/dns-test/entrypoint.sh"]

FROM harness AS tool
COPY --from=builder /out/dns-regressor-prolog /usr/local/bin/dns-regressor-prolog
