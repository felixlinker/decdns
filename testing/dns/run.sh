#!/bin/sh
# Invoke with `sh testing/dns/run.sh ...` from any working directory.
set -eu
cd "$(dirname "$0")"
compose() { docker compose -f compose.yaml "$@"; }
command=${1:-help}
[ "$#" -eq 0 ] || shift
case "$command" in
  record|replay)
    compose --profile '*' down
    export DNS_MODE=$command
    if [ "$command" = record ]; then
      compose up -d --build --wait proxy client
    else
      compose up -d --no-build --pull never --wait proxy client
    fi
    ;;
  dig)
    compose exec -T client dig -4 "$@" +norecurse +dnssec +nocookie +noadflag +bufsize=1232
    ;;
  build-tool) compose --profile tool build tool ;;
  query)
    compose --profile tool run --rm --no-deps tool client \
      timeout "${DNS_QUERY_TIMEOUT:-65}" dns-regressor-prolog --timeout "${DNS_QUERY_LIMIT_SECONDS:-60}" \
      query "$@" +RTS -N2 -RTS
    ;;
  logs) compose logs "$@" proxy ;;
  stats)
    compose exec -T proxy python3 -c 'import sqlite3; c=sqlite3.connect("/recordings/dns.sqlite"); print("transport outcome entries"); print("\n".join(str(row) for row in c.execute("SELECT transport,outcome,count(*) FROM responses GROUP BY transport,outcome")))'
    ;;
  down) compose --profile '*' down ;;
  *)
    echo 'Usage: sh testing/dns/run.sh record|replay|dig ARGS...|build-tool|query ROOTS KEYS TYPE NAME|logs|stats|down'
    ;;
esac
