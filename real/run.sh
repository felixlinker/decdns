#!/bin/sh
# Run from any directory: sh real/run.sh start|stop|status|logs|stats|compare
set -eu
cd "$(dirname "$0")"
compose() { docker compose -f docker/compose.yaml "$@"; }
command=${1:-help}
[ "$#" -eq 0 ] || shift
export EVALUATION_GID="$(id -g)"
case "$command" in
  start)
    while [ "$#" -gt 0 ]; do
      case "$1" in
        --limit|--timeout|--dig-timeout)
          option=$1
          [ "$#" -ge 2 ] || { echo "Missing value for $option" >&2; exit 1; }
          case "$2" in ''|*[!0-9]*) echo "Invalid value for $option: $2" >&2; exit 1 ;; esac
          case "$option" in
            --limit) export EVALUATION_LIMIT=$2 ;;
            --timeout) [ "$2" -gt 0 ]; export EVALUATION_TIMEOUT=$2 ;;
            --dig-timeout) [ "$2" -gt 0 ]; export EVALUATION_DIG_TIMEOUT=$2 ;;
          esac
          shift 2
          ;;
        *) echo "Unknown start option: $1" >&2; exit 1 ;;
      esac
    done
    output_dir=${EVALUATION_OUTPUT:-.}
    # Resolve once so Compose paths and host-side creation agree.
    mkdir -p "$output_dir/tool" "$output_dir/dig"
    export EVALUATION_OUTPUT="$(cd "$output_dir" && pwd)"
    chmod g+rwx "$EVALUATION_OUTPUT/tool" "$EVALUATION_OUTPUT/dig"
    compose down
    compose build
    compose up -d --wait proxy
    compose up -d --no-build evaluation
    echo 'Evaluation started. Use sh real/run.sh logs to follow progress.'
    ;;
  stop)
    compose stop --timeout 10 evaluation
    compose stop --timeout 10 proxy
    ;;
  status) compose ps --all ;;
  logs) compose logs -f evaluation ;;
  stats)
    compose exec -T proxy python3 -c 'import sqlite3; c=sqlite3.connect("/recordings/dns.sqlite"); print("transport outcome entries"); print("\n".join(str(row) for row in c.execute("SELECT transport,outcome,count(*) FROM responses GROUP BY transport,outcome")))'
    ;;
  compare) exec python3 compare_outputs.py ;;
  *)
    echo 'Usage: sh real/run.sh start [--limit N] [--timeout SECONDS] [--dig-timeout SECONDS]'
    echo '       sh real/run.sh stop|status|logs|stats|compare'
    ;;
esac
