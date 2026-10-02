#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
./prolog/build.sh
swipl -q -s prolog/test/dnssec_tests.pl -g run_tests -t halt
