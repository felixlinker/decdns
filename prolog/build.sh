#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
command -v swipl >/dev/null
# Values reported by the installed SWI executable, including its library paths.
eval "$(swipl --dump-runtime-variables)"
mkdir -p prolog/build
stack --stack-root "$PWD/.stack-root" build dns-regressor:lib
stack --stack-root "$PWD/.stack-root" exec -- ghc --make \
    -dynamic -fPIC -no-link -Wall -package dns-regressor -package text \
    -odir prolog/build -hidir prolog/build -stubdir prolog/build \
    prolog/haskell/Bridge.hs
stack --stack-root "$PWD/.stack-root" exec -- ghc \
    -dynamic -shared -fPIC -flink-rts -no-hs-main -threaded \
    -package dns-regressor -package text \
    -I"$PLBASE/include" -Iprolog/build -L"$PLLIBDIR" -lswipl \
    -optl-Wl,-rpath,"$PLLIBDIR" \
    prolog/build/Bridge.o prolog/dnssec_bridge.c \
    -odir prolog/build -o "prolog/build/dnssec_native.$PLSOEXT"
