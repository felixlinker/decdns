#!/bin/sh
# Regenerate committed fixtures with BIND tools; tests do not need signing tools.
set -eu
cd "$(dirname "$0")"
fixtures=$PWD
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cp ./*.unsigned "$work/"
cd "$work"
cp wire.unsigned wire-nsec3.unsigned
cp wire.unsigned wire-optout.unsigned
for zone in . child.test. external.test.; do
  dnssec-keygen -q -a ECDSAP256SHA256 -f KSK "$zone" >/dev/null
  dnssec-keygen -q -a ECDSAP256SHA256 "$zone" >/dev/null
done
sign() {
  origin=$1
  file=$2
  shift 2
  dnssec-signzone -q -n 1 -S -g -s 20200101000000 -e 20350101000000 \
    -o "$origin" -f "$fixtures/$file.zone" "$@" "$file.unsigned"
}
sign child.test. child
sign external.test. external
sign . test
rm dsset-child.test. dsset-external.test.
sign . wire
sign . wire-complete
sign . wire-nsec3 -3 deadbeef -H 0
sign . wire-optout -3 deadbeef -H 0 -A
