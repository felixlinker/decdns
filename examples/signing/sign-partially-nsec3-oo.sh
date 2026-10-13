rm ./dsset-*
dnssec-signzone -3 deadbeef -H 1 -A -g -S -K ./keys/ -o com. -f ./out/ns.com. ./ns.com.
