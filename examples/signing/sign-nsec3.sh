rm ./dsset-*
dnssec-signzone -3 deadbeef -H 1 -g -S -K ./keys/ -o b.com. -f ./out/ns.b.com. ./ns.b.com.
dnssec-signzone -3 deadbeef -H 1 -g -S -K ./keys/ -o com. -f ./out/ns.com. ./ns.com.
