rm ./dsset-*
dnssec-signzone -g -S -K ./keys/ -o b.com. -f ./out/ns.b.com. ./ns.b.com.
dnssec-signzone -g -S -K ./keys/ -o com. -f ./out/ns.com. ./ns.com.
