rm ./dsset-*
dnssec-signzone -g -S -K ./keys/ -o com. -f ./out/ns.com. ./ns.com.
