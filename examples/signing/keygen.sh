mkdir -p keys
rm keys/*
dnssec-keygen -a ECDSAP256SHA256 -f ZSK -K keys com.
dnssec-keygen -a ECDSAP256SHA256 -f KSK -K keys com.
dnssec-keygen -a ECDSAP256SHA256 -f ZSK -K keys b.com.
dnssec-keygen -a ECDSAP256SHA256 -f KSK -K keys b.com.
dnssec-keygen -a ECDSAP256SHA256 -f ZSK -K keys example.
dnssec-keygen -a ECDSAP256SHA256 -f KSK -K keys example.
