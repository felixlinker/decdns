# Querying fixtures

All signed fixtures originate at the DNS root (`.`), using ECDSAP256SHA256.
Signatures have fixed inception/expiration dates of 2020-01-01 and 2035-01-01.
The tests derive a trusted root DS from the fixture KSK.

- `test.zone` serves the root and contains the `*.test.` test data, signed DS
  records for `child.test.` and `external.test.`, and their delegations.
- `child.zone` and `external.zone` are signed child zones. CoreDNS serves the
  root on IPv4 loopback and both children on IPv6 loopback at the same temporary
  port. The external delegation requires a separate address lookup.
- `wire.zone` serves a smaller signed root with CNAME/target records and an
  **unsigned** `child.test.` delegation. Its signed NSEC bitmap contains NS but
  neither DS nor SOA, proving that the child is unsigned.
- `wire-complete.zone` changes the target address for the complete-response test.
- `wire-nsec3.zone` proves the same unsigned delegation with a matching NSEC3.
- `wire-optout.zone` omits the unsigned delegation from the NSEC3 chain and
  proves it using the closest encloser and an opt-out interval.

The counting wire server loads real records through the existing zone parser
and wire serializer. It adds matching RRSIGs and supplies root DNSKEYs in the
additional section with DNSKEY signatures to preserve the data-lookup cache and budget assertions.
The delegation cases assert AD=0; signed CoreDNS answers assert AD=1. Rejection
cases remove proofs/signatures or tamper with signed data.

CoreDNS's `file` plugin cannot serve NSEC3 zones, so the counting wire server
covers NSEC, NSEC3, and NSEC3 opt-out denial proofs.

To regenerate the committed `.zone` files, install BIND's `dnssec-keygen` and
`dnssec-signzone`, then run:

```sh
sh test/fixtures/querying/generate.sh
```

The script signs child zones before the root so their DS records are included.
Wire fixtures are signed without child DS records. Keys are generated in a
temporary directory and removed afterward; they are not required to run tests.
The `.unsigned` files are the editable sources. Regeneration creates fresh keys
and signatures, so output changes even when source records are unchanged.
