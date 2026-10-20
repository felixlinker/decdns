:- use_module(dnssec).
all_resolvable(Queries) :- findall(resolvable(Q, N, V, A), resolvable(Q, N, V, A), Queries).
resolvable(Q, N, V, A) :-
  starting_zone(Z),
  dif(Q, soa), dif(Q, ns), dif(Q, cname), dif(Q, dname),
  dif(Q, ds), dif(Q, dnskey), dif(Q, nsec), dif(Q, nsec3), dif(Q, nsec3param), dif(Q, rrsig),
  resolves(_, Q, Z, N, _, V, A).
