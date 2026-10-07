:- use_module(dnssec).
all_resolvable(Queries) :- findall(resolvable(S, Q, N, V, A), resolvable(S, Q, N, V, A), Queries).
resolvable(S, Q, N, V, A) :-
  dif(Q, ns), dif(Q, cname), dif(Q, dname),
  dif(Q, ds), dif(Q, dnskey), dif(Q, nsec), dif(Q, nsec3), dif(Q, rrsig),
  resolves(S, Q, Z, N, _, V, A), dif(Z, referral).
