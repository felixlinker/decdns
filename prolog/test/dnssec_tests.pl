:- use_module('../dnssec').
:- use_module(library(chr)).

:- begin_tests(dnssec).

key(dnskey(257, 13, "2iBXCSBo2Dvk4eEPxxdfogVY5J0Woop4tpS6RD27va9od/QOpb5dbBKlI4gMNljr47o9rRruikkUpyEFnJ1DiA==")).
ds(ds(59726, 13, 2, "BAFD7C691EB978754C2CA32C5697AC4B21C8DC6534DA5EFDFBE9A7B17882501B")).
signing_key(dnskey(256, 13, "ZMpXruovycFOC7+ebNH5sG+Gir52Skfa7RkkaEYMi662o3XCMTdQE3NiPdADM4MPKx/FJxjpscuPPeA+cxpTOA==")).
signature(rrsig(soa, 13, 1, 1200, 1783775562, 1781183562, 23099, [com],
 "Zudwp50+lG2UoUgg+dQwrEdbYBPu/6JvmO9dJE58XE8seqNAQvWl4JL0d7il/YOQsrV4lAyNfHlZvzTZ7rDvZg==")).
rrset([soa([com,ns1], [com,example], 0, 0, 0, 0, 0)]).

% DS and signature vectors are shared with DNS.SEC.VerificationSpec.
test(digest_valid) :- key(K), ds(D), digestValid([com,a], K, D).
test(digest_invalid, [fail]) :- key(K), ds(D), digestValid([com,b], K, D).
test(digest_delays_and_wakes) :-
    key(K), ds(D), digestValid([com,Label], K, D),
    var(Label), once(find_chr_constraint(digestValid([com,Label], K, D))),
    Label = a,
    \+ find_chr_constraint(digestValid(_, _, _)).
test(digest_delayed_invalid, [fail]) :-
    key(K), ds(D), digestValid([com,Label], K, D), Label = b.
test(digest_nested_key_variable) :-
    key(dnskey(F,A,B)), ds(D), digestValid([com,a], dnskey(F,A,Bytes), D),
    var(Bytes), Bytes = B.
test(signature_valid) :- signing_key(K), signature(S), rrset(R), rrsigValid([com], R, S, K).
test(signature_changed_rrset, [fail]) :-
    signing_key(K), signature(S), rrsigValid([com], [soa([com,ns1],[com,example],1,0,0,0,0)], S, K).
test(signature_delayed) :-
    signing_key(K), signature(S), rrsigValid([com], R, S, K), var(R), rrset(R).
test(nsec_unsigned) :- nsecUnsigned([com,a], nsec([com,b], [ns,rrsig,nsec])).
test(nsec_signed, [fail]) :- nsecUnsigned([com,a], nsec([com,b], [ns,ds])).
test(nsec_root) :- nsecUnsigned([], nsec([], [ns])).
test(nsec_delayed_bitmap) :- nsecUnsigned([com,a], nsec([com,b], Types)), var(Types), Types = [ns].

% RFC 5155 appendix A: hash(example.) for salt aabbccdd and 12 iterations.
nsec3(Flags, Next, Types, nsec3(1, Flags, 12, "AABBCCDD", Next, Types)).
zero_hash("0000000000000000000000000000000000000000").
max_hash("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF").
example_hash([example,'0p9mhaveqvm6t7vbl5lop2u3t2rp3tom']).
zero_owner([example,'00000000000000000000000000000000']).

test(nsec3_unsigned) :-
    zero_hash(Z), nsec3(0,Z,[ns],N), example_hash(H), nsec3Unsigned([example], H, N).
test(nsec3_wrong_hash, [fail]) :-
    zero_hash(Z), nsec3(0,Z,[ns],N), zero_owner(H), nsec3Unsigned([example], H, N).
test(nsec3_empty_salt) :-
    % A malformed base32 owner is a failed proof, not a decoding exception.
    \+ nsec3Unsigned([example], [example,invalid], nsec3(1,0,0,"","0000000000000000000000000000000000000000",[ns])).
test(closest_encloser) :-
    zero_hash(Z), max_hash(M), nsec3(1,Z,[],E), nsec3(0,M,[],N),
    example_hash(EH), zero_owner(NH), closestEnclosers([example,a], EH, E, NH, N).
test(closest_encloser_not_optout, [fail]) :-
    zero_hash(Z), max_hash(M), nsec3(0,Z,[],E), nsec3(0,M,[],N),
    example_hash(EH), zero_owner(NH), closestEnclosers([example,a], EH, E, NH, N).
test(malformed_hex, [throws(error(dnssec_input_error(_),_))]) :-
    key(K), digestValid([com,a], K, ds(59726,13,2,"xyz")).
test(unsupported_digest, [throws(error(dnssec_input_error(_),_))]) :-
    key(K), digestValid([com,a], K, ds(59726,13,3,"00")).
test(malformed_record, [throws(error(dnssec_input_error(_),_))]) :-
    nsecUnsigned([com], nsec([com], not_a_list)).
test(negative_iterations, [throws(error(dnssec_input_error(_),_))]) :-
    nsec3Unsigned([example], [example,invalid], nsec3(1,0,-1,"","0000000000000000000000000000000000000000",[ns])).
test(native_requires_ground, [throws(error(instantiation_error,_))]) :-
    dnssec:check(nsecUnsigned([com], nsec([com], _))).
test(cyclic_input, [throws(error(type_error(acyclic_term,_),_))]) :-
    X = [com|X], dnssec:check(nsecUnsigned(X, nsec([com],[ns]))).
test(repeated_calls) :-
    key(K), ds(D), forall(between(1, 100, _), digestValid([com,a], K, D)).

test(is_name) :- isName([com,example]).
test(is_name_root) :- isName([]).
test(is_name_invalid_ground, [fail]) :- isName([42]).
test(is_name_at_limit) :- length(Name, 127), maplist(=(a), Name), isName(Name).
test(is_name_oversized, [fail]) :- isName(Names), length(Names, 128), maplist(=(a), Names).
test(is_name_rejects_non_ground_lower_bound, [fail]) :-
    length(Names, 128), \+ ground(Names), isName(Names).
test(is_name_partial_lower_bound) :- dnssec:min_name_length([com,_Label], 7).
test(is_name_delays_and_wakes) :-
    isName([com,Label]), var(Label),
    once(find_chr_constraint(isName([com,Label]))),
    Label = example,
    \+ find_chr_constraint(isName(_)).
test(is_name_delayed_oversized, [fail]) :-
    isName(Names), length(Names, 128), maplist(=(a), Names).

test(backtracking_rejects_only_invalid_candidate) :-
    key(K), ds(D), findall(Label, (member(Label, [b,a]), digestValid([com,Label], K, D)), [a]).
test(nsec3_delayed) :-
    zero_hash(Z), nsec3(0,Z,[ns],N), nsec3Unsigned([example], H, N), var(H), example_hash(H).
test(closest_encloser_delayed) :-
    zero_hash(Z), max_hash(M), nsec3(1,Z,[],E), nsec3(0,M,[],N), example_hash(EH),
    closestEnclosers([example,a], EH, E, NH, N), var(NH), zero_owner(NH).
test(improper_list, [throws(error(dnssec_input_error(_),_))]) :-
    nsecUnsigned([com], nsec([com], [ns|invalid])).
test(out_of_range_integer, [throws(error(dnssec_input_error(_),_))]) :-
    nsecUnsigned([com], nsec([com], [999999999999999999999999999999])).
test(float_field, [throws(error(dnssec_input_error(_),_))]) :-
    nsec3Unsigned([example], [example,invalid], nsec3(1,0,1.5,"","0000000000000000000000000000000000000000",[ns])).
test(embedded_nul, [throws(error(dnssec_input_error(_),_))]) :-
    string_codes(Hex, [48,0,48]), key(K), digestValid([com,a], K, ds(59726,13,2,Hex)).
test(wrong_record_type, [throws(error(dnssec_input_error(_),_))]) :-
    key(K), digestValid([com,a], K, a('1.2.3.4')).
test(wrong_operation, [throws(error(dnssec_input_error(_),_))]) :-
    dnssec:check(unknown_predicate).

:- end_tests(dnssec).
