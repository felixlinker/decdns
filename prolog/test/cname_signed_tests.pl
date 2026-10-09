:- module(cname_signed_tests, []).
:- use_module(resolvable_tests, [read_terms/2]).
:- include('../resolvable').
:- include('../../pl/cname-signed').

:- begin_tests(cname_signed).

test(print_all_resolvable) :-
    with_output_to(string(Output), cname_signed_tests:print_all_resolvable),
    setup_call_cleanup(open_string(Output, Stream),
                       read_terms(Stream, Clauses), close(Stream)),
    % Authenticated resolution through the CNAME requires validating the
    % child zone's DNSKEY against its parent's DS record.
    memberchk(resolvable([com,ns], a, [com,a], top(), ad(1)), Clauses).

:- end_tests(cname_signed).
