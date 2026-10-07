:- ensure_loaded('../headers').
:- ensure_loaded('fixtures/resolution').

:- begin_tests(headers).

test(auxiliary_rules_use_generated_predicate) :-
    user:all_resolvable(a, [test,ns], Names),
    sort(Names, [[test,a],[test,b]]).
test(generated_clauses_can_call_constraints) :-
    user:resolvable([test,ns], a, [test,a], top(), ad(1)).
test(referrals_are_excluded, [fail]) :-
    user:resolvable([test,ns], a, [test,delegated], _, _).
test(generated_predicate_is_tabled) :-
    predicate_property(user:resolves(_,_,_,_,_,_,_), tabled).

:- end_tests(headers).
