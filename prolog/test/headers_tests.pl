:- ensure_loaded('../resolvable').
:- ensure_loaded('fixtures/resolution').

:- begin_tests(headers).

test(auxiliary_rules_use_generated_predicate) :-
    user:all_resolvable(Queries),
    sort(Queries, [resolvable([test,ns], a, [test,a], top(), ad(1)),
                   resolvable([test,ns], a, [test,b], top(), ad(1))]).
test(generated_clauses_can_call_constraints, [nondet]) :-
    user:resolvable([test,ns], a, [test,a], top(), ad(1)).
test(referrals_are_excluded, [fail]) :-
    user:resolvable([test,ns], a, [test,delegated], _, _).

:- end_tests(headers).
