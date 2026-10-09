:- module(resolvable_tests, [read_terms/2]).
:- include('../resolvable').
:- dynamic resolves/7.

read_terms(Stream, Terms) :-
    read_term(Stream, Term, []),
    ( Term == end_of_file -> Terms = []
    ; Terms = [Term|Rest], read_terms(Stream, Rest)
    ).

:- begin_tests(resolvable_printing).

printed_terms(Terms) :-
    with_output_to(string(Output), resolvable_tests:print_all_resolvable),
    setup_call_cleanup(open_string(Output, Stream),
                       read_terms(Stream, Terms), close(Stream)).

test(no_answers) :- printed_terms([]).

test(facts, [setup(assertz(resolvable_tests:resolves(s, a, z, n, [], v, ad(1)), Ref)),
             cleanup(erase(Ref))]) :-
    printed_terms([resolvable(s, a, n, v, ad(1))]).

test(residual_constraints,
     [setup(assertz((resolvable_tests:resolves(s, a, z, N, [], V, ad(1)) :-
                         dif(N, excluded), dif(V, invalid)), Ref)),
      cleanup(erase(Ref))]) :-
    printed_terms([(resolvable(s, a, Name, Value, ad(1)) :- Body)]),
    call(Body),
    \+ Name = excluded,
    \+ Value = invalid,
    Name = allowed,
    Value = valid.

test(chr_constraint,
     [setup(assertz((resolvable_tests:resolves(s, a, z, N, [], v, ad(1)) :-
                         dnssec:isName(N)), Ref)),
      cleanup(erase(Ref))]) :-
    printed_terms([(resolvable(s, a, Name, v, ad(1)) :- Body)]),
    assertion(Body == dnssec:isName(Name)).

test(mixed_constraints_share_variables,
     [setup(assertz((resolvable_tests:resolves(s, a, z, N, [], v, ad(1)) :-
                         dnssec:isName(N), dif(N, [excluded]),
                         dnssec:nsecUnsigned(N, nsec([next], _Types))), Ref)),
      cleanup(erase(Ref))]) :-
    printed_terms([(resolvable(s, a, Name, v, ad(1)) :- Body)]),
    resolvable_tests:comma_list(Body, Goals),
    assertion(length(Goals, 3)),
    memberchk(dnssec:isName(IsName), Goals), assertion(IsName == Name),
    memberchk(dnssec:nsecUnsigned(Owner, nsec([next], Bitmap)), Goals),
    assertion(Owner == Name),
    call(Body),
    \+ Name = [excluded],
    Name = [allowed], Bitmap = [ns].

test(body_only_variables,
     [setup(assertz((resolvable_tests:resolves(s, a, z, n, [], v, ad(1)) :-
                         dnssec:isName(Hidden),
                         dnssec:nsecUnsigned(Hidden, nsec([next], Types)),
                         dif(Types, [])), Ref)),
      cleanup(erase(Ref))]) :-
    printed_terms([(resolvable(s, a, n, v, ad(1)) :- Body)]),
    resolvable_tests:comma_list(Body, Goals),
    assertion(length(Goals, 3)),
    memberchk(dnssec:isName(Name), Goals),
    memberchk(dnssec:nsecUnsigned(Owner, nsec([next], Bitmap)), Goals),
    assertion(Owner == Name),
    call(Body),
    \+ Bitmap = [],
    Name = [allowed], Bitmap = [ns].

test(chr_answers_are_isolated,
     [setup(assertz((resolvable_tests:resolves(s, a, z, N, [], V, ad(1)) :-
                         (V = conditional, dnssec:isName(N) ; V = unconditional)), Ref)),
      cleanup(erase(Ref))]) :-
    printed_terms([(resolvable(s, a, Name, conditional, ad(1)) :- Body),
                   resolvable(s, a, _, unconditional, ad(1))]),
    assertion(Body == dnssec:isName(Name)),
    \+ resolvable_tests:current_chr_constraint(dnssec:isName(_)).

test(all_resolvable_returns_plain_clauses,
     [setup(assertz((resolvable_tests:resolves(s, a, z, N, [], v, ad(1)) :-
                         dnssec:isName(N)), Ref)),
      cleanup(erase(Ref))]) :-
    resolvable_tests:all_resolvable([Clause]),
    term_attvars(Clause, []),
    Clause = (resolvable(s, a, Name, v, ad(1)) :- Body),
    assertion(Body == dnssec:isName(Name)).

test(multiple_answers,
     [setup(assertz((resolvable_tests:resolves(s, a, z, N, [], v, ad(1)) :-
                         member(N, [first, second])), Ref)),
      cleanup(erase(Ref))]) :-
    printed_terms([resolvable(s, a, first, v, ad(1)),
                   resolvable(s, a, second, v, ad(1))]).

test(shared_variables,
     [setup(assertz((resolvable_tests:resolves(s, a, z, N, [], N, ad(1)) :-
                         dif(N, excluded)), Ref)),
      cleanup(erase(Ref))]) :-
    printed_terms([(resolvable(s, a, Name, Value, ad(1)) :- Body)]),
    assertion(Name == Value),
    call(Body),
    \+ Name = excluded,
    Name = allowed.

test(quoted_terms,
     [setup(assertz(resolvable_tests:resolves('server name', a, z,
                                             ['quoted.name'], [], "a string", ad(1)), Ref)),
      cleanup(erase(Ref))]) :-
    printed_terms([resolvable('server name', a, ['quoted.name'], "a string", ad(1))]).

:- end_tests(resolvable_printing).
