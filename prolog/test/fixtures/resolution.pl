% Non-module clauses, as emitted by the Haskell compiler.
resolves([test,ns], a, [test], [test,a], [a('1.2.3.4')], top(), ad(1)) :-
    nsecUnsigned([test,a], nsec([test,b], [ns])).
resolves([test,ns], a, [test], [test,b], [a('1.2.3.5')], top(), ad(1)).
resolves([test,ns], a, referral, [test,delegated], [a('1.2.3.6')], top(), ad(1)).
