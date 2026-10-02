:- module(dnssec, [digestValid/3, rrsigValid/4, nsecUnsigned/2,
                   nsec3Unsigned/3, closestEnclosers/5]).
:- use_module(library(chr)).
:- use_module(library(shlib)).
:- prolog_load_context(directory, Directory),
   directory_file_path(Directory, 'build/dnssec_native', Library),
   use_foreign_library(Library).

:- table resolves/7.
all_resolvable(Q, RS, Names) :- bagof(Name, resolvable(RS, Q, Name, _, _), Names).
resolvable(S, Q, N, V, A) :- resolves(S, Q, Z, N, _, V, A), dif(Z, referral).

:- chr_constraint digestValid/3, rrsigValid/4, nsecUnsigned/2,
                  nsec3Unsigned/3, closestEnclosers/5.

digestValid(Name, Key, DS) <=> ground([Name, Key, DS]) |
    check(0, [Name, Key, DS]).
rrsigValid(Name, Data, Sig, Key) <=> ground([Name, Data, Sig, Key]) |
    check(1, [Name, Data, Sig, Key]).
nsecUnsigned(Name, NSEC) <=> ground([Name, NSEC]) |
    check(2, [Name, NSEC]).
nsec3Unsigned(Name, HashedOwner, NSEC3) <=> ground([Name, HashedOwner, NSEC3]) |
    check(3, [Name, HashedOwner, NSEC3]).
closestEnclosers(Name, ExistingName, ExistingData, MissingName, MissingData) <=>
    ground([Name, ExistingName, ExistingData, MissingName, MissingData]) |
    check(4, [Name, ExistingName, ExistingData, MissingName, MissingData]).
