:- module(dnssec, [digestValid/3, rrsigValid/4, nsecUnsigned/2,
                   nsec3Unsigned/3, closestEnclosers/5, isName/1]).
:- use_module(library(chr)).
:- use_module(library(shlib)).
:- prolog_load_context(directory, Directory),
   directory_file_path(Directory, 'build/dnssec_native', Library),
   use_foreign_library(Library).

:- chr_constraint digestValid/3, rrsigValid/4, nsecUnsigned/2,
                  nsec3Unsigned/3, closestEnclosers/5, isName/1.

digestValid(Name, Key, DS) <=> ground([Name, Key, DS]) |
    check(digestValid(Name, Key, DS)).
rrsigValid(Name, Data, Sig, Key) <=> ground([Name, Data, Sig, Key]) |
    check(rrsigValid(Name, Data, Sig, Key)).
nsecUnsigned(Name, NSEC) <=> ground([Name, NSEC]) |
    check(nsecUnsigned(Name, NSEC)).
nsec3Unsigned(Name, HashedOwner, NSEC3) <=> ground([Name, HashedOwner, NSEC3]) |
    check(nsec3Unsigned(Name, HashedOwner, NSEC3)).
closestEnclosers(Name, ExistingName, ExistingData, MissingName, MissingData) <=>
    ground([Name, ExistingName, ExistingData, MissingName, MissingData]) |
    check(closestEnclosers(Name, ExistingName, ExistingData, MissingName, MissingData)).

isName(Name) <=> min_name_length(Name, Length), Length > 255 | fail.
isName(Name) <=> ground(Name) | min_name_length(Name, Length), Length =< 255.

min_name_length(X, 1) :- var(X), !.
min_name_length([], 1).
min_name_length([Label|Labels], Length) :-
    min_dns_label_length(Label, LabelLength),
    min_name_length(Labels, RecLength),
    Length is LabelLength + RecLength.

min_dns_label_length(X, 2) :- var(X), !.
min_dns_label_length(Label, 1 + Length) :- atom(Label), atom_length(Label, Length), Length =< 63.
