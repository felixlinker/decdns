# DNSSEC constraints for SWI-Prolog

This library runs the existing `DNS.SEC.Verification` Haskell functions from
CHR constraints. It accepts the names and record terms emitted by
`decdns pl-resolvable`.

Build from `dns-regressor/` with `make -C prolog`; this requires Make, Stack,
SWI-Prolog (including its headers and shared library), and a C compiler.
Stack uses the repository-local `.stack-root` and builds `Prolog.Bridge` as
part of the main library. The native adapter is built in `prolog/build/`
using Stack's generated bridge header. The shared object links the Haskell
runtime and package libraries, so keep the local build dependencies available when loading it.
The build is intended for Unix systems with a shared SWI-Prolog installation.

Remove native build products with `make -C prolog clean`.
Stack handles Haskell dependency tracking; Make recompiles the C adapter when
its source or generated header changes and relinks when the Haskell shared
library changes. An unchanged build only performs Stack and dependency checks.

Run the Haskell tests with
`stack --stack-root "$PWD/.stack-root" test` from `dns-regressor/`.

```prolog
:- use_module('/absolute/path/to/dns-regressor/prolog/dnssec').

% Names are lists of atoms, ordered from the root towards the leaf.
% [] denotes the root; [com, a] denotes a.com.
% The second argument is DNSKEY; the third is DS.
digestValid([com, a], dnskey(257, 13, KeyBase64), ds(Tag, 13, 2, DigestHex)).
```

Load the non-module header before the compiler output:

```sh
swipl prolog/resolvable.pl output.pl
```

`resolvable.pl` imports the DNSSEC constraints and provides
`all_resolvable/1`, `print_all_resolvable/0`, and `resolvable/3` in the same `user` module as the
generated clauses. For example, with `pl/cname-signed.pl` loaded:

```prolog
?- all_resolvable(Queries).
?- print_all_resolvable.
```

`print_all_resolvable/0` prints each resolvable query as a Prolog clause,
with its residual constraints as the body. Answers without residual constraints
are printed as facts. `all_resolvable/1` returns these same plain clause terms:
`resolvable(...)` for unconditional answers and `(resolvable(...) :- Body)`
for constrained answers. The body includes both standard residual goals such as
`dif/2` and module-qualified CHR constraints, with variable sharing preserved,
including variables that occur only in the body.

The compiler does not yet insert imports automatically. If you load generated
clauses into an explicit module instead, that module must also import the
constraints and define its auxiliary rules.

Exported constraints:

| Constraint | Existing implementation |
| --- | --- |
| `isName(Name)` | Delayed logic-programming name-length check (maximum 255) |
| `digestValid(Name, DNSKEY, DS)` | `verifyDigest` |
| `rrsigValid(Name, RRSet, RRSIG, DNSKEY)` | `verifySignature` |
| `nsecUnsigned(Name, NSEC)` | `nsecUnsignedSubzone Name Name` |
| `nsec3Unsigned(Name, HashedOwner, NSEC3)` | `nsec3UnsignedSubzone` |
| `closestEnclosers(Name, ExistingOwner, ExistingNSEC3, MissingOwner, MissingNSEC3)` | `verifyClosestEncloserProof` |

DNSSEC verification waits until **all arguments, including nested fields, are
ground**. `isName/1` can also reject a partial name whose minimum wire length
already exceeds 255 bytes. Valid checks disappear from the constraint store; invalid checks
fail the current Prolog branch. Nonground residual constraints are conditional
answers, not verified proofs. Failure is appropriate for candidate proofs
under backtracking; a shared bottom-up store needs a separate design for
rejecting individual derivations.

## Representation and bridge

DNSKEY public keys and RRSIG signatures are Base64 strings. DS digests and
NSEC3 salt/next-hash fields are hex strings. Hex case is accepted either way;
empty NSEC3 salt is `""`. The next-hash field is decoded hex, not the Base32hex
text used for the hashed owner's label. Record terms use the compiler's
constructors, including `dnskey/3`, `rrsig/9`, `nsec/2`, and `nsec3/6`.

`dnssec.pl` defines the CHR rules and loads the shared library.
`dnssec_bridge.c` registers the private `dnssec:check/1` predicate and exposes a small term
reading API. `../src/Prolog/Bridge.hs` copies ground SWI terms into the
existing Haskell term representation, reuses the RDATA parser and binary decoders,
and calls the existing verifier. It never retains SWI term handles across
calls. Text is copied as UTF-8 and binary data is decoded only in Haskell.

Malformed input raises `error(dnssec_input_error(Message), Context)`;
invalid proofs simply fail. The private native predicate also rejects
nonground or cyclic inputs. Ground trees deeper than 1024 are rejected.
Unsupported DS digest algorithms raise an exception. Negative NSEC3 iteration
counts are rejected during record parsing.

Verification retains the current Haskell model: RRSIG verification supports
ECDSA P-256/SHA-256 and does not check wall-clock inception/expiration. It
is not a new general DNSSEC validator. GHC's runtime is initialized when the
library is loaded and shut down when it is unloaded; unload only after all
calls have finished. The adapter assumes it owns this runtime initialization.
