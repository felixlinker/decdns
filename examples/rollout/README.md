# Rollout configurations

Each subdirectory is a complete, unsigned configuration. Files are named after
servers, and may contain multiple zones. These examples preserve witnesses from
the rollout audits without requiring the ignored `reports/` directory.

`exampleDirectories` in `test/Resolvable/Update/RolloutSpec.hs` explicitly lists
all configurations used for rollout tests. It includes these directories and the
eligible existing CNAME and NS examples. Add new rollout inputs to that list.
`examples/signing` is reserved for configuring zones and is not included.
The tests cover every ordered pair, including identity updates, with names
`examples/<old> -> examples/<new>`.

DNSSEC and known cyclic configurations are omitted from the list. Listed inputs
are validated: missing zone files, parsing errors, DNSSEC records, and cyclic
rule or projected dependency orders cause failures rather than silent exclusions.

| Configuration | What it exercises |
|---|---|
| `delayed-target-old`, `delayed-target-new` | An existing A answer switches to an alias before its target is ready. |
| `delegated-wildcard`, `wildcard` | Removing a delegation must preserve wildcard answers throughout its subtree. |
| `direct-address`, `delegated-address`, `alias` | Transitions between direct answers, delegations, and aliases. |
| `address-to-alias-old`, `address-to-alias-new` | Coordinated changes between A and CNAME records. |
| `wildcard-alias-txt` | A TXT descendant changes wildcard applicability while an alias is introduced. |
| `wildcard-explicit`, `wildcard-txt` | Adding explicit owners changes the coverage of an unchanged wildcard. |
| `wildcard-nested` | Nested wildcard coverage. |
| `wildcard-cname`, `wildcard-cname-descendant` | Wildcard aliases and changes in name existence. |
| `dname` | Transitions between delegation and subtree redirection. |
| `colocated-child` | Several zones served by one server; compare with `cname/base`. |

The properties sample concrete queries from the symbolic intersection of the
initial and requested final resolvable query sets, including wildcard names.
They check those queries after every emitted step, using both `ns.com.` and
`root.net.` as starting servers. Endpoint computations are shared across pairs.
A pair with no common resolvable queries satisfies the invariant vacuously and
is labelled accordingly in the test output.

These are query-continuity properties. They do not assert unchanged answer data,
final inventory equality, intermediate zone validity, or arbitrary-cache safety.
Consequently, examples exhibiting only those other problems may pass. Known
query outages are deliberately left as ordinary failing properties.

Run from the repository root:

```sh
stack --stack-root "$PWD/.stack-root" test --test-arguments='-p /Resolvable.Update.Rollout/'
```
