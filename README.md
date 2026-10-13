# DNS Regressor

This repository contains a Haskell implementation of a tool that can reason about resolvability of DNS zones during updates.
By "resolvability," we mean: Is there an answer to a DNS query?
We consider resolvability with respect to a list of servers to start the queries at.

The tool provides many commands, but its core utility is to compute two things, given two sets of zone files (one being an old configuration, one being the updated configuration):

1. Are there any queries that were resolvable in the old configuration but are not resolvable in the new configuration?
We intend to apply some lightweight reasoning about whether regressions in resolvability are "trivial" (thus, most likely intended).
For example, if all records that could directly answer a query were deleted, we would like to consider that deletion "trivial" and thus probably intended.
2. Can the updates be rolled out in a way so that resolvability of queries is not degraded during the rollout, e.g., because resolvers have cached data that conflicts with the update?
For example, say that a domain `a.com` aliases to `b.com`, but `b.com` is decommissioned and its addresses moved to `a.com`.
In that case, we should first remove the CNAME record for `a.com` and add its new IP addresses before deleting `b.com`.
A resolver having cached `a.com`'s CNAME record may otherwise fail looking up `b.com`.
The rollout computation can also determine that no such rollout is possible, e.g., because the update introduces a cyclic dependency.
The command-line interface currently does not print a diagnostic in that case.

Our tool compiles DNS zone files into logic programs that can be serialized as Prolog programs.
Generally, our output will use SWI-Prolog syntax.
We do not use SWI-Prolog reasoning, however, as SWI-Prolog's reasoning about negation does not match our needs.

## Available commands

The tool reasons statically about DNS zone files, as described above, and it requires zone files as arguments.
Zone files must be provided as paths to files (or globs), and the respective file name indicates the name server's name serving the contents of that file.
The arguments are as follows:

```sh
stack run -- [command] [args]
```

Available commands are:

- `query ROOT_KEYS TYPE NAME`:
  Resolve a fully qualified name against real DNS, starting at `a.root-servers.net.`.
  For example, `stack run -- query root-keys NS com.` or `stack run -- query root-keys A amazon.com.`.
  The root keys file contains root (`.`) DS records in zone-file format.
  These records are trusted and used to authenticate the root DNSKEY RRSet.
  Prints the first complete answer and logs network lookups as `%` Prolog comments.
  A per-command budget of 10 DNS lookups limits network traffic.
- `pl-resolvable [names...] -- [zone files...]`:
  Compile a list of zone files to a Prolog program that can reason about resolvability and print it to stdout.
  Resolution will start at the name servers provided as first list of arguments.
- `pl-regressions [names...] -- [old zone files...] -- [new zone files...]`:
  Compile a list of old and new zone files to a Prolog program that can reason about regressions in resolvability during an update.
  Arguments are as with the previous command, but the list of zone files reflect the old and new configuration respectively.
- `resolvable [names...] -- [zone files...]`:
  Like `pl-resolvable`, but reason statically and symbolically.
  Output is printed to stdout as Prolog facts.
- `regressions [names...] -- [old zone files...] -- [new zone files...]`:
  Like `pl-regressions`, but reason statically and symbolically.
  Output is printed to stdout as Prolog facts.
  If a non-empty rollout is found, also prints the order in which the updates can be rolled out.
- `diff [old zone files...] -- [new zone files...]`:
  Print the diff of the zone file configurations.
- `dependencies [names...] -- [zone files...]`:
  Arguments like for `pl-resolvable`.
  Computes resolution of which name at a specific name server can lead to resolution of a different name at a different name server.
- `visualize [names...] -- [zone files...]`:
  Export the record-to-rule compilation and dependencies as JSON for the browser-based compilation explorer in `visualizer/`.
  Each dependency includes the matching body predicate and its unifier. The JSON is written to standard output.
- `parse-only [zone files...]`:
  Parses the list of zone files and prints all records to stdout.
  This command is supposed to aid debugging.

## Structure

Note that the contents of `visualizer` are fully vibe coded, and are only intended for debugging purposes.
