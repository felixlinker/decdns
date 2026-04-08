# DNS Regressor

This repository contains a Haskell implementation of a tool that can reason about resolvability of DNS zones during updates.
By "resolvability," we mean: Is there an answer to a DNS query?
We consider resolvability with respect to a list of servers to start the queries at.

The tool provides many commands, but its core utility is to compute two things, given two sets of zone files (one being an old configuration, one being the updated configuration):

1. Are there any queries that were resolvable in the old configuration but are not resolvable in the new configuration?
We intend to apply some lightweight reasoning about whether regressions in resolvability are "trivial" (thus, most likely intended).
For example, if all records that could directly answer a query were deleted, we would like to consider that deletion "trivial" and thus probably intended.
2. Can the updates be rolled out in a way so that resolvability of queries is not degraded during the rollout, e.g., because resolvers some cached data that conflicts with the update?
For example, say that a domain `a.com` aliases to `b.com`, but `b.com` is decommissioned and its addresses moved to `a.com`.
In that case, we should first remove the CNAME record for `a.com` and add its new IP addresses before deleting `b.com`.
A resolver having cached `a.com`'s CNAME record may otherwise fail looking up `b.com`.
The tool can also report that no such roll out is possible, e.g., because the update introduces a cyclic dependency.

Our tool compiles DNS zone files into logic programs that can be serialized as Prolog programs.
Generally, our output will use SWI-Prolog syntax.
We do not use SWI-Prolog reasoning, however, as SWI-Prolog's reasoning about negation does not match our needs.

## Available commands

The command line utilities provided by this repository all require zone files as arguments.
Zone files must be provided as paths to files (or globs), and the respective file name indicates the name server's name serving the contents of that file.

### `dns-regressor-interactive`

Description: This tool runs an interactive, non-deterministic resolver and can enumerate every way by which a query is resolvable.

Instructions:
- Execute by running `stack run dns-regressor-interactive -- [zone files...]`
- When in interactive mode, you can use these commands:
  - `:slist [names...]`: Set the list of name server that resolution can start from.
  - `:? [qname] [qtype]`: Show every way by which querying for `QNAME` and `QTYPE` could be resolved.
  - `:print`: Print all parsed zone file contents.
  - `:q`: Exit the interactive mode.

### `dns-regressor-prolog`

Description: This tool reasons statically about DNS zone files, as described above.
It takes a command and a list of arguments:

```sh
stack run dns-regressor-prolog -- [command] [args]
```

Available commands are:

- `pl-resolvable [names...] -- [zone files...]`:
  Compile a list of zone files to a Prolog program that can reason about resolvability and print it to stdout.
  Resolution will start at the name servers provided as first list of arguments.
- `pl-regressions [names...] -- [old zone files...] -- [new zone files...]`:
  Compile a list of old and new zone files to a Prolog program that can reason about regressions in resolvability during an update.
  Arguments are as with the previous command, but the list of zone files reflect the old and new configuration respectively.
- `resolvable [names...] -- [zone files...]`:
  Like `pl-resolvable`, but reason statically and symbolically.
  Out is printed to stdout as Prolog facts.
- `regressions [names...] -- [old zone files...] -- [new zone files...]`:
  Like `pl-regressions`, but reason statically and symbolically.
  Out is printed to stdout as Prolog facts.
  Also prints the order in which the updates can be rolled out.
- `diff [old zone files...] -- [new zone files...]`:
  Print the diff of the zone file configurations.
- `dependencies [names...] -- [zone files...]`:
  Arguments like for `pl-resolvable`.
  Computes resolution of which name at a specific name server can lead to resolution of a different name at a different name server.
- `parse-only [zone files...]`:
  Parses the list of zone files and prints all records to stdout.
  This command is supposed to aid debugging.
