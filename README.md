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

- `query ROOT_ADDRESSES ROOT_KEYS TYPE NAME`:
  Resolve a fully qualified name against real DNS, starting with the servers in the root addresses file.
  For example, `stack run -- query root-addresses root-keys NS com.` or `stack run -- query root-addresses root-keys A amazon.com.`.
  The root addresses file contains root NS records and their A and AAAA records in zone-file format.
  The root keys file contains root (`.`) DS records in zone-file format.
  These records are considered trusted and used to authenticate the root DNSKEY RRSet.
  Prints the first complete answer and logs network lookups as `%` Prolog comments.
- `pl-resolvable ROOT_KEYS ROOT_ZONE_NAME [zone files...]`:
  Compile a list of zone files to a Prolog program that can reason about resolvability and print it to stdout.
  Resolution starts at the apex of the zone `ROOT_ZONE_NAME`, as with `resolvable`.
- `resolvable ROOT_KEYS ROOT_ZONE_NAME [zone files...]`: Reason statically
and symbolically about all supplied zones. Resolution starts at the apex of the
  zone `ROOT_ZONE_NAME`, which must be supplied as one of the input zone files.
  `ROOT_ZONE_NAME` must be fully qualified, for example `com.` or `.`. The root
  keys file contains trusted DS records in zone-file format; it may be empty.
  Output is printed to stdout as facts.
- `resolves ROOT_KEYS ROOT_ZONE_NAME [zone files...]`:
  Accepts the same arguments as `resolvable`, and prints the full resolution facts,
  including nameservers, answer data, verification, and authentication status.
- `regressions ROOT_KEYS_OLD ROOT_KEYS_NEW ROOT_ZONE_NAME [old zone files...] :: [new zone files...]`:
  Compile a list of old and new zone files to a Prolog program and
  compute regressions in resolvability. Arguments are as with the previous
  command, but the list of zone files reflect the old and new configuration
  respectively. Output is printed to stdout as Prolog facts.
- `parse-only [zone files...]`:
  Parses the list of zone files and prints all records to stdout.
  This command is supposed to aid debugging.
