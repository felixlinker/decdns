# DecDNS

This repository contains a Haskell implementation of DecDNS, a tool that can reason about resolvability of DNS zones.
By "resolvability," we mean: Is there an answer to a DNS query?

The tool provides many commands, but its core utility is to compute a change-impact analysis of a zone configuration update.
That is, it can compute (a) whether an update leads to a loss in resolvability, i.e., whether there are queries that cannot be resolved anymore after the update, and (b) whether an update reads to a gain in resolvability, i.e., whether there now resolvable queries that were not resolvable before.

DecDNS compiles DNS zone files into constraint logic programs.
Generally, our output will use SWI-Prolog syntax.
We do not use SWI-Prolog for our reasoning, however.

## Installation and Pre-requisites

To compile DecDNS, you require `stack`.
We recommend installing `stack` via [GHCup](https://www.haskell.org/ghcup/).

After having installed `stack`, you can run `stack install` to compile DecDNS.

We also provide a SWI-Prolog integration.
We refer to the documentation in `prolog/README.md` for configuring this integration.

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
  including nameservers, answer data, alias following, verification, and authentication status.
- `regressions ROOT_KEYS_OLD ROOT_KEYS_NEW ROOT_ZONE_NAME [old zone files...] :: [new zone files...]`:
  Compile a list of old and new zone files to a Prolog program and
  compute regressions in resolvability. Arguments are as with the previous
  command, but the list of zone files reflect the old and new configuration
  respectively. Output is printed to stdout as Prolog facts.
- `parse-only [zone files...]`:
  Parses the list of zone files and prints all records to stdout.
  This command is supposed to aid debugging.
