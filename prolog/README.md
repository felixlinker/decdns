# DecDNS Integration for SWI-Prolog

This directory contains an integration of DecDNS constraint logic programs for SWI-Prolog.
Concretely, it provides support for all constraints of DecDNS in `dnssec.pl` and `dnssec_bridge.c`.
The C file integrates a Haskell foreign function interface that is provided in `src/Prolog/Bridge.hs` and registers itself as a foreign SWI-Prolog module.

## Prerequisites

To use this extension, you need to install [SWI-Prolog](https://www.swi-prolog.org/), `make`, a C compiler, and `stack`.
We provide instructions for installing `stack` in the repository's

To compile the integration, simply run `make`.
The build is intended for Unix systems with a shared SWI-Prolog installation.

## Usage

To use the integration, first compile a zone configuration using DecDNS and the `pl-resolvable` command.
Then, load SWI-Prolog with the `resolvable.pl` module and the output from DecDNS:

```sh
% swipl resolvable.pl output.pl
```
