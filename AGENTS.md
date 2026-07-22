- This repository contains a Haskell implementation of a tool that can reason about resolvability of DNS zones during updates.
The tools purpose and CLI usage is documented in the `README.md`.
- Aim to reuse as much existing code as possible. You may export previously unexported functions from a module to reuse them.
- Always check whether you can reuse existing code before implementing new features.
- When you find yourself reimplementing functionality or logic that has already been implemented, directly reuse or, when not immediately possible, generalize existing code.
- Re-run tests whenever possible.
If you implement new features, also implement corresponding test cases.
You can re-run all tests by executing `stack --stack-root "$PWD/.stack-root" test`.
- Run all `stack` commands with `--stack-root "$PWD/.stack-root"` and the default restricted/sandboxed permissions so that Stack uses the repository-local cache instead of requesting access to the cache outside the repository (for example, `~/.stack`).
Only request broader permissions if the command actually fails because it needs a resource such as a missing dependency from the network.
- If you find that a test case fails, always fix that.
You may fix a failing test case both by changing the implementation or the test case.
Always think about which option is the correct one and include your reasoning in your final report.
- Favor simple code. Simplify your logic whenever possible.
- The repository contains the following packages:
  - `src/DNS`: Type definitions and auxiliary functions for handling DNS zones.
  - `src/Parsing`: Parsing.
  - `src/Resolvable`: Compiling zones to prolog program and reasoning about resolvability.
  - `pkg/Util`: General, auxiliary code.
