# DNS Regressor compilation explorer

This browser app connects DNS record sets to their compiled rules and shows the unifications used by their dependencies.

An arrow runs from the rule containing the body predicate to the rule whose head can satisfy it. Its round starting port is lower on the source node; its arrowhead enters through an upper port on the target node. Hover an edge to see the exact predicate and unifier, or select a rule to emphasize all of its connected edges. Dependency cycles are outlined in red.

Drag the canvas to pan and use the mouse wheel or the toolbar to zoom. **Fit** frames the entire focused graph, while **Reset** returns to the default readable scale.

Within each column, nodes are reordered and vertically offset to reduce the total span of their dependency edges while preserving readable separation.

## Run the app

The web tool requires Node.js 22.13 or newer; `.nvmrc` selects Node 24 when using `nvm`.

From this directory:

```sh
DNS_REGRESSOR_VISUALIZATION_DIR=/tmp/dns-regressor-visualizations npm run dev
```

Open the displayed local URL. The development server scans the configured directory for version 2 JSON exports. You can also put the setting in an ignored `.env.local` file; `.env.example` shows the format.

## Export visualizations

Create the configured directory, then export compiler data into it from the repository root:

```sh
mkdir -p /tmp/dns-regressor-visualizations

stack --stack-root "$PWD/.stack-root" run -- \
  visualize ns.com. -- \
  path/to/ns.com. path/to/ns.child.com. \
  > /tmp/dns-regressor-visualizations/ns-com.json
```

The app initially opens the first valid JSON file in filename order. Choose another export from the header. **Reload** makes the server scan the directory again, preserving the selected filename if it still exists. Invalid JSON files are skipped and reported in the UI.

Because `visualize` writes to standard output, exports can also be piped directly into other tools.

## Validate

```sh
npm test
```
