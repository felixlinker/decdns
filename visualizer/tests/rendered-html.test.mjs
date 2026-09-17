import assert from "node:assert/strict";
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { bodyToHeadEdge, layoutGraphLayers, moveRuleSelection, predicateFromOtherRule, projectOwnerServerGraph, ruleEdgePorts, ruleNodeSourceLines, ruleSelectionForSource, sourceHasCycle } from "../app/graph.ts";
import { readVisualizationCatalog, readVisualizationFile } from "../build/visualization-directory.ts";

async function render() {
  const workerUrl = new URL("../dist/server/index.js", import.meta.url);
  workerUrl.searchParams.set("test", `${process.pid}-${Date.now()}`);
  const { default: worker } = await import(workerUrl.href);
  return worker.fetch(
    new Request("http://localhost/", { headers: { accept: "text/html" } }),
    { ASSETS: { fetch: async () => new Response("Not found", { status: 404 }) } },
    { waitUntil() {}, passThroughOnException() {} },
  );
}

async function fetchBuilt(path, environment = {}) {
  const workerUrl = new URL("../dist/server/index.js", import.meta.url);
  workerUrl.searchParams.set("test", `${process.pid}-${Date.now()}-${Math.random()}`);
  const { default: worker } = await import(workerUrl.href);
  return worker.fetch(
    new Request(`http://localhost${path}`),
    { ASSETS: { fetch: async () => new Response("Not found", { status: 404 }) }, ...environment },
    { waitUntil() {}, passThroughOnException() {} },
  );
}

test("server-renders the compilation explorer shell", async () => {
  const response = await render();
  assert.equal(response.status, 200);
  assert.match(response.headers.get("content-type") ?? "", /^text\/html\b/i);
  const html = await response.text();
  assert.match(html, /<title>DNS Regressor Compilation Explorer<\/title>/i);
  assert.match(html, /Compilation explorer/);
  assert.match(html, />RRSets</);
  assert.doesNotMatch(html, />All nodes</);
  assert.doesNotMatch(html, /Record sets|Compiled rules|Dependency edges|Unifier bindings|Dependency cycles/);
  assert.doesNotMatch(html, /retained by the compiler|Round lower port|matching-head ingress/i);
  assert.match(html, /aria-label="Zoom in"/);
  assert.match(html, /aria-label="Project graph by owner name and name server"/);
  assert.match(html, /aria-label="Hide RRSets panel"/);
  assert.match(html, /aria-label="Hide inspector panel"/);
  assert.match(html, /Drag canvas to pan.*Scroll to zoom/);
  assert.doesNotMatch(html, /Red outlines mark rules in a dependency cycle/);
  assert.match(html, /Visualization file/);
  assert.match(html, />Reload</);
  assert.doesNotMatch(html, /Cycle example|Auth example|Open export/);
  assert.doesNotMatch(html, /codex-preview|react-loading-skeleton/i);
});

test("dependency arrows run from the body rule to the matching head rule", () => {
  const bodyRule = { id: "body-rule" };
  const headRule = { id: "head-rule" };
  const placements = new Map([[bodyRule.id, bodyRule], [headRule.id, headRule]]);
  const edge = bodyToHeadEdge({ dependentRuleId: bodyRule.id, dependencyRuleId: headRule.id }, placements);

  assert.deepEqual(edge, { from: bodyRule, to: headRule });
});

test("dependency edges use separate ingress and egress ports", () => {
  const ports = ruleEdgePorts(100);

  assert.ok(Math.abs(ports.ingress - 28) < Number.EPSILON * 32);
  assert.ok(Math.abs(ports.egress - 72) < Number.EPSILON * 32);
  assert.ok(ports.ingress < ports.egress);
});

test("owner and name-server projection merges nodes and their dependencies", () => {
  const sources = [
    { id: "a-a", owner: "a.example.", server: "ns1.example." },
    { id: "a-aaaa", owner: "a.example.", server: "ns1.example." },
    { id: "a-other-server", owner: "a.example.", server: "ns2.example." },
    { id: "b-a", owner: "b.example.", server: "ns1.example." },
  ];
  const rules = [
    { id: "rule-1", sourceId: "a-a" },
    { id: "rule-2", sourceId: "a-aaaa" },
    { id: "rule-3", sourceId: "a-other-server" },
    { id: "rule-4", sourceId: "b-a" },
  ];
  const matches = [
    { dependentRuleId: "rule-1", dependencyRuleId: "rule-4", predicate: "first" },
    { dependentRuleId: "rule-2", dependencyRuleId: "rule-4", predicate: "second" },
    { dependentRuleId: "rule-3", dependencyRuleId: "rule-1", predicate: "third" },
  ];

  const projected = projectOwnerServerGraph(sources, rules, matches);

  assert.deepEqual(projected.nodes.map(({ owner, server, ruleIds }) => ({ owner, server, ruleIds })), [
    { owner: "a.example.", server: "ns1.example.", ruleIds: ["rule-1", "rule-2"] },
    { owner: "a.example.", server: "ns2.example.", ruleIds: ["rule-3"] },
    { owner: "b.example.", server: "ns1.example.", ruleIds: ["rule-4"] },
  ]);
  assert.equal(projected.edges.length, 2);
  assert.deepEqual(projected.edges.map((edge) => edge.matches.map((match) => match.predicate)), [
    ["first", "second"],
    ["third"],
  ]);
});

test("configured directories expose valid visualization exports", async () => {
  const directory = await mkdtemp(join(tmpdir(), "dns-regressor-visualizations-"));
  const validDataset = { version: 2, rootServers: [], zoneFiles: [], sources: [], rules: [], matches: [] };
  try {
    await writeFile(join(directory, "valid.json"), JSON.stringify(validDataset));
    await writeFile(join(directory, "invalid.json"), JSON.stringify({ version: 1 }));
    await writeFile(join(directory, "notes.txt"), "not an export");

    const catalog = await readVisualizationCatalog(directory);
    assert.deepEqual(catalog.files.map((file) => file.name), ["valid.json"]);
    assert.deepEqual(catalog.errors.map((error) => error.name), ["invalid.json"]);
    assert.deepEqual(await readVisualizationFile(directory, "valid.json"), validDataset);
    await assert.rejects(() => readVisualizationFile(directory, "../valid.json"), /invalid visualization filename/);
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
});

test("production build exposes the visualization directory API", async () => {
  const directory = await mkdtemp(join(tmpdir(), "dns-regressor-visualizations-api-"));
  const previousDirectory = process.env.DNS_REGRESSOR_VISUALIZATION_DIR;
  const dataset = { version: 2, rootServers: [], zoneFiles: [], sources: [], rules: [], matches: [] };
  try {
    await writeFile(join(directory, "example.json"), JSON.stringify(dataset));
    process.env.DNS_REGRESSOR_VISUALIZATION_DIR = directory;

    const catalogResponse = await fetchBuilt("/api/visualizations");
    assert.equal(catalogResponse.status, 200);
    assert.deepEqual((await catalogResponse.json()).files.map((file) => file.name), ["example.json"]);

    const datasetResponse = await fetchBuilt("/api/visualizations?file=example.json");
    assert.equal(datasetResponse.status, 200);
    assert.deepEqual(await datasetResponse.json(), dataset);
  } finally {
    if (previousDirectory === undefined) delete process.env.DNS_REGRESSOR_VISUALIZATION_DIR;
    else process.env.DNS_REGRESSOR_VISUALIZATION_DIR = previousDirectory;
    await rm(directory, { recursive: true, force: true });
  }
});

test("an RRSet identifies whether any compiled rule is cyclic", () => {
  const source = { ruleIds: ["acyclic", "cyclic"] };

  assert.equal(sourceHasCycle(source, new Set(["cyclic"])), true);
  assert.equal(sourceHasCycle(source, new Set(["other"])), false);
});

test("selecting an RRSet selects every rule compiled from it", () => {
  assert.deepEqual(ruleSelectionForSource({ ruleIds: ["rule-1", "rule-3"] }, "rule-2"), new Set(["rule-1", "rule-3"]));
  assert.deepEqual(ruleSelectionForSource(null, "rule-2"), new Set(["rule-2"]));
});

test("RRSet rule navigation follows export order and wraps at its ends", () => {
  const ruleIds = ["rule-3", "rule-1", "rule-2"];

  assert.equal(moveRuleSelection(ruleIds, "rule-3", -1), "rule-2");
  assert.equal(moveRuleSelection(ruleIds, "rule-3", 1), "rule-1");
  assert.equal(moveRuleSelection(ruleIds, "rule-1", 1), "rule-2");
  assert.equal(moveRuleSelection(ruleIds, "rule-2", 1), "rule-3");
});

test("rule nodes identify the name server serving their source record", () => {
  assert.deepEqual(
    ruleNodeSourceLines({ owner: "example.", server: "ns1.example.", recordType: "A" }),
    { record: "example. A", server: "served by ns1.example." },
  );
  assert.deepEqual(ruleNodeSourceLines(null), { record: "synthetic", server: "" });
});

test("dependency cards show the unchanged predicate from the other rule", () => {
  const match = {
    dependentRuleId: "body-rule",
    dependencyRuleId: "head-rule",
    predicate: "resolves(BodyVariable)",
  };
  const rules = new Map([
    ["body-rule", { head: "dependent(Head)" }],
    ["head-rule", { head: "resolves(ProviderVariable)" }],
  ]);

  assert.equal(predicateFromOtherRule(match, "body-rule", rules), "resolves(ProviderVariable)");
  assert.equal(predicateFromOtherRule(match, "head-rule", rules), "resolves(BodyVariable)");
});

test("all-nodes layout follows DAG layers and preserves graph order within them", () => {
  const edges = [
    { from: "root", to: "middle-b" },
    { from: "root", to: "middle-a" },
    { from: "middle-a", to: "leaf-a" },
    { from: "other-root", to: "short-leaf" },
    { from: "middle-b", to: "leaf-b" },
  ];
  assert.deepEqual(
    layoutGraphLayers(
      ["middle-a", "root", "leaf-a", "middle-b", "leaf-b", "other-root", "short-leaf"],
      edges,
      { top: 24, nodeHeight: 108, minimumGap: 66 },
    ),
    [
      { id: "middle-a", layer: 1, y: 24 },
      { id: "root", layer: 0, y: 24 },
      { id: "leaf-a", layer: 2, y: 24 },
      { id: "middle-b", layer: 1, y: 198 },
      { id: "leaf-b", layer: 2, y: 198 },
      { id: "other-root", layer: 0, y: 198 },
      { id: "short-leaf", layer: 2, y: 372 },
    ],
  );
});

test("all-nodes layout handles cycles as one layered component", () => {
  assert.deepEqual(
    layoutGraphLayers(
      ["cycle-b", "root", "cycle-a", "leaf"],
      [
        { from: "root", to: "cycle-a" },
        { from: "cycle-a", to: "cycle-b" },
        { from: "cycle-b", to: "cycle-a" },
        { from: "cycle-b", to: "leaf" },
      ],
      { top: 10, nodeHeight: 100, minimumGap: 20 },
    ),
    [
      { id: "cycle-b", layer: 1, y: 10 },
      { id: "root", layer: 0, y: 10 },
      { id: "cycle-a", layer: 1, y: 130 },
      { id: "leaf", layer: 2, y: 10 },
    ],
  );
});
