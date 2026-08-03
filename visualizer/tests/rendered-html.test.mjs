import assert from "node:assert/strict";
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { bodyToHeadEdge, groupSourcesByName, minimizeArrowLayout, ruleEdgePorts, visualColumnOrder } from "../app/graph.ts";
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

test("server-renders the compilation explorer shell", async () => {
  const response = await render();
  assert.equal(response.status, 200);
  assert.match(response.headers.get("content-type") ?? "", /^text\/html\b/i);
  const html = await response.text();
  assert.match(html, /<title>DNS Regressor Compilation Explorer<\/title>/i);
  assert.match(html, /Compilation explorer/);
  assert.match(html, />Names</);
  assert.match(html, /Record sets/);
  assert.doesNotMatch(html, /retained by the compiler|Round lower port|matching-head ingress/i);
  assert.match(html, /aria-label="Zoom in"/);
  assert.match(html, /Drag canvas to pan.*Scroll to zoom/);
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

test("left-column sources are grouped by name, not record type", () => {
  const groups = groupSourcesByName([
    { owner: "example.", server: "ns1.", recordType: "A", records: ["A record"], ruleIds: ["r1"] },
    { owner: "example.", server: "ns1.", recordType: "NS", records: ["NS record"], ruleIds: ["r2"] },
    { owner: "other.", server: "ns2.", recordType: "A", records: ["other A"], ruleIds: ["r3"] },
  ]);

  assert.equal(groups.length, 2);
  assert.deepEqual(groups[0].recordTypes, ["A", "NS"]);
  assert.deepEqual(groups[0].ruleIds, ["r1", "r2"]);
});

test("column layout reorders and offsets nodes to minimize vertical edge span", () => {
  const ports = ruleEdgePorts(108);
  const edges = [
    { from: "body-1", to: "head-1" },
    { from: "body-2", to: "head-2" },
    { from: "body-3", to: "head-3" },
    { from: "consumer", to: "body-2" },
  ];
  const placements = minimizeArrowLayout(
    {
      dependency: ["head-1", "head-2", "head-3"],
      compiled: ["body-3", "body-1", "body-2"],
      dependent: ["consumer"],
    },
    edges,
    { top: 68, nodeHeight: 108, minimumGap: 66, ingressPortY: ports.ingress, egressPortY: ports.egress },
  );
  const byId = new Map(placements.map((placement) => [placement.id, placement]));
  const verticalSpan = edges.reduce((total, edge) => (
    total + Math.abs(byId.get(edge.from).y + ports.egress - byId.get(edge.to).y - ports.ingress)
  ), 0);

  assert.ok(verticalSpan < 0.01);
  assert.ok(byId.get("consumer").y > 68, "the shorter right column should not remain top-aligned");
  for (const column of ["dependency", "compiled", "dependent"]) {
    const ys = placements.filter((placement) => placement.column === column).map((placement) => placement.y).sort((a, b) => a - b);
    assert.ok(ys.every((value, index) => index === 0 || value - ys[index - 1] >= 174 - 1e-8));
  }
});

test("visual columns run from body consumers to head providers", () => {
  assert.deepEqual(visualColumnOrder, ["dependent", "compiled", "dependency"]);
});
