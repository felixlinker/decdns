"use client";

import {
  KeyboardEvent,
  PointerEvent as ReactPointerEvent,
  useCallback,
  useEffect,
  useMemo,
  useRef,
  useState,
} from "react";
import { bodyToHeadEdge, groupSourcesByNameAndServer, minimizeArrowLayout, predicateFromOtherRule, ruleEdgePorts, ruleNodeSourceLines, sourceGroupKey, visualColumnOrder } from "./graph";
import type { NameGroup } from "./graph";

type Source = {
  id: string;
  server: string;
  owner: string;
  recordType: string;
  records: string[];
  ruleIds: string[];
};
type SourceName = NameGroup<Source>;

type Predicate = { index: number; kind: string; text: string };
type Rule = { id: string; sourceId: string | null; text: string; head: string; body: Predicate[] };
type Binding = { variable: string; target: string; targetKind: string };
type Match = {
  dependentRuleId: string;
  dependencyRuleId: string;
  predicate: string;
  unifier: string;
  bindings: Binding[];
};

type Dataset = {
  version: number;
  rootServers: string[];
  zoneFiles: string[];
  sources: Source[];
  rules: Rule[];
  matches: Match[];
};

type VisualizationFile = {
  name: string;
  size: number;
  modifiedAt: string;
};

type VisualizationCatalog = {
  directory: string;
  files: VisualizationFile[];
  errors: { name: string; error: string }[];
};

type DirectorySelection = {
  catalog: VisualizationCatalog;
  filename: string;
  dataset: Dataset | null;
};

type Placement = { id: string; x: number; y: number; column: "dependency" | "compiled" | "dependent" };
type Viewport = { x: number; y: number; scale: number };

const nodeWidth = 360;
const nodeHeight = 108;
const { ingress: ingressPortY, egress: egressPortY } = ruleEdgePorts(nodeHeight);
const rowGap = 66;
const graphViewportWidth = 1040;
const graphViewportHeight = 680;
const graphContentWidth = 1768;
const defaultViewport: Viewport = { x: 25, y: 24, scale: 0.56 };
const minScale = 0.18;
const maxScale = 2.4;

export default function Home() {
  const [data, setData] = useState<Dataset | null>(null);
  const [datasetName, setDatasetName] = useState("");
  const [catalog, setCatalog] = useState<VisualizationCatalog | null>(null);
  const [selectedFile, setSelectedFile] = useState("");
  const [isReloading, setIsReloading] = useState(false);
  const [directoryNotice, setDirectoryNotice] = useState("");
  const [loadError, setLoadError] = useState("");
  const [selectedSourceKey, setSelectedSourceKey] = useState("");
  const [selectedRuleId, setSelectedRuleId] = useState("");
  const [search, setSearch] = useState("");
  const [serverFilter, setServerFilter] = useState("all");
  const [viewport, setViewport] = useState<Viewport>(defaultViewport);
  const [isPanning, setIsPanning] = useState(false);
  const graphSvg = useRef<SVGSVGElement>(null);
  const panStart = useRef<{ pointerId: number; point: DOMPoint; viewport: Viewport } | null>(null);

  const installDataset = useCallback((next: Dataset, name: string) => {
    if (next.version !== 2 || !Array.isArray(next.sources) || !Array.isArray(next.rules) || !Array.isArray(next.matches)) {
      throw new Error("this is not a DNS Regressor visualization export (version 2)");
    }
    setData(next);
    setDatasetName(name);
    const cycleRuleId = findCycles(next)[0]?.[0];
    const focusRuleId = cycleRuleId ?? next.matches[0]?.dependentRuleId;
    const focusRule = next.rules.find((rule) => rule.id === focusRuleId);
    const focusSource = focusRule?.sourceId
      ? next.sources.find((source) => source.id === focusRule.sourceId)
      : next.sources.find((source) => source.recordType === "NS") ?? next.sources[0];
    setSelectedSourceKey(focusSource ? sourceGroupKey(focusSource.owner, focusSource.server) : "");
    setSelectedRuleId(focusRule?.id ?? "");
    setServerFilter("all");
    setSearch("");
    setViewport(defaultViewport);
    setLoadError("");
  }, []);

  const installDirectorySelection = useCallback((selection: DirectorySelection) => {
    setCatalog(selection.catalog);
    setSelectedFile(selection.filename);
    setDirectoryNotice(catalogNotice(selection.catalog));
    if (selection.dataset) {
      installDataset(selection.dataset, selection.filename);
      return;
    }
    setData(null);
    setDatasetName("");
    setSelectedSourceKey("");
    setSelectedRuleId("");
    setLoadError("");
  }, [installDataset]);

  useEffect(() => {
    let cancelled = false;
    void fetchDirectorySelection().then(
      (selection) => { if (!cancelled) installDirectorySelection(selection); },
      (error) => { if (!cancelled) setLoadError(`Could not read the visualization directory: ${messageOf(error)}`); },
    );
    return () => { cancelled = true; };
  }, [installDirectorySelection]);

  async function selectVisualization(filename: string) {
    if (!filename) return;
    try {
      installDataset(await fetchServerDataset(filename), filename);
      setSelectedFile(filename);
    } catch (error) {
      setLoadError(`Could not load ${filename}: ${messageOf(error)}`);
    }
  }

  async function reloadDirectory() {
    setIsReloading(true);
    try {
      installDirectorySelection(await fetchDirectorySelection(selectedFile));
    } catch (error) {
      setLoadError(`Could not reload the visualization directory: ${messageOf(error)}`);
    } finally {
      setIsReloading(false);
    }
  }

  const indexes = useMemo(() => makeIndexes(data), [data]);
  const cycles = useMemo(() => findCycles(data), [data]);
  const cycleNodes = useMemo(() => new Set(cycles.flat()), [cycles]);
  const cycleMembership = useMemo(
    () => new Map(cycles.flatMap((component, index) => component.map((ruleId) => [ruleId, index] as const))),
    [cycles],
  );
  const sourceNames = useMemo(() => groupSourcesByNameAndServer(data?.sources ?? []), [data]);
  const selectedSourceName = selectedSourceKey ? sourceNames.find((group) => group.key === selectedSourceKey) ?? null : null;

  const filteredSourceNames = useMemo(() => {
    const needle = search.trim().toLowerCase();
    return sourceNames.filter((group) => {
      const serverMatches = serverFilter === "all" || group.server === serverFilter;
      const textMatches = !needle || [group.owner, group.server, ...group.recordTypes, ...group.records]
        .join(" ").toLowerCase().includes(needle) || group.ruleIds.some((id) => indexes.rules.get(id)?.text.toLowerCase().includes(needle));
      return serverMatches && textMatches;
    });
  }, [indexes.rules, search, serverFilter, sourceNames]);

  const graph = useMemo(
    () => buildGraph(data, indexes, selectedSourceName, search),
    [data, indexes, search, selectedSourceName],
  );

  const activeRuleId = graph.placements.some((placement) => placement.id === selectedRuleId)
    ? selectedRuleId
    : graph.placements.find((placement) => placement.column === "compiled")?.id ?? graph.placements[0]?.id ?? "";
  const selectedRule = activeRuleId ? indexes.rules.get(activeRuleId) ?? null : null;
  const selectedRuleSource = selectedRule?.sourceId ? indexes.sources.get(selectedRule.sourceId) ?? null : null;
  const selectedMatches = data?.matches.filter(
    (match) => match.dependentRuleId === activeRuleId || match.dependencyRuleId === activeRuleId,
  ) ?? [];
  const servers = data ? [...new Set(data.sources.map((source) => source.server))] : [];

  function graphPoint(clientX: number, clientY: number) {
    const svg = graphSvg.current;
    const matrix = svg?.getScreenCTM();
    if (!svg || !matrix) return null;
    const point = svg.createSVGPoint();
    point.x = clientX;
    point.y = clientY;
    return point.matrixTransform(matrix.inverse());
  }

  function zoomAround(point: { x: number; y: number }, factor: number) {
    setViewport((current) => scaledViewport(current, point, factor));
  }

  function zoomFromCenter(factor: number) {
    zoomAround({ x: graphViewportWidth / 2, y: graphViewportHeight / 2 }, factor);
  }

  function fitGraph() {
    const margin = 54;
    const scale = clamp(Math.min(
      (graphViewportWidth - margin * 2) / graph.width,
      (graphViewportHeight - margin * 2) / graph.height,
    ), minScale, maxScale);
    setViewport({
      x: (graphViewportWidth - graph.width * scale) / 2,
      y: (graphViewportHeight - graph.height * scale) / 2,
      scale,
    });
  }

  function startPan(event: ReactPointerEvent<SVGSVGElement>) {
    if (event.button !== 0 || (event.target as Element).closest(".rule-node")) return;
    const point = graphPoint(event.clientX, event.clientY);
    if (!point) return;
    event.preventDefault();
    event.currentTarget.setPointerCapture(event.pointerId);
    panStart.current = { pointerId: event.pointerId, point, viewport };
    setIsPanning(true);
  }

  function movePan(event: ReactPointerEvent<SVGSVGElement>) {
    const start = panStart.current;
    if (!start || start.pointerId !== event.pointerId) return;
    const point = graphPoint(event.clientX, event.clientY);
    if (!point) return;
    event.preventDefault();
    setViewport({
      ...start.viewport,
      x: start.viewport.x + point.x - start.point.x,
      y: start.viewport.y + point.y - start.point.y,
    });
  }

  function stopPan(event: ReactPointerEvent<SVGSVGElement>) {
    if (panStart.current?.pointerId !== event.pointerId) return;
    if (event.currentTarget.hasPointerCapture(event.pointerId)) event.currentTarget.releasePointerCapture(event.pointerId);
    panStart.current = null;
    setIsPanning(false);
  }

  useEffect(() => {
    const svg = graphSvg.current;
    if (!svg) return;
    function wheelZoom(event: WheelEvent) {
      const matrix = svg!.getScreenCTM();
      if (!matrix) return;
      const pointer = svg!.createSVGPoint();
      pointer.x = event.clientX;
      pointer.y = event.clientY;
      const point = pointer.matrixTransform(matrix.inverse());
      event.preventDefault();
      setViewport((current) => scaledViewport(current, point, Math.exp(-event.deltaY * 0.0015)));
    }
    svg.addEventListener("wheel", wheelZoom, { passive: false });
    return () => svg.removeEventListener("wheel", wheelZoom);
  }, [graph.placements.length]);

  return (
    <main className="app-shell">
      <header className="topbar">
        <div className="brand-lockup">
          <div className="brand-mark" aria-hidden="true">DR</div>
          <div>
            <p className="eyebrow">DNS REGRESSOR</p>
            <h1>Compilation explorer</h1>
          </div>
        </div>
        <div className="dataset-meta">
          <span className="status-dot" aria-hidden="true" />
          <div>
            <strong>{datasetName || "No visualization selected"}</strong>
            <span>{data
              ? `${data.zoneFiles.length} zone files · roots ${data.rootServers.join(", ")}`
              : catalog
                ? `${catalog.files.length} file${catalog.files.length === 1 ? "" : "s"} · ${catalog.directory}`
                : "Reading configured directory…"}</span>
          </div>
        </div>
        <div className="header-actions">
          <label className="visualization-picker">
            <span>Visualization file</span>
            <select
              value={selectedFile}
              disabled={!catalog?.files.length || isReloading}
              onChange={(event) => void selectVisualization(event.target.value)}
            >
              {!catalog?.files.length && <option value="">No valid JSON files</option>}
              {catalog?.files.map((file) => <option key={file.name} value={file.name}>{file.name}</option>)}
            </select>
          </label>
          <button className="button primary" disabled={isReloading} onClick={() => void reloadDirectory()}>
            {isReloading ? "Reloading…" : "Reload"}
          </button>
        </div>
      </header>

      {loadError && <div className="error-banner" role="alert">{loadError}</div>}
      {directoryNotice && <div className="directory-notice" role="status">{directoryNotice}</div>}

      <section className="summary-strip" aria-label="Dataset summary">
        <Metric label="Name servers" value={servers.length} />
        <Metric label="Record sets" value={data?.sources.length ?? 0} />
        <Metric label="Compiled rules" value={data?.rules.length ?? 0} />
        <Metric label="Dependency edges" value={data?.matches.length ?? 0} tone="green" />
        <Metric label="Unifier bindings" value={data?.matches.reduce((count, match) => count + match.bindings.length, 0) ?? 0} />
        <Metric label="Dependency cycles" value={cycles.length} tone={cycles.length ? "red" : "default"} />
      </section>

      <div className="workspace">
        <aside className="source-panel">
          <div className="panel-heading">
            <div>
              <p className="eyebrow">INPUT</p>
              <h2>Names</h2>
            </div>
            <span className="count-badge">{filteredSourceNames.length}</span>
          </div>
          <label className="search-box">
            <span aria-hidden="true">⌕</span>
            <input value={search} onChange={(event) => setSearch(event.target.value)} placeholder="Find name or rule" />
          </label>
          <div className="server-filters" aria-label="Filter by name server">
            <button className={serverFilter === "all" ? "active" : ""} onClick={() => setServerFilter("all")}>All servers</button>
            {servers.map((server) => (
              <button key={server} className={serverFilter === server ? "active" : ""} onClick={() => setServerFilter(server)}>{server}</button>
            ))}
          </div>
          <div className="source-list">
            {filteredSourceNames.map((group) => (
              <button
                key={group.key}
                className={`source-card name-card ${selectedSourceKey === group.key ? "selected" : ""}`}
                onClick={() => { setSelectedSourceKey(group.key); setSelectedRuleId(""); setViewport(defaultViewport); }}
              >
                <span className="source-copy">
                  <strong>{group.owner}</strong>
                  <span>{group.server} — {group.recordTypes.join(" · ")}</span>
                </span>
                <span className="rule-count">{group.ruleIds.length}<small>rules</small></span>
              </button>
            ))}
            {!filteredSourceNames.length && <p className="empty-state">No names match the current filters.</p>}
          </div>
        </aside>

        <section className="graph-panel">
          <div className="graph-toolbar">
            <div>
              <p className="eyebrow">COMPILATION + REASONING</p>
              <h2>{selectedSourceName ? <span>{selectedSourceName.owner} · {selectedSourceName.server}</span> : "Rule graph"}</h2>
              <p>{selectedSourceName ? `${selectedSourceName.sources.length} record set${selectedSourceName.sources.length === 1 ? "" : "s"} across ${selectedSourceName.recordTypes.length} type${selectedSourceName.recordTypes.length === 1 ? "" : "s"} compile to ${selectedSourceName.ruleIds.length} rules` : "Select a name to focus its compiled rules."}</p>
            </div>
          </div>

          <div className={`graph-scroll ${isPanning ? "is-panning" : ""}`}>
            <div className="graph-controls" aria-label="Graph zoom controls">
              <button type="button" onClick={() => zoomFromCenter(1 / 1.22)} aria-label="Zoom out">−</button>
              <span className="zoom-readout" aria-live="polite">{Math.round(viewport.scale * 100)}%</span>
              <button type="button" onClick={() => zoomFromCenter(1.22)} aria-label="Zoom in">+</button>
              <button type="button" className="text-control" onClick={fitGraph}>Fit</button>
              <button type="button" className="text-control" onClick={() => setViewport(defaultViewport)}>Reset</button>
            </div>
            <div className="graph-gesture-hint">Drag canvas to pan · Scroll to zoom</div>
            {graph.placements.length ? (
              <svg
                ref={graphSvg}
                className="rule-graph"
                viewBox={`0 0 ${graphViewportWidth} ${graphViewportHeight}`}
                role="img"
                aria-label="Interactive rule dependency graph. Drag to pan and scroll to zoom."
                onPointerDown={startPan}
                onPointerMove={movePan}
                onPointerUp={stopPan}
                onPointerCancel={stopPan}
              >
                <defs>
                  <marker id="arrow-dependency" markerWidth="8" markerHeight="8" refX="7" refY="4" orient="auto" markerUnits="strokeWidth">
                    <path d="M0,0 L8,4 L0,8 z" className="arrow-head" />
                  </marker>
                  <marker id="egress-dependency" markerWidth="5" markerHeight="5" refX="2.5" refY="2.5" markerUnits="strokeWidth">
                    <circle cx="2.5" cy="2.5" r="2.3" className="egress-dot" />
                  </marker>
                  <marker id="arrow-cycle" markerWidth="8" markerHeight="8" refX="7" refY="4" orient="auto" markerUnits="strokeWidth">
                    <path d="M0,0 L8,4 L0,8 z" className="cycle-marker" />
                  </marker>
                  <marker id="egress-cycle" markerWidth="5" markerHeight="5" refX="2.5" refY="2.5" markerUnits="strokeWidth">
                    <circle cx="2.5" cy="2.5" r="2.3" className="cycle-marker" />
                  </marker>
                </defs>
                <g transform={`translate(${viewport.x} ${viewport.y}) scale(${viewport.scale})`}>
                  <GraphColumn x={64} label="Body consumers" sublabel="bodies use selected heads" />
                  <GraphColumn x={704} label="Compiled rules" sublabel="from selected records" />
                  <GraphColumn x={1344} label="Head providers" sublabel="heads satisfy selected bodies" />
                  {graph.edges.map(({ match, from, to }, index) => {
                    const cyclic = cycleMembership.has(match.dependencyRuleId)
                      && cycleMembership.get(match.dependencyRuleId) === cycleMembership.get(match.dependentRuleId);
                    const connected = match.dependencyRuleId === activeRuleId || match.dependentRuleId === activeRuleId;
                    return (
                      <path
                        key={`${match.dependencyRuleId}-${match.dependentRuleId}-${index}`}
                        d={edgePath(from, to, index)}
                        className={`graph-edge ${cyclic ? "cyclic" : ""} ${connected ? "connected" : "muted"}`}
                        markerStart={`url(#egress-${cyclic ? "cycle" : "dependency"})`}
                        markerEnd={`url(#arrow-${cyclic ? "cycle" : "dependency"})`}
                      >
                        <title>{`Body ${match.dependentRuleId} → head ${match.dependencyRuleId}\n${match.predicate}\n${match.unifier}`}</title>
                      </path>
                    );
                  })}
                  {graph.placements.map((placement) => {
                    const rule = indexes.rules.get(placement.id);
                    const source = rule?.sourceId ? indexes.sources.get(rule.sourceId) : null;
                    if (!rule) return null;
                    const isCycle = cycleNodes.has(rule.id);
                    const sourceLines = ruleNodeSourceLines(source);
                    return (
                      <g
                        key={rule.id}
                        transform={`translate(${placement.x} ${placement.y})`}
                        className={`rule-node ${activeRuleId === rule.id ? "selected" : ""} ${isCycle ? "in-cycle" : ""}`}
                        role="button"
                        tabIndex={0}
                        aria-label={`Inspect ${rule.id}${source ? ` served by ${source.server}` : ""}`}
                        onClick={() => setSelectedRuleId(rule.id)}
                        onKeyDown={(event: KeyboardEvent<SVGGElement>) => { if (event.key === "Enter" || event.key === " ") setSelectedRuleId(rule.id); }}
                      >
                        <rect width={nodeWidth} height={nodeHeight} rx="10" />
                        <text x="15" y="25" className="node-id">{rule.id.toUpperCase()}</text>
                        {isCycle && <text x={nodeWidth - 16} y="25" textAnchor="end" className="cycle-label">CYCLE</text>}
                        <text x="15" y="48" className="node-source">{truncate(sourceLines.record, 47)}</text>
                        {sourceLines.server && <text x="15" y="64" className="node-server">{truncate(sourceLines.server, 55)}</text>}
                        <text x="15" y="84" className="node-head">{truncate(rule.head, 54)}</text>
                        <text x={nodeWidth - 15} y="99" textAnchor="end" className="node-body-count">{rule.body.length} body</text>
                      </g>
                    );
                  })}
                </g>
              </svg>
            ) : <div className="graph-empty">No compiled rules match this focus and search.</div>}
          </div>
          <div className="graph-footnote">
            <span>Red outlines mark rules in a dependency cycle</span>
          </div>
        </section>

        <aside className="inspector-panel">
          <div className="panel-heading inspector-heading">
            <div><p className="eyebrow">INSPECTOR</p><h2>{selectedRule?.id.toUpperCase() ?? "Select a rule"}</h2></div>
            {selectedRule && <span className={`cycle-chip ${cycleNodes.has(selectedRule.id) ? "visible" : ""}`}>cycle</span>}
          </div>
          {selectedRule ? (
            <div className="inspector-content">
              {selectedRuleSource && (
                <section className="detail-section source-detail">
                  <p className="section-label">Compiled from <span>{selectedRuleSource.recordType}</span></p>
                  <code title={`${selectedRuleSource.owner} · ${selectedRuleSource.server}`}>
                    {selectedRuleSource.owner} · {selectedRuleSource.server}
                  </code>
                </section>
              )}
              <section className="detail-section">
                <p className="section-label">Rule head</p>
                <code>{selectedRule.head}</code>
              </section>
              <section className="detail-section">
                <p className="section-label">Rule body</p>
                {selectedRule.body.length ? selectedRule.body.map((predicate) => (
                  <div key={predicate.index} className="predicate-row">
                    <span>{predicate.index + 1}</span><code>{predicate.text}</code><small>{predicate.kind}</small>
                  </div>
                )) : <p className="muted">Fact: no body predicates.</p>}
              </section>
              <section className="detail-section matches-section">
                <p className="section-label">Dependency matches <span>{selectedMatches.length}</span></p>
                <div className="match-list">
                  {selectedMatches.map((match, index) => {
                    const incoming = match.dependentRuleId === selectedRule.id;
                    const otherId = incoming ? match.dependencyRuleId : match.dependentRuleId;
                    const matchingPredicate = predicateFromOtherRule(match, selectedRule.id, indexes.rules);
                    return (
                      <button key={`${otherId}-${index}`} className="match-card" onClick={() => setSelectedRuleId(otherId)}>
                        <span className="match-topline"><strong>{incoming ? "depends on" : "used by"} {otherId.toUpperCase()}</strong><em>Dependency</em></span>
                        <code>{matchingPredicate}</code>
                        <span className="unifier-label">Unifier</span>
                        <code>{match.unifier}</code>
                        <span className="binding-list">
                          {match.bindings.map((binding) => (
                            <span key={`${binding.variable}-${binding.target}`} className={`binding ${binding.targetKind}`}>
                              {binding.variable} → {binding.target}<small>{binding.targetKind}</small>
                            </span>
                          ))}
                        </span>
                      </button>
                    );
                  })}
                  {!selectedMatches.length && <p className="muted">No rule heads unify with this rule’s resolves predicates.</p>}
                </div>
              </section>
            </div>
          ) : <p className="empty-state inspector-empty">Choose a rule node to inspect its predicates and unifiers.</p>}
        </aside>
      </div>
    </main>
  );
}

function Metric({ label, value, tone = "default" }: { label: string; value: number; tone?: string }) {
  return <div className={`metric ${tone}`}><strong>{value.toLocaleString()}</strong><span>{label}</span></div>;
}

function GraphColumn({ x, label, sublabel }: { x: number; label: string; sublabel: string }) {
  return <g transform={`translate(${x} 0)`}><text y="23" className="column-label">{label}</text><text y="41" className="column-sublabel">{sublabel}</text></g>;
}

function makeIndexes(data: Dataset | null) {
  return {
    rules: new Map((data?.rules ?? []).map((rule) => [rule.id, rule])),
    sources: new Map((data?.sources ?? []).map((source) => [source.id, source])),
  };
}

function buildGraph(
  data: Dataset | null,
  indexes: ReturnType<typeof makeIndexes>,
  source: SourceName | null,
  search: string,
) {
  if (!data) return { placements: [] as Placement[], edges: [] as { match: Match; from: Placement; to: Placement }[], width: graphContentWidth, height: 300 };
  const needle = search.trim().toLowerCase();
  const matchesSearch = (id: string) => !needle || indexes.rules.get(id)?.text.toLowerCase().includes(needle);
  let compiled = (source?.ruleIds ?? data.rules.map((rule) => rule.id)).filter(matchesSearch);
  if (!compiled.length && needle) compiled = data.rules.filter((rule) => rule.text.toLowerCase().includes(needle)).map((rule) => rule.id);
  compiled = compiled.slice(0, 24);
  const compiledSet = new Set(compiled);
  const relevantMatches = data.matches.filter((match) => compiledSet.has(match.dependentRuleId) || compiledSet.has(match.dependencyRuleId));
  const dependencies = unique(relevantMatches.filter((match) => compiledSet.has(match.dependentRuleId)).map((match) => match.dependencyRuleId))
    .filter((id) => !compiledSet.has(id)).slice(0, 24);
  const dependents = unique(relevantMatches.filter((match) => compiledSet.has(match.dependencyRuleId)).map((match) => match.dependentRuleId))
    .filter((id) => !compiledSet.has(id) && !dependencies.includes(id)).slice(0, 24);
  const visibleIds = new Set([...dependencies, ...compiled, ...dependents]);
  const layoutEdges = relevantMatches
    .filter((match) => visibleIds.has(match.dependentRuleId) && visibleIds.has(match.dependencyRuleId))
    .map((match) => ({ from: match.dependentRuleId, to: match.dependencyRuleId }));
  const columnXs = Object.fromEntries(visualColumnOrder.map((column, index) => [column, 64 + index * 640])) as Record<Placement["column"], number>;
  const placements = minimizeArrowLayout(
    { dependency: dependencies, compiled, dependent: dependents },
    layoutEdges,
    { top: 68, maximumTopOffset: rowGap, nodeHeight, minimumGap: rowGap, ingressPortY, egressPortY },
  ).map(({ id, column, y }) => ({ id, column, y, x: columnXs[column] }));
  const byId = new Map(placements.map((placement) => [placement.id, placement]));
  const edges = relevantMatches.map((match) => {
    const endpoints = bodyToHeadEdge(match, byId);
    return endpoints ? { match, ...endpoints } : null;
  }).filter((edge): edge is { match: Match; from: Placement; to: Placement } => edge !== null);
  return {
    placements,
    edges,
    width: graphContentWidth,
    height: Math.max(390, ...placements.map((placement) => placement.y + nodeHeight + 68)),
  };
}

function edgePath(from: Placement, to: Placement, index: number) {
  const lane = ((index % 9) - 4) * 14;
  if (from.x === to.x) {
    const edgeX = from.x + nodeWidth;
    const y1 = from.y + egressPortY;
    const y2 = to.y + ingressPortY;
    const loopX = edgeX + 82 + (index % 6) * 18;
    return `M ${edgeX} ${y1} C ${loopX} ${y1 + lane}, ${loopX} ${y2 + lane}, ${edgeX} ${y2}`;
  }
  const travelsRight = to.x > from.x;
  const x1 = travelsRight ? from.x + nodeWidth : from.x;
  const x2 = travelsRight ? to.x : to.x + nodeWidth;
  const y1 = from.y + egressPortY;
  const y2 = to.y + ingressPortY;
  const direction = travelsRight ? 1 : -1;
  const bend = Math.max(82, Math.abs(x2 - x1) * 0.43);
  return `M ${x1} ${y1} C ${x1 + direction * bend} ${y1 + lane}, ${x2 - direction * bend} ${y2 + lane}, ${x2} ${y2}`;
}

function findCycles(data: Dataset | null) {
  if (!data) return [] as string[][];
  const adjacency = new Map<string, string[]>();
  data.rules.forEach((rule) => adjacency.set(rule.id, []));
  data.matches.forEach((match) => adjacency.get(match.dependencyRuleId)?.push(match.dependentRuleId));
  let nextIndex = 0;
  const indexes = new Map<string, number>();
  const lows = new Map<string, number>();
  const stack: string[] = [];
  const onStack = new Set<string>();
  const components: string[][] = [];
  function visit(node: string) {
    indexes.set(node, nextIndex); lows.set(node, nextIndex); nextIndex += 1;
    stack.push(node); onStack.add(node);
    for (const neighbor of adjacency.get(node) ?? []) {
      if (!indexes.has(neighbor)) { visit(neighbor); lows.set(node, Math.min(lows.get(node)!, lows.get(neighbor)!)); }
      else if (onStack.has(neighbor)) lows.set(node, Math.min(lows.get(node)!, indexes.get(neighbor)!));
    }
    if (lows.get(node) === indexes.get(node)) {
      const component: string[] = [];
      let member = "";
      do { member = stack.pop()!; onStack.delete(member); component.push(member); } while (member !== node);
      const selfLoop = component.length === 1 && (adjacency.get(component[0]) ?? []).includes(component[0]);
      if (component.length > 1 || selfLoop) components.push(component);
    }
  }
  data.rules.forEach((rule) => { if (!indexes.has(rule.id)) visit(rule.id); });
  return components;
}

function unique<T>(values: T[]) { return [...new Set(values)]; }
function clamp(value: number, minimum: number, maximum: number) { return Math.min(maximum, Math.max(minimum, value)); }
function scaledViewport(current: Viewport, point: { x: number; y: number }, factor: number): Viewport {
  const scale = clamp(current.scale * factor, minScale, maxScale);
  const worldX = (point.x - current.x) / current.scale;
  const worldY = (point.y - current.y) / current.scale;
  return { x: point.x - worldX * scale, y: point.y - worldY * scale, scale };
}
function truncate(value: string, max: number) { return value.length > max ? `${value.slice(0, max - 1)}…` : value; }
function messageOf(error: unknown) { return error instanceof Error ? error.message : String(error); }

async function fetchDirectorySelection(preferredFilename = ""): Promise<DirectorySelection> {
  const catalog = await fetchCatalog();
  const filename = catalog.files.some((file) => file.name === preferredFilename)
    ? preferredFilename
    : catalog.files[0]?.name ?? "";
  const dataset = filename ? await fetchServerDataset(filename) : null;
  return { catalog, filename, dataset };
}

async function fetchCatalog() {
  return fetchJson<VisualizationCatalog>("/api/visualizations");
}

async function fetchServerDataset(filename: string) {
  return fetchJson<Dataset>(`/api/visualizations?file=${encodeURIComponent(filename)}`);
}

async function fetchJson<T>(path: string): Promise<T> {
  const response = await fetch(path, { cache: "no-store" });
  if (!response.ok) {
    const body = await response.json().catch(() => null) as { error?: string } | null;
    throw new Error(body?.error ?? `server returned ${response.status}`);
  }
  return response.json() as Promise<T>;
}

function catalogNotice(catalog: VisualizationCatalog) {
  if (!catalog.errors.length) return "";
  const names = catalog.errors.map((entry) => entry.name).join(", ");
  return `Skipped ${catalog.errors.length} invalid JSON file${catalog.errors.length === 1 ? "" : "s"}: ${names}`;
}
