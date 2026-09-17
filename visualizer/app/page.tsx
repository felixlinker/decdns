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
import { bodyToHeadEdge, layoutGraphLayers, moveRuleSelection, predicateFromOtherRule, projectOwnerServerGraph, ruleEdgePorts, ruleNodeSourceLines, ruleSelectionForSource, sourceHasCycle } from "./graph";

type Source = {
  id: string;
  server: string;
  owner: string;
  recordType: string;
  records: string[];
  ruleIds: string[];
};
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

type Placement = { id: string; x: number; y: number };
type GraphNode = { id: string; owner: string | null; server: string | null; ruleIds: string[] };
type GraphEdge = { matches: Match[]; from: Placement; to: Placement };
type Viewport = { x: number; y: number; scale: number };

const nodeWidth = 360;
const nodeHeight = 108;
const { ingress: ingressPortY, egress: egressPortY } = ruleEdgePorts(nodeHeight);
const rowGap = 66;
const graphViewportWidth = 1040;
const graphViewportHeight = 680;
const graphColumnSpacing = 640;
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
  const [selectedSourceId, setSelectedSourceId] = useState("");
  const [selectedRuleId, setSelectedRuleId] = useState("");
  const [search, setSearch] = useState("");
  const [serverFilter, setServerFilter] = useState("all");
  const [showSourcePanel, setShowSourcePanel] = useState(true);
  const [showInspectorPanel, setShowInspectorPanel] = useState(true);
  const [projectByOwnerServer, setProjectByOwnerServer] = useState(false);
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
    setSelectedSourceId(focusSource?.id ?? "");
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
    setSelectedSourceId("");
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
  const selectedSource = selectedSourceId ? indexes.sources.get(selectedSourceId) ?? null : null;

  const filteredSources = useMemo(() => {
    const needle = search.trim().toLowerCase();
    return (data?.sources ?? []).filter((source) => {
      const serverMatches = serverFilter === "all" || source.server === serverFilter;
      const textMatches = !needle || [source.owner, source.server, source.recordType, ...source.records]
        .join(" ").toLowerCase().includes(needle) || source.ruleIds.some((id) => indexes.rules.get(id)?.text.toLowerCase().includes(needle));
      return serverMatches && textMatches;
    });
  }, [data, indexes.rules, search, serverFilter]);

  const graph = useMemo(() => buildGraph(data, projectByOwnerServer), [data, projectByOwnerServer]);
  const graphCycles = useMemo(
    () => findGraphCycles(graph.nodes.map((node) => node.id), graph.edges.map((edge) => ({ from: edge.from.id, to: edge.to.id }))),
    [graph],
  );
  const graphCycleMembership = useMemo(
    () => new Map(graphCycles.flatMap((component, index) => component.map((nodeId) => [nodeId, index] as const))),
    [graphCycles],
  );

  const selectedSourceRuleIds = selectedSource?.ruleIds.filter((id) => indexes.rules.has(id)) ?? [];
  const activeRuleId = selectedSource
    ? selectedSourceRuleIds.includes(selectedRuleId) ? selectedRuleId : selectedSourceRuleIds[0] ?? ""
    : graph.nodes.some((node) => node.ruleIds.includes(selectedRuleId))
      ? selectedRuleId
      : graph.nodes[0]?.ruleIds[0] ?? "";
  const selectedRuleIds = ruleSelectionForSource(selectedSource, activeRuleId);
  const selectedGraphNodeIds = new Set(graph.nodes
    .filter((node) => node.ruleIds.some((ruleId) => selectedRuleIds.has(ruleId)))
    .map((node) => node.id));
  const activeSourceRuleIndex = selectedSourceRuleIds.indexOf(activeRuleId);
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

  function selectRule(ruleId: string) {
    if (!selectedSourceRuleIds.includes(ruleId)) setSelectedSourceId("");
    setSelectedRuleId(ruleId);
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

      <div className={`workspace ${showSourcePanel ? "" : "hide-source-panel"} ${showInspectorPanel ? "" : "hide-inspector-panel"}`}>
        <aside className="source-panel" hidden={!showSourcePanel}>
          <div className="panel-heading">
            <div>
              <p className="eyebrow">INPUT</p>
              <h2>RRSets</h2>
            </div>
            <span className="count-badge">{filteredSources.length}</span>
          </div>
          <label className="search-box">
            <span aria-hidden="true">⌕</span>
            <input value={search} onChange={(event) => setSearch(event.target.value)} placeholder="Find RRSet or rule" />
          </label>
          <div className="server-filters" aria-label="Filter by name server">
            <button className={serverFilter === "all" ? "active" : ""} onClick={() => setServerFilter("all")}>All servers</button>
            {servers.map((server) => (
              <button key={server} className={serverFilter === server ? "active" : ""} onClick={() => setServerFilter(server)}>{server}</button>
            ))}
          </div>
          <div className="source-list">
            {filteredSources.map((source) => {
              const hasCycle = sourceHasCycle(source, cycleNodes);
              return (
                <button
                  key={source.id}
                  className={`source-card rrset-card ${hasCycle ? "has-cycle" : ""} ${selectedSourceId === source.id ? "selected" : ""}`}
                  aria-label={`${source.owner} ${source.recordType} on ${source.server}, ${source.ruleIds.length} rules${hasCycle ? ", contains a dependency cycle" : ""}`}
                  onClick={() => { setSelectedSourceId(source.id); setSelectedRuleId(source.ruleIds[0] ?? ""); }}
                >
                  <span className="source-copy">
                    <strong>{source.owner} {source.recordType}</strong>
                    <span>served by {source.server}</span>
                  </span>
                  {hasCycle && <span className="source-cycle-label">Cycle</span>}
                  <span className="rule-count">{source.ruleIds.length}<small>rules</small></span>
                </button>
              );
            })}
            {!filteredSources.length && <p className="empty-state">No RRSets match the current filters.</p>}
          </div>
        </aside>

        <section className="graph-panel">
          <div className={`graph-scroll ${isPanning ? "is-panning" : ""}`}>
            <div className="panel-toggles" aria-label="Panel visibility controls">
              <button
                type="button"
                className={showSourcePanel ? "active" : ""}
                aria-label={`${showSourcePanel ? "Hide" : "Show"} RRSets panel`}
                aria-pressed={showSourcePanel}
                onClick={() => setShowSourcePanel((visible) => !visible)}
              >☰ RRSets</button>
              <button
                type="button"
                className={showInspectorPanel ? "active" : ""}
                aria-label={`${showInspectorPanel ? "Hide" : "Show"} inspector panel`}
                aria-pressed={showInspectorPanel}
                onClick={() => setShowInspectorPanel((visible) => !visible)}
              >Inspector ☰</button>
            </div>
            <div className="graph-controls" aria-label="Graph zoom controls">
              <button
                type="button"
                className={`text-control projection-control ${projectByOwnerServer ? "active" : ""}`}
                aria-label={projectByOwnerServer ? "Show full rule graph" : "Project graph by owner name and name server"}
                aria-pressed={projectByOwnerServer}
                onClick={() => { setProjectByOwnerServer((projected) => !projected); setViewport(defaultViewport); }}
              >{projectByOwnerServer ? "Rules" : "Owner + server"}</button>
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
                  {graph.edges.map(({ matches, from, to }, index) => {
                    const cyclic = graphCycleMembership.has(from.id)
                      && graphCycleMembership.get(from.id) === graphCycleMembership.get(to.id);
                    const connected = selectedGraphNodeIds.has(from.id) || selectedGraphNodeIds.has(to.id);
                    const firstMatch = matches[0];
                    return (
                      <path
                        key={`${from.id}-${to.id}-${index}`}
                        d={edgePath(from, to, index)}
                        className={`graph-edge ${cyclic ? "cyclic" : ""} ${connected ? "connected" : "muted"}`}
                        markerStart={`url(#egress-${cyclic ? "cycle" : "dependency"})`}
                        markerEnd={`url(#arrow-${cyclic ? "cycle" : "dependency"})`}
                      >
                        <title>{projectByOwnerServer
                          ? `${matches.length} rule dependenc${matches.length === 1 ? "y" : "ies"}`
                          : `Body ${firstMatch.dependentRuleId} → head ${firstMatch.dependencyRuleId}\n${firstMatch.predicate}\n${firstMatch.unifier}`}</title>
                      </path>
                    );
                  })}
                  {graph.placements.map((placement) => {
                    const node = graph.nodesById.get(placement.id);
                    const rule = node ? indexes.rules.get(node.ruleIds[0]) : null;
                    const source = rule?.sourceId ? indexes.sources.get(rule.sourceId) : null;
                    if (!rule || !node) return null;
                    const isCycle = graphCycleMembership.has(node.id);
                    const sourceLines = ruleNodeSourceLines(source);
                    const selected = selectedGraphNodeIds.has(node.id);
                    return (
                      <g
                        key={node.id}
                        transform={`translate(${placement.x} ${placement.y})`}
                        className={`rule-node ${projectByOwnerServer ? "projected-node" : ""} ${selected ? "selected" : ""} ${node.ruleIds.includes(activeRuleId) ? "active" : ""} ${isCycle ? "in-cycle" : ""}`}
                        role="button"
                        tabIndex={0}
                        aria-label={projectByOwnerServer && node.owner && node.server
                          ? `Inspect ${node.owner} served by ${node.server}, ${node.ruleIds.length} rules`
                          : `Inspect ${rule.id}${source ? ` served by ${source.server}` : ""}`}
                        onClick={() => selectRule(node.ruleIds[0])}
                        onKeyDown={(event: KeyboardEvent<SVGGElement>) => { if (event.key === "Enter" || event.key === " ") selectRule(node.ruleIds[0]); }}
                      >
                        <rect width={nodeWidth} height={nodeHeight} rx="10" />
                        <text x="15" y="25" className="node-id">{projectByOwnerServer ? "OWNER + NAME SERVER" : rule.id.toUpperCase()}</text>
                        {isCycle && <text x={nodeWidth - 16} y="25" textAnchor="end" className="cycle-label">CYCLE</text>}
                        <text x="15" y="48" className="node-source">{truncate(projectByOwnerServer ? node.owner ?? "synthetic" : sourceLines.record, 47)}</text>
                        {(projectByOwnerServer ? node.server : sourceLines.server) && <text x="15" y="64" className="node-server">{truncate(projectByOwnerServer ? `served by ${node.server}` : sourceLines.server, 55)}</text>}
                        <text x="15" y="84" className="node-head">{truncate(projectByOwnerServer ? `${node.ruleIds.length} compiled rule${node.ruleIds.length === 1 ? "" : "s"}` : rule.head, 54)}</text>
                        {!projectByOwnerServer && <text x={nodeWidth - 15} y="99" textAnchor="end" className="node-body-count">{rule.body.length} body</text>}
                      </g>
                    );
                  })}
                </g>
              </svg>
            ) : <div className="graph-empty">This visualization contains no compiled rules.</div>}
          </div>
        </section>

        <aside className="inspector-panel" hidden={!showInspectorPanel}>
          <div className="panel-heading inspector-heading">
            <div><p className="eyebrow">INSPECTOR</p><h2>{selectedRule?.id.toUpperCase() ?? "Select a rule"}</h2></div>
            <div className="inspector-heading-actions">
              {selectedSourceRuleIds.length > 1 && (
                <div className="rule-navigation" aria-label="Selected RRSet rule navigation">
                  <button
                    type="button"
                    aria-label="Previous rule from selected RRSet"
                    onClick={() => setSelectedRuleId(moveRuleSelection(selectedSourceRuleIds, activeRuleId, -1))}
                  >←</button>
                  <span>{activeSourceRuleIndex + 1} / {selectedSourceRuleIds.length}</span>
                  <button
                    type="button"
                    aria-label="Next rule from selected RRSet"
                    onClick={() => setSelectedRuleId(moveRuleSelection(selectedSourceRuleIds, activeRuleId, 1))}
                  >→</button>
                </div>
              )}
              {selectedRule && <span className={`cycle-chip ${cycleNodes.has(selectedRule.id) ? "visible" : ""}`}>cycle</span>}
            </div>
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
                      <button key={`${otherId}-${index}`} className="match-card" onClick={() => selectRule(otherId)}>
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

function makeIndexes(data: Dataset | null) {
  return {
    rules: new Map((data?.rules ?? []).map((rule) => [rule.id, rule])),
    sources: new Map((data?.sources ?? []).map((source) => [source.id, source])),
  };
}

function buildGraph(data: Dataset | null, projectByOwnerServer: boolean) {
  if (!data) return emptyGraph();
  const projection = projectByOwnerServer
    ? projectOwnerServerGraph(data.sources, data.rules, data.matches)
    : {
        nodes: data.rules.map((rule): GraphNode => ({ id: rule.id, owner: null, server: null, ruleIds: [rule.id] })),
        edges: data.matches.map((match) => ({
          from: match.dependentRuleId,
          to: match.dependencyRuleId,
          matches: [match],
        })),
      };
  const orderedIds = projection.nodes.map((node) => node.id);
  const layoutEdges = projection.edges.map(({ from, to }) => ({ from, to }));
  const layered = layoutGraphLayers(
    orderedIds,
    layoutEdges,
    { top: 24, nodeHeight, minimumGap: rowGap },
  );
  const placements: Placement[] = layered.map(({ id, layer, y }) => (
    { id, y, x: 64 + layer * graphColumnSpacing }
  ));
  const byId = new Map(placements.map((placement) => [placement.id, placement]));
  const edges = projection.edges.map((edge) => {
    const endpoints = bodyToHeadEdge({ dependentRuleId: edge.from, dependencyRuleId: edge.to }, byId);
    return endpoints ? { matches: edge.matches, ...endpoints } : null;
  }).filter((edge): edge is GraphEdge => edge !== null);
  return {
    nodes: projection.nodes,
    nodesById: new Map(projection.nodes.map((node) => [node.id, node])),
    placements,
    edges,
    width: 64 * 2 + nodeWidth + Math.max(0, ...layered.map(({ layer }) => layer)) * graphColumnSpacing,
    height: Math.max(300, ...placements.map((placement) => placement.y + nodeHeight + 24)),
  };
}

function emptyGraph() {
  return {
    nodes: [] as GraphNode[],
    nodesById: new Map<string, GraphNode>(),
    placements: [] as Placement[],
    edges: [] as GraphEdge[],
    width: 64 * 2 + nodeWidth,
    height: 300,
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
  return findGraphCycles(
    data.rules.map((rule) => rule.id),
    data.matches.map((match) => ({ from: match.dependencyRuleId, to: match.dependentRuleId })),
  );
}

function findGraphCycles(nodeIds: readonly string[], edges: readonly { from: string; to: string }[]) {
  const adjacency = new Map(nodeIds.map((id) => [id, [] as string[]]));
  edges.forEach((edge) => adjacency.get(edge.from)?.push(edge.to));
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
  nodeIds.forEach((id) => { if (!indexes.has(id)) visit(id); });
  return components;
}

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
