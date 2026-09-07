type DependencyMatch = {
  dependentRuleId: string;
  dependencyRuleId: string;
};

type PredicateMatch = DependencyMatch & { predicate: string };

type NamedSource = {
  owner: string;
  server: string;
  recordType: string;
};

export type LayoutEdge = { from: string; to: string };
type GraphLayerOptions = {
  top: number;
  nodeHeight: number;
  minimumGap: number;
};

export function ruleNodeSourceLines(source: Pick<NamedSource, "owner" | "server" | "recordType"> | null) {
  return source
    ? { record: `${source.owner} ${source.recordType}`, server: `served by ${source.server}` }
    : { record: "synthetic", server: "" };
}

export function sourceHasCycle(source: { ruleIds: readonly string[] }, cycleRuleIds: ReadonlySet<string>) {
  return source.ruleIds.some((ruleId) => cycleRuleIds.has(ruleId));
}

export function ruleSelectionForSource(source: { ruleIds: readonly string[] } | null, activeRuleId: string) {
  return new Set(source ? source.ruleIds : activeRuleId ? [activeRuleId] : []);
}

export function moveRuleSelection(ruleIds: readonly string[], activeRuleId: string, offset: -1 | 1) {
  if (!ruleIds.length) return "";
  const currentIndex = ruleIds.indexOf(activeRuleId);
  const nextIndex = ((currentIndex < 0 ? 0 : currentIndex) + offset + ruleIds.length) % ruleIds.length;
  return ruleIds[nextIndex];
}

export function predicateFromOtherRule(
  match: PredicateMatch,
  selectedRuleId: string,
  rules: ReadonlyMap<string, { head: string }>,
) {
  if (match.dependentRuleId === selectedRuleId) {
    return rules.get(match.dependencyRuleId)?.head ?? match.predicate;
  }
  return match.predicate;
}

export function bodyToHeadEdge<T>(match: DependencyMatch, placements: ReadonlyMap<string, T>) {
  const from = placements.get(match.dependentRuleId);
  const to = placements.get(match.dependencyRuleId);
  return from && to ? { from, to } : null;
}

export function ruleEdgePorts(nodeHeight: number) {
  return { ingress: nodeHeight * 0.28, egress: nodeHeight * 0.72 };
}

export function layoutGraphLayers(
  ruleIds: readonly string[],
  edges: readonly LayoutEdge[],
  options: GraphLayerOptions,
) {
  const nodeSet = new Set(ruleIds);
  const adjacency = new Map(ruleIds.map((id) => [id, [] as string[]]));
  for (const edge of edges) {
    if (nodeSet.has(edge.from) && nodeSet.has(edge.to)) adjacency.get(edge.from)!.push(edge.to);
  }

  const components = stronglyConnectedComponents(ruleIds, adjacency);
  const componentByNode = new Map(components.flatMap((members, component) => (
    members.map((id) => [id, component] as const)
  )));
  const graphOrder = new Map(ruleIds.map((id, index) => [id, index]));
  const componentOrder = components.map((members) => Math.min(...members.map((id) => graphOrder.get(id)!)));
  const successors = components.map(() => new Set<number>());
  const predecessors = components.map(() => new Set<number>());
  for (const edge of edges) {
    const from = componentByNode.get(edge.from);
    const to = componentByNode.get(edge.to);
    if (from === undefined || to === undefined || from === to) continue;
    successors[from].add(to);
    predecessors[to].add(from);
  }

  const indegrees = predecessors.map((incoming) => incoming.size);
  const ready = components.map((_, component) => component)
    .filter((component) => indegrees[component] === 0)
    .sort((left, right) => componentOrder[left] - componentOrder[right]);
  const topological: number[] = [];
  while (ready.length) {
    const component = ready.shift()!;
    topological.push(component);
    for (const successor of successors[component]) {
      indegrees[successor] -= 1;
      if (indegrees[successor] === 0) {
        ready.push(successor);
        ready.sort((left, right) => componentOrder[left] - componentOrder[right]);
      }
    }
  }

  const layers = components.map(() => 0);
  for (const component of topological) {
    for (const successor of successors[component]) {
      layers[successor] = Math.max(layers[successor], layers[component] + 1);
    }
  }
  const lastLayer = Math.max(0, ...layers);
  components.forEach((_, component) => {
    const isLeaf = successors[component].size === 0;
    const isIsolated = isLeaf && predecessors[component].size === 0;
    if (isLeaf && !isIsolated) layers[component] = lastLayer;
  });

  const spacing = options.nodeHeight + options.minimumGap;
  const indexesWithinLayer = new Map<number, number>();
  return ruleIds.map((id) => {
    const layer = layers[componentByNode.get(id)!];
    const index = indexesWithinLayer.get(layer) ?? 0;
    indexesWithinLayer.set(layer, index + 1);
    return { id, layer, y: options.top + index * spacing };
  });
}

function stronglyConnectedComponents(ruleIds: readonly string[], adjacency: ReadonlyMap<string, readonly string[]>) {
  const indexes = new Map<string, number>();
  const lowLinks = new Map<string, number>();
  const stack: string[] = [];
  const onStack = new Set<string>();
  const components: string[][] = [];
  let nextIndex = 0;

  function visit(id: string) {
    indexes.set(id, nextIndex);
    lowLinks.set(id, nextIndex);
    nextIndex += 1;
    stack.push(id);
    onStack.add(id);
    for (const neighbor of adjacency.get(id) ?? []) {
      if (!indexes.has(neighbor)) {
        visit(neighbor);
        lowLinks.set(id, Math.min(lowLinks.get(id)!, lowLinks.get(neighbor)!));
      } else if (onStack.has(neighbor)) {
        lowLinks.set(id, Math.min(lowLinks.get(id)!, indexes.get(neighbor)!));
      }
    }
    if (lowLinks.get(id) !== indexes.get(id)) return;
    const component: string[] = [];
    let member: string;
    do {
      member = stack.pop()!;
      onStack.delete(member);
      component.push(member);
    } while (member !== id);
    components.push(component);
  }

  ruleIds.forEach((id) => { if (!indexes.has(id)) visit(id); });
  return components;
}
