type DependencyMatch = {
  dependentRuleId: string;
  dependencyRuleId: string;
};

type PredicateMatch = DependencyMatch & { predicate: string };

type NamedSource = {
  owner: string;
  server: string;
  recordType: string;
  records: string[];
  ruleIds: string[];
};

export type NameGroup<T extends NamedSource> = {
  key: string;
  owner: string;
  server: string;
  sources: T[];
  recordTypes: string[];
  records: string[];
  ruleIds: string[];
};

export type LayoutColumn = "dependency" | "compiled" | "dependent";
export type LayoutEdge = { from: string; to: string };
export type LayoutPlacement = { id: string; column: LayoutColumn; y: number };
type LayoutColumns = Record<LayoutColumn, string[]>;
type LayoutOptions = {
  top: number;
  maximumTopOffset: number;
  nodeHeight: number;
  minimumGap: number;
  ingressPortY: number;
  egressPortY: number;
};

export const visualColumnOrder: LayoutColumn[] = ["dependent", "compiled", "dependency"];

export function sourceGroupKey(owner: string, server: string) {
  return `${owner}\u0000${server}`;
}

export function groupSourcesByNameAndServer<T extends NamedSource>(sources: readonly T[]): NameGroup<T>[] {
  const grouped = new Map<string, T[]>();
  for (const source of sources) {
    const key = sourceGroupKey(source.owner, source.server);
    grouped.set(key, [...(grouped.get(key) ?? []), source]);
  }
  return [...grouped].map(([key, members]) => ({
    key,
    owner: members[0].owner,
    server: members[0].server,
    sources: members,
    recordTypes: unique(members.map((source) => source.recordType)),
    records: members.flatMap((source) => source.records),
    ruleIds: unique(members.flatMap((source) => source.ruleIds)),
  }));
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

export function minimizeArrowLayout(
  inputColumns: LayoutColumns,
  edges: readonly LayoutEdge[],
  options: LayoutOptions,
): LayoutPlacement[] {
  const columns: LayoutColumns = {
    dependency: [...inputColumns.dependency],
    compiled: [...inputColumns.compiled],
    dependent: [...inputColumns.dependent],
  };
  const spacing = options.nodeHeight + options.minimumGap;
  const maxCount = Math.max(1, ...Object.values(columns).map((ids) => ids.length));
  const positions = new Map<string, number>();

  for (const column of visualColumnOrder) {
    const topOffset = (maxCount - columns[column].length) * spacing / 2;
    columns[column].forEach((id, index) => positions.set(id, options.top + topOffset + index * spacing));
  }

  for (let iteration = 0; iteration < 16; iteration += 1) {
    const sweep = iteration % 2 === 0 ? visualColumnOrder : [...visualColumnOrder].reverse();
    for (const column of sweep) {
      const ranked = columns[column].map((id) => ({
        id,
        target: preferredTop(id, edges, positions, options),
        previous: positions.get(id) ?? options.top,
      })).sort((left, right) => left.target - right.target || left.previous - right.previous || left.id.localeCompare(right.id));
      const packed = packTargets(
        ranked.map(({ target }) => target),
        spacing,
        options.top,
        options.top + options.maximumTopOffset,
      );
      columns[column] = ranked.map(({ id }) => id);
      ranked.forEach(({ id }, index) => positions.set(id, packed[index]));
    }
  }

  const minimumY = Math.min(...positions.values());
  const topCorrection = Number.isFinite(minimumY) ? options.top - minimumY : 0;
  return visualColumnOrder.flatMap((column) => columns[column].map((id) => ({
    id,
    column,
    y: positions.get(id)! + topCorrection,
  })));
}

function preferredTop(
  id: string,
  edges: readonly LayoutEdge[],
  positions: ReadonlyMap<string, number>,
  options: LayoutOptions,
) {
  const suggestions: number[] = [];
  for (const edge of edges) {
    if (edge.from === edge.to) continue;
    if (edge.from === id && positions.has(edge.to)) {
      suggestions.push(positions.get(edge.to)! + options.ingressPortY - options.egressPortY);
    } else if (edge.to === id && positions.has(edge.from)) {
      suggestions.push(positions.get(edge.from)! + options.egressPortY - options.ingressPortY);
    }
  }
  if (!suggestions.length) return positions.get(id) ?? options.top;
  return suggestions.reduce((sum, value) => sum + value, 0) / suggestions.length;
}

function packTargets(targets: number[], spacing: number, top: number, maximumTop: number) {
  const blocks: { start: number; end: number; total: number; count: number }[] = [];
  targets.forEach((target, index) => {
    blocks.push({ start: index, end: index, total: target - index * spacing, count: 1 });
    while (blocks.length > 1) {
      const right = blocks[blocks.length - 1];
      const left = blocks[blocks.length - 2];
      if (left.total / left.count <= right.total / right.count) break;
      blocks.splice(-2, 2, {
        start: left.start,
        end: right.end,
        total: left.total + right.total,
        count: left.count + right.count,
      });
    }
  });
  const packed = new Array<number>(targets.length);
  for (const block of blocks) {
    const average = block.total / block.count;
    for (let index = block.start; index <= block.end; index += 1) packed[index] = average + index * spacing;
  }
  const shift = packed.length
    ? Math.max(top - packed[0], Math.min(0, maximumTop - packed[0]))
    : 0;
  return packed.map((value) => value + shift);
}

function unique<T>(values: T[]) {
  return [...new Set(values)];
}
