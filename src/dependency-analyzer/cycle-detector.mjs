/**
 * @fileoverview Cycle Detection for Dependency Graphs
 *
 * Implements multiple algorithms for detecting circular dependencies in monorepo
 * package graphs. Cycles are a critical issue that can cause build failures,
 * infinite loops, and make the dependency graph non-DAG.
 *
 * @module dependency-analyzer/cycle-detector
 */

/**
 * @typedef {Object} Cycle
 * @property {string[]} path - The packages forming the cycle (first and last are the same)
 * @property {number} length - Number of unique packages in the cycle
 * @property {string} description - Human-readable description of the cycle
 */

/**
 * @typedef {Object} CycleDetectionResult
 * @property {boolean} hasCycles - Whether any cycles were detected
 * @property {Cycle[]} cycles - All detected cycles
 * @property {string[]} involvedPackages - Unique list of packages involved in cycles
 * @property {BreakpointSuggestion[]} breakpointSuggestions - Suggested edges to break
 * @property {string} summary - Human-readable summary
 */

/**
 * @typedef {Object} BreakpointSuggestion
 * @property {string} from - Source package of the edge to break
 * @property {string} to - Target package of the edge to break
 * @property {number} score - Score indicating how good this breakpoint is (higher = better)
 * @property {string} reason - Explanation of why this is a good breakpoint
 */

/**
 * @typedef {Object} DependencyGraph
 * @property {Map<string, Object>} packages - Package information
 * @property {Map<string, Set<string>>} adjacencyList - Forward adjacency list
 * @property {Map<string, Set<string>>} reverseAdjacencyList - Reverse adjacency list
 */

/**
 * Node state during DFS traversal
 * @enum {number}
 */
const NodeState = {
  UNVISITED: 0,
  VISITING: 1,
  VISITED: 2
};

/**
 * Detect all cycles in the dependency graph using DFS with back-edge detection
 * This uses Tarjan's algorithm variant for finding strongly connected components
 *
 * @param {DependencyGraph} graph - The dependency graph
 * @returns {CycleDetectionResult} Cycle detection results
 */
export function detectCycles(graph) {
  const cycles = [];
  const state = new Map();
  const parent = new Map();
  const discoveryTime = new Map();
  const lowLink = new Map();
  const onStack = new Set();
  const stack = [];
  let time = 0;

  // Initialize all nodes
  for (const pkgName of graph.packages.keys()) {
    state.set(pkgName, NodeState.UNVISITED);
  }

  /**
   * DFS helper function to find cycles
   * @param {string} node - Current node
   * @param {string[]} path - Current path from root
   */
  function dfs(node, path) {
    state.set(node, NodeState.VISITING);
    const currentPath = [...path, node];

    const neighbors = graph.adjacencyList.get(node) || new Set();

    for (const neighbor of neighbors) {
      const neighborState = state.get(neighbor);

      if (neighborState === NodeState.VISITING) {
        // Found a cycle! Extract it
        const cycleStartIndex = currentPath.indexOf(neighbor);
        if (cycleStartIndex !== -1) {
          const cyclePath = [...currentPath.slice(cycleStartIndex), neighbor];
          cycles.push({
            path: cyclePath,
            length: cyclePath.length - 1, // -1 because first and last are same
            description: formatCycleDescription(cyclePath)
          });
        }
      } else if (neighborState === NodeState.UNVISITED) {
        parent.set(neighbor, node);
        dfs(neighbor, currentPath);
      }
    }

    state.set(node, NodeState.VISITED);
  }

  // Run DFS from each unvisited node
  for (const pkgName of graph.packages.keys()) {
    if (state.get(pkgName) === NodeState.UNVISITED) {
      dfs(pkgName, []);
    }
  }

  // Deduplicate cycles (same cycle can be detected from different starting points)
  const uniqueCycles = deduplicateCycles(cycles);

  // Find all packages involved in cycles
  const involvedPackages = new Set();
  for (const cycle of uniqueCycles) {
    for (let i = 0; i < cycle.path.length - 1; i++) {
      involvedPackages.add(cycle.path[i]);
    }
  }

  // Generate breakpoint suggestions
  const breakpointSuggestions = generateBreakpointSuggestions(graph, uniqueCycles);

  return {
    hasCycles: uniqueCycles.length > 0,
    cycles: uniqueCycles,
    involvedPackages: Array.from(involvedPackages).sort(),
    breakpointSuggestions,
    summary: generateCycleSummary(uniqueCycles, involvedPackages)
  };
}

/**
 * Format a cycle path as a human-readable description
 * @param {string[]} cyclePath - The cycle path
 * @returns {string} Formatted description
 */
function formatCycleDescription(cyclePath) {
  const shortNames = cyclePath.map(p => p.replace('@unrdf/', ''));
  return shortNames.join(' -> ');
}

/**
 * Remove duplicate cycles (same cycle starting from different nodes)
 * @param {Cycle[]} cycles - List of detected cycles
 * @returns {Cycle[]} Deduplicated cycles
 */
function deduplicateCycles(cycles) {
  if (cycles.length === 0) return [];

  const seen = new Set();
  const unique = [];

  for (const cycle of cycles) {
    // Normalize the cycle by rotating to start with the lexicographically smallest element
    const normalized = normalizeCycle(cycle.path);
    const key = normalized.join('|');

    if (!seen.has(key)) {
      seen.add(key);
      unique.push({
        ...cycle,
        path: normalized
      });
    }
  }

  // Sort by length (shortest first) then alphabetically
  return unique.sort((a, b) => {
    if (a.length !== b.length) return a.length - b.length;
    return a.description.localeCompare(b.description);
  });
}

/**
 * Normalize a cycle by rotating to start with the smallest element
 * @param {string[]} cyclePath - The cycle path (first and last are the same)
 * @returns {string[]} Normalized cycle path
 */
function normalizeCycle(cyclePath) {
  if (cyclePath.length <= 1) return cyclePath;

  // Remove the duplicate last element
  const path = cyclePath.slice(0, -1);

  // Find the index of the minimum element
  let minIndex = 0;
  for (let i = 1; i < path.length; i++) {
    if (path[i] < path[minIndex]) {
      minIndex = i;
    }
  }

  // Rotate to start with the minimum element
  const rotated = [...path.slice(minIndex), ...path.slice(0, minIndex)];

  // Add the first element again at the end
  return [...rotated, rotated[0]];
}

/**
 * Generate suggestions for which edges to break to eliminate cycles
 * @param {DependencyGraph} graph - The dependency graph
 * @param {Cycle[]} cycles - Detected cycles
 * @returns {BreakpointSuggestion[]} Sorted list of breakpoint suggestions
 */
function generateBreakpointSuggestions(graph, cycles) {
  if (cycles.length === 0) return [];

  // Count how many cycles each edge participates in
  const edgeParticipation = new Map();

  for (const cycle of cycles) {
    const path = cycle.path;
    for (let i = 0; i < path.length - 1; i++) {
      const from = path[i];
      const to = path[i + 1];
      const key = `${from}->${to}`;

      if (!edgeParticipation.has(key)) {
        edgeParticipation.set(key, {
          from,
          to,
          cycleCount: 0,
          fromDependents: (graph.reverseAdjacencyList.get(from) || new Set()).size,
          toDependencies: (graph.adjacencyList.get(to) || new Set()).size
        });
      }
      edgeParticipation.get(key).cycleCount++;
    }
  }

  // Score each edge as a breakpoint candidate
  const suggestions = [];

  for (const [key, info] of edgeParticipation) {
    // Higher score = better breakpoint
    // We prefer to break edges that:
    // 1. Participate in many cycles (breaks multiple cycles at once)
    // 2. Go from less-depended-on packages (fewer downstream effects)
    // 3. Go to packages with fewer dependencies (likely a utility package)

    const score =
      info.cycleCount * 10 +                    // Prefer edges in many cycles
      (10 - Math.min(10, info.fromDependents)) + // Prefer from packages with few dependents
      (10 - Math.min(10, info.toDependencies));  // Prefer to packages with few deps

    const reasons = [];
    if (info.cycleCount > 1) {
      reasons.push(`breaks ${info.cycleCount} cycles`);
    }
    if (info.fromDependents <= 2) {
      reasons.push(`${info.from.replace('@unrdf/', '')} has few dependents`);
    }
    if (info.toDependencies <= 2) {
      reasons.push(`${info.to.replace('@unrdf/', '')} is a leaf/near-leaf package`);
    }

    suggestions.push({
      from: info.from,
      to: info.to,
      score,
      reason: reasons.length > 0 ? reasons.join('; ') : 'Standard edge in cycle'
    });
  }

  // Sort by score (highest first)
  return suggestions.sort((a, b) => b.score - a.score);
}

/**
 * Generate a human-readable summary of cycle detection results
 * @param {Cycle[]} cycles - Detected cycles
 * @param {Set<string>} involvedPackages - Packages involved in cycles
 * @returns {string} Summary text
 */
function generateCycleSummary(cycles, involvedPackages) {
  if (cycles.length === 0) {
    return 'No circular dependencies detected. The dependency graph is a valid DAG.';
  }

  const lines = [];
  lines.push(`CRITICAL: ${cycles.length} circular dependenc${cycles.length === 1 ? 'y' : 'ies'} detected!`);
  lines.push(`${involvedPackages.size} packages are involved in cycles.`);
  lines.push('');
  lines.push('Cycles must be broken before the dependency graph can be considered valid.');
  lines.push('See breakpoint suggestions for recommended fixes.');

  return lines.join('\n');
}

/**
 * Find Strongly Connected Components using Tarjan's algorithm
 * This is useful for understanding the cycle structure
 *
 * @param {DependencyGraph} graph - The dependency graph
 * @returns {{ components: string[][], hasCycles: boolean }} SCCs and cycle status
 */
export function findStronglyConnectedComponents(graph) {
  const index = new Map();
  const lowlink = new Map();
  const onStack = new Set();
  const stack = [];
  const components = [];
  let currentIndex = 0;

  /**
   * Tarjan's algorithm DFS
   * @param {string} v - Current vertex
   */
  function strongConnect(v) {
    index.set(v, currentIndex);
    lowlink.set(v, currentIndex);
    currentIndex++;
    stack.push(v);
    onStack.add(v);

    const neighbors = graph.adjacencyList.get(v) || new Set();

    for (const w of neighbors) {
      if (!index.has(w)) {
        // Successor w has not been visited; recurse on it
        strongConnect(w);
        lowlink.set(v, Math.min(lowlink.get(v), lowlink.get(w)));
      } else if (onStack.has(w)) {
        // Successor w is on the stack and hence in the current SCC
        lowlink.set(v, Math.min(lowlink.get(v), index.get(w)));
      }
    }

    // If v is a root node, pop the stack and generate an SCC
    if (lowlink.get(v) === index.get(v)) {
      const component = [];
      let w;
      do {
        w = stack.pop();
        onStack.delete(w);
        component.push(w);
      } while (w !== v);

      if (component.length > 0) {
        components.push(component.sort());
      }
    }
  }

  // Run on all vertices
  for (const v of graph.packages.keys()) {
    if (!index.has(v)) {
      strongConnect(v);
    }
  }

  // Check if any SCC has more than one node (indicates a cycle)
  const hasCycles = components.some(c => c.length > 1);

  return { components, hasCycles };
}

/**
 * Generate a denial receipt for circular dependencies (GOS guard)
 * @param {CycleDetectionResult} result - Cycle detection result
 * @returns {Object} Denial receipt in GOS format
 */
export function generateCircularDependencyDenialReceipt(result) {
  if (!result.hasCycles) {
    return {
      type: 'APPROVAL',
      guard: 'H_circular_dependency',
      status: 'PASSED',
      message: 'No circular dependencies detected',
      timestamp: new Date().toISOString(),
      metrics: {
        cyclesFound: 0,
        packagesAnalyzed: 0
      }
    };
  }

  return {
    type: 'DENIAL',
    guard: 'H_circular_dependency',
    status: 'FAILED',
    message: `Circular dependencies detected: ${result.cycles.length} cycle(s)`,
    timestamp: new Date().toISOString(),
    metrics: {
      cyclesFound: result.cycles.length,
      involvedPackages: result.involvedPackages.length,
      suggestedBreakpoints: result.breakpointSuggestions.slice(0, 3).map(s => ({
        edge: `${s.from} -> ${s.to}`,
        score: s.score
      }))
    },
    cycles: result.cycles.map(c => ({
      path: c.description,
      length: c.length
    })),
    remediation: [
      'Review the suggested breakpoint edges',
      'Consider extracting shared code to a common utility package',
      'Use dependency inversion (interfaces) to break circular references',
      'Merge tightly coupled packages if they represent a single concern'
    ]
  };
}

/**
 * Format cycle detection results as a detailed report
 * @param {CycleDetectionResult} result - Cycle detection result
 * @returns {string} Formatted report
 */
export function formatCycleReport(result) {
  const lines = [];

  lines.push('='.repeat(80));
  lines.push('CIRCULAR DEPENDENCY ANALYSIS REPORT');
  lines.push('='.repeat(80));
  lines.push('');

  if (!result.hasCycles) {
    lines.push('STATUS: PASSED');
    lines.push('');
    lines.push('No circular dependencies detected.');
    lines.push('The dependency graph forms a valid Directed Acyclic Graph (DAG).');
    lines.push('');
    lines.push('='.repeat(80));
    return lines.join('\n');
  }

  lines.push('STATUS: FAILED - CRITICAL');
  lines.push('');
  lines.push(result.summary);
  lines.push('');

  lines.push('-'.repeat(80));
  lines.push('DETECTED CYCLES');
  lines.push('-'.repeat(80));

  for (let i = 0; i < result.cycles.length; i++) {
    const cycle = result.cycles[i];
    lines.push(`Cycle ${i + 1} (${cycle.length} packages):`);
    lines.push(`  ${cycle.description}`);
    lines.push('');
  }

  lines.push('-'.repeat(80));
  lines.push('INVOLVED PACKAGES');
  lines.push('-'.repeat(80));
  lines.push(result.involvedPackages.map(p => `  - ${p}`).join('\n'));
  lines.push('');

  lines.push('-'.repeat(80));
  lines.push('BREAKPOINT SUGGESTIONS (Highest Priority First)');
  lines.push('-'.repeat(80));

  for (let i = 0; i < Math.min(10, result.breakpointSuggestions.length); i++) {
    const suggestion = result.breakpointSuggestions[i];
    lines.push(`${i + 1}. Break edge: ${suggestion.from} -> ${suggestion.to}`);
    lines.push(`   Score: ${suggestion.score}`);
    lines.push(`   Reason: ${suggestion.reason}`);
    lines.push('');
  }

  lines.push('-'.repeat(80));
  lines.push('REMEDIATION STRATEGIES');
  lines.push('-'.repeat(80));
  lines.push('1. Extract shared utilities: Move shared code to a new leaf package');
  lines.push('2. Dependency inversion: Define interfaces in a base package');
  lines.push('3. Merge packages: Combine tightly coupled packages');
  lines.push('4. Lazy loading: Use dynamic imports to break compile-time cycles');
  lines.push('5. Event-based: Use event emitters instead of direct imports');
  lines.push('');

  lines.push('='.repeat(80));

  return lines.join('\n');
}

/**
 * Verify that a proposed fix would eliminate a cycle
 * @param {DependencyGraph} graph - The original dependency graph
 * @param {string} fromPkg - The source package of the edge to remove
 * @param {string} toPkg - The target package of the edge to remove
 * @returns {{ wouldFix: boolean, remainingCycles: number }} Fix verification result
 */
export function verifyBreakpointFix(graph, fromPkg, toPkg) {
  // Create a copy of the adjacency list without the specified edge
  const modifiedAdjList = new Map();
  for (const [pkg, deps] of graph.adjacencyList) {
    const newDeps = new Set(deps);
    if (pkg === fromPkg) {
      newDeps.delete(toPkg);
    }
    modifiedAdjList.set(pkg, newDeps);
  }

  // Create a modified graph for testing
  const modifiedGraph = {
    packages: graph.packages,
    adjacencyList: modifiedAdjList,
    reverseAdjacencyList: graph.reverseAdjacencyList
  };

  // Re-run cycle detection
  const result = detectCycles(modifiedGraph);

  return {
    wouldFix: !result.hasCycles,
    remainingCycles: result.cycles.length
  };
}

export default {
  detectCycles,
  findStronglyConnectedComponents,
  generateCircularDependencyDenialReceipt,
  formatCycleReport,
  verifyBreakpointFix
};
