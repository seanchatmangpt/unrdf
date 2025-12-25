import graphlibPkg from '@dagrejs/graphlib';
const { alg } = graphlibPkg;
import { z } from 'zod';

/**
 * Relationship Path Finder
 *
 * Discovers relationships between entities through graph path analysis
 * CORE ALGORITHM: Shortest paths, all paths, relationship chains
 */

const PathOptionsSchema = z.object({
  maxDepth: z.number().int().positive().default(5),
  maxPaths: z.number().int().positive().default(100),
  weightFunction: z.function().optional(),
}).passthrough();

/**
 * Find shortest path between two nodes
 *
 * @param {Graph} graph - Graphlib graph instance
 * @param {string} source - Source node ID
 * @param {string} target - Target node ID
 * @param {Object} options - Path options
 * @returns {Object|null} Path result with nodes and edges
 */
export function findShortestPath(graph, source, target, options = {}) {
  const opts = PathOptionsSchema.parse(options);

  if (!graph.hasNode(source) || !graph.hasNode(target)) {
    return null;
  }

  // Use Dijkstra's algorithm
  const weightFn = opts.weightFunction || ((edge) => {
    const edgeData = graph.edge(edge);
    return edgeData?.weight || 1;
  });

  try {
    const dijkstraResult = alg.dijkstra(graph, source, weightFn);

    if (!dijkstraResult[target] || dijkstraResult[target].distance === Infinity) {
      return null;
    }

    // Reconstruct path
    const path = [];
    let current = target;

    while (current !== source) {
      path.unshift(current);
      current = dijkstraResult[current].predecessor;
      if (!current) return null;
    }
    path.unshift(source);

    // Get edges along path
    const edges = [];
    for (let i = 0; i < path.length - 1; i++) {
      const edge = graph.edge(path[i], path[i + 1]);
      edges.push({
        from: path[i],
        to: path[i + 1],
        predicate: edge?.predicate,
        weight: edge?.weight || 1,
      });
    }

    return {
      path,
      edges,
      length: path.length - 1,
      totalWeight: dijkstraResult[target].distance,
    };
  } catch (error) {
    return null;
  }
}

/**
 * Find all paths between two nodes (up to max depth)
 *
 * @param {Graph} graph - Graphlib graph instance
 * @param {string} source - Source node ID
 * @param {string} target - Target node ID
 * @param {Object} options - Path options
 * @returns {Array<Object>} All paths found
 */
export function findAllPaths(graph, source, target, options = {}) {
  const opts = PathOptionsSchema.parse(options);

  if (!graph.hasNode(source) || !graph.hasNode(target)) {
    return [];
  }

  const allPaths = [];
  const visited = new Set();

  /**
   * DFS to find all paths
   */
  function dfs(current, currentPath, depth) {
    if (depth > opts.maxDepth || allPaths.length >= opts.maxPaths) {
      return;
    }

    if (current === target) {
      allPaths.push([...currentPath]);
      return;
    }

    visited.add(current);

    const outEdges = graph.outEdges(current) || [];
    for (const edge of outEdges) {
      const neighbor = edge.w;

      if (!visited.has(neighbor)) {
        currentPath.push(neighbor);
        dfs(neighbor, currentPath, depth + 1);
        currentPath.pop();
      }
    }

    visited.delete(current);
  }

  dfs(source, [source], 0);

  // Convert paths to detailed format
  return allPaths.map(path => {
    const edges = [];
    for (let i = 0; i < path.length - 1; i++) {
      const edge = graph.edge(path[i], path[i + 1]);
      edges.push({
        from: path[i],
        to: path[i + 1],
        predicate: edge?.predicate,
      });
    }

    return {
      path,
      edges,
      length: path.length - 1,
    };
  });
}

/**
 * Find common neighbors between two nodes
 *
 * @param {Graph} graph - Graphlib graph instance
 * @param {string} node1 - First node ID
 * @param {string} node2 - Second node ID
 * @returns {Object} Common neighbors analysis
 */
export function findCommonNeighbors(graph, node1, node2) {
  if (!graph.hasNode(node1) || !graph.hasNode(node2)) {
    return {
      commonOutNeighbors: [],
      commonInNeighbors: [],
      commonNeighbors: [],
    };
  }

  // Get neighbors
  const outNeighbors1 = new Set(
    (graph.outEdges(node1) || []).map(e => e.w)
  );
  const outNeighbors2 = new Set(
    (graph.outEdges(node2) || []).map(e => e.w)
  );

  const inNeighbors1 = new Set(
    (graph.inEdges(node1) || []).map(e => e.v)
  );
  const inNeighbors2 = new Set(
    (graph.inEdges(node2) || []).map(e => e.v)
  );

  // Find intersections
  const commonOutNeighbors = Array.from(outNeighbors1)
    .filter(n => outNeighbors2.has(n));

  const commonInNeighbors = Array.from(inNeighbors1)
    .filter(n => inNeighbors2.has(n));

  const allNeighbors1 = new Set([...outNeighbors1, ...inNeighbors1]);
  const allNeighbors2 = new Set([...outNeighbors2, ...inNeighbors2]);

  const commonNeighbors = Array.from(allNeighbors1)
    .filter(n => allNeighbors2.has(n));

  return {
    commonOutNeighbors,
    commonInNeighbors,
    commonNeighbors,
    jaccardSimilarity: commonNeighbors.length /
      (allNeighbors1.size + allNeighbors2.size - commonNeighbors.length),
  };
}

/**
 * Find nodes within K hops
 *
 * @param {Graph} graph - Graphlib graph instance
 * @param {string} source - Source node ID
 * @param {number} k - Number of hops
 * @returns {Map<string, number>} Node to distance mapping
 */
export function findKHopNeighbors(graph, source, k = 2) {
  if (!graph.hasNode(source)) {
    return new Map();
  }

  const distances = new Map();
  distances.set(source, 0);

  const queue = [source];

  while (queue.length > 0) {
    const current = queue.shift();
    const currentDist = distances.get(current);

    if (currentDist >= k) continue;

    const outEdges = graph.outEdges(current) || [];
    for (const edge of outEdges) {
      const neighbor = edge.w;

      if (!distances.has(neighbor)) {
        distances.set(neighbor, currentDist + 1);
        queue.push(neighbor);
      }
    }
  }

  return distances;
}

/**
 * Discover relationship chains (paths with predicate sequences)
 *
 * @param {Graph} graph - Graphlib graph instance
 * @param {string} source - Source node ID
 * @param {string} target - Target node ID
 * @param {Object} options - Options
 * @returns {Array<Object>} Relationship chains
 */
export function discoverRelationshipChains(graph, source, target, options = {}) {
  const paths = findAllPaths(graph, source, target, options);

  return paths.map(pathResult => {
    const predicateChain = pathResult.edges.map(e => e.predicate);
    const predicatePattern = predicateChain.join(' -> ');

    return {
      ...pathResult,
      predicateChain,
      predicatePattern,
    };
  });
}

export default {
  findShortestPath,
  findAllPaths,
  findCommonNeighbors,
  findKHopNeighbors,
  discoverRelationshipChains,
};
