import { z } from 'zod';

/**
 * PageRank Centrality Analyzer
 *
 * Computes PageRank and other centrality metrics for RDF knowledge graphs
 * CORE ALGORITHM: Entity importance scoring
 */

const PageRankOptionsSchema = z.object({
  dampingFactor: z.number().min(0).max(1).default(0.85),
  maxIterations: z.number().int().positive().default(100),
  tolerance: z.number().positive().default(1e-6),
  initialValue: z.number().positive().optional(),
}).passthrough();

/**
 * Compute PageRank scores for all nodes in graph
 *
 * @param {Graph} graph - Graphlib graph instance
 * @param {Object} options - PageRank options
 * @returns {Map<string, number>} Node ID to PageRank score mapping
 */
export function computePageRank(graph, options = {}) {
  const opts = PageRankOptionsSchema.parse(options);

  const nodes = graph.nodes();
  const nodeCount = nodes.length;

  if (nodeCount === 0) {
    return new Map();
  }

  const dampingFactor = opts.dampingFactor;
  const initialValue = opts.initialValue || (1.0 / nodeCount);

  // Initialize scores
  const scores = new Map();
  const newScores = new Map();

  for (const node of nodes) {
    scores.set(node, initialValue);
    newScores.set(node, 0);
  }

  // Compute out-degree for each node
  const outDegrees = new Map();
  for (const node of nodes) {
    const outEdges = graph.outEdges(node) || [];
    outDegrees.set(node, outEdges.length);
  }

  // Power iteration
  let iteration = 0;
  let delta = Infinity;

  while (iteration < opts.maxIterations && delta > opts.tolerance) {
    // Reset new scores
    for (const node of nodes) {
      newScores.set(node, (1 - dampingFactor) / nodeCount);
    }

    // Distribute scores
    for (const node of nodes) {
      const currentScore = scores.get(node);
      const outDegree = outDegrees.get(node);

      if (outDegree === 0) {
        // Distribute to all nodes (random teleportation)
        const contribution = (dampingFactor * currentScore) / nodeCount;
        for (const targetNode of nodes) {
          newScores.set(targetNode, newScores.get(targetNode) + contribution);
        }
      } else {
        // Distribute to successors
        const outEdges = graph.outEdges(node) || [];
        const contribution = (dampingFactor * currentScore) / outDegree;

        for (const edge of outEdges) {
          const target = edge.w;
          newScores.set(target, newScores.get(target) + contribution);
        }
      }
    }

    // Calculate convergence delta
    delta = 0;
    for (const node of nodes) {
      const diff = Math.abs(newScores.get(node) - scores.get(node));
      delta = Math.max(delta, diff);
    }

    // Swap scores
    for (const node of nodes) {
      scores.set(node, newScores.get(node));
    }

    iteration++;
  }

  return scores;
}

/**
 * Compute degree centrality (in/out/total)
 *
 * @param {Graph} graph - Graphlib graph instance
 * @returns {Object} Centrality metrics
 */
export function computeDegreeCentrality(graph) {
  const nodes = graph.nodes();
  const nodeCount = nodes.length;

  const inDegree = new Map();
  const outDegree = new Map();
  const totalDegree = new Map();

  for (const node of nodes) {
    const inEdges = graph.inEdges(node) || [];
    const outEdges = graph.outEdges(node) || [];

    const inDeg = inEdges.length;
    const outDeg = outEdges.length;
    const totalDeg = inDeg + outDeg;

    // Normalized by max possible degree
    inDegree.set(node, nodeCount > 1 ? inDeg / (nodeCount - 1) : 0);
    outDegree.set(node, nodeCount > 1 ? outDeg / (nodeCount - 1) : 0);
    totalDegree.set(node, nodeCount > 1 ? totalDeg / (2 * (nodeCount - 1)) : 0);
  }

  return {
    inDegree,
    outDegree,
    totalDegree,
  };
}

/**
 * Compute betweenness centrality (simplified approximation)
 *
 * @param {Graph} graph - Graphlib graph instance
 * @param {Object} options - Options
 * @returns {Map<string, number>} Node betweenness scores
 */
export function computeBetweennessCentrality(graph, options = {}) {
  const nodes = graph.nodes();
  const betweenness = new Map();

  // Initialize
  for (const node of nodes) {
    betweenness.set(node, 0);
  }

  // For each pair of nodes, find shortest paths
  for (const source of nodes) {
    // BFS from source
    const distances = new Map();
    const predecessors = new Map();
    const queue = [source];

    distances.set(source, 0);
    predecessors.set(source, []);

    while (queue.length > 0) {
      const current = queue.shift();
      const currentDist = distances.get(current);

      const outEdges = graph.outEdges(current) || [];
      for (const edge of outEdges) {
        const neighbor = edge.w;

        if (!distances.has(neighbor)) {
          distances.set(neighbor, currentDist + 1);
          predecessors.set(neighbor, [current]);
          queue.push(neighbor);
        } else if (distances.get(neighbor) === currentDist + 1) {
          predecessors.get(neighbor).push(current);
        }
      }
    }

    // Accumulate betweenness from this source
    const dependency = new Map();
    for (const node of nodes) {
      dependency.set(node, 0);
    }

    // Process nodes in reverse order of distance
    const nodesByDistance = Array.from(distances.entries())
      .sort((a, b) => b[1] - a[1])
      .map(([node]) => node);

    for (const node of nodesByDistance) {
      if (node === source) continue;

      const preds = predecessors.get(node) || [];
      const nodeDep = 1 + dependency.get(node);

      for (const pred of preds) {
        dependency.set(pred, dependency.get(pred) + nodeDep / preds.length);
      }

      betweenness.set(node, betweenness.get(node) + dependency.get(node));
    }
  }

  // Normalize
  const nodeCount = nodes.length;
  const normalizer = (nodeCount - 1) * (nodeCount - 2);

  if (normalizer > 0) {
    for (const node of nodes) {
      betweenness.set(node, betweenness.get(node) / normalizer);
    }
  }

  return betweenness;
}

/**
 * Get top K nodes by centrality score
 *
 * @param {Map<string, number>} scores - Centrality scores
 * @param {number} k - Number of top nodes to return
 * @returns {Array<{node: string, score: number}>} Top nodes
 */
export function getTopNodes(scores, k = 10) {
  return Array.from(scores.entries())
    .map(([node, score]) => ({ node, score }))
    .sort((a, b) => b.score - a.score)
    .slice(0, k);
}

export default {
  computePageRank,
  computeDegreeCentrality,
  computeBetweennessCentrality,
  getTopNodes,
};
