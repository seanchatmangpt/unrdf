import { z } from 'zod';

/**
 * Community Detection & Clustering
 *
 * Identifies communities and clusters in RDF knowledge graphs
 * ALGORITHMS: Label propagation, modularity-based clustering
 */

const ClusteringOptionsSchema = z.object({
  maxIterations: z.number().int().positive().default(100),
  minCommunitySize: z.number().int().positive().default(2),
  seed: z.number().int().optional(),
}).passthrough();

/**
 * Label Propagation Algorithm for community detection
 *
 * @param {Graph} graph - Graphlib graph instance
 * @param {Object} options - Clustering options
 * @returns {Map<string, number>} Node to community ID mapping
 */
export function detectCommunitiesLPA(graph, options = {}) {
  const opts = ClusteringOptionsSchema.parse(options);

  const nodes = graph.nodes();
  const labels = new Map();

  // Initialize: each node gets unique label
  for (let i = 0; i < nodes.length; i++) {
    labels.set(nodes[i], i);
  }

  // Iterate until convergence
  let changed = true;
  let iteration = 0;

  while (changed && iteration < opts.maxIterations) {
    changed = false;

    // Process nodes in random order
    const shuffledNodes = [...nodes].sort(() => Math.random() - 0.5);

    for (const node of shuffledNodes) {
      // Get neighbor labels
      const neighborLabels = new Map();

      const inEdges = graph.inEdges(node) || [];
      const outEdges = graph.outEdges(node) || [];

      for (const edge of [...inEdges, ...outEdges]) {
        const neighbor = edge.v === node ? edge.w : edge.v;
        const label = labels.get(neighbor);

        if (label !== undefined) {
          neighborLabels.set(label, (neighborLabels.get(label) || 0) + 1);
        }
      }

      if (neighborLabels.size === 0) continue;

      // Find most frequent label
      let maxCount = 0;
      let maxLabel = labels.get(node);

      for (const [label, count] of neighborLabels.entries()) {
        if (count > maxCount || (count === maxCount && Math.random() > 0.5)) {
          maxCount = count;
          maxLabel = label;
        }
      }

      // Update label if changed
      if (labels.get(node) !== maxLabel) {
        labels.set(node, maxLabel);
        changed = true;
      }
    }

    iteration++;
  }

  return labels;
}

/**
 * Simple greedy modularity-based clustering
 *
 * @param {Graph} graph - Graphlib graph instance
 * @param {Object} options - Clustering options
 * @returns {Map<string, number>} Node to community ID mapping
 */
export function detectCommunitiesModularity(graph, options = {}) {
  const opts = ClusteringOptionsSchema.parse(options);

  const nodes = graph.nodes();
  const edges = graph.edges();
  const m = edges.length;

  if (m === 0) {
    // Each node is its own community
    const communities = new Map();
    nodes.forEach((node, idx) => communities.set(node, idx));
    return communities;
  }

  // Initialize: each node in its own community
  const communities = new Map();
  nodes.forEach((node, idx) => communities.set(node, idx));

  // Calculate degrees
  const degrees = new Map();
  for (const node of nodes) {
    const degree = (graph.nodeEdges(node) || []).length;
    degrees.set(node, degree);
  }

  // Greedy optimization
  let improved = true;
  let iteration = 0;

  while (improved && iteration < opts.maxIterations) {
    improved = false;

    for (const node of nodes) {
      const currentCommunity = communities.get(node);
      const nodeDegree = degrees.get(node);

      // Calculate modularity gain for each neighbor community
      const neighborCommunities = new Map();

      const nodeEdges = graph.nodeEdges(node) || [];
      for (const edge of nodeEdges) {
        const neighbor = edge.v === node ? edge.w : edge.v;
        const neighborCommunity = communities.get(neighbor);

        if (!neighborCommunities.has(neighborCommunity)) {
          neighborCommunities.set(neighborCommunity, 0);
        }
        neighborCommunities.set(
          neighborCommunity,
          neighborCommunities.get(neighborCommunity) + 1
        );
      }

      // Find best community
      let bestCommunity = currentCommunity;
      let bestGain = 0;

      for (const [community, edgeCount] of neighborCommunities.entries()) {
        if (community === currentCommunity) continue;

        // Simplified modularity gain
        const gain = edgeCount / m;

        if (gain > bestGain) {
          bestGain = gain;
          bestCommunity = community;
        }
      }

      // Move node if beneficial
      if (bestCommunity !== currentCommunity && bestGain > 0) {
        communities.set(node, bestCommunity);
        improved = true;
      }
    }

    iteration++;
  }

  // Renumber communities to be sequential
  return renumberCommunities(communities);
}

/**
 * K-core decomposition
 *
 * @param {Graph} graph - Graphlib graph instance
 * @param {number} k - K value for core
 * @returns {Set<string>} Nodes in k-core
 */
export function findKCore(graph, k = 2) {
  const nodes = new Set(graph.nodes());
  const degrees = new Map();

  // Initialize degrees
  for (const node of nodes) {
    const degree = (graph.nodeEdges(node) || []).length;
    degrees.set(node, degree);
  }

  // Iteratively remove nodes with degree < k
  let changed = true;
  while (changed) {
    changed = false;

    for (const node of nodes) {
      if (degrees.get(node) < k) {
        nodes.delete(node);

        // Update neighbor degrees
        const edges = graph.nodeEdges(node) || [];
        for (const edge of edges) {
          const neighbor = edge.v === node ? edge.w : edge.v;
          if (nodes.has(neighbor)) {
            degrees.set(neighbor, degrees.get(neighbor) - 1);
          }
        }

        changed = true;
      }
    }
  }

  return nodes;
}

/**
 * Get community statistics
 *
 * @param {Map<string, number>} communities - Community assignments
 * @returns {Object} Community statistics
 */
export function getCommunityStats(communities) {
  const communityGroups = new Map();

  for (const [node, communityId] of communities.entries()) {
    if (!communityGroups.has(communityId)) {
      communityGroups.set(communityId, []);
    }
    communityGroups.get(communityId).push(node);
  }

  const sizes = Array.from(communityGroups.values()).map(c => c.length);

  return {
    totalCommunities: communityGroups.size,
    communities: communityGroups,
    sizes,
    averageSize: sizes.reduce((a, b) => a + b, 0) / sizes.length,
    maxSize: Math.max(...sizes),
    minSize: Math.min(...sizes),
  };
}

/**
 * Helper: Renumber communities to be sequential
 */
function renumberCommunities(communities) {
  const uniqueCommunities = new Set(communities.values());
  const mapping = new Map();
  let nextId = 0;

  for (const oldId of uniqueCommunities) {
    mapping.set(oldId, nextId++);
  }

  const renumbered = new Map();
  for (const [node, oldId] of communities.entries()) {
    renumbered.set(node, mapping.get(oldId));
  }

  return renumbered;
}

export default {
  detectCommunitiesLPA,
  detectCommunitiesModularity,
  findKCore,
  getCommunityStats,
};
