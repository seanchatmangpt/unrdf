/**
 * @file Federation Node Selection
 * @module @unrdf/daemon/integrations/federation-node-selection
 * @description Node selection strategies for federated query execution
 */

/**
 * Select nodes for federated query execution based on strategy and health
 * @param {Object} coordinator - Cluster coordinator with listPeers method
 * @param {string} coordinator.listPeers - Function returning array of peers
 * @param {string} strategy - Selection strategy: 'best-node', 'selective', or 'broadcast'
 * @param {Array<string>} [excludeNodes=[]] - Node IDs to exclude from selection
 * @param {Map<string, Object>} [nodeMetrics=new Map()] - Map of node metrics by ID
 * @returns {Array<string>} Selected node IDs
 * @throws {TypeError} If coordinator or strategy invalid
 * @example
 * const nodes = selectNodes(coordinator, 'best-node', [], metrics);
 */
export function selectNodes(coordinator, strategy, excludeNodes = [], nodeMetrics = new Map()) {
  const peers = coordinator.listPeers?.() || [];
  const availableNodes = peers
    .filter(p => !excludeNodes.includes(p.id) && p.status === 'healthy')
    .map(p => p.id);
  if (availableNodes.length === 0) {
    return peers.filter(p => !excludeNodes.includes(p.id)).map(p => p.id);
  }
  switch (strategy) {
    case 'best-node':
      return [selectBestNode(availableNodes, nodeMetrics)];
    case 'selective':
      return selectTopNodes(availableNodes, Math.max(1, Math.ceil(availableNodes.length * 0.5)), nodeMetrics);
    case 'broadcast':
    default:
      return availableNodes;
  }
}

/**
 * Select single best node based on success rate and latency
 * @param {Array<string>} nodeIds - Array of candidate node IDs
 * @param {Map<string, Object>} nodeMetrics - Node metrics map
 * @returns {string} Best performing node ID
 * @example
 * const best = selectBestNode(['node-1', 'node-2'], metrics);
 */
export function selectBestNode(nodeIds, nodeMetrics) {
  let bestNode = nodeIds[0];
  let bestScore = -Infinity;
  for (const nodeId of nodeIds) {
    const metrics = nodeMetrics.get(nodeId) || {
      queryCount: 0,
      successRate: 1.0,
      avgDuration: 0,
    };
    const score = metrics.successRate / (1 + metrics.avgDuration / 1000);
    if (score > bestScore) {
      bestScore = score;
      bestNode = nodeId;
    }
  }
  return bestNode;
}

/**
 * Select top N nodes ranked by performance score
 * @param {Array<string>} nodeIds - Array of candidate node IDs
 * @param {number} count - Number of nodes to select
 * @param {Map<string, Object>} nodeMetrics - Node metrics map
 * @returns {Array<string>} Top performing node IDs
 * @example
 * const top3 = selectTopNodes(['node-1', 'node-2', 'node-3'], 3, metrics);
 */
export function selectTopNodes(nodeIds, count, nodeMetrics) {
  const scored = nodeIds.map(nodeId => {
    const metrics = nodeMetrics.get(nodeId) || {
      queryCount: 0,
      successRate: 1.0,
      avgDuration: 0,
    };
    const score = metrics.successRate / (1 + metrics.avgDuration / 1000);
    return { nodeId, score };
  });
  return scored.sort((a, b) => b.score - a.score).slice(0, count).map(s => s.nodeId);
}

/**
 * Update node metrics based on query results
 * @param {Map<string, Object>} nodeMetrics - Node metrics map to update
 * @param {Array<Object>} peerResults - Array of peer result objects
 * @param {string} peerResults[].nodeId - Node ID
 * @param {boolean} peerResults[].success - Whether query succeeded
 * @param {number} peerResults[].duration - Query duration in ms
 * @returns {void}
 * @example
 * updateNodeMetrics(metrics, [{nodeId: 'node-1', success: true, duration: 50}]);
 */
export function updateNodeMetrics(nodeMetrics, peerResults) {
  for (const result of peerResults) {
    const nodeId = result.nodeId;
    const existing = nodeMetrics.get(nodeId) || {
      queryCount: 0,
      successCount: 0,
      totalDuration: 0,
      successRate: 1.0,
      avgDuration: 0,
    };
    const isSuccess = result.success === true;
    existing.queryCount += 1;
    if (isSuccess) {
      existing.successCount += 1;
    }
    existing.totalDuration += result.duration || 0;
    existing.successRate = existing.successCount / existing.queryCount;
    existing.avgDuration = existing.totalDuration / existing.queryCount;
    nodeMetrics.set(nodeId, existing);
  }
}
