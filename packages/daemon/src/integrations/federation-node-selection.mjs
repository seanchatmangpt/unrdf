/**
 * @file Federation Node Selection
 * @module @unrdf/daemon/integrations/federation-node-selection
 * @description Node selection strategies for federated query execution
 */

export function selectNodes(coordinator, strategy, excludeNodes = []) {
  const peers = coordinator.listPeers?.() || [];
  const availableNodes = peers
    .filter(p => !excludeNodes.includes(p.id) && p.status === 'healthy')
    .map(p => p.id);
  if (availableNodes.length === 0) {
    return peers.filter(p => !excludeNodes.includes(p.id)).map(p => p.id);
  }
  switch (strategy) {
    case 'best-node':
      return [selectBestNode(availableNodes, new Map())];
    case 'selective':
      return selectTopNodes(availableNodes, Math.max(1, Math.ceil(availableNodes.length * 0.5)), new Map());
    case 'broadcast':
    default:
      return availableNodes;
  }
}

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
