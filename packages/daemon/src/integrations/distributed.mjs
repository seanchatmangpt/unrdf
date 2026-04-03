/**
 * @file Distributed Task Distribution Integration
 * @module @unrdf/daemon/integrations/distributed
 * @description Raft-based leadership coordination and distributed work distribution
 * across cluster nodes with operation scoping and health monitoring
 */

import { z } from 'zod';
import {
  detectInjection,
  validatePayload,
} from '../security-audit.mjs';

/**
 * Operation with cluster scope configuration
 */
const DistributedOperationSchema = z.object({
  id: z.string().min(1),
  type: z.string().min(1),
  clusterScope: z.enum(['local', 'leader', 'global']).default('local'),
  payload: z.record(z.any()),
  timestamp: z.number().default(() => Date.now()),
});

/**
 * Integrate Raft node leadership tracking with daemon
 * Listens to Raft leader election events and updates daemon leadership state.
 * Only executes 'leader' scoped operations on the elected leader.
 *
 * @param {Object} daemon - UnrdfDaemon instance
 * @param {Object} raftNode - RaftCoordinator instance with leader events
 * @throws {TypeError} If daemon or raftNode are invalid
 * @example
 * integrateRaftNode(daemon, raftCoordinator);
 * // Now daemon.isLeader reflects Raft leader state
 */
export function integrateRaftNode(daemon, raftNode) {
  if (!daemon || typeof daemon.emit !== 'function') {
    throw new TypeError('integrateRaftNode: daemon must be EventEmitter-like');
  }
  if (!raftNode || typeof raftNode.on !== 'function') {
    throw new TypeError('integrateRaftNode: raftNode must be EventEmitter-like');
  }

  daemon.isLeader = false;
  daemon.currentLeaderId = null;

  raftNode.on('leader_elected', (event) => {
    const { leaderId } = event || {};
    daemon.isLeader = leaderId === daemon.id;
    daemon.currentLeaderId = leaderId;
    daemon.emit('leadership_changed', { isLeader: daemon.isLeader, leaderId });
  });

  raftNode.on('leader_lost', () => {
    daemon.isLeader = false;
    daemon.currentLeaderId = null;
    daemon.emit('leadership_lost');
  });

  daemon.raftNode = raftNode;
}

/**
 * Distribute operations across cluster nodes based on scope and strategy
 * Routes 'local' operations to current node, 'leader' to leader, 'global' via strategy.
 *
 * @param {Object} daemon - Daemon instance with isLeader flag
 * @param {Object} membershipManager - ClusterManager instance with node list
 * @param {Array} operations - Array of DistributedOperation objects
 * @param {string} [strategy='round-robin'] - Strategy: 'round-robin', 'least-loaded', 'hash'
 * @returns {Map<string, Array>} Map of nodeId to operations array
 * @throws {Error} If validation or distribution fails
 * @example
 * const distribution = distributeWork(daemon, clusterManager, ops, 'round-robin');
 * distribution.forEach((ops, nodeId) => {
 *   sendOperationsToNode(nodeId, ops);
 * });
 */
export function distributeWork(daemon, membershipManager, operations, strategy = 'round-robin') {
  if (!daemon) {
    throw new TypeError('distributeWork: daemon required');
  }
  if (!membershipManager || typeof membershipManager.getHealthyNodes !== 'function') {
    throw new TypeError('distributeWork: membershipManager must have getHealthyNodes method');
  }
  if (!Array.isArray(operations)) {
    throw new TypeError('distributeWork: operations must be an array');
  }

  // Security: Validate strategy for injection
  const strategyInjection = detectInjection(strategy, 'command');
  if (strategyInjection.detected) {
    throw new Error(`Security violation: Invalid strategy - ${strategyInjection.reason}`);
  }

  const distribution = new Map();
  const healthyNodes = membershipManager.getHealthyNodes();

  if (healthyNodes.length === 0) {
    throw new Error('distributeWork: no healthy nodes available');
  }

  let roundRobinIndex = 0;

  for (const op of operations) {
    // Security: Validate operation payload
    const payloadValidation = validatePayload(op, { type: 'rdf' });
    if (!payloadValidation.valid) {
      throw new Error(`Security validation failed for operation: ${payloadValidation.reason}`);
    }

    const validated = DistributedOperationSchema.parse(op);

    let targetNodeId;

    if (validated.clusterScope === 'local') {
      targetNodeId = daemon.id;
    } else if (validated.clusterScope === 'leader') {
      targetNodeId = daemon.currentLeaderId || healthyNodes[0];
    } else if (validated.clusterScope === 'global') {
      if (strategy === 'round-robin') {
        targetNodeId = healthyNodes[roundRobinIndex % healthyNodes.length];
        roundRobinIndex++;
      } else if (strategy === 'hash') {
        const hash = hashOperation(validated.id);
        targetNodeId = healthyNodes[hash % healthyNodes.length];
      } else if (strategy === 'least-loaded') {
        targetNodeId = selectLeastLoadedNode(healthyNodes, distribution);
      } else {
        targetNodeId = healthyNodes[0];
      }
    }

    if (!distribution.has(targetNodeId)) {
      distribution.set(targetNodeId, []);
    }
    distribution.get(targetNodeId).push(validated);
  }

  return distribution;
}

/**
 * Hash operation ID for consistent distribution
 *
 * @param {string} operationId - Operation identifier
 * @returns {number} Hash value
 * @private
 */
function hashOperation(operationId) {
  let hash = 0;
  for (let i = 0; i < operationId.length; i++) {
    hash = ((hash << 5) - hash) + operationId.charCodeAt(i);
    hash = hash & hash;
  }
  return Math.abs(hash);
}

/**
 * Select least loaded node from healthy nodes
 *
 * @param {Array<string>} healthyNodes - Array of healthy node IDs
 * @param {Map<string, Array>} distribution - Current distribution map
 * @returns {string} Selected node ID
 * @private
 */
function selectLeastLoadedNode(healthyNodes, distribution) {
  let minLoad = Infinity;
  let selectedNode = healthyNodes[0];

  for (const nodeId of healthyNodes) {
    const load = distribution.get(nodeId)?.length || 0;
    if (load < minLoad) {
      minLoad = load;
      selectedNode = nodeId;
    }
  }

  return selectedNode;
}
