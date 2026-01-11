/**
 * @file Task Assignment and Distribution Management
 * @module @unrdf/daemon/integrations/task-distributor
 * @description Distributed task distributor class managing operation assignments,
 * node load tracking, and heartbeat health monitoring integration
 */

import { z } from 'zod';

/**
 * Task assignment record
 */
const TaskAssignmentSchema = z.object({
  operationId: z.string().min(1),
  nodeId: z.string().min(1),
  assignedAt: z.number().default(() => Date.now()),
  status: z.enum(['pending', 'executing', 'completed', 'failed']).default('pending'),
});

/**
 * Node health status for heartbeat integration
 */
const NodeHealthStatusSchema = z.object({
  nodeId: z.string(),
  healthy: z.boolean().default(true),
  lastHeartbeat: z.number().default(() => Date.now()),
  pendingOperations: z.number().default(0),
  completedOperations: z.number().default(0),
});

/**
 * Distributed Task Distributor
 * Manages task assignment across cluster members with load tracking,
 * status monitoring, and heartbeat health integration.
 *
 * @class DistributedTaskDistributor
 * @example
 * const distributor = new DistributedTaskDistributor({ membershipManager });
 * await distributor.distribute(operations, 'round-robin');
 * const assignments = distributor.getAssignments('node-1');
 */
export class DistributedTaskDistributor {
  /**
   * Create a task distributor
   * @param {Object} options - Configuration options
   * @param {Object} options.membershipManager - ClusterManager instance
   * @param {Object} [options.logger] - Logger instance
   * @param {number} [options.heartbeatInterval=5000] - Health check interval
   * @throws {TypeError} If membershipManager is invalid
   */
  constructor(options = {}) {
    if (!options.membershipManager) {
      throw new TypeError('DistributedTaskDistributor: membershipManager required');
    }
    this.membershipManager = options.membershipManager;
    this.logger = options.logger || console;
    this.heartbeatInterval = options.heartbeatInterval || 5000;
    this.assignments = new Map();
    this.nodeLoadMap = new Map();
    this.nodeHealthMap = new Map();
    this.heartbeatTimer = null;
  }

  /**
   * Distribute operations across nodes with specified strategy
   *
   * @param {Array} operations - Operations to distribute
   * @param {string} [strategy='round-robin'] - Distribution strategy
   * @returns {Map<string, Array>} Distribution map (nodeId -> operations[])
   * @throws {Error} If distribution fails validation
   */
  distribute(operations, strategy = 'round-robin') {
    if (!Array.isArray(operations)) {
      throw new TypeError('distribute: operations must be an array');
    }

    const healthyNodes = this.membershipManager.getHealthyNodes();

    if (healthyNodes.length === 0) {
      throw new Error('distribute: no healthy nodes available');
    }

    const distribution = new Map();
    let roundRobinIndex = 0;

    for (const op of operations) {
      // Validate structure before assignment
      TaskAssignmentSchema.parse({
        operationId: op.id,
        nodeId: '',
        status: 'pending',
      });

      let targetNodeId;

      if (strategy === 'round-robin') {
        targetNodeId = healthyNodes[roundRobinIndex % healthyNodes.length];
        roundRobinIndex++;
      } else if (strategy === 'least-loaded') {
        targetNodeId = this._selectLeastLoadedNode(healthyNodes, distribution);
      } else if (strategy === 'hash') {
        const hash = this._hashOperation(op.id);
        targetNodeId = healthyNodes[hash % healthyNodes.length];
      } else {
        targetNodeId = healthyNodes[0];
      }

      if (!distribution.has(targetNodeId)) {
        distribution.set(targetNodeId, []);
      }
      distribution.get(targetNodeId).push(op);
      this.assignToNode(op.id, targetNodeId);
    }

    return distribution;
  }

  /**
   * Assign operation to specific node
   *
   * @param {string} operationId - Operation identifier
   * @param {string} nodeId - Target node identifier
   * @throws {Error} If assignment validation fails
   */
  assignToNode(operationId, nodeId) {
    const assignment = TaskAssignmentSchema.parse({
      operationId,
      nodeId,
    });
    this.assignments.set(operationId, assignment);
    this.nodeLoadMap.set(nodeId, (this.nodeLoadMap.get(nodeId) || 0) + 1);

    const health = this.nodeHealthMap.get(nodeId);
    if (health) {
      health.pendingOperations += 1;
    }
  }

  /**
   * Get all assignments for a specific node
   *
   * @param {string} nodeId - Node identifier
   * @returns {Array<Object>} Array of assignment objects
   */
  getAssignments(nodeId) {
    return Array.from(this.assignments.values()).filter((a) => a.nodeId === nodeId);
  }

  /**
   * Update operation status
   *
   * @param {string} operationId - Operation identifier
   * @param {string} status - New status (pending, executing, completed, failed)
   * @throws {Error} If operation not found or status invalid
   */
  updateStatus(operationId, status) {
    const assignment = this.assignments.get(operationId);
    if (!assignment) {
      throw new Error(`updateStatus: operation ${operationId} not found`);
    }
    if (!['pending', 'executing', 'completed', 'failed'].includes(status)) {
      throw new Error(`updateStatus: invalid status ${status}`);
    }
    assignment.status = status;
    this.logger.log(`[TaskDistributor] Operation ${operationId} status: ${status}`);
  }

  /**
   * Start heartbeat health monitoring
   * Synchronizes daemon heartbeat with node health tracking
   *
   * @returns {void}
   */
  startHeartbeat() {
    if (this.heartbeatTimer) {
      return;
    }

    this.heartbeatTimer = setInterval(() => {
      const healthyNodes = this.membershipManager.getHealthyNodes();

      healthyNodes.forEach((nodeId) => {
        if (!this.nodeHealthMap.has(nodeId)) {
          this.nodeHealthMap.set(nodeId, NodeHealthStatusSchema.parse({
            nodeId,
          }));
        }
        const health = this.nodeHealthMap.get(nodeId);
        health.lastHeartbeat = Date.now();
        health.healthy = true;
      });
    }, this.heartbeatInterval);
  }

  /**
   * Stop heartbeat health monitoring
   *
   * @returns {void}
   */
  stopHeartbeat() {
    if (this.heartbeatTimer) {
      clearInterval(this.heartbeatTimer);
      this.heartbeatTimer = null;
    }
  }

  /**
   * Get node health status
   *
   * @param {string} nodeId - Node identifier
   * @returns {Object|null} Health status or null if not found
   */
  getNodeHealth(nodeId) {
    return this.nodeHealthMap.get(nodeId) || null;
  }

  /**
   * Hash operation ID for consistent distribution
   *
   * @param {string} operationId - Operation identifier
   * @returns {number} Hash value
   * @private
   */
  _hashOperation(operationId) {
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
  _selectLeastLoadedNode(healthyNodes, distribution) {
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
}
