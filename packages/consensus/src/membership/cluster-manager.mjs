/**
 * @fileoverview Cluster Manager for Dynamic Node Membership
 * @module consensus/membership/cluster-manager
 *
 * @description
 * Manages dynamic cluster membership with auto-discovery and health monitoring.
 * Provides add/remove nodes without downtime.
 *
 * Key features:
 * - Dynamic node addition/removal
 * - Auto-discovery of cluster nodes
 * - Health monitoring with heartbeat
 * - Failure detection and recovery
 * - Membership change coordination via Raft
 */

import { EventEmitter } from 'events';
import { randomUUID } from 'crypto';
import { z } from 'zod';
import { trace, SpanStatusCode, metrics } from '@opentelemetry/api';

const tracer = trace.getTracer('unrdf-consensus');
const meter = metrics.getMeter('unrdf-consensus');

/**
 * Node health status
 * @enum {string}
 */
export const NodeHealth = {
  HEALTHY: 'healthy',
  DEGRADED: 'degraded',
  UNHEALTHY: 'unhealthy',
  UNKNOWN: 'unknown',
};

/**
 * Node metadata schema
 */
const NodeMetadataSchema = z.object({
  nodeId: z.string(),
  host: z.string(),
  port: z.number().int().positive(),
  capabilities: z.array(z.string()).default([]),
  metadata: z.record(z.string(), z.unknown()).optional(),
  joinedAt: z.number().default(() => Date.now()),
});

/**
 * Cluster manager configuration schema
 */
const ClusterConfigSchema = z.object({
  nodeId: z.string(),
  healthCheckInterval: z.number().positive().default(5000),
  healthCheckTimeout: z.number().positive().default(2000),
  maxFailedHealthChecks: z.number().int().positive().default(3),
  autoDiscovery: z.boolean().default(false),
  discoveryInterval: z.number().positive().default(10000),
});

/**
 * Cluster Manager
 *
 * Manages cluster membership and health monitoring.
 * Coordinates node addition/removal via Raft consensus.
 *
 * @class ClusterManager
 * @extends EventEmitter
 *
 * @example
 * const manager = new ClusterManager({
 *   nodeId: 'node-1',
 *   healthCheckInterval: 5000,
 *   autoDiscovery: true
 * });
 *
 * await manager.initialize(raftCoordinator);
 *
 * // Add node to cluster
 * await manager.addNode({
 *   nodeId: 'node-2',
 *   host: 'localhost',
 *   port: 8081
 * });
 *
 * // Listen for membership changes
 * manager.on('node_joined', ({ nodeId }) => {
 *   console.log(`Node joined: ${nodeId}`);
 * });
 */
export class ClusterManager extends EventEmitter {
  /**
   * Create a cluster manager
   * @param {Object} config - Cluster configuration
   */
  constructor(config) {
    super();
    this.config = ClusterConfigSchema.parse(config);

    this.raftCoordinator = null;
    this.nodes = new Map(); // nodeId -> metadata
    this.nodeHealth = new Map(); // nodeId -> health status
    this.failedHealthChecks = new Map(); // nodeId -> count
    this.healthCheckTimer = null;
    this.discoveryTimer = null;

    // Metrics
    this.nodeCountGauge = meter.createObservableGauge('cluster.nodes.total', {
      description: 'Total number of nodes in cluster',
    });

    this.healthyNodeCountGauge = meter.createObservableGauge('cluster.nodes.healthy', {
      description: 'Number of healthy nodes',
    });

    this.membershipChanges = meter.createCounter('cluster.membership.changes', {
      description: 'Total membership changes',
    });

    this.nodeCountGauge.addCallback(result => {
      result.observe(this.nodes.size);
    });

    this.healthyNodeCountGauge.addCallback(result => {
      const healthy = Array.from(this.nodeHealth.values()).filter(h => h === NodeHealth.HEALTHY).length;
      result.observe(healthy);
    });
  }

  /**
   * Initialize the cluster manager
   * @param {Object} raftCoordinator - Raft coordinator instance
   * @returns {Promise<void>}
   */
  async initialize(raftCoordinator) {
    return tracer.startActiveSpan('cluster.initialize', async span => {
      try {
        span.setAttribute('node.id', this.config.nodeId);

        this.raftCoordinator = raftCoordinator;

        // Listen to Raft events
        this.raftCoordinator.on('command_applied', ({ command }) => {
          this.handleMembershipCommand(command);
        });

        this.raftCoordinator.on('peer_connected', ({ nodeId }) => {
          this.nodeHealth.set(nodeId, NodeHealth.HEALTHY);
          this.failedHealthChecks.delete(nodeId);
        });

        this.raftCoordinator.on('peer_disconnected', ({ nodeId }) => {
          this.nodeHealth.set(nodeId, NodeHealth.UNHEALTHY);
        });

        // Start health monitoring
        this.startHealthMonitoring();

        // Start auto-discovery if enabled
        if (this.config.autoDiscovery) {
          this.startAutoDiscovery();
        }

        this.emit('initialized');
        span.setStatus({ code: SpanStatusCode.OK });
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Add a node to the cluster
   * @param {Object} nodeMetadata - Node metadata
   * @returns {Promise<void>}
   */
  async addNode(nodeMetadata) {
    return tracer.startActiveSpan('cluster.addNode', async span => {
      try {
        const metadata = NodeMetadataSchema.parse(nodeMetadata);
        span.setAttribute('node.id', metadata.nodeId);

        // Replicate via Raft
        if (this.raftCoordinator) {
          await this.raftCoordinator.replicateCommand({
            type: 'REGISTER_NODE',
            nodeId: metadata.nodeId,
            data: metadata,
          });
        }

        // Add to local state
        this.nodes.set(metadata.nodeId, metadata);
        this.nodeHealth.set(metadata.nodeId, NodeHealth.UNKNOWN);

        // Add as Raft peer
        this.raftCoordinator.addPeer(metadata.nodeId, metadata.host, metadata.port);

        this.membershipChanges.add(1, { operation: 'add' });
        this.emit('node_joined', { nodeId: metadata.nodeId, metadata });

        span.setStatus({ code: SpanStatusCode.OK });
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Remove a node from the cluster
   * @param {string} nodeId - Node ID to remove
   * @returns {Promise<void>}
   */
  async removeNode(nodeId) {
    return tracer.startActiveSpan('cluster.removeNode', async span => {
      try {
        span.setAttribute('node.id', nodeId);

        if (!this.nodes.has(nodeId)) {
          throw new Error(`Node not found: ${nodeId}`);
        }

        // Replicate via Raft
        if (this.raftCoordinator) {
          await this.raftCoordinator.replicateCommand({
            type: 'DEREGISTER_NODE',
            nodeId,
            data: {},
          });
        }

        // Remove from local state
        this.nodes.delete(nodeId);
        this.nodeHealth.delete(nodeId);
        this.failedHealthChecks.delete(nodeId);

        // Remove as Raft peer
        this.raftCoordinator.removePeer(nodeId);

        this.membershipChanges.add(1, { operation: 'remove' });
        this.emit('node_left', { nodeId });

        span.setStatus({ code: SpanStatusCode.OK });
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Handle membership command from Raft
   * @param {Object} command - Membership command
   * @private
   */
  handleMembershipCommand(command) {
    switch (command.type) {
      case 'REGISTER_NODE':
        if (!this.nodes.has(command.nodeId)) {
          this.nodes.set(command.nodeId, command.data);
          this.nodeHealth.set(command.nodeId, NodeHealth.UNKNOWN);
        }
        break;

      case 'DEREGISTER_NODE':
        this.nodes.delete(command.nodeId);
        this.nodeHealth.delete(command.nodeId);
        this.failedHealthChecks.delete(command.nodeId);
        break;

      default:
      // Not a membership command
    }
  }

  /**
   * Start health monitoring
   * @private
   */
  startHealthMonitoring() {
    this.healthCheckTimer = setInterval(async () => {
      await this.performHealthChecks();
    }, this.config.healthCheckInterval);
  }

  /**
   * Perform health checks on all nodes
   * @private
   */
  async performHealthChecks() {
    const healthChecks = Array.from(this.nodes.keys()).map(nodeId => this.checkNodeHealth(nodeId));

    await Promise.allSettled(healthChecks);
  }

  /**
   * Check health of a node
   * @param {string} nodeId - Node ID to check
   * @returns {Promise<string>} Health status
   * @private
   */
  async checkNodeHealth(nodeId) {
    return tracer.startActiveSpan('cluster.healthCheck', async span => {
      try {
        span.setAttribute('node.id', nodeId);

        if (!this.raftCoordinator) {
          return NodeHealth.UNKNOWN;
        }

        const isConnected = this.raftCoordinator.transport.isConnected(nodeId);

        let health;
        if (isConnected) {
          health = NodeHealth.HEALTHY;
          this.failedHealthChecks.delete(nodeId);
        } else {
          const failedCount = (this.failedHealthChecks.get(nodeId) || 0) + 1;
          this.failedHealthChecks.set(nodeId, failedCount);

          if (failedCount >= this.config.maxFailedHealthChecks) {
            health = NodeHealth.UNHEALTHY;
          } else {
            health = NodeHealth.DEGRADED;
          }
        }

        const previousHealth = this.nodeHealth.get(nodeId);
        this.nodeHealth.set(nodeId, health);

        if (previousHealth !== health) {
          this.emit('node_health_changed', { nodeId, health, previousHealth });

          // Auto-remove unhealthy nodes after threshold
          if (health === NodeHealth.UNHEALTHY) {
            this.emit('node_unhealthy', { nodeId });
          }
        }

        span.setAttribute('health.status', health);
        span.setStatus({ code: SpanStatusCode.OK });
        return health;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        return NodeHealth.UNHEALTHY;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Start auto-discovery
   * @private
   */
  startAutoDiscovery() {
    this.discoveryTimer = setInterval(() => {
      this.performDiscovery();
    }, this.config.discoveryInterval);
  }

  /**
   * Perform cluster discovery
   * @private
   */
  async performDiscovery() {
    // In production, implement mDNS, etcd, or consul discovery
    // For now, emit event for external discovery integration
    this.emit('discovery_requested');
  }

  /**
   * Get all nodes in cluster
   * @returns {Array<Object>} Array of node metadata
   */
  getNodes() {
    return Array.from(this.nodes.values());
  }

  /**
   * Get healthy nodes
   * @returns {Array<Object>} Array of healthy node metadata
   */
  getHealthyNodes() {
    return Array.from(this.nodes.values()).filter(
      node => this.nodeHealth.get(node.nodeId) === NodeHealth.HEALTHY
    );
  }

  /**
   * Get node health
   * @param {string} nodeId - Node ID
   * @returns {string} Health status
   */
  getNodeHealth(nodeId) {
    return this.nodeHealth.get(nodeId) || NodeHealth.UNKNOWN;
  }

  /**
   * Get cluster statistics
   * @returns {Object} Cluster statistics
   */
  getStats() {
    const healthStats = {};
    for (const health of Object.values(NodeHealth)) {
      healthStats[health] = 0;
    }

    for (const health of this.nodeHealth.values()) {
      healthStats[health]++;
    }

    return {
      totalNodes: this.nodes.size,
      healthStats,
      autoDiscovery: this.config.autoDiscovery,
    };
  }

  /**
   * Shutdown the cluster manager
   * @returns {Promise<void>}
   */
  async shutdown() {
    if (this.healthCheckTimer) {
      clearInterval(this.healthCheckTimer);
      this.healthCheckTimer = null;
    }

    if (this.discoveryTimer) {
      clearInterval(this.discoveryTimer);
      this.discoveryTimer = null;
    }

    this.emit('shutdown');
  }
}

/**
 * Create a cluster manager
 * @param {Object} config - Cluster configuration
 * @returns {ClusterManager} New cluster manager instance
 */
export function createClusterManager(config) {
  return new ClusterManager(config);
}
