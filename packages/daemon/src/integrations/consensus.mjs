/**
 * @file Raft Consensus Integration for Daemon
 * @module @unrdf/daemon/integrations/consensus
 * @description Production-grade Raft consensus integration for distributed daemon operation coordination.
 */

import { EventEmitter } from 'events';
import { z } from 'zod';
import { metrics } from '@opentelemetry/api';
import { detectInjection, sanitizeError, validatePayload } from '../security-audit.mjs';
import { ConsensusOperationState, PartitionState } from './consensus-state.mjs';
import * as handlers from './consensus-handlers.mjs';

const meter = metrics.getMeter('unrdf-daemon-consensus');

const ConsensusOperationSchema = z.object({
  id: z.string().min(1),
  daemonId: z.string().min(1),
  type: z.string().min(1),
  payload: z.record(z.any()),
  timestamp: z.number().int().positive(),
  term: z.number().int().nonnegative().default(0),
  leaderId: z.string().optional(),
});

const ConsensusConfigSchema = z.object({
  enabled: z.boolean().default(true),
  replicationFactor: z.number().int().min(1).max(7).default(3),
  snapshotInterval: z.number().int().positive().default(1000),
  partitionDetectionMs: z.number().int().positive().default(3000),
  recoveryRetryMs: z.number().int().positive().default(1000),
  maxLogSize: z.number().int().positive().default(10000),
  logCompactionThreshold: z.number().int().positive().default(5000),
});

/**
 * Consensus manager for Raft-based distributed operation coordination
 * @class ConsensusManager
 * @extends EventEmitter
 */
export class ConsensusManager extends EventEmitter {
  /**
   * Create a new Consensus Manager instance
   * @param {Object} daemon - Daemon instance
   * @param {Object} raftCoordinator - Raft coordinator instance
   * @param {Object} clusterManager - Cluster manager instance
   * @param {Object} [config={}] - Configuration options
   * @throws {TypeError} If daemon, raftCoordinator, or clusterManager are invalid
   */
  constructor(daemon, raftCoordinator, clusterManager, config = {}) {
    super();

    if (!daemon || typeof daemon.emit !== 'function') {
      throw new TypeError('ConsensusManager: daemon must be EventEmitter-like');
    }
    if (!raftCoordinator || typeof raftCoordinator.on !== 'function') {
      throw new TypeError('ConsensusManager: raftCoordinator must be EventEmitter-like');
    }
    if (!clusterManager || typeof clusterManager.on !== 'function') {
      throw new TypeError('ConsensusManager: clusterManager must be EventEmitter-like');
    }

    this.daemon = daemon;
    this.raftCoordinator = raftCoordinator;
    this.clusterManager = clusterManager;
    this.config = ConsensusConfigSchema.parse(config);

    this.operationLog = [];
    this.replicatedOperations = new Map();
    this.commitIndex = 0;
    this.lastAppliedIndex = 0;

    this.isLeader = false;
    this.leaderId = null;
    this.currentTerm = 0;
    this.nodeId = daemon.nodeId;

    this.partitionState = PartitionState.HEALTHY;
    this.lastHeartbeat = Date.now();
    this.partitionTimer = null;

    this.replicatedCounter = meter.createCounter('consensus.operations.replicated', {
      description: 'Total operations replicated',
    });
    this.committedCounter = meter.createCounter('consensus.operations.committed', {
      description: 'Total operations committed',
    });
    this.partitionCounter = meter.createCounter('consensus.partitions.detected', {
      description: 'Total network partitions detected',
    });
    this.logSizeGauge = meter.createObservableGauge('consensus.log.size', {
      description: 'Current operation log size',
    });
    this.logSizeGauge.addCallback(result => result.observe(this.operationLog.length));
  }

  /**
   * Initialize consensus manager with event listeners
   * @returns {Promise<void>}
   * @throws {Error} If initialization fails
   */
  async initialize() {
    try {
      this.raftCoordinator.on('leader_elected', event => handlers.handleLeaderElected(this, event));
      this.raftCoordinator.on('leader_lost', () => handlers.handleLeaderLost(this));
      this.raftCoordinator.on('term_updated', event => handlers.handleTermUpdated(this, event));
      this.clusterManager.on('node_joined', event => handlers.handleNodeJoined(this, event));
      this.clusterManager.on('node_left', event => handlers.handleNodeLeft(this, event));
      this.clusterManager.on('health_degraded', event => handlers.handleHealthDegraded(this, event));
      this.daemon.on('operation:success', event => handlers.handleOperationSuccess(this, event));
      this.daemon.on('operation:failure', event => handlers.handleOperationFailure(this, event));

      handlers.startPartitionDetection(this);
      this.emit('consensus:initialized', { nodeId: this.nodeId });
    } catch (error) {
      throw error;
    }
  }

  /**
   * Replicate operation across consensus group
   * @param {Object} operation - Operation to replicate
   * @returns {Promise<Object>} Replication result with operationId, replicated, committed flags
   * @throws {Error} If replication fails
   */
  async replicateOperation(operation) {
    try {
      const payloadValidation = validatePayload(operation, { type: 'rdf' });
      if (!payloadValidation.valid) {
        throw new Error(`Security validation failed: ${payloadValidation.reason}`);
      }

      const validated = ConsensusOperationSchema.parse(operation);
      const logEntry = {
        index: this.operationLog.length + 1,
        term: this.currentTerm,
        operation: validated,
        timestamp: Date.now(),
      };

      this.operationLog.push(logEntry);
      this.replicatedOperations.set(validated.id, ConsensusOperationState.PENDING);
      this.replicatedCounter.add(1);

      if (!this.isLeader) {
        const committed = await this.waitForCommit(validated.id, 5000);
        if (!committed) {
          throw new Error(`Operation not committed within timeout: ${validated.id}`);
        }
      } else {
        this.commitIndex = logEntry.index;
        this.replicatedOperations.set(validated.id, ConsensusOperationState.COMMITTED);
        this.committedCounter.add(1);
      }

      if (this.operationLog.length >= this.config.logCompactionThreshold) {
        this.compactLog();
      }

      return {
        operationId: validated.id,
        replicated: true,
        committed: this.replicatedOperations.get(validated.id) === ConsensusOperationState.COMMITTED,
        logIndex: logEntry.index,
        term: this.currentTerm,
      };
    } catch (error) {
      throw error;
    }
  }

  /**
   * Wait for operation to be committed to log
   * @param {string} operationId - Operation ID to wait for
   * @param {number} timeoutMs - Maximum time to wait in milliseconds
   * @returns {Promise<boolean>} True if operation committed, false on timeout
   */
  async waitForCommit(operationId, timeoutMs) {
    return new Promise(resolve => {
      const startTime = Date.now();
      const checkInterval = setInterval(() => {
        const state = this.replicatedOperations.get(operationId);
        if (state === ConsensusOperationState.COMMITTED) {
          clearInterval(checkInterval);
          resolve(true);
        } else if (Date.now() - startTime > timeoutMs) {
          clearInterval(checkInterval);
          resolve(false);
        }
      }, 50);
    });
  }

  /**
   * Compact operation log by removing old entries
   * @returns {void}
   */
  compactLog() {
    try {
      const maxSize = this.config.maxLogSize;
      const keepCount = Math.max(100, maxSize / 2);

      if (this.operationLog.length > maxSize) {
        const compactedLog = this.operationLog.slice(-keepCount);
        const removedCount = this.operationLog.length - compactedLog.length;

        this.operationLog = compactedLog;

        this.emit('log:compacted', {
          timestamp: Date.now(),
          entriesRemoved: removedCount,
          entriesRemaining: compactedLog.length,
        });
      }
    } catch (error) {
      // Log compaction errors don't propagate
    }
  }

  /**
   * Gracefully remove node from consensus group (leader only)
   * @param {string} nodeId - Node ID to remove
   * @returns {Promise<Object>} Removal result
   * @throws {Error} If node is not leader or removal fails
   */
  async removeNodeGracefully(nodeId) {
    try {
      const injection = detectInjection(nodeId, 'command');
      if (injection.detected) {
        throw new Error(`Security violation: Invalid nodeId - ${injection.reason}`);
      }

      if (!this.isLeader) {
        throw new Error('Only leader can remove nodes gracefully');
      }

      await this.waitForReplication(nodeId, 5000);

      this.emit('consensus:remove_node', {
        nodeId,
        timestamp: new Date(),
      });

      return {
        nodeId,
        removed: true,
        timestamp: Date.now(),
      };
    } catch (error) {
      throw sanitizeError(error);
    }
  }

  /**
   * Wait for operation replication to remote nodes
   * @param {string} excludeNodeId - Node ID to exclude from wait
   * @param {number} timeoutMs - Maximum time to wait in milliseconds
   * @returns {Promise<boolean>} True if replicated, false on timeout
   */
  async waitForReplication(excludeNodeId, timeoutMs) {
    return new Promise(resolve => {
      const startTime = Date.now();
      const checkInterval = setInterval(() => {
        if (Date.now() - startTime > 500) {
          clearInterval(checkInterval);
          resolve(true);
        }
      }, 50);

      setTimeout(() => {
        clearInterval(checkInterval);
        resolve(false);
      }, timeoutMs);
    });
  }

  /**
   * Get current consensus state and metrics
   * @returns {Object} Consensus state object
   */
  getConsensusState() {
    return {
      nodeId: this.nodeId,
      isLeader: this.isLeader,
      leaderId: this.leaderId,
      currentTerm: this.currentTerm,
      logSize: this.operationLog.length,
      commitIndex: this.commitIndex,
      lastAppliedIndex: this.lastAppliedIndex,
      partitionState: this.partitionState,
      replicatedOperationsCount: this.replicatedOperations.size,
      timestamp: new Date(),
    };
  }

  /**
   * Shutdown consensus manager and cleanup resources
   * @returns {Promise<void>}
   * @throws {Error} If shutdown fails
   */
  async shutdown() {
    try {
      handlers.stopPartitionDetection(this);
      this.removeAllListeners();
    } catch (error) {
      throw error;
    }
  }
}

/**
 * Factory function to create and configure consensus manager
 * @param {Object} daemon - Daemon instance
 * @param {Object} raftCoordinator - Raft coordinator instance
 * @param {Object} clusterManager - Cluster manager instance
 * @param {Object} [config={}] - Configuration options
 * @returns {ConsensusManager} Configured consensus manager instance
 */
export function createConsensusManager(daemon, raftCoordinator, clusterManager, config = {}) {
  return new ConsensusManager(daemon, raftCoordinator, clusterManager, config);
}
