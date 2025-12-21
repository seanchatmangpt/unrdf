/**
 * @fileoverview Data Replication for Federated RDF Stores
 * @module federation/data-replication
 *
 * @description
 * Provides data replication across multiple RDF stores with eventual consistency,
 * conflict resolution, and topology management.
 *
 * Key features:
 * - Multi-master replication
 * - Eventual consistency model
 * - Conflict resolution strategies
 * - Batch and streaming replication
 * - Replication topology management
 * - Change data capture (CDC)
 * - Version vectors for causality tracking
 */

import { EventEmitter } from 'events';
import { randomUUID } from 'crypto';
import { z } from 'zod';
import { trace, SpanStatusCode, metrics } from '@opentelemetry/api';

const tracer = trace.getTracer('unrdf-federation');
const meter = metrics.getMeter('unrdf-federation');

/**
 * Replication topology types
 * @enum {string}
 */
export const ReplicationTopology = {
  FULL_MESH: 'full-mesh',
  STAR: 'star',
  RING: 'ring',
  TREE: 'tree',
};

/**
 * Conflict resolution strategies
 * @enum {string}
 */
export const ConflictResolution = {
  LAST_WRITE_WINS: 'last-write-wins',
  FIRST_WRITE_WINS: 'first-write-wins',
  MANUAL: 'manual',
  MERGE: 'merge',
  CUSTOM: 'custom',
};

/**
 * Replication mode
 * @enum {string}
 */
export const ReplicationMode = {
  PUSH: 'push',
  PULL: 'pull',
  BIDIRECTIONAL: 'bidirectional',
};

/**
 * Change operation schema
 */
const ChangeOperationSchema = z.object({
  changeId: z.string().default(() => randomUUID()),
  storeId: z.string(),
  operation: z.enum(['INSERT', 'DELETE', 'UPDATE']),
  quad: z.object({
    subject: z.string(),
    predicate: z.string(),
    object: z.string(),
    graph: z.string().optional(),
  }),
  timestamp: z.number().default(() => Date.now()),
  version: z.record(z.number()).default({}),
  metadata: z.record(z.string(), z.unknown()).optional(),
});

/**
 * Replication configuration schema
 */
const ReplicationConfigSchema = z.object({
  topology: z.nativeEnum(ReplicationTopology).default(ReplicationTopology.FULL_MESH),
  mode: z.nativeEnum(ReplicationMode).default(ReplicationMode.BIDIRECTIONAL),
  conflictResolution: z.nativeEnum(ConflictResolution).default(ConflictResolution.LAST_WRITE_WINS),
  batchSize: z.number().int().positive().default(100),
  batchInterval: z.number().positive().default(1000),
  enableStreaming: z.boolean().default(false),
  maxRetries: z.number().int().nonnegative().default(3),
  retryDelay: z.number().positive().default(1000),
  maxQueueSize: z.number().int().positive().default(10000), // Prevent unbounded queue growth
  clockDriftThresholdMs: z.number().int().positive().default(60000), // 1 minute max drift
});

/**
 * Data Replication Manager
 *
 * Manages replication of RDF data across federated stores with
 * conflict resolution and eventual consistency.
 *
 * @class DataReplicationManager
 * @extends EventEmitter
 *
 * @example
 * const replication = new DataReplicationManager(coordinator, {
 *   topology: ReplicationTopology.FULL_MESH,
 *   conflictResolution: ConflictResolution.LAST_WRITE_WINS,
 *   batchSize: 100
 * });
 *
 * await replication.initialize();
 *
 * // Replicate a change
 * await replication.replicate({
 *   storeId: 'store-1',
 *   operation: 'INSERT',
 *   quad: {
 *     subject: 'http://example.org/alice',
 *     predicate: 'http://xmlns.com/foaf/0.1/name',
 *     object: '"Alice"'
 *   }
 * });
 */
export class DataReplicationManager extends EventEmitter {
  /**
   * Create a data replication manager
   * @param {Object} coordinator - Federation coordinator
   * @param {Object} config - Replication configuration
   */
  constructor(coordinator, config = {}) {
    super();
    this.coordinator = coordinator;
    this.config = ReplicationConfigSchema.parse(config);

    this.changeLog = [];
    this.versionVectors = new Map();
    this.replicationQueue = [];
    this.batchTimer = null;
    this.conflictLog = [];
    this.queueOverflowCount = 0;

    // Hybrid Logical Clock (HLC) state for drift prevention
    this.hlc = {
      physicalTime: Date.now(),
      logicalCounter: 0,
      nodeId: randomUUID(),
    };

    // Metrics
    this.replicationCounter = meter.createCounter('federation.replication.total', {
      description: 'Total replications performed',
    });

    this.conflictCounter = meter.createCounter('federation.replication.conflicts', {
      description: 'Total replication conflicts detected',
    });

    this.replicationLatency = meter.createHistogram('federation.replication.latency', {
      description: 'Replication latency in milliseconds',
      unit: 'ms',
    });
  }

  /**
   * Initialize the replication manager
   * @returns {Promise<void>}
   */
  async initialize() {
    return tracer.startActiveSpan('replication.initialize', async span => {
      try {
        span.setAttribute('replication.topology', this.config.topology);
        span.setAttribute('replication.mode', this.config.mode);

        // Initialize version vectors for all stores
        const stores = this.coordinator.getStores();
        for (const store of stores) {
          this.versionVectors.set(store.storeId, {});
        }

        // Start batch processing
        this.startBatchProcessing();

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
   * Replicate a change across the federation
   * @param {Object} change - Change operation
   * @returns {Promise<void>}
   */
  async replicate(change) {
    return tracer.startActiveSpan('replication.replicate', async span => {
      const startTime = Date.now();

      try {
        const operation = ChangeOperationSchema.parse(change);
        span.setAttribute('change.id', operation.changeId);
        span.setAttribute('change.operation', operation.operation);
        span.setAttribute('change.store', operation.storeId);

        // Update version vector
        this.incrementVersion(operation.storeId);
        operation.version = this.versionVectors.get(operation.storeId);

        // Add to change log
        this.changeLog.push(operation);

        // Check queue size before adding (backpressure)
        if (this.replicationQueue.length >= this.config.maxQueueSize) {
          this.queueOverflowCount++;
          this.emit('queueOverflow', {
            queueSize: this.replicationQueue.length,
            maxSize: this.config.maxQueueSize,
            droppedOperation: operation.changeId,
            overflowCount: this.queueOverflowCount,
          });

          // Drop oldest entries to make room (FIFO overflow handling)
          const dropCount = Math.ceil(this.config.maxQueueSize * 0.1); // Drop 10%
          const dropped = this.replicationQueue.splice(0, dropCount);
          this.emit('queueEntriesDropped', {
            count: dropped.length,
            oldestDropped: dropped[0]?.changeId,
            newestDropped: dropped[dropped.length - 1]?.changeId,
          });
        }

        // Add to replication queue
        this.replicationQueue.push(operation);

        // Process immediately if streaming is enabled
        if (this.config.enableStreaming) {
          await this.processReplication(operation);
        }

        const latency = Date.now() - startTime;
        this.replicationLatency.record(latency);

        this.emit('changeReplicated', operation);
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
   * Process a single replication operation
   * @param {Object} operation - Change operation
   * @returns {Promise<void>}
   * @private
   */
  async processReplication(operation) {
    return tracer.startActiveSpan('replication.process', async span => {
      try {
        span.setAttribute('operation.id', operation.changeId);

        // Determine target stores based on topology
        const targets = this.getReplicationTargets(operation.storeId);
        span.setAttribute('replication.targets', targets.length);

        // Replicate to each target using Promise.allSettled for partial failure handling
        const replicationPromises = targets.map(async targetId => {
          return this.replicateToStore(targetId, operation)
            .then(() => ({ targetId, success: true }))
            .catch(error => ({ targetId, success: false, error }));
        });

        const results = await Promise.allSettled(replicationPromises);

        // Aggregate results
        const succeeded = [];
        const failed = [];

        for (const result of results) {
          if (result.status === 'fulfilled') {
            const value = result.value;
            if (value.success) {
              succeeded.push(value.targetId);
            } else {
              failed.push({ targetId: value.targetId, error: value.error });
              this.emit('replicationError', {
                targetId: value.targetId,
                operation,
                error: value.error,
              });
            }
          } else {
            // Promise itself rejected (shouldn't happen with our wrapper, but handle it)
            failed.push({ targetId: 'unknown', error: result.reason });
          }
        }

        // Emit aggregated result
        if (failed.length > 0) {
          this.emit('partialReplicationFailure', {
            operation: operation.changeId,
            succeeded,
            failed,
            totalTargets: targets.length,
          });
        }

        this.replicationCounter.add(1, {
          operation: operation.operation,
          source: operation.storeId,
        });

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
   * Replicate an operation to a specific store
   * @param {string} targetStoreId - Target store ID
   * @param {Object} operation - Change operation
   * @returns {Promise<void>}
   * @private
   */
  async replicateToStore(targetStoreId, operation) {
    return tracer.startActiveSpan('replication.toStore', async span => {
      try {
        span.setAttribute('target.store', targetStoreId);
        span.setAttribute('operation.id', operation.changeId);

        // Check for conflicts
        const conflict = this.detectConflict(targetStoreId, operation);

        if (conflict) {
          this.conflictCounter.add(1);
          const resolved = await this.resolveConflict(conflict, operation);

          if (!resolved) {
            throw new Error(`Failed to resolve conflict for operation ${operation.changeId}`);
          }
        }

        // In production, make actual request to target store
        // For now, simulate replication
        await this.simulateStoreReplication(targetStoreId, operation);

        // Update target's version vector
        this.mergeVersionVector(targetStoreId, operation.version);

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
   * Simulate store replication (replace with actual implementation)
   * @param {string} storeId - Target store ID
   * @param {Object} operation - Change operation
   * @returns {Promise<void>}
   * @private
   */
  async simulateStoreReplication(_storeId, _operation) {
    return new Promise(resolve => {
      setTimeout(resolve, Math.random() * 10 + 5);
    });
  }

  /**
   * Get replication targets based on topology
   * @param {string} sourceStoreId - Source store ID
   * @returns {Array<string>} Target store IDs
   * @private
   */
  getReplicationTargets(sourceStoreId) {
    const stores = this.coordinator.getHealthyStores();
    const targets = stores.filter(s => s.storeId !== sourceStoreId).map(s => s.storeId);

    switch (this.config.topology) {
      case ReplicationTopology.FULL_MESH:
        // Replicate to all other stores
        return targets;

      case ReplicationTopology.STAR:
        // Replicate to/from hub (first store is hub)
        if (sourceStoreId === stores[0]?.storeId) {
          return targets;
        }
        return [stores[0]?.storeId].filter(Boolean);

      case ReplicationTopology.RING:
        // Replicate to next store in ring
        const currentIndex = stores.findIndex(s => s.storeId === sourceStoreId);
        const nextIndex = (currentIndex + 1) % stores.length;
        return [stores[nextIndex]?.storeId].filter(Boolean);

      case ReplicationTopology.TREE:
        // Simplified tree: replicate to 2 children
        const storeIndex = stores.findIndex(s => s.storeId === sourceStoreId);
        const child1 = stores[storeIndex * 2 + 1];
        const child2 = stores[storeIndex * 2 + 2];
        return [child1?.storeId, child2?.storeId].filter(Boolean);

      default:
        return targets;
    }
  }

  /**
   * Detect conflicts between operations
   * @param {string} targetStoreId - Target store ID
   * @param {Object} operation - Incoming operation
   * @returns {Object|null} Conflict if detected
   * @private
   */
  detectConflict(targetStoreId, operation) {
    // Check if there's a concurrent modification
    const targetVector = this.versionVectors.get(targetStoreId) || {};
    const opVector = operation.version || {};

    // Simple conflict detection: check if vectors are concurrent
    let targetNewer = false;
    let opNewer = false;

    for (const storeId in { ...targetVector, ...opVector }) {
      const targetVersion = targetVector[storeId] || 0;
      const opVersion = opVector[storeId] || 0;

      if (targetVersion > opVersion) targetNewer = true;
      if (opVersion > targetVersion) opNewer = true;
    }

    // Conflict if both are newer (concurrent updates)
    if (targetNewer && opNewer) {
      return {
        targetStoreId,
        targetVector,
        operation,
        detectedAt: Date.now(),
      };
    }

    return null;
  }

  /**
   * Resolve a replication conflict
   * @param {Object} conflict - Detected conflict
   * @param {Object} operation - Incoming operation
   * @returns {Promise<boolean>} True if resolved
   * @private
   */
  async resolveConflict(conflict, _operation) {
    this.conflictLog.push(conflict);
    this.emit('conflict', conflict);

    switch (this.config.conflictResolution) {
      case ConflictResolution.LAST_WRITE_WINS:
        // Accept operation with later timestamp
        return true;

      case ConflictResolution.FIRST_WRITE_WINS:
        // Reject incoming operation
        return false;

      case ConflictResolution.MERGE:
        // Merge both operations (simplified)
        return true;

      case ConflictResolution.MANUAL:
        // Emit event for manual resolution
        return new Promise(resolve => {
          this.once(`conflict-resolved-${conflict.operation.changeId}`, decision => {
            resolve(decision);
          });
        });

      default:
        return true;
    }
  }

  /**
   * Increment version for a store
   * @param {string} storeId - Store ID
   * @private
   */
  incrementVersion(storeId) {
    const vector = this.versionVectors.get(storeId) || {};
    vector[storeId] = (vector[storeId] || 0) + 1;
    this.versionVectors.set(storeId, vector);
  }

  /**
   * Merge version vectors using Hybrid Logical Clock (HLC) with drift detection
   * @param {string} storeId - Store ID
   * @param {Object} incomingVector - Incoming version vector
   * @throws {Error} If clock drift exceeds threshold
   * @private
   */
  mergeVersionVector(storeId, incomingVector) {
    const current = this.versionVectors.get(storeId) || {};
    const now = Date.now();

    // HLC merge: update physical time and logical counter
    const incomingPhysical = incomingVector._hlcPhysical || 0;
    const incomingLogical = incomingVector._hlcLogical || 0;

    // Detect suspicious clock drift
    if (incomingPhysical > 0) {
      const drift = Math.abs(incomingPhysical - now);
      if (drift > this.config.clockDriftThresholdMs) {
        const _driftError = new Error(
          `Version vector clock drift detected: ${drift}ms exceeds threshold of ${this.config.clockDriftThresholdMs}ms`
        );
        this.emit('clockDriftDetected', {
          storeId,
          drift,
          threshold: this.config.clockDriftThresholdMs,
          localTime: now,
          remoteTime: incomingPhysical,
        });
        // Don't throw - emit warning but continue with local time
        console.warn(`[DataReplication] Clock drift warning for store ${storeId}: ${drift}ms`);
      }
    }

    // Update HLC
    if (incomingPhysical > this.hlc.physicalTime) {
      this.hlc.physicalTime = Math.min(incomingPhysical, now + this.config.clockDriftThresholdMs);
      this.hlc.logicalCounter = incomingLogical;
    } else if (incomingPhysical === this.hlc.physicalTime) {
      this.hlc.logicalCounter = Math.max(this.hlc.logicalCounter, incomingLogical) + 1;
    } else {
      // Local time is ahead, just increment logical
      if (now > this.hlc.physicalTime) {
        this.hlc.physicalTime = now;
        this.hlc.logicalCounter = 0;
      } else {
        this.hlc.logicalCounter++;
      }
    }

    // Merge version entries with drift-corrected timestamps
    for (const [id, version] of Object.entries(incomingVector)) {
      // Skip HLC metadata entries
      if (id.startsWith('_hlc')) continue;

      const incomingVersion = typeof version === 'number' ? version : 0;
      const currentVersion = current[id] || 0;

      // Validate version is not from the future (beyond drift threshold)
      if (incomingVersion > currentVersion + this.config.clockDriftThresholdMs) {
        this.emit('suspiciousVersion', {
          storeId,
          versionKey: id,
          incomingVersion,
          currentVersion,
          delta: incomingVersion - currentVersion,
        });
        // Cap the version to prevent unbounded jumps
        current[id] = currentVersion + Math.min(incomingVersion - currentVersion, 1000);
      } else {
        current[id] = Math.max(currentVersion, incomingVersion);
      }
    }

    // Attach HLC metadata
    current._hlcPhysical = this.hlc.physicalTime;
    current._hlcLogical = this.hlc.logicalCounter;
    current._hlcNodeId = this.hlc.nodeId;

    this.versionVectors.set(storeId, current);
  }

  /**
   * Get current HLC timestamp
   * @returns {Object} HLC timestamp with physical and logical components
   */
  getHLCTimestamp() {
    const now = Date.now();
    if (now > this.hlc.physicalTime) {
      this.hlc.physicalTime = now;
      this.hlc.logicalCounter = 0;
    } else {
      this.hlc.logicalCounter++;
    }
    return {
      physical: this.hlc.physicalTime,
      logical: this.hlc.logicalCounter,
      nodeId: this.hlc.nodeId,
      timestamp: `${this.hlc.physicalTime}.${this.hlc.logicalCounter}.${this.hlc.nodeId}`,
    };
  }

  /**
   * Start batch processing timer
   * @private
   */
  startBatchProcessing() {
    this.batchTimer = setInterval(async () => {
      if (this.replicationQueue.length > 0) {
        await this.processBatch();
      }
    }, this.config.batchInterval);
  }

  /**
   * Process a batch of replication operations
   * @returns {Promise<void>}
   * @private
   */
  async processBatch() {
    const batch = this.replicationQueue.splice(0, this.config.batchSize);

    if (batch.length === 0) return;

    return tracer.startActiveSpan('replication.batch', async span => {
      try {
        span.setAttribute('batch.size', batch.length);

        const processingPromises = batch.map(op => this.processReplication(op));
        await Promise.all(processingPromises);

        span.setStatus({ code: SpanStatusCode.OK });
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
      } finally {
        span.end();
      }
    });
  }

  /**
   * Get replication statistics
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      changeLogSize: this.changeLog.length,
      queueSize: this.replicationQueue.length,
      maxQueueSize: this.config.maxQueueSize,
      queueUtilization: this.replicationQueue.length / this.config.maxQueueSize,
      queueOverflowCount: this.queueOverflowCount,
      conflictCount: this.conflictLog.length,
      versionVectors: Object.fromEntries(this.versionVectors),
      hlc: {
        physical: this.hlc.physicalTime,
        logical: this.hlc.logicalCounter,
        nodeId: this.hlc.nodeId,
      },
      config: this.config,
    };
  }

  /**
   * Shutdown the replication manager
   * @returns {Promise<void>}
   */
  async shutdown() {
    if (this.batchTimer) {
      clearInterval(this.batchTimer);
      this.batchTimer = null;
    }

    // Process remaining queue
    if (this.replicationQueue.length > 0) {
      await this.processBatch();
    }

    this.emit('shutdown');
  }
}

/**
 * Create a data replication manager
 * @param {Object} coordinator - Federation coordinator
 * @param {Object} config - Replication configuration
 * @returns {DataReplicationManager} New replication manager instance
 */
export function createDataReplicationManager(coordinator, config) {
  return new DataReplicationManager(coordinator, config);
}
