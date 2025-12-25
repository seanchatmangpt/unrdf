/**
 * @fileoverview Distributed State Machine for Workflow Synchronization
 * @module consensus/state/distributed-state-machine
 *
 * @description
 * Implements a replicated state machine synchronized via Raft consensus.
 * Provides strong consistency for workflow state across the cluster.
 *
 * Key features:
 * - State replication via Raft log
 * - Automatic state synchronization
 * - Snapshot support for large states
 * - Query interface for state access
 * - Event notification on state changes
 */

import { EventEmitter } from 'events';
import { randomUUID } from 'crypto';
import { z } from 'zod';
import { trace, SpanStatusCode, metrics } from '@opentelemetry/api';

const tracer = trace.getTracer('unrdf-consensus');
const meter = metrics.getMeter('unrdf-consensus');

/**
 * State operation types
 * @enum {string}
 */
export const StateOperation = {
  SET: 'set',
  DELETE: 'delete',
  UPDATE: 'update',
  BATCH: 'batch',
};

/**
 * State change schema
 */
const StateChangeSchema = z.object({
  operation: z.nativeEnum(StateOperation),
  key: z.string().optional(),
  value: z.any().optional(),
  changes: z.array(z.any()).optional(),
  timestamp: z.number().default(() => Date.now()),
  sourceNode: z.string(),
});

/**
 * State machine configuration schema
 */
const StateMachineConfigSchema = z.object({
  nodeId: z.string(),
  snapshotInterval: z.number().positive().default(1000),
  maxStateSize: z.number().positive().default(100000),
  enableSnapshots: z.boolean().default(true),
  persistSnapshots: z.boolean().default(false),
});

/**
 * Distributed State Machine
 *
 * Provides a replicated key-value store synchronized via Raft.
 * All state changes are logged and replicated across the cluster.
 *
 * @class DistributedStateMachine
 * @extends EventEmitter
 *
 * @example
 * const stateMachine = new DistributedStateMachine({
 *   nodeId: 'node-1',
 *   enableSnapshots: true
 * });
 *
 * await stateMachine.initialize(raftCoordinator);
 *
 * // Set state (replicated via Raft)
 * await stateMachine.set('workflow:123', {
 *   status: 'running',
 *   progress: 0.5
 * });
 *
 * // Get state (local read)
 * const state = stateMachine.get('workflow:123');
 *
 * // Listen for state changes
 * stateMachine.on('state_changed', ({ key, value }) => {
 *   console.log(`State changed: ${key}`, value);
 * });
 */
export class DistributedStateMachine extends EventEmitter {
  /**
   * Create a distributed state machine
   * @param {Object} config - State machine configuration
   */
  constructor(config) {
    super();
    this.config = StateMachineConfigSchema.parse(config);

    this.raftCoordinator = null;
    this.state = new Map();
    this.changeLog = [];
    this.snapshots = [];
    this.lastSnapshotIndex = 0;

    // Metrics
    this.stateSize = meter.createObservableGauge('state.size', {
      description: 'Number of entries in state machine',
    });

    this.changeLogSize = meter.createObservableGauge('state.changelog.size', {
      description: 'Number of entries in change log',
    });

    this.stateOperations = meter.createCounter('state.operations.total', {
      description: 'Total state operations',
    });

    this.stateSize.addCallback(result => {
      result.observe(this.state.size);
    });

    this.changeLogSize.addCallback(result => {
      result.observe(this.changeLog.length);
    });
  }

  /**
   * Initialize the state machine
   * @param {Object} raftCoordinator - Raft coordinator instance
   * @returns {Promise<void>}
   */
  async initialize(raftCoordinator) {
    return tracer.startActiveSpan('state.initialize', async span => {
      try {
        span.setAttribute('node.id', this.config.nodeId);

        this.raftCoordinator = raftCoordinator;

        // Listen to Raft command applications
        this.raftCoordinator.on('command_applied', ({ command, entry }) => {
          this.applyStateChange(command, entry);
        });

        // Restore from snapshot if available
        if (this.config.enableSnapshots) {
          await this.restoreFromSnapshot();
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
   * Set a value in the state machine
   * @param {string} key - State key
   * @param {*} value - State value
   * @returns {Promise<void>}
   */
  async set(key, value) {
    return tracer.startActiveSpan('state.set', async span => {
      try {
        span.setAttribute('state.key', key);

        const change = StateChangeSchema.parse({
          operation: StateOperation.SET,
          key,
          value,
          sourceNode: this.config.nodeId,
        });

        // Replicate via Raft
        await this.raftCoordinator.replicateCommand({
          type: 'UPDATE_STATE',
          workflowId: key,
          data: change,
        });

        this.stateOperations.add(1, { operation: 'set' });

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
   * Get a value from the state machine
   * @param {string} key - State key
   * @returns {*} State value or undefined
   */
  get(key) {
    return this.state.get(key);
  }

  /**
   * Check if a key exists in the state machine
   * @param {string} key - State key
   * @returns {boolean} True if key exists
   */
  has(key) {
    return this.state.has(key);
  }

  /**
   * Delete a value from the state machine
   * @param {string} key - State key
   * @returns {Promise<void>}
   */
  async delete(key) {
    return tracer.startActiveSpan('state.delete', async span => {
      try {
        span.setAttribute('state.key', key);

        const change = StateChangeSchema.parse({
          operation: StateOperation.DELETE,
          key,
          sourceNode: this.config.nodeId,
        });

        // Replicate via Raft
        await this.raftCoordinator.replicateCommand({
          type: 'UPDATE_STATE',
          workflowId: key,
          data: change,
        });

        this.stateOperations.add(1, { operation: 'delete' });

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
   * Update a value in the state machine
   * @param {string} key - State key
   * @param {Function} updateFn - Update function (oldValue) => newValue
   * @returns {Promise<void>}
   */
  async update(key, updateFn) {
    const oldValue = this.state.get(key);
    const newValue = updateFn(oldValue);
    await this.set(key, newValue);
  }

  /**
   * Batch update multiple keys
   * @param {Array<Object>} changes - Array of {key, value} objects
   * @returns {Promise<void>}
   */
  async batchUpdate(changes) {
    return tracer.startActiveSpan('state.batchUpdate', async span => {
      try {
        span.setAttribute('state.changeCount', changes.length);

        const change = StateChangeSchema.parse({
          operation: StateOperation.BATCH,
          changes,
          sourceNode: this.config.nodeId,
        });

        // Replicate via Raft
        await this.raftCoordinator.replicateCommand({
          type: 'UPDATE_STATE',
          workflowId: `batch:${randomUUID()}`,
          data: change,
        });

        this.stateOperations.add(changes.length, { operation: 'batch' });

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
   * Apply a state change from Raft log
   * @param {Object} command - Raft command
   * @param {Object} entry - Log entry
   * @private
   */
  applyStateChange(command, entry) {
    if (command.type !== 'UPDATE_STATE') return;

    const change = command.data;

    try {
      switch (change.operation) {
        case StateOperation.SET:
          this.state.set(change.key, change.value);
          this.emit('state_changed', { key: change.key, value: change.value, operation: 'set' });
          break;

        case StateOperation.DELETE:
          this.state.delete(change.key);
          this.emit('state_changed', { key: change.key, operation: 'delete' });
          break;

        case StateOperation.UPDATE:
          if (this.state.has(change.key)) {
            this.state.set(change.key, change.value);
            this.emit('state_changed', { key: change.key, value: change.value, operation: 'update' });
          }
          break;

        case StateOperation.BATCH:
          for (const item of change.changes) {
            this.state.set(item.key, item.value);
            this.emit('state_changed', { key: item.key, value: item.value, operation: 'set' });
          }
          break;

        default:
          this.emit('error', { error: new Error(`Unknown operation: ${change.operation}`) });
      }

      // Add to change log
      this.changeLog.push({ ...change, index: entry.index });

      // Create snapshot if needed
      if (this.config.enableSnapshots && this.changeLog.length - this.lastSnapshotIndex >= this.config.snapshotInterval) {
        this.createSnapshot();
      }
    } catch (error) {
      this.emit('error', { error, source: 'apply_state_change' });
    }
  }

  /**
   * Create a snapshot of current state
   * @private
   */
  createSnapshot() {
    const snapshot = {
      id: randomUUID(),
      timestamp: Date.now(),
      index: this.changeLog.length,
      state: new Map(this.state),
      stateSize: this.state.size,
    };

    this.snapshots.push(snapshot);
    this.lastSnapshotIndex = this.changeLog.length;

    // Keep only last 5 snapshots
    if (this.snapshots.length > 5) {
      this.snapshots.shift();
    }

    this.emit('snapshot_created', {
      snapshotId: snapshot.id,
      index: snapshot.index,
      stateSize: snapshot.stateSize,
    });
  }

  /**
   * Restore state from latest snapshot
   * @private
   */
  async restoreFromSnapshot() {
    if (this.snapshots.length === 0) return;

    const latestSnapshot = this.snapshots[this.snapshots.length - 1];
    this.state = new Map(latestSnapshot.state);
    this.lastSnapshotIndex = latestSnapshot.index;

    this.emit('snapshot_restored', {
      snapshotId: latestSnapshot.id,
      index: latestSnapshot.index,
      stateSize: latestSnapshot.stateSize,
    });
  }

  /**
   * Get all keys in state machine
   * @returns {Array<string>} Array of keys
   */
  keys() {
    return Array.from(this.state.keys());
  }

  /**
   * Get all values in state machine
   * @returns {Array<*>} Array of values
   */
  values() {
    return Array.from(this.state.values());
  }

  /**
   * Get all entries in state machine
   * @returns {Array<[string, *]>} Array of [key, value] pairs
   */
  entries() {
    return Array.from(this.state.entries());
  }

  /**
   * Get state machine statistics
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      stateSize: this.state.size,
      changeLogSize: this.changeLog.length,
      snapshotCount: this.snapshots.length,
      lastSnapshotIndex: this.lastSnapshotIndex,
      latestSnapshot: this.snapshots.length > 0 ? this.snapshots[this.snapshots.length - 1].id : null,
    };
  }

  /**
   * Shutdown the state machine
   */
  async shutdown() {
    if (this.config.enableSnapshots && this.changeLog.length > this.lastSnapshotIndex) {
      this.createSnapshot();
    }

    this.emit('shutdown');
  }
}

/**
 * Create a distributed state machine
 * @param {Object} config - State machine configuration
 * @returns {DistributedStateMachine} New state machine instance
 */
export function createDistributedStateMachine(config) {
  return new DistributedStateMachine(config);
}
