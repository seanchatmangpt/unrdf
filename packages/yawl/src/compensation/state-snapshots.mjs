/**
 * @file State Snapshots - Checkpoint for Undo
 * @module yawl/compensation/snapshots
 *
 * @description
 * Creates and restores state snapshots for compensation rollback.
 * Used when compensation workflow fails and state must be restored.
 * Receipt-based snapshots for cryptographic auditability.
 */

import { z } from 'zod';
import { randomUUID } from 'crypto';
import { generateReceipt } from '../receipt-core.mjs';

// ============================================================================
// SCHEMAS
// ============================================================================

/**
 * Schema for state snapshot
 */
export const StateSnapshotSchema = z.object({
  /** Snapshot ID */
  id: z.string().uuid(),
  /** Case ID */
  caseId: z.string().min(1),
  /** Task ID (optional - can be workflow-level) */
  taskId: z.string().min(1).optional(),
  /** Snapshot state data */
  state: z.any(),
  /** Created at timestamp */
  createdAt: z.coerce.date(),
  /** Receipt hash for verification */
  receiptHash: z.string().optional(),
  /** Snapshot type */
  type: z.enum(['before-task', 'after-task', 'compensation-point']).default('before-task'),
  /** Metadata */
  metadata: z.record(z.string(), z.any()).optional(),
});

/**
 * @typedef {z.infer<typeof StateSnapshotSchema>} StateSnapshot
 */

// ============================================================================
// SNAPSHOT MANAGER
// ============================================================================

/**
 * Manages state snapshots for compensation rollback
 */
export class SnapshotManager {
  /**
   * @param {Object} config
   * @param {number} [config.maxSnapshots=100] - Maximum snapshots to retain
   * @param {boolean} [config.receiptBased=true] - Use receipt-based snapshots
   */
  constructor(config = {}) {
    this.maxSnapshots = config.maxSnapshots || 100;
    this.receiptBased = config.receiptBased !== false;

    /** @type {Map<string, StateSnapshot>} */
    this.snapshots = new Map();

    /** @type {Map<string, string[]>} Case ID → Snapshot IDs */
    this.snapshotsByCase = new Map();

    /** @type {Map<string, string[]>} Task ID → Snapshot IDs */
    this.snapshotsByTask = new Map();
  }

  /**
   * Create a state snapshot before task execution
   * @param {string} caseId - Case ID
   * @param {*} state - Current state to snapshot
   * @param {Object} [options] - Snapshot options
   * @returns {Promise<StateSnapshot>} Created snapshot
   *
   * @example
   * const snapshot = await manager.createSnapshot('case-123', {
   *   workItems: [...],
   *   caseData: {...}
   * }, { taskId: 'book-hotel' });
   */
  async createSnapshot(caseId, state, options = {}) {
    const snapshot = {
      id: randomUUID(),
      caseId,
      taskId: options.taskId,
      state: this._cloneState(state),
      createdAt: new Date(),
      type: options.type || 'before-task',
      metadata: options.metadata,
    };

    // Create receipt for snapshot if enabled
    if (this.receiptBased) {
      const receipt = await generateReceipt({
        eventType: 'WORK_ITEM_CREATED',
        caseId,
        taskId: options.taskId || 'snapshot',
        payload: {
          decision: 'SNAPSHOT_CREATED',
          justification: {
            reasoning: `State snapshot created for compensation rollback`,
          },
          context: {
            snapshotId: snapshot.id,
            type: snapshot.type,
          },
        },
      }, options.previousReceipt);

      snapshot.receiptHash = receipt.receiptHash;
    }

    const validated = StateSnapshotSchema.parse(snapshot);
    this.snapshots.set(validated.id, validated);

    // Index by case
    if (!this.snapshotsByCase.has(caseId)) {
      this.snapshotsByCase.set(caseId, []);
    }
    this.snapshotsByCase.get(caseId).push(validated.id);

    // Index by task if provided
    if (options.taskId) {
      if (!this.snapshotsByTask.has(options.taskId)) {
        this.snapshotsByTask.set(options.taskId, []);
      }
      this.snapshotsByTask.get(options.taskId).push(validated.id);
    }

    // Enforce max snapshots limit
    this._enforceLimit();

    return validated;
  }

  /**
   * Restore state from a snapshot
   * @param {string} caseId - Case ID
   * @param {string} snapshotId - Snapshot ID to restore
   * @returns {*} Restored state
   *
   * @example
   * const state = manager.restoreSnapshot('case-123', 'snapshot-456');
   */
  restoreSnapshot(caseId, snapshotId) {
    const snapshot = this.snapshots.get(snapshotId);
    if (!snapshot) {
      throw new Error(`Snapshot not found: ${snapshotId}`);
    }

    if (snapshot.caseId !== caseId) {
      throw new Error(`Snapshot ${snapshotId} does not belong to case ${caseId}`);
    }

    return this._cloneState(snapshot.state);
  }

  /**
   * Get latest snapshot for a case
   * @param {string} caseId - Case ID
   * @param {Object} [options] - Filter options
   * @returns {StateSnapshot|null} Latest snapshot or null
   */
  getLatestSnapshot(caseId, options = {}) {
    let snapshots = this.snapshotsByCase.get(caseId) || [];

    if (options.taskId) {
      const taskSnapshots = new Set(this.snapshotsByTask.get(options.taskId) || []);
      snapshots = snapshots.filter(id => taskSnapshots.has(id));
    }

    if (snapshots.length === 0) {
      return null;
    }

    // Get most recent snapshot
    const snapshotId = snapshots[snapshots.length - 1];
    return this.snapshots.get(snapshotId) || null;
  }

  /**
   * Get all snapshots for a case
   * @param {string} caseId - Case ID
   * @returns {StateSnapshot[]} Snapshots ordered by creation time
   */
  getSnapshotsForCase(caseId) {
    const snapshotIds = this.snapshotsByCase.get(caseId) || [];
    return snapshotIds
      .map(id => this.snapshots.get(id))
      .filter(Boolean)
      .sort((a, b) => a.createdAt.getTime() - b.createdAt.getTime());
  }

  /**
   * Get all snapshots for a task
   * @param {string} taskId - Task ID
   * @returns {StateSnapshot[]} Snapshots ordered by creation time
   */
  getSnapshotsForTask(taskId) {
    const snapshotIds = this.snapshotsByTask.get(taskId) || [];
    return snapshotIds
      .map(id => this.snapshots.get(id))
      .filter(Boolean)
      .sort((a, b) => a.createdAt.getTime() - b.createdAt.getTime());
  }

  /**
   * Delete snapshots for a case
   * @param {string} caseId - Case ID
   * @returns {number} Number of snapshots deleted
   */
  deleteSnapshotsForCase(caseId) {
    const snapshotIds = this.snapshotsByCase.get(caseId) || [];

    for (const id of snapshotIds) {
      this.snapshots.delete(id);
    }

    this.snapshotsByCase.delete(caseId);

    return snapshotIds.length;
  }

  /**
   * Delete old snapshots to enforce limit
   * @private
   */
  _enforceLimit() {
    if (this.snapshots.size <= this.maxSnapshots) {
      return;
    }

    // Sort by creation time
    const sorted = Array.from(this.snapshots.values())
      .sort((a, b) => a.createdAt.getTime() - b.createdAt.getTime());

    // Delete oldest snapshots
    const toDelete = sorted.slice(0, sorted.length - this.maxSnapshots);
    for (const snapshot of toDelete) {
      this.snapshots.delete(snapshot.id);

      // Remove from indexes
      const caseSnapshots = this.snapshotsByCase.get(snapshot.caseId);
      if (caseSnapshots) {
        const index = caseSnapshots.indexOf(snapshot.id);
        if (index !== -1) {
          caseSnapshots.splice(index, 1);
        }
      }

      if (snapshot.taskId) {
        const taskSnapshots = this.snapshotsByTask.get(snapshot.taskId);
        if (taskSnapshots) {
          const index = taskSnapshots.indexOf(snapshot.id);
          if (index !== -1) {
            taskSnapshots.splice(index, 1);
          }
        }
      }
    }
  }

  /**
   * Deep clone state object
   * @param {*} state - State to clone
   * @returns {*} Cloned state
   * @private
   */
  _cloneState(state) {
    if (state === null || state === undefined) {
      return state;
    }

    // Handle Date objects
    if (state instanceof Date) {
      return new Date(state.getTime());
    }

    // Handle Map objects
    if (state instanceof Map) {
      return new Map(Array.from(state.entries()).map(([k, v]) => [k, this._cloneState(v)]));
    }

    // Handle Set objects
    if (state instanceof Set) {
      return new Set(Array.from(state).map(v => this._cloneState(v)));
    }

    // Handle arrays
    if (Array.isArray(state)) {
      return state.map(item => this._cloneState(item));
    }

    // Handle objects
    if (typeof state === 'object') {
      const cloned = {};
      for (const [key, value] of Object.entries(state)) {
        cloned[key] = this._cloneState(value);
      }
      return cloned;
    }

    // Primitives
    return state;
  }

  /**
   * Get snapshot statistics
   * @returns {Object} Statistics
   */
  getStats() {
    const snapshots = Array.from(this.snapshots.values());
    return {
      total: snapshots.length,
      cases: this.snapshotsByCase.size,
      tasks: this.snapshotsByTask.size,
      byType: {
        'before-task': snapshots.filter(s => s.type === 'before-task').length,
        'after-task': snapshots.filter(s => s.type === 'after-task').length,
        'compensation-point': snapshots.filter(s => s.type === 'compensation-point').length,
      },
    };
  }

  /**
   * Clear all snapshots
   */
  clear() {
    this.snapshots.clear();
    this.snapshotsByCase.clear();
    this.snapshotsByTask.clear();
  }
}

/**
 * Create a new snapshot manager
 * @param {Object} config - Configuration
 * @returns {SnapshotManager} New manager instance
 */
export function createSnapshotManager(config = {}) {
  return new SnapshotManager(config);
}
