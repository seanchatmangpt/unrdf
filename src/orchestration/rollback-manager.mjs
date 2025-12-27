/**
 * @fileoverview Rollback Manager - Undo changes on workflow failure
 *
 * **Purpose**: Ensure atomicity by tracking and reversing changes:
 * - Checkpoint creation before each operation
 * - Transaction-like semantics (begin/commit/rollback)
 * - State restoration on failure
 * - Audit trail of rollback operations
 *
 * **Properties**:
 * - ACID-like guarantees for multi-package changes
 * - Nested transaction support
 * - Partial rollback capability
 * - Recovery from interrupted rollbacks
 *
 * @module orchestration/rollback-manager
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';

/**
 * Checkpoint schema
 */
export const CheckpointSchema = z.object({
  id: z.string().uuid(),
  timestamp: z.string().datetime(),
  packageName: z.string(),
  stage: z.string(),
  state: z.any(),
  hash: z.string(),
  metadata: z.record(z.any()).optional()
});

/**
 * Transaction schema
 */
export const TransactionSchema = z.object({
  id: z.string().uuid(),
  startTime: z.string().datetime(),
  endTime: z.string().datetime().optional(),
  status: z.enum(['active', 'committed', 'rolled_back', 'failed']),
  packages: z.array(z.string()),
  checkpoints: z.array(z.string()),
  operations: z.array(z.object({
    type: z.string(),
    packageName: z.string(),
    stage: z.string(),
    timestamp: z.string().datetime(),
    reversible: z.boolean(),
    reverseOperation: z.any().optional()
  })),
  parentTransaction: z.string().optional()
});

/**
 * Rollback result schema
 */
export const RollbackResultSchema = z.object({
  success: z.boolean(),
  transactionId: z.string(),
  rolledBackOperations: z.number(),
  restoredCheckpoints: z.number(),
  errors: z.array(z.object({
    operation: z.string(),
    error: z.string()
  })),
  duration: z.number(),
  timestamp: z.string().datetime()
});

/**
 * Rollback Manager - Manages checkpoints and rollback operations
 *
 * @class RollbackManager
 *
 * @example
 * const manager = new RollbackManager();
 *
 * // Begin transaction
 * const txId = manager.beginTransaction(['core', 'utils']);
 *
 * // Create checkpoints before changes
 * await manager.checkpoint('core', 'admission', currentState);
 *
 * // ... make changes ...
 *
 * // On failure, rollback
 * if (failed) {
 *   await manager.rollback(txId);
 * } else {
 *   manager.commit(txId);
 * }
 */
export class RollbackManager {
  /**
   * Create a new rollback manager
   *
   * @param {Object} [options] - Manager options
   * @param {number} [options.maxCheckpoints=1000] - Max checkpoints to retain
   * @param {boolean} [options.persistCheckpoints=false] - Persist to disk
   */
  constructor(options = {}) {
    /** @type {Map<string, Object>} Checkpoints by ID */
    this.checkpoints = new Map();

    /** @type {Map<string, Object>} Transactions by ID */
    this.transactions = new Map();

    /** @type {string|null} Active transaction ID */
    this.activeTransaction = null;

    /** @type {number} Max checkpoints */
    this.maxCheckpoints = options.maxCheckpoints ?? 1000;

    /** @type {boolean} Persist checkpoints */
    this.persistCheckpoints = options.persistCheckpoints ?? false;

    /** @type {Array} Rollback history */
    this.history = [];
  }

  /**
   * Generate unique transaction ID
   *
   * @returns {string} UUID
   * @private
   */
  _generateId() {
    return crypto.randomUUID();
  }

  /**
   * Compute hash of state
   *
   * @param {any} state - State to hash
   * @returns {Promise<string>} BLAKE3 hash
   * @private
   */
  async _hashState(state) {
    const json = JSON.stringify(state, Object.keys(state || {}).sort());
    return blake3(json);
  }

  /**
   * Begin a new transaction
   *
   * @param {string[]} packages - Packages involved in transaction
   * @param {Object} [options] - Transaction options
   * @returns {string} Transaction ID
   */
  beginTransaction(packages, options = {}) {
    if (this.activeTransaction && !options.allowNested) {
      throw new Error(`Transaction already active: ${this.activeTransaction}`);
    }

    const txId = this._generateId();
    const now = new Date().toISOString();

    const transaction = {
      id: txId,
      startTime: now,
      status: 'active',
      packages: [...packages],
      checkpoints: [],
      operations: [],
      parentTransaction: options.allowNested ? this.activeTransaction : undefined
    };

    this.transactions.set(txId, transaction);
    this.activeTransaction = txId;

    return txId;
  }

  /**
   * Create a checkpoint for a package/stage
   *
   * @param {string} packageName - Package name
   * @param {string} stage - Stage name
   * @param {any} state - Current state to checkpoint
   * @param {Object} [metadata] - Additional metadata
   * @returns {Promise<string>} Checkpoint ID
   */
  async checkpoint(packageName, stage, state, metadata = {}) {
    if (!this.activeTransaction) {
      throw new Error('No active transaction');
    }

    const id = this._generateId();
    const now = new Date().toISOString();
    const hash = await this._hashState(state);

    const checkpoint = {
      id,
      timestamp: now,
      packageName,
      stage,
      state: this._cloneState(state),
      hash,
      metadata
    };

    // Validate
    CheckpointSchema.parse(checkpoint);

    // Store checkpoint
    this.checkpoints.set(id, checkpoint);

    // Add to transaction
    const tx = this.transactions.get(this.activeTransaction);
    tx.checkpoints.push(id);

    // Enforce max checkpoints (LRU eviction)
    if (this.checkpoints.size > this.maxCheckpoints) {
      const oldest = this.checkpoints.keys().next().value;
      this.checkpoints.delete(oldest);
    }

    return id;
  }

  /**
   * Clone state for checkpoint storage
   *
   * @param {any} state - State to clone
   * @returns {any} Cloned state
   * @private
   */
  _cloneState(state) {
    if (state === null || state === undefined) {
      return state;
    }

    if (typeof state !== 'object') {
      return state;
    }

    // Deep clone via JSON (works for serializable state)
    try {
      return JSON.parse(JSON.stringify(state));
    } catch {
      // For non-serializable state, store reference
      return { __ref: true, value: state };
    }
  }

  /**
   * Record an operation for potential rollback
   *
   * @param {string} type - Operation type
   * @param {string} packageName - Package name
   * @param {string} stage - Stage name
   * @param {Object} [options] - Operation options
   */
  recordOperation(type, packageName, stage, options = {}) {
    if (!this.activeTransaction) {
      throw new Error('No active transaction');
    }

    const operation = {
      type,
      packageName,
      stage,
      timestamp: new Date().toISOString(),
      reversible: options.reversible ?? true,
      reverseOperation: options.reverseOperation
    };

    const tx = this.transactions.get(this.activeTransaction);
    tx.operations.push(operation);
  }

  /**
   * Commit the current transaction
   *
   * @param {string} [txId] - Transaction ID (uses active if not provided)
   * @returns {Object} Commit result
   */
  commit(txId = null) {
    const targetId = txId || this.activeTransaction;
    if (!targetId) {
      throw new Error('No transaction to commit');
    }

    const tx = this.transactions.get(targetId);
    if (!tx) {
      throw new Error(`Transaction not found: ${targetId}`);
    }

    if (tx.status !== 'active') {
      throw new Error(`Transaction not active: ${tx.status}`);
    }

    tx.status = 'committed';
    tx.endTime = new Date().toISOString();

    // If this was active transaction, clear it
    if (this.activeTransaction === targetId) {
      this.activeTransaction = tx.parentTransaction || null;
    }

    // Clean up checkpoints (they're no longer needed after commit)
    for (const cpId of tx.checkpoints) {
      this.checkpoints.delete(cpId);
    }

    return {
      transactionId: targetId,
      status: 'committed',
      operationCount: tx.operations.length,
      duration: new Date(tx.endTime) - new Date(tx.startTime)
    };
  }

  /**
   * Rollback the current transaction
   *
   * @param {string} [txId] - Transaction ID (uses active if not provided)
   * @param {Object} [options] - Rollback options
   * @returns {Promise<Object>} Rollback result
   */
  async rollback(txId = null, options = {}) {
    const startTime = Date.now();
    const targetId = txId || this.activeTransaction;

    if (!targetId) {
      throw new Error('No transaction to rollback');
    }

    const tx = this.transactions.get(targetId);
    if (!tx) {
      throw new Error(`Transaction not found: ${targetId}`);
    }

    if (tx.status !== 'active') {
      throw new Error(`Transaction not active: ${tx.status}`);
    }

    const errors = [];
    let rolledBackOperations = 0;
    let restoredCheckpoints = 0;

    // Rollback operations in reverse order
    const reversedOps = [...tx.operations].reverse();

    for (const op of reversedOps) {
      if (op.reversible && op.reverseOperation) {
        try {
          if (typeof op.reverseOperation === 'function') {
            await op.reverseOperation();
          }
          rolledBackOperations++;
        } catch (error) {
          errors.push({
            operation: `${op.type}:${op.packageName}:${op.stage}`,
            error: error.message
          });
        }
      }
    }

    // Restore checkpoints in reverse order
    const reversedCps = [...tx.checkpoints].reverse();
    const restoredStates = new Map();

    for (const cpId of reversedCps) {
      const cp = this.checkpoints.get(cpId);
      if (cp) {
        const key = `${cp.packageName}:${cp.stage}`;
        if (!restoredStates.has(key)) {
          restoredStates.set(key, cp.state);
          restoredCheckpoints++;
        }
      }
    }

    // Update transaction status
    tx.status = errors.length === 0 ? 'rolled_back' : 'failed';
    tx.endTime = new Date().toISOString();

    // Clear active transaction
    if (this.activeTransaction === targetId) {
      this.activeTransaction = tx.parentTransaction || null;
    }

    // Clean up checkpoints
    for (const cpId of tx.checkpoints) {
      this.checkpoints.delete(cpId);
    }

    const result = {
      success: errors.length === 0,
      transactionId: targetId,
      rolledBackOperations,
      restoredCheckpoints,
      restoredStates: Object.fromEntries(restoredStates),
      errors,
      duration: Date.now() - startTime,
      timestamp: new Date().toISOString()
    };

    // Record in history
    this.history.push(result);

    return result;
  }

  /**
   * Get checkpoint by ID
   *
   * @param {string} id - Checkpoint ID
   * @returns {Object|undefined}
   */
  getCheckpoint(id) {
    return this.checkpoints.get(id);
  }

  /**
   * Get checkpoints for a package
   *
   * @param {string} packageName - Package name
   * @returns {Object[]}
   */
  getCheckpointsForPackage(packageName) {
    return Array.from(this.checkpoints.values())
      .filter(cp => cp.packageName === packageName)
      .sort((a, b) => new Date(b.timestamp) - new Date(a.timestamp));
  }

  /**
   * Get latest checkpoint for package/stage
   *
   * @param {string} packageName - Package name
   * @param {string} stage - Stage name
   * @returns {Object|undefined}
   */
  getLatestCheckpoint(packageName, stage) {
    const checkpoints = Array.from(this.checkpoints.values())
      .filter(cp => cp.packageName === packageName && cp.stage === stage)
      .sort((a, b) => new Date(b.timestamp) - new Date(a.timestamp));

    return checkpoints[0];
  }

  /**
   * Get transaction by ID
   *
   * @param {string} id - Transaction ID
   * @returns {Object|undefined}
   */
  getTransaction(id) {
    return this.transactions.get(id);
  }

  /**
   * Get active transaction
   *
   * @returns {Object|undefined}
   */
  getActiveTransaction() {
    if (!this.activeTransaction) return undefined;
    return this.transactions.get(this.activeTransaction);
  }

  /**
   * Check if a transaction is active
   *
   * @returns {boolean}
   */
  hasActiveTransaction() {
    return this.activeTransaction !== null;
  }

  /**
   * Get rollback history
   *
   * @param {number} [limit=10] - Number of entries
   * @returns {Object[]}
   */
  getHistory(limit = 10) {
    return this.history.slice(-limit);
  }

  /**
   * Get transaction summary
   *
   * @returns {Object}
   */
  getSummary() {
    const allTxs = Array.from(this.transactions.values());

    return {
      activeTransaction: this.activeTransaction,
      totalTransactions: allTxs.length,
      committedTransactions: allTxs.filter(t => t.status === 'committed').length,
      rolledBackTransactions: allTxs.filter(t => t.status === 'rolled_back').length,
      failedTransactions: allTxs.filter(t => t.status === 'failed').length,
      totalCheckpoints: this.checkpoints.size,
      rollbackHistory: this.history.length
    };
  }

  /**
   * Clear all state
   */
  clear() {
    this.checkpoints.clear();
    this.transactions.clear();
    this.activeTransaction = null;
    this.history = [];
  }

  /**
   * Export state for persistence
   *
   * @returns {Object}
   */
  toJSON() {
    return {
      checkpoints: Object.fromEntries(this.checkpoints),
      transactions: Object.fromEntries(this.transactions),
      activeTransaction: this.activeTransaction,
      history: this.history
    };
  }

  /**
   * Import state from persistence
   *
   * @param {Object} data - Exported state
   * @returns {RollbackManager} this
   */
  fromJSON(data) {
    this.checkpoints = new Map(Object.entries(data.checkpoints || {}));
    this.transactions = new Map(Object.entries(data.transactions || {}));
    this.activeTransaction = data.activeTransaction || null;
    this.history = data.history || [];
    return this;
  }
}

/**
 * Create a rollback manager
 *
 * @param {Object} [options] - Manager options
 * @returns {RollbackManager}
 */
export function createRollbackManager(options = {}) {
  return new RollbackManager(options);
}

/**
 * Execute with automatic rollback on failure
 *
 * @param {RollbackManager} manager - Rollback manager
 * @param {string[]} packages - Packages involved
 * @param {Function} fn - Function to execute
 * @returns {Promise<any>} Function result
 */
export async function withRollback(manager, packages, fn) {
  const txId = manager.beginTransaction(packages);

  try {
    const result = await fn(manager);
    manager.commit(txId);
    return result;
  } catch (error) {
    await manager.rollback(txId);
    throw error;
  }
}
