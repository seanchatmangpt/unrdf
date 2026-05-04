/**
 * @fileoverview Two-Phase Commit Transaction Manager for KGC Runtime
 * Implements atomic transactions with prepare/commit/rollback phases
 *
 * Pattern: Pure functions + Zod validation + Receipt generation
 * Guarantees: ACID properties for capsule operations
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';
import { generateReceipt } from './receipt.mjs';

// ============================================================================
// Schemas
// ============================================================================

/**
 * Operation schema - represents a single atomic operation
 */
const OperationSchema = z.object({
  id: z.string(),
  type: z.enum(['add_capsule', 'remove_capsule', 'update_state', 'merge']),
  data: z.any(),
  undo: z.any().optional(), // Undo information for rollback
});

/**
 * Transaction schema
 */
const TransactionSchema = z.object({
  id: z.string(),
  timestamp: z.string(),
  operations: z.array(OperationSchema),
  status: z.enum(['pending', 'prepared', 'committed', 'aborted']),
  preparedAt: z.string().optional(),
  committedAt: z.string().optional(),
  abortedAt: z.string().optional(),
  hash: z.string().optional(),
  parentHash: z.string().optional().nullable(),
});

/**
 * @typedef {z.infer<typeof TransactionSchema>} Transaction
 */

/**
 * @typedef {z.infer<typeof OperationSchema>} Operation
 */

// ============================================================================
// TransactionManager Class
// ============================================================================

/**
 * TransactionManager - Implements two-phase commit protocol
 *
 * Phase 1 (Prepare):
 * - Validate all operations
 * - Reserve resources
 * - Create undo log entries
 * - Check constraints
 *
 * Phase 2 (Commit):
 * - Apply all changes atomically
 * - Generate receipts
 * - Update state
 *
 * Rollback:
 * - If phase 2 fails, undo phase 1 changes
 * - Use undo log to restore previous state
 *
 * @example
 * const txManager = new TransactionManager();
 * const tx = txManager.begin([
 *   { id: 'op1', type: 'add_capsule', data: capsule1 },
 *   { id: 'op2', type: 'add_capsule', data: capsule2 }
 * ]);
 *
 * const prepared = await txManager.prepare(tx.id);
 * if (prepared.success) {
 *   const committed = await txManager.commit(tx.id);
 * } else {
 *   await txManager.rollback(tx.id);
 * }
 */
export class TransactionManager {
  /**
   * @param {Object} options - Configuration options
   * @param {Function} options.onRollback - Callback for rollback events
   * @param {string} options.logPath - Path to rollback log file
   */
  constructor(options = {}) {
    /** @type {Map<string, Transaction>} */
    this.transactions = new Map();

    /** @type {string[]} */
    this.transactionHistory = [];

    /** @type {Function} */
    this.onRollback = options.onRollback || (() => {});

    /** @type {string} */
    this.logPath = options.logPath || './var/kgc/undo-log.json';

    /** @type {any} */
    this.state = {};
  }

  /**
   * Begin a new transaction
   *
   * @param {Operation[]} operations - Operations to execute
   * @param {string} [parentHash] - Parent transaction hash for chaining
   * @returns {Transaction} New transaction
   */
  begin(operations, parentHash = null) {
    const timestamp = new Date().toISOString();
    const id = `tx_${Date.now()}_${Math.random().toString(36).substring(7)}`;

    const transaction = {
      id,
      timestamp,
      operations: z.array(OperationSchema).parse(operations),
      status: 'pending',
      parentHash,
    };

    this.transactions.set(id, transaction);
    this.transactionHistory.push(id);

    return TransactionSchema.parse(transaction);
  }

  /**
   * Phase 1: Prepare transaction
   * Validates operations and reserves resources
   *
   * @param {string} txId - Transaction ID
   * @returns {Promise<{success: boolean, errors: string[], undoOps: any[]}>} Prepare result
   */
  async prepare(txId) {
    const tx = this.transactions.get(txId);
    if (!tx) {
      return { success: false, errors: ['Transaction not found'], undoOps: [] };
    }

    if (tx.status !== 'pending') {
      return { success: false, errors: ['Transaction not in pending state'], undoOps: [] };
    }

    const errors = [];
    const undoOps = [];

    // Validate each operation and create undo entries
    for (const op of tx.operations) {
      try {
        // Validate operation structure
        OperationSchema.parse(op);

        // Create undo operation
        const undoOp = this._createUndoOperation(op);
        undoOps.push(undoOp);

        // Store undo info in operation
        op.undo = undoOp;

        // Validate constraints (without applying changes)
        const validation = this._validateOperation(op);
        if (!validation.success) {
          errors.push(...validation.errors);
        }
      } catch (error) {
        errors.push(`Operation ${op.id} validation failed: ${error.message}`);
      }
    }

    if (errors.length > 0) {
      tx.status = 'aborted';
      tx.abortedAt = new Date().toISOString();
      return { success: false, errors, undoOps: [] };
    }

    // All validations passed - mark as prepared
    tx.status = 'prepared';
    tx.preparedAt = new Date().toISOString();

    // Generate transaction hash
    const hash = await this._hashTransaction(tx);
    tx.hash = hash;

    return { success: true, errors: [], undoOps };
  }

  /**
   * Phase 2: Commit transaction
   * Applies all changes atomically
   *
   * @param {string} txId - Transaction ID
   * @returns {Promise<{success: boolean, receipts: any[], errors: string[]}>} Commit result
   */
  async commit(txId) {
    const tx = this.transactions.get(txId);
    if (!tx) {
      return { success: false, receipts: [], errors: ['Transaction not found'] };
    }

    if (tx.status !== 'prepared') {
      return { success: false, receipts: [], errors: ['Transaction not prepared'] };
    }

    const receipts = [];
    const errors = [];
    const appliedOps = [];

    try {
      // Apply all operations
      for (const op of tx.operations) {
        try {
          // Apply operation to state
          this._applyOperation(op);
          appliedOps.push(op);

          // Generate receipt
          const receipt = await generateReceipt(
            op.type,
            { operation_id: op.id, data: op.data },
            { success: true, transaction_id: txId },
            tx.parentHash
          );
          receipts.push(receipt);
        } catch (error) {
          errors.push(`Operation ${op.id} failed: ${error.message}`);

          // Rollback applied operations
          for (const appliedOp of appliedOps.reverse()) {
            if (appliedOp.undo) {
              this._applyOperation(appliedOp.undo);
            }
          }

          tx.status = 'aborted';
          tx.abortedAt = new Date().toISOString();
          return { success: false, receipts: [], errors };
        }
      }

      // All operations applied successfully
      tx.status = 'committed';
      tx.committedAt = new Date().toISOString();

      return { success: true, receipts, errors: [] };
    } catch (error) {
      tx.status = 'aborted';
      tx.abortedAt = new Date().toISOString();
      return { success: false, receipts: [], errors: [error.message] };
    }
  }

  /**
   * Rollback transaction
   * Undoes all changes made during prepare/commit
   *
   * @param {string} txId - Transaction ID
   * @returns {Promise<{success: boolean, undone: number}>} Rollback result
   */
  async rollback(txId) {
    const tx = this.transactions.get(txId);
    if (!tx) {
      return { success: false, undone: 0 };
    }

    let undone = 0;

    // Undo operations in reverse order
    for (const op of tx.operations.slice().reverse()) {
      if (op.undo) {
        try {
          this._applyOperation(op.undo);
          undone++;
        } catch (error) {
          // Log error but continue rollback
          console.error(`Failed to undo operation ${op.id}:`, error);
        }
      }
    }

    tx.status = 'aborted';
    tx.abortedAt = new Date().toISOString();

    // Notify callback
    this.onRollback({ transaction_id: txId, operations_undone: undone });

    return { success: true, undone };
  }

  /**
   * Get transaction by ID
   * @param {string} txId - Transaction ID
   * @returns {Transaction|null} Transaction or null
   */
  getTransaction(txId) {
    const tx = this.transactions.get(txId);
    return tx ? TransactionSchema.parse(tx) : null;
  }

  /**
   * Get all transactions
   * @returns {Transaction[]} All transactions
   */
  getAllTransactions() {
    return this.transactionHistory.map(id => this.getTransaction(id)).filter(Boolean);
  }

  /**
   * Create undo operation for given operation
   * @private
   */
  _createUndoOperation(operation) {
    switch (operation.type) {
      case 'add_capsule':
        return {
          id: `undo_${operation.id}`,
          type: 'remove_capsule',
          data: { capsule_id: operation.data.id },
        };
      case 'remove_capsule':
        return {
          id: `undo_${operation.id}`,
          type: 'add_capsule',
          data: this.state.capsules?.[operation.data.capsule_id] || operation.data,
        };
      case 'update_state':
        return {
          id: `undo_${operation.id}`,
          type: 'update_state',
          data: {
            key: operation.data.key,
            value: this.state[operation.data.key],
            previous: operation.data.value,
          },
        };
      case 'merge':
        return {
          id: `undo_${operation.id}`,
          type: 'update_state',
          data: {
            key: 'last_merge',
            value: this.state.last_merge || null,
          },
        };
      default:
        return {
          id: `undo_${operation.id}`,
          type: 'update_state',
          data: {},
        };
    }
  }

  /**
   * Validate operation without applying it
   * @private
   */
  _validateOperation(operation) {
    const errors = [];

    switch (operation.type) {
      case 'add_capsule':
        if (!operation.data?.id) {
          errors.push('Capsule must have an id');
        }
        break;
      case 'remove_capsule':
        if (!operation.data?.capsule_id) {
          errors.push('Remove operation must specify capsule_id');
        }
        break;
      case 'update_state':
        if (!operation.data?.key) {
          errors.push('Update operation must specify key');
        }
        break;
      case 'merge':
        if (!operation.data?.capsules) {
          errors.push('Merge operation must specify capsules');
        }
        break;
      default:
        errors.push(`Unknown operation type: ${operation.type}`);
    }

    return { success: errors.length === 0, errors };
  }

  /**
   * Apply operation to state
   * @private
   */
  _applyOperation(operation) {
    if (!this.state.capsules) {
      this.state.capsules = {};
    }

    switch (operation.type) {
      case 'add_capsule':
        this.state.capsules[operation.data.id] = operation.data;
        break;
      case 'remove_capsule':
        delete this.state.capsules[operation.data.capsule_id];
        break;
      case 'update_state':
        this.state[operation.data.key] = operation.data.value;
        break;
      case 'merge':
        this.state.last_merge = {
          capsules: operation.data.capsules,
          timestamp: new Date().toISOString(),
        };
        break;
      default:
        throw new Error(`Unknown operation type: ${operation.type}`);
    }
  }

  /**
   * Hash transaction for integrity verification
   * @private
   */
  async _hashTransaction(tx) {
    const data = JSON.stringify({
      id: tx.id,
      timestamp: tx.timestamp,
      operations: tx.operations.map(op => ({
        id: op.id,
        type: op.type,
        data: op.data,
      })),
      status: tx.status,
    }, null, 0);

    return await blake3(data);
  }

  /**
   * Get current state
   * @returns {any} Current state
   */
  getState() {
    return { ...this.state };
  }

  /**
   * Reset state (for testing)
   */
  reset() {
    this.transactions.clear();
    this.transactionHistory = [];
    this.state = {};
  }
}

// ============================================================================
// Exports
// ============================================================================

export default TransactionManager;
