/**
 * @fileoverview Atomic Capsule Admission using Two-Phase Commit
 * Ensures all-or-nothing capsule admission across conflicts
 *
 * Pattern: Transaction wrapper around merge operations
 */

import { z } from 'zod';
import { TransactionManager } from './transaction.mjs';
import { mergeCapsules } from './merge.mjs';
import { RollbackLog } from './rollback.mjs';

// ============================================================================
// Schemas
// ============================================================================

/**
 * Atomic admission request schema
 */
const AtomicAdmissionRequestSchema = z.object({
  capsules: z.array(z.any()),
  totalOrder: z.object({
    rules: z.array(z.any()),
    default_rule: z.any(),
  }),
  bounds: z.record(z.any()).optional(),
});

/**
 * Atomic admission result schema
 */
const AtomicAdmissionResultSchema = z.object({
  success: z.boolean(),
  transaction_id: z.string().optional(),
  admitted: z.array(z.string()),
  denied: z.array(z.string()),
  conflict_receipts: z.array(z.any()),
  receipts: z.array(z.any()).optional(),
  errors: z.array(z.string()).optional(),
});

/**
 * @typedef {z.infer<typeof AtomicAdmissionRequestSchema>} AtomicAdmissionRequest
 */

/**
 * @typedef {z.infer<typeof AtomicAdmissionResultSchema>} AtomicAdmissionResult
 */

// ============================================================================
// AtomicAdmissionGate Class
// ============================================================================

/**
 * AtomicAdmissionGate - Atomic capsule admission using transactions
 *
 * Features:
 * - All-or-nothing admission (either all capsules admitted or none)
 * - Automatic rollback on conflict
 * - Receipt generation for all operations
 * - Integration with merge conflict resolution
 *
 * @example
 * const gate = new AtomicAdmissionGate();
 * const result = await gate.admitCapsules(capsules, totalOrder);
 * if (result.success) {
 *   console.log('All capsules admitted:', result.admitted);
 * } else {
 *   console.log('Admission failed, rolled back');
 * }
 */
export class AtomicAdmissionGate {
  /**
   * @param {Object} options - Configuration options
   * @param {string} options.logPath - Path to rollback log
   */
  constructor(options = {}) {
    /** @type {TransactionManager} */
    this.txManager = new TransactionManager({
      logPath: options.logPath,
      onRollback: (event) => this._handleRollback(event),
    });

    /** @type {RollbackLog} */
    this.rollbackLog = new RollbackLog(options.logPath);

    /** @type {Map<string, any>} */
    this.admittedCapsules = new Map();
  }

  /**
   * Admit capsules atomically
   *
   * @param {any[]} capsules - Capsules to admit
   * @param {any} totalOrder - Conflict resolution rules
   * @param {any} [bounds] - Optional resource bounds
   * @returns {Promise<AtomicAdmissionResult>} Admission result
   */
  async admitCapsules(capsules, totalOrder, bounds = {}) {
    try {
      // Validate input
      const request = AtomicAdmissionRequestSchema.parse({
        capsules,
        totalOrder,
        bounds,
      });

      // Phase 1: Merge and detect conflicts
      const mergeResult = mergeCapsules(request.capsules, request.totalOrder);

      // Check if all capsules were admitted (no conflicts)
      if (mergeResult.denied.length > 0) {
        // Some capsules denied - reject entire batch
        return AtomicAdmissionResultSchema.parse({
          success: false,
          admitted: [],
          denied: mergeResult.denied,
          conflict_receipts: mergeResult.conflict_receipts,
          errors: ['Conflict detected - admission denied to maintain atomicity'],
        });
      }

      // Phase 2: Create transaction for admission
      const operations = mergeResult.admitted.map(capsuleId => {
        const capsule = request.capsules.find(c => c.id === capsuleId);
        return {
          id: `admit_${capsuleId}`,
          type: 'add_capsule',
          data: capsule,
        };
      });

      const tx = this.txManager.begin(operations);

      // Phase 3: Prepare transaction
      const prepareResult = await this.txManager.prepare(tx.id);

      if (!prepareResult.success) {
        // Prepare failed - abort
        return AtomicAdmissionResultSchema.parse({
          success: false,
          admitted: [],
          denied: mergeResult.admitted,
          conflict_receipts: [],
          errors: prepareResult.errors,
        });
      }

      // Phase 4: Commit transaction
      const commitResult = await this.txManager.commit(tx.id);

      if (!commitResult.success) {
        // Commit failed - rollback
        await this.txManager.rollback(tx.id);

        // Log rollback
        await this.rollbackLog.append(
          tx.id,
          prepareResult.undoOps,
          tx.hash
        );

        return AtomicAdmissionResultSchema.parse({
          success: false,
          admitted: [],
          denied: mergeResult.admitted,
          conflict_receipts: [],
          errors: commitResult.errors,
        });
      }

      // Success - store admitted capsules
      for (const capsuleId of mergeResult.admitted) {
        const capsule = request.capsules.find(c => c.id === capsuleId);
        this.admittedCapsules.set(capsuleId, capsule);
      }

      return AtomicAdmissionResultSchema.parse({
        success: true,
        transaction_id: tx.id,
        admitted: mergeResult.admitted,
        denied: [],
        conflict_receipts: mergeResult.conflict_receipts,
        receipts: commitResult.receipts,
      });
    } catch (error) {
      return AtomicAdmissionResultSchema.parse({
        success: false,
        admitted: [],
        denied: capsules.map(c => c.id),
        conflict_receipts: [],
        errors: [error.message],
      });
    }
  }

  /**
   * Admit single capsule (convenience method)
   *
   * @param {any} capsule - Capsule to admit
   * @param {any} totalOrder - Conflict resolution rules
   * @returns {Promise<AtomicAdmissionResult>} Admission result
   */
  async admitCapsule(capsule, totalOrder) {
    return this.admitCapsules([capsule], totalOrder);
  }

  /**
   * Rollback a transaction by ID
   *
   * @param {string} transactionId - Transaction ID to rollback
   * @returns {Promise<{success: boolean, operations_undone: number}>} Rollback result
   */
  async rollbackTransaction(transactionId) {
    // Rollback using transaction manager
    const result = await this.txManager.rollback(transactionId);

    // Remove rolled back capsules from admitted set
    const tx = this.txManager.getTransaction(transactionId);
    if (tx) {
      for (const op of tx.operations) {
        if (op.type === 'add_capsule' && op.data?.id) {
          this.admittedCapsules.delete(op.data.id);
        }
      }
    }

    return result;
  }

  /**
   * Get all admitted capsules
   * @returns {any[]} Admitted capsules
   */
  getAdmittedCapsules() {
    return Array.from(this.admittedCapsules.values());
  }

  /**
   * Check if capsule is admitted
   *
   * @param {string} capsuleId - Capsule ID
   * @returns {boolean} True if admitted
   */
  isAdmitted(capsuleId) {
    return this.admittedCapsules.has(capsuleId);
  }

  /**
   * Handle rollback event
   * @private
   */
  _handleRollback(event) {
    console.log(`Transaction ${event.transaction_id} rolled back: ${event.operations_undone} operations undone`);
  }

  /**
   * Reset state (for testing)
   */
  reset() {
    this.txManager.reset();
    this.admittedCapsules.clear();
  }
}

// ============================================================================
// Cascading Rollback Support
// ============================================================================

/**
 * Rollback transaction and all dependent transactions
 *
 * @param {AtomicAdmissionGate} gate - Admission gate
 * @param {string} transactionId - Root transaction to rollback
 * @returns {Promise<{success: boolean, rolled_back: string[]}>} Rollback result
 */
export async function cascadingRollback(gate, transactionId) {
  const rolledBack = [];
  const toRollback = [transactionId];

  while (toRollback.length > 0) {
    const txId = toRollback.pop();

    // Rollback this transaction
    const result = await gate.rollbackTransaction(txId);

    if (result.success) {
      rolledBack.push(txId);

      // Find dependent transactions (those with this tx as parent)
      const allTx = gate.txManager.getAllTransactions();
      const dependents = allTx.filter(tx => tx.parentHash === txId);

      for (const depTx of dependents) {
        toRollback.push(depTx.id);
      }
    }
  }

  return {
    success: true,
    rolled_back: rolledBack,
  };
}

// ============================================================================
// Exports
// ============================================================================

export default AtomicAdmissionGate;
