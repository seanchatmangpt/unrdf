/**
 * @file Daemon ΔGate (DeltaGate) Integration
 * @module @unrdf/daemon/integrations/v6-deltagate
 * @description Integrates UNRDF v6 ΔGate control plane with daemon background operations.
 * Provides unified receipt coordination, delta contract validation, state tracking,
 * and rollback support for cross-package delta flows.
 *
 * KEY FEATURES:
 * - Delta contract validation before operation execution
 * - Deterministic receipt generation with ΔGate schema
 * - Differential state tracking (old→new)
 * - Rollback support via delta reversal
 * - Cross-package delta coordination (YAWL, streaming, hooks)
 * - Health status tracked via deltas
 */

import { EventEmitter } from 'events';

// Import schemas
import {
  DeltaOperationSchema,
  DeltaContractSchema,
  DeltaReceiptSchema,
  HealthStatusSchema,
} from './v6-deltagate.schema.mjs';

// Import helpers
import { hashData, generateUUID, getNs, getISOTimestamp } from './v6-deltagate.helpers.mjs';

// Import evaluators
import {
  checkAdmissibility,
  applyOperations,
  reverseOperation,
  evaluateCondition,
  evaluateConstraint,
  validateChain,
} from './v6-deltagate.evaluators.mjs';

// Import receipt generation
import { generateReceipt } from './v6-deltagate.receipts.mjs';

// Import state management
import { captureState, storeHistory } from './v6-deltagate.state.mjs';

// Re-export schemas for external use
export {
  DeltaOperationSchema,
  DeltaContractSchema,
  DeltaReceiptSchema,
  HealthStatusSchema,
};

// =============================================================================
// DaemonDeltaGate Class
// =============================================================================

/**
 * Daemon ΔGate - Unified receipt coordination and delta validation
 *
 * Bridges daemon scheduling with v6-core delta gate operations.
 * Enforces admissibility policies, generates receipts, tracks state,
 * and supports rollback via delta reversal.
 *
 * @class
 * @extends EventEmitter
 *
 * @example
 * const gate = new DaemonDeltaGate({ daemonId: 'my-daemon' });
 * const receipt = await gate.proposeDelta(delta);
 * if (receipt.applied) {
 *   console.log('Delta applied:', receipt.stateHash);
 * }
 */
export class DaemonDeltaGate extends EventEmitter {
  /**
   * Create a new DaemonDeltaGate instance
   *
   * @param {Object} options - Configuration
   * @param {string} options.daemonId - Unique daemon identifier
   * @param {Object} [options.store] - State store (defaults to in-memory)
   * @param {Function} [options.onDeltaApplied] - Delta applied hook
   * @param {Function} [options.onDeltaRejected] - Delta rejected hook
   * @param {Object} [options.logger] - Logger instance
   * @param {number} [options.maxHistorySize=1000] - Max receipt history
   */
  constructor(options = {}) {
    super();

    this.daemonId = options.daemonId || `daemon-${generateUUID()}`;
    this.store = options.store || new Map();
    this.onDeltaApplied = options.onDeltaApplied;
    this.onDeltaRejected = options.onDeltaRejected;
    this.logger = options.logger || console;
    this.maxHistorySize = options.maxHistorySize || 1000;

    // Internal state
    this.deltaHistory = [];
    this.receiptHistory = [];
    this.stateHistory = new Map();
    this.deltasProcessed = 0;
    this.deltasRejected = 0;
    this.lastReceiptHash = null;
    this.lastDeltaId = null;

    // Coordinator for cross-package deltas
    this.coordinators = new Map();
  }

  /**
   * Propose delta for application
   *
   * 1. Validate delta contract
   * 2. Capture old state
   * 3. Check admissibility
   * 4. Apply operations atomically
   * 5. Generate receipt with proof chain
   * 6. Emit events for cross-package coordination
   *
   * @param {Object} delta - Delta contract to apply
   * @param {Object} [context={}] - Execution context
   * @returns {Promise<Object>} Receipt with stateHash and proof chain
   * @throws {Error} On validation failure
   *
   * @example
   * const delta = {
   *   id: uuid(),
   *   operations: [
   *     { op: 'set', path: 'status', newValue: 'running' }
   *   ],
   *   source: { package: '@unrdf/yawl', actor: 'system' }
   * };
   * const receipt = await gate.proposeDelta(delta);
   */
  async proposeDelta(delta, context = {}) {
    const startNs = getNs();

    try {
      // 1. Validate contract
      const validated = DeltaContractSchema.parse(delta);
      this.logger.debug(`[DeltaGate] Validating delta ${validated.id}`);

      // 2. Capture old state
      const oldState = captureState(this.store);
      const oldStateHash = hashData(oldState);

      // 3. Check admissibility
      const admissibilityCheck = await checkAdmissibility(
        validated,
        context,
        (condition, ctx) => evaluateCondition(condition, ctx, this.store, this.logger),
        (constraint, ctx) => evaluateConstraint(constraint, ctx, this.logger)
      );

      if (!admissibilityCheck.lawful) {
        return await this._rejectDelta(validated, admissibilityCheck.reason);
      }

      // 4. Apply operations atomically
      const applyResult = applyOperations(validated.operations, this.store);
      if (!applyResult.success) {
        return await this._rejectDelta(validated, applyResult.reason);
      }

      // 5. Capture new state and generate receipt
      const newState = captureState(this.store);
      const newStateHash = hashData(newState);

      const receipt = await generateReceipt(
        {
          deltaId: validated.id,
          applied: true,
          stateHash: newStateHash,
          operationsApplied: validated.operations.length,
          operationsFailed: 0,
        },
        this.lastReceiptHash
      );

      // 6. Store in history
      storeHistory(
        this.deltaHistory,
        this.receiptHistory,
        this.stateHistory,
        validated,
        receipt,
        oldState,
        newState,
        this.maxHistorySize
      );

      this.deltasProcessed++;
      this.lastDeltaId = validated.id;
      this.lastReceiptHash = receipt.receiptHash;

      // 7. Emit events for coordination
      const endNs = getNs();
      this.emit('delta:applied', {
        deltaId: validated.id,
        receipt,
        oldStateHash,
        newStateHash,
        elapsedNs: Number(endNs - startNs),
      });

      // 8. Invoke hooks
      if (this.onDeltaApplied) {
        await this.onDeltaApplied(validated, receipt);
      }

      this.logger.info(`[DeltaGate] Delta applied: ${validated.id}`, { stateHash: newStateHash });
      return receipt;
    } catch (error) {
      this.logger.error(`[DeltaGate] Delta proposal failed: ${error.message}`, { deltaId: delta?.id });
      throw error;
    }
  }

  /**
   * Propose rollback via reversed delta
   *
   * Reverses previous delta by proposing inverse operations.
   * Validates that current state matches expected state before rollback.
   *
   * @param {string} receiptId - Receipt ID to rollback
   * @param {Object} [context={}] - Execution context
   * @returns {Promise<Object>} Rollback receipt
   * @throws {Error} If receipt not found or state mismatch
   *
   * @example
   * const rollbackReceipt = await gate.rollback(receiptId);
   */
  async rollback(receiptId, context = {}) {
    const receipt = this.receiptHistory.find((r) => r.id === receiptId);
    if (!receipt) {
      throw new Error(`Receipt not found: ${receiptId}`);
    }

    if (!receipt.applied) {
      throw new Error(`Cannot rollback rejected receipt: ${receiptId}`);
    }

    const originalDelta = this.deltaHistory.find((d) => d.id === receipt.deltaId);
    if (!originalDelta) {
      throw new Error(`Original delta not found: ${receipt.deltaId}`);
    }

    // Create reversed delta
    const reversedOperations = originalDelta.operations
      .slice()
      .reverse()
      .map((op) => reverseOperation(op));

    const reversedDelta = DeltaContractSchema.parse({
      id: generateUUID(),
      timestamp_ns: getNs(),
      timestamp_iso: getISOTimestamp(),
      operations: reversedOperations,
      source: {
        package: '@unrdf/daemon',
        actor: 'rollback',
        nodeId: this.daemonId,
      },
      previousDeltaId: receipt.deltaId,
    });

    return this.proposeDelta(reversedDelta, context);
  }

  /**
   * Register cross-package coordinator
   *
   * Allows other packages (YAWL, streaming, hooks) to coordinate deltas.
   *
   * @param {string} packageName - Package name (e.g., '@unrdf/yawl')
   * @param {Object} coordinator - Coordinator implementation
   * @returns {void}
   *
   * @example
   * gate.registerCoordinator('@unrdf/yawl', yawlCoordinator);
   */
  registerCoordinator(packageName, coordinator) {
    this.coordinators.set(packageName, coordinator);
  }

  /**
   * Get delta by ID
   *
   * @param {string} deltaId - Delta ID
   * @returns {Object|null} Delta or null if not found
   */
  getDelta(deltaId) {
    return this.deltaHistory.find((d) => d.id === deltaId) || null;
  }

  /**
   * Get receipt by ID
   *
   * @param {string} receiptId - Receipt ID
   * @returns {Object|null} Receipt or null if not found
   */
  getReceipt(receiptId) {
    return this.receiptHistory.find((r) => r.id === receiptId) || null;
  }

  /**
   * Get health status
   *
   * Returns operational health metrics tracked via delta processing.
   *
   * @returns {Object} Health status
   *
   * @example
   * const health = gate.getHealthStatus();
   * console.log(health.status); // 'healthy' | 'degraded' | 'unhealthy'
   */
  getHealthStatus() {
    const totalAttempts = this.deltasProcessed + this.deltasRejected;
    let status = 'healthy';

    if (totalAttempts === 0) {
      status = 'healthy';
    } else if (this.deltasRejected / totalAttempts > 0.1) {
      status = 'degraded';
    } else if (this.deltasRejected / totalAttempts > 0.3) {
      status = 'unhealthy';
    }

    return HealthStatusSchema.parse({
      status,
      deltasProcessed: this.deltasProcessed,
      deltasRejected: this.deltasRejected,
      lastDeltaId: this.lastDeltaId,
      lastReceiptHash: this.lastReceiptHash,
      timestamp_ns: getNs(),
    });
  }

  /**
   * Get receipt chain (proof chain)
   *
   * Returns ordered list of receipts with proof chain validation.
   *
   * @returns {Array<Object>} Ordered receipts with chain validation
   */
  getReceiptChain() {
    return this.receiptHistory.map((receipt, index) => ({
      ...receipt,
      chainValid: validateChain(this.receiptHistory, index),
    }));
  }

  // =========================================================================
  // Private Methods
  // =========================================================================

  /**
   * Reject delta
   * @private
   */
  async _rejectDelta(delta, reason) {
    const oldState = captureState(this.store);
    const receipt = await generateReceipt(
      {
        deltaId: delta.id,
        applied: false,
        reason,
        operationsApplied: 0,
        operationsFailed: delta.operations.length,
      },
      this.lastReceiptHash
    );

    this.deltasRejected++;

    // Store rejected delta and receipt in history
    storeHistory(
      this.deltaHistory,
      this.receiptHistory,
      this.stateHistory,
      delta,
      receipt,
      oldState,
      oldState,
      this.maxHistorySize
    );

    this.emit('delta:rejected', { deltaId: delta.id, receipt, reason });

    if (this.onDeltaRejected) {
      await this.onDeltaRejected(delta, reason);
    }

    this.logger.warn(`[DeltaGate] Delta rejected: ${delta.id}`, { reason });
    return receipt;
  }
}
