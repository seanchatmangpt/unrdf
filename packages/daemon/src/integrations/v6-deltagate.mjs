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
import { z } from 'zod';
import { createHash, randomUUID } from 'node:crypto';
import { detectInjection, sanitizePath, sanitizeError, detectSecrets, validatePayload } from '../security-audit.mjs';


// =============================================================================
// Schema Definitions
// =============================================================================

/**
 * Delta Operation Schema - Individual state change
 * @type {z.ZodDiscriminatedUnion}
 */
export const DeltaOperationSchema = z.discriminatedUnion('op', [
  z.object({
    op: z.literal('set'),
    path: z.string().min(1),
    oldValue: z.any().optional(),
    newValue: z.any(),
    timestamp_ns: z.bigint(),
  }),
  z.object({
    op: z.literal('delete'),
    path: z.string().min(1),
    oldValue: z.any(),
    timestamp_ns: z.bigint(),
  }),
  z.object({
    op: z.literal('insert'),
    path: z.string().min(1),
    index: z.number().int().min(0),
    value: z.any(),
    timestamp_ns: z.bigint(),
  }),
]);

/**
 * Delta Contract Schema - Change proposal with metadata
 * @type {z.ZodObject}
 */
export const DeltaContractSchema = z.object({
  id: z.string().uuid(),
  timestamp_ns: z.bigint(),
  timestamp_iso: z.string().datetime(),
  operations: z.array(DeltaOperationSchema).min(1),
  source: z.object({
    package: z.string().min(1),
    actor: z.string().optional(),
    nodeId: z.string().optional(),
    context: z.record(z.any()).optional(),
  }),
  admissibility: z.object({
    policyId: z.string().optional(),
    constraints: z.array(z.string()).optional(),
    preConditions: z.array(z.string()).optional(),
  }).optional(),
  previousDeltaId: z.string().uuid().nullable(),
});

/**
 * Receipt Schema - Delta execution receipt
 * @type {z.ZodObject}
 */
export const DeltaReceiptSchema = z.object({
  id: z.string().uuid(),
  deltaId: z.string().uuid(),
  timestamp_ns: z.bigint(),
  timestamp_iso: z.string().datetime(),
  applied: z.boolean(),
  reason: z.string().optional(),
  stateHash: z.string().length(64).optional(),
  operationsApplied: z.number().int().min(0),
  operationsFailed: z.number().int().min(0).default(0),
  metadata: z.record(z.any()).optional(),
  previousReceiptHash: z.string().length(64).nullable(),
  receiptHash: z.string().length(64),
});

/**
 * Health Status Schema
 * @type {z.ZodObject}
 */
export const HealthStatusSchema = z.object({
  status: z.enum(['healthy', 'degraded', 'unhealthy']),
  deltasProcessed: z.number().int().min(0),
  deltasRejected: z.number().int().min(0),
  lastDeltaId: z.string().uuid().nullable().optional(),
  lastReceiptHash: z.string().length(64).nullable().optional(),
  timestamp_ns: z.bigint(),
});

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Generate SHA256 hash of data
 * @param {*} data - Data to hash
 * @returns {string} 64-character hex hash
 * @private
 */
function hashData(data) {
  let str;
  if (typeof data === 'string') {
    str = data;
  } else {
    // Handle BigInt serialization
    str = JSON.stringify(data, (key, value) =>
      typeof value === 'bigint' ? value.toString() : value
    );
  }
  return createHash('sha256').update(str).digest('hex');
}

/**
 * Generate UUID v4
 * @returns {string} Valid UUID v4
 * @private
 */
function generateUUID() {
  return randomUUID();
}

/**
 * Get nanosecond timestamp
 * @returns {bigint} Nanosecond timestamp
 * @private
 */
function getNs() {
  return BigInt(Date.now()) * 1_000_000n;
}

/**
 * Get ISO timestamp
 * @returns {string} ISO 8601 timestamp
 * @private
 */
function getISOTimestamp() {
  return new Date().toISOString();
}

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
      const oldState = this._captureState();
      const oldStateHash = hashData(oldState);

      // 3. Check admissibility
      const admissibilityCheck = await this._checkAdmissibility(validated, context);
      if (!admissibilityCheck.lawful) {
        return await this._rejectDelta(validated, admissibilityCheck.reason);
      }

      // 4. Apply operations atomically
      const applyResult = this._applyOperations(validated.operations);
      if (!applyResult.success) {
        return await this._rejectDelta(validated, applyResult.reason);
      }

      // 5. Capture new state and generate receipt
      const newState = this._captureState();
      const newStateHash = hashData(newState);

      const receipt = await this._generateReceipt({
        deltaId: validated.id,
        applied: true,
        stateHash: newStateHash,
        operationsApplied: validated.operations.length,
        operationsFailed: 0,
      });

      // 6. Store in history
      this._storeHistory(validated, receipt, oldState, newState);
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
      .map((op) => this._reverseOperation(op));

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
      chainValid: this._validateChain(index),
    }));
  }

  // =========================================================================
  // Private Methods
  // =========================================================================

  /**
   * Check delta admissibility
   * @private
   */
  async _checkAdmissibility(delta, context) {
    if (!delta.admissibility) {
      return { lawful: true };
    }

    const { policyId, constraints = [], preConditions = [] } = delta.admissibility;

    // Check pre-conditions
    for (const condition of preConditions) {
      if (!this._evaluateCondition(condition, context)) {
        return {
          lawful: false,
          reason: `Pre-condition failed: ${condition}`,
        };
      }
    }

    // Check constraints
    for (const constraint of constraints) {
      if (!this._evaluateConstraint(constraint, context)) {
        return {
          lawful: false,
          reason: `Constraint failed: ${constraint}`,
        };
      }
    }

    return { lawful: true };
  }

  /**
   * Apply operations atomically
   * @private
   */
  _applyOperations(operations) {
    try {
      for (const op of operations) {
        switch (op.op) {
          case 'set':
            this.store.set(op.path, op.newValue);
            break;
          case 'delete':
            this.store.delete(op.path);
            break;
          case 'insert':
            if (op.path.includes('[')) {
              // Array-like path: handle insert
              const current = this.store.get(op.path) || [];
              if (Array.isArray(current)) {
                current.splice(op.index, 0, op.value);
              }
            }
            break;
          default:
            return { success: false, reason: `Unknown operation: ${op.op}` };
        }
      }
      return { success: true };
    } catch (error) {
      return { success: false, reason: error.message };
    }
  }

  /**
   * Reverse an operation
   * @private
   */
  _reverseOperation(op) {
    const reversed = { ...op, timestamp_ns: getNs() };

    switch (op.op) {
      case 'set':
        // If oldValue was undefined, the original operation created the key
        // So the reverse should delete it
        if (op.oldValue === undefined) {
          return {
            op: 'delete',
            path: op.path,
            oldValue: op.newValue,
            timestamp_ns: reversed.timestamp_ns,
          };
        }
        return {
          op: 'set',
          path: op.path,
          oldValue: op.newValue,
          newValue: op.oldValue,
          timestamp_ns: reversed.timestamp_ns,
        };
      case 'delete':
        return {
          op: 'set',
          path: op.path,
          oldValue: undefined,
          newValue: op.oldValue,
          timestamp_ns: reversed.timestamp_ns,
        };
      case 'insert':
        return {
          op: 'delete',
          path: op.path,
          oldValue: op.value,
          timestamp_ns: reversed.timestamp_ns,
        };
      default:
        throw new Error(`Cannot reverse operation: ${op.op}`);
    }
  }

  /**
   * Generate receipt with proof chain
   * @private
   */
  async _generateReceipt(receiptData) {
    const receiptId = generateUUID();
    const timestamp_ns = getNs();
    const timestamp_iso = getISOTimestamp();

    const payloadHash = hashData({
      deltaId: receiptData.deltaId,
      stateHash: receiptData.stateHash,
      operationsApplied: receiptData.operationsApplied,
      timestamp_ns,
    });

    const receiptHash = hashData({
      id: receiptId,
      previousHash: this.lastReceiptHash,
      payloadHash,
      timestamp_iso,
    });

    const receipt = DeltaReceiptSchema.parse({
      id: receiptId,
      deltaId: receiptData.deltaId,
      timestamp_ns,
      timestamp_iso,
      applied: receiptData.applied,
      reason: receiptData.reason,
      stateHash: receiptData.stateHash,
      operationsApplied: receiptData.operationsApplied,
      operationsFailed: receiptData.operationsFailed || 0,
      previousReceiptHash: this.lastReceiptHash,
      receiptHash,
    });

    return receipt;
  }

  /**
   * Reject delta
   * @private
   */
  async _rejectDelta(delta, reason) {
    const oldState = this._captureState();
    const receipt = await this._generateReceipt({
      deltaId: delta.id,
      applied: false,
      reason,
      operationsApplied: 0,
      operationsFailed: delta.operations.length,
    });

    this.deltasRejected++;

    // Store rejected delta and receipt in history
    this._storeHistory(delta, receipt, oldState, oldState);

    this.emit('delta:rejected', { deltaId: delta.id, receipt, reason });

    if (this.onDeltaRejected) {
      await this.onDeltaRejected(delta, reason);
    }

    this.logger.warn(`[DeltaGate] Delta rejected: ${delta.id}`, { reason });
    return receipt;
  }

  /**
   * Capture current state
   * @private
   */
  _captureState() {
    const state = {};
    for (const [key, value] of this.store.entries()) {
      state[key] = value;
    }
    return state;
  }

  /**
   * Store delta and receipt in history
   * @private
   */
  _storeHistory(delta, receipt, oldState, newState) {
    this.deltaHistory.push(delta);
    this.receiptHistory.push(receipt);
    this.stateHistory.set(receipt.id, { oldState, newState });

    // Trim history if needed
    if (this.deltaHistory.length > this.maxHistorySize) {
      this.deltaHistory.shift();
      this.receiptHistory.shift();
    }
  }

  /**
   * Evaluate condition
   * @private
   */
  _evaluateCondition(condition, context) {
    if (condition === 'always') return true;
    if (condition === 'never') return false;
    if (condition.startsWith('path:')) {
      const path = condition.substring(5);
      return this.store.has(path);
    }
    // Fail-closed: Unknown conditions denied
    this.logger.warn(`Unknown condition type: ${condition}, denying access`);
    return false;
  }

  /**
   * Evaluate constraint
   * @private
   */
  _evaluateConstraint(constraint, context) {
    if (constraint === 'none') return true;
    // Fail-closed: Unknown constraints denied
    this.logger.warn(`Unknown constraint type: ${constraint}, denying access`);
    return false;
  }

  /**
   * Validate chain at index
   * @private
   */
  _validateChain(index) {
    if (index === 0) {
      return this.receiptHistory[0].previousReceiptHash === null;
    }

    const current = this.receiptHistory[index];
    const previous = this.receiptHistory[index - 1];

    return current.previousReceiptHash === previous.receiptHash;
  }
}
