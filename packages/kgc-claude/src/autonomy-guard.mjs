/**
 * Autonomy Guard - Bounded autonomy with capacity-limited channels
 *
 * Defines explicit budget/capacity for change per epoch:
 * C_τ = max admissible |Δ|, max tool ops, max touched files, max graph rewrite cost
 *
 * Enforces via guards H and preserve(Q). The actuator cannot exceed
 * the channel capacity regardless of "autonomy mode."
 *
 * @module @unrdf/kgc-claude/autonomy-guard
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { now, toISO } from '@unrdf/kgc-4d';
import { DENIAL_REASONS } from './constants.mjs';

/**
 * Budget configuration schema
 */
export const BudgetSchema = z.object({
  /** Maximum delta operations per epoch */
  maxDeltaSize: z.number().int().positive().default(100),
  /** Maximum tool operations per epoch */
  maxToolOps: z.number().int().positive().default(50),
  /** Maximum files that can be touched per epoch */
  maxFilesTouched: z.number().int().positive().default(20),
  /** Maximum graph rewrite cost (abstract units) */
  maxRewriteCost: z.number().positive().default(1000),
  /** Epoch duration in nanoseconds (default: 1 hour) */
  epochDuration: z.bigint().default(3600000000000n),
});

/**
 * @typedef {z.infer<typeof BudgetSchema>} Budget
 */

/**
 * Usage tracking schema
 */
export const UsageSchema = z.object({
  deltaSize: z.number().int().nonnegative().default(0),
  toolOps: z.number().int().nonnegative().default(0),
  filesTouched: z.number().int().nonnegative().default(0),
  rewriteCost: z.number().nonnegative().default(0),
  epochStart: z.bigint(),
});

/**
 * @typedef {z.infer<typeof UsageSchema>} Usage
 */

/**
 * Denial receipt schema
 */
export const DenialReceiptSchema = z.object({
  id: z.string().uuid(),
  t_ns: z.bigint(),
  timestamp_iso: z.string(),
  reason: z.enum([
    'budget_exceeded',
    'delta_too_large',
    'tools_exceeded',
    'files_exceeded',
    'invariant_violated',
    'shard_conflict',
  ]),
  requested: z.object({
    deltaSize: z.number().optional(),
    toolOps: z.number().optional(),
    filesTouched: z.number().optional(),
    rewriteCost: z.number().optional(),
  }),
  budget: BudgetSchema,
  usage: UsageSchema,
  receiptHash: z.string(),
});

/**
 * @typedef {z.infer<typeof DenialReceiptSchema>} DenialReceipt
 */

/**
 * Generate UUID v4
 * @returns {string}
 */
function generateUUID() {
  if (typeof crypto !== 'undefined' && crypto.randomUUID) {
    return crypto.randomUUID();
  }
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

/**
 * JSON stringify with BigInt support
 * @param {any} obj
 * @returns {string}
 */
function stringifyWithBigInt(obj) {
  return JSON.stringify(obj, (key, value) => {
    if (typeof value === 'bigint') {
      return value.toString();
    }
    return value;
  });
}

/**
 * Create an autonomy guard with specified budget
 *
 * @param {Partial<Budget>} [budgetConfig]
 * @returns {AutonomyGuard}
 *
 * @example
 * const guard = createAutonomyGuard({
 *   maxDeltaSize: 50,
 *   maxToolOps: 25,
 *   maxFilesTouched: 10,
 * });
 *
 * // Check if operation is allowed
 * const result = guard.check({ deltaSize: 5, toolOps: 2, filesTouched: 1 });
 * if (result.allowed) {
 *   // Proceed with operation
 *   guard.consume({ deltaSize: 5, toolOps: 2, filesTouched: 1 });
 * } else {
 *   // Handle denial
 *   console.log('Denied:', result.denialReceipt);
 * }
 */
export function createAutonomyGuard(budgetConfig = {}) {
  const budget = BudgetSchema.parse(budgetConfig);

  /** @type {Usage} */
  let usage = {
    deltaSize: 0,
    toolOps: 0,
    filesTouched: 0,
    rewriteCost: 0,
    epochStart: now(),
  };

  /** @type {Set<string>} */
  const touchedFiles = new Set();

  /** @type {DenialReceipt[]} */
  const denialHistory = [];

  /**
   * Check if current epoch has expired
   */
  function checkEpochExpiry() {
    const currentTime = now();
    if (currentTime - usage.epochStart > budget.epochDuration) {
      // Reset usage for new epoch
      usage = {
        deltaSize: 0,
        toolOps: 0,
        filesTouched: 0,
        rewriteCost: 0,
        epochStart: currentTime,
      };
      touchedFiles.clear();
    }
  }

  /**
   * Create a denial receipt
   * @param {string} reason
   * @param {Object} requested
   * @returns {Promise<DenialReceipt>}
   */
  async function createDenialReceipt(reason, requested) {
    const id = generateUUID();
    const t_ns = now();

    const receiptContent = {
      id,
      t_ns: t_ns.toString(),
      reason,
      requested,
      budget,
      usage,
    };
    const receiptHash = await blake3(stringifyWithBigInt(receiptContent));

    const receipt = DenialReceiptSchema.parse({
      id,
      t_ns,
      timestamp_iso: toISO(t_ns),
      reason,
      requested,
      budget,
      usage,
      receiptHash,
    });

    denialHistory.push(receipt);
    return receipt;
  }

  return {
    /**
     * Get current budget configuration
     * @returns {Budget}
     */
    getBudget() {
      return { ...budget };
    },

    /**
     * Get current usage
     * @returns {Usage}
     */
    getUsage() {
      checkEpochExpiry();
      return { ...usage };
    },

    /**
     * Get remaining capacity
     * @returns {{ deltaSize: number, toolOps: number, filesTouched: number, rewriteCost: number }}
     */
    getRemaining() {
      checkEpochExpiry();
      return {
        deltaSize: budget.maxDeltaSize - usage.deltaSize,
        toolOps: budget.maxToolOps - usage.toolOps,
        filesTouched: budget.maxFilesTouched - usage.filesTouched,
        rewriteCost: budget.maxRewriteCost - usage.rewriteCost,
      };
    },

    /**
     * Check if an operation is allowed within budget
     *
     * @param {Object} request
     * @param {number} [request.deltaSize] - Number of delta operations
     * @param {number} [request.toolOps] - Number of tool operations
     * @param {string[]} [request.files] - Files to touch
     * @param {number} [request.rewriteCost] - Rewrite cost
     * @returns {Promise<{ allowed: boolean, denialReceipt?: DenialReceipt }>}
     */
    async check(request) {
      checkEpochExpiry();

      const newDeltaSize = usage.deltaSize + (request.deltaSize || 0);
      const newToolOps = usage.toolOps + (request.toolOps || 0);
      const newRewriteCost = usage.rewriteCost + (request.rewriteCost || 0);

      // Calculate new files touched
      const newFiles = (request.files || []).filter((f) => !touchedFiles.has(f));
      const newFilesTouched = usage.filesTouched + newFiles.length;

      // Check delta size
      if (newDeltaSize > budget.maxDeltaSize) {
        const receipt = await createDenialReceipt(DENIAL_REASONS.DELTA_TOO_LARGE, {
          deltaSize: request.deltaSize,
        });
        return { allowed: false, denialReceipt: receipt };
      }

      // Check tool ops
      if (newToolOps > budget.maxToolOps) {
        const receipt = await createDenialReceipt(DENIAL_REASONS.TOOLS_EXCEEDED, {
          toolOps: request.toolOps,
        });
        return { allowed: false, denialReceipt: receipt };
      }

      // Check files touched
      if (newFilesTouched > budget.maxFilesTouched) {
        const receipt = await createDenialReceipt(DENIAL_REASONS.FILES_EXCEEDED, {
          filesTouched: newFiles.length,
        });
        return { allowed: false, denialReceipt: receipt };
      }

      // Check rewrite cost
      if (newRewriteCost > budget.maxRewriteCost) {
        const receipt = await createDenialReceipt(DENIAL_REASONS.BUDGET_EXCEEDED, {
          rewriteCost: request.rewriteCost,
        });
        return { allowed: false, denialReceipt: receipt };
      }

      return { allowed: true };
    },

    /**
     * Consume budget for an allowed operation
     *
     * @param {Object} consumption
     * @param {number} [consumption.deltaSize]
     * @param {number} [consumption.toolOps]
     * @param {string[]} [consumption.files]
     * @param {number} [consumption.rewriteCost]
     */
    consume(consumption) {
      checkEpochExpiry();

      usage.deltaSize += consumption.deltaSize || 0;
      usage.toolOps += consumption.toolOps || 0;
      usage.rewriteCost += consumption.rewriteCost || 0;

      for (const file of consumption.files || []) {
        if (!touchedFiles.has(file)) {
          touchedFiles.add(file);
          usage.filesTouched++;
        }
      }
    },

    /**
     * Get denial history
     * @returns {DenialReceipt[]}
     */
    getDenialHistory() {
      return [...denialHistory];
    },

    /**
     * Reset epoch (for testing or manual reset)
     */
    resetEpoch() {
      usage = {
        deltaSize: 0,
        toolOps: 0,
        filesTouched: 0,
        rewriteCost: 0,
        epochStart: now(),
      };
      touchedFiles.clear();
    },

    /**
     * Create a scoped guard with reduced budget
     *
     * @param {Partial<Budget>} scopedBudget
     * @returns {AutonomyGuard}
     */
    createScope(scopedBudget) {
      const remaining = this.getRemaining();
      return createAutonomyGuard({
        maxDeltaSize: Math.min(scopedBudget.maxDeltaSize || remaining.deltaSize, remaining.deltaSize),
        maxToolOps: Math.min(scopedBudget.maxToolOps || remaining.toolOps, remaining.toolOps),
        maxFilesTouched: Math.min(scopedBudget.maxFilesTouched || remaining.filesTouched, remaining.filesTouched),
        maxRewriteCost: Math.min(scopedBudget.maxRewriteCost || remaining.rewriteCost, remaining.rewriteCost),
        epochDuration: budget.epochDuration,
      });
    },
  };
}

/**
 * @typedef {ReturnType<typeof createAutonomyGuard>} AutonomyGuard
 */

/**
 * Create a guard-protected execution context
 *
 * @param {AutonomyGuard} guard - Autonomy guard
 * @param {Object} estimate - Estimated resource usage
 * @param {Function} operation - Operation to execute
 * @returns {Promise<{ success: boolean, result?: any, denialReceipt?: DenialReceipt }>}
 *
 * @example
 * const result = await withGuard(guard, { deltaSize: 5, toolOps: 2 }, async () => {
 *   // Do work
 *   return someResult;
 * });
 *
 * if (!result.success) {
 *   console.log('Denied:', result.denialReceipt.reason);
 * }
 */
export async function withGuard(guard, estimate, operation) {
  const check = await guard.check(estimate);

  if (!check.allowed) {
    return { success: false, denialReceipt: check.denialReceipt };
  }

  try {
    const result = await operation();
    guard.consume(estimate);
    return { success: true, result };
  } catch (error) {
    // Don't consume budget on failure
    throw error;
  }
}

/**
 * Create a default production guard with reasonable limits
 * @returns {AutonomyGuard}
 */
export function createProductionGuard() {
  return createAutonomyGuard({
    maxDeltaSize: 500,
    maxToolOps: 100,
    maxFilesTouched: 50,
    maxRewriteCost: 10000,
    epochDuration: 3600000000000n, // 1 hour
  });
}

/**
 * Create a strict guard for high-risk operations
 * @returns {AutonomyGuard}
 */
export function createStrictGuard() {
  return createAutonomyGuard({
    maxDeltaSize: 50,
    maxToolOps: 10,
    maxFilesTouched: 5,
    maxRewriteCost: 500,
    epochDuration: 1800000000000n, // 30 minutes
  });
}
