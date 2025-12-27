/**
 * Budget Enforcer - Resource constraint enforcement
 *
 * Budget:
 *   B := {time ≤ T, steps ≤ S, bytes ≤ M, net ≤ N_allow, root ≤ R}
 *   enforce(B) as guardable preconditions
 *
 * @module @unrdf/kgc-claude/budget-enforcer
 */

import { z } from 'zod';
import { now } from '@unrdf/kgc-4d';
import { blake3 } from 'hash-wasm';

/**
 * Budget definition schema
 */
export const BudgetSchema = z.object({
  /** Maximum time in milliseconds */
  time_ms: z.number().int().positive(),
  /** Maximum number of steps/operations */
  steps: z.number().int().positive(),
  /** Maximum bytes processed */
  bytes: z.number().int().positive(),
  /** Network operations limit */
  net_ops: z.number().int().nonnegative().default(0),
  /** Root operations limit */
  root_ops: z.number().int().nonnegative().default(0),
});

/**
 * @typedef {z.infer<typeof BudgetSchema>} Budget
 */

/**
 * Budget usage schema
 */
export const BudgetUsageSchema = z.object({
  time_ms: z.number().int().nonnegative(),
  steps: z.number().int().nonnegative(),
  bytes: z.number().int().nonnegative(),
  net_ops: z.number().int().nonnegative(),
  root_ops: z.number().int().nonnegative(),
  start_time: z.number().int(),
});

/**
 * @typedef {z.infer<typeof BudgetUsageSchema>} BudgetUsage
 */

/**
 * Budget check result schema
 */
export const BudgetCheckSchema = z.object({
  allowed: z.boolean(),
  exhausted_resources: z.array(z.string()),
  remaining: BudgetUsageSchema.partial(),
  utilization: z.record(z.number()), // percentage used per resource
});

/**
 * @typedef {z.infer<typeof BudgetCheckSchema>} BudgetCheck
 */

/**
 * Budget receipt schema
 */
export const BudgetReceiptSchema = z.object({
  id: z.string(),
  budget_id: z.string(),
  operation: z.string(),
  cost: BudgetUsageSchema.partial(),
  allowed: z.boolean(),
  exhausted: z.array(z.string()),
  t_ns: z.bigint(),
  hash: z.string(),
});

/**
 * @typedef {z.infer<typeof BudgetReceiptSchema>} BudgetReceipt
 */

/**
 * BudgetEnforcer - Tracks and enforces resource budgets
 */
export class BudgetEnforcer {
  /**
   * @param {Budget} budget
   * @param {string} [budgetId]
   */
  constructor(budget, budgetId) {
    this.budget = BudgetSchema.parse(budget);
    this.budgetId = budgetId || `budget-${Date.now()}`;
    this.usage = {
      time_ms: 0,
      steps: 0,
      bytes: 0,
      net_ops: 0,
      root_ops: 0,
      start_time: Date.now(),
    };
    /** @type {BudgetReceipt[]} */
    this.receipts = [];
  }

  /**
   * Update time usage from start time
   */
  updateTimeUsage() {
    this.usage.time_ms = Date.now() - this.usage.start_time;
  }

  /**
   * Check if budget allows operation
   * @param {Partial<BudgetUsage>} cost
   * @returns {BudgetCheck}
   */
  check(cost = {}) {
    this.updateTimeUsage();

    const exhausted = [];
    const remaining = {};
    const utilization = {};

    // Check time
    const projectedTime = this.usage.time_ms + (cost.time_ms || 0);
    remaining.time_ms = this.budget.time_ms - projectedTime;
    utilization.time = projectedTime / this.budget.time_ms;
    if (projectedTime > this.budget.time_ms) {
      exhausted.push('time_ms');
    }

    // Check steps
    const projectedSteps = this.usage.steps + (cost.steps || 0);
    remaining.steps = this.budget.steps - projectedSteps;
    utilization.steps = projectedSteps / this.budget.steps;
    if (projectedSteps > this.budget.steps) {
      exhausted.push('steps');
    }

    // Check bytes
    const projectedBytes = this.usage.bytes + (cost.bytes || 0);
    remaining.bytes = this.budget.bytes - projectedBytes;
    utilization.bytes = projectedBytes / this.budget.bytes;
    if (projectedBytes > this.budget.bytes) {
      exhausted.push('bytes');
    }

    // Check net_ops
    const projectedNet = this.usage.net_ops + (cost.net_ops || 0);
    remaining.net_ops = this.budget.net_ops - projectedNet;
    utilization.net_ops = this.budget.net_ops > 0
      ? projectedNet / this.budget.net_ops
      : 0;
    if (this.budget.net_ops > 0 && projectedNet > this.budget.net_ops) {
      exhausted.push('net_ops');
    }

    // Check root_ops
    const projectedRoot = this.usage.root_ops + (cost.root_ops || 0);
    remaining.root_ops = this.budget.root_ops - projectedRoot;
    utilization.root_ops = this.budget.root_ops > 0
      ? projectedRoot / this.budget.root_ops
      : 0;
    if (this.budget.root_ops > 0 && projectedRoot > this.budget.root_ops) {
      exhausted.push('root_ops');
    }

    return BudgetCheckSchema.parse({
      allowed: exhausted.length === 0,
      exhausted_resources: exhausted,
      remaining,
      utilization,
    });
  }

  /**
   * Consume budget
   * @param {string} operation
   * @param {Partial<BudgetUsage>} cost
   * @returns {Promise<BudgetReceipt>}
   */
  async consume(operation, cost = {}) {
    const checkResult = this.check(cost);

    const t_ns = now();
    const receiptData = JSON.stringify({
      budget_id: this.budgetId,
      operation,
      cost,
      allowed: checkResult.allowed,
      exhausted: checkResult.exhausted_resources,
      t_ns: t_ns.toString(),
    });
    const hash = await blake3(receiptData);

    const receipt = BudgetReceiptSchema.parse({
      id: `budget-receipt-${t_ns}`,
      budget_id: this.budgetId,
      operation,
      cost,
      allowed: checkResult.allowed,
      exhausted: checkResult.exhausted_resources,
      t_ns,
      hash,
    });

    this.receipts.push(receipt);

    // Only consume if allowed
    if (checkResult.allowed) {
      this.usage.steps += cost.steps || 0;
      this.usage.bytes += cost.bytes || 0;
      this.usage.net_ops += cost.net_ops || 0;
      this.usage.root_ops += cost.root_ops || 0;
    }

    return receipt;
  }

  /**
   * Check if any budget is exhausted
   * @returns {boolean}
   */
  isExhausted() {
    return !this.check().allowed;
  }

  /**
   * Get current utilization
   * @returns {Record<string, number>}
   */
  getUtilization() {
    return this.check().utilization;
  }

  /**
   * Get remaining budget
   * @returns {Partial<BudgetUsage>}
   */
  getRemaining() {
    return this.check().remaining;
  }

  /**
   * Get usage
   * @returns {BudgetUsage}
   */
  getUsage() {
    this.updateTimeUsage();
    return { ...this.usage };
  }

  /**
   * Get all receipts
   * @returns {BudgetReceipt[]}
   */
  getReceipts() {
    return [...this.receipts];
  }

  /**
   * Reset usage (but keep budget limits)
   */
  reset() {
    this.usage = {
      time_ms: 0,
      steps: 0,
      bytes: 0,
      net_ops: 0,
      root_ops: 0,
      start_time: Date.now(),
    };
    this.receipts = [];
  }
}

/**
 * Budget enforcement decorator
 * @param {BudgetEnforcer} enforcer
 * @returns {Function}
 */
export function withBudget(enforcer) {
  /**
   * @param {Function} fn
   * @param {string} operation
   * @param {Partial<BudgetUsage>} cost
   * @returns {Function}
   */
  return function budgeted(fn, operation, cost) {
    return async function(...args) {
      const receipt = await enforcer.consume(operation, cost);

      if (!receipt.allowed) {
        return {
          success: false,
          exhausted: receipt.exhausted,
          receipt,
        };
      }

      return fn(...args);
    };
  };
}

/**
 * Create budget enforcer with common defaults
 * @param {Object} [options]
 * @returns {BudgetEnforcer}
 */
export function createBudget(options = {}) {
  return new BudgetEnforcer({
    time_ms: options.time_ms ?? 30000,
    steps: options.steps ?? 100,
    bytes: options.bytes ?? 10 * 1024 * 1024,
    net_ops: options.net_ops ?? 0,
    root_ops: options.root_ops ?? 0,
  });
}

export default BudgetEnforcer;
