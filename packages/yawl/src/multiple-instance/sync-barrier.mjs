/**
 * @file YAWL Multiple Instance - Synchronization Barrier
 * @module @unrdf/yawl/multiple-instance/sync-barrier
 *
 * @description
 * Implements AND-join synchronization barrier for multiple instances.
 * Blocks until all N instances complete, then releases with aggregated results.
 *
 * Key features:
 * - Wait for N instances to complete
 * - Aggregate receipts and results
 * - Timeout handling (cancels all instances)
 * - Failure handling (configurable: cancel all vs continue)
 * - Thread-safe completion tracking
 */

import { z } from 'zod';

// =============================================================================
// Zod Schemas
// =============================================================================

/**
 * Barrier configuration schema
 */
export const BarrierConfigSchema = z.object({
  /** Number of instances to wait for */
  count: z.number().int().positive(),
  /** Timeout in milliseconds */
  timeout: z.number().int().positive().default(30000),
  /** Cancel all on first failure */
  cancelOnFailure: z.boolean().default(true),
  /** Barrier identifier */
  id: z.string().min(1).optional(),
});

/**
 * Instance completion schema
 */
export const InstanceCompletionSchema = z.object({
  /** Instance identifier */
  instanceId: z.string().min(1),
  /** Completion timestamp */
  timestamp: z.string(),
  /** Instance result data */
  result: z.unknown().optional(),
  /** Instance receipt */
  receipt: z.object({
    id: z.string(),
    taskInstanceId: z.string(),
    action: z.string(),
    timestamp: z.string(),
  }).passthrough().optional(),
  /** Whether instance failed */
  failed: z.boolean().default(false),
  /** Error if instance failed */
  error: z.string().optional(),
});

/**
 * Barrier result schema
 */
export const BarrierResultSchema = z.object({
  /** Whether all instances completed successfully */
  success: z.boolean(),
  /** Number of completed instances */
  completedCount: z.number().int().nonnegative(),
  /** Individual instance results */
  instances: z.array(InstanceCompletionSchema),
  /** Aggregated receipts */
  receipts: z.array(z.object({
    id: z.string(),
  }).passthrough()),
  /** Barrier completion timestamp */
  completionTimestamp: z.string().optional(),
  /** Timeout occurred */
  timedOut: z.boolean().default(false),
  /** Cancellation occurred */
  cancelled: z.boolean().default(false),
  /** Failure messages */
  failures: z.array(z.string()).default([]),
});

// =============================================================================
// SyncBarrier Class
// =============================================================================

/**
 * Synchronization barrier for multiple instances (AND-join)
 *
 * @class
 * @example
 * const barrier = createSyncBarrier(3, { timeout: 10000 });
 * await barrier.arrive({ instanceId: 'i1', result: { data: 1 } });
 * await barrier.arrive({ instanceId: 'i2', result: { data: 2 } });
 * await barrier.arrive({ instanceId: 'i3', result: { data: 3 } });
 * const result = await barrier.wait(); // Resolves when all 3 arrive
 */
export class SyncBarrier {
  /**
   * Create a synchronization barrier
   * @param {number} count - Number of instances to wait for
   * @param {Object} [options] - Barrier options
   * @param {number} [options.timeout=30000] - Timeout in milliseconds
   * @param {boolean} [options.cancelOnFailure=true] - Cancel all on first failure
   * @param {string} [options.id] - Barrier identifier
   */
  constructor(count, options = {}) {
    const config = BarrierConfigSchema.parse({ count, ...options });

    this.id = config.id ?? `barrier-${Date.now()}-${Math.random().toString(36).slice(2)}`;
    this.count = config.count;
    this.timeout = config.timeout;
    this.cancelOnFailure = config.cancelOnFailure;

    this._arrivals = [];
    this._completed = false;
    this._cancelled = false;
    this._timedOut = false;
    this._failures = [];
    this._cancellationHandlers = [];

    this._waitPromise = null;
    this._resolveWait = null;
    this._rejectWait = null;
    this._timeoutHandle = null;
  }

  /**
   * Mark an instance as arrived/completed
   * @param {Object} completion - Instance completion data
   * @param {string} completion.instanceId - Instance identifier
   * @param {*} [completion.result] - Instance result
   * @param {Object} [completion.receipt] - Instance receipt
   * @param {boolean} [completion.failed=false] - Whether instance failed
   * @param {string} [completion.error] - Error message if failed
   * @returns {Promise<void>}
   */
  async arrive(completion) {
    if (this._completed) {
      throw new Error(`Barrier ${this.id} already completed`);
    }

    if (this._cancelled) {
      throw new Error(`Barrier ${this.id} was cancelled`);
    }

    const validated = InstanceCompletionSchema.parse({
      ...completion,
      timestamp: new Date().toISOString(),
    });

    // Handle failure
    if (validated.failed) {
      this._failures.push(validated.error ?? `Instance ${validated.instanceId} failed`);

      if (this.cancelOnFailure) {
        await this._cancel('Cancelling all instances due to failure');
        return;
      }
    }

    // Record arrival
    this._arrivals.push(validated);

    // Check if all instances have arrived
    if (this._arrivals.length >= this.count) {
      this._complete();
    }
  }

  /**
   * Wait for all instances to complete
   * @returns {Promise<Object>} Aggregated result
   * @throws {Error} If barrier times out or is cancelled
   */
  async wait() {
    if (this._waitPromise) {
      return this._waitPromise;
    }

    this._waitPromise = new Promise((resolve, reject) => {
      this._resolveWait = resolve;
      this._rejectWait = reject;

      // Set timeout
      this._timeoutHandle = setTimeout(() => {
        this._handleTimeout();
      }, this.timeout);

      // If already completed, resolve immediately
      if (this._completed) {
        this._complete();
      }
    });

    return this._waitPromise;
  }

  /**
   * Register a cancellation handler
   * @param {Function} handler - Cancellation handler function
   */
  onCancel(handler) {
    if (typeof handler !== 'function') {
      throw new Error('Cancellation handler must be a function');
    }
    this._cancellationHandlers.push(handler);
  }

  /**
   * Get current barrier state
   * @returns {Object} Current state
   */
  getState() {
    return {
      id: this.id,
      count: this.count,
      arrivals: this._arrivals.length,
      completed: this._completed,
      cancelled: this._cancelled,
      timedOut: this._timedOut,
      failures: this._failures.length,
    };
  }

  /**
   * Complete the barrier
   * @private
   */
  _complete() {
    if (this._completed) {
      return;
    }

    this._completed = true;

    // Clear timeout
    if (this._timeoutHandle) {
      clearTimeout(this._timeoutHandle);
      this._timeoutHandle = null;
    }

    // Build result
    const result = {
      success: this._failures.length === 0 && !this._cancelled && !this._timedOut,
      completedCount: this._arrivals.length,
      instances: this._arrivals,
      receipts: this._arrivals
        .filter(a => a.receipt)
        .map(a => a.receipt),
      completionTimestamp: new Date().toISOString(),
      timedOut: this._timedOut,
      cancelled: this._cancelled,
      failures: this._failures,
    };

    const validated = BarrierResultSchema.parse(result);

    if (this._resolveWait) {
      this._resolveWait(validated);
    }
  }

  /**
   * Handle timeout
   * @private
   */
  async _handleTimeout() {
    if (this._completed) {
      return;
    }

    this._timedOut = true;
    await this._cancel('Barrier timeout exceeded');
  }

  /**
   * Cancel the barrier
   * @param {string} reason - Cancellation reason
   * @private
   */
  async _cancel(reason) {
    if (this._cancelled || this._completed) {
      return;
    }

    this._cancelled = true;
    this._failures.push(reason);

    // Invoke cancellation handlers
    for (const handler of this._cancellationHandlers) {
      try {
        await handler(reason);
      } catch (error) {
        console.error('Cancellation handler error:', error);
      }
    }

    this._complete();
  }
}

// =============================================================================
// Factory Function
// =============================================================================

/**
 * Create a synchronization barrier
 * @param {number} count - Number of instances to wait for
 * @param {Object} [options] - Barrier options
 * @param {number} [options.timeout=30000] - Timeout in milliseconds
 * @param {boolean} [options.cancelOnFailure=true] - Cancel all on first failure
 * @param {string} [options.id] - Barrier identifier
 * @returns {SyncBarrier} Synchronization barrier
 *
 * @example
 * const barrier = createSyncBarrier(5);
 * for (let i = 0; i < 5; i++) {
 *   processInstance(i).then(result => barrier.arrive({
 *     instanceId: `instance-${i}`,
 *     result
 *   }));
 * }
 * const aggregated = await barrier.wait();
 */
export function createSyncBarrier(count, options = {}) {
  return new SyncBarrier(count, options);
}
