/**
 * @fileoverview Bulkhead Isolation Pattern
 * Resource pools with separate limits to prevent cascade failures
 */

import { z } from 'zod';
import { generateReceipt } from './receipt.mjs';

/**
 * Bulkhead configuration schema
 */
export const BulkheadConfigSchema = z.object({
  name: z.string(),
  maxConcurrent: z.number().positive(),
  maxQueueSize: z.number().nonnegative(),
  timeout: z.number().positive().optional(),
});

/**
 * @typedef {z.infer<typeof BulkheadConfigSchema>} BulkheadConfig
 */

/**
 * Bulkhead statistics
 * @typedef {{
 *   name: string,
 *   active: number,
 *   queued: number,
 *   completed: number,
 *   rejected: number,
 *   timedOut: number,
 *   maxConcurrent: number,
 * }} BulkheadStats
 */

/**
 * Bulkhead isolation manager for resource pools
 * Prevents cascade failures by isolating resources
 */
export class BulkheadManager {
  /**
   * @param {BulkheadConfig} config - Bulkhead configuration
   */
  constructor(config) {
    this.config = BulkheadConfigSchema.parse(config);
    this.active = new Set();
    this.queue = [];
    this.stats = {
      name: config.name,
      active: 0,
      queued: 0,
      completed: 0,
      rejected: 0,
      timedOut: 0,
      maxConcurrent: config.maxConcurrent,
    };
  }

  /**
   * Execute function with bulkhead protection
   * @template T
   * @param {() => Promise<T>} fn - Async function to execute
   * @param {object} [options] - Execution options
   * @param {number} [options.priority=0] - Execution priority (higher = sooner)
   * @returns {Promise<T>} Function result
   */
  async execute(fn, options = {}) {
    const priority = options.priority ?? 0;

    // Check if at capacity
    if (this.active.size >= this.config.maxConcurrent) {
      // Check queue capacity
      if (this.queue.length >= this.config.maxQueueSize) {
        this.stats.rejected++;
        const error = new Error(`Bulkhead '${this.config.name}' queue full (max: ${this.config.maxQueueSize})`);
        error.code = 'BULKHEAD_QUEUE_FULL';
        throw error;
      }

      // Add to queue
      return new Promise((resolve, reject) => {
        this.queue.push({ fn, resolve, reject, priority });
        this.queue.sort((a, b) => b.priority - a.priority); // Higher priority first
        this.stats.queued = this.queue.length;
      });
    }

    // Execute immediately
    return this._executeImmediate(fn);
  }

  /**
   * Execute function immediately (internal)
   * @template T
   * @param {() => Promise<T>} fn - Function to execute
   * @returns {Promise<T>} Function result
   * @private
   */
  async _executeImmediate(fn) {
    const taskId = Symbol('task');
    this.active.add(taskId);
    this.stats.active = this.active.size;

    try {
      // Apply timeout if configured
      const result = this.config.timeout
        ? await this._withTimeout(fn(), this.config.timeout)
        : await fn();

      this.stats.completed++;
      return result;
    } catch (error) {
      if (error.code === 'BULKHEAD_TIMEOUT') {
        this.stats.timedOut++;
      }
      throw error;
    } finally {
      this.active.delete(taskId);
      this.stats.active = this.active.size;
      this._processQueue();
    }
  }

  /**
   * Process queued tasks
   * @private
   */
  _processQueue() {
    while (this.queue.length > 0 && this.active.size < this.config.maxConcurrent) {
      const task = this.queue.shift();
      this.stats.queued = this.queue.length;

      this._executeImmediate(task.fn)
        .then(task.resolve)
        .catch(task.reject);
    }
  }

  /**
   * Wrap promise with timeout
   * @template T
   * @param {Promise<T>} promise - Promise to wrap
   * @param {number} timeout - Timeout in milliseconds
   * @returns {Promise<T>} Promise with timeout
   * @private
   */
  async _withTimeout(promise, timeout) {
    return Promise.race([
      promise,
      new Promise((_, reject) => {
        setTimeout(() => {
          const error = new Error(`Bulkhead '${this.config.name}' operation timed out after ${timeout}ms`);
          error.code = 'BULKHEAD_TIMEOUT';
          reject(error);
        }, timeout);
      }),
    ]);
  }

  /**
   * Get current statistics
   * @returns {BulkheadStats} Current statistics
   */
  getStats() {
    return { ...this.stats };
  }

  /**
   * Clear queue (reject all pending)
   * @returns {number} Number of tasks rejected
   */
  clearQueue() {
    const count = this.queue.length;
    const error = new Error(`Bulkhead '${this.config.name}' queue cleared`);
    error.code = 'BULKHEAD_CLEARED';

    for (const task of this.queue) {
      task.reject(error);
    }

    this.queue = [];
    this.stats.queued = 0;
    this.stats.rejected += count;

    return count;
  }

  /**
   * Wait for all active tasks to complete
   * @returns {Promise<void>}
   */
  async drain() {
    while (this.active.size > 0 || this.queue.length > 0) {
      await new Promise(resolve => setTimeout(resolve, 10));
    }
  }

  /**
   * Generate receipt for bulkhead operation
   * @param {string} operation - Operation name
   * @param {Record<string, any>} inputs - Operation inputs
   * @param {Record<string, any>} outputs - Operation outputs
   * @returns {Promise<import('./receipt.mjs').Receipt>} Receipt
   */
  async generateReceipt(operation, inputs, outputs) {
    return generateReceipt(
      `bulkhead:${this.config.name}:${operation}`,
      { ...inputs, bulkhead: this.config.name },
      { ...outputs, stats: this.getStats() }
    );
  }
}

/**
 * Multi-bulkhead coordinator for managing multiple resource pools
 */
export class BulkheadCoordinator {
  constructor() {
    /** @type {Map<string, BulkheadManager>} */
    this.bulkheads = new Map();
  }

  /**
   * Register a bulkhead
   * @param {BulkheadConfig} config - Bulkhead configuration
   * @returns {BulkheadManager} Created bulkhead
   */
  register(config) {
    const bulkhead = new BulkheadManager(config);
    this.bulkheads.set(config.name, bulkhead);
    return bulkhead;
  }

  /**
   * Get bulkhead by name
   * @param {string} name - Bulkhead name
   * @returns {BulkheadManager | undefined} Bulkhead manager
   */
  get(name) {
    return this.bulkheads.get(name);
  }

  /**
   * Execute on named bulkhead
   * @template T
   * @param {string} name - Bulkhead name
   * @param {() => Promise<T>} fn - Function to execute
   * @param {object} [options] - Execution options
   * @returns {Promise<T>} Function result
   */
  async execute(name, fn, options) {
    const bulkhead = this.bulkheads.get(name);
    if (!bulkhead) {
      throw new Error(`Bulkhead '${name}' not found`);
    }
    return bulkhead.execute(fn, options);
  }

  /**
   * Get statistics for all bulkheads
   * @returns {Record<string, BulkheadStats>} All statistics
   */
  getAllStats() {
    const stats = {};
    for (const [name, bulkhead] of this.bulkheads.entries()) {
      stats[name] = bulkhead.getStats();
    }
    return stats;
  }

  /**
   * Drain all bulkheads
   * @returns {Promise<void>}
   */
  async drainAll() {
    await Promise.all(
      Array.from(this.bulkheads.values()).map(b => b.drain())
    );
  }
}
