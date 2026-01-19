/**
 * @file Bulkhead Helper Classes
 * @module @unrdf/daemon/degradation/bulkhead-helpers
 * @description Helper classes for bulkhead pattern including semaphore and thread pool.
 */

import { trace, SpanStatusCode } from '@opentelemetry/api';
import {
  BulkheadResultSchema,
  ThreadPoolConfigSchema,
} from './degradation.schema.mjs';

const tracer = trace.getTracer('@unrdf/daemon/bulkhead');

/**
 * Rejection policies for bulkhead
 * @readonly
 * @enum {string}
 */
export const RejectionPolicy = {
  /** Immediately reject when full */
  REJECT: 'reject',
  /** Queue the request for later execution */
  QUEUE: 'queue',
  /** Silently drop the request */
  DROP: 'drop',
};

/**
 * Bulkhead Rejection Error
 * Thrown when bulkhead rejects a request
 */
export class BulkheadRejectionError extends Error {
  /**
   * @param {string} message - Error message
   * @param {string} bulkheadName - Name of the bulkhead
   * @param {string} reason - Rejection reason
   */
  constructor(message, bulkheadName, reason) {
    super(message);
    this.name = 'BulkheadRejectionError';
    this.bulkheadName = bulkheadName;
    this.reason = reason;
    this.timestamp = Date.now();
  }
}

/**
 * Semaphore for concurrency control
 */
export class Semaphore {
  /**
   * @param {number} permits - Maximum concurrent permits
   */
  constructor(permits) {
    this.permits = permits;
    this.available = permits;
    this.waiters = [];
  }

  /**
   * Acquire a permit
   * @param {number} [timeout] - Optional timeout in ms
   * @returns {Promise<boolean>} True if acquired
   */
  async acquire(timeout) {
    if (this.available > 0) {
      this.available--;
      return true;
    }

    if (timeout === 0) {
      return false;
    }

    return new Promise((resolve) => {
      const waiter = { resolve, timestamp: Date.now() };
      this.waiters.push(waiter);

      if (timeout && timeout > 0) {
        setTimeout(() => {
          const index = this.waiters.indexOf(waiter);
          if (index !== -1) {
            this.waiters.splice(index, 1);
            resolve(false);
          }
        }, timeout);
      }
    });
  }

  /**
   * Try to acquire without waiting
   * @returns {boolean} True if acquired
   */
  tryAcquire() {
    if (this.available > 0) {
      this.available--;
      return true;
    }
    return false;
  }

  /**
   * Release a permit
   */
  release() {
    if (this.waiters.length > 0) {
      const waiter = this.waiters.shift();
      waiter.resolve(true);
    } else {
      this.available = Math.min(this.available + 1, this.permits);
    }
  }

  /**
   * Get number of available permits
   * @returns {number}
   */
  getAvailable() {
    return this.available;
  }

  /**
   * Get number of waiting requests
   * @returns {number}
   */
  getWaiters() {
    return this.waiters.length;
  }
}

/**
 * Thread Pool Simulation
 * Simulates thread pool isolation for async operations
 */
export class ThreadPool {
  /**
   * Create a thread pool
   * @param {Object} config - Thread pool configuration
   * @param {Function} BulkheadClass - Bulkhead class to use
   */
  constructor(config = {}, BulkheadClass) {
    const validated = ThreadPoolConfigSchema.parse(config);

    this.name = validated.name;
    this.coreSize = validated.coreSize;
    this.maxSize = validated.maxSize;
    this.keepAliveTime = validated.keepAliveTime;
    this.queueCapacity = validated.queueCapacity;

    this._bulkhead = new BulkheadClass({
      name: `threadpool-${this.name}`,
      maxConcurrent: this.maxSize,
      maxQueue: this.queueCapacity,
      queueTimeout: this.keepAliveTime,
      rejectionPolicy: RejectionPolicy.QUEUE,
    });
  }

  /**
   * Submit a task to the thread pool
   * @param {Function} task - Task to execute
   * @returns {Promise<Object>} Execution result
   */
  async submit(task) {
    return this._bulkhead.execute(task);
  }

  /**
   * Get thread pool metrics
   * @returns {Object}
   */
  getMetrics() {
    const bulkheadMetrics = this._bulkhead.getMetrics();
    return {
      ...bulkheadMetrics,
      coreSize: this.coreSize,
      maxSize: this.maxSize,
      activeThreads: bulkheadMetrics.activeCalls,
      queuedTasks: bulkheadMetrics.queueSize,
    };
  }

  /**
   * Shutdown the thread pool
   */
  shutdown() {
    this._bulkhead.reset();
  }
}

/**
 * Bulkhead registry for managing multiple bulkheads
 */
export class BulkheadRegistry {
  /**
   * @param {Function} BulkheadClass - Bulkhead class to instantiate
   */
  constructor(BulkheadClass) {
    this._bulkheads = new Map();
    this._BulkheadClass = BulkheadClass;
  }

  /**
   * Get or create a bulkhead
   * @param {string} name - Bulkhead name
   * @param {Object} [config] - Configuration if creating new
   * @returns {Object} Bulkhead instance
   */
  get(name, config = {}) {
    if (!this._bulkheads.has(name)) {
      this._bulkheads.set(name, new this._BulkheadClass({ ...config, name }));
    }
    return this._bulkheads.get(name);
  }

  /**
   * Remove a bulkhead
   * @param {string} name - Bulkhead name
   */
  remove(name) {
    this._bulkheads.delete(name);
  }

  /**
   * Get all bulkheads
   * @returns {Map<string, Object>}
   */
  getAll() {
    return new Map(this._bulkheads);
  }

  /**
   * Get metrics for all bulkheads
   * @returns {Object}
   */
  getAllMetrics() {
    const metrics = {};
    for (const [name, bulkhead] of this._bulkheads) {
      metrics[name] = bulkhead.getMetrics();
    }
    return metrics;
  }
}

/**
 * Create rejection result
 * @param {string} name - Bulkhead name
 * @param {number} startTime - Start timestamp
 * @param {string} reason - Rejection reason
 * @returns {Object}
 */
export function createRejectionResult(name, startTime, reason) {
  const error = new BulkheadRejectionError(
    `Bulkhead '${name}' rejected: ${reason}`,
    name,
    reason
  );

  return BulkheadResultSchema.parse({
    success: false,
    error,
    waitTime: Date.now() - startTime,
    executionTime: 0,
    wasQueued: false,
    rejected: true,
    rejectionReason: reason,
  });
}

/**
 * Create drop result
 * @param {string} name - Bulkhead name
 * @returns {Object}
 */
export function createDropResult(name) {
  return BulkheadResultSchema.parse({
    success: false,
    error: new BulkheadRejectionError(
      `Bulkhead '${name}' dropped request`,
      name,
      'dropped'
    ),
    waitTime: 0,
    executionTime: 0,
    wasQueued: false,
    rejected: true,
    rejectionReason: 'dropped',
  });
}
