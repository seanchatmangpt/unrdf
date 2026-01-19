/**
 * @file Bulkhead Isolation Pattern Implementation
 * @module @unrdf/daemon/degradation/bulkhead
 * @description Bulkhead pattern for resource isolation using semaphores,
 * thread pool simulation, and queue-based execution with configurable rejection policies.
 */

import { trace, SpanStatusCode } from '@opentelemetry/api';
import {
  BulkheadConfigSchema,
  BulkheadMetricsSchema,
  BulkheadResultSchema,
  ThreadPoolConfigSchema,
  RejectionPolicySchema,
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
 * @private
 */
class Semaphore {
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
 * Bulkhead Implementation
 * Isolates resources to prevent cascading failures
 * @example
 * const bulkhead = new Bulkhead({
 *   name: 'database',
 *   maxConcurrent: 10,
 *   maxQueue: 100
 * });
 *
 * const result = await bulkhead.execute(async () => {
 *   return await db.query('SELECT * FROM users');
 * });
 */
export class Bulkhead {
  /**
   * Create a bulkhead
   * @param {Object} config - Bulkhead configuration
   * @param {string} [config.name='default'] - Bulkhead name
   * @param {number} [config.maxConcurrent=10] - Max concurrent executions
   * @param {number} [config.maxQueue=100] - Max queued requests
   * @param {number} [config.queueTimeout=5000] - Queue timeout in ms
   * @param {string} [config.rejectionPolicy='reject'] - Policy when full
   * @param {Function} [config.onRejected] - Callback when rejected
   * @param {Function} [config.onQueued] - Callback when queued
   * @param {Function} [config.onExecuted] - Callback when executed
   */
  constructor(config = {}) {
    const validated = BulkheadConfigSchema.parse(config);

    this.name = validated.name;
    this.maxConcurrent = validated.maxConcurrent;
    this.maxQueue = validated.maxQueue;
    this.queueTimeout = validated.queueTimeout;
    this.rejectionPolicy = validated.rejectionPolicy;

    this.onRejected = validated.onRejected;
    this.onQueued = validated.onQueued;
    this.onExecuted = validated.onExecuted;

    this._semaphore = new Semaphore(this.maxConcurrent);
    this._queue = [];
    this._activeCalls = 0;

    this._metrics = {
      totalExecuted: 0,
      totalRejected: 0,
      totalQueued: 0,
      totalDropped: 0,
      totalTimedOut: 0,
      totalWaitTime: 0,
      waitTimeCount: 0,
    };

    this._eventListeners = new Map();
  }

  /**
   * Get number of active calls
   * @returns {number}
   */
  get activeCalls() {
    return this._activeCalls;
  }

  /**
   * Get current queue size
   * @returns {number}
   */
  get queueSize() {
    return this._queue.length;
  }

  /**
   * Check if bulkhead is at capacity
   * @returns {boolean}
   */
  get isAtCapacity() {
    return this._activeCalls >= this.maxConcurrent;
  }

  /**
   * Check if queue is full
   * @returns {boolean}
   */
  get isQueueFull() {
    return this._queue.length >= this.maxQueue;
  }

  /**
   * Execute a function through the bulkhead
   * @param {Function} fn - Async function to execute
   * @param {Object} [options] - Execution options
   * @param {number} [options.timeout] - Override queue timeout
   * @param {string} [options.priority] - Execution priority (not implemented)
   * @returns {Promise<Object>} Execution result
   * @throws {BulkheadRejectionError} When rejected
   * @example
   * const result = await bulkhead.execute(async () => {
   *   return await fetch('https://api.example.com');
   * });
   */
  async execute(fn, options = {}) {
    const span = tracer.startSpan('bulkhead.execute', {
      attributes: {
        'bulkhead.name': this.name,
        'bulkhead.active_calls': this._activeCalls,
        'bulkhead.queue_size': this._queue.length,
      },
    });

    const startTime = Date.now();
    let wasQueued = false;
    let waitTime = 0;

    try {
      if (this._semaphore.tryAcquire()) {
        this._activeCalls++;
        span.setAttribute('bulkhead.acquired_immediately', true);
      } else {
        const result = await this._handleCapacityReached(fn, options, span, startTime);
        if (result) {
          return result;
        }
        wasQueued = true;
        waitTime = Date.now() - startTime;
      }

      const executionStart = Date.now();
      const result = await fn();
      const executionTime = Date.now() - executionStart;

      this._metrics.totalExecuted++;
      this._metrics.totalWaitTime += waitTime;
      this._metrics.waitTimeCount++;

      if (this.onExecuted) {
        this.onExecuted(executionTime);
      }

      this._emit('executed', { duration: executionTime, waitTime });

      span.setAttribute('bulkhead.success', true);
      span.setAttribute('bulkhead.wait_time', waitTime);
      span.setAttribute('bulkhead.execution_time', executionTime);
      span.setStatus({ code: SpanStatusCode.OK });

      return BulkheadResultSchema.parse({
        success: true,
        result,
        waitTime,
        executionTime,
        wasQueued,
        rejected: false,
      });
    } catch (error) {
      span.setAttribute('bulkhead.success', false);
      span.setAttribute('bulkhead.error', error.message);
      span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });

      return BulkheadResultSchema.parse({
        success: false,
        error,
        waitTime,
        executionTime: Date.now() - startTime - waitTime,
        wasQueued,
        rejected: error instanceof BulkheadRejectionError,
        rejectionReason: error instanceof BulkheadRejectionError ? error.reason : undefined,
      });
    } finally {
      if (this._activeCalls > 0) {
        this._activeCalls--;
        this._semaphore.release();
        this._processQueue();
      }
      span.end();
    }
  }

  /**
   * Handle when bulkhead is at capacity
   * @private
   */
  async _handleCapacityReached(fn, options, span, startTime) {
    const policy = this.rejectionPolicy;

    if (policy === RejectionPolicy.REJECT) {
      return this._reject(span, startTime, 'Bulkhead at capacity');
    }

    if (policy === RejectionPolicy.DROP) {
      this._metrics.totalDropped++;
      span.setAttribute('bulkhead.dropped', true);
      this._emit('dropped', {});

      return BulkheadResultSchema.parse({
        success: false,
        error: new BulkheadRejectionError(
          `Bulkhead '${this.name}' dropped request`,
          this.name,
          'dropped'
        ),
        waitTime: 0,
        executionTime: 0,
        wasQueued: false,
        rejected: true,
        rejectionReason: 'dropped',
      });
    }

    if (policy === RejectionPolicy.QUEUE) {
      if (this.isQueueFull) {
        return this._reject(span, startTime, 'Queue full');
      }

      return this._enqueue(fn, options, span, startTime);
    }

    return this._reject(span, startTime, 'Unknown policy');
  }

  /**
   * Reject a request
   * @private
   */
  _reject(span, startTime, reason) {
    this._metrics.totalRejected++;

    if (this.onRejected) {
      this.onRejected(reason);
    }

    this._emit('rejected', { reason });

    span.setAttribute('bulkhead.rejected', true);
    span.setAttribute('bulkhead.rejection_reason', reason);

    const error = new BulkheadRejectionError(
      `Bulkhead '${this.name}' rejected: ${reason}`,
      this.name,
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
   * Enqueue a request
   * @private
   */
  async _enqueue(fn, options, span, startTime) {
    const timeout = options.timeout ?? this.queueTimeout;

    this._metrics.totalQueued++;

    if (this.onQueued) {
      this.onQueued(this._queue.length);
    }

    this._emit('queued', { position: this._queue.length });

    span.setAttribute('bulkhead.queued', true);
    span.setAttribute('bulkhead.queue_position', this._queue.length);

    return new Promise((resolve) => {
      const queueEntry = {
        fn,
        resolve,
        startTime,
        timestamp: Date.now(),
        timeoutId: null,
      };

      if (timeout > 0) {
        queueEntry.timeoutId = setTimeout(() => {
          const index = this._queue.indexOf(queueEntry);
          if (index !== -1) {
            this._queue.splice(index, 1);
            this._metrics.totalTimedOut++;

            this._emit('timeout', { waitTime: Date.now() - startTime });

            resolve(BulkheadResultSchema.parse({
              success: false,
              error: new BulkheadRejectionError(
                `Bulkhead '${this.name}' queue timeout`,
                this.name,
                'timeout'
              ),
              waitTime: Date.now() - startTime,
              executionTime: 0,
              wasQueued: true,
              rejected: true,
              rejectionReason: 'timeout',
            }));
          }
        }, timeout);
      }

      this._queue.push(queueEntry);
    });
  }

  /**
   * Process queued requests
   * @private
   */
  _processQueue() {
    while (this._queue.length > 0 && this._semaphore.tryAcquire()) {
      const entry = this._queue.shift();

      if (entry.timeoutId) {
        clearTimeout(entry.timeoutId);
      }

      this._activeCalls++;
      const waitTime = Date.now() - entry.timestamp;

      this._executeQueued(entry, waitTime);
    }
  }

  /**
   * Execute a queued entry
   * @private
   */
  async _executeQueued(entry, waitTime) {
    const span = tracer.startSpan('bulkhead.execute_queued', {
      attributes: {
        'bulkhead.name': this.name,
        'bulkhead.wait_time': waitTime,
      },
    });

    try {
      const executionStart = Date.now();
      const result = await entry.fn();
      const executionTime = Date.now() - executionStart;

      this._metrics.totalExecuted++;
      this._metrics.totalWaitTime += waitTime;
      this._metrics.waitTimeCount++;

      if (this.onExecuted) {
        this.onExecuted(executionTime);
      }

      span.setStatus({ code: SpanStatusCode.OK });

      entry.resolve(BulkheadResultSchema.parse({
        success: true,
        result,
        waitTime,
        executionTime,
        wasQueued: true,
        rejected: false,
      }));
    } catch (error) {
      span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });

      entry.resolve(BulkheadResultSchema.parse({
        success: false,
        error,
        waitTime,
        executionTime: 0,
        wasQueued: true,
        rejected: false,
      }));
    } finally {
      this._activeCalls--;
      this._semaphore.release();
      span.end();
      this._processQueue();
    }
  }

  /**
   * Get bulkhead metrics
   * @returns {Object} Metrics snapshot
   */
  getMetrics() {
    return BulkheadMetricsSchema.parse({
      name: this.name,
      activeCalls: this._activeCalls,
      queueSize: this._queue.length,
      maxConcurrent: this.maxConcurrent,
      maxQueue: this.maxQueue,
      totalExecuted: this._metrics.totalExecuted,
      totalRejected: this._metrics.totalRejected,
      totalQueued: this._metrics.totalQueued,
      totalDropped: this._metrics.totalDropped,
      totalTimedOut: this._metrics.totalTimedOut,
      averageWaitTime: this._metrics.waitTimeCount > 0
        ? this._metrics.totalWaitTime / this._metrics.waitTimeCount
        : 0,
    });
  }

  /**
   * Reset bulkhead metrics
   */
  reset() {
    this._metrics = {
      totalExecuted: 0,
      totalRejected: 0,
      totalQueued: 0,
      totalDropped: 0,
      totalTimedOut: 0,
      totalWaitTime: 0,
      waitTimeCount: 0,
    };

    this._emit('reset', {});
  }

  /**
   * Add event listener
   * @param {string} event - Event name
   * @param {Function} callback - Event callback
   * @returns {Function} Unsubscribe function
   */
  on(event, callback) {
    if (!this._eventListeners.has(event)) {
      this._eventListeners.set(event, new Set());
    }
    this._eventListeners.get(event).add(callback);

    return () => {
      this._eventListeners.get(event)?.delete(callback);
    };
  }

  /**
   * Emit event
   * @private
   */
  _emit(event, data) {
    const listeners = this._eventListeners.get(event);
    if (listeners) {
      for (const callback of listeners) {
        try {
          callback({ ...data, bulkhead: this.name, timestamp: Date.now() });
        } catch {
          // Ignore listener errors
        }
      }
    }
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
   */
  constructor(config = {}) {
    const validated = ThreadPoolConfigSchema.parse(config);

    this.name = validated.name;
    this.coreSize = validated.coreSize;
    this.maxSize = validated.maxSize;
    this.keepAliveTime = validated.keepAliveTime;
    this.queueCapacity = validated.queueCapacity;

    this._bulkhead = new Bulkhead({
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
 * Create a bulkhead with default configuration
 * @param {Object} config - Configuration overrides
 * @returns {Bulkhead}
 */
export function createBulkhead(config = {}) {
  return new Bulkhead(config);
}

/**
 * Create a thread pool
 * @param {Object} config - Configuration overrides
 * @returns {ThreadPool}
 */
export function createThreadPool(config = {}) {
  return new ThreadPool(config);
}

/**
 * Bulkhead registry for managing multiple bulkheads
 */
export class BulkheadRegistry {
  constructor() {
    this._bulkheads = new Map();
  }

  /**
   * Get or create a bulkhead
   * @param {string} name - Bulkhead name
   * @param {Object} [config] - Configuration if creating new
   * @returns {Bulkhead}
   */
  get(name, config = {}) {
    if (!this._bulkheads.has(name)) {
      this._bulkheads.set(name, new Bulkhead({ ...config, name }));
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
   * @returns {Map<string, Bulkhead>}
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

/** Global bulkhead registry */
export const globalBulkheadRegistry = new BulkheadRegistry();
