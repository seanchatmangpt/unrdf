/**
 * @file Batch Operation Scheduler
 * @module @unrdf/daemon/batch-scheduler
 * @description Efficient batch scheduling for high-throughput operation orchestration
 */

/**
 * Batch operation scheduler for high-throughput scenarios
 * Accumulates operations and flushes them in batches to improve throughput
 * @private
 */
export class BatchScheduler {
  /**
   * @param {number} [batchSize=100] - Operations to batch before processing
   * @param {number} [flushIntervalMs=10] - Maximum wait time before flush
   */
  constructor(batchSize = 100, flushIntervalMs = 10) {
    this.batchSize = batchSize;
    this.flushIntervalMs = flushIntervalMs;
    this.pendingOperations = [];
    this.flushTimer = null;
    this.onFlush = null;
  }

  /**
   * Add operation to batch
   * @param {Object} operation - Operation to add
   */
  add(operation) {
    this.pendingOperations.push(operation);

    if (this.pendingOperations.length >= this.batchSize) {
      this.flush();
    } else if (!this.flushTimer) {
      this.flushTimer = setTimeout(() => this.flush(), this.flushIntervalMs);
    }
  }

  /**
   * Flush pending operations
   */
  flush() {
    if (this.flushTimer) {
      clearTimeout(this.flushTimer);
      this.flushTimer = null;
    }

    if (this.pendingOperations.length > 0 && this.onFlush) {
      const ops = this.pendingOperations.splice(0);
      this.onFlush(ops);
    }
  }

  /**
   * Set flush callback
   * @param {Function} callback - Callback to call with batched operations
   */
  setFlushCallback(callback) {
    this.onFlush = callback;
  }

  /**
   * Clear pending operations
   */
  clear() {
    if (this.flushTimer) {
      clearTimeout(this.flushTimer);
      this.flushTimer = null;
    }
    this.pendingOperations = [];
  }
}
