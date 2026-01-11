/**
 * @file Optimized Daemon Core Class with Performance Enhancements
 * @module @unrdf/daemon/optimized
 * @description High-performance daemon controller with O(1) cache operations,
 * efficient batch scheduling, and minimal metadata footprint for 10K+ concurrent operations
 */

import { EventEmitter } from 'events';
import { DaemonConfigSchema } from './schemas.mjs';
import { OptimizedLRUCache } from './lru-cache-optimized.mjs';
import { BatchScheduler } from './batch-scheduler.mjs';

/**
 * Optimized operation metadata with minimal footprint
 * Stores only essential information to reduce memory usage
 * @private
 */
class CompactOperationMetadata {
  /**
   * @param {Object} op - Operation data
   */
  constructor(op) {
    this.id = op.id;
    this.name = op.name || op.id;
    this.status = op.status || 'scheduled';
    this.createdAt = op.createdAt || Date.now();
    // Only store metadata if provided
    if (op.metadata) {
      this.metadata = op.metadata;
    }
  }

  /**
   * Convert to minimal JSON for transmission
   * @returns {Object} Minimal representation
   */
  toMinimal() {
    const result = {
      id: this.id,
      name: this.name,
      status: this.status,
      createdAt: this.createdAt,
    };
    if (this.metadata) {
      result.metadata = this.metadata;
    }
    return result;
  }
}

/**
 * Optimized daemon for managing scheduled operations with high concurrency
 * Designed for 10K+ concurrent operations with minimal overhead
 */
export class OptimizedDaemon extends EventEmitter {
  /**
   * Creates a new optimized daemon instance
   * @param {Object} config - Daemon configuration
   * @param {string} [config.id='default-daemon'] - Unique daemon identifier
   * @param {string} [config.nodeId] - Node identifier in cluster
   * @param {string} [config.clusterId] - Cluster identifier
   * @param {number} [config.maxConcurrent=100] - Maximum concurrent operations
   * @param {number} [config.cacheSize=5000] - LRU cache size for completed operations
   * @param {number} [config.batchSize=100] - Batch size for scheduling
   * @param {number} [config.batchFlushMs=10] - Batch flush interval
   * @param {Object} [config.logger] - Logger instance
   * @throws {Error} If configuration is invalid
   */
  constructor(config = {}) {
    super();
    const validated = DaemonConfigSchema.parse(config);

    this.config = {
      ...validated,
      cacheSize: config.cacheSize || 5000,
      batchSize: config.batchSize || 100,
      batchFlushMs: config.batchFlushMs || 10,
    };

    this.nodeId = config.nodeId || `node-${Date.now()}`;
    this.clusterId = config.clusterId || 'default-cluster';
    this.isLeader = false;
    this.isRunning = false;

    this.operations = new Map();
    this.operationQueue = [];
    this.completedOperations = new OptimizedLRUCache(this.config.cacheSize);
    this.batchScheduler = new BatchScheduler(this.config.batchSize, this.config.batchFlushMs);
    this.operationTimings = [];

    this.logger = config.logger || console;
    this.startTime = null;
    this.activeCount = 0;
    this.totalProcessed = 0;

    // Setup batch flush callback
    this.batchScheduler.setFlushCallback((ops) => {
      this._processBatch(ops);
    });

    // Setup periodic metrics
    this.metricsInterval = null;
  }

  /**
   * Start the daemon and initialize scheduler
   * @returns {Promise<void>}
   */
  async start() {
    if (this.isRunning) {
      return;
    }

    this.isRunning = true;
    this.startTime = Date.now();
    this.operationTimings = [];

    // Periodic metrics collection (every 1s)
    this.metricsInterval = setInterval(() => {
      this._collectMetrics();
    }, 1000);

    this.logger.info(`[Daemon ${this.nodeId}] Started with cache size ${this.config.cacheSize}`);
    this.emit('daemon:started', { nodeId: this.nodeId, timestamp: new Date() });
  }

  /**
   * Gracefully stop the daemon
   * @returns {Promise<void>}
   */
  async stop() {
    if (!this.isRunning) {
      return;
    }

    this.isRunning = false;
    this.batchScheduler.flush();

    if (this.metricsInterval) {
      clearInterval(this.metricsInterval);
      this.metricsInterval = null;
    }

    this.logger.info(`[Daemon ${this.nodeId}] Stopped`);
    this.emit('daemon:stopped', { nodeId: this.nodeId, timestamp: new Date() });
  }

  /**
   * Schedule an operation for execution (uses batching)
   * @param {Object} operation - Operation to schedule
   * @param {string} operation.id - Operation identifier
   * @param {Function} operation.handler - Operation handler function
   * @param {string} [operation.name] - Human-readable operation name
   * @param {Object} [operation.metadata] - Operation metadata
   * @throws {Error} If operation is invalid
   */
  schedule(operation) {
    if (!operation || !operation.id || typeof operation.handler !== 'function') {
      throw new Error('Invalid operation: must have id and handler function');
    }

    const metadata = new CompactOperationMetadata({
      ...operation,
      status: 'scheduled',
      createdAt: Date.now(),
    });

    this.operations.set(operation.id, {
      id: operation.id,
      handler: operation.handler,
      metadata,
    });

    // Use batch scheduler for high-throughput scenarios
    this.batchScheduler.add({
      operationId: operation.id,
      name: metadata.name,
    });
  }

  /**
   * Process a batch of scheduled operations
   * @private
   */
  _processBatch(operations) {
    this.operationQueue.push(...operations.map((op) => op.operationId));
    this.emit('batch:scheduled', {
      count: operations.length,
      timestamp: new Date(),
    });
  }

  /**
   * Remove a scheduled operation
   * @param {string} operationId - Operation identifier to remove
   * @returns {boolean} Whether operation was found and removed
   */
  unschedule(operationId) {
    if (!this.operations.has(operationId)) {
      return false;
    }

    this.operations.delete(operationId);
    const index = this.operationQueue.indexOf(operationId);
    if (index > -1) {
      this.operationQueue.splice(index, 1);
    }

    this.logger.debug(`[Daemon ${this.nodeId}] Operation unscheduled: ${operationId}`);
    return true;
  }

  /**
   * List all scheduled operations
   * @returns {Array} Array of scheduled operations
   */
  listOperations() {
    return Array.from(this.operations.values()).map((op) => ({
      id: op.id,
      name: op.metadata.name,
      status: op.metadata.status,
      createdAt: op.metadata.createdAt,
      ...op.metadata.toMinimal(),
    }));
  }

  /**
   * Execute an operation immediately with timing
   * @param {string} operationId - Operation identifier to execute
   * @returns {Promise<*>} Operation result
   * @throws {Error} If operation not found or execution fails
   */
  async execute(operationId) {
    const operation = this.operations.get(operationId);
    if (!operation) {
      throw new Error(`Operation not found: ${operationId}`);
    }

    const startTime = performance.now();
    this.activeCount += 1;

    try {
      this.emit('operation:started', {
        operationId,
        name: operation.metadata.name,
        timestamp: new Date(),
      });

      const result = await operation.handler();
      const duration = performance.now() - startTime;

      // Record timing for metrics
      this.operationTimings.push(duration);

      const completed = {
        operationId,
        status: 'success',
        result,
        duration,
        timestamp: Date.now(),
      };

      this.completedOperations.set(operationId, completed);
      this.totalProcessed += 1;

      this.emit('operation:success', {
        operationId,
        name: operation.metadata.name,
        duration,
        timestamp: new Date(),
      });

      return result;
    } catch (error) {
      const duration = performance.now() - startTime;

      // Trim old timings to prevent memory leak
      if (this.operationTimings.length > 100000) {
        this.operationTimings = this.operationTimings.slice(-50000);
      }

      const completed = {
        operationId,
        status: 'failure',
        error: error.message,
        duration,
        timestamp: Date.now(),
      };

      this.completedOperations.set(operationId, completed);
      this.totalProcessed += 1;

      this.emit('operation:failure', {
        operationId,
        name: operation.metadata.name,
        error: error.message,
        duration,
        timestamp: new Date(),
      });

      throw error;
    } finally {
      this.activeCount -= 1;
    }
  }

  /**
   * Collect and emit metrics
   * @private
   */
  _collectMetrics() {
    if (this.operationTimings.length === 0) {
      return;
    }

    const sorted = [...this.operationTimings].sort((a, b) => a - b);
    const len = sorted.length;
    const p50Idx = Math.floor(len * 0.5);
    const p95Idx = Math.floor(len * 0.95);
    const p99Idx = Math.floor(len * 0.99);

    this.emit('metrics:collected', {
      sampleSize: len,
      p50: sorted[p50Idx],
      p95: sorted[p95Idx],
      p99: sorted[p99Idx],
      mean: sorted.reduce((a, b) => a + b, 0) / len,
    });
  }

  /**
   * Get daemon health status
   * @returns {Object} Health information
   */
  getHealth() {
    const uptime = this.startTime ? Date.now() - this.startTime : 0;

    return {
      nodeId: this.nodeId,
      clusterId: this.clusterId,
      isRunning: this.isRunning,
      isLeader: this.isLeader,
      uptime,
      activeOperations: this.activeCount,
      queuedOperations: this.operationQueue.length,
      completedOperations: this.completedOperations.size(),
      totalProcessed: this.totalProcessed,
      cacheStats: this.completedOperations.getStats(),
      timestamp: new Date(),
    };
  }

  /**
   * Get daemon metrics with percentiles
   * @returns {Object} Performance metrics
   */
  getMetrics() {
    const cacheStats = this.completedOperations.getStats();
    const timings = this.operationTimings;

    if (timings.length === 0) {
      return {
        nodeId: this.nodeId,
        totalOperations: 0,
        successfulOperations: 0,
        failedOperations: 0,
        averageDuration: 0,
        successRate: 0,
        latency: { p50: 0, p95: 0, p99: 0 },
        throughput: 0,
        cacheHitRate: 0,
        timestamp: new Date(),
      };
    }

    const sorted = [...timings].sort((a, b) => a - b);
    const len = sorted.length;
    const completed = this.completedOperations.entries();
    const successCount = completed.filter(([, op]) => op.status === 'success').length;
    const failureCount = completed.filter(([, op]) => op.status === 'failure').length;
    const uptime = this.startTime ? (Date.now() - this.startTime) / 1000 : 0;

    return {
      nodeId: this.nodeId,
      totalOperations: completed.length,
      successfulOperations: successCount,
      failedOperations: failureCount,
      averageDuration: sorted.reduce((a, b) => a + b, 0) / len,
      successRate: completed.length > 0 ? (successCount / completed.length) * 100 : 0,
      latency: {
        p50: sorted[Math.floor(len * 0.5)] || 0,
        p95: sorted[Math.floor(len * 0.95)] || 0,
        p99: sorted[Math.floor(len * 0.99)] || 0,
      },
      throughput: uptime > 0 ? this.totalProcessed / uptime : 0,
      cacheHitRate: cacheStats.hitRate,
      timestamp: new Date(),
    };
  }

  /**
   * Reset metrics for next measurement period
   */
  resetMetrics() {
    this.operationTimings = [];
    this.totalProcessed = 0;
  }
}
