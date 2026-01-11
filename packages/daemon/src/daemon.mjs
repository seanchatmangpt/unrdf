/**
 * @file Daemon Core Class
 * @module @unrdf/daemon
 * @description Main daemon controller managing scheduled task execution, clustering, and event-driven operations
 */

import { EventEmitter } from 'events';
import { DaemonConfigSchema } from './schemas.mjs';

/**
 * Simple LRU cache implementation for completed operations
 * @private
 */
class LRUCache {
  /**
   * @param {number} maxSize - Maximum cache size
   */
  constructor(maxSize = 1000) {
    this.maxSize = maxSize;
    this.cache = new Map();
  }

  /**
   * Set value in cache
   * @param {string} key - Cache key
   * @param {*} value - Value to store
   */
  set(key, value) {
    if (this.cache.has(key)) {
      this.cache.delete(key);
    }
    this.cache.set(key, value);
    if (this.cache.size > this.maxSize) {
      const firstKey = this.cache.keys().next().value;
      this.cache.delete(firstKey);
    }
  }

  /**
   * Get value from cache
   * @param {string} key - Cache key
   * @returns {*} Cached value or undefined
   */
  get(key) {
    return this.cache.get(key);
  }

  /**
   * Check if key exists
   * @param {string} key - Cache key
   * @returns {boolean} Whether key exists
   */
  has(key) {
    return this.cache.has(key);
  }

  /**
   * Get all entries as array
   * @returns {Array} Array of [key, value] pairs
   */
  entries() {
    return Array.from(this.cache.entries());
  }
}

/**
 * Core daemon for managing scheduled operations, clustering, and event-driven task execution
 */
export class Daemon extends EventEmitter {
  /**
   * Creates a new daemon instance
   * @param {Object} config - Daemon configuration
   * @param {string} [config.id='default-daemon'] - Unique daemon identifier
   * @param {string} [config.nodeId] - Node identifier in cluster
   * @param {string} [config.clusterId] - Cluster identifier
   * @param {number} [config.maxConcurrent=5] - Maximum concurrent operations
   * @param {Object} [config.logger] - Logger instance
   * @throws {Error} If configuration is invalid
   */
  constructor(config = {}) {
    super();
    const validated = DaemonConfigSchema.parse(config);

    this.config = validated;
    this.nodeId = config.nodeId || `node-${Date.now()}`;
    this.clusterId = config.clusterId || 'default-cluster';
    this.isLeader = false;
    this.isRunning = false;

    this.operations = new Map();
    this.operationQueue = [];
    this.completedOperations = new LRUCache(1000);

    this.logger = config.logger || console;
    this.startTime = null;
    this.activeCount = 0;
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
    this.logger.info(`[Daemon ${this.nodeId}] Started`);
    this._safeEmit('daemon:started', { nodeId: this.nodeId, timestamp: new Date() });
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
    this.logger.info(`[Daemon ${this.nodeId}] Stopped`);
    this._safeEmit('daemon:stopped', { nodeId: this.nodeId, timestamp: new Date() });
  }

  /**
   * Schedule an operation for execution
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

    this.operations.set(operation.id, {
      ...operation,
      status: 'scheduled',
      createdAt: Date.now(),
    });

    this.operationQueue.push(operation.id);
    this.logger.debug(`[Daemon ${this.nodeId}] Operation scheduled: ${operation.id}`);
    this._safeEmit('operation:enqueued', {
      operationId: operation.id,
      name: operation.name,
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
    return Array.from(this.operations.values()).map(op => ({
      id: op.id,
      name: op.name || op.id,
      status: op.status,
      createdAt: new Date(op.createdAt),
      metadata: op.metadata,
    }));
  }

  /**
   * Execute an operation immediately
   * @param {string} operationId - Operation identifier to execute
   * @returns {Promise<*>} Operation result
   * @throws {Error} If operation not found or execution fails
   */
  async execute(operationId) {
    const operation = this.operations.get(operationId);
    if (!operation) {
      throw new Error(`Operation not found: ${operationId}`);
    }

    const startTime = Date.now();
    this.activeCount += 1;

    try {
      this._safeEmit('operation:started', {
        operationId,
        name: operation.name,
        timestamp: new Date(),
      });

      const result = await operation.handler();

      const duration = Date.now() - startTime;
      const completed = {
        operationId,
        status: 'success',
        result,
        duration,
        timestamp: Date.now(),
      };

      this.completedOperations.set(operationId, completed);
      this._safeEmit('operation:success', {
        operationId,
        name: operation.name,
        duration,
        timestamp: new Date(),
      });

      return result;
    } catch (error) {
      const duration = Date.now() - startTime;
      const completed = {
        operationId,
        status: 'failure',
        error: error.message,
        duration,
        timestamp: Date.now(),
      };

      this.completedOperations.set(operationId, completed);
      this._safeEmit('operation:failure', {
        operationId,
        name: operation.name,
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
   * Safely emit an event, catching any listener errors
   * @private
   * @param {string} event - Event name
   * @param {*} data - Event data
   */
  _safeEmit(event, data) {
    try {
      this.emit(event, data);
    } catch (error) {
      this.logger.warn(`[Daemon ${this.nodeId}] Listener error for event '${event}': ${error.message}`);
    }
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
      completedOperations: this.completedOperations.entries().length,
      timestamp: new Date(),
    };
  }

  /**
   * Get daemon metrics
   * @returns {Object} Performance and operation metrics
   */
  getMetrics() {
    const completed = this.completedOperations.entries();
    const totalDuration = completed.reduce((sum, [, op]) => sum + (op.duration || 0), 0);
    const successCount = completed.filter(([, op]) => op.status === 'success').length;
    const failureCount = completed.filter(([, op]) => op.status === 'failure').length;

    return {
      nodeId: this.nodeId,
      totalOperations: completed.length,
      successfulOperations: successCount,
      failedOperations: failureCount,
      averageDuration: completed.length > 0 ? totalDuration / completed.length : 0,
      totalDuration,
      successRate: completed.length > 0 ? (successCount / completed.length) * 100 : 0,
      timestamp: new Date(),
    };
  }
}
