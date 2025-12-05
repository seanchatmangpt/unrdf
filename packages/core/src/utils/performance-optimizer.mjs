/**
 * @file Performance Optimizer for UNRDF
 * @module performance-optimizer
 *
 * @description
 * Implements performance optimizations to meet KGC PRD success metrics:
 * - p50 pre-hook pipeline ≤ 200 µs
 * - p99 ≤ 2 ms (10k triples store, afterHashOnly=true)
 * - Receipt write ≤ 5 ms median (no canonicalization) / ≤ 200 ms with URDNA2015 on 100k triples
 * - Hook engine ≥ 10k exec/min sustained
 * - Error isolation 100%
 */

import { randomUUID } from 'crypto';
import { z } from 'zod';
import { PerformanceMetricsSchema } from './schemas.mjs';

/**
 * Performance optimization configuration
 */
const PerformanceConfigSchema = z.object({
  enableFastPath: z.boolean().default(true),
  enableCaching: z.boolean().default(true),
  enableBatchProcessing: z.boolean().default(true),
  enableParallelExecution: z.boolean().default(true),
  maxConcurrency: z.number().int().positive().default(10),
  cacheSize: z.number().int().positive().default(10000),
  batchSize: z.number().int().positive().default(1000),
  timeoutMs: z.number().int().positive().default(2000), // KGC PRD: p99 ≤ 2ms
  maxHooks: z.number().int().positive().default(10000), // KGC PRD: 10k exec/min
  afterHashOnly: z.boolean().default(false), // KGC PRD fast path
  enableProfiling: z.boolean().default(false),
  enableMemoryOptimization: z.boolean().default(true),
  enableQueryOptimization: z.boolean().default(true),
});

/**
 * Performance metrics tracking
 */
const PerformanceMetrics = {
  transactionLatency: [],
  hookExecutionRate: 0,
  errorRate: 0,
  memoryUsage: [],
  cacheStats: { hits: 0, misses: 0, size: 0 },
  backpressure: { queueDepth: 0, watermarks: { high: 1000, low: 100 } },
};

/**
 * Performance Optimizer for UNRDF
 */
export class PerformanceOptimizer {
  /**
   * Create a new performance optimizer
   * @param {Object} [config] - Performance configuration
   */
  constructor(config = {}) {
    this.config = PerformanceConfigSchema.parse(config);
    this.metrics = { ...PerformanceMetrics };
    this.cache = new Map();
    this.batchQueue = [];
    this.processingQueue = [];
    this.isProcessing = false;

    // Performance tracking
    this.startTime = Date.now();
    this.totalTransactions = 0;
    this.totalHooks = 0;
    this.totalErrors = 0;

    // Memory optimization
    this.memoryThreshold = 100 * 1024 * 1024; // 100MB
    this.lastGcTime = Date.now();
    this.gcInterval = 30000; // 30 seconds

    // Query optimization
    this.queryCache = new Map();
    this.queryStats = new Map();

    // Initialize performance monitoring
    this._initializePerformanceMonitoring();
  }

  /**
   * Initialize performance monitoring
   * @private
   */
  _initializePerformanceMonitoring() {
    if (this.config.enableProfiling) {
      // Set up performance monitoring
      setInterval(() => {
        this._updateMemoryUsage();
        this._checkMemoryThreshold();
        this._cleanupCache();
      }, 5000); // Every 5 seconds
    }
  }

  /**
   * Optimize transaction processing
   * @param {Function} transactionFn - Transaction function
   * @param {Object} context - Transaction context
   * @returns {Promise<any>} Optimized transaction result
   */
  async optimizeTransaction(transactionFn, context) {
    const startTime = process.hrtime.bigint();

    try {
      // Fast path optimization
      if (this.config.enableFastPath && context.afterHashOnly) {
        return await this._fastPathTransaction(transactionFn, context);
      }

      // Batch processing optimization
      if (this.config.enableBatchProcessing && context.batchable) {
        return await this._batchTransaction(transactionFn, context);
      }

      // Parallel execution optimization
      if (this.config.enableParallelExecution && context.parallelizable) {
        return await this._parallelTransaction(transactionFn, context);
      }

      // Standard execution
      const result = await transactionFn(context);

      // Update metrics
      const duration = Number(process.hrtime.bigint() - startTime) / 1000000; // Convert to ms
      this._updateTransactionMetrics(duration, true);

      return result;
    } catch (error) {
      const duration = Number(process.hrtime.bigint() - startTime) / 1000000;
      this._updateTransactionMetrics(duration, false);
      throw error;
    }
  }

  /**
   * Optimize hook execution
   * @param {Function} hookFn - Hook function
   * @param {Object} context - Hook context
   * @returns {Promise<any>} Optimized hook result
   */
  async optimizeHook(hookFn, context) {
    const startTime = process.hrtime.bigint();

    try {
      // Check if hook is cached
      if (this.config.enableCaching) {
        const cacheKey = this._generateCacheKey(hookFn, context);
        const cached = this.cache.get(cacheKey);
        if (cached && this._isCacheValid(cached)) {
          this.metrics.cacheStats.hits++;
          return cached.result;
        }
        this.metrics.cacheStats.misses++;
      }

      // Execute hook
      const result = await hookFn(context);

      // Cache result if enabled
      if (this.config.enableCaching) {
        const cacheKey = this._generateCacheKey(hookFn, context);
        this.cache.set(cacheKey, {
          result,
          timestamp: Date.now(),
          ttl: context.cacheTtl || 300000, // 5 minutes default
        });
      }

      // Update metrics
      const duration = Number(process.hrtime.bigint() - startTime) / 1000000;
      this._updateHookMetrics(duration, true);

      return result;
    } catch (error) {
      const duration = Number(process.hrtime.bigint() - startTime) / 1000000;
      this._updateHookMetrics(duration, false);
      throw error;
    }
  }

  /**
   * Optimize query execution
   * @param {string} query - SPARQL query
   * @param {Function} queryFn - Query function
   * @param {Object} context - Query context
   * @returns {Promise<any>} Optimized query result
   */
  async optimizeQuery(query, queryFn, context) {
    if (!this.config.enableQueryOptimization) {
      return await queryFn(query, context);
    }

    // Check query cache
    const cacheKey = this._hashQuery(query);
    const cached = this.queryCache.get(cacheKey);
    if (cached && this._isCacheValid(cached)) {
      this.metrics.cacheStats.hits++;
      return cached.result;
    }

    // Execute query
    const startTime = process.hrtime.bigint();
    const result = await queryFn(query, context);
    const duration = Number(process.hrtime.bigint() - startTime) / 1000000;

    // Cache result
    this.queryCache.set(cacheKey, {
      result,
      timestamp: Date.now(),
      ttl: context.queryCacheTtl || 600000, // 10 minutes default
    });

    // Update query stats
    this.queryStats.set(cacheKey, {
      executionCount: (this.queryStats.get(cacheKey)?.executionCount || 0) + 1,
      totalDuration: (this.queryStats.get(cacheKey)?.totalDuration || 0) + duration,
      lastExecuted: Date.now(),
    });

    this.metrics.cacheStats.misses++;
    return result;
  }

  /**
   * Fast path transaction execution
   * @param {Function} transactionFn - Transaction function
   * @param {Object} context - Transaction context
   * @returns {Promise<any>} Fast path result
   * @private
   */
  async _fastPathTransaction(transactionFn, context) {
    // Skip expensive operations for fast path
    const fastContext = {
      ...context,
      skipCanonicalization: true,
      skipValidation: true,
      skipHooks: false, // Still run hooks but optimize them
      timeout: Math.min(context.timeout || 2000, 1000), // Max 1s for fast path
    };

    return await transactionFn(fastContext);
  }

  /**
   * Batch transaction processing
   * @param {Function} transactionFn - Transaction function
   * @param {Object} context - Transaction context
   * @returns {Promise<any>} Batch result
   * @private
   */
  async _batchTransaction(transactionFn, context) {
    // Add to batch queue
    this.batchQueue.push({ transactionFn, context });

    // Process batch if size threshold reached
    if (this.batchQueue.length >= this.config.batchSize) {
      return await this._processBatch();
    }

    // Return promise that resolves when batch is processed
    return new Promise((resolve, reject) => {
      const batchId = randomUUID();
      this.processingQueue.push({ batchId, resolve, reject });
    });
  }

  /**
   * Process batch queue
   * @returns {Promise<any>} Batch processing result
   * @private
   */
  async _processBatch() {
    if (this.isProcessing) {
      return; // Already processing
    }

    this.isProcessing = true;
    const batch = this.batchQueue.splice(0, this.config.batchSize);

    try {
      // Process batch in parallel
      const results = await Promise.all(
        batch.map(({ transactionFn, context }) => transactionFn(context))
      );

      // Resolve all pending promises
      this.processingQueue.forEach(({ resolve }) => resolve(results));
      this.processingQueue = [];

      return results;
    } catch (error) {
      // Reject all pending promises
      this.processingQueue.forEach(({ reject }) => reject(error));
      this.processingQueue = [];
      throw error;
    } finally {
      this.isProcessing = false;
    }
  }

  /**
   * Parallel transaction execution
   * @param {Function} transactionFn - Transaction function
   * @param {Object} context - Transaction context
   * @returns {Promise<any>} Parallel result
   * @private
   */
  async _parallelTransaction(transactionFn, context) {
    // Split context into parallelizable parts
    const parallelParts = this._splitContext(context);

    // Execute parts in parallel
    const results = await Promise.all(parallelParts.map(part => transactionFn(part)));

    // Merge results
    return this._mergeResults(results);
  }

  /**
   * Update transaction metrics
   * @param {number} duration - Transaction duration
   * @param {boolean} success - Whether transaction succeeded
   * @private
   */
  _updateTransactionMetrics(duration, success) {
    this.metrics.transactionLatency.push({
      timestamp: Date.now(),
      duration,
      success,
    });

    // Keep only last 1000 measurements
    if (this.metrics.transactionLatency.length > 1000) {
      this.metrics.transactionLatency = this.metrics.transactionLatency.slice(-1000);
    }

    this.totalTransactions++;

    if (!success) {
      this.totalErrors++;
    }

    // Check if we're meeting performance targets
    this._checkPerformanceTargets();
  }

  /**
   * Update hook metrics
   * @param {number} duration - Hook duration
   * @param {boolean} success - Whether hook succeeded
   * @private
   */
  _updateHookMetrics(duration, success) {
    this.totalHooks++;

    if (!success) {
      this.totalErrors++;
    }

    // Calculate hook execution rate (per minute)
    const now = Date.now();
    const timeElapsed = (now - this.startTime) / 60000; // minutes
    this.metrics.hookExecutionRate = this.totalHooks / timeElapsed;
  }

  /**
   * Check performance targets
   * @private
   */
  _checkPerformanceTargets() {
    const recentLatencies = this.metrics.transactionLatency
      .filter(l => Date.now() - l.timestamp < 60000) // Last minute
      .map(l => l.duration)
      .sort((a, b) => a - b);

    if (recentLatencies.length === 0) return;

    const p50 = this._calculatePercentile(recentLatencies, 0.5);
    const p99 = this._calculatePercentile(recentLatencies, 0.99);

    // Check KGC PRD targets
    if (p50 > 0.2) {
      // 200 µs
      console.warn(`[Performance] p50 latency ${p50.toFixed(3)}ms exceeds target 0.2ms`);
    }

    if (p99 > 2) {
      // 2 ms
      console.warn(`[Performance] p99 latency ${p99.toFixed(3)}ms exceeds target 2ms`);
    }

    if (this.metrics.hookExecutionRate < 10000) {
      // 10k exec/min
      console.warn(
        `[Performance] Hook execution rate ${this.metrics.hookExecutionRate.toFixed(0)}/min below target 10000/min`
      );
    }
  }

  /**
   * Calculate percentile
   * @param {Array<number>} values - Sorted values
   * @param {number} percentile - Percentile (0-1)
   * @returns {number} Percentile value
   * @private
   */
  _calculatePercentile(values, percentile) {
    if (values.length === 0) return 0;
    const index = Math.ceil(values.length * percentile) - 1;
    return values[Math.max(0, index)];
  }

  /**
   * Generate cache key
   * @param {Function} fn - Function
   * @param {Object} context - Context
   * @returns {string} Cache key
   * @private
   */
  _generateCacheKey(fn, context) {
    const fnString = fn.toString();
    const contextString = JSON.stringify(context);
    return this._hashString(fnString + contextString);
  }

  /**
   * Hash string
   * @param {string} str - String to hash
   * @returns {string} Hash
   * @private
   */
  _hashString(str) {
    let hash = 0;
    for (let i = 0; i < str.length; i++) {
      const char = str.charCodeAt(i);
      hash = (hash << 5) - hash + char;
      hash = hash & hash; // Convert to 32-bit integer
    }
    return hash.toString(36);
  }

  /**
   * Hash query
   * @param {string} query - SPARQL query
   * @returns {string} Query hash
   * @private
   */
  _hashQuery(query) {
    return this._hashString(query);
  }

  /**
   * Check if cache entry is valid
   * @param {Object} cached - Cached entry
   * @returns {boolean} Is valid
   * @private
   */
  _isCacheValid(cached) {
    return Date.now() - cached.timestamp < cached.ttl;
  }

  /**
   * Update memory usage
   * @private
   */
  _updateMemoryUsage() {
    const memUsage = process.memoryUsage();
    this.metrics.memoryUsage.push({
      timestamp: Date.now(),
      rss: memUsage.rss,
      heapUsed: memUsage.heapUsed,
      heapTotal: memUsage.heapTotal,
      external: memUsage.external,
    });

    // Keep only last 100 measurements
    if (this.metrics.memoryUsage.length > 100) {
      this.metrics.memoryUsage = this.metrics.memoryUsage.slice(-100);
    }
  }

  /**
   * Check memory threshold
   * @private
   */
  _checkMemoryThreshold() {
    const memUsage = process.memoryUsage();
    if (memUsage.heapUsed > this.memoryThreshold) {
      console.warn(
        `[Performance] Memory usage ${(memUsage.heapUsed / 1024 / 1024).toFixed(2)}MB exceeds threshold ${(this.memoryThreshold / 1024 / 1024).toFixed(2)}MB`
      );

      // Force garbage collection if available
      if (global.gc) {
        global.gc();
        this.lastGcTime = Date.now();
      }
    }
  }

  /**
   * Cleanup cache
   * @private
   */
  _cleanupCache() {
    const _now = Date._now();

    // Cleanup main cache
    for (const [key, value] of this.cache.entries()) {
      if (!this._isCacheValid(value)) {
        this.cache.delete(key);
      }
    }

    // Cleanup query cache
    for (const [key, value] of this.queryCache.entries()) {
      if (!this._isCacheValid(value)) {
        this.queryCache.delete(key);
      }
    }

    // Limit cache size
    if (this.cache.size > this.config.cacheSize) {
      const entries = Array.from(this.cache.entries());
      entries.sort((a, b) => a[1].timestamp - b[1].timestamp);
      const toDelete = entries.slice(0, entries.length - this.config.cacheSize);
      toDelete.forEach(([key]) => this.cache.delete(key));
    }
  }

  /**
   * Split context for parallel processing
   * @param {Object} context - Context to split
   * @returns {Array<Object>} Split contexts
   * @private
   */
  _splitContext(context) {
    // Simple implementation - split by delta additions/removals
    if (context.delta && context.delta.additions) {
      const chunkSize = Math.ceil(context.delta.additions.length / this.config.maxConcurrency);
      const chunks = [];

      for (let i = 0; i < context.delta.additions.length; i += chunkSize) {
        chunks.push({
          ...context,
          delta: {
            ...context.delta,
            additions: context.delta.additions.slice(i, i + chunkSize),
            removals: [], // Only process additions in parallel
          },
        });
      }

      return chunks;
    }

    return [context];
  }

  /**
   * Merge parallel results
   * @param {Array} results - Results to merge
   * @returns {Object} Merged result
   * @private
   */
  _mergeResults(results) {
    // Simple implementation - merge receipts
    if (results.length === 1) {
      return results[0];
    }

    const merged = {
      ...results[0],
      hookResults: [],
      hookErrors: [],
    };

    results.forEach(result => {
      if (result.hookResults) {
        merged.hookResults.push(...result.hookResults);
      }
      if (result.hookErrors) {
        merged.hookErrors.push(...result.hookErrors);
      }
    });

    return merged;
  }

  /**
   * Get performance metrics
   * @returns {Object} Performance metrics
   */
  getMetrics() {
    const now = Date.now();
    const oneMinuteAgo = now - 60000;

    // Calculate transaction latency percentiles
    const recentLatencies = this.metrics.transactionLatency
      .filter(l => l.timestamp > oneMinuteAgo)
      .map(l => l.duration)
      .sort((a, b) => a - b);

    const p50 = this._calculatePercentile(recentLatencies, 0.5);
    const p95 = this._calculatePercentile(recentLatencies, 0.95);
    const p99 = this._calculatePercentile(recentLatencies, 0.99);
    const max = recentLatencies.length > 0 ? recentLatencies[recentLatencies.length - 1] : 0;

    // Calculate error rate
    const errorRate = this.totalTransactions > 0 ? this.totalErrors / this.totalTransactions : 0;

    // Get current memory usage
    const currentMemory = process.memoryUsage();

    // Calculate cache hit rate
    const totalCacheOps = this.metrics.cacheStats.hits + this.metrics.cacheStats.misses;
    const cacheHitRate = totalCacheOps > 0 ? this.metrics.cacheStats.hits / totalCacheOps : 0;

    return PerformanceMetricsSchema.parse({
      transactionLatency: { p50, p95, p99, max },
      hookExecutionRate: this.metrics.hookExecutionRate,
      errorRate,
      memoryUsage: currentMemory,
      cacheStats: {
        hitRate: cacheHitRate,
        size: this.cache.size,
        maxSize: this.config.cacheSize,
      },
      backpressure: this.metrics.backpressure,
    });
  }

  /**
   * Reset metrics
   */
  resetMetrics() {
    this.metrics = { ...PerformanceMetrics };
    this.startTime = Date.now();
    this.totalTransactions = 0;
    this.totalHooks = 0;
    this.totalErrors = 0;
  }

  /**
   * Shutdown optimizer
   */
  async shutdown() {
    // Process remaining batch
    if (this.batchQueue.length > 0) {
      await this._processBatch();
    }

    // Clear caches
    this.cache.clear();
    this.queryCache.clear();
    this.queryStats.clear();

    console.log('[Performance] Optimizer shutdown complete');
  }
}

/**
 * Create a performance optimizer instance
 * @param {Object} [config] - Configuration
 * @returns {PerformanceOptimizer} Performance optimizer
 */
export function createPerformanceOptimizer(config = {}) {
  return new PerformanceOptimizer(config);
}

/**
 * Default performance optimizer instance
 */
export const defaultPerformanceOptimizer = createPerformanceOptimizer();
