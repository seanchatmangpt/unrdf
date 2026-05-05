/**
 * KGC Multiverse - Parallel Executor
 * Implements worker pool for 10k universe parallel operations
 *
 * Features:
 * - piscina worker pool (configurable worker count)
 * - Batch processing with backpressure
 * - Memory management with GC hints
 * - Peak memory tracking
 * - Graceful shutdown and cleanup
 *
 * @module @unrdf/kgc-multiverse/parallel-executor
 */

import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';
import Piscina from 'piscina';
import { TaskType } from './worker-task.mjs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/**
 * Default configuration for parallel executor
 * @readonly
 */
export const DEFAULT_CONFIG = {
  /** Number of worker threads */
  workerCount: 10,
  /** Maximum concurrent tasks per worker */
  concurrentTasksPerWorker: 2,
  /** Batch size for operations */
  batchSize: 100,
  /** GC hint interval (run GC every N operations) */
  gcInterval: 100,
  /** Maximum queue size (backpressure threshold) */
  maxQueue: 1000,
  /** Idle timeout for workers (ms) */
  idleTimeout: 30000,
};

/**
 * Executor Statistics
 * @typedef {Object} ExecutorStats
 * @property {number} tasksCompleted - Total tasks completed
 * @property {number} tasksFailed - Total tasks failed
 * @property {number} peakMemoryMB - Peak memory usage in MB
 * @property {number} totalDurationMs - Total execution duration
 * @property {number} throughputPerSec - Operations per second
 */

/**
 * Parallel Executor Class
 * Manages worker pool and batch operations for 10k+ universes
 */
export class ParallelExecutor {
  /**
   * Create a new ParallelExecutor
   *
   * @param {Object} [options={}] - Configuration options
   * @param {number} [options.workerCount=10] - Number of worker threads
   * @param {number} [options.batchSize=100] - Batch size for operations
   * @param {number} [options.gcInterval=100] - GC hint interval
   * @param {number} [options.maxQueue=1000] - Maximum queue size
   * @param {Object} [options.logger] - Logger instance
   */
  constructor(options = {}) {
    /** @private */
    this._config = { ...DEFAULT_CONFIG, ...options };

    /** @private */
    this._logger = options.logger || console;

    /** @private */
    this._pool = null;

    /** @private */
    this._stats = {
      tasksCompleted: 0,
      tasksFailed: 0,
      peakMemoryMB: 0,
      totalDurationMs: 0,
      throughputPerSec: 0,
    };

    /** @private */
    this._startTime = null;

    /** @private */
    this._isShutdown = false;
  }

  /**
   * Initialize the worker pool
   * Must be called before any operations
   *
   * @returns {Promise<void>}
   *
   * @example
   * const executor = new ParallelExecutor({ workerCount: 8 });
   * await executor.initialize();
   */
  async initialize() {
    if (this._pool) {
      throw new Error('ParallelExecutor already initialized');
    }

    const workerPath = join(__dirname, 'worker-task.mjs');

    this._pool = new Piscina({
      filename: workerPath,
      minThreads: this._config.workerCount,
      maxThreads: this._config.workerCount,
      concurrentTasksPerWorker: this._config.concurrentTasksPerWorker,
      maxQueue: this._config.maxQueue,
      idleTimeout: this._config.idleTimeout,
    });

    this._startTime = Date.now();
    this._isShutdown = false;

    this._logger.debug?.(`ParallelExecutor initialized with ${this._config.workerCount} workers`);
  }

  /**
   * Check if executor is initialized
   *
   * @returns {boolean} True if initialized and not shutdown
   */
  isInitialized() {
    return this._pool !== null && !this._isShutdown;
  }

  /**
   * Update memory stats
   * @private
   */
  _updateMemoryStats() {
    const usage = process.memoryUsage();
    const currentMB = usage.heapUsed / (1024 * 1024);
    if (currentMB > this._stats.peakMemoryMB) {
      this._stats.peakMemoryMB = currentMB;
    }
  }

  /**
   * Trigger GC if available
   * @private
   */
  _gcHint() {
    if (global.gc) {
      global.gc();
    }
  }

  /**
   * Run a single task in the worker pool
   *
   * @param {Object} task - Task configuration
   * @returns {Promise<Object>} Task result
   */
  async runTask(task) {
    if (!this._pool || this._isShutdown) {
      throw new Error('ParallelExecutor not initialized or shutdown');
    }

    try {
      const result = await this._pool.run(task);
      this._stats.tasksCompleted++;
      this._updateMemoryStats();

      // GC hint every N operations
      if (this._stats.tasksCompleted % this._config.gcInterval === 0) {
        this._gcHint();
      }

      return result;
    } catch (error) {
      this._stats.tasksFailed++;
      throw error;
    }
  }

  /**
   * Create multiple universes in parallel
   *
   * @param {number} count - Number of universes to create
   * @param {Object} [options={}] - Creation options
   * @param {number} [options.batchSize] - Override batch size
   * @param {string} [options.createdBy] - Creator identifier
   * @param {Function} [options.onProgress] - Progress callback (created, total)
   * @returns {AsyncGenerator<Object>} Yields created universes
   *
   * @example
   * const executor = new ParallelExecutor();
   * await executor.initialize();
   *
   * for await (const universe of executor.createUniverses(10000)) {
   *   console.log('Created:', universe.id.Q_ID);
   * }
   */
  async *createUniverses(count, options = {}) {
    if (!this._pool || this._isShutdown) {
      throw new Error('ParallelExecutor not initialized or shutdown');
    }

    const batchSize = options.batchSize || this._config.batchSize;
    let created = 0;

    while (created < count) {
      const batchCount = Math.min(batchSize, count - created);
      const tasks = [];

      // Create batch of tasks
      for (let i = 0; i < batchCount; i++) {
        tasks.push(
          this.runTask({
            type: TaskType.CREATE_UNIVERSE,
            params: {
              batchIndex: created + i,
              createdBy: options.createdBy,
            },
          })
        );
      }

      // Wait for batch to complete
      const results = await Promise.all(tasks);

      for (const universe of results) {
        created++;
        if (options.onProgress) {
          options.onProgress(created, count);
        }
        yield universe;
      }

      // GC hint between batches
      this._gcHint();
      this._updateMemoryStats();
    }
  }

  /**
   * Apply morphisms to universes in parallel
   *
   * @param {Array<Object>} universes - Universes to transform
   * @param {Object} morphismConfig - Morphism configuration
   * @param {Object} [options={}] - Apply options
   * @param {number} [options.batchSize] - Override batch size
   * @param {Function} [options.onProgress] - Progress callback
   * @returns {AsyncGenerator<Object>} Yields transformation results
   *
   * @example
   * const results = [];
   * for await (const result of executor.applyMorphismsParallel(universes, morphism)) {
   *   results.push(result);
   * }
   */
  async *applyMorphismsParallel(universes, morphismConfig, options = {}) {
    if (!this._pool || this._isShutdown) {
      throw new Error('ParallelExecutor not initialized or shutdown');
    }

    const batchSize = options.batchSize || this._config.batchSize;
    const total = universes.length;
    let processed = 0;

    for (let i = 0; i < total; i += batchSize) {
      const batch = universes.slice(i, i + batchSize);
      const tasks = batch.map((universe) =>
        this.runTask({
          type: TaskType.APPLY_MORPHISM,
          params: {
            universe,
            morphismConfig,
          },
        })
      );

      const results = await Promise.all(tasks);

      for (const result of results) {
        processed++;
        if (options.onProgress) {
          options.onProgress(processed, total);
        }
        yield result;
      }

      // GC hint between batches
      this._gcHint();
      this._updateMemoryStats();
    }
  }

  /**
   * Generate receipts in parallel
   *
   * @param {Array<Object>} operations - Operations to generate receipts for
   * @param {Object} [options={}] - Generation options
   * @param {number} [options.batchSize] - Override batch size
   * @param {string} [options.operationType='morphism'] - Operation type
   * @param {Function} [options.onProgress] - Progress callback
   * @returns {AsyncGenerator<Object>} Yields generated receipts
   *
   * @example
   * const receipts = [];
   * for await (const receipt of executor.generateReceiptsParallel(operations)) {
   *   receipts.push(receipt);
   * }
   */
  async *generateReceiptsParallel(operations, options = {}) {
    if (!this._pool || this._isShutdown) {
      throw new Error('ParallelExecutor not initialized or shutdown');
    }

    const batchSize = options.batchSize || this._config.batchSize;
    const operationType = options.operationType || 'morphism';
    const total = operations.length;
    let processed = 0;

    for (let i = 0; i < total; i += batchSize) {
      const batch = operations.slice(i, i + batchSize);
      const tasks = batch.map((op, idx) =>
        this.runTask({
          type: TaskType.GENERATE_RECEIPT,
          params: {
            universeID: op.universeID || `Q*_batch${i + idx}`,
            operations: op.deltas || [op],
            operationType,
            batchIndex: i + idx,
          },
        })
      );

      const results = await Promise.all(tasks);

      for (const receipt of results) {
        processed++;
        if (options.onProgress) {
          options.onProgress(processed, total);
        }
        yield receipt;
      }

      // GC hint between batches
      this._gcHint();
      this._updateMemoryStats();
    }
  }

  /**
   * Freeze universes in parallel
   *
   * @param {Array<Object>} universes - Universes to freeze
   * @param {Object} [options={}] - Freeze options
   * @param {number} [options.batchSize] - Override batch size
   * @param {Function} [options.onProgress] - Progress callback
   * @returns {AsyncGenerator<Object>} Yields frozen universes
   *
   * @example
   * const frozen = [];
   * for await (const frozenUniverse of executor.freezeUniversesParallel(universes)) {
   *   frozen.push(frozenUniverse);
   * }
   */
  async *freezeUniversesParallel(universes, options = {}) {
    if (!this._pool || this._isShutdown) {
      throw new Error('ParallelExecutor not initialized or shutdown');
    }

    const batchSize = options.batchSize || this._config.batchSize;
    const total = universes.length;
    let processed = 0;

    for (let i = 0; i < total; i += batchSize) {
      const batch = universes.slice(i, i + batchSize);
      const tasks = batch.map((universe) =>
        this.runTask({
          type: TaskType.FREEZE_UNIVERSE,
          params: {
            universe,
            quads: [],
          },
        })
      );

      const results = await Promise.all(tasks);

      for (const frozen of results) {
        processed++;
        if (options.onProgress) {
          options.onProgress(processed, total);
        }
        yield frozen;
      }

      // GC hint between batches
      this._gcHint();
      this._updateMemoryStats();
    }
  }

  /**
   * Collect all results from an async generator
   * Useful for operations where all results are needed
   *
   * @param {AsyncGenerator} generator - Async generator
   * @returns {Promise<Array>} All results
   */
  async collectAll(generator) {
    const results = [];
    for await (const item of generator) {
      results.push(item);
    }
    return results;
  }

  /**
   * Get current executor statistics
   *
   * @returns {ExecutorStats} Current statistics
   *
   * @example
   * const stats = executor.getStats();
   * console.log('Tasks completed:', stats.tasksCompleted);
   * console.log('Peak memory:', stats.peakMemoryMB, 'MB');
   */
  getStats() {
    const currentDuration = this._startTime ? Date.now() - this._startTime : 0;
    const throughput = currentDuration > 0
      ? (this._stats.tasksCompleted / currentDuration) * 1000
      : 0;

    return {
      ...this._stats,
      totalDurationMs: currentDuration,
      throughputPerSec: throughput,
    };
  }

  /**
   * Get pool information
   *
   * @returns {Object} Pool statistics
   */
  getPoolInfo() {
    if (!this._pool) {
      return { initialized: false };
    }

    return {
      initialized: true,
      threads: this._pool.threads?.length || this._config.workerCount,
      queueSize: this._pool.queueSize || 0,
      runTime: this._pool.runTime || 0,
      waitTime: this._pool.waitTime || 0,
      completed: this._pool.completed || 0,
    };
  }

  /**
   * Graceful shutdown
   * Waits for pending tasks to complete
   *
   * @param {Object} [options={}] - Shutdown options
   * @param {number} [options.timeout=30000] - Maximum wait time (ms)
   * @returns {Promise<void>}
   *
   * @example
   * await executor.shutdown();
   * console.log('Executor shutdown complete');
   */
  async shutdown(options = {}) {
    if (!this._pool || this._isShutdown) {
      return;
    }

    const timeout = options.timeout || 30000;
    this._isShutdown = true;

    this._logger.debug?.('ParallelExecutor shutting down...');

    try {
      // Wait for pending tasks with timeout
      await Promise.race([
        this._pool.destroy(),
        new Promise((_, reject) =>
          setTimeout(() => reject(new Error('Shutdown timeout')), timeout)
        ),
      ]);
    } catch (error) {
      this._logger.warn?.(`Shutdown warning: ${error.message}`);
    }

    const stats = this.getStats();
    this._stats.totalDurationMs = stats.totalDurationMs;
    this._stats.throughputPerSec = stats.throughputPerSec;

    this._pool = null;

    this._logger.debug?.('ParallelExecutor shutdown complete');
  }
}

/**
 * Create a ParallelExecutor instance
 * Factory function for convenience
 *
 * @param {Object} [options={}] - Configuration options
 * @returns {ParallelExecutor} New executor instance
 *
 * @example
 * import { createParallelExecutor } from './parallel-executor.mjs';
 * const executor = createParallelExecutor({ workerCount: 8 });
 * await executor.initialize();
 */
export function createParallelExecutor(options = {}) {
  return new ParallelExecutor(options);
}

/**
 * Run a quick 10k universe creation benchmark
 * Convenience function for testing and validation
 *
 * @param {Object} [options={}] - Benchmark options
 * @param {number} [options.count=10000] - Number of universes
 * @param {number} [options.workerCount=10] - Number of workers
 * @returns {Promise<Object>} Benchmark results
 *
 * @example
 * const results = await benchmark10k({ count: 1000 });
 * console.log('Throughput:', results.throughputPerSec, 'ops/sec');
 */
export async function benchmark10k(options = {}) {
  const count = options.count || 10000;
  const executor = createParallelExecutor({
    workerCount: options.workerCount || 10,
  });

  await executor.initialize();

  const startTime = Date.now();
  let created = 0;

  for await (const _universe of executor.createUniverses(count)) {
    created++;
  }

  const duration = Date.now() - startTime;
  const stats = executor.getStats();

  await executor.shutdown();

  return {
    count: created,
    durationMs: duration,
    throughputPerSec: (created / duration) * 1000,
    peakMemoryMB: stats.peakMemoryMB,
    passed: duration < 120000 && stats.peakMemoryMB < 512,
  };
}
