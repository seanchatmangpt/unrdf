/**
 * @file Parallel Hook Executor - Execute hooks in parallel using dependency graph and worker pool
 * @module hooks/parallel-executor
 */

import { DependencyGraph, buildExecutionLayers } from './dependency-graph.mjs';
import { WorkerPool, createWorkerPool } from './worker-pool.mjs';
import { z } from 'zod';

/* ========================================================================= */
/* Zod Schemas                                                              */
/* ========================================================================= */

export const ParallelExecutorConfigSchema = z.object({
  maxWorkers: z.number().int().positive().optional().default(4),
  taskTimeout: z.number().int().positive().optional().default(30000),
  enableCycleDetection: z.boolean().optional().default(true),
  fallbackToSequential: z.boolean().optional().default(true),
});

export const HookExecutionTaskSchema = z.object({
  hook: z.object({
    id: z.string(),
    name: z.string().optional(),
    run: z.function(),
    condition: z.any().optional(),
    dependsOn: z.array(z.string()).optional(),
  }),
  store: z.any(),
  delta: z.object({
    adds: z.array(z.any()).optional(),
    deletes: z.array(z.any()).optional(),
  }),
  options: z.object({
    env: z.any().optional(),
    debug: z.boolean().optional(),
  }).optional(),
});

/* ========================================================================= */
/* Parallel Executor Class                                                    */
/* ========================================================================= */

/**
 * Parallel hook executor using dependency graph and worker pool.
 *
 * @class ParallelHookExecutor
 */
export class ParallelHookExecutor {
  /**
   * Create a new parallel hook executor.
   *
   * @param {Object} [config={}] - Executor configuration
   * @param {number} [config.maxWorkers=4] - Maximum worker threads
   * @param {number} [config.taskTimeout=30000] - Task timeout in ms
   * @param {boolean} [config.enableCycleDetection=true] - Enable cycle detection
   * @param {boolean} [config.fallbackToSequential=true] - Fallback to sequential if cycle detected
   */
  constructor(config = {}) {
    const validatedConfig = ParallelExecutorConfigSchema.parse(config);

    this.maxWorkers = validatedConfig.maxWorkers;
    this.taskTimeout = validatedConfig.taskTimeout;
    this.enableCycleDetection = validatedConfig.enableCycleDetection;
    this.fallbackToSequential = validatedConfig.fallbackToSequential;

    // Create worker pool
    this.workerPool = createWorkerPool({
      minWorkers: 1,
      maxWorkers: this.maxWorkers,
      taskTimeout: this.taskTimeout,
    });

    /** @type {DependencyGraph} - Dependency graph for hooks */
    this.dependencyGraph = new DependencyGraph();
  }

  /**
   * Register hooks for execution.
   *
   * @param {Array<{id: string, name?: string, run: Function, condition?: any, dependsOn?: string[]}>} hooks
   *   - Hook definitions
   */
  registerHooks(hooks) {
    // Clear existing graph
    this.dependencyGraph.clear();

    // Add hooks to dependency graph
    for (const hook of hooks) {
      this.dependencyGraph.addNode(hook.id, hook.dependsOn || []);
    }
  }

  /**
   * Execute hooks in parallel based on dependency graph.
   *
   * @param {Array<{id: string, name?: string, run: Function, condition?: any, dependsOn?: string[]}>} hooks
   *   - Hooks to execute
   * @param {any} store - N3 Store instance
   * @param {{adds?: any[], deletes?: any[]}} delta - Proposed changes
   * @param {Object} [options={}] - Execution options
   * @returns {Promise<Array<{hookId: string, success: boolean, result?: any, error?: string}>>}
   */
  async execute(hooks, store, delta, options = {}) {
    // Build dependency graph
    this.registerHooks(hooks);

    // Build execution layers
    const { layers, hasCycle, cycle } = this.dependencyGraph.topologicalSort();

    if (hasCycle) {
      if (this.fallbackToSequential) {
        console.warn('Cycle detected in hook dependencies, falling back to sequential execution:', cycle);
        return this._executeSequential(hooks, store, delta, options);
      } else {
        throw new Error('Cycle detected in hook dependencies: ' + cycle.join(' -> '));
      }
    }

    // Execute each layer in parallel
    /** @type {Array<{hookId: string, success: boolean, result?: any, error?: string}>} */
    const results = [];

    for (const layer of layers) {
      const layerResults = await this._executeLayer(hooks, layer, store, delta, options);
      results.push(...layerResults);
    }

    return results;
  }

  /**
   * Execute a single layer of hooks in parallel.
   *
   * @private
   * @param {Array} hooks - All registered hooks
   * @param {string[]} layer - Hook IDs in this layer
   * @param {any} store - N3 Store instance
   * @param {{adds?: any[], deletes?: any[]}} delta - Proposed changes
   * @param {Object} options - Execution options
   * @returns {Promise<Array<{hookId: string, success: boolean, result?: any, error?: string}>>}
   */
  async _executeLayer(hooks, layer, store, delta, options) {
    // Create lookup map for hooks
    const hookMap = new Map(hooks.map(h => [h.id, h]));

    // Create tasks for worker pool
    const tasks = layer.map(hookId => ({
      taskFn: async (hook, store, delta, opts) => {
        try {
          const event = {
            store,
            delta,
            ...opts,
          };

          const result = await hook.run(event, opts);

          return {
            hookId: hook.id,
            success: true,
            result,
          };
        } catch (error) {
          return {
            hookId: hook.id,
            success: false,
            error: error.message,
          };
        }
      },
      args: [hookMap.get(hookId), store, delta, options],
    }));

    // Execute layer in parallel using worker pool
    const poolResults = await this.workerPool.executeBatch(tasks);

    // Unwrap worker pool results (handle both worker and direct execution formats)
    const layerResults = poolResults.map(poolResult => {
      if (poolResult.workerId === 'direct') {
        // Direct execution fallback - result is already unwrapped
        return poolResult.result;
      } else {
        // Worker execution - unwrap from { success, result } format
        return poolResult.result;
      }
    });

    return layerResults;
  }

  /**
   * Execute hooks sequentially (fallback for cycles).
   *
   * @private
   * @param {Array} hooks - Hooks to execute
   * @param {any} store - N3 Store instance
   * @param {{adds?: any[], deletes?: any[]}} delta - Proposed changes
   * @param {Object} options - Execution options
   * @returns {Promise<Array<{hookId: string, success: boolean, result?: any, error?: string}>>}
   */
  async _executeSequential(hooks, store, delta, options) {
    /** @type {Array<{hookId: string, success: boolean, result?: any, error?: string}>} */
    const results = [];

    for (const hook of hooks) {
      try {
        const event = {
          store,
          delta,
          ...options,
        };

        const result = await hook.run(event, options);

        results.push({
          hookId: hook.id,
          success: true,
          result,
        });
      } catch (error) {
        results.push({
          hookId: hook.id,
          success: false,
          error: error.message,
        });

        // Stop on first error in sequential mode
        break;
      }
    }

    return results;
  }

  /**
   * Get execution statistics.
   *
   * @returns {{graphStats: object, poolStats: object}} - Statistics from graph and worker pool
   */
  getStats() {
    return {
      graphStats: this.dependencyGraph.getStats(),
      poolStats: this.workerPool.getStats(),
    };
  }

  /**
   * Shutdown the executor and clean up resources.
   *
   * @returns {Promise<void>}
   */
  async shutdown() {
    await this.workerPool.shutdown();
    this.dependencyGraph.clear();
  }
}

/* ========================================================================= */
/* Factory Functions                                                          */
/* ========================================================================= */

/**
 * Create a parallel hook executor.
 *
 * @param {Object} [config={}] - Executor configuration
 * @returns {ParallelHookExecutor} - New executor instance
 */
export function createParallelExecutor(config = {}) {
  return new ParallelHookExecutor(config);
}

/* ========================================================================= */
/* Convenience Functions                                                      */
/* ========================================================================= */

/**
 * Execute hooks in parallel (convenience function).
 *
 * @param {Array<{id: string, name?: string, run: Function, condition?: any, dependsOn?: string[]}>} hooks
 *   - Hooks to execute
 * @param {any} store - N3 Store instance
 * @param {{adds?: any[], deletes?: any[]}} delta - Proposed changes
 * @param {Object} [options={}] - Execution options
 * @param {Object} [executorConfig={}] - Executor configuration
 * @returns {Promise<Array<{hookId: string, success: boolean, result?: any, error?: string}>>}
 */
export async function executeHooksParallel(hooks, store, delta, options = {}, executorConfig = {}) {
  const executor = createParallelExecutor(executorConfig);
  try {
    return await executor.execute(hooks, store, delta, options);
  } finally {
    await executor.shutdown();
  }
}
