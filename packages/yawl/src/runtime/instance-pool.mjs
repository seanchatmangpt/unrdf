/**
 * Minimal Instance Pool - Execution Management
 *
 * BUILT BY COPYING patterns from instance-tracker.mjs and wp14-runtime-apriori.mjs
 * NO new dependencies, NO invention.
 *
 * @module @unrdf/yawl/runtime/instance-pool
 */

import { z } from 'zod';
import { now } from '@unrdf/kgc-4d';
import { InstanceStatus } from './task-spawner.mjs';

// =============================================================================
// Schemas (Copied from instance-tracker.mjs)
// =============================================================================

/**
 * Aggregate status schema
 */
export const AggregateStatusSchema = z.object({
  parentTaskId: z.string().min(1),
  totalInstances: z.number().int().nonnegative(),
  statusCounts: z.object({
    spawned: z.number().int().nonnegative(),
    enabled: z.number().int().nonnegative(),
    active: z.number().int().nonnegative(),
    completed: z.number().int().nonnegative(),
    failed: z.number().int().nonnegative(),
    cancelled: z.number().int().nonnegative(),
  }),
  allCompleted: z.boolean(),
  anyFailed: z.boolean(),
  firstSpawnedAt: z.bigint().optional(),
  lastCompletedAt: z.bigint().optional(),
  instances: z.object({
    spawned: z.array(z.string()),
    enabled: z.array(z.string()),
    active: z.array(z.string()),
    completed: z.array(z.string()),
    failed: z.array(z.string()),
    cancelled: z.array(z.string()),
  }),
});

// =============================================================================
// Instance Pool (Copied from MultiInstanceTracker)
// =============================================================================

/**
 * Minimal instance pool for tracking and execution
 *
 * PATTERN SOURCE: instance-tracker.mjs MultiInstanceTracker class
 */
export class InstancePool {
  constructor() {
    // Map: instanceId -> instance record (pattern from instance-tracker.mjs)
    this._instances = new Map();

    // Map: parentTaskId -> Set<instanceId> (pattern from instance-tracker.mjs)
    this._parentToInstances = new Map();
  }

  /**
   * Register instance in pool
   * @param {Object} instance - Instance record
   */
  register(instance) {
    this._instances.set(instance.id, instance);

    // Update parent mapping
    const parentSet = this._parentToInstances.get(instance.parentTaskId) || new Set();
    parentSet.add(instance.id);
    this._parentToInstances.set(instance.parentTaskId, parentSet);
  }

  /**
   * Get instance by ID
   * @param {string} instanceId - Instance ID
   * @returns {Object|null} Instance record
   */
  get(instanceId) {
    return this._instances.get(instanceId) ?? null;
  }

  /**
   * Get all instances for parent task
   * @param {string} parentTaskId - Parent task ID
   * @returns {Array<Object>} Instance records
   */
  getByParent(parentTaskId) {
    const instanceIds = this._parentToInstances.get(parentTaskId) || new Set();
    return Array.from(instanceIds).map(id => this._instances.get(id)).filter(Boolean);
  }

  /**
   * Update instance status
   * @param {string} instanceId - Instance ID
   * @param {string} newStatus - New status
   */
  updateStatus(instanceId, newStatus) {
    const instance = this._instances.get(instanceId);
    if (!instance) {
      throw new Error(`Instance ${instanceId} not found in pool`);
    }

    instance.status = newStatus;

    if (newStatus === InstanceStatus.COMPLETED || newStatus === InstanceStatus.FAILED || newStatus === InstanceStatus.CANCELLED) {
      instance.completedAt = now();
    }
  }

  /**
   * Get aggregate status for parent task
   *
   * PATTERN SOURCE: instance-tracker.mjs getAggregateStatus()
   *
   * @param {string} parentTaskId - Parent task ID
   * @returns {Object} Aggregate status
   */
  getAggregateStatus(parentTaskId) {
    const instances = this.getByParent(parentTaskId);

    // Status counts (pattern from instance-tracker.mjs)
    const statusCounts = {
      spawned: 0,
      enabled: 0,
      active: 0,
      completed: 0,
      failed: 0,
      cancelled: 0,
    };

    const instancesByStatus = {
      spawned: [],
      enabled: [],
      active: [],
      completed: [],
      failed: [],
      cancelled: [],
    };

    let firstSpawnedAt = null;
    let lastCompletedAt = null;

    for (const instance of instances) {
      statusCounts[instance.status]++;
      instancesByStatus[instance.status].push(instance.id);

      if (!firstSpawnedAt || instance.spawnedAt < firstSpawnedAt) {
        firstSpawnedAt = instance.spawnedAt;
      }

      if (instance.completedAt && (!lastCompletedAt || instance.completedAt > lastCompletedAt)) {
        lastCompletedAt = instance.completedAt;
      }
    }

    return {
      parentTaskId,
      totalInstances: instances.length,
      statusCounts,
      allCompleted: statusCounts.completed === instances.length,
      anyFailed: statusCounts.failed > 0,
      firstSpawnedAt,
      lastCompletedAt,
      instances: instancesByStatus,
    };
  }

  /**
   * Remove instance from pool
   * @param {string} instanceId - Instance ID
   */
  remove(instanceId) {
    const instance = this._instances.get(instanceId);
    if (instance) {
      this._instances.delete(instanceId);

      const parentSet = this._parentToInstances.get(instance.parentTaskId);
      if (parentSet) {
        parentSet.delete(instanceId);
        if (parentSet.size === 0) {
          this._parentToInstances.delete(instance.parentTaskId);
        }
      }
    }
  }

  /**
   * Clear all instances for parent task
   * @param {string} parentTaskId - Parent task ID
   */
  clearParent(parentTaskId) {
    const instanceIds = this._parentToInstances.get(parentTaskId);
    if (instanceIds) {
      for (const id of instanceIds) {
        this._instances.delete(id);
      }
      this._parentToInstances.delete(parentTaskId);
    }
  }

  /**
   * Get pool size
   * @returns {number} Total instances in pool
   */
  size() {
    return this._instances.size;
  }
}

// =============================================================================
// Global Instance Pool (Singleton pattern from instance-tracker.mjs)
// =============================================================================

/**
 * Global instance pool singleton
 */
export const globalInstancePool = new InstancePool();

// =============================================================================
// Execution Functions (Simple Promise.all pattern)
// =============================================================================

/**
 * Execute instances in parallel
 *
 * PATTERN: Promise.all() from WP12 spawning logic
 *
 * @param {Array<Object>} instances - Instance records
 * @param {Function} executorFn - Executor function (instance) => Promise<outputData>
 * @param {Object} [options] - Execution options
 * @returns {Promise<Array<Object>>} Completed instances
 *
 * @example
 * const results = await executeInstances(instances, async (instance) => {
 *   // Execute instance work
 *   return { result: 'success' };
 * });
 */
export async function executeInstances(instances, executorFn, options = {}) {
  const pool = options.pool ?? globalInstancePool;

  // Register instances in pool
  for (const instance of instances) {
    pool.register(instance);
  }

  // Execute in parallel (Promise.all pattern from WP12)
  const results = await Promise.all(
    instances.map(async (instance) => {
      try {
        // Update to active
        pool.updateStatus(instance.id, InstanceStatus.ACTIVE);

        // Execute instance work
        const outputData = await executorFn(instance);

        // Update to completed
        pool.updateStatus(instance.id, InstanceStatus.COMPLETED);
        instance.outputData = outputData;

        return instance;
      } catch (error) {
        // Update to failed
        pool.updateStatus(instance.id, InstanceStatus.FAILED);
        instance.error = error instanceof Error ? error.message : String(error);

        if (options.failFast) {
          throw error;
        }

        return instance;
      }
    })
  );

  return results;
}

/**
 * Execute instances sequentially
 *
 * @param {Array<Object>} instances - Instance records
 * @param {Function} executorFn - Executor function
 * @param {Object} [options] - Execution options
 * @returns {Promise<Array<Object>>} Completed instances
 */
export async function executeInstancesSequential(instances, executorFn, options = {}) {
  const pool = options.pool ?? globalInstancePool;
  const results = [];

  // Register instances in pool
  for (const instance of instances) {
    pool.register(instance);
  }

  // Execute sequentially
  for (const instance of instances) {
    try {
      pool.updateStatus(instance.id, InstanceStatus.ACTIVE);

      const outputData = await executorFn(instance);

      pool.updateStatus(instance.id, InstanceStatus.COMPLETED);
      instance.outputData = outputData;

      results.push(instance);
    } catch (error) {
      pool.updateStatus(instance.id, InstanceStatus.FAILED);
      instance.error = error instanceof Error ? error.message : String(error);

      results.push(instance);

      if (options.failFast) {
        throw error;
      }
    }
  }

  return results;
}

/**
 * Wait for all instances to complete
 *
 * PATTERN: Simple polling from WP14 waitForBarrier()
 *
 * @param {string} parentTaskId - Parent task ID
 * @param {Object} [options] - Wait options
 * @returns {Promise<Object>} Aggregate status
 */
export async function waitForCompletion(parentTaskId, options = {}) {
  const pool = options.pool ?? globalInstancePool;
  const checkInterval = options.checkInterval ?? 100; // ms
  const timeout = options.timeout ?? 30000; // 30s default
  const startTime = Date.now();

  while (true) {
    const status = pool.getAggregateStatus(parentTaskId);

    if (status.allCompleted) {
      return status;
    }

    if (Date.now() - startTime > timeout) {
      throw new Error(`Timeout waiting for instances of ${parentTaskId} to complete`);
    }

    await new Promise(resolve => setTimeout(resolve, checkInterval));
  }
}

/**
 * Get completion percentage
 *
 * @param {string} parentTaskId - Parent task ID
 * @param {Object} [options] - Options
 * @returns {number} Percentage (0-100)
 */
export function getCompletionPercentage(parentTaskId, options = {}) {
  const pool = options.pool ?? globalInstancePool;
  const status = pool.getAggregateStatus(parentTaskId);

  if (status.totalInstances === 0) return 0;

  const terminal = status.statusCounts.completed + status.statusCounts.failed + status.statusCounts.cancelled;
  return Math.round((terminal / status.totalInstances) * 100);
}
