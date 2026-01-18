/**
 * @file YAWL WP13 - Multiple Instances with Design-Time Knowledge
 * @module @unrdf/yawl/multiple-instance/wp13-design-time
 *
 * @description
 * Implementation of Van der Aalst's WP13 pattern - Multiple instances created
 * based on design-time knowledge (static cardinality). All instances must
 * complete before workflow continues (AND-join synchronization).
 *
 * Key features:
 * - Spawn N instances where N is known at design time
 * - AND-join synchronization (wait for all to complete)
 * - Result aggregation
 * - Cancellation propagates to all instances
 * - Receipt generation for each instance
 *
 * @see https://www.workflowpatterns.com/patterns/control/multiple_instance/wcp13.php
 */

import { z } from 'zod';
import { createSyncBarrier } from './sync-barrier.mjs';

// =============================================================================
// Zod Schemas
// =============================================================================

/**
 * Task schema for multiple instance execution
 */
export const MultipleInstanceTaskSchema = z.object({
  /** Task identifier */
  id: z.string().min(1),
  /** Task name */
  name: z.string().min(1).optional(),
  /** Task executor function */
  execute: z.function().optional(),
  /** Pre-condition validator */
  validatePreCondition: z.function().optional(),
  /** Post-condition validator */
  validatePostCondition: z.function().optional(),
});

/**
 * Spawn options schema
 */
export const SpawnOptionsSchema = z.object({
  /** Timeout for all instances (milliseconds) */
  timeout: z.number().int().positive().default(30000),
  /** Cancel all on first failure */
  cancelOnFailure: z.boolean().default(true),
  /** Input data transformation function */
  transformInput: z.function().optional(),
  /** Barrier identifier */
  barrierId: z.string().min(1).optional(),
});

/**
 * Instance result schema
 */
export const InstanceResultSchema = z.object({
  /** Instance index */
  index: z.number().int().nonnegative(),
  /** Instance identifier */
  instanceId: z.string().min(1),
  /** Instance output data */
  outputData: z.unknown().optional(),
  /** Instance receipt */
  receipt: z.object({
    id: z.string(),
    taskInstanceId: z.string(),
    action: z.string(),
    timestamp: z.string(),
  }).passthrough().optional(),
  /** Whether instance succeeded */
  success: z.boolean(),
  /** Error if instance failed */
  error: z.string().optional(),
});

/**
 * Spawn result schema
 */
export const SpawnResultSchema = z.object({
  /** Whether all instances completed successfully */
  success: z.boolean(),
  /** Number of instances spawned */
  count: z.number().int().positive(),
  /** Individual instance results */
  instances: z.array(InstanceResultSchema),
  /** Aggregated receipts */
  receipts: z.array(z.object({
    id: z.string(),
  }).passthrough()),
  /** Barrier that synchronized instances */
  barrier: z.object({
    id: z.string(),
    count: z.number(),
    arrivals: z.number(),
    completed: z.boolean(),
  }),
  /** Total execution time (milliseconds) */
  executionTime: z.number().nonnegative(),
  /** Whether timeout occurred */
  timedOut: z.boolean().default(false),
  /** Failure messages */
  failures: z.array(z.string()).default([]),
});

// =============================================================================
// WP13 Implementation
// =============================================================================

/**
 * Spawn N instances with design-time knowledge (static cardinality)
 *
 * Creates N instances of a task, all executing concurrently, with AND-join
 * synchronization. The workflow continues only when all instances complete.
 *
 * @param {Object} task - Task definition
 * @param {string} task.id - Task identifier
 * @param {Function} task.execute - Task executor function
 * @param {number} count - Number of instances to spawn (known at design time)
 * @param {*} inputData - Input data for all instances
 * @param {import('./sync-barrier.mjs').SyncBarrier} [syncBarrier] - Optional sync barrier
 * @param {Object} [options] - Spawn options
 * @param {number} [options.timeout=30000] - Timeout in milliseconds
 * @param {boolean} [options.cancelOnFailure=true] - Cancel all on failure
 * @param {Function} [options.transformInput] - Transform input per instance
 * @returns {Promise<Object>} Spawn result with aggregated instances
 *
 * @example
 * const task = {
 *   id: 'process-item',
 *   execute: async (data) => ({ processed: data.value * 2 })
 * };
 * const result = await spawnInstancesDesignTime(
 *   task,
 *   3,
 *   { value: 10 }
 * );
 * // result.instances contains 3 completed instances
 */
export async function spawnInstancesDesignTime(
  task,
  count,
  inputData,
  syncBarrier = null,
  options = {}
) {
  // Validate inputs
  const validatedTask = MultipleInstanceTaskSchema.parse(task);
  const validatedOptions = SpawnOptionsSchema.parse(options);

  if (!Number.isInteger(count) || count < 1) {
    throw new Error(`Count must be positive integer, got ${count}`);
  }

  if (!validatedTask.execute) {
    throw new Error(`Task ${validatedTask.id} must have execute function`);
  }

  const startTime = Date.now();

  // Create or use provided barrier
  const barrier = syncBarrier ?? createSyncBarrier(count, {
    timeout: validatedOptions.timeout,
    cancelOnFailure: validatedOptions.cancelOnFailure,
    id: validatedOptions.barrierId,
  });

  // Track spawned instances for cancellation
  const instances = [];
  const instancePromises = [];

  // Spawn N instances
  for (let i = 0; i < count; i++) {
    const instanceId = `${validatedTask.id}-instance-${i}-${Date.now()}`;

    // Transform input data per instance
    const instanceInput = validatedOptions.transformInput
      ? validatedOptions.transformInput(i, inputData)
      : { ...inputData, _instanceIndex: i };

    // Create instance execution promise
    const instancePromise = executeInstance(
      validatedTask,
      instanceId,
      i,
      instanceInput,
      barrier
    );

    instances.push({ instanceId, index: i });
    instancePromises.push(instancePromise);
  }

  // Set up cancellation propagation
  barrier.onCancel(async (reason) => {
    // Note: Individual instances handle their own cancellation
    // This just logs the cancellation event
    console.warn(`Barrier cancelled: ${reason}`);
  });

  // Wait for barrier to release (all instances complete or timeout/failure)
  const barrierResult = await barrier.wait();

  const executionTime = Date.now() - startTime;

  // Build spawn result
  const result = {
    success: barrierResult.success,
    count,
    instances: barrierResult.instances.map(inst => {
      const instance = {
        index: instances.find(i => i.instanceId === inst.instanceId)?.index ?? -1,
        instanceId: inst.instanceId,
        outputData: inst.result,
        success: !inst.failed,
        error: inst.error,
      };
      // Only include receipt if present
      if (inst.receipt) {
        instance.receipt = inst.receipt;
      }
      return instance;
    }),
    receipts: barrierResult.receipts,
    barrier: {
      id: barrier.id,
      count: barrier.count,
      arrivals: barrierResult.completedCount,
      completed: true,
    },
    executionTime,
    timedOut: barrierResult.timedOut,
    failures: barrierResult.failures,
  };

  return SpawnResultSchema.parse(result);
}

/**
 * Execute a single instance
 * @param {Object} task - Task definition
 * @param {string} instanceId - Instance identifier
 * @param {number} index - Instance index
 * @param {*} inputData - Instance input data
 * @param {import('./sync-barrier.mjs').SyncBarrier} barrier - Sync barrier
 * @returns {Promise<void>}
 * @private
 */
async function executeInstance(task, instanceId, index, inputData, barrier) {
  try {
    // Execute task
    const result = await task.execute(inputData);

    // Generate receipt
    const receipt = {
      id: `receipt-${instanceId}-complete-${Date.now()}`,
      taskInstanceId: instanceId,
      taskId: task.id,
      action: 'complete',
      timestamp: new Date().toISOString(),
      instanceIndex: index,
      inputData,
      outputData: result,
    };

    // Arrive at barrier
    await barrier.arrive({
      instanceId,
      result,
      receipt,
      failed: false,
    });
  } catch (error) {
    // Handle instance failure
    await barrier.arrive({
      instanceId,
      result: null,
      failed: true,
      error: error.message ?? String(error),
    });
  }
}

// =============================================================================
// Convenience Functions
// =============================================================================

/**
 * Create a multiple instance task definition
 * @param {Object} config - Task configuration
 * @param {string} config.id - Task identifier
 * @param {string} [config.name] - Task name
 * @param {Function} config.execute - Task executor
 * @returns {Object} Task definition
 *
 * @example
 * const task = createMultipleInstanceTask({
 *   id: 'send-email',
 *   name: 'Send Email to Recipient',
 *   execute: async (data) => {
 *     await sendEmail(data.recipient, data.message);
 *     return { sent: true, recipient: data.recipient };
 *   }
 * });
 */
export function createMultipleInstanceTask(config) {
  return MultipleInstanceTaskSchema.parse(config);
}

/**
 * Calculate expected completion time (estimate)
 * @param {number} count - Number of instances
 * @param {number} avgInstanceTime - Average instance execution time (ms)
 * @returns {number} Expected completion time in milliseconds
 *
 * @example
 * const expectedTime = estimateCompletionTime(100, 500);
 * // expectedTime = 500 (all instances run in parallel)
 */
export function estimateCompletionTime(count, avgInstanceTime) {
  // In parallel execution, completion time = max(instance times) ≈ avgInstanceTime
  return avgInstanceTime;
}
