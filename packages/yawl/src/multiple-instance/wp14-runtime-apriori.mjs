/**
 * @file WP14 - Multiple Instances with Runtime A Priori Knowledge
 * @module @unrdf/yawl/multiple-instance/wp14-runtime-apriori
 * @description
 * Implements YAWL WP14 pattern: Multiple instances where count is determined
 * at runtime BEFORE spawning instances. All instances synchronize on completion.
 *
 * Pattern: Spawn N instances based on runtime expression evaluation, then AND-join.
 * Example: "Process one instance per item in input collection"
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { now } from '@unrdf/kgc-4d';
import { evaluateExpression } from './expression-evaluator.mjs';
import { TaskInstance } from '../task-core.mjs';
import { createSyncBarrier, SyncBarrier } from './sync-barrier.mjs';

// =============================================================================
// Schemas
// =============================================================================

/**
 * WP14 execution result schema
 */
export const WP14ResultSchema = z.object({
  success: z.boolean(),
  instances: z.array(z.any()),
  barrier: z.any(),
  countEvaluation: z.object({
    count: z.number().int().nonnegative(),
    expression: z.string(),
    evaluatedAt: z.bigint(),
  }),
  receipt: z.object({
    id: z.string(),
    pattern: z.literal('WP14'),
    patternName: z.string().optional(),
    taskId: z.string(),
    timestamp: z.bigint(),
    executionTime: z.bigint().optional(),
    hash: z.string(),
    countEvaluation: z.object({
      expression: z.string(),
      type: z.string(),
      count: z.number().int().nonnegative(),
      evaluatedAt: z.bigint(),
    }),
    barrier: z.object({
      id: z.string(),
      totalInstances: z.number().int().positive(),
    }),
  }),
});

// =============================================================================
// Instance Data Slicing
// =============================================================================


/**
 * Slice input data for per-instance distribution
 * If inputData.items is array, distribute one item per instance
 * Otherwise, each instance gets full inputData with instance index
 *
 * @param {Object} inputData - Input data
 * @param {number} instanceCount - Number of instances
 * @returns {Array<Object>} Per-instance input data
 */
export function sliceInputData(inputData, instanceCount) {
  const slices = [];

  // Check for items array
  if (inputData.items && Array.isArray(inputData.items)) {
    const items = inputData.items;

    if (items.length !== instanceCount) {
      console.warn(
        `Item count (${items.length}) != instance count (${instanceCount}). ` +
          `Distributing available items.`
      );
    }

    // Distribute items to instances
    for (let i = 0; i < instanceCount; i++) {
      slices.push({
        ...inputData,
        item: items[i] ?? null,
        itemIndex: i,
        totalInstances: instanceCount,
      });
    }
  } else {
    // No items array - each instance gets full data with index
    for (let i = 0; i < instanceCount; i++) {
      slices.push({
        ...inputData,
        instanceIndex: i,
        totalInstances: instanceCount,
      });
    }
  }

  return slices;
}

// =============================================================================
// WP14 Implementation
// =============================================================================

/**
 * Spawn multiple instances with runtime a priori count determination
 *
 * Workflow:
 * 1. Evaluate count expression to determine N
 * 2. Create barrier for N instances
 * 3. Spawn N task instances with sliced input data
 * 4. Return instances and barrier for synchronization
 *
 * @param {Object} task - Task definition or TaskInstance
 * @param {string|Object} countExpression - Expression to evaluate for count
 * @param {Object} inputData - Input data for instances
 * @param {Object} [options] - Additional options
 * @param {Object} [options.barrier] - Existing barrier (optional)
 * @param {string} [options.caseId] - Case ID for instances
 * @returns {Promise<Object>} Result with instances, barrier, and receipt
 *
 * @example
 * const result = await spawnInstancesRuntimeApriori(
 *   taskDef,
 *   "count($.items)",
 *   { items: [1, 2, 3, 4, 5] }
 * );
 * // Spawns 5 instances, each with one item
 */
export async function spawnInstancesRuntimeApriori(task, countExpression, inputData, options = {}) {
  const startTime = now();

  // Step 1: Evaluate count expression
  const evaluation = await evaluateExpression(countExpression, inputData);
  const { count } = evaluation;

  if (count === 0) {
    throw new Error('WP14: Count expression evaluated to 0, cannot spawn instances');
  }

  // Step 2: Create or use existing barrier
  const barrier =
    options.barrier instanceof SyncBarrier
      ? options.barrier
      : createSyncBarrier(count, {
          id: options.barrierId,
          threshold: options.threshold,
          timeout: options.timeout,
        });

  if (barrier.count !== count) {
    throw new Error(`Barrier count mismatch: expected ${count}, got ${barrier.count}`);
  }

  // Step 3: Slice input data per instance
  const dataSlices = sliceInputData(inputData, count);

  // Step 4: Create task instances
  const instances = [];
  const caseId = options.caseId ?? `case-${Date.now()}`;

  for (let i = 0; i < count; i++) {
    // Create instance
    let instance;

    if (task instanceof TaskInstance) {
      // Clone existing TaskInstance
      instance = new TaskInstance(task.taskDefinition, caseId, {
        inputData: dataSlices[i],
        id: `${caseId}-${task.taskDefId}-instance-${i}`,
      });
    } else {
      // Create new TaskInstance from definition
      instance = new TaskInstance(task, caseId, {
        inputData: dataSlices[i],
        id: `${caseId}-${task.id}-instance-${i}`,
      });
    }

    // Attach barrier reference and index
    instance._barrierId = barrier.id;
    instance._instanceIndex = i;

    instances.push(instance);

    // If we have an execute function, run it and report to barrier
    if (options.execute) {
      options.execute(instance).then(
        result => barrier.arrive({ instanceId: instance.id, result, failed: false }),
        error => barrier.arrive({ instanceId: instance.id, error: error.message, failed: true })
      );
    }
  }

  // Step 5: Wait for barrier if requested (default to true for WP14 compliance)
  const shouldWait = options.wait !== false;
  let barrierResult = null;
  if (shouldWait) {
    barrierResult = await barrier.wait();
  }

  // Step 6: Generate receipt
  const receipt = await generateWP14Receipt(
    task,
    countExpression,
    evaluation,
    barrier,
    instances,
    startTime
  );

  return WP14ResultSchema.parse({
    success: shouldWait ? barrierResult.success : true,
    instances,
    barrier: barrier.getState(),
    countEvaluation: {
      count: evaluation.count,
      expression: evaluation.expression,
      evaluatedAt: evaluation.evaluatedAt,
    },
    receipt,
  });
}

/**
 * Generate WP14 execution receipt
 * @param {Object} task - Task definition
 * @param {string|Object} countExpression - Count expression
 * @param {Object} evaluation - Evaluation result
 * @param {Object} barrier - Barrier object
 * @param {Array<TaskInstance>} instances - Spawned instances
 * @param {bigint} startTime - Execution start time
 * @returns {Promise<Object>} Receipt object
 */
async function generateWP14Receipt(
  task,
  countExpression,
  evaluation,
  barrier,
  instances,
  startTime
) {
  const receipt = {
    id: `wp14-receipt-${barrier.id}`,
    pattern: 'WP14',
    patternName: 'Multiple Instances with Runtime A Priori Knowledge',
    taskId: task.id ?? task.taskDefId,
    timestamp: now(),
    executionTime: now() - startTime,
    countEvaluation: {
      expression:
        typeof countExpression === 'string' ? countExpression : countExpression.expression,
      type: evaluation.type,
      count: evaluation.count,
      evaluatedAt: evaluation.evaluatedAt,
      proof: evaluation.proof,
    },
    barrier: {
      id: barrier.id,
      totalInstances: barrier.totalInstances,
      createdAt: barrier.createdAt,
    },
    instances: instances.map(inst => ({
      id: inst.id,
      instanceIndex: inst._instanceIndex,
    })),
  };

  // Compute receipt hash
  const receiptData = JSON.stringify({
    pattern: receipt.pattern,
    taskId: receipt.taskId,
    count: evaluation.count,
    expression: receipt.countEvaluation.expression,
    barrierId: barrier.id,
    instanceIds: receipt.instances.map(i => i.id),
  });

  receipt.hash = await blake3(receiptData);

  return receipt;
}

/**
 * Wait for all instances in barrier to complete
 * @param {Object} barrier - Barrier object
 * @param {Array<TaskInstance>} instances - Task instances
 * @param {Object} [options] - Wait options
 * @param {number} [options.timeout] - Timeout in milliseconds
 * @returns {Promise<Object>} Completion result
 */
export async function waitForBarrier(barrier, instances, options = {}) {
  const timeout = options.timeout ?? 30000; // 30s default
  const startTime = Date.now();

  return new Promise((resolve, reject) => {
    const checkInterval = setInterval(() => {
      // Check timeout
      if (Date.now() - startTime > timeout) {
        clearInterval(checkInterval);
        reject(
          new Error(
            `Barrier timeout: ${barrier.completedInstances}/${barrier.totalInstances} completed`
          )
        );
        return;
      }

      // Check completion
      if (isBarrierComplete(barrier)) {
        clearInterval(checkInterval);
        resolve({
          barrier,
          instances,
          completedAt: barrier.completedAt,
        });
      }
    }, 100); // Check every 100ms
  });
}
