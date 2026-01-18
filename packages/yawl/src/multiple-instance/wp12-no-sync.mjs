/**
 * YAWL WP12 - Multiple Instances without Synchronization
 *
 * Pattern: Within a given process instance, multiple instances of a task can be
 * created. These instances are independent and run to completion without synchronization.
 *
 * Key characteristics:
 * - No synchronization barrier
 * - Instances complete independently
 * - Each instance gets unique input data
 * - Parent task doesn't wait for completion
 *
 * @module @unrdf/yawl/multiple-instance/wp12-no-sync
 */

import { z } from 'zod';
import { now } from '@unrdf/kgc-4d';
import { blake3 } from 'hash-wasm';
import { TaskInstance, TaskDefinition } from '../task-core.mjs';
import { generateReceiptBatch } from '../receipt-batch.mjs';
import {
  MultiInstanceTracker,
  globalInstanceTracker,
  InstanceStatus,
} from './instance-tracker.mjs';

// =============================================================================
// Zod Schemas
// =============================================================================

/**
 * Spawn options schema
 */
export const SpawnOptionsSchema = z.object({
  /** Base input data for all instances */
  baseInputData: z.record(z.unknown()).default({}),
  /** Per-instance input data array */
  instanceInputs: z.array(z.record(z.unknown())).optional(),
  /** Instance tracker to use */
  tracker: z.any().optional(),
  /** Generate receipts? */
  generateReceipts: z.boolean().default(true),
  /** Use batch receipt generation? */
  useBatchReceipts: z.boolean().default(true),
  /** Instance ID prefix */
  instanceIdPrefix: z.string().optional(),
});

/**
 * Spawn result schema
 */
export const SpawnResultSchema = z.object({
  /** Parent task instance ID */
  parentTaskId: z.string().min(1),
  /** Spawned instance IDs */
  instanceIds: z.array(z.string().min(1)),
  /** Task instances */
  instances: z.array(z.any()),
  /** Instance receipts */
  instanceReceipts: z.array(z.any()).optional(),
  /** Aggregate receipt */
  aggregateReceipt: z.any().optional(),
  /** Spawn metadata */
  metadata: z.object({
    totalInstances: z.number().int().positive(),
    spawnedAt: z.bigint(),
    completedAt: z.bigint().optional(),
    duration: z.number().optional(),
  }),
});

// =============================================================================
// Core WP12 Functions
// =============================================================================

/**
 * Spawn multiple task instances without synchronization (WP12)
 *
 * Creates N independent instances of a task that run to completion
 * without any synchronization barrier. Each instance:
 * - Has unique input data
 * - Runs independently
 * - Generates its own receipt
 * - Does not block other instances
 *
 * @param {TaskDefinition|Object} taskDef - Task definition
 * @param {string} caseId - Case ID
 * @param {number} count - Number of instances to spawn
 * @param {Object} [options] - Spawn options
 * @param {Object} [options.baseInputData] - Base input for all instances
 * @param {Array<Object>} [options.instanceInputs] - Per-instance inputs
 * @param {MultiInstanceTracker} [options.tracker] - Instance tracker
 * @param {boolean} [options.generateReceipts=true] - Generate receipts?
 * @param {boolean} [options.useBatchReceipts=true] - Use batch receipts?
 * @param {string} [options.instanceIdPrefix] - Instance ID prefix
 * @returns {Promise<SpawnResult>}
 *
 * @example
 * const result = await spawnInstancesNoSync(
 *   taskDef,
 *   'case-123',
 *   5,
 *   {
 *     baseInputData: { sharedParam: 'value' },
 *     instanceInputs: [
 *       { index: 0, data: 'a' },
 *       { index: 1, data: 'b' },
 *       { index: 2, data: 'c' },
 *       { index: 3, data: 'd' },
 *       { index: 4, data: 'e' },
 *     ]
 *   }
 * );
 * // Returns: { instanceIds: [...], instances: [...], aggregateReceipt: {...} }
 */
export async function spawnInstancesNoSync(taskDef, caseId, count, options = {}) {
  const startTime = performance.now();

  // Validate inputs
  if (count <= 0) {
    throw new Error(`Instance count must be positive, got: ${count}`);
  }

  // Extract options with defaults
  const {
    baseInputData = {},
    instanceInputs,
    tracker = globalInstanceTracker,
    generateReceipts = true,
    useBatchReceipts = true,
    instanceIdPrefix,
  } = options;

  // Validate instance inputs length if provided
  if (instanceInputs && instanceInputs.length !== count) {
    throw new Error(
      `instanceInputs length (${instanceInputs.length}) must match count (${count})`
    );
  }

  // Ensure we have a TaskDefinition instance
  const definition = taskDef instanceof TaskDefinition
    ? taskDef
    : new TaskDefinition(taskDef);

  const spawnedAt = now();
  const parentTaskId = `${caseId}-${definition.id}-mi-parent-${Date.now()}`;

  // Create instances
  const instances = [];
  const instanceIds = [];
  const receiptEvents = [];

  for (let i = 0; i < count; i++) {
    // Generate instance ID
    const prefix = instanceIdPrefix ?? `${caseId}-${definition.id}`;
    const instanceId = `${prefix}-mi-${i}-${Date.now()}-${i}`;

    // Merge base input with instance-specific input
    const instanceInput = {
      ...baseInputData,
      ...(instanceInputs?.[i] ?? {}),
      __mi_metadata: {
        parentTaskId,
        instanceIndex: i,
        totalInstances: count,
        spawnedAt: spawnedAt.toString(),
      },
    };

    // Create task instance
    const instance = new TaskInstance(definition, caseId, {
      id: instanceId,
      inputData: instanceInput,
    });

    instances.push(instance);
    instanceIds.push(instanceId);

    // Register with tracker
    tracker.registerInstance({
      instanceId,
      parentTaskId,
      instanceIndex: i,
      totalInstances: count,
      instanceInputData: instanceInputs?.[i],
    });

    // Prepare receipt event if needed
    if (generateReceipts) {
      receiptEvents.push({
        eventType: 'TASK_ENABLED',
        caseId,
        taskId: instanceId,
        workItemId: `wi-${instanceId}`,
        payload: {
          decision: 'SPAWN_MI_INSTANCE',
          justification: {
            reasoning: `WP12 instance ${i + 1}/${count} spawned without synchronization`,
            conditionChecked: 'WP12_NO_SYNC',
          },
          actor: 'system',
          context: {
            pattern: 'WP12',
            parentTaskId,
            instanceIndex: i,
            totalInstances: count,
          },
        },
      });
    }
  }

  // Generate receipts
  let instanceReceipts;
  let aggregateReceipt;

  if (generateReceipts) {
    if (useBatchReceipts && receiptEvents.length > 1) {
      // Use batch receipt generation for performance
      const { receipts } = await generateReceiptBatch(receiptEvents);
      instanceReceipts = receipts;
    } else {
      // Generate receipts individually
      instanceReceipts = [];
      let prevReceipt = null;
      for (const event of receiptEvents) {
        const receipt = await generateSingleReceipt(event, prevReceipt);
        instanceReceipts.push(receipt);
        prevReceipt = receipt;
      }
    }

    // Create aggregate receipt for the MI operation
    aggregateReceipt = await generateAggregateReceipt({
      parentTaskId,
      caseId,
      taskDefId: definition.id,
      instanceIds,
      instanceReceipts,
      spawnedAt,
      count,
    });
  }

  const completedAt = now();
  const duration = performance.now() - startTime;

  return {
    parentTaskId,
    instanceIds,
    instances,
    instanceReceipts,
    aggregateReceipt,
    metadata: {
      totalInstances: count,
      spawnedAt,
      completedAt,
      duration,
    },
  };
}

/**
 * Generate a single receipt (helper for non-batch mode)
 *
 * @param {Object} event - Receipt event
 * @param {Object|null} previousReceipt - Previous receipt for chaining
 * @returns {Promise<Object>}
 */
async function generateSingleReceipt(event, previousReceipt) {
  const { generateReceipt: genReceipt } = await import('../receipt-core.mjs');
  return genReceipt(event, previousReceipt);
}

/**
 * Generate aggregate receipt for MI operation
 *
 * @param {Object} options - Aggregate receipt options
 * @returns {Promise<Object>}
 */
async function generateAggregateReceipt(options) {
  const {
    parentTaskId,
    caseId,
    taskDefId,
    instanceIds,
    instanceReceipts,
    spawnedAt,
    count,
  } = options;

  // Compute Merkle root of instance receipt hashes
  const receiptHashes = instanceReceipts.map(r => r.receiptHash);
  const merkleRoot = await computeMerkleRoot(receiptHashes);

  const aggregateEvent = {
    eventType: 'CONTROL_FLOW_EVALUATED',
    caseId,
    taskId: parentTaskId,
    payload: {
      decision: 'SPAWN_MI_NO_SYNC',
      justification: {
        reasoning: `WP12: Spawned ${count} independent instances without synchronization`,
        conditionChecked: 'WP12_PATTERN',
      },
      actor: 'system',
      context: {
        pattern: 'WP12',
        parentTaskId,
        taskDefId,
        totalInstances: count,
        instanceIds,
        merkleRoot,
        spawnedAt: spawnedAt.toString(),
      },
    },
  };

  const { generateReceipt: genReceipt } = await import('../receipt-core.mjs');
  return genReceipt(aggregateEvent, null);
}

/**
 * Compute Merkle root of hashes
 *
 * @param {string[]} hashes - BLAKE3 hashes
 * @returns {Promise<string>}
 */
async function computeMerkleRoot(hashes) {
  if (hashes.length === 0) return blake3('EMPTY_TREE');
  if (hashes.length === 1) return hashes[0];

  // Simple binary Merkle tree
  let level = [...hashes];

  while (level.length > 1) {
    const nextLevel = [];

    for (let i = 0; i < level.length; i += 2) {
      if (i + 1 < level.length) {
        // Hash pair
        const combined = `${level[i]}:${level[i + 1]}`;
        nextLevel.push(await blake3(combined));
      } else {
        // Odd node, promote to next level
        nextLevel.push(level[i]);
      }
    }

    level = nextLevel;
  }

  return level[0];
}

// =============================================================================
// Instance Lifecycle Functions
// =============================================================================

/**
 * Mark instance as enabled
 *
 * @param {string} instanceId - Instance ID
 * @param {MultiInstanceTracker} [tracker] - Tracker to use
 */
export function enableInstance(instanceId, tracker = globalInstanceTracker) {
  tracker.updateInstanceStatus(instanceId, InstanceStatus.ENABLED);
}

/**
 * Mark instance as active (started)
 *
 * @param {string} instanceId - Instance ID
 * @param {MultiInstanceTracker} [tracker] - Tracker to use
 */
export function startInstance(instanceId, tracker = globalInstanceTracker) {
  tracker.updateInstanceStatus(instanceId, InstanceStatus.ACTIVE);
}

/**
 * Mark instance as completed
 *
 * @param {string} instanceId - Instance ID
 * @param {Object} [outputData] - Instance output data
 * @param {MultiInstanceTracker} [tracker] - Tracker to use
 */
export function completeInstance(instanceId, outputData = {}, tracker = globalInstanceTracker) {
  tracker.updateInstanceStatus(instanceId, InstanceStatus.COMPLETED, { outputData });
}

/**
 * Mark instance as failed
 *
 * @param {string} instanceId - Instance ID
 * @param {Error|string} error - Error
 * @param {MultiInstanceTracker} [tracker] - Tracker to use
 */
export function failInstance(instanceId, error, tracker = globalInstanceTracker) {
  const errorMsg = error instanceof Error ? error.message : error;
  tracker.updateInstanceStatus(instanceId, InstanceStatus.FAILED, { error: errorMsg });
}

/**
 * Mark instance as cancelled
 *
 * @param {string} instanceId - Instance ID
 * @param {MultiInstanceTracker} [tracker] - Tracker to use
 */
export function cancelInstance(instanceId, tracker = globalInstanceTracker) {
  tracker.updateInstanceStatus(instanceId, InstanceStatus.CANCELLED);
}

// =============================================================================
// Query Functions
// =============================================================================

/**
 * Get aggregate status for MI task
 *
 * @param {string} parentTaskId - Parent task ID
 * @param {MultiInstanceTracker} [tracker] - Tracker to use
 * @returns {Object} Aggregate status
 */
export function getAggregateStatus(parentTaskId, tracker = globalInstanceTracker) {
  return tracker.getAggregateStatus(parentTaskId);
}

/**
 * Check if all instances are complete
 *
 * @param {string} parentTaskId - Parent task ID
 * @param {MultiInstanceTracker} [tracker] - Tracker to use
 * @returns {boolean}
 */
export function areAllInstancesComplete(parentTaskId, tracker = globalInstanceTracker) {
  return tracker.areAllInstancesComplete(parentTaskId);
}

/**
 * Get completion percentage
 *
 * @param {string} parentTaskId - Parent task ID
 * @param {MultiInstanceTracker} [tracker] - Tracker to use
 * @returns {number} Percentage (0-100)
 */
export function getCompletionPercentage(parentTaskId, tracker = globalInstanceTracker) {
  return tracker.getCompletionPercentage(parentTaskId);
}

// =============================================================================
// Exports
// =============================================================================

export {
  MultiInstanceTracker,
  globalInstanceTracker,
  InstanceStatus,
};

export default {
  spawnInstancesNoSync,
  enableInstance,
  startInstance,
  completeInstance,
  failInstance,
  cancelInstance,
  getAggregateStatus,
  areAllInstancesComplete,
  getCompletionPercentage,
  MultiInstanceTracker,
  globalInstanceTracker,
};
