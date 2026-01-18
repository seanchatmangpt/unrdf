/**
 * Minimal Task Spawner - Multiple Instance Infrastructure
 *
 * BUILT BY COPYING patterns from WP12-14 implementations.
 * NO new dependencies, NO invention.
 *
 * @module @unrdf/yawl/runtime/task-spawner
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { now } from '@unrdf/kgc-4d';

// =============================================================================
// Schemas (Copied from WP12)
// =============================================================================

/**
 * Spawn options schema
 */
export const SpawnOptionsSchema = z.object({
  /** Base input data for all instances */
  baseInputData: z.record(z.string(), z.any()).default({}),
  /** Per-instance input data array */
  instanceInputs: z.array(z.record(z.string(), z.any())).optional(),
  /** Generate receipts? */
  generateReceipts: z.boolean().default(true),
  /** Instance ID prefix */
  instanceIdPrefix: z.string().optional(),
  /** Case ID */
  caseId: z.string().default('default-case'),
});

/**
 * Instance record schema
 */
export const InstanceRecordSchema = z.object({
  id: z.string().min(1),
  parentTaskId: z.string().min(1),
  instanceIndex: z.number().int().nonnegative(),
  totalInstances: z.number().int().positive(),
  status: z.enum(['spawned', 'enabled', 'active', 'completed', 'failed', 'cancelled']),
  inputData: z.record(z.string(), z.any()),
  outputData: z.record(z.string(), z.any()).optional(),
  spawnedAt: z.bigint(),
  completedAt: z.bigint().optional(),
  error: z.string().optional(),
  receipts: z.array(z.any()).default([]),
});

/**
 * Spawn result schema
 */
export const SpawnResultSchema = z.object({
  parentTaskId: z.string().min(1),
  instanceIds: z.array(z.string().min(1)),
  instances: z.array(InstanceRecordSchema),
  aggregateReceipt: z.any().optional(),
  metadata: z.object({
    totalInstances: z.number().int().positive(),
    spawnedAt: z.bigint(),
    pattern: z.string().optional(),
  }),
});

// =============================================================================
// Instance Status (Copied from instance-tracker.mjs)
// =============================================================================

export const InstanceStatus = Object.freeze({
  SPAWNED: 'spawned',
  ENABLED: 'enabled',
  ACTIVE: 'active',
  COMPLETED: 'completed',
  FAILED: 'failed',
  CANCELLED: 'cancelled',
});

// =============================================================================
// Receipt Generation (Copied from task-execution.mjs)
// =============================================================================

/**
 * Generate receipt for instance spawn
 * @param {Object} instance - Instance record
 * @param {string} action - Action performed
 * @returns {Promise<Object>} Receipt
 */
async function generateInstanceReceipt(instance, action) {
  const receipt = {
    id: `receipt-${instance.id}-${action}-${Date.now()}`,
    instanceId: instance.id,
    parentTaskId: instance.parentTaskId,
    action,
    timestamp: now(),
    status: instance.status,
    instanceIndex: instance.instanceIndex,
    totalInstances: instance.totalInstances,
  };

  // Hash receipt (BLAKE3 pattern from task-execution.mjs)
  const receiptHash = await blake3(JSON.stringify({
    id: receipt.id,
    instanceId: receipt.instanceId,
    action: receipt.action,
    timestamp: receipt.timestamp.toString(),
    status: receipt.status,
  }));

  receipt.hash = receiptHash;
  return receipt;
}

/**
 * Generate aggregate receipt from instance receipts
 * @param {Array<Object>} instanceReceipts - Instance receipts
 * @param {Object} metadata - Spawn metadata
 * @returns {Promise<Object>} Aggregate receipt
 */
async function generateAggregateReceipt(instanceReceipts, metadata) {
  // Compute Merkle root (pattern from WP12)
  const hashes = instanceReceipts.map(r => r.hash);
  const merkleRoot = await blake3(hashes.join(''));

  const aggregateReceipt = {
    id: `aggregate-${metadata.parentTaskId}-${Date.now()}`,
    parentTaskId: metadata.parentTaskId,
    pattern: metadata.pattern ?? 'MI',
    timestamp: now(),
    totalInstances: metadata.totalInstances,
    merkleRoot,
    instanceHashes: hashes,
  };

  // Hash aggregate receipt
  const aggregateHash = await blake3(JSON.stringify({
    id: aggregateReceipt.id,
    parentTaskId: aggregateReceipt.parentTaskId,
    pattern: aggregateReceipt.pattern,
    merkleRoot: aggregateReceipt.merkleRoot,
  }));

  aggregateReceipt.hash = aggregateHash;
  return aggregateReceipt;
}

// =============================================================================
// Task Spawner (Copied from WP12 spawnInstancesNoSync)
// =============================================================================

/**
 * Spawn N task instances (minimal implementation)
 *
 * PATTERN SOURCE: wp12-no-sync.mjs lines 100-150
 *
 * @param {string} taskId - Task definition ID
 * @param {number} count - Number of instances to spawn
 * @param {Object} [options] - Spawn options
 * @returns {Promise<Object>} Spawn result
 *
 * @example
 * const result = await spawnInstances('process-order', 5, {
 *   baseInputData: { shared: 'value' },
 *   instanceInputs: [
 *     { orderId: 'ORD-1' },
 *     { orderId: 'ORD-2' },
 *     { orderId: 'ORD-3' },
 *     { orderId: 'ORD-4' },
 *     { orderId: 'ORD-5' },
 *   ],
 * });
 */
export async function spawnInstances(taskId, count, options = {}) {
  if (!Number.isInteger(count) || count <= 0) {
    throw new Error(`Invalid instance count: ${count}`);
  }

  // Validate options (Zod pattern)
  const validatedOptions = SpawnOptionsSchema.parse(options);

  const parentTaskId = `${taskId}-mi-${Date.now()}`;
  const instanceIdPrefix = validatedOptions.instanceIdPrefix ?? `${parentTaskId}-inst`;
  const spawnedAt = now();

  // Create N instances in parallel (Array.from + Promise.all pattern from WP12)
  const instances = await Promise.all(
    Array.from({ length: count }, async (_, i) => {
      const instanceId = `${instanceIdPrefix}-${i}`;

      // Merge base + instance-specific input (pattern from WP12)
      const instanceInput = {
        ...validatedOptions.baseInputData,
        ...(validatedOptions.instanceInputs?.[i] ?? {}),
      };

      // Create instance record
      const instance = {
        id: instanceId,
        parentTaskId,
        instanceIndex: i,
        totalInstances: count,
        status: InstanceStatus.SPAWNED,
        inputData: instanceInput,
        spawnedAt,
        receipts: [],
      };

      // Generate spawn receipt if requested
      if (validatedOptions.generateReceipts) {
        const receipt = await generateInstanceReceipt(instance, 'spawn');
        instance.receipts.push(receipt);
      }

      return instance;
    })
  );

  // Generate aggregate receipt (pattern from WP12)
  let aggregateReceipt = null;
  if (validatedOptions.generateReceipts) {
    const instanceReceipts = instances.map(i => i.receipts[0]);
    aggregateReceipt = await generateAggregateReceipt(instanceReceipts, {
      parentTaskId,
      totalInstances: count,
      pattern: 'MI',
    });
  }

  // Validate result before returning (Zod pattern)
  const result = {
    parentTaskId,
    instanceIds: instances.map(i => i.id),
    instances,
    aggregateReceipt,
    metadata: {
      totalInstances: count,
      spawnedAt,
      pattern: 'MI',
    },
  };

  return SpawnResultSchema.parse(result);
}

// =============================================================================
// Instance State Transitions (Copied from task-execution.mjs)
// =============================================================================

/**
 * Enable instance
 * @param {Object} instance - Instance record
 * @param {Object} [options] - Options
 * @returns {Promise<Object>} Updated instance
 */
export async function enableInstance(instance, options = {}) {
  if (instance.status !== InstanceStatus.SPAWNED) {
    throw new Error(`Cannot enable instance in ${instance.status} state`);
  }

  instance.status = InstanceStatus.ENABLED;

  if (options.generateReceipt !== false) {
    const receipt = await generateInstanceReceipt(instance, 'enable');
    instance.receipts.push(receipt);
  }

  return instance;
}

/**
 * Start instance execution
 * @param {Object} instance - Instance record
 * @param {Object} [options] - Options
 * @returns {Promise<Object>} Updated instance
 */
export async function startInstance(instance, options = {}) {
  if (instance.status !== InstanceStatus.ENABLED) {
    throw new Error(`Cannot start instance in ${instance.status} state`);
  }

  instance.status = InstanceStatus.ACTIVE;

  if (options.generateReceipt !== false) {
    const receipt = await generateInstanceReceipt(instance, 'start');
    instance.receipts.push(receipt);
  }

  return instance;
}

/**
 * Complete instance
 * @param {Object} instance - Instance record
 * @param {Object} [outputData] - Output data
 * @param {Object} [options] - Options
 * @returns {Promise<Object>} Updated instance
 */
export async function completeInstance(instance, outputData = {}, options = {}) {
  if (instance.status !== InstanceStatus.ACTIVE) {
    throw new Error(`Cannot complete instance in ${instance.status} state`);
  }

  instance.status = InstanceStatus.COMPLETED;
  instance.outputData = outputData;
  instance.completedAt = now();

  if (options.generateReceipt !== false) {
    const receipt = await generateInstanceReceipt(instance, 'complete');
    instance.receipts.push(receipt);
  }

  return instance;
}

/**
 * Fail instance
 * @param {Object} instance - Instance record
 * @param {Error|string} error - Error
 * @param {Object} [options] - Options
 * @returns {Promise<Object>} Updated instance
 */
export async function failInstance(instance, error, options = {}) {
  instance.status = InstanceStatus.FAILED;
  instance.error = error instanceof Error ? error.message : String(error);
  instance.completedAt = now();

  if (options.generateReceipt !== false) {
    const receipt = await generateInstanceReceipt(instance, 'fail');
    instance.receipts.push(receipt);
  }

  return instance;
}

/**
 * Cancel instance
 * @param {Object} instance - Instance record
 * @param {Object} [options] - Options
 * @returns {Promise<Object>} Updated instance
 */
export async function cancelInstance(instance, options = {}) {
  if (instance.status === InstanceStatus.COMPLETED || instance.status === InstanceStatus.FAILED) {
    throw new Error(`Cannot cancel instance in ${instance.status} state`);
  }

  instance.status = InstanceStatus.CANCELLED;
  instance.completedAt = now();

  if (options.generateReceipt !== false) {
    const receipt = await generateInstanceReceipt(instance, 'cancel');
    instance.receipts.push(receipt);
  }

  return instance;
}
