/**
 * Async Workflow - Long-running work as workflow graph inside O
 *
 * Instead of waiting for protocol-level async, async work is modeled explicitly:
 * - Enqueue a WorkItem node in O
 * - Attach constraints Q, budgets C_τ
 * - Tool executors append receipts as events
 * - Completion is a state transition, not a returned value
 *
 * @module @unrdf/kgc-claude/async-workflow
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { dataFactory } from '@unrdf/oxigraph';
import { now, toISO, VectorClock } from '@unrdf/kgc-4d';
import { GRAPHS, PREDICATES, WORK_ITEM_STATUS } from './constants.mjs';

/**
 * Work item constraint schema
 */
export const WorkItemConstraintSchema = z.object({
  /** Maximum execution time in nanoseconds */
  maxDuration: z.bigint().optional(),
  /** Required capabilities */
  requiredCapabilities: z.array(z.string()).default([]),
  /** Dependency work item IDs */
  dependencies: z.array(z.string()).default([]),
  /** SPARQL precondition query */
  precondition: z.string().optional(),
  /** Retry count */
  maxRetries: z.number().int().nonnegative().default(3),
});

/**
 * @typedef {z.infer<typeof WorkItemConstraintSchema>} WorkItemConstraint
 */

/**
 * Work item budget schema
 */
export const WorkItemBudgetSchema = z.object({
  maxDeltaSize: z.number().int().positive().default(100),
  maxToolOps: z.number().int().positive().default(50),
  maxFilesTouched: z.number().int().positive().default(20),
});

/**
 * @typedef {z.infer<typeof WorkItemBudgetSchema>} WorkItemBudget
 */

/**
 * Execution receipt schema
 */
export const ExecutionReceiptSchema = z.object({
  id: z.string().uuid(),
  workItemId: z.string(),
  executorId: z.string(),
  t_ns: z.bigint(),
  timestamp_iso: z.string(),
  status: z.enum(['started', 'progress', 'completed', 'failed', 'cancelled']),
  progress: z.number().min(0).max(100).optional(),
  output: z.any().optional(),
  error: z.string().optional(),
  receiptHash: z.string(),
  previousReceiptHash: z.string().nullable(),
});

/**
 * @typedef {z.infer<typeof ExecutionReceiptSchema>} ExecutionReceipt
 */

/**
 * Work item schema
 */
export const WorkItemSchema = z.object({
  id: z.string().uuid(),
  type: z.string(),
  payload: z.any(),
  status: z.enum(['queued', 'assigned', 'executing', 'completed', 'failed', 'cancelled']),
  constraints: WorkItemConstraintSchema.default({}),
  budget: WorkItemBudgetSchema.default({}),
  executorId: z.string().optional(),
  createdAt: z.bigint(),
  assignedAt: z.bigint().optional(),
  completedAt: z.bigint().optional(),
  retryCount: z.number().int().nonnegative().default(0),
  receipts: z.array(z.string()).default([]),
  result: z.any().optional(),
  error: z.string().optional(),
});

/**
 * @typedef {z.infer<typeof WorkItemSchema>} WorkItem
 */

/**
 * Generate UUID v4
 * @returns {string}
 */
function generateUUID() {
  if (typeof crypto !== 'undefined' && crypto.randomUUID) {
    return crypto.randomUUID();
  }
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

/**
 * Work item queue (in-memory for MVP)
 * @type {Map<string, { item: WorkItem, receipts: ExecutionReceipt[] }>}
 */
const workItemQueue = new Map();

/**
 * Executor registry
 * @type {Map<string, { capabilities: string[], busy: boolean }>}
 */
const executorRegistry = new Map();

/**
 * Create and enqueue a new work item
 *
 * @param {Object} config
 * @param {string} config.type - Work item type
 * @param {any} config.payload - Work item payload
 * @param {Partial<WorkItemConstraint>} [config.constraints]
 * @param {Partial<WorkItemBudget>} [config.budget]
 * @returns {WorkItem}
 *
 * @example
 * const item = enqueueWorkItem({
 *   type: 'file_edit',
 *   payload: { path: 'src/foo.ts', content: '...' },
 *   constraints: { maxDuration: 60000000000n }, // 60s
 *   budget: { maxToolOps: 10 },
 * });
 */
export function enqueueWorkItem(config) {
  const id = generateUUID();
  const t_ns = now();

  const item = WorkItemSchema.parse({
    id,
    type: config.type,
    payload: config.payload,
    status: WORK_ITEM_STATUS.QUEUED,
    constraints: WorkItemConstraintSchema.parse(config.constraints || {}),
    budget: WorkItemBudgetSchema.parse(config.budget || {}),
    createdAt: t_ns,
    receipts: [],
  });

  workItemQueue.set(id, { item, receipts: [] });
  return item;
}

/**
 * Register an executor with capabilities
 *
 * @param {string} executorId - Executor identifier
 * @param {string[]} capabilities - Supported work item types
 */
export function registerExecutor(executorId, capabilities = []) {
  executorRegistry.set(executorId, { capabilities, busy: false });
}

/**
 * Unregister an executor
 * @param {string} executorId
 */
export function unregisterExecutor(executorId) {
  executorRegistry.delete(executorId);
}

/**
 * Find an available executor for a work item
 *
 * @param {WorkItem} item
 * @returns {string|null} Executor ID or null
 */
function findExecutor(item) {
  for (const [id, executor] of executorRegistry) {
    if (executor.busy) continue;

    // Check capabilities
    const hasCapabilities = item.constraints.requiredCapabilities.every((cap) =>
      executor.capabilities.includes(cap),
    );

    if (hasCapabilities) {
      return id;
    }
  }
  return null;
}

/**
 * Check if work item dependencies are satisfied
 *
 * @param {WorkItem} item
 * @returns {boolean}
 */
function checkDependencies(item) {
  for (const depId of item.constraints.dependencies) {
    const dep = workItemQueue.get(depId);
    if (!dep || dep.item.status !== WORK_ITEM_STATUS.COMPLETED) {
      return false;
    }
  }
  return true;
}

/**
 * Assign work item to an executor
 *
 * @param {string} workItemId
 * @param {string} [executorId] - Specific executor or auto-assign
 * @returns {{ success: boolean, reason?: string }}
 */
export function assignWorkItem(workItemId, executorId) {
  const entry = workItemQueue.get(workItemId);
  if (!entry) {
    return { success: false, reason: `Work item not found: ${workItemId}` };
  }

  const { item } = entry;

  if (item.status !== WORK_ITEM_STATUS.QUEUED) {
    return { success: false, reason: `Work item not in queued state: ${item.status}` };
  }

  // Check dependencies
  if (!checkDependencies(item)) {
    return { success: false, reason: 'Dependencies not satisfied' };
  }

  // Find or validate executor
  const targetExecutor = executorId || findExecutor(item);
  if (!targetExecutor) {
    return { success: false, reason: 'No available executor' };
  }

  const executor = executorRegistry.get(targetExecutor);
  if (!executor) {
    return { success: false, reason: `Executor not found: ${targetExecutor}` };
  }

  if (executor.busy) {
    return { success: false, reason: `Executor busy: ${targetExecutor}` };
  }

  // Assign
  item.status = WORK_ITEM_STATUS.ASSIGNED;
  item.executorId = targetExecutor;
  item.assignedAt = now();
  executor.busy = true;

  return { success: true };
}

/**
 * Start executing a work item
 *
 * @param {string} workItemId
 * @returns {Promise<ExecutionReceipt|null>}
 */
export async function startExecution(workItemId) {
  const entry = workItemQueue.get(workItemId);
  if (!entry) return null;

  const { item, receipts } = entry;

  if (item.status !== WORK_ITEM_STATUS.ASSIGNED) {
    return null;
  }

  item.status = WORK_ITEM_STATUS.EXECUTING;

  const receipt = await createExecutionReceipt(item.id, item.executorId, 'started', {
    previousReceiptHash: receipts.length > 0 ? receipts[receipts.length - 1].receiptHash : null,
  });

  receipts.push(receipt);
  item.receipts.push(receipt.id);

  return receipt;
}

/**
 * Report execution progress
 *
 * @param {string} workItemId
 * @param {number} progress - Progress percentage (0-100)
 * @param {any} [partialOutput]
 * @returns {Promise<ExecutionReceipt|null>}
 */
export async function reportProgress(workItemId, progress, partialOutput) {
  const entry = workItemQueue.get(workItemId);
  if (!entry) return null;

  const { item, receipts } = entry;

  if (item.status !== WORK_ITEM_STATUS.EXECUTING) {
    return null;
  }

  const receipt = await createExecutionReceipt(item.id, item.executorId, 'progress', {
    progress,
    output: partialOutput,
    previousReceiptHash: receipts.length > 0 ? receipts[receipts.length - 1].receiptHash : null,
  });

  receipts.push(receipt);
  item.receipts.push(receipt.id);

  return receipt;
}

/**
 * Complete a work item
 *
 * @param {string} workItemId
 * @param {any} result - Execution result
 * @returns {Promise<ExecutionReceipt|null>}
 */
export async function completeWorkItem(workItemId, result) {
  const entry = workItemQueue.get(workItemId);
  if (!entry) return null;

  const { item, receipts } = entry;

  if (item.status !== WORK_ITEM_STATUS.EXECUTING) {
    return null;
  }

  item.status = WORK_ITEM_STATUS.COMPLETED;
  item.completedAt = now();
  item.result = result;

  // Release executor
  if (item.executorId) {
    const executor = executorRegistry.get(item.executorId);
    if (executor) executor.busy = false;
  }

  const receipt = await createExecutionReceipt(item.id, item.executorId, 'completed', {
    output: result,
    progress: 100,
    previousReceiptHash: receipts.length > 0 ? receipts[receipts.length - 1].receiptHash : null,
  });

  receipts.push(receipt);
  item.receipts.push(receipt.id);

  return receipt;
}

/**
 * Fail a work item
 *
 * @param {string} workItemId
 * @param {string} error - Error message
 * @param {boolean} [retry=true] - Whether to retry
 * @returns {Promise<ExecutionReceipt|null>}
 */
export async function failWorkItem(workItemId, error, retry = true) {
  const entry = workItemQueue.get(workItemId);
  if (!entry) return null;

  const { item, receipts } = entry;

  // Release executor
  if (item.executorId) {
    const executor = executorRegistry.get(item.executorId);
    if (executor) executor.busy = false;
  }

  // Check retry
  if (retry && item.retryCount < item.constraints.maxRetries) {
    item.status = WORK_ITEM_STATUS.QUEUED;
    item.retryCount++;
    item.executorId = undefined;
    item.assignedAt = undefined;
  } else {
    item.status = WORK_ITEM_STATUS.FAILED;
    item.completedAt = now();
    item.error = error;
  }

  const receipt = await createExecutionReceipt(item.id, item.executorId, 'failed', {
    error,
    previousReceiptHash: receipts.length > 0 ? receipts[receipts.length - 1].receiptHash : null,
  });

  receipts.push(receipt);
  item.receipts.push(receipt.id);

  return receipt;
}

/**
 * Cancel a work item
 *
 * @param {string} workItemId
 * @returns {Promise<ExecutionReceipt|null>}
 */
export async function cancelWorkItem(workItemId) {
  const entry = workItemQueue.get(workItemId);
  if (!entry) return null;

  const { item, receipts } = entry;

  // Release executor
  if (item.executorId) {
    const executor = executorRegistry.get(item.executorId);
    if (executor) executor.busy = false;
  }

  item.status = WORK_ITEM_STATUS.CANCELLED;
  item.completedAt = now();

  const receipt = await createExecutionReceipt(item.id, item.executorId, 'cancelled', {
    previousReceiptHash: receipts.length > 0 ? receipts[receipts.length - 1].receiptHash : null,
  });

  receipts.push(receipt);
  item.receipts.push(receipt.id);

  return receipt;
}

/**
 * Create an execution receipt
 *
 * @param {string} workItemId
 * @param {string} executorId
 * @param {string} status
 * @param {Object} options
 * @returns {Promise<ExecutionReceipt>}
 */
async function createExecutionReceipt(workItemId, executorId, status, options = {}) {
  const id = generateUUID();
  const t_ns = now();

  const receiptContent = {
    id,
    workItemId,
    executorId,
    t_ns: t_ns.toString(),
    status,
    progress: options.progress,
    output: options.output,
    error: options.error,
    previousReceiptHash: options.previousReceiptHash,
  };

  const receiptHash = await blake3(JSON.stringify(receiptContent));

  return ExecutionReceiptSchema.parse({
    id,
    workItemId,
    executorId: executorId || 'unknown',
    t_ns,
    timestamp_iso: toISO(t_ns),
    status,
    progress: options.progress,
    output: options.output,
    error: options.error,
    receiptHash,
    previousReceiptHash: options.previousReceiptHash,
  });
}

/**
 * Get work item by ID
 *
 * @param {string} workItemId
 * @returns {WorkItem|null}
 */
export function getWorkItem(workItemId) {
  const entry = workItemQueue.get(workItemId);
  return entry ? entry.item : null;
}

/**
 * Get work item receipts
 *
 * @param {string} workItemId
 * @returns {ExecutionReceipt[]}
 */
export function getWorkItemReceipts(workItemId) {
  const entry = workItemQueue.get(workItemId);
  return entry ? [...entry.receipts] : [];
}

/**
 * Get queued work items
 * @returns {WorkItem[]}
 */
export function getQueuedItems() {
  return [...workItemQueue.values()]
    .filter((e) => e.item.status === WORK_ITEM_STATUS.QUEUED)
    .map((e) => e.item);
}

/**
 * Get executing work items
 * @returns {WorkItem[]}
 */
export function getExecutingItems() {
  return [...workItemQueue.values()]
    .filter((e) => e.item.status === WORK_ITEM_STATUS.EXECUTING)
    .map((e) => e.item);
}

/**
 * Persist work item to KGC store
 *
 * @param {Object} store - KGCStore instance
 * @param {WorkItem} item
 * @returns {Promise<{ receipt: Object }>}
 */
export async function persistWorkItem(store, item) {
  const itemUri = dataFactory.namedNode(`http://kgc.io/workitem/${item.id}`);

  const deltas = [
    {
      type: 'add',
      subject: itemUri,
      predicate: dataFactory.namedNode(PREDICATES.WORK_ITEM_ID),
      object: dataFactory.literal(item.id),
    },
    {
      type: 'add',
      subject: itemUri,
      predicate: dataFactory.namedNode(PREDICATES.WORK_ITEM_STATUS),
      object: dataFactory.literal(item.status),
    },
    {
      type: 'add',
      subject: itemUri,
      predicate: dataFactory.namedNode(PREDICATES.WORK_ITEM_CONSTRAINTS),
      object: dataFactory.literal(JSON.stringify(item.constraints)),
    },
    {
      type: 'add',
      subject: itemUri,
      predicate: dataFactory.namedNode(PREDICATES.WORK_ITEM_BUDGET),
      object: dataFactory.literal(JSON.stringify(item.budget)),
    },
  ];

  if (item.executorId) {
    deltas.push({
      type: 'add',
      subject: itemUri,
      predicate: dataFactory.namedNode(PREDICATES.EXECUTOR_ID),
      object: dataFactory.literal(item.executorId),
    });
  }

  return store.appendEvent(
    {
      type: 'WORK_ITEM',
      payload: {
        workItemId: item.id,
        type: item.type,
        status: item.status,
      },
    },
    deltas,
  );
}

/**
 * Clear all work items (for testing)
 */
export function clearWorkItems() {
  workItemQueue.clear();
}

/**
 * Clear all executors (for testing)
 */
export function clearExecutors() {
  executorRegistry.clear();
}
