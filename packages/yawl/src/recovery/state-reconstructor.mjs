/**
 * State Reconstructor - Rebuild case state from receipt chains
 *
 * Reconstructs YNetRunner state by replaying receipts.
 * Novel approach: Deterministic state reconstruction from cryptographic proofs.
 *
 * @module @unrdf/yawl/recovery/state-reconstructor
 */

import { z } from 'zod';
import { now } from '@unrdf/kgc-4d';

// =============================================================================
// Schemas
// =============================================================================

const ReconstructCaseOptionsSchema = z.object({
  caseId: z.string().min(1),
  workflowId: z.string().min(1),
  receipts: z.array(z.any()),
  workflow: z.any(), // YawlWorkflow instance
});

const ReconstructedStateSchema = z.object({
  caseId: z.string(),
  workflowId: z.string(),
  status: z.enum(['CREATED', 'RUNNING', 'COMPLETED', 'CANCELLED', 'FAILED']),
  createdAt: z.bigint(),
  startedAt: z.bigint().nullable(),
  completedAt: z.bigint().nullable(),
  data: z.record(z.any()),
  workItems: z.map(z.string(), z.any()),
  completedTasks: z.set(z.string()),
  activatedTasks: z.set(z.string()),
  marking: z.map(z.string(), z.number()),
  receipts: z.array(z.any()),
  eventLog: z.array(z.any()),
});

// =============================================================================
// State Reconstruction
// =============================================================================

/**
 * Reconstruct case state from receipt chain
 *
 * Replays all receipts in order to rebuild the complete case state.
 * This is the core recovery mechanism.
 *
 * @param {Object} options - Reconstruction options
 * @param {string} options.caseId - Case ID
 * @param {string} options.workflowId - Workflow ID
 * @param {Array} options.receipts - Receipt chain (ordered by timestamp)
 * @param {Object} options.workflow - Workflow definition
 * @returns {Object} Reconstructed case state
 *
 * @example
 * const state = reconstructCase({
 *   caseId: 'case-123',
 *   workflowId: 'approval-v1',
 *   receipts: receiptChain,
 *   workflow: workflowDef,
 * });
 *
 * console.log(`Reconstructed ${state.workItems.size} work items`);
 * console.log(`Marking: ${JSON.stringify(Object.fromEntries(state.marking))}`);
 */
export function reconstructCase(options) {
  const validated = ReconstructCaseOptionsSchema.parse(options);
  const { caseId, workflowId, receipts, workflow } = validated;

  // Initialize empty state
  const state = {
    caseId,
    workflowId,
    status: 'CREATED',
    createdAt: null,
    startedAt: null,
    completedAt: null,
    data: {},
    workItems: new Map(),
    completedTasks: new Set(),
    activatedTasks: new Set(),
    marking: new Map(),
    receipts: [],
    eventLog: [],
  };

  // Replay each receipt in order
  for (const receipt of receipts) {
    applyReceiptToState(state, receipt, workflow);
    state.receipts.push(receipt);
  }

  return state;
}

/**
 * Apply a single receipt to state
 *
 * Updates state based on receipt event type.
 *
 * @param {Object} state - Current state
 * @param {Object} receipt - Receipt to apply
 * @param {Object} workflow - Workflow definition
 * @private
 */
function applyReceiptToState(state, receipt, workflow) {
  const { eventType, taskId, workItemId, payload, t_ns } = receipt;

  // Update event log
  state.eventLog.push({
    type: eventType,
    taskId,
    workItemId,
    timestamp: t_ns,
    payload,
  });

  switch (eventType) {
    case 'CASE_CREATED':
      state.createdAt = t_ns;
      state.data = payload.context?.initialData || {};
      // Initialize marking with start task input token
      const startTaskId = workflow.getStartTaskId();
      if (startTaskId) {
        state.marking.set(`input:${startTaskId}`, 1);
      }
      break;

    case 'TASK_ENABLED':
      state.activatedTasks.add(taskId);
      // Add work item
      if (workItemId) {
        state.workItems.set(workItemId, {
          id: workItemId,
          taskId,
          status: 'ENABLED',
          enabledAt: t_ns,
          startedAt: null,
          completedAt: null,
          output: null,
          actor: payload.actor,
        });
      }
      // Update marking: input → enabled
      const inputCondition = `input:${taskId}`;
      const enabledCondition = `enabled:${taskId}`;
      removeTokens(state.marking, inputCondition, 1);
      addTokens(state.marking, enabledCondition, 1);
      break;

    case 'TASK_STARTED':
      if (state.status === 'CREATED') {
        state.status = 'RUNNING';
        state.startedAt = t_ns;
      }
      // Update work item status
      if (workItemId && state.workItems.has(workItemId)) {
        const workItem = state.workItems.get(workItemId);
        workItem.status = 'ACTIVE';
        workItem.startedAt = t_ns;
      }
      // Update marking: enabled → started
      const enabledCond = `enabled:${taskId}`;
      const startedCond = `started:${taskId}`;
      removeTokens(state.marking, enabledCond, 1);
      addTokens(state.marking, startedCond, 1);
      break;

    case 'TASK_COMPLETED':
      state.completedTasks.add(taskId);
      // Update work item status
      if (workItemId && state.workItems.has(workItemId)) {
        const workItem = state.workItems.get(workItemId);
        workItem.status = 'COMPLETED';
        workItem.completedAt = t_ns;
        workItem.output = payload.context?.output || {};
      }
      // Merge output into case data
      if (payload.context?.output) {
        state.data = { ...state.data, ...payload.context.output };
      }
      // Update marking: started → output
      const startedCondition = `started:${taskId}`;
      const outputCondition = `output:${taskId}`;
      removeTokens(state.marking, startedCondition, 1);
      addTokens(state.marking, outputCondition, 1);

      // Add tokens to downstream tasks
      if (payload.context?.downstreamEnabled) {
        for (const downstreamTaskId of payload.context.downstreamEnabled) {
          addTokens(state.marking, `input:${downstreamTaskId}`, 1);
        }
      }

      // Check if case completed
      if (payload.context?.caseCompleted) {
        state.status = 'COMPLETED';
        state.completedAt = t_ns;
      }
      break;

    case 'TASK_CANCELLED':
      if (workItemId && state.workItems.has(workItemId)) {
        const workItem = state.workItems.get(workItemId);
        workItem.status = 'CANCELLED';
        workItem.completedAt = t_ns;
      }
      // Remove tokens from enabled/started conditions
      const enabledCancelled = `enabled:${taskId}`;
      const startedCancelled = `started:${taskId}`;
      removeTokens(state.marking, enabledCancelled, 1);
      removeTokens(state.marking, startedCancelled, 1);

      if (payload.context?.caseCancelled) {
        state.status = 'CANCELLED';
        state.completedAt = t_ns;
      }
      break;

    case 'TASK_FAILED':
      if (workItemId && state.workItems.has(workItemId)) {
        const workItem = state.workItems.get(workItemId);
        workItem.status = 'FAILED';
        workItem.completedAt = t_ns;
        workItem.error = payload.context?.error;
      }
      state.status = 'FAILED';
      state.completedAt = t_ns;
      break;

    case 'WORK_ITEM_CREATED':
      // Multiple instance pattern - add additional work item
      if (workItemId) {
        state.workItems.set(workItemId, {
          id: workItemId,
          taskId,
          status: 'ENABLED',
          enabledAt: t_ns,
          startedAt: null,
          completedAt: null,
          output: null,
          actor: payload.actor,
        });
      }
      break;

    default:
      // Unknown event type - log but don't fail
      console.warn(`Unknown receipt event type: ${eventType}`);
  }
}

/**
 * Rebuild tokens (Petri net marking) from receipts
 *
 * @param {Array} receipts - Receipt chain
 * @param {Object} workflow - Workflow definition
 * @returns {Map<string, number>} Marking (conditionId → token count)
 *
 * @example
 * const marking = rebuildTokens(receipts, workflow);
 * console.log(`Tokens at input:Approval = ${marking.get('input:Approval')}`);
 */
export function rebuildTokens(receipts, workflow) {
  const marking = new Map();

  // Initialize with start task input token
  const startTaskId = workflow.getStartTaskId();
  if (startTaskId) {
    marking.set(`input:${startTaskId}`, 1);
  }

  // Replay token operations from receipts
  for (const receipt of receipts) {
    const { eventType, taskId } = receipt;

    switch (eventType) {
      case 'TASK_ENABLED':
        removeTokens(marking, `input:${taskId}`, 1);
        addTokens(marking, `enabled:${taskId}`, 1);
        break;

      case 'TASK_STARTED':
        removeTokens(marking, `enabled:${taskId}`, 1);
        addTokens(marking, `started:${taskId}`, 1);
        break;

      case 'TASK_COMPLETED':
        removeTokens(marking, `started:${taskId}`, 1);
        addTokens(marking, `output:${taskId}`, 1);
        // Add tokens to downstream
        if (receipt.payload?.context?.downstreamEnabled) {
          for (const downstreamId of receipt.payload.context.downstreamEnabled) {
            addTokens(marking, `input:${downstreamId}`, 1);
          }
        }
        break;

      case 'TASK_CANCELLED':
        removeTokens(marking, `enabled:${taskId}`, 1);
        removeTokens(marking, `started:${taskId}`, 1);
        break;
    }
  }

  return marking;
}

/**
 * Rebuild work items from receipts
 *
 * @param {Array} receipts - Receipt chain
 * @returns {Object} Work item collections
 *
 * @example
 * const items = rebuildWorkItems(receipts);
 * console.log(`${items.enabled.size} enabled, ${items.completed.size} completed`);
 */
export function rebuildWorkItems(receipts) {
  const allWorkItems = new Map();
  const enabled = new Set();
  const active = new Set();
  const completed = new Set();

  for (const receipt of receipts) {
    const { eventType, taskId, workItemId, t_ns, payload } = receipt;

    if (!workItemId) continue;

    switch (eventType) {
      case 'TASK_ENABLED':
      case 'WORK_ITEM_CREATED':
        allWorkItems.set(workItemId, {
          id: workItemId,
          taskId,
          status: 'ENABLED',
          enabledAt: t_ns,
          startedAt: null,
          completedAt: null,
          output: null,
          actor: payload.actor,
        });
        enabled.add(workItemId);
        break;

      case 'TASK_STARTED':
        if (allWorkItems.has(workItemId)) {
          const item = allWorkItems.get(workItemId);
          item.status = 'ACTIVE';
          item.startedAt = t_ns;
        }
        enabled.delete(workItemId);
        active.add(workItemId);
        break;

      case 'TASK_COMPLETED':
        if (allWorkItems.has(workItemId)) {
          const item = allWorkItems.get(workItemId);
          item.status = 'COMPLETED';
          item.completedAt = t_ns;
          item.output = payload.context?.output || {};
        }
        active.delete(workItemId);
        completed.add(workItemId);
        break;

      case 'TASK_CANCELLED':
        if (allWorkItems.has(workItemId)) {
          const item = allWorkItems.get(workItemId);
          item.status = 'CANCELLED';
          item.completedAt = t_ns;
        }
        enabled.delete(workItemId);
        active.delete(workItemId);
        break;
    }
  }

  return {
    allWorkItems,
    enabled,
    active,
    completed,
  };
}

/**
 * Rebuild case data context from receipts
 *
 * @param {Array} receipts - Receipt chain
 * @returns {Object} Current data context
 *
 * @example
 * const data = rebuildData(receipts);
 * console.log(`Case data:`, data);
 */
export function rebuildData(receipts) {
  let data = {};

  for (const receipt of receipts) {
    const { eventType, payload } = receipt;

    if (eventType === 'CASE_CREATED' && payload.context?.initialData) {
      data = { ...payload.context.initialData };
    }

    if (eventType === 'TASK_COMPLETED' && payload.context?.output) {
      data = { ...data, ...payload.context.output };
    }
  }

  return data;
}

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Add tokens to a condition
 * @param {Map} marking - Petri net marking
 * @param {string} conditionId - Condition ID
 * @param {number} count - Token count
 * @private
 */
function addTokens(marking, conditionId, count = 1) {
  const current = marking.get(conditionId) || 0;
  marking.set(conditionId, current + count);
}

/**
 * Remove tokens from a condition
 * @param {Map} marking - Petri net marking
 * @param {string} conditionId - Condition ID
 * @param {number} count - Token count
 * @private
 */
function removeTokens(marking, conditionId, count = 1) {
  const current = marking.get(conditionId) || 0;
  const remaining = Math.max(0, current - count);
  if (remaining === 0) {
    marking.delete(conditionId);
  } else {
    marking.set(conditionId, remaining);
  }
}

// =============================================================================
// Exports
// =============================================================================

export const schemas = {
  ReconstructCaseOptionsSchema,
  ReconstructedStateSchema,
};
