/**
 * @file YAWL Workflow Cancellation - Work item cancellation and region logic
 * @module @unrdf/yawl/api/workflow-cancellation
 *
 * @description
 * Handles work item cancellation with cancellation region support.
 * Pure functions with no side effects except event append.
 */

import {
  YAWL_EVENT_TYPES,
  WORK_ITEM_STATUS,
  now,
  toISO,
  createReceipt,
} from './workflow-creation.mjs';
import { WorkItemSchema } from './workflow-execution.mjs';
import { WorkflowError } from '../../errors.mjs';

// ============================================================================
// Core Cancellation Functions
// ============================================================================

/**
 * Cancel a work item, aborting it with a reason.
 *
 * Triggers cancellation region logic if the task is in a cancellation region.
 * Logs cancellation receipt and updates work item status.
 *
 * @param {Object} workItem - Work item to cancel
 * @param {string} reason - Reason for cancellation
 * @param {Object} [options] - Cancel options
 * @param {Object} [options.caseObj] - Case object for cancellation region handling
 * @param {Object} [options.workflow] - Workflow for cancellation region lookup
 * @param {Object} [options.store] - KGC-4D store for event logging
 * @returns {Promise<Object>} Receipt with cancellation details
 *
 * @throws {Error} If work item is already completed or cancelled
 *
 * @example
 * const receipt = await cancelWorkItem(workItem, 'Customer cancelled order');
 */
export async function cancelWorkItem(workItem, reason, options = {}) {
  // Validate work item
  const validWorkItem = WorkItemSchema.parse(workItem);

  // Validate reason
  if (!reason || typeof reason !== 'string') {
    throw new TypeError('Cancel reason must be a non-empty string');
  }

  // Check current status - can't cancel completed or already cancelled
  if (
    validWorkItem.status === WORK_ITEM_STATUS.COMPLETED ||
    validWorkItem.status === WORK_ITEM_STATUS.CANCELLED
  ) {
    throw new Error(
      `Cannot cancel work item ${validWorkItem.id}: status is ${validWorkItem.status}`
    );
  }

  const t_ns = now();
  const cancelTime = toISO(t_ns);

  // Update work item status (mutate original for in-place updates)
  const previousStatus = workItem.status;
  workItem.status = WORK_ITEM_STATUS.CANCELLED;
  workItem.endTime = cancelTime;
  workItem.result = { cancelled: true, reason };

  // Handle cancellation region - cancel all tasks in the region
  let cancelledInRegion = [];
  if (options.workflow && options.caseObj) {
    // Find cancellation region for this task
    const task = options.workflow.getTask(workItem.taskId);
    if (task?.cancellationRegion) {
      const regionTasks = options.workflow.getCancellationRegion(
        task.cancellationRegion
      );
      for (const taskId of regionTasks) {
        if (taskId !== workItem.taskId) {
          const regionWorkItem = options.caseObj.getWorkItem(taskId);
          if (
            regionWorkItem &&
            regionWorkItem.status !== WORK_ITEM_STATUS.COMPLETED &&
            regionWorkItem.status !== WORK_ITEM_STATUS.CANCELLED
          ) {
            regionWorkItem.status = WORK_ITEM_STATUS.CANCELLED;
            regionWorkItem.endTime = cancelTime;
            regionWorkItem.result = {
              cancelled: true,
              reason: `Cancelled by region trigger: ${reason}`,
            };
            cancelledInRegion.push(taskId);
          }
        }
      }
    }
  }

  // Append event to KGC-4D store if available
  let eventReceipt = null;
  if (options.store?.appendEvent) {
    const { receipt } = await options.store.appendEvent(
      {
        type: YAWL_EVENT_TYPES.WORK_ITEM_CANCELLED,
        payload: {
          workItemId: workItem.id,
          taskId: workItem.taskId,
          caseId: workItem.caseId,
          previousStatus,
          reason,
          cancelledInRegion,
          cancelTime,
        },
      },
      []
    );
    eventReceipt = receipt;
  }

  // Create receipt
  const receipt = await createReceipt(
    YAWL_EVENT_TYPES.WORK_ITEM_CANCELLED,
    {
      workItemId: workItem.id,
      taskId: workItem.taskId,
      caseId: workItem.caseId,
      previousStatus,
      reason,
      cancelledInRegion,
      cancelTime,
      eventId: eventReceipt?.id,
    }
  );

  return {
    workItem,
    cancelledInRegion,
    receipt,
  };
}
