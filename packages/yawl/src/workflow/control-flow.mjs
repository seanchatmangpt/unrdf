/**
 * @file YAWL Control Flow Evaluation - Split/join semantics evaluation
 * @module @unrdf/yawl/workflow/control-flow
 *
 * @description
 * Control flow evaluation functions for YAWL workflows:
 * - evaluateDownstream: Determine which tasks to enable after task completion
 * - canEnable: Check if a task can be enabled based on join semantics
 *
 * Implements YAWL split/join patterns:
 * - Sequence: Single path
 * - AND: Parallel paths (fork/synchronization)
 * - XOR: Exclusive choice/merge
 * - OR: Multi-choice/structured synchronization
 *
 * @example
 * import { evaluateDownstream, canEnable } from '@unrdf/yawl/workflow/control-flow';
 *
 * const toEnable = evaluateDownstream(workflow, 'review-task', { amount: 500 });
 * const canStart = canEnable(workflow, 'merge-task', completed, activated);
 */

import { SPLIT_TYPE, JOIN_TYPE } from '../patterns.mjs';

/**
 * Evaluate which downstream tasks should be enabled after task completion
 *
 * Implements YAWL split semantics:
 * - SEQUENCE: Enable single outgoing task
 * - AND: Enable all outgoing tasks (parallel split)
 * - XOR: Enable first matching condition (exclusive choice)
 * - OR: Enable all matching conditions (multi-choice)
 *
 * @param {Object} workflow - Workflow instance
 * @param {string} completedTaskId - Task that completed
 * @param {Object} [context={}] - Evaluation context (data, case state)
 * @returns {string[]} Task IDs to enable
 *
 * @example
 * const toEnable = evaluateDownstream(workflow, 'review', { amount: 500 });
 * // Returns ['approve'] if amount < 1000
 */
export function evaluateDownstream(workflow, completedTaskId, context = {}) {
  const taskDef = workflow._tasks.get(completedTaskId);
  if (!taskDef) return [];

  const outFlows = workflow._outgoingFlows.get(completedTaskId) ?? [];
  if (outFlows.length === 0) return [];

  const splitType = taskDef.splitType ?? SPLIT_TYPE.SEQUENCE;
  const toEnable = [];

  // Sort flows by priority (higher first)
  const sortedFlows = [...outFlows].sort((a, b) => (b.priority ?? 0) - (a.priority ?? 0));

  switch (splitType) {
    case SPLIT_TYPE.SEQUENCE:
      // Enable the single outgoing task
      if (sortedFlows.length > 0) {
        toEnable.push(sortedFlows[0].to);
      }
      break;

    case SPLIT_TYPE.AND:
      // Enable all outgoing tasks (parallel split)
      for (const flow of sortedFlows) {
        toEnable.push(flow.to);
      }
      break;

    case SPLIT_TYPE.XOR:
      // Enable first matching condition (exclusive choice)
      for (const flow of sortedFlows) {
        if (!flow.condition) {
          if (flow.isDefault) {
            // Save default for fallback
            continue;
          }
          toEnable.push(flow.to);
          break;
        }
        try {
          if (flow.condition(context)) {
            toEnable.push(flow.to);
            break;
          }
        } catch {
          // Skip flow on condition error
        }
      }
      // Use default flow if no match
      if (toEnable.length === 0) {
        const defaultFlow = sortedFlows.find((f) => f.isDefault);
        if (defaultFlow) {
          toEnable.push(defaultFlow.to);
        } else if (sortedFlows.length > 0) {
          // Fallback to last flow if no default
          toEnable.push(sortedFlows[sortedFlows.length - 1].to);
        }
      }
      break;

    case SPLIT_TYPE.OR:
      // Enable all matching conditions (multi-choice)
      for (const flow of sortedFlows) {
        if (!flow.condition) {
          toEnable.push(flow.to);
        } else {
          try {
            if (flow.condition(context)) {
              toEnable.push(flow.to);
            }
          } catch {
            // Skip flow on condition error
          }
        }
      }
      // Must enable at least one
      if (toEnable.length === 0 && sortedFlows.length > 0) {
        toEnable.push(sortedFlows[0].to);
      }
      break;

    default:
      // Unknown split type - enable all
      for (const flow of sortedFlows) {
        toEnable.push(flow.to);
      }
  }

  return toEnable;
}

/**
 * Check if a task can be enabled based on join semantics
 *
 * Implements YAWL join semantics:
 * - SEQUENCE: Any single incoming complete
 * - AND: All incoming must be complete (synchronization)
 * - XOR: Any one incoming complete (simple merge)
 * - OR: All ACTIVATED incoming must be complete (structured sync merge)
 *
 * @param {Object} workflow - Workflow instance
 * @param {string} taskId - Task to check
 * @param {Set<string>} completedTasks - Set of completed task IDs
 * @param {Set<string>} [activatedTasks=new Set()] - Set of tasks activated by OR-split
 * @returns {boolean} True if task can be enabled
 *
 * @example
 * const completed = new Set(['task-a', 'task-b']);
 * if (canEnable(workflow, 'merge-task', completed)) {
 *   // Enable the merge task
 * }
 */
export function canEnable(workflow, taskId, completedTasks, activatedTasks = new Set()) {
  const taskDef = workflow._tasks.get(taskId);
  if (!taskDef) return false;

  const inFlows = workflow._incomingFlows.get(taskId) ?? [];
  if (inFlows.length === 0) return true; // Start task

  const joinType = taskDef.joinType ?? JOIN_TYPE.SEQUENCE;
  const incomingTaskIds = inFlows.map((f) => f.from);

  switch (joinType) {
    case JOIN_TYPE.SEQUENCE:
      // Single incoming must be complete
      return incomingTaskIds.some((id) => completedTasks.has(id));

    case JOIN_TYPE.AND:
      // All incoming must be complete (synchronization)
      return incomingTaskIds.every((id) => completedTasks.has(id));

    case JOIN_TYPE.XOR:
      // Any one incoming complete (simple merge)
      return incomingTaskIds.some((id) => completedTasks.has(id));

    case JOIN_TYPE.OR:
      // All ACTIVATED incoming must be complete (structured sync merge)
      const activated = incomingTaskIds.filter((id) => activatedTasks.has(id));
      if (activated.length === 0) {
        // If none activated, any one complete suffices
        return incomingTaskIds.some((id) => completedTasks.has(id));
      }
      return activated.every((id) => completedTasks.has(id));

    default:
      return incomingTaskIds.some((id) => completedTasks.has(id));
  }
}

// =============================================================================
// Module Exports
// =============================================================================

export default {
  evaluateDownstream,
  canEnable,
};
