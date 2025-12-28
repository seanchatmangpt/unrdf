/**
 * @file YAWL Workflow Mutations - Workflow modification operations
 * @module @unrdf/yawl/workflow/mutations
 *
 * @description
 * Mutation operations for Workflow instances:
 * - addTask: Add new tasks to workflow
 * - addFlow: Add new control flows
 * - setStart: Set starting task
 * - setEnd: Set ending tasks
 * - lock: Lock workflow to prevent modifications
 *
 * All mutations check for workflow lock status before applying changes.
 *
 * @example
 * import { addTask, addFlow } from '@unrdf/yawl/workflow/mutations';
import { WorkflowError } from '../../errors.mjs';
 *
 * addTask(workflow, { id: 'new-task', name: 'New Task' });
 * addFlow(workflow, { from: 'task-a', to: 'new-task' });
 */

import { validateTaskDef, validateFlowDef } from '../patterns.mjs';

/**
 * Lock the workflow (no more modifications)
 * @param {Object} workflow - Workflow instance
 * @returns {Object} workflow
 */
export function lockWorkflow(workflow) {
  workflow._locked = true;
  return workflow;
}

/**
 * Add a task to the workflow
 * @param {Object} workflow - Workflow instance
 * @param {Object} taskDef - Task definition
 * @returns {Object} workflow
 * @throws {Error} If workflow is locked
 */
export function addTask(workflow, taskDef) {
  if (workflow._locked) {
    throw new Error('Cannot modify locked workflow');
  }

  const validated = validateTaskDef(taskDef);

  if (workflow._tasks.has(validated.id)) {
    throw new Error(`Task '${validated.id}' already exists in workflow`);
  }

  workflow._tasks.set(validated.id, validated);
  workflow._outgoingFlows.set(validated.id, []);
  workflow._incomingFlows.set(validated.id, []);

  if (validated.cancellationRegion) {
    workflow._taskToRegion.set(validated.id, validated.cancellationRegion);
  }

  workflow.modifiedAt = new Date();
  return workflow;
}

/**
 * Add a flow between tasks
 * @param {Object} workflow - Workflow instance
 * @param {Object} flowDef - Flow definition
 * @returns {Object} workflow
 * @throws {Error} If workflow is locked or tasks don't exist
 */
export function addFlow(workflow, flowDef) {
  if (workflow._locked) {
    throw new Error('Cannot modify locked workflow');
  }

  const validated = validateFlowDef(flowDef);

  if (!workflow._tasks.has(validated.from)) {
    throw new Error(`Source task '${validated.from}' not found`);
  }
  if (!workflow._tasks.has(validated.to)) {
    throw new Error(`Target task '${validated.to}' not found`);
  }

  workflow._flows.push(validated);
  workflow._outgoingFlows.get(validated.from).push(validated);
  workflow._incomingFlows.get(validated.to).push(validated);

  workflow.modifiedAt = new Date();
  return workflow;
}

/**
 * Set the start task
 * @param {Object} workflow - Workflow instance
 * @param {string} taskId - Start task ID
 * @returns {Object} workflow
 */
export function setStart(workflow, taskId) {
  if (workflow._locked) {
    throw new Error('Cannot modify locked workflow');
  }
  if (!workflow._tasks.has(taskId)) {
    throw new Error(`Task '${taskId}' not found`);
  }
  workflow._startTaskId = taskId;
  workflow.modifiedAt = new Date();
  return workflow;
}

/**
 * Set end tasks
 * @param {Object} workflow - Workflow instance
 * @param {string[]} taskIds - End task IDs
 * @returns {Object} workflow
 */
export function setEnd(workflow, taskIds) {
  if (workflow._locked) {
    throw new Error('Cannot modify locked workflow');
  }
  for (const taskId of taskIds) {
    if (!workflow._tasks.has(taskId)) {
      throw new Error(`Task '${taskId}' not found`);
    }
  }
  workflow._endTaskIds = [...taskIds];
  workflow.modifiedAt = new Date();
  return workflow;
}

// =============================================================================
// Module Exports
// =============================================================================

export default {
  lockWorkflow,
  addTask,
  addFlow,
  setStart,
  setEnd,
};
