/**
 * @file YAWL Case Lifecycle - Petri net marking and task lifecycle methods
 * @module @unrdf/yawl/case-lifecycle
 *
 * @description
 * Petri net token management and task lifecycle operations (enable, start, complete, cancel).
 * Implements YAWL semantics for work item state transitions.
 */

import { now } from '@unrdf/kgc-4d';
import { YawlTask, TaskStatus } from './task.mjs';
import { buildReceipt } from './receipt.mjs';
import { CaseStatus } from './case-core.mjs';

// =============================================================================
// Petri Net Marking (Token Management)
// =============================================================================

/**
 * Get the current Petri net marking (token placement)
 * @this {import('./case-core.mjs').Case}
 * @returns {Object<string, number>} Map of conditionId to token count
 */
export function getMarking() {
  return Object.fromEntries(this._marking);
}

/**
 * Get token count for a specific condition
 * @this {import('./case-core.mjs').Case}
 * @param {string} conditionId - Condition/place identifier
 * @returns {number} Number of tokens in the condition
 */
export function getTokens(conditionId) {
  return this._marking.get(conditionId) ?? 0;
}

/**
 * Add tokens to a condition
 * @this {import('./case-core.mjs').Case}
 * @param {string} conditionId - Condition identifier
 * @param {number} [count=1] - Number of tokens to add
 * @private
 */
export function _addTokens(conditionId, count = 1) {
  const current = this._marking.get(conditionId) ?? 0;
  this._marking.set(conditionId, current + count);
}

/**
 * Remove tokens from a condition
 * @this {import('./case-core.mjs').Case}
 * @param {string} conditionId - Condition identifier
 * @param {number} [count=1] - Number of tokens to remove
 * @returns {boolean} True if tokens were available and removed
 * @private
 */
export function _removeTokens(conditionId, count = 1) {
  const current = this._marking.get(conditionId) ?? 0;
  if (current < count) return false;
  const remaining = current - count;
  if (remaining === 0) {
    this._marking.delete(conditionId);
  } else {
    this._marking.set(conditionId, remaining);
  }
  return true;
}

/**
 * Enable a work item by producing tokens in its output condition
 * Called when a task becomes enabled (ready to fire)
 * @this {import('./case-core.mjs').Case}
 * @param {YawlTask} workItem - Work item being enabled
 */
export function enable(workItem) {
  const taskDefId = this.getTaskDefIdForWorkItem(workItem.id);

  // Consume token from input condition
  const inputConditionId = `input:${taskDefId}`;
  _removeTokens.call(this, inputConditionId, 1);

  // Produce token in enabled condition
  const enabledConditionId = `enabled:${taskDefId}`;
  _addTokens.call(this, enabledConditionId, 1);

  // Log event
  this._appendEvent({
    type: 'TOKENS_CONSUMED',
    conditionId: inputConditionId,
    count: 1,
    workItemId: workItem.id,
  });

  this._appendEvent({
    type: 'TOKENS_PRODUCED',
    conditionId: enabledConditionId,
    count: 1,
    workItemId: workItem.id,
  });
}

/**
 * Disable a work item by consuming tokens from its enabled condition
 * Called when a task is cancelled or superseded
 * @this {import('./case-core.mjs').Case}
 * @param {YawlTask} workItem - Work item being disabled
 */
export function disable(workItem) {
  const taskDefId = this.getTaskDefIdForWorkItem(workItem.id);

  // Consume token from enabled condition
  const enabledConditionId = `enabled:${taskDefId}`;
  _removeTokens.call(this, enabledConditionId, 1);

  // Log event
  this._appendEvent({
    type: 'TOKENS_CONSUMED',
    conditionId: enabledConditionId,
    count: 1,
    workItemId: workItem.id,
    reason: 'disabled',
  });
}

/**
 * Fire a transition: consume input tokens, produce output tokens
 * Called when a task completes
 * @this {import('./case-core.mjs').Case}
 * @param {YawlTask} workItem - Completed work item
 * @private
 */
export function _fireTransition(workItem) {
  const taskDefId = this.getTaskDefIdForWorkItem(workItem.id);

  // Consume token from started condition
  const startedConditionId = `started:${taskDefId}`;
  _removeTokens.call(this, startedConditionId, 1);

  // Produce tokens in output conditions (based on split semantics)
  const outputConditionId = `output:${taskDefId}`;
  _addTokens.call(this, outputConditionId, 1);

  // Log events
  this._appendEvent({
    type: 'TRANSITION_FIRED',
    taskId: taskDefId,
    workItemId: workItem.id,
  });
}

/**
 * Check if a task can be enabled based on token availability and join semantics
 * @this {import('./case-core.mjs').Case}
 * @param {string} taskDefId - Task definition ID
 * @returns {boolean} True if task can be enabled
 */
export function canEnableTask(taskDefId) {
  // Check circuit breaker
  if (this.circuitBreakers.get(taskDefId) === false) {
    return false;
  }

  // Use workflow's canEnable method which considers join semantics
  return this.workflow.canEnable(taskDefId, this.completedTasks, this.activatedTasks);
}

// =============================================================================
// Task Lifecycle
// =============================================================================

/**
 * Start the case (enable start task)
 * @this {import('./case-core.mjs').Case}
 * @returns {Promise<{task: YawlTask, receipt: import('./receipt.mjs').YawlReceipt}>}
 */
export async function start() {
  if (this._status !== CaseStatus.CREATED) {
    throw new Error(`Cannot start case: already ${this._status}`);
  }

  const startTaskId = this.workflow.getStartTaskId();
  if (!startTaskId) {
    throw new Error('Workflow has no start task');
  }

  this._status = CaseStatus.RUNNING;
  this.startedAt = now();

  // Create and enable start task
  const task = await enableTask.call(this, startTaskId);

  return { task: task.task, receipt: task.receipt };
}

/**
 * Enable a task
 * @this {import('./case-core.mjs').Case}
 * @param {string} taskId - Task definition ID
 * @param {string} [actor] - Actor performing the action
 * @returns {Promise<{task: YawlTask, receipt: import('./receipt.mjs').YawlReceipt}>}
 */
export async function enableTask(taskId, actor) {
  const taskDef = this.workflow.getTask(taskId);
  if (!taskDef) {
    throw new Error(`Task ${taskId} not found in workflow`);
  }

  // Check circuit breaker
  if (this.circuitBreakers.get(taskId) === false) {
    throw new Error(`Task ${taskId} is disabled by circuit breaker`);
  }

  const beforeState = this.getState();

  // Create task instance
  const workItemId = `${this.id}-${taskId}-${Date.now()}`;
  const task = new YawlTask({
    id: workItemId,
    name: taskDef.name ?? taskId,
    caseId: this.id,
    role: taskDef.role,
    timeout: taskDef.timeout,
    cancellationRegion: taskDef.cancellationRegion,
  });

  task.enable();
  this.workItems.set(workItemId, task);
  this.activatedTasks.add(taskId);

  // Track work item by task
  if (!this.workItemsByTask.has(taskId)) {
    this.workItemsByTask.set(taskId, new Set());
  }
  this.workItemsByTask.get(taskId).add(workItemId);

  // Update Petri net marking
  enable.call(this, task);

  const afterState = this.getState();
  const previousReceipt = this.receipts[this.receipts.length - 1];

  const receipt = await buildReceipt({
    caseId: this.id,
    taskId,
    action: 'enable',
    actor,
    beforeState,
    afterState,
    previousReceipt,
  });

  this.receipts.push(receipt);

  // Append event
  this._appendEvent({
    type: 'TASK_ENABLED',
    taskId,
    workItemId,
    actor,
    receiptId: receipt.id,
  });

  return { task, receipt };
}

/**
 * Start a task
 * @this {import('./case-core.mjs').Case}
 * @param {string} workItemId - Work item ID
 * @param {string} [resourceId] - Resource performing the task
 * @param {string} [actor] - Actor performing the action
 * @returns {Promise<{task: YawlTask, receipt: import('./receipt.mjs').YawlReceipt}>}
 */
export async function startTask(workItemId, resourceId, actor) {
  const task = this.workItems.get(workItemId);
  if (!task) {
    throw new Error(`Work item ${workItemId} not found`);
  }

  const beforeState = this.getState();
  const taskDefId = this.getTaskDefIdForWorkItem(workItemId);

  task.start(resourceId);

  // Update marking: move token from enabled to started
  const enabledConditionId = `enabled:${taskDefId}`;
  const startedConditionId = `started:${taskDefId}`;
  _removeTokens.call(this, enabledConditionId, 1);
  _addTokens.call(this, startedConditionId, 1);

  const afterState = this.getState();

  const previousReceipt = this.receipts[this.receipts.length - 1];

  const receipt = await buildReceipt({
    caseId: this.id,
    taskId: taskDefId,
    action: 'start',
    actor: actor ?? resourceId,
    beforeState,
    afterState,
    previousReceipt,
  });

  this.receipts.push(receipt);

  // Append event
  this._appendEvent({
    type: 'TASK_STARTED',
    taskId: taskDefId,
    workItemId,
    resourceId,
    actor,
    receiptId: receipt.id,
  });

  return { task, receipt };
}

/**
 * Complete a task
 * @this {import('./case-core.mjs').Case}
 * @param {string} workItemId - Work item ID
 * @param {Object} [output] - Task output data
 * @param {string} [actor] - Actor performing the action
 * @returns {Promise<{task: YawlTask, receipt: import('./receipt.mjs').YawlReceipt, downstreamEnabled: Array}>}
 */
export async function completeTask(workItemId, output = {}, actor) {
  const task = this.workItems.get(workItemId);
  if (!task) {
    throw new Error(`Work item ${workItemId} not found`);
  }

  const beforeState = this.getState();
  const taskDefId = this.getTaskDefIdForWorkItem(workItemId);

  task.complete(output);
  this.completedTasks.add(taskDefId);

  // Fire the Petri net transition
  _fireTransition.call(this, task);

  // Update case data with task output
  this.data = { ...this.data, ...output };

  // Produce tokens in downstream input conditions
  const toEnable = this.workflow.evaluateDownstream(taskDefId, {
    data: this.data,
    case: this,
    output,
  });

  for (const nextTaskId of toEnable) {
    const inputConditionId = `input:${nextTaskId}`;
    _addTokens.call(this, inputConditionId, 1);
  }

  // Enable downstream tasks based on join semantics
  const downstreamEnabled = [];
  for (const nextTaskId of toEnable) {
    if (this.workflow.canEnable(nextTaskId, this.completedTasks, this.activatedTasks)) {
      const enabled = await enableTask.call(this, nextTaskId, actor);
      downstreamEnabled.push({
        taskId: nextTaskId,
        workItemId: enabled.task.id,
        enabledAt: enabled.task.enabledAt,
      });
    }
  }

  // Check if case is complete
  const endTaskIds = this.workflow.getEndTaskIds();
  const allEndTasksComplete = endTaskIds.every(id =>
    this.completedTasks.has(id)
  );

  if (allEndTasksComplete) {
    this._status = CaseStatus.COMPLETED;
    this.completedAt = now();
  }

  const afterState = this.getState();
  const previousReceipt = this.receipts[this.receipts.length - 1];

  const receipt = await buildReceipt({
    caseId: this.id,
    taskId: taskDefId,
    action: 'complete',
    actor,
    beforeState,
    afterState,
    previousReceipt,
    output,
    downstreamEnabled,
  });

  this.receipts.push(receipt);

  // Append event
  this._appendEvent({
    type: 'TASK_COMPLETED',
    taskId: taskDefId,
    workItemId,
    output,
    downstreamEnabled: downstreamEnabled.map(d => d.taskId),
    actor,
    receiptId: receipt.id,
  });

  return { task, receipt, downstreamEnabled };
}

/**
 * Cancel a single work item
 * @this {import('./case-core.mjs').Case}
 * @param {string} workItemId - Work item ID
 * @param {string} [reason] - Cancellation reason
 * @param {string} [actor] - Actor performing the action
 * @returns {Promise<{task: YawlTask, receipt: import('./receipt.mjs').YawlReceipt}>}
 */
export async function cancelTask(workItemId, reason, actor) {
  const task = this.workItems.get(workItemId);
  if (!task) {
    throw new Error(`Work item ${workItemId} not found`);
  }

  const beforeState = this.getState();
  const taskDefId = this.getTaskDefIdForWorkItem(workItemId);

  // Update Petri net marking
  disable.call(this, task);

  task.cancel(reason);
  const afterState = this.getState();

  const previousReceipt = this.receipts[this.receipts.length - 1];

  const receipt = await buildReceipt({
    caseId: this.id,
    taskId: taskDefId,
    action: 'cancel',
    actor,
    beforeState,
    afterState,
    previousReceipt,
    output: { reason },
  });

  this.receipts.push(receipt);

  // Append event
  this._appendEvent({
    type: 'TASK_CANCELLED',
    taskId: taskDefId,
    workItemId,
    reason,
    actor,
    receiptId: receipt.id,
  });

  return { task, receipt };
}

/**
 * Cancel all tasks in a cancellation region
 * @this {import('./case-core.mjs').Case}
 * @param {string} regionId - Region ID
 * @param {string} [reason] - Cancellation reason
 * @param {string} [actor] - Actor performing the action
 * @returns {Promise<{cancelled: Array, receipts: Array}>}
 */
export async function cancelRegion(regionId, reason, actor) {
  const taskIds = this.workflow.getTasksInRegion(regionId);
  const cancelled = [];
  const newReceipts = [];

  for (const [workItemId, task] of this.workItems) {
    const taskDefId = this.getTaskDefIdForWorkItem(workItemId);
    if (taskIds.includes(taskDefId) && !task.isTerminal()) {
      const result = await cancelTask.call(this, workItemId, reason ?? `Region ${regionId} cancelled`, actor);
      cancelled.push(result.task);
      newReceipts.push(result.receipt);
    }
  }

  // Append event
  this._appendEvent({
    type: 'REGION_CANCELLED',
    regionId,
    cancelledCount: cancelled.length,
    reason,
    actor,
  });

  return { cancelled, receipts: newReceipts };
}

/**
 * Set circuit breaker state for a task
 * @this {import('./case-core.mjs').Case}
 * @param {string} taskId - Task definition ID
 * @param {boolean} enabled - Whether task is enabled
 * @returns {Promise<{cancelled: Array}>}
 */
export async function setCircuitBreaker(taskId, enabled) {
  this.circuitBreakers.set(taskId, enabled);

  // Append event
  this._appendEvent({
    type: 'CIRCUIT_BREAKER_SET',
    taskId,
    enabled,
  });

  if (!enabled) {
    // Cancel any pending work items for this task
    const cancelled = [];
    for (const [workItemId, task] of this.workItems) {
      const taskDefId = this.getTaskDefIdForWorkItem(workItemId);
      if (taskDefId === taskId && !task.isTerminal()) {
        const result = await cancelTask.call(this, workItemId, 'Circuit breaker disabled');
        cancelled.push(result.task);
      }
    }
    return { cancelled };
  }

  return { cancelled: [] };
}

// =============================================================================
// Install Methods on Case Prototype
// =============================================================================

/**
 * Install lifecycle methods on Case prototype
 * @param {Function} CaseClass - Case class constructor
 */
export function installLifecycleMethods(CaseClass) {
  // Petri net marking
  CaseClass.prototype.getMarking = getMarking;
  CaseClass.prototype.getTokens = getTokens;
  CaseClass.prototype._addTokens = _addTokens;
  CaseClass.prototype._removeTokens = _removeTokens;
  CaseClass.prototype.enable = enable;
  CaseClass.prototype.disable = disable;
  CaseClass.prototype._fireTransition = _fireTransition;
  CaseClass.prototype.canEnableTask = canEnableTask;

  // Task lifecycle
  CaseClass.prototype.start = start;
  CaseClass.prototype.enableTask = enableTask;
  CaseClass.prototype.startTask = startTask;
  CaseClass.prototype.completeTask = completeTask;
  CaseClass.prototype.cancelTask = cancelTask;
  CaseClass.prototype.cancelRegion = cancelRegion;
  CaseClass.prototype.setCircuitBreaker = setCircuitBreaker;
}
