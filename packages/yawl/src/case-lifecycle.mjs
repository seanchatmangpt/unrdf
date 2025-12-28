/**
 * @file YAWL Case Lifecycle - Status transitions and task execution
 * @module @unrdf/yawl/case-lifecycle
 */

import { randomUUID } from 'crypto';
import { now } from '@unrdf/kgc-4d';
import { YawlTask } from './task.mjs';
import { buildReceipt } from './receipt.mjs';
import { CaseStatus } from './case-core.mjs';
import { WorkflowError } from '../errors.mjs';

// =============================================================================
// Case Lifecycle Methods
// =============================================================================

/**
 * Lifecycle methods for Case class.
 * Handles task enabling, starting, completing, and cancellation.
 */
export const CaseLifecycleMixin = {
  /**
   * Start the case (enable start task)
   * @returns {Promise<{task: import('./task.mjs').YawlTask, receipt: import('./receipt.mjs').YawlReceipt}>}
   */
  async start() {
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
    const task = await this.enableTask(startTaskId);

    return { task: task.task, receipt: task.receipt };
  },

  /**
   * Enable a task
   * @param {string} taskId - Task definition ID
   * @param {string} [actor] - Actor performing the action
   * @returns {Promise<{task: import('./task.mjs').YawlTask, receipt: import('./receipt.mjs').YawlReceipt}>}
   */
  async enableTask(taskId, actor) {
    const taskDef = this.workflow.getTask(taskId);
    if (!taskDef) {
      throw new Error(`Task ${taskId} not found in workflow`);
    }

    // Check circuit breaker
    if (this.circuitBreakers.get(taskId) === false) {
      throw new Error(`Task ${taskId} is disabled by circuit breaker`);
    }

    const beforeState = this.getState();

    // Create task instance with UUID
    const workItemId = randomUUID();
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
    this.enable(task);

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
  },

  /**
   * Start a task
   * @param {string} workItemId - Work item ID
   * @param {string} [resourceId] - Resource performing the task
   * @param {string} [actor] - Actor performing the action
   * @returns {Promise<{task: import('./task.mjs').YawlTask, receipt: import('./receipt.mjs').YawlReceipt}>}
   */
  async startTask(workItemId, resourceId, actor) {
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
    this._removeTokens(enabledConditionId, 1);
    this._addTokens(startedConditionId, 1);

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
  },

  /**
   * Complete a task
   * @param {string} workItemId - Work item ID
   * @param {Object} [output] - Task output data
   * @param {string} [actor] - Actor performing the action
   * @returns {Promise<{task: import('./task.mjs').YawlTask, receipt: import('./receipt.mjs').YawlReceipt, downstreamEnabled: Array}>}
   */
  async completeTask(workItemId, output = {}, actor) {
    const task = this.workItems.get(workItemId);
    if (!task) {
      throw new Error(`Work item ${workItemId} not found`);
    }

    const beforeState = this.getState();
    const taskDefId = this.getTaskDefIdForWorkItem(workItemId);

    task.complete(output);
    this.completedTasks.add(taskDefId);

    // Fire the Petri net transition
    this._fireTransition(task);

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
      this._addTokens(inputConditionId, 1);
    }

    // Enable downstream tasks based on join semantics
    const downstreamEnabled = [];
    for (const nextTaskId of toEnable) {
      if (this.workflow.canEnable(nextTaskId, this.completedTasks, this.activatedTasks)) {
        const enabled = await this.enableTask(nextTaskId, actor);
        downstreamEnabled.push({
          taskId: nextTaskId,
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
  },

  /**
   * Cancel a single work item
   * @param {string} workItemId - Work item ID
   * @param {string} [reason] - Cancellation reason
   * @param {string} [actor] - Actor performing the action
   * @returns {Promise<{task: import('./task.mjs').YawlTask, receipt: import('./receipt.mjs').YawlReceipt}>}
   */
  async cancelTask(workItemId, reason, actor) {
    const task = this.workItems.get(workItemId);
    if (!task) {
      throw new Error(`Work item ${workItemId} not found`);
    }

    const beforeState = this.getState();
    const taskDefId = this.getTaskDefIdForWorkItem(workItemId);

    // Update Petri net marking
    this.disable(task);

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
  },

  /**
   * Cancel all tasks in a cancellation region
   * @param {string} regionId - Region ID
   * @param {string} [reason] - Cancellation reason
   * @param {string} [actor] - Actor performing the action
   * @returns {Promise<{cancelled: Array, receipts: Array}>}
   */
  async cancelRegion(regionId, reason, actor) {
    const taskIds = this.workflow.getTasksInRegion(regionId);
    const cancelled = [];
    const newReceipts = [];

    for (const [workItemId, task] of this.workItems) {
      const taskDefId = this.getTaskDefIdForWorkItem(workItemId);
      if (taskIds.includes(taskDefId) && !task.isTerminal()) {
        const result = await this.cancelTask(workItemId, reason ?? `Region ${regionId} cancelled`, actor);
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
  },

  /**
   * Set circuit breaker state for a task
   * @param {string} taskId - Task definition ID
   * @param {boolean} enabled - Whether task is enabled
   * @returns {Promise<{cancelled: Array}>}
   */
  async setCircuitBreaker(taskId, enabled) {
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
          const result = await this.cancelTask(workItemId, 'Circuit breaker disabled');
          cancelled.push(result.task);
        }
      }
      return { cancelled };
    }

    return { cancelled: [] };
  },
};
