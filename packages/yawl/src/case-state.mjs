/**
 * @file YAWL Case State - Petri net marking and state management
 * @module @unrdf/yawl/case-state
 */

import { now, toISO } from '@unrdf/kgc-4d';
import { YawlTask, TaskStatus } from './task.mjs';

// =============================================================================
// Case State Methods
// =============================================================================

/**
 * State management methods for Case class.
 * Handles Petri net marking, token operations, and state snapshots.
 */
export const CaseStateMixin = {
  /**
   * Initialize marking with tokens in input conditions
   * @private
   */
  _initializeMarking() {
    // Place initial tokens in the input condition
    // In YAWL, the start task's input condition gets a token
    const startTaskId = this.workflow.getStartTaskId();
    if (startTaskId) {
      const inputConditionId = `input:${startTaskId}`;
      this._marking.set(inputConditionId, 1);
    }
  },

  /**
   * Get the current Petri net marking (token placement)
   * @returns {Object<string, number>} Map of conditionId to token count
   */
  getMarking() {
    return Object.fromEntries(this._marking);
  },

  /**
   * Get token count for a specific condition
   * @param {string} conditionId - Condition/place identifier
   * @returns {number} Number of tokens in the condition
   */
  getTokens(conditionId) {
    return this._marking.get(conditionId) ?? 0;
  },

  /**
   * Add tokens to a condition
   * @param {string} conditionId - Condition identifier
   * @param {number} [count=1] - Number of tokens to add
   * @private
   */
  _addTokens(conditionId, count = 1) {
    const current = this._marking.get(conditionId) ?? 0;
    this._marking.set(conditionId, current + count);
  },

  /**
   * Remove tokens from a condition
   * @param {string} conditionId - Condition identifier
   * @param {number} [count=1] - Number of tokens to remove
   * @returns {boolean} True if tokens were available and removed
   * @private
   */
  _removeTokens(conditionId, count = 1) {
    const current = this._marking.get(conditionId) ?? 0;
    if (current < count) return false;
    const remaining = current - count;
    if (remaining === 0) {
      this._marking.delete(conditionId);
    } else {
      this._marking.set(conditionId, remaining);
    }
    return true;
  },

  /**
   * Enable a work item by producing tokens in its output condition
   * Called when a task becomes enabled (ready to fire)
   * @param {import('./task.mjs').YawlTask} workItem - Work item being enabled
   */
  enable(workItem) {
    const taskDefId = this.getTaskDefIdForWorkItem(workItem.id);

    // Consume token from input condition
    const inputConditionId = `input:${taskDefId}`;
    this._removeTokens(inputConditionId, 1);

    // Produce token in enabled condition
    const enabledConditionId = `enabled:${taskDefId}`;
    this._addTokens(enabledConditionId, 1);

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
  },

  /**
   * Disable a work item by consuming tokens from its enabled condition
   * Called when a task is cancelled or superseded
   * @param {import('./task.mjs').YawlTask} workItem - Work item being disabled
   */
  disable(workItem) {
    const taskDefId = this.getTaskDefIdForWorkItem(workItem.id);

    // Consume token from enabled condition
    const enabledConditionId = `enabled:${taskDefId}`;
    this._removeTokens(enabledConditionId, 1);

    // Log event
    this._appendEvent({
      type: 'TOKENS_CONSUMED',
      conditionId: enabledConditionId,
      count: 1,
      workItemId: workItem.id,
      reason: 'disabled',
    });
  },

  /**
   * Fire a transition: consume input tokens, produce output tokens
   * Called when a task completes
   * @param {import('./task.mjs').YawlTask} workItem - Completed work item
   * @private
   */
  _fireTransition(workItem) {
    const taskDefId = this.getTaskDefIdForWorkItem(workItem.id);

    // Consume token from started condition
    const startedConditionId = `started:${taskDefId}`;
    this._removeTokens(startedConditionId, 1);

    // Produce tokens in output conditions (based on split semantics)
    const outputConditionId = `output:${taskDefId}`;
    this._addTokens(outputConditionId, 1);

    // Log events
    this._appendEvent({
      type: 'TRANSITION_FIRED',
      taskId: taskDefId,
      workItemId: workItem.id,
    });
  },

  /**
   * Check if a task can be enabled based on token availability and join semantics
   * @param {string} taskDefId - Task definition ID
   * @returns {boolean} True if task can be enabled
   */
  canEnableTask(taskDefId) {
    // Check circuit breaker
    if (this.circuitBreakers.get(taskDefId) === false) {
      return false;
    }

    // Use workflow's canEnable method which considers join semantics
    return this.workflow.canEnable(taskDefId, this.completedTasks, this.activatedTasks);
  },

  // ===========================================================================
  // Event Log
  // ===========================================================================

  /**
   * Append an event to the case event log
   * @param {Object} eventData - Event data
   * @private
   */
  _appendEvent(eventData) {
    const timestamp = now();
    this.eventLog.push({
      ...eventData,
      caseId: this.id,
      timestamp: timestamp.toString(),
      timestampISO: toISO(timestamp),
    });
  },

  /**
   * Get all events for this case
   * @returns {Array<Object>} Event log
   */
  getEvents() {
    return [...this.eventLog];
  },

  // ===========================================================================
  // State Snapshots
  // ===========================================================================

  /**
   * Get current case state (for hashing and receipts)
   * @returns {Object} State snapshot
   */
  getState() {
    return {
      id: this.id,
      workflowId: this.workflowId,
      status: this._status,
      data: this.data,
      workItems: [...this.workItems.values()].map(t => t.toJSON()),
      completedTasks: [...this.completedTasks],
      activatedTasks: [...this.activatedTasks],
      marking: Object.fromEntries(this._marking),
    };
  },

  /**
   * Serialize to JSON
   * @returns {Object} JSON representation
   */
  toJSON() {
    return {
      id: this.id,
      workflowId: this.workflowId,
      status: this._status,
      createdAt: this.createdAt?.toString(),
      startedAt: this.startedAt?.toString(),
      completedAt: this.completedAt?.toString(),
      data: this.data,
      workItems: [...this.workItems.values()].map(t => t.toJSON()),
      receipts: this.receipts.map(r => r.toJSON()),
      completedTasks: [...this.completedTasks],
      activatedTasks: [...this.activatedTasks],
      circuitBreakers: Object.fromEntries(this.circuitBreakers),
      marking: Object.fromEntries(this._marking),
      eventLog: this.eventLog,
    };
  },
};
