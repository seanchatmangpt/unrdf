/**
 * @file YAWL Case Core - Case class with state management
 * @module @unrdf/yawl/case-core
 *
 * @description
 * Core Case class implementation with constructor, status accessors,
 * work item accessors, event logging, and utility methods.
 */

import { z } from 'zod';
import { now, toISO } from '@unrdf/kgc-4d';
import { YawlTask, TaskStatus, TaskStatus_RUNNING } from './task.mjs';

// =============================================================================
// Case Status Enum
// =============================================================================

/**
 * Case lifecycle status values
 * @readonly
 * @enum {string}
 */
export const CaseStatus = Object.freeze({
  /** Case created but not started */
  CREATED: 'created',
  /** Case is running */
  RUNNING: 'running',
  /** Case completed successfully */
  COMPLETED: 'completed',
  /** Case was suspended */
  SUSPENDED: 'suspended',
  /** Case was cancelled */
  CANCELLED: 'cancelled',
  /** Case failed */
  FAILED: 'failed',
});

// =============================================================================
// Case Schema
// =============================================================================

const CaseDataSchema = z.object({
  id: z.string().min(1),
  workflowId: z.string().min(1),
  status: z.enum(['created', 'running', 'completed', 'suspended', 'cancelled', 'failed']).default('created'),
  createdAt: z.bigint().optional(),
  startedAt: z.bigint().optional(),
  completedAt: z.bigint().optional(),
  data: z.record(z.string(), z.any()).optional(),
});

// =============================================================================
// Case Class
// =============================================================================

/**
 * Represents a runtime instance of a workflow with Petri net semantics.
 *
 * Implements token-based state tracking where:
 * - Conditions (places) hold tokens
 * - Tasks (transitions) consume and produce tokens
 * - Join semantics determine when tasks can fire
 *
 * @class
 */
export class Case {
  /**
   * Create a new Case instance
   * @param {Object} data - Case configuration
   * @param {string} data.id - Unique case identifier
   * @param {string} data.workflowId - Parent workflow ID
   * @param {string} [data.status='created'] - Initial status
   * @param {Object} [data.data={}] - Case variables/data
   * @param {import('./workflow.mjs').YawlWorkflow} workflow - Workflow definition
   */
  constructor(data, workflow) {
    const validated = CaseDataSchema.parse(data);
    this.id = validated.id;
    this.workflowId = validated.workflowId;
    this.workflow = workflow;
    this._status = validated.status;
    this.createdAt = validated.createdAt ?? now();
    this.startedAt = validated.startedAt;
    this.completedAt = validated.completedAt;
    this.data = validated.data ?? {};

    /**
     * Work items by ID
     * @type {Map<string, YawlTask>}
     */
    this.workItems = new Map();

    /**
     * Work items indexed by task definition ID
     * @type {Map<string, Set<string>>}
     */
    this.workItemsByTask = new Map();

    /**
     * Receipt chain for audit trail
     * @type {Array<import('./receipt.mjs').YawlReceipt>}
     */
    this.receipts = [];

    /**
     * Completed task definition IDs
     * @type {Set<string>}
     */
    this.completedTasks = new Set();

    /**
     * Activated task IDs (for OR-join semantics)
     * @type {Set<string>}
     */
    this.activatedTasks = new Set();

    /**
     * Circuit breaker state by task definition ID
     * @type {Map<string, boolean>}
     */
    this.circuitBreakers = new Map();

    /**
     * Petri net marking: tokens in each condition
     * Key: conditionId, Value: token count
     * @type {Map<string, number>}
     */
    this._marking = new Map();

    /**
     * Event log for this case
     * @type {Array<Object>}
     */
    this.eventLog = [];

    // Initialize marking with input condition tokens
    this._initializeMarking();
  }

  // ===========================================================================
  // Status Accessors
  // ===========================================================================

  /**
   * Get current case status
   * @returns {string} Current status from CaseStatus enum
   */
  getStatus() {
    return this._status;
  }

  /**
   * Get current case status (alias for getStatus)
   * @returns {string}
   */
  get status() {
    return this._status;
  }

  /**
   * Set case status
   * @param {string} value - New status
   */
  set status(value) {
    this._status = value;
  }

  /**
   * Check if case is complete
   * All end tasks must be completed, or case status is terminal
   * @returns {boolean} True if case has finished execution
   */
  isComplete() {
    // Terminal statuses are complete
    if ([CaseStatus.COMPLETED, CaseStatus.CANCELLED, CaseStatus.FAILED].includes(this._status)) {
      return true;
    }

    // Check if all end tasks are complete
    const endTaskIds = this.workflow.getEndTaskIds();
    if (endTaskIds.length === 0) {
      return false;
    }

    return endTaskIds.every(taskId => this.completedTasks.has(taskId));
  }

  // ===========================================================================
  // Work Item Accessors
  // ===========================================================================

  /**
   * Get all work items in this case
   * @returns {YawlTask[]} Array of all work items
   */
  getWorkItems() {
    return [...this.workItems.values()];
  }

  /**
   * Get a specific work item by ID
   * @param {string} workItemId - Work item identifier
   * @returns {YawlTask|undefined} Work item or undefined if not found
   */
  getWorkItem(workItemId) {
    return this.workItems.get(workItemId);
  }

  /**
   * Get work items filtered by status
   * @param {string} statusFilter - Status to filter by (from TaskStatus)
   * @returns {YawlTask[]} Array of matching work items
   */
  getWorkItemsByStatus(statusFilter) {
    return [...this.workItems.values()].filter(wi => wi.status === statusFilter);
  }

  /**
   * Get enabled work items (ready to be started)
   * @returns {YawlTask[]} Array of enabled work items
   */
  getEnabledWorkItems() {
    return this.getWorkItemsByStatus(TaskStatus.ENABLED);
  }

  /**
   * Get active work items (currently executing)
   * @returns {YawlTask[]} Array of running work items
   */
  getActiveWorkItems() {
    return this.getWorkItemsByStatus(TaskStatus_RUNNING);
  }

  /**
   * Get running work items (alias for getActiveWorkItems)
   * @returns {YawlTask[]} Array of running work items
   */
  getRunningWorkItems() {
    return this.getActiveWorkItems();
  }

  /**
   * Get work items for a specific task definition
   * @param {string} taskDefId - Task definition ID
   * @returns {YawlTask[]} Array of work items for this task
   */
  getWorkItemsForTask(taskDefId) {
    const workItemIds = this.workItemsByTask.get(taskDefId);
    if (!workItemIds) return [];
    return [...workItemIds].map(id => this.workItems.get(id)).filter(Boolean);
  }

  // ===========================================================================
  // Petri Net Marking - Initialization
  // ===========================================================================

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
  }

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
  }

  /**
   * Get all events for this case
   * @returns {Array<Object>} Event log
   */
  getEvents() {
    return [...this.eventLog];
  }

  // ===========================================================================
  // Utility Methods
  // ===========================================================================

  /**
   * Get task definition ID from work item ID
   * @param {string} workItemId - Work item ID
   * @returns {string} Task definition ID
   */
  getTaskDefIdForWorkItem(workItemId) {
    // Work item ID format: {caseId}-{taskDefId}-{timestamp}
    // Case ID may contain dashes, so we use it as a prefix to extract the task ID
    const caseIdPrefix = `${this.id}-`;
    if (workItemId.startsWith(caseIdPrefix)) {
      const remainder = workItemId.substring(caseIdPrefix.length);
      // remainder is now: {taskDefId}-{timestamp}
      // Remove the timestamp (last part after last dash)
      const lastDashIndex = remainder.lastIndexOf('-');
      if (lastDashIndex !== -1) {
        return remainder.substring(0, lastDashIndex);
      }
      return remainder;
    }
    return workItemId;
  }

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
  }

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
  }

  /**
   * Create Case from JSON
   * @param {Object} json - JSON representation
   * @param {import('./workflow.mjs').YawlWorkflow} workflow - Workflow definition
   * @returns {Case} Restored case
   */
  static fromJSON(json, workflow) {
    const caseInstance = new Case({
      id: json.id,
      workflowId: json.workflowId,
      status: json.status,
      createdAt: json.createdAt ? BigInt(json.createdAt) : undefined,
      startedAt: json.startedAt ? BigInt(json.startedAt) : undefined,
      completedAt: json.completedAt ? BigInt(json.completedAt) : undefined,
      data: json.data,
    }, workflow);

    // Restore work items
    for (const wiJson of json.workItems || []) {
      const task = YawlTask.fromJSON(wiJson);
      caseInstance.workItems.set(task.id, task);

      const taskDefId = caseInstance.getTaskDefIdForWorkItem(task.id);
      if (!caseInstance.workItemsByTask.has(taskDefId)) {
        caseInstance.workItemsByTask.set(taskDefId, new Set());
      }
      caseInstance.workItemsByTask.get(taskDefId).add(task.id);
    }

    // Restore completed/activated tasks
    caseInstance.completedTasks = new Set(json.completedTasks || []);
    caseInstance.activatedTasks = new Set(json.activatedTasks || []);

    // Restore circuit breakers
    caseInstance.circuitBreakers = new Map(Object.entries(json.circuitBreakers || {}));

    // Restore marking
    caseInstance._marking = new Map(Object.entries(json.marking || {}));

    // Restore event log
    caseInstance.eventLog = json.eventLog || [];

    return caseInstance;
  }
}

// Backwards compatibility alias
export { Case as YawlCase };
