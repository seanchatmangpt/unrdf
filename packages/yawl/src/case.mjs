/**
 * @file YAWL Case - Runtime instance of a workflow with Petri net semantics
 * @module @unrdf/yawl/case
 *
 * @description
 * Case lifecycle management for YAWL workflow instances.
 * Implements Petri net marking (token placement) for workflow state tracking.
 * Integrates with KGC-4D for event sourcing and RDF for semantic representation.
 *
 * @example
 * import { createCase, Case, CaseStatus } from '@unrdf/yawl/case';
 * import { YawlWorkflow } from '@unrdf/yawl/workflow';
 *
 * const workflow = new YawlWorkflow({ id: 'expense-approval' });
 * workflow.addTask({ id: 'submit', name: 'Submit Request' });
 * workflow.addTask({ id: 'approve', name: 'Approve Request' });
 * workflow.setStart('submit');
 * workflow.setEnd(['approve']);
 *
 * const caseInstance = createCase(workflow);
 * console.log(caseInstance.getStatus()); // 'running'
 * console.log(caseInstance.getEnabledWorkItems()); // [{ submit work item }]
 */

import { z } from 'zod';
import { now, toISO } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';
import { YawlTask, TaskStatus } from './task.mjs';
import { buildReceipt } from './receipt.mjs';
import {
  // Namespaces
  YAWL,
  YAWL_WORK,

  // Classes
  WorkflowCase,
  WorkItem as WorkItemClass,
  Condition,

  // Status values
  Case_Active,
  Case_Completed,
  Case_Suspended,
  Case_Cancelled,
  Case_Failed,
  WorkItem_Enabled,
  WorkItem_Started,
  WorkItem_Completed,
  WorkItem_Cancelled,

  // Properties
  specId,
  status as statusProp,
  createdAt,
  completedAt,
  workItems as workItemsProp,
  caseData,
  taskRef,
  caseRef,

  // RDF terms
  rdfType,

  // URI factories
  caseUri,
  workItemUri,
  specUri,
  caseGraph,
  conditionUri,

  // Literal factories
  dateTimeLiteral,
  stringLiteral,
  integerLiteral,
} from './ontology/yawl-ontology.mjs';

const { quad, namedNode } = dataFactory;

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

/**
 * Map CaseStatus to RDF terms
 * @type {Object<string, import('oxigraph').NamedNode>}
 */
const CASE_STATUS_RDF_MAP = {
  [CaseStatus.CREATED]: Case_Active,
  [CaseStatus.RUNNING]: Case_Active,
  [CaseStatus.COMPLETED]: Case_Completed,
  [CaseStatus.SUSPENDED]: Case_Suspended,
  [CaseStatus.CANCELLED]: Case_Cancelled,
  [CaseStatus.FAILED]: Case_Failed,
};

/**
 * Map WorkItem status to RDF terms
 * @type {Object<string, import('oxigraph').NamedNode>}
 */
const WORKITEM_STATUS_RDF_MAP = {
  [TaskStatus.ENABLED]: WorkItem_Enabled,
  [TaskStatus.ACTIVE]: WorkItem_Started,
  'running': WorkItem_Started, // backwards compatibility
  [TaskStatus.COMPLETED]: WorkItem_Completed,
  [TaskStatus.CANCELLED]: WorkItem_Cancelled,
};

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
    // Support both 'running' (backwards compat) and 'active' (current)
    const running = this.getWorkItemsByStatus('running');
    const active = this.getWorkItemsByStatus(TaskStatus.ACTIVE);
    return [...running, ...active];
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
  // Petri Net Marking (Token Management)
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

  /**
   * Get the current Petri net marking (token placement)
   * @returns {Object<string, number>} Map of conditionId to token count
   */
  getMarking() {
    return Object.fromEntries(this._marking);
  }

  /**
   * Get token count for a specific condition
   * @param {string} conditionId - Condition/place identifier
   * @returns {number} Number of tokens in the condition
   */
  getTokens(conditionId) {
    return this._marking.get(conditionId) ?? 0;
  }

  /**
   * Add tokens to a condition
   * @param {string} conditionId - Condition identifier
   * @param {number} [count=1] - Number of tokens to add
   * @private
   */
  _addTokens(conditionId, count = 1) {
    const current = this._marking.get(conditionId) ?? 0;
    this._marking.set(conditionId, current + count);
  }

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
  }

  /**
   * Enable a work item by producing tokens in its output condition
   * Called when a task becomes enabled (ready to fire)
   * @param {YawlTask} workItem - Work item being enabled
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
  }

  /**
   * Disable a work item by consuming tokens from its enabled condition
   * Called when a task is cancelled or superseded
   * @param {YawlTask} workItem - Work item being disabled
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
  }

  /**
   * Fire a transition: consume input tokens, produce output tokens
   * Called when a task completes
   * @param {YawlTask} workItem - Completed work item
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
  }

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
  }

  // ===========================================================================
  // Task Lifecycle
  // ===========================================================================

  /**
   * Start the case (enable start task)
   * @returns {Promise<{task: YawlTask, receipt: import('./receipt.mjs').YawlReceipt}>}
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
  }

  /**
   * Enable a task
   * @param {string} taskId - Task definition ID
   * @param {string} [actor] - Actor performing the action
   * @returns {Promise<{task: YawlTask, receipt: import('./receipt.mjs').YawlReceipt}>}
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
  }

  /**
   * Start a task
   * @param {string} workItemId - Work item ID
   * @param {string} [resourceId] - Resource performing the task
   * @param {string} [actor] - Actor performing the action
   * @returns {Promise<{task: YawlTask, receipt: import('./receipt.mjs').YawlReceipt}>}
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
  }

  /**
   * Complete a task
   * @param {string} workItemId - Work item ID
   * @param {Object} [output] - Task output data
   * @param {string} [actor] - Actor performing the action
   * @returns {Promise<{task: YawlTask, receipt: import('./receipt.mjs').YawlReceipt, downstreamEnabled: Array}>}
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
   * @param {string} workItemId - Work item ID
   * @param {string} [reason] - Cancellation reason
   * @param {string} [actor] - Actor performing the action
   * @returns {Promise<{task: YawlTask, receipt: import('./receipt.mjs').YawlReceipt}>}
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
  }

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
  }

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
    const parts = workItemId.split('-');
    if (parts.length >= 2) {
      // Remove caseId prefix and timestamp suffix
      return parts.slice(1, -1).join('-');
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

// =============================================================================
// Factory Function
// =============================================================================

/**
 * Options for creating a new case
 * @typedef {Object} CreateCaseOptions
 * @property {string} [caseId] - Custom case ID (auto-generated if not provided)
 * @property {Object} [initialData={}] - Initial case variables
 * @property {boolean} [autoStart=true] - Automatically start the case
 */

/**
 * Create a new case instance for a workflow.
 *
 * Initializes work items for all tasks and auto-enables initial tasks.
 *
 * @param {import('./workflow.mjs').YawlWorkflow} workflow - Workflow definition
 * @param {CreateCaseOptions} [options={}] - Creation options
 * @returns {Promise<Case>} Created and optionally started case
 *
 * @example
 * import { createCase } from '@unrdf/yawl/case';
 *
 * const workflow = new YawlWorkflow({ id: 'expense-approval' });
 * // ... configure workflow ...
 *
 * const caseInstance = await createCase(workflow, {
 *   caseId: 'expense-001',
 *   initialData: { amount: 500, submitter: 'john@example.com' },
 *   autoStart: true
 * });
 */
export async function createCase(workflow, options = {}) {
  const {
    caseId = `case-${workflow.id}-${Date.now()}-${Math.random().toString(36).slice(2, 6)}`,
    initialData = {},
    autoStart = true,
  } = options;

  // Validate workflow
  const validation = workflow.validate();
  if (!validation.valid) {
    throw new Error(`Invalid workflow: ${validation.errors.join(', ')}`);
  }

  const caseInstance = new Case({
    id: caseId,
    workflowId: workflow.id,
    data: initialData,
  }, workflow);

  if (autoStart) {
    await caseInstance.start();
  }

  return caseInstance;
}

// =============================================================================
// RDF Integration
// =============================================================================

/**
 * Convert a case instance to RDF quads and add to store.
 *
 * Creates YAWL RDF representation including:
 * - Case metadata (id, spec, status, timestamps)
 * - Work items with status and task references
 * - Petri net marking (token counts in conditions)
 *
 * @param {Case} caseInstance - Case to serialize
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - RDF store to add quads to
 * @returns {Promise<{quadCount: number, caseUri: import('oxigraph').NamedNode}>}
 *
 * @example
 * import { caseToRDF } from '@unrdf/yawl/case';
 * import { createStore } from '@unrdf/oxigraph';
 *
 * const store = createStore();
 * const { quadCount, caseUri } = await caseToRDF(caseInstance, store);
 * console.log(`Added ${quadCount} quads for case ${caseUri.value}`);
 */
export async function caseToRDF(caseInstance, store) {
  const caseNode = caseUri(caseInstance.id);
  const graph = caseGraph(caseInstance.id);
  let quadCount = 0;

  // Case type
  store.add(quad(caseNode, rdfType, WorkflowCase, graph));
  quadCount++;

  // Specification reference
  store.add(quad(caseNode, specId, specUri(caseInstance.workflowId), graph));
  quadCount++;

  // Status
  const statusNode = CASE_STATUS_RDF_MAP[caseInstance._status] || Case_Active;
  store.add(quad(caseNode, statusProp, statusNode, graph));
  quadCount++;

  // Timestamps
  if (caseInstance.createdAt) {
    store.add(quad(caseNode, createdAt, dateTimeLiteral(toISO(caseInstance.createdAt)), graph));
    quadCount++;
  }

  if (caseInstance.startedAt) {
    store.add(quad(caseNode, namedNode(YAWL + 'startedAt'), dateTimeLiteral(toISO(caseInstance.startedAt)), graph));
    quadCount++;
  }

  if (caseInstance.completedAt) {
    store.add(quad(caseNode, completedAt, dateTimeLiteral(toISO(caseInstance.completedAt)), graph));
    quadCount++;
  }

  // Case data
  if (Object.keys(caseInstance.data).length > 0) {
    store.add(quad(caseNode, caseData, stringLiteral(JSON.stringify(caseInstance.data)), graph));
    quadCount++;
  }

  // Work items
  for (const [wiId, workItem] of caseInstance.workItems) {
    const wiNode = workItemUri(wiId);

    // Work item type
    store.add(quad(wiNode, rdfType, WorkItemClass, graph));
    quadCount++;

    // Case reference
    store.add(quad(wiNode, caseRef, caseNode, graph));
    quadCount++;

    // Task reference
    const taskDefId = caseInstance.getTaskDefIdForWorkItem(wiId);
    store.add(quad(wiNode, taskRef, namedNode(YAWL + 'task-' + taskDefId), graph));
    quadCount++;

    // Status
    const wiStatusNode = WORKITEM_STATUS_RDF_MAP[workItem.status] || WorkItem_Enabled;
    store.add(quad(wiNode, statusProp, wiStatusNode, graph));
    quadCount++;

    // Timestamps
    if (workItem.enabledAt) {
      store.add(quad(wiNode, namedNode(YAWL + 'enabledAt'), dateTimeLiteral(toISO(workItem.enabledAt)), graph));
      quadCount++;
    }

    if (workItem.startedAt) {
      store.add(quad(wiNode, namedNode(YAWL + 'startedAt'), dateTimeLiteral(toISO(workItem.startedAt)), graph));
      quadCount++;
    }

    if (workItem.completedAt) {
      store.add(quad(wiNode, completedAt, dateTimeLiteral(toISO(workItem.completedAt)), graph));
      quadCount++;
    }

    // Link from case
    store.add(quad(caseNode, workItemsProp, wiNode, graph));
    quadCount++;
  }

  // Petri net marking (tokens in conditions)
  for (const [condId, tokenCount] of caseInstance._marking) {
    const condNode = conditionUri(condId);

    store.add(quad(condNode, rdfType, Condition, graph));
    quadCount++;

    store.add(quad(condNode, caseRef, caseNode, graph));
    quadCount++;

    store.add(quad(condNode, namedNode(YAWL + 'tokenCount'), integerLiteral(tokenCount), graph));
    quadCount++;
  }

  return { quadCount, caseUri: caseNode };
}

/**
 * Load case state from RDF store.
 *
 * Reconstructs a Case instance from its RDF representation.
 * Requires the workflow definition to be provided separately.
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - RDF store to read from
 * @param {string} caseId - Case identifier to load
 * @param {import('./workflow.mjs').YawlWorkflow} workflow - Workflow definition
 * @returns {Promise<Case|null>} Loaded case or null if not found
 *
 * @example
 * import { caseFromRDF } from '@unrdf/yawl/case';
 *
 * const caseInstance = await caseFromRDF(store, 'case-123', workflow);
 * if (caseInstance) {
 *   console.log(`Loaded case with ${caseInstance.getWorkItems().length} work items`);
 * }
 */
export async function caseFromRDF(store, caseId, workflow) {
  const caseNode = caseUri(caseId);
  const graph = caseGraph(caseId);

  // Check if case exists
  const typeQuads = [...store.match(caseNode, rdfType, WorkflowCase, graph)];
  if (typeQuads.length === 0) {
    return null;
  }

  // Extract case data
  const caseDataQuads = {
    id: caseId,
    workflowId: workflow.id,
    status: CaseStatus.CREATED,
    data: {},
  };

  // Get status
  const statusQuads = [...store.match(caseNode, statusProp, null, graph)];
  if (statusQuads.length > 0) {
    const statusValue = statusQuads[0].object.value;
    // Reverse lookup status
    for (const [status, rdfTerm] of Object.entries(CASE_STATUS_RDF_MAP)) {
      if (rdfTerm.value === statusValue) {
        caseDataQuads.status = status;
        break;
      }
    }
  }

  // Get timestamps
  const createdAtQuads = [...store.match(caseNode, createdAt, null, graph)];
  if (createdAtQuads.length > 0) {
    const dateStr = createdAtQuads[0].object.value;
    caseDataQuads.createdAt = BigInt(new Date(dateStr).getTime() * 1000000);
  }

  const startedAtQuads = [...store.match(caseNode, namedNode(YAWL + 'startedAt'), null, graph)];
  if (startedAtQuads.length > 0) {
    const dateStr = startedAtQuads[0].object.value;
    caseDataQuads.startedAt = BigInt(new Date(dateStr).getTime() * 1000000);
  }

  const completedAtQuads = [...store.match(caseNode, completedAt, null, graph)];
  if (completedAtQuads.length > 0) {
    const dateStr = completedAtQuads[0].object.value;
    caseDataQuads.completedAt = BigInt(new Date(dateStr).getTime() * 1000000);
  }

  // Get case data
  const caseDataLiteralQuads = [...store.match(caseNode, caseData, null, graph)];
  if (caseDataLiteralQuads.length > 0) {
    try {
      caseDataQuads.data = JSON.parse(caseDataLiteralQuads[0].object.value);
    } catch {
      // Invalid JSON, keep empty data
    }
  }

  // Create case instance
  const caseInstance = new Case(caseDataQuads, workflow);

  // Clear auto-initialized marking (will restore from RDF)
  caseInstance._marking.clear();

  // Load work items
  const wiLinkQuads = [...store.match(caseNode, workItemsProp, null, graph)];
  for (const wiLinkQuad of wiLinkQuads) {
    const wiNode = wiLinkQuad.object;
    const wiId = wiNode.value.replace(YAWL_WORK, '');

    // Get task reference
    const taskRefQuads = [...store.match(wiNode, taskRef, null, graph)];
    const taskDefId = taskRefQuads.length > 0
      ? taskRefQuads[0].object.value.replace(YAWL + 'task-', '')
      : wiId;

    // Get status
    const wiStatusQuads = [...store.match(wiNode, statusProp, null, graph)];
    let wiStatus = TaskStatus.INACTIVE;
    if (wiStatusQuads.length > 0) {
      const statusValue = wiStatusQuads[0].object.value;
      for (const [status, rdfTerm] of Object.entries(WORKITEM_STATUS_RDF_MAP)) {
        if (rdfTerm.value === statusValue) {
          wiStatus = status;
          break;
        }
      }
    }

    // Get timestamps
    let enabledAt, startedAt, wiCompletedAt;

    const enabledAtQuads = [...store.match(wiNode, namedNode(YAWL + 'enabledAt'), null, graph)];
    if (enabledAtQuads.length > 0) {
      enabledAt = BigInt(new Date(enabledAtQuads[0].object.value).getTime() * 1000000);
    }

    const wiStartedAtQuads = [...store.match(wiNode, namedNode(YAWL + 'startedAt'), null, graph)];
    if (wiStartedAtQuads.length > 0) {
      startedAt = BigInt(new Date(wiStartedAtQuads[0].object.value).getTime() * 1000000);
    }

    const wiCompletedAtQuads = [...store.match(wiNode, completedAt, null, graph)];
    if (wiCompletedAtQuads.length > 0) {
      wiCompletedAt = BigInt(new Date(wiCompletedAtQuads[0].object.value).getTime() * 1000000);
    }

    // Create task
    const task = new YawlTask({
      id: wiId,
      name: taskDefId,
      caseId: caseId,
      status: wiStatus,
      enabledAt,
      startedAt,
      completedAt: wiCompletedAt,
    });

    caseInstance.workItems.set(wiId, task);

    if (!caseInstance.workItemsByTask.has(taskDefId)) {
      caseInstance.workItemsByTask.set(taskDefId, new Set());
    }
    caseInstance.workItemsByTask.get(taskDefId).add(wiId);

    // Track completed tasks
    if (wiStatus === TaskStatus.COMPLETED) {
      caseInstance.completedTasks.add(taskDefId);
    }
    caseInstance.activatedTasks.add(taskDefId);
  }

  // Load Petri net marking (conditions with tokens)
  const conditionQuads = [...store.match(null, rdfType, Condition, graph)];
  for (const condQuad of conditionQuads) {
    const condNode = condQuad.subject;

    // Check this condition belongs to this case
    const condCaseRefQuads = [...store.match(condNode, caseRef, caseNode, graph)];
    if (condCaseRefQuads.length === 0) continue;

    // Get token count
    const tokenQuads = [...store.match(condNode, namedNode(YAWL + 'tokenCount'), null, graph)];
    if (tokenQuads.length > 0) {
      const tokenCount = parseInt(tokenQuads[0].object.value, 10);
      const condId = condNode.value.replace(YAWL + 'condition-', '');
      caseInstance._marking.set(condId, tokenCount);
    }
  }

  return caseInstance;
}

// =============================================================================
// Exports
// =============================================================================

export default {
  // Classes
  Case,
  YawlCase: Case,

  // Enums
  CaseStatus,

  // Factory
  createCase,

  // RDF Integration
  caseToRDF,
  caseFromRDF,
};
