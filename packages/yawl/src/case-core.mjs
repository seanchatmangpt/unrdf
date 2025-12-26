/**
 * @file YAWL Case Core - Core data structures and work item management
 * @module @unrdf/yawl/case-core
 */

import { z } from 'zod';
import { now } from '@unrdf/kgc-4d';
import { TaskStatus } from './task.mjs';

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

export const CaseDataSchema = z.object({
  id: z.string().min(1),
  workflowId: z.string().min(1),
  status: z.enum(['created', 'running', 'completed', 'suspended', 'cancelled', 'failed']).default('created'),
  createdAt: z.bigint().optional(),
  startedAt: z.bigint().optional(),
  completedAt: z.bigint().optional(),
  data: z.record(z.string(), z.any()).optional(),
});

// =============================================================================
// Case Core Class
// =============================================================================

/**
 * Core Case class with basic functionality.
 * Extended by lifecycle, state, and RDF modules.
 *
 * @class
 */
export class CaseCore {
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
     * @type {Map<string, import('./task.mjs').YawlTask>}
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
   * @returns {import('./task.mjs').YawlTask[]} Array of all work items
   */
  getWorkItems() {
    return [...this.workItems.values()];
  }

  /**
   * Get a specific work item by ID
   * @param {string} workItemId - Work item identifier
   * @returns {import('./task.mjs').YawlTask|undefined} Work item or undefined if not found
   */
  getWorkItem(workItemId) {
    return this.workItems.get(workItemId);
  }

  /**
   * Get work items filtered by status
   * @param {string} statusFilter - Status to filter by (from TaskStatus)
   * @returns {import('./task.mjs').YawlTask[]} Array of matching work items
   */
  getWorkItemsByStatus(statusFilter) {
    return [...this.workItems.values()].filter(wi => wi.status === statusFilter);
  }

  /**
   * Get enabled work items (ready to be started)
   * @returns {import('./task.mjs').YawlTask[]} Array of enabled work items
   */
  getEnabledWorkItems() {
    return this.getWorkItemsByStatus(TaskStatus.ENABLED);
  }

  /**
   * Get active work items (currently executing)
   * @returns {import('./task.mjs').YawlTask[]} Array of running work items
   */
  getActiveWorkItems() {
    return this.getWorkItemsByStatus(TaskStatus.RUNNING);
  }

  /**
   * Get running work items (alias for getActiveWorkItems)
   * @returns {import('./task.mjs').YawlTask[]} Array of running work items
   */
  getRunningWorkItems() {
    return this.getActiveWorkItems();
  }

  /**
   * Get work items for a specific task definition
   * @param {string} taskDefId - Task definition ID
   * @returns {import('./task.mjs').YawlTask[]} Array of work items for this task
   */
  getWorkItemsForTask(taskDefId) {
    const workItemIds = this.workItemsByTask.get(taskDefId);
    if (!workItemIds) return [];
    return [...workItemIds].map(id => this.workItems.get(id)).filter(Boolean);
  }

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
}
