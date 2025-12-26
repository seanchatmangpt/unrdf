/**
 * YAWL Task Instance - Core state machine implementation
 * @module @unrdf/yawl/task-instance
 */

import { now } from '@unrdf/kgc-4d';
import { TaskDefinition, TaskStatus } from './task-definitions.mjs';

/**
 * Represents a runtime task instance in a workflow case
 * Implements full state machine with validated transitions and receipts
 */
export class TaskInstance {
  /**
   * @param {TaskDefinition|Object} taskDef - Task definition or spec
   * @param {string} caseId - Case ID this instance belongs to
   * @param {Object} [options] - Additional options
   */
  constructor(taskDef, caseId, options = {}) {
    // Handle both TaskDefinition instance and plain objects
    const def = taskDef instanceof TaskDefinition ? taskDef : new TaskDefinition(taskDef);

    /** @type {string} Unique instance ID */
    this.id = options.id ?? `${caseId}-${def.id}-${Date.now()}`;
    /** @type {string} Task definition ID */
    this.taskDefId = def.id;
    /** @type {string} Case ID */
    this.caseId = caseId;
    /** @type {string} Human-readable name */
    this.name = def.name;
    /** @type {TaskDefinition} Reference to task definition */
    this.taskDefinition = def;
    /** @type {string} Current status */
    this.status = TaskStatus.DISABLED;
    /** @type {Object} Input data */
    this.inputData = options.inputData ?? {};
    /** @type {Object} Output data */
    this.outputData = {};
    /** @type {string|undefined} Assigned resource ID */
    this.assignedResource = options.assignedResource;
    /** @type {string|undefined} Assigned role */
    this.role = options.role;
    /** @type {bigint|undefined} Enabled timestamp */
    this.enabledAt = undefined;
    /** @type {bigint|undefined} Started timestamp */
    this.startedAt = undefined;
    /** @type {bigint|undefined} Completed timestamp */
    this.completedAt = undefined;
    /** @type {number|undefined} Timeout override */
    this.timeout = options.timeout ?? def.timeout;
    /** @type {Map<string, Object>} Status history for audit */
    this.statusHistory = new Map();
    /** @type {Array<Object>} Transition receipts chain */
    this.receipts = [];
    /** @type {string|null} Last receipt hash for chaining */
    this._lastReceiptHash = null;
    /** @type {string|null} Current state hash */
    this._stateHash = null;
  }

  // ===========================================================================
  // State Queries
  // ===========================================================================

  /**
   * Get current status
   * @returns {string} TaskStatus value
   */
  getStatus() {
    return this.status;
  }

  /**
   * Check if task is enabled (can be started)
   * @returns {boolean}
   */
  isEnabled() {
    return this.status === TaskStatus.ENABLED;
  }

  /**
   * Check if task is active (currently executing)
   * @returns {boolean}
   */
  isActive() {
    return this.status === TaskStatus.ACTIVE;
  }

  /**
   * Check if task is completed successfully
   * @returns {boolean}
   */
  isComplete() {
    return this.status === TaskStatus.COMPLETED;
  }

  /**
   * Check if task is disabled (not yet enabled)
   * @returns {boolean}
   */
  isDisabled() {
    return this.status === TaskStatus.DISABLED;
  }

  /**
   * Check if task is in a terminal state
   * @returns {boolean}
   */
  isTerminal() {
    return [
      TaskStatus.COMPLETED,
      TaskStatus.CANCELLED,
      TaskStatus.FAILED,
      TaskStatus.TIMEOUT,
    ].includes(this.status);
  }

  // ===========================================================================
  // Data Access
  // ===========================================================================

  /**
   * Get input data
   * @returns {Object} Input parameters
   */
  getInputData() {
    return { ...this.inputData };
  }

  /**
   * Set input data
   * @param {Object} data - Input parameters
   * @returns {TaskInstance} this
   */
  setInputData(data) {
    this.inputData = { ...data };
    return this;
  }

  /**
   * Set output data
   * @param {Object} data - Output data
   * @returns {TaskInstance} this
   */
  setOutputData(data) {
    this.outputData = { ...data };
    return this;
  }

  /**
   * Get output data
   * @returns {Object} Output data
   */
  getOutputData() {
    return { ...this.outputData };
  }

  // ===========================================================================
  // Timing
  // ===========================================================================

  /**
   * Get enabled timestamp
   * @returns {bigint|undefined}
   */
  getEnabledAt() {
    return this.enabledAt;
  }

  /**
   * Get started timestamp
   * @returns {bigint|undefined}
   */
  getStartedAt() {
    return this.startedAt;
  }

  /**
   * Get completed timestamp
   * @returns {bigint|undefined}
   */
  getCompletedAt() {
    return this.completedAt;
  }

  /**
   * Get total duration from enabled to completed (or now if not completed)
   * @returns {bigint|null} Duration in nanoseconds
   */
  getDuration() {
    if (!this.enabledAt) return null;
    const end = this.completedAt ?? now();
    return end - this.enabledAt;
  }

  /**
   * Get execution time (from start to completion)
   * @returns {bigint|null} Duration in nanoseconds
   */
  getExecutionTime() {
    if (!this.startedAt) return null;
    const end = this.completedAt ?? now();
    return end - this.startedAt;
  }

  /**
   * Get wait time (from enabled to started)
   * @returns {bigint|null} Duration in nanoseconds
   */
  getWaitTime() {
    if (!this.enabledAt || !this.startedAt) return null;
    return this.startedAt - this.enabledAt;
  }

  // ===========================================================================
  // Receipt Chain
  // ===========================================================================

  /**
   * Get all transition receipts
   * @returns {Object[]}
   */
  getReceipts() {
    return [...this.receipts];
  }

  // ===========================================================================
  // Serialization
  // ===========================================================================

  /**
   * Serialize to JSON-compatible object
   * @returns {Object}
   */
  toJSON() {
    return {
      id: this.id,
      taskDefId: this.taskDefId,
      caseId: this.caseId,
      name: this.name,
      status: this.status,
      inputData: this.inputData,
      outputData: this.outputData,
      assignedResource: this.assignedResource,
      role: this.role,
      enabledAt: this.enabledAt?.toString(),
      startedAt: this.startedAt?.toString(),
      completedAt: this.completedAt?.toString(),
      timeout: this.timeout,
      receipts: this.receipts.map(r => ({
        ...r,
        timestamp: r.timestamp.toString(),
      })),
    };
  }

  /**
   * Create from JSON object
   * @param {Object} json - Serialized task instance
   * @param {TaskDefinition|Object} taskDef - Task definition
   * @returns {TaskInstance}
   */
  static fromJSON(json, taskDef) {
    const instance = new TaskInstance(taskDef, json.caseId, {
      id: json.id,
      inputData: json.inputData,
      assignedResource: json.assignedResource,
      role: json.role,
      timeout: json.timeout,
    });

    instance.status = json.status;
    instance.outputData = json.outputData ?? {};
    instance.enabledAt = json.enabledAt ? BigInt(json.enabledAt) : undefined;
    instance.startedAt = json.startedAt ? BigInt(json.startedAt) : undefined;
    instance.completedAt = json.completedAt ? BigInt(json.completedAt) : undefined;

    // Restore receipts
    if (json.receipts) {
      instance.receipts = json.receipts.map(r => ({
        ...r,
        timestamp: BigInt(r.timestamp),
      }));
      if (instance.receipts.length > 0) {
        instance._lastReceiptHash = instance.receipts[instance.receipts.length - 1].hash;
      }
    }

    return instance;
  }
}

/**
 * Create a task instance from definition
 * @param {TaskDefinition|Object} taskDef - Task definition
 * @param {string} caseId - Case ID
 * @param {Object} [options] - Additional options
 * @returns {TaskInstance}
 */
export function createTaskInstance(taskDef, caseId, options) {
  return new TaskInstance(taskDef, caseId, options);
}
