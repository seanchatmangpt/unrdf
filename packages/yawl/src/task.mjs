/**
 * YAWL Task - State machine and transitions for workflow work items
 *
 * Implements:
 * - TaskDefinition: Workflow definition (template)
 * - TaskInstance: Runtime instance with state machine
 * - State transitions with validation and receipts
 *
 * @module @unrdf/yawl/task
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { now, toISO } from '@unrdf/kgc-4d';
import { SPLIT_TYPE, JOIN_TYPE } from './patterns.mjs';

// =============================================================================
// Task Status Enum
// =============================================================================

/**
 * Task status enumeration
 * @readonly
 * @enum {string}
 */
export const TaskStatus = Object.freeze({
  /** Task is defined but not yet enabled (disabled) */
  DISABLED: 'disabled',
  /** Task is enabled and can be started */
  ENABLED: 'enabled',
  /** Task is currently being executed (active) */
  ACTIVE: 'active',
  /** Task completed successfully */
  COMPLETED: 'completed',
  /** Task was cancelled */
  CANCELLED: 'cancelled',
  /** Task failed due to error */
  FAILED: 'failed',
  /** Task timed out */
  TIMEOUT: 'timeout',
});

// For backwards compatibility
export const TaskStatus_INACTIVE = 'inactive';
export const TaskStatus_RUNNING = 'running';

/**
 * Valid state transitions for task state machine
 * @type {Record<string, string[]>}
 */
export const VALID_TRANSITIONS = Object.freeze({
  [TaskStatus.DISABLED]: [TaskStatus.ENABLED, TaskStatus.CANCELLED],
  [TaskStatus.ENABLED]: [TaskStatus.DISABLED, TaskStatus.ACTIVE, TaskStatus.CANCELLED, TaskStatus.TIMEOUT],
  [TaskStatus.ACTIVE]: [TaskStatus.COMPLETED, TaskStatus.CANCELLED, TaskStatus.FAILED, TaskStatus.TIMEOUT],
  [TaskStatus.COMPLETED]: [], // Terminal state
  [TaskStatus.CANCELLED]: [], // Terminal state
  [TaskStatus.FAILED]: [], // Terminal state
  [TaskStatus.TIMEOUT]: [], // Terminal state
});

// =============================================================================
// Zod Schemas
// =============================================================================

/**
 * Task Definition Schema
 */
export const TaskDefinitionSchema = z.object({
  id: z.string().min(1),
  name: z.string().optional(),
  kind: z.enum(['AtomicTask', 'CompositeTask', 'MultipleInstanceTask', 'EmptyTask']).default('AtomicTask'),
  inputConditions: z.array(z.string()).default([]),
  outputConditions: z.array(z.string()).default([]),
  splitType: z.enum(['sequence', 'and', 'xor', 'or']).default('sequence'),
  joinType: z.enum(['sequence', 'and', 'xor', 'or']).default('sequence'),
  timeout: z.number().positive().optional(),
  requiredRoles: z.array(z.string()).default([]),
  resourcePattern: z.string().optional(),
  cancellationRegion: z.string().optional(),
  cancellationSet: z.array(z.string()).default([]),
  preCondition: z.function().optional(),
  postCondition: z.function().optional(),
  decomposesTo: z.string().optional(),
});

/**
 * Task Instance Schema
 */
export const TaskInstanceSchema = z.object({
  id: z.string().min(1),
  taskDefId: z.string().min(1),
  caseId: z.string().min(1),
  name: z.string().optional(),
  status: z.enum([
    'disabled',
    'enabled',
    'active',
    'completed',
    'cancelled',
    'failed',
    'timeout',
    // Legacy compatibility
    'inactive',
    'running',
  ]).default('disabled'),
  inputData: z.record(z.unknown()).default({}),
  outputData: z.record(z.unknown()).default({}),
  assignedResource: z.string().optional(),
  role: z.string().optional(),
  enabledAt: z.bigint().optional(),
  startedAt: z.bigint().optional(),
  completedAt: z.bigint().optional(),
  timeout: z.number().positive().optional(),
});

/**
 * Transition Receipt Schema
 */
export const TransitionReceiptSchema = z.object({
  id: z.string().min(1),
  taskInstanceId: z.string().min(1),
  caseId: z.string().min(1),
  action: z.enum(['enable', 'disable', 'start', 'complete', 'cancel', 'fail', 'timeout']),
  timestamp: z.bigint(),
  beforeStatus: z.string(),
  afterStatus: z.string(),
  beforeHash: z.string().min(1),
  afterHash: z.string().min(1),
  previousReceiptHash: z.string().optional(),
  justification: z.object({
    hookId: z.string().optional(),
    reason: z.string(),
    validated: z.boolean().default(true),
  }),
  actor: z.string().optional(),
  inputData: z.record(z.unknown()).optional(),
  outputData: z.record(z.unknown()).optional(),
});

// =============================================================================
// TaskDefinition Class
// =============================================================================

/**
 * Represents a task definition (template) in a workflow specification
 */
export class TaskDefinition {
  /**
   * @param {Object} spec - Task specification
   */
  constructor(spec) {
    const validated = TaskDefinitionSchema.parse(spec);

    /** @type {string} Unique task ID */
    this.id = validated.id;
    /** @type {string} Human-readable name */
    this.name = validated.name ?? validated.id;
    /** @type {string} Task kind */
    this.kind = validated.kind;
    /** @type {string[]} Input condition IDs */
    this.inputConditions = validated.inputConditions;
    /** @type {string[]} Output condition IDs */
    this.outputConditions = validated.outputConditions;
    /** @type {string} Split type for outgoing flows */
    this.splitType = validated.splitType;
    /** @type {string} Join type for incoming flows */
    this.joinType = validated.joinType;
    /** @type {number|undefined} Timeout in milliseconds */
    this.timeout = validated.timeout;
    /** @type {string[]} Required role IDs for execution */
    this.requiredRoles = validated.requiredRoles;
    /** @type {string|undefined} Resource allocation pattern */
    this.resourcePattern = validated.resourcePattern;
    /** @type {string|undefined} Cancellation region ID */
    this.cancellationRegion = validated.cancellationRegion;
    /** @type {string[]} Tasks to cancel when this task completes */
    this.cancellationSet = validated.cancellationSet;
    /** @type {Function|undefined} Pre-condition hook */
    this.preCondition = validated.preCondition;
    /** @type {Function|undefined} Post-condition hook */
    this.postCondition = validated.postCondition;
    /** @type {string|undefined} Sub-workflow ID for composite tasks */
    this.decomposesTo = validated.decomposesTo;
  }

  /**
   * Get input conditions
   * @returns {string[]} Condition IDs that must be satisfied
   */
  getInputConditions() {
    return [...this.inputConditions];
  }

  /**
   * Get output conditions
   * @returns {string[]} Condition IDs produced on completion
   */
  getOutputConditions() {
    return [...this.outputConditions];
  }

  /**
   * Get split type for outgoing flows
   * @returns {string} AND/XOR/OR/sequence
   */
  getSplitType() {
    return this.splitType;
  }

  /**
   * Get join type for incoming flows
   * @returns {string} AND/XOR/OR/sequence
   */
  getJoinType() {
    return this.joinType;
  }

  /**
   * Get timeout in milliseconds
   * @returns {number|undefined}
   */
  getTimeout() {
    return this.timeout;
  }

  /**
   * Get required role IDs
   * @returns {string[]}
   */
  getRequiredRoles() {
    return [...this.requiredRoles];
  }

  /**
   * Check if this is an atomic task
   * @returns {boolean}
   */
  isAtomic() {
    return this.kind === 'AtomicTask';
  }

  /**
   * Check if this is a composite task
   * @returns {boolean}
   */
  isComposite() {
    return this.kind === 'CompositeTask';
  }

  /**
   * Check if this is an empty task (routing only)
   * @returns {boolean}
   */
  isEmpty() {
    return this.kind === 'EmptyTask';
  }

  /**
   * Validate pre-conditions for task enablement
   * @param {Object} context - Validation context
   * @returns {Promise<{valid: boolean, reason?: string}>}
   */
  async validatePreCondition(context) {
    if (!this.preCondition) {
      return { valid: true };
    }
    try {
      const result = await this.preCondition(context);
      return typeof result === 'boolean'
        ? { valid: result, reason: result ? undefined : 'Pre-condition failed' }
        : result;
    } catch (error) {
      return { valid: false, reason: `Pre-condition error: ${error.message}` };
    }
  }

  /**
   * Validate post-conditions for task completion
   * @param {Object} context - Validation context with output data
   * @returns {Promise<{valid: boolean, reason?: string}>}
   */
  async validatePostCondition(context) {
    if (!this.postCondition) {
      return { valid: true };
    }
    try {
      const result = await this.postCondition(context);
      return typeof result === 'boolean'
        ? { valid: result, reason: result ? undefined : 'Post-condition failed' }
        : result;
    } catch (error) {
      return { valid: false, reason: `Post-condition error: ${error.message}` };
    }
  }

  /**
   * Serialize to JSON
   * @returns {Object}
   */
  toJSON() {
    return {
      id: this.id,
      name: this.name,
      kind: this.kind,
      inputConditions: this.inputConditions,
      outputConditions: this.outputConditions,
      splitType: this.splitType,
      joinType: this.joinType,
      timeout: this.timeout,
      requiredRoles: this.requiredRoles,
      resourcePattern: this.resourcePattern,
      cancellationRegion: this.cancellationRegion,
      cancellationSet: this.cancellationSet,
      decomposesTo: this.decomposesTo,
    };
  }

  /**
   * Create from JSON
   * @param {Object} json
   * @returns {TaskDefinition}
   */
  static fromJSON(json) {
    return new TaskDefinition(json);
  }
}

// =============================================================================
// TaskInstance Class
// =============================================================================

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
  // State Transitions
  // ===========================================================================

  /**
   * Validate a state transition
   * @param {string} fromStatus - Current status
   * @param {string} toStatus - Target status
   * @returns {{valid: boolean, reason?: string}}
   */
  validateTransition(fromStatus, toStatus) {
    const validTargets = VALID_TRANSITIONS[fromStatus] ?? [];
    if (!validTargets.includes(toStatus)) {
      return {
        valid: false,
        reason: `Invalid transition from ${fromStatus} to ${toStatus}. Valid targets: ${validTargets.join(', ') || 'none'}`,
      };
    }
    return { valid: true };
  }

  /**
   * Compute state hash for receipts
   * @returns {Promise<string>}
   */
  async computeStateHash() {
    const state = {
      id: this.id,
      taskDefId: this.taskDefId,
      caseId: this.caseId,
      status: this.status,
      inputData: this.inputData,
      outputData: this.outputData,
      enabledAt: this.enabledAt?.toString(),
      startedAt: this.startedAt?.toString(),
      completedAt: this.completedAt?.toString(),
    };
    return blake3(JSON.stringify(state));
  }

  /**
   * Generate a transition receipt
   * @param {string} action - Transition action
   * @param {string} beforeStatus - Status before transition
   * @param {string} beforeHash - State hash before transition
   * @param {Object} [justification] - Hook validation justification
   * @returns {Promise<Object>} Receipt object
   */
  async generateReceipt(action, beforeStatus, beforeHash, justification = {}) {
    const afterHash = await this.computeStateHash();

    const receipt = {
      id: `receipt-${this.id}-${action}-${Date.now()}`,
      taskInstanceId: this.id,
      caseId: this.caseId,
      action,
      timestamp: now(),
      beforeStatus,
      afterStatus: this.status,
      beforeHash,
      afterHash,
      previousReceiptHash: this._lastReceiptHash,
      justification: {
        hookId: justification.hookId,
        reason: justification.reason ?? `Transition ${action} executed`,
        validated: justification.validated ?? true,
      },
      actor: justification.actor,
      inputData: action === 'start' ? this.inputData : undefined,
      outputData: action === 'complete' ? this.outputData : undefined,
    };

    // Compute receipt hash for chaining
    const receiptHash = await blake3(JSON.stringify({
      id: receipt.id,
      taskInstanceId: receipt.taskInstanceId,
      caseId: receipt.caseId,
      action: receipt.action,
      timestamp: receipt.timestamp.toString(),
      beforeHash: receipt.beforeHash,
      afterHash: receipt.afterHash,
      previousReceiptHash: receipt.previousReceiptHash,
    }));

    receipt.hash = receiptHash;
    this._lastReceiptHash = receiptHash;
    this.receipts.push(receipt);

    return receipt;
  }

  /**
   * Enable this task (disabled -> enabled)
   * @param {Object} [options] - Enable options
   * @param {Object} [options.justification] - Hook validation info
   * @returns {Promise<{task: TaskInstance, receipt: Object}>}
   */
  async enable(options = {}) {
    const validation = this.validateTransition(this.status, TaskStatus.ENABLED);
    if (!validation.valid) {
      throw new Error(`Cannot enable task ${this.id}: ${validation.reason}`);
    }

    // Validate pre-conditions from task definition
    const preCheck = await this.taskDefinition.validatePreCondition({
      taskInstance: this,
      inputData: this.inputData,
    });
    if (!preCheck.valid) {
      throw new Error(`Cannot enable task ${this.id}: ${preCheck.reason}`);
    }

    const beforeStatus = this.status;
    const beforeHash = await this.computeStateHash();

    this.status = TaskStatus.ENABLED;
    this.enabledAt = now();
    this.statusHistory.set(`enabled:${this.enabledAt}`, {
      status: TaskStatus.ENABLED,
      timestamp: this.enabledAt,
    });

    const receipt = await this.generateReceipt('enable', beforeStatus, beforeHash, {
      ...options.justification,
      reason: options.justification?.reason ?? 'Task enabled - pre-conditions satisfied',
    });

    return { task: this, receipt };
  }

  /**
   * Disable this task (enabled -> disabled)
   * @param {Object} [options] - Disable options
   * @returns {Promise<{task: TaskInstance, receipt: Object}>}
   */
  async disable(options = {}) {
    const validation = this.validateTransition(this.status, TaskStatus.DISABLED);
    if (!validation.valid) {
      throw new Error(`Cannot disable task ${this.id}: ${validation.reason}`);
    }

    const beforeStatus = this.status;
    const beforeHash = await this.computeStateHash();

    this.status = TaskStatus.DISABLED;
    this.statusHistory.set(`disabled:${now()}`, {
      status: TaskStatus.DISABLED,
      timestamp: now(),
    });

    const receipt = await this.generateReceipt('disable', beforeStatus, beforeHash, {
      ...options.justification,
      reason: options.justification?.reason ?? 'Task disabled',
    });

    return { task: this, receipt };
  }

  /**
   * Start this task (enabled -> active)
   * @param {string} [resourceId] - Resource performing the task
   * @param {Object} [options] - Start options
   * @returns {Promise<{task: TaskInstance, receipt: Object}>}
   */
  async start(resourceId, options = {}) {
    const validation = this.validateTransition(this.status, TaskStatus.ACTIVE);
    if (!validation.valid) {
      throw new Error(`Cannot start task ${this.id}: ${validation.reason}`);
    }

    const beforeStatus = this.status;
    const beforeHash = await this.computeStateHash();

    this.status = TaskStatus.ACTIVE;
    this.startedAt = now();
    if (resourceId) {
      this.assignedResource = resourceId;
    }
    this.statusHistory.set(`active:${this.startedAt}`, {
      status: TaskStatus.ACTIVE,
      timestamp: this.startedAt,
      resource: resourceId,
    });

    const receipt = await this.generateReceipt('start', beforeStatus, beforeHash, {
      ...options.justification,
      reason: options.justification?.reason ?? `Task started by ${resourceId ?? 'system'}`,
      actor: resourceId,
    });

    return { task: this, receipt };
  }

  /**
   * Complete this task (active -> completed)
   * @param {Object} [outputData] - Output data from execution
   * @param {Object} [options] - Complete options
   * @returns {Promise<{task: TaskInstance, receipt: Object}>}
   */
  async complete(outputData = {}, options = {}) {
    const validation = this.validateTransition(this.status, TaskStatus.COMPLETED);
    if (!validation.valid) {
      throw new Error(`Cannot complete task ${this.id}: ${validation.reason}`);
    }

    // Validate post-conditions from task definition
    const postCheck = await this.taskDefinition.validatePostCondition({
      taskInstance: this,
      inputData: this.inputData,
      outputData,
    });
    if (!postCheck.valid) {
      throw new Error(`Cannot complete task ${this.id}: ${postCheck.reason}`);
    }

    const beforeStatus = this.status;
    const beforeHash = await this.computeStateHash();

    this.status = TaskStatus.COMPLETED;
    this.completedAt = now();
    this.outputData = outputData;
    this.statusHistory.set(`completed:${this.completedAt}`, {
      status: TaskStatus.COMPLETED,
      timestamp: this.completedAt,
      outputData,
    });

    const receipt = await this.generateReceipt('complete', beforeStatus, beforeHash, {
      ...options.justification,
      reason: options.justification?.reason ?? 'Task completed - post-conditions satisfied',
    });

    return { task: this, receipt };
  }

  /**
   * Cancel this task (any non-terminal -> cancelled)
   * @param {string} [reason] - Cancellation reason
   * @param {Object} [options] - Cancel options
   * @returns {Promise<{task: TaskInstance, receipt: Object}>}
   */
  async cancel(reason, options = {}) {
    const validation = this.validateTransition(this.status, TaskStatus.CANCELLED);
    if (!validation.valid) {
      throw new Error(`Cannot cancel task ${this.id}: ${validation.reason}`);
    }

    const beforeStatus = this.status;
    const beforeHash = await this.computeStateHash();

    this.status = TaskStatus.CANCELLED;
    this.completedAt = now();
    this.outputData = { cancelled: true, reason };
    this.statusHistory.set(`cancelled:${this.completedAt}`, {
      status: TaskStatus.CANCELLED,
      timestamp: this.completedAt,
      reason,
    });

    const receipt = await this.generateReceipt('cancel', beforeStatus, beforeHash, {
      ...options.justification,
      reason: options.justification?.reason ?? `Task cancelled: ${reason ?? 'no reason provided'}`,
    });

    return { task: this, receipt };
  }

  /**
   * Fail this task (active -> failed)
   * @param {Error|string} error - Error that caused failure
   * @param {Object} [options] - Fail options
   * @returns {Promise<{task: TaskInstance, receipt: Object}>}
   */
  async fail(error, options = {}) {
    const validation = this.validateTransition(this.status, TaskStatus.FAILED);
    if (!validation.valid) {
      throw new Error(`Cannot fail task ${this.id}: ${validation.reason}`);
    }

    const beforeStatus = this.status;
    const beforeHash = await this.computeStateHash();

    const errorMessage = error instanceof Error ? error.message : error;
    this.status = TaskStatus.FAILED;
    this.completedAt = now();
    this.outputData = {
      failed: true,
      error: errorMessage,
      stack: error instanceof Error ? error.stack : undefined,
    };
    this.statusHistory.set(`failed:${this.completedAt}`, {
      status: TaskStatus.FAILED,
      timestamp: this.completedAt,
      error: errorMessage,
    });

    const receipt = await this.generateReceipt('fail', beforeStatus, beforeHash, {
      ...options.justification,
      reason: options.justification?.reason ?? `Task failed: ${errorMessage}`,
    });

    return { task: this, receipt };
  }

  /**
   * Mark task as timed out (enabled/active -> timeout)
   * @param {Object} [options] - Timeout options
   * @returns {Promise<{task: TaskInstance, receipt: Object}>}
   */
  async timedOut(options = {}) {
    const validation = this.validateTransition(this.status, TaskStatus.TIMEOUT);
    if (!validation.valid) {
      throw new Error(`Cannot timeout task ${this.id}: ${validation.reason}`);
    }

    const beforeStatus = this.status;
    const beforeHash = await this.computeStateHash();

    this.status = TaskStatus.TIMEOUT;
    this.completedAt = now();
    this.outputData = {
      timedOut: true,
      timeout: this.timeout,
      duration: this.getDuration()?.toString(),
    };
    this.statusHistory.set(`timeout:${this.completedAt}`, {
      status: TaskStatus.TIMEOUT,
      timestamp: this.completedAt,
    });

    const receipt = await this.generateReceipt('timeout', beforeStatus, beforeHash, {
      ...options.justification,
      reason: options.justification?.reason ?? `Task timed out after ${this.timeout}ms`,
    });

    return { task: this, receipt };
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

  /**
   * Verify receipt chain integrity
   * @returns {Promise<{valid: boolean, errors: string[]}>}
   */
  async verifyReceiptChain() {
    const errors = [];
    let previousHash = null;

    for (let i = 0; i < this.receipts.length; i++) {
      const receipt = this.receipts[i];

      // Verify chain link
      if (receipt.previousReceiptHash !== previousHash) {
        errors.push(
          `Receipt ${i} chain broken: expected previousHash ${previousHash}, got ${receipt.previousReceiptHash}`
        );
      }

      // Verify receipt hash
      const computedHash = await blake3(JSON.stringify({
        id: receipt.id,
        taskInstanceId: receipt.taskInstanceId,
        caseId: receipt.caseId,
        action: receipt.action,
        timestamp: receipt.timestamp.toString(),
        beforeHash: receipt.beforeHash,
        afterHash: receipt.afterHash,
        previousReceiptHash: receipt.previousReceiptHash,
      }));

      if (computedHash !== receipt.hash) {
        errors.push(`Receipt ${i} hash mismatch: computed ${computedHash}, stored ${receipt.hash}`);
      }

      previousHash = receipt.hash;
    }

    return {
      valid: errors.length === 0,
      errors,
    };
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

// =============================================================================
// Legacy YawlTask Class (Backwards Compatibility)
// =============================================================================

/**
 * Legacy task data schema (backwards compatible)
 */
const TaskDataSchema = z.object({
  id: z.string().min(1),
  name: z.string().optional(),
  caseId: z.string().optional(),
  status: z.enum([
    'inactive',
    'enabled',
    'running',
    'completed',
    'cancelled',
    'failed',
    'timeout',
    // New status values
    'disabled',
    'active',
  ]).default('inactive'),
  input: z.record(z.unknown()).optional(),
  output: z.record(z.unknown()).optional(),
  assignedResource: z.string().optional(),
  role: z.string().optional(),
  enabledAt: z.bigint().optional(),
  startedAt: z.bigint().optional(),
  completedAt: z.bigint().optional(),
  timeout: z.number().positive().optional(),
  cancellationRegion: z.string().optional(),
});

/**
 * Legacy YawlTask class for backwards compatibility
 * @deprecated Use TaskInstance instead
 */
export class YawlTask {
  /**
   * @param {Object} data - Task data
   */
  constructor(data) {
    const validated = TaskDataSchema.parse(data);
    this.id = validated.id;
    this.name = validated.name ?? validated.id;
    this.caseId = validated.caseId;
    this.status = validated.status;
    this.input = validated.input ?? {};
    this.output = validated.output ?? {};
    this.assignedResource = validated.assignedResource;
    this.role = validated.role;
    this.enabledAt = validated.enabledAt;
    this.startedAt = validated.startedAt;
    this.completedAt = validated.completedAt;
    this.timeout = validated.timeout;
    this.cancellationRegion = validated.cancellationRegion;
    /** @type {Map<string, bigint>} Status history for time-travel */
    this.statusHistory = new Map();
  }

  /**
   * Enable this task (make it available for execution)
   * @returns {YawlTask} this
   */
  enable() {
    if (this.status !== 'inactive' && this.status !== TaskStatus.DISABLED) {
      throw new Error(`Cannot enable task ${this.id}: current status is ${this.status}`);
    }
    this.status = TaskStatus.ENABLED;
    this.enabledAt = now();
    this.statusHistory.set(`enabled:${this.enabledAt}`, this.enabledAt);
    return this;
  }

  /**
   * Start executing this task
   * @param {string} [resourceId] - Resource performing the task
   * @returns {YawlTask} this
   */
  start(resourceId) {
    if (this.status !== TaskStatus.ENABLED) {
      throw new Error(`Cannot start task ${this.id}: current status is ${this.status}`);
    }
    this.status = 'running';
    this.startedAt = now();
    this.assignedResource = resourceId;
    this.statusHistory.set(`running:${this.startedAt}`, this.startedAt);
    return this;
  }

  /**
   * Complete this task with output data
   * @param {Object} [output] - Output data from task execution
   * @returns {YawlTask} this
   */
  complete(output = {}) {
    if (this.status !== 'running' && this.status !== TaskStatus.ACTIVE) {
      throw new Error(`Cannot complete task ${this.id}: current status is ${this.status}`);
    }
    this.status = TaskStatus.COMPLETED;
    this.completedAt = now();
    this.output = output;
    this.statusHistory.set(`completed:${this.completedAt}`, this.completedAt);
    return this;
  }

  /**
   * Cancel this task
   * @param {string} [reason] - Cancellation reason
   * @returns {YawlTask} this
   */
  cancel(reason) {
    if (this.status === TaskStatus.COMPLETED || this.status === TaskStatus.CANCELLED) {
      throw new Error(`Cannot cancel task ${this.id}: already ${this.status}`);
    }
    this.status = TaskStatus.CANCELLED;
    this.completedAt = now();
    this.output = { cancelled: true, reason };
    this.statusHistory.set(`cancelled:${this.completedAt}`, this.completedAt);
    return this;
  }

  /**
   * Fail this task with error
   * @param {Error|string} error - Error that caused failure
   * @returns {YawlTask} this
   */
  fail(error) {
    if (this.status === TaskStatus.COMPLETED || this.status === TaskStatus.CANCELLED) {
      throw new Error(`Cannot fail task ${this.id}: already ${this.status}`);
    }
    this.status = TaskStatus.FAILED;
    this.completedAt = now();
    this.output = {
      failed: true,
      error: error instanceof Error ? error.message : error,
    };
    this.statusHistory.set(`failed:${this.completedAt}`, this.completedAt);
    return this;
  }

  /**
   * Mark task as timed out
   * @returns {YawlTask} this
   */
  timedOut() {
    if (this.status !== 'running' && this.status !== TaskStatus.ENABLED && this.status !== TaskStatus.ACTIVE) {
      throw new Error(`Cannot timeout task ${this.id}: current status is ${this.status}`);
    }
    this.status = TaskStatus.TIMEOUT;
    this.completedAt = now();
    this.output = { timedOut: true };
    this.statusHistory.set(`timeout:${this.completedAt}`, this.completedAt);
    return this;
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

  /**
   * Get duration from enabled to completed (or now if not completed)
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
   * Serialize to JSON-compatible object
   * @returns {Object}
   */
  toJSON() {
    return {
      id: this.id,
      name: this.name,
      caseId: this.caseId,
      status: this.status,
      input: this.input,
      output: this.output,
      assignedResource: this.assignedResource,
      role: this.role,
      enabledAt: this.enabledAt?.toString(),
      startedAt: this.startedAt?.toString(),
      completedAt: this.completedAt?.toString(),
      timeout: this.timeout,
      cancellationRegion: this.cancellationRegion,
    };
  }

  /**
   * Create from JSON object
   * @param {Object} json
   * @returns {YawlTask}
   */
  static fromJSON(json) {
    return new YawlTask({
      ...json,
      enabledAt: json.enabledAt ? BigInt(json.enabledAt) : undefined,
      startedAt: json.startedAt ? BigInt(json.startedAt) : undefined,
      completedAt: json.completedAt ? BigInt(json.completedAt) : undefined,
    });
  }
}

// =============================================================================
// Factory Functions
// =============================================================================

/**
 * Create a task definition from spec
 * @param {Object} spec - Task specification
 * @returns {TaskDefinition}
 */
export function createTaskDefinition(spec) {
  return new TaskDefinition(spec);
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

/**
 * Build a transition receipt manually
 * @param {Object} params - Receipt parameters
 * @returns {Promise<Object>}
 */
export async function buildTransitionReceipt({
  taskInstanceId,
  caseId,
  action,
  beforeStatus,
  afterStatus,
  beforeState,
  afterState,
  previousReceiptHash,
  justification = {},
  actor,
  inputData,
  outputData,
}) {
  const beforeHash = await blake3(JSON.stringify(beforeState));
  const afterHash = await blake3(JSON.stringify(afterState));
  const timestamp = now();

  const receipt = {
    id: `receipt-${taskInstanceId}-${action}-${Date.now()}`,
    taskInstanceId,
    caseId,
    action,
    timestamp,
    beforeStatus,
    afterStatus,
    beforeHash,
    afterHash,
    previousReceiptHash,
    justification: {
      hookId: justification.hookId,
      reason: justification.reason ?? `Transition ${action}`,
      validated: justification.validated ?? true,
    },
    actor,
    inputData,
    outputData,
  };

  // Compute receipt hash
  receipt.hash = await blake3(JSON.stringify({
    id: receipt.id,
    taskInstanceId: receipt.taskInstanceId,
    caseId: receipt.caseId,
    action: receipt.action,
    timestamp: receipt.timestamp.toString(),
    beforeHash: receipt.beforeHash,
    afterHash: receipt.afterHash,
    previousReceiptHash: receipt.previousReceiptHash,
  }));

  return receipt;
}
