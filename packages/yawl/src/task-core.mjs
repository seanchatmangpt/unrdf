/**
 * YAWL Task Core - Base classes and schemas
 *
 * Contains:
 * - Task status enums and constants
 * - Zod validation schemas
 * - TaskDefinition class (workflow template)
 * - TaskInstance class structure (runtime state)
 *
 * @module @unrdf/yawl/task-core
 */

import { z } from 'zod';
import { now } from '@unrdf/kgc-4d';
import { TaskExecutionError } from '../errors.mjs';

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
// TaskInstance Class - Core Structure
// =============================================================================

/**
 * Represents a runtime task instance in a workflow case
 * Implements full state machine with validated transitions and receipts
 *
 * This file contains the core structure. Execution methods are in task-execution.mjs
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
}
