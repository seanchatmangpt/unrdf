/**
 * YAWL Task Definitions - Schemas, constants, and TaskDefinition class
 * @module @unrdf/yawl/task-definitions
 */

import { z } from 'zod';

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

/**
 * Legacy task data schema (backwards compatible)
 */
export const TaskDataSchema = z.object({
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
