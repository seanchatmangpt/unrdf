/**
 * YAWL Task RDF - Serialization and legacy compatibility
 *
 * Contains:
 * - JSON serialization (toJSON/fromJSON)
 * - Receipt building utilities
 * - Legacy YawlTask class for backwards compatibility
 *
 * @module @unrdf/yawl/task-rdf
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { now } from '@unrdf/kgc-4d';
import { TaskStatus, TaskInstance, TaskDefinition } from './task-core.mjs';

// =============================================================================
// Serialization Methods
// =============================================================================

/**
 * Serialize TaskInstance to JSON-compatible object
 * @param {import('./task-core.mjs').TaskInstance} taskInstance - Task instance
 * @returns {Object}
 */
export function serializeTaskInstance(taskInstance) {
  return {
    id: taskInstance.id,
    taskDefId: taskInstance.taskDefId,
    caseId: taskInstance.caseId,
    name: taskInstance.name,
    status: taskInstance.status,
    inputData: taskInstance.inputData,
    outputData: taskInstance.outputData,
    assignedResource: taskInstance.assignedResource,
    role: taskInstance.role,
    enabledAt: taskInstance.enabledAt?.toString(),
    startedAt: taskInstance.startedAt?.toString(),
    completedAt: taskInstance.completedAt?.toString(),
    timeout: taskInstance.timeout,
    receipts: taskInstance.receipts.map(r => ({
      ...r,
      timestamp: r.timestamp.toString(),
    })),
  };
}

/**
 * Deserialize TaskInstance from JSON object
 * @param {Object} json - Serialized task instance
 * @param {import('./task-core.mjs').TaskDefinition|Object} taskDef - Task definition
 * @returns {import('./task-core.mjs').TaskInstance}
 */
export function deserializeTaskInstance(json, taskDef) {
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

// =============================================================================
// Extend TaskInstance with serialization methods
// =============================================================================

/**
 * Add serialization methods to TaskInstance prototype
 * @param {import('./task-core.mjs').TaskInstance} TaskInstanceClass
 */
export function extendTaskInstance(TaskInstanceClass) {
  /**
   * Serialize to JSON-compatible object
   * @returns {Object}
   */
  TaskInstanceClass.prototype.toJSON = function() {
    return serializeTaskInstance(this);
  };

  /**
   * Create from JSON object
   * @param {Object} json - Serialized task instance
   * @param {import('./task-core.mjs').TaskDefinition|Object} taskDef - Task definition
   * @returns {import('./task-core.mjs').TaskInstance}
   */
  TaskInstanceClass.fromJSON = function(json, taskDef) {
    return deserializeTaskInstance(json, taskDef);
  };
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
    this.status = TaskStatus.ACTIVE;
    this.startedAt = now();
    this.assignedResource = resourceId;
    this.statusHistory.set(`active:${this.startedAt}`, this.startedAt);
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
