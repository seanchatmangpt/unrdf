/**
 * YAWL Task Legacy - Backwards compatibility for YawlTask
 * @module @unrdf/yawl/task-legacy
 * @deprecated Use TaskInstance instead
 */

import { now } from '@unrdf/kgc-4d';
import { TaskStatus, TaskDataSchema } from './task-definitions.mjs';

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
