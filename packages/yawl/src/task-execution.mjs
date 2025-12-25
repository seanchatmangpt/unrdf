/**
 * YAWL Task Execution - State transitions and legacy compatibility
 * @module @unrdf/yawl/task-execution
 */

import { blake3 } from 'hash-wasm';
import { now } from '@unrdf/kgc-4d';
import { TaskStatus, VALID_TRANSITIONS } from './task-definitions.mjs';
import { TaskInstance } from './task-instance.mjs';

// =============================================================================
// TaskInstance State Transition Methods (Prototype Extension)
// =============================================================================

/**
 * Validate a state transition
 * @param {string} fromStatus - Current status
 * @param {string} toStatus - Target status
 * @returns {{valid: boolean, reason?: string}}
 */
TaskInstance.prototype.validateTransition = function(fromStatus, toStatus) {
  const validTargets = VALID_TRANSITIONS[fromStatus] ?? [];
  if (!validTargets.includes(toStatus)) {
    return {
      valid: false,
      reason: `Invalid transition from ${fromStatus} to ${toStatus}. Valid targets: ${validTargets.join(', ') || 'none'}`,
    };
  }
  return { valid: true };
};

/**
 * Compute state hash for receipts
 * @returns {Promise<string>}
 */
TaskInstance.prototype.computeStateHash = async function() {
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
};

/**
 * Generate a transition receipt
 * @param {string} action - Transition action
 * @param {string} beforeStatus - Status before transition
 * @param {string} beforeHash - State hash before transition
 * @param {Object} [justification] - Hook validation justification
 * @returns {Promise<Object>} Receipt object
 */
TaskInstance.prototype.generateReceipt = async function(action, beforeStatus, beforeHash, justification = {}) {
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
};

/**
 * Enable this task (disabled -> enabled)
 * @param {Object} [options] - Enable options
 * @param {Object} [options.justification] - Hook validation info
 * @returns {Promise<{task: TaskInstance, receipt: Object}>}
 */
TaskInstance.prototype.enable = async function(options = {}) {
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
};

/**
 * Disable this task (enabled -> disabled)
 * @param {Object} [options] - Disable options
 * @returns {Promise<{task: TaskInstance, receipt: Object}>}
 */
TaskInstance.prototype.disable = async function(options = {}) {
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
};

/**
 * Start this task (enabled -> active)
 * @param {string} [resourceId] - Resource performing the task
 * @param {Object} [options] - Start options
 * @returns {Promise<{task: TaskInstance, receipt: Object}>}
 */
TaskInstance.prototype.start = async function(resourceId, options = {}) {
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
};

/**
 * Complete this task (active -> completed)
 * @param {Object} [outputData] - Output data from execution
 * @param {Object} [options] - Complete options
 * @returns {Promise<{task: TaskInstance, receipt: Object}>}
 */
TaskInstance.prototype.complete = async function(outputData = {}, options = {}) {
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
};

/**
 * Cancel this task (any non-terminal -> cancelled)
 * @param {string} [reason] - Cancellation reason
 * @param {Object} [options] - Cancel options
 * @returns {Promise<{task: TaskInstance, receipt: Object}>}
 */
TaskInstance.prototype.cancel = async function(reason, options = {}) {
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
};

/**
 * Fail this task (active -> failed)
 * @param {Error|string} error - Error that caused failure
 * @param {Object} [options] - Fail options
 * @returns {Promise<{task: TaskInstance, receipt: Object}>}
 */
TaskInstance.prototype.fail = async function(error, options = {}) {
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
};

/**
 * Mark task as timed out (enabled/active -> timeout)
 * @param {Object} [options] - Timeout options
 * @returns {Promise<{task: TaskInstance, receipt: Object}>}
 */
TaskInstance.prototype.timedOut = async function(options = {}) {
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
};

/**
 * Verify receipt chain integrity
 * @returns {Promise<{valid: boolean, errors: string[]}>}
 */
TaskInstance.prototype.verifyReceiptChain = async function() {
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
};

// =============================================================================
// Factory Functions
// =============================================================================

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
