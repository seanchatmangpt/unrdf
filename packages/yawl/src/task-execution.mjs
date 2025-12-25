/**
 * YAWL Task Execution - State transitions and receipt generation
 *
 * Contains:
 * - Receipt generation logic
 * - State transition methods (enable, disable, start, complete, cancel, fail, timeout)
 * - Execution state management
 *
 * @module @unrdf/yawl/task-execution
 */

import { blake3 } from 'hash-wasm';
import { now } from '@unrdf/kgc-4d';
import { TaskStatus } from './task-core.mjs';
import { validateTransition, computeStateHash } from './task-validation.mjs';

// =============================================================================
// Receipt Generation
// =============================================================================

/**
 * Generate a transition receipt
 * @param {import('./task-core.mjs').TaskInstance} taskInstance - Task instance
 * @param {string} action - Transition action
 * @param {string} beforeStatus - Status before transition
 * @param {string} beforeHash - State hash before transition
 * @param {Object} [justification] - Hook validation justification
 * @returns {Promise<Object>} Receipt object
 */
export async function generateReceipt(taskInstance, action, beforeStatus, beforeHash, justification = {}) {
  const afterHash = await computeStateHash(taskInstance);

  const receipt = {
    id: `receipt-${taskInstance.id}-${action}-${Date.now()}`,
    taskInstanceId: taskInstance.id,
    caseId: taskInstance.caseId,
    action,
    timestamp: now(),
    beforeStatus,
    afterStatus: taskInstance.status,
    beforeHash,
    afterHash,
    previousReceiptHash: taskInstance._lastReceiptHash,
    justification: {
      hookId: justification.hookId,
      reason: justification.reason ?? `Transition ${action} executed`,
      validated: justification.validated ?? true,
    },
    actor: justification.actor,
    inputData: action === 'start' ? taskInstance.inputData : undefined,
    outputData: action === 'complete' ? taskInstance.outputData : undefined,
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
  taskInstance._lastReceiptHash = receiptHash;
  taskInstance.receipts.push(receipt);

  return receipt;
}

// =============================================================================
// State Transition Functions
// =============================================================================

/**
 * Enable a task (disabled -> enabled)
 * @param {import('./task-core.mjs').TaskInstance} taskInstance - Task instance
 * @param {Object} [options] - Enable options
 * @param {Object} [options.justification] - Hook validation info
 * @returns {Promise<{task: import('./task-core.mjs').TaskInstance, receipt: Object}>}
 */
export async function enableTask(taskInstance, options = {}) {
  const validation = validateTransition(taskInstance.status, TaskStatus.ENABLED);
  if (!validation.valid) {
    throw new Error(`Cannot enable task ${taskInstance.id}: ${validation.reason}`);
  }

  // Validate pre-conditions from task definition
  const preCheck = await taskInstance.taskDefinition.validatePreCondition({
    taskInstance,
    inputData: taskInstance.inputData,
  });
  if (!preCheck.valid) {
    throw new Error(`Cannot enable task ${taskInstance.id}: ${preCheck.reason}`);
  }

  const beforeStatus = taskInstance.status;
  const beforeHash = await computeStateHash(taskInstance);

  taskInstance.status = TaskStatus.ENABLED;
  taskInstance.enabledAt = now();
  taskInstance.statusHistory.set(`enabled:${taskInstance.enabledAt}`, {
    status: TaskStatus.ENABLED,
    timestamp: taskInstance.enabledAt,
  });

  const receipt = await generateReceipt(taskInstance, 'enable', beforeStatus, beforeHash, {
    ...options.justification,
    reason: options.justification?.reason ?? 'Task enabled - pre-conditions satisfied',
  });

  return { task: taskInstance, receipt };
}

/**
 * Disable a task (enabled -> disabled)
 * @param {import('./task-core.mjs').TaskInstance} taskInstance - Task instance
 * @param {Object} [options] - Disable options
 * @returns {Promise<{task: import('./task-core.mjs').TaskInstance, receipt: Object}>}
 */
export async function disableTask(taskInstance, options = {}) {
  const validation = validateTransition(taskInstance.status, TaskStatus.DISABLED);
  if (!validation.valid) {
    throw new Error(`Cannot disable task ${taskInstance.id}: ${validation.reason}`);
  }

  const beforeStatus = taskInstance.status;
  const beforeHash = await computeStateHash(taskInstance);

  taskInstance.status = TaskStatus.DISABLED;
  taskInstance.statusHistory.set(`disabled:${now()}`, {
    status: TaskStatus.DISABLED,
    timestamp: now(),
  });

  const receipt = await generateReceipt(taskInstance, 'disable', beforeStatus, beforeHash, {
    ...options.justification,
    reason: options.justification?.reason ?? 'Task disabled',
  });

  return { task: taskInstance, receipt };
}

/**
 * Start a task (enabled -> active)
 * @param {import('./task-core.mjs').TaskInstance} taskInstance - Task instance
 * @param {string} [resourceId] - Resource performing the task
 * @param {Object} [options] - Start options
 * @returns {Promise<{task: import('./task-core.mjs').TaskInstance, receipt: Object}>}
 */
export async function startTask(taskInstance, resourceId, options = {}) {
  const validation = validateTransition(taskInstance.status, TaskStatus.ACTIVE);
  if (!validation.valid) {
    throw new Error(`Cannot start task ${taskInstance.id}: ${validation.reason}`);
  }

  const beforeStatus = taskInstance.status;
  const beforeHash = await computeStateHash(taskInstance);

  taskInstance.status = TaskStatus.ACTIVE;
  taskInstance.startedAt = now();
  if (resourceId) {
    taskInstance.assignedResource = resourceId;
  }
  taskInstance.statusHistory.set(`active:${taskInstance.startedAt}`, {
    status: TaskStatus.ACTIVE,
    timestamp: taskInstance.startedAt,
    resource: resourceId,
  });

  const receipt = await generateReceipt(taskInstance, 'start', beforeStatus, beforeHash, {
    ...options.justification,
    reason: options.justification?.reason ?? `Task started by ${resourceId ?? 'system'}`,
    actor: resourceId,
  });

  return { task: taskInstance, receipt };
}

/**
 * Complete a task (active -> completed)
 * @param {import('./task-core.mjs').TaskInstance} taskInstance - Task instance
 * @param {Object} [outputData] - Output data from execution
 * @param {Object} [options] - Complete options
 * @returns {Promise<{task: import('./task-core.mjs').TaskInstance, receipt: Object}>}
 */
export async function completeTask(taskInstance, outputData = {}, options = {}) {
  const validation = validateTransition(taskInstance.status, TaskStatus.COMPLETED);
  if (!validation.valid) {
    throw new Error(`Cannot complete task ${taskInstance.id}: ${validation.reason}`);
  }

  // Validate post-conditions from task definition
  const postCheck = await taskInstance.taskDefinition.validatePostCondition({
    taskInstance,
    inputData: taskInstance.inputData,
    outputData,
  });
  if (!postCheck.valid) {
    throw new Error(`Cannot complete task ${taskInstance.id}: ${postCheck.reason}`);
  }

  const beforeStatus = taskInstance.status;
  const beforeHash = await computeStateHash(taskInstance);

  taskInstance.status = TaskStatus.COMPLETED;
  taskInstance.completedAt = now();
  taskInstance.outputData = outputData;
  taskInstance.statusHistory.set(`completed:${taskInstance.completedAt}`, {
    status: TaskStatus.COMPLETED,
    timestamp: taskInstance.completedAt,
    outputData,
  });

  const receipt = await generateReceipt(taskInstance, 'complete', beforeStatus, beforeHash, {
    ...options.justification,
    reason: options.justification?.reason ?? 'Task completed - post-conditions satisfied',
  });

  return { task: taskInstance, receipt };
}

/**
 * Cancel a task (any non-terminal -> cancelled)
 * @param {import('./task-core.mjs').TaskInstance} taskInstance - Task instance
 * @param {string} [reason] - Cancellation reason
 * @param {Object} [options] - Cancel options
 * @returns {Promise<{task: import('./task-core.mjs').TaskInstance, receipt: Object}>}
 */
export async function cancelTask(taskInstance, reason, options = {}) {
  const validation = validateTransition(taskInstance.status, TaskStatus.CANCELLED);
  if (!validation.valid) {
    throw new Error(`Cannot cancel task ${taskInstance.id}: ${validation.reason}`);
  }

  const beforeStatus = taskInstance.status;
  const beforeHash = await computeStateHash(taskInstance);

  taskInstance.status = TaskStatus.CANCELLED;
  taskInstance.completedAt = now();
  taskInstance.outputData = { cancelled: true, reason };
  taskInstance.statusHistory.set(`cancelled:${taskInstance.completedAt}`, {
    status: TaskStatus.CANCELLED,
    timestamp: taskInstance.completedAt,
    reason,
  });

  const receipt = await generateReceipt(taskInstance, 'cancel', beforeStatus, beforeHash, {
    ...options.justification,
    reason: options.justification?.reason ?? `Task cancelled: ${reason ?? 'no reason provided'}`,
  });

  return { task: taskInstance, receipt };
}

/**
 * Fail a task (active -> failed)
 * @param {import('./task-core.mjs').TaskInstance} taskInstance - Task instance
 * @param {Error|string} error - Error that caused failure
 * @param {Object} [options] - Fail options
 * @returns {Promise<{task: import('./task-core.mjs').TaskInstance, receipt: Object}>}
 */
export async function failTask(taskInstance, error, options = {}) {
  const validation = validateTransition(taskInstance.status, TaskStatus.FAILED);
  if (!validation.valid) {
    throw new Error(`Cannot fail task ${taskInstance.id}: ${validation.reason}`);
  }

  const beforeStatus = taskInstance.status;
  const beforeHash = await computeStateHash(taskInstance);

  const errorMessage = error instanceof Error ? error.message : error;
  taskInstance.status = TaskStatus.FAILED;
  taskInstance.completedAt = now();
  taskInstance.outputData = {
    failed: true,
    error: errorMessage,
    stack: error instanceof Error ? error.stack : undefined,
  };
  taskInstance.statusHistory.set(`failed:${taskInstance.completedAt}`, {
    status: TaskStatus.FAILED,
    timestamp: taskInstance.completedAt,
    error: errorMessage,
  });

  const receipt = await generateReceipt(taskInstance, 'fail', beforeStatus, beforeHash, {
    ...options.justification,
    reason: options.justification?.reason ?? `Task failed: ${errorMessage}`,
  });

  return { task: taskInstance, receipt };
}

/**
 * Mark task as timed out (enabled/active -> timeout)
 * @param {import('./task-core.mjs').TaskInstance} taskInstance - Task instance
 * @param {Object} [options] - Timeout options
 * @returns {Promise<{task: import('./task-core.mjs').TaskInstance, receipt: Object}>}
 */
export async function timeoutTask(taskInstance, options = {}) {
  const validation = validateTransition(taskInstance.status, TaskStatus.TIMEOUT);
  if (!validation.valid) {
    throw new Error(`Cannot timeout task ${taskInstance.id}: ${validation.reason}`);
  }

  const beforeStatus = taskInstance.status;
  const beforeHash = await computeStateHash(taskInstance);

  taskInstance.status = TaskStatus.TIMEOUT;
  taskInstance.completedAt = now();
  taskInstance.outputData = {
    timedOut: true,
    timeout: taskInstance.timeout,
    duration: taskInstance.getDuration()?.toString(),
  };
  taskInstance.statusHistory.set(`timeout:${taskInstance.completedAt}`, {
    status: TaskStatus.TIMEOUT,
    timestamp: taskInstance.completedAt,
  });

  const receipt = await generateReceipt(taskInstance, 'timeout', beforeStatus, beforeHash, {
    ...options.justification,
    reason: options.justification?.reason ?? `Task timed out after ${taskInstance.timeout}ms`,
  });

  return { task: taskInstance, receipt };
}

// =============================================================================
// Extend TaskInstance with execution methods
// =============================================================================

/**
 * Add execution methods to TaskInstance prototype
 * @param {import('./task-core.mjs').TaskInstance} TaskInstanceClass
 */
export function extendTaskInstance(TaskInstanceClass) {
  /**
   * Generate a transition receipt
   * @param {string} action - Transition action
   * @param {string} beforeStatus - Status before transition
   * @param {string} beforeHash - State hash before transition
   * @param {Object} [justification] - Hook validation justification
   * @returns {Promise<Object>} Receipt object
   */
  TaskInstanceClass.prototype.generateReceipt = async function(action, beforeStatus, beforeHash, justification = {}) {
    return generateReceipt(this, action, beforeStatus, beforeHash, justification);
  };

  /**
   * Enable this task (disabled -> enabled)
   * @param {Object} [options] - Enable options
   * @returns {Promise<{task: TaskInstance, receipt: Object}>}
   */
  TaskInstanceClass.prototype.enable = async function(options = {}) {
    return enableTask(this, options);
  };

  /**
   * Disable this task (enabled -> disabled)
   * @param {Object} [options] - Disable options
   * @returns {Promise<{task: TaskInstance, receipt: Object}>}
   */
  TaskInstanceClass.prototype.disable = async function(options = {}) {
    return disableTask(this, options);
  };

  /**
   * Start this task (enabled -> active)
   * @param {string} [resourceId] - Resource performing the task
   * @param {Object} [options] - Start options
   * @returns {Promise<{task: TaskInstance, receipt: Object}>}
   */
  TaskInstanceClass.prototype.start = async function(resourceId, options = {}) {
    return startTask(this, resourceId, options);
  };

  /**
   * Complete this task (active -> completed)
   * @param {Object} [outputData] - Output data from execution
   * @param {Object} [options] - Complete options
   * @returns {Promise<{task: TaskInstance, receipt: Object}>}
   */
  TaskInstanceClass.prototype.complete = async function(outputData = {}, options = {}) {
    return completeTask(this, outputData, options);
  };

  /**
   * Cancel this task (any non-terminal -> cancelled)
   * @param {string} [reason] - Cancellation reason
   * @param {Object} [options] - Cancel options
   * @returns {Promise<{task: TaskInstance, receipt: Object}>}
   */
  TaskInstanceClass.prototype.cancel = async function(reason, options = {}) {
    return cancelTask(this, reason, options);
  };

  /**
   * Fail this task (active -> failed)
   * @param {Error|string} error - Error that caused failure
   * @param {Object} [options] - Fail options
   * @returns {Promise<{task: TaskInstance, receipt: Object}>}
   */
  TaskInstanceClass.prototype.fail = async function(error, options = {}) {
    return failTask(this, error, options);
  };

  /**
   * Mark task as timed out (enabled/active -> timeout)
   * @param {Object} [options] - Timeout options
   * @returns {Promise<{task: TaskInstance, receipt: Object}>}
   */
  TaskInstanceClass.prototype.timedOut = async function(options = {}) {
    return timeoutTask(this, options);
  };
}
