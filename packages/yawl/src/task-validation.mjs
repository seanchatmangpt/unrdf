/**
 * YAWL Task Validation - Pre/post-conditions and receipt verification
 *
 * Contains:
 * - State transition validation
 * - Pre-condition validation (TaskDefinition)
 * - Post-condition validation (TaskDefinition)
 * - Receipt chain verification
 *
 * @module @unrdf/yawl/task-validation
 */

import { blake3 } from 'hash-wasm';
import { VALID_TRANSITIONS } from './task-core.mjs';

// =============================================================================
// TaskDefinition Validation Methods
// =============================================================================

/**
 * Validate pre-conditions for task enablement
 * @param {import('./task-core.mjs').TaskDefinition} taskDef - Task definition
 * @param {Object} context - Validation context
 * @returns {Promise<{valid: boolean, reason?: string}>}
 */
export async function validatePreCondition(taskDef, context) {
  if (!taskDef.preCondition) {
    return { valid: true };
  }
  try {
    const result = await taskDef.preCondition(context);
    return typeof result === 'boolean'
      ? { valid: result, reason: result ? undefined : 'Pre-condition failed' }
      : result;
  } catch (error) {
    return { valid: false, reason: `Pre-condition error: ${error.message}` };
  }
}

/**
 * Validate post-conditions for task completion
 * @param {import('./task-core.mjs').TaskDefinition} taskDef - Task definition
 * @param {Object} context - Validation context with output data
 * @returns {Promise<{valid: boolean, reason?: string}>}
 */
export async function validatePostCondition(taskDef, context) {
  if (!taskDef.postCondition) {
    return { valid: true };
  }
  try {
    const result = await taskDef.postCondition(context);
    return typeof result === 'boolean'
      ? { valid: result, reason: result ? undefined : 'Post-condition failed' }
      : result;
  } catch (error) {
    return { valid: false, reason: `Post-condition error: ${error.message}` };
  }
}

// =============================================================================
// TaskInstance Validation Methods
// =============================================================================

/**
 * Validate a state transition
 * @param {string} fromStatus - Current status
 * @param {string} toStatus - Target status
 * @returns {{valid: boolean, reason?: string}}
 */
export function validateTransition(fromStatus, toStatus) {
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
 * @param {import('./task-core.mjs').TaskInstance} taskInstance - Task instance
 * @returns {Promise<string>}
 */
export async function computeStateHash(taskInstance) {
  const state = {
    id: taskInstance.id,
    taskDefId: taskInstance.taskDefId,
    caseId: taskInstance.caseId,
    status: taskInstance.status,
    inputData: taskInstance.inputData,
    outputData: taskInstance.outputData,
    enabledAt: taskInstance.enabledAt?.toString(),
    startedAt: taskInstance.startedAt?.toString(),
    completedAt: taskInstance.completedAt?.toString(),
  };
  return blake3(JSON.stringify(state));
}

/**
 * Verify receipt chain integrity
 * @param {Object[]} receipts - Array of receipts to verify
 * @returns {Promise<{valid: boolean, errors: string[]}>}
 */
export async function verifyReceiptChain(receipts) {
  const errors = [];
  let previousHash = null;

  for (let i = 0; i < receipts.length; i++) {
    const receipt = receipts[i];

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

/**
 * Add validation methods to TaskDefinition prototype
 * @param {import('./task-core.mjs').TaskDefinition} TaskDefinitionClass
 */
export function extendTaskDefinition(TaskDefinitionClass) {
  /**
   * Validate pre-conditions for task enablement
   * @param {Object} context - Validation context
   * @returns {Promise<{valid: boolean, reason?: string}>}
   */
  TaskDefinitionClass.prototype.validatePreCondition = async function(context) {
    return validatePreCondition(this, context);
  };

  /**
   * Validate post-conditions for task completion
   * @param {Object} context - Validation context with output data
   * @returns {Promise<{valid: boolean, reason?: string}>}
   */
  TaskDefinitionClass.prototype.validatePostCondition = async function(context) {
    return validatePostCondition(this, context);
  };
}

/**
 * Add validation methods to TaskInstance prototype
 * @param {import('./task-core.mjs').TaskInstance} TaskInstanceClass
 */
export function extendTaskInstance(TaskInstanceClass) {
  /**
   * Validate a state transition
   * @param {string} fromStatus - Current status
   * @param {string} toStatus - Target status
   * @returns {{valid: boolean, reason?: string}}
   */
  TaskInstanceClass.prototype.validateTransition = function(fromStatus, toStatus) {
    return validateTransition(fromStatus, toStatus);
  };

  /**
   * Compute state hash for receipts
   * @returns {Promise<string>}
   */
  TaskInstanceClass.prototype.computeStateHash = async function() {
    return computeStateHash(this);
  };

  /**
   * Verify receipt chain integrity
   * @returns {Promise<{valid: boolean, errors: string[]}>}
   */
  TaskInstanceClass.prototype.verifyReceiptChain = async function() {
    return verifyReceiptChain(this.receipts);
  };
}
