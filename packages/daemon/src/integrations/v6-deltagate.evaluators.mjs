/**
 * @file v6 DeltaGate Evaluators
 * @module @unrdf/daemon/integrations/v6-deltagate-evaluators
 * @description Delta evaluation and application functions
 */

import { getNs } from './v6-deltagate.helpers.mjs';

/**
 * Check delta admissibility
 * @param {Object} delta - Delta to check
 * @param {Object} context - Evaluation context
 * @param {Function} evaluateCondition - Condition evaluator
 * @param {Function} evaluateConstraint - Constraint evaluator
 * @returns {Promise<Object>} Admissibility result
 */
export async function checkAdmissibility(delta, context, evaluateCondition, evaluateConstraint) {
  if (!delta.admissibility) {
    return { lawful: true };
  }

  const { policyId: _policyId, constraints = [], preConditions = [] } = delta.admissibility;

  // Check pre-conditions
  for (const condition of preConditions) {
    if (!evaluateCondition(condition, context)) {
      return {
        lawful: false,
        reason: `Pre-condition failed: ${condition}`,
      };
    }
  }

  // Check constraints
  for (const constraint of constraints) {
    if (!evaluateConstraint(constraint, context)) {
      return {
        lawful: false,
        reason: `Constraint failed: ${constraint}`,
      };
    }
  }

  return { lawful: true };
}

/**
 * Apply operations atomically
 * @param {Array} operations - Operations to apply
 * @param {Map} store - State store
 * @returns {Object} Application result
 */
export function applyOperations(operations, store) {
  try {
    for (const op of operations) {
      switch (op.op) {
        case 'set':
          store.set(op.path, op.newValue);
          break;
        case 'delete':
          store.delete(op.path);
          break;
        case 'insert':
          if (op.path.includes('[')) {
            // Array-like path: handle insert
            const current = store.get(op.path) || [];
            if (Array.isArray(current)) {
              current.splice(op.index, 0, op.value);
            }
          }
          break;
        default:
          return { success: false, reason: `Unknown operation: ${op.op}` };
      }
    }
    return { success: true };
  } catch (error) {
    return { success: false, reason: error.message };
  }
}

/**
 * Reverse an operation
 * @param {Object} op - Operation to reverse
 * @returns {Object} Reversed operation
 */
export function reverseOperation(op) {
  const reversed = { ...op, timestamp_ns: getNs() };

  switch (op.op) {
    case 'set':
      // If oldValue was undefined, the original operation created the key
      // So the reverse should delete it
      if (op.oldValue === undefined) {
        return {
          op: 'delete',
          path: op.path,
          oldValue: op.newValue,
          timestamp_ns: reversed.timestamp_ns,
        };
      }
      return {
        op: 'set',
        path: op.path,
        oldValue: op.newValue,
        newValue: op.oldValue,
        timestamp_ns: reversed.timestamp_ns,
      };
    case 'delete':
      return {
        op: 'set',
        path: op.path,
        oldValue: undefined,
        newValue: op.oldValue,
        timestamp_ns: reversed.timestamp_ns,
      };
    case 'insert':
      return {
        op: 'delete',
        path: op.path,
        oldValue: op.value,
        timestamp_ns: reversed.timestamp_ns,
      };
    default:
      throw new Error(`Cannot reverse operation: ${op.op}`);
  }
}

/**
 * Evaluate condition
 * @param {string} condition - Condition to evaluate
 * @param {Object} context - Evaluation context
 * @param {Map} store - State store
 * @param {Object} [logger] - Logger instance
 * @returns {boolean} Evaluation result
 */
export function evaluateCondition(condition, context, store, logger = console) {
  if (condition === 'always') return true;
  if (condition === 'never') return false;
  if (condition.startsWith('path:')) {
    const path = condition.substring(5);
    return store.has(path);
  }
  // Fail-closed: Unknown conditions denied
  logger.warn(`Unknown condition type: ${condition}, denying access`);
  return false;
}

/**
 * Evaluate constraint
 * @param {string} constraint - Constraint to evaluate
 * @param {Object} context - Evaluation context
 * @param {Object} [logger] - Logger instance
 * @returns {boolean} Evaluation result
 */
export function evaluateConstraint(constraint, context, logger = console) {
  if (constraint === 'none') return true;
  // Fail-closed: Unknown constraints denied
  logger.warn(`Unknown constraint type: ${constraint}, denying access`);
  return false;
}

/**
 * Validate chain at index
 * @param {Array} receiptHistory - Receipt history
 * @param {number} index - Index to validate
 * @returns {boolean} Whether chain is valid
 */
export function validateChain(receiptHistory, index) {
  if (index === 0) {
    return receiptHistory[0].previousReceiptHash === null;
  }

  const current = receiptHistory[index];
  const previous = receiptHistory[index - 1];

  return current.previousReceiptHash === previous.receiptHash;
}
