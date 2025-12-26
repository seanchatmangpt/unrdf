/**
 * YAWL Task - Main entry point
 *
 * Exports:
 * - Core classes and constants
 * - Validation utilities
 * - Execution methods
 * - Serialization helpers
 * - Factory functions
 * - Legacy compatibility
 *
 * @module @unrdf/yawl/task
 */

// =============================================================================
// Core Exports
// =============================================================================

export {
  TaskStatus,
  TaskStatus_INACTIVE,
  TaskStatus_RUNNING,
  VALID_TRANSITIONS,
  TaskDefinitionSchema,
  TaskInstanceSchema,
  TransitionReceiptSchema,
  TaskDefinition,
  TaskInstance,
} from './task-core.mjs';

// =============================================================================
// Validation Exports
// =============================================================================

export {
  validatePreCondition,
  validatePostCondition,
  validateTransition,
  computeStateHash,
  verifyReceiptChain,
} from './task-validation.mjs';

// =============================================================================
// Execution Exports
// =============================================================================

export {
  generateReceipt,
  enableTask,
  disableTask,
  startTask,
  completeTask,
  cancelTask,
  failTask,
  timeoutTask,
} from './task-execution.mjs';

// =============================================================================
// Serialization Exports
// =============================================================================

export {
  serializeTaskInstance,
  deserializeTaskInstance,
  buildTransitionReceipt,
  YawlTask,
} from './task-rdf.mjs';

// =============================================================================
// Apply Extensions to Classes
// =============================================================================

import { TaskDefinition, TaskInstance } from './task-core.mjs';
import { extendTaskDefinition as extendTaskDefinitionValidation, extendTaskInstance as extendTaskInstanceValidation } from './task-validation.mjs';
import { extendTaskInstance as extendTaskInstanceExecution } from './task-execution.mjs';
import { extendTaskInstance as extendTaskInstanceRDF } from './task-rdf.mjs';

// Extend TaskDefinition with validation methods
extendTaskDefinitionValidation(TaskDefinition);

// Extend TaskInstance with all methods
extendTaskInstanceValidation(TaskInstance);
extendTaskInstanceExecution(TaskInstance);
extendTaskInstanceRDF(TaskInstance);

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
