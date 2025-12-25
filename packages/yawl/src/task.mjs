/**
 * YAWL Task - Barrel export for all task-related functionality
 * @module @unrdf/yawl/task
 */

// Re-export all definitions
export {
  TaskStatus,
  TaskStatus_INACTIVE,
  TaskStatus_RUNNING,
  VALID_TRANSITIONS,
  TaskDefinitionSchema,
  TaskInstanceSchema,
  TransitionReceiptSchema,
  TaskDataSchema,
  TaskDefinition,
  createTaskDefinition,
} from './task-definitions.mjs';

// Re-export task instance
export {
  TaskInstance,
  createTaskInstance,
} from './task-instance.mjs';

// Re-export execution
export {
  buildTransitionReceipt,
} from './task-execution.mjs';

// Re-export legacy
export {
  YawlTask,
} from './task-legacy.mjs';
