/**
 * @file YAWL Workflow API - Main entry point
 * @module @unrdf/yawl/api
 *
 * @description
 * Re-exports all YAWL workflow API functions, constants, and schemas.
 * Split into focused modules for better maintainability.
 */

// Export workflow creation functions and utilities
export {
  createWorkflow,
  buildControlFlowGraph,
  findInitialTasks,
  createWorkflowRDF,
  generateId,
  now,
  toISO,
  createHash,
  createReceipt,
  YAWL_NS,
  YAWL_EVENT_TYPES,
  WORK_ITEM_STATUS,
  CONTROL_FLOW_PATTERNS,
  TaskSchema,
  ControlFlowSchema,
  ResourceSchema,
  WorkflowSpecSchema,
  WorkflowOptionsSchema,
  ReceiptSchema,
} from './workflow-creation.mjs';

// Export workflow execution functions
export {
  enableTask,
  startTask,
  completeTask,
  evaluateControlFlowAndEnable,
  evaluateCondition,
  checkAllPredecessorsComplete,
  WorkItemSchema,
  EnableTaskOptionsSchema,
} from './workflow-execution.mjs';

// Export workflow query functions
export {
  createCase,
  createCaseRDF,
  CaseOptionsSchema,
} from './workflow-query.mjs';

// Export workflow cancellation functions
export {
  cancelWorkItem,
} from './workflow-cancellation.mjs';

// Export workflow time machine functions
export {
  replayCase,
  queryCaseEvents,
  reconstructWorkItemHistory,
  determineHistoricalCaseStatus,
} from './workflow-timemachine.mjs';

// Default export with all functions
export default {
  // Core API
  createWorkflow,
  createCase,
  enableTask,
  startTask,
  completeTask,
  cancelWorkItem,
  replayCase,

  // Constants
  YAWL_NS,
  YAWL_EVENT_TYPES,
  WORK_ITEM_STATUS,
  CONTROL_FLOW_PATTERNS,

  // Schemas
  TaskSchema,
  ControlFlowSchema,
  ResourceSchema,
  WorkflowSpecSchema,
  WorkflowOptionsSchema,
  CaseOptionsSchema,
  WorkItemSchema,
  EnableTaskOptionsSchema,
  ReceiptSchema,

  // Helper functions (exported for testing/advanced use)
  buildControlFlowGraph,
  findInitialTasks,
  createWorkflowRDF,
  createCaseRDF,
  evaluateControlFlowAndEnable,
  evaluateCondition,
  checkAllPredecessorsComplete,
  queryCaseEvents,
  reconstructWorkItemHistory,
  determineHistoricalCaseStatus,
  generateId,
  now,
  toISO,
  createHash,
  createReceipt,
};
