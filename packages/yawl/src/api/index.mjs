/**
 * @file YAWL Workflow API - Main entry point
 * @module @unrdf/yawl/api
 *
 * @description
 * Re-exports all YAWL workflow API functions, constants, and schemas.
 * Split into focused modules for better maintainability.
 */

// Import workflow creation functions and utilities
import {
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

// Import workflow execution functions
import {
  enableTask,
  startTask,
  completeTask,
  evaluateControlFlowAndEnable,
  evaluateCondition,
  checkAllPredecessorsComplete,
  WorkItemSchema,
  EnableTaskOptionsSchema,
} from './workflow-execution.mjs';

// Import workflow query functions
import {
  createCase,
  createCaseRDF,
  CaseOptionsSchema,
} from './workflow-query.mjs';

// Import workflow cancellation functions
import {
  cancelWorkItem,
} from './workflow-cancellation.mjs';

// Import workflow time machine functions
import {
  replayCase,
  queryCaseEvents,
  reconstructWorkItemHistory,
  determineHistoricalCaseStatus,
} from './workflow-timemachine.mjs';

// Export all named exports
export {
  // Workflow creation
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
  // Workflow execution
  enableTask,
  startTask,
  completeTask,
  evaluateControlFlowAndEnable,
  evaluateCondition,
  checkAllPredecessorsComplete,
  WorkItemSchema,
  EnableTaskOptionsSchema,
  // Workflow query
  createCase,
  createCaseRDF,
  CaseOptionsSchema,
  // Workflow cancellation
  cancelWorkItem,
  // Workflow time machine
  replayCase,
  queryCaseEvents,
  reconstructWorkItemHistory,
  determineHistoricalCaseStatus,
};

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
