/**
 * @file YAWL Workflow API - Main Entry Point (Barrel Export)
 * @module @unrdf/yawl/api/workflow-api
 *
 * @description
 * High-level workflow interface for @unrdf/yawl implementing YAWL patterns
 * with KGC-4D event sourcing, hooks integration, and cryptographic receipts.
 *
 * This module re-exports all functionality from focused sub-modules:
 * - workflow-api-validation.mjs: Constants, schemas, utilities
 * - workflow-api-core.mjs: Workflow and case creation
 * - workflow-api-execution.mjs: Task lifecycle operations
 * - workflow-api-replay.mjs: Case replay and time travel
 *
 * All functions are pure (no side effects except event append).
 * Returns receipts with cryptographic justification for all state transitions.
 */

// ============================================================================
// Re-export from validation module
// ============================================================================

export {
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

  // Utilities
  generateId,
  now,
  toISO,
  createHash,
  createReceipt,
} from './workflow-api-validation.mjs';

// ============================================================================
// Re-export from core module
// ============================================================================

export {
  // Core API functions
  createWorkflow,
  createCase,

  // Helper functions (exported for testing/advanced use)
  buildControlFlowGraph,
  findInitialTasks,
  createWorkflowRDF,
  createCaseRDF,
} from './workflow-api-core.mjs';

// ============================================================================
// Re-export from execution module
// ============================================================================

export {
  // Task lifecycle functions
  enableTask,
  startTask,
  completeTask,
  cancelWorkItem,

  // Control flow helpers (exported for testing/advanced use)
  evaluateControlFlowAndEnable,
  evaluateCondition,
  checkAllPredecessorsComplete,
} from './workflow-api-execution.mjs';

// ============================================================================
// Re-export from replay module
// ============================================================================

export {
  // Replay functions
  replayCase,

  // Helper functions (exported for testing/advanced use)
  queryCaseEvents,
  reconstructWorkItemHistory,
  determineHistoricalCaseStatus,
} from './workflow-api-replay.mjs';

// ============================================================================
// Default Export (for backward compatibility)
// ============================================================================

import {
  YAWL_NS,
  YAWL_EVENT_TYPES,
  WORK_ITEM_STATUS,
  CONTROL_FLOW_PATTERNS,
  TaskSchema,
  ControlFlowSchema,
  ResourceSchema,
  WorkflowSpecSchema,
  WorkflowOptionsSchema,
  CaseOptionsSchema,
  WorkItemSchema,
  EnableTaskOptionsSchema,
  ReceiptSchema,
} from './workflow-api-validation.mjs';

import {
  createWorkflow,
  createCase,
} from './workflow-api-core.mjs';

import {
  enableTask,
  startTask,
  completeTask,
  cancelWorkItem,
} from './workflow-api-execution.mjs';

import {
  replayCase,
} from './workflow-api-replay.mjs';

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
};
