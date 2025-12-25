/**
 * YAWL Event-Sourcing Integration with KGC-4D
 * Implements deterministic workflow replay with time-travel capabilities
 *
 * @module yawl-events
 * @description
 * Barrel export for YAWL event-sourcing functionality.
 * Provides event types, appending, case reconstruction, receipt generation,
 * and audit trail functionality for YAWL workflow execution with KGC-4D backend.
 *
 * All events are immutable and stored in the KGC-4D EventLog named graph.
 * State changes are tracked as deltas for deterministic replay.
 * Receipts include BLAKE3 hashes for cryptographic verification.
 */

// Import for default export
import {
  YAWL_EVENT_TYPES as _YAWL_EVENT_TYPES,
  YAWL_NS as _YAWL_NS,
  YAWL_PREDICATES as _YAWL_PREDICATES,
  appendWorkflowEvent as _appendWorkflowEvent,
} from './yawl-events-core.mjs';

import {
  reconstructCase as _reconstructCase,
  getWorkflowAuditTrail as _getWorkflowAuditTrail,
} from './yawl-events-kgc4d.mjs';

import {
  createWorkflowReceipt as _createWorkflowReceipt,
  verifyWorkflowReceipt as _verifyWorkflowReceipt,
  createCase as _createCase,
  enableTask as _enableTask,
  startWorkItem as _startWorkItem,
  completeWorkItem as _completeWorkItem,
  recordControlFlowEvaluation as _recordControlFlowEvaluation,
} from './yawl-events-receipts.mjs';

// Re-export from core module
export {
  YAWL_EVENT_TYPES,
  YAWL_NS,
  YAWL_PREDICATES,
  ReceiptSchema,
  CaseCreatedSchema,
  TaskEnabledSchema,
  TaskStartedSchema,
  TaskCompletedSchema,
  TaskCancelledSchema,
  WorkItemCreatedSchema,
  ControlFlowEvaluatedSchema,
  EventSchemas,
  generateUUID,
  serializeCaseState,
  caseGraphUri,
  appendWorkflowEvent,
} from './yawl-events-core.mjs';

// Re-export from KGC-4D module
export {
  reconstructCase,
  getWorkflowAuditTrail,
} from './yawl-events-kgc4d.mjs';

// Re-export from receipts module
export {
  createWorkflowReceipt,
  verifyWorkflowReceipt,
  createCase,
  enableTask,
  startWorkItem,
  completeWorkItem,
  recordControlFlowEvaluation,
} from './yawl-events-receipts.mjs';

// Default export for convenience
export default {
  // Event types
  YAWL_EVENT_TYPES: _YAWL_EVENT_TYPES,
  YAWL_NS: _YAWL_NS,
  YAWL_PREDICATES: _YAWL_PREDICATES,

  // Core functions
  appendWorkflowEvent: _appendWorkflowEvent,
  reconstructCase: _reconstructCase,
  createWorkflowReceipt: _createWorkflowReceipt,
  verifyWorkflowReceipt: _verifyWorkflowReceipt,
  getWorkflowAuditTrail: _getWorkflowAuditTrail,

  // High-level workflow functions
  createCase: _createCase,
  enableTask: _enableTask,
  startWorkItem: _startWorkItem,
  completeWorkItem: _completeWorkItem,
  recordControlFlowEvaluation: _recordControlFlowEvaluation,
};
