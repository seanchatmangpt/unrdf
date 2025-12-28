/**
 * Execution Receipt - V6 Workflow/Hook/Task Receipts
 *
 * For YAWL workflow execution, task lifecycle, and control flow decisions.
 *
 * @module @unrdf/v6-core/receipts/execution-receipt
 */

import { z } from 'zod';
import { BaseReceiptSchema, RECEIPT_TYPES } from './base-receipt.mjs';

// =============================================================================
// Event Types
// =============================================================================

/**
 * Execution event types
 * @readonly
 * @enum {string}
 */
export const EXECUTION_EVENT_TYPES = Object.freeze({
  CASE_CREATED: 'CASE_CREATED',
  TASK_ENABLED: 'TASK_ENABLED',
  TASK_STARTED: 'TASK_STARTED',
  TASK_COMPLETED: 'TASK_COMPLETED',
  TASK_CANCELLED: 'TASK_CANCELLED',
  TASK_FAILED: 'TASK_FAILED',
  TASK_TIMEOUT: 'TASK_TIMEOUT',
  WORK_ITEM_CREATED: 'WORK_ITEM_CREATED',
  CONTROL_FLOW_EVALUATED: 'CONTROL_FLOW_EVALUATED',
});

// =============================================================================
// Schemas
// =============================================================================

/**
 * Event type schema
 */
export const ExecutionEventTypeSchema = z.enum([
  'CASE_CREATED',
  'TASK_ENABLED',
  'TASK_STARTED',
  'TASK_COMPLETED',
  'TASK_CANCELLED',
  'TASK_FAILED',
  'TASK_TIMEOUT',
  'WORK_ITEM_CREATED',
  'CONTROL_FLOW_EVALUATED',
]);

/**
 * Justification schema - explains why the decision was made
 */
export const JustificationSchema = z.object({
  /** Hook that validated the transition */
  hookValidated: z.string().optional(),
  /** SPARQL query used for control flow evaluation */
  sparqlQuery: z.string().optional(),
  /** Human-readable reasoning */
  reasoning: z.string().optional(),
  /** Condition that was checked */
  conditionChecked: z.string().optional(),
  /** Actor who approved (for manual tasks) */
  approvedBy: z.string().optional(),
});

/**
 * Execution payload schema
 */
export const ExecutionPayloadSchema = z.object({
  /** The decision made (e.g., 'APPROVE', 'ENABLE', 'COMPLETE') */
  decision: z.string().optional(),
  /** Justification for the decision */
  justification: JustificationSchema.optional(),
  /** Actor who made the decision */
  actor: z.string().optional(),
  /** Additional context data */
  context: z.any().optional(),
}).passthrough();

/**
 * Execution receipt schema - extends base with workflow-specific fields
 */
export const ExecutionReceiptSchema = BaseReceiptSchema.extend({
  receiptType: z.literal(RECEIPT_TYPES.EXECUTION),

  /** Event type (TASK_ENABLED, etc.) */
  eventType: ExecutionEventTypeSchema,

  /** Workflow case identifier */
  caseId: z.string().min(1),

  /** Task identifier */
  taskId: z.string().min(1),

  /** Work item identifier (optional) */
  workItemId: z.string().optional(),

  /** Execution decision payload */
  payload: ExecutionPayloadSchema,
});

// =============================================================================
// Type Definitions (JSDoc)
// =============================================================================

/**
 * @typedef {Object} Justification
 * @property {string} [hookValidated] - Hook that validated the transition
 * @property {string} [sparqlQuery] - SPARQL query used for evaluation
 * @property {string} [reasoning] - Human-readable reasoning
 * @property {string} [conditionChecked] - Condition that was checked
 * @property {string} [approvedBy] - Actor who approved
 */

/**
 * @typedef {Object} ExecutionPayload
 * @property {string} decision - The decision made
 * @property {Justification} [justification] - Justification for decision
 * @property {string} [actor] - Actor who made decision
 * @property {Object} [context] - Additional context
 */

/**
 * @typedef {Object} ExecutionReceipt
 * @property {string} id - UUID of receipt
 * @property {'execution'} receiptType - Receipt type discriminator
 * @property {bigint} t_ns - Nanosecond timestamp
 * @property {string} timestamp_iso - ISO timestamp
 * @property {string|null} previousHash - Previous receipt hash
 * @property {string} payloadHash - Payload hash
 * @property {string} receiptHash - Receipt hash
 * @property {string} eventType - Execution event type
 * @property {string} caseId - Workflow case ID
 * @property {string} taskId - Task ID
 * @property {string} [workItemId] - Work item ID
 * @property {ExecutionPayload} payload - Decision payload
 * @property {Object} [attestation] - Signature/attestation
 * @property {Object} [vectorClock] - Vector clock
 * @property {string} [gitRef] - Git reference
 * @property {string} [kgcEventId] - KGC event ID
 */

// =============================================================================
// Exports
// =============================================================================

export default ExecutionReceiptSchema;
