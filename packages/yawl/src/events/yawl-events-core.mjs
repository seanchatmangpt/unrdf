/**
 * YAWL Event-Sourcing Core
 * Event types, schemas, validation, and event appending
 *
 * @module yawl-events-core
 * @description
 * Core event infrastructure for YAWL workflow execution with KGC-4D backend.
 * Provides event types, validation schemas, and event appending functions.
 */

import { z } from 'zod';
import { dataFactory } from '@unrdf/oxigraph';
import { GRAPHS, PREDICATES } from '@unrdf/kgc-4d';

// ============================================================================
// YAWL Event Types
// ============================================================================

/**
 * YAWL workflow event types
 * @readonly
 * @enum {string}
 */
export const YAWL_EVENT_TYPES = Object.freeze({
  /** Case (workflow instance) created */
  CASE_CREATED: 'YAWL_CASE_CREATED',
  /** Task became enabled (ready for execution) */
  TASK_ENABLED: 'YAWL_TASK_ENABLED',
  /** Task execution started */
  TASK_STARTED: 'YAWL_TASK_STARTED',
  /** Task execution completed */
  TASK_COMPLETED: 'YAWL_TASK_COMPLETED',
  /** Task execution cancelled */
  TASK_CANCELLED: 'YAWL_TASK_CANCELLED',
  /** Work item created for a task */
  WORK_ITEM_CREATED: 'YAWL_WORK_ITEM_CREATED',
  /** Control flow condition evaluated */
  CONTROL_FLOW_EVALUATED: 'YAWL_CONTROL_FLOW_EVALUATED',
});

/**
 * YAWL namespace for RDF predicates
 * @constant {string}
 */
export const YAWL_NS = 'http://yawl.io/';

/**
 * YAWL-specific predicates for RDF serialization
 * @readonly
 */
export const YAWL_PREDICATES = Object.freeze({
  CASE_ID: `${YAWL_NS}caseId`,
  SPEC_ID: `${YAWL_NS}specId`,
  TASK_ID: `${YAWL_NS}taskId`,
  WORK_ITEM_ID: `${YAWL_NS}workItemId`,
  ENABLED_AT: `${YAWL_NS}enabledAt`,
  STARTED_AT: `${YAWL_NS}startedAt`,
  COMPLETED_AT: `${YAWL_NS}completedAt`,
  CANCELLED_AT: `${YAWL_NS}cancelledAt`,
  CREATED_AT: `${YAWL_NS}createdAt`,
  RESULT: `${YAWL_NS}result`,
  REASON: `${YAWL_NS}reason`,
  SPARQL_QUERY: `${YAWL_NS}sparqlQuery`,
  BEFORE_HASH: `${YAWL_NS}beforeHash`,
  AFTER_HASH: `${YAWL_NS}afterHash`,
  RECEIPT_HASH: `${YAWL_NS}receiptHash`,
  JUSTIFICATION: `${YAWL_NS}justification`,
  STATE: `${YAWL_NS}state`,
});

// ============================================================================
// Zod Schemas for Event Validation
// ============================================================================

/**
 * Receipt schema - cryptographic proof of state transition
 * FIXED: Allow null values for optional justification fields
 */
export const ReceiptSchema = z.object({
  /** BLAKE3 hash of state before operation */
  beforeHash: z.string().length(64, 'BLAKE3 hash must be 64 hex chars'),
  /** BLAKE3 hash of state after operation */
  afterHash: z.string().length(64, 'BLAKE3 hash must be 64 hex chars'),
  /** BLAKE3 hash of decision/event data */
  hash: z.string().length(64, 'BLAKE3 hash must be 64 hex chars'),
  /** Justification for the state transition */
  justification: z.object({
    hookValidated: z.string().nullable().optional(),
    conditionChecked: z.string().nullable().optional(),
    sparqlQuery: z.string().nullable().optional(),
    reasoning: z.string().nullable().optional(),
  }),
  /** Git reference from snapshot (if available) */
  gitRef: z.string().nullable().optional(),
  /** Nanosecond timestamp */
  t_ns: z.string(),
  /** ISO timestamp for display */
  timestamp_iso: z.string(),
});

/**
 * CASE_CREATED event payload schema
 */
export const CaseCreatedSchema = z.object({
  caseId: z.string().min(1),
  specId: z.string().min(1),
  timestamp: z.string(),
  receipt: ReceiptSchema,
});

/**
 * TASK_ENABLED event payload schema
 */
export const TaskEnabledSchema = z.object({
  taskId: z.string().min(1),
  caseId: z.string().min(1),
  workItemId: z.string().min(1),
  enabledAt: z.string(),
  receipt: ReceiptSchema,
});

/**
 * TASK_STARTED event payload schema
 */
export const TaskStartedSchema = z.object({
  workItemId: z.string().min(1),
  startedAt: z.string(),
  receipt: ReceiptSchema,
});

/**
 * TASK_COMPLETED event payload schema
 */
export const TaskCompletedSchema = z.object({
  workItemId: z.string().min(1),
  completedAt: z.string(),
  result: z.any(),
  receipt: ReceiptSchema,
});

/**
 * TASK_CANCELLED event payload schema
 */
export const TaskCancelledSchema = z.object({
  workItemId: z.string().min(1),
  cancelledAt: z.string(),
  reason: z.string(),
  receipt: ReceiptSchema,
});

/**
 * WORK_ITEM_CREATED event payload schema
 */
export const WorkItemCreatedSchema = z.object({
  workItemId: z.string().min(1),
  taskId: z.string().min(1),
  caseId: z.string().min(1),
  createdAt: z.string(),
});

/**
 * CONTROL_FLOW_EVALUATED event payload schema
 */
export const ControlFlowEvaluatedSchema = z.object({
  caseId: z.string().min(1),
  taskId: z.string().min(1),
  result: z.boolean(),
  timestamp: z.string(),
  sparqlQuery: z.string(),
  receipt: ReceiptSchema,
});

/**
 * Map of event types to their validation schemas
 */
export const EventSchemas = {
  [YAWL_EVENT_TYPES.CASE_CREATED]: CaseCreatedSchema,
  [YAWL_EVENT_TYPES.TASK_ENABLED]: TaskEnabledSchema,
  [YAWL_EVENT_TYPES.TASK_STARTED]: TaskStartedSchema,
  [YAWL_EVENT_TYPES.TASK_COMPLETED]: TaskCompletedSchema,
  [YAWL_EVENT_TYPES.TASK_CANCELLED]: TaskCancelledSchema,
  [YAWL_EVENT_TYPES.WORK_ITEM_CREATED]: WorkItemCreatedSchema,
  [YAWL_EVENT_TYPES.CONTROL_FLOW_EVALUATED]: ControlFlowEvaluatedSchema,
};

// ============================================================================
// Helper Functions
// ============================================================================

/**
 * Generate a UUID (works in both Node.js and browser)
 * @returns {string} UUID v4
 */
export function generateUUID() {
  if (typeof crypto !== 'undefined' && crypto.randomUUID) {
    return crypto.randomUUID();
  }
  // Fallback for older environments
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

/**
 * Serialize case state to deterministic string for hashing
 * @param {Object} caseState - Case state object
 * @returns {string} Deterministic JSON string
 */
export function serializeCaseState(caseState) {
  // Sort keys for deterministic serialization
  const sortedKeys = Object.keys(caseState).sort();
  const sorted = {};
  for (const key of sortedKeys) {
    const value = caseState[key];
    if (value && typeof value === 'object' && !Array.isArray(value)) {
      sorted[key] = serializeCaseState(value);
    } else if (Array.isArray(value)) {
      sorted[key] = value.map((item) =>
        typeof item === 'object' ? serializeCaseState(item) : item
      );
    } else {
      sorted[key] = value;
    }
  }
  return JSON.stringify(sorted);
}

/**
 * Create named graph URI for a case
 * @param {string} caseId - Case UUID
 * @returns {string} Named graph URI
 */
export function caseGraphUri(caseId) {
  return `${YAWL_NS}case/${caseId}`;
}

// ============================================================================
// Event Appending
// ============================================================================

/**
 * Append a workflow event to the KGC-4D store
 *
 * Integrates with KGC-4D store.appendEvent() to:
 * - Store event in EventLog named graph
 * - Create delta for workflow state update
 * - Return receipt with BLAKE3 hash
 *
 * @param {import('@unrdf/kgc-4d').KGCStore} store - KGC-4D store instance
 * @param {keyof typeof YAWL_EVENT_TYPES} eventType - Event type
 * @param {Object} payload - Event payload (validated against schema)
 * @param {Object} [options={}] - Additional options
 * @param {string} [options.gitRef] - Git reference for the event
 * @param {Object} [options.previousState] - Previous case state for hash chaining
 * @returns {Promise<Object>} Receipt with event ID and hashes
 *
 * @example
 * const receipt = await appendWorkflowEvent(store, 'CASE_CREATED', {
 *   caseId: 'uuid-here',
 *   specId: 'approval-workflow',
 *   timestamp: new Date().toISOString(),
 *   receipt: { ... }
 * });
 */
export async function appendWorkflowEvent(store, eventType, payload, options = {}) {
  // Validate event type
  if (!YAWL_EVENT_TYPES[eventType] && !Object.values(YAWL_EVENT_TYPES).includes(eventType)) {
    throw new Error(`Invalid YAWL event type: ${eventType}`);
  }

  // Normalize event type to full form
  const normalizedType = YAWL_EVENT_TYPES[eventType] || eventType;

  // Extract caseId for case-specific graph (check before schema validation)
  const caseId = payload.caseId || (payload.workItemId && options.caseId);
  if (!caseId) {
    throw new Error('caseId is required for workflow events');
  }

  // Validate payload against schema
  const schema = EventSchemas[normalizedType];
  if (schema) {
    const result = schema.safeParse(payload);
    if (!result.success) {
      throw new Error(`Invalid ${normalizedType} payload: ${result.error.message}`);
    }
  }

  // Build deltas for RDF store update
  const deltas = buildEventDeltas(normalizedType, payload, caseId);

  // Append to KGC-4D store
  const { receipt } = await store.appendEvent(
    {
      type: normalizedType,
      payload: {
        ...payload,
        yawl_case_id: caseId,
        yawl_event_type: normalizedType,
      },
      git_ref: options.gitRef || null,
    },
    deltas
  );

  return {
    eventId: receipt.id,
    t_ns: receipt.t_ns,
    timestamp_iso: receipt.timestamp_iso,
    event_count: receipt.event_count,
    caseId,
    eventType: normalizedType,
    payloadHash: payload.receipt?.hash || null,
  };
}

/**
 * Build RDF deltas for a workflow event
 * @param {string} eventType - YAWL event type
 * @param {Object} payload - Event payload
 * @param {string} caseId - Case UUID
 * @returns {Array<Object>} Array of delta operations
 */
function buildEventDeltas(eventType, payload, caseId) {
  const deltas = [];
  const caseSubject = dataFactory.namedNode(caseGraphUri(caseId));

  switch (eventType) {
    case YAWL_EVENT_TYPES.CASE_CREATED: {
      // Add case triple
      deltas.push({
        type: 'add',
        subject: caseSubject,
        predicate: dataFactory.namedNode(YAWL_PREDICATES.SPEC_ID),
        object: dataFactory.literal(payload.specId),
      });
      deltas.push({
        type: 'add',
        subject: caseSubject,
        predicate: dataFactory.namedNode(YAWL_PREDICATES.STATE),
        object: dataFactory.literal('active'),
      });
      deltas.push({
        type: 'add',
        subject: caseSubject,
        predicate: dataFactory.namedNode(YAWL_PREDICATES.CREATED_AT),
        object: dataFactory.literal(payload.timestamp),
      });
      break;
    }

    case YAWL_EVENT_TYPES.TASK_ENABLED: {
      const workItemSubject = dataFactory.namedNode(`${YAWL_NS}workitem/${payload.workItemId}`);
      deltas.push({
        type: 'add',
        subject: workItemSubject,
        predicate: dataFactory.namedNode(YAWL_PREDICATES.TASK_ID),
        object: dataFactory.literal(payload.taskId),
      });
      deltas.push({
        type: 'add',
        subject: workItemSubject,
        predicate: dataFactory.namedNode(YAWL_PREDICATES.CASE_ID),
        object: dataFactory.namedNode(caseGraphUri(caseId)),
      });
      deltas.push({
        type: 'add',
        subject: workItemSubject,
        predicate: dataFactory.namedNode(YAWL_PREDICATES.STATE),
        object: dataFactory.literal('enabled'),
      });
      deltas.push({
        type: 'add',
        subject: workItemSubject,
        predicate: dataFactory.namedNode(YAWL_PREDICATES.ENABLED_AT),
        object: dataFactory.literal(payload.enabledAt),
      });
      break;
    }

    case YAWL_EVENT_TYPES.TASK_STARTED: {
      const workItemSubject = dataFactory.namedNode(`${YAWL_NS}workitem/${payload.workItemId}`);
      // Delete old state
      deltas.push({
        type: 'delete',
        subject: workItemSubject,
        predicate: dataFactory.namedNode(YAWL_PREDICATES.STATE),
        object: dataFactory.literal('enabled'),
      });
      // Add new state
      deltas.push({
        type: 'add',
        subject: workItemSubject,
        predicate: dataFactory.namedNode(YAWL_PREDICATES.STATE),
        object: dataFactory.literal('started'),
      });
      deltas.push({
        type: 'add',
        subject: workItemSubject,
        predicate: dataFactory.namedNode(YAWL_PREDICATES.STARTED_AT),
        object: dataFactory.literal(payload.startedAt),
      });
      break;
    }

    case YAWL_EVENT_TYPES.TASK_COMPLETED: {
      const workItemSubject = dataFactory.namedNode(`${YAWL_NS}workitem/${payload.workItemId}`);
      // Delete old state
      deltas.push({
        type: 'delete',
        subject: workItemSubject,
        predicate: dataFactory.namedNode(YAWL_PREDICATES.STATE),
        object: dataFactory.literal('started'),
      });
      // Add new state
      deltas.push({
        type: 'add',
        subject: workItemSubject,
        predicate: dataFactory.namedNode(YAWL_PREDICATES.STATE),
        object: dataFactory.literal('completed'),
      });
      deltas.push({
        type: 'add',
        subject: workItemSubject,
        predicate: dataFactory.namedNode(YAWL_PREDICATES.COMPLETED_AT),
        object: dataFactory.literal(payload.completedAt),
      });
      if (payload.result !== undefined) {
        deltas.push({
          type: 'add',
          subject: workItemSubject,
          predicate: dataFactory.namedNode(YAWL_PREDICATES.RESULT),
          object: dataFactory.literal(JSON.stringify(payload.result)),
        });
      }
      break;
    }

    case YAWL_EVENT_TYPES.TASK_CANCELLED: {
      const workItemSubject = dataFactory.namedNode(`${YAWL_NS}workitem/${payload.workItemId}`);
      // Add cancelled state (keep previous state for audit)
      deltas.push({
        type: 'add',
        subject: workItemSubject,
        predicate: dataFactory.namedNode(YAWL_PREDICATES.STATE),
        object: dataFactory.literal('cancelled'),
      });
      deltas.push({
        type: 'add',
        subject: workItemSubject,
        predicate: dataFactory.namedNode(YAWL_PREDICATES.CANCELLED_AT),
        object: dataFactory.literal(payload.cancelledAt),
      });
      deltas.push({
        type: 'add',
        subject: workItemSubject,
        predicate: dataFactory.namedNode(YAWL_PREDICATES.REASON),
        object: dataFactory.literal(payload.reason),
      });
      break;
    }

    case YAWL_EVENT_TYPES.WORK_ITEM_CREATED: {
      const workItemSubject = dataFactory.namedNode(`${YAWL_NS}workitem/${payload.workItemId}`);
      deltas.push({
        type: 'add',
        subject: workItemSubject,
        predicate: dataFactory.namedNode(YAWL_PREDICATES.TASK_ID),
        object: dataFactory.literal(payload.taskId),
      });
      deltas.push({
        type: 'add',
        subject: workItemSubject,
        predicate: dataFactory.namedNode(YAWL_PREDICATES.CASE_ID),
        object: dataFactory.namedNode(caseGraphUri(caseId)),
      });
      deltas.push({
        type: 'add',
        subject: workItemSubject,
        predicate: dataFactory.namedNode(YAWL_PREDICATES.CREATED_AT),
        object: dataFactory.literal(payload.createdAt),
      });
      break;
    }

    case YAWL_EVENT_TYPES.CONTROL_FLOW_EVALUATED: {
      // Store control flow decision for audit
      const decisionSubject = dataFactory.namedNode(
        `${YAWL_NS}decision/${generateUUID()}`
      );
      deltas.push({
        type: 'add',
        subject: decisionSubject,
        predicate: dataFactory.namedNode(YAWL_PREDICATES.CASE_ID),
        object: dataFactory.namedNode(caseGraphUri(caseId)),
      });
      deltas.push({
        type: 'add',
        subject: decisionSubject,
        predicate: dataFactory.namedNode(YAWL_PREDICATES.TASK_ID),
        object: dataFactory.literal(payload.taskId),
      });
      deltas.push({
        type: 'add',
        subject: decisionSubject,
        predicate: dataFactory.namedNode(YAWL_PREDICATES.RESULT),
        object: dataFactory.literal(String(payload.result)),
      });
      deltas.push({
        type: 'add',
        subject: decisionSubject,
        predicate: dataFactory.namedNode(YAWL_PREDICATES.SPARQL_QUERY),
        object: dataFactory.literal(payload.sparqlQuery),
      });
      break;
    }
  }

  return deltas;
}
