/**
 * YAWL Event-Sourcing Integration with KGC-4D
 * Implements deterministic workflow replay with time-travel capabilities
 *
 * @module yawl-events
 * @description
 * Provides event types, appending, case reconstruction, receipt generation,
 * and audit trail functionality for YAWL workflow execution with KGC-4D backend.
 *
 * All events are immutable and stored in the KGC-4D EventLog named graph.
 * State changes are tracked as deltas for deterministic replay.
 * Receipts include BLAKE3 hashes for cryptographic verification.
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { dataFactory } from '@unrdf/oxigraph';
import { now, toISO, VectorClock } from '@unrdf/kgc-4d';
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
 */
const ReceiptSchema = z.object({
  /** BLAKE3 hash of state before operation */
  beforeHash: z.string().length(64, 'BLAKE3 hash must be 64 hex chars'),
  /** BLAKE3 hash of state after operation */
  afterHash: z.string().length(64, 'BLAKE3 hash must be 64 hex chars'),
  /** BLAKE3 hash of decision/event data */
  hash: z.string().length(64, 'BLAKE3 hash must be 64 hex chars'),
  /** Justification for the state transition */
  justification: z.object({
    hookValidated: z.string().optional(),
    conditionChecked: z.string().optional(),
    sparqlQuery: z.string().optional(),
    reasoning: z.string().optional(),
  }),
  /** Git reference from snapshot (if available) */
  gitRef: z.string().optional(),
  /** Nanosecond timestamp */
  t_ns: z.string(),
  /** ISO timestamp for display */
  timestamp_iso: z.string(),
});

/**
 * CASE_CREATED event payload schema
 */
const CaseCreatedSchema = z.object({
  caseId: z.string().uuid(),
  specId: z.string().min(1),
  timestamp: z.string(),
  receipt: ReceiptSchema,
});

/**
 * TASK_ENABLED event payload schema
 */
const TaskEnabledSchema = z.object({
  taskId: z.string().min(1),
  caseId: z.string().uuid(),
  workItemId: z.string().uuid(),
  enabledAt: z.string(),
  receipt: ReceiptSchema,
});

/**
 * TASK_STARTED event payload schema
 */
const TaskStartedSchema = z.object({
  workItemId: z.string().uuid(),
  startedAt: z.string(),
  receipt: ReceiptSchema,
});

/**
 * TASK_COMPLETED event payload schema
 */
const TaskCompletedSchema = z.object({
  workItemId: z.string().uuid(),
  completedAt: z.string(),
  result: z.any(),
  receipt: ReceiptSchema,
});

/**
 * TASK_CANCELLED event payload schema
 */
const TaskCancelledSchema = z.object({
  workItemId: z.string().uuid(),
  cancelledAt: z.string(),
  reason: z.string(),
  receipt: ReceiptSchema,
});

/**
 * WORK_ITEM_CREATED event payload schema
 */
const WorkItemCreatedSchema = z.object({
  workItemId: z.string().uuid(),
  taskId: z.string().min(1),
  caseId: z.string().uuid(),
  createdAt: z.string(),
});

/**
 * CONTROL_FLOW_EVALUATED event payload schema
 */
const ControlFlowEvaluatedSchema = z.object({
  caseId: z.string().uuid(),
  taskId: z.string().min(1),
  result: z.boolean(),
  timestamp: z.string(),
  sparqlQuery: z.string(),
  receipt: ReceiptSchema,
});

/**
 * Map of event types to their validation schemas
 */
const EventSchemas = {
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
function generateUUID() {
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
function serializeCaseState(caseState) {
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
function caseGraphUri(caseId) {
  return `${YAWL_NS}case/${caseId}`;
}

// ============================================================================
// Event Appending - Helper Functions
// ============================================================================

/**
 * Validate and normalize event type
 * @param {string} eventType - Event type to validate
 * @returns {string} Normalized event type
 * @throws {Error} If event type is invalid
 */
function validateEventType(eventType) {
  if (!YAWL_EVENT_TYPES[eventType] && !Object.values(YAWL_EVENT_TYPES).includes(eventType)) {
    throw new Error(`Invalid YAWL event type: ${eventType}`);
  }
  return YAWL_EVENT_TYPES[eventType] || eventType;
}

/**
 * Validate event payload against schema
 * @param {string} eventType - Normalized event type
 * @param {Object} payload - Event payload to validate
 * @throws {Error} If payload validation fails
 */
function validateEventPayload(eventType, payload) {
  const schema = EventSchemas[eventType];
  if (schema) {
    const result = schema.safeParse(payload);
    if (!result.success) {
      throw new Error(`Invalid ${eventType} payload: ${result.error.message}`);
    }
  }
}

/**
 * Extract case ID from event payload
 * @param {Object} payload - Event payload
 * @param {Object} options - Event options
 * @returns {string} Case ID
 * @throws {Error} If case ID cannot be determined
 */
function extractCaseId(payload, options) {
  const caseId = payload.caseId || (payload.workItemId && options.caseId);
  if (!caseId) {
    throw new Error('caseId is required for workflow events');
  }
  return caseId;
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
  const normalizedType = validateEventType(eventType);
  validateEventPayload(normalizedType, payload);
  const caseId = extractCaseId(payload, options);

  const deltas = buildEventDeltas(normalizedType, payload, caseId);

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

// ============================================================================
// Event Delta Builders - Extracted per Event Type
// ============================================================================

/**
 * Build deltas for CASE_CREATED event
 * @param {Object} payload - Event payload
 * @param {string} caseId - Case UUID
 * @param {Object} caseSubject - RDF subject node
 * @returns {Array<Object>} Delta operations
 */
function buildCaseCreatedDeltas(payload, caseId, caseSubject) {
  return [
    {
      type: 'add',
      subject: caseSubject,
      predicate: dataFactory.namedNode(YAWL_PREDICATES.SPEC_ID),
      object: dataFactory.literal(payload.specId),
    },
    {
      type: 'add',
      subject: caseSubject,
      predicate: dataFactory.namedNode(YAWL_PREDICATES.STATE),
      object: dataFactory.literal('active'),
    },
    {
      type: 'add',
      subject: caseSubject,
      predicate: dataFactory.namedNode(YAWL_PREDICATES.CREATED_AT),
      object: dataFactory.literal(payload.timestamp),
    },
  ];
}

/**
 * Build deltas for TASK_ENABLED event
 * @param {Object} payload - Event payload
 * @param {string} caseId - Case UUID
 * @returns {Array<Object>} Delta operations
 */
function buildTaskEnabledDeltas(payload, caseId) {
  const workItemSubject = dataFactory.namedNode(`${YAWL_NS}workitem/${payload.workItemId}`);
  return [
    {
      type: 'add',
      subject: workItemSubject,
      predicate: dataFactory.namedNode(YAWL_PREDICATES.TASK_ID),
      object: dataFactory.literal(payload.taskId),
    },
    {
      type: 'add',
      subject: workItemSubject,
      predicate: dataFactory.namedNode(YAWL_PREDICATES.CASE_ID),
      object: dataFactory.namedNode(caseGraphUri(caseId)),
    },
    {
      type: 'add',
      subject: workItemSubject,
      predicate: dataFactory.namedNode(YAWL_PREDICATES.STATE),
      object: dataFactory.literal('enabled'),
    },
    {
      type: 'add',
      subject: workItemSubject,
      predicate: dataFactory.namedNode(YAWL_PREDICATES.ENABLED_AT),
      object: dataFactory.literal(payload.enabledAt),
    },
  ];
}

/**
 * Build deltas for TASK_STARTED event
 * @param {Object} payload - Event payload
 * @returns {Array<Object>} Delta operations
 */
function buildTaskStartedDeltas(payload) {
  const workItemSubject = dataFactory.namedNode(`${YAWL_NS}workitem/${payload.workItemId}`);
  return [
    {
      type: 'delete',
      subject: workItemSubject,
      predicate: dataFactory.namedNode(YAWL_PREDICATES.STATE),
      object: dataFactory.literal('enabled'),
    },
    {
      type: 'add',
      subject: workItemSubject,
      predicate: dataFactory.namedNode(YAWL_PREDICATES.STATE),
      object: dataFactory.literal('started'),
    },
    {
      type: 'add',
      subject: workItemSubject,
      predicate: dataFactory.namedNode(YAWL_PREDICATES.STARTED_AT),
      object: dataFactory.literal(payload.startedAt),
    },
  ];
}

/**
 * Build deltas for TASK_COMPLETED event
 * @param {Object} payload - Event payload
 * @returns {Array<Object>} Delta operations
 */
function buildTaskCompletedDeltas(payload) {
  const workItemSubject = dataFactory.namedNode(`${YAWL_NS}workitem/${payload.workItemId}`);
  const deltas = [
    {
      type: 'delete',
      subject: workItemSubject,
      predicate: dataFactory.namedNode(YAWL_PREDICATES.STATE),
      object: dataFactory.literal('started'),
    },
    {
      type: 'add',
      subject: workItemSubject,
      predicate: dataFactory.namedNode(YAWL_PREDICATES.STATE),
      object: dataFactory.literal('completed'),
    },
    {
      type: 'add',
      subject: workItemSubject,
      predicate: dataFactory.namedNode(YAWL_PREDICATES.COMPLETED_AT),
      object: dataFactory.literal(payload.completedAt),
    },
  ];

  if (payload.result !== undefined) {
    deltas.push({
      type: 'add',
      subject: workItemSubject,
      predicate: dataFactory.namedNode(YAWL_PREDICATES.RESULT),
      object: dataFactory.literal(JSON.stringify(payload.result)),
    });
  }

  return deltas;
}

/**
 * Build deltas for TASK_CANCELLED event
 * @param {Object} payload - Event payload
 * @returns {Array<Object>} Delta operations
 */
function buildTaskCancelledDeltas(payload) {
  const workItemSubject = dataFactory.namedNode(`${YAWL_NS}workitem/${payload.workItemId}`);
  return [
    {
      type: 'add',
      subject: workItemSubject,
      predicate: dataFactory.namedNode(YAWL_PREDICATES.STATE),
      object: dataFactory.literal('cancelled'),
    },
    {
      type: 'add',
      subject: workItemSubject,
      predicate: dataFactory.namedNode(YAWL_PREDICATES.CANCELLED_AT),
      object: dataFactory.literal(payload.cancelledAt),
    },
    {
      type: 'add',
      subject: workItemSubject,
      predicate: dataFactory.namedNode(YAWL_PREDICATES.REASON),
      object: dataFactory.literal(payload.reason),
    },
  ];
}

/**
 * Build deltas for WORK_ITEM_CREATED event
 * @param {Object} payload - Event payload
 * @param {string} caseId - Case UUID
 * @returns {Array<Object>} Delta operations
 */
function buildWorkItemCreatedDeltas(payload, caseId) {
  const workItemSubject = dataFactory.namedNode(`${YAWL_NS}workitem/${payload.workItemId}`);
  return [
    {
      type: 'add',
      subject: workItemSubject,
      predicate: dataFactory.namedNode(YAWL_PREDICATES.TASK_ID),
      object: dataFactory.literal(payload.taskId),
    },
    {
      type: 'add',
      subject: workItemSubject,
      predicate: dataFactory.namedNode(YAWL_PREDICATES.CASE_ID),
      object: dataFactory.namedNode(caseGraphUri(caseId)),
    },
    {
      type: 'add',
      subject: workItemSubject,
      predicate: dataFactory.namedNode(YAWL_PREDICATES.CREATED_AT),
      object: dataFactory.literal(payload.createdAt),
    },
  ];
}

/**
 * Build deltas for CONTROL_FLOW_EVALUATED event
 * @param {Object} payload - Event payload
 * @param {string} caseId - Case UUID
 * @returns {Array<Object>} Delta operations
 */
function buildControlFlowEvaluatedDeltas(payload, caseId) {
  const decisionSubject = dataFactory.namedNode(`${YAWL_NS}decision/${generateUUID()}`);
  return [
    {
      type: 'add',
      subject: decisionSubject,
      predicate: dataFactory.namedNode(YAWL_PREDICATES.CASE_ID),
      object: dataFactory.namedNode(caseGraphUri(caseId)),
    },
    {
      type: 'add',
      subject: decisionSubject,
      predicate: dataFactory.namedNode(YAWL_PREDICATES.TASK_ID),
      object: dataFactory.literal(payload.taskId),
    },
    {
      type: 'add',
      subject: decisionSubject,
      predicate: dataFactory.namedNode(YAWL_PREDICATES.RESULT),
      object: dataFactory.literal(String(payload.result)),
    },
    {
      type: 'add',
      subject: decisionSubject,
      predicate: dataFactory.namedNode(YAWL_PREDICATES.SPARQL_QUERY),
      object: dataFactory.literal(payload.sparqlQuery),
    },
  ];
}

/**
 * Build RDF deltas for a workflow event
 * @param {string} eventType - YAWL event type
 * @param {Object} payload - Event payload
 * @param {string} caseId - Case UUID
 * @returns {Array<Object>} Array of delta operations
 */
function buildEventDeltas(eventType, payload, caseId) {
  const caseSubject = dataFactory.namedNode(caseGraphUri(caseId));

  switch (eventType) {
    case YAWL_EVENT_TYPES.CASE_CREATED:
      return buildCaseCreatedDeltas(payload, caseId, caseSubject);
    case YAWL_EVENT_TYPES.TASK_ENABLED:
      return buildTaskEnabledDeltas(payload, caseId);
    case YAWL_EVENT_TYPES.TASK_STARTED:
      return buildTaskStartedDeltas(payload);
    case YAWL_EVENT_TYPES.TASK_COMPLETED:
      return buildTaskCompletedDeltas(payload);
    case YAWL_EVENT_TYPES.TASK_CANCELLED:
      return buildTaskCancelledDeltas(payload);
    case YAWL_EVENT_TYPES.WORK_ITEM_CREATED:
      return buildWorkItemCreatedDeltas(payload, caseId);
    case YAWL_EVENT_TYPES.CONTROL_FLOW_EVALUATED:
      return buildControlFlowEvaluatedDeltas(payload, caseId);
    default:
      return [];
  }
}

// ============================================================================
// Case Reconstruction - Helper Functions
// ============================================================================

/**
 * Query events from store up to target time
 * @param {import('@unrdf/kgc-4d').KGCStore} store - KGC-4D store instance
 * @param {bigint} targetTime - Target time in nanoseconds
 * @returns {Array<Object>} Array of time quads
 */
function queryEventTimeQuads(store, targetTime) {
  const eventLogGraph = dataFactory.namedNode(GRAPHS.EVENT_LOG);
  const tNsPredi = dataFactory.namedNode(PREDICATES.T_NS);
  return [...store.match(null, tNsPredi, null, eventLogGraph)];
}

/**
 * Filter and collect case events from time quads
 * @param {import('@unrdf/kgc-4d').KGCStore} store - KGC-4D store instance
 * @param {Array<Object>} timeQuads - Event time quads
 * @param {string} caseId - Case UUID to filter
 * @param {bigint} targetTime - Target time in nanoseconds
 * @returns {Array<Object>} Filtered case events
 */
function collectCaseEvents(store, timeQuads, caseId, targetTime) {
  const eventLogGraph = dataFactory.namedNode(GRAPHS.EVENT_LOG);
  const payloadPredi = dataFactory.namedNode(PREDICATES.PAYLOAD);
  const typePredi = dataFactory.namedNode(PREDICATES.TYPE);
  const caseEvents = [];

  for (const timeQuad of timeQuads) {
    const eventTime = BigInt(timeQuad.object.value);
    if (eventTime <= targetTime) {
      const payloadQuads = [...store.match(timeQuad.subject, payloadPredi, null, eventLogGraph)];
      const typeQuads = [...store.match(timeQuad.subject, typePredi, null, eventLogGraph)];

      if (payloadQuads.length > 0 && typeQuads.length > 0) {
        try {
          const payload = JSON.parse(payloadQuads[0].object.value);
          const eventType = typeQuads[0].object.value;

          if (payload.yawl_case_id === caseId || payload.caseId === caseId) {
            caseEvents.push({
              subject: timeQuad.subject,
              t_ns: eventTime,
              type: eventType,
              payload,
            });
          }
        } catch {
          continue;
        }
      }
    }
  }

  return caseEvents;
}

/**
 * Sort events chronologically
 * @param {Array<Object>} events - Events to sort
 * @returns {Array<Object>} Sorted events
 */
function sortEventsByTime(events) {
  return events.sort((a, b) => {
    if (a.t_ns < b.t_ns) return -1;
    if (a.t_ns > b.t_ns) return 1;
    return 0;
  });
}

/**
 * Create initial empty case state
 * @param {string} caseId - Case UUID
 * @returns {Object} Empty case state
 */
function createEmptyCaseState(caseId) {
  return {
    caseId,
    state: null,
    specId: null,
    createdAt: null,
    workItems: {},
    controlFlowDecisions: [],
    eventCount: 0,
  };
}

// ============================================================================
// Case Reconstruction (Time Travel)
// ============================================================================

/**
 * Reconstruct a case's state at a specific target time
 *
 * Uses KGC-4D reconstructState() and filters events to the specific case.
 * Replays all task transitions deterministically.
 * Returns full case state with work items, hash-verified.
 *
 * @param {import('@unrdf/kgc-4d').KGCStore} store - KGC-4D store instance
 * @param {import('@unrdf/kgc-4d').GitBackbone} gitBackbone - Git backbone for snapshots
 * @param {string} caseId - Case UUID to reconstruct
 * @param {bigint} targetTime - Target time in nanoseconds
 * @returns {Promise<Object>} Reconstructed case state at target time
 *
 * @example
 * const caseAtT = await reconstructCase(store, git, caseId, time_T);
 * console.log(caseAtT.state); // 'active'
 * console.log(caseAtT.workItems); // Work items at time T
 * console.log(caseAtT.stateHash); // Hash for verification
 */
export async function reconstructCase(store, gitBackbone, caseId, targetTime) {
  if (typeof targetTime !== 'bigint') {
    throw new TypeError('targetTime must be BigInt nanoseconds');
  }

  const allEventTimeQuads = queryEventTimeQuads(store, targetTime);
  const caseEvents = collectCaseEvents(store, allEventTimeQuads, caseId, targetTime);
  const sortedEvents = sortEventsByTime(caseEvents);
  const caseState = createEmptyCaseState(caseId);

  for (const event of sortedEvents) {
    replayEventToState(caseState, event);
  }

  const stateHash = await blake3(serializeCaseState(caseState));
  const workItemsArray = Object.values(caseState.workItems);

  return {
    caseId,
    state: caseState.state,
    specId: caseState.specId,
    createdAt: caseState.createdAt,
    workItems: workItemsArray,
    controlFlowDecisions: caseState.controlFlowDecisions,
    eventCount: caseState.eventCount,
    reconstructedAt: targetTime.toString(),
    reconstructedAt_iso: toISO(targetTime),
    stateHash,
    verified: true,
  };
}

// ============================================================================
// Event Replay - Extracted per Event Type
// ============================================================================

/**
 * Replay CASE_CREATED event
 * @param {Object} caseState - Mutable case state
 * @param {Object} payload - Event payload
 */
function replayCaseCreated(caseState, payload) {
  caseState.state = 'active';
  caseState.specId = payload.specId;
  caseState.createdAt = payload.timestamp;
}

/**
 * Replay TASK_ENABLED event
 * @param {Object} caseState - Mutable case state
 * @param {Object} payload - Event payload
 */
function replayTaskEnabled(caseState, payload) {
  caseState.workItems[payload.workItemId] = {
    workItemId: payload.workItemId,
    taskId: payload.taskId,
    state: 'enabled',
    enabledAt: payload.enabledAt,
  };
}

/**
 * Replay TASK_STARTED event
 * @param {Object} caseState - Mutable case state
 * @param {Object} payload - Event payload
 */
function replayTaskStarted(caseState, payload) {
  if (caseState.workItems[payload.workItemId]) {
    caseState.workItems[payload.workItemId].state = 'started';
    caseState.workItems[payload.workItemId].startedAt = payload.startedAt;
  }
}

/**
 * Replay TASK_COMPLETED event
 * @param {Object} caseState - Mutable case state
 * @param {Object} payload - Event payload
 */
function replayTaskCompleted(caseState, payload) {
  if (caseState.workItems[payload.workItemId]) {
    caseState.workItems[payload.workItemId].state = 'completed';
    caseState.workItems[payload.workItemId].completedAt = payload.completedAt;
    caseState.workItems[payload.workItemId].result = payload.result;
  }
}

/**
 * Replay TASK_CANCELLED event
 * @param {Object} caseState - Mutable case state
 * @param {Object} payload - Event payload
 */
function replayTaskCancelled(caseState, payload) {
  if (caseState.workItems[payload.workItemId]) {
    caseState.workItems[payload.workItemId].state = 'cancelled';
    caseState.workItems[payload.workItemId].cancelledAt = payload.cancelledAt;
    caseState.workItems[payload.workItemId].reason = payload.reason;
  }
}

/**
 * Replay WORK_ITEM_CREATED event
 * @param {Object} caseState - Mutable case state
 * @param {Object} payload - Event payload
 */
function replayWorkItemCreated(caseState, payload) {
  caseState.workItems[payload.workItemId] = {
    workItemId: payload.workItemId,
    taskId: payload.taskId,
    state: 'created',
    createdAt: payload.createdAt,
  };
}

/**
 * Replay CONTROL_FLOW_EVALUATED event
 * @param {Object} caseState - Mutable case state
 * @param {Object} payload - Event payload
 */
function replayControlFlowEvaluated(caseState, payload) {
  caseState.controlFlowDecisions.push({
    taskId: payload.taskId,
    result: payload.result,
    timestamp: payload.timestamp,
    sparqlQuery: payload.sparqlQuery,
  });
}

/**
 * Replay a single event to case state (pure function)
 * @param {Object} caseState - Mutable case state object
 * @param {Object} event - Event to replay
 */
function replayEventToState(caseState, event) {
  const { type, payload } = event;
  caseState.eventCount++;

  switch (type) {
    case YAWL_EVENT_TYPES.CASE_CREATED:
      replayCaseCreated(caseState, payload);
      break;
    case YAWL_EVENT_TYPES.TASK_ENABLED:
      replayTaskEnabled(caseState, payload);
      break;
    case YAWL_EVENT_TYPES.TASK_STARTED:
      replayTaskStarted(caseState, payload);
      break;
    case YAWL_EVENT_TYPES.TASK_COMPLETED:
      replayTaskCompleted(caseState, payload);
      break;
    case YAWL_EVENT_TYPES.TASK_CANCELLED:
      replayTaskCancelled(caseState, payload);
      break;
    case YAWL_EVENT_TYPES.WORK_ITEM_CREATED:
      replayWorkItemCreated(caseState, payload);
      break;
    case YAWL_EVENT_TYPES.CONTROL_FLOW_EVALUATED:
      replayControlFlowEvaluated(caseState, payload);
      break;
  }
}

// ============================================================================
// Receipt Generation - Helper Functions
// ============================================================================

/**
 * Calculate hashes for receipt
 * @param {Object} beforeState - State before transition
 * @param {Object} afterState - State after transition
 * @param {Object} decision - Decision data
 * @returns {Promise<Object>} Hash object with beforeHash, afterHash, decisionHash
 */
async function calculateReceiptHashes(beforeState, afterState, decision) {
  const beforeSerialized = serializeCaseState(beforeState);
  const afterSerialized = serializeCaseState(afterState);
  const decisionSerialized = serializeCaseState(decision);

  const [beforeHash, afterHash, decisionHash] = await Promise.all([
    blake3(beforeSerialized),
    blake3(afterSerialized),
    blake3(decisionSerialized),
  ]);

  return { beforeHash, afterHash, decisionHash };
}

/**
 * Build receipt object from hashes and justification
 * @param {Object} hashes - Hash object
 * @param {Object} justification - Justification details
 * @param {string} [gitRef] - Git reference
 * @returns {Object} Receipt object
 */
function buildReceiptObject(hashes, justification, gitRef) {
  const t_ns = now();

  return {
    beforeHash: hashes.beforeHash,
    afterHash: hashes.afterHash,
    hash: hashes.decisionHash,
    justification: {
      ...(justification.hookValidated && { hookValidated: justification.hookValidated }),
      ...(justification.conditionChecked && { conditionChecked: justification.conditionChecked }),
      ...(justification.sparqlQuery && { sparqlQuery: justification.sparqlQuery }),
      ...(justification.reasoning && { reasoning: justification.reasoning }),
    },
    ...(gitRef && { gitRef }),
    t_ns: t_ns.toString(),
    timestamp_iso: toISO(t_ns),
  };
}

// ============================================================================
// Receipt Generation
// ============================================================================

/**
 * Create a workflow receipt with cryptographic proofs
 *
 * Generates:
 * - Before state hash (from parent task completion)
 * - After state hash (after task enabled/completed)
 * - BLAKE3 hash of decision (task enabled or not)
 * - Justification: which hook validated, what condition checked
 * - Git ref from snapshot
 *
 * @param {Object} options - Receipt options
 * @param {Object} options.beforeState - State before the transition
 * @param {Object} options.afterState - State after the transition
 * @param {Object} options.decision - Decision data (event payload)
 * @param {Object} [options.justification] - Justification details
 * @param {string} [options.gitRef] - Git reference from snapshot
 * @returns {Promise<Object>} Receipt with all cryptographic proofs
 *
 * @example
 * const receipt = await createWorkflowReceipt({
 *   beforeState: { workItems: [] },
 *   afterState: { workItems: [{ id: 'w1', state: 'enabled' }] },
 *   decision: { taskEnabled: true, taskId: 'Approval' },
 *   justification: { hookValidated: 'pre-enable-hook', conditionChecked: 'has_manager' },
 * });
 */
export async function createWorkflowReceipt(options) {
  const {
    beforeState,
    afterState,
    decision,
    justification = {},
    gitRef,
  } = options;

  const hashes = await calculateReceiptHashes(beforeState, afterState, decision);
  const receipt = buildReceiptObject(hashes, justification, gitRef);
  const validated = ReceiptSchema.parse(receipt);

  return validated;
}

/**
 * Verify a workflow receipt by checking hash chain
 *
 * @param {Object} receipt - Receipt to verify
 * @param {Object} beforeState - State that should produce beforeHash
 * @param {Object} afterState - State that should produce afterHash
 * @param {Object} decision - Decision that should produce hash
 * @returns {Promise<Object>} Verification result
 */
export async function verifyWorkflowReceipt(receipt, beforeState, afterState, decision) {
  try {
    const hashes = await calculateReceiptHashes(beforeState, afterState, decision);

    const valid =
      hashes.beforeHash === receipt.beforeHash &&
      hashes.afterHash === receipt.afterHash &&
      hashes.decisionHash === receipt.hash;

    return {
      valid,
      verified: {
        beforeHash: hashes.beforeHash === receipt.beforeHash,
        afterHash: hashes.afterHash === receipt.afterHash,
        decisionHash: hashes.decisionHash === receipt.hash,
      },
      receipt,
    };
  } catch (error) {
    return {
      valid: false,
      error: error.message,
      receipt,
    };
  }
}

// ============================================================================
// Audit Trail - Helper Functions
// ============================================================================

/**
 * Get RDF predicates for audit trail querying
 * @returns {Object} Predicate nodes
 */
function getAuditTrailPredicates() {
  return {
    eventLogGraph: dataFactory.namedNode(GRAPHS.EVENT_LOG),
    tNsPredi: dataFactory.namedNode(PREDICATES.T_NS),
    payloadPredi: dataFactory.namedNode(PREDICATES.PAYLOAD),
    typePredi: dataFactory.namedNode(PREDICATES.TYPE),
    gitRefPredi: dataFactory.namedNode(PREDICATES.GIT_REF),
  };
}

/**
 * Process a single time quad to extract event data
 * @param {import('@unrdf/kgc-4d').KGCStore} store - KGC-4D store instance
 * @param {Object} timeQuad - Time quad to process
 * @param {string} caseId - Case UUID
 * @param {Object} predicates - RDF predicates
 * @returns {Object|null} Event data or null if not a case event
 */
function processTimeQuadForEvent(store, timeQuad, caseId, predicates) {
  const eventTime = BigInt(timeQuad.object.value);
  const payloadQuads = [...store.match(timeQuad.subject, predicates.payloadPredi, null, predicates.eventLogGraph)];
  const typeQuads = [...store.match(timeQuad.subject, predicates.typePredi, null, predicates.eventLogGraph)];
  const gitRefQuads = [...store.match(timeQuad.subject, predicates.gitRefPredi, null, predicates.eventLogGraph)];

  if (payloadQuads.length === 0 || typeQuads.length === 0) {
    return null;
  }

  try {
    const payload = JSON.parse(payloadQuads[0].object.value);
    const eventType = typeQuads[0].object.value;
    const gitRef = gitRefQuads.length > 0 ? gitRefQuads[0].object.value : null;

    if (payload.yawl_case_id !== caseId && payload.caseId !== caseId) {
      return null;
    }

    return {
      eventId: timeQuad.subject.value.split('/').pop(),
      t_ns: eventTime.toString(),
      timestamp_iso: toISO(eventTime),
      type: eventType,
      payload,
      gitRef,
    };
  } catch {
    return null;
  }
}

/**
 * Extract receipts and SPARQL queries from event
 * @param {Object} event - Event object
 * @param {Array<Object>} receipts - Receipts array to populate
 * @param {Array<Object>} sparqlQueries - SPARQL queries array to populate
 */
function extractEventMetadata(event, receipts, sparqlQueries) {
  if (event.payload.receipt) {
    receipts.push({
      eventId: event.eventId,
      ...event.payload.receipt,
    });
  }

  if (event.type === YAWL_EVENT_TYPES.CONTROL_FLOW_EVALUATED && event.payload.sparqlQuery) {
    sparqlQueries.push({
      eventId: event.eventId,
      taskId: event.payload.taskId,
      result: event.payload.result,
      timestamp: event.payload.timestamp,
      query: event.payload.sparqlQuery,
    });
  }
}

/**
 * Collect audit trail events for a case
 * @param {import('@unrdf/kgc-4d').KGCStore} store - KGC-4D store instance
 * @param {Array<Object>} timeQuads - Event time quads
 * @param {string} caseId - Case UUID
 * @param {Object} predicates - RDF predicates
 * @returns {Object} Collected events, receipts, and SPARQL queries
 */
function collectAuditTrailData(store, timeQuads, caseId, predicates) {
  const events = [];
  const receipts = [];
  const sparqlQueries = [];

  for (const timeQuad of timeQuads) {
    const event = processTimeQuadForEvent(store, timeQuad, caseId, predicates);
    if (event) {
      events.push(event);
      extractEventMetadata(event, receipts, sparqlQueries);
    }
  }

  return { events, receipts, sparqlQueries };
}

// ============================================================================
// Audit Trail
// ============================================================================

/**
 * Get the complete workflow audit trail for a case
 *
 * Queries EventLog for all events for the case.
 * Returns chronological list with receipts.
 * Includes SPARQL queries that determined routing.
 * Fully reproducible and verifiable.
 *
 * @param {import('@unrdf/kgc-4d').KGCStore} store - KGC-4D store instance
 * @param {string} caseId - Case UUID
 * @returns {Promise<Object>} Audit trail with events, receipts, and verification
 *
 * @example
 * const audit = await getWorkflowAuditTrail(store, caseId);
 * console.log(audit.events); // Chronological event list
 * console.log(audit.receipts); // All receipts for verification
 * console.log(audit.sparqlQueries); // Control flow queries
 */
export async function getWorkflowAuditTrail(store, caseId) {
  const predicates = getAuditTrailPredicates();
  const allEventTimeQuads = [...store.match(null, predicates.tNsPredi, null, predicates.eventLogGraph)];
  const { events, receipts, sparqlQueries } = collectAuditTrailData(store, allEventTimeQuads, caseId, predicates);

  events.sort((a, b) => {
    const aTime = BigInt(a.t_ns);
    const bTime = BigInt(b.t_ns);
    if (aTime < bTime) return -1;
    if (aTime > bTime) return 1;
    return 0;
  });

  const auditHash = await blake3(serializeCaseState({
    caseId,
    events: events.map((e) => ({ id: e.eventId, type: e.type, t_ns: e.t_ns })),
    eventCount: events.length,
  }));

  return {
    caseId,
    events,
    receipts,
    sparqlQueries,
    eventCount: events.length,
    auditHash,
    exportedAt: toISO(now()),
    reproducible: true,
    verifiable: true,
  };
}

// ============================================================================
// High-Level Workflow Functions
// ============================================================================

/**
 * Create a new workflow case
 *
 * @param {import('@unrdf/kgc-4d').KGCStore} store - KGC-4D store instance
 * @param {string} specId - Workflow specification ID
 * @param {Object} [options={}] - Additional options
 * @returns {Promise<Object>} Created case with ID and receipt
 *
 * @example
 * const case1 = await createCase(store, 'approval-workflow');
 * console.log(case1.caseId);
 * console.log(case1.receipt);
 */
export async function createCase(store, specId, options = {}) {
  const caseId = generateUUID();
  const timestamp = toISO(now());

  // Create receipt for case creation
  const receipt = await createWorkflowReceipt({
    beforeState: { empty: true },
    afterState: { caseId, specId, state: 'active' },
    decision: { action: 'create_case', specId },
    justification: {
      reasoning: 'New workflow case instantiated',
    },
    gitRef: options.gitRef,
  });

  // Append event
  const eventReceipt = await appendWorkflowEvent(store, 'CASE_CREATED', {
    caseId,
    specId,
    timestamp,
    receipt,
  }, options);

  return {
    caseId,
    specId,
    state: 'active',
    createdAt: timestamp,
    receipt,
    eventReceipt,
  };
}

/**
 * Enable a task for a case (creates work item)
 *
 * @param {import('@unrdf/kgc-4d').KGCStore} store - KGC-4D store instance
 * @param {string} caseId - Case UUID
 * @param {string} taskId - Task identifier
 * @param {Object} [options={}] - Additional options
 * @returns {Promise<Object>} Work item with ID and receipt
 */
export async function enableTask(store, caseId, taskId, options = {}) {
  const workItemId = generateUUID();
  const enabledAt = toISO(now());

  // Create receipt
  const receipt = await createWorkflowReceipt({
    beforeState: options.beforeState || { caseId },
    afterState: { caseId, workItemId, taskId, state: 'enabled' },
    decision: { action: 'enable_task', taskId, caseId },
    justification: options.justification || {
      reasoning: `Task ${taskId} enabled`,
    },
    gitRef: options.gitRef,
  });

  // Append event
  const eventReceipt = await appendWorkflowEvent(store, 'TASK_ENABLED', {
    taskId,
    caseId,
    workItemId,
    enabledAt,
    receipt,
  }, options);

  return {
    workItemId,
    taskId,
    caseId,
    state: 'enabled',
    enabledAt,
    receipt,
    eventReceipt,
  };
}

/**
 * Start a work item (begin task execution)
 *
 * @param {import('@unrdf/kgc-4d').KGCStore} store - KGC-4D store instance
 * @param {string} workItemId - Work item UUID
 * @param {string} caseId - Case UUID (for event context)
 * @param {Object} [options={}] - Additional options
 * @returns {Promise<Object>} Updated work item with receipt
 */
export async function startWorkItem(store, workItemId, caseId, options = {}) {
  const startedAt = toISO(now());

  // Create receipt
  const receipt = await createWorkflowReceipt({
    beforeState: options.beforeState || { workItemId, state: 'enabled' },
    afterState: { workItemId, state: 'started' },
    decision: { action: 'start_work_item', workItemId },
    justification: options.justification || {
      reasoning: `Work item ${workItemId} started`,
    },
    gitRef: options.gitRef,
  });

  // Append event
  const eventReceipt = await appendWorkflowEvent(store, 'TASK_STARTED', {
    workItemId,
    startedAt,
    receipt,
  }, { ...options, caseId });

  return {
    workItemId,
    state: 'started',
    startedAt,
    receipt,
    eventReceipt,
  };
}

/**
 * Complete a work item with result
 *
 * @param {import('@unrdf/kgc-4d').KGCStore} store - KGC-4D store instance
 * @param {string} workItemId - Work item UUID
 * @param {string} caseId - Case UUID (for event context)
 * @param {*} result - Task result data
 * @param {Object} [options={}] - Additional options
 * @returns {Promise<Object>} Completed work item with receipt
 */
export async function completeWorkItem(store, workItemId, caseId, result, options = {}) {
  const completedAt = toISO(now());

  // Create receipt
  const receipt = await createWorkflowReceipt({
    beforeState: options.beforeState || { workItemId, state: 'started' },
    afterState: { workItemId, state: 'completed', result },
    decision: { action: 'complete_work_item', workItemId, result },
    justification: options.justification || {
      reasoning: `Work item ${workItemId} completed`,
    },
    gitRef: options.gitRef,
  });

  // Append event
  const eventReceipt = await appendWorkflowEvent(store, 'TASK_COMPLETED', {
    workItemId,
    completedAt,
    result,
    receipt,
  }, { ...options, caseId });

  return {
    workItemId,
    state: 'completed',
    completedAt,
    result,
    receipt,
    eventReceipt,
  };
}

/**
 * Record a control flow evaluation
 *
 * @param {import('@unrdf/kgc-4d').KGCStore} store - KGC-4D store instance
 * @param {string} caseId - Case UUID
 * @param {string} taskId - Task being evaluated
 * @param {boolean} result - Evaluation result
 * @param {string} sparqlQuery - SPARQL query used for evaluation
 * @param {Object} [options={}] - Additional options
 * @returns {Promise<Object>} Control flow decision with receipt
 */
export async function recordControlFlowEvaluation(store, caseId, taskId, result, sparqlQuery, options = {}) {
  const timestamp = toISO(now());

  // Create receipt
  const receipt = await createWorkflowReceipt({
    beforeState: options.beforeState || { caseId, taskId },
    afterState: { caseId, taskId, evaluated: true, result },
    decision: { action: 'evaluate_control_flow', taskId, result, sparqlQuery },
    justification: {
      sparqlQuery,
      conditionChecked: options.conditionChecked || taskId,
      reasoning: `Control flow for ${taskId}: ${result ? 'enabled' : 'blocked'}`,
    },
    gitRef: options.gitRef,
  });

  // Append event
  const eventReceipt = await appendWorkflowEvent(store, 'CONTROL_FLOW_EVALUATED', {
    caseId,
    taskId,
    result,
    timestamp,
    sparqlQuery,
    receipt,
  }, options);

  return {
    caseId,
    taskId,
    result,
    timestamp,
    sparqlQuery,
    receipt,
    eventReceipt,
  };
}

// ============================================================================
// Exports
// ============================================================================

export default {
  // Event types
  YAWL_EVENT_TYPES,
  YAWL_NS,
  YAWL_PREDICATES,

  // Core functions
  appendWorkflowEvent,
  reconstructCase,
  createWorkflowReceipt,
  verifyWorkflowReceipt,
  getWorkflowAuditTrail,

  // High-level workflow functions
  createCase,
  enableTask,
  startWorkItem,
  completeWorkItem,
  recordControlFlowEvaluation,
};
