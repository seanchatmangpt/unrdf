/**
 * YAWL Receipt Core - Schemas, Utilities, and Core Receipt Functions
 *
 * @module @unrdf/yawl/receipt-core
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';
import { now, toISO } from '@unrdf/kgc-4d';

// =============================================================================
// Constants
// =============================================================================

/**
 * BLAKE3 hash length in hex characters
 * @constant {number}
 */
export const BLAKE3_HEX_LENGTH = 64;

/**
 * Supported event types for receipts
 * @readonly
 * @enum {string}
 */
export const RECEIPT_EVENT_TYPES = Object.freeze({
  CASE_CREATED: 'CASE_CREATED',
  TASK_ENABLED: 'TASK_ENABLED',
  TASK_STARTED: 'TASK_STARTED',
  TASK_COMPLETED: 'TASK_COMPLETED',
  TASK_CANCELLED: 'TASK_CANCELLED',
  TASK_FAILED: 'TASK_FAILED',
  TASK_TIMEOUT: 'TASK_TIMEOUT',
  WORK_ITEM_CREATED: 'WORK_ITEM_CREATED',
  CONTROL_FLOW_EVALUATED: 'CONTROL_FLOW_EVALUATED',
  RESOURCE_ALLOCATED: 'RESOURCE_ALLOCATED',
  RESOURCE_RELEASED: 'RESOURCE_RELEASED',
});

// =============================================================================
// Zod Schemas
// =============================================================================

/**
 * Justification payload schema - explains why the decision was made
 */
const JustificationSchema = z.object({
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
 * Payload schema - the decision data being receipted
 */
const PayloadSchema = z.object({
  /** The decision made (e.g., 'APPROVE', 'ENABLE', 'COMPLETE') */
  decision: z.string(),
  /** Justification for the decision */
  justification: JustificationSchema.optional(),
  /** Actor who made the decision */
  actor: z.string().optional(),
  /** Additional context data - using any for maximum flexibility */
  context: z.any().optional(),
}).passthrough();

/**
 * Vector clock schema for causality tracking
 * Using passthrough to allow flexible VectorClock JSON serialization
 */
const VectorClockSchema = z.object({
  nodeId: z.string().min(1),
  counters: z.record(z.string(), z.string()),
}).passthrough();

/**
 * Event type schema using enum values
 */
const EventTypeSchema = z.enum([
  'CASE_CREATED',
  'TASK_ENABLED',
  'TASK_STARTED',
  'TASK_COMPLETED',
  'TASK_CANCELLED',
  'TASK_FAILED',
  'TASK_TIMEOUT',
  'WORK_ITEM_CREATED',
  'CONTROL_FLOW_EVALUATED',
  'RESOURCE_ALLOCATED',
  'RESOURCE_RELEASED',
]);

/**
 * Full receipt schema
 */
export const ReceiptSchema = z.object({
  // Identity
  id: z.string().uuid(),
  eventType: EventTypeSchema,

  // Timestamps
  t_ns: z.bigint(),
  timestamp_iso: z.string(),

  // Workflow context
  caseId: z.string().min(1),
  taskId: z.string().min(1),
  workItemId: z.string().optional(),

  // Cryptographic proof chain
  previousReceiptHash: z.string().length(BLAKE3_HEX_LENGTH).nullable(),
  payloadHash: z.string().length(BLAKE3_HEX_LENGTH),
  receiptHash: z.string().length(BLAKE3_HEX_LENGTH),

  // KGC-4D integration
  kgcEventId: z.string().optional(),
  gitRef: z.string().optional(),
  vectorClock: VectorClockSchema.optional(),

  // Decision payload
  payload: PayloadSchema,
});

/**
 * Verification result schema
 */
const VerificationResultSchema = z.object({
  valid: z.boolean(),
  error: z.string().optional(),
  checks: z.object({
    payloadHashValid: z.boolean(),
    chainHashValid: z.boolean(),
    timestampValid: z.boolean(),
  }).optional(),
});

// Export for testing/external use
export { JustificationSchema, PayloadSchema, VectorClockSchema, VerificationResultSchema };

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
 * @typedef {Object} Payload
 * @property {string} decision - The decision made
 * @property {Justification} [justification] - Justification for decision
 * @property {string} [actor] - Actor who made decision
 * @property {Object} [context] - Additional context
 */

/**
 * @typedef {Object} Receipt
 * @property {string} id - UUID of receipt
 * @property {string} eventType - Type of event (TASK_ENABLED, etc.)
 * @property {bigint} t_ns - Nanosecond timestamp
 * @property {string} timestamp_iso - ISO timestamp string
 * @property {string} caseId - Workflow case ID
 * @property {string} taskId - Task ID
 * @property {string} [workItemId] - Work item ID
 * @property {string|null} previousReceiptHash - 64-char BLAKE3 hash
 * @property {string} payloadHash - 64-char BLAKE3 hash
 * @property {string} receiptHash - 64-char BLAKE3 hash
 * @property {string} [kgcEventId] - KGC event ID
 * @property {string} [gitRef] - Git reference
 * @property {Object} [vectorClock] - Vector clock for causality
 * @property {Payload} payload - Decision payload
 */

/**
 * @typedef {Object} VerificationResult
 * @property {boolean} valid - Whether receipt is valid
 * @property {string} [error] - Error message if invalid
 * @property {Object} [checks] - Individual check results
 */

/**
 * @typedef {Object} ReceiptEvent
 * @property {string} eventType - Event type
 * @property {string} caseId - Case ID
 * @property {string} taskId - Task ID
 * @property {string} [workItemId] - Work item ID
 * @property {Payload} payload - Event payload
 * @property {string} [kgcEventId] - KGC event ID
 * @property {string} [gitRef] - Git reference
 * @property {Object} [vectorClock] - Vector clock
 */

// =============================================================================
// Utility Functions
// =============================================================================

/**
 * Generate a UUID v4
 * @returns {string} UUID string
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
 * Serialize object deterministically for hashing
 * Keys are sorted alphabetically at all levels
 *
 * @param {Object} obj - Object to serialize
 * @returns {string} Deterministic JSON string
 */
export function deterministicSerialize(obj) {
  if (obj === null || obj === undefined) {
    return JSON.stringify(null);
  }

  if (typeof obj === 'bigint') {
    return obj.toString();
  }

  if (typeof obj !== 'object') {
    return JSON.stringify(obj);
  }

  if (Array.isArray(obj)) {
    const items = obj.map((item) => deterministicSerialize(item));
    return `[${items.join(',')}]`;
  }

  // Sort keys alphabetically for deterministic ordering
  const sortedKeys = Object.keys(obj).sort();
  const pairs = sortedKeys.map((key) => {
    const value = obj[key];
    const serializedValue = deterministicSerialize(value);
    return `${JSON.stringify(key)}:${serializedValue}`;
  });

  return `{${pairs.join(',')}}`;
}

/**
 * Compute BLAKE3 hash of data
 *
 * @param {string|Object} data - Data to hash
 * @returns {Promise<string>} 64-character hex hash
 */
export async function computeBlake3(data) {
  const serialized = typeof data === 'string' ? data : deterministicSerialize(data);
  return blake3(serialized);
}

/**
 * Compute chained receipt hash from previousHash and payloadHash
 * Chain format: previousHash:payloadHash
 *
 * @param {string|null} previousHash - Previous receipt hash (null for genesis)
 * @param {string} payloadHash - Current payload hash
 * @returns {Promise<string>} 64-character hex hash
 */
export async function computeChainHash(previousHash, payloadHash) {
  const chainInput = `${previousHash || 'GENESIS'}:${payloadHash}`;
  return blake3(chainInput);
}

// =============================================================================
// Receipt Generation
// =============================================================================

/**
 * Generate a cryptographic receipt for a workflow event
 *
 * Creates a BLAKE3 receipt that chains to the previous receipt,
 * forming an immutable audit trail.
 *
 * @param {ReceiptEvent} event - The event to receipt
 * @param {Receipt|null} [previousReceipt=null] - Previous receipt for chaining
 * @returns {Promise<Receipt>} Complete receipt with cryptographic proofs
 *
 * @example
 * const receipt = await generateReceipt({
 *   eventType: 'TASK_ENABLED',
 *   caseId: 'case-123',
 *   taskId: 'Approval',
 *   workItemId: 'wi-456',
 *   payload: {
 *     decision: 'ENABLE',
 *     justification: {
 *       hookValidated: 'pre-enable-hook',
 *       sparqlQuery: 'ASK { ?case :hasManager ?m }',
 *       reasoning: 'All preconditions satisfied',
 *     },
 *     actor: 'system',
 *   },
 *   kgcEventId: 'evt-789',
 *   gitRef: 'abc123',
 * });
 *
 * @example
 * // Chain receipts
 * const r1 = await generateReceipt(event1, null);
 * const r2 = await generateReceipt(event2, r1);
 * console.log(r2.previousReceiptHash === r1.receiptHash); // true
 */
export async function generateReceipt(event, previousReceipt = null) {
  // 1. Generate receipt ID and timestamp
  const id = generateUUID();
  const t_ns = now();
  const timestamp_iso = toISO(t_ns);

  // 2. Validate event type
  if (!Object.values(RECEIPT_EVENT_TYPES).includes(event.eventType)) {
    throw new Error(`Invalid event type: ${event.eventType}`);
  }

  // 3. Serialize payload deterministically and compute hash
  const payloadToHash = {
    eventType: event.eventType,
    caseId: event.caseId,
    taskId: event.taskId,
    workItemId: event.workItemId || null,
    payload: event.payload,
    t_ns: t_ns.toString(),
  };
  const payloadHash = await computeBlake3(payloadToHash);

  // 4. Get previous receipt hash for chain
  const previousReceiptHash = previousReceipt ? previousReceipt.receiptHash : null;

  // 5. Compute chained receipt hash
  const receiptHash = await computeChainHash(previousReceiptHash, payloadHash);

  // 6. Build complete receipt
  const receipt = {
    id,
    eventType: event.eventType,
    t_ns,
    timestamp_iso,
    caseId: event.caseId,
    taskId: event.taskId,
    workItemId: event.workItemId || undefined,
    previousReceiptHash,
    payloadHash,
    receiptHash,
    kgcEventId: event.kgcEventId || undefined,
    gitRef: event.gitRef || undefined,
    vectorClock: event.vectorClock || undefined,
    payload: event.payload,
  };

  // 7. Validate receipt against schema
  return ReceiptSchema.parse(receipt);
}

// =============================================================================
// Receipt Verification
// =============================================================================

/**
 * Verify a receipt's cryptographic integrity
 *
 * Recomputes hashes and verifies the chain link.
 *
 * @param {Receipt} receipt - Receipt to verify
 * @returns {Promise<VerificationResult>} Verification result
 *
 * @example
 * const result = await verifyReceipt(receipt);
 * if (!result.valid) {
 *   console.error('Receipt verification failed:', result.error);
 * }
 */
export async function verifyReceipt(receipt) {
  try {
    // 1. Recompute payload hash
    const payloadToHash = {
      eventType: receipt.eventType,
      caseId: receipt.caseId,
      taskId: receipt.taskId,
      workItemId: receipt.workItemId || null,
      payload: receipt.payload,
      t_ns: receipt.t_ns.toString(),
    };
    const computedPayloadHash = await computeBlake3(payloadToHash);
    const payloadHashValid = computedPayloadHash === receipt.payloadHash;

    // 2. Recompute chain hash
    const computedReceiptHash = await computeChainHash(
      receipt.previousReceiptHash,
      receipt.payloadHash
    );
    const chainHashValid = computedReceiptHash === receipt.receiptHash;

    // 3. Validate timestamp (must be positive and reasonable)
    const timestampValid = receipt.t_ns > 0n;

    // 4. Determine overall validity
    const valid = payloadHashValid && chainHashValid && timestampValid;

    if (!valid) {
      const errors = [];
      if (!payloadHashValid) errors.push('payload hash mismatch');
      if (!chainHashValid) errors.push('chain hash mismatch');
      if (!timestampValid) errors.push('invalid timestamp');

      return {
        valid: false,
        error: `Verification failed: ${errors.join(', ')}`,
        checks: {
          payloadHashValid,
          chainHashValid,
          timestampValid,
        },
      };
    }

    return {
      valid: true,
      checks: {
        payloadHashValid,
        chainHashValid,
        timestampValid,
      },
    };
  } catch (error) {
    return {
      valid: false,
      error: `Verification error: ${error.message}`,
    };
  }
}

/**
 * Verify a receipt chain link
 *
 * @param {Receipt} current - Current receipt
 * @param {Receipt} previous - Previous receipt in chain
 * @returns {Promise<VerificationResult>} Chain verification result
 */
export async function verifyChainLink(current, previous) {
  // Verify current receipt independently
  const currentResult = await verifyReceipt(current);
  if (!currentResult.valid) {
    return currentResult;
  }

  // Verify previous receipt independently
  const previousResult = await verifyReceipt(previous);
  if (!previousResult.valid) {
    return {
      valid: false,
      error: `Previous receipt invalid: ${previousResult.error}`,
    };
  }

  // Verify chain link
  if (current.previousReceiptHash !== previous.receiptHash) {
    return {
      valid: false,
      error: `Chain broken: expected ${previous.receiptHash}, got ${current.previousReceiptHash}`,
    };
  }

  // Verify temporal ordering
  if (current.t_ns <= previous.t_ns) {
    return {
      valid: false,
      error: 'Temporal ordering violated: current receipt timestamp not after previous',
    };
  }

  return { valid: true };
}
