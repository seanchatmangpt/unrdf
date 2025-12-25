/**
 * YAWL Receipt - BLAKE3 Cryptographic Proof of Workflow Transitions
 *
 * Implements cryptographic receipts for audit trail and verification.
 * Each receipt chains to the previous, creating an immutable proof chain.
 *
 * @module @unrdf/yawl/receipt
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';
import { now, toISO, VectorClock } from '@unrdf/kgc-4d';

// =============================================================================
// Constants
// =============================================================================

/**
 * BLAKE3 hash length in hex characters
 * @constant {number}
 */
const BLAKE3_HEX_LENGTH = 64;

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
  hookValidated: z.string().nullable().optional(),
  /** SPARQL query used for control flow evaluation */
  sparqlQuery: z.string().nullable().optional(),
  /** Human-readable reasoning */
  reasoning: z.string().nullable().optional(),
  /** Condition that was checked */
  conditionChecked: z.string().nullable().optional(),
  /** Actor who approved (for manual tasks) */
  approvedBy: z.string().nullable().optional(),
});

/**
 * Payload schema - the decision data being receipted
 */
const PayloadSchema = z.object({
  /** The decision made (e.g., 'APPROVE', 'ENABLE', 'COMPLETE') */
  decision: z.string().optional(),
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
  kgcEventId: z.string().nullable().optional(),
  gitRef: z.string().nullable().optional(),
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
 * Serialize object deterministically for hashing
 * Keys are sorted alphabetically at all levels
 *
 * @param {Object} obj - Object to serialize
 * @returns {string} Deterministic JSON string
 */
function deterministicSerialize(obj) {
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
async function computeBlake3(data) {
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
async function computeChainHash(previousHash, payloadHash) {
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

// =============================================================================
// ProofChain Class
// =============================================================================

/**
 * ProofChain - Manages a chain of receipts with Merkle root computation
 *
 * Provides append, verification, and Merkle root computation for
 * a sequence of cryptographic receipts.
 *
 * @example
 * const chain = new ProofChain('node-1');
 *
 * // Append receipts
 * await chain.append(await generateReceipt(event1, null));
 * await chain.append(await generateReceipt(event2, chain.getLatest()));
 *
 * // Verify entire chain
 * const result = await chain.verify();
 * console.log(result.valid); // true
 *
 * // Get Merkle root
 * const root = await chain.getMerkleRoot();
 * console.log(root); // 64-char hex hash
 */
export class ProofChain {
  /**
   * Create a new proof chain
   *
   * @param {string} nodeId - Node identifier for vector clock
   */
  constructor(nodeId) {
    if (!nodeId || typeof nodeId !== 'string') {
      throw new TypeError('ProofChain requires a string nodeId');
    }

    /** @type {string} */
    this.nodeId = nodeId;

    /** @type {Receipt[]} */
    this.receipts = [];

    /** @type {VectorClock} */
    this.vectorClock = new VectorClock(nodeId);

    /** @type {string|null} */
    this._merkleRoot = null;

    /** @type {boolean} */
    this._dirty = false;
  }

  /**
   * Get the number of receipts in the chain
   * @returns {number}
   */
  get length() {
    return this.receipts.length;
  }

  /**
   * Get the latest receipt in the chain
   * @returns {Receipt|null}
   */
  getLatest() {
    return this.receipts.length > 0 ? this.receipts[this.receipts.length - 1] : null;
  }

  /**
   * Append a receipt to the chain
   *
   * Validates that the receipt properly chains to the previous receipt.
   *
   * @param {Receipt} receipt - Receipt to append
   * @returns {Promise<void>}
   * @throws {Error} If receipt doesn't chain properly
   */
  async append(receipt) {
    // Validate receipt independently
    const verifyResult = await verifyReceipt(receipt);
    if (!verifyResult.valid) {
      throw new Error(`Cannot append invalid receipt: ${verifyResult.error}`);
    }

    // If not first receipt, validate chain link
    if (this.receipts.length > 0) {
      const latest = this.getLatest();
      if (receipt.previousReceiptHash !== latest.receiptHash) {
        throw new Error(
          `Chain broken: expected previousReceiptHash ${latest.receiptHash}, got ${receipt.previousReceiptHash}`
        );
      }
      if (receipt.t_ns <= latest.t_ns) {
        throw new Error('Temporal ordering violated: receipt timestamp must be after latest');
      }
    } else {
      // First receipt must have null previousReceiptHash
      if (receipt.previousReceiptHash !== null) {
        throw new Error('Genesis receipt must have null previousReceiptHash');
      }
    }

    // Increment vector clock
    this.vectorClock.increment();

    // Add receipt
    this.receipts.push(receipt);
    this._dirty = true;
    this._merkleRoot = null;
  }

  /**
   * Verify the entire chain
   *
   * Validates all receipts and chain links.
   *
   * @returns {Promise<VerificationResult>}
   */
  async verify() {
    if (this.receipts.length === 0) {
      return { valid: true };
    }

    // Verify first receipt (genesis)
    const firstResult = await verifyReceipt(this.receipts[0]);
    if (!firstResult.valid) {
      return {
        valid: false,
        error: `Receipt 0 invalid: ${firstResult.error}`,
      };
    }

    if (this.receipts[0].previousReceiptHash !== null) {
      return {
        valid: false,
        error: 'Genesis receipt must have null previousReceiptHash',
      };
    }

    // Verify chain links
    for (let i = 1; i < this.receipts.length; i++) {
      const linkResult = await verifyChainLink(this.receipts[i], this.receipts[i - 1]);
      if (!linkResult.valid) {
        return {
          valid: false,
          error: `Chain link ${i - 1} -> ${i} invalid: ${linkResult.error}`,
        };
      }
    }

    return { valid: true };
  }

  /**
   * Compute the Merkle root of all receipt hashes
   *
   * Uses a binary Merkle tree with BLAKE3 for internal nodes.
   *
   * @returns {Promise<string>} 64-character hex Merkle root
   */
  async getMerkleRoot() {
    if (this._merkleRoot && !this._dirty) {
      return this._merkleRoot;
    }

    if (this.receipts.length === 0) {
      // Empty tree has zero hash
      this._merkleRoot = '0'.repeat(BLAKE3_HEX_LENGTH);
      this._dirty = false;
      return this._merkleRoot;
    }

    // Build Merkle tree from receipt hashes
    let level = this.receipts.map((r) => r.receiptHash);

    while (level.length > 1) {
      const nextLevel = [];
      for (let i = 0; i < level.length; i += 2) {
        if (i + 1 < level.length) {
          // Hash pair of nodes
          const combined = `${level[i]}:${level[i + 1]}`;
          nextLevel.push(await blake3(combined));
        } else {
          // Odd node promoted to next level
          nextLevel.push(level[i]);
        }
      }
      level = nextLevel;
    }

    this._merkleRoot = level[0];
    this._dirty = false;
    return this._merkleRoot;
  }

  /**
   * Get receipt by index
   *
   * @param {number} index - Receipt index
   * @returns {Receipt|undefined}
   */
  getReceipt(index) {
    return this.receipts[index];
  }

  /**
   * Get receipt by ID
   *
   * @param {string} id - Receipt UUID
   * @returns {Receipt|undefined}
   */
  getReceiptById(id) {
    return this.receipts.find((r) => r.id === id);
  }

  /**
   * Get receipts for a specific case
   *
   * @param {string} caseId - Case ID
   * @returns {Receipt[]}
   */
  getReceiptsForCase(caseId) {
    return this.receipts.filter((r) => r.caseId === caseId);
  }

  /**
   * Get receipts for a specific task
   *
   * @param {string} taskId - Task ID
   * @returns {Receipt[]}
   */
  getReceiptsForTask(taskId) {
    return this.receipts.filter((r) => r.taskId === taskId);
  }

  /**
   * Get the proof path for a receipt (Merkle proof)
   *
   * @param {number} index - Receipt index
   * @returns {Promise<string[]>} Array of sibling hashes for proof
   */
  async getMerkleProof(index) {
    if (index < 0 || index >= this.receipts.length) {
      throw new RangeError(`Index ${index} out of range [0, ${this.receipts.length})`);
    }

    const proof = [];
    let level = this.receipts.map((r) => r.receiptHash);
    let idx = index;

    while (level.length > 1) {
      // Get sibling index
      const siblingIdx = idx % 2 === 0 ? idx + 1 : idx - 1;

      if (siblingIdx < level.length) {
        proof.push({
          hash: level[siblingIdx],
          position: idx % 2 === 0 ? 'right' : 'left',
        });
      }

      // Build next level
      const nextLevel = [];
      for (let i = 0; i < level.length; i += 2) {
        if (i + 1 < level.length) {
          const combined = `${level[i]}:${level[i + 1]}`;
          nextLevel.push(await blake3(combined));
        } else {
          nextLevel.push(level[i]);
        }
      }

      level = nextLevel;
      idx = Math.floor(idx / 2);
    }

    return proof;
  }

  /**
   * Verify a Merkle proof
   *
   * @param {string} receiptHash - Hash to verify
   * @param {Array<{hash: string, position: string}>} proof - Merkle proof path
   * @param {string} merkleRoot - Expected Merkle root
   * @returns {Promise<boolean>} Whether proof is valid
   */
  async verifyMerkleProof(receiptHash, proof, merkleRoot) {
    let currentHash = receiptHash;

    for (const step of proof) {
      const combined =
        step.position === 'right'
          ? `${currentHash}:${step.hash}`
          : `${step.hash}:${currentHash}`;
      currentHash = await blake3(combined);
    }

    return currentHash === merkleRoot;
  }

  /**
   * Serialize chain to JSON
   *
   * @returns {Object} JSON-serializable chain data
   */
  toJSON() {
    return {
      nodeId: this.nodeId,
      vectorClock: this.vectorClock.toJSON(),
      receipts: this.receipts.map((r) => ({
        ...r,
        t_ns: r.t_ns.toString(),
      })),
      merkleRoot: this._merkleRoot,
    };
  }

  /**
   * Create chain from JSON
   *
   * @param {Object} json - Serialized chain data
   * @returns {ProofChain}
   */
  static fromJSON(json) {
    const chain = new ProofChain(json.nodeId);
    chain.vectorClock = VectorClock.fromJSON(json.vectorClock);
    chain.receipts = json.receipts.map((r) => ({
      ...r,
      t_ns: BigInt(r.t_ns),
    }));
    chain._merkleRoot = json.merkleRoot;
    chain._dirty = false;
    return chain;
  }

  /**
   * Export chain as audit trail
   *
   * @returns {Promise<Object>} Complete audit trail with verification
   */
  async exportAuditTrail() {
    const verifyResult = await this.verify();
    const merkleRoot = await this.getMerkleRoot();

    return {
      nodeId: this.nodeId,
      receiptCount: this.receipts.length,
      firstReceiptTime: this.receipts[0]?.timestamp_iso || null,
      lastReceiptTime: this.receipts[this.receipts.length - 1]?.timestamp_iso || null,
      merkleRoot,
      chainValid: verifyResult.valid,
      validationError: verifyResult.error || null,
      vectorClock: this.vectorClock.toJSON(),
      receipts: this.receipts.map((r) => ({
        id: r.id,
        eventType: r.eventType,
        timestamp_iso: r.timestamp_iso,
        caseId: r.caseId,
        taskId: r.taskId,
        receiptHash: r.receiptHash,
        decision: r.payload.decision,
      })),
      exportedAt: toISO(now()),
    };
  }
}

// =============================================================================
// Legacy YawlReceipt Class (Backward Compatibility)
// =============================================================================

/**
 * Legacy receipt data schema
 */
const LegacyReceiptDataSchema = z.object({
  id: z.string().min(1),
  caseId: z.string().min(1),
  taskId: z.string().min(1),
  action: z.enum(['enable', 'start', 'complete', 'cancel', 'fail', 'timeout']),
  timestamp: z.bigint(),
  actor: z.string().optional(),
  beforeHash: z.string().min(1),
  afterHash: z.string().min(1),
  previousReceiptHash: z.string().optional(),
  sparqlQuery: z.string().optional(),
  sparqlResult: z.unknown().optional(),
  input: z.record(z.unknown()).optional(),
  output: z.record(z.unknown()).optional(),
  downstreamEnabled: z
    .array(
      z.object({
        taskId: z.string(),
        enabledAt: z.bigint(),
      })
    )
    .optional(),
});

/**
 * YawlReceipt - Legacy receipt class for backward compatibility
 *
 * @deprecated Use generateReceipt() and ProofChain instead
 */
export class YawlReceipt {
  /**
   * @param {Object} data - Receipt data
   */
  constructor(data) {
    const validated = LegacyReceiptDataSchema.parse(data);
    this.id = validated.id;
    this.caseId = validated.caseId;
    this.taskId = validated.taskId;
    this.action = validated.action;
    this.timestamp = validated.timestamp;
    this.actor = validated.actor;
    this.beforeHash = validated.beforeHash;
    this.afterHash = validated.afterHash;
    this.previousReceiptHash = validated.previousReceiptHash;
    this.sparqlQuery = validated.sparqlQuery;
    this.sparqlResult = validated.sparqlResult;
    this.input = validated.input;
    this.output = validated.output;
    this.downstreamEnabled = validated.downstreamEnabled ?? [];
    /** @type {string|null} Computed hash of this receipt */
    this._hash = null;
  }

  /**
   * Compute the hash of this receipt
   * @returns {Promise<string>} BLAKE3 hash
   */
  async computeHash() {
    if (this._hash) return this._hash;

    const payload = JSON.stringify({
      id: this.id,
      caseId: this.caseId,
      taskId: this.taskId,
      action: this.action,
      timestamp: this.timestamp.toString(),
      actor: this.actor,
      beforeHash: this.beforeHash,
      afterHash: this.afterHash,
      previousReceiptHash: this.previousReceiptHash,
    });

    this._hash = await blake3(payload);
    return this._hash;
  }

  /**
   * Get cached hash or compute if not available
   * @returns {Promise<string>}
   */
  async getHash() {
    return this._hash ?? this.computeHash();
  }

  /**
   * Verify this receipt's hash integrity
   * @returns {Promise<boolean>}
   */
  async verify() {
    const storedHash = this._hash;
    this._hash = null;
    const computedHash = await this.computeHash();

    if (storedHash && storedHash !== computedHash) {
      return false;
    }
    return true;
  }

  /**
   * Verify receipt chain integrity
   * @param {YawlReceipt|null} previousReceipt - Previous receipt in chain
   * @returns {Promise<{valid: boolean, reason?: string}>}
   */
  async verifyChain(previousReceipt) {
    if (!(await this.verify())) {
      return { valid: false, reason: 'Receipt hash mismatch' };
    }

    if (!previousReceipt) {
      if (this.previousReceiptHash) {
        return {
          valid: false,
          reason: 'Expected no previous receipt but previousReceiptHash is set',
        };
      }
      return { valid: true };
    }

    const previousHash = await previousReceipt.getHash();
    if (this.previousReceiptHash !== previousHash) {
      return {
        valid: false,
        reason: `Chain broken: expected ${previousHash}, got ${this.previousReceiptHash}`,
      };
    }

    return { valid: true };
  }

  /**
   * Check if this receipt is valid (has required fields)
   * @returns {boolean}
   */
  get valid() {
    return !!(
      this.id &&
      this.caseId &&
      this.taskId &&
      this.action &&
      this.timestamp &&
      this.beforeHash &&
      this.afterHash
    );
  }

  /**
   * Get timestamp as ISO string
   * @returns {string}
   */
  get timestampISO() {
    return toISO(this.timestamp);
  }

  /**
   * Serialize to JSON-compatible object
   * @returns {Object}
   */
  toJSON() {
    return {
      id: this.id,
      caseId: this.caseId,
      taskId: this.taskId,
      action: this.action,
      timestamp: this.timestamp.toString(),
      timestampISO: this.timestampISO,
      actor: this.actor,
      beforeHash: this.beforeHash,
      afterHash: this.afterHash,
      previousReceiptHash: this.previousReceiptHash,
      hash: this._hash,
      sparqlQuery: this.sparqlQuery,
      sparqlResult: this.sparqlResult,
      input: this.input,
      output: this.output,
      downstreamEnabled: this.downstreamEnabled?.map((d) => ({
        taskId: d.taskId,
        enabledAt: d.enabledAt.toString(),
      })),
      valid: this.valid,
    };
  }

  /**
   * Create from JSON object
   * @param {Object} json
   * @returns {YawlReceipt}
   */
  static fromJSON(json) {
    const receipt = new YawlReceipt({
      ...json,
      timestamp: BigInt(json.timestamp),
      downstreamEnabled: json.downstreamEnabled?.map((d) => ({
        taskId: d.taskId,
        enabledAt: BigInt(d.enabledAt),
      })),
    });
    receipt._hash = json.hash;
    return receipt;
  }
}

// =============================================================================
// Legacy Receipt Builder (Backward Compatibility)
// =============================================================================

/**
 * Build a legacy receipt for a task transition
 *
 * @deprecated Use generateReceipt() instead
 * @param {Object} params
 * @returns {Promise<YawlReceipt>}
 */
export async function buildReceipt({
  caseId,
  taskId,
  action,
  actor,
  beforeState,
  afterState,
  previousReceipt,
  sparqlQuery,
  sparqlResult,
  input,
  output,
  downstreamEnabled = [],
}) {
  const beforeHash = await blake3(JSON.stringify(beforeState));
  const afterHash = await blake3(JSON.stringify(afterState));

  let previousReceiptHash;
  if (previousReceipt) {
    previousReceiptHash = await previousReceipt.getHash();
  }

  const receipt = new YawlReceipt({
    id: `receipt-${Date.now()}-${Math.random().toString(36).slice(2, 8)}`,
    caseId,
    taskId,
    action,
    timestamp: now(),
    actor,
    beforeHash,
    afterHash,
    previousReceiptHash,
    sparqlQuery,
    sparqlResult,
    input,
    output,
    downstreamEnabled,
  });

  await receipt.computeHash();

  return receipt;
}

// =============================================================================
// Exports
// =============================================================================

export default {
  // Main API
  generateReceipt,
  verifyReceipt,
  verifyChainLink,
  ProofChain,

  // Legacy API
  YawlReceipt,
  buildReceipt,

  // Constants
  RECEIPT_EVENT_TYPES,

  // Schemas
  ReceiptSchema,
  JustificationSchema,
  PayloadSchema,
};
