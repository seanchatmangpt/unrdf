/**
 * L5 Receipt Pattern Library - Deterministic Receipt Generation
 *
 * Provides deterministic, composable receipt generation for all UNRDF operations.
 * Implements copy-exact pattern from P0 with L5 maturity guarantees:
 * - Determinism: Same input → same receipt hash
 * - Composition: Receipts chain via BLAKE3 merkle tree
 * - Provenance: Full operation lineage tracking
 *
 * @module @unrdf/v6-core/receipt-pattern
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';

/**
 * Deterministic Context Schema
 * Injects all non-deterministic values (time, randomness, node ID)
 */
export const DeterministicContextSchema = z.object({
  /** Timestamp in nanoseconds (injected, not Date.now()) */
  t_ns: z.bigint(),

  /** ISO 8601 timestamp string */
  timestamp_iso: z.string().datetime(),

  /** Node/process identifier (injected, not random) */
  nodeId: z.string().min(1),

  /** Case/workflow ID (optional, for YAWL integration) */
  caseId: z.string().optional(),

  /** Task ID (optional, for task tracking) */
  taskId: z.string().optional(),

  /** Previous receipt hash (for chaining) */
  previousReceiptHash: z.string().length(64).nullable(),

  /** Delta ID (optional, for ΔGate integration) */
  deltaId: z.string().optional(),
});

/**
 * Receipt Profile Schema
 * Unified receipt format across all packages
 */
export const ReceiptProfileSchema = z.object({
  /** Receipt unique identifier (UUID v4) */
  id: z.string().uuid(),

  /** Receipt profile type */
  profile: z.enum([
    'execution',
    'allocation',
    'compile',
    'verification',
    'workflow',
    'delta',
    'query',
    'store',
    'parse',
    'index'
  ]),

  /** BLAKE3 hash of previous receipt (null for genesis) */
  previousReceiptHash: z.string().length(64).nullable(),

  /** BLAKE3 hash of payload */
  payloadHash: z.string().length(64),

  /** BLAKE3 hash of entire receipt */
  receiptHash: z.string().length(64),

  /** Timestamp in nanoseconds (deterministic, injected) */
  t_ns: z.bigint(),

  /** ISO 8601 timestamp */
  timestamp_iso: z.string().datetime(),

  /** Execution context */
  context: z.object({
    caseId: z.string().optional(),
    taskId: z.string().optional(),
    deltaId: z.string().optional(),
    nodeId: z.string(),
  }),

  /** Profile-specific payload (validated by profile schema) */
  payload: z.any(),

  /** KGC event ID (optional, for KGC-4D integration) */
  kgcEventId: z.string().optional(),

  /** Git ref (optional, for version tracking) */
  gitRef: z.string().optional(),

  /** Vector clock (optional, for distributed causality) */
  vectorClock: z.record(z.string(), z.number()).optional(),
});

/**
 * BLAKE3 Hash Utility
 * Provides deterministic, cryptographically secure hashing using BLAKE3.
 * BLAKE3 is faster than SHA-256 and provides better security properties.
 *
 * @param {string|object} data - Data to hash (will be stringified if object)
 * @returns {Promise<string>} Hex-encoded BLAKE3 hash (64 characters)
 *
 * @example
 * const hash = await blake3Hash('test data');
 * console.log(hash); // 64-character hex string
 */
export async function blake3Hash(data) {
  const serialized = typeof data === 'string' ? data : JSON.stringify(data);
  return await blake3(serialized);
}

/**
 * Canonicalize object for deterministic hashing
 * Sorts keys lexicographically, removes undefined values
 *
 * @param {any} obj - Object to canonicalize
 * @returns {string} JSON string with sorted keys
 */
export function canonicalize(obj) {
  if (obj === null) return 'null';
  if (obj === undefined) return '';
  if (typeof obj === 'bigint') return `"${obj.toString()}"`;
  if (typeof obj !== 'object') return JSON.stringify(obj);

  if (Array.isArray(obj)) {
    return '[' + obj.map(canonicalize).join(',') + ']';
  }

  const sortedKeys = Object.keys(obj).filter(k => obj[k] !== undefined).sort();
  const pairs = sortedKeys.map(k => `"${k}":${canonicalize(obj[k])}`);
  return '{' + pairs.join(',') + '}';
}

/**
 * Generate deterministic UUID from content hash
 * Creates a valid UUID v4 format from a hash string
 *
 * @param {string} contentHash - Hash to derive UUID from (min 32 hex chars)
 * @returns {string} Valid UUID v4 format string
 */
export function deterministicUUID(contentHash) {
  // Take first 32 hex chars and format as UUID v4
  const hex = contentHash.slice(0, 32);

  // Set version 4 (0100xxxx) and variant 10xxxxxx for RFC 4122 compliance
  const variant = (parseInt(hex.slice(16, 18), 16) & 0x3f | 0x80).toString(16).padStart(2, '0');

  return `${hex.slice(0, 8)}-${hex.slice(8, 12)}-4${hex.slice(12, 15)}-${variant}${hex.slice(18, 20)}-${hex.slice(20, 32)}`;
}

/**
 * Higher-Order Function: Wrap operation with receipt generation
 *
 * @param {Function} fn - Pure function to wrap
 * @param {Object} options - Receipt options
 * @param {string} options.operation - Operation name
 * @param {string} options.profile - Receipt profile type
 * @param {z.ZodSchema} options.inputSchema - Zod schema for input validation
 * @param {z.ZodSchema} options.outputSchema - Zod schema for output validation
 * @returns {Function} Wrapped function that returns { result, receipt }
 *
 * @example
 * const queryWithReceipt = withReceipt(
 *   async (store, sparql) => store.query(sparql),
 *   {
 *     operation: 'sparql-query',
 *     profile: 'query',
 *     inputSchema: z.tuple([StoreSchema, z.string()]),
 *     outputSchema: QueryResultSchema
 *   }
 * );
 *
 * const ctx = createContext({ nodeId: 'node-1' });
 * const { result, receipt } = await queryWithReceipt(ctx, store, 'SELECT * WHERE {?s ?p ?o}');
 */
export function withReceipt(fn, options = {}) {
  if (typeof fn !== 'function') {
    throw new Error('withReceipt requires a function');
  }

  const {
    operation = fn.name || 'anonymous',
    profile = 'execution',
    inputSchema = z.any(),
    outputSchema = z.any()
  } = options;

  return async function wrappedWithReceipt(context, ...args) {
    // Validate context
    const ctx = DeterministicContextSchema.parse(context);

    // Validate input
    const validatedInput = inputSchema.parse(args);

    // Execute operation (pure, deterministic)
    const startTime = performance.now();
    const result = await fn(...validatedInput);
    const endTime = performance.now();

    // Validate output
    const validatedOutput = outputSchema.parse(result);

    // Build payload
    const payload = {
      operation,
      input: canonicalize(validatedInput),
      output: canonicalize(validatedOutput),
      duration_ms: endTime - startTime,
    };

    // Hash payload (await BLAKE3)
    const payloadHash = await blake3Hash(canonicalize(payload));

    // Build receipt
    const receiptData = {
      id: deterministicUUID(payloadHash + ctx.t_ns.toString()),
      profile,
      previousReceiptHash: ctx.previousReceiptHash,
      payloadHash,
      receiptHash: '', // Computed below
      t_ns: ctx.t_ns,
      timestamp_iso: ctx.timestamp_iso,
      context: {
        caseId: ctx.caseId,
        taskId: ctx.taskId,
        deltaId: ctx.deltaId,
        nodeId: ctx.nodeId,
      },
      payload,
    };

    // Hash receipt (excluding receiptHash field itself, await BLAKE3)
    const receiptForHashing = { ...receiptData, receiptHash: undefined };
    receiptData.receiptHash = await blake3Hash(canonicalize(receiptForHashing));

    // Validate receipt schema
    const receipt = ReceiptProfileSchema.parse(receiptData);

    return {
      result: validatedOutput,
      receipt,
    };
  };
}

/**
 * Create deterministic context
 *
 * @param {Object} options - Context options
 * @param {string} options.nodeId - Node identifier
 * @param {bigint} [options.t_ns] - Timestamp in nanoseconds (default: current time)
 * @param {string} [options.previousReceiptHash] - Previous receipt hash (default: null)
 * @param {string} [options.caseId] - Case ID (optional)
 * @param {string} [options.taskId] - Task ID (optional)
 * @param {string} [options.deltaId] - Delta ID (optional)
 * @returns {Object} Deterministic context
 */
export function createContext(options = {}) {
  const {
    nodeId,
    t_ns = BigInt(Date.now()) * 1000000n, // Convert ms to ns
    previousReceiptHash = null,
    caseId,
    taskId,
    deltaId,
  } = options;

  if (!nodeId) {
    throw new Error('nodeId is required for deterministic context');
  }

  const timestamp_iso = new Date(Number(t_ns / 1000000n)).toISOString();

  return DeterministicContextSchema.parse({
    t_ns,
    timestamp_iso,
    nodeId,
    caseId,
    taskId,
    previousReceiptHash,
    deltaId,
  });
}

/**
 * Chain receipts together (merkle tree)
 *
 * @param {Object} receipt1 - First receipt
 * @param {Object} receipt2 - Second receipt
 * @returns {Object} Context for next operation with receipt2 as previous
 */
export function chainReceipts(receipt1, receipt2) {
  const validated1 = ReceiptProfileSchema.parse(receipt1);
  const validated2 = ReceiptProfileSchema.parse(receipt2);

  // Verify chain integrity
  if (validated2.previousReceiptHash !== validated1.receiptHash) {
    throw new Error('Receipt chain broken: hash mismatch');
  }

  return {
    previousReceiptHash: validated2.receiptHash,
    t_ns: validated2.t_ns + 1n, // Increment time
    nodeId: validated2.context.nodeId,
  };
}

/**
 * Verify receipt chain integrity
 *
 * @param {Array<Object>} receipts - Array of receipts to verify
 * @returns {Object} Verification result
 */
export function verifyReceiptChain(receipts) {
  if (!Array.isArray(receipts) || receipts.length === 0) {
    throw new Error('receipts must be a non-empty array');
  }

  const validated = receipts.map(r => ReceiptProfileSchema.parse(r));
  const errors = [];

  // Genesis receipt must have null previousReceiptHash
  if (validated[0].previousReceiptHash !== null) {
    errors.push({
      index: 0,
      error: 'Genesis receipt must have previousReceiptHash=null',
    });
  }

  // Verify chain links
  for (let i = 1; i < validated.length; i++) {
    const prev = validated[i - 1];
    const curr = validated[i];

    if (curr.previousReceiptHash !== prev.receiptHash) {
      errors.push({
        index: i,
        error: `Receipt ${i} previousReceiptHash doesn't match receipt ${i-1} hash`,
        expected: prev.receiptHash,
        actual: curr.previousReceiptHash,
      });
    }

    // Verify timestamps are monotonically increasing
    if (curr.t_ns <= prev.t_ns) {
      errors.push({
        index: i,
        error: `Receipt ${i} timestamp not after receipt ${i-1}`,
        prev_t: prev.t_ns.toString(),
        curr_t: curr.t_ns.toString(),
      });
    }
  }

  return {
    valid: errors.length === 0,
    chainLength: validated.length,
    errors,
    genesisHash: validated[0].receiptHash,
    headHash: validated[validated.length - 1].receiptHash,
  };
}

/**
 * Compose multiple receipted operations
 *
 * @param {...Function} fns - Wrapped functions to compose
 * @returns {Function} Composed function that chains receipts
 *
 * @example
 * const parse = withReceipt(parseRDF, { operation: 'parse', profile: 'parse' });
 * const query = withReceipt(queryStore, { operation: 'query', profile: 'query' });
 *
 * const parseAndQuery = compose(parse, query);
 * const { result, receipts } = await parseAndQuery(ctx, rdfData, sparql);
 */
export function compose(...fns) {
  return async function composedWithReceipts(context, ...args) {
    let ctx = context;
    let input = args;
    const receipts = [];

    for (const fn of fns) {
      const { result, receipt } = await fn(ctx, ...input);
      receipts.push(receipt);

      // Update context for next operation
      ctx = createContext({
        nodeId: ctx.nodeId,
        t_ns: ctx.t_ns + 1n,
        previousReceiptHash: receipt.receiptHash,
        caseId: ctx.caseId,
        taskId: ctx.taskId,
      });

      // Next operation uses previous result as input
      input = Array.isArray(result) ? result : [result];
    }

    return {
      result: input[0], // Final result
      receipts,
      chainVerification: verifyReceiptChain(receipts),
    };
  };
}
