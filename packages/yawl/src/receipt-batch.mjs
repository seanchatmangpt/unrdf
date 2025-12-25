/**
 * Batch Receipt Generation - High-Performance BLAKE3 Hashing
 *
 * Optimizations:
 * 1. Pre-serialization batching - serialize all payloads upfront
 * 2. Parallel hashing - use Promise.all for concurrent BLAKE3
 * 3. Chain hash precomputation - compute chain in single pass
 * 4. Object pooling - reuse receipt objects to reduce GC pressure
 *
 * Target: 100K receipts/sec (vs 45K baseline)
 *
 * @module @unrdf/yawl/receipt-batch
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';
import { now, toISO, VectorClock } from '@unrdf/kgc-4d';
import { RECEIPT_EVENT_TYPES, ReceiptSchema } from './receipt.mjs';

// =============================================================================
// Constants
// =============================================================================

const BLAKE3_HEX_LENGTH = 64;
const DEFAULT_BATCH_SIZE = 1000;
const DEFAULT_PARALLEL_WORKERS = 4;

// =============================================================================
// Object Pool for Receipt Reuse
// =============================================================================

/**
 * Receipt object pool for reduced GC pressure
 */
class ReceiptPool {
  constructor(initialSize = 1000) {
    /** @type {Object[]} */
    this.pool = [];
    this.created = 0;
    this.reused = 0;

    // Pre-allocate
    for (let i = 0; i < initialSize; i++) {
      this.pool.push(this._createEmpty());
    }
  }

  _createEmpty() {
    return {
      id: '',
      eventType: '',
      t_ns: 0n,
      timestamp_iso: '',
      caseId: '',
      taskId: '',
      workItemId: undefined,
      previousReceiptHash: null,
      payloadHash: '',
      receiptHash: '',
      kgcEventId: undefined,
      gitRef: undefined,
      vectorClock: undefined,
      payload: null,
    };
  }

  acquire() {
    if (this.pool.length > 0) {
      this.reused++;
      return this.pool.pop();
    }
    this.created++;
    return this._createEmpty();
  }

  release(receipt) {
    // Reset and return to pool
    receipt.id = '';
    receipt.eventType = '';
    receipt.t_ns = 0n;
    receipt.timestamp_iso = '';
    receipt.caseId = '';
    receipt.taskId = '';
    receipt.workItemId = undefined;
    receipt.previousReceiptHash = null;
    receipt.payloadHash = '';
    receipt.receiptHash = '';
    receipt.kgcEventId = undefined;
    receipt.gitRef = undefined;
    receipt.vectorClock = undefined;
    receipt.payload = null;
    this.pool.push(receipt);
  }

  getStats() {
    return {
      poolSize: this.pool.length,
      created: this.created,
      reused: this.reused,
      reuseRate: this.reused / (this.created + this.reused) || 0,
    };
  }
}

/** Global receipt pool */
const receiptPool = new ReceiptPool();

// =============================================================================
// Utility Functions
// =============================================================================

/**
 * Fast UUID v4 generation
 * @returns {string}
 */
function generateUUID() {
  if (typeof crypto !== 'undefined' && crypto.randomUUID) {
    return crypto.randomUUID();
  }
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

/**
 * Deterministic serialization - optimized version
 * Uses sorted keys for reproducibility
 *
 * @param {Object} obj
 * @returns {string}
 */
function deterministicSerialize(obj) {
  if (obj === null || obj === undefined) return 'null';
  if (typeof obj === 'bigint') return obj.toString();
  if (typeof obj !== 'object') return JSON.stringify(obj);

  if (Array.isArray(obj)) {
    return '[' + obj.map(deterministicSerialize).join(',') + ']';
  }

  const sortedKeys = Object.keys(obj).sort();
  const pairs = sortedKeys.map(key =>
    `${JSON.stringify(key)}:${deterministicSerialize(obj[key])}`
  );
  return '{' + pairs.join(',') + '}';
}

/**
 * Pre-serialize events for batch hashing
 * @param {Array} events
 * @returns {string[]}
 */
function preSerializeEvents(events) {
  return events.map(event => {
    const payloadToHash = {
      eventType: event.eventType,
      caseId: event.caseId,
      taskId: event.taskId,
      workItemId: event.workItemId || null,
      payload: event.payload,
      t_ns: event.t_ns.toString(),
    };
    return deterministicSerialize(payloadToHash);
  });
}

// =============================================================================
// Batch Hashing
// =============================================================================

/**
 * Hash multiple strings in parallel using BLAKE3
 * Splits work across chunks for optimal parallelism
 *
 * @param {string[]} data - Strings to hash
 * @param {number} workers - Number of parallel workers
 * @returns {Promise<string[]>} - Hashes in same order as input
 */
async function parallelHash(data, workers = DEFAULT_PARALLEL_WORKERS) {
  if (data.length === 0) return [];
  if (data.length <= workers) {
    // Small batch - hash all in parallel
    return Promise.all(data.map(d => blake3(d)));
  }

  // Split into chunks and hash each chunk
  const chunkSize = Math.ceil(data.length / workers);
  const chunks = [];

  for (let i = 0; i < data.length; i += chunkSize) {
    chunks.push(data.slice(i, i + chunkSize));
  }

  // Process chunks in parallel
  const chunkResults = await Promise.all(
    chunks.map(chunk => Promise.all(chunk.map(d => blake3(d))))
  );

  // Flatten results
  return chunkResults.flat();
}

/**
 * Compute chain hashes in batch
 * Uses previous hash for chaining
 *
 * @param {string[]} payloadHashes
 * @param {string|null} previousReceiptHash
 * @returns {Promise<string[]>}
 */
async function computeChainHashes(payloadHashes, previousReceiptHash = null) {
  const chainInputs = [];
  let prevHash = previousReceiptHash;

  for (const payloadHash of payloadHashes) {
    chainInputs.push(`${prevHash || 'GENESIS'}:${payloadHash}`);
    // Chain to next - use payload hash as proxy for now
    // Actual receipt hash computed below
  }

  // Hash all chain inputs in parallel
  const hashes = await parallelHash(chainInputs);

  // Update chain linkage
  const result = [];
  for (let i = 0; i < hashes.length; i++) {
    result.push(hashes[i]);
    prevHash = hashes[i]; // Chain for next
  }

  return result;
}

// =============================================================================
// Batch Receipt Generation
// =============================================================================

/**
 * @typedef {Object} BatchReceiptEvent
 * @property {string} eventType - Event type
 * @property {string} caseId - Case ID
 * @property {string} taskId - Task ID
 * @property {string} [workItemId] - Work item ID
 * @property {Object} payload - Event payload
 * @property {string} [kgcEventId] - KGC event ID
 * @property {string} [gitRef] - Git reference
 * @property {Object} [vectorClock] - Vector clock
 */

/**
 * @typedef {Object} BatchResult
 * @property {Object[]} receipts - Generated receipts
 * @property {number} duration - Processing time in ms
 * @property {number} throughput - Receipts per second
 * @property {Object} stats - Pool and processing stats
 */

/**
 * Generate receipts in batch with parallel hashing
 *
 * Optimizations applied:
 * - Pre-serialization of payloads
 * - Parallel BLAKE3 hashing
 * - Chain hash computation in batch
 * - Object pooling for receipt reuse
 *
 * @param {BatchReceiptEvent[]} events - Events to receipt
 * @param {Object} [options] - Batch options
 * @param {Object|null} [options.previousReceipt] - Previous receipt for chaining
 * @param {number} [options.workers] - Number of parallel workers
 * @param {boolean} [options.usePool] - Use object pooling
 * @param {boolean} [options.validate] - Validate receipts with Zod
 * @returns {Promise<BatchResult>}
 *
 * @example
 * const events = [
 *   { eventType: 'TASK_ENABLED', caseId: 'c1', taskId: 't1', payload: { decision: 'ENABLE' } },
 *   { eventType: 'TASK_STARTED', caseId: 'c1', taskId: 't1', payload: { decision: 'START' } },
 * ];
 * const { receipts, throughput } = await generateReceiptBatch(events);
 * console.log(`Generated ${receipts.length} at ${throughput.toFixed(0)} receipts/sec`);
 */
export async function generateReceiptBatch(events, options = {}) {
  const {
    previousReceipt = null,
    workers = DEFAULT_PARALLEL_WORKERS,
    usePool = true,
    validate = false,
  } = options;

  const startTime = performance.now();

  if (events.length === 0) {
    return {
      receipts: [],
      duration: 0,
      throughput: 0,
      stats: { eventsProcessed: 0 },
    };
  }

  // 1. Validate event types upfront
  for (const event of events) {
    if (!Object.values(RECEIPT_EVENT_TYPES).includes(event.eventType)) {
      throw new Error(`Invalid event type: ${event.eventType}`);
    }
  }

  // 2. Generate timestamps for all events
  const baseTime = now();
  const timestamps = events.map((_, i) => baseTime + BigInt(i));

  // 3. Prepare events with timestamps
  const preparedEvents = events.map((event, i) => ({
    ...event,
    t_ns: timestamps[i],
  }));

  // 4. Pre-serialize all payloads
  const serializedPayloads = preSerializeEvents(preparedEvents);

  // 5. Compute payload hashes in parallel
  const payloadHashes = await parallelHash(serializedPayloads, workers);

  // 6. Compute chain hashes in batch
  const previousHash = previousReceipt?.receiptHash || null;

  // For chain hashes, we need to compute sequentially to maintain chain
  // But we can parallelize the actual hashing
  const chainInputs = [];
  let prevHash = previousHash;
  for (let i = 0; i < payloadHashes.length; i++) {
    chainInputs.push(`${prevHash || 'GENESIS'}:${payloadHashes[i]}`);
    // We'll use the result hash as the prev for next iteration
    // This requires sequential resolution
  }

  // Hash chain inputs - but we need to do this carefully to maintain order
  // For true chaining, each receipt hash depends on the previous
  // We can still parallelize within small batches
  const receiptHashes = [];
  prevHash = previousHash;

  // Batch chain hashing in chunks
  const CHAIN_BATCH_SIZE = 100;
  for (let i = 0; i < payloadHashes.length; i += CHAIN_BATCH_SIZE) {
    const batchEnd = Math.min(i + CHAIN_BATCH_SIZE, payloadHashes.length);
    const batchInputs = [];

    for (let j = i; j < batchEnd; j++) {
      batchInputs.push(`${prevHash || 'GENESIS'}:${payloadHashes[j]}`);
      // Compute hash synchronously for this batch item
      const hash = await blake3(batchInputs[batchInputs.length - 1]);
      receiptHashes.push(hash);
      prevHash = hash;
    }
  }

  // 7. Build receipt objects
  const receipts = [];

  for (let i = 0; i < events.length; i++) {
    const event = events[i];
    const t_ns = timestamps[i];

    // Use pool or create new object
    const receipt = usePool ? receiptPool.acquire() : {};

    receipt.id = generateUUID();
    receipt.eventType = event.eventType;
    receipt.t_ns = t_ns;
    receipt.timestamp_iso = toISO(t_ns);
    receipt.caseId = event.caseId;
    receipt.taskId = event.taskId;
    receipt.workItemId = event.workItemId || undefined;
    receipt.previousReceiptHash = i === 0 ? previousHash : receiptHashes[i - 1];
    receipt.payloadHash = payloadHashes[i];
    receipt.receiptHash = receiptHashes[i];
    receipt.kgcEventId = event.kgcEventId || undefined;
    receipt.gitRef = event.gitRef || undefined;
    receipt.vectorClock = event.vectorClock || undefined;
    receipt.payload = event.payload;

    // Validate if requested (slower but safer)
    if (validate) {
      receipts.push(ReceiptSchema.parse(receipt));
    } else {
      receipts.push(receipt);
    }
  }

  const endTime = performance.now();
  const duration = endTime - startTime;
  const throughput = (events.length / duration) * 1000;

  return {
    receipts,
    duration,
    throughput,
    stats: {
      eventsProcessed: events.length,
      workers,
      poolStats: usePool ? receiptPool.getStats() : null,
    },
  };
}

/**
 * Verify batch of receipts in parallel
 *
 * @param {Object[]} receipts - Receipts to verify
 * @param {number} [workers] - Number of parallel workers
 * @returns {Promise<{valid: boolean, invalidCount: number, results: boolean[]}>}
 */
export async function verifyReceiptBatch(receipts, workers = DEFAULT_PARALLEL_WORKERS) {
  if (receipts.length === 0) {
    return { valid: true, invalidCount: 0, results: [] };
  }

  // Prepare verification data
  const verificationData = receipts.map(receipt => ({
    payloadToHash: deterministicSerialize({
      eventType: receipt.eventType,
      caseId: receipt.caseId,
      taskId: receipt.taskId,
      workItemId: receipt.workItemId || null,
      payload: receipt.payload,
      t_ns: receipt.t_ns.toString(),
    }),
    expectedPayloadHash: receipt.payloadHash,
    chainInput: `${receipt.previousReceiptHash || 'GENESIS'}:${receipt.payloadHash}`,
    expectedReceiptHash: receipt.receiptHash,
  }));

  // Hash payloads in parallel
  const payloadHashes = await parallelHash(
    verificationData.map(d => d.payloadToHash),
    workers
  );

  // Hash chain inputs in parallel
  const chainHashes = await parallelHash(
    verificationData.map(d => d.chainInput),
    workers
  );

  // Verify
  const results = verificationData.map((data, i) => {
    const payloadValid = payloadHashes[i] === data.expectedPayloadHash;
    const chainValid = chainHashes[i] === data.expectedReceiptHash;
    return payloadValid && chainValid;
  });

  const invalidCount = results.filter(r => !r).length;

  return {
    valid: invalidCount === 0,
    invalidCount,
    results,
  };
}

/**
 * Release receipts back to pool
 * Call this when receipts are no longer needed to enable reuse
 *
 * @param {Object[]} receipts
 */
export function releaseReceipts(receipts) {
  for (const receipt of receipts) {
    receiptPool.release(receipt);
  }
}

/**
 * Get pool statistics
 * @returns {Object}
 */
export function getPoolStats() {
  return receiptPool.getStats();
}

/**
 * Reset pool to initial state
 * @param {number} [size] - New pool size
 */
export function resetPool(size = 1000) {
  receiptPool.pool = [];
  receiptPool.created = 0;
  receiptPool.reused = 0;
  for (let i = 0; i < size; i++) {
    receiptPool.pool.push(receiptPool._createEmpty());
  }
}

// =============================================================================
// Exports
// =============================================================================

export default {
  generateReceiptBatch,
  verifyReceiptBatch,
  releaseReceipts,
  getPoolStats,
  resetPool,
  parallelHash,
};
