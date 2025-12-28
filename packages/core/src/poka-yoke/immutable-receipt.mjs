/**
 * Poka-Yoke Pattern: Immutable Receipts
 * 
 * Prevents receipt tampering by freezing all receipt objects.
 * Once created, receipts cannot be modified.
 * 
 * State machine: Building → Sealed (via Object.freeze)
 * 
 * @module @unrdf/core/poka-yoke/immutable-receipt
 */

import { createHash } from 'node:crypto';
import { z } from 'zod';

/**
 * Receipt schema with strict validation
 */
const ReceiptSchema = z.object({
  id: z.string(),
  hash: z.string(),
  timestamp: z.string(),
  timestamp_iso: z.string(),
  eventType: z.string(),
  payload: z.any(),
  receiptType: z.enum(['kgc', 'blockchain', 'hook']),
  proof: z.any().optional(),
  chain: z.string().optional(),
  signer: z.string().optional(),
});

/**
 * Generate deterministic receipt ID
 * @param {string} eventType - Event type
 * @param {bigint} timestamp - Timestamp in nanoseconds
 * @returns {string} Receipt ID
 */
function generateReceiptId(eventType, timestamp) {
  if (process.env.DETERMINISTIC === '1') {
    return `receipt-${eventType}-${timestamp}`;
  }
  const random = Math.random().toString(36).substring(2, 10);
  return `receipt-${eventType}-${timestamp}-${random}`;
}

/**
 * Get current timestamp
 * @returns {bigint} Nanoseconds since epoch
 */
function getCurrentTimestamp() {
  if (process.env.DETERMINISTIC === '1') {
    // Use fixed timestamp in deterministic mode
    return 1704067200000n * 1_000_000n;
  }
  return BigInt(Date.now()) * 1_000_000n;
}

/**
 * Convert nanosecond timestamp to ISO string
 * @param {bigint} timestamp_ns - Timestamp in nanoseconds
 * @returns {string} ISO 8601 timestamp
 */
function toISO(timestamp_ns) {
  const ms = Number(timestamp_ns / 1_000_000n);
  return new Date(ms).toISOString();
}

/**
 * Deep freeze an object (recursive)
 * @param {Object} obj - Object to freeze
 * @returns {Object} Frozen object
 */
function deepFreeze(obj) {
  // Freeze the object itself
  Object.freeze(obj);
  
  // Recursively freeze all properties
  Object.keys(obj).forEach(key => {
    const value = obj[key];
    if (value !== null && typeof value === 'object' && !Object.isFrozen(value)) {
      deepFreeze(value);
    }
  });
  
  return obj;
}

/**
 * Create an immutable receipt
 * 
 * POKA-YOKE GUARANTEE:
 * - Returned receipt is deeply frozen (Object.freeze)
 * - Payload is frozen recursively
 * - Any modification attempt will:
 *   - Throw in strict mode
 *   - Silently fail in non-strict mode (still safe)
 * 
 * @param {string} eventType - Event type
 * @param {any} payload - Event payload (will be frozen)
 * @param {Object} options - Receipt options
 * @param {bigint} [options.timestamp] - Override timestamp
 * @param {string} [options.receiptType='kgc'] - Receipt type
 * @param {any} [options.proof] - Optional proof
 * @param {string} [options.chain] - Optional chain hash
 * @param {string} [options.signer] - Optional signer ID
 * @returns {Promise<Object>} Immutable receipt (frozen)
 * 
 * @example
 * const receipt = await createImmutableReceipt('test', { value: 42 });
 * receipt.hash = 'fake';  // Throws in strict mode, ignored otherwise
 * receipt.payload.value = 999;  // Throws in strict mode, ignored otherwise
 */
export async function createImmutableReceipt(eventType, payload, options = {}) {
  // Input validation
  if (!eventType || typeof eventType !== 'string') {
    throw new TypeError('createImmutableReceipt: eventType must be a non-empty string');
  }
  if (payload === undefined || payload === null) {
    throw new TypeError('createImmutableReceipt: payload is required');
  }
  
  const timestamp = options.timestamp || getCurrentTimestamp();
  const id = generateReceiptId(eventType, timestamp);
  const timestamp_iso = toISO(timestamp);
  
  // Build canonical receipt for hashing
  // Deep clone payload to avoid external mutations
  const payloadCopy = JSON.parse(JSON.stringify(payload));
  
  const canonicalReceipt = {
    chain: options.chain,
    eventType,
    id,
    payload: payloadCopy,
    proof: options.proof,
    receiptType: options.receiptType || 'kgc',
    signer: options.signer,
    timestamp: timestamp.toString(),
    timestamp_iso,
  };
  
  // Remove undefined fields
  Object.keys(canonicalReceipt).forEach(key => {
    if (canonicalReceipt[key] === undefined) {
      delete canonicalReceipt[key];
    }
  });
  
  // Sort keys for canonical JSON
  const sortedKeys = Object.keys(canonicalReceipt).sort();
  const canonicalData = {};
  for (const key of sortedKeys) {
    canonicalData[key] = canonicalReceipt[key];
  }
  
  // Compute hash (SHA256)
  const canonicalJson = JSON.stringify(canonicalData);
  const hash = createHash('sha256').update(canonicalJson).digest('hex');
  
  // Build final receipt
  const receipt = {
    id,
    hash,
    timestamp: timestamp.toString(),
    timestamp_iso,
    eventType,
    payload: payloadCopy,
    receiptType: options.receiptType || 'kgc',
    ...(options.proof && { proof: options.proof }),
    ...(options.chain && { chain: options.chain }),
    ...(options.signer && { signer: options.signer }),
  };
  
  // Validate with Zod
  const validated = ReceiptSchema.parse(receipt);
  
  // POKA-YOKE: Deep freeze the receipt
  // This makes tampering IMPOSSIBLE (throws in strict mode)
  return deepFreeze(validated);
}

/**
 * Verify that an object is frozen (for testing)
 * @param {Object} obj - Object to check
 * @returns {boolean} True if frozen
 */
export function isFrozen(obj) {
  return Object.isFrozen(obj);
}

/**
 * Verify that an object is deeply frozen (recursive check)
 * @param {Object} obj - Object to check
 * @returns {boolean} True if deeply frozen
 */
export function isDeeplyFrozen(obj) {
  if (!Object.isFrozen(obj)) {
    return false;
  }
  
  // Check all properties recursively
  for (const key of Object.keys(obj)) {
    const value = obj[key];
    if (value !== null && typeof value === 'object') {
      if (!isDeeplyFrozen(value)) {
        return false;
      }
    }
  }
  
  return true;
}
