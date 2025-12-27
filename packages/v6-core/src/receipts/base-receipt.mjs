/**
 * Base Receipt Schema - V6 Unified Receipt System
 *
 * Minimal superset schema for all receipt types with BLAKE3 hashing.
 *
 * @module @unrdf/v6-core/receipts/base-receipt
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';

// =============================================================================
// Constants
// =============================================================================

/**
 * BLAKE3 hash length in hex characters
 * @constant {number}
 */
export const BLAKE3_HEX_LENGTH = 64;

/**
 * Receipt type discriminator values
 * @readonly
 * @enum {string}
 */
export const RECEIPT_TYPES = Object.freeze({
  EXECUTION: 'execution',
  ALLOCATION: 'allocation',
  COMPILE: 'compile',
  VERIFICATION: 'verification',
});

// =============================================================================
// Base Schemas
// =============================================================================

/**
 * Receipt type discriminator schema
 */
export const ReceiptTypeSchema = z.enum([
  'execution',
  'allocation',
  'compile',
  'verification',
]);

/**
 * Signature/attestation hook schema
 */
export const AttestationSchema = z.object({
  /** Signature algorithm (e.g., 'ed25519', 'ecdsa-secp256k1') */
  algorithm: z.string(),
  /** Public key hex */
  publicKey: z.string(),
  /** Signature hex */
  signature: z.string(),
  /** Signer identity */
  signer: z.string().optional(),
}).optional();

/**
 * Vector clock schema for causality tracking
 */
export const VectorClockSchema = z.object({
  nodeId: z.string().min(1),
  counters: z.record(z.string(), z.string()),
}).passthrough();

/**
 * Base receipt schema - common fields for all receipt types
 */
export const BaseReceiptSchema = z.object({
  // Identity
  /** Unique receipt identifier (UUID v4) */
  id: z.string().uuid(),

  /** Receipt type discriminator */
  receiptType: ReceiptTypeSchema,

  // Timestamps
  /** Nanosecond timestamp (bigint as string in JSON) */
  t_ns: z.bigint(),

  /** ISO 8601 timestamp */
  timestamp_iso: z.string(),

  // Cryptographic proof chain
  /** Hash of previous receipt (null for genesis) */
  previousHash: z.string().length(BLAKE3_HEX_LENGTH).nullable(),

  /** Hash of payload data */
  payloadHash: z.string().length(BLAKE3_HEX_LENGTH),

  /** Hash of complete receipt (chained) */
  receiptHash: z.string().length(BLAKE3_HEX_LENGTH),

  // Optional extensions
  /** Signature/attestation (optional) */
  attestation: AttestationSchema,

  /** Vector clock for distributed causality */
  vectorClock: VectorClockSchema.optional(),

  /** Git reference for code provenance */
  gitRef: z.string().optional(),

  /** KGC event ID for integration */
  kgcEventId: z.string().optional(),
});

// =============================================================================
// Type Definitions (JSDoc)
// =============================================================================

/**
 * @typedef {Object} Attestation
 * @property {string} algorithm - Signature algorithm
 * @property {string} publicKey - Public key hex
 * @property {string} signature - Signature hex
 * @property {string} [signer] - Signer identity
 */

/**
 * @typedef {Object} VectorClock
 * @property {string} nodeId - Node identifier
 * @property {Record<string, string>} counters - Vector clock counters
 */

/**
 * @typedef {Object} BaseReceipt
 * @property {string} id - UUID of receipt
 * @property {'execution'|'allocation'|'compile'|'verification'} receiptType - Receipt type
 * @property {bigint} t_ns - Nanosecond timestamp
 * @property {string} timestamp_iso - ISO timestamp
 * @property {string|null} previousHash - Previous receipt hash
 * @property {string} payloadHash - Payload hash
 * @property {string} receiptHash - Receipt hash
 * @property {Attestation} [attestation] - Signature/attestation
 * @property {VectorClock} [vectorClock] - Vector clock
 * @property {string} [gitRef] - Git reference
 * @property {string} [kgcEventId] - KGC event ID
 */

// =============================================================================
// Utility Functions
// =============================================================================

/**
 * Generate a UUID v4
 * @param {Object} [context={}] - Execution context with uuid for determinism
 * @returns {string} UUID string
 */
export function generateUUID(context = {}) {
  // Use context-provided UUID for determinism if available
  if (context.uuid) return context.uuid;
  if (context.receiptId) return context.receiptId;

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

/**
 * Verify base receipt structure and hashes
 *
 * @param {BaseReceipt} receipt - Receipt to verify
 * @returns {Promise<{valid: boolean, error?: string, checks?: Object}>}
 */
export async function verifyBaseReceipt(receipt) {
  try {
    // Validate schema
    BaseReceiptSchema.parse(receipt);

    // Verify payload hash
    const payload = { ...receipt };
    delete payload.receiptHash;
    delete payload.payloadHash;
    delete payload.previousHash;

    const expectedPayloadHash = await computeBlake3(payload);
    const payloadHashValid = expectedPayloadHash === receipt.payloadHash;

    // Verify chain hash
    const expectedChainHash = await computeChainHash(
      receipt.previousHash,
      receipt.payloadHash
    );
    const chainHashValid = expectedChainHash === receipt.receiptHash;

    // Verify timestamp
    const timestampValid = receipt.t_ns > 0n;

    const allValid = payloadHashValid && chainHashValid && timestampValid;

    return {
      valid: allValid,
      error: allValid ? undefined : 'Hash or timestamp verification failed',
      checks: {
        payloadHashValid,
        chainHashValid,
        timestampValid,
      },
    };
  } catch (error) {
    return {
      valid: false,
      error: error.message,
    };
  }
}
