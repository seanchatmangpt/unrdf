/**
 * KGC Receipts - Batch Receipt Generator
 * Generates cryptographic receipts for batch operations with Q* identifiers
 *
 * @module @unrdf/receipts/batch-receipt-generator
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';

/**
 * Receipt Schema (Zod validation)
 * Matches Q* format: Q*_ID, Q*_RDF, Q*_PROV
 */
const ReceiptSchema = z.object({
  Q_ID: z.string().regex(/^Q\*_[a-f0-9]{16}$/),  // Receipt Q* identifier
  Q_RDF: z.string().url(),                        // RDF URI representation
  Q_PROV: z.object({                              // Provenance metadata
    timestamp: z.bigint(),                        // Nanosecond timestamp
    batchSize: z.number().int().positive(),       // Number of operations in batch
    operationType: z.string(),                    // Type of operation (e.g., 'morphism')
    universeID: z.string(),                       // Target universe Q*_ID
    contentHash: z.string(),                      // BLAKE3 hash of batch content
    merkleRoot: z.string().optional(),            // Optional Merkle tree root
  }),
});

/**
 * Operation Schema
 */
const OperationSchema = z.object({
  type: z.enum(['add', 'delete', 'update', 'morphism']),
  subject: z.string(),
  predicate: z.string(),
  object: z.any(),
  timestamp: z.bigint().optional(),
});

/**
 * Generate Receipt ID
 * Creates unique Q* identifier for receipt
 *
 * @param {string} universeID - Universe Q*_ID
 * @param {bigint} timestamp - Nanosecond timestamp
 * @returns {Promise<string>} Receipt Q*_ID
 *
 * @example
 * const receiptID = await generateReceiptID('Q*_0123...', 1234567890n);
 * console.assert(receiptID.startsWith('Q*_'), 'Receipt ID has Q* prefix');
 */
async function generateReceiptID(universeID, timestamp) {
  const combined = `receipt-${universeID}-${timestamp}`;
  const hash = await blake3(combined);
  return `Q*_${hash.slice(0, 16)}`;
}

/**
 * Compute Content Hash
 * Computes BLAKE3 hash of serialized operations
 *
 * @param {Array<Object>} operations - Array of operations
 * @returns {Promise<string>} BLAKE3 hash (64 hex chars)
 *
 * @example
 * const hash = await computeContentHash(operations);
 * console.assert(hash.length === 64, 'Hash is 64 hex chars');
 */
async function computeContentHash(operations) {
  // Canonical serialization: sort by timestamp, then subject
  const sorted = [...operations].sort((a, b) => {
    const tCompare = (a.timestamp || 0n) < (b.timestamp || 0n) ? -1 :
                     (a.timestamp || 0n) > (b.timestamp || 0n) ? 1 : 0;
    if (tCompare !== 0) return tCompare;

    return a.subject < b.subject ? -1 : a.subject > b.subject ? 1 : 0;
  });

  // Serialize to JSON (deterministic)
  const serialized = JSON.stringify(sorted, (key, value) =>
    typeof value === 'bigint' ? value.toString() : value
  );

  return blake3(serialized);
}

/**
 * Generate Batch Receipt
 * Creates a cryptographic receipt for a batch of operations
 *
 * @param {Object} options - Receipt generation options
 * @param {string} options.universeID - Target universe Q*_ID
 * @param {Array<Object>} options.operations - Array of operations
 * @param {string} options.operationType - Type of operation
 * @param {string} [options.merkleRoot] - Optional Merkle tree root
 * @returns {Promise<Object>} Generated receipt with Q* format
 * @throws {Error} If validation fails
 *
 * @example
 * import { generateBatchReceipt } from './batch-receipt-generator.mjs';
 * const receipt = await generateBatchReceipt({
 *   universeID: 'Q*_0123456789abcdef',
 *   operations: [
 *     { type: 'add', subject: 'ex:s1', predicate: 'ex:p1', object: 'ex:o1' }
 *   ],
 *   operationType: 'morphism',
 * });
 * console.assert(receipt.Q_ID.startsWith('Q*_'), 'Receipt has Q* ID');
 */
export async function generateBatchReceipt(options) {
  // Validate options
  if (!options || typeof options !== 'object') {
    throw new TypeError('generateBatchReceipt: options must be object');
  }

  if (typeof options.universeID !== 'string' || !options.universeID.startsWith('Q*_')) {
    throw new TypeError('generateBatchReceipt: universeID must be Q* identifier');
  }

  if (!Array.isArray(options.operations) || options.operations.length === 0) {
    throw new TypeError('generateBatchReceipt: operations must be non-empty array');
  }

  if (typeof options.operationType !== 'string') {
    throw new TypeError('generateBatchReceipt: operationType must be string');
  }

  // Validate each operation
  options.operations.forEach((op, idx) => {
    try {
      OperationSchema.parse(op);
    } catch (err) {
      throw new Error(`generateBatchReceipt: Invalid operation at index ${idx}: ${err.message}`);
    }
  });

  // Generate timestamp
  const timestamp = typeof process !== 'undefined' && process.hrtime
    ? process.hrtime.bigint()
    : BigInt(Date.now()) * 1_000_000n;

  // Compute content hash
  const contentHash = await computeContentHash(options.operations);

  // Generate receipt ID
  const Q_ID = await generateReceiptID(options.universeID, timestamp);
  const Q_RDF = `http://kgc.io/receipts/${Q_ID.slice(3)}`; // Remove Q*_ prefix

  // Build provenance
  const Q_PROV = {
    timestamp,
    batchSize: options.operations.length,
    operationType: options.operationType,
    universeID: options.universeID,
    contentHash,
    ...(options.merkleRoot && { merkleRoot: options.merkleRoot }),
  };

  const receipt = { Q_ID, Q_RDF, Q_PROV };

  // Validate receipt
  ReceiptSchema.parse(receipt);

  return receipt;
}

/**
 * Verify Batch Receipt
 * Verifies receipt integrity by recomputing content hash
 *
 * @param {Object} receipt - Receipt to verify
 * @param {Array<Object>} operations - Original operations
 * @returns {Promise<Object>} Verification result
 *
 * @example
 * const result = await verifyBatchReceipt(receipt, operations);
 * console.log('Valid:', result.valid);
 * console.log('Hash match:', result.hashMatch);
 */
export async function verifyBatchReceipt(receipt, operations) {
  // Validate receipt schema
  try {
    ReceiptSchema.parse(receipt);
  } catch (err) {
    return {
      valid: false,
      reason: `Invalid receipt schema: ${err.message}`,
    };
  }

  // Validate operations
  if (!Array.isArray(operations) || operations.length === 0) {
    return {
      valid: false,
      reason: 'Operations must be non-empty array',
    };
  }

  // Verify batch size
  if (operations.length !== receipt.Q_PROV.batchSize) {
    return {
      valid: false,
      reason: `Batch size mismatch: expected ${receipt.Q_PROV.batchSize}, got ${operations.length}`,
    };
  }

  // Recompute content hash
  const recomputedHash = await computeContentHash(operations);

  // Compare hashes
  if (recomputedHash !== receipt.Q_PROV.contentHash) {
    return {
      valid: false,
      reason: 'Content hash mismatch',
      expected: receipt.Q_PROV.contentHash,
      actual: recomputedHash,
    };
  }

  // All checks passed
  return {
    valid: true,
    receiptID: receipt.Q_ID,
    timestamp: receipt.Q_PROV.timestamp,
    batchSize: receipt.Q_PROV.batchSize,
    contentHash: receipt.Q_PROV.contentHash,
  };
}

/**
 * Serialize Receipt
 * Converts receipt to JSON string
 *
 * @param {Object} receipt - Receipt to serialize
 * @returns {string} JSON string
 *
 * @example
 * const json = serializeReceipt(receipt);
 * const parsed = JSON.parse(json);
 */
export function serializeReceipt(receipt) {
  return JSON.stringify(receipt, (key, value) =>
    typeof value === 'bigint' ? value.toString() : value
  );
}

/**
 * Deserialize Receipt
 * Parses receipt from JSON string
 *
 * @param {string} json - JSON string
 * @returns {Object} Parsed receipt
 * @throws {Error} If JSON is invalid or receipt schema fails
 *
 * @example
 * const receipt = deserializeReceipt(json);
 * console.assert(receipt.Q_ID.startsWith('Q*_'));
 */
export function deserializeReceipt(json) {
  if (typeof json !== 'string') {
    throw new TypeError('deserializeReceipt: json must be string');
  }

  let parsed;
  try {
    parsed = JSON.parse(json, (key, value) => {
      // Convert timestamp strings back to BigInt
      if (key === 'timestamp' && typeof value === 'string') {
        return BigInt(value);
      }
      return value;
    });
  } catch (err) {
    throw new Error(`deserializeReceipt: Invalid JSON: ${err.message}`);
  }

  // Validate schema
  ReceiptSchema.parse(parsed);

  return parsed;
}

/**
 * Batch Multiple Operations
 * Groups operations by universe and generates receipts
 *
 * @param {Array<Object>} operationGroups - Array of { universeID, operations, operationType }
 * @returns {Promise<Array<Object>>} Array of receipts
 *
 * @example
 * const receipts = await batchMultipleOperations([
 *   { universeID: 'Q*_abc...', operations: [...], operationType: 'morphism' },
 *   { universeID: 'Q*_def...', operations: [...], operationType: 'update' },
 * ]);
 */
export async function batchMultipleOperations(operationGroups) {
  if (!Array.isArray(operationGroups)) {
    throw new TypeError('batchMultipleOperations: operationGroups must be array');
  }

  const receipts = [];

  for (const group of operationGroups) {
    const receipt = await generateBatchReceipt(group);
    receipts.push(receipt);
  }

  return receipts;
}
