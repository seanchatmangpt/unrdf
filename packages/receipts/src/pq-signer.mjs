/**
 * Post-Quantum Receipt Signer
 * Main entry point for PQ-enabled receipts
 *
 * @module @unrdf/receipts/pq-signer
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import {
  generateHybridKeyPair,
  signHybrid,
  verifyHybrid,
  serializeHybridSignature,
  deserializeHybridSignature,
  getHybridSecurityLevel,
  HybridKeyPairSchema,
  HybridSignatureSchema,
} from './hybrid-signature.mjs';
import {
  generateDilithium3KeyPair,
  signDilithium3,
  verifyDilithium3,
  getDilithium3SecurityLevel,
} from './dilithium3.mjs';

/**
 * PQ Receipt Schema
 */
export const PQReceiptSchema = z.object({
  Q_ID: z.string().regex(/^Q\*_[a-f0-9]{16}$/),
  Q_RDF: z.string().url(),
  Q_PROV: z.object({
    timestamp: z.bigint(),
    batchSize: z.number().int().positive(),
    operationType: z.string(),
    universeID: z.string(),
    contentHash: z.string(),
    merkleRoot: z.string().optional(),
    signatureScheme: z.enum(['classical', 'postQuantum', 'hybrid']),
    signature: z.any().optional(), // Signature data (flexible schema)
  }),
});

/**
 * Create PQ Receipt
 * Generates receipt with optional post-quantum signature
 *
 * @param {Object} options - Receipt options
 * @param {string} options.universeID - Universe Q*_ID
 * @param {Array<Object>} options.operations - Operations to batch
 * @param {string} options.operationType - Operation type
 * @param {string} [options.signatureScheme='classical'] - Signature scheme
 * @param {Object} [options.keyPair] - Key pair for signing
 * @param {string} [options.merkleRoot] - Optional Merkle root
 * @returns {Promise<Object>} PQ-enabled receipt
 * @throws {Error} If validation fails
 *
 * @example
 * // Classical receipt (backward compatible)
 * const receipt1 = await createPQReceipt({
 *   universeID: 'Q*_0123456789abcdef',
 *   operations: [...],
 *   operationType: 'insert',
 * });
 *
 * // Post-quantum receipt
 * const keyPair = await generateDilithium3KeyPair();
 * const receipt2 = await createPQReceipt({
 *   universeID: 'Q*_0123456789abcdef',
 *   operations: [...],
 *   operationType: 'insert',
 *   signatureScheme: 'postQuantum',
 *   keyPair,
 * });
 *
 * // Hybrid receipt (both signatures)
 * const hybridKeyPair = await generateHybridKeyPair();
 * const receipt3 = await createPQReceipt({
 *   universeID: 'Q*_0123456789abcdef',
 *   operations: [...],
 *   operationType: 'insert',
 *   signatureScheme: 'hybrid',
 *   keyPair: hybridKeyPair,
 * });
 */
export async function createPQReceipt(options) {
  // Validate options
  if (!options || typeof options !== 'object') {
    throw new TypeError('createPQReceipt: options must be object');
  }

  if (typeof options.universeID !== 'string' || !options.universeID.startsWith('Q*_')) {
    throw new TypeError('createPQReceipt: universeID must be Q* identifier');
  }

  if (!Array.isArray(options.operations) || options.operations.length === 0) {
    throw new TypeError('createPQReceipt: operations must be non-empty array');
  }

  if (typeof options.operationType !== 'string') {
    throw new TypeError('createPQReceipt: operationType must be string');
  }

  const signatureScheme = options.signatureScheme || 'classical';
  if (!['classical', 'postQuantum', 'hybrid'].includes(signatureScheme)) {
    throw new TypeError('createPQReceipt: signatureScheme must be classical, postQuantum, or hybrid');
  }

  // Generate timestamp
  const timestamp = typeof process !== 'undefined' && process.hrtime
    ? process.hrtime.bigint()
    : BigInt(Date.now()) * 1_000_000n;

  // Compute content hash
  const contentHash = await computeContentHash(options.operations);

  // Generate receipt ID
  const Q_ID = await generateReceiptID(options.universeID, timestamp);
  const Q_RDF = `http://kgc.io/receipts/${Q_ID.slice(3)}`;

  // Build provenance
  const Q_PROV = {
    timestamp,
    batchSize: options.operations.length,
    operationType: options.operationType,
    universeID: options.universeID,
    contentHash,
    signatureScheme,
    ...(options.merkleRoot && { merkleRoot: options.merkleRoot }),
  };

  // Add signature if requested
  if (signatureScheme !== 'classical') {
    if (!options.keyPair) {
      throw new TypeError('createPQReceipt: keyPair required for PQ signatures');
    }

    const receiptData = JSON.stringify({ Q_ID, Q_RDF, Q_PROV }, (key, value) =>
      typeof value === 'bigint' ? value.toString() : value
    );

    if (signatureScheme === 'postQuantum') {
      const signature = await signDilithium3(receiptData, options.keyPair);
      Q_PROV.signature = {
        type: 'Dilithium3',
        signature: Buffer.from(signature.signature).toString('base64'),
        publicKey: Buffer.from(signature.publicKey).toString('base64'),
      };
    } else if (signatureScheme === 'hybrid') {
      const signature = await signHybrid(receiptData, options.keyPair);
      Q_PROV.signature = {
        type: 'Hybrid',
        data: serializeHybridSignature(signature),
      };
    }
  }

  const receipt = { Q_ID, Q_RDF, Q_PROV };

  // Validate receipt
  PQReceiptSchema.parse(receipt);

  return receipt;
}

/**
 * Verify PQ Receipt
 * Verifies receipt including optional PQ signature
 *
 * @param {Object} receipt - Receipt to verify
 * @param {Array<Object>} operations - Original operations
 * @returns {Promise<Object>} Verification result
 *
 * @example
 * const result = await verifyPQReceipt(receipt, operations);
 * console.log('Valid:', result.valid);
 * console.log('Signature verified:', result.signatureValid);
 */
export async function verifyPQReceipt(receipt, operations) {
  try {
    // Validate receipt schema
    PQReceiptSchema.parse(receipt);

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
    if (recomputedHash !== receipt.Q_PROV.contentHash) {
      return {
        valid: false,
        reason: 'Content hash mismatch',
        expected: receipt.Q_PROV.contentHash,
        actual: recomputedHash,
      };
    }

    // Verify signature if present
    let signatureValid = true;
    if (receipt.Q_PROV.signature) {
      const cleanReceipt = {
        Q_ID: receipt.Q_ID,
        Q_RDF: receipt.Q_RDF,
        Q_PROV: {
          ...receipt.Q_PROV,
          signature: undefined,
        },
      };
      const dataToVerify = JSON.stringify(cleanReceipt, (key, value) =>
        typeof value === 'bigint' ? value.toString() : value
      );

      if (receipt.Q_PROV.signature.type === 'Dilithium3') {
        const sig = {
          signature: new Uint8Array(Buffer.from(receipt.Q_PROV.signature.signature, 'base64')),
          publicKey: new Uint8Array(Buffer.from(receipt.Q_PROV.signature.publicKey, 'base64')),
          algorithm: 'Dilithium3',
        };
        signatureValid = await verifyDilithium3(dataToVerify, sig);
      } else if (receipt.Q_PROV.signature.type === 'Hybrid') {
        const sig = deserializeHybridSignature(receipt.Q_PROV.signature.data);
        const result = await verifyHybrid(dataToVerify, sig);
        signatureValid = result.valid;
      }
    }

    return {
      valid: true,
      signatureValid,
      signatureScheme: receipt.Q_PROV.signatureScheme,
      receiptID: receipt.Q_ID,
      timestamp: receipt.Q_PROV.timestamp,
      batchSize: receipt.Q_PROV.batchSize,
      contentHash: receipt.Q_PROV.contentHash,
    };
  } catch (err) {
    return {
      valid: false,
      reason: `Verification error: ${err.message}`,
    };
  }
}

/**
 * Generate Receipt ID
 * Creates unique Q* identifier
 *
 * @param {string} universeID - Universe Q*_ID
 * @param {bigint} timestamp - Nanosecond timestamp
 * @returns {Promise<string>} Receipt Q*_ID
 */
async function generateReceiptID(universeID, timestamp) {
  const combined = `receipt-${universeID}-${timestamp}`;
  const hash = await blake3(combined);
  return `Q*_${hash.slice(0, 16)}`;
}

/**
 * Compute Content Hash
 * Computes BLAKE3 hash of operations
 *
 * @param {Array<Object>} operations - Operations array
 * @returns {Promise<string>} BLAKE3 hash
 */
async function computeContentHash(operations) {
  const sorted = [...operations].sort((a, b) => {
    const tCompare = (a.timestamp || 0n) < (b.timestamp || 0n) ? -1 :
                     (a.timestamp || 0n) > (b.timestamp || 0n) ? 1 : 0;
    if (tCompare !== 0) return tCompare;
    return a.subject < b.subject ? -1 : a.subject > b.subject ? 1 : 0;
  });

  const serialized = JSON.stringify(sorted, (key, value) =>
    typeof value === 'bigint' ? value.toString() : value
  );

  return blake3(serialized);
}

/**
 * Batch Sign Multiple Receipts
 * Signs multiple receipts in a single operation (performance optimization)
 *
 * @param {Array<Object>} receipts - Array of receipts to sign
 * @param {Object} keyPair - Signing key pair
 * @param {string} signatureScheme - Signature scheme
 * @returns {Promise<Array<Object>>} Signed receipts
 *
 * @example
 * const signed = await batchSignReceipts(receipts, keyPair, 'hybrid');
 */
export async function batchSignReceipts(receipts, keyPair, signatureScheme) {
  if (!Array.isArray(receipts) || receipts.length === 0) {
    throw new TypeError('batchSignReceipts: receipts must be non-empty array');
  }

  const signed = [];

  for (const receipt of receipts) {
    const receiptData = JSON.stringify(receipt, (key, value) =>
      typeof value === 'bigint' ? value.toString() : value
    );

    let signature;
    if (signatureScheme === 'postQuantum') {
      signature = await signDilithium3(receiptData, keyPair);
      receipt.Q_PROV.signature = {
        type: 'Dilithium3',
        signature: Buffer.from(signature.signature).toString('base64'),
        publicKey: Buffer.from(signature.publicKey).toString('base64'),
      };
    } else if (signatureScheme === 'hybrid') {
      signature = await signHybrid(receiptData, keyPair);
      receipt.Q_PROV.signature = {
        type: 'Hybrid',
        data: serializeHybridSignature(signature),
      };
    }

    receipt.Q_PROV.signatureScheme = signatureScheme;
    signed.push(receipt);
  }

  return signed;
}

/**
 * Get PQ Capabilities
 * Returns available PQ signature schemes and their properties
 *
 * @returns {Object} PQ capabilities
 *
 * @example
 * const caps = getPQCapabilities();
 * console.log('Supported schemes:', caps.schemes);
 * console.log('Hybrid security:', caps.hybrid.securityLevel);
 */
export function getPQCapabilities() {
  return {
    schemes: ['classical', 'postQuantum', 'hybrid'],
    classical: {
      algorithm: 'BLAKE3',
      securityBits: 256,
      quantumResistant: false,
    },
    postQuantum: getDilithium3SecurityLevel(),
    hybrid: getHybridSecurityLevel(),
    recommended: 'hybrid',
    migrationPath: {
      v1: 'classical (backward compatible)',
      v2: 'postQuantum (opt-in)',
      v3: 'hybrid (future-proof)',
    },
  };
}

// Re-export key generation functions
export {
  generateHybridKeyPair,
  generateDilithium3KeyPair,
  signHybrid,
  verifyHybrid,
  signDilithium3,
  verifyDilithium3,
  getHybridSecurityLevel,
  getDilithium3SecurityLevel,
};
