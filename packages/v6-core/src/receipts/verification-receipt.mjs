/**
 * Verification Receipt - V6 Hash/Signature/Merkle Proof Receipts
 *
 * For chain verification, cryptographic proof validation, and anchoring.
 *
 * @module @unrdf/v6-core/receipts/verification-receipt
 */

import { z } from 'zod';
import { BaseReceiptSchema, RECEIPT_TYPES, BLAKE3_HEX_LENGTH } from './base-receipt.mjs';

// =============================================================================
// Event Types
// =============================================================================

/**
 * Verification event types
 * @readonly
 * @enum {string}
 */
export const VERIFICATION_EVENT_TYPES = Object.freeze({
  HASH_VERIFIED: 'HASH_VERIFIED',
  SIGNATURE_VERIFIED: 'SIGNATURE_VERIFIED',
  MERKLE_PROOF_VERIFIED: 'MERKLE_PROOF_VERIFIED',
  CHAIN_VERIFIED: 'CHAIN_VERIFIED',
  BLOCKCHAIN_ANCHORED: 'BLOCKCHAIN_ANCHORED',
});

// =============================================================================
// Schemas
// =============================================================================

/**
 * Event type schema
 */
export const VerificationEventTypeSchema = z.enum([
  'HASH_VERIFIED',
  'SIGNATURE_VERIFIED',
  'MERKLE_PROOF_VERIFIED',
  'CHAIN_VERIFIED',
  'BLOCKCHAIN_ANCHORED',
]);

/**
 * Merkle proof step schema
 */
export const MerkleProofStepSchema = z.object({
  /** Sibling hash */
  hash: z.string().length(BLAKE3_HEX_LENGTH),
  /** Position ('left' or 'right') */
  position: z.enum(['left', 'right']),
});

/**
 * Blockchain anchor schema
 */
export const BlockchainAnchorSchema = z.object({
  /** Blockchain network (e.g., 'ethereum', 'polygon') */
  network: z.string(),
  /** Transaction hash */
  txHash: z.string(),
  /** Block number */
  blockNumber: z.number(),
  /** Block timestamp */
  blockTimestamp: z.number(),
  /** Contract address */
  contractAddress: z.string().optional(),
}).optional();

/**
 * Verification payload schema
 */
export const VerificationPayloadSchema = z.object({
  /** Verification result (e.g., 'VALID', 'INVALID', 'ANCHORED') */
  result: z.string(),
  /** Verification method used */
  method: z.string(),
  /** Additional verification details */
  details: z.record(z.string(), z.any()).optional(),
  /** Error message if verification failed */
  errorMessage: z.string().optional(),
}).passthrough();

/**
 * Verification receipt schema - extends base with verification-specific fields
 */
export const VerificationReceiptSchema = BaseReceiptSchema.extend({
  receiptType: z.literal(RECEIPT_TYPES.VERIFICATION),

  /** Event type (HASH_VERIFIED, etc.) */
  eventType: VerificationEventTypeSchema,

  /** Hash that was verified */
  verifiedHash: z.string().length(BLAKE3_HEX_LENGTH),

  /** Merkle root (if applicable) */
  merkleRoot: z.string().length(BLAKE3_HEX_LENGTH).optional(),

  /** Merkle proof path (if applicable) */
  proofPath: z.array(MerkleProofStepSchema).optional(),

  /** Signature validation result (if applicable) */
  signatureValid: z.boolean().optional(),

  /** Blockchain anchor info (if applicable) */
  blockchainAnchor: BlockchainAnchorSchema,

  /** Verification payload */
  payload: VerificationPayloadSchema,
});

// =============================================================================
// Type Definitions (JSDoc)
// =============================================================================

/**
 * @typedef {Object} MerkleProofStep
 * @property {string} hash - Sibling hash
 * @property {'left'|'right'} position - Position
 */

/**
 * @typedef {Object} BlockchainAnchor
 * @property {string} network - Blockchain network
 * @property {string} txHash - Transaction hash
 * @property {number} blockNumber - Block number
 * @property {number} blockTimestamp - Block timestamp
 * @property {string} [contractAddress] - Contract address
 */

/**
 * @typedef {Object} VerificationPayload
 * @property {string} result - Verification result
 * @property {string} method - Verification method
 * @property {Record<string, any>} [details] - Additional details
 * @property {string} [errorMessage] - Error message if failed
 */

/**
 * @typedef {Object} VerificationReceipt
 * @property {string} id - UUID of receipt
 * @property {'verification'} receiptType - Receipt type discriminator
 * @property {bigint} t_ns - Nanosecond timestamp
 * @property {string} timestamp_iso - ISO timestamp
 * @property {string|null} previousHash - Previous receipt hash
 * @property {string} payloadHash - Payload hash
 * @property {string} receiptHash - Receipt hash
 * @property {string} eventType - Verification event type
 * @property {string} verifiedHash - Hash that was verified
 * @property {string} [merkleRoot] - Merkle root
 * @property {MerkleProofStep[]} [proofPath] - Merkle proof path
 * @property {boolean} [signatureValid] - Signature validation result
 * @property {BlockchainAnchor} [blockchainAnchor] - Blockchain anchor info
 * @property {VerificationPayload} payload - Verification payload
 * @property {Object} [attestation] - Signature/attestation
 * @property {Object} [vectorClock] - Vector clock
 * @property {string} [gitRef] - Git reference
 * @property {string} [kgcEventId] - KGC event ID
 */

// =============================================================================
// Exports
// =============================================================================

export default VerificationReceiptSchema;
