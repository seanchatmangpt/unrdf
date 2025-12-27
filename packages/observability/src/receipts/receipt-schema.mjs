/**
 * Receipt Schema Definitions
 *
 * Zod schemas for tamper-evident receipt system
 * Supports hash chaining, merkle proofs, and external anchoring
 *
 * @module @unrdf/observability/receipts/receipt-schema
 */

import { z } from 'zod';

/**
 * Receipt schema - cryptographic proof of an operation
 *
 * Fields:
 * - id: Unique receipt identifier (UUID or deterministic)
 * - hash: BLAKE3 hash of receipt content (64-char hex)
 * - timestamp_ns: Nanosecond timestamp (BigInt as string)
 * - timestamp_iso: ISO 8601 timestamp for human readability
 * - operation: Operation type ('admit', 'freeze', 'publish', etc.)
 * - payload: Operation-specific data (any JSON)
 * - previousHash: Hash of previous receipt (null for genesis)
 * - actor: Who performed the operation
 * - metadata: Optional additional metadata
 */
export const ReceiptSchema = z.object({
  id: z.string().min(1),
  hash: z.string().regex(/^[0-9a-f]{64}$/i, 'Must be 64-character hex hash'),
  timestamp_ns: z.string().regex(/^\d+$/, 'Must be numeric string'),
  timestamp_iso: z.string().datetime(),
  operation: z.string().min(1),
  payload: z.any(),
  previousHash: z.string().regex(/^[0-9a-f]{64}$/i).nullable(),
  actor: z.string().min(1).optional(),
  metadata: z.record(z.any()).optional(),
});

/**
 * Merkle proof schema - proves receipt inclusion in tree
 */
export const MerkleProofSchema = z.object({
  receiptId: z.string(),
  receiptHash: z.string().regex(/^[0-9a-f]{64}$/i),
  root: z.string().regex(/^[0-9a-f]{64}$/i),
  siblings: z.array(z.object({
    hash: z.string().regex(/^[0-9a-f]{64}$/i),
    position: z.enum(['left', 'right']),
  })),
  index: z.number().int().nonnegative(),
});

/**
 * Anchor schema - external timestamping/anchoring proof
 */
export const AnchorSchema = z.object({
  merkleRoot: z.string().regex(/^[0-9a-f]{64}$/i),
  anchorType: z.enum(['blockchain', 'git', 'timestamp-service']),
  anchorData: z.object({
    // Blockchain
    txHash: z.string().optional(),
    blockNumber: z.number().optional(),
    network: z.string().optional(),
    // Git
    commitSha: z.string().optional(),
    repository: z.string().optional(),
    // Timestamp service
    timestampToken: z.string().optional(),
    authority: z.string().optional(),
  }),
  timestamp: z.string().datetime(),
});

/**
 * Verification result schema
 */
export const VerificationResultSchema = z.object({
  valid: z.boolean(),
  receiptId: z.string().optional(),
  errors: z.array(z.string()).default([]),
  checks: z.object({
    hashIntegrity: z.boolean().optional(),
    chainLink: z.boolean().optional(),
    temporalOrder: z.boolean().optional(),
    merkleProof: z.boolean().optional(),
  }).optional(),
});

/**
 * Chain export schema - audit trail export
 */
export const ChainExportSchema = z.object({
  chainId: z.string(),
  receiptCount: z.number().int().nonnegative(),
  firstReceipt: z.string().datetime().optional(),
  lastReceipt: z.string().datetime().optional(),
  merkleRoot: z.string().regex(/^[0-9a-f]{64}$/i),
  chainValid: z.boolean(),
  receipts: z.array(ReceiptSchema),
  exportedAt: z.string().datetime(),
  anchor: AnchorSchema.optional(),
});

/**
 * Type exports for TypeScript/JSDoc
 */

/**
 * @typedef {z.infer<typeof ReceiptSchema>} Receipt
 * @typedef {z.infer<typeof MerkleProofSchema>} MerkleProof
 * @typedef {z.infer<typeof AnchorSchema>} Anchor
 * @typedef {z.infer<typeof VerificationResultSchema>} VerificationResult
 * @typedef {z.infer<typeof ChainExportSchema>} ChainExport
 */

export default {
  ReceiptSchema,
  MerkleProofSchema,
  AnchorSchema,
  VerificationResultSchema,
  ChainExportSchema,
};
