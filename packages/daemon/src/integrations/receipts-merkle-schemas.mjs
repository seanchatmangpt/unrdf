/**
 * @file Receipts Merkle Schemas
 * @module @unrdf/daemon/integrations/receipts-merkle-schemas
 * @description Zod validation schemas for receipt generation and Merkle tree operations
 */

import { z } from 'zod';

/** BLAKE3 hash length in hex characters */
export const HASH_LENGTH = 64;

/** Default batch size for Merkle tree construction */
export const DEFAULT_BATCH_SIZE = 100;

/**
 * Daemon operation schema - represents a single daemon operation
 */
export const DaemonOperationSchema = z.object({
  operationId: z.string().uuid(),
  operationType: z.string().min(1),
  timestamp_ns: z.bigint(),
  nodeId: z.string().min(1),
  daemonId: z.string().min(1),
  payload: z.record(z.any()),
  hash: z.string().length(HASH_LENGTH).optional(),
});

/**
 * Receipt schema - proof of operation
 */
export const DaemonReceiptSchema = z.object({
  id: z.string().uuid(),
  operationId: z.string().uuid(),
  operationType: z.string(),
  timestamp_ns: z.bigint(),
  timestamp_iso: z.string(),
  payloadHash: z.string().length(HASH_LENGTH),
  previousHash: z.string().length(HASH_LENGTH).nullable(),
  receiptHash: z.string().length(HASH_LENGTH),
  batchIndex: z.number().int().nonnegative(),
  merkleLeafHash: z.string().length(HASH_LENGTH),
});

/**
 * Merkle tree node schema
 */
export const MerkleNodeSchema = z.object({
  hash: z.string().length(HASH_LENGTH),
  isLeaf: z.boolean(),
  level: z.number().int().nonnegative(),
});

/**
 * Merkle proof schema
 */
export const MerkleProofSchema = z.object({
  leafHash: z.string().length(HASH_LENGTH),
  leafIndex: z.number().int().nonnegative(),
  proofPath: z.array(z.object({
    hash: z.string().length(HASH_LENGTH),
    position: z.enum(['left', 'right']),
  })),
  merkleRoot: z.string().length(HASH_LENGTH),
  batchSize: z.number().int().positive(),
});

/**
 * Batch proof schema
 */
export const BatchProofSchema = z.object({
  batchId: z.string().uuid(),
  batchNumber: z.number().int().nonnegative(),
  merkleRoot: z.string().length(HASH_LENGTH),
  leafCount: z.number().int().positive(),
  treeDepth: z.number().int().nonnegative(),
  timestamp_ns: z.bigint(),
  receipts: z.array(DaemonReceiptSchema),
});

/**
 * Chain verification result schema
 */
export const ChainVerificationSchema = z.object({
  valid: z.boolean(),
  totalReceipts: z.number().int().nonnegative(),
  validReceipts: z.number().int().nonnegative(),
  tamperedReceipts: z.array(z.object({
    receiptId: z.string(),
    reason: z.string(),
  })),
  merkleRootConsistent: z.boolean(),
  chainLinksValid: z.boolean(),
});

/**
 * Generator options schema
 */
export const GeneratorOptionsSchema = z.object({
  batchSize: z.number().int().min(10).max(100).default(DEFAULT_BATCH_SIZE),
  maxBufferSize: z.number().int().min(100).max(10000).default(1000),
});
