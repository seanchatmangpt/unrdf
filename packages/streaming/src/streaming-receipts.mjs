/**
 * Streaming Operations with L5 Receipt Support
 *
 * Wraps streaming operations with per-chunk receipts and merkle proof aggregation
 *
 * @module @unrdf/streaming/streaming-receipts
 */

import { z } from 'zod';
import {
  withReceipt,
  blake3Hash,
} from '../../v6-core/src/receipt-pattern.mjs';

/**
 * Stream Chunk Schema
 */
export const StreamChunkSchema = z.object({
  index: z.number().int().nonnegative(),
  data: z.any(),
  isLast: z.boolean(),
});

/**
 * Stream Options Schema
 */
export const StreamOptionsSchema = z.object({
  chunkSize: z.number().int().positive().default(100),
  maxChunks: z.number().int().positive().optional(),
});

/**
 * Pure function: Process stream chunk
 *
 * @param {Object} chunk - Stream chunk
 * @param {Function} transform - Transform function
 * @returns {Object} Transformed chunk
 */
function processChunkImpl(chunk, transform = (x) => x) {
  const validated = StreamChunkSchema.parse(chunk);
  return {
    ...validated,
    data: transform(validated.data),
  };
}

/**
 * Wrapped: Process stream chunk with receipt
 */
export const processChunk = withReceipt(processChunkImpl, {
  operation: 'processChunk',
  profile: 'execution',
  inputSchema: z.tuple([StreamChunkSchema, z.function().optional()]),
  outputSchema: StreamChunkSchema,
});

/**
 * Generate merkle tree for stream receipts
 *
 * @param {Array<Object>} receipts - Array of chunk receipts
 * @returns {Object} Merkle root and proof
 */
export function generateStreamMerkleProof(receipts) {
  if (!Array.isArray(receipts) || receipts.length === 0) {
    throw new Error('receipts must be a non-empty array');
  }

  // Build merkle tree bottom-up
  let currentLevel = receipts.map(r => r.receiptHash);

  const tree = [currentLevel];

  while (currentLevel.length > 1) {
    const nextLevel = [];
    for (let i = 0; i < currentLevel.length; i += 2) {
      const left = currentLevel[i];
      const right = currentLevel[i + 1] || left; // Duplicate if odd
      const combined = blake3Hash(left + right);
      nextLevel.push(combined);
    }
    tree.push(nextLevel);
    currentLevel = nextLevel;
  }

  return {
    root: currentLevel[0],
    tree,
    chunkCount: receipts.length,
  };
}

/**
 * L5 Determinism Test: Stream processing
 */
export async function testStreamDeterminism(context, iterations = 100) {
  const chunk = {
    index: 0,
    data: { value: 'test-data' },
    isLast: true,
  };

  const receipts = [];
  const hashes = new Set();

  for (let i = 0; i < iterations; i++) {
    const { receipt } = await processChunk(context, chunk);
    receipts.push(receipt);
    hashes.add(receipt.receiptHash);
  }

  return {
    iterations,
    uniqueHashes: hashes.size,
    deterministic: hashes.size === 1,
    expectedHash: receipts[0].receiptHash,
  };
}
