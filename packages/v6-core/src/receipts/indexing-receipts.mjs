/**
 * Indexing Operations with L5 Receipt Support
 *
 * Wraps indexing operations with receipts
 *
 * @module @unrdf/v6-core/receipts/indexing-receipts
 */

import { z } from 'zod';
import {
  withReceipt,
  blake3Hash,
  canonicalize,
} from '../receipt-pattern.mjs';

/**
 * Index Config Schema
 */
export const IndexConfigSchema = z.object({
  entity: z.string(),
  fields: z.array(z.string()),
  type: z.enum(['btree', 'hash', 'fulltext']).default('btree'),
});

/**
 * Index Result Schema
 */
export const IndexResultSchema = z.object({
  indexId: z.string(),
  indexHash: z.string().length(64),
  fieldCount: z.number().int().positive(),
  status: z.enum(['created', 'updated', 'exists']),
});

/**
 * Pure function: Create index
 *
 * @param {Object} config - Index configuration
 * @returns {Promise<Object>} Index result
 */
async function createIndexImpl(config) {
  const validated = IndexConfigSchema.parse(config);

  // Generate index hash using BLAKE3
  const indexHash = await blake3Hash(canonicalize(validated));
  const indexId = `idx-${indexHash.slice(0, 16)}`;

  return {
    indexId,
    indexHash,
    fieldCount: validated.fields.length,
    status: 'created',
  };
}

/**
 * Wrapped: Create index with receipt
 */
export const createIndex = withReceipt(createIndexImpl, {
  operation: 'createIndex',
  profile: 'index',
  inputSchema: z.tuple([IndexConfigSchema]),
  outputSchema: IndexResultSchema,
});

/**
 * L5 Determinism Test
 */
export async function testIndexingDeterminism(context, iterations = 100) {
  const config = {
    entity: 'http://example.org/Person',
    fields: ['name', 'age', 'email'],
    type: 'btree',
  };

  const receipts = [];
  const hashes = new Set();

  for (let i = 0; i < iterations; i++) {
    const { receipt } = await createIndex(context, config);
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
