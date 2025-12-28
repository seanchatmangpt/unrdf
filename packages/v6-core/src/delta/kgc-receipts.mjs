/**
 * KGC Delta Generation with L5 Receipt Support
 *
 * Wraps delta generation with merkle proof and receipts
 *
 * @module @unrdf/v6-core/delta/kgc-receipts
 */

import { z } from 'zod';
import {
  withReceipt,
  blake3Hash,
  canonicalize,
} from '../receipt-pattern.mjs';

/**
 * Delta Schema
 */
export const DeltaSchema = z.object({
  type: z.enum(['create', 'update', 'delete', 'composite']),
  target: z.object({
    entity: z.string(),
    scope: z.string().optional(),
  }),
  changes: z.array(
    z.object({
      operation: z.enum(['add_triple', 'remove_triple', 'replace_value', 'execute_sparql']),
      subject: z.any().optional(),
      predicate: z.any().optional(),
      object: z.any().optional(),
      sparql: z.string().optional(),
    })
  ),
});

/**
 * Delta Result Schema
 */
export const DeltaResultSchema = z.object({
  deltaId: z.string(),
  deltaHash: z.string().length(64),
  merkleProof: z.object({
    root: z.string().length(64),
    leaves: z.array(z.string()),
  }),
  changeCount: z.number().int().nonnegative(),
});

/**
 * Pure function: Generate delta
 *
 * @param {Object} delta - Delta specification
 * @returns {Object} Delta result with merkle proof
 */
function generateDeltaImpl(delta) {
  const validated = DeltaSchema.parse(delta);

  // Generate delta hash
  const deltaHash = blake3Hash(canonicalize(validated));

  // Generate merkle tree for changes
  const leaves = validated.changes.map(change => blake3Hash(canonicalize(change)));
  const merkleRoot = leaves.length > 0 ? blake3Hash(canonicalize(leaves)) : deltaHash;

  return {
    deltaId: `delta-${deltaHash.slice(0, 16)}`,
    deltaHash,
    merkleProof: {
      root: merkleRoot,
      leaves,
    },
    changeCount: validated.changes.length,
  };
}

/**
 * Wrapped: Generate delta with receipt
 */
export const generateDelta = withReceipt(generateDeltaImpl, {
  operation: 'generateDelta',
  profile: 'delta',
  inputSchema: z.tuple([DeltaSchema]),
  outputSchema: DeltaResultSchema,
});

/**
 * L5 Determinism Test
 */
export async function testKGCDeterminism(context, iterations = 100) {
  const delta = {
    type: 'create',
    target: {
      entity: 'http://example.org/Alice',
      scope: 'default',
    },
    changes: [
      {
        operation: 'add_triple',
        subject: { value: 'http://example.org/Alice' },
        predicate: { value: 'http://xmlns.com/foaf/0.1/name' },
        object: { value: 'Alice' },
      },
    ],
  };

  const receipts = [];
  const hashes = new Set();

  for (let i = 0; i < iterations; i++) {
    const { receipt } = await generateDelta(context, delta);
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
