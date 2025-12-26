/**
 * Atomic Capsule Application - Apply capsule deltas with ACID guarantees
 * @module agent-8/apply
 */

import { dataFactory } from '@unrdf/oxigraph';
import { z } from 'zod';
import { generateReceipt, serializeDelta } from './utils.mjs';
import { UNIVERSE_GRAPH } from './store.mjs';

/**
 * RDF Term schemas for validation
 */
const NamedNodeSchema = z.object({
  termType: z.literal('NamedNode'),
  value: z.string()
});

const BlankNodeSchema = z.object({
  termType: z.literal('BlankNode'),
  value: z.string()
});

const LiteralSchema = z.object({
  termType: z.literal('Literal'),
  value: z.string(),
  language: z.string().optional(),
  datatype: z.any().optional()
});

const RdfTermSchema = z.union([
  NamedNodeSchema,
  BlankNodeSchema,
  LiteralSchema
]);

const QuadSchema = z.object({
  subject: z.union([NamedNodeSchema, BlankNodeSchema]),
  predicate: NamedNodeSchema,
  object: RdfTermSchema,
  graph: z.any().optional()
});

/**
 * Capsule schema for validation
 */
export const CapsuleSchema = z.object({
  delta: z.object({
    add: z.array(QuadSchema).optional().default([]),
    del: z.array(QuadSchema).optional().default([])
  }),
  metadata: z.object({
    id: z.string().optional(),
    label: z.string().optional(),
    timestamp: z.string().optional()
  }).optional().default({})
});

/**
 * Validate capsule structure before apply
 * @param {Object} capsule - Capsule to validate
 * @returns {Object} Validated capsule
 * @throws {TypeError} If capsule is malformed
 *
 * @example
 * const validated = validateCapsule(capsule);
 */
export function validateCapsule(capsule) {
  if (!capsule || typeof capsule !== 'object') {
    throw new TypeError('Capsule must be an object');
  }

  try {
    return CapsuleSchema.parse(capsule);
  } catch (error) {
    if (error instanceof z.ZodError) {
      const issues = error.issues.map(i => `${i.path.join('.')}: ${i.message}`).join(', ');
      throw new TypeError(`Invalid capsule structure: ${issues}`);
    }
    throw error;
  }
}

/**
 * Apply capsule deltas atomically to store
 *
 * Implements ACID semantics:
 * - Atomicity: All operations succeed or all fail (with rollback)
 * - Consistency: Quads are valid RDF
 * - Isolation: No partial state visible
 * - Durability: Changes persist in store
 *
 * @param {AtomicStore} store - Target store
 * @param {Object} capsule - Capsule from Agent 2/3
 * @param {Object} capsule.delta - Delta operations
 * @param {Array<Object>} [capsule.delta.add] - Quads to add
 * @param {Array<Object>} [capsule.delta.del] - Quads to delete
 * @param {Object} [capsule.metadata] - Optional metadata
 * @param {Object} [options] - Apply options
 * @param {string} [options.parentHash] - Parent receipt hash for chain
 * @returns {Promise<Object>} Receipt { hash, timestamp, success, parentHash, stats }
 * @throws {Error} If operation fails (with rollback)
 *
 * @example
 * import { applyCapsule } from './apply.mjs';
 * import { createAtomicStore } from './store.mjs';
 *
 * const store = createAtomicStore();
 * const capsule = {
 *   delta: {
 *     add: [quad1, quad2],
 *     del: []
 *   }
 * };
 *
 * const receipt = await applyCapsule(store, capsule);
 * console.log(receipt.hash); // BLAKE3 hash
 * console.log(receipt.stats.added); // 2
 */
export async function applyCapsule(store, capsule, options = {}) {
  // 1. VALIDATE INPUT
  if (!store || typeof store !== 'object') {
    throw new TypeError('Store must be an AtomicStore instance');
  }

  const validated = validateCapsule(capsule);
  const { delta, metadata } = validated;

  // 2. CAPTURE ROLLBACK STATE
  const addedQuads = [];
  const deletedQuads = [];
  const originalSize = store.size();

  try {
    // 3. APPLY DELETES FIRST (order matters for idempotence)
    for (const deltaQuad of delta.del) {
      // Create quad with universe graph
      const quad = dataFactory.quad(
        deltaQuad.subject,
        deltaQuad.predicate,
        deltaQuad.object,
        deltaQuad.graph || UNIVERSE_GRAPH
      );

      // Track existing quads before deletion
      const exists = [...store.match(
        quad.subject,
        quad.predicate,
        quad.object,
        quad.graph
      )];

      if (exists.length > 0) {
        deletedQuads.push(...exists);
        for (const existingQuad of exists) {
          store.delete(existingQuad);
        }
      }
    }

    // 4. APPLY ADDS
    for (const deltaQuad of delta.add) {
      // Create quad with universe graph
      const quad = dataFactory.quad(
        deltaQuad.subject,
        deltaQuad.predicate,
        deltaQuad.object,
        deltaQuad.graph || UNIVERSE_GRAPH
      );

      store.add(quad);
      addedQuads.push(quad);
    }

    // 5. GENERATE RECEIPT
    const timestamp = new Date().toISOString();
    const receipt = await generateReceipt(delta, timestamp, {
      parentHash: options.parentHash || null,
      metadata
    });

    // 6. STORE RECEIPT IN STORE (for chain tracking)
    store._addReceipt(receipt);

    return receipt;

  } catch (error) {
    // 7. ROLLBACK ON ERROR

    // Re-add deleted quads
    for (const quad of deletedQuads) {
      try {
        store.add(quad);
      } catch (rollbackError) {
        // Log but continue rollback
        console.error('Rollback warning: failed to re-add quad', rollbackError);
      }
    }

    // Remove added quads
    for (const quad of addedQuads) {
      try {
        store.delete(quad);
      } catch (rollbackError) {
        // Log but continue rollback
        console.error('Rollback warning: failed to remove quad', rollbackError);
      }
    }

    // Verify rollback succeeded
    const finalSize = store.size();
    if (finalSize !== originalSize) {
      throw new Error(
        `CRITICAL: Rollback failed - store size ${finalSize} != original ${originalSize}. Original error: ${error.message}`
      );
    }

    // Throw original error with rollback context
    throw new Error(`applyCapsule failed (rolled back): ${error.message}`);
  }
}

/**
 * Apply multiple capsules sequentially (batched)
 *
 * Each capsule is applied atomically, and receipts are chained.
 * If any capsule fails, previous capsules remain applied (no global rollback).
 *
 * @param {AtomicStore} store - Target store
 * @param {Array<Object>} capsules - Array of capsules
 * @returns {Promise<Array<Object>>} Array of receipts
 * @throws {Error} If any capsule application fails
 *
 * @example
 * const receipts = await applyBatch(store, [capsule1, capsule2, capsule3]);
 * console.log(receipts.length); // 3
 * console.log(receipts[1].parentHash === receipts[0].hash); // true
 */
export async function applyBatch(store, capsules) {
  if (!Array.isArray(capsules)) {
    throw new TypeError('Capsules must be an array');
  }

  const receipts = [];
  let parentHash = null;

  for (const capsule of capsules) {
    const receipt = await applyCapsule(store, capsule, { parentHash });
    receipts.push(receipt);
    parentHash = receipt.hash;
  }

  return receipts;
}
