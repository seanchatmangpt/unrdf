/**
 * Receipt Utilities - Hashing, verification, and delta serialization
 * @module agent-8/utils
 */

import { createBLAKE3 } from 'hash-wasm';
import { z } from 'zod';

/**
 * Serialize quad to N-Quads format (canonical)
 * @param {Object} quad - RDF quad
 * @returns {string} N-Quad string
 *
 * @example
 * const quad = dataFactory.quad(
 *   dataFactory.namedNode('http://ex.org/s'),
 *   dataFactory.namedNode('http://ex.org/p'),
 *   dataFactory.literal('object')
 * );
 * const nquad = quadToNQuad(quad);
 * // Returns: '<http://ex.org/s> <http://ex.org/p> "object" .'
 */
export function quadToNQuad(quad) {
  const s = termToNQuad(quad.subject);
  const p = termToNQuad(quad.predicate);
  const o = termToNQuad(quad.object);
  const g = quad.graph ? ` ${termToNQuad(quad.graph)}` : '';
  return `${s} ${p} ${o}${g} .`;
}

/**
 * Serialize RDF term to N-Quads format
 * @param {Object} term - RDF term
 * @returns {string} Serialized term
 */
function termToNQuad(term) {
  if (!term || !term.termType) {
    throw new TypeError('Invalid RDF term: missing termType');
  }

  switch (term.termType) {
    case 'NamedNode':
      return `<${term.value}>`;
    case 'BlankNode':
      return `_:${term.value}`;
    case 'Literal': {
      const escaped = term.value
        .replace(/\\/g, '\\\\')
        .replace(/"/g, '\\"')
        .replace(/\n/g, '\\n')
        .replace(/\r/g, '\\r')
        .replace(/\t/g, '\\t');

      if (term.language) {
        return `"${escaped}"@${term.language}`;
      }
      if (term.datatype && term.datatype.value !== 'http://www.w3.org/2001/XMLSchema#string') {
        return `"${escaped}"^^<${term.datatype.value}>`;
      }
      return `"${escaped}"`;
    }
    case 'DefaultGraph':
      return '';
    default:
      throw new TypeError(`Unknown term type: ${term.termType}`);
  }
}

/**
 * Serialize delta for hashing (canonical, deterministic)
 * @param {Object} delta - Capsule delta
 * @param {Array<Object>} delta.add - Quads to add
 * @param {Array<Object>} delta.del - Quads to delete
 * @returns {string} Canonical N-Quads representation
 *
 * @example
 * const delta = {
 *   add: [quad1, quad2],
 *   del: [quad3]
 * };
 * const canonical = serializeDelta(delta);
 */
export function serializeDelta(delta) {
  if (!delta || typeof delta !== 'object') {
    throw new TypeError('Delta must be an object');
  }

  const add = Array.isArray(delta.add) ? delta.add : [];
  const del = Array.isArray(delta.del) ? delta.del : [];

  // Convert to N-Quads and sort for deterministic ordering
  const addNQuads = add.map(quadToNQuad).sort();
  const delNQuads = del.map(quadToNQuad).sort();

  return JSON.stringify({
    add: addNQuads.join('\n'),
    del: delNQuads.join('\n')
  });
}

/**
 * Hash receipt data using BLAKE3 (deterministic)
 * @param {Object} data - Data to hash
 * @returns {Promise<string>} BLAKE3 hash (hex)
 *
 * @example
 * const hash = await hashReceipt({ delta, timestamp: '2025-12-26...' });
 * console.assert(hash.length === 64, 'BLAKE3 hash is 64 hex chars');
 */
export async function hashReceipt(data) {
  if (!data || typeof data !== 'object') {
    throw new TypeError('Data to hash must be an object');
  }

  // Canonical JSON serialization
  const canonical = JSON.stringify(data, Object.keys(data).sort());

  // BLAKE3 hash
  const hasher = await createBLAKE3();
  hasher.init();
  hasher.update(canonical);
  return hasher.digest('hex');
}

/**
 * Verify receipt hash matches data
 * @param {Object} receipt - Receipt with hash field
 * @param {Object} data - Original data that was hashed
 * @returns {Promise<boolean>} True if hash matches
 *
 * @example
 * const receipt = { hash: 'abc123...', timestamp: '...' };
 * const data = { delta: {...}, timestamp: '...' };
 * const valid = await verifyReceipt(receipt, data);
 */
export async function verifyReceipt(receipt, data) {
  if (!receipt || typeof receipt !== 'object' || !receipt.hash) {
    throw new TypeError('Receipt must have a hash field');
  }

  const expectedHash = await hashReceipt(data);
  return receipt.hash === expectedHash;
}

/**
 * Generate deterministic receipt from delta and metadata
 * @param {Object} delta - Capsule delta
 * @param {string} timestamp - ISO timestamp
 * @param {Object} [options] - Additional options
 * @param {string} [options.parentHash] - Parent receipt hash
 * @param {Object} [options.metadata] - Capsule metadata
 * @returns {Promise<Object>} Receipt object
 *
 * @example
 * const receipt = await generateReceipt(delta, timestamp, { parentHash: 'abc...' });
 */
export async function generateReceipt(delta, timestamp, options = {}) {
  const { parentHash = null, metadata = {} } = options;

  // Serialize delta
  const deltaStr = serializeDelta(delta);

  // Create hashable data
  const hashData = {
    delta: deltaStr,
    timestamp,
    parentHash
  };

  // Generate hash
  const hash = await hashReceipt(hashData);

  // Count operations
  const added = Array.isArray(delta.add) ? delta.add.length : 0;
  const deleted = Array.isArray(delta.del) ? delta.del.length : 0;

  return {
    hash,
    timestamp,
    success: true,
    parentHash,
    stats: {
      added,
      deleted,
      capsuleId: metadata.id || null
    }
  };
}

/**
 * Deserialize delta from N-Quads string
 * NOTE: This is a simplified version - full RDF parsing should use @unrdf/oxigraph
 * @param {string} nquads - N-Quads string
 * @returns {Object} Delta object with add/del arrays
 */
export function deserializeDelta(nquads) {
  if (typeof nquads !== 'string') {
    throw new TypeError('N-Quads must be a string');
  }

  // This is a placeholder - actual implementation would use proper RDF parser
  // For now, return empty delta
  return {
    add: [],
    del: []
  };
}

/**
 * Receipt schema for validation
 */
export const ReceiptSchema = z.object({
  hash: z.string().length(64), // BLAKE3 hex
  timestamp: z.string(), // ISO 8601
  success: z.boolean(),
  parentHash: z.string().length(64).nullable(),
  stats: z.object({
    added: z.number().int().nonnegative(),
    deleted: z.number().int().nonnegative(),
    capsuleId: z.string().nullable().optional()
  })
});
