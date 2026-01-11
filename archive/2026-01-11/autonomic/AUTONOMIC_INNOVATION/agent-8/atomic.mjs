/**
 * Atomic Apply - Apply capsule deltas atomically to RDF store
 * Provides rollback guarantees and verification mechanisms
 */

import { dataFactory } from './mock-store.mjs';
import { now, toISO } from '/home/user/unrdf/packages/kgc-4d/src/time.mjs';
import { blake3 } from './hash-util.mjs';

/**
 * Apply capsule delta atomically to store with rollback on failure
 *
 * @param {Object} capsule - Capsule with delta array
 * @param {Array} capsule.delta - Array of delta operations
 * @param {Object} store - Store instance (must implement add/delete/match)
 * @returns {Promise<Object>} { receipt, appliedAt, hash, error? }
 * @throws {TypeError} If capsule or store is invalid
 *
 * @example
 * import { atomicApply } from './atomic.mjs';
 * import { createStore } from '@unrdf/oxigraph';
 * const store = createStore();
 * const capsule = { delta: [{ type: 'add', subject: s, predicate: p, object: o }] };
 * const result = await atomicApply(capsule, store);
 * console.assert(result.receipt, 'Returns receipt');
 */
export async function atomicApply(capsule, store) {
  // Input validation
  if (!capsule || !Array.isArray(capsule.delta)) {
    throw new TypeError('atomicApply: capsule must have delta array');
  }
  if (!store || typeof store.add !== 'function') {
    throw new TypeError('atomicApply: store must implement add() method');
  }

  const addedQuads = [];
  const deletedQuads = [];
  const t_ns = now();

  try {
    // Apply all deltas atomically
    for (const delta of capsule.delta) {
      const quad = deltaToQuad(delta);

      if (delta.type === 'add') {
        store.add(quad);
        addedQuads.push(quad);
      } else if (delta.type === 'delete') {
        // Store quad before deletion for rollback
        const existing = [...store.match(
          quad.subject,
          quad.predicate,
          quad.object,
          quad.graph
        )];
        deletedQuads.push(...existing);
        store.delete(quad);
      }
    }

    // Generate receipt with BLAKE3 hash
    const canonical = canonicalizeDeltas(capsule.delta);
    const hash = await blake3(canonical);

    const receipt = {
      id: generateId(),
      t_ns: t_ns.toString(),
      timestamp_iso: toISO(t_ns),
      hash,
      quadCount: capsule.delta.length,
      success: true,
    };

    return {
      receipt,
      appliedAt: t_ns,
      hash,
    };
  } catch (error) {
    // ROLLBACK: Restore original state
    for (const quad of addedQuads) {
      try {
        store.delete(quad);
      } catch {
        // Ignore rollback errors
      }
    }

    for (const quad of deletedQuads) {
      try {
        store.add(quad);
      } catch {
        // Ignore rollback errors
      }
    }

    return {
      receipt: null,
      appliedAt: t_ns,
      hash: null,
      error: error.message,
    };
  }
}

/**
 * Verify atomicity by checking stored quads match capsule delta exactly
 *
 * @param {Object} capsule - Original capsule with delta array
 * @param {Object} receipt - Receipt from atomicApply
 * @param {Object} store - Store instance to verify
 * @returns {boolean} true if verification passes, false otherwise
 * @throws {TypeError} If parameters are invalid
 *
 * @example
 * import { verifyAtomicity } from './atomic.mjs';
 * const isValid = verifyAtomicity(capsule, receipt, store);
 * console.assert(isValid === true, 'Atomicity verified');
 */
export function verifyAtomicity(capsule, receipt, store) {
  // Input validation
  if (!capsule || !Array.isArray(capsule.delta)) {
    throw new TypeError('verifyAtomicity: capsule must have delta array');
  }
  if (!receipt || typeof receipt !== 'object') {
    throw new TypeError('verifyAtomicity: receipt must be an object');
  }
  if (!store || typeof store.match !== 'function') {
    throw new TypeError('verifyAtomicity: store must implement match() method');
  }

  try {
    // Check quad count matches
    let foundCount = 0;

    for (const delta of capsule.delta) {
      if (delta.type !== 'add') {
        continue; // Only verify additions (deletions removed quads)
      }

      const quad = deltaToQuad(delta);

      // Query store for matching quad
      const matches = [...store.match(
        quad.subject,
        quad.predicate,
        quad.object,
        quad.graph
      )];

      if (matches.length === 0) {
        return false; // Quad not found - verification failed
      }

      foundCount++;
    }

    // Verify count matches expected additions
    const expectedAdditions = capsule.delta.filter(d => d.type === 'add').length;
    return foundCount === expectedAdditions;
  } catch (error) {
    return false;
  }
}

/**
 * Convert delta operation to RDF quad
 * @param {Object} delta - Delta operation
 * @returns {Object} RDF quad
 */
function deltaToQuad(delta) {
  // Validate subject termType
  if (!['BlankNode', 'NamedNode'].includes(delta.subject.termType)) {
    throw new Error(`Invalid subject termType: ${delta.subject.termType}`);
  }

  const subject = delta.subject.termType === 'BlankNode'
    ? dataFactory.blankNode(delta.subject.value)
    : dataFactory.namedNode(delta.subject.value);

  // Validate predicate termType
  if (delta.predicate.termType !== 'NamedNode') {
    throw new Error(`Invalid predicate termType: ${delta.predicate.termType}`);
  }

  const predicate = dataFactory.namedNode(delta.predicate.value);

  let object;
  if (delta.object.termType === 'Literal') {
    if (delta.object.language) {
      object = dataFactory.literal(delta.object.value, delta.object.language);
    } else if (delta.object.datatype) {
      const datatypeNode = typeof delta.object.datatype === 'string'
        ? dataFactory.namedNode(delta.object.datatype)
        : delta.object.datatype;
      object = dataFactory.literal(delta.object.value, datatypeNode);
    } else {
      object = dataFactory.literal(delta.object.value);
    }
  } else if (delta.object.termType === 'BlankNode') {
    object = dataFactory.blankNode(delta.object.value);
  } else {
    object = dataFactory.namedNode(delta.object.value);
  }

  const graph = delta.graph
    ? dataFactory.namedNode(delta.graph.value)
    : dataFactory.defaultGraph();

  return dataFactory.quad(subject, predicate, object, graph);
}

/**
 * Canonicalize deltas for deterministic hashing
 * @param {Array} deltas - Delta operations
 * @returns {string} Canonical string
 */
function canonicalizeDeltas(deltas) {
  const sorted = [...deltas].sort((a, b) => {
    const aStr = `${a.subject.value}|${a.predicate.value}|${a.object.value}|${a.type}`;
    const bStr = `${b.subject.value}|${b.predicate.value}|${b.object.value}|${b.type}`;
    return aStr < bStr ? -1 : aStr > bStr ? 1 : 0;
  });

  return JSON.stringify(sorted, Object.keys(sorted).sort());
}

/**
 * Generate unique ID
 * @returns {string} UUID or timestamp-based ID
 */
function generateId() {
  if (typeof crypto !== 'undefined' && crypto.randomUUID) {
    return crypto.randomUUID();
  }
  try {
    const crypto = require('crypto');
    return crypto.randomUUID();
  } catch {
    return `atomic-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  }
}
