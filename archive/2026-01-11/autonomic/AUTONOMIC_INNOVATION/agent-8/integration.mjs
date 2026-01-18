/**
 * Integration Layer - KGC-4D and Oxigraph integration
 * Connects atomic apply with freeze snapshots and store instances
 */

import { createStore, dataFactory } from './mock-store.mjs';
import { now, toISO } from '/home/user/unrdf/packages/kgc-4d/src/time.mjs';
import { blake3 } from './hash-util.mjs';

/**
 * Integrate capsule with KGC-4D freeze mechanism
 *
 * @param {Object} capsule - Capsule with delta array
 * @returns {Promise<Object>} { frozenSnapshot, hash }
 * @throws {TypeError} If capsule is invalid
 *
 * @example
 * import { integrateWithKGC4D } from './integration.mjs';
 * const result = await integrateWithKGC4D(capsule);
 * console.assert(result.hash, 'Returns hash');
 */
export async function integrateWithKGC4D(capsule) {
  // Input validation
  if (!capsule || typeof capsule !== 'object') {
    throw new TypeError('integrateWithKGC4D: capsule must be an object');
  }
  if (!Array.isArray(capsule.delta)) {
    throw new TypeError('integrateWithKGC4D: capsule must have delta array');
  }

  try {
    // Create receipt from capsule
    const t_ns = now();
    const canonical = canonicalizeDeltas(capsule.delta);
    const hash = await blake3(canonical);

    const receipt = {
      id: generateId(),
      t_ns: t_ns.toString(),
      timestamp_iso: toISO(t_ns),
      hash,
      quadCount: capsule.delta.length,
      capsuleHash: capsule.hash || hash,
    };

    // Create frozen snapshot representation
    const frozenSnapshot = {
      receipt,
      deltaCount: capsule.delta.length,
      createdAt: toISO(t_ns),
      frozen: true,
    };

    return {
      frozenSnapshot,
      hash,
    };
  } catch (error) {
    throw new Error(`Failed to integrate with KGC-4D: ${error.message}`);
  }
}

/**
 * Integrate capsule with Oxigraph store
 *
 * @param {Object} capsule - Capsule with delta array
 * @param {Object} [store] - Optional store instance (creates new if not provided)
 * @returns {Promise<Object>} { success, error?, quadCount }
 * @throws {TypeError} If capsule is invalid
 *
 * @example
 * import { integrateWithOxigraph } from './integration.mjs';
 * const result = await integrateWithOxigraph(capsule);
 * console.assert(result.success === true, 'Integration succeeded');
 */
export async function integrateWithOxigraph(capsule, store) {
  // Input validation
  if (!capsule || typeof capsule !== 'object') {
    throw new TypeError('integrateWithOxigraph: capsule must be an object');
  }
  if (!Array.isArray(capsule.delta)) {
    throw new TypeError('integrateWithOxigraph: capsule must have delta array');
  }

  const targetStore = store || createStore();
  const addedQuads = [];
  const deletedQuads = [];

  try {
    // Convert and apply deltas
    for (const delta of capsule.delta) {
      const quad = deltaToQuad(delta);

      if (delta.type === 'add') {
        targetStore.add(quad);
        addedQuads.push(quad);
      } else if (delta.type === 'delete') {
        // Store existing quads for rollback
        const existing = [...targetStore.match(
          quad.subject,
          quad.predicate,
          quad.object,
          quad.graph
        )];
        deletedQuads.push(...existing);
        targetStore.delete(quad);
      }
    }

    return {
      success: true,
      quadCount: capsule.delta.length,
    };
  } catch (error) {
    // ROLLBACK on failure
    for (const quad of addedQuads) {
      try {
        targetStore.delete(quad);
      } catch {
        // Ignore rollback errors
      }
    }

    for (const quad of deletedQuads) {
      try {
        targetStore.add(quad);
      } catch {
        // Ignore rollback errors
      }
    }

    return {
      success: false,
      error: error.message,
      quadCount: 0,
    };
  }
}

/**
 * Convert delta to RDF quad
 * @param {Object} delta - Delta operation
 * @returns {Object} RDF quad
 */
function deltaToQuad(delta) {
  const subject = delta.subject.termType === 'BlankNode'
    ? dataFactory.blankNode(delta.subject.value)
    : dataFactory.namedNode(delta.subject.value);

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
 * Canonicalize deltas
 * @param {Array} deltas - Delta array
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
 * @returns {string} UUID
 */
function generateId() {
  if (typeof crypto !== 'undefined' && crypto.randomUUID) {
    return crypto.randomUUID();
  }
  try {
    const crypto = require('crypto');
    return crypto.randomUUID();
  } catch {
    return `integration-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  }
}
