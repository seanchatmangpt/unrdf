/**
 * @file Commutativity analysis and conflict certificate generation
 * @module agent-5/commutativity
 */

import { createHash } from 'node:crypto';

/**
 * @typedef {Object} Quad
 * @property {string} subject
 * @property {string} predicate
 * @property {string} object
 * @property {string} [operation] - 'add' or 'del'
 */

/**
 * @typedef {Object} Witness
 * @property {Quad} quadA
 * @property {Quad} quadB
 * @property {'write-write' | 'read-write' | 'contradiction'} conflictType
 * @property {'order' | 'merge' | 'manual'} canResolveBy
 */

/**
 * @typedef {Object} ReorderResult
 * @property {boolean} ok
 * @property {string} [reason]
 * @property {Witness} [witness]
 */

/**
 * @typedef {Object} Capsule
 * @property {string} id
 * @property {Object} delta
 * @property {Quad[]} [delta.add]
 * @property {Quad[]} [delta.del]
 * @property {Object} [intent]
 */

/**
 * Normalize quad to canonical string for comparison
 * @param {Quad} quad
 * @returns {string}
 */
function normalizeQuad(quad) {
  return `${quad.subject}|${quad.predicate}|${quad.object}`;
}

/**
 * Check if two quads are identical
 * @param {Quad} a
 * @param {Quad} b
 * @returns {boolean}
 */
function quadsEqual(a, b) {
  return a.subject === b.subject &&
         a.predicate === b.predicate &&
         a.object === b.object;
}

/**
 * Extract write set from capsule
 * @param {Capsule} capsule
 * @returns {Map<string, Quad[]>} Map of (subject|predicate) to quads
 */
function extractWriteSet(capsule) {
  const writeSet = new Map();

  const addQuads = capsule.delta?.add || [];
  const delQuads = capsule.delta?.del || [];

  for (const quad of addQuads) {
    const key = `${quad.subject}|${quad.predicate}`;
    if (!writeSet.has(key)) {
      writeSet.set(key, []);
    }
    writeSet.get(key).push({ ...quad, operation: 'add' });
  }

  for (const quad of delQuads) {
    const key = `${quad.subject}|${quad.predicate}`;
    if (!writeSet.has(key)) {
      writeSet.set(key, []);
    }
    writeSet.get(key).push({ ...quad, operation: 'del' });
  }

  return writeSet;
}

/**
 * Check if two capsules can safely reorder
 * @param {Capsule} capsuleA
 * @param {Capsule} capsuleB
 * @returns {ReorderResult}
 */
export function canReorder(capsuleA, capsuleB) {
  const writeSetA = extractWriteSet(capsuleA);
  const writeSetB = extractWriteSet(capsuleB);

  // Check for overlapping (subject, predicate) pairs
  for (const [key, quadsA] of writeSetA.entries()) {
    if (!writeSetB.has(key)) {
      continue; // No overlap on this key
    }

    const quadsB = writeSetB.get(key);

    // Check all pairs of quads for conflicts
    for (const quadA of quadsA) {
      for (const quadB of quadsB) {
        // Same operation on identical quad → commutative (idempotent)
        if (quadA.operation === quadB.operation && quadsEqual(quadA, quadB)) {
          continue;
        }

        // Different operations or different objects → conflict
        let conflictType;
        let canResolveBy;

        if (quadA.operation !== quadB.operation && quadsEqual(quadA, quadB)) {
          // add + del on same quad
          conflictType = 'contradiction';
          canResolveBy = 'order';
        } else if (quadA.operation === quadB.operation) {
          // Same operation, different objects
          conflictType = 'write-write';
          canResolveBy = 'manual';
        } else {
          // Different operations, different or same objects
          conflictType = 'write-write';
          canResolveBy = 'order';
        }

        return {
          ok: false,
          reason: `Conflict on (${quadA.subject}, ${quadA.predicate}): ${quadA.operation} vs ${quadB.operation}`,
          witness: {
            quadA,
            quadB,
            conflictType,
            canResolveBy
          }
        };
      }
    }
  }

  // No overlaps or all overlaps are commutative
  return { ok: true };
}

/**
 * Generate deterministic conflict certificate
 * @param {Capsule} capsuleA
 * @param {Capsule} capsuleB
 * @returns {string} Certificate or empty string if no conflict
 */
export function conflictCertificate(capsuleA, capsuleB) {
  const result = canReorder(capsuleA, capsuleB);

  if (result.ok) {
    return '';
  }

  // Create canonical witness representation
  const witnessCanonical = JSON.stringify({
    quadA: {
      subject: result.witness.quadA.subject,
      predicate: result.witness.quadA.predicate,
      object: result.witness.quadA.object,
      operation: result.witness.quadA.operation
    },
    quadB: {
      subject: result.witness.quadB.subject,
      predicate: result.witness.quadB.predicate,
      object: result.witness.quadB.object,
      operation: result.witness.quadB.operation
    },
    conflictType: result.witness.conflictType,
    canResolveBy: result.witness.canResolveBy
  }, Object.keys({
    quadA: null,
    quadB: null,
    conflictType: null,
    canResolveBy: null
  }).sort());

  const capsuleAId = capsuleA.id || 'unknown';
  const capsuleBId = capsuleB.id || 'unknown';

  // Create hash chain
  const hashInput = `${capsuleAId}|${capsuleBId}|${witnessCanonical}`;
  const signature = createHash('sha256').update(hashInput).digest('hex');

  // Format certificate
  const certificate = [
    'CONFLICT_CERT_V1',
    `capsuleA_id: ${capsuleAId}`,
    `capsuleB_id: ${capsuleBId}`,
    `witness: ${witnessCanonical}`,
    `signature: ${signature}`,
    `timestamp: ${new Date().toISOString()}`
  ].join('\n');

  return certificate;
}
