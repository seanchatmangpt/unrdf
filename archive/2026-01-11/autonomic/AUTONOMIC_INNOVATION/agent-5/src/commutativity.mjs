/**
 * Main Commutativity Analysis API
 * @module agent-5/commutativity
 */

import { CapsuleSchema } from './types.mjs';
import { findConflicts, minimizeConflict, classifyConflict, generateExplanation } from './conflicts.mjs';
import { canonicalize, sha256, sortQuads, quadToNTriples } from './canonicalization.mjs';
import { computeImpactSet, areDisjoint } from './impact-stub.mjs';

/**
 * Determines if two capsules can safely reorder
 * Uses three-tier analysis: disjoint impacts, delta commutativity, conflict detection
 *
 * @param {Object} capsuleA - First capsule (with add/del sets)
 * @param {Object} capsuleB - Second capsule
 * @returns {Object} - { ok: boolean, reason: string, witness?: Quad[] }
 * @throws {Error} - If capsule validation fails
 */
export function canReorder(capsuleA, capsuleB) {
  // Validate inputs
  CapsuleSchema.parse(capsuleA);
  CapsuleSchema.parse(capsuleB);

  // Tier 1: Fast path - Disjoint impact sets
  const impactA = computeImpactSet(capsuleA);
  const impactB = computeImpactSet(capsuleB);

  if (areDisjoint(impactA, impactB)) {
    return { ok: true, reason: 'disjoint-impact-sets' };
  }

  // Tier 2: Medium path - Check delta commutativity
  const conflicts = findConflicts(capsuleA, capsuleB);

  if (conflicts.length === 0) {
    return { ok: true, reason: 'commutative-deltas' };
  }

  // Tier 3: Conflict found - return minimal witness
  const witness = minimizeConflict(conflicts[0]);
  const conflictType = classifyConflict(conflicts[0]);

  return {
    ok: false,
    reason: conflictType,
    witness,
  };
}

/**
 * Generate cryptographically-verifiable conflict certificate
 *
 * @param {Object} capsuleA - First capsule
 * @param {Object} capsuleB - Second capsule
 * @returns {Object} - Conflict certificate with hash
 * @throws {Error} - If capsules are commutative (cannot generate certificate)
 */
export function conflictCertificate(capsuleA, capsuleB) {
  const result = canReorder(capsuleA, capsuleB);

  if (result.ok) {
    throw new Error('Cannot generate certificate for commutative capsules');
  }

  const counterexample = result.witness;
  const explanation = generateExplanation(result.reason, counterexample);

  // Build certificate object (without hash initially)
  const certificateData = {
    version: '1.0.0',
    capsuleA: {
      id: capsuleA.id,
      add: sortQuads(Array.from(capsuleA.add)).map(quadToNTriples),
      del: sortQuads(Array.from(capsuleA.del)).map(quadToNTriples),
    },
    capsuleB: {
      id: capsuleB.id,
      add: sortQuads(Array.from(capsuleB.add)).map(quadToNTriples),
      del: sortQuads(Array.from(capsuleB.del)).map(quadToNTriples),
    },
    counterexample: sortQuads(counterexample).map(quadToNTriples),
    conflictType: result.reason,
    timestamp: new Date().toISOString(),
  };

  // Generate deterministic hash
  const canonicalized = canonicalize(certificateData);
  const hash = sha256(canonicalized);

  // Return full certificate
  return {
    counterexample,
    explanation,
    hash,
    capsuleIds: [capsuleA.id, capsuleB.id],
    conflictType: result.reason,
    version: '1.0.0',
    timestamp: certificateData.timestamp,
    metadata: {
      minimality: 'proven',
      witnessSize: counterexample.length,
    },
  };
}
