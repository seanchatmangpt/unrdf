/**
 * Conflict Detection and Minimization
 * @module agent-5/conflicts
 */

import { quadToNTriples } from './canonicalization.mjs';

/**
 * Check if two quads are equal
 * @param {Object} q1 - First quad
 * @param {Object} q2 - Second quad
 * @returns {boolean} - True if quads are equal
 */
export function quadsEqual(q1, q2) {
  return quadToNTriples(q1) === quadToNTriples(q2);
}

/**
 * Find all conflicting quad pairs between two capsules
 * @param {Object} capsuleA - First capsule
 * @param {Object} capsuleB - Second capsule
 * @returns {Array<Object>} - Array of conflict objects
 */
export function findConflicts(capsuleA, capsuleB) {
  const conflicts = [];

  // Convert Sets to Arrays for easier iteration
  const aAdd = Array.from(capsuleA.add);
  const aDel = Array.from(capsuleA.del);
  const bAdd = Array.from(capsuleB.add);
  const bDel = Array.from(capsuleB.del);

  // Check: A adds what B deletes
  for (const quad of aAdd) {
    for (const bQuad of bDel) {
      if (quadsEqual(quad, bQuad)) {
        conflicts.push({
          type: 'add-del-conflict',
          quad,
          direction: 'A-adds-B-deletes',
        });
      }
    }
  }

  // Check: B adds what A deletes
  for (const quad of bAdd) {
    for (const aQuad of aDel) {
      if (quadsEqual(quad, aQuad)) {
        conflicts.push({
          type: 'add-del-conflict',
          quad,
          direction: 'B-adds-A-deletes',
        });
      }
    }
  }

  return conflicts;
}

/**
 * Minimize conflict to smallest quad set
 * For single-quad conflicts, returns the quad itself
 * @param {Object} conflict - Conflict object from findConflicts
 * @returns {Array<Object>} - Minimal witness set (array of quads)
 */
export function minimizeConflict(conflict) {
  // For single-quad conflicts, the quad itself is minimal
  if (conflict.quad) {
    return [conflict.quad];
  }

  // For complex conflicts (future expansion)
  return [conflict.quad];
}

/**
 * Classify conflict type for human-readable output
 * @param {Object} conflict - Conflict object
 * @returns {string} - Conflict type classification
 */
export function classifyConflict(conflict) {
  if (conflict.type === 'add-del-conflict') {
    if (conflict.direction === 'A-adds-B-deletes') {
      return 'add-del-conflict';
    }
    if (conflict.direction === 'B-adds-A-deletes') {
      return 'del-add-conflict';
    }
  }

  if (conflict.type === 'write-write-conflict') {
    return 'write-write-conflict';
  }

  return 'unknown-conflict';
}

/**
 * Generate human-readable explanation for conflict
 * @param {string} conflictType - Classification from classifyConflict
 * @param {Array<Object>} witness - Witness quads
 * @returns {string} - Human-readable explanation
 */
export function generateExplanation(conflictType, witness) {
  if (witness.length === 0) {
    return 'Conflict detected but no witness available';
  }

  const quadStr = quadToNTriples(witness[0]);

  const templates = {
    'add-del-conflict': () =>
      `Capsule A adds triple ${quadStr}, but Capsule B deletes it. Order matters.`,
    'del-add-conflict': () =>
      `Capsule A deletes triple ${quadStr}, but Capsule B adds it. Order matters.`,
    'write-write-conflict': () =>
      `Both capsules modify ${quadStr} with different operations. Non-commutative.`,
  };

  return templates[conflictType]?.() || `Conflict detected: ${conflictType}`;
}
