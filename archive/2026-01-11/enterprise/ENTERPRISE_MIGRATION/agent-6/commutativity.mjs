/**
 * Commutativity Analysis for Delta Operations
 *
 * Determines if two deltas commute (A;B == B;A) and generates
 * formal proof witnesses of equivalence or counter-examples.
 *
 * @module agent-6/commutativity
 */

import { computeImpactSet, serializeImpactSet } from './impact-set.mjs';
import { detectConflict } from './conflict-detector.mjs';

/**
 * @typedef {import('./impact-set.mjs').Delta} Delta
 * @typedef {import('./impact-set.mjs').ImpactSet} ImpactSet
 * @typedef {import('./conflict-detector.mjs').ConflictResult} ConflictResult
 */

/**
 * @typedef {Object} CommutativityResult
 * @property {boolean} commutes - Whether deltas commute
 * @property {string} reason - Reason for verdict
 * @property {Object} [witness] - Proof witness (equivalence or counter-example)
 */

/**
 * Check if two deltas are read-only
 * @param {ImpactSet} impactA - Impact set A
 * @param {ImpactSet} impactB - Impact set B
 * @returns {boolean} True if both are read-only
 */
function areBothReadOnly(impactA, impactB) {
  return impactA.writes.size === 0 &&
         impactA.deletes.size === 0 &&
         impactB.writes.size === 0 &&
         impactB.deletes.size === 0;
}

/**
 * Check if two deltas write to completely disjoint sets
 * @param {ImpactSet} impactA - Impact set A
 * @param {ImpactSet} impactB - Impact set B
 * @returns {boolean} True if writes are disjoint
 */
function haveDisjointWrites(impactA, impactB) {
  // Check writes don't overlap
  for (const write of impactA.writes) {
    if (impactB.writes.has(write) || impactB.reads.has(write)) {
      return false;
    }
  }

  for (const write of impactB.writes) {
    if (impactA.reads.has(write)) {
      return false;
    }
  }

  // Check deletes don't overlap
  for (const del of impactA.deletes) {
    if (impactB.reads.has(del) || impactB.writes.has(del) || impactB.deletes.has(del)) {
      return false;
    }
  }

  for (const del of impactB.deletes) {
    if (impactA.reads.has(del) || impactA.writes.has(del)) {
      return false;
    }
  }

  return true;
}

/**
 * Check if deltas are idempotent commutative operations
 * @param {Delta} deltaA - First delta
 * @param {Delta} deltaB - Second delta
 * @returns {boolean} True if both are idempotent and same operation
 */
function areIdempotentCommutative(deltaA, deltaB) {
  // Same type of idempotent operation
  if (deltaA.type !== deltaB.type) return false;

  const type = deltaA.type.toUpperCase();

  // Idempotent operations
  const idempotentOps = ['SET', 'DELETE', 'UPSERT'];
  const isIdempotent = idempotentOps.some(op => type.includes(op));

  if (!isIdempotent) return false;

  // Same target path
  if (deltaA.path !== deltaB.path) return false;

  // For SET operations with same value
  if (type.includes('SET')) {
    return JSON.stringify(deltaA.data) === JSON.stringify(deltaB.data);
  }

  // For DELETE operations on same path
  if (type.includes('DELETE')) {
    return true;
  }

  return false;
}

/**
 * Generate equivalence witness for commutative deltas
 * @param {Delta} deltaA - First delta
 * @param {Delta} deltaB - Second delta
 * @param {ImpactSet} impactA - Impact set A
 * @param {ImpactSet} impactB - Impact set B
 * @returns {Object} Equivalence witness
 */
function generateEquivalenceWitness(deltaA, deltaB, impactA, impactB) {
  return {
    type: 'EQUIVALENCE',
    claim: 'A;B ≡ B;A',
    proof: {
      method: 'IMPACT_DISJOINT',
      impactA: serializeImpactSet(impactA),
      impactB: serializeImpactSet(impactB),
      reasoning: 'Deltas operate on disjoint resource sets, order-independent'
    },
    verified: true
  };
}

/**
 * Generate counter-example witness for non-commutative deltas
 * @param {Delta} deltaA - First delta
 * @param {Delta} deltaB - Second delta
 * @param {ConflictResult} conflict - Conflict result
 * @returns {Object} Counter-example witness
 */
function generateCounterExampleWitness(deltaA, deltaB, conflict) {
  return {
    type: 'COUNTER_EXAMPLE',
    claim: 'A;B ≢ B;A',
    proof: {
      method: 'CONFLICT_DETECTED',
      conflictType: conflict.type,
      conflictingPaths: conflict.conflictingPaths,
      reasoning: conflict.description,
      example: {
        stateInitial: 'S0',
        stateAfterAB: 'S0 → A → B',
        stateAfterBA: 'S0 → B → A',
        difference: `Results differ due to ${conflict.type} conflict`
      }
    },
    verified: true
  };
}

/**
 * Check if two deltas commute (A;B == B;A)
 *
 * Two deltas commute if executing them in either order produces
 * the same final state. This is true when:
 * 1. Both are read-only operations
 * 2. They operate on completely disjoint resource sets
 * 3. They are identical idempotent operations
 * 4. They have no read-write or write-write dependencies
 *
 * @param {Delta} deltaA - First delta
 * @param {Delta} deltaB - Second delta
 * @returns {CommutativityResult} Commutativity verdict with reasoning
 *
 * @example
 * checkCommutativity(
 *   { type: 'UPDATE', path: '/users/123', fields: ['name'] },
 *   { type: 'UPDATE', path: '/users/456', fields: ['email'] }
 * )
 * // Returns: { commutes: true, reason: '...', witness: {...} }
 *
 * @example
 * checkCommutativity(
 *   { type: 'UPDATE', path: '/counter', data: { op: 'INCREMENT' } },
 *   { type: 'UPDATE', path: '/counter', data: { op: 'INCREMENT' } }
 * )
 * // Returns: { commutes: false, reason: 'Write-write conflict', ... }
 */
export function checkCommutativity(deltaA, deltaB) {
  if (!deltaA || typeof deltaA !== 'object') {
    throw new Error('deltaA must be a non-null object');
  }

  if (!deltaB || typeof deltaB !== 'object') {
    throw new Error('deltaB must be a non-null object');
  }

  // Compute impact sets
  const impactA = computeImpactSet(deltaA);
  const impactB = computeImpactSet(deltaB);

  // Rule 1: Both read-only → always commute
  if (areBothReadOnly(impactA, impactB)) {
    return {
      commutes: true,
      reason: 'Both operations are read-only',
      witness: {
        type: 'EQUIVALENCE',
        claim: 'A;B ≡ B;A',
        proof: {
          method: 'READ_ONLY',
          reasoning: 'Read operations do not affect state, order-independent'
        },
        verified: true
      }
    };
  }

  // Rule 2: Idempotent operations with same target
  if (areIdempotentCommutative(deltaA, deltaB)) {
    return {
      commutes: true,
      reason: 'Identical idempotent operations',
      witness: {
        type: 'EQUIVALENCE',
        claim: 'A;B ≡ B;A',
        proof: {
          method: 'IDEMPOTENT',
          reasoning: 'Idempotent operations with same parameters are order-independent'
        },
        verified: true
      }
    };
  }

  // Rule 3: Check for conflicts
  const conflict = detectConflict(deltaA, deltaB);

  if (conflict.hasConflict) {
    return {
      commutes: false,
      reason: `Conflict detected: ${conflict.type}`,
      witness: generateCounterExampleWitness(deltaA, deltaB, conflict)
    };
  }

  // Rule 4: Disjoint writes → commute
  if (haveDisjointWrites(impactA, impactB)) {
    return {
      commutes: true,
      reason: 'Operations affect disjoint resource sets',
      witness: generateEquivalenceWitness(deltaA, deltaB, impactA, impactB)
    };
  }

  // Conservative: if uncertain, assume non-commutative
  return {
    commutes: false,
    reason: 'Unable to prove commutativity (conservative)',
    witness: {
      type: 'UNKNOWN',
      claim: 'A;B ? B;A',
      proof: {
        method: 'CONSERVATIVE',
        reasoning: 'Cannot statically prove equivalence, assume non-commutative'
      },
      verified: false
    }
  };
}

/**
 * Generate proof witness for commutativity check
 *
 * Creates a formal proof object showing either:
 * - Equivalence: why A;B == B;A
 * - Counter-example: why A;B != B;A
 *
 * @param {Delta} deltaA - First delta
 * @param {Delta} deltaB - Second delta
 * @returns {Object} Proof witness
 */
export function generateWitness(deltaA, deltaB) {
  const result = checkCommutativity(deltaA, deltaB);
  return result.witness;
}

/**
 * Check commutativity for a sequence of deltas
 * @param {Delta[]} deltas - Sequence of deltas
 * @returns {Object} Commutativity matrix
 */
export function checkBatchCommutativity(deltas) {
  if (!Array.isArray(deltas)) {
    throw new Error('deltas must be an array');
  }

  const matrix = [];
  let allCommute = true;

  for (let i = 0; i < deltas.length; i++) {
    for (let j = i + 1; j < deltas.length; j++) {
      const result = checkCommutativity(deltas[i], deltas[j]);
      matrix.push({
        indexA: i,
        indexB: j,
        result
      });

      if (!result.commutes) {
        allCommute = false;
      }
    }
  }

  return {
    totalPairs: (deltas.length * (deltas.length - 1)) / 2,
    commutativityMatrix: matrix,
    allCommute,
    commutativeCount: matrix.filter(m => m.result.commutes).length
  };
}

/**
 * Find maximal commutative subsets (deltas that all commute pairwise)
 * @param {Delta[]} deltas - Deltas to analyze
 * @returns {Delta[][]} Maximal commutative subsets
 */
export function findCommutativeSubsets(deltas) {
  if (!Array.isArray(deltas)) {
    throw new Error('deltas must be an array');
  }

  const subsets = [];

  // Greedy algorithm: build maximal sets
  const used = new Set();

  for (let i = 0; i < deltas.length; i++) {
    if (used.has(i)) continue;

    const subset = [deltas[i]];
    used.add(i);

    // Try to add each remaining delta
    for (let j = i + 1; j < deltas.length; j++) {
      if (used.has(j)) continue;

      // Check if j commutes with all in subset
      const commutesWithAll = subset.every(delta => {
        const result = checkCommutativity(delta, deltas[j]);
        return result.commutes;
      });

      if (commutesWithAll) {
        subset.push(deltas[j]);
        used.add(j);
      }
    }

    subsets.push(subset);
  }

  return subsets;
}

/**
 * Verify a commutativity witness
 * @param {Object} witness - Witness to verify
 * @param {Delta} deltaA - First delta
 * @param {Delta} deltaB - Second delta
 * @returns {Object} Verification result
 */
export function verifyWitness(witness, deltaA, deltaB) {
  if (!witness || typeof witness !== 'object') {
    return { valid: false, errors: ['Witness must be a non-null object'] };
  }

  const errors = [];

  // Check required fields
  if (!witness.type || !witness.claim || !witness.proof) {
    errors.push('Witness missing required fields (type, claim, proof)');
  }

  // Re-check commutativity
  const recomputed = checkCommutativity(deltaA, deltaB);

  // Verify witness claim matches actual result
  if (witness.type === 'EQUIVALENCE' && !recomputed.commutes) {
    errors.push('Witness claims equivalence but deltas do not commute');
  }

  if (witness.type === 'COUNTER_EXAMPLE' && recomputed.commutes) {
    errors.push('Witness claims counter-example but deltas commute');
  }

  return {
    valid: errors.length === 0,
    errors,
    witnessType: witness.type,
    recomputedResult: recomputed
  };
}

/**
 * Generate a commutativity certificate chain for all pairs
 * @param {Delta[]} deltas - Deltas to analyze
 * @returns {Object} Certificate chain
 */
export function generateCommutativityChain(deltas) {
  if (!Array.isArray(deltas)) {
    throw new Error('deltas must be an array');
  }

  const witnesses = [];

  for (let i = 0; i < deltas.length; i++) {
    for (let j = i + 1; j < deltas.length; j++) {
      const witness = generateWitness(deltas[i], deltas[j]);
      witnesses.push({
        indexA: i,
        indexB: j,
        witness
      });
    }
  }

  return {
    version: '1.0.0',
    timestamp: new Date().toISOString(),
    deltaCount: deltas.length,
    witnessCount: witnesses.length,
    witnesses,
    summary: {
      commutativePairs: witnesses.filter(w => w.witness.type === 'EQUIVALENCE').length,
      nonCommutativePairs: witnesses.filter(w => w.witness.type === 'COUNTER_EXAMPLE').length,
      unknownPairs: witnesses.filter(w => w.witness.type === 'UNKNOWN').length
    }
  };
}

/**
 * Check if a delta sequence can be parallelized
 * @param {Delta[]} deltas - Sequence to check
 * @returns {Object} Parallelization analysis
 */
export function analyzeParallelizability(deltas) {
  if (!Array.isArray(deltas)) {
    throw new Error('deltas must be an array');
  }

  const batchResult = checkBatchCommutativity(deltas);

  return {
    fullyParallelizable: batchResult.allCommute,
    parallelizableRatio: batchResult.commutativeCount / batchResult.totalPairs,
    commutativeSubsets: findCommutativeSubsets(deltas),
    recommendations: batchResult.allCommute
      ? 'All deltas commute - can execute in any order or in parallel'
      : 'Some deltas have dependencies - use commutativity matrix to find parallelizable subsets'
  };
}
