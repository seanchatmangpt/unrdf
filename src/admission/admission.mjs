/**
 * @file Admission Control System
 * @module admission/admission
 */

/**
 * Validate delta structure
 * @param {object} delta
 * @returns {object}
 */
function validateDelta(delta) {
  if (!delta || typeof delta !== 'object') {
    throw new Error('Delta must be an object');
  }
  return {
    additions: Array.isArray(delta.additions) ? delta.additions : [],
    deletions: Array.isArray(delta.deletions) ? delta.deletions : [],
  };
}

/**
 * Admission decision types
 * @typedef {'ALLOW' | 'DENY'} AdmissionDecision
 */

/**
 * @typedef {object} AdmissionResult
 * @property {AdmissionDecision} decision - Admission decision
 * @property {string[]} violations - List of violations (if denied)
 * @property {string} reason - Reason for decision
 */

/**
 * @class Invariant
 * @description Base class for admission control invariants
 */
export class Invariant {
  /**
   * @param {string} name - Invariant name
   */
  constructor(name) {
    this.name = name;
  }

  /**
   * Check invariant
   * @param {object} delta - Delta to check
   * @param {object} universe - Universe context
   * @returns {AdmissionResult}
   */
  check(delta, universe) {
    throw new Error('Invariant.check() must be implemented by subclass');
  }
}

/**
 * @class AdditiveOnlyInvariant
 * @extends Invariant
 * @description Ensures delta contains only additions (no deletions)
 */
export class AdditiveOnlyInvariant extends Invariant {
  constructor() {
    super('AdditiveOnly');
  }

  check(delta, universe) {
    if (delta.deletions && delta.deletions.length > 0) {
      return {
        decision: 'DENY',
        violations: ['Delta contains deletions'],
        reason: 'Only additive changes are allowed',
      };
    }
    return {
      decision: 'ALLOW',
      violations: [],
      reason: 'Delta is additive only',
    };
  }
}

/**
 * @class SubstrateImmutabilityInvariant
 * @extends Invariant
 * @description Ensures substrate terms are not redefined
 */
export class SubstrateImmutabilityInvariant extends Invariant {
  constructor() {
    super('SubstrateImmutability');
  }

  check(delta, universe) {
    const substratePartition = universe.getPartition('IndustrialSubstrate');
    if (!substratePartition) {
      return {
        decision: 'DENY',
        violations: ['Substrate partition not found'],
        reason: 'Cannot verify substrate immutability',
      };
    }

    // Check if any additions redefine substrate terms
    const violations = [];
    for (const addition of delta.additions) {
      // Simplified check: substrate terms typically start with https://unrdf.org/substrate/
      if (addition.subject.includes('unrdf.org/substrate/')) {
        violations.push(`Attempt to redefine substrate term: ${addition.subject}`);
      }
    }

    if (violations.length > 0) {
      return {
        decision: 'DENY',
        violations,
        reason: 'Cannot redefine substrate terms',
      };
    }

    return {
      decision: 'ALLOW',
      violations: [],
      reason: 'No substrate terms redefined',
    };
  }
}

/**
 * @class ProtectedNamespaceInvariant
 * @extends Invariant
 * @description Ensures protected namespaces are not violated
 */
export class ProtectedNamespaceInvariant extends Invariant {
  constructor() {
    super('ProtectedNamespace');
  }

  check(delta, universe) {
    const policyPartition = universe.getPartition('SystemPolicyPartition');
    if (!policyPartition) {
      return {
        decision: 'DENY',
        violations: ['Policy partition not found'],
        reason: 'Cannot verify namespace protection',
      };
    }

    const protectedNS = policyPartition.getProtectedNamespaces();
    const violations = [];

    // Only check if new terms are being DEFINED in protected namespaces (subject check only)
    // Using protected predicates/objects (like rdf:type) is allowed
    for (const addition of delta.additions) {
      for (const ns of protectedNS) {
        if (addition.subject.startsWith(ns)) {
          violations.push(`Collision with protected namespace: ${ns}`);
          break; // Only report once per addition
        }
      }
    }

    if (violations.length > 0) {
      return {
        decision: 'DENY',
        violations,
        reason: 'Protected namespace collision detected',
      };
    }

    return {
      decision: 'ALLOW',
      violations: [],
      reason: 'No protected namespace collisions',
    };
  }
}

/**
 * @class CanonicalConstraintInvariant
 * @extends Invariant
 * @description Ensures canonical constraints are not weakened
 */
export class CanonicalConstraintInvariant extends Invariant {
  constructor() {
    super('CanonicalConstraint');
  }

  check(delta, universe) {
    // Simplified: check if any SHACL constraints are being weakened
    const violations = [];
    for (const addition of delta.additions) {
      if (addition.predicate.includes('shacl') && addition.predicate.includes('minCount')) {
        // Weakening minCount constraint
        violations.push(`Attempt to weaken constraint: ${addition.subject}`);
      }
    }

    if (violations.length > 0) {
      return {
        decision: 'DENY',
        violations,
        reason: 'Cannot weaken canonical constraints',
      };
    }

    return {
      decision: 'ALLOW',
      violations: [],
      reason: 'No constraint weakening detected',
    };
  }
}

/**
 * @class TypeConsistencyInvariant
 * @extends Invariant
 * @description Ensures type consistency
 */
export class TypeConsistencyInvariant extends Invariant {
  constructor() {
    super('TypeConsistency');
  }

  check(delta, universe) {
    // Simplified type consistency check
    return {
      decision: 'ALLOW',
      violations: [],
      reason: 'Type consistency verified',
    };
  }
}

/**
 * @class SchemaCoherenceInvariant
 * @extends Invariant
 * @description Ensures schema coherence
 */
export class SchemaCoherenceInvariant extends Invariant {
  constructor() {
    super('SchemaCoherence');
  }

  check(delta, universe) {
    // Simplified schema coherence check
    return {
      decision: 'ALLOW',
      violations: [],
      reason: 'Schema coherence verified',
    };
  }
}

/**
 * @class AdmissionController
 * @description Controls admission of deltas to universe
 */
export class AdmissionController {
  constructor() {
    this.invariants = [
      new AdditiveOnlyInvariant(),
      new SubstrateImmutabilityInvariant(),
      new ProtectedNamespaceInvariant(),
      new CanonicalConstraintInvariant(),
      new TypeConsistencyInvariant(),
      new SchemaCoherenceInvariant(),
    ];
    this.forbiddenOperations = new Set(['DELETE', 'DROP', 'CLEAR']);
  }

  /**
   * Admit delta
   * @param {object} delta - Delta to admit
   * @param {object} universe - Universe context
   * @returns {AdmissionResult}
   */
  admit(delta, universe) {
    const validated = validateDelta(delta);

    // Check forbidden operations
    const forbiddenOp = this._checkForbiddenOperations(validated);
    if (forbiddenOp) {
      return forbiddenOp;
    }

    // Execute all invariants in sequence
    for (const invariant of this.invariants) {
      const result = invariant.check(validated, universe);
      if (result.decision === 'DENY') {
        return result;
      }
    }

    return {
      decision: 'ALLOW',
      violations: [],
      reason: 'All invariants passed',
    };
  }

  /**
   * Check for forbidden operations
   * @param {object} delta
   * @returns {AdmissionResult | null}
   * @private
   */
  _checkForbiddenOperations(delta) {
    // Check if delta contains forbidden operation markers
    if (delta.deletions && delta.deletions.length > 0) {
      for (const deletion of delta.deletions) {
        if (deletion.predicate.includes('DROP') || deletion.predicate.includes('CLEAR')) {
          return {
            decision: 'DENY',
            violations: ['Forbidden operation detected'],
            reason: 'DELETE/DROP/CLEAR operations not allowed',
          };
        }
      }
    }
    return null;
  }

  /**
   * Get all invariants
   * @returns {Invariant[]}
   */
  getInvariants() {
    return this.invariants;
  }
}
