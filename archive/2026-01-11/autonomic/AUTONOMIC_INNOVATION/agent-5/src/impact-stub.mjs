/**
 * Stub implementation of Agent 4's impact set computation
 * This is a simplified version for Agent 5 testing until Agent 4 is fully implemented
 * @module agent-5/impact-stub
 */

/**
 * Compute impact set from capsule (simplified stub)
 * @param {Object} capsule - Capsule with add/del sets
 * @returns {Object} - Impact set with subjects, predicates, graphs
 */
export function computeImpactSet(capsule) {
  const subjects = new Set();
  const predicates = new Set();
  const graphs = new Set();

  // Process add quads
  for (const quad of capsule.add) {
    subjects.add(quad.subject.value);
    predicates.add(quad.predicate.value);
    graphs.add(quad.graph?.value || 'http://kgc.io/Universe');
  }

  // Process del quads
  for (const quad of capsule.del) {
    subjects.add(quad.subject.value);
    predicates.add(quad.predicate.value);
    graphs.add(quad.graph?.value || 'http://kgc.io/Universe');
  }

  return { subjects, predicates, graphs };
}

/**
 * Check if two impact sets are disjoint (no overlap)
 * Focus on subjects - if capsules touch different subjects, they're disjoint
 * Sharing predicates or graphs doesn't necessarily cause conflicts
 *
 * @param {Object} impactA - First impact set
 * @param {Object} impactB - Second impact set
 * @returns {boolean} - True if disjoint (no shared subjects)
 */
export function areDisjoint(impactA, impactB) {
  // Check subjects - primary indicator of potential conflicts
  for (const s of impactA.subjects) {
    if (impactB.subjects.has(s)) {
      return false;
    }
  }

  // If subjects are disjoint, capsules can safely reorder
  // Even if they share predicates or graphs
  return true;
}
