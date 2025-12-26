/**
 * Impact Set Computation Engine
 *
 * Extracts what resources, properties, and graphs are affected by a capsule (diff).
 * Provides core algorithms for impact analysis, merging, and intersection.
 *
 * @module impact
 */

import { z } from 'zod';

/**
 * @typedef {Object} Quad
 * @property {Object} subject - RDF term (subject)
 * @property {Object} predicate - RDF term (predicate)
 * @property {Object} object - RDF term (object)
 * @property {Object} [graph] - RDF term (graph) - optional
 */

/**
 * @typedef {Object} Capsule
 * @property {Object} delta - The change set
 * @property {Array<Quad>} delta.add - Quads to add
 * @property {Array<Quad>} delta.del - Quads to delete
 * @property {string} [id] - Optional capsule identifier
 * @property {Object} [metadata] - Optional metadata
 */

/**
 * @typedef {Object} ImpactSet
 * @property {Set<string>} subjects - All touched subject IRIs
 * @property {Set<string>} predicates - All touched predicate IRIs
 * @property {Set<string>} graphs - All touched graph IRIs
 * @property {Object} cardinality - Quad counts
 * @property {number} cardinality.quadsAdded - Number of quads added
 * @property {number} cardinality.quadsDeleted - Number of quads deleted
 * @property {number} cardinality.netChange - Net change (added - deleted)
 * @property {Object} resourcesAffected - Resource-level metrics
 * @property {number} resourcesAffected.directAffected - Unique subjects touched
 * @property {number} [resourcesAffected.transitive] - (Future) Transitive closure count
 */

// Zod validation schema for capsule input
const CapsuleSchema = z.object({
  delta: z.object({
    add: z.array(z.any()),
    del: z.array(z.any()),
  }),
  id: z.string().optional(),
  metadata: z.any().optional(),
});

/**
 * Default graph IRI for quads without explicit graph
 */
const DEFAULT_GRAPH = 'http://kgc.io/Universe';

/**
 * Extract IRI value from RDF term
 *
 * Handles different term types (NamedNode, BlankNode, Literal)
 * and returns a string representation suitable for Set membership.
 *
 * @param {Object} term - RDF term (NamedNode, BlankNode, Literal)
 * @returns {string} Term value/IRI
 */
function extractTermValue(term) {
  if (!term) {
    return '';
  }

  if (term.termType === 'NamedNode') {
    return term.value;
  }

  if (term.termType === 'BlankNode') {
    return `_:${term.value}`;
  }

  if (term.termType === 'Literal') {
    return `"${term.value}"`;
  }

  if (term.termType === 'DefaultGraph') {
    return DEFAULT_GRAPH;
  }

  return term.value || String(term);
}

/**
 * Validate capsule structure before processing
 *
 * @param {Capsule} capsule - Capsule to validate
 * @throws {Error} If capsule is malformed
 */
function validateCapsule(capsule) {
  try {
    CapsuleSchema.parse(capsule);
  } catch (error) {
    throw new Error(`Invalid capsule: ${error.message}`);
  }
}

/**
 * Compute impact set from capsule
 *
 * Analyzes a capsule (diff) to determine:
 * - Which subjects are affected
 * - Which predicates are used
 * - Which graphs are modified
 * - Cardinality metrics (quads added/deleted)
 * - Resource-level metrics
 *
 * Algorithm:
 * 1. Initialize empty Sets for subjects, predicates, graphs
 * 2. Walk delta.add: extract and collect all IRIs
 * 3. Walk delta.del: extract and collect all IRIs
 * 4. Compute derived metrics
 *
 * Complexity: O(n) where n = |delta.add| + |delta.del|
 * Space: O(k) where k = unique IRIs
 *
 * @param {Capsule} capsule - The capsule to analyze
 * @returns {ImpactSet} Impact set with subjects, predicates, graphs, and metrics
 * @throws {Error} If capsule is malformed
 */
export function computeImpactSet(capsule) {
  validateCapsule(capsule);

  const subjects = new Set();
  const predicates = new Set();
  const graphs = new Set();

  let quadsAdded = 0;
  let quadsDeleted = 0;

  // Process additions
  for (const quad of capsule.delta.add) {
    subjects.add(extractTermValue(quad.subject));
    predicates.add(extractTermValue(quad.predicate));

    const graphValue = quad.graph
      ? extractTermValue(quad.graph)
      : DEFAULT_GRAPH;
    graphs.add(graphValue);

    quadsAdded++;
  }

  // Process deletions
  for (const quad of capsule.delta.del) {
    subjects.add(extractTermValue(quad.subject));
    predicates.add(extractTermValue(quad.predicate));

    const graphValue = quad.graph
      ? extractTermValue(quad.graph)
      : DEFAULT_GRAPH;
    graphs.add(graphValue);

    quadsDeleted++;
  }

  return {
    subjects,
    predicates,
    graphs,
    cardinality: {
      quadsAdded,
      quadsDeleted,
      netChange: quadsAdded - quadsDeleted,
    },
    resourcesAffected: {
      directAffected: subjects.size,
    },
  };
}

/**
 * Merge multiple impact sets
 *
 * Combines multiple impact sets into a single unified impact set.
 * Useful for batch operations or analyzing cumulative effects.
 *
 * Algorithm:
 * 1. Create union of all subjects, predicates, graphs
 * 2. Sum cardinality metrics
 * 3. Recompute resourcesAffected from merged subjects
 *
 * Complexity: O(m * k) where m = number of impact sets, k = avg size
 *
 * @param {Array<ImpactSet>} impacts - Array of impact sets to merge
 * @returns {ImpactSet} Merged impact set
 */
export function mergeImpactSets(impacts) {
  if (!impacts || impacts.length === 0) {
    return {
      subjects: new Set(),
      predicates: new Set(),
      graphs: new Set(),
      cardinality: {
        quadsAdded: 0,
        quadsDeleted: 0,
        netChange: 0,
      },
      resourcesAffected: {
        directAffected: 0,
      },
    };
  }

  const mergedSubjects = new Set();
  const mergedPredicates = new Set();
  const mergedGraphs = new Set();

  let totalAdded = 0;
  let totalDeleted = 0;

  for (const impact of impacts) {
    // Union all sets
    for (const subject of impact.subjects) {
      mergedSubjects.add(subject);
    }
    for (const predicate of impact.predicates) {
      mergedPredicates.add(predicate);
    }
    for (const graph of impact.graphs) {
      mergedGraphs.add(graph);
    }

    // Sum cardinality
    totalAdded += impact.cardinality.quadsAdded;
    totalDeleted += impact.cardinality.quadsDeleted;
  }

  return {
    subjects: mergedSubjects,
    predicates: mergedPredicates,
    graphs: mergedGraphs,
    cardinality: {
      quadsAdded: totalAdded,
      quadsDeleted: totalDeleted,
      netChange: totalAdded - totalDeleted,
    },
    resourcesAffected: {
      directAffected: mergedSubjects.size,
    },
  };
}

/**
 * Find intersection of two impact sets
 *
 * Computes the overlap between two impact sets.
 * Used for conflict detection - non-empty intersection indicates
 * potential conflicts between capsules.
 *
 * Algorithm:
 * 1. Compute intersection of subjects, predicates, graphs
 * 2. Return new ImpactSet with intersected Sets
 * 3. Cardinality is undefined (intersection != addition)
 *
 * Use case: Agent 5 (Commutativity) checks if two capsules
 *           have non-empty intersection → potential conflict
 *
 * Complexity: O(min(k1, k2)) where k1, k2 are set sizes
 *
 * @param {ImpactSet} a - First impact set
 * @param {ImpactSet} b - Second impact set
 * @returns {ImpactSet} Intersection impact set (cardinality is undefined)
 */
export function impactSetIntersection(a, b) {
  const intersectedSubjects = new Set();
  const intersectedPredicates = new Set();
  const intersectedGraphs = new Set();

  // Compute intersection of subjects
  for (const subject of a.subjects) {
    if (b.subjects.has(subject)) {
      intersectedSubjects.add(subject);
    }
  }

  // Compute intersection of predicates
  for (const predicate of a.predicates) {
    if (b.predicates.has(predicate)) {
      intersectedPredicates.add(predicate);
    }
  }

  // Compute intersection of graphs
  for (const graph of a.graphs) {
    if (b.graphs.has(graph)) {
      intersectedGraphs.add(graph);
    }
  }

  return {
    subjects: intersectedSubjects,
    predicates: intersectedPredicates,
    graphs: intersectedGraphs,
    cardinality: {
      quadsAdded: 0,
      quadsDeleted: 0,
      netChange: 0,
    },
    resourcesAffected: {
      directAffected: intersectedSubjects.size,
    },
  };
}
