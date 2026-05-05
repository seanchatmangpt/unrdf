/**
 * @file impact-set.mjs
 * @description Impact set computation for RDF capsule deltas.
 * Determines which subjects, predicates, and graphs are touched by a capsule.
 */

import crypto from 'node:crypto'

/**
 * @typedef {Object} ImpactSet
 * @property {Set<string>} subjects - Set of subject IRIs
 * @property {Set<string>} predicates - Set of predicate IRIs
 * @property {Set<string>} graphs - Set of graph IRIs
 * @property {Object} cardinality - Cardinality metrics
 * @property {number} cardinality.subjects - Number of unique subjects
 * @property {number} cardinality.predicates - Number of unique predicates
 * @property {number} cardinality.graphs - Number of unique graphs
 * @property {number} cardinality.totalQuads - Total number of quads
 * @property {string} summary - Human-readable summary
 */

/**
 * @typedef {Object} Capsule
 * @property {Array<Object>} delta - Array of quads
 */

/**
 * Compute the impact set of a capsule delta.
 * Determines which RDF resources (subjects, predicates, graphs) are touched.
 *
 * @param {Capsule} capsule - Capsule with delta field
 * @returns {ImpactSet} Impact set with subjects, predicates, graphs, and cardinality
 *
 * @example
 * const impactSet = computeImpactSet(capsule)
 * console.log(impactSet.cardinality.subjects) // 5
 */
export function computeImpactSet(capsule) {
  if (!capsule || !capsule.delta) {
    throw new Error('Capsule must have delta field')
  }

  const subjects = new Set()
  const predicates = new Set()
  const graphs = new Set()

  // Single pass through all quads
  for (const quad of capsule.delta) {
    if (!quad) continue

    // Extract subject IRI
    if (quad.subject) {
      const subjectIRI = quad.subject.value || quad.subject.id || quad.subject
      if (typeof subjectIRI === 'string') {
        subjects.add(subjectIRI)
      }
    }

    // Extract predicate IRI
    if (quad.predicate) {
      const predicateIRI = quad.predicate.value || quad.predicate.id || quad.predicate
      if (typeof predicateIRI === 'string') {
        predicates.add(predicateIRI)
      }
    }

    // Extract graph IRI (default graph if not specified)
    if (quad.graph) {
      const graphIRI = quad.graph.value || quad.graph.id || quad.graph
      if (typeof graphIRI === 'string') {
        graphs.add(graphIRI)
      }
    } else {
      graphs.add('urn:x-rdfjs:default')
    }
  }

  const cardinality = {
    subjects: subjects.size,
    predicates: predicates.size,
    graphs: graphs.size,
    totalQuads: capsule.delta.length
  }

  const impactSet = {
    subjects,
    predicates,
    graphs,
    cardinality,
    summary: summarizeImpactSet({ subjects, predicates, graphs, cardinality })
  }

  return impactSet
}

/**
 * Generate human-readable summary of impact set.
 *
 * @param {ImpactSet} impactSet - Impact set to summarize
 * @returns {string} Human-readable summary
 *
 * @example
 * summarizeImpactSet(impactSet)
 * // "Modified 5 subjects, 3 predicates, 2 graphs. Total 12 quads."
 */
export function summarizeImpactSet(impactSet) {
  const { cardinality } = impactSet
  return `Modified ${cardinality.subjects} subjects, ${cardinality.predicates} predicates, ${cardinality.graphs} graphs. Total ${cardinality.totalQuads} quads.`
}

/**
 * Compute deterministic hash of impact set.
 * Used for caching, conflict detection, and audit trails.
 *
 * @param {ImpactSet} impactSet - Impact set to hash
 * @returns {string} SHA-256 hash (hex)
 *
 * @example
 * const hash1 = hashImpactSet(impactSet)
 * const hash2 = hashImpactSet(impactSet)
 * assert(hash1 === hash2) // Deterministic
 */
export function hashImpactSet(impactSet) {
  // Sort all Sets to ensure determinism
  const sortedSubjects = Array.from(impactSet.subjects).sort()
  const sortedPredicates = Array.from(impactSet.predicates).sort()
  const sortedGraphs = Array.from(impactSet.graphs).sort()

  // Canonical representation
  const canonical = JSON.stringify({
    subjects: sortedSubjects,
    predicates: sortedPredicates,
    graphs: sortedGraphs,
    cardinality: impactSet.cardinality
  })

  // SHA-256 hash
  return crypto.createHash('sha256').update(canonical).digest('hex')
}
