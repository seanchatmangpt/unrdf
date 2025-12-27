/**
 * @file cardinality.mjs
 * @description Cardinality analysis and structural metrics for RDF capsule deltas.
 */

/**
 * @typedef {Object} Capsule
 * @property {Array<Object>} delta - Array of quads
 */

/**
 * Group quads by predicate and count occurrences.
 * Useful for understanding "what kind of changes" (e.g., 80% rdf:type assertions).
 *
 * @param {Capsule} capsule - Capsule with delta field
 * @returns {Map<string, number>} Map of predicate IRI to quad count
 *
 * @example
 * const grouped = groupByPredicate(capsule)
 * grouped.get('http://www.w3.org/1999/02/22-rdf-syntax-ns#type') // 42
 */
export function groupByPredicate(capsule) {
  if (!capsule || !capsule.delta) {
    throw new Error('Capsule must have delta field')
  }

  const predicateMap = new Map()

  for (const quad of capsule.delta) {
    if (!quad || !quad.predicate) continue

    const predicateIRI = quad.predicate.value || quad.predicate.id || quad.predicate
    if (typeof predicateIRI !== 'string') continue

    predicateMap.set(predicateIRI, (predicateMap.get(predicateIRI) || 0) + 1)
  }

  return predicateMap
}

/**
 * Group quads by subject and count occurrences.
 * Identifies "hotspots" — resources with many changes.
 *
 * @param {Capsule} capsule - Capsule with delta field
 * @returns {Map<string, number>} Map of subject IRI to quad count
 *
 * @example
 * const grouped = groupBySubject(capsule)
 * grouped.get('http://example.org/resource1') // 8
 */
export function groupBySubject(capsule) {
  if (!capsule || !capsule.delta) {
    throw new Error('Capsule must have delta field')
  }

  const subjectMap = new Map()

  for (const quad of capsule.delta) {
    if (!quad || !quad.subject) continue

    const subjectIRI = quad.subject.value || quad.subject.id || quad.subject
    if (typeof subjectIRI !== 'string') continue

    subjectMap.set(subjectIRI, (subjectMap.get(subjectIRI) || 0) + 1)
  }

  return subjectMap
}

/**
 * Analyze structural properties of capsule delta.
 *
 * @param {Capsule} capsule - Capsule with delta field
 * @returns {Object} Structural analysis
 * @returns {number} return.density - Average quads per subject
 * @returns {number} return.spanCount - Number of distinct graphs touched
 * @returns {number} return.orphanedSubjects - Subjects only appearing as objects
 *
 * @example
 * const analysis = analyzeGraphStructure(capsule)
 * console.log(`Density: ${analysis.density.toFixed(2)} quads/subject`)
 */
export function analyzeGraphStructure(capsule) {
  if (!capsule || !capsule.delta) {
    throw new Error('Capsule must have delta field')
  }

  const subjects = new Set()
  const graphs = new Set()
  const objectSubjects = new Set() // Subjects that appear as objects

  for (const quad of capsule.delta) {
    if (!quad) continue

    // Track subjects
    if (quad.subject) {
      const subjectIRI = quad.subject.value || quad.subject.id || quad.subject
      if (typeof subjectIRI === 'string') {
        subjects.add(subjectIRI)
      }
    }

    // Track objects that are IRIs (potential subjects)
    if (quad.object) {
      const objectValue = quad.object.value || quad.object.id || quad.object
      const objectType = quad.object.termType || quad.object.type
      if (objectType === 'NamedNode' || objectType === 'namedNode' || objectType === 'BlankNode') {
        if (typeof objectValue === 'string') {
          objectSubjects.add(objectValue)
        }
      }
    }

    // Track graphs
    if (quad.graph) {
      const graphIRI = quad.graph.value || quad.graph.id || quad.graph
      if (typeof graphIRI === 'string') {
        graphs.add(graphIRI)
      }
    } else {
      graphs.add('urn:x-rdfjs:default')
    }
  }

  // Compute density
  const totalQuads = capsule.delta.length
  const uniqueSubjects = subjects.size
  const density = uniqueSubjects > 0 ? totalQuads / uniqueSubjects : 0

  // Orphaned subjects: appear as objects but NOT as subjects
  const orphanedSubjects = Array.from(objectSubjects).filter(iri => !subjects.has(iri)).length

  return {
    density,
    spanCount: graphs.size,
    orphanedSubjects
  }
}
