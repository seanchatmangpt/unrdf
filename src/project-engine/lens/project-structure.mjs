/**
 * @file Project structure lens - map low-level triple changes to semantic changes
 * @module project-engine/lens/project-structure
 */

/**
 * Ontology lens for project structure diff
 * Maps triple changes to project-level semantic changes
 *
 * Used with diffOntologyFromDelta to convert:
 * - Triple additions/removals → FeatureAdded/FeatureRemoved, RoleAdded/RoleRemoved
 *
 * @type {OntologyLensFn}
 */
export function ProjectStructureLens(triple, direction) {
  const { subject, predicate, object } = triple

  // Rule 1: Feature type assertions
  if (
    predicate === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' &&
    object === 'http://example.org/unrdf/project#Feature'
  ) {
    return {
      kind: direction === 'added' ? 'FeatureAdded' : 'FeatureRemoved',
      entity: subject,
      details: { resourceType: 'Feature' },
    }
  }

  // Rule 2: File role assignments
  if (predicate === 'http://example.org/unrdf/project#hasRole') {
    return {
      kind: direction === 'added' ? 'RoleAdded' : 'RoleRemoved',
      entity: subject,
      role: object,
      details: { roleType: extractRoleName(object) },
    }
  }

  // Rule 3: File feature membership
  if (predicate === 'http://example.org/unrdf/project#belongsToFeature') {
    return {
      kind: direction === 'added' ? 'FeatureMemberAdded' : 'FeatureMemberRemoved',
      entity: subject,
      role: 'belongsToFeature',
      details: { feature: object },
    }
  }

  // Rule 4: Module changes
  if (predicate === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' &&
      object === 'http://example.org/unrdf/project#Module') {
    return {
      kind: direction === 'added' ? 'ModuleAdded' : 'ModuleRemoved',
      entity: subject,
      details: { resourceType: 'Module' },
    }
  }

  // Ignore other triples
  return null
}

/**
 * Extract role name from IRI
 *
 * @private
 */
function extractRoleName(iri) {
  const match = iri.match(/#([A-Za-z]+)$/)
  return match ? match[1] : iri
}
