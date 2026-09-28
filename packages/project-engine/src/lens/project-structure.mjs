/**
 * @file Project structure lens - map low-level triple changes to semantic changes
 * @module project-engine/lens/project-structure
 */

import { RDF_TYPE, PROJECT } from '../namespaces.mjs';

/**
 * Ontology lens for project structure diff.
 * Maps triple additions/removals to FeatureAdded/FeatureRemoved, RoleAdded/RoleRemoved, ...
 *
 * @param {{subject: string, predicate: string, object: string}} triple
 * @param {'added'|'removed'} direction
 * @returns {Object|null} Semantic change, or null if the triple is not project-relevant
 */
export function ProjectStructureLens(triple, direction) {
  const { subject, predicate, object } = triple;
  const added = direction === 'added';

  if (predicate === RDF_TYPE && object === PROJECT.Feature) {
    return {
      kind: added ? 'FeatureAdded' : 'FeatureRemoved',
      entity: subject,
      details: { resourceType: 'Feature' },
    };
  }

  if (predicate === PROJECT.hasRole) {
    return {
      kind: added ? 'RoleAdded' : 'RoleRemoved',
      entity: subject,
      role: object,
      details: { roleType: extractRoleName(object) },
    };
  }

  if (predicate === PROJECT.belongsToFeature) {
    return {
      kind: added ? 'FeatureMemberAdded' : 'FeatureMemberRemoved',
      entity: subject,
      role: 'belongsToFeature',
      details: { feature: object },
    };
  }

  if (predicate === RDF_TYPE && object === PROJECT.Module) {
    return {
      kind: added ? 'ModuleAdded' : 'ModuleRemoved',
      entity: subject,
      details: { resourceType: 'Module' },
    };
  }

  return null;
}

function extractRoleName(iri) {
  const match = iri.match(/#([A-Za-z]+)$/);
  return match ? match[1] : iri;
}
