/**
 * @file index.mjs
 * @description RDF Universe - Core partition management and ontology registry
 *
 * This module provides the foundational layer for managing RDF data across
 * multiple logical partitions with strict governance and allow-listing.
 *
 * @module @unrdf/universe
 */

// Export utility functions
export {
  validateIri,
  loadTurtle,
  parseQuads,
  canonicalizeQuads,
  serializeQuadsToNTriples,
  computeContentHash,
  hashString,
  extractNamespaces,
  extractNamespaceFromIri,
  mergeStores,
} from './rdf-utils.mjs';

// Export ontology classes
export { OntologyRelease, AllowedOntology } from './ontology-release.mjs';

// Export registry
export { OntologyRegistry } from './registry.mjs';

// Export partition classes
export {
  Partition,
  IndustrialSubstrate,
  CorporateCanon,
  BusinessUnitOverlay,
  RegionalOverlay,
  ExecutionLedger,
  SystemPolicyPartition,
} from './partition.mjs';

// Export universe
export { Universe } from './universe.mjs';
