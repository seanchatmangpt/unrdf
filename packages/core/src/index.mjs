/**
 * @unrdf/core - RDF Graph Operations and SPARQL Execution
 *
 * Core substrate for UNRDF v5. Provides:
 * - RDF store operations (create, add, query quads)
 * - SPARQL query execution
 * - RDF canonicalization
 * - Type definitions and constants
 *
 * @module @unrdf/core
 */

// Export RDF operations
export {
  createStore,
  addQuad,
  removeQuad,
  getQuads,
  iterateQuads,
  countQuads,
  namedNode,
  literal,
  blankNode,
  variable,
  defaultGraph,
  quad,
} from './rdf/store.mjs';

export { canonicalize, toNTriples, sortQuads, isIsomorphic } from './rdf/canonicalize.mjs';

// Export SPARQL operations
export {
  executeQuery,
  prepareQuery,
  executeSelect,
  executeConstruct,
  executeAsk,
} from './sparql/executor.mjs';

// Export types and utilities
export {
  createTerms,
  createNamedNode,
  createLiteral,
  createBlankNode,
  createVariable,
  createQuad,
} from './types.mjs';

// Export constants
export { RDF, RDFS, OWL, XSD, FOAF, DCTERMS, SKOS, COMMON_PREFIXES } from './constants.mjs';

// Export validation utilities
export {
  QuadSchema,
  StoreSchema,
  QueryOptionsSchema,
  validateQuad,
  validateStore,
} from './validation/index.mjs';
