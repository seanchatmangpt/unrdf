/**
 * @unrdf/core - RDF Graph Operations and SPARQL Execution
 *
 * Core substrate for UNRDF v5. Provides:
 * - RDF store operations (create, add, query quads)
 * - SPARQL query execution (sync and async)
 * - RDF canonicalization
 * - Type definitions and constants
 *
 * @module @unrdf/core
 */

// ============================================================================
// === Synchronous APIs (NEW - Primary) ===
// ============================================================================

// Synchronous RDF Store (UnrdfStore class with direct method calls)
export { UnrdfStore, createStore as createUnrdfStore } from './rdf/unrdf-store.mjs';

// Synchronous SPARQL Executors (no async/await required)
export {
  executeQuerySync,
  executeSelectSync,
  executeAskSync,
  executeConstructSync,
  prepareQuerySync,
} from './sparql/executor-sync.mjs';

// ============================================================================
// === Async APIs (Existing - Backward Compatibility) ===
// ============================================================================

// Async RDF operations (functional style with async store)
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

// Async SPARQL operations (legacy functional API)
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

// ============================================================================
// === Error Handling & Debugging ===
// ============================================================================

// Error classes and utilities
export {
  UnrdfError,
  ValidationError,
  ConfigError,
  QueryError,
  StoreError,
  NetworkError,
  TimeoutError,
  ParserError,
  ERROR_CODES,
  createError,
  wrapError,
  assertError,
} from './errors.mjs';

// Debug utilities
export {
  DebugLogger,
  createDebugger,
  PerformanceTracker,
  perfTracker,
  trace,
  traceMethod,
  formatBytes,
  getSystemInfo,
  dumpDebugSnapshot,
} from './debug.mjs';

// Error recovery patterns
export {
  retry,
  CircuitBreaker,
  fallback,
  withTimeout,
  bulkOperation,
  RateLimiter,
  withRecovery,
} from './recovery.mjs';

// ============================================================================
// === RDF-star (W3C RDF 1.2) Support ===
// ============================================================================

// RDF-star factory and utilities
export {
  RDFStarFactory,
  factory as rdfStarFactory,
  RDFSTAR,
  isQuotedTriple,
  extractBaseTriple,
} from './rdf-star.mjs';

// Quoted triple class
export {
  QuotedTriple,
  createQuotedTriple,
} from './quoted-triple.mjs';

// Annotation helpers
export {
  AnnotationBuilder,
  createAnnotationBuilder,
  createProvenance,
  createTemporal,
  createConfidence,
  createMultiSource,
  mergeAnnotations,
  extractAnnotations,
} from './annotation.mjs';

// RDF-star schemas
export {
  QuotedTripleSchema,
  ProvenanceSchema,
  TemporalSchema,
  ConfidenceSchema,
  MultiSourceSchema,
  AnnotationSchema,
  AnnotatedTripleSchema,
  SPARQLStarOptionsSchema,
  validateQuotedTriple,
  validateProvenance,
  validateTemporal,
  validateConfidence,
  validateAnnotation,
  safeParseQuotedTriple,
  safeParseAnnotation,
} from './rdf-star.schema.mjs';
