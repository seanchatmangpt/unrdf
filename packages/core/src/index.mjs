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

export { UnrdfStore, createStore as createUnrdfStore } from './rdf/unrdf-store.mjs';
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
export { cloneQuad } from './utils/quad-utils.mjs';
export { diffGraphFromStores, diffGraphFromDelta, diffOntologyFromGraphDiff } from './diff.mjs';
export {
  executeQuery,
  prepareQuery,
  executeSelect,
  executeConstruct,
  executeAsk,
} from './sparql/executor.mjs';
export {
  createTerms,
  createNamedNode,
  createLiteral,
  createBlankNode,
  createVariable,
  createQuad,
} from './types.mjs';
export { RDF, RDFS, OWL, XSD, FOAF, DCTERMS, SKOS, COMMON_PREFIXES } from './constants.mjs';
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
// === Visualization & Debugging ===
// ============================================================================

export { toDOT, toMermaid, toASCII, toHTML, extractSubgraph } from './viz/graph-visualizer.mjs';
export {
  explainQuery,
  formatPlanAsTree,
  trackQueryStats,
  compareQueryPerformance,
} from './viz/query-explainer.mjs';
export {
  getGraphStatistics,
  analyzeNamespaces,
  detectOrphans,
  assessDataQuality,
  checkSchemaConformance,
  generateInspectionReport,
} from './debug/rdf-inspector.mjs';

// ============================================================================
// === RDF-star (W3C RDF 1.2) Support ===
// ============================================================================

export {
  RDFStarFactory,
  factory as rdfStarFactory,
  RDFSTAR,
  isQuotedTriple,
  extractBaseTriple,
} from './rdf-star.mjs';
export { QuotedTriple, createQuotedTriple } from './quoted-triple.mjs';
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

// Capability standing, receipts, replay, and deterministic execution plans
export { Standing, Disposition, CapabilityLedger, createCapabilityLedger } from './capability-ledger.mjs';
export { ReceiptChain, createReceiptChain, canonicalizeJSON, hashCanonical, compareReplay } from './receipt-chain.mjs';
export { ExecutionPlan, createExecutionPlan } from './execution-plan.mjs';
