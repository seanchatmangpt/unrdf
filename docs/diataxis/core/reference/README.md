# Reference — @unrdf/core

Reference material is for looking things up. It is accurate and complete, not tutorial-style.

## Index

| Document                         | Covers                                                                                                                                |
| -------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------- |
| [store-api.md](./store-api.md)   | `UnrdfStore`, `createUnrdfStore`, functional store API (`createStore`, `addQuad`, `getQuads`, …)                                      |
| [sparql-api.md](./sparql-api.md) | `executeQuerySync`, `executeSelectSync`, `executeAskSync`, `executeConstructSync`, `prepareQuerySync`, async wrappers, `QueryBuilder` |
| [shacl-api.md](./shacl-api.md)   | `shacl`, `ShapeBuilder`, `PropertyBuilder`, `createValidator`, `validateGraph`, `ConstraintType`                                      |

## All public exports at a glance

```javascript
import {
  // === UnrdfStore (class-based, recommended) ===
  UnrdfStore,
  createUnrdfStore, // alias: createStore from rdf/unrdf-store.mjs

  // === Synchronous SPARQL executors ===
  executeQuerySync,
  executeSelectSync,
  executeAskSync,
  executeConstructSync,
  prepareQuerySync,

  // === Async functional API (backward compat) ===
  createStore, // functional Oxigraph store factory
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

  // === Serialization / Canonicalization ===
  canonicalize,
  toNTriples,
  sortQuads,
  isIsomorphic,

  // === Graph diff ===
  diffGraphFromStores,
  diffGraphFromDelta,
  diffOntologyFromGraphDiff,

  // === Async SPARQL (legacy, wraps sync) ===
  executeQuery,
  prepareQuery,
  executeSelect,
  executeConstruct,
  executeAsk,

  // === Term factories ===
  createTerms,
  createNamedNode,
  createLiteral,
  createBlankNode,
  createVariable,
  createQuad,

  // === Namespace constants ===
  RDF,
  RDFS,
  OWL,
  XSD,
  FOAF,
  DCTERMS,
  SKOS,
  COMMON_PREFIXES,

  // === Zod schemas + validators ===
  QuadSchema,
  StoreSchema,
  QueryOptionsSchema,
  validateQuad,
  validateStore,

  // === SHACL ===
  shacl,
  ShapeBuilder,
  PropertyBuilder,
  CommonShapes,
  createValidator,
  validateGraph,
  validateConstraint,
  generateReport,
  validateNodeKind,
  fastValidate,
  ConstraintType,

  // === Error classes ===
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

  // === Recovery ===
  retry,
  CircuitBreaker,
  fallback,
  withTimeout,
  bulkOperation,
  RateLimiter,
  withRecovery,

  // === Visualization ===
  toDOT,
  toMermaid,
  toASCII,
  toHTML,
  extractSubgraph,
  explainQuery,
  formatPlanAsTree,
  trackQueryStats,
  compareQueryPerformance,
  getGraphStatistics,
  analyzeNamespaces,
  detectOrphans,
  assessDataQuality,
  checkSchemaConformance,
  generateInspectionReport,

  // === RDF-star ===
  RDFStarFactory,
  rdfStarFactory,
  RDFSTAR,
  isQuotedTriple,
  extractBaseTriple,
  QuotedTriple,
  createQuotedTriple,

  // === Annotations ===
  AnnotationBuilder,
  createAnnotationBuilder,
  createProvenance,
  createTemporal,
  createConfidence,
  createMultiSource,
  mergeAnnotations,
  extractAnnotations,
} from '@unrdf/core';
```
