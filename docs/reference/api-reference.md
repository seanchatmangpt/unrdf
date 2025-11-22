# UNRDF API Reference

> **Version**: 4.0.0
> **Last Updated**: 2025-11-21
> **Stability**: Production-Ready

---

## Table of Contents

1. [Overview](#overview)
2. [Core Exports](#core-exports)
3. [Knowledge Engine](#knowledge-engine)
4. [Composables](#composables)
5. [React Hooks](#react-hooks)
6. [CLI](#cli)
7. [Utilities](#utilities)
8. [Schemas](#schemas)
9. [Error Handling](#error-handling)
10. [Performance Characteristics](#performance-characteristics)

---

## Overview

UNRDF is a production-ready RDF knowledge graph library featuring:

- **Knowledge Hooks**: Content-addressed, deterministic hook execution
- **SPARC Methodology**: Specification, Pseudocode, Architecture, Refinement, Completion
- **Knowledge Substrate Optimization**: 80/20 query optimization framework
- **AI/Semantic Integration**: Embedding and semantic analysis capabilities
- **Distributed Federation**: Multi-source RDF federation
- **Real-time Streaming**: Change feed and subscription support
- **HTF Framework**: Homotopy Type Framework for formal verification

### Design Philosophy

> **No TypeScript. Ever.** TypeScript is an illusion of safety that collapses at runtime. UNRDF guarantees correctness at the only level that matters: execution.
>
> - **JSDoc is the source of truth**
> - **Zod is the contract** for runtime validation
> - **Composables everywhere** for consistent interfaces

---

## Core Exports

### Entry Point: `unrdf`

```javascript
import {
  // Store context
  initStore,
  useStoreContext,

  // Core composables
  useGraph,
  useTurtle,
  useTerms,

  // Validation and reasoning
  useReasoner,
  useCanon,

  // Type safety
  useZod,

  // Change tracking
  useDelta,

  // Engines
  RdfEngine,

  // Knowledge Engine (all exports)
  KnowledgeHookManager,
  TransactionManager,
  defineHook,
  // ... and more
} from 'unrdf';
```

---

## Knowledge Engine

### Module: `unrdf/knowledge-engine`

The Knowledge Engine provides comprehensive RDF processing capabilities including parsing, querying, validation, reasoning, and transaction management.

---

### `parseTurtle(ttl, baseIRI)`

Parse a Turtle string into an N3 Store.

**Type Signature**:
```javascript
/**
 * @param {string} ttl - The Turtle string to parse
 * @param {string} [baseIRI='http://example.org/'] - Base IRI for resolving relative URIs
 * @returns {Promise<Store>} Promise resolving to N3 Store
 * @throws {TypeError} If ttl is not a string
 * @throws {TypeError} If baseIRI is not a string
 * @throws {Error} If parsing fails
 */
async function parseTurtle(ttl, baseIRI)
```

**Parameters**:

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `ttl` | `string` | Yes | - | Turtle-formatted RDF data |
| `baseIRI` | `string` | No | `'http://example.org/'` | Base IRI for relative URI resolution |

**Returns**: `Promise<Store>` - N3.js Store containing parsed quads

**Example**:
```javascript
import { parseTurtle } from 'unrdf/knowledge-engine';

const ttl = `
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .

  ex:alice a foaf:Person ;
    foaf:name "Alice" ;
    foaf:knows ex:bob .
`;

const store = await parseTurtle(ttl, 'http://example.org/');
console.log(`Parsed ${store.size} quads`);
```

**Performance**: ~1-5ms for small documents (<1000 quads), scales linearly

**FMEA - Common Misuse**:
- Passing non-UTF8 encoded strings causes parsing errors
- Missing prefix declarations result in relative URI issues
- Large files (>10MB) should be streamed instead

**Related**: [`toTurtle`](#toturtle), [`parseJsonLd`](#parsejsonld)

**Version**: 1.0.0 | **Stability**: Stable

---

### `toTurtle(store, options)`

Serialize an N3 Store to Turtle format.

**Type Signature**:
```javascript
/**
 * @param {Store} store - The store to serialize
 * @param {Object} [options] - Serialization options
 * @param {Object} [options.prefixes] - Prefix mappings
 * @param {string} [options.baseIRI] - Base IRI for output
 * @returns {Promise<string>} Promise resolving to Turtle string
 * @throws {TypeError} If store is not a valid Store instance
 * @throws {Error} If serialization fails
 */
async function toTurtle(store, options)
```

**Parameters**:

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `store` | `Store` | Yes | - | N3 Store to serialize |
| `options.prefixes` | `Object` | No | `{}` | Prefix to IRI mappings |
| `options.baseIRI` | `string` | No | - | Base IRI for @base directive |

**Returns**: `Promise<string>` - Turtle-formatted string

**Example**:
```javascript
import { toTurtle } from 'unrdf/knowledge-engine';

const turtle = await toTurtle(store, {
  prefixes: {
    ex: 'http://example.org/',
    foaf: 'http://xmlns.com/foaf/0.1/'
  },
  baseIRI: 'http://example.org/'
});

console.log(turtle);
// @base <http://example.org/> .
// @prefix ex: <http://example.org/> .
// @prefix foaf: <http://xmlns.com/foaf/0.1/> .
// ex:alice a foaf:Person ; foaf:name "Alice" .
```

**Performance**: ~2-10ms for typical stores

**Version**: 1.0.0 | **Stability**: Stable

---

### `toNQuads(store, options)`

Serialize an N3 Store to canonical N-Quads format.

**Type Signature**:
```javascript
/**
 * @param {Store} store - The store to serialize
 * @param {Object} [options] - Serialization options
 * @returns {Promise<string>} Promise resolving to N-Quads string
 * @throws {TypeError} If store is not a valid Store instance
 * @throws {Error} If serialization fails
 */
async function toNQuads(store, options)
```

**Returns**: `Promise<string>` - N-Quads formatted string (one quad per line)

**Example**:
```javascript
import { toNQuads } from 'unrdf/knowledge-engine';

const nquads = await toNQuads(store);
// <http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" .
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `parseJsonLd(jsonld, options)`

Parse JSON-LD document into an N3 Store.

**Type Signature**:
```javascript
/**
 * @param {string|Object} jsonld - JSON-LD string or object
 * @param {Object} [options] - Parsing options
 * @param {string} [options.baseIRI] - Base IRI for resolution
 * @returns {Promise<Store>} Promise resolving to N3 Store
 * @throws {TypeError} If jsonld is not string or object
 * @throws {Error} If parsing fails or invalid JSON-LD structure
 */
async function parseJsonLd(jsonld, options)
```

**Example**:
```javascript
import { parseJsonLd } from 'unrdf/knowledge-engine';

const jsonld = {
  "@context": {
    "ex": "http://example.org/",
    "foaf": "http://xmlns.com/foaf/0.1/"
  },
  "@id": "ex:alice",
  "@type": "foaf:Person",
  "foaf:name": "Alice"
};

const store = await parseJsonLd(jsonld);
```

**FMEA - Edge Cases**:
- Empty `@graph` arrays return empty stores
- Missing `@id` on items skips those items
- Nested objects require proper `@id` references

**Version**: 1.0.0 | **Stability**: Stable

---

### `toJsonLd(store, options)`

Serialize an N3 Store to JSON-LD format.

**Type Signature**:
```javascript
/**
 * @param {Store} store - The store to serialize
 * @param {Object} [options] - Serialization options
 * @param {Object} [options.context] - JSON-LD context
 * @param {string} [options.baseIRI] - Base IRI
 * @returns {Promise<Object>} Promise resolving to JSON-LD object
 * @throws {TypeError} If store is not valid
 * @throws {Error} If serialization fails
 */
async function toJsonLd(store, options)
```

**Returns**: `Promise<Object>` - JSON-LD document with `@context` and `@graph`

**Example**:
```javascript
import { toJsonLd } from 'unrdf/knowledge-engine';

const jsonld = await toJsonLd(store, {
  context: {
    ex: 'http://example.org/',
    foaf: 'http://xmlns.com/foaf/0.1/'
  }
});

console.log(JSON.stringify(jsonld, null, 2));
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `query(store, sparql, options)`

Execute any SPARQL 1.1 query against a store.

**Type Signature**:
```javascript
/**
 * @param {Store} store - The store to query against
 * @param {string} sparql - SPARQL query string
 * @param {Object} [options] - Query options
 * @param {number} [options.limit] - Maximum number of results
 * @param {AbortSignal} [options.signal] - Abort signal for cancellation
 * @param {boolean} [options.deterministic] - Enable deterministic results
 * @returns {Promise<Array|boolean|Store>} Query results based on query type
 * @throws {TypeError} If store or sparql invalid
 * @throws {Error} If query execution fails
 */
async function query(store, sparql, options)
```

**Parameters**:

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `store` | `Store` | Yes | - | N3 Store to query |
| `sparql` | `string` | Yes | - | SPARQL 1.1 query |
| `options.limit` | `number` | No | - | Result limit |
| `options.signal` | `AbortSignal` | No | - | Cancellation signal |
| `options.deterministic` | `boolean` | No | `false` | Deterministic ordering |

**Returns**:
- SELECT queries: `Array<Object>` - Array of binding objects
- ASK queries: `boolean` - Boolean result
- CONSTRUCT/DESCRIBE: `Store` - New store with results

**Example**:
```javascript
import { query } from 'unrdf/knowledge-engine';

// SELECT query
const results = await query(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name ?friend
  WHERE {
    ?person foaf:name ?name ;
            foaf:knows ?friend .
  }
  LIMIT 10
`);

console.log(results);
// [{ name: 'Alice', friend: 'http://example.org/bob' }]

// ASK query
const exists = await query(store, `
  ASK { ?s ?p ?o }
`);
// true

// CONSTRUCT query
const subgraph = await query(store, `
  CONSTRUCT {
    ?person a <http://example.org/KnownPerson> .
  }
  WHERE {
    ?person <http://xmlns.com/foaf/0.1/knows> ?other .
  }
`);
```

**Performance**:
- Uses cached singleton QueryEngine (0ms overhead vs 100-500ms for new instance)
- Simple queries: <10ms
- Complex queries with JOINs: 10-100ms
- Federation queries: 100ms-10s depending on sources

**FMEA - Performance Traps**:
- Unbounded queries without LIMIT can exhaust memory
- OPTIONAL clauses with cross-products cause exponential blowup
- Federation queries without source limits cause timeouts

**OpenTelemetry**: All queries emit spans with attributes:
- `query.type`: SELECT, ASK, CONSTRUCT, DESCRIBE
- `query.length`: Query string length
- `query.store_size`: Number of quads in store
- `query.result_count`: Number of results

**Version**: 1.0.0 | **Stability**: Stable

---

### `select(store, sparql, options)`

Execute a SPARQL SELECT query.

**Type Signature**:
```javascript
/**
 * @param {Store} store - The store to query
 * @param {string} sparql - SPARQL SELECT query
 * @param {Object} [options] - Query options
 * @returns {Promise<Array<Object>>} Array of binding objects
 * @throws {Error} If not a SELECT query or execution fails
 */
async function select(store, sparql, options)
```

**Example**:
```javascript
import { select } from 'unrdf/knowledge-engine';

const bindings = await select(store, `
  SELECT ?name ?age WHERE {
    ?person <http://example.org/name> ?name ;
            <http://example.org/age> ?age .
  }
`);

bindings.forEach(b => console.log(`${b.name}: ${b.age}`));
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `ask(store, sparql, options)`

Execute a SPARQL ASK query.

**Type Signature**:
```javascript
/**
 * @param {Store} store - The store to query
 * @param {string} sparql - SPARQL ASK query
 * @param {Object} [options] - Query options
 * @returns {Promise<boolean>} Boolean result
 * @throws {Error} If not an ASK query or execution fails
 */
async function ask(store, sparql, options)
```

**Example**:
```javascript
import { ask } from 'unrdf/knowledge-engine';

const hasData = await ask(store, `
  ASK WHERE {
    ?person a <http://xmlns.com/foaf/0.1/Person> .
  }
`);

if (hasData) {
  console.log('Store contains Person data');
}
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `construct(store, sparql, options)`

Execute a SPARQL CONSTRUCT query.

**Type Signature**:
```javascript
/**
 * @param {Store} store - The store to query
 * @param {string} sparql - SPARQL CONSTRUCT query
 * @param {Object} [options] - Query options
 * @returns {Promise<Store>} New store with constructed quads
 * @throws {Error} If not a CONSTRUCT query or execution fails
 */
async function construct(store, sparql, options)
```

**Example**:
```javascript
import { construct } from 'unrdf/knowledge-engine';

const derived = await construct(store, `
  CONSTRUCT {
    ?person <http://example.org/type> <http://example.org/Adult> .
  }
  WHERE {
    ?person <http://example.org/age> ?age .
    FILTER (?age >= 18)
  }
`);
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `describe(store, sparql, options)`

Execute a SPARQL DESCRIBE query.

**Type Signature**:
```javascript
/**
 * @param {Store} store - The store to query
 * @param {string} sparql - SPARQL DESCRIBE query
 * @param {Object} [options] - Query options
 * @returns {Promise<Store>} Store with described resource quads
 * @throws {Error} If not a DESCRIBE query or execution fails
 */
async function describe(store, sparql, options)
```

**Example**:
```javascript
import { describe } from 'unrdf/knowledge-engine';

const resourceData = await describe(store, `
  DESCRIBE <http://example.org/alice>
`);
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `update(store, sparql, options)`

Execute a SPARQL UPDATE operation (INSERT, DELETE, etc.).

**Type Signature**:
```javascript
/**
 * @param {Store} store - The store to update
 * @param {string} sparql - SPARQL UPDATE query
 * @param {Object} [options] - Update options
 * @returns {Promise<Store>} The updated store
 * @throws {TypeError} If store or sparql invalid
 * @throws {Error} If update execution fails
 */
async function update(store, sparql, options)
```

**Example**:
```javascript
import { update } from 'unrdf/knowledge-engine';

const updated = await update(store, `
  INSERT DATA {
    <http://example.org/alice> <http://example.org/age> "30" .
  }
`);

// Or DELETE
await update(store, `
  DELETE WHERE {
    <http://example.org/alice> <http://example.org/temp> ?o .
  }
`);
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `getQueryStats(store, sparql, options)`

Get execution statistics for a SPARQL query.

**Type Signature**:
```javascript
/**
 * @param {Store} store - The store to query
 * @param {string} sparql - SPARQL query
 * @param {Object} [options] - Query options
 * @returns {Promise<Object>} Execution statistics
 */
async function getQueryStats(store, sparql, options)
```

**Returns**:
```javascript
{
  duration: number,    // Execution time in milliseconds
  resultCount: number, // Number of results
  success: boolean,    // Whether query succeeded
  error?: string       // Error message if failed
}
```

**Example**:
```javascript
import { getQueryStats } from 'unrdf/knowledge-engine';

const stats = await getQueryStats(store, `SELECT * WHERE { ?s ?p ?o }`);
console.log(`Query took ${stats.duration}ms, returned ${stats.resultCount} results`);
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `validateShacl(store, shapes, options)`

Validate a store against SHACL shapes.

**Type Signature**:
```javascript
/**
 * @param {Store} store - Store containing data to validate
 * @param {Store|string} shapes - SHACL shapes as Store or Turtle string
 * @param {Object} [options] - Validation options
 * @param {boolean} [options.strict] - Enable strict validation
 * @param {boolean} [options.includeDetails] - Include detailed results
 * @returns {{conforms: boolean, results: Array<Object>}} Validation report
 * @throws {TypeError} If store or shapes invalid
 * @throws {Error} If validation fails
 */
function validateShacl(store, shapes, options)
```

**Parameters**:

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `store` | `Store` | Yes | - | Data store to validate |
| `shapes` | `Store\|string` | Yes | - | SHACL shapes |
| `options.strict` | `boolean` | No | `false` | Strict validation mode |
| `options.includeDetails` | `boolean` | No | `false` | Include detailed results |

**Returns**:
```javascript
{
  conforms: boolean,           // Overall conformance
  results: [{
    message: string | null,    // Validation message
    path: string | null,       // Property path
    focusNode: string | null,  // Focus node IRI
    severity: string | null,   // sh:Violation, sh:Warning, sh:Info
    sourceConstraint: string | null,
    sourceConstraintComponent: string | null,
    sourceShape: string | null,
    value: string | null
  }]
}
```

**Example**:
```javascript
import { validateShacl } from 'unrdf/knowledge-engine';

const shapes = `
  @prefix sh: <http://www.w3.org/ns/shacl#> .
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .

  ex:PersonShape a sh:NodeShape ;
    sh:targetClass foaf:Person ;
    sh:property [
      sh:path foaf:name ;
      sh:minCount 1 ;
      sh:maxCount 1 ;
      sh:datatype xsd:string
    ] ;
    sh:property [
      sh:path foaf:age ;
      sh:datatype xsd:integer ;
      sh:minInclusive 0 ;
      sh:maxInclusive 150
    ] .
`;

const report = validateShacl(dataStore, shapes);

if (!report.conforms) {
  report.results.forEach(r => {
    console.error(`Validation error: ${r.message} at ${r.focusNode}`);
  });
}
```

**OpenTelemetry**: Emits spans with:
- `validate.shapes_type`: 'turtle' or 'store'
- `validate.data_size`: Number of quads
- `validate.conforms`: Boolean result
- `validate.error_count`, `validate.warning_count`

**Version**: 1.0.0 | **Stability**: Stable

---

### `validateShaclMultiple(store, shapesList, options)`

Validate against multiple SHACL shape sets.

**Type Signature**:
```javascript
/**
 * @param {Store} store - Store to validate
 * @param {Array<Store|string>} shapesList - Array of shape sets
 * @param {Object} [options] - Validation options
 * @returns {{conforms: boolean, results: Array, shapeResults: Array}}
 * @throws {TypeError} If shapesList is empty or invalid
 */
function validateShaclMultiple(store, shapesList, options)
```

**Example**:
```javascript
import { validateShaclMultiple } from 'unrdf/knowledge-engine';

const report = validateShaclMultiple(store, [
  personShapes,
  organizationShapes,
  contactShapes
]);

console.log(`Overall: ${report.conforms}`);
console.log(`Total results: ${report.totalResults}`);
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `formatValidationReport(validationResult, options)`

Format a validation report for display or analysis.

**Type Signature**:
```javascript
/**
 * @param {Object} validationResult - Result from validateShacl
 * @param {Object} [options] - Formatting options
 * @param {boolean} [options.includeSummary] - Include summary statistics
 * @param {boolean} [options.groupBySeverity] - Group results by severity
 * @returns {Object} Formatted validation report
 */
function formatValidationReport(validationResult, options)
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `hasValidationErrors(validationResult)`

Check if validation result contains errors (sh:Violation).

**Type Signature**:
```javascript
/**
 * @param {Object} validationResult - Validation result
 * @returns {boolean} True if any violations exist
 */
function hasValidationErrors(validationResult)
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `getValidationErrors(validationResult)`

Extract only error-level results.

**Type Signature**:
```javascript
/**
 * @param {Object} validationResult - Validation result
 * @returns {Array<Object>} Array of violation results
 */
function getValidationErrors(validationResult)
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `getValidationWarnings(validationResult)`

Extract warning-level results.

**Type Signature**:
```javascript
/**
 * @param {Object} validationResult - Validation result
 * @returns {Array<Object>} Array of warning results
 */
function getValidationWarnings(validationResult)
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `reason(store, rules, options)`

Apply N3 reasoning rules to a store.

**Type Signature**:
```javascript
/**
 * @param {Store} store - Store to reason over
 * @param {string} rules - N3 rules in Notation3 format
 * @param {Object} [options] - Reasoning options
 * @returns {Promise<Store>} Store with inferred triples
 * @throws {Error} If reasoning fails
 */
async function reason(store, rules, options)
```

**Example**:
```javascript
import { reason } from 'unrdf/knowledge-engine';

const rules = `
  @prefix ex: <http://example.org/> .
  @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

  { ?x ex:parent ?y } => { ?y ex:child ?x } .
  { ?x ex:parent ?y . ?y ex:parent ?z } => { ?x ex:grandparent ?z } .
`;

const reasoned = await reason(store, rules);
console.log(`Inferred ${reasoned.size - store.size} new triples`);
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `reasonMultiple(store, rulesList, options)`

Apply multiple rule sets sequentially.

**Type Signature**:
```javascript
/**
 * @param {Store} store - Store to reason over
 * @param {Array<string>} rulesList - Array of rule sets
 * @param {Object} [options] - Reasoning options
 * @returns {Promise<Store>} Store with all inferences
 */
async function reasonMultiple(store, rulesList, options)
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `extractInferred(originalStore, reasonedStore)`

Extract only the inferred triples.

**Type Signature**:
```javascript
/**
 * @param {Store} originalStore - Original store
 * @param {Store} reasonedStore - Store after reasoning
 * @returns {Store} Store containing only inferred triples
 */
function extractInferred(originalStore, reasonedStore)
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `getReasoningStats(originalStore, reasonedStore)`

Get statistics about reasoning results.

**Type Signature**:
```javascript
/**
 * @param {Store} originalStore - Original store
 * @param {Store} reasonedStore - Store after reasoning
 * @returns {Object} Reasoning statistics
 */
function getReasoningStats(originalStore, reasonedStore)
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `validateRules(rules)`

Validate N3 rules syntax.

**Type Signature**:
```javascript
/**
 * @param {string} rules - N3 rules to validate
 * @returns {Object} Validation result
 */
function validateRules(rules)
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `createReasoningSession(initialStore, rules, options)`

Create a persistent reasoning session.

**Type Signature**:
```javascript
/**
 * @param {Store} initialStore - Initial store
 * @param {string} rules - N3 rules
 * @param {Object} [options] - Session options
 * @returns {Object} Reasoning session
 */
async function createReasoningSession(initialStore, rules, options)
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `canonicalize(store, options)`

Canonicalize a store using URDNA2015 algorithm.

**Type Signature**:
```javascript
/**
 * @param {Store} store - Store to canonicalize
 * @param {Object} [options] - Canonicalization options
 * @returns {Promise<string>} Canonical N-Quads string
 */
async function canonicalize(store, options)
```

**Example**:
```javascript
import { canonicalize } from 'unrdf/knowledge-engine';

const canonical = await canonicalize(store);
// Produces deterministic, sorted N-Quads output
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `isIsomorphic(storeA, storeB, options)`

Check if two stores are graph-isomorphic.

**Type Signature**:
```javascript
/**
 * @param {Store} storeA - First store
 * @param {Store} storeB - Second store
 * @param {Object} [options] - Comparison options
 * @returns {Promise<boolean>} True if stores are isomorphic
 */
async function isIsomorphic(storeA, storeB, options)
```

**Example**:
```javascript
import { isIsomorphic } from 'unrdf/knowledge-engine';

const identical = await isIsomorphic(store1, store2);
if (identical) {
  console.log('Stores are semantically equivalent');
}
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `getCanonicalHash(store, options)`

Get cryptographic hash of canonical form.

**Type Signature**:
```javascript
/**
 * @param {Store} store - Store to hash
 * @param {Object} [options] - Hash options
 * @returns {Promise<string>} SHA-256 hash of canonical form
 */
async function getCanonicalHash(store, options)
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `groupByIsomorphism(stores, options)`

Group stores by isomorphism class.

**Type Signature**:
```javascript
/**
 * @param {Array<Store>} stores - Array of stores
 * @param {Object} [options] - Grouping options
 * @returns {Promise<Array<Array<Store>>>} Groups of isomorphic stores
 */
async function groupByIsomorphism(stores, options)
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `findDuplicates(stores, options)`

Find duplicate stores based on isomorphism.

**Type Signature**:
```javascript
/**
 * @param {Array<Store>} stores - Array of stores
 * @param {Object} [options] - Options
 * @returns {Promise<Array>} Array of duplicate groups
 */
async function findDuplicates(stores, options)
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `getCanonicalizationStats(store, options)`

Get canonicalization statistics.

**Type Signature**:
```javascript
/**
 * @param {Store} store - Store to analyze
 * @param {Object} [options] - Options
 * @returns {Promise<Object>} Canonicalization statistics
 */
async function getCanonicalizationStats(store, options)
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `createCanonicalizationSession(options)`

Create a canonicalization session for batch operations.

**Type Signature**:
```javascript
/**
 * @param {Object} [options] - Session options
 * @returns {Promise<Object>} Canonicalization session
 */
async function createCanonicalizationSession(options)
```

**Version**: 1.0.0 | **Stability**: Stable

---

### Class: `KnowledgeHookManager`

Production-ready hook manager extending TransactionManager with content-addressed, file-based hooks.

**Type Signature**:
```javascript
/**
 * @extends TransactionManager
 */
class KnowledgeHookManager {
  /**
   * @param {Object} [options] - Manager options
   * @param {string} [options.basePath] - Base path for file resolution
   * @param {boolean} [options.enableKnowledgeHooks=true] - Enable hooks
   * @param {boolean} [options.strictMode=false] - Strict error handling
   */
  constructor(options)
}
```

**Methods**:

| Method | Parameters | Returns | Description |
|--------|------------|---------|-------------|
| `addKnowledgeHook(hook)` | `Object` hook | `void` | Register a knowledge hook |
| `removeKnowledgeHook(name)` | `string` name | `boolean` | Remove a hook by name |
| `getKnowledgeHooks()` | - | `Array` | Get all registered hooks |
| `clearKnowledgeHooks()` | - | `void` | Remove all hooks |
| `executeKnowledgeHook(name, event, options)` | `string`, `Object`, `Object` | `Promise<Object>` | Execute single hook |
| `executeAllKnowledgeHooks(event, options)` | `Object`, `Object` | `Promise<Array>` | Execute all matching hooks |
| `apply(store, delta, options)` | `Store`, `Object`, `Object` | `Promise<Object>` | Apply transaction with hooks |
| `loadPolicyPack(packName)` | `string` | `Promise<boolean>` | Load and activate policy pack |
| `deactivatePolicyPack(packName)` | `string` | `boolean` | Deactivate policy pack |
| `getStats()` | - | `Object` | Get manager statistics |
| `clearCaches()` | - | `void` | Clear all caches |

**Example**:
```javascript
import { KnowledgeHookManager, defineHook } from 'unrdf/knowledge-engine';

const manager = new KnowledgeHookManager({
  basePath: process.cwd(),
  enableKnowledgeHooks: true,
  strictMode: true
});

// Register a compliance hook
const complianceHook = defineHook({
  meta: {
    name: 'compliance:gdpr-check',
    description: 'Validate GDPR compliance',
    version: '1.0.0'
  },
  when: {
    kind: 'sparql-ask',
    query: 'ASK { ?s <http://example.org/hasPersonalData> true }'
  },
  run: async (event, context) => {
    const { graph } = context;
    // Perform GDPR validation
    return { success: true, compliant: true };
  }
});

manager.addKnowledgeHook(complianceHook);

// Apply transaction with hook execution
const result = await manager.apply(store, {
  additions: [quad1, quad2],
  removals: []
});

console.log('Hook results:', result.receipt.knowledgeHookResults);
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `defineHook(definition)`

Create a validated, frozen knowledge hook definition.

**Type Signature**:
```javascript
/**
 * @param {Object} definition - Hook definition
 * @param {Object} definition.meta - Hook metadata
 * @param {string} definition.meta.name - Unique hook name
 * @param {string} [definition.meta.description] - Description
 * @param {string} [definition.meta.version] - Semantic version
 * @param {Object} definition.when - Trigger condition
 * @param {Function} definition.run - Main execution function
 * @param {Function} [definition.before] - Pre-execution hook
 * @param {Function} [definition.after] - Post-execution hook
 * @param {Object} [definition.determinism] - Determinism config
 * @param {Object} [definition.receipt] - Receipt config
 * @returns {Object} Frozen, validated hook definition
 * @throws {TypeError} If definition is invalid
 */
function defineHook(definition)
```

**Hook Condition Types**:

| Kind | Description | Required Fields |
|------|-------------|-----------------|
| `sparql-ask` | SPARQL ASK query | `ref` or `query` |
| `sparql-select` | SPARQL SELECT query | `ref` or `query` |
| `shacl` | SHACL validation | `ref` or `shapes` |
| `delta` | Change detection | `spec.change`, `spec.key` |
| `threshold` | Threshold condition | `spec.var`, `spec.op`, `spec.value` |
| `count` | Count condition | `spec.op`, `spec.value` |
| `window` | Time window | `spec.size`, `spec.aggregate` |

**Example**:
```javascript
import { defineHook } from 'unrdf/knowledge-engine';

const auditHook = defineHook({
  meta: {
    name: 'audit:transaction-log',
    description: 'Log all transactions to audit trail',
    version: '1.0.0',
    tags: ['audit', 'compliance']
  },
  when: {
    kind: 'delta',
    spec: {
      change: 'any',
      key: ['additions', 'removals']
    }
  },
  run: async (event, context) => {
    const { payload } = event;
    console.log(`Transaction: ${payload.additionsCount} additions, ${payload.removalsCount} removals`);
    return { success: true, logged: true };
  },
  determinism: {
    seed: 42,
    algorithm: 'lcg'
  },
  receipt: {
    anchor: 'git-notes',
    format: 'json'
  }
});
```

**Version**: 1.0.0 | **Stability**: Stable

---

### Class: `TransactionManager`

Base transaction manager with hook support and receipt generation.

**Type Signature**:
```javascript
class TransactionManager {
  /**
   * @param {Object} [options] - Manager options
   * @param {boolean} [options.strictMode=false] - Strict mode
   * @param {number} [options.maxHooks=100] - Maximum hooks
   */
  constructor(options)
}
```

**Methods**:

| Method | Returns | Description |
|--------|---------|-------------|
| `addHook(hook)` | `void` | Add transaction hook |
| `removeHook(id)` | `boolean` | Remove hook by ID |
| `apply(store, delta, options)` | `Promise<{store, receipt}>` | Apply delta to store |
| `getStats()` | `Object` | Get manager statistics |

**Version**: 1.0.0 | **Stability**: Stable

---

### `createKnowledgeEngine(options)`

Factory function to create a complete knowledge engine instance.

**Type Signature**:
```javascript
/**
 * @param {Object} [options] - Engine options
 * @param {string} [options.baseIRI='http://example.org/'] - Base IRI
 * @param {boolean} [options.strictMode=false] - Strict validation
 * @param {number} [options.maxHooks=100] - Maximum hooks
 * @returns {Object} Knowledge engine instance with all methods
 */
function createKnowledgeEngine(options)
```

**Returns**: Engine object with all parsing, querying, validation, reasoning, and transaction methods bound.

**Example**:
```javascript
import { createKnowledgeEngine } from 'unrdf/knowledge-engine';

const engine = createKnowledgeEngine({
  baseIRI: 'http://example.org/',
  strictMode: true
});

const store = await engine.parseTurtle(ttl);
const results = await engine.query(store, 'SELECT * WHERE { ?s ?p ?o }');
const report = engine.validateShacl(store, shapes);
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `createSimpleEngine(baseIRI)`

Create a simple engine with defaults.

**Type Signature**:
```javascript
/**
 * @param {string} [baseIRI='http://example.org/'] - Base IRI
 * @returns {Object} Simple knowledge engine
 */
function createSimpleEngine(baseIRI)
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `createStrictEngine(baseIRI)`

Create a strict engine with validation enabled.

**Type Signature**:
```javascript
/**
 * @param {string} [baseIRI='http://example.org/'] - Base IRI
 * @returns {Object} Strict knowledge engine
 */
function createStrictEngine(baseIRI)
```

**Version**: 1.0.0 | **Stability**: Stable

---

## Composables

### Module: `unrdf` (composables)

Composables provide reactive-style interfaces for RDF operations using the unctx context system.

---

### `useGraph()`

High-level RDF graph operations with context.

**Type Signature**:
```javascript
/**
 * @returns {Object} Graph operations interface
 * @throws {Error} If store context not initialized
 */
function useGraph()
```

**Returns**:

| Property/Method | Type | Description |
|-----------------|------|-------------|
| `query(sparql, options)` | `async function` | Execute SPARQL query |
| `select(sparql)` | `async function` | Execute SELECT query |
| `ask(sparql)` | `async function` | Execute ASK query |
| `construct(sparql)` | `async function` | Execute CONSTRUCT query |
| `update(sparql)` | `async function` | Execute UPDATE query |
| `validate(shapes)` | `function` | Validate against SHACL |
| `validateOrThrow(shapes)` | `function` | Validate, throw on failure |
| `serialize(options)` | `function` | Serialize to string |
| `pointer()` | `function` | Get Clownface pointer |
| `stats()` | `function` | Get graph statistics |
| `isIsomorphic(other)` | `function` | Check isomorphism |
| `union(...others)` | `function` | Create union graph |
| `difference(other)` | `function` | Create difference graph |
| `intersection(other)` | `function` | Create intersection graph |
| `skolemize(baseIRI)` | `function` | Skolemize blank nodes |
| `toJSONLD(options)` | `function` | Convert to JSON-LD |
| `size` | `number` | Number of quads |

**Example**:
```javascript
import { initStore, useGraph } from 'unrdf';

const runApp = initStore();

runApp(async () => {
  const graph = useGraph();

  // Query
  const results = await graph.select(`
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT ?name WHERE { ?s foaf:name ?name }
  `);

  // Validate
  const report = graph.validate(shaclShapes);
  if (!report.conforms) {
    throw new Error('Validation failed');
  }

  // Serialize
  const turtle = graph.serialize({ format: 'Turtle' });
});
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `useTurtle(graphDir, options)`

Turtle file I/O operations.

**Type Signature**:
```javascript
/**
 * @param {string} [graphDir='./graph'] - Directory for Turtle files
 * @param {Object} [options] - Options
 * @param {string} [options.baseIRI] - Base IRI for parsing
 * @param {boolean} [options.autoLoad=true] - Auto-load .ttl files
 * @param {boolean} [options.validateOnLoad=true] - Validate on load
 * @returns {Object} Turtle file system interface
 */
function useTurtle(graphDir, options)
```

**Returns**:

| Property/Method | Type | Description |
|-----------------|------|-------------|
| `store` | `Object` | Store context |
| `graphDir` | `string` | Directory path |
| `engine` | `RdfEngine` | Engine instance |
| `loadAll(options)` | `function` | Load all .ttl files |
| `load(fileName, options)` | `function` | Load specific file |
| `save(fileName, options)` | `function` | Save to file |
| `saveDefault(options)` | `function` | Save to default.ttl |
| `loadDefault(options)` | `function` | Load default.ttl |
| `listFiles()` | `function` | List .ttl files |
| `stats()` | `function` | Get store stats |
| `clear()` | `function` | Clear store |
| `parse(ttl, options)` | `function` | Parse Turtle string |
| `serialize(options)` | `function` | Serialize to Turtle |

**Example**:
```javascript
import { initStore, useTurtle } from 'unrdf';

const runApp = initStore();

runApp(() => {
  const turtle = useTurtle('./my-graph');

  // Load all files
  const { loaded, files } = turtle.loadAll();
  console.log(`Loaded ${loaded} files`);

  // Save changes
  turtle.save('updated', { createBackup: true });
});
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `useZod(options)`

Dynamic Zod schema generation from RDF data.

**Type Signature**:
```javascript
/**
 * @param {Object} [options] - Options
 * @param {boolean} [options.strict=true] - Enable strict validation
 * @returns {Object} Zod schema generation interface
 */
function useZod(options)
```

**Returns**:

| Method | Parameters | Returns | Description |
|--------|------------|---------|-------------|
| `generateFromResults(results, options)` | `Array, Object` | `z.ZodSchema` | Generate from SPARQL results |
| `generateFromPattern(pattern, options)` | `string, Object` | `z.ZodSchema` | Generate from store patterns |
| `generateFromSHACL(shapeIRI, options)` | `string, Object` | `z.ZodSchema` | Generate from SHACL shapes |
| `validate(data, schema, options)` | `any, ZodSchema, Object` | `Object` | Validate data |

**Example**:
```javascript
import { initStore, useGraph, useZod } from 'unrdf';

const runApp = initStore();

runApp(async () => {
  const graph = useGraph();
  const zod = useZod();

  const results = await graph.select(`
    SELECT ?id ?name ?age WHERE {
      ?id <http://example.org/name> ?name ;
          <http://example.org/age> ?age .
    }
  `);

  // Generate schema from results
  const schema = zod.generateFromResults(results);

  // Validate new data
  const validation = zod.validate(newData, schema);
  if (!validation.success) {
    console.error('Invalid data:', validation.error);
  }
});
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `useReasoner()`

N3 reasoning composable.

**Type Signature**:
```javascript
/**
 * @returns {Object} Reasoner interface
 */
function useReasoner()
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `useTerms()`

RDF term utilities composable.

**Type Signature**:
```javascript
/**
 * @returns {Object} Term utilities interface
 */
function useTerms()
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `useDelta()`

Change tracking and provenance composable.

**Type Signature**:
```javascript
/**
 * @returns {Object} Delta tracking interface
 */
function useDelta()
```

**Version**: 1.0.0 | **Stability**: Stable

---

### `useCanon()`

Canonicalization composable.

**Type Signature**:
```javascript
/**
 * @returns {Object} Canonicalization interface
 */
function useCanon()
```

**Version**: 1.0.0 | **Stability**: Stable

---

## React Hooks

### Module: `unrdf/react-hooks`

React hooks follow an 80/20 optimization pattern with tiered exports.

---

### Tier 1: Essential (60% usage)

#### `useKnowledgeEngine(options)`

Core knowledge engine hook.

**Type Signature**:
```javascript
/**
 * @param {Object} [options] - Engine options
 * @returns {Object} Knowledge engine state and methods
 */
function useKnowledgeEngine(options)
```

**Returns**:
```javascript
{
  engine: Object,        // Knowledge engine instance
  store: Store,          // Current N3 store
  loading: boolean,      // Loading state
  error: Error | null,   // Error state
  query: Function,       // Query function
  validate: Function,    // Validation function
  reason: Function,      // Reasoning function
  // ... additional methods
}
```

**Example**:
```javascript
import { useKnowledgeEngine } from 'unrdf/react-hooks';

function KnowledgeGraph() {
  const { store, query, loading, error } = useKnowledgeEngine({
    baseIRI: 'http://example.org/'
  });

  const [results, setResults] = useState([]);

  useEffect(() => {
    query(store, 'SELECT * WHERE { ?s ?p ?o } LIMIT 10')
      .then(setResults);
  }, [store]);

  if (loading) return <div>Loading...</div>;
  if (error) return <div>Error: {error.message}</div>;

  return (
    <ul>
      {results.map((r, i) => (
        <li key={i}>{r.s} - {r.p} - {r.o}</li>
      ))}
    </ul>
  );
}
```

**Version**: 1.0.0 | **Stability**: Stable

---

#### `useKnowledgeEngineContext()`

Access knowledge engine from context.

**Version**: 1.0.0 | **Stability**: Stable

---

#### `useTransaction()`

Transaction management hook.

**Version**: 1.0.0 | **Stability**: Stable

---

#### `useKnowledgeHook(hookDefinition)`

Register and manage knowledge hooks.

**Version**: 1.0.0 | **Stability**: Stable

---

#### `useChangeFeed(options)`

Real-time change feed subscription.

**Type Signature**:
```javascript
/**
 * @param {Object} [options] - Feed options
 * @returns {Object} Change feed state and controls
 */
function useChangeFeed(options)
```

**Returns**:
```javascript
{
  changes: Array,        // Array of changes
  isConnected: boolean,  // Connection state
  subscribe: Function,   // Subscribe to changes
  unsubscribe: Function, // Unsubscribe
  clear: Function        // Clear changes
}
```

**Version**: 1.0.0 | **Stability**: Stable

---

#### `useDarkMatterCore(options)`

80/20 query optimization hook.

**Type Signature**:
```javascript
/**
 * @param {Object} [options] - Optimization options
 * @returns {Object} Dark matter optimization interface
 */
function useDarkMatterCore(options)
```

**Version**: 1.0.0 | **Stability**: Stable

---

#### `useQueryAnalyzer()`

Query analysis and optimization.

**Version**: 1.0.0 | **Stability**: Stable

---

#### `useErrorBoundary()`

Error boundary hook for graceful error handling.

**Version**: 1.0.0 | **Stability**: Stable

---

### Tier 2: Important (20% usage)

#### `useGraphDiff()`

Graph difference and comparison.

**Version**: 1.0.0 | **Stability**: Stable

---

#### `useSPARQLEditor(options)`

SPARQL query editor with syntax highlighting.

**Version**: 1.0.0 | **Stability**: Stable

---

### Tier 3: Standard (15% usage)

#### `useFederatedSystem(options)`

Multi-source federation management.

**Version**: 1.0.0 | **Stability**: Stable

---

#### `useStreamProcessor(options)`

Stream processing for large datasets.

**Version**: 1.0.0 | **Stability**: Stable

---

#### `useOptimizer()`

Query optimizer interface.

**Version**: 1.0.0 | **Stability**: Stable

---

#### `useSemanticAnalyzer()`

Semantic analysis and NLP integration.

**Version**: 1.0.0 | **Stability**: Stable

---

#### `useGraphMerge()`

Graph merge operations.

**Version**: 1.0.0 | **Stability**: Stable

---

#### `usePolicyPack()`

Policy pack management.

**Version**: 1.0.0 | **Stability**: Stable

---

#### `useRecovery()`

Error recovery and rollback.

**Version**: 1.0.0 | **Stability**: Stable

---

#### `useGraphVisualizer()`

Graph visualization utilities.

**Version**: 1.0.0 | **Stability**: Stable

---

#### `useResultsPaginator()`

Results pagination.

**Version**: 1.0.0 | **Stability**: Stable

---

### Composition Hooks

#### `useKnowledgeStack()`

Pre-configured bundle for common patterns.

**Type Signature**:
```javascript
/**
 * @returns {Object} Combined hook functionality
 */
function useKnowledgeStack()
```

**Version**: 1.0.0 | **Stability**: Stable

---

#### `useCRUDStack()`

CRUD operations bundle.

**Version**: 1.0.0 | **Stability**: Stable

---

#### `useDashboardStack()`

Dashboard analytics bundle.

**Version**: 1.0.0 | **Stability**: Stable

---

#### `useProductionStack()`

Production-ready bundle with error handling and monitoring.

**Version**: 1.0.0 | **Stability**: Stable

---

#### `useOfflineStore()`

Offline-first storage with IndexedDB and sync queue.

**Type Signature**:
```javascript
/**
 * @param {Object} [options] - Offline options
 * @returns {Object} Offline store interface
 */
function useOfflineStore(options)
```

**Version**: 1.0.0 | **Stability**: Stable

---

### Category Exports

Access advanced hooks via category imports:

```javascript
import * as Federation from 'unrdf/react-hooks/federation';
import * as Streaming from 'unrdf/react-hooks/streaming';
import * as DarkMatter from 'unrdf/react-hooks/dark-matter';
import * as AISemantic from 'unrdf/react-hooks/ai-semantic';
import * as AdvancedUtility from 'unrdf/react-hooks/advanced-utility';
import * as PolicySecurity from 'unrdf/react-hooks/policy-security';
import * as ErrorRecovery from 'unrdf/react-hooks/error-recovery';
import * as FormUI from 'unrdf/react-hooks/form-ui';
import * as Composition from 'unrdf/react-hooks/composition';
```

---

## CLI

### Module: `unrdf/cli`

Command-line interface for RDF store management.

**Usage**:
```bash
npx unrdf <command> [options]
```

---

### Commands

#### `store`

Store management operations.

```bash
unrdf store backup [--path <path>]
unrdf store restore <backup-file>
unrdf store import <file> [--format turtle|nquads|jsonld]
```

#### `graph`

Graph operations.

```bash
unrdf graph list
unrdf graph get <graph-iri>
unrdf graph create <graph-iri>
unrdf graph delete <graph-iri>
unrdf graph validate <graph-iri> --shapes <shapes-file>
unrdf graph export <graph-iri> --format <format>
unrdf graph describe <graph-iri>
unrdf graph update <graph-iri> --file <data-file>
```

#### `context`

Context management.

```bash
unrdf context list
unrdf context create <name>
unrdf context use <name>
unrdf context current
unrdf context get <name>
unrdf context delete <name>
```

---

## Utilities

### Module: `unrdf/utils`

Comprehensive utility library organized by domain.

---

### Term Utilities

Functions for RDF term manipulation.

```javascript
import { asNamedNode, asLiteral, asBlankNode } from 'unrdf/utils';

const subject = asNamedNode('http://example.org/alice');
const name = asLiteral('Alice', 'xsd:string');
const blank = asBlankNode('b1');
```

---

### Quad Utilities

Quad/JSON transformations and filtering.

```javascript
import { quadToJSON, jsonToQuad, filterQuads } from 'unrdf/utils';

const json = quadToJSON(quad);
const quad = jsonToQuad(json);
const filtered = filterQuads(store, { predicate: 'http://example.org/name' });
```

---

### Graph Utilities

Store operations and query helpers.

```javascript
import { getObjects, getSubjects, getPredicates } from 'unrdf/utils';

const objects = getObjects(store, subject, predicate);
const subjects = getSubjects(store, predicate, object);
```

---

### Validation Utilities

Zod schemas and validation.

```javascript
import { validateQuadJSON, QuadSchema } from 'unrdf/utils';

const result = validateQuadJSON(data);
if (!result.success) {
  console.error(result.errors);
}
```

---

### I/O Utilities

File operations for RDF formats.

```javascript
import { readTurtleFile, writeTurtleFile } from 'unrdf/utils';

const store = await readTurtleFile('./data.ttl');
await writeTurtleFile(store, './output.ttl');
```

---

### Debug Utilities

Introspection, logging, and performance.

```javascript
import { inspectStore, measurePerformance } from 'unrdf/utils';

inspectStore(store);
const { duration, result } = await measurePerformance(() => query(store, sparql));
```

---

### ID Utilities

Blank nodes, UUIDs, and IRI generation.

```javascript
import { generateUUID, generateBlankNode, mintIRI } from 'unrdf/utils';

const id = generateUUID();
const blank = generateBlankNode();
const iri = mintIRI('http://example.org/', 'person');
```

---

### Namespace Utilities

Vocabulary and namespace management.

```javascript
import { createNamespaceManager } from 'unrdf/utils';

const ns = createNamespaceManager({
  ex: 'http://example.org/',
  foaf: 'http://xmlns.com/foaf/0.1/'
});

const personType = ns.expand('foaf:Person');
const compact = ns.compact('http://xmlns.com/foaf/0.1/Person');
```

---

### SPARQL Utilities

Query building and operations.

```javascript
import { createSPARQLBuilder } from 'unrdf/utils';

const query = createSPARQLBuilder()
  .prefix('foaf', 'http://xmlns.com/foaf/0.1/')
  .select('?name', '?email')
  .where('?person', 'a', 'foaf:Person')
  .where('?person', 'foaf:name', '?name')
  .optional('?person', 'foaf:mbox', '?email')
  .limit(100)
  .build();
```

---

### Transform Utilities

Data transformations.

```javascript
import { transformStore, mapQuads } from 'unrdf/utils';

const transformed = transformStore(store, quad => ({
  ...quad,
  graph: newGraph
}));
```

---

### Merge Utilities

Store merging operations.

```javascript
import { mergeStores, deduplicateStore } from 'unrdf/utils';

const merged = mergeStores(store1, store2, store3);
const deduped = deduplicateStore(merged);
```

---

### Quality Utilities

Data quality assessment.

```javascript
import { assessDataQuality } from 'unrdf/utils';

const quality = assessDataQuality(store);
console.log(`Completeness: ${quality.completeness}%`);
console.log(`Consistency: ${quality.consistency}%`);
```

---

## Schemas

### Module: `unrdf/knowledge-engine` (schemas)

Zod schemas for runtime validation.

---

### Core Schemas

| Schema | Description |
|--------|-------------|
| `HookMetaSchema` | Hook metadata validation |
| `FileRefSchema` | Content-addressed file references |
| `ConditionSchema` | Hook condition validation |
| `KnowledgeHookSchema` | Complete hook definition |
| `HookEventSchema` | Hook event validation |
| `HookResultSchema` | Execution result validation |
| `TransactionDeltaSchema` | Transaction delta validation |
| `TransactionReceiptSchema` | Receipt validation |
| `ManagerConfigSchema` | Manager configuration |
| `ObservabilityConfigSchema` | OpenTelemetry configuration |

---

### Condition Schemas

| Schema | Kind | Description |
|--------|------|-------------|
| `SparqlAskConditionSchema` | `sparql-ask` | SPARQL ASK conditions |
| `SparqlSelectConditionSchema` | `sparql-select` | SPARQL SELECT conditions |
| `ShaclConditionSchema` | `shacl` | SHACL validation conditions |
| `DeltaConditionSchema` | `delta` | Change detection conditions |
| `ThresholdConditionSchema` | `threshold` | Threshold conditions |
| `CountConditionSchema` | `count` | Count conditions |
| `WindowConditionSchema` | `window` | Time window conditions |

---

### Validation Functions

| Function | Parameters | Returns | Description |
|----------|------------|---------|-------------|
| `validateKnowledgeHook(hook)` | `any` | `{success, data, errors}` | Validate hook definition |
| `validateHookEvent(event)` | `any` | `{success, data, errors}` | Validate hook event |
| `validateCondition(condition)` | `any` | `{success, data, errors}` | Validate condition |
| `validateManagerConfig(config)` | `any` | `{success, data, errors}` | Validate config |
| `validateTransactionDelta(delta)` | `any` | `{success, data, errors}` | Validate delta |
| `createKnowledgeHook(definition)` | `any` | `Object` | Create validated hook |
| `createHookEvent(event)` | `any` | `Object` | Create validated event |
| `createCondition(condition)` | `any` | `Object` | Create validated condition |

---

## Error Handling

### Error Types

| Error | Thrown By | Cause |
|-------|-----------|-------|
| `TypeError` | All functions | Invalid parameter types |
| `Error` | Parsing functions | Invalid RDF syntax |
| `Error` | Query functions | SPARQL execution failure |
| `Error` | Validation functions | SHACL validation failure |
| `Error` | Hook functions | Hook execution failure |

### Error Patterns

```javascript
try {
  const store = await parseTurtle(invalidTurtle);
} catch (error) {
  if (error instanceof TypeError) {
    console.error('Invalid input type:', error.message);
  } else {
    console.error('Parsing failed:', error.message);
  }
}
```

### Validation Error Format

```javascript
{
  success: false,
  data: null,
  errors: [
    {
      path: 'meta.name',
      message: 'Required',
      code: 'invalid_type',
      received: undefined,
      expected: 'string'
    }
  ]
}
```

---

## Performance Characteristics

### Parsing Performance

| Operation | Small (<1000 quads) | Medium (1K-100K) | Large (>100K) |
|-----------|---------------------|------------------|---------------|
| `parseTurtle` | 1-5ms | 10-100ms | 100ms-1s |
| `toTurtle` | 2-10ms | 20-200ms | 200ms-2s |
| `parseJsonLd` | 5-20ms | 50-500ms | 500ms-5s |
| `toJsonLd` | 5-20ms | 50-500ms | 500ms-5s |

### Query Performance

| Query Type | Simple | Complex (JOINs) | Federation |
|------------|--------|-----------------|------------|
| SELECT | <10ms | 10-100ms | 100ms-10s |
| ASK | <5ms | 5-50ms | 50ms-5s |
| CONSTRUCT | <20ms | 20-200ms | 200ms-20s |

### Validation Performance

| Operation | Small Shapes | Large Shapes |
|-----------|--------------|--------------|
| `validateShacl` | 10-50ms | 50-500ms |

### Memory Usage

- Base overhead: ~50MB
- Per 1000 quads: ~1MB additional
- Query engine cache: ~20MB
- Large stores (>1M quads): Consider streaming

---

## FMEA: Common Failure Modes

### API Misuse

| Misuse | Impact | Prevention |
|--------|--------|------------|
| Unbounded queries | Memory exhaustion | Always use LIMIT |
| Nested OPTIONAL | Exponential blowup | Restructure queries |
| Large file parsing | Memory exhaustion | Use streaming |
| Missing prefixes | Relative URI issues | Always declare prefixes |
| Unvalidated input | Runtime crashes | Use Zod schemas |

### Performance Traps

| Trap | Detection | Mitigation |
|------|-----------|------------|
| Query engine re-initialization | Slow first query | Use singleton |
| Large CONSTRUCT results | Memory spikes | Add LIMIT |
| Federation without timeouts | Hanging queries | Set signal |
| Synchronous validation | UI blocking | Use async patterns |

### Deprecated Features

| Feature | Deprecation | Alternative |
|---------|-------------|-------------|
| None currently | - | - |

---

## Related Documentation

- [Getting Started Guide](/docs/guides/getting-started.md)
- [Knowledge Hooks Guide](/docs/guides/knowledge-hooks.md)
- [Core Concepts](/docs/core-concepts.md)
- [CLI Reference](/docs/cli/commands.md)
- [Migration Guide](/docs/migration-guide.md)

---

> **Document Version**: 1.0.0
> **Generated**: 2025-11-21
> **Maintainer**: UNRDF Team
