# UNRDF Capability Cross-Reference Index

**Generated**: 2025-12-28T04:09:54.772Z
**Total Capabilities**: 48
**Total Packages Analyzed**: 2

---

## Overview

This index provides a comprehensive cross-reference of all capabilities across the UNRDF ecosystem. Each capability lists all packages that provide or use it, enabling developers to:

1. Find implementations of specific capabilities
2. Discover alternative packages for the same capability
3. Understand capability distribution across packages
4. Identify potential composition opportunities

---

## Index by Capability

### addQuad(quad)

**Type**: Function

**Provided by**:

- **@unrdf/core** (RDF Store Operations)
  - Evidence: src/index.mjs:67
  - Runtime: Node.js, Browser

### blankNode(name)

**Type**: Function

**Provided by**:

- **@unrdf/core** (RDF Data Factory)
  - Evidence: src/index.mjs:200
  - Runtime: Node.js, Browser

### canonicalize(dataset)

**Type**: Function

**Provided by**:

- **@unrdf/core** (Serialization & Canonicalization)
  - Evidence: src/utils/canonicalize.mjs:12
  - Runtime: Node.js, Browser

### CircuitBreaker

**Type**: Class

**Provided by**:

- **@unrdf/core** (Observability & Debugging)
  - Evidence: src/utils/resilience.mjs:45
  - Runtime: Node.js, Browser

### COMMON_PREFIXES

**Type**: Const

**Provided by**:

- **@unrdf/core** (RDF Namespaces)
  - Evidence: src/constants.mjs:33
  - Runtime: Node.js, Browser

### countQuads(pattern)

**Type**: Function

**Provided by**:

- **@unrdf/core** (RDF Store Operations)
  - Evidence: src/index.mjs:123
  - Runtime: Node.js, Browser

### createStore()

**Type**: Function

**Provided by**:

- **@unrdf/core** (RDF Store Operations)
  - Evidence: src/index.mjs:45
  - Runtime: Node.js, Browser
- **@unrdf/oxigraph** (Store Creation & Management)
  - Evidence: src/store.mjs:8
  - Runtime: Node.js

### dataFactory.blankNode(name)

**Type**: Method

**Provided by**:

- **@unrdf/oxigraph** (RDF Data Factory)
  - Evidence: src/index.mjs:46
  - Runtime: Node.js

### dataFactory.literal(value, lang/datatype)

**Type**: Method

**Provided by**:

- **@unrdf/oxigraph** (RDF Data Factory)
  - Evidence: src/index.mjs:34
  - Runtime: Node.js

### dataFactory.namedNode(iri)

**Type**: Method

**Provided by**:

- **@unrdf/oxigraph** (RDF Data Factory)
  - Evidence: src/index.mjs:22
  - Runtime: Node.js

### dataFactory.quad(s, p, o, g)

**Type**: Method

**Provided by**:

- **@unrdf/oxigraph** (RDF Data Factory)
  - Evidence: src/index.mjs:58
  - Runtime: Node.js

### DCTERMS

**Type**: Const

**Provided by**:

- **@unrdf/core** (RDF Namespaces)
  - Evidence: src/constants.mjs:26
  - Runtime: Node.js, Browser

### DebugLogger

**Type**: Class

**Provided by**:

- **@unrdf/core** (Observability & Debugging)
  - Evidence: src/logger.mjs:12
  - Runtime: Node.js, Browser

### defaultGraph()

**Type**: Function

**Provided by**:

- **@unrdf/core** (RDF Data Factory)
  - Evidence: src/index.mjs:266
  - Runtime: Node.js, Browser

### executeAsk(query)

**Type**: Function (async)

**Provided by**:

- **@unrdf/core** (SPARQL Query Execution)
  - Evidence: src/sparql/index.mjs:67
  - Runtime: Node.js, Browser

### executeConstruct(query)

**Type**: Function (async)

**Provided by**:

- **@unrdf/core** (SPARQL Query Execution)
  - Evidence: src/sparql/index.mjs:89
  - Runtime: Node.js, Browser

### executeQuery(query)

**Type**: Function (async)

**Provided by**:

- **@unrdf/core** (SPARQL Query Execution)
  - Evidence: src/sparql/index.mjs:12
  - Runtime: Node.js, Browser

### executeQuerySync(query)

**Type**: Function (sync)

**Provided by**:

- **@unrdf/core** (SPARQL Query Execution)
  - Evidence: src/sparql/index.mjs:134
  - Runtime: Node.js

### executeSelect(query)

**Type**: Function (async)

**Provided by**:

- **@unrdf/core** (SPARQL Query Execution)
  - Evidence: src/sparql/index.mjs:45
  - Runtime: Node.js, Browser

### getQuads(pattern)

**Type**: Function

**Provided by**:

- **@unrdf/core** (RDF Store Operations)
  - Evidence: src/index.mjs:101
  - Runtime: Node.js, Browser

### isIsomorphic(ds1, ds2)

**Type**: Function

**Provided by**:

- **@unrdf/core** (Serialization & Canonicalization)
  - Evidence: src/utils/canonicalize.mjs:89
  - Runtime: Node.js, Browser

### literal(value, lang/datatype)

**Type**: Function

**Provided by**:

- **@unrdf/core** (RDF Data Factory)
  - Evidence: src/index.mjs:178
  - Runtime: Node.js, Browser

### namedNode(iri)

**Type**: Function

**Provided by**:

- **@unrdf/core** (RDF Data Factory)
  - Evidence: src/index.mjs:156
  - Runtime: Node.js, Browser

### OWL

**Type**: Const

**Provided by**:

- **@unrdf/core** (RDF Namespaces)
  - Evidence: src/constants.mjs:19
  - Runtime: Node.js, Browser

### OxigraphStore

**Type**: Class

**Provided by**:

- **@unrdf/oxigraph** (Store Creation & Management)
  - Evidence: src/store.mjs:22
  - Runtime: Node.js

### PerformanceTracker

**Type**: Class

**Provided by**:

- **@unrdf/core** (Observability & Debugging)
  - Evidence: src/metrics.mjs:8
  - Runtime: Node.js, Browser

### prepareQuery(query)

**Type**: Function (async)

**Provided by**:

- **@unrdf/core** (SPARQL Query Execution)
  - Evidence: src/sparql/index.mjs:111
  - Runtime: Node.js, Browser

### quad(s, p, o, g)

**Type**: Function

**Provided by**:

- **@unrdf/core** (RDF Data Factory)
  - Evidence: src/index.mjs:244
  - Runtime: Node.js, Browser

### RateLimiter

**Type**: Class

**Provided by**:

- **@unrdf/core** (Observability & Debugging)
  - Evidence: src/utils/resilience.mjs:89
  - Runtime: Node.js, Browser

### RDF

**Type**: Const

**Provided by**:

- **@unrdf/core** (RDF Namespaces)
  - Evidence: src/constants.mjs:5
  - Runtime: Node.js, Browser

### RDFS

**Type**: Const

**Provided by**:

- **@unrdf/core** (RDF Namespaces)
  - Evidence: src/constants.mjs:12
  - Runtime: Node.js, Browser

### removeQuad(quad)

**Type**: Function

**Provided by**:

- **@unrdf/core** (RDF Store Operations)
  - Evidence: src/index.mjs:89
  - Runtime: Node.js, Browser

### retry(fn, options)

**Type**: Function

**Provided by**:

- **@unrdf/core** (Observability & Debugging)
  - Evidence: src/utils/resilience.mjs:134
  - Runtime: Node.js, Browser

### sortQuads(quads)

**Type**: Function

**Provided by**:

- **@unrdf/core** (Serialization & Canonicalization)
  - Evidence: src/utils/canonicalize.mjs:67
  - Runtime: Node.js, Browser

### store.dump(format, graph)

**Type**: Method

**Provided by**:

- **@unrdf/oxigraph** (Store Creation & Management)
  - Evidence: src/store.mjs:177
  - Runtime: Node.js

### store.insert(quad)

**Type**: Method

**Provided by**:

- **@unrdf/oxigraph** (Data Insertion & Deletion)
  - Evidence: src/store.mjs:34
  - Runtime: Node.js

### store.load(content, format)

**Type**: Method

**Provided by**:

- **@unrdf/oxigraph** (Store Creation & Management)
  - Evidence: src/store.mjs:156
  - Runtime: Node.js

### store.match(s, p, o, g)

**Type**: Method

**Provided by**:

- **@unrdf/oxigraph** (Pattern Matching & Filtering)
  - Evidence: src/store.mjs:93
  - Runtime: Node.js

### store.query(sparql)

**Type**: Method

**Provided by**:

- **@unrdf/oxigraph** (Query Execution)
  - Evidence: src/store.mjs:115
  - Runtime: Node.js

### store.remove(quad)

**Type**: Method

**Provided by**:

- **@unrdf/oxigraph** (Data Insertion & Deletion)
  - Evidence: src/store.mjs:56
  - Runtime: Node.js

### store.removeMatches(pattern)

**Type**: Method

**Provided by**:

- **@unrdf/oxigraph** (Data Insertion & Deletion)
  - Evidence: src/store.mjs:78
  - Runtime: Node.js

### toNTriples(quads)

**Type**: Function

**Provided by**:

- **@unrdf/core** (Serialization & Canonicalization)
  - Evidence: src/utils/canonicalize.mjs:45
  - Runtime: Node.js, Browser

### UnrdfStore

**Type**: Class

**Provided by**:

- **@unrdf/core** (RDF Store Operations)
  - Evidence: src/index.mjs:12
  - Runtime: Node.js, Browser

### validateIRI(iri)

**Type**: Function

**Provided by**:

- **@unrdf/core** (Validation & Constraints)
  - Evidence: src/validation/index.mjs:67
  - Runtime: Node.js, Browser

### validateLiteral(value, datatype)

**Type**: Function

**Provided by**:

- **@unrdf/core** (Validation & Constraints)
  - Evidence: src/validation/index.mjs:89
  - Runtime: Node.js, Browser

### validateTriple(quad)

**Type**: Function

**Provided by**:

- **@unrdf/core** (Validation & Constraints)
  - Evidence: src/validation/index.mjs:45
  - Runtime: Node.js, Browser

### ValidationError

**Type**: Class

**Provided by**:

- **@unrdf/core** (Validation & Constraints)
  - Evidence: src/validation/index.mjs:8
  - Runtime: Node.js, Browser

### variable(name)

**Type**: Function

**Provided by**:

- **@unrdf/core** (RDF Data Factory)
  - Evidence: src/index.mjs:222
  - Runtime: Node.js, Browser

---

## Index by Package

### @unrdf/core

**Total Capabilities**: 36

#### RDF Store Operations

- `addQuad(quad)` (Function)
- `countQuads(pattern)` (Function)
- `createStore()` (Function)
- `getQuads(pattern)` (Function)
- `removeQuad(quad)` (Function)
- `UnrdfStore` (Class)

#### RDF Data Factory

- `blankNode(name)` (Function)
- `defaultGraph()` (Function)
- `literal(value, lang/datatype)` (Function)
- `namedNode(iri)` (Function)
- `quad(s, p, o, g)` (Function)
- `variable(name)` (Function)

#### Serialization & Canonicalization

- `canonicalize(dataset)` (Function)
- `isIsomorphic(ds1, ds2)` (Function)
- `sortQuads(quads)` (Function)
- `toNTriples(quads)` (Function)

#### Observability & Debugging

- `CircuitBreaker` (Class)
- `DebugLogger` (Class)
- `PerformanceTracker` (Class)
- `RateLimiter` (Class)
- `retry(fn, options)` (Function)

#### RDF Namespaces

- `COMMON_PREFIXES` (Const)
- `DCTERMS` (Const)
- `OWL` (Const)
- `RDF` (Const)
- `RDFS` (Const)

#### SPARQL Query Execution

- `executeAsk(query)` (Function (async))
- `executeConstruct(query)` (Function (async))
- `executeQuery(query)` (Function (async))
- `executeQuerySync(query)` (Function (sync))
- `executeSelect(query)` (Function (async))
- `prepareQuery(query)` (Function (async))

#### Validation & Constraints

- `validateIRI(iri)` (Function)
- `validateLiteral(value, datatype)` (Function)
- `validateTriple(quad)` (Function)
- `ValidationError` (Class)

### @unrdf/oxigraph

**Total Capabilities**: 13

#### Store Creation & Management

- `createStore()` (Function)
- `OxigraphStore` (Class)
- `store.dump(format, graph)` (Method)
- `store.load(content, format)` (Method)

#### RDF Data Factory

- `dataFactory.blankNode(name)` (Method)
- `dataFactory.literal(value, lang/datatype)` (Method)
- `dataFactory.namedNode(iri)` (Method)
- `dataFactory.quad(s, p, o, g)` (Method)

#### Data Insertion & Deletion

- `store.insert(quad)` (Method)
- `store.remove(quad)` (Method)
- `store.removeMatches(pattern)` (Method)

#### Pattern Matching & Filtering

- `store.match(s, p, o, g)` (Method)

#### Query Execution

- `store.query(sparql)` (Method)

---

**Last Updated**: 2025-12-28T04:09:54.773Z
**Source**: Automated capability map analysis
