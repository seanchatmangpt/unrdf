# @unrdf/core

![Version](https://img.shields.io/badge/version-[VERSION]--beta.1-blue) ![Production Ready](https://img.shields.io/badge/production-ready-green)

**UNRDF Core - RDF Graph Operations, SPARQL Execution, and Foundational Substrate**

The essential RDF substrate that all UNRDF packages and applications build upon.

## Features

- **RDF Graph Operations**: Create, query, and manipulate RDF graphs
- **SPARQL Execution**: Execute SPARQL queries via Comunica
- **Canonicalization**: Deterministic RDF canonicalization
- **Type-Safe**: Full TypeScript support with Zod validation
- **Minimal Dependencies**: Focused on core functionality only

## Installation

```bash
pnpm add @unrdf/core
```

## 📚 Examples

See these examples that demonstrate @unrdf/core:

- **[01-minimal-parse-query.mjs](../../examples/01-minimal-parse-query.mjs)** - Start here! Minimal parse & query example (3 min)
- **[minimal-core-example.mjs](../../examples/minimal-core-example.mjs)** - Direct @unrdf/core usage without substrate wrapper
- **[context-example.mjs](../../examples/context-example.mjs)** - Understanding the core context system
- **[comprehensive-feature-test.mjs](../../examples/comprehensive-feature-test.mjs)** - All core features integrated
- **[graph-visualization.mjs](../../examples/graph-visualization.mjs)** - toDOT, toMermaid, toASCII, toHTML, extractSubgraph
- **[graph-diff.mjs](../../examples/graph-diff.mjs)** - diffGraphFromStores, diffGraphFromDelta, diffOntologyFromGraphDiff

**New to UNRDF?** Start with the [Quick Start Guide](../../examples/QUICKSTART.md).

## Quick Start

### Basic RDF Operations (2 minutes)

```javascript
import { createStore } from '@unrdf/oxigraph';
import { dataFactory } from '@unrdf/oxigraph';

// 1. Create an in-memory RDF store
const store = createStore();

// 2. Create RDF terms
const { namedNode, literal, quad } = dataFactory;

const alice = namedNode('http://example.com/alice');
const name = namedNode('http://xmlns.com/foaf/0.1/name');
const aliceName = literal('Alice');

// 3. Add triple to store
const triple = quad(alice, name, aliceName);
store.add(triple);

// 4. Check if triple exists
console.log(store.has(triple)); // true

// 5. Query triples by pattern
const matches = store.match(null, name, null);
for (const quad of matches) {
  console.log(`${quad.subject.value} has name ${quad.object.value}`);
}
// Output: http://example.com/alice has name Alice
```

### SPARQL Queries (3 minutes)

```javascript
// SELECT query
const results = store.query(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?person ?name
  WHERE {
    ?person foaf:name ?name .
  }
`);

console.log(results);
// [{ person: 'http://example.com/alice', name: 'Alice' }]

// ASK query (boolean)
const exists = store.query(`
  ASK { <http://example.com/alice> ?p ?o }
`);
console.log(exists); // true

// CONSTRUCT query (build new graph)
const constructed = store.query(`
  CONSTRUCT { ?s ?p ?o }
  WHERE { ?s ?p ?o }
`);
console.log(constructed); // Array of quads
```

## Common Patterns

### Pattern 1: Loading RDF Data from Turtle

```javascript
import { createStore } from '@unrdf/oxigraph';

const store = createStore();

const turtleData = `
  @prefix ex: <http://example.com/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .

  ex:alice foaf:name "Alice" ;
           foaf:age 30 ;
           foaf:knows ex:bob .

  ex:bob foaf:name "Bob" ;
         foaf:age 25 .
`;

// Load Turtle data into store
store.load(turtleData, {
  format: 'text/turtle',
  base_iri: 'http://example.com/',
});

console.log(`Loaded ${store.size()} triples`);
```

### Pattern 2: Querying Relationships

```javascript
// Find all people Alice knows
const aliceFriends = store.query(`
  PREFIX ex: <http://example.com/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?friendName
  WHERE {
    ex:alice foaf:knows ?friend .
    ?friend foaf:name ?friendName .
  }
`);

aliceFriends.forEach(row => {
  console.log(`Alice knows ${row.friendName}`);
});
```

### Pattern 3: Complex SPARQL with FILTER

```javascript
// Find people older than 25
const results = store.query(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?person ?name ?age
  WHERE {
    ?person foaf:name ?name ;
            foaf:age ?age .
    FILTER (?age > 25)
  }
  ORDER BY DESC(?age)
`);

console.log(results);
// [{ person: '...alice', name: 'Alice', age: 30 }]
```

### Pattern 4: Updating Data with SPARQL UPDATE

```javascript
// Update Alice's age
store.update(`
  PREFIX ex: <http://example.com/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  DELETE { ex:alice foaf:age ?oldAge }
  INSERT { ex:alice foaf:age 31 }
  WHERE { ex:alice foaf:age ?oldAge }
`);
```

### Pattern 5: Exporting Data

```javascript
// Export as Turtle
const turtle = store.dump({ format: 'text/turtle' });
console.log(turtle);

// Export as N-Triples
const ntriples = store.dump({ format: 'application/n-triples' });

// Export as JSON-LD
const jsonld = store.dump({ format: 'application/ld+json' });
```

### Pattern 6: Named Graphs

```javascript
import { dataFactory } from '@unrdf/oxigraph';

const { namedNode, literal, quad } = dataFactory;

// Add to specific graph
const graphUri = namedNode('http://example.com/graph1');
const triple = quad(
  namedNode('http://example.com/resource'),
  namedNode('http://example.com/property'),
  literal('value'),
  graphUri // 4th parameter = graph
);

store.add(triple);

// Query specific graph
const results = store.query(`
  SELECT ?s ?p ?o
  WHERE {
    GRAPH <http://example.com/graph1> {
      ?s ?p ?o
    }
  }
`);
```

### Pattern 7: Bulk Operations with UnrdfStore

```javascript
import { createStore, bulkAdd, bulkRemove } from '@unrdf/core';
import { namedNode, literal, quad } from '@unrdf/oxigraph';

// Create UnrdfStore for bulk operations
const store = createStore(); // Returns UnrdfStore with bulk methods

// Add multiple quads at once (much faster than individual adds)
const quads = [
  quad(
    namedNode('http://example.com/alice'),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal('Alice')
  ),
  quad(
    namedNode('http://example.com/bob'),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal('Bob')
  ),
  quad(
    namedNode('http://example.com/alice'),
    namedNode('http://xmlns.com/foaf/0.1/knows'),
    namedNode('http://example.com/bob')
  ),
];

store.bulkAdd(quads);
console.log(`Added ${store.size()} quads`);

// Remove multiple quads by pattern (efficient batch delete)
const toRemove = store.match(
  namedNode('http://example.com/bob'),
  namedNode('http://xmlns.com/foaf/0.1/name'),
  null
);

store.bulkRemove(toRemove);
console.log(`Removed ${store.size()} quads`);
```

### Pattern 8: Graph Diffing

```javascript
import { createStore, diffGraphFromStores } from '@unrdf/core';
import { namedNode, literal, quad } from '@unrdf/oxigraph';

// Create two stores with different data
const storeA = createStore();
const storeB = createStore();

const { namedNode, literal } = { namedNode, literal }; // Destructure for brevity

storeA.bulkAdd([
  quad(
    namedNode('http://example.com/alice'),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal('Alice')
  ),
  quad(
    namedNode('http://example.com/bob'),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal('Bob')
  ),
]);

storeB.bulkAdd([
  quad(
    namedNode('http://example.com/alice'),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal('Alice')
  ),
  quad(
    namedNode('http://example.com/alice'),
    namedNode('http://xmlns.com/foaf/0.1/age'),
    literal('30')
  ),
  quad(
    namedNode('http://example.com/bob'),
    namedNode('http://xmlns.com/foaf/0.1/knows'),
    namedNode('http://example.com/alice')
  ),
]);

// Diff the stores
const diff = diffGraphFromStores(storeA, storeB);
console.log(diff);
// {
//   added: [quad(object)],
//   removed: [quad(predicate)],
//   modified: []
// }

// Use with delta for incremental tracking
const delta = diffGraphFromDelta(storeA, storeB);
console.log(delta);
// { added: [...], removed: [...] }
```

### Pattern 9: Common RDF Prefixes for Normalization

```javascript
import { COMMON_PREFIXES } from '@unrdf/core';

console.log(COMMON_PREFIXES);
// {
//   rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
//   rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
//   owl: 'http://www.w3.org/2002/07/owl#',
//   xsd: 'http://www.w3.org/2001/XMLSchema#',
//   foaf: 'http://xmlns.com/foaf/0.1/',
//   dct: 'http://purl.org/dc/terms/',
//   skos: 'http://www.w3.org/2004/02/skos/core#',
//   ...more
// }

// Use for normalizing IRIs in data processing
const normalized = COMMON_PREFIXES['foaf']; // 'http://xmlns.com/foaf/0.1/'
```

## Error Handling

```javascript
import { createStore } from '@unrdf/oxigraph';

try {
  const store = createStore();

  // Load data
  store.load(turtleData, { format: 'text/turtle' });

  // Execute query
  const results = store.query('SELECT * WHERE { ?s ?p ?o }');

  console.log(`Found ${results.length} results`);
} catch (error) {
  if (error.message.includes('parse')) {
    console.error('Invalid RDF syntax:', error.message);
  } else if (error.message.includes('query')) {
    console.error('Invalid SPARQL query:', error.message);
  } else {
    console.error('Unexpected error:', error);
  }
}
```

## API Reference

### Graph Diff

Compute triple-level and ontology-level diffs between RDF graphs. All functions are pure.

See **[examples/graph-diff.mjs](../../examples/graph-diff.mjs)** for a runnable demonstration.

```javascript
import {
  diffGraphFromStores,
  diffGraphFromDelta,
  diffOntologyFromGraphDiff,
} from '@unrdf/core/diff';

// Diff two store snapshots — requires store.getQuads()
const diff = diffGraphFromStores(beforeStore, afterStore);
// → { added: DiffTriple[], removed: DiffTriple[] }
// DiffTriple: { subject, predicate, object } — string IRI values, no termType wrapper

// Diff from a transaction delta (additions/removals arrays)
const diff2 = diffGraphFromDelta({ additions: [quad], removals: [quad] });

// Classify changes with a lens function
function myLens(triple, direction) {
  if (triple.predicate === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type') {
    return { kind: direction === 'added' ? 'class-added' : 'class-removed', triple };
  }
  return null; // ignore this triple
}
const ontDiff = diffOntologyFromGraphDiff(diff, myLens);
// → { triples: GraphDiff, changes: OntologyChange[] }
// OntologyChange: { kind: string, triple: DiffTriple }
```

**Store contract:** `diffGraphFromStores` calls `store.getQuads()` — any object implementing that method works (no oxigraph dependency).

**Idempotent:** `diffGraphFromStores(store, store)` always returns `{ added: [], removed: [] }`.

### Graph Visualization

Export an RDF store as DOT, Mermaid, ASCII, or HTML — all pure functions, no setup required.

See **[examples/graph-visualization.mjs](../../examples/graph-visualization.mjs)** for a runnable demonstration.

```javascript
import {
  toDOT,
  toMermaid,
  toASCII,
  toHTML,
  extractSubgraph,
} from '@unrdf/core/viz/graph-visualizer';

// DOT format — paste into Graphviz / dot CLI
const dot = toDOT(store, { direction: 'LR', nodeShape: 'box', limit: 50 });
// → 'digraph RDF {\n  rankdir=LR;\n  ...'

// Mermaid — paste into any Mermaid renderer
const mermaid = toMermaid(store, { direction: 'TB' });
// → 'graph TB\n  n0["foaf:Alice"] -->|rdf:type| n1["foaf:Person"]\n ...'

// ASCII — print to terminal for debugging
const ascii = toASCII(store, { limit: 10 });

// HTML — standalone page with D3.js force-directed graph
const html = toHTML(store, { width: 800, height: 600, theme: 'dark' });

// extractSubgraph — filter to a subject's neighbourhood
const aliceStore = extractSubgraph(store, { subject: 'https://example.org/Alice', depth: 2 });
// Returns same interface as the input store (use .match() to iterate quads)
```

**Options (all optional):**

- `limit` — max quads to include (default: unlimited)
- `direction` — graph direction: `'TB'` | `'LR'` | `'BT'` | `'RL'` (default `'TB'`)
- `nodeShape` — DOT node shape: `'box'` | `'ellipse'` | `'circle'` | `'diamond'`
- `theme` — HTML theme: `'light'` | `'dark'` | `'neutral'`
- `width` / `height` — HTML canvas dimensions

**`extractSubgraph(store, options)`:**

- `subject` — filter to quads with this subject IRI
- `predicate` — filter to quads with this predicate IRI
- `depth` — neighbourhood depth (1 = direct relationships only)
- `maxTriples` — max quads in the result

All functions throw `TypeError` if `store` is falsy or lacks a `.match()` method.

### Store Operations

| Method                        | Description            | Example                              |
| ----------------------------- | ---------------------- | ------------------------------------ |
| `createStore(quads?)`         | Create new RDF store   | `const store = createStore()`        |
| `store.add(quad)`             | Add quad to store      | `store.add(quad(s, p, o))`           |
| `store.delete(quad)`          | Remove quad            | `store.delete(quad(s, p, o))`        |
| `store.has(quad)`             | Check if quad exists   | `store.has(quad(s, p, o))`           |
| `store.match(s?, p?, o?, g?)` | Query quads by pattern | `store.match(null, predicate, null)` |
| `store.size()`                | Get number of quads    | `console.log(store.size())`          |
| `store.clear()`               | Remove all quads       | `store.clear()`                      |

### SPARQL Queries

| Method                 | Description           | Returns                                                                |
| ---------------------- | --------------------- | ---------------------------------------------------------------------- |
| `store.query(sparql)`  | Execute SPARQL query  | SELECT: Array of bindings<br>ASK: Boolean<br>CONSTRUCT: Array of quads |
| `store.update(sparql)` | Execute SPARQL UPDATE | `void`                                                                 |

**Query Types:**

- **SELECT**: Returns array of variable bindings
- **ASK**: Returns boolean (true/false)
- **CONSTRUCT**: Returns array of quads
- **DESCRIBE**: Returns array of quads

### Data Import/Export

| Method                      | Description     | Formats                                            |
| --------------------------- | --------------- | -------------------------------------------------- |
| `store.load(data, options)` | Load RDF data   | Turtle, TriG, N-Triples, N-Quads, RDF/XML, JSON-LD |
| `store.dump(options)`       | Export RDF data | Turtle, TriG, N-Triples, N-Quads, RDF/XML, JSON-LD |

**Load Options:**

```javascript
{
  format: 'text/turtle',           // Required: RDF format
  base_iri: 'http://example.com/', // Optional: base IRI
  to_graph: graphUri               // Optional: target graph
}
```

**Dump Options:**

```javascript
{
  format: 'text/turtle',      // Required: output format
  from_graph: graphUri        // Optional: source graph
}
```

### RDF Term Factory

```javascript
import { dataFactory } from '@unrdf/oxigraph';

const { namedNode, literal, blankNode, quad, defaultGraph } = dataFactory;

// Create terms
const subject = namedNode('http://example.com/alice');
const predicate = namedNode('http://xmlns.com/foaf/0.1/name');
const object = literal('Alice', 'en');
const blank = blankNode();
const graph = defaultGraph();

// Create quad
const triple = quad(subject, predicate, object);
const quadWithGraph = quad(subject, predicate, object, graph);
```

## Dependencies

- @comunica/query-sparql
- n3
- rdf-canonize
- @rdfjs/\*

## Architecture

The `@unrdf/core` package is designed as a modular, high-performance RDF substrate. It provides the foundational types, store implementations, and query executors used across the UNRDF ecosystem.

```mermaid
graph TD
    A[Application/Extensions] --> B[@unrdf/core]
    subgraph "@unrdf/core"
        B --> C[UnrdfStore]
        B --> D[SPARQL Executor]
        B --> E[Canonicalizer]
        B --> F[Serializers/Parsers]
        C --> G[In-Memory Quad Index]
        D --> H[Sync/Async Engines]
        E --> I[N-Quads Deterministic]
        F --> J[Turtle/JSON-LD/N-Triples]
    end
    C --- D
    E --- F
```

### Core Components:

1.  **UnrdfStore**: A high-performance, synchronous-first RDF store optimized for in-memory operations. It implements the RDF/JS Store interface while providing bulk operation extensions.
2.  **SPARQL Executor**: A flexible query engine that supports both synchronous execution (for local stores) and asynchronous execution (for remote or complex sources).
3.  **Canonicalizer**: Implements deterministic RDF canonicalization, essential for signing, hashing, and graph comparison.
4.  **Serialization Layer**: Pluggable parsers and serializers for common RDF formats (Turtle, JSON-LD, N-Triples, N-Quads).

## Troubleshooting

### Common Issues

#### "Cannot parse Turtle syntax"

**Symptom**: `Parser error: Unexpected token`
**Solution**: Ensure your Turtle syntax is valid. Check that all prefixes are defined, IRIs are enclosed in `<>`, and literals are properly quoted.

#### "SPARQL query returns empty results"

**Symptom**: `results.length === 0` even when data exists.
**Solution**:

1. Verify prefix definitions match the data exactly.
2. Check for case sensitivity in IRIs and literals.
3. Use a simple `SELECT * WHERE { ?s ?p ?o }` to verify data presence.

#### "Performance is slower than expected"

**Symptom**: Query execution takes >100ms for small datasets.
**Solution**:

1. Use the **Synchronous API** (`UnrdfStore` + `executeQuerySync`) for in-memory operations to avoid microtask overhead.
2. Ensure you are using `bulkAdd()` for large datasets instead of individual `add()` calls.

## SLA Requirements

To ensure the reliability and performance of the UNRDF ecosystem, `@unrdf/core` adheres to the following Service Level Agreement (SLA) targets for in-memory operations (tested on modern hardware, ~1000 quads):

| Metric                      | Target     | Description                                  |
| --------------------------- | ---------- | -------------------------------------------- |
| **Availability**            | 99.99%     | Core logic is stateless and side-effect free |
| **Store Initialization**    | < 5ms      | Time to create a new `UnrdfStore` instance   |
| **Triple Insertion**        | < 1ms      | Latency for single `add()` operation         |
| **Query Latency (Simple)**  | < 10ms     | SELECT query with 1-2 triple patterns        |
| **Query Latency (Complex)** | < 50ms     | SELECT query with JOINs and FILTERs          |
| **Canonicalization**        | < 100ms    | Deterministic hashing of up to 5000 quads    |
| **Memory Overhead**         | < 2KB/quad | Heap usage per RDF quad stored               |

## Error Handling

`@unrdf/core` provides a typed error hierarchy and recovery utilities.

### Error Classes (`packages/core/src/errors.mjs`)

All error classes extend `UnrdfError` which extends `Error`. Each has `.code`, `.context`, `.timestamp`, and `.docsUrl`.

```javascript
import {
  UnrdfError,
  ValidationError,
  QueryError,
  StoreError,
  NetworkError,
  TimeoutError,
  ParserError,
} from '@unrdf/core';

try {
  // ...operation...
} catch (e) {
  if (e instanceof ValidationError) {
    /* bad input */
  }
  if (e instanceof NetworkError) {
    /* transport error */
  }
}

const err = new ValidationError('invalid quad', { quad: badQuad });
// err.name === 'ValidationError'
// err.code === 'ERR_UNRDF'  (default)
// err.context === { quad: badQuad }
```

### Recovery Utilities (`packages/core/src/recovery.mjs`)

```javascript
import { retry, CircuitBreaker, RateLimiter, withRecovery } from '@unrdf/core';

// retry — exponential backoff; only retries NetworkError and TimeoutError by default
const result = await retry(async () => fetchRDF(url), {
  maxAttempts: 3,
  initialDelay: 100,
  backoffMultiplier: 2,
});

// CircuitBreaker — opens after N failures; half-opens after timeout
const breaker = new CircuitBreaker({ failureThreshold: 5, timeout: 60000 });
const data = await breaker.execute(async () => queryRemoteStore());
// breaker.getState() → {state: 'closed'|'open'|'half-open', failures, successes}

// RateLimiter — caps operations per time window
const limiter = new RateLimiter({ maxOperations: 10, windowMs: 1000 });
await limiter.execute(async () => insertQuad(quad));
// limiter.getStatus() → {current, max, windowMs, available}
// limiter.reset()

// withRecovery — composable: retry + circuit breaker + fallback
const robustQuery = withRecovery(async sparql => store.query(sparql), {
  retry: { maxAttempts: 3, initialDelay: 50 },
  fallback: async () => [], // return empty on total failure
});
```

**Note:** `retry` and `withRecovery` use `defaultShouldRetry` which only retries `NetworkError` and `TimeoutError`. Pass a custom `shouldRetry` function to retry on other error types.

**Example**: [`examples/error-handling.mjs`](../../examples/error-handling.mjs)

## RDF-star

`@unrdf/core` implements W3C RDF 1.2 quoted triples and annotation helpers for provenance, temporal validity, and confidence.

### RDFStarFactory

```javascript
import { RDFStarFactory, isQuotedTriple } from '@unrdf/core';

const factory = new RDFStarFactory();
const alice = factory.namedNode('https://example.org/Alice');
const knows = factory.namedNode('http://xmlns.com/foaf/0.1/knows');
const bob = factory.namedNode('https://example.org/Bob');

// Create a quoted triple: <<Alice foaf:knows Bob>>
const quoted = factory.quotedTriple(alice, knows, bob);
// quoted.termType === 'QuotedTriple'
// quoted.value === '<<https://example.org/Alice http://xmlns.com/foaf/0.1/knows https://example.org/Bob>>'

isQuotedTriple(quoted); // → true
isQuotedTriple({ termType: 'NamedNode', value: 'x' }); // → false
```

**Note:** Oxigraph's current JS bindings don't support quoted triples as subjects/objects. `quotedTriple()` returns a wrapper object with `.triple`, `.identifier`, `.termType`, and `.value`.

### Annotation Helpers (`packages/core/src/annotation.mjs`)

```javascript
import {
  AnnotationBuilder,
  createAnnotationBuilder,
  createProvenance,
  createTemporal,
  createConfidence,
} from '@unrdf/core';

// Fluent builder — available methods: source, creator, confidence, validFrom, validTo, created, modified
const annotation = new AnnotationBuilder()
  .source('https://example.org/dataset')
  .creator('Alice')
  .confidence(0.95, 'ml-classifier')
  .validFrom('2026-01-01T00:00:00Z')
  .validTo('2027-01-01T00:00:00Z')
  .build();
// → { provenance: {source, creator}, temporal: {validFrom, validTo}, confidence: {confidence, method}, ... }

// Standalone helpers — schema-validated, throw ZodError on invalid input
const prov = createProvenance({ source: 'https://...', creator: 'Alice', license: 'https://...' });
const temp = createTemporal({ validFrom: '2026-01-01T00:00:00Z', validTo: '2027-01-01T00:00:00Z' });
const conf = createConfidence(0.95, 'ml-model');
// createConfidence(1.5, 'x') throws — scores must be in [0, 1]
```

**Example**: [`examples/rdf-star.mjs`](../../examples/rdf-star.mjs)

## When to Use @unrdf/core

✅ Always needed for:

- Basic RDF graph operations
- SPARQL query execution
- Building RDF applications
- Any UNRDF-based project

## When to Add Extensions

After @unrdf/core, you can add:

- `@unrdf/hooks` - Policy enforcement and validation
- `@unrdf/federation` - Peer discovery and queries
- `@unrdf/streaming` - Change feeds and subscriptions
- `@unrdf/browser` - Browser SDK with IndexedDB
- `@unrdf/knowledge-engine` - Rule engine (optional)
- `@unrdf/dark-matter` - Query optimization (optional)

## Documentation

See [UNRDF Documentation](https://github.com/unrdf/unrdf) for full API reference and guides.

## License

MIT
