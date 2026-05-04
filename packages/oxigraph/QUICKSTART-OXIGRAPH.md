# @unrdf/oxigraph - Quick Start Guide

**80/20 Guide**: Get high-performance SPARQL benchmarking running in 5 minutes.

## Prerequisites

- Node.js 18+
- pnpm
- Terminal

## One-Command Demo (5-Minute Benchmark)

Experience the performance of Oxigraph in under 5 minutes. This demo initializes the WASM engine, generates a synthetic dataset, and executes a full battery of SPARQL benchmarks.

```bash
# Run from the package root
node examples/production-benchmark.mjs
```

**What happens in these 5 minutes:**
1. **WASM Initialization** (~1s): The Rust-based engine is loaded into the Node.js runtime.
2. **Dataset Generation** (~1s): 200+ RDF triples are generated in memory.
3. **Execution Phase** (~2s):
    - ✅ **Add Operations**: Tests raw insertion throughput.
    - ✅ **SELECT Queries**: Executes complex pattern matching.
    - ✅ **ASK Queries**: Tests existence check performance.
    - ✅ **CONSTRUCT Queries**: Measures graph construction speed.
    - ✅ **Pattern Matching**: Directly queries the triple store index.
4. **Verification**: Ensures all results are logically correct and performance is within production tolerances.

## Quick Start

### 1. Install

```bash
pnpm add @unrdf/oxigraph
```

### 2. Basic Usage

```javascript
import { createStore, dataFactory } from '@unrdf/oxigraph'

// Create store
const store = createStore()

// Add triple
const ex = dataFactory.namedNode('http://example.com/')
const name = dataFactory.namedNode('http://schema.org/name')
const value = dataFactory.literal('Example')

store.add(dataFactory.triple(ex, name, value))

// Query
const results = store.query('SELECT ?s ?p ?o WHERE { ?s ?p ?o }')
console.log(results)
```

### 3. Load RDF Data

```javascript
const turtle = `
  @prefix ex: <http://example.com/> .
  @prefix schema: <http://schema.org/> .

  ex:person1 schema:name "Person 1" ;
             schema:age 30 .
`

store.load(turtle, {
  format: 'text/turtle',
  base_iri: 'http://example.com/'
})
```

### 4. SPARQL Queries

```javascript
// SELECT
const selectResults = store.query(
  'SELECT ?name WHERE { ?s <http://schema.org/name> ?name }'
)

// ASK
const exists = store.query(
  'ASK { ?s <http://schema.org/name> ?name }'
)

// CONSTRUCT
const constructed = store.query(
  'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }'
)
```

### 5. Export Data

```javascript
// Export as Turtle
const turtle = store.dump({ format: 'text/turtle' })

// Export as N-Quads
const nquads = store.dump({ format: 'application/n-quads' })
```

## Benchmarking

```bash
pnpm test:bench
```

**Benchmark Suite**:
- Add Operations (throughput)
- SELECT Queries (performance)
- ASK Queries (existence checks)
- CONSTRUCT Queries (graph construction)
- Pattern Matching (triple patterns)
- Delete Operations (removal)
- Bulk Load (import speed)
- Dump Operations (export speed)

## Performance Characteristics

| Operation | Performance | Notes |
|-----------|-------------|-------|
| **Add Triple** | ~1ms | Single insertion |
| **SELECT Query** | <10ms | Simple patterns |
| **CONSTRUCT** | <20ms | 100 triples |
| **Bulk Load** | ~50ms | 1000 triples |
| **Pattern Match** | <5ms | Subject/predicate/object |

## Supported Formats

- Turtle (`text/turtle`)
- TriG (`application/trig`)
- N-Triples (`application/n-triples`)
- N-Quads (`application/n-quads`)
- JSON-LD (`application/ld+json`)
- RDF/XML (`application/rdf+xml`)

## Troubleshooting

### "WASM initialization failed"

**Solution**: Ensure Node.js 18+ is installed

### "Query timeout"

**Solution**: Simplify query or increase dataset indexing

### "Format not supported"

**Solution**: Check format string matches supported list above

## Support

- Issues: https://github.com/unrdf/unrdf/issues
- Oxigraph Docs: https://oxigraph.org/
