# @unrdf/core

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

**New to UNRDF?** Start with the [Quick Start Guide](../../examples/QUICKSTART.md).

## Quick Start

```javascript
import { createStore, executeQuery } from '@unrdf/core'

// Create an in-memory RDF store
const store = createStore()

// Add RDF data
const { namedNode, literal } = createTerms()
store.addQuad(
  namedNode('http://example.com/alice'),
  namedNode('http://xmlns.com/foaf/0.1/name'),
  literal('Alice')
)

// Query with SPARQL
const results = await executeQuery(
  store,
  'SELECT ?name WHERE { ?s foaf:name ?name }'
)
```

## API

### Store Operations
- `createStore()` - Create an in-memory RDF store
- `store.addQuad(quad)` - Add a quad to the store
- `store.removeQuad(quad)` - Remove a quad
- `store.getQuads(subject, predicate, object)` - Query quads
- `store.iterateQuads()` - Iterate all quads

### SPARQL
- `executeQuery(store, sparqlQuery)` - Execute SPARQL query
- `prepareQuery(sparqlQuery)` - Parse SPARQL query

### Types & Terms
- `createTerms()` - Create RDF terms (namedNode, literal, etc.)
- `canonicalize(quads)` - Deterministic RDF canonicalization

## Dependencies

- @comunica/query-sparql
- n3
- rdf-canonize
- @rdfjs/*

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
