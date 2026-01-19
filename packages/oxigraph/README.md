# @unrdf/oxigraph

![Version](https://img.shields.io/badge/version-6.0.0--rc.3-blue) ![Production Ready](https://img.shields.io/badge/production-ready-green)


Oxigraph-backed RDF store implementation for UNRDF. This package provides a benchmarking implementation using the [Oxigraph SPARQL engine](https://oxigraph.org/) compiled to WebAssembly for JavaScript.

## Features

- **SPARQL 1.1 Query Support**: Full SELECT, CONSTRUCT, DESCRIBE, and ASK query support
- **SPARQL 1.1 Update Support**: INSERT, DELETE, and other update operations
- **Multiple RDF Formats**: Load and dump Turtle, TriG, N-Triples, N-Quads, RDF/XML, JSON-LD
- **High Performance**: Rust-based implementation compiled to WASM
- **Compatible API**: Drop-in alternative to the current UNRDF engine
- **Comprehensive Benchmarks**: Built-in performance benchmarking suite

## Installation

```bash
pnpm install @unrdf/oxigraph
```

## Usage

### Basic Store Operations

```javascript
import { createStore, dataFactory } from '@unrdf/oxigraph';

// Create a new store
const store = createStore();

// Create RDF terms
const ex = dataFactory.namedNode('http://example.com/');
const name = dataFactory.namedNode('http://schema.org/name');
const value = dataFactory.literal('Example');

// Add a triple
const triple = dataFactory.triple(ex, name, value);
store.add(triple);

// Check if triple exists
console.log(store.has(triple)); // true

// Query the store
const results = store.query('SELECT ?s ?p ?o WHERE { ?s ?p ?o }');
console.log(results);
```

### SPARQL Queries

```javascript
// SELECT query
const selectResults = store.query(
  'SELECT ?name WHERE { ?s <http://schema.org/name> ?name }'
);

// ASK query
const exists = store.query(
  'ASK { ?s <http://schema.org/name> ?name }'
);

// CONSTRUCT query
const constructed = store.query(
  'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o FILTER (?p = <http://schema.org/name>) }'
);
```

### Loading RDF Data

```javascript
const turtle = `
  @prefix ex: <http://example.com/> .
  @prefix schema: <http://schema.org/> .

  ex:person1 schema:name "Person 1" ;
             schema:age 30 .
`;

store.load(turtle, {
  format: 'text/turtle',
  base_iri: 'http://example.com/',
});
```

### Exporting RDF Data

```javascript
// Export as Turtle
const turtle = store.dump({
  format: 'text/turtle',
});

// Export as N-Quads
const nquads = store.dump({
  format: 'application/n-quads',
});
```

## API Reference

### `createStore(quads?)`

Creates a new Oxigraph store.

**Parameters:**
- `quads` (Array, optional): Initial quads to populate the store

**Returns:** `OxigraphStore` instance

### `OxigraphStore`

#### `add(quad)`

Adds a quad to the store.

#### `delete(quad)`

Removes a quad from the store.

#### `has(quad)`

Checks if a quad exists in the store.

#### `match(subject?, predicate?, object?, graph?)`

Returns all quads matching the given pattern.

#### `query(sparqlQuery, options?)`

Executes a SPARQL query. Returns results based on query type:
- SELECT: Array of bindings
- ASK: Boolean
- CONSTRUCT/DESCRIBE: Array of quads

#### `update(sparqlUpdateQuery, options?)`

Executes a SPARQL UPDATE query.

#### `load(data, options)`

Loads RDF data into the store.

**Options:**
- `format` (required): RDF format ('text/turtle', 'application/n-quads', etc.)
- `baseIri`: Base IRI for relative IRIs
- `toNamedGraph`: Graph to load into

#### `dump(options)`

Exports store contents in specified format.

**Options:**
- `format` (required): RDF format
- `fromNamedGraph`: Graph to export from

#### `size()`

Returns the number of quads in the store.

#### `clear()`

Removes all quads from the store.

## Benchmarking

Run the benchmark suite to compare performance:

```bash
pnpm test:bench
```

The benchmark suite includes:

- **Add Operations**: Triple insertion throughput
- **SELECT Queries**: Query execution performance
- **ASK Queries**: Existence check performance
- **CONSTRUCT Queries**: Graph construction performance
- **Pattern Matching**: Triple pattern matching throughput
- **Delete Operations**: Triple deletion throughput
- **Bulk Load**: RDF data loading performance
- **Dump Operations**: RDF data export performance

## Supported RDF Formats

- **Turtle** (`text/turtle`, `ttl`)
- **TriG** (`application/trig`, `trig`)
- **N-Triples** (`application/n-triples`, `nt`)
- **N-Quads** (`application/n-quads`, `nq`)
- **JSON-LD** (`application/ld+json`, `jsonld`)
- **RDF/XML** (`application/rdf+xml`, `rdf`)

## Performance Notes

- Oxigraph is highly optimized for SPARQL query execution
- The WASM implementation provides near-native performance
- Bulk operations benefit from Oxigraph's efficient indexing
- Memory usage is comparable to native Rust implementation

## References

- [Oxigraph Documentation](https://oxigraph.org/)
- [SPARQL 1.1 Specification](https://www.w3.org/TR/sparql11-overview/)
- [RDF 1.1 Specification](https://www.w3.org/TR/rdf11-concepts/)

## License

MIT
