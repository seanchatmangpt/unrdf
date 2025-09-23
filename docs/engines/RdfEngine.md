# RdfEngine

The `RdfEngine` is the core RDF engine that powers all unrdf operations. It enforces unrdf's "One Path" philosophy by using a single, opinionated implementation for each RDF operation.

## Overview

`RdfEngine` encapsulates all external RDF library interactions and provides a unified interface for RDF operations. It uses:

- **N3.js** for store management and parsing/serialization
- **@comunica/query-sparql** for SPARQL operations
- **rdf-validate-shacl** for SHACL validation
- **eyereasoner** for N3 reasoning
- **rdf-canonize** for URDNA2015 canonicalization
- **jsonld** for JSON-LD processing
- **@zazuko/env (Clownface)** for graph traversal

## Constructor

```javascript
import { RdfEngine } from 'unrdf';

const engine = new RdfEngine(options);
```

### Options

- `baseIRI` - Base IRI for parsing operations (default: 'http://example.org/')
- `deterministic` - Enable deterministic operations (default: true)
- `timeoutMs` - Default timeout for operations (default: 30000)
- `onMetric` - Metrics callback function
- `logger` - Logger instance (default: console)

### Example

```javascript
const engine = new RdfEngine({
  baseIRI: 'https://example.org/',
  deterministic: true,
  timeoutMs: 60000,
  onMetric: (metric) => console.log(metric),
  logger: customLogger
});
```

## Store Management

### `createStore(quads)`
Create a new N3.Store instance.

```javascript
const store = engine.createStore();
const storeWithQuads = engine.createStore([quad1, quad2]);
```

### `namedNode(value)`
Create a named node.

```javascript
const subject = engine.namedNode('http://example.org/alice');
```

### `literal(value, languageOrDatatype)`
Create a literal.

```javascript
const name = engine.literal('Alice');
const age = engine.literal('30', 'http://www.w3.org/2001/XMLSchema#integer');
const englishName = engine.literal('Alice', null, 'en');
```

### `blankNode(value)`
Create a blank node.

```javascript
const bnode = engine.blankNode('b1');
```

### `quad(s, p, o, g)`
Create a quad.

```javascript
const quad = engine.quad(
  engine.namedNode('http://example.org/alice'),
  engine.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
  engine.namedNode('http://xmlns.com/foaf/0.1/Person')
);
```

## Parsing and Serialization

### `parseTurtle(ttl, options)`
Parse Turtle string into N3.Store.

```javascript
const store = engine.parseTurtle(`
  @prefix ex: <http://example.org/> .
  ex:alice a ex:Person .
`, { baseIRI: 'https://example.org/' });
```

### `parseNQuads(nq)`
Parse N-Quads string into N3.Store.

```javascript
const store = engine.parseNQuads(`
  <http://example.org/alice> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .
`);
```

### `serializeTurtle(store, options)`
Serialize store to Turtle format.

```javascript
const turtle = await engine.serializeTurtle(store, {
  prefixes: { ex: 'http://example.org/' }
});
```

### `serializeNQuads(store)`
Serialize store to N-Quads format.

```javascript
const nquads = await engine.serializeNQuads(store);
```

## SPARQL Operations

### `query(store, sparql, opts)`
Execute SPARQL query with streaming, paging, and timeout.

```javascript
const results = await engine.query(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?person ?name WHERE {
    ?person a foaf:Person ;
            foaf:name ?name .
  }
`, { limit: 1000 });
```

**Query Types:**
- **SELECT**: Returns bindings
- **ASK**: Returns boolean
- **CONSTRUCT**: Returns quads
- **DESCRIBE**: Returns quads
- **UPDATE**: Modifies store

**Options:**
- `limit` - Maximum number of results
- `signal` - Abort signal
- `deterministic` - Enable deterministic results

## Validation

### `validateShacl(dataStore, shapesInput)`
Validate store against SHACL shapes.

```javascript
const report = await engine.validateShacl(dataStore, shapesStore);

if (report.conforms) {
  console.log('Validation passed!');
} else {
  console.log('Validation failed:', report.results);
}
```

### `validateShaclOrThrow(dataStore, shapesInput)`
Validate and throw on failure.

```javascript
try {
  const report = await engine.validateShaclOrThrow(dataStore, shapesStore);
} catch (error) {
  console.error('Validation failed:', error.message);
}
```

## Reasoning

### `reason(dataStore, rulesStore)`
Apply N3 reasoning rules.

```javascript
const inferred = await engine.reason(dataStore, rulesStore);
console.log(`Inferred ${inferred.size} new triples`);
```

**Rules Format:**
```javascript
const rules = `
  @prefix ex: <http://example.org/> .
  
  { ?person ex:parent ?parent } => { ?person ex:ancestor ?parent } .
  { ?person ex:ancestor ?ancestor . ?ancestor ex:ancestor ?grandparent } => { ?person ex:ancestor ?grandparent } .
`;
```

## Canonicalization

### `canonicalize(store)`
Canonicalize store using URDNA2015.

```javascript
const canonical = await engine.canonicalize(store);
console.log('Canonical N-Quads:', canonical);
```

### `isIsomorphic(a, b)`
Check if two stores are isomorphic.

```javascript
const isIsomorphic = await engine.isIsomorphic(store1, store2);
console.log('Stores are isomorphic:', isIsomorphic);
```

## JSON-LD Operations

### `toJSONLD(store, opts)`
Convert store to JSON-LD.

```javascript
const jsonld = await engine.toJSONLD(store, {
  context: { ex: 'http://example.org/' },
  frame: { '@type': 'ex:Person' }
});
```

### `fromJSONLD(jsonldDoc)`
Convert JSON-LD to store.

```javascript
const store = await engine.fromJSONLD({
  '@context': { ex: 'http://example.org/' },
  '@id': 'ex:alice',
  '@type': 'ex:Person',
  'ex:name': 'Alice'
});
```

## Set Operations

### `union(...stores)`
Create union of multiple stores.

```javascript
const unioned = engine.union(store1, store2, store3);
```

### `difference(a, b)`
Create difference between stores.

```javascript
const diff = engine.difference(store1, store2);
```

### `intersection(a, b)`
Create intersection of stores.

```javascript
const intersection = engine.intersection(store1, store2);
```

## Utilities

### `skolemize(store, baseIRI)`
Skolemize blank nodes.

```javascript
const skolemized = engine.skolemize(store, 'http://example.org/.well-known/genid/');
```

### `getStats(store)`
Get store statistics.

```javascript
const stats = engine.getStats(store);
console.log(stats);
// {
//   quads: 10,
//   subjects: 5,
//   predicates: 3,
//   objects: 8,
//   graphs: 1
// }
```

### `getClownface(store)`
Get Clownface pointer for graph traversal.

```javascript
const cf = engine.getClownface(store);
const person = cf.namedNode('http://example.org/alice');
const types = person.out(engine.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'));
```

## Examples

### Basic Usage

```javascript
import { RdfEngine } from 'unrdf';

const engine = new RdfEngine();

// Create store and add data
const store = engine.createStore();
store.add(
  engine.quad(
    engine.namedNode('http://example.org/alice'),
    engine.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    engine.namedNode('http://xmlns.com/foaf/0.1/Person')
  )
);

// Query data
const results = await engine.query(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?person WHERE {
    ?person a foaf:Person .
  }
`);

console.log('Results:', results.results);
```

### Turtle Processing

```javascript
import { RdfEngine } from 'unrdf';

const engine = new RdfEngine();

// Parse Turtle
const store = engine.parseTurtle(`
  @prefix ex: <http://example.org/> .
  ex:alice a ex:Person .
  ex:alice ex:name "Alice" .
`);

// Serialize to Turtle
const turtle = await engine.serializeTurtle(store);
console.log(turtle);
```

### SHACL Validation

```javascript
import { RdfEngine } from 'unrdf';

const engine = new RdfEngine();

// Create data store
const dataStore = engine.createStore();
dataStore.add(
  engine.quad(
    engine.namedNode('http://example.org/alice'),
    engine.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    engine.namedNode('http://example.org/Person')
  )
);

// Create shapes store
const shapesStore = engine.parseTurtle(`
  @prefix sh: <http://www.w3.org/ns/shacl#> .
  @prefix ex: <http://example.org/> .
  
  ex:PersonShape a sh:NodeShape ;
    sh:targetClass ex:Person ;
    sh:property [
      sh:path ex:name ;
      sh:datatype xsd:string ;
      sh:minCount 1 ;
    ] .
`);

// Validate
const report = await engine.validateShacl(dataStore, shapesStore);

if (report.conforms) {
  console.log('Validation passed!');
} else {
  console.log('Validation failed:', report.results);
}
```

### N3 Reasoning

```javascript
import { RdfEngine } from 'unrdf';

const engine = new RdfEngine();

// Create data store
const dataStore = engine.createStore();
dataStore.add(
  engine.quad(
    engine.namedNode('http://example.org/alice'),
    engine.namedNode('http://example.org/parent'),
    engine.namedNode('http://example.org/bob')
  )
);

// Create rules store
const rulesStore = engine.parseTurtle(`
  @prefix ex: <http://example.org/> .
  
  { ?person ex:parent ?parent } => { ?person ex:ancestor ?parent } .
  { ?person ex:ancestor ?ancestor . ?ancestor ex:ancestor ?grandparent } => { ?person ex:ancestor ?grandparent } .
`);

// Apply reasoning
const inferred = await engine.reason(dataStore, rulesStore);
console.log(`Inferred ${inferred.size} new triples`);
```

### Canonicalization

```javascript
import { RdfEngine } from 'unrdf';

const engine = new RdfEngine();

// Create stores
const store1 = engine.createStore();
const store2 = engine.createStore();

// Add same data in different order
store1.add(quad1, quad2);
store2.add(quad2, quad1);

// Check if isomorphic
const isIsomorphic = await engine.isIsomorphic(store1, store2);
console.log('Stores are isomorphic:', isIsomorphic);

// Canonicalize
const canonical1 = await engine.canonicalize(store1);
const canonical2 = await engine.canonicalize(store2);
console.log('Canonical forms are equal:', canonical1 === canonical2);
```

## Performance Considerations

### Timeout Management

```javascript
// Set appropriate timeouts
const engine = new RdfEngine({ timeoutMs: 60000 });

// Or per-operation
const results = await engine.query(store, sparql, { timeoutMs: 30000 });
```

### Memory Management

```javascript
// Clear stores when done
const store = engine.createStore();
// ... use store
store.clear();
```

### Deterministic Operations

```javascript
// Enable deterministic mode for consistent results
const engine = new RdfEngine({ deterministic: true });
```

### Streaming

```javascript
// Use streaming for large datasets
const results = await engine.query(store, sparql, { limit: 1000 });
```

## Error Handling

### Common Error Types

- **Parse Errors**: Invalid input format
- **Query Errors**: Invalid SPARQL syntax
- **Validation Errors**: SHACL validation failures
- **Reasoning Errors**: N3 reasoning failures
- **Timeout Errors**: Operation timeouts
- **Canonicalization Errors**: URDNA2015 failures

### Error Recovery

```javascript
try {
  const result = await engine.operation();
} catch (error) {
  if (error.message.includes('timeout')) {
    // Retry with longer timeout
    const retry = new RdfEngine({ timeoutMs: 60000 });
    return await retry.operation();
  }
  throw error;
}
```

## Best Practices

### 1. Use Engine Consistently

```javascript
// Good: Use engine for all operations
const engine = new RdfEngine();
const store = engine.createStore();
const results = await engine.query(store, sparql);

// Avoid: Mixing direct library usage
const store = new Store();
const engine = new QueryEngine();
```

### 2. Configure Appropriately

```javascript
// Set appropriate timeouts
const engine = new RdfEngine({ timeoutMs: 30000 });

// Enable deterministic operations
const engine = new RdfEngine({ deterministic: true });
```

### 3. Handle Errors Gracefully

```javascript
try {
  const result = await engine.operation();
} catch (error) {
  console.error('Operation failed:', error.message);
  // Provide fallback or rethrow
}
```

### 4. Monitor Performance

```javascript
const engine = new RdfEngine({
  onMetric: (metric) => {
    console.log(`${metric.event}: ${metric.durMs}ms`);
  }
});
```

## Troubleshooting

### Common Issues

#### Engine Creation
```javascript
// Error: Cannot create engine
// Solution: Check dependencies
pnpm install unrdf
```

#### Operation Failures
```javascript
// Error: Operation failed
// Solution: Check error message and retry
try {
  const result = await engine.operation();
} catch (error) {
  console.error('Operation failed:', error.message);
}
```

#### Performance Issues
```javascript
// For large datasets, use streaming
const results = await engine.query(store, sparql, { limit: 1000 });
```

### Getting Help

1. Check the [API Reference](../api-reference.md)
2. Look at [Examples](../examples/)
3. Review [Core Concepts](../core-concepts.md)
4. Check the [GitHub Issues](https://github.com/gitvan/unrdf/issues)
