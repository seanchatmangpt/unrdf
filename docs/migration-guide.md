# Migration Guide

This guide helps you migrate from other RDF libraries to unrdf.

## From rdflib.js

### Basic Store Operations

**Before (rdflib.js):**
```javascript
import $rdf from 'rdflib';

const store = $rdf.graph();
const subject = $rdf.namedNode('http://example.org/alice');
const predicate = $rdf.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
const object = $rdf.namedNode('http://xmlns.com/foaf/0.1/Person');

store.add(subject, predicate, object);
```

**After (unrdf):**
```javascript
import { useStore } from 'unrdf';

const store = useStore();
store.add(
  store.quad(
    store.namedNode('http://example.org/alice'),
    store.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    store.namedNode('http://xmlns.com/foaf/0.1/Person')
  )
);
```

### SPARQL Queries

**Before (rdflib.js):**
```javascript
import $rdf from 'rdflib';

const query = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?person WHERE {
    ?person a foaf:Person .
  }
`;

$rdf.sparqlToJson(store, query, (err, results) => {
  if (err) throw err;
  console.log(results);
});
```

**After (unrdf):**
```javascript
import { useGraph } from 'unrdf';

const graph = useGraph(store);
const results = await graph.select(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?person WHERE {
    ?person a foaf:Person .
  }
`);
console.log(results.results);
```

### Turtle Parsing

**Before (rdflib.js):**
```javascript
import $rdf from 'rdflib';

const turtle = `
  @prefix ex: <http://example.org/> .
  ex:alice a ex:Person .
`;

$rdf.parse(turtle, store, 'http://example.org/', 'text/turtle');
```

**After (unrdf):**
```javascript
import { useTurtle } from 'unrdf';

const turtle = await useTurtle();
const store = await turtle.parse(`
  @prefix ex: <http://example.org/> .
  ex:alice a ex:Person .
`);
```

## From N3.js

### Store Creation

**Before (N3.js):**
```javascript
import { Store, DataFactory } from 'n3';

const store = new Store();
const { namedNode, literal, quad } = DataFactory;

store.add(quad(
  namedNode('http://example.org/alice'),
  namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
  namedNode('http://xmlns.com/foaf/0.1/Person')
));
```

**After (unrdf):**
```javascript
import { useStore } from 'unrdf';

const store = useStore();
store.add(
  store.quad(
    store.namedNode('http://example.org/alice'),
    store.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    store.namedNode('http://xmlns.com/foaf/0.1/Person')
  )
);
```

### Turtle Parsing

**Before (N3.js):**
```javascript
import { Parser, Store } from 'n3';

const parser = new Parser();
const store = new Store(parser.parse(turtle));
```

**After (unrdf):**
```javascript
import { useTurtle } from 'unrdf';

const turtle = await useTurtle();
const store = await turtle.parse(turtle);
```

### SPARQL Queries

**Before (N3.js):**
```javascript
import { QueryEngine } from '@comunica/query-sparql';

const engine = new QueryEngine();
const bindings = await engine.queryBindings(sparql, { sources: [store] });
```

**After (unrdf):**
```javascript
import { useGraph } from 'unrdf';

const graph = useGraph(store);
const results = await graph.select(sparql);
```

## From Comunica

### Query Execution

**Before (Comunica):**
```javascript
import { QueryEngine } from '@comunica/query-sparql';

const engine = new QueryEngine();
const bindings = await engine.queryBindings(sparql, { sources: [store] });
const results = [];
for await (const binding of bindings) {
  results.push(binding);
}
```

**After (unrdf):**
```javascript
import { useGraph } from 'unrdf';

const graph = useGraph(store);
const results = await graph.select(sparql);
```

### ASK Queries

**Before (Comunica):**
```javascript
import { QueryEngine } from '@comunica/query-sparql';

const engine = new QueryEngine();
const boolean = await engine.queryBoolean(sparql, { sources: [store] });
```

**After (unrdf):**
```javascript
import { useGraph } from 'unrdf';

const graph = useGraph(store);
const boolean = await graph.ask(sparql);
```

## From rdf-validate-shacl

### SHACL Validation

**Before (rdf-validate-shacl):**
```javascript
import SHACLValidator from 'rdf-validate-shacl';
import rdf from 'rdf-ext';

const dataDataset = rdf.dataset([...dataStore]);
const shapesDataset = rdf.dataset([...shapesStore]);
const validator = new SHACLValidator(shapesDataset);
const report = await validator.validate(dataDataset);
```

**After (unrdf):**
```javascript
import { useValidator } from 'unrdf';

const validator = useValidator();
const report = await validator.validate(dataStore, shapesStore);
```

## From jsonld

### JSON-LD Processing

**Before (jsonld):**
```javascript
import jsonld from 'jsonld';

const nquads = await jsonld.toRDF(jsonldDoc, { format: 'application/n-quads' });
const doc = await jsonld.fromRDF(nquads, { format: 'application/n-quads' });
```

**After (unrdf):**
```javascript
import { useJsonLd } from 'unrdf';

const jsonld = useJsonLd();
const store = await jsonld.fromJSONLD(jsonldDoc);
const doc = await jsonld.toJSONLD(store);
```

## From rdf-canonize

### Canonicalization

**Before (rdf-canonize):**
```javascript
import rdfCanonize from 'rdf-canonize';

const canonical = await rdfCanonize.canonize(nquads, {
  algorithm: 'URDNA2015',
  format: 'application/n-quads'
});
```

**After (unrdf):**
```javascript
import { useCanon } from 'unrdf';

const canon = useCanon();
const canonical = await canon.canonicalize(store);
```

## From Clownface

### Graph Traversal

**Before (Clownface):**
```javascript
import $rdf from '@zazuko/env';

const dataset = $rdf.dataset();
const cf = $rdf.clownface({ dataset });
const person = cf.namedNode('http://example.org/alice');
const types = person.out($rdf.ns.rdf.type);
```

**After (unrdf):**
```javascript
import { useGraph } from 'unrdf';

const graph = useGraph(store);
const results = await graph.select(`
  SELECT ?type WHERE {
    <http://example.org/alice> a ?type .
  }
`);
```

## Common Migration Patterns

### 1. Store Management

**Pattern: Multiple stores**
```javascript
// Before: Manual store management
const store1 = new Store();
const store2 = new Store();
const merged = new Store([...store1, ...store2]);

// After: Composable store management
const store1 = useStore();
const store2 = useStore();
const graph = useGraph(store1);
const merged = graph.union(store2);
```

### 2. Error Handling

**Pattern: Try-catch blocks**
```javascript
// Before: Manual error handling
try {
  const results = await engine.queryBindings(sparql, { sources: [store] });
} catch (error) {
  console.error('Query failed:', error);
}

// After: Consistent error handling
try {
  const results = await graph.select(sparql);
} catch (error) {
  console.error('Query failed:', error.message);
}
```

### 3. Configuration

**Pattern: Engine configuration**
```javascript
// Before: Multiple configuration objects
const engine = new QueryEngine();
const parser = new Parser({ baseIRI: 'http://example.org/' });
const validator = new SHACLValidator(shapes);

// After: Unified configuration
const engine = new RdfEngine({ 
  baseIRI: 'http://example.org/',
  timeoutMs: 30000 
});
```

### 4. Async Operations

**Pattern: Promise handling**
```javascript
// Before: Manual promise handling
const results = [];
for await (const binding of bindings) {
  results.push(binding);
}

// After: Automatic promise handling
const results = await graph.select(sparql);
```

## Migration Checklist

### 1. Dependencies
- [ ] Remove old RDF libraries from package.json
- [ ] Add unrdf to package.json
- [ ] Update import statements

### 2. Store Operations
- [ ] Replace store creation with `useStore()`
- [ ] Update quad creation to use composable methods
- [ ] Replace manual store operations with composable methods

### 3. SPARQL Queries
- [ ] Replace direct Comunica usage with `useGraph()`
- [ ] Update query result handling
- [ ] Add proper error handling

### 4. Validation
- [ ] Replace SHACL validation with `useValidator()`
- [ ] Update validation result handling
- [ ] Add Zod validation where appropriate

### 5. Serialization
- [ ] Replace manual serialization with composable methods
- [ ] Update file I/O operations
- [ ] Add proper error handling

### 6. Testing
- [ ] Update test imports
- [ ] Update test assertions
- [ ] Add edge case testing

## Performance Considerations

### 1. Memory Usage
```javascript
// Before: Manual memory management
const stores = [];
// ... create many stores
stores.forEach(store => store.clear());

// After: Automatic memory management
const stores = [];
// ... create many stores
stores.forEach(store => store.clear());
```

### 2. Timeout Management
```javascript
// Before: No timeout management
const results = await engine.queryBindings(sparql, { sources: [store] });

// After: Built-in timeout management
const graph = useGraph(store, { timeoutMs: 30000 });
const results = await graph.select(sparql);
```

### 3. Deterministic Operations
```javascript
// Before: Non-deterministic results
const results = await engine.queryBindings(sparql, { sources: [store] });

// After: Deterministic results
const engine = new RdfEngine({ deterministic: true });
const graph = useGraph(store);
const results = await graph.select(sparql);
```

## Troubleshooting

### Common Issues

#### 1. Import Errors
```javascript
// Error: Cannot resolve module 'unrdf'
// Solution: Check package.json and node_modules
pnpm install unrdf
```

#### 2. Async/Await Issues
```javascript
// Error: Cannot use await in non-async function
// Solution: Make function async
async function processData() {
  const results = await graph.select(sparql);
}
```

#### 3. Store Type Issues
```javascript
// Error: Store is not iterable
// Solution: Use composable methods
const graph = useGraph(store);
const results = await graph.select(sparql);
```

#### 4. Validation Errors
```javascript
// Error: Validation failed
// Solution: Check data and shapes
const report = await validator.validate(dataStore, shapesStore);
if (!report.conforms) {
  console.error('Validation errors:', report.results);
}
```

### Getting Help

1. Check the [API Reference](./api-reference.md)
2. Look at [Examples](./examples/)
3. Review [Core Concepts](./core-concepts.md)
4. Check the [GitHub Issues](https://github.com/gitvan/unrdf/issues)

## Best Practices

### 1. Use Composables Consistently
```javascript
// Good: Use composables for all operations
const store = useStore();
const graph = useGraph(store);

// Avoid: Mixing direct engine usage
const engine = new RdfEngine();
const results = await engine.query(store, sparql);
```

### 2. Handle Errors Properly
```javascript
// Good: Comprehensive error handling
try {
  const results = await graph.select(sparql);
} catch (error) {
  console.error('Query failed:', error.message);
  // Provide fallback or rethrow
}
```

### 3. Use Appropriate Timeouts
```javascript
// Good: Set timeouts based on operation complexity
const quickOp = useGraph(store, { timeoutMs: 5000 });
const slowOp = useReasoner({ timeoutMs: 60000 });
```

### 4. Validate Data
```javascript
// Good: Validate inputs and outputs
const schema = zod.z.object({ name: zod.z.string() });
const validation = zod.validateResults(results, schema);
if (!validation.validated) {
  throw new Error('Invalid data structure');
}
```

This migration guide should help you transition from other RDF libraries to unrdf smoothly. If you encounter issues not covered here, please check the documentation or open an issue on GitHub.
