# useGraph

The `useGraph` composable provides high-level graph operations including SPARQL queries and set operations.

## Overview

`useGraph` wraps an N3.Store with a comprehensive API for graph operations. It provides methods for SPARQL queries (SELECT, ASK, CONSTRUCT, UPDATE), SHACL validation, and set operations (union, difference, intersection).

## Basic Usage

```javascript
import { useGraph } from 'unrdf';

// Create a graph from a store
const store = useStore();
const graph = useGraph(store);

// Execute SPARQL queries
const results = await graph.select(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?person WHERE {
    ?person a foaf:Person .
  }
`);

console.log('Found persons:', results.results);
```

## API Reference

### Constructor

```javascript
const graph = useGraph(store);
```

Creates a new graph instance from an N3.Store.

**Parameters:**
- `store` - N3.Store instance or useStore instance

### Properties

#### `store`
The underlying N3.Store instance.

```javascript
const graph = useGraph(store);
console.log(graph.store); // N3.Store instance
```

#### `size`
Number of quads in the graph.

```javascript
const graph = useGraph(store);
console.log(graph.size); // Number of quads
```

#### `stats`
Store statistics.

```javascript
const graph = useGraph(store);
const stats = graph.stats;
console.log(stats);
// {
//   quads: 10,
//   subjects: 5,
//   predicates: 3,
//   objects: 8,
//   graphs: 1
// }
```

### Methods

#### `select(sparql, options)`
Execute a SPARQL SELECT query.

```javascript
const results = await graph.select(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?person ?name WHERE {
    ?person a foaf:Person ;
            foaf:name ?name .
  }
`);

console.log(results);
// {
//   type: 'select',
//   variables: ['person', 'name'],
//   results: [
//     { person: 'http://example.org/alice', name: 'Alice' },
//     { person: 'http://example.org/bob', name: 'Bob' }
//   ]
// }
```

**Parameters:**
- `sparql` - SPARQL SELECT query string
- `options` - Query options (optional)
  - `limit` - Maximum number of results
  - `signal` - Abort signal

#### `ask(sparql, options)`
Execute a SPARQL ASK query.

```javascript
const hasPersons = await graph.ask(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  ASK WHERE {
    ?person a foaf:Person .
  }
`);

console.log('Has persons:', hasPersons); // true or false
```

**Parameters:**
- `sparql` - SPARQL ASK query string
- `options` - Query options (optional)

#### `construct(sparql, options)`
Execute a SPARQL CONSTRUCT query.

```javascript
const constructed = await graph.construct(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  CONSTRUCT {
    ?person foaf:name ?name .
  } WHERE {
    ?person a foaf:Person ;
            foaf:name ?name .
  }
`);

console.log(constructed);
// {
//   type: 'construct',
//   store: N3.Store instance,
//   quads: [quad1, quad2, ...]
// }
```

**Parameters:**
- `sparql` - SPARQL CONSTRUCT query string
- `options` - Query options (optional)

#### `update(sparql, options)`
Execute a SPARQL UPDATE query.

```javascript
await graph.update(`
  PREFIX ex: <http://example.org/>
  INSERT DATA {
    ex:alice ex:age 30 .
  }
`);
```

**Parameters:**
- `sparql` - SPARQL UPDATE query string
- `options` - Query options (optional)

#### `validate(dataStore, shapesStore)`
Validate the graph against SHACL shapes.

```javascript
const report = await graph.validate(dataStore, shapesStore);

if (report.conforms) {
  console.log('Validation passed!');
} else {
  console.log('Validation failed:', report.results);
}
```

**Parameters:**
- `dataStore` - Data store to validate
- `shapesStore` - SHACL shapes store

#### `union(...stores)`
Create the union of multiple stores.

```javascript
const store1 = useStore();
const store2 = useStore();
const graph1 = useGraph(store1);
const graph2 = useGraph(store2);

const unioned = graph1.union(store2);
console.log(unioned.size); // Total quads from both stores
```

**Parameters:**
- `...stores` - Stores to union

#### `difference(store1, store2)`
Create the difference between two stores.

```javascript
const diff = graph1.difference(store2);
console.log(diff.size); // Quads in store1 but not in store2
```

**Parameters:**
- `store1` - First store
- `store2` - Second store

#### `intersection(store1, store2)`
Create the intersection of two stores.

```javascript
const intersection = graph1.intersection(store2);
console.log(intersection.size); // Quads in both stores
```

**Parameters:**
- `store1` - First store
- `store2` - Second store

## Examples

### Basic SPARQL Queries

```javascript
import { useStore, useGraph } from 'unrdf';

const store = useStore();
const graph = useGraph(store);

// Add some data
store.add(
  store.quad(
    store.namedNode('http://example.org/alice'),
    store.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    store.namedNode('http://xmlns.com/foaf/0.1/Person')
  ),
  store.quad(
    store.namedNode('http://example.org/alice'),
    store.namedNode('http://xmlns.com/foaf/0.1/name'),
    store.literal('Alice')
  )
);

// SELECT query
const results = await graph.select(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?person ?name WHERE {
    ?person a foaf:Person ;
            foaf:name ?name .
  }
`);

console.log('Results:', results.results);

// ASK query
const hasPersons = await graph.ask(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  ASK WHERE {
    ?person a foaf:Person .
  }
`);

console.log('Has persons:', hasPersons);
```

### CONSTRUCT Queries

```javascript
import { useStore, useGraph } from 'unrdf';

const store = useStore();
const graph = useGraph(store);

// Add data
store.add(
  store.quad(
    store.namedNode('http://example.org/alice'),
    store.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    store.namedNode('http://xmlns.com/foaf/0.1/Person')
  ),
  store.quad(
    store.namedNode('http://example.org/alice'),
    store.namedNode('http://xmlns.com/foaf/0.1/name'),
    store.literal('Alice')
  )
);

// CONSTRUCT query
const constructed = await graph.construct(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  CONSTRUCT {
    ?person foaf:name ?name .
  } WHERE {
    ?person a foaf:Person ;
            foaf:name ?name .
  }
`);

console.log('Constructed quads:', constructed.quads);
console.log('Constructed store size:', constructed.store.size);
```

### UPDATE Queries

```javascript
import { useStore, useGraph } from 'unrdf';

const store = useStore();
const graph = useGraph(store);

// INSERT DATA
await graph.update(`
  PREFIX ex: <http://example.org/>
  INSERT DATA {
    ex:alice a foaf:Person .
    ex:alice foaf:name "Alice" .
  }
`);

console.log('Store size after INSERT:', store.size);

// DELETE DATA
await graph.update(`
  PREFIX ex: <http://example.org/>
  DELETE DATA {
    ex:alice foaf:name "Alice" .
  }
`);

console.log('Store size after DELETE:', store.size);
```

### Set Operations

```javascript
import { useStore, useGraph } from 'unrdf';

const store1 = useStore();
const store2 = useStore();
const graph1 = useGraph(store1);
const graph2 = useGraph(store2);

// Add data to both stores
store1.add(quad1, quad2);
store2.add(quad2, quad3);

// Union
const unioned = graph1.union(store2);
console.log('Union size:', unioned.size); // 3

// Difference
const diff = graph1.difference(store2);
console.log('Difference size:', diff.size); // 1 (quad1)

// Intersection
const intersection = graph1.intersection(store2);
console.log('Intersection size:', intersection.size); // 1 (quad2)
```

### SHACL Validation

```javascript
import { useStore, useGraph } from 'unrdf';

const dataStore = useStore();
const shapesStore = useStore();
const graph = useGraph(dataStore);

// Add data
dataStore.add(
  dataStore.quad(
    dataStore.namedNode('http://example.org/alice'),
    dataStore.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    dataStore.namedNode('http://example.org/Person')
  ),
  dataStore.quad(
    dataStore.namedNode('http://example.org/alice'),
    dataStore.namedNode('http://example.org/name'),
    dataStore.literal('Alice')
  )
);

// Add shapes
shapesStore.add(
  shapesStore.quad(
    shapesStore.namedNode('http://example.org/PersonShape'),
    shapesStore.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    shapesStore.namedNode('http://www.w3.org/ns/shacl#NodeShape')
  ),
  shapesStore.quad(
    shapesStore.namedNode('http://example.org/PersonShape'),
    shapesStore.namedNode('http://www.w3.org/ns/shacl#targetClass'),
    shapesStore.namedNode('http://example.org/Person')
  )
);

// Validate
const report = await graph.validate(dataStore, shapesStore);

if (report.conforms) {
  console.log('Validation passed!');
} else {
  console.log('Validation failed:', report.results);
}
```

### Query with Options

```javascript
import { useStore, useGraph } from 'unrdf';

const store = useStore();
const graph = useGraph(store);

// Add data
for (let i = 0; i < 1000; i++) {
  store.add(
    store.quad(
      store.namedNode(`http://example.org/person${i}`),
      store.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      store.namedNode('http://xmlns.com/foaf/0.1/Person')
    )
  );
}

// Query with limit
const results = await graph.select(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?person WHERE {
    ?person a foaf:Person .
  }
`, { limit: 10 });

console.log('Limited results:', results.results.length); // 10
```

## Performance Considerations

### Query Optimization

```javascript
// Use appropriate limits for large datasets
const results = await graph.select(sparql, { limit: 1000 });

// Use ASK queries when you only need boolean results
const hasData = await graph.ask(sparql);
```

### Memory Management

```javascript
// Clear stores when done
const graph = useGraph(store);
// ... use graph
store.clear();
```

### Streaming Results

```javascript
// For very large result sets, consider processing in batches
const batchSize = 1000;
let offset = 0;

while (true) {
  const results = await graph.select(`
    SELECT ?s ?p ?o WHERE {
      ?s ?p ?o .
    } LIMIT ${batchSize} OFFSET ${offset}
  `);
  
  if (results.results.length === 0) break;
  
  // Process batch
  processBatch(results.results);
  
  offset += batchSize;
}
```

## Best Practices

### 1. Use Appropriate Query Types

```javascript
// Use SELECT for data retrieval
const results = await graph.select(sparql);

// Use ASK for boolean checks
const exists = await graph.ask(sparql);

// Use CONSTRUCT for data transformation
const transformed = await graph.construct(sparql);

// Use UPDATE for data modification
await graph.update(sparql);
```

### 2. Handle Errors Gracefully

```javascript
try {
  const results = await graph.select(sparql);
} catch (error) {
  console.error('Query failed:', error.message);
}
```

### 3. Use Prefixes Consistently

```javascript
const sparql = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>
  
  SELECT ?person WHERE {
    ?person a foaf:Person .
  }
`;
```

### 4. Validate Data

```javascript
// Validate before querying
if (store.size === 0) {
  throw new Error('Store is empty');
}

const results = await graph.select(sparql);
```

## Common Patterns

### Query Builder

```javascript
function buildPersonQuery(filters = {}) {
  let sparql = `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT ?person ?name WHERE {
      ?person a foaf:Person .
      ?person foaf:name ?name .
  `;
  
  if (filters.minAge) {
    sparql += `?person foaf:age ?age . FILTER(?age >= ${filters.minAge}) .`;
  }
  
  sparql += '}';
  return sparql;
}

const query = buildPersonQuery({ minAge: 18 });
const results = await graph.select(query);
```

### Result Processing

```javascript
async function processResults(graph, sparql) {
  const results = await graph.select(sparql);
  
  return results.results.map(row => ({
    id: row.person,
    name: row.name,
    // ... other processing
  }));
}
```

### Batch Operations

```javascript
async function batchQuery(graph, queries) {
  const results = [];
  
  for (const query of queries) {
    try {
      const result = await graph.select(query);
      results.push(result);
    } catch (error) {
      console.error('Query failed:', error.message);
      results.push(null);
    }
  }
  
  return results;
}
```

## Troubleshooting

### Common Issues

#### Query Syntax Errors
```javascript
try {
  const results = await graph.select('INVALID SPARQL');
} catch (error) {
  console.error('Query syntax error:', error.message);
}
```

#### Empty Results
```javascript
const results = await graph.select(sparql);
if (results.results.length === 0) {
  console.log('No results found');
}
```

#### Memory Issues
```javascript
// For large result sets, use limits
const results = await graph.select(sparql, { limit: 1000 });
```

### Getting Help

1. Check the [API Reference](../api-reference.md)
2. Look at [Examples](../examples/)
3. Review [Core Concepts](../core-concepts.md)
4. Check the [GitHub Issues](https://github.com/gitvan/unrdf/issues)