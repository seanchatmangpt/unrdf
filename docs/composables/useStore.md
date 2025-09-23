# useStore

The `useStore` composable provides a clean, composable interface for managing N3.Store instances.

## Overview

`useStore` is the foundation of unrdf's store management. It wraps N3.Store with a consistent API and provides methods for adding, removing, and querying RDF data.

## Basic Usage

```javascript
import { useStore } from 'unrdf';

// Create a new store
const store = useStore();

// Add some data
store.add(
  store.quad(
    store.namedNode('http://example.org/alice'),
    store.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    store.namedNode('http://xmlns.com/foaf/0.1/Person')
  )
);

console.log(`Store has ${store.size} quads`);
```

## API Reference

### Constructor

```javascript
const store = useStore();
```

Creates a new store instance with an empty N3.Store.

### Properties

#### `store`
The underlying N3.Store instance.

```javascript
const store = useStore();
console.log(store.store); // N3.Store instance
```

#### `size`
Number of quads in the store.

```javascript
const store = useStore();
store.add(quad1, quad2);
console.log(store.size); // 2
```

### Methods

#### `add(...quads)`
Add one or more quads to the store.

```javascript
const store = useStore();
const quad1 = store.quad(
  store.namedNode('http://example.org/alice'),
  store.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
  store.namedNode('http://xmlns.com/foaf/0.1/Person')
);

const quad2 = store.quad(
  store.namedNode('http://example.org/alice'),
  store.namedNode('http://xmlns.com/foaf/0.1/name'),
  store.literal('Alice')
);

// Add single quad
store.add(quad1);

// Add multiple quads
store.add(quad1, quad2);
```

#### `remove(...quads)`
Remove one or more quads from the store.

```javascript
const store = useStore();
store.add(quad1, quad2);

// Remove single quad
store.remove(quad1);

// Remove multiple quads
store.remove(quad1, quad2);
```

#### `clear()`
Remove all quads from the store.

```javascript
const store = useStore();
store.add(quad1, quad2);
console.log(store.size); // 2

store.clear();
console.log(store.size); // 0
```

#### `serialize()`
Serialize the store to Turtle format.

```javascript
const store = useStore();
store.add(quad1, quad2);

const turtle = store.serialize();
console.log(turtle);
// @prefix ex: <http://example.org/> .
// ex:alice a foaf:Person ;
//     foaf:name "Alice" .
```

#### `stats()`
Get statistics about the store.

```javascript
const store = useStore();
store.add(quad1, quad2);

const stats = store.stats();
console.log(stats);
// {
//   quads: 2,
//   subjects: 1,
//   predicates: 2,
//   objects: 2,
//   graphs: 1
// }
```

### Term Creation Methods

#### `namedNode(iri)`
Create a named node.

```javascript
const store = useStore();
const subject = store.namedNode('http://example.org/alice');
```

#### `literal(value, datatype)`
Create a literal.

```javascript
const store = useStore();
const name = store.literal('Alice');
const age = store.literal('30', 'http://www.w3.org/2001/XMLSchema#integer');
const englishName = store.literal('Alice', null, 'en');
```

#### `blankNode(id)`
Create a blank node.

```javascript
const store = useStore();
const bnode = store.blankNode('b1');
```

#### `quad(s, p, o, g)`
Create a quad.

```javascript
const store = useStore();
const quad = store.quad(
  store.namedNode('http://example.org/alice'),
  store.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
  store.namedNode('http://xmlns.com/foaf/0.1/Person')
);
```

## Examples

### Basic Store Operations

```javascript
import { useStore } from 'unrdf';

const store = useStore();

// Create some data
const alice = store.namedNode('http://example.org/alice');
const bob = store.namedNode('http://example.org/bob');
const type = store.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
const person = store.namedNode('http://xmlns.com/foaf/0.1/Person');
const name = store.namedNode('http://xmlns.com/foaf/0.1/name');

// Add quads
store.add(
  store.quad(alice, type, person),
  store.quad(alice, name, store.literal('Alice')),
  store.quad(bob, type, person),
  store.quad(bob, name, store.literal('Bob'))
);

console.log(`Store has ${store.size} quads`);

// Serialize to Turtle
const turtle = store.serialize();
console.log(turtle);
```

### Working with Blank Nodes

```javascript
import { useStore } from 'unrdf';

const store = useStore();

// Create a blank node
const bnode = store.blankNode('b1');

// Add quads with blank nodes
store.add(
  store.quad(
    store.namedNode('http://example.org/alice'),
    store.namedNode('http://xmlns.com/foaf/0.1/knows'),
    bnode
  ),
  store.quad(
    bnode,
    store.namedNode('http://xmlns.com/foaf/0.1/name'),
    store.literal('Bob')
  )
);

console.log(store.serialize());
```

### Store Statistics

```javascript
import { useStore } from 'unrdf';

const store = useStore();

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

// Get statistics
const stats = store.stats();
console.log('Store statistics:', stats);
// {
//   quads: 2,
//   subjects: 1,
//   predicates: 2,
//   objects: 2,
//   graphs: 1
// }
```

### Error Handling

```javascript
import { useStore } from 'unrdf';

const store = useStore();

try {
  // Add valid quads
  store.add(validQuad);
  
  // Remove quads
  store.remove(validQuad);
  
  // Clear store
  store.clear();
  
} catch (error) {
  console.error('Store operation failed:', error.message);
}
```

## Performance Considerations

### Memory Management

```javascript
// Clear stores when done to free memory
const store = useStore();
// ... use store
store.clear();
```

### Large Datasets

```javascript
// For large datasets, consider using streaming
const store = useStore();

// Add quads in batches
const batchSize = 1000;
for (let i = 0; i < quads.length; i += batchSize) {
  const batch = quads.slice(i, i + batchSize);
  store.add(...batch);
}
```

### Serialization

```javascript
// Serialization can be expensive for large stores
const store = useStore();
// ... add many quads

// Consider if you really need serialization
if (store.size < 10000) {
  const turtle = store.serialize();
}
```

## Best Practices

### 1. Use Consistent Naming

```javascript
const store = useStore();
const alice = store.namedNode('http://example.org/alice');
const type = store.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
const person = store.namedNode('http://xmlns.com/foaf/0.1/Person');

store.add(store.quad(alice, type, person));
```

### 2. Handle Errors Gracefully

```javascript
try {
  store.add(quad);
} catch (error) {
  console.error('Failed to add quad:', error.message);
}
```

### 3. Use Appropriate Data Types

```javascript
// Use appropriate datatypes for literals
const age = store.literal('30', 'http://www.w3.org/2001/XMLSchema#integer');
const name = store.literal('Alice', null, 'en');
const date = store.literal('2023-01-01', 'http://www.w3.org/2001/XMLSchema#date');
```

### 4. Clear Stores When Done

```javascript
const store = useStore();
// ... use store
store.clear(); // Free memory
```

## Common Patterns

### Store Factory

```javascript
function createPersonStore(name, age) {
  const store = useStore();
  const person = store.namedNode(`http://example.org/${name.toLowerCase()}`);
  
  store.add(
    store.quad(person, store.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'), store.namedNode('http://xmlns.com/foaf/0.1/Person')),
    store.quad(person, store.namedNode('http://xmlns.com/foaf/0.1/name'), store.literal(name)),
    store.quad(person, store.namedNode('http://xmlns.com/foaf/0.1/age'), store.literal(age.toString(), 'http://www.w3.org/2001/XMLSchema#integer'))
  );
  
  return store;
}

const aliceStore = createPersonStore('Alice', 30);
```

### Store Validation

```javascript
function validateStore(store) {
  if (store.size === 0) {
    throw new Error('Store is empty');
  }
  
  const stats = store.stats();
  if (stats.subjects === 0) {
    throw new Error('Store has no subjects');
  }
  
  return true;
}
```

### Store Merging

```javascript
function mergeStores(...stores) {
  const merged = useStore();
  
  for (const store of stores) {
    for (const quad of store.store) {
      merged.add(quad);
    }
  }
  
  return merged;
}
```

## Troubleshooting

### Common Issues

#### Store is Empty
```javascript
const store = useStore();
console.log(store.size); // 0

// Check if you're adding quads correctly
store.add(quad);
console.log(store.size); // 1
```

#### Serialization Issues
```javascript
const store = useStore();
store.add(quad);

try {
  const turtle = store.serialize();
  console.log(turtle);
} catch (error) {
  console.error('Serialization failed:', error.message);
}
```

#### Memory Issues
```javascript
// For large stores, clear when done
const store = useStore();
// ... add many quads
store.clear(); // Free memory
```

### Getting Help

1. Check the [API Reference](../api-reference.md)
2. Look at [Examples](../examples/)
3. Review [Core Concepts](../core-concepts.md)
4. Check the [GitHub Issues](https://github.com/gitvan/unrdf/issues)