# Core API Reference

This chapter documents UNRDF's core API for store context management and RDF operations.

## Store Context API

### `initStore(initialQuads, options)`

Initialize the root store context for your application. This must be called at the application root.

**Parameters:**
- `initialQuads` (Array<Quad>, optional): Initial RDF quads to populate the store. Default: `[]`
- `options` (Object, optional): Store configuration options

**Returns:** `Function` - A runner function that accepts your application logic

**Example:**
```javascript
import { initStore, useStoreContext } from 'unrdf';

// Initialize store at application root
const runApp = initStore([], { baseIRI: 'http://example.org/' });

runApp(() => {
  // Your application code here
  const store = useStoreContext();
  // All composables will use the same store
});
```

---

### `useStoreContext()`

Access the current store context. Must be called within an `initStore()` runner.

**Returns:** `StoreContext` - The current store context instance

**Throws:** `Error` - If store context is not initialized

**Example:**
```javascript
const store = useStoreContext();
console.log('Store size:', store.stats().quads);
```

---

### `createStoreContext(initialQuads, options)`

Create a store context instance without setting it as the active context. Useful for creating isolated stores.

**Parameters:**
- `initialQuads` (Array<Quad>, optional): Initial quads. Default: `[]`
- `options` (Object, optional): Store options

**Returns:** `StoreContext` - A new store context instance

**Throws:**
- `TypeError` - If initialQuads is not an array
- `TypeError` - If options is not an object

**Example:**
```javascript
const isolatedStore = createStoreContext([quad1, quad2], {
  baseIRI: 'http://example.org/'
});
```

---

## StoreContext Operations

### Sender Operations (PRIMARY)

These operations modify the store state and are the primary way to interact with UNRDF.

#### `add(...quads)`

Add quads to the store.

**Parameters:**
- `...quads` (Quad[]): One or more quads to add

**Returns:** `StoreContext` - The context for chaining

**Throws:** `TypeError` - If any quad is invalid

**Example:**
```javascript
const store = useStoreContext();

store.add(
  quad(
    namedNode('http://example.org/alice'),
    namedNode('http://xmlns.com/foaf/0.1/knows'),
    namedNode('http://example.org/bob')
  )
);

// Chaining
store
  .add(quad1)
  .add(quad2)
  .add(quad3);
```

---

#### `remove(...quads)`

Remove quads from the store.

**Parameters:**
- `...quads` (Quad[]): One or more quads to remove

**Returns:** `StoreContext` - The context for chaining

**Throws:** `TypeError` - If any quad is invalid

**Example:**
```javascript
store.remove(
  quad(
    namedNode('http://example.org/alice'),
    namedNode('http://xmlns.com/foaf/0.1/knows'),
    namedNode('http://example.org/charlie')
  )
);
```

---

#### `clear()`

Clear all quads from the store.

**Returns:** `StoreContext` - The context for chaining

**Example:**
```javascript
store.clear();
console.log('Store cleared, size:', store.stats().quads); // 0
```

---

### Term Creation

#### `namedNode(value)`

Create a named node (IRI).

**Parameters:**
- `value` (string): The IRI value

**Returns:** `NamedNode` - Named node term

**Throws:** `TypeError` - If value is not a string

**Example:**
```javascript
const alice = store.namedNode('http://example.org/alice');
const foafKnows = store.namedNode('http://xmlns.com/foaf/0.1/knows');
```

---

#### `literal(value, datatype)`

Create a literal value.

**Parameters:**
- `value` (string): The literal value
- `datatype` (string, optional): Datatype IRI or language tag

**Returns:** `Literal` - Literal term

**Throws:** `TypeError` - If value is not a string

**Example:**
```javascript
// Plain literal
const name = store.literal('Alice');

// Typed literal
const age = store.literal('30', 'http://www.w3.org/2001/XMLSchema#integer');

// Language-tagged literal
const greeting = store.literal('Hello', 'en');
```

---

#### `blankNode(value)`

Create a blank node.

**Parameters:**
- `value` (string, optional): Blank node identifier

**Returns:** `BlankNode` - Blank node term

**Throws:** `TypeError` - If value is provided but not a string

**Example:**
```javascript
// Auto-generated identifier
const person = store.blankNode();

// Specific identifier
const person2 = store.blankNode('person1');
```

---

#### `quad(subject, predicate, object, graph)`

Create a quad.

**Parameters:**
- `subject` (Term): Subject term
- `predicate` (Term): Predicate term
- `object` (Term): Object term
- `graph` (Term, optional): Graph term (defaults to default graph)

**Returns:** `Quad` - Quad object

**Throws:** `TypeError` - If any required parameter is missing

**Example:**
```javascript
const q = store.quad(
  store.namedNode('http://example.org/alice'),
  store.namedNode('http://xmlns.com/foaf/0.1/name'),
  store.literal('Alice'),
  store.namedNode('http://example.org/graph1')
);

store.add(q);
```

---

### Reader Operations (OPTIONAL)

These operations read from the store without modifying it. Use sparingly in sender-only mode.

#### `serialize(options)`

Serialize the store to a string format.

**Parameters:**
- `options` (Object, optional): Serialization options
  - `format` (string): Output format ('Turtle' or 'N-Quads'). Default: 'Turtle'
  - `prefixes` (Object): Prefix mappings for Turtle format

**Returns:** `Promise<string>` - Serialized string

**Throws:**
- `TypeError` - If options is not an object
- `Error` - If format is unsupported

**Example:**
```javascript
// Serialize to Turtle with prefixes
const turtle = await store.serialize({
  format: 'Turtle',
  prefixes: {
    ex: 'http://example.org/',
    foaf: 'http://xmlns.com/foaf/0.1/'
  }
});
console.log(turtle);

// Serialize to N-Quads
const nquads = await store.serialize({ format: 'N-Quads' });
```

---

#### `stats()`

Get statistics about the store.

**Returns:** `Object` - Store statistics
- `quads` (number): Total number of quads
- `subjects` (number): Unique subjects count
- `predicates` (number): Unique predicates count
- `objects` (number): Unique objects count
- `graphs` (number): Unique graphs count

**Example:**
```javascript
const stats = store.stats();
console.log(`Store contains ${stats.quads} quads`);
console.log(`Unique subjects: ${stats.subjects}`);
console.log(`Unique predicates: ${stats.predicates}`);
```

---

#### `query(sparql, options)`

Execute a SPARQL query against the store.

**Parameters:**
- `sparql` (string): SPARQL query string
- `options` (Object, optional): Query options
  - `limit` (number): Result limit
  - `signal` (AbortSignal): Abort signal
  - `deterministic` (boolean): Enable deterministic results

**Returns:** `Promise<Object>` - Query result object
- `type` (string): Query type ('select', 'ask', 'construct', 'describe', 'update')
- `rows` (Array, for SELECT): Result bindings
- `boolean` (boolean, for ASK): Boolean result
- `store` (Store, for CONSTRUCT/DESCRIBE): Result store

**Throws:** `Error` - If query is invalid

**Example:**
```javascript
// SELECT query
const result = await store.query(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?person ?name WHERE {
    ?person foaf:name ?name .
  }
`);
console.log('Results:', result.rows);

// ASK query
const hasAlice = await store.query(`
  ASK { ?s foaf:name "Alice" }
`);
console.log('Has Alice:', hasAlice.boolean);

// UPDATE query
await store.query(`
  PREFIX ex: <http://example.org/>
  INSERT DATA {
    ex:alice ex:age "30" .
  }
`);
```

---

#### `canonicalize(options)`

Canonicalize the store using URDNA2015 algorithm.

**Parameters:**
- `options` (Object, optional): Canonicalization options
  - `timeoutMs` (number): Timeout in milliseconds. Default: 30000
  - `onMetric` (Function): Metrics callback

**Returns:** `Promise<string>` - Canonicalized N-Quads string

**Throws:** `Error` - If canonicalization fails

**Example:**
```javascript
const canonical = await store.canonicalize({
  timeoutMs: 10000,
  onMetric: (name, data) => {
    console.log(`Metric ${name}:`, data);
  }
});
```

---

#### `isIsomorphic(store1, store2, options)`

Check if two stores are isomorphic (structurally equivalent).

**Parameters:**
- `store1` (Store): First store
- `store2` (Store): Second store
- `options` (Object, optional): Isomorphism options

**Returns:** `Promise<boolean>` - True if stores are isomorphic

**Throws:** `Error` - If isomorphism check fails

**Example:**
```javascript
const store1 = new Store();
const store2 = new Store();

// Add quads to stores...

const isEqual = await store.isIsomorphic(store1, store2);
console.log('Stores are isomorphic:', isEqual);
```

---

#### `hash(options)`

Generate a canonical hash of the store.

**Parameters:**
- `options` (Object, optional): Hash options
  - `algorithm` (string): Hash algorithm. Default: 'SHA-256'

**Returns:** `Promise<string>` - Hexadecimal hash string

**Throws:** `Error` - If hashing fails

**Example:**
```javascript
const storeHash = await store.hash({ algorithm: 'SHA-256' });
console.log('Store hash:', storeHash);

// Compare stores by hash
const hash1 = await store1.hash();
const hash2 = await store2.hash();
console.log('Stores identical:', hash1 === hash2);
```

---

## Best Practices

### Sender-Only Pattern

UNRDF enforces a sender-only model for optimal performance:

```javascript
// ✅ GOOD: Sender operations
store
  .add(quad1)
  .add(quad2)
  .remove(quad3);

// ⚠️ USE SPARINGLY: Reader operations
const stats = store.stats();
const turtle = await store.serialize();
```

### Chaining Operations

All sender operations return the context for chaining:

```javascript
store
  .clear()
  .add(quad(alice, knows, bob))
  .add(quad(bob, knows, charlie))
  .add(quad(charlie, knows, alice));
```

### Error Handling

Always handle errors from async operations:

```javascript
try {
  const result = await store.query(sparqlQuery);
  console.log('Query results:', result);
} catch (error) {
  console.error('Query failed:', error.message);
}
```

### Context Isolation

Use `createStoreContext` for isolated stores:

```javascript
// Main application store
const runApp = initStore();

runApp(async () => {
  const mainStore = useStoreContext();

  // Isolated temporary store
  const tempStore = createStoreContext();
  tempStore.add(quad1);

  // Main store is unaffected
});
```
