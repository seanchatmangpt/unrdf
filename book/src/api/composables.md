# Composables API Reference

UNRDF provides composable functions for common RDF operations. All composables use the store context established by `initStore()`.

## Graph Operations

### `useGraph()`

Access high-level RDF graph operations with SPARQL querying and graph transformations.

**Returns:** `Object` - Graph operations interface

**Throws:** `Error` - If store context is not initialized

**Example:**
```javascript
import { initStore, useGraph } from 'unrdf';

const runApp = initStore();

runApp(async () => {
  const graph = useGraph();

  // Execute SPARQL queries
  const results = await graph.select(`
    SELECT ?s ?p ?o WHERE { ?s ?p ?o }
  `);
});
```

---

### Graph API Methods

#### `query(sparql, options)`

Execute any valid SPARQL 1.1 query.

**Parameters:**
- `sparql` (string): SPARQL query string
- `options` (Object, optional): Query options
  - `limit` (number): Result limit
  - `signal` (AbortSignal): Abort signal

**Returns:** `Promise<Object>` - Query result object

**Throws:** `TypeError` - If sparql is not a string

**Example:**
```javascript
const result = await graph.query(`
  PREFIX ex: <http://example.org/>
  SELECT ?name WHERE {
    ?person ex:name ?name .
  }
`, { limit: 10 });
```

---

#### `select(sparql)`

Execute a SPARQL SELECT query.

**Parameters:**
- `sparql` (string): SPARQL SELECT query string

**Returns:** `Promise<Array<Object>>` - Array of result bindings

**Throws:**
- `TypeError` - If sparql is not a string
- `Error` - If query is not a SELECT query

**Example:**
```javascript
const people = await graph.select(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?person ?name WHERE {
    ?person foaf:name ?name .
  }
`);

people.forEach(row => {
  console.log(`${row.person.value}: ${row.name.value}`);
});
```

---

#### `ask(sparql)`

Execute a SPARQL ASK query.

**Parameters:**
- `sparql` (string): SPARQL ASK query string

**Returns:** `Promise<boolean>` - Boolean result

**Throws:**
- `TypeError` - If sparql is not a string
- `Error` - If query is not an ASK query

**Example:**
```javascript
const hasAlice = await graph.ask(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  ASK {
    ?person foaf:name "Alice" .
  }
`);

console.log('Has Alice:', hasAlice);
```

---

#### `construct(sparql)`

Execute a SPARQL CONSTRUCT query.

**Parameters:**
- `sparql` (string): SPARQL CONSTRUCT query string

**Returns:** `Promise<Store>` - New store with constructed triples

**Throws:**
- `TypeError` - If sparql is not a string
- `Error` - If query is not a CONSTRUCT query

**Example:**
```javascript
const inferredStore = await graph.construct(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>
  CONSTRUCT {
    ?person1 ex:friend ?person2 .
  }
  WHERE {
    ?person1 foaf:knows ?person2 .
  }
`);
```

---

#### `update(sparql)`

Execute a SPARQL UPDATE query.

**Parameters:**
- `sparql` (string): SPARQL UPDATE query string

**Returns:** `Promise<Object>` - Update result

**Throws:**
- `TypeError` - If sparql is not a string
- `Error` - If query is not an UPDATE query

**Example:**
```javascript
await graph.update(`
  PREFIX ex: <http://example.org/>
  INSERT DATA {
    ex:alice ex:age "30" .
    ex:alice ex:city "NYC" .
  }
`);
```

---

#### `serialize(options)`

Serialize the graph to a string.

**Parameters:**
- `options` (Object, optional): Serialization options
  - `format` (string): Output format ('Turtle' or 'N-Quads'). Default: 'Turtle'
  - `prefixes` (Object): Prefix mappings

**Returns:** `string` - Serialized string

**Throws:**
- `TypeError` - If options is not an object
- `Error` - If format is unsupported

**Example:**
```javascript
const turtle = graph.serialize({
  format: 'Turtle',
  prefixes: {
    ex: 'http://example.org/',
    foaf: 'http://xmlns.com/foaf/0.1/'
  }
});
```

---

#### `stats()`

Get graph statistics.

**Returns:** `Object` - Graph statistics

**Example:**
```javascript
const stats = graph.stats();
console.log(`Quads: ${stats.quads}`);
console.log(`Subjects: ${stats.subjects}`);
```

---

#### `union(...otherGraphs)`

Create a new graph containing the union of this graph and others.

**Parameters:**
- `...otherGraphs` (Object|Store): Other useGraph instances or Stores

**Returns:** `Object` - New useGraph instance with union

**Example:**
```javascript
const graph1 = useGraph();
const graph2 = createTemporaryGraph(store2);

const combined = graph1.union(graph2);
console.log('Combined size:', combined.size);
```

---

#### `difference(otherGraph)`

Create a new graph containing quads in this graph but not in the other.

**Parameters:**
- `otherGraph` (Object|Store): Another useGraph instance or Store

**Returns:** `Object` - New useGraph instance with difference

**Example:**
```javascript
const diff = graph1.difference(graph2);
```

---

#### `intersection(otherGraph)`

Create a new graph containing only quads that exist in both graphs.

**Parameters:**
- `otherGraph` (Object|Store): Another useGraph instance or Store

**Returns:** `Object` - New useGraph instance with intersection

**Example:**
```javascript
const common = graph1.intersection(graph2);
```

---

## Turtle Operations

### `useTurtle(graphDir, options)`

File system operations for Turtle (.ttl) files.

**Parameters:**
- `graphDir` (string, optional): Directory containing Turtle files. Default: './graph'
- `options` (Object, optional): Turtle options
  - `baseIRI` (string): Base IRI for parsing. Default: 'http://example.org/'
  - `autoLoad` (boolean): Automatically load all .ttl files. Default: true
  - `validateOnLoad` (boolean): Validate files on load. Default: true

**Returns:** `Object` - Turtle file system interface

**Example:**
```javascript
import { initStore, useTurtle } from 'unrdf';

const runApp = initStore();

runApp(() => {
  const turtle = useTurtle('./my-data', {
    baseIRI: 'http://example.org/',
    autoLoad: true
  });

  // Load all .ttl files
  turtle.loadAll();
});
```

---

### Turtle API Methods

#### `loadAll(options)`

Load all .ttl files from the graph directory.

**Parameters:**
- `options` (Object, optional): Load options
  - `merge` (boolean): Merge with existing store. Default: true
  - `validate` (boolean): Validate files on load

**Returns:** `Object` - Load result
- `loaded` (number): Number of files loaded
- `files` (Array<string>): Loaded file names

**Example:**
```javascript
const result = turtle.loadAll({ merge: true, validate: true });
console.log(`Loaded ${result.loaded} files:`, result.files);
```

---

#### `load(fileName, options)`

Load a specific Turtle file.

**Parameters:**
- `fileName` (string): Name of the file (without .ttl extension)
- `options` (Object, optional): Load options
  - `merge` (boolean): Merge with existing store. Default: true
  - `validate` (boolean): Validate file on load

**Returns:** `Store` - Loaded store

**Throws:** `Error` - If file not found

**Example:**
```javascript
const store = turtle.load('ontology', {
  merge: true,
  validate: true
});
```

---

#### `save(fileName, options)`

Save the current store to a Turtle file.

**Parameters:**
- `fileName` (string): Name of the file (without .ttl extension)
- `options` (Object, optional): Save options
  - `prefixes` (Object): Prefix mappings
  - `createBackup` (boolean): Create backup of existing file. Default: false

**Returns:** `Object` - Save result
- `path` (string): File path
- `bytes` (number): File size in bytes

**Example:**
```javascript
const result = turtle.save('export', {
  prefixes: {
    ex: 'http://example.org/',
    foaf: 'http://xmlns.com/foaf/0.1/'
  },
  createBackup: true
});

console.log(`Saved to ${result.path} (${result.bytes} bytes)`);
```

---

#### `listFiles()`

List all .ttl files in the graph directory.

**Returns:** `Array<string>` - Array of file names

**Example:**
```javascript
const files = turtle.listFiles();
console.log('Available files:', files);
```

---

#### `parse(ttl, options)`

Parse a Turtle string into a store.

**Parameters:**
- `ttl` (string): Turtle string
- `options` (Object, optional): Parse options
  - `addToStore` (boolean): Add parsed data to context store. Default: false

**Returns:** `Store` - Parsed store

**Example:**
```javascript
const turtleData = `
  @prefix ex: <http://example.org/> .
  ex:alice ex:knows ex:bob .
`;

const store = turtle.parse(turtleData, { addToStore: true });
```

---

## Delta Operations

### `useDelta(options)`

Track and apply graph changes (deltas).

**Parameters:**
- `options` (Object, optional): Delta options
  - `deterministic` (boolean): Enable deterministic operations. Default: true

**Returns:** `Object` - Delta interface

**Example:**
```javascript
import { initStore, useDelta } from 'unrdf';

const runApp = initStore();

runApp(() => {
  const delta = useDelta({ deterministic: true });

  // Compare and sync stores
  const changes = delta.compareWith(newStore);
  delta.apply(changes);
});
```

---

### Delta API Methods

#### `compareWith(newStore)`

Compare current context store with new data and return differences.

**Parameters:**
- `newStore` (Store): New data store to compare against

**Returns:** `Object` - Comparison result
- `added` (Store): Quads added in new store
- `removed` (Store): Quads removed from context
- `addedCount` (number): Number of added quads
- `removedCount` (number): Number of removed quads
- `unchangedCount` (number): Number of unchanged quads
- `contextSize` (number): Size of context store
- `newDataSize` (number): Size of new store

**Example:**
```javascript
const newStore = engine.parseTurtle(newData);
const changes = delta.compareWith(newStore);

console.log(`Added: ${changes.addedCount}, Removed: ${changes.removedCount}`);
```

---

#### `syncWith(newStore, options)`

Compare and apply changes in one operation.

**Parameters:**
- `newStore` (Store): New data store
- `options` (Object, optional): Sync options
  - `dryRun` (boolean): Don't actually apply changes. Default: false

**Returns:** `Object` - Apply result with comparison info

**Example:**
```javascript
const result = delta.syncWith(newStore, { dryRun: false });
console.log(`Synced: +${result.added} -${result.removed}`);
```

---

#### `apply(changes, options)`

Apply changes to the current store.

**Parameters:**
- `changes` (Object): Changes to apply
  - `added` (Store): Quads to add
  - `removed` (Store): Quads to remove
- `options` (Object, optional): Apply options
  - `dryRun` (boolean): Don't actually apply changes. Default: false

**Returns:** `Object` - Apply result
- `success` (boolean): Success flag
- `added` (number): Number of quads added
- `removed` (number): Number of quads removed
- `originalSize` (number): Original store size
- `finalSize` (number): Final store size
- `dryRun` (boolean): Whether this was a dry run

**Example:**
```javascript
const changes = delta.compareWith(newStore);
const result = delta.apply(changes, { dryRun: false });

console.log(`Applied: +${result.added} -${result.removed}`);
console.log(`Store size: ${result.originalSize} → ${result.finalSize}`);
```

---

#### `getStats(changes)`

Get statistics about changes.

**Parameters:**
- `changes` (Object): Changes object

**Returns:** `Object` - Statistics
- `added` (Object): Added statistics
- `removed` (Object): Removed statistics
- `total` (Object): Total statistics
- `coverage` (Object): Coverage information

**Example:**
```javascript
const stats = delta.getStats(changes);
console.log('Net change:', stats.total.netChange);
console.log('Affected subjects:', stats.coverage.addedSubjects);
```

---

#### `merge(...changeSets)`

Merge multiple change sets.

**Parameters:**
- `...changeSets` (Object): Change sets to merge

**Returns:** `Object` - Merged changes

**Example:**
```javascript
const merged = delta.merge(changes1, changes2, changes3);
```

---

#### `invert(changes)`

Invert changes (swap added and removed).

**Parameters:**
- `changes` (Object): Changes to invert

**Returns:** `Object` - Inverted changes

**Example:**
```javascript
const inverted = delta.invert(changes);
// Apply inverted to undo changes
delta.apply(inverted);
```

---

## Term Operations

### `useTerms()`

Create and manipulate RDF terms.

**Returns:** `Object` - Terms interface

**Example:**
```javascript
import { initStore, useTerms } from 'unrdf';

const runApp = initStore();

runApp(() => {
  const terms = useTerms();

  const alice = terms.namedNode('http://example.org/alice');
  const name = terms.literal('Alice');
});
```

---

## Prefix Operations

### `usePrefixes()`

Manage namespace prefixes.

**Returns:** `Object` - Prefixes interface

**Example:**
```javascript
import { initStore, usePrefixes } from 'unrdf';

const runApp = initStore();

runApp(() => {
  const prefixes = usePrefixes();

  prefixes.register('ex', 'http://example.org/');
  prefixes.register('foaf', 'http://xmlns.com/foaf/0.1/');

  const expanded = prefixes.expand('ex:alice');
  // => 'http://example.org/alice'
});
```

---

## Validation Operations

### `useValidator()`

SHACL validation operations.

**Returns:** `Object` - Validator interface

**Example:**
```javascript
import { initStore, useValidator } from 'unrdf';

const runApp = initStore();

runApp(() => {
  const validator = useValidator();

  const report = validator.validate(shapesStore);
  console.log('Valid:', report.conforms);
});
```

---

## Reasoning Operations

### `useReasoner()`

Inference and reasoning operations.

**Returns:** `Object` - Reasoner interface

**Example:**
```javascript
import { initStore, useReasoner } from 'unrdf';

const runApp = initStore();

runApp(() => {
  const reasoner = useReasoner();

  const inferred = reasoner.inferTransitive(
    'http://www.w3.org/2000/01/rdf-schema#subClassOf'
  );
});
```

---

## Canonicalization Operations

### `useCanon()`

RDF canonicalization using URDNA2015.

**Returns:** `Object` - Canonicalization interface

**Example:**
```javascript
import { initStore, useCanon } from 'unrdf';

const runApp = initStore();

runApp(async () => {
  const canon = useCanon();

  const canonical = await canon.canonicalize();
  const hash = await canon.hash();

  console.log('Canonical hash:', hash);
});
```

---

## Type Safety Operations

### `useZod()`

Zod-based type validation for RDF data.

**Returns:** `Object` - Zod validation interface

**Example:**
```javascript
import { initStore, useZod } from 'unrdf';
import { z } from 'zod';

const runApp = initStore();

runApp(() => {
  const zodRdf = useZod();

  const PersonSchema = z.object({
    name: z.string(),
    age: z.number(),
    knows: z.array(z.string())
  });

  const person = zodRdf.validate('http://example.org/alice', PersonSchema);
});
```

---

## Best Practices

### Composable Composition

Combine multiple composables for complex operations:

```javascript
const graph = useGraph();
const delta = useDelta();
const turtle = useTurtle();

// Load data
turtle.loadAll();

// Query data
const results = await graph.select('SELECT * WHERE { ?s ?p ?o }');

// Track changes
const newStore = turtle.load('updates');
const changes = delta.compareWith(newStore);
delta.apply(changes);
```

### Error Handling

Always handle errors from async operations:

```javascript
try {
  const results = await graph.select(sparqlQuery);
} catch (error) {
  console.error('Query failed:', error.message);
}
```

### Performance

Use deterministic mode for reproducible results:

```javascript
const delta = useDelta({ deterministic: true });
const graph = useGraph();

const results = await graph.query(sparql, { deterministic: true });
```
