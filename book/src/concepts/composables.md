# Composables

UNRDF provides a suite of **composable functions** that work seamlessly with the Store Context. Each composable has a specific purpose and can be combined to build powerful RDF applications.

## What are Composables?

Composables are reusable functions that:

1. **Access the Store Context** automatically (no parameters needed)
2. **Provide focused APIs** for specific RDF operations
3. **Compose together** to build complex workflows
4. **Maintain type safety** through JSDoc annotations
5. **Follow async/await** patterns consistently

Think of composables as **specialized tools** in your RDF toolkit—each designed for a specific task, all sharing the same underlying store.

## Core Composables

### `useStoreContext()`

The foundation composable that provides direct store access.

```javascript
import { initStore, useStoreContext } from 'unrdf';

const runApp = initStore();

runApp(() => {
  const ctx = useStoreContext();

  // Access underlying N3.Store
  console.log(ctx.store.size);

  // Create terms
  const person = ctx.namedNode('http://example.org/alice');
  const name = ctx.literal('Alice');

  // Add/remove quads
  ctx.add(ctx.quad(person, ctx.namedNode('http://xmlns.com/foaf/0.1/name'), name));
  ctx.remove(someQuad);
  ctx.clear();

  // Get statistics
  const stats = ctx.stats();

  // Serialize
  const turtle = ctx.serialize({ format: 'Turtle' });
});
```

**When to use:**
- Direct store manipulation
- Term creation
- Low-level operations
- Custom logic

## `useGraph()` - SPARQL & Graph Operations

High-level interface for querying and manipulating RDF graphs.

**Source Reference** (`src/composables/use-graph.mjs`)

### Basic Usage

```javascript
import { initStore, useGraph } from 'unrdf';

const runApp = initStore();

runApp(async () => {
  const graph = useGraph();

  // Execute SPARQL SELECT
  const results = await graph.select(`
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?name ?age WHERE {
      ?person foaf:name ?name ;
              foaf:age ?age .
    }
  `);

  console.log(results);
  // [{ name: 'Alice', age: 30 }, { name: 'Bob', age: 28 }]
});
```

### Query Methods

#### `query(sparql, options)` - Execute Any SPARQL Query

```javascript
const result = await graph.query(`
  SELECT ?s ?p ?o WHERE { ?s ?p ?o }
`, {
  limit: 100,
  signal: abortController.signal  // For cancellation
});
```

#### `select(sparql)` - Execute SELECT Query

```javascript
const rows = await graph.select(`
  PREFIX ex: <http://example.org/>

  SELECT ?name WHERE {
    ?person ex:name ?name .
  }
`);

// Returns: [{ name: 'Alice' }, { name: 'Bob' }]
```

#### `ask(sparql)` - Execute ASK Query

```javascript
const exists = await graph.ask(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  ASK {
    ?person foaf:name "Alice" .
  }
`);

console.log(exists);  // true or false
```

#### `construct(sparql)` - Execute CONSTRUCT Query

```javascript
const newStore = await graph.construct(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>

  CONSTRUCT {
    ?person ex:hasName ?name .
  } WHERE {
    ?person foaf:name ?name .
  }
`);

console.log(newStore.size);  // Number of constructed triples
```

#### `update(sparql)` - Execute UPDATE Query

```javascript
await graph.update(`
  PREFIX ex: <http://example.org/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  INSERT DATA {
    ex:charlie foaf:name "Charlie" .
  }
`);
```

### Graph Operations

#### `union(...graphs)` - Combine Graphs

```javascript
const graph1 = useGraph();
const graph2 = useGraph();

// Union of graphs
const combined = graph1.union(graph2);
console.log(combined.size);
```

#### `intersection(graph)` - Find Common Quads

```javascript
const common = graph1.intersection(graph2);
console.log(`${common.size} shared quads`);
```

#### `difference(graph)` - Find Unique Quads

```javascript
const unique = graph1.difference(graph2);
console.log(`${unique.size} quads only in graph1`);
```

### Validation

#### `validate(shapesInput)` - SHACL Validation

```javascript
const shapes = await parseTurtle(`
  @prefix sh: <http://www.w3.org/ns/shacl#> .
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .

  ex:PersonShape a sh:NodeShape ;
    sh:targetClass foaf:Person ;
    sh:property [
      sh:path foaf:name ;
      sh:minCount 1 ;
      sh:datatype xsd:string ;
    ] .
`);

const report = graph.validate(shapes);

if (!report.conforms) {
  console.log('Validation errors:', report.results);
}
```

#### `validateOrThrow(shapesInput)` - Validate or Throw

```javascript
try {
  graph.validateOrThrow(shapes);
  console.log('Valid!');
} catch (err) {
  console.error('Validation failed:', err.message);
}
```

### Serialization & Stats

```javascript
// Serialize to Turtle
const turtle = graph.serialize({
  format: 'Turtle',
  prefixes: {
    ex: 'http://example.org/',
    foaf: 'http://xmlns.com/foaf/0.1/'
  }
});

// Serialize to N-Quads
const nquads = graph.serialize({ format: 'N-Quads' });

// Get statistics
const stats = graph.stats();
console.log(stats);
// { quads: 100, subjects: 50, predicates: 10, objects: 80, graphs: 1 }

// Get size
console.log(graph.size);  // Number of quads
```

## `useTurtle()` - File I/O Operations

Manage Turtle files with automatic parsing and serialization.

**Source Reference** (`src/composables/use-turtle.mjs`)

### Basic Usage

```javascript
import { initStore, useTurtle } from 'unrdf';

const runApp = initStore();

runApp(() => {
  const turtle = useTurtle('./graph', {
    baseIRI: 'http://example.org/',
    autoLoad: true,
    validateOnLoad: true
  });

  // Load all .ttl files
  const { loaded, files } = turtle.loadAll();
  console.log(`Loaded ${loaded} files:`, files);

  // Save to file
  turtle.save('people', {
    prefixes: {
      ex: 'http://example.org/',
      foaf: 'http://xmlns.com/foaf/0.1/'
    },
    createBackup: true
  });
});
```

### File Operations

#### `loadAll(options)` - Load All Turtle Files

```javascript
const result = turtle.loadAll({
  merge: true,      // Merge with existing store
  validate: true    // Validate on load
});

console.log(`Loaded ${result.loaded} files`);
console.log('Files:', result.files);
```

#### `load(fileName, options)` - Load Specific File

```javascript
const store = turtle.load('people', {
  merge: true,
  validate: true
});
```

#### `save(fileName, options)` - Save to File

```javascript
turtle.save('output', {
  prefixes: {
    ex: 'http://example.org/',
    foaf: 'http://xmlns.com/foaf/0.1/'
  },
  createBackup: true
});
```

#### `saveDefault()` / `loadDefault()` - Default File

```javascript
// Save to default.ttl
turtle.saveDefault();

// Load from default.ttl
const store = turtle.loadDefault();
```

### Parsing & Serialization

#### `parse(ttl, options)` - Parse Turtle String

```javascript
const store = turtle.parse(`
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .

  ex:alice foaf:name "Alice" .
`, {
  addToStore: true  // Add parsed quads to context store
});
```

#### `serialize(options)` - Serialize to Turtle

```javascript
const turtleString = turtle.serialize({
  prefixes: {
    ex: 'http://example.org/',
    foaf: 'http://xmlns.com/foaf/0.1/'
  }
});

console.log(turtleString);
```

### Utility Methods

```javascript
// List all .ttl files
const files = turtle.listFiles();
console.log('Turtle files:', files);

// Get statistics
const stats = turtle.stats();

// Clear store
turtle.clear();

// Access properties
console.log(turtle.graphDir);   // './graph'
console.log(turtle.engine);      // RdfEngine instance
console.log(turtle.store);       // StoreContext
```

## `useDelta()` - Graph Diff & Patch

Track changes and apply patches to RDF graphs.

**Source Reference** (`src/composables/use-delta.mjs`)

### Basic Usage

```javascript
import { initStore, useDelta, useTurtle } from 'unrdf';

const runApp = initStore();

runApp(() => {
  const delta = useDelta({ deterministic: true });
  const turtle = useTurtle();

  // Original data
  turtle.parse(`
    @prefix ex: <http://example.org/> .
    ex:alice ex:age 30 .
  `, { addToStore: true });

  // New data
  const newData = turtle.parse(`
    @prefix ex: <http://example.org/> .
    ex:alice ex:age 31 .
    ex:bob ex:age 28 .
  `);

  // Compare
  const changes = delta.compareWith(newData);

  console.log(`Added: ${changes.addedCount} quads`);
  console.log(`Removed: ${changes.removedCount} quads`);
});
```

### Comparison Methods

#### `compareWith(newStore)` - Compare Stores

```javascript
const changes = delta.compareWith(newStore);

console.log(changes);
// {
//   added: Store,           // Quads to add
//   removed: Store,         // Quads to remove
//   addedCount: 2,
//   removedCount: 1,
//   unchangedCount: 5,
//   contextSize: 6,
//   newDataSize: 7
// }
```

#### `syncWith(newStore, options)` - Sync Stores

```javascript
const result = delta.syncWith(newStore, {
  dryRun: false  // Set true to preview without applying
});

console.log(`Synced: +${result.added} -${result.removed}`);
```

### Applying Changes

#### `apply(changes, options)` - Apply Changes

```javascript
const result = delta.apply(changes, {
  dryRun: false
});

console.log(result);
// {
//   success: true,
//   added: 2,
//   removed: 1,
//   originalSize: 6,
//   finalSize: 7,
//   dryRun: false
// }
```

### Change Analysis

#### `getStats(changes)` - Get Change Statistics

```javascript
const stats = delta.getStats(changes);

console.log(stats);
// {
//   added: { quads: 2, subjects: 1, predicates: 1, objects: 2 },
//   removed: { quads: 1, subjects: 1, predicates: 1, objects: 1 },
//   total: { quads: 3, netChange: 1 },
//   coverage: {
//     addedSubjects: ['http://example.org/bob'],
//     removedSubjects: [],
//     addedPredicates: ['http://example.org/age'],
//     removedPredicates: []
//   }
// }
```

#### `isEmpty(changes)` - Check if Empty

```javascript
if (delta.isEmpty(changes)) {
  console.log('No changes detected');
}
```

### Patch Operations

#### `createPatch(changes, options)` - Create Patch

```javascript
const patch = delta.createPatch(changes, {
  format: 'Turtle'  // or 'N-Quads'
});

console.log(patch);
// {
//   added: '...',       // Turtle/N-Quads string
//   removed: '...',     // Turtle/N-Quads string
//   addedCount: 2,
//   removedCount: 1,
//   format: 'Turtle',
//   stats: { ... }
// }
```

#### `applyPatch(patch, options)` - Apply Patch

```javascript
const result = delta.applyPatch(patch, {
  dryRun: false
});
```

### Advanced Operations

#### `merge(...changeSets)` - Merge Changes

```javascript
const merged = delta.merge(changes1, changes2, changes3);

console.log(`Total changes: ${merged.added.size + merged.removed.size}`);
```

#### `invert(changes)` - Invert Changes

```javascript
const inverted = delta.invert(changes);

// Apply and rollback
delta.apply(changes);       // Apply changes
delta.apply(inverted);      // Rollback
```

## `useTerms()` - Term Creation Helpers

Simplified term creation with namespace support.

```javascript
import { initStore, useTerms } from 'unrdf';

const runApp = initStore();

runApp(() => {
  const terms = useTerms();

  // Create terms
  const person = terms.iri('http://example.org/alice');
  const name = terms.lit('Alice');
  const age = terms.lit('30', 'http://www.w3.org/2001/XMLSchema#integer');
  const blank = terms.blank('b1');

  // Create quad
  const quad = terms.quad(person,
    terms.iri('http://xmlns.com/foaf/0.1/name'),
    name
  );
});
```

## `useReasoner()` - OWL Reasoning

Perform inference and reasoning over RDF graphs.

```javascript
import { initStore, useReasoner, useTurtle } from 'unrdf';

const runApp = initStore();

runApp(async () => {
  const reasoner = useReasoner();
  const turtle = useTurtle();

  // Load ontology
  turtle.parse(`
    @prefix ex: <http://example.org/> .
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

    ex:Student rdfs:subClassOf ex:Person .
    ex:alice a ex:Student .
  `, { addToStore: true });

  // Perform reasoning
  const inferred = await reasoner.reason();

  console.log(`Inferred ${inferred.size} new triples`);
  // ex:alice a ex:Person (inferred from subclass)
});
```

## `useCanon()` - Canonicalization

Canonicalize graphs using URDNA2015 algorithm.

```javascript
import { initStore, useCanon } from 'unrdf';

const runApp = initStore();

runApp(async () => {
  const canon = useCanon();

  // Canonicalize current store
  const canonical = await canon.canonicalize();

  console.log(canonical);  // Canonical N-Quads

  // Generate hash
  const hash = await canon.hash();
  console.log(`Graph hash: ${hash}`);

  // Check isomorphism
  const isIso = await canon.isIsomorphic(otherStore);
  console.log(`Isomorphic: ${isIso}`);
});
```

## `useZod()` - Type Safety Integration

Bridge RDF and Zod schemas for runtime validation.

```javascript
import { z } from 'zod';
import { initStore, useZod } from 'unrdf';

const runApp = initStore();

runApp(() => {
  const zodHelper = useZod();

  // Define schema
  const PersonSchema = z.object({
    id: z.string().url(),
    name: z.string().min(1),
    age: z.number().int().min(0).max(150).optional(),
    email: z.string().email().optional()
  });

  // Convert object to RDF
  const person = {
    id: 'http://example.org/alice',
    name: 'Alice Smith',
    age: 30,
    email: 'alice@example.org'
  };

  const quads = zodHelper.toRdf(person, PersonSchema);

  // Convert RDF back to object
  const reconstructed = zodHelper.fromRdf(quads, PersonSchema);

  console.log(reconstructed);
  // { id: '...', name: 'Alice Smith', age: 30, email: '...' }
});
```

## Composable Composition

The power of composables comes from **combining them**:

### Example: Complete ETL Pipeline

```javascript
import {
  initStore,
  useTurtle,
  useGraph,
  useDelta,
  useCanon
} from 'unrdf';

const runApp = initStore();

runApp(async () => {
  const turtle = useTurtle('./data');
  const graph = useGraph();
  const delta = useDelta();
  const canon = useCanon();

  // 1. Load data
  turtle.loadAll();

  // 2. Query and validate
  const people = await graph.select(`
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT ?name WHERE {
      ?person foaf:name ?name .
    }
  `);

  // 3. Transform
  const transformed = await graph.construct(`
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX ex: <http://example.org/>
    CONSTRUCT {
      ?person ex:fullName ?name .
    } WHERE {
      ?person foaf:name ?name .
    }
  `);

  // 4. Track changes
  const changes = delta.compareWith(transformed);
  console.log(`Changes: +${changes.addedCount} -${changes.removedCount}`);

  // 5. Generate hash
  const hash = await canon.hash();
  console.log(`Data hash: ${hash}`);

  // 6. Save result
  turtle.save('output');
});
```

### Example: Data Validation Pipeline

```javascript
runApp(async () => {
  const turtle = useTurtle('./data');
  const graph = useGraph();

  // Load data
  turtle.load('people');

  // Load SHACL shapes
  const shapes = await turtle.parse(`
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    # ... shapes ...
  `);

  // Validate
  const report = graph.validate(shapes);

  if (!report.conforms) {
    console.error('Validation failed');
    for (const result of report.results) {
      console.log(`- ${result.message}`);
    }
    return;
  }

  console.log('✓ Data is valid');

  // Process valid data
  // ...
});
```

## Best Practices

### 1. Choose the Right Composable

```javascript
// ✅ Good: Use specific composables
const graph = useGraph();        // For queries
const turtle = useTurtle();      // For file I/O
const delta = useDelta();        // For diffs

// ❌ Avoid: Using wrong composable
const ctx = useStoreContext();
// Don't manually implement what composables provide
```

### 2. Combine Composables

```javascript
// ✅ Good: Combine multiple composables
runApp(() => {
  const turtle = useTurtle('./data');
  const graph = useGraph();
  const delta = useDelta();

  turtle.loadAll();
  const results = await graph.select('...');
  const changes = delta.compareWith(newData);
});

// ❌ Avoid: Reinventing functionality
runApp(() => {
  const ctx = useStoreContext();
  // Manually implementing query logic
});
```

### 3. Use Inside Context

```javascript
// ✅ Good: Composables inside runApp()
const runApp = initStore();
runApp(() => {
  const graph = useGraph();
  // Use composable
});

// ❌ Avoid: Outside context
const graph = useGraph();  // Error!
```

### 4. Leverage Async/Await

```javascript
// ✅ Good: Use async/await
runApp(async () => {
  const results = await graph.select('...');
  const canonical = await canon.canonicalize();
});

// ❌ Avoid: Mixing callbacks
runApp(() => {
  graph.select('...').then(results => {
    // Harder to read
  });
});
```

## Creating Custom Composables

You can create your own composables following the pattern:

```javascript
import { useStoreContext } from 'unrdf';

export function useMyCustomComposable(options = {}) {
  // Access store context
  const ctx = useStoreContext();

  return {
    // Your custom methods
    doSomething() {
      // Use ctx.store, ctx.add, etc.
    },

    doSomethingElse() {
      // ...
    }
  };
}

// Usage
const runApp = initStore();
runApp(() => {
  const custom = useMyCustomComposable({ option: 'value' });
  custom.doSomething();
});
```

## Summary

- **Composables** are reusable functions for RDF operations
- **`useGraph()`** - SPARQL queries and graph operations
- **`useTurtle()`** - File I/O with Turtle format
- **`useDelta()`** - Graph diffs and patches
- **`useTerms()`** - Term creation helpers
- **`useReasoner()`** - OWL reasoning and inference
- **`useCanon()`** - Canonicalization and hashing
- **`useZod()`** - Type safety with Zod schemas
- **Composition** - Combine composables for complex workflows
- **Context-Aware** - All composables share the same store

Each composable is focused, composable, and type-safe, making UNRDF development clean and maintainable.

## Next Steps

- **[Transactions](./transactions.md)** - Learn about atomic operations
- **[Store Context](./store-context.md)** - Review context management
- **[RDF Fundamentals](./rdf-fundamentals.md)** - RDF basics
