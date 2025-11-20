# Store Context Pattern

UNRDF uses a **context-based architecture** powered by [unctx](https://github.com/unjs/unctx) to manage RDF stores across async operations. This chapter explains the Store Context pattern and how it enables clean, composable RDF applications.

## The Problem: Shared State in RDF

Traditional RDF libraries face a common challenge:

```javascript
// ❌ Problem: Passing stores everywhere
function createPerson(store, name) {
  // Need to pass store to every function
}

function queryPeople(store) {
  // Store coupling throughout codebase
}

function validateData(store, shapes) {
  // Tight coupling to store instance
}
```

This approach leads to:
- **Boilerplate**: Passing stores as parameters everywhere
- **Coupling**: Functions tightly bound to store instances
- **Async Issues**: Context lost across async boundaries
- **Testing Complexity**: Mocking and isolating becomes difficult

## The Solution: Store Context

UNRDF solves this with **async-aware context** using unctx:

```javascript
import { initStore, useStoreContext, useGraph } from 'unrdf';

// Initialize context once
const runApp = initStore();

// All operations use the same store automatically
runApp(() => {
  const ctx = useStoreContext();  // Access store anywhere
  const graph = useGraph();        // Composables share context

  // No need to pass store around!
});
```

### Key Benefits

1. **Context Preservation**: Works across async/await boundaries
2. **Composability**: Functions don't need store parameters
3. **Type Safety**: Context ensures store is always available
4. **Isolation**: Each context is independent (great for testing)
5. **Performance**: Single store instance, minimal overhead

## Core Concepts

### 1. `initStore()` - Context Initialization

Creates a store context and returns a function to run code within that context:

```javascript
import { initStore } from 'unrdf';

// Initialize with empty store
const runApp = initStore();

// Initialize with initial quads
const runApp = initStore([quad1, quad2, quad3]);

// Initialize with options
const runApp = initStore([], {
  baseIRI: 'http://example.org/'
});
```

**Source Reference** (`src/context/index.mjs:473-479`):

```javascript
export function initStore(initialQuads = [], options = {}) {
  const context = createStoreContext(initialQuads, options);

  return (fn) => {
    return storeContext.callAsync(context, fn);
  };
}
```

### 2. `runApp()` - Context Execution

The function returned by `initStore()` executes code within the store context:

```javascript
const runApp = initStore();

// Everything inside this callback has access to the store
runApp(() => {
  // Store context is active here
  const ctx = useStoreContext();
  const graph = useGraph();

  // All operations use the same store
});
```

### 3. `useStoreContext()` - Context Access

Access the current store context from anywhere:

```javascript
import { useStoreContext } from 'unrdf';

function addPerson(name, age) {
  // Access store from context - no parameters needed!
  const ctx = useStoreContext();

  const person = ctx.namedNode(`http://example.org/people/${name}`);
  const foafName = ctx.namedNode('http://xmlns.com/foaf/0.1/name');
  const foafAge = ctx.namedNode('http://xmlns.com/foaf/0.1/age');

  ctx.add(
    ctx.quad(person, foafName, ctx.literal(name)),
    ctx.quad(person, foafAge, ctx.literal(String(age), 'http://www.w3.org/2001/XMLSchema#integer'))
  );
}

const runApp = initStore();
runApp(() => {
  addPerson('Alice', 30);  // No store parameter!
  addPerson('Bob', 28);

  const ctx = useStoreContext();
  console.log(`Store size: ${ctx.store.size}`);  // 4 quads
});
```

## Context API

The `StoreContext` provides both **SENDER** (write) and **READER** (read) operations:

### SENDER Operations (Primary)

These operations modify the store:

```javascript
const runApp = initStore();

runApp(() => {
  const ctx = useStoreContext();

  // Add quads to store
  ctx.add(quad1, quad2, quad3);

  // Remove quads from store
  ctx.remove(quad1);

  // Clear all quads
  ctx.clear();
});
```

**Source Reference** (`src/context/index.mjs:107-155`):

```javascript
/**
 * Add quads to the store (SENDER operation)
 */
add(...quads) {
  for (const q of quads) {
    if (q === null || q === undefined) {
      throw new TypeError("[StoreContext] Cannot add null or undefined quad");
    }
    if (typeof q !== "object" || !q.termType) {
      throw new TypeError("[StoreContext] Invalid quad");
    }
    store.add(q);
  }
  return this;
},

/**
 * Remove quads from the store (SENDER operation)
 */
remove(...quads) {
  for (const q of quads) {
    if (q === null || q === undefined) {
      throw new TypeError("[StoreContext] Cannot remove null or undefined quad");
    }
    if (typeof q !== "object" || !q.termType) {
      throw new TypeError("[StoreContext] Invalid quad");
    }
    store.delete(q);
  }
  return this;
},
```

### Term Creation

Create RDF terms (URIs, literals, blank nodes, quads):

```javascript
runApp(() => {
  const ctx = useStoreContext();

  // Create named node (URI)
  const person = ctx.namedNode('http://example.org/alice');

  // Create literal (string value)
  const name = ctx.literal('Alice Smith');

  // Create typed literal
  const age = ctx.literal('30', 'http://www.w3.org/2001/XMLSchema#integer');

  // Create blank node
  const address = ctx.blankNode('addr1');

  // Create quad
  const quad = ctx.quad(person,
    ctx.namedNode('http://xmlns.com/foaf/0.1/name'),
    name
  );
});
```

### READER Operations (Optional)

These operations query the store:

```javascript
runApp(async () => {
  const ctx = useStoreContext();

  // Get store statistics
  const stats = ctx.stats();
  console.log(stats);
  // { quads: 10, subjects: 5, predicates: 3, objects: 8, graphs: 1 }

  // Serialize store
  const turtle = ctx.serialize({ format: 'Turtle' });
  const nquads = ctx.serialize({ format: 'N-Quads' });

  // Execute SPARQL query
  const result = await ctx.query(`
    SELECT ?name WHERE {
      ?person <http://xmlns.com/foaf/0.1/name> ?name .
    }
  `);

  // Canonicalize (for hash-based verification)
  const canonical = await ctx.canonicalize();

  // Generate hash
  const hash = await ctx.hash();
});
```

## Context Isolation

Each `runApp()` call creates an **isolated context**:

```javascript
import { initStore, useStoreContext } from 'unrdf';

// Context 1
const runApp1 = initStore();
runApp1(() => {
  const ctx = useStoreContext();
  ctx.add(ctx.quad(
    ctx.namedNode('http://example.org/alice'),
    ctx.namedNode('http://xmlns.com/foaf/0.1/name'),
    ctx.literal('Alice')
  ));

  console.log(`App1 store size: ${ctx.store.size}`);  // 1
});

// Context 2 (completely independent)
const runApp2 = initStore();
runApp2(() => {
  const ctx = useStoreContext();
  console.log(`App2 store size: ${ctx.store.size}`);  // 0

  ctx.add(ctx.quad(
    ctx.namedNode('http://example.org/bob'),
    ctx.namedNode('http://xmlns.com/foaf/0.1/name'),
    ctx.literal('Bob')
  ));

  console.log(`App2 store size: ${ctx.store.size}`);  // 1
});

// App1 and App2 have separate stores!
```

This isolation is crucial for:
- **Testing**: Each test gets a clean context
- **Multi-tenancy**: Separate data per user/request
- **Modularity**: Independent components

## Async Context Preservation

The power of unctx is **async context preservation**:

```javascript
const runApp = initStore();

runApp(async () => {
  const ctx = useStoreContext();

  // Add data
  ctx.add(ctx.quad(
    ctx.namedNode('http://example.org/alice'),
    ctx.namedNode('http://xmlns.com/foaf/0.1/name'),
    ctx.literal('Alice')
  ));

  // Context preserved across await!
  await new Promise(resolve => setTimeout(resolve, 100));

  // Still has access to the same store
  const ctx2 = useStoreContext();
  console.log(ctx === ctx2);  // true
  console.log(ctx.store.size);  // 1

  // Works with async functions
  async function queryData() {
    // Context automatically available
    const ctx = useStoreContext();
    return await ctx.query('SELECT ?s ?p ?o WHERE { ?s ?p ?o }');
  }

  const results = await queryData();
  console.log(results);
});
```

**Without async context**, this would fail:

```javascript
// ❌ Traditional approach loses context
let globalStore = new Store();

async function doWork() {
  await someAsyncOp();
  // globalStore might have changed!
  globalStore.add(quad);
}
```

## Composable Integration

All UNRDF composables use the store context automatically:

```javascript
import {
  initStore,
  useStoreContext,
  useGraph,
  useTurtle,
  useDelta
} from 'unrdf';

const runApp = initStore();

runApp(async () => {
  // All composables share the same store
  const ctx = useStoreContext();
  const graph = useGraph();
  const turtle = useTurtle('./graph');
  const delta = useDelta();

  // Add data via context
  ctx.add(ctx.quad(
    ctx.namedNode('http://example.org/alice'),
    ctx.namedNode('http://xmlns.com/foaf/0.1/name'),
    ctx.literal('Alice')
  ));

  // Query via graph composable
  const results = await graph.select(`
    SELECT ?name WHERE {
      ?person <http://xmlns.com/foaf/0.1/name> ?name .
    }
  `);

  console.log(results);  // [{ name: 'Alice' }]

  // Save via turtle composable
  turtle.save('people');

  // Track changes via delta composable
  const newData = await turtle.parse(`
    @prefix ex: <http://example.org/> .
    @prefix foaf: <http://xmlns.com/foaf/0.1/> .

    ex:alice foaf:age 30 .
  `);

  const changes = delta.compareWith(newData);
  console.log(`Added ${changes.addedCount} quads`);
});
```

**Source Reference** (`src/composables/use-turtle.mjs:39-48`):

```javascript
export function useTurtle(graphDir = "./graph", options = {}) {
  // Get the engine from context
  const storeContext = useStoreContext();
  const engine = storeContext.engine;

  // All operations use the context store
  return {
    get store() {
      return storeContext;
    },
    // ... methods use storeContext.store
  };
}
```

## Memory Management

The store context manages memory efficiently:

```javascript
const runApp = initStore();

runApp(() => {
  const ctx = useStoreContext();

  // Add 1 million quads
  for (let i = 0; i < 1_000_000; i++) {
    ctx.add(ctx.quad(
      ctx.namedNode(`http://example.org/item${i}`),
      ctx.namedNode('http://xmlns.com/foaf/0.1/name'),
      ctx.literal(`Item ${i}`)
    ));
  }

  console.log(`Store size: ${ctx.store.size}`);  // 1,000,000

  // Clear when done
  ctx.clear();

  console.log(`Store size: ${ctx.store.size}`);  // 0
});

// Context automatically cleaned up after runApp() completes
```

## Real-World Examples

### Example 1: Multi-Function Pipeline

```javascript
import { initStore, useStoreContext, useGraph } from 'unrdf';

// Step 1: Load data
async function loadPeople() {
  const ctx = useStoreContext();

  const people = [
    { name: 'Alice', age: 30 },
    { name: 'Bob', age: 28 },
    { name: 'Carol', age: 32 }
  ];

  for (const person of people) {
    const uri = ctx.namedNode(`http://example.org/${person.name.toLowerCase()}`);
    ctx.add(
      ctx.quad(uri, ctx.namedNode('http://xmlns.com/foaf/0.1/name'), ctx.literal(person.name)),
      ctx.quad(uri, ctx.namedNode('http://xmlns.com/foaf/0.1/age'), ctx.literal(String(person.age), 'http://www.w3.org/2001/XMLSchema#integer'))
    );
  }
}

// Step 2: Query data
async function findPeopleOver25() {
  const graph = useGraph();

  return await graph.select(`
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

    SELECT ?name ?age WHERE {
      ?person foaf:name ?name ;
              foaf:age ?age .
      FILTER (?age > 25)
    }
  `);
}

// Step 3: Pipeline
const runApp = initStore();

runApp(async () => {
  await loadPeople();

  const results = await findPeopleOver25();

  console.log('People over 25:');
  for (const row of results) {
    console.log(`- ${row.name}: ${row.age}`);
  }

  const ctx = useStoreContext();
  console.log(`Total quads: ${ctx.store.size}`);
});
```

### Example 2: Testing with Isolated Contexts

```javascript
import { describe, it, expect } from 'vitest';
import { initStore, useStoreContext } from 'unrdf';

describe('Person Management', () => {
  it('should add person', () => {
    const runApp = initStore();

    runApp(() => {
      const ctx = useStoreContext();

      const alice = ctx.namedNode('http://example.org/alice');
      const name = ctx.namedNode('http://xmlns.com/foaf/0.1/name');

      ctx.add(ctx.quad(alice, name, ctx.literal('Alice')));

      expect(ctx.store.size).toBe(1);
    });
  });

  it('should be isolated from previous test', () => {
    const runApp = initStore();

    runApp(() => {
      const ctx = useStoreContext();

      // New context - completely empty
      expect(ctx.store.size).toBe(0);
    });
  });
});
```

### Example 3: Request-Scoped Data (Server)

```javascript
import { initStore, useStoreContext } from 'unrdf';

export async function handleRequest(req, res) {
  // Each request gets isolated context
  const runApp = initStore();

  await runApp(async () => {
    const ctx = useStoreContext();

    // Load user data
    const userId = req.params.userId;
    const userData = await fetchUserData(userId);

    // Build RDF graph
    const user = ctx.namedNode(`http://example.org/users/${userId}`);
    ctx.add(
      ctx.quad(user, ctx.namedNode('http://xmlns.com/foaf/0.1/name'), ctx.literal(userData.name)),
      ctx.quad(user, ctx.namedNode('http://xmlns.com/foaf/0.1/mbox'), ctx.namedNode(`mailto:${userData.email}`))
    );

    // Process and respond
    const turtle = ctx.serialize({ format: 'Turtle' });
    res.setHeader('Content-Type', 'text/turtle');
    res.send(turtle);
  });

  // Context automatically cleaned up after response
}
```

## Best Practices

### 1. Always Initialize Context

```javascript
// ✅ Good: Always use initStore()
const runApp = initStore();
runApp(() => {
  const ctx = useStoreContext();
  // Work with context
});

// ❌ Avoid: Using composables without context
try {
  const ctx = useStoreContext();  // Throws error!
} catch (err) {
  console.error('Context not initialized');
}
```

### 2. One Context Per Scope

```javascript
// ✅ Good: One context for entire operation
const runApp = initStore();
runApp(async () => {
  await step1();
  await step2();
  await step3();
});

// ❌ Avoid: Multiple nested contexts (usually)
runApp(() => {
  const runApp2 = initStore();  // Nested context - rarely needed
  runApp2(() => {
    // ...
  });
});
```

### 3. Use Composables Inside Context

```javascript
// ✅ Good: Composables inside runApp()
const runApp = initStore();
runApp(() => {
  const graph = useGraph();
  const turtle = useTurtle();
  // Use composables
});

// ❌ Avoid: Composables outside context
const graph = useGraph();  // Error: Context not initialized
```

### 4. Clean Up When Done

```javascript
// ✅ Good: Clear large datasets
runApp(() => {
  const ctx = useStoreContext();

  // Process large dataset
  processMillionsOfQuads();

  // Clear when done
  ctx.clear();
});

// Context automatically cleaned up after runApp()
```

## Common Patterns

### Pattern 1: Load-Process-Save

```javascript
const runApp = initStore();

runApp(() => {
  const turtle = useTurtle('./data');
  const graph = useGraph();

  // Load
  turtle.loadAll();

  // Process
  const results = await graph.select(`
    SELECT ?s ?p ?o WHERE { ?s ?p ?o }
  `);

  // Transform
  // ... modify data ...

  // Save
  turtle.save('processed-data');
});
```

### Pattern 2: Validate-Transform-Export

```javascript
const runApp = initStore();

runApp(async () => {
  const ctx = useStoreContext();
  const graph = useGraph();

  // Load data
  // ...

  // Validate
  const shapes = await parseTurtle(shaclShapes);
  const report = graph.validate(shapes);

  if (!report.conforms) {
    throw new Error('Validation failed');
  }

  // Transform
  const transformed = await graph.construct(`
    CONSTRUCT {
      ?s <http://example.org/valid> true .
    } WHERE {
      ?s ?p ?o .
    }
  `);

  // Export
  const jsonld = await toJsonLd(transformed);
  console.log(jsonld);
});
```

## Summary

- **Store Context** manages RDF stores using unctx async context
- **`initStore()`** creates isolated contexts for each operation
- **`useStoreContext()`** accesses store from anywhere (no parameters needed)
- **Async Preservation** maintains context across async/await
- **Composability** enables clean, decoupled functions
- **Isolation** perfect for testing and multi-tenancy
- **Memory Efficient** automatic cleanup after context ends

The Store Context pattern is the foundation that makes UNRDF's composable architecture possible.

## Next Steps

- **[Composables](./composables.md)** - Learn about UNRDF's composable functions
- **[Transactions](./transactions.md)** - Master atomic operations with transactions
- **[RDF Fundamentals](./rdf-fundamentals.md)** - Review RDF basics if needed
