# Tutorial 3: Composables & Context

**Time to Complete**: 15 minutes
**Difficulty**: Intermediate
**Prerequisites**: Tutorial 2 completed

## What You'll Learn

In this tutorial, you'll learn how to:
- Use the context system with `initStore()`
- Access the Store with `useStore()`
- Query data with `useGraph()`
- Create RDF terms with `useTerms()`
- Parse and serialize with `useTurtle()`

## Introduction

UNRDF uses a context-based architecture (powered by unctx) that eliminates prop drilling and makes it easy to share a single RDF Store across your application. Composables are functions that access this shared context.

**Key Concept**: You initialize a Store once, then any composable can access it without passing it around.

## Step 1: Initialize the Context

Use `initStore()` to create a context and run code within it:

```javascript
import { initStore, useStore } from 'unrdf';

// Initialize a context with an empty Store
const runApp = initStore();

// Run code within the context
runApp(() => {
  const store = useStore();
  console.log(`Store size: ${store.size}`);
  console.log(`Store ready: ${store !== null}`);
});
```

**Output:**
```
Store size: 0
Store ready: true
```

**What happened?**
- `initStore()` creates a context with a new N3 Store
- Returns a function that runs code within that context
- `useStore()` accesses the Store from the context
- No need to pass the Store as a parameter!

## Step 2: Initialize with Data

You can initialize the context with existing quads or Turtle data:

```javascript
import { initStore, useStore } from 'unrdf';
import { DataFactory } from 'n3';

const { namedNode, literal, quad } = DataFactory;

// Create some initial data
const initialQuads = [
  quad(
    namedNode('http://example.org/alice'),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal('Alice Smith')
  ),
];

// Initialize with data
const runApp = initStore(initialQuads, {
  baseIRI: 'http://example.org/',
});

runApp(() => {
  const store = useStore();
  console.log(`Store initialized with ${store.size} triples`);
});
```

**Output:**
```
Store initialized with 1 triples
```

## Step 3: Query Data with useGraph()

`useGraph()` provides a high-level API for querying:

```javascript
import { initStore, useGraph } from 'unrdf';
import { parseTurtle } from 'unrdf/knowledge-engine';

// Sample data
const turtleData = `
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:alice a foaf:Person ;
  foaf:name "Alice Smith" ;
  foaf:age 30 .

ex:bob a foaf:Person ;
  foaf:name "Bob Jones" ;
  foaf:age 25 .
`;

// Parse data
const store = await parseTurtle(turtleData);

// Initialize context with parsed data
const runApp = initStore(store.getQuads());

runApp(async () => {
  const graph = useGraph();

  // Execute SELECT query
  const results = await graph.select(`
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT ?name ?age WHERE {
      ?person foaf:name ?name ;
              foaf:age ?age .
    }
  `);

  console.log('Query results:');
  results.forEach(row => {
    console.log(`- ${row.name.value}, age ${row.age.value}`);
  });

  // Execute ASK query
  const hasAlice = await graph.ask(`
    PREFIX ex: <http://example.org/>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    ASK { ex:alice foaf:name "Alice Smith" }
  `);

  console.log(`\nHas Alice? ${hasAlice}`);

  // Get statistics
  console.log(`\nGraph size: ${graph.size}`);
  console.log('Graph stats:', graph.stats());
});
```

**Output:**
```
Query results:
- Alice Smith, age 30
- Bob Jones, age 25

Has Alice? true

Graph size: 6
Graph stats: { subjects: 2, predicates: 3, objects: 6 }
```

## Step 4: Create RDF Terms with useTerms()

`useTerms()` provides helpers for creating RDF terms:

```javascript
import { initStore, useStore, useTerms } from 'unrdf';

const runApp = initStore();

runApp(() => {
  const store = useStore();
  const terms = useTerms();

  // Create RDF terms
  const subject = terms.iri('http://example.org/person1');
  const predicate = terms.iri('http://xmlns.com/foaf/0.1/name');
  const object = terms.lit('John Doe');

  // Create a quad
  const quad = terms.quad(subject, predicate, object);

  // Add to store
  store.add(quad);

  console.log(`Added quad, store size: ${store.size}`);

  // Create typed literals
  const age = terms.lit('30', 'http://www.w3.org/2001/XMLSchema#integer');
  const ageQuad = terms.quad(
    subject,
    terms.iri('http://xmlns.com/foaf/0.1/age'),
    age
  );

  store.add(ageQuad);
  console.log(`Store size after adding age: ${store.size}`);
});
```

**Output:**
```
Added quad, store size: 1
Store size after adding age: 2
```

**useTerms() API:**
- `terms.iri(uri)` - Create a NamedNode
- `terms.lit(value, datatype?)` - Create a Literal
- `terms.blank(id?)` - Create a BlankNode
- `terms.quad(s, p, o, g?)` - Create a Quad

## Step 5: Parse and Serialize with useTurtle()

`useTurtle()` provides parsing and serialization helpers:

```javascript
import { initStore, useTurtle } from 'unrdf';

const runApp = initStore();

runApp(async () => {
  const turtle = useTurtle();

  // Parse Turtle
  const turtleData = `
    @prefix ex: <http://example.org/> .
    ex:subject ex:predicate ex:object .
  `;

  const store = await turtle.parse(turtleData);
  console.log(`Parsed ${store.size} triples`);

  // Serialize to Turtle
  const serialized = await turtle.serialize(store);
  console.log('Serialized:');
  console.log(serialized);
});
```

## Step 6: Build a Composable Application

Let's combine everything into a practical example:

```javascript
import { initStore, useStore, useGraph, useTerms, useTurtle } from 'unrdf';

async function buildSocialNetwork() {
  // Initialize empty context
  const runApp = initStore([], {
    baseIRI: 'http://example.org/',
  });

  await runApp(async () => {
    const store = useStore();
    const terms = useTerms();
    const graph = useGraph();
    const turtle = useTurtle();

    console.log('=== Building Social Network ===\n');

    // Step 1: Add people
    console.log('Step 1: Adding people...');

    const alice = terms.iri('http://example.org/alice');
    const bob = terms.iri('http://example.org/bob');
    const foafPerson = terms.iri('http://xmlns.com/foaf/0.1/Person');
    const foafName = terms.iri('http://xmlns.com/foaf/0.1/name');
    const foafKnows = terms.iri('http://xmlns.com/foaf/0.1/knows');

    store.add(terms.quad(alice, terms.iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'), foafPerson));
    store.add(terms.quad(alice, foafName, terms.lit('Alice Smith')));
    store.add(terms.quad(alice, foafKnows, bob));

    store.add(terms.quad(bob, terms.iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'), foafPerson));
    store.add(terms.quad(bob, foafName, terms.lit('Bob Jones')));

    console.log(`  Added ${store.size} triples\n`);

    // Step 2: Query the network
    console.log('Step 2: Querying the network...');

    const friends = await graph.select(`
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      SELECT ?person ?name ?friend WHERE {
        ?person foaf:name ?name ;
                foaf:knows ?friend .
      }
    `);

    console.log('  Friendships found:');
    friends.forEach(row => {
      console.log(`  - ${row.name.value} knows ${row.friend.value}`);
    });
    console.log();

    // Step 3: Export to Turtle
    console.log('Step 3: Exporting to Turtle...');
    const serialized = await turtle.serialize(store);
    console.log(serialized);

    // Step 4: Statistics
    console.log('=== Statistics ===');
    console.log(`Total triples: ${graph.size}`);
    console.log('Graph stats:', graph.stats());
  });
}

// Run the application
await buildSocialNetwork();
```

**Output:**
```
=== Building Social Network ===

Step 1: Adding people...
  Added 5 triples

Step 2: Querying the network...
  Friendships found:
  - Alice Smith knows http://example.org/bob

Step 3: Exporting to Turtle...
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix ex: <http://example.org/> .

ex:alice a foaf:Person ;
  foaf:name "Alice Smith" ;
  foaf:knows ex:bob .

ex:bob a foaf:Person ;
  foaf:name "Bob Jones" .

=== Statistics ===
Total triples: 5
Graph stats: { subjects: 2, predicates: 3, objects: 5 }
```

## Understanding Context Sharing

The key benefit of the context system is that all composables share the same Store:

```javascript
const runApp = initStore();

runApp(() => {
  const store1 = useStore();
  const store2 = useStore();
  const graph1 = useGraph();
  const graph2 = useGraph();

  // These are all the same instance!
  console.log('store1 === store2:', store1 === store2);
  console.log('graph1.store === graph2.store:', graph1.store === graph2.store);
});
```

**Output:**
```
store1 === store2: true
graph1.store === graph2.store: true
```

## Error Handling

If you try to use composables outside a context, you'll get a clear error:

```javascript
import { useStore } from 'unrdf';

try {
  // This will fail - no context initialized!
  const store = useStore();
} catch (error) {
  console.log('Error:', error.message);
  // Error: Store context not initialized. Call initStore() first.
}
```

## What You Learned

- How to initialize a context with `initStore()`
- How to access the Store with `useStore()`
- How to query data with `useGraph()`
- How to create RDF terms with `useTerms()`
- How to parse and serialize with `useTurtle()`
- How composables share the same Store automatically
- How to build a complete composable application

## Next Steps

In the next tutorial, you'll learn advanced Knowledge Hook patterns including the KnowledgeHookManager, transactions, and policy packs.

**Continue to**: [Tutorial 4: Advanced Knowledge Hooks](./04-advanced-hooks.md)

## Complete Example

See the full working example at: `examples/context-example.mjs`

## Reference

- [Composables API Reference](../reference/api/composables.md)
- [Context System Explanation](../explanation/architecture/context-system.md)
- [initStore() Reference](../reference/api/context.md#initStore)
