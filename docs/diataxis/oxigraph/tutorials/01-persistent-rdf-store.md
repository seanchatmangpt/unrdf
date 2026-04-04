# Tutorial 01: Build Your First Oxigraph Store

In this tutorial you will install `@unrdf/oxigraph`, create an in-memory RDF store, add triples about some people, run three types of SPARQL query against the data, and export the result as Turtle. By the end you will have a complete, working script that you can run with Node.js.

Estimated time: 15 minutes.

## What You Need

- Node.js 18 or later
- pnpm (`npm install -g pnpm` if not installed)

## What Is Oxigraph?

Oxigraph is a SPARQL 1.1 engine written in Rust. The `@unrdf/oxigraph` package ships it compiled to WebAssembly so it runs inside Node.js with near-native speed. You get:

- A quad store that holds your RDF data in memory
- Full SPARQL SELECT, CONSTRUCT, DESCRIBE, and ASK support
- Load and dump operations for Turtle, N-Triples, N-Quads, TriG, JSON-LD, and RDF/XML

RDF data is made of **triples**: subject → predicate → object. A triple says something about a resource: `<Alice> <hasName> "Alice Smith"`. A quad adds a fourth component — the named graph the triple belongs to.

## Step 1 — Create the Project

```bash
mkdir oxigraph-tutorial
cd oxigraph-tutorial
pnpm init -y
pnpm add @unrdf/oxigraph
```

Create a file called `tutorial.mjs`:

```bash
touch tutorial.mjs
```

## Step 2 — Create a Store and Add Triples

Open `tutorial.mjs` and paste this content:

```javascript
import { createStore, dataFactory } from '@unrdf/oxigraph';

// Create an empty store
const store = createStore();

// dataFactory creates RDF terms
const { namedNode, literal, triple } = dataFactory;

// Common namespace prefixes
const foaf = s => namedNode(`http://xmlns.com/foaf/0.1/${s}`);
const ex = s => namedNode(`http://example.org/${s}`);
const schema = s => namedNode(`http://schema.org/${s}`);
const rdfType = namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');

// Describe Alice
store.add(triple(ex('alice'), rdfType, foaf('Person')));
store.add(triple(ex('alice'), foaf('name'), literal('Alice Smith')));
store.add(triple(ex('alice'), schema('email'), literal('alice@example.org')));
store.add(triple(ex('alice'), foaf('age'), literal('30')));

// Describe Bob
store.add(triple(ex('bob'), rdfType, foaf('Person')));
store.add(triple(ex('bob'), foaf('name'), literal('Bob Jones')));
store.add(triple(ex('bob'), schema('email'), literal('bob@example.org')));
store.add(triple(ex('bob'), foaf('age'), literal('25')));

// Alice knows Bob
store.add(triple(ex('alice'), foaf('knows'), ex('bob')));

console.log(`Store contains ${store.size} triples`);
```

Run it:

```bash
node tutorial.mjs
```

Expected output:

```
Store contains 9 triples
```

## Step 3 — Run a SELECT Query

SELECT queries return bindings — rows of variable-to-value mappings.

Add this to `tutorial.mjs`:

```javascript
// --- SELECT ---
const selectQuery = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX schema: <http://schema.org/>

  SELECT ?name ?age ?email WHERE {
    ?person a foaf:Person ;
            foaf:name  ?name ;
            foaf:age   ?age ;
            schema:email ?email .
  }
  ORDER BY ASC(?age)
`;

const people = store.query(selectQuery);

console.log('\nPeople in the store:');
for (const binding of people) {
  console.log(
    `  ${binding.get('name').value}, age ${binding.get('age').value}, ${binding.get('email').value}`
  );
}
```

Run again:

```bash
node tutorial.mjs
```

New output:

```
People in the store:
  Bob Jones, age 25, bob@example.org
  Alice Smith, age 30, alice@example.org
```

Each element in `people` is a `Map`-like object. Use `.get('variableName')` to retrieve a term, then `.value` to get its string representation.

## Step 4 — Run an ASK Query

ASK queries return `true` or `false` — a quick existence check.

```javascript
// --- ASK ---
const aliceExists = store.query(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  ASK { <http://example.org/alice> a foaf:Person }
`);

const carolExists = store.query(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  ASK { <http://example.org/carol> a foaf:Person }
`);

console.log(`\nAlice is in the store: ${aliceExists}`); // true
console.log(`Carol is in the store: ${carolExists}`); // false
```

## Step 5 — Run a CONSTRUCT Query

CONSTRUCT queries return an array of new triples assembled by the query. Use them to reshape or extract a subgraph.

```javascript
// --- CONSTRUCT ---
const schemaGraph = store.query(`
  PREFIX foaf:   <http://xmlns.com/foaf/0.1/>
  PREFIX schema: <http://schema.org/>

  CONSTRUCT {
    ?person schema:name  ?name ;
            schema:email ?email .
  }
  WHERE {
    ?person a foaf:Person ;
            foaf:name  ?name ;
            schema:email ?email .
  }
`);

console.log(`\nConstructed ${schemaGraph.length} schema triples`);
for (const quad of schemaGraph) {
  console.log(
    `  ${quad.subject.value} → ${quad.predicate.value.split('/').pop()} → ${quad.object.value}`
  );
}
```

Expected output:

```
Constructed 4 schema triples
  http://example.org/alice → name → Alice Smith
  http://example.org/alice → email → alice@example.org
  http://example.org/bob → name → Bob Jones
  http://example.org/bob → email → bob@example.org
```

## Step 6 — Export to Turtle

Use `store.dump()` to serialize the whole store to a string.

```javascript
// --- DUMP ---
const turtle = store.dump({ format: 'text/turtle' });

console.log('\nTurtle export:');
console.log(turtle);
```

You can write the output to a file:

```javascript
import { writeFileSync } from 'node:fs';
writeFileSync('people.ttl', turtle);
console.log('Saved people.ttl');
```

## Step 7 — Verify the Round-Trip

Load the exported Turtle back into a fresh store and confirm the count matches.

```javascript
const freshStore = createStore();
freshStore.load(turtle, { format: 'text/turtle' });

console.log(`\nRound-trip check: ${freshStore.size} triples (expected 9)`);
```

Expected:

```
Round-trip check: 9 triples (expected 9)
```

## What You Accomplished

- Installed `@unrdf/oxigraph` and imported `createStore` and `dataFactory`
- Created RDF terms with `namedNode`, `literal`, and `triple`
- Added triples to a store with `store.add()`
- Ran SELECT, ASK, and CONSTRUCT SPARQL queries with `store.query()`
- Serialized the store to Turtle with `store.dump()` and reloaded it with `store.load()`

## Complete Script

The full `tutorial.mjs` file:

```javascript
import { createStore, dataFactory } from '@unrdf/oxigraph';

const store = createStore();
const { namedNode, literal, triple } = dataFactory;
const foaf = s => namedNode(`http://xmlns.com/foaf/0.1/${s}`);
const ex = s => namedNode(`http://example.org/${s}`);
const schema = s => namedNode(`http://schema.org/${s}`);
const rdfType = namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');

store.add(triple(ex('alice'), rdfType, foaf('Person')));
store.add(triple(ex('alice'), foaf('name'), literal('Alice Smith')));
store.add(triple(ex('alice'), schema('email'), literal('alice@example.org')));
store.add(triple(ex('alice'), foaf('age'), literal('30')));
store.add(triple(ex('bob'), rdfType, foaf('Person')));
store.add(triple(ex('bob'), foaf('name'), literal('Bob Jones')));
store.add(triple(ex('bob'), schema('email'), literal('bob@example.org')));
store.add(triple(ex('bob'), foaf('age'), literal('25')));
store.add(triple(ex('alice'), foaf('knows'), ex('bob')));

console.log(`Store contains ${store.size} triples`);

const people = store.query(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX schema: <http://schema.org/>
  SELECT ?name ?age ?email WHERE {
    ?person a foaf:Person ;
            foaf:name ?name ;
            foaf:age ?age ;
            schema:email ?email .
  } ORDER BY ASC(?age)
`);
for (const b of people) {
  console.log(`  ${b.get('name').value}, age ${b.get('age').value}`);
}

const turtle = store.dump({ format: 'text/turtle' });
const freshStore = createStore();
freshStore.load(turtle, { format: 'text/turtle' });
console.log(`Round-trip: ${freshStore.size} triples`);
```

## Next Steps

- See [how-to/01-configure-persistent-backend.md](../how-to/01-configure-persistent-backend.md) to add query caching.
- See [how-to/03-bulk-load-turtle-file.md](../how-to/03-bulk-load-turtle-file.md) to load large existing Turtle files.
- See [reference/oxigraph-store-api.md](../reference/oxigraph-store-api.md) for the complete API.
