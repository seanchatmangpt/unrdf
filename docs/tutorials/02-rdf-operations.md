# Tutorial 2: Working with RDF Data

**Time to Complete**: 20 minutes
**Difficulty**: Beginner
**Prerequisites**: Tutorial 1 completed

## What You'll Learn

In this tutorial, you'll learn how to:
- Parse Turtle/TriG files into an N3 Store
- Execute SPARQL queries (SELECT, ASK, CONSTRUCT)
- Serialize RDF data back to Turtle
- Understand the Store and DataFactory basics

## Introduction

UNRDF provides a unified API for working with RDF data. At its core, it uses the N3.js library but provides simpler, more intuitive functions for parsing, querying, and serializing RDF.

## Step 1: Parse Turtle Data

Let's start by parsing some Turtle/TriG data into a Store:

```javascript
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { executeSelectSync } from '@unrdf/core';

// Sample Turtle data
const turtleData = `
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:alice a foaf:Person ;
  foaf:name "Alice Smith" ;
  foaf:age 30 ;
  foaf:knows ex:bob .

ex:bob a foaf:Person ;
  foaf:name "Bob Jones" ;
  foaf:age 25 .
`;

// Parse into a Store
const store = await parseTurtle(turtleData);

console.log(`Parsed ${store.size} triples`);
// Output: Parsed 6 triples
```

**What happened?**
- `parseTurtle()` takes a Turtle/TriG string and returns an N3 Store
- The Store contains all the triples from the parsed data
- `.size` gives you the number of quads in the store

## Step 2: Query with SPARQL SELECT

Now let's query the data using SPARQL SELECT:

```javascript
import { select } from '@unrdf/knowledge-engine';

// Query all people and their names
const results = await select(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?person ?name ?age
  WHERE {
    ?person a foaf:Person ;
            foaf:name ?name ;
            foaf:age ?age .
  }
  ORDER BY ?name
`);

console.log('People found:');
results.forEach(row => {
  console.log(`- ${row.name.value} (age ${row.age.value})`);
});

// Output:
// People found:
// - Alice Smith (age 30)
// - Bob Jones (age 25)
```

**What happened?**
- `select()` executes a SPARQL SELECT query
- Returns an array of result bindings (objects with variable names as keys)
- Each binding value is an RDF Term (NamedNode, Literal, etc.)
- Access the string value with `.value`

## Step 3: Query with SPARQL ASK

ASK queries check if a pattern exists:

```javascript
import { ask } from '@unrdf/knowledge-engine';

// Check if Alice knows Bob
const aliceKnowsBob = await ask(store, `
  PREFIX ex: <http://example.org/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  ASK {
    ex:alice foaf:knows ex:bob .
  }
`);

console.log(`Does Alice know Bob? ${aliceKnowsBob}`);
// Output: Does Alice know Bob? true

// Check if Bob knows Alice
const bobKnowsAlice = await ask(store, `
  PREFIX ex: <http://example.org/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  ASK {
    ex:bob foaf:knows ex:alice .
  }
`);

console.log(`Does Bob know Alice? ${bobKnowsAlice}`);
// Output: Does Bob know Alice? false
```

**What happened?**
- `ask()` returns a boolean
- Returns `true` if the pattern matches, `false` otherwise
- Useful for validation and conditional logic

## Step 4: Query with SPARQL CONSTRUCT

CONSTRUCT queries create new RDF graphs:

```javascript
import { construct } from '@unrdf/knowledge-engine';

// Extract just the names into a new graph
const nameGraph = await construct(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

  CONSTRUCT {
    ?person rdfs:label ?name .
  }
  WHERE {
    ?person foaf:name ?name .
  }
`);

console.log(`New graph has ${nameGraph.size} triples`);
// Output: New graph has 2 triples

// Serialize the new graph
const newTurtle = await toTurtle(nameGraph);
console.log(newTurtle);

// Output:
// @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
// <http://example.org/alice> rdfs:label "Alice Smith" .
// <http://example.org/bob> rdfs:label "Bob Jones" .
```

**What happened?**
- `construct()` returns a new Store with transformed data
- Creates triples based on the CONSTRUCT template
- The new Store can be queried, serialized, or used in hooks

## Step 5: Serialize Back to Turtle

Convert a Store back to Turtle:

```javascript
const serialized = await toTurtle(store);
console.log('Serialized Turtle:');
console.log(serialized);
```

**Other serialization formats:**

```javascript
import { toNQuads, toJsonLd } from '@unrdf/knowledge-engine';

// N-Quads format
const nquads = await toNQuads(store);

// JSON-LD format
const jsonld = await toJsonLd(store);
```

## Step 6: Build a Complete Workflow

Let's combine everything into a practical example:

```javascript
import { parseTurtle, select, toTurtle } from '@unrdf/knowledge-engine';

async function processUserData() {
  // 1. Load data
  const turtleData = `
    @prefix ex: <http://example.org/> .
    @prefix foaf: <http://xmlns.com/foaf/0.1/> .

    ex:alice a foaf:Person ;
      foaf:name "Alice Smith" ;
      foaf:age 30 ;
      foaf:mbox <mailto:alice@example.org> .

    ex:bob a foaf:Person ;
      foaf:name "Bob Jones" ;
      foaf:age 25 ;
      foaf:mbox <mailto:bob@example.org> .
  `;

  const store = await parseTurtle(turtleData);
  console.log(`Loaded ${store.size} triples`);

  // 2. Query users over 25
  const adults = await select(store, `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?name ?age ?email
    WHERE {
      ?person foaf:name ?name ;
              foaf:age ?age ;
              foaf:mbox ?email .
      FILTER(?age >= 25)
    }
  `);

  console.log('\nUsers 25 or older:');
  adults.forEach(row => {
    console.log(`- ${row.name.value}, age ${row.age.value}, email: ${row.email.value}`);
  });

  // 3. Export results
  const output = await toTurtle(store);
  console.log('\nExported data:');
  console.log(output);

  return adults;
}

// Run the workflow
await processUserData();
```

**Output:**
```
Loaded 8 triples

Users 25 or older:
- Alice Smith, age 30, email: mailto:alice@example.org
- Bob Jones, age 25, email: mailto:bob@example.org

Exported data:
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:alice a foaf:Person ;
  foaf:name "Alice Smith" ;
  foaf:age 30 ;
  foaf:mbox <mailto:alice@example.org> .

ex:bob a foaf:Person ;
  foaf:name "Bob Jones" ;
  foaf:age 25 ;
  foaf:mbox <mailto:bob@example.org> .
```

## What You Learned

- How to parse Turtle data with `parseTurtle()`
- How to execute SPARQL queries with `select()`, `ask()`, `construct()`
- How to serialize RDF data with `toTurtle()`, `toNQuads()`, `toJsonLd()`
- How to build a complete load → query → save workflow
- How to access values from SPARQL results

## Common Patterns

### Pattern 1: Load from File

```javascript
import { readFile } from 'fs/promises';
import { parseTurtle } from '@unrdf/knowledge-engine';

const content = await readFile('./data.ttl', 'utf-8');
const store = await parseTurtle(content);
```

### Pattern 2: Count Results

```javascript
const results = await select(store, `...`);
console.log(`Found ${results.length} matches`);
```

### Pattern 3: Check for Optional Values

```javascript
results.forEach(row => {
  const email = row.email?.value || 'no email';
  console.log(email);
});
```

## Next Steps

In the next tutorial, you'll learn how to use composables and the context system to build more powerful applications.

**Continue to**: [Tutorial 3: Composables & Context](./03-composables-context.md)

## Complete Example

See the full working example at: `examples/context-example.mjs`

## Reference

- [parseTurtle API Reference](../reference/api/knowledge-engine.md#parseTurtle)
- [SPARQL Query API Reference](../reference/api/knowledge-engine.md#query)
- [Serialization How-To](../how-to/parsing/convert-formats.md)
