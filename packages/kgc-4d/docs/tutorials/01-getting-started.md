# Tutorial 1: Getting Started with KGC 4D

In this tutorial, you'll create your first KGC Store, add some data, and query it. This is the foundation for everything else in KGC 4D.

**Time:** ~15 minutes
**Goals:**
- Set up a KGC Store
- Create RDF data using the data factory
- Add quads to the store
- Query data using SPARQL

## Step 1: Install Dependencies

KGC 4D requires `@unrdf/kgc-4d` and `@unrdf/oxigraph`:

```bash
npm install @unrdf/kgc-4d @unrdf/oxigraph
```

## Step 2: Create Your First Store

Create a file called `hello-kgc.mjs`:

```javascript
import { KGCStore } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

// Create a new KGC Store
const store = new KGCStore();
console.log('✓ KGC Store created');
```

Run it:
```bash
node hello-kgc.mjs
```

You should see: `✓ KGC Store created`

## Step 3: Create RDF Data

Let's add some basic information about a person. RDF uses quads (subject, predicate, object, graph):

```javascript
import { KGCStore } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

const store = new KGCStore();

// Create named nodes and literals
const alice = dataFactory.namedNode('http://example.org/alice');
const name = dataFactory.namedNode('http://example.org/name');
const nameValue = dataFactory.literal('Alice');

// Create a quad (statement)
const quad = dataFactory.quad(
  alice,           // subject
  name,            // predicate
  nameValue,       // object
  store.UNIVERSE   // graph
);

console.log('Quad created:', quad);
```

## Step 4: Add Data to the Store

Now let's add this quad to the Universe graph:

```javascript
import { KGCStore } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

const store = new KGCStore();

const alice = dataFactory.namedNode('http://example.org/alice');
const name = dataFactory.namedNode('http://example.org/name');
const age = dataFactory.namedNode('http://example.org/age');
const rdfType = dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
const person = dataFactory.namedNode('http://example.org/Person');

// Add multiple quads about Alice
const quads = [
  dataFactory.quad(alice, rdfType, person, store.UNIVERSE),
  dataFactory.quad(alice, name, dataFactory.literal('Alice'), store.UNIVERSE),
  dataFactory.quad(alice, age, dataFactory.literal('30'), store.UNIVERSE),
];

quads.forEach(quad => store.addQuad(quad));
console.log(`✓ Added ${quads.length} quads to the store`);
```

## Step 5: Query Your Data

Use SPARQL to retrieve the data:

```javascript
import { KGCStore } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

const store = new KGCStore();

const alice = dataFactory.namedNode('http://example.org/alice');
const name = dataFactory.namedNode('http://example.org/name');
const age = dataFactory.namedNode('http://example.org/age');
const rdfType = dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
const person = dataFactory.namedNode('http://example.org/Person');

const quads = [
  dataFactory.quad(alice, rdfType, person, store.UNIVERSE),
  dataFactory.quad(alice, name, dataFactory.literal('Alice'), store.UNIVERSE),
  dataFactory.quad(alice, age, dataFactory.literal('30'), store.UNIVERSE),
];

quads.forEach(quad => store.addQuad(quad));

// Query: Find the name of all people
const query = `
  PREFIX ex: <http://example.org/>
  SELECT ?person ?name
  WHERE {
    GRAPH <kgc:Universe> {
      ?person a ex:Person ;
              ex:name ?name .
    }
  }
`;

const results = store.querySync(query);
console.log('Query Results:');
results.forEach(binding => {
  console.log(`  ${binding.get('person').value} has name ${binding.get('name').value}`);
});
```

Run this and you should see:
```
Query Results:
  http://example.org/alice has name Alice
```

## Step 6: Add More Entities

Let's expand our data:

```javascript
import { KGCStore } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

const store = new KGCStore();

const rdfType = dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
const name = dataFactory.namedNode('http://example.org/name');
const age = dataFactory.namedNode('http://example.org/age');
const knows = dataFactory.namedNode('http://example.org/knows');
const person = dataFactory.namedNode('http://example.org/Person');

const alice = dataFactory.namedNode('http://example.org/alice');
const bob = dataFactory.namedNode('http://example.org/bob');

// Create a social network
const data = [
  // Alice
  dataFactory.quad(alice, rdfType, person, store.UNIVERSE),
  dataFactory.quad(alice, name, dataFactory.literal('Alice'), store.UNIVERSE),
  dataFactory.quad(alice, age, dataFactory.literal('30'), store.UNIVERSE),

  // Bob
  dataFactory.quad(bob, rdfType, person, store.UNIVERSE),
  dataFactory.quad(bob, name, dataFactory.literal('Bob'), store.UNIVERSE),
  dataFactory.quad(bob, age, dataFactory.literal('25'), store.UNIVERSE),

  // Relationships
  dataFactory.quad(alice, knows, bob, store.UNIVERSE),
];

data.forEach(quad => store.addQuad(quad));

// Query relationships
const relationshipQuery = `
  PREFIX ex: <http://example.org/>
  SELECT ?person1 ?person2
  WHERE {
    GRAPH <kgc:Universe> {
      ?person1 ex:knows ?person2 ;
               a ex:Person .
    }
  }
`;

const relationships = store.querySync(relationshipQuery);
console.log('Relationships:');
relationships.forEach(binding => {
  const p1 = binding.get('person1').value.split('/').pop();
  const p2 = binding.get('person2').value.split('/').pop();
  console.log(`  ${p1} knows ${p2}`);
});
```

Output:
```
Relationships:
  alice knows bob
```

## Summary

You've learned:
- ✓ Create a KGC Store
- ✓ Build RDF quads using the data factory
- ✓ Add quads to the Universe graph
- ✓ Query data using SPARQL

## Next Steps

Now that you understand basic operations, move to [Tutorial 2: Working with Events](./02-working-with-events.md) to learn how KGC 4D tracks changes over time.

## Key Takeaways

| Concept | Meaning |
|---------|---------|
| **Quad** | An RDF statement: subject, predicate, object, and a named graph |
| **Universe** | The current observable state graph |
| **SPARQL** | A query language for RDF data |
| **Data Factory** | Helper for creating properly-formatted RDF nodes and quads |

## Troubleshooting

**Q: I get an error "Cannot find module '@unrdf/kgc-4d'"**
A: Make sure you've installed dependencies: `npm install @unrdf/kgc-4d @unrdf/oxigraph`

**Q: My SPARQL query returns no results**
A: Check that your triple patterns match your data exactly (including URIs and literal values).

**Q: What's the difference between a named node and a literal?**
A: Named nodes are URIs (like `http://example.org/alice`), while literals are values like strings, numbers, or booleans.
