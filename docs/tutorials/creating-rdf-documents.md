# Tutorial: Creating RDF Documents

Learn how to create, structure, and serialize RDF documents using UNRDF's knowledge engine.

## Learning Objectives

By the end of this tutorial, you will:

- Understand RDF data model fundamentals
- Create RDF documents in Turtle format
- Use UNRDF to parse and serialize RDF
- Work with named graphs and blank nodes

## Prerequisites

- UNRDF installed (`pnpm add unrdf`)
- Basic JavaScript/Node.js knowledge
- Understanding of URIs and namespaces

## Step 1: Understanding RDF Triples

RDF (Resource Description Framework) represents data as **triples**:

```
Subject -> Predicate -> Object
```

Example:

```
<http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" .
```

This says: "The resource alice has the property name with value Alice."

## Step 2: Writing Turtle Syntax

Turtle is the most human-readable RDF format. Create your first document:

```javascript
// src/create-rdf.mjs
const turtleDocument = `
# Prefixes make URIs shorter
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# A person resource
ex:alice
    a foaf:Person ;                    # rdf:type shorthand
    foaf:name "Alice Smith" ;          # String literal
    foaf:age "30"^^xsd:integer ;       # Typed literal
    foaf:homepage <http://alice.example.org/> ;  # URI object
    foaf:knows ex:bob, ex:carol .      # Multiple objects

ex:bob a foaf:Person ;
    foaf:name "Bob Jones" .

ex:carol a foaf:Person ;
    foaf:name "Carol White" .
`;
```

### Turtle Syntax Rules

| Syntax | Meaning |
|--------|---------|
| `@prefix` | Namespace declaration |
| `a` | Shorthand for `rdf:type` |
| `;` | Same subject, new predicate |
| `,` | Same subject and predicate, new object |
| `.` | End of statement |
| `""` | String literal |
| `""^^xsd:type` | Typed literal |
| `<uri>` | Full URI |
| `prefix:local` | Prefixed name |

## Step 3: Parsing Turtle with UNRDF

```javascript
import { parseTurtle } from 'unrdf/knowledge-engine';

const store = await parseTurtle(turtleDocument);

console.log(`Parsed ${store.size} triples`);

// Iterate over triples
for (const quad of store) {
  console.log(`${quad.subject.value} -> ${quad.predicate.value} -> ${quad.object.value}`);
}
```

## Step 4: Creating RDF Programmatically

Use N3's DataFactory to create triples in code:

```javascript
import { parseTurtle, toTurtle } from 'unrdf/knowledge-engine';
import { DataFactory, Store } from 'n3';

const { namedNode, literal, quad } = DataFactory;

// Create an empty store
const store = new Store();

// Define namespaces
const ex = (local) => namedNode(`http://example.org/${local}`);
const foaf = (local) => namedNode(`http://xmlns.com/foaf/0.1/${local}`);
const rdf = (local) => namedNode(`http://www.w3.org/1999/02/22-rdf-syntax-ns#${local}`);

// Add triples programmatically
store.addQuad(quad(
  ex('alice'),
  rdf('type'),
  foaf('Person')
));

store.addQuad(quad(
  ex('alice'),
  foaf('name'),
  literal('Alice Smith')
));

store.addQuad(quad(
  ex('alice'),
  foaf('age'),
  literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
));

// Serialize to Turtle
const output = await toTurtle(store);
console.log(output);
```

## Step 5: Working with Blank Nodes

Blank nodes represent anonymous resources:

```javascript
const turtleWithBlanks = `
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:alice foaf:knows [
    a foaf:Person ;
    foaf:name "Unknown Person"
] .
`;

const store = await parseTurtle(turtleWithBlanks);

// Blank nodes have generated IDs
for (const quad of store) {
  console.log(`Subject type: ${quad.subject.termType}`);
  // BlankNode subjects show as "_:b0" etc.
}
```

### Creating Blank Nodes Programmatically

```javascript
import { DataFactory, Store } from 'n3';

const { blankNode, namedNode, literal, quad } = DataFactory;

const store = new Store();
const address = blankNode(); // Generate unique blank node

store.addQuad(quad(
  namedNode('http://example.org/alice'),
  namedNode('http://example.org/address'),
  address
));

store.addQuad(quad(
  address,
  namedNode('http://example.org/city'),
  literal('New York')
));

store.addQuad(quad(
  address,
  namedNode('http://example.org/country'),
  literal('USA')
));
```

## Step 6: Named Graphs

Named graphs allow grouping triples with a fourth element:

```javascript
const turtleWithGraphs = `
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

# Default graph
ex:alice foaf:name "Alice" .

# Named graph syntax (N-Quads)
`;

// For named graphs, use N-Quads format
const nquads = `
<http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" <http://example.org/graph1> .
<http://example.org/bob> <http://xmlns.com/foaf/0.1/name> "Bob" <http://example.org/graph2> .
`;

import { toNQuads } from 'unrdf/knowledge-engine';

// Or add with DataFactory
const store = new Store();
const graph1 = namedNode('http://example.org/graph1');

store.addQuad(quad(
  namedNode('http://example.org/alice'),
  namedNode('http://xmlns.com/foaf/0.1/name'),
  literal('Alice'),
  graph1  // Fourth parameter is the graph
));

// Query specific graph
const graph1Quads = store.getQuads(null, null, null, graph1);
console.log(`Graph1 has ${graph1Quads.length} triples`);
```

## Step 7: JSON-LD Interoperability

UNRDF supports JSON-LD for web-friendly RDF:

```javascript
import { parseJsonLd, toJsonLd } from 'unrdf/knowledge-engine';

const jsonldDoc = {
  "@context": {
    "foaf": "http://xmlns.com/foaf/0.1/",
    "name": "foaf:name",
    "knows": { "@id": "foaf:knows", "@type": "@id" }
  },
  "@id": "http://example.org/alice",
  "@type": "foaf:Person",
  "name": "Alice",
  "knows": "http://example.org/bob"
};

// Parse JSON-LD to store
const store = await parseJsonLd(jsonldDoc);

// Convert back to JSON-LD
const outputJsonLd = await toJsonLd(store, {
  context: { foaf: "http://xmlns.com/foaf/0.1/" }
});
```

## Step 8: Validation During Creation

Use SHACL to validate as you create:

```javascript
import { parseTurtle, validateShacl } from 'unrdf/knowledge-engine';

const shapes = `
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:PersonShape a sh:NodeShape ;
    sh:targetClass foaf:Person ;
    sh:property [
        sh:path foaf:name ;
        sh:minCount 1
    ] .
`;

async function createValidatedPerson(name, data) {
  const turtle = `
    @prefix ex: <http://example.org/> .
    @prefix foaf: <http://xmlns.com/foaf/0.1/> .

    ex:${name} a foaf:Person ;
        ${data} .
  `;

  const store = await parseTurtle(turtle);
  const report = await validateShacl(store, shapes);

  if (!report.conforms) {
    throw new Error('Invalid person data');
  }

  return store;
}

// This will succeed
const alice = await createValidatedPerson('alice', 'foaf:name "Alice"');

// This will throw (missing name)
try {
  const noName = await createValidatedPerson('invalid', 'foaf:age 30');
} catch (e) {
  console.log('Validation failed:', e.message);
}
```

## Exercise: Build a Contact Book

Create a contact book application:

```javascript
// exercises/contact-book.mjs
import { parseTurtle, toTurtle, select } from 'unrdf/knowledge-engine';
import { DataFactory, Store } from 'n3';

const { namedNode, literal, quad } = DataFactory;

class ContactBook {
  constructor() {
    this.store = new Store();
    this.ex = (local) => namedNode(`http://example.org/contacts/${local}`);
    this.foaf = (local) => namedNode(`http://xmlns.com/foaf/0.1/${local}`);
    this.rdf = (local) => namedNode(`http://www.w3.org/1999/02/22-rdf-syntax-ns#${local}`);
  }

  addContact(id, name, email) {
    const contact = this.ex(id);

    this.store.addQuad(quad(contact, this.rdf('type'), this.foaf('Person')));
    this.store.addQuad(quad(contact, this.foaf('name'), literal(name)));
    this.store.addQuad(quad(contact, this.foaf('mbox'), namedNode(`mailto:${email}`)));
  }

  async findByName(searchName) {
    return await select(this.store, `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>

      SELECT ?contact ?name ?email
      WHERE {
        ?contact a foaf:Person ;
                 foaf:name ?name ;
                 foaf:mbox ?email .
        FILTER(CONTAINS(LCASE(?name), LCASE("${searchName}")))
      }
    `);
  }

  async export() {
    return await toTurtle(this.store);
  }
}

// Usage
const book = new ContactBook();
book.addContact('alice', 'Alice Smith', 'alice@example.com');
book.addContact('bob', 'Bob Jones', 'bob@example.com');

const results = await book.findByName('alice');
console.log(results);

const turtle = await book.export();
console.log(turtle);
```

## Common Mistakes

### Mistake 1: Forgetting Await

```javascript
// Wrong - store will be a Promise
const store = parseTurtle(data);

// Correct
const store = await parseTurtle(data);
```

### Mistake 2: Invalid URIs

```javascript
// Wrong - spaces in URIs
const uri = namedNode('http://example.org/my resource');

// Correct - encode or use underscores
const uri = namedNode('http://example.org/my_resource');
const uri2 = namedNode(encodeURI('http://example.org/my resource'));
```

### Mistake 3: Missing Prefixes

```turtle
# Wrong - undefined prefix
ex:alice foaf:name "Alice" .

# Correct - declare prefixes
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
ex:alice foaf:name "Alice" .
```

## Summary

You learned how to:

- Write RDF in Turtle syntax
- Parse Turtle documents with UNRDF
- Create RDF programmatically with DataFactory
- Work with blank nodes and named graphs
- Convert between Turtle and JSON-LD
- Validate RDF during creation

## Next Steps

- [Knowledge Hooks Tutorial](./knowledge-hooks.md) - Create reactive triggers
- [SPARQL Tutorial](./sparql.md) - Advanced querying
- [Validation Guide](../guides/validation-rules.md) - SHACL deep dive
