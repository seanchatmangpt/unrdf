# RDF Fundamentals

This chapter introduces RDF (Resource Description Framework) from a developer's perspective, focusing on practical understanding and implementation using UNRDF.

## What is RDF?

RDF is a W3C standard for representing information as **graphs** of interconnected data. Unlike traditional databases that organize data in tables, RDF uses a **graph model** where:

- **Everything is a resource** (documents, people, concepts, events)
- **Resources have properties** (attributes and relationships)
- **Properties connect resources** (forming a web of linked data)
- **Data is machine-readable** (enabling automated reasoning)

> **Developer Insight**: Think of RDF as a schema-less graph database with built-in semantics. It's like JSON-LD meets graph theory meets type safety.

## The Triple Model

The foundation of RDF is the **triple** (also called a **statement**):

```text
Subject → Predicate → Object
```

Every piece of information is expressed as a combination of these three elements:

```turtle
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

# Triple: Subject → Predicate → Object
ex:alice foaf:name "Alice Smith" .
#   ↑         ↑           ↑
# Subject  Predicate   Object
```

### Triple Components

| Component | Description | Example |
|-----------|-------------|---------|
| **Subject** | The resource being described | `ex:alice` |
| **Predicate** | The property or relationship | `foaf:name` |
| **Object** | The value or target resource | `"Alice Smith"` |

## RDF Terms

RDF uses three types of terms to build triples:

### 1. URIs/IRIs (Internationalized Resource Identifiers)

URIs uniquely identify resources globally:

```javascript
import { namedNode } from '@rdfjs/data-model';

// Create IRIs
const person = namedNode('http://example.org/people/alice');
const predicate = namedNode('http://xmlns.com/foaf/0.1/name');
const knows = namedNode('http://xmlns.com/foaf/0.1/knows');
```

**In UNRDF:**

```javascript
import { initStore, useStoreContext } from 'unrdf';

const runApp = initStore();

runApp(() => {
  const ctx = useStoreContext();

  // Create named nodes (IRIs)
  const alice = ctx.namedNode('http://example.org/alice');
  const name = ctx.namedNode('http://xmlns.com/foaf/0.1/name');

  console.log(alice.termType);  // 'NamedNode'
  console.log(alice.value);     // 'http://example.org/alice'
});
```

### 2. Literals

Literals represent data values (strings, numbers, dates):

```javascript
import { literal } from '@rdfjs/data-model';

// Plain string
const name = literal('Alice Smith');

// Typed literal (integer)
const age = literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer'));

// Language-tagged string
const greeting = literal('Hello', 'en');
```

**In UNRDF:**

```javascript
runApp(() => {
  const ctx = useStoreContext();

  // Plain literal
  const name = ctx.literal('Alice Smith');

  // Typed literal
  const age = ctx.literal('30', 'http://www.w3.org/2001/XMLSchema#integer');

  console.log(name.value);      // 'Alice Smith'
  console.log(age.datatype);    // NamedNode with value xsd:integer
});
```

#### Common XSD Datatypes

```turtle
# String
"Hello World"

# Integer
"42"^^xsd:integer

# Decimal
"3.14"^^xsd:decimal

# Boolean
"true"^^xsd:boolean

# Date
"2025-01-15"^^xsd:date

# DateTime
"2025-01-15T10:30:00Z"^^xsd:dateTime
```

### 3. Blank Nodes

Blank nodes are anonymous resources without URIs:

```javascript
import { blankNode } from '@rdfjs/data-model';

// Create blank node
const anon = blankNode();  // Auto-generated ID
const specific = blankNode('b1');  // Specific ID
```

**In UNRDF:**

```javascript
runApp(() => {
  const ctx = useStoreContext();

  // Auto-generated blank node
  const b1 = ctx.blankNode();

  // Named blank node
  const b2 = ctx.blankNode('address');

  console.log(b1.termType);  // 'BlankNode'
  console.log(b2.value);     // '_:address'
});
```

**Use cases for blank nodes:**
- Modeling nested objects (addresses, contact info)
- Intermediate nodes in complex relationships
- Anonymous resources that don't need global identity

## Quads: Triples + Context

UNRDF extends triples to **quads** by adding a fourth component: the **graph**:

```text
Subject → Predicate → Object → Graph
```

Graphs allow you to organize triples into named contexts:

```javascript
import { quad, namedNode, literal, defaultGraph } from '@rdfjs/data-model';

// Quad in default graph
const q1 = quad(
  namedNode('http://example.org/alice'),
  namedNode('http://xmlns.com/foaf/0.1/name'),
  literal('Alice'),
  defaultGraph()
);

// Quad in named graph
const q2 = quad(
  namedNode('http://example.org/bob'),
  namedNode('http://xmlns.com/foaf/0.1/name'),
  literal('Bob'),
  namedNode('http://example.org/graph/users')
);
```

**In UNRDF:**

```javascript
runApp(() => {
  const ctx = useStoreContext();

  const subject = ctx.namedNode('http://example.org/alice');
  const predicate = ctx.namedNode('http://xmlns.com/foaf/0.1/name');
  const object = ctx.literal('Alice Smith');

  // Create quad
  const quad = ctx.quad(subject, predicate, object);

  // Add to store
  ctx.add(quad);

  console.log(ctx.store.size);  // 1
});
```

## Namespaces and Prefixes

Namespaces shorten URIs for readability:

```turtle
# Without prefixes (verbose)
<http://xmlns.com/foaf/0.1/Person> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .

# With prefixes (readable)
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

foaf:Person rdf:type foaf:Person .
```

### Common Namespaces

| Prefix | Namespace | Purpose |
|--------|-----------|---------|
| `rdf:` | `http://www.w3.org/1999/02/22-rdf-syntax-ns#` | RDF vocabulary |
| `rdfs:` | `http://www.w3.org/2000/01/rdf-schema#` | RDF Schema |
| `xsd:` | `http://www.w3.org/2001/XMLSchema#` | XML Schema datatypes |
| `owl:` | `http://www.w3.org/2002/07/owl#` | Web Ontology Language |
| `foaf:` | `http://xmlns.com/foaf/0.1/` | Friend of a Friend |
| `dc:` | `http://purl.org/dc/elements/1.1/` | Dublin Core |
| `schema:` | `https://schema.org/` | Schema.org |

**In UNRDF:**

```javascript
import { useTurtle, initStore } from 'unrdf';

const runApp = initStore();

runApp(() => {
  const turtle = useTurtle('./graph');

  // Parse with automatic prefix handling
  const store = turtle.parse(`
    @prefix ex: <http://example.org/> .
    @prefix foaf: <http://xmlns.com/foaf/0.1/> .

    ex:alice a foaf:Person ;
             foaf:name "Alice Smith" ;
             foaf:age 30 .
  `);

  // Serialize with prefixes
  const output = turtle.serialize({
    prefixes: {
      ex: 'http://example.org/',
      foaf: 'http://xmlns.com/foaf/0.1/'
    }
  });

  console.log(output);
});
```

## RDF Serialization Formats

RDF can be represented in multiple formats:

### Turtle (Terse RDF Triple Language)

Human-readable, compact format:

```turtle
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:alice a foaf:Person ;
    foaf:name "Alice Smith" ;
    foaf:age 30 ;
    foaf:knows ex:bob .

ex:bob a foaf:Person ;
    foaf:name "Bob Jones" ;
    foaf:age 28 .
```

**Features:**
- Supports prefixes
- Uses `;` to repeat subjects
- Uses `,` to repeat subject+predicate
- `a` is shorthand for `rdf:type`

### N-Quads

Line-based format for quads:

```nquads
<http://example.org/alice> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .
<http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice Smith" .
<http://example.org/alice> <http://xmlns.com/foaf/0.1/age> "30"^^<http://www.w3.org/2001/XMLSchema#integer> .
<http://example.org/alice> <http://xmlns.com/foaf/0.1/knows> <http://example.org/bob> <http://example.org/graph/social> .
```

**Features:**
- One quad per line
- No prefixes (full URIs)
- Fourth element for named graphs
- Machine-optimized

### JSON-LD

JSON format for linked data:

```json
{
  "@context": {
    "foaf": "http://xmlns.com/foaf/0.1/",
    "ex": "http://example.org/"
  },
  "@id": "ex:alice",
  "@type": "foaf:Person",
  "foaf:name": "Alice Smith",
  "foaf:age": 30,
  "foaf:knows": {
    "@id": "ex:bob"
  }
}
```

**Features:**
- Standard JSON syntax
- `@context` for namespace mapping
- `@id` for URIs
- `@type` for rdf:type
- Easy integration with web APIs

**In UNRDF:**

```javascript
import { parseTurtle, toNQuads, toJsonLd } from 'unrdf';

const runApp = initStore();

runApp(async () => {
  // Parse Turtle
  const turtleData = `
    @prefix ex: <http://example.org/> .
    @prefix foaf: <http://xmlns.com/foaf/0.1/> .

    ex:alice foaf:name "Alice" .
  `;

  const store = await parseTurtle(turtleData);

  // Convert to N-Quads
  const nquads = await toNQuads(store);
  console.log(nquads);

  // Convert to JSON-LD
  const jsonld = await toJsonLd(store);
  console.log(JSON.stringify(jsonld, null, 2));
});
```

## Practical Examples

### Example 1: Social Network

```turtle
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:alice a foaf:Person ;
    foaf:name "Alice Smith" ;
    foaf:age "30"^^xsd:integer ;
    foaf:mbox <mailto:alice@example.org> ;
    foaf:knows ex:bob, ex:carol .

ex:bob a foaf:Person ;
    foaf:name "Bob Jones" ;
    foaf:age "28"^^xsd:integer ;
    foaf:knows ex:alice .

ex:carol a foaf:Person ;
    foaf:name "Carol Williams" ;
    foaf:age "32"^^xsd:integer .
```

**In UNRDF:**

```javascript
import { initStore, useStoreContext, useTurtle } from 'unrdf';

const runApp = initStore();

runApp(() => {
  const ctx = useStoreContext();
  const turtle = useTurtle('./graph');

  // Create nodes
  const alice = ctx.namedNode('http://example.org/alice');
  const bob = ctx.namedNode('http://example.org/bob');
  const foafName = ctx.namedNode('http://xmlns.com/foaf/0.1/name');
  const foafKnows = ctx.namedNode('http://xmlns.com/foaf/0.1/knows');

  // Create quads
  const quads = [
    ctx.quad(alice, foafName, ctx.literal('Alice Smith')),
    ctx.quad(alice, foafKnows, bob),
    ctx.quad(bob, foafName, ctx.literal('Bob Jones'))
  ];

  // Add to store
  quads.forEach(q => ctx.add(q));

  console.log(`Store contains ${ctx.store.size} quads`);

  // Save as Turtle
  turtle.save('social-network', {
    prefixes: {
      ex: 'http://example.org/',
      foaf: 'http://xmlns.com/foaf/0.1/'
    }
  });
});
```

### Example 2: Product Catalog

```turtle
@prefix ex: <http://example.org/> .
@prefix schema: <https://schema.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:product123 a schema:Product ;
    schema:name "Wireless Mouse" ;
    schema:description "Ergonomic wireless mouse with USB receiver" ;
    schema:price "29.99"^^xsd:decimal ;
    schema:priceCurrency "USD" ;
    schema:availability schema:InStock ;
    schema:brand [
        a schema:Brand ;
        schema:name "TechCo"
    ] .
```

**In UNRDF:**

```javascript
runApp(() => {
  const ctx = useStoreContext();

  // Create product
  const product = ctx.namedNode('http://example.org/product123');
  const schemaProduct = ctx.namedNode('https://schema.org/Product');
  const schemaName = ctx.namedNode('https://schema.org/name');
  const schemaPrice = ctx.namedNode('https://schema.org/price');
  const schemaBrand = ctx.namedNode('https://schema.org/brand');
  const rdfType = ctx.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');

  // Create brand (blank node)
  const brand = ctx.blankNode('brand');

  // Build graph
  ctx.add(
    ctx.quad(product, rdfType, schemaProduct),
    ctx.quad(product, schemaName, ctx.literal('Wireless Mouse')),
    ctx.quad(product, schemaPrice, ctx.literal('29.99', 'http://www.w3.org/2001/XMLSchema#decimal')),
    ctx.quad(product, schemaBrand, brand),
    ctx.quad(brand, schemaName, ctx.literal('TechCo'))
  );

  console.log(`Product catalog: ${ctx.store.size} quads`);
});
```

## Best Practices

### 1. Use Meaningful URIs

```javascript
// ✅ Good: Descriptive URIs
const person = ctx.namedNode('http://example.org/people/alice-smith');
const property = ctx.namedNode('http://example.org/schema/employeeId');

// ❌ Avoid: Opaque identifiers
const person = ctx.namedNode('http://example.org/p1');
const property = ctx.namedNode('http://example.org/x');
```

### 2. Choose Appropriate Datatypes

```javascript
// ✅ Good: Typed literals
const age = ctx.literal('30', 'http://www.w3.org/2001/XMLSchema#integer');
const price = ctx.literal('19.99', 'http://www.w3.org/2001/XMLSchema#decimal');
const active = ctx.literal('true', 'http://www.w3.org/2001/XMLSchema#boolean');

// ❌ Avoid: Untyped when type matters
const age = ctx.literal('30');  // Treated as string
```

### 3. Use Standard Vocabularies

```javascript
// ✅ Good: Reuse standard vocabularies
import { FOAF, SCHEMA, DC } from 'unrdf/vocabularies';

const person = ctx.quad(
  ctx.namedNode('http://example.org/alice'),
  ctx.namedNode(FOAF.name),
  ctx.literal('Alice')
);

// ❌ Avoid: Creating custom vocabularies for common concepts
const customName = ctx.namedNode('http://example.org/hasName');
```

### 4. Organize with Namespaces

```javascript
// ✅ Good: Clear namespace organization
const prefixes = {
  ex: 'http://example.org/',
  foaf: 'http://xmlns.com/foaf/0.1/',
  schema: 'https://schema.org/',
  xsd: 'http://www.w3.org/2001/XMLSchema#'
};

// ❌ Avoid: Mixing namespaces inconsistently
```

## Next Steps

Now that you understand RDF fundamentals:

1. **[Store Context Pattern](./store-context.md)** - Learn how UNRDF manages state
2. **[Composables](./composables.md)** - Explore UNRDF's composable functions
3. **[Transactions](./transactions.md)** - Master atomic operations

## Summary

- **RDF** is a graph-based data model using triples (subject-predicate-object)
- **Terms** include URIs (resources), Literals (values), and Blank Nodes (anonymous)
- **Quads** extend triples with graph context
- **Namespaces** shorten URIs and organize vocabularies
- **Formats** include Turtle (human), N-Quads (machine), JSON-LD (web)
- **UNRDF** provides a composable, type-safe API for RDF operations

The composable nature of UNRDF makes it easy to work with RDF while maintaining clean, maintainable code.
