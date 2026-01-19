# @unrdf/core

![Version](https://img.shields.io/badge/version-6.0.0--rc.3-blue) ![Production Ready](https://img.shields.io/badge/production-ready-green)

**UNRDF Core - RDF Graph Operations, SPARQL Execution, and Foundational Substrate**

The essential RDF substrate that all UNRDF packages and applications build upon.

## Features

- **RDF Graph Operations**: Create, query, and manipulate RDF graphs
- **SPARQL Execution**: Execute SPARQL queries via Comunica
- **Canonicalization**: Deterministic RDF canonicalization
- **Type-Safe**: Full TypeScript support with Zod validation
- **Minimal Dependencies**: Focused on core functionality only

## Installation

```bash
pnpm add @unrdf/core
```

## 📚 Examples

See these examples that demonstrate @unrdf/core:

- **[01-minimal-parse-query.mjs](../../examples/01-minimal-parse-query.mjs)** - Start here! Minimal parse & query example (3 min)
- **[minimal-core-example.mjs](../../examples/minimal-core-example.mjs)** - Direct @unrdf/core usage without substrate wrapper
- **[context-example.mjs](../../examples/context-example.mjs)** - Understanding the core context system
- **[comprehensive-feature-test.mjs](../../examples/comprehensive-feature-test.mjs)** - All core features integrated

**New to UNRDF?** Start with the [Quick Start Guide](../../examples/QUICKSTART.md).

## Quick Start

### Basic RDF Operations (2 minutes)

```javascript
import { createStore } from '@unrdf/oxigraph';
import { dataFactory } from '@unrdf/oxigraph';

// 1. Create an in-memory RDF store
const store = createStore();

// 2. Create RDF terms
const { namedNode, literal, quad } = dataFactory;

const alice = namedNode('http://example.com/alice');
const name = namedNode('http://xmlns.com/foaf/0.1/name');
const aliceName = literal('Alice');

// 3. Add triple to store
const triple = quad(alice, name, aliceName);
store.add(triple);

// 4. Check if triple exists
console.log(store.has(triple)); // true

// 5. Query triples by pattern
const matches = store.match(null, name, null);
for (const quad of matches) {
  console.log(`${quad.subject.value} has name ${quad.object.value}`);
}
// Output: http://example.com/alice has name Alice
```

### SPARQL Queries (3 minutes)

```javascript
// SELECT query
const results = store.query(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?person ?name
  WHERE {
    ?person foaf:name ?name .
  }
`);

console.log(results);
// [{ person: 'http://example.com/alice', name: 'Alice' }]

// ASK query (boolean)
const exists = store.query(`
  ASK { <http://example.com/alice> ?p ?o }
`);
console.log(exists); // true

// CONSTRUCT query (build new graph)
const constructed = store.query(`
  CONSTRUCT { ?s ?p ?o }
  WHERE { ?s ?p ?o }
`);
console.log(constructed); // Array of quads
```

## Common Patterns

### Pattern 1: Loading RDF Data from Turtle

```javascript
import { createStore } from '@unrdf/oxigraph';

const store = createStore();

const turtleData = `
  @prefix ex: <http://example.com/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .

  ex:alice foaf:name "Alice" ;
           foaf:age 30 ;
           foaf:knows ex:bob .

  ex:bob foaf:name "Bob" ;
         foaf:age 25 .
`;

// Load Turtle data into store
store.load(turtleData, {
  format: 'text/turtle',
  base_iri: 'http://example.com/',
});

console.log(`Loaded ${store.size()} triples`);
```

### Pattern 2: Querying Relationships

```javascript
// Find all people Alice knows
const aliceFriends = store.query(`
  PREFIX ex: <http://example.com/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?friendName
  WHERE {
    ex:alice foaf:knows ?friend .
    ?friend foaf:name ?friendName .
  }
`);

aliceFriends.forEach(row => {
  console.log(`Alice knows ${row.friendName}`);
});
```

### Pattern 3: Complex SPARQL with FILTER

```javascript
// Find people older than 25
const results = store.query(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  SELECT ?person ?name ?age
  WHERE {
    ?person foaf:name ?name ;
            foaf:age ?age .
    FILTER (?age > 25)
  }
  ORDER BY DESC(?age)
`);

console.log(results);
// [{ person: '...alice', name: 'Alice', age: 30 }]
```

### Pattern 4: Updating Data with SPARQL UPDATE

```javascript
// Update Alice's age
store.update(`
  PREFIX ex: <http://example.com/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  DELETE { ex:alice foaf:age ?oldAge }
  INSERT { ex:alice foaf:age 31 }
  WHERE { ex:alice foaf:age ?oldAge }
`);
```

### Pattern 5: Exporting Data

```javascript
// Export as Turtle
const turtle = store.dump({ format: 'text/turtle' });
console.log(turtle);

// Export as N-Triples
const ntriples = store.dump({ format: 'application/n-triples' });

// Export as JSON-LD
const jsonld = store.dump({ format: 'application/ld+json' });
```

### Pattern 6: Named Graphs

```javascript
import { dataFactory } from '@unrdf/oxigraph';

const { namedNode, literal, quad } = dataFactory;

// Add to specific graph
const graphUri = namedNode('http://example.com/graph1');
const triple = quad(
  namedNode('http://example.com/resource'),
  namedNode('http://example.com/property'),
  literal('value'),
  graphUri // 4th parameter = graph
);

store.add(triple);

// Query specific graph
const results = store.query(`
  SELECT ?s ?p ?o
  WHERE {
    GRAPH <http://example.com/graph1> {
      ?s ?p ?o
    }
  }
`);
```

## Error Handling

```javascript
import { createStore } from '@unrdf/oxigraph';

try {
  const store = createStore();

  // Load data
  store.load(turtleData, { format: 'text/turtle' });

  // Execute query
  const results = store.query('SELECT * WHERE { ?s ?p ?o }');

  console.log(`Found ${results.length} results`);
} catch (error) {
  if (error.message.includes('parse')) {
    console.error('Invalid RDF syntax:', error.message);
  } else if (error.message.includes('query')) {
    console.error('Invalid SPARQL query:', error.message);
  } else {
    console.error('Unexpected error:', error);
  }
}
```

## API Reference

### Store Operations

| Method                        | Description            | Example                              |
| ----------------------------- | ---------------------- | ------------------------------------ |
| `createStore(quads?)`         | Create new RDF store   | `const store = createStore()`        |
| `store.add(quad)`             | Add quad to store      | `store.add(quad(s, p, o))`           |
| `store.delete(quad)`          | Remove quad            | `store.delete(quad(s, p, o))`        |
| `store.has(quad)`             | Check if quad exists   | `store.has(quad(s, p, o))`           |
| `store.match(s?, p?, o?, g?)` | Query quads by pattern | `store.match(null, predicate, null)` |
| `store.size()`                | Get number of quads    | `console.log(store.size())`          |
| `store.clear()`               | Remove all quads       | `store.clear()`                      |

### SPARQL Queries

| Method                 | Description           | Returns                                                                |
| ---------------------- | --------------------- | ---------------------------------------------------------------------- |
| `store.query(sparql)`  | Execute SPARQL query  | SELECT: Array of bindings<br>ASK: Boolean<br>CONSTRUCT: Array of quads |
| `store.update(sparql)` | Execute SPARQL UPDATE | `void`                                                                 |

**Query Types:**

- **SELECT**: Returns array of variable bindings
- **ASK**: Returns boolean (true/false)
- **CONSTRUCT**: Returns array of quads
- **DESCRIBE**: Returns array of quads

### Data Import/Export

| Method                      | Description     | Formats                                            |
| --------------------------- | --------------- | -------------------------------------------------- |
| `store.load(data, options)` | Load RDF data   | Turtle, TriG, N-Triples, N-Quads, RDF/XML, JSON-LD |
| `store.dump(options)`       | Export RDF data | Turtle, TriG, N-Triples, N-Quads, RDF/XML, JSON-LD |

**Load Options:**

```javascript
{
  format: 'text/turtle',           // Required: RDF format
  base_iri: 'http://example.com/', // Optional: base IRI
  to_graph: graphUri               // Optional: target graph
}
```

**Dump Options:**

```javascript
{
  format: 'text/turtle',      // Required: output format
  from_graph: graphUri        // Optional: source graph
}
```

### RDF Term Factory

```javascript
import { dataFactory } from '@unrdf/oxigraph';

const { namedNode, literal, blankNode, quad, defaultGraph } = dataFactory;

// Create terms
const subject = namedNode('http://example.com/alice');
const predicate = namedNode('http://xmlns.com/foaf/0.1/name');
const object = literal('Alice', 'en');
const blank = blankNode();
const graph = defaultGraph();

// Create quad
const triple = quad(subject, predicate, object);
const quadWithGraph = quad(subject, predicate, object, graph);
```

## Dependencies

- @comunica/query-sparql
- n3
- rdf-canonize
- @rdfjs/\*

## When to Use @unrdf/core

✅ Always needed for:

- Basic RDF graph operations
- SPARQL query execution
- Building RDF applications
- Any UNRDF-based project

## When to Add Extensions

After @unrdf/core, you can add:

- `@unrdf/hooks` - Policy enforcement and validation
- `@unrdf/federation` - Peer discovery and queries
- `@unrdf/streaming` - Change feeds and subscriptions
- `@unrdf/browser` - Browser SDK with IndexedDB
- `@unrdf/knowledge-engine` - Rule engine (optional)
- `@unrdf/dark-matter` - Query optimization (optional)

## Documentation

See [UNRDF Documentation](https://github.com/unrdf/unrdf) for full API reference and guides.

## License

MIT
