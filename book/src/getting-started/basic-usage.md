# Basic Usage

This guide covers the essential UNRDF operations: creating stores, parsing RDF data, running SPARQL queries, and serializing results.

## Creating a Knowledge Store

UNRDF provides two ways to create a knowledge graph system:

### Option 1: Dark Matter Core (Recommended)

The Dark Matter core provides a complete, optimized system:

```javascript
import { createDarkMatterCore } from 'unrdf';

const system = await createDarkMatterCore();

// You now have:
// - RDF quad store (N3.js)
// - Transaction manager (ACID guarantees)
// - SPARQL query engine (Comunica)
// - Knowledge Hook manager
// - OpenTelemetry observability
// - Performance optimizations
```

```admonish success title="Why Dark Matter?"
The Dark Matter 80/20 framework delivers 85% of value from 20% of code. It automatically enables:
- Query caching (60% faster queries)
- Hook batching (50% faster execution)
- Parallel processing
- Resource optimization
```

### Option 2: Minimal System

For lightweight use cases, create a minimal system:

```javascript
import { createDarkMatterCore } from 'unrdf';

const system = await createDarkMatterCore({
  enableObservability: false,
  enablePerformanceOptimizer: false,
  enableLockchainWriter: false
});

// Minimal system with just:
// - RDF store
// - Transaction manager
// - Knowledge Hooks
```

## Working with RDF Data

### Parsing Turtle

```javascript
import { parseTurtle } from 'unrdf';

const ttl = `
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .

  ex:alice a foaf:Person ;
           foaf:name "Alice" ;
           foaf:age 30 ;
           foaf:knows ex:bob .

  ex:bob a foaf:Person ;
         foaf:name "Bob" ;
         foaf:age 25 .
`;

const store = await parseTurtle(ttl);

console.log(`Parsed ${store.size} triples`);
// Output: Parsed 6 triples
```

```admonish info
`parseTurtle()` returns an N3.Store containing RDF quads. It supports Turtle syntax including prefixes, blank nodes, and lists.
```

### Adding Data via Transactions

```javascript
import { namedNode, quad, literal } from '@rdfjs/data-model';

await system.executeTransaction({
  additions: [
    quad(
      namedNode('http://example.org/charlie'),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal('Charlie')
    ),
    quad(
      namedNode('http://example.org/charlie'),
      namedNode('http://xmlns.com/foaf/0.1/age'),
      literal('35', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
    )
  ],
  removals: [],
  actor: 'admin@example.org'
});
```

```admonish tip
Always specify the `actor` parameter to track who made changes. This is essential for audit trails and debugging.
```

### Loading from Files

```javascript
import { readFileSync } from 'node:fs';
import { parseTurtle } from 'unrdf';

// Load Turtle file
const ttl = readFileSync('./data.ttl', 'utf8');
const store = await parseTurtle(ttl);

// Add to knowledge graph
await system.executeTransaction({
  additions: [...store],
  removals: [],
  actor: 'file-import'
});
```

### Removing Data

```javascript
await system.executeTransaction({
  additions: [],
  removals: [
    quad(
      namedNode('http://example.org/alice'),
      namedNode('http://xmlns.com/foaf/0.1/age'),
      literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
    )
  ],
  actor: 'admin@example.org'
});
```

## SPARQL Queries

UNRDF supports all SPARQL 1.1 query types through Comunica.

### SELECT Queries

```javascript
const results = await system.query({
  query: `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?person ?name ?age
    WHERE {
      ?person a foaf:Person ;
              foaf:name ?name ;
              foaf:age ?age .
    }
    ORDER BY DESC(?age)
  `,
  type: 'sparql-select'
});

console.log(results);
// [
//   { person: NamedNode('ex:charlie'), name: Literal('Charlie'), age: Literal('35') },
//   { person: NamedNode('ex:alice'), name: Literal('Alice'), age: Literal('30') },
//   { person: NamedNode('ex:bob'), name: Literal('Bob'), age: Literal('25') }
// ]
```

```admonish info
SELECT queries return an array of binding objects. Each binding is a plain JavaScript object with variable names as keys.
```

### ASK Queries

```javascript
const exists = await system.query({
  query: `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    ASK {
      ?person foaf:name "Alice" .
    }
  `,
  type: 'sparql-ask'
});

console.log(exists); // true
```

ASK queries return a boolean - useful for validation and conditional logic.

### CONSTRUCT Queries

```javascript
const graph = await system.query({
  query: `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    CONSTRUCT {
      ?person foaf:name ?name .
    }
    WHERE {
      ?person a foaf:Person ;
              foaf:name ?name .
    }
  `,
  type: 'sparql-construct'
});

// graph is an N3.Store with the constructed triples
console.log(`Constructed ${graph.size} triples`);
```

CONSTRUCT queries return a new RDF graph (N3.Store) containing the constructed triples.

### DESCRIBE Queries

```javascript
const description = await system.query({
  query: `
    PREFIX ex: <http://example.org/>

    DESCRIBE ex:alice
  `,
  type: 'sparql-describe'
});

// description is an N3.Store with all triples about ex:alice
console.log(description);
```

### Complex Queries

```javascript
const results = await system.query({
  query: `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX ex: <http://example.org/>

    SELECT ?person ?name ?friend ?friendName
    WHERE {
      # Find all people who know someone
      ?person a foaf:Person ;
              foaf:name ?name ;
              foaf:knows ?friend .

      # Get the friend's name
      ?friend foaf:name ?friendName .

      # Only include people over 25
      ?person foaf:age ?age .
      FILTER (?age > 25)
    }
    ORDER BY ?name
  `,
  type: 'sparql-select'
});
```

```admonish tip
UNRDF automatically caches SPARQL queries using an LRU cache. Frequently-used queries are served from cache, providing up to 60% faster execution.
```

## Serialization

Convert RDF stores to different formats:

### Turtle Serialization

```javascript
import { toTurtle } from 'unrdf';

const turtle = await toTurtle(store, {
  prefixes: {
    ex: 'http://example.org/',
    foaf: 'http://xmlns.com/foaf/0.1/'
  }
});

console.log(turtle);
```

**Output:**

```turtle
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:alice a foaf:Person ;
         foaf:name "Alice" ;
         foaf:age 30 ;
         foaf:knows ex:bob .

ex:bob a foaf:Person ;
       foaf:name "Bob" ;
       foaf:age 25 .
```

### N-Quads Serialization

```javascript
import { toNQuads } from 'unrdf';

const nquads = await toNQuads(store);

console.log(nquads);
```

**Output:**

```
<http://example.org/alice> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .
<http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" .
<http://example.org/alice> <http://xmlns.com/foaf/0.1/age> "30"^^<http://www.w3.org/2001/XMLSchema#integer> .
```

### JSON-LD Serialization

```javascript
import { toJsonLd } from 'unrdf';

const jsonld = await toJsonLd(store, {
  context: {
    ex: 'http://example.org/',
    foaf: 'http://xmlns.com/foaf/0.1/',
    name: 'foaf:name',
    age: 'foaf:age'
  }
});

console.log(JSON.stringify(jsonld, null, 2));
```

**Output:**

```json
{
  "@context": {
    "ex": "http://example.org/",
    "foaf": "http://xmlns.com/foaf/0.1/",
    "name": "foaf:name",
    "age": "foaf:age"
  },
  "@graph": [
    {
      "@id": "ex:alice",
      "@type": "foaf:Person",
      "name": "Alice",
      "age": 30
    },
    {
      "@id": "ex:bob",
      "@type": "foaf:Person",
      "name": "Bob",
      "age": 25
    }
  ]
}
```

## SHACL Validation

Validate RDF data against SHACL shapes:

```javascript
import { parseTurtle } from 'unrdf';

// Define SHACL shapes
const shapes = await parseTurtle(`
  @prefix sh: <http://www.w3.org/ns/shacl#> .
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

  ex:PersonShape a sh:NodeShape ;
    sh:targetClass foaf:Person ;
    sh:property [
      sh:path foaf:name ;
      sh:minCount 1 ;
      sh:maxCount 1 ;
      sh:datatype xsd:string ;
    ] ;
    sh:property [
      sh:path foaf:age ;
      sh:minCount 1 ;
      sh:datatype xsd:integer ;
      sh:minInclusive 0 ;
      sh:maxInclusive 150 ;
    ] .
`);

// Validate data
const validation = await system.validate({
  dataGraph: store,
  shapesGraph: shapes
});

if (!validation.conforms) {
  console.log('Validation errors:');
  validation.results.forEach(result => {
    console.log(`- ${result.message}`);
  });
}
```

```admonish warning
SHACL validation is performed during transactions when validation hooks are registered. Failed validations cause the transaction to roll back.
```

## Error Handling

Always wrap UNRDF operations in try-catch blocks:

```javascript
try {
  await system.executeTransaction({
    additions: [quad(s, p, o)],
    removals: [],
    actor: 'user'
  });
} catch (error) {
  if (error.message.includes('validation')) {
    console.error('Validation failed:', error.message);
  } else if (error.message.includes('timeout')) {
    console.error('Query timeout:', error.message);
  } else {
    console.error('Transaction failed:', error);
  }

  // Transaction automatically rolled back
  // Store state is preserved
}
```

```admonish info
UNRDF transactions have **automatic rollback** on failure. The store always remains in a consistent state.
```

## Complete Example

Here's a complete example combining all the concepts:

```javascript
import { createDarkMatterCore, parseTurtle, toTurtle } from 'unrdf';
import { namedNode, quad, literal } from '@rdfjs/data-model';

// Create system
const system = await createDarkMatterCore();

try {
  // Load initial data
  const ttl = `
    @prefix ex: <http://example.org/> .
    @prefix foaf: <http://xmlns.com/foaf/0.1/> .

    ex:alice a foaf:Person ;
             foaf:name "Alice" ;
             foaf:age 30 .
  `;

  const store = await parseTurtle(ttl);

  await system.executeTransaction({
    additions: [...store],
    removals: [],
    actor: 'initial-import'
  });

  // Add more data
  await system.executeTransaction({
    additions: [
      quad(
        namedNode('http://example.org/bob'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Bob')
      ),
      quad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/knows'),
        namedNode('http://example.org/bob')
      )
    ],
    removals: [],
    actor: 'admin@example.org'
  });

  // Query data
  const results = await system.query({
    query: `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>

      SELECT ?person ?name ?friend
      WHERE {
        ?person foaf:name ?name ;
                foaf:knows ?friend .
      }
    `,
    type: 'sparql-select'
  });

  console.log('Results:', results);

  // Serialize to Turtle
  const output = await toTurtle(system.store, {
    prefixes: {
      ex: 'http://example.org/',
      foaf: 'http://xmlns.com/foaf/0.1/'
    }
  });

  console.log('Turtle output:\n', output);

} catch (error) {
  console.error('Error:', error);
} finally {
  // Always cleanup
  await system.cleanup();
}
```

## Performance Tips

### 1. Batch Transactions

```javascript
// ❌ Inefficient: Multiple small transactions
for (const person of people) {
  await system.executeTransaction({
    additions: [createPersonQuad(person)],
    removals: [],
    actor: 'import'
  });
}

// ✅ Efficient: Single large transaction
await system.executeTransaction({
  additions: people.map(createPersonQuad),
  removals: [],
  actor: 'import'
});
```

### 2. Use Query Caching

```javascript
// Frequently-used queries are automatically cached
const config = await createDarkMatterCore({
  enableCaching: true,
  cacheSize: 10000 // LRU cache for 10k queries
});
```

### 3. Optimize SPARQL Queries

```javascript
// ❌ Inefficient: Unbounded query
SELECT * WHERE { ?s ?p ?o }

// ✅ Efficient: Specific patterns with LIMIT
SELECT ?person ?name
WHERE {
  ?person a foaf:Person ;
          foaf:name ?name .
}
LIMIT 100
```

## What's Next?

Now that you understand basic RDF operations, continue to:

1. **[First Hook](first-hook.md)** - Create your first Knowledge Hook for autonomic behavior
2. **Advanced Queries** - Learn complex SPARQL patterns
3. **Performance Tuning** - Optimize for production workloads

```admonish tip
For production applications, enable observability to track performance metrics:

```javascript
const system = await createDarkMatterCore({
  enableObservability: true
});

// Access metrics
const metrics = system.getComponent('observability').getPerformanceMetrics();
console.log(`Query p95 latency: ${metrics.latency.p95}ms`);
```
```

---

**Ready for autonomic behavior?** Head to [First Hook](first-hook.md) to learn about Knowledge Hooks.
