# Getting Started with unrdf

This guide will help you get up and running with unrdf quickly.

## Installation

Install unrdf using your preferred package manager:

```bash
# npm
npm install unrdf

# pnpm
pnpm add unrdf

# yarn
yarn add unrdf
```

## Basic Usage

### 1. Create a Store

```javascript
import { useStore } from 'unrdf';

// Create an empty store
const store = useStore();

// Or create a store with initial data
const storeWithData = useStore([
  // ... initial quads
]);
```

### 2. Parse Turtle Data

```javascript
import { useTurtle } from 'unrdf';

const turtle = useTurtle();
const store = await turtle.parse(`
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  
  ex:person a foaf:Person ;
    foaf:name "John Doe" ;
    foaf:age 30 .
`);
```

### 3. Query with SPARQL

```javascript
import { useGraph } from 'unrdf';

const graph = useGraph(store);

// SELECT query
const results = await graph.select(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name ?age WHERE {
    ?person a foaf:Person ;
      foaf:name ?name ;
      foaf:age ?age .
  }
`);

// ASK query
const hasPersons = await graph.ask(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  ASK { ?person a foaf:Person }
`);
```

### 4. Validate with SHACL

```javascript
import { useValidator } from 'unrdf';

const validator = useValidator();

const shapes = `
  @prefix sh: <http://www.w3.org/ns/shacl#> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  
  foaf:PersonShape a sh:NodeShape ;
    sh:targetClass foaf:Person ;
    sh:property [
      sh:path foaf:name ;
      sh:minCount 1 ;
      sh:datatype xsd:string
    ] .
`;

const validation = await validator.validate(store, shapes);
if (!validation.valid) {
  console.log('Validation errors:', validation.errors);
}
```

## Working with Terms

```javascript
import { useTerms } from 'unrdf';

const terms = useTerms();

// Create RDF terms
const subject = terms.iri('http://example.org/person');
const predicate = terms.iri('http://xmlns.com/foaf/0.1/name');
const object = terms.lit('John Doe');
const graph = terms.iri('http://example.org/graph');

// Create a quad
const quad = terms.quad(subject, predicate, object, graph);
```

## Working with Prefixes

```javascript
import { usePrefixes } from 'unrdf';

const prefixes = usePrefixes({
  'ex': 'http://example.org/',
  'foaf': 'http://xmlns.com/foaf/0.1/'
});

// Expand CURIEs
const fullIRI = prefixes.expand('ex:person'); // http://example.org/person

// Shrink IRIs
const curie = prefixes.shrink('http://example.org/person'); // ex:person
```

## JSON-LD Integration

```javascript
import { useJsonLd } from 'unrdf';

const jsonld = useJsonLd();

// Convert store to JSON-LD
const doc = await jsonld.toJSONLD(store, {
  context: {
    '@vocab': 'http://example.org/',
    'name': 'http://xmlns.com/foaf/0.1/name'
  }
});

// Convert JSON-LD to store
const storeFromJsonld = await jsonld.fromJSONLD({
  '@context': { '@vocab': 'http://example.org/' },
  '@graph': [
    {
      '@id': 'person1',
      '@type': 'Person',
      'name': 'John Doe'
    }
  ]
});
```

## Graph Traversal with Clownface

```javascript
import { usePointer } from 'unrdf';

const pointer = usePointer(store);

// Get a pointer to a specific node
const personPointer = pointer.node('ex:person');

// Traverse the graph
const name = personPointer.out('foaf:name').value;
const friends = personPointer.out('foaf:knows').toArray();

// Filter by type
const persons = pointer.ofType('foaf:Person').toArray();
```

## Type Safety with Zod

```javascript
import { useZod } from 'unrdf';
import { z } from 'zod';

const zod = useZod();

// Define a schema
const PersonSchema = z.object({
  name: z.string(),
  age: z.number().optional(),
  email: z.string().email().optional()
});

// Validate data
const personData = { name: 'John Doe', age: 30 };
const validation = zod.validate(PersonSchema, personData);

if (validation.success) {
  console.log('Valid person:', validation.data);
} else {
  console.log('Validation errors:', validation.errors);
}
```

## Performance Monitoring

```javascript
import { useMetrics } from 'unrdf';

const metrics = useMetrics();

// Wrap functions with metrics
const wrappedQuery = metrics.wrap('sparql-query', async () => {
  return await graph.select('SELECT * WHERE { ?s ?p ?o }');
});

// Create timers
const timer = metrics.timer('complex-operation');
// ... do work ...
timer.end();

// Get performance data
const lastMetric = metrics.last();
const timeline = metrics.timeline();
```

## Caching

```javascript
import { useCache } from 'unrdf';

const cache = useCache({
  maxSize: 1000,
  defaultTTL: 300000 // 5 minutes
});

// Cache expensive operations
const result = await cache.get('expensive-query', async () => {
  return await graph.select('SELECT * WHERE { ?s ?p ?o }');
});

// Cache with custom TTL
const result2 = await cache.get('long-term-query', async () => {
  return await expensiveOperation();
}, { ttl: 3600000 }); // 1 hour
```

## Next Steps

- Explore the [Core Concepts](./core-concepts.md) to understand unrdf's philosophy
- Check out the [Composables API](./composables/) for detailed documentation
- See [Examples](./examples.md) for real-world usage patterns
- Read the [API Reference](./api-reference.md) for complete technical details