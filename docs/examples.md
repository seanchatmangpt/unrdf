# Examples

Real-world usage patterns and examples for unrdf.

## Basic Examples

### Hello World

```javascript
import { useStore, useTurtle, useGraph } from 'unrdf';

// Create a store and parse some Turtle data
const store = useStore();
const turtle = useTurtle();

await turtle.parse(`
  @prefix ex: <http://example.org/> .
  ex:person a ex:Person ;
    ex:name "John Doe" .
`);

// Query the data
const graph = useGraph(store);
const results = await graph.select(`
  PREFIX ex: <http://example.org/>
  SELECT ?name WHERE {
    ?person a ex:Person ;
      ex:name ?name .
  }
`);

console.log(results); // [{ name: "John Doe" }]
```

### Working with Terms

```javascript
import { useTerms, useStore } from 'unrdf';

const terms = useTerms();
const store = useStore();

// Create RDF terms
const person = terms.iri('http://example.org/person');
const name = terms.iri('http://example.org/name');
const nameValue = terms.lit('John Doe');

// Create a quad
const quad = terms.quad(person, name, nameValue);

// Add to store
store.add([quad]);
```

## Intermediate Examples

### Data Validation with SHACL

```javascript
import { useStore, useTurtle, useValidator } from 'unrdf';

const store = useStore();
const turtle = useTurtle();
const validator = useValidator();

// Parse data
await turtle.parse(`
  @prefix ex: <http://example.org/> .
  ex:person a ex:Person ;
    ex:name "John Doe" ;
    ex:age 30 .
`);

// Define SHACL shapes
const shapes = `
  @prefix sh: <http://www.w3.org/ns/shacl#> .
  @prefix ex: <http://example.org/> .
  
  ex:PersonShape a sh:NodeShape ;
    sh:targetClass ex:Person ;
    sh:property [
      sh:path ex:name ;
      sh:minCount 1 ;
      sh:datatype xsd:string
    ] ;
    sh:property [
      sh:path ex:age ;
      sh:minCount 1 ;
      sh:datatype xsd:integer ;
      sh:minInclusive 0
    ] .
`;

// Validate
const validation = await validator.validate(store, shapes);

if (validation.valid) {
  console.log('Data is valid');
} else {
  console.log('Validation errors:', validation.errors);
}
```

### Graph Traversal with Clownface

```javascript
import { useStore, useTurtle, usePointer } from 'unrdf';

const store = useStore();
const turtle = useTurtle();
const pointer = usePointer(store);

// Parse data
await turtle.parse(`
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  
  ex:person a foaf:Person ;
    foaf:name "John Doe" ;
    foaf:knows ex:friend .
    
  ex:friend a foaf:Person ;
    foaf:name "Jane Smith" .
`);

// Traverse the graph
const personPointer = pointer.node('ex:person');

// Get name
const name = personPointer.out('foaf:name').value;
console.log('Name:', name); // "John Doe"

// Get friends
const friends = personPointer.out('foaf:knows').toArray();
console.log('Friends:', friends);

// Filter by type
const persons = pointer.ofType('foaf:Person').toArray();
console.log('All persons:', persons);
```

### JSON-LD Integration

```javascript
import { useStore, useJsonLd } from 'unrdf';

const store = useStore();
const jsonld = useJsonLd();

// Convert store to JSON-LD
const doc = await jsonld.toJSONLD(store, {
  context: {
    '@vocab': 'http://example.org/',
    'name': 'http://xmlns.com/foaf/0.1/name'
  }
});

console.log('JSON-LD:', JSON.stringify(doc, null, 2));

// Convert JSON-LD to store
const jsonldData = {
  '@context': {
    '@vocab': 'http://example.org/',
    'name': 'http://xmlns.com/foaf/0.1/name'
  },
  '@graph': [
    {
      '@id': 'person1',
      '@type': 'Person',
      'name': 'John Doe'
    }
  ]
};

const newStore = await jsonld.fromJSONLD(jsonldData);
```

## Advanced Examples

### Reasoning with N3 Rules

```javascript
import { useStore, useTurtle, useReasoner } from 'unrdf';

const store = useStore();
const turtle = useTurtle();
const reasoner = useReasoner();

// Parse data
await turtle.parse(`
  @prefix ex: <http://example.org/> .
  ex:john ex:parent ex:mary .
  ex:mary ex:parent ex:alice .
`);

// Define N3 rules
const rules = `
  @prefix ex: <http://example.org/> .
  
  # Parent rule
  { ?person ex:parent ?parent } => { ?parent ex:child ?person } .
  
  # Grandparent rule
  { ?person ex:parent ?parent . ?parent ex:parent ?grandparent } => 
    { ?person ex:grandparent ?grandparent } .
`;

// Apply reasoning
const reasonedStore = await reasoner.reason(store, rules);

// Query for inferred facts
const graph = useGraph(reasonedStore);
const children = await graph.select(`
  PREFIX ex: <http://example.org/>
  SELECT ?parent ?child WHERE {
    ?parent ex:child ?child .
  }
`);

console.log('Children:', children);
```

### Canonicalization and Isomorphism

```javascript
import { useStore, useTurtle, useCanon } from 'unrdf';

const store1 = useStore();
const store2 = useStore();
const turtle = useTurtle();
const canon = useCanon();

// Parse same data in different order
await turtle.parse(`
  @prefix ex: <http://example.org/> .
  ex:person ex:name "John" .
  ex:person ex:age 30 .
`);

await turtle.parse(`
  @prefix ex: <http://example.org/> .
  ex:person ex:age 30 .
  ex:person ex:name "John" .
`);

// Check if stores are isomorphic
const areIsomorphic = await canon.isomorphic(store1, store2);
console.log('Are isomorphic:', areIsomorphic); // true

// Canonicalize a store
const canonicalStore = await canon.canonicalize(store1);
```

### Performance Monitoring

```javascript
import { useStore, useTurtle, useGraph, useMetrics } from 'unrdf';

const store = useStore();
const turtle = useTurtle();
const graph = useGraph(store);
const metrics = useMetrics();

// Parse data
await turtle.parse(`
  @prefix ex: <http://example.org/> .
  ex:person a ex:Person ;
    ex:name "John Doe" .
`);

// Wrap expensive operations with metrics
const wrappedQuery = metrics.wrap('sparql-query', async () => {
  return await graph.select(`
    PREFIX ex: <http://example.org/>
    SELECT ?name WHERE {
      ?person a ex:Person ;
        ex:name ?name .
    }
  `);
});

// Execute and monitor
const results = await wrappedQuery();

// Get performance data
const lastMetric = metrics.last();
console.log('Query took:', lastMetric.duration, 'ms');
```

### Caching Expensive Operations

```javascript
import { useStore, useTurtle, useGraph, useCache } from 'unrdf';

const store = useStore();
const turtle = useTurtle();
const graph = useGraph(store);
const cache = useCache({
  maxSize: 1000,
  defaultTTL: 300000 // 5 minutes
});

// Parse data
await turtle.parse(`
  @prefix ex: <http://example.org/> .
  ex:person a ex:Person ;
    ex:name "John Doe" .
`);

// Cache expensive query
const results = await cache.get('person-query', async () => {
  return await graph.select(`
    PREFIX ex: <http://example.org/>
    SELECT ?name WHERE {
      ?person a ex:Person ;
        ex:name ?name .
    }
  `);
});

// Subsequent calls will use cache
const cachedResults = await cache.get('person-query', async () => {
  return await graph.select(`
    PREFIX ex: <http://example.org/>
    SELECT ?name WHERE {
      ?person a ex:Person ;
        ex:name ?name .
    }
  `);
});
```

## Real-World Examples

### Building a Knowledge Graph

```javascript
import { 
  useStore, useTurtle, useGraph, useValidator, 
  useReasoner, useCanon, useJsonLd 
} from 'unrdf';

class KnowledgeGraph {
  constructor() {
    this.store = useStore();
    this.turtle = useTurtle();
    this.graph = useGraph(this.store);
    this.validator = useValidator();
    this.reasoner = useReasoner();
    this.canon = useCanon();
    this.jsonld = useJsonLd();
  }

  async addData(turtleData) {
    const parsedStore = await this.turtle.parse(turtleData);
    // Merge with existing store
    this.store.add(parsedStore.getQuads(null, null, null, null));
  }

  async validate(shapes) {
    return await this.validator.validate(this.store, shapes);
  }

  async reason(rules) {
    this.store = await this.reasoner.reason(this.store, rules);
  }

  async query(sparql) {
    return await this.graph.select(sparql);
  }

  async export(format = 'Turtle') {
    switch (format) {
      case 'Turtle':
        return await this.graph.serialize({ format: 'Turtle' });
      case 'JSON-LD':
        return await this.jsonld.toJSONLD(this.store);
      default:
        throw new Error(`Unsupported format: ${format}`);
    }
  }

  async canonicalize() {
    this.store = await this.canon.canonicalize(this.store);
  }
}

// Usage
const kg = new KnowledgeGraph();

await kg.addData(`
  @prefix ex: <http://example.org/> .
  ex:john a ex:Person ;
    ex:name "John Doe" ;
    ex:age 30 .
`);

const results = await kg.query(`
  PREFIX ex: <http://example.org/>
  SELECT ?name WHERE {
    ?person a ex:Person ;
      ex:name ?name .
  }
`);

console.log('Results:', results);
```

### Data Pipeline

```javascript
import { 
  useStore, useTurtle, useGraph, useValidator, 
  useReasoner, useCanon, useJsonLd, useMetrics 
} from 'unrdf';

class DataPipeline {
  constructor() {
    this.store = useStore();
    this.turtle = useTurtle();
    this.graph = useGraph(this.store);
    this.validator = useValidator();
    this.reasoner = useReasoner();
    this.canon = useCanon();
    this.jsonld = useJsonLd();
    this.metrics = useMetrics();
  }

  async process(turtleData, shapes, rules) {
    // Step 1: Parse data
    const parseTimer = this.metrics.timer('parse');
    const parsedStore = await this.turtle.parse(turtleData);
    parseTimer.end();

    // Step 2: Validate
    const validateTimer = this.metrics.timer('validate');
    const validation = await this.validator.validate(parsedStore, shapes);
    validateTimer.end();

    if (!validation.valid) {
      throw new Error(`Validation failed: ${validation.errors.join(', ')}`);
    }

    // Step 3: Add to store
    this.store.add(parsedStore.getQuads(null, null, null, null));

    // Step 4: Apply reasoning
    const reasonTimer = this.metrics.timer('reason');
    this.store = await this.reasoner.reason(this.store, rules);
    reasonTimer.end();

    // Step 5: Canonicalize
    const canonTimer = this.metrics.timer('canonicalize');
    this.store = await this.canon.canonicalize(this.store);
    canonTimer.end();

    // Step 6: Export
    const exportTimer = this.metrics.timer('export');
    const jsonld = await this.jsonld.toJSONLD(this.store);
    exportTimer.end();

    return {
      store: this.store,
      jsonld,
      metrics: this.metrics.timeline()
    };
  }
}

// Usage
const pipeline = new DataPipeline();

const result = await pipeline.process(
  turtleData,
  shapes,
  rules
);

console.log('Processing complete');
console.log('Metrics:', result.metrics);
```

### API Server

```javascript
import { 
  useStore, useTurtle, useGraph, useValidator, 
  useReasoner, useCanon, useJsonLd, useCache 
} from 'unrdf';

class RDFAPI {
  constructor() {
    this.store = useStore();
    this.turtle = useTurtle();
    this.graph = useGraph(this.store);
    this.validator = useValidator();
    this.reasoner = useReasoner();
    this.canon = useCanon();
    this.jsonld = useJsonLd();
    this.cache = useCache();
  }

  async addData(turtleData) {
    const parsedStore = await this.turtle.parse(turtleData);
    this.store.add(parsedStore.getQuads(null, null, null, null));
    return { success: true };
  }

  async query(sparql) {
    return await this.cache.get(`query:${sparql}`, async () => {
      return await this.graph.select(sparql);
    });
  }

  async validate(shapes) {
    return await this.validator.validate(this.store, shapes);
  }

  async reason(rules) {
    this.store = await this.reasoner.reason(this.store, rules);
    return { success: true };
  }

  async export(format = 'Turtle') {
    switch (format) {
      case 'Turtle':
        return await this.graph.serialize({ format: 'Turtle' });
      case 'JSON-LD':
        return await this.jsonld.toJSONLD(this.store);
      default:
        throw new Error(`Unsupported format: ${format}`);
    }
  }
}

// Usage with Express.js
import express from 'express';

const app = express();
const rdfAPI = new RDFAPI();

app.post('/data', async (req, res) => {
  try {
    const result = await rdfAPI.addData(req.body);
    res.json(result);
  } catch (error) {
    res.status(400).json({ error: error.message });
  }
});

app.post('/query', async (req, res) => {
  try {
    const results = await rdfAPI.query(req.body.sparql);
    res.json(results);
  } catch (error) {
    res.status(400).json({ error: error.message });
  }
});

app.get('/export/:format', async (req, res) => {
  try {
    const data = await rdfAPI.export(req.params.format);
    res.send(data);
  } catch (error) {
    res.status(400).json({ error: error.message });
  }
});
```

## Best Practices

### 1. Error Handling

```javascript
import { useStore, useTurtle, useGraph } from 'unrdf';

const store = useStore();
const turtle = useTurtle();
const graph = useGraph(store);

try {
  // Parse data
  await turtle.parse(turtleData);
  
  // Query data
  const results = await graph.select(sparql);
  
  console.log('Results:', results);
} catch (error) {
  console.error('Error:', error.message);
  // Handle error appropriately
}
```

### 2. Performance Optimization

```javascript
import { useStore, useTurtle, useGraph, useCache, useMetrics } from 'unrdf';

const store = useStore();
const turtle = useTurtle();
const graph = useGraph(store);
const cache = useCache();
const metrics = useMetrics();

// Cache expensive operations
const results = await cache.get('expensive-query', async () => {
  return await graph.select(expensiveSparql);
});

// Monitor performance
const wrappedOperation = metrics.wrap('operation', async () => {
  return await expensiveOperation();
});
```

### 3. Data Validation

```javascript
import { useStore, useTurtle, useValidator } from 'unrdf';

const store = useStore();
const turtle = useTurtle();
const validator = useValidator();

// Parse and validate data
const parsedStore = await turtle.parse(turtleData);
const validation = await validator.validate(parsedStore, shapes);

if (!validation.valid) {
  throw new Error(`Validation failed: ${validation.errors.join(', ')}`);
}

// Add validated data to store
store.add(parsedStore.getQuads(null, null, null, null));
```

### 4. Resource Management

```javascript
import { useStore, useTurtle, useGraph } from 'unrdf';

// Create store once and reuse
const store = useStore();
const turtle = useTurtle();
const graph = useGraph(store);

// Process multiple data sources
const dataSources = ['data1.ttl', 'data2.ttl', 'data3.ttl'];

for (const source of dataSources) {
  const data = await readFile(source);
  await turtle.parse(data);
}

// Query all data
const results = await graph.select(`
  SELECT * WHERE { ?s ?p ?o }
`);
```

## See Also

- [Getting Started](./getting-started.md) - Basic usage guide
- [Core Concepts](./core-concepts.md) - Understanding unrdf's philosophy
- [Composables](./composables/) - Complete API reference
- [API Reference](./api-reference.md) - Technical documentation
