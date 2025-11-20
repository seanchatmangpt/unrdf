# Knowledge Engine Deep Dive

The Knowledge Engine is the heart of UNRDF. This chapter covers every API, configuration option, and advanced pattern.

## Creating an Engine

### Basic Creation

```typescript
import { createKnowledgeEngine } from 'unrdf';

const engine = await createKnowledgeEngine();
```

### Advanced Configuration

```typescript
const engine = await createKnowledgeEngine({
  // Store configuration
  store: {
    type: 'memory', // or 'persistent', 'distributed'
    backend: 'n3', // or 'oxigraph', 'postgres'
    connectionString: process.env.DATABASE_URL
  },
  
  // Transaction settings
  transactions: {
    isolation: 'serializable', // ACID level
    timeout: 30000, // 30 seconds
    retries: 3,
    autoCommit: false
  },
  
  // Query optimization
  query: {
    enableCache: true,
    cacheSize: '1GB',
    cacheTTL: 3600,
    optimizer: 'aggressive', // or 'conservative', 'disabled'
    parallelism: 4 // Parallel query execution
  },
  
  // Knowledge Hooks
  hooks: {
    enabled: true,
    sandbox: 'isolated-vm', // or 'vm2', 'worker-threads'
    timeout: 5000,
    memoryLimit: '128MB'
  },
  
  // Policy Packs
  policies: {
    enabled: true,
    strictMode: true, // Fail on policy violations
    packs: ['compliance-v1', 'security-v2']
  },
  
  // Browser features
  browser: {
    enableIndexedDB: true,
    dbName: 'unrdf-knowledge-graph',
    syncInterval: 5000 // Sync with server every 5s
  },
  
  // Streaming
  streaming: {
    enabled: true,
    batchSize: 100,
    flushInterval: 1000,
    enableWindowing: true
  },
  
  // Federation
  federation: {
    enabled: false,
    topology: 'mesh', // or 'hierarchical', 'ring', 'star'
    consensus: 'byzantine-ftb',
    nodes: []
  },
  
  // Observability
  observability: {
    enabled: true,
    serviceName: 'knowledge-api',
    exporters: ['console', 'jaeger'],
    sampleRate: 1.0, // 100% in dev, 0.1 in prod
    includeQueries: true,
    includeResults: false // Don't log PII
  }
});
```

## Core Operations

### Inserting Data

```typescript
import { namedNode, literal, quad } from '@rdfjs/data-model';

// Single triple
await engine.insert([
  quad(
    namedNode('http://example.org/alice'),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal('Alice'),
    namedNode('http://example.org/graph1')
  )
]);

// Batch insert (more efficient)
const quads = [/* array of quads */];
await engine.insert(quads);

// With options
await engine.insert(quads, {
  graph: namedNode('http://example.org/default'),
  validate: true, // Run Policy Pack validation
  notify: true, // Trigger Knowledge Hooks
  transaction: existingTransaction // Use existing tx
});
```

### Deleting Data

```typescript
// Delete specific quads
await engine.delete([
  quad(subject, predicate, object, graph)
]);

// Delete by pattern (all triples about alice)
await engine.deleteMatching({
  subject: namedNode('http://example.org/alice')
});

// Delete entire graph
await engine.deleteGraph(namedNode('http://example.org/graph1'));
```

### Updating Data

```typescript
// Atomic update (delete + insert)
await engine.update({
  delete: [
    quad(alice, foaf.age, literal('30'))
  ],
  insert: [
    quad(alice, foaf.age, literal('31'))
  ]
});
```

## Querying

### Basic SPARQL

```typescript
const results = await engine.query(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name ?age WHERE {
    ?person foaf:name ?name ;
            foaf:age ?age .
  }
  ORDER BY ?name
  LIMIT 10
`);

// Results: Array<{ name: Term, age: Term }>
```

### Type-Safe Queries

```typescript
import { z } from 'zod';

const PersonSchema = z.object({
  name: z.string(),
  age: z.number(),
  email: z.string().email().optional()
});

const results = await engine.queryTyped({
  query: `
    SELECT ?name ?age ?email WHERE {
      ?person foaf:name ?name ;
              foaf:age ?age .
      OPTIONAL { ?person foaf:mbox ?email }
    }
  `,
  schema: PersonSchema
});

// Results: Array<{ name: string, age: number, email?: string }>
// Fully type-safe! IDE autocomplete works!
```

### Parameterized Queries

```typescript
// Safe from SPARQL injection
const results = await engine.query({
  query: `
    SELECT ?name WHERE {
      ?person foaf:name ?name ;
              foaf:age ?age .
      FILTER(?age > ?minAge)
    }
  `,
  bindings: {
    minAge: 18
  }
});
```

### Semantic Search

```typescript
// Vector-based similarity search
const results = await engine.semanticSearch({
  query: 'Find laptops with good battery life',
  limit: 10,
  minSimilarity: 0.7,
  fields: ['schema:name', 'schema:description']
});
```

## Transactions

### Manual Transactions

```typescript
const tx = await engine.beginTransaction();

try {
  await engine.insert([...], { transaction: tx });
  await engine.delete([...], { transaction: tx });
  
  await tx.commit();
} catch (error) {
  await tx.rollback();
  throw error;
}
```

### Automatic Transactions

```typescript
// Wraps operation in transaction automatically
await engine.transaction(async (tx) => {
  await engine.insert([...], { transaction: tx });
  await engine.delete([...], { transaction: tx });
  // Auto-commits on success, rolls back on error
});
```

### Read-Only Transactions

```typescript
// Snapshot isolation for consistent reads
const results = await engine.transaction(async (tx) => {
  const users = await engine.query(userQuery, { transaction: tx });
  const posts = await engine.query(postQuery, { transaction: tx });
  return { users, posts };
}, { readOnly: true });
```

## Performance Optimization

### Query Profiling

```typescript
const { results, metrics } = await engine.queryWithMetrics(sparql);

console.log(metrics);
// {
//   duration: 42, // ms
//   quadsScanned: 1000,
//   cacheHit: false,
//   planningTime: 2,
//   executionTime: 40
// }
```

### Explain Plans

```typescript
const plan = await engine.explainQuery(sparql);

console.log(plan);
// {
//   type: 'join',
//   left: { type: 'scan', pattern: '?s ?p ?o' },
//   right: { type: 'index', field: 'subject' },
//   estimatedCost: 100
// }
```

### Batch Operations

```typescript
// More efficient than individual inserts
await engine.batch([
  { type: 'insert', quads: [...] },
  { type: 'delete', quads: [...] },
  { type: 'insert', quads: [...] }
]);
```

## Advanced Features

### Named Graphs

```typescript
// Query specific graph
const results = await engine.query(sparql, {
  defaultGraph: namedNode('http://example.org/graph1')
});

// Query union of graphs
const results = await engine.query(sparql, {
  graphs: [
    namedNode('http://example.org/graph1'),
    namedNode('http://example.org/graph2')
  ],
  unionDefaultGraph: true
});
```

### Reasoning

```typescript
// Enable OWL reasoning
const engine = await createKnowledgeEngine({
  reasoning: {
    enabled: true,
    profile: 'RDFS', // or 'OWL-DL', 'OWL-FULL'
    materialize: true // Pre-compute inferences
  }
});

// Queries include inferred triples
const results = await engine.query(sparql);
```

### Serialization

```typescript
// Export to different formats
const turtle = await engine.serialize('turtle');
const jsonld = await engine.serialize('jsonld');
const ntriples = await engine.serialize('ntriples');
const rdfxml = await engine.serialize('rdfxml');

// Import from formats
await engine.deserialize(turtleData, 'turtle');
```

## Next Steps

- **[Transaction Management](./transactions.md)**
- **[Query Optimization](./query-optimization.md)**
- **[Store Operations](./store-ops.md)**

---

> **💡 Pro Tip:** Always use `queryTyped()` with Zod schemas for type safety and runtime validation.
