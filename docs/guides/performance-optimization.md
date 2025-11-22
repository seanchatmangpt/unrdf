# How-To: Optimize Performance

Task-oriented guide for optimizing UNRDF applications.

## Quick Wins

```javascript
// 1. Limit query results
const results = await select(store, `SELECT ?x WHERE { ... } LIMIT 100`);

// 2. Use specific patterns first
const fast = `SELECT ?name WHERE { ?p a foaf:Person ; foaf:name ?name }`;

// 3. Avoid SELECT * in production
const explicit = `SELECT ?name ?email WHERE { ... }`;  // Not SELECT *
```

## How to Optimize SPARQL Queries

### Order Patterns by Selectivity

```javascript
// SLOW: Broad pattern first
const slow = `
  SELECT ?name WHERE {
    ?s ?p ?o .                  # Matches everything
    ?s a foaf:Person .          # Then filters
    ?s foaf:name ?name .
  }
`;

// FAST: Specific pattern first
const fast = `
  SELECT ?name WHERE {
    ?s a foaf:Person .          # Matches few
    ?s foaf:name ?name .        # On small set
  }
`;
```

### Use BIND for Computed Values

```javascript
// Instead of computing in JavaScript
const results = await select(store, `
  SELECT ?name ?salary ?bonus
  WHERE {
    ?p foaf:name ?name ; ex:salary ?salary .
    BIND (?salary * 0.1 AS ?bonus)
  }
`);
```

### Avoid Cartesian Products

```javascript
// BAD: Creates cartesian product
const bad = `
  SELECT ?person ?dept
  WHERE {
    ?person a foaf:Person .
    ?dept a ex:Department .  # No join condition!
  }
`;

// GOOD: Properly joined
const good = `
  SELECT ?person ?dept
  WHERE {
    ?person a foaf:Person ;
            ex:department ?dept .
    ?dept a ex:Department .
  }
`;
```

## How to Cache Query Results

```javascript
class QueryCache {
  constructor(maxAge = 60000) {
    this.cache = new Map();
    this.maxAge = maxAge;
  }

  async query(store, sparql, options = {}) {
    const key = `${sparql}:${JSON.stringify(options)}`;
    const cached = this.cache.get(key);

    if (cached && Date.now() - cached.timestamp < this.maxAge) {
      return cached.results;
    }

    const results = await select(store, sparql, options);
    this.cache.set(key, { results, timestamp: Date.now() });
    return results;
  }

  invalidate(pattern) {
    for (const key of this.cache.keys()) {
      if (key.includes(pattern)) {
        this.cache.delete(key);
      }
    }
  }
}

const cache = new QueryCache(30000);  // 30 second cache
const results = await cache.query(store, 'SELECT ...');
```

## How to Process Large Datasets

### Streaming Parse

```javascript
import { Parser, Store } from 'n3';

async function streamParse(turtleStream) {
  const store = new Store();
  const parser = new Parser();

  return new Promise((resolve, reject) => {
    parser.parse(turtleStream, (error, quad) => {
      if (error) reject(error);
      else if (quad) store.addQuad(quad);
      else resolve(store);
    });
  });
}
```

### Batch Processing

```javascript
async function processInBatches(store, batchSize = 1000) {
  const quads = [...store];
  const results = [];

  for (let i = 0; i < quads.length; i += batchSize) {
    const batch = quads.slice(i, i + batchSize);
    const batchResults = await processBatch(batch);
    results.push(...batchResults);

    // Allow event loop to process other tasks
    await new Promise(resolve => setImmediate(resolve));
  }

  return results;
}
```

### Pagination for Large Results

```javascript
async function* paginatedQuery(store, sparql, pageSize = 100) {
  let offset = 0;

  while (true) {
    const page = await select(store, `
      ${sparql}
      LIMIT ${pageSize}
      OFFSET ${offset}
    `);

    if (page.length === 0) break;

    yield page;
    offset += pageSize;
  }
}

// Usage
for await (const page of paginatedQuery(store, 'SELECT ?x WHERE { ... }')) {
  console.log(`Processing ${page.length} results`);
}
```

## How to Optimize Validation

### Validate Only Changed Data

```javascript
async function validateDelta(oldStore, newStore, shapes) {
  // Find added quads
  const added = [];
  for (const quad of newStore) {
    if (!oldStore.has(quad)) {
      added.push(quad);
    }
  }

  // Create store with only new data
  const deltaStore = new Store(added);

  // Validate delta
  return await validateShacl(deltaStore, shapes);
}
```

### Use Targeted Shapes

```javascript
// Instead of validating everything
const allShapes = `
  ex:PersonShape sh:targetClass foaf:Person ; ...
  ex:ProductShape sh:targetClass ex:Product ; ...
  ex:OrderShape sh:targetClass ex:Order ; ...
`;

// Validate only what you're changing
async function validatePerson(store) {
  const personShapes = `ex:PersonShape sh:targetClass foaf:Person ; ...`;
  return await validateShacl(store, personShapes);
}
```

## How to Reduce Memory Usage

### Use Store Partitioning

```javascript
class PartitionedStore {
  constructor() {
    this.partitions = new Map();
  }

  getPartition(graphName) {
    if (!this.partitions.has(graphName)) {
      this.partitions.set(graphName, new Store());
    }
    return this.partitions.get(graphName);
  }

  addQuad(quad) {
    const graphName = quad.graph?.value || 'default';
    this.getPartition(graphName).addQuad(quad);
  }

  async queryPartition(graphName, sparql) {
    const partition = this.partitions.get(graphName);
    if (!partition) return [];
    return await select(partition, sparql);
  }
}
```

### Clear Unused Data

```javascript
function pruneOldData(store, maxAge) {
  const cutoff = new Date(Date.now() - maxAge).toISOString();

  const toRemove = [];
  for (const quad of store) {
    if (quad.predicate.value === 'http://purl.org/dc/terms/created') {
      if (quad.object.value < cutoff) {
        toRemove.push(quad);
      }
    }
  }

  toRemove.forEach(quad => store.removeQuad(quad));
  console.log(`Removed ${toRemove.length} old quads`);
}
```

## How to Profile Performance

```javascript
class PerformanceProfiler {
  constructor() {
    this.metrics = {};
  }

  async profile(name, fn) {
    const start = performance.now();
    const result = await fn();
    const duration = performance.now() - start;

    if (!this.metrics[name]) {
      this.metrics[name] = { count: 0, total: 0, min: Infinity, max: 0 };
    }

    const m = this.metrics[name];
    m.count++;
    m.total += duration;
    m.min = Math.min(m.min, duration);
    m.max = Math.max(m.max, duration);

    return result;
  }

  report() {
    console.log('\nPerformance Report:');
    for (const [name, m] of Object.entries(this.metrics)) {
      console.log(`${name}:`);
      console.log(`  Count: ${m.count}`);
      console.log(`  Total: ${m.total.toFixed(2)}ms`);
      console.log(`  Avg: ${(m.total / m.count).toFixed(2)}ms`);
      console.log(`  Min: ${m.min.toFixed(2)}ms`);
      console.log(`  Max: ${m.max.toFixed(2)}ms`);
    }
  }
}

const profiler = new PerformanceProfiler();

const results = await profiler.profile('select-people', async () => {
  return await select(store, 'SELECT ?name WHERE { ?p foaf:name ?name }');
});

profiler.report();
```

## Performance Checklist

### Queries

- [ ] Patterns ordered by selectivity
- [ ] No SELECT * in production
- [ ] LIMIT on unbounded queries
- [ ] No cartesian products
- [ ] BIND for computed values

### Caching

- [ ] Frequent queries cached
- [ ] Cache invalidation strategy
- [ ] TTL configured appropriately

### Memory

- [ ] Large datasets streamed
- [ ] Partitioned by graph when possible
- [ ] Old data pruned regularly

### Validation

- [ ] Only validate changed data
- [ ] Targeted shapes (not all-at-once)
- [ ] Severity levels used appropriately

## Related

- [API Reference](../reference/api-reference.md) - Query options
- [Architecture](../explanation/system-design.md) - Performance design
- [Troubleshooting](./troubleshooting.md) - Common issues
