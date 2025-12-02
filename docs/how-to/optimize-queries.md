# How-To: Optimize Queries

**Problem**: Your SPARQL queries are slow, or you need to improve performance for high-throughput RDF applications.

## Solution

Use UNRDF's Dark Matter (80/20) framework to identify critical query patterns, optimize them, and cache results. Apply 80/20 principle: optimize the 20% of queries that represent 80% of your workload.

### Identify Critical Queries

Use `CriticalPathAnalyzer` to find hot queries:

```javascript
import { createKnowledgeSubstrateCore } from 'unrdf/knowledge-engine';

const system = createKnowledgeSubstrateCore({
  config: {
    enablePerformanceTracking: true
  }
});

// Execute queries
for (let i = 0; i < 100; i++) {
  await system.txManager.apply(store, delta);
  system.hooks.evaluateHook(event);
}

// Analyze critical paths
const analyzer = system.criticalPathAnalyzer;
const critical = analyzer.identifyCriticalPaths();

console.log('Top 20% queries by frequency:');
critical.queries.forEach(q => {
  console.log(`${q.pattern}: ${q.frequency} calls, ${q.totalTime}ms`);
});
```

### Query Optimization

Use `QueryOptimizer` for automatic tuning:

```javascript
import { QueryOptimizer } from 'unrdf/knowledge-engine/dark-matter';

const optimizer = new QueryOptimizer({
  cacheSize: 1000,
  enableJoinOptimization: true,
  enableIndexing: true
});

// Optimize query
const optimized = optimizer.optimize(`
  PREFIX schema: <http://schema.org/>

  SELECT ?person ?name ?email WHERE {
    ?person a schema:Person ;
            schema:name ?name ;
            schema:email ?email .
    FILTER (?age > 18)
  }
`);

// Execute optimized query
const results = select(store, optimized.query);
console.log(`Optimization: ${optimized.improvements.join(', ')}`);
```

### Query Result Caching

Cache frequently-used query results:

```javascript
class QueryCache {
  constructor(maxSize = 100) {
    this.cache = new Map();
    this.maxSize = maxSize;
    this.hits = 0;
    this.misses = 0;
  }

  getCacheKey(store, query) {
    const storeHash = getCanonicalHash(store);
    const queryHash = crypto.createHash('sha256').update(query).digest('hex');
    return `${storeHash}:${queryHash}`;
  }

  async query(store, sparql) {
    const key = this.getCacheKey(store, sparql);

    if (this.cache.has(key)) {
      this.hits++;
      return this.cache.get(key);
    }

    this.misses++;
    const results = select(store, sparql);

    // Add to cache
    if (this.cache.size >= this.maxSize) {
      // Evict oldest entry
      const firstKey = this.cache.keys().next().value;
      this.cache.delete(firstKey);
    }

    this.cache.set(key, results);
    return results;
  }

  getHitRate() {
    const total = this.hits + this.misses;
    return total > 0 ? (this.hits / total) * 100 : 0;
  }
}

// Use cache
const cache = new QueryCache(100);

const results1 = await cache.query(store, sparql);  // MISS
const results2 = await cache.query(store, sparql);  // HIT

console.log(`Cache hit rate: ${cache.getHitRate()}%`);
```

### Index Optimization

Use indexes for frequent access patterns:

```javascript
import { getObjects, getSubjects } from 'unrdf/utils';

class IndexedStore {
  constructor(store) {
    this.store = store;
    this.indexes = {
      bySubject: new Map(),
      byPredicate: new Map(),
      byObject: new Map()
    };

    this.buildIndexes();
  }

  buildIndexes() {
    this.store.forEach(quad => {
      // Index by subject
      if (!this.indexes.bySubject.has(quad.subject.value)) {
        this.indexes.bySubject.set(quad.subject.value, []);
      }
      this.indexes.bySubject.get(quad.subject.value).push(quad);

      // Index by predicate
      if (!this.indexes.byPredicate.has(quad.predicate.value)) {
        this.indexes.byPredicate.set(quad.predicate.value, []);
      }
      this.indexes.byPredicate.get(quad.predicate.value).push(quad);
    });
  }

  getQuadsBySubject(subject) {
    return this.indexes.bySubject.get(subject) || [];
  }

  getQuadsByPredicate(predicate) {
    return this.indexes.byPredicate.get(predicate) || [];
  }
}

// Use indexed store
const indexed = new IndexedStore(store);

// Fast lookup (O(1) instead of O(n))
const aliceQuads = indexed.getQuadsBySubject('http://example.org/alice');
```

### Batch Query Execution

Execute multiple queries in parallel:

```javascript
async function batchQuery(store, queries) {
  const results = await Promise.all(
    queries.map(async (query) => ({
      query,
      results: select(store, query),
      duration: performance.now()
    }))
  );

  return results;
}

// Execute 10 queries concurrently
const queries = [...]; // Array of SPARQL queries
const results = await batchQuery(store, queries);
```

### FILTER Optimization

Move FILTER clauses to reduce intermediate results:

```javascript
// ❌ SLOW: FILTER after all joins
const slow = `
  SELECT ?person ?name ?email WHERE {
    ?person a schema:Person .
    ?person schema:name ?name .
    ?person schema:email ?email .
    ?person schema:age ?age .
    FILTER (?age > 18)
  }
`;

// ✅ FAST: FILTER early to reduce joins
const fast = `
  SELECT ?person ?name ?email WHERE {
    ?person a schema:Person .
    ?person schema:age ?age .
    FILTER (?age > 18)
    ?person schema:name ?name .
    ?person schema:email ?email .
  }
`;
```

### Limit Result Size

Use LIMIT and OFFSET for large datasets:

```javascript
function paginateQuery(store, baseQuery, page = 0, pageSize = 100) {
  const offset = page * pageSize;

  const query = `
    ${baseQuery}
    LIMIT ${pageSize}
    OFFSET ${offset}
  `;

  return select(store, query);
}

// Get first 100 results
const page1 = paginateQuery(store, baseQuery, 0, 100);

// Get next 100
const page2 = paginateQuery(store, baseQuery, 1, 100);
```

### Query Analysis

Profile query performance:

```javascript
class QueryProfiler {
  async profile(store, query) {
    const start = performance.now();
    const results = select(store, query);
    const duration = performance.now() - start;

    return {
      query,
      resultCount: results.length,
      duration,
      avgTimePerResult: duration / results.length,
      storeSize: store.size
    };
  }

  async compareQueries(store, queries) {
    const profiles = await Promise.all(
      queries.map(q => this.profile(store, q))
    );

    // Sort by duration
    profiles.sort((a, b) => b.duration - a.duration);

    console.log('Query Performance Report:');
    profiles.forEach((p, i) => {
      console.log(`${i + 1}. ${p.duration.toFixed(2)}ms (${p.resultCount} results)`);
    });

    return profiles;
  }
}
```

## Variations

### Materialized Views

Pre-compute expensive queries:

```javascript
class MaterializedView {
  constructor(store, query, refreshInterval = 60000) {
    this.store = store;
    this.query = query;
    this.refreshInterval = refreshInterval;
    this.cachedResults = null;
    this.lastRefresh = 0;

    this.refresh();
    setInterval(() => this.refresh(), refreshInterval);
  }

  async refresh() {
    this.cachedResults = select(this.store, this.query);
    this.lastRefresh = Date.now();
  }

  getResults() {
    return this.cachedResults;
  }
}

// Create materialized view
const expensiveView = new MaterializedView(store, `
  SELECT ?person ?score WHERE {
    ?person a schema:Person .
    # Complex aggregation...
  }
`, 60000);  // Refresh every minute

// Fast access
const results = expensiveView.getResults();
```

### Adaptive Query Rewriting

Automatically rewrite queries for performance:

```javascript
function rewriteQuery(query, stats) {
  let optimized = query;

  // Move FILTERs earlier
  optimized = optimized.replace(
    /(\?[\w]+\s+\w+:\w+\s+\?[\w]+\s+\.)\s+(FILTER[^}]+)/g,
    '$2\n  $1'
  );

  // Add LIMIT if result set large
  if (stats.avgResultSize > 1000 && !optimized.includes('LIMIT')) {
    optimized += '\nLIMIT 1000';
  }

  return optimized;
}
```

### Dark Matter Query Patterns

Use Dark Matter framework for comprehensive optimization:

```javascript
import { createKnowledgeSubstrateCore } from 'unrdf/knowledge-engine';

const system = createKnowledgeSubstrateCore({
  config: {
    mode: 'performance',  // Focus on 80/20 optimization
    coreComponents: ['txManager', 'hooks', 'observability']
  }
});

// System automatically:
// 1. Tracks query patterns
// 2. Identifies top 20% hot queries
// 3. Applies optimization strategies
// 4. Caches results

// Query through system
const results = await system.query(store, sparql);

// Get performance insights
const metrics = system.observability.getMetrics();
console.log(`Avg query time: ${metrics.avgQueryTime}ms`);
console.log(`Cache hit rate: ${metrics.cacheHitRate}%`);
```

## Related Guides

- [How-To: Query with SPARQL](./query-with-sparql.md) - Query fundamentals
- [Explanation: Dark Matter](../explanation/concepts/knowledge-substrate.md) - 80/20 framework
- [How-To: Assess Data Quality](./assess-data-quality.md) - Quality vs performance tradeoffs
- [Reference: Dark Matter API](../reference/api/knowledge-engine.md#dark-matter) - Complete API
