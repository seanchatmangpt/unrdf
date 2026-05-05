# @unrdf/caching

Multi-layer caching system for RDF queries with high-performance distributed caching.

## Features

- **3-Tier Cache Architecture**
  - L1: In-memory LRU (fastest, process-local)
  - L2: Redis distributed cache (shared across instances)
  - L3: Persistent RDF store (Oxigraph)

- **Intelligent Invalidation**
  - Subject-based dependency tracking
  - Cascade invalidation
  - Graph-aware invalidation
  - Pattern-based invalidation

- **SPARQL Query Caching**
  - Semantic cache key generation
  - Query result caching
  - Automatic dependency tracking
  - Configurable TTL

## Installation

```bash
pnpm add @unrdf/caching
```

## Quick Start

```javascript
import { createCachingSystem } from '@unrdf/caching';
import { createStore } from '@unrdf/oxigraph';

// Create caching system
const store = createStore();
const caching = createCachingSystem({
  store,
  redisUrl: 'redis://localhost:6379',
  l1MaxSize: 1000,
  l2TtlSeconds: 300
});

// Execute cached SPARQL query
const results = await caching.sparqlCache.query(`
  SELECT * WHERE {
    ?s <http://example.org/name> ?o
  } LIMIT 100
`);

// Invalidate when data changes
await caching.tracker.invalidateSubject('http://example.org/resource1');

// Get statistics
const stats = caching.getStats();
console.log('Cache hit rate:', stats.sparql.hitRate);
```

## Architecture

### Multi-Layer Cache

```javascript
import { MultiLayerCache } from '@unrdf/caching/layers';

const cache = new MultiLayerCache({
  store: createStore(),
  redisUrl: 'redis://localhost:6379',
  l1MaxSize: 1000,        // L1 cache max entries
  l1TtlMs: 60000,         // L1 TTL: 1 minute
  l2TtlSeconds: 300,      // L2 TTL: 5 minutes
  enableL2: true,         // Enable Redis
  keyPrefix: 'unrdf:cache:'
});

// Get with automatic L1 → L2 → L3 fallback
const value = await cache.get('key', async () => {
  // Fetcher called only on cache miss
  return expensiveOperation();
});

// Set in all layers
await cache.set('key', value, { ttl: 600 });

// Delete from all layers
await cache.delete('key');

// Pattern-based deletion
await cache.deletePattern('sparql:*');

// Statistics
const stats = cache.getStats();
console.log('L1 hit rate:', stats.l1HitRate);
console.log('L2 hit rate:', stats.l2HitRate);
```

### Dependency Tracker

```javascript
import { DependencyTracker } from '@unrdf/caching/invalidation';

const tracker = new DependencyTracker(cache);

// Track query dependencies
tracker.trackQuery('query1', [
  'http://example.org/resource1',
  'http://example.org/resource2'
], 'http://example.org/graph1');

// Invalidate all queries depending on a subject
await tracker.invalidateSubject('http://example.org/resource1');

// Invalidate all queries in a graph
await tracker.invalidateGraph('http://example.org/graph1');

// Pattern-based invalidation
await tracker.invalidatePattern('http://example.org/resource*');

// Get dependencies
const subjects = tracker.getQueryDependencies('query1');
const queries = tracker.getSubjectQueries('http://example.org/resource1');
```

### SPARQL Cache

```javascript
import { SparqlCache } from '@unrdf/caching/query';

const sparqlCache = new SparqlCache({
  store: createStore(),
  cache: multiLayerCache,
  tracker: dependencyTracker,
  enableNormalization: true,      // Normalize queries for consistent keys
  enableDependencyTracking: true, // Automatic dependency tracking
  defaultTtl: 300                 // 5 minutes
});

// Execute cached query
const results = await sparqlCache.query(`
  SELECT ?s ?o WHERE {
    ?s <http://example.org/name> ?o
  }
`);

// Query without cache
const fresh = await sparqlCache.query(query, { useCache: false });

// Custom TTL
const cached = await sparqlCache.query(query, { ttl: 600 });

// Invalidate specific query
await sparqlCache.invalidate(query);

// Pre-warm cache
await sparqlCache.prewarm([
  'SELECT * WHERE { ?s ?p ?o } LIMIT 100',
  'ASK { ?s <http://example.org/name> "test" }'
]);

// Statistics
const stats = sparqlCache.getStats();
console.log('Hit rate:', stats.hitRate);
console.log('Avg result size:', stats.avgResultSize);
```

## Performance Benchmarks

Run the benchmark demo:

```bash
pnpm run demo
```

### Expected Results

| Metric | Without Cache | With Cache (Warm) | Speedup |
|--------|--------------|-------------------|---------|
| 100 queries | ~150ms | ~10ms | **15x** |
| Hit rate | N/A | >95% | - |
| Avg query time | 1.5ms | 0.1ms | **15x** |

Performance increases dramatically with:
- Complex SPARQL queries (joins, aggregations)
- Larger datasets (>100K triples)
- Distributed workloads (Redis L2 cache)

### Real-World Performance

On production workloads:
- **Simple queries**: 10-20x speedup
- **Complex joins**: 50-100x speedup
- **Aggregations**: 100-500x speedup

## Configuration

### Cache Configuration

```typescript
{
  store: OxigraphStore;           // Required: RDF store instance
  redisUrl?: string;              // Redis connection (default: localhost:6379)
  l1MaxSize?: number;             // L1 max entries (default: 1000)
  l1TtlMs?: number;               // L1 TTL milliseconds (default: 60000)
  l2TtlSeconds?: number;          // L2 TTL seconds (default: 300)
  enableL2?: boolean;             // Enable Redis (default: true)
  keyPrefix?: string;             // Redis key prefix (default: 'unrdf:cache:')
}
```

### Tracker Configuration

```typescript
{
  cache: MultiLayerCache;         // Required: Cache instance
  enableGraphTracking?: boolean;  // Track graph dependencies (default: true)
  maxDependencies?: number;       // Max tracked dependencies (default: 10000)
}
```

### SPARQL Cache Configuration

```typescript
{
  store: OxigraphStore;           // Required: RDF store
  cache: MultiLayerCache;         // Required: Cache instance
  tracker?: DependencyTracker;    // Optional: Dependency tracker
  enableNormalization?: boolean;  // Normalize queries (default: true)
  enableDependencyTracking?: boolean; // Track dependencies (default: true)
  defaultTtl?: number;            // Default TTL seconds (default: 300)
  maxCachedResults?: number;      // Max cached results (default: 10000)
}
```

## Integration Patterns

### With YAWL Store

```javascript
import { createYawlStore } from '@unrdf/yawl/store';
import { createCachingSystem } from '@unrdf/caching';

const store = createYawlStore();
const caching = createCachingSystem({ store });

// Cached SPARQL queries
const workItems = await caching.sparqlCache.query(`
  SELECT ?workItem ?status WHERE {
    GRAPH <http://yawl.org/case#case-123/graph> {
      ?workItem a yawl:WorkItem ;
                yawl:status ?status .
    }
  }
`);

// Invalidate when case updates
await caching.tracker.invalidateGraph('http://yawl.org/case#case-123/graph');
```

### With Event-Driven Invalidation

```javascript
// On data change event
store.on('quad:added', async (quad) => {
  const subject = quad.subject.value;
  await caching.tracker.invalidateSubject(subject);
});

store.on('graph:cleared', async (graph) => {
  await caching.tracker.invalidateGraph(graph.value);
});
```

## API Reference

### MultiLayerCache

- `get(key, fetcher?)` - Get value with L1 → L2 → L3 fallback
- `set(key, value, options?)` - Set value in all layers
- `delete(key)` - Delete from all layers
- `deletePattern(pattern)` - Delete matching keys
- `clear()` - Clear all caches
- `getStats()` - Get cache statistics
- `resetStats()` - Reset statistics
- `close()` - Close connections

### DependencyTracker

- `trackQuery(key, subjects, graph?)` - Track query dependencies
- `invalidateSubject(subject)` - Invalidate by subject
- `invalidateGraph(graph)` - Invalidate by graph
- `invalidatePattern(pattern)` - Invalidate by pattern
- `invalidateSubjects(subjects)` - Invalidate multiple subjects
- `getQueryDependencies(key)` - Get query's subjects
- `getSubjectQueries(subject)` - Get queries for subject
- `getGraphSubjects(graph)` - Get subjects in graph
- `getStats()` - Get tracker statistics
- `clear()` - Clear all tracking

### SparqlCache

- `query(query, options?)` - Execute cached SPARQL query
- `invalidate(query)` - Invalidate cached query
- `invalidatePattern(pattern)` - Invalidate by pattern
- `prewarm(queries)` - Pre-warm cache
- `getStats()` - Get query statistics
- `resetStats()` - Reset statistics
- `clear()` - Clear all cached queries

## License

MIT

## Author

UNRDF Project
