# @unrdf/caching Capability Map

**Version**: 1.0.0
**Status**: Production Ready
**Runtime**: Node.js ≥18.0.0
**Last Updated**: 2025-12-28

---

## Overview

Multi-layer caching system for RDF queries with Redis and LRU. Provides intelligent cache invalidation, SPARQL query result caching, semantic cache key generation, and dependency tracking for high-performance RDF operations.

**Key Capabilities**:
- **Multi-Layer Cache**: L1 (Memory LRU) → L2 (Redis) → L3 (Store) hierarchy
- **Smart Invalidation**: Dependency tracking with subject-based invalidation
- **SPARQL Caching**: Cache SELECT/ASK/CONSTRUCT results with semantic keys
- **Performance Optimization**: Sub-millisecond L1 hits, <10ms L2 hits

**Package Exports**:
```javascript
import {
  createCachingSystem,
  MultiLayerCache,
  DependencyTracker,
  SparqlCache
} from '@unrdf/caching';
```

**Dependencies**:
- Required: `@unrdf/oxigraph` (workspace), `lru-cache` (^11.0.2), `msgpackr` (^1.11.2), `zod` (^4.1.13)
- Optional: `ioredis` (^5.4.1) for L2 Redis cache

**Evidence**:
- Test Coverage: Not specified
- Test Files: vitest-based test suite
- OTEL Validation: Not specified
- Example Files: Cache demo with benchmarks

---

## Capability Atoms

### Core Capabilities (Tier 1)

| Atom | Type | Runtime | Evidence | Compositions |
|------|------|---------|----------|--------------|
| `MultiLayerCache` | Class | Node | [src/layers/multi-layer-cache.mjs](file:///home/user/unrdf/packages/caching/src/layers/multi-layer-cache.mjs) | C1 |
| `createMultiLayerCache()` | Function | Node | [src/layers/multi-layer-cache.mjs](file:///home/user/unrdf/packages/caching/src/layers/multi-layer-cache.mjs) | C1 |
| `DependencyTracker` | Class | Node | [src/invalidation/dependency-tracker.mjs](file:///home/user/unrdf/packages/caching/src/invalidation/dependency-tracker.mjs) | C2 |
| `SparqlCache` | Class | Node | [src/query/sparql-cache.mjs](file:///home/user/unrdf/packages/caching/src/query/sparql-cache.mjs) | C3 |
| `createCachingSystem()` | Function | Node | [src/index.mjs](file:///home/user/unrdf/packages/caching/src/index.mjs) | C4 |

**Verification**:
```bash
timeout 5s pnpm --filter @unrdf/caching test
```

---

## Composition Patterns

**C1**: **Multi-Layer Cache** - L1 LRU → L2 Redis → L3 Store
```javascript
import { createMultiLayerCache } from '@unrdf/caching';

const cache = createMultiLayerCache({
  l1MaxSize: 1000,       // L1: In-memory LRU
  redisUrl: 'redis://localhost:6379',  // L2: Redis
  l2TtlSeconds: 3600     // L2: 1 hour TTL
});

const value = await cache.get('key');
if (!value) {
  const computed = await expensiveOperation();
  await cache.set('key', computed);
}
```

**C2**: **Dependency Tracking** - Track dependencies → Invalidate on change
```javascript
import { DependencyTracker } from '@unrdf/caching';

const tracker = new DependencyTracker(cache);

// Track query dependencies
await tracker.trackQuery('query-1', [
  'http://example.org/subject1',
  'http://example.org/subject2'
]);

// Invalidate when subject changes
await tracker.invalidateSubject('http://example.org/subject1');
// query-1 cache entry is invalidated
```

**C3**: **SPARQL Cache** - Cache query results with semantic keys
```javascript
import { SparqlCache } from '@unrdf/caching';

const sparqlCache = new SparqlCache({
  store,
  cache,
  tracker
});

const results = await sparqlCache.query('SELECT ?s ?p ?o WHERE { ?s ?p ?o }');
// Subsequent identical queries served from cache
```

**C4**: **Complete Caching System** - Unified cache + tracker + SPARQL
```javascript
import { createCachingSystem } from '@unrdf/caching';

const system = await createCachingSystem({
  store,
  redisUrl: 'redis://localhost:6379',
  l1MaxSize: 1000,
  l2TtlSeconds: 3600
});

const results = await system.sparqlCache.query('SELECT ...');
const stats = system.getStats();
console.log(`L1 hit rate: ${stats.cache.l1.hitRate}`);
```

---

## Performance Model

**Theoretical Performance**:

Based on cache hierarchy:
- Time Complexity: O(1) for L1 hit, O(1) for L2 hit, O(n) for L3 miss
- Space Complexity: O(k) for cached entries (k=cache size)
- Scalability: Limited by Redis capacity

**Empirical Benchmarks**:

Not available in performance-analysis.md. Package is new.

**Performance Characteristics**:
- L1 (LRU) hit: <1ms (in-memory)
- L2 (Redis) hit: <10ms (network overhead)
- L3 (Store) miss: Depends on query complexity
- Invalidation: O(d) where d=dependencies per key

**Optimization Strategies**:
1. **Semantic Keys**: Hash normalized SPARQL for better hit rates
2. **Batching**: Batch Redis operations
3. **Prefetching**: Pre-warm cache with common queries

**Verification**:
```bash
timeout 30s pnpm --filter @unrdf/caching run benchmark
```

---

## Runtime Compatibility Matrix

| Capability | Node.js | Browser | BEAM/WASM | Notes |
|------------|---------|---------|-----------|-------|
| Multi-Layer Cache | ✅ ≥18.0 | ⚠️ Partial | ❌ Not supported | Browser = L1 only |
| Dependency Tracker | ✅ ≥18.0 | ✅ ES2020+ | ⏳ Planned | Universal logic |
| SPARQL Cache | ✅ ≥18.0 | ⚠️ Partial | ⏳ Planned | Requires Oxigraph |
| Redis (L2) | ✅ ≥18.0 | ❌ Not supported | ❌ Not supported | Server-only |

**Legend**:
- ✅ Fully supported
- ⏳ Planned/In progress
- ❌ Not supported
- ⚠️ Partial support

**Browser Considerations**:
- L1 cache only (no Redis)
- IndexedDB for persistence (future)

**Node.js Considerations**:
- Requires Redis for L2 cache
- ESM-only: Requires `"type": "module"`

---

## Evidence & Verification

### Source Code References

All capability atoms are traceable to source:
- Multi-Layer: [src/layers/multi-layer-cache.mjs](file:///home/user/unrdf/packages/caching/src/layers/multi-layer-cache.mjs)
- Dependency Tracking: [src/invalidation/dependency-tracker.mjs](file:///home/user/unrdf/packages/caching/src/invalidation/dependency-tracker.mjs)
- SPARQL Cache: [src/query/sparql-cache.mjs](file:///home/user/unrdf/packages/caching/src/query/sparql-cache.mjs)

### Verification Commands

**Quick Verification** (< 5 seconds):
```bash
timeout 5s pnpm --filter @unrdf/caching test
```

---

## Cross-References

### Related Packages
- **@unrdf/oxigraph**: RDF store backend
- **@unrdf/streaming**: Real-time invalidation

### External Resources
- [Redis](https://redis.io/)
- [LRU Cache](https://github.com/isaacs/node-lru-cache)

---

**Document Metadata**:
- **Template Version**: 1.0.0
- **Generated**: 2025-12-28
- **Maintainer**: @unrdf/core-team
- **Last Review**: 2025-12-28
- **Next Review**: 2026-03-28
