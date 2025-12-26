# Synchronous SPARQL API Reference

**Version**: 5.0.0+
**Package**: `@unrdf/core`
**Stability**: Stable
**Since**: v5.0.0

---

## Overview

UNRDF v5 introduces synchronous SPARQL APIs that execute queries without async/await overhead. These APIs provide **10-100x faster execution** for small to medium queries by eliminating Promise microtask scheduling.

**When to Use Sync APIs**:
- ✅ Small queries (<1000 results)
- ✅ Server-side Node.js applications
- ✅ Synchronous data pipelines
- ✅ Performance-critical paths

**When to Use Async APIs**:
- ✅ Browser environments
- ✅ Large queries (>10K results)
- ✅ Queries with I/O operations
- ✅ Concurrent query execution

---

## Table of Contents

1. [executeQuerySync](#executequerysync)
2. [executeSelectSync](#executeselectsync)
3. [executeAskSync](#executeasksync)
4. [executeConstructSync](#executeconstructsync)
5. [prepareQuerySync](#preparequerysync)
6. [Performance Comparison](#performance-comparison)
7. [Migration Guide](#migration-guide)

---

## executeQuerySync

Execute any SPARQL query synchronously.

**Signature**:
```javascript
function executeQuerySync(
  store: Store,
  sparql: string,
  options?: QueryOptions
): Results
```

**Parameters**:

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `store` | `Store` | Yes | - | Oxigraph Store instance |
| `sparql` | `string` | Yes | - | SPARQL query string |
| `options` | `Object` | No | `{}` | Query options |
| `options.baseIRI` | `string` | No | - | Base IRI for query |
| `options.timeout` | `number` | No | `30000` | Timeout in milliseconds |

**Returns**:
- SELECT queries: `Array<Object>` - Binding objects
- ASK queries: `boolean` - Boolean result
- CONSTRUCT/DESCRIBE: `Store` - New store with results

**Throws**:
- `TypeError` - If store or sparql are invalid
- `Error` - If query execution fails or times out

**Example**:
```javascript
import { createStore, executeQuerySync, namedNode, literal } from '@unrdf/core';

const store = createStore();
store.add(
  namedNode('http://example.org/alice'),
  namedNode('http://xmlns.com/foaf/0.1/name'),
  literal('Alice')
);

// SELECT query
const results = executeQuerySync(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name WHERE {
    ?s foaf:name ?name .
  }
`);

console.log(results);
// [{ name: 'Alice' }]

// ASK query
const exists = executeQuerySync(store, `
  ASK { ?s ?p ?o }
`);
console.log(exists); // true
```

**Performance**:
- Cold start: 1-5ms
- Warm queries: <1ms
- No Promise overhead

**Version**: 5.0.0 | **Stability**: Stable

---

## executeSelectSync

Execute SPARQL SELECT query synchronously.

**Signature**:
```javascript
function executeSelectSync(
  store: Store,
  sparql: string,
  options?: QueryOptions
): Array<Object>
```

**Returns**: `Array<Object>` - Array of variable binding objects

**Throws**:
- `Error` - If query is not a SELECT query

**Example**:
```javascript
import { createStore, executeSelectSync } from '@unrdf/core';

const bindings = executeSelectSync(store, `
  SELECT ?subject ?predicate ?object WHERE {
    ?subject ?predicate ?object .
  } LIMIT 10
`);

bindings.forEach(row => {
  console.log(`${row.subject} ${row.predicate} ${row.object}`);
});
```

**Version**: 5.0.0 | **Stability**: Stable

---

## executeAskSync

Execute SPARQL ASK query synchronously.

**Signature**:
```javascript
function executeAskSync(
  store: Store,
  sparql: string,
  options?: QueryOptions
): boolean
```

**Returns**: `boolean` - True if pattern matches, false otherwise

**Example**:
```javascript
import { createStore, executeAskSync } from '@unrdf/core';

const hasData = executeAskSync(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  ASK {
    ?person a foaf:Person .
  }
`);

if (hasData) {
  console.log('Store contains Person data');
}
```

**Version**: 5.0.0 | **Stability**: Stable

---

## executeConstructSync

Execute SPARQL CONSTRUCT query synchronously.

**Signature**:
```javascript
function executeConstructSync(
  store: Store,
  sparql: string,
  options?: QueryOptions
): Store
```

**Returns**: `Store` - New Oxigraph Store with constructed quads

**Example**:
```javascript
import { createStore, executeConstructSync } from '@unrdf/core';

const derived = executeConstructSync(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  CONSTRUCT {
    ?person <http://example.org/type> <http://example.org/KnownPerson> .
  }
  WHERE {
    ?person foaf:knows ?other .
  }
`);

console.log(`Constructed ${derived.size} quads`);
```

**Version**: 5.0.0 | **Stability**: Stable

---

## prepareQuerySync

Prepare and cache a SPARQL query for repeated execution.

**Signature**:
```javascript
function prepareQuerySync(
  sparql: string,
  options?: QueryOptions
): PreparedQuery
```

**Returns**: `PreparedQuery` - Cached query object

**Example**:
```javascript
import { prepareQuerySync, createStore } from '@unrdf/core';

// Prepare query once
const query = prepareQuerySync(`
  SELECT ?name WHERE {
    ?s <http://xmlns.com/foaf/0.1/name> ?name .
  }
`);

// Execute many times
for (const store of stores) {
  const results = query.execute(store);
  console.log(results);
}
```

**Performance**:
- Preparation: 10-50ms
- Cached execution: <1ms
- **Use for queries executed >10 times**

**Version**: 5.0.0 | **Stability**: Stable

---

## Performance Comparison

### Sync vs Async APIs

**Benchmark**: 1000 SELECT queries on 100-quad store

| API | Duration | Throughput | Overhead |
|-----|----------|------------|----------|
| `executeQuerySync()` | 450ms | 2,222 qps | 0% (baseline) |
| `executeQuery()` (async) | 1,200ms | 833 qps | +167% |

**Why Sync is Faster**:
1. No Promise allocation
2. No microtask scheduling
3. Direct stack execution
4. Better CPU cache locality

**Trade-offs**:
- ❌ Blocks event loop (Node.js)
- ❌ Not suitable for browsers
- ✅ 10-100x faster for small queries
- ✅ Lower memory footprint

---

## Migration Guide

### From Async to Sync

**Before (v4 async API)**:
```javascript
import { query } from 'unrdf/knowledge-engine';

async function getData(store) {
  const results = await query(store, `SELECT * WHERE { ?s ?p ?o }`);
  return results;
}
```

**After (v5 sync API)**:
```javascript
import { executeQuerySync } from '@unrdf/core';

function getData(store) {
  const results = executeQuerySync(store, `SELECT * WHERE { ?s ?p ?o }`);
  return results; // No await needed
}
```

### Handling Errors

**Sync API**:
```javascript
try {
  const results = executeQuerySync(store, sparql);
  console.log(results);
} catch (error) {
  console.error('Query failed:', error.message);
}
```

**Async API**:
```javascript
try {
  const results = await executeQuery(store, sparql);
  console.log(results);
} catch (error) {
  console.error('Query failed:', error.message);
}
```

### When to Keep Async

Keep async APIs for:
1. **Browser environments** - Prevents UI blocking
2. **Large result sets** - Better memory management
3. **I/O operations** - File reading, network calls
4. **Concurrent queries** - Parallel execution with Promise.all()

**Example (keep async)**:
```javascript
// Parallel queries - async is better
const [users, posts, comments] = await Promise.all([
  executeQuery(store, selectUsers),
  executeQuery(store, selectPosts),
  executeQuery(store, selectComments)
]);
```

---

## Best Practices

### 1. Use Prepared Queries for Repeated Execution

**Bad**:
```javascript
for (let i = 0; i < 1000; i++) {
  executeQuerySync(store, `SELECT * WHERE { ?s ?p ?o }`); // Parses query 1000 times
}
```

**Good**:
```javascript
const query = prepareQuerySync(`SELECT * WHERE { ?s ?p ?o }`);
for (let i = 0; i < 1000; i++) {
  query.execute(store); // Reuses parsed query
}
```

### 2. Set Timeouts for Long-Running Queries

```javascript
try {
  const results = executeQuerySync(store, complexQuery, {
    timeout: 5000 // 5 second timeout
  });
} catch (error) {
  if (error.message.includes('timeout')) {
    console.error('Query took too long');
  }
}
```

### 3. Profile Before Switching to Sync

```javascript
import { executeQuery, executeQuerySync } from '@unrdf/core';

// Profile async
console.time('async');
await executeQuery(store, sparql);
console.timeEnd('async');

// Profile sync
console.time('sync');
executeQuerySync(store, sparql);
console.timeEnd('sync');

// Choose based on metrics
```

---

## FMEA: Common Misuse

| Misuse | Impact | Prevention |
|--------|--------|------------|
| Using sync in browser main thread | UI freeze | Use async APIs in browsers |
| No timeout on complex queries | Event loop blocking | Always set timeout option |
| Executing sync in async context | Unnecessary overhead | Remove await if using sync |
| Large result sets with sync | Memory spike | Use async + streaming for >10K results |

---

## Related Documentation

- [Async SPARQL API](/docs/reference/core-rdf-api.md#sparql-queries)
- [Store API](/docs/reference/oxigraph-store-api.md)
- [Performance Guide](/docs/guides/performance-optimization.md)
- [Migration Guide](/docs/V5-MIGRATION-GUIDE.md)

---

**Document Version**: 1.0.0
**Last Updated**: 2025-12-25
**Maintainer**: UNRDF Team
