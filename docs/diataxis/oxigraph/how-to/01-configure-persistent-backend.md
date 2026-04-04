# How to Configure the Oxigraph Backend with Query Caching

This guide shows how to use `CachedQueryStore` to add an LRU result cache, automatic cache invalidation on mutations, and query-pattern analysis to the base `OxigraphStore`.

## When to Use This

Use `CachedQueryStore` when:

- Your application issues the same SELECT query repeatedly (e.g. entity detail views, autocomplete, polling)
- You want to measure cache hit rates and average query latency
- Your read-to-write ratio is high (reads outnumber writes)

Do not use it when:

- Your store is updated on every request and cache hits are unlikely
- You need to minimise memory overhead (the cache holds copies of result arrays)

## Step 1 — Import and Create the Cached Store

```javascript
import { createCachedStore } from '@unrdf/oxigraph/query-cache';

const store = createCachedStore({
  cacheSize: 500, // maximum cached query results (LRU eviction)
  cacheTtlMs: 30_000, // entries expire after 30 seconds
  cacheResults: true, // cache SELECT/CONSTRUCT/ASK results
  analyzePatterns: true, // extract query type, variables, predicates
});
```

All four options are optional. Their defaults are:

| Option            | Default | Type    |
| ----------------- | ------- | ------- |
| `cacheSize`       | `1000`  | number  |
| `cacheTtlMs`      | `60000` | number  |
| `cacheResults`    | `true`  | boolean |
| `analyzePatterns` | `true`  | boolean |

## Step 2 — Use the Store Normally

`CachedQueryStore` extends `OxigraphStore`, so every method is identical:

```javascript
import { dataFactory } from '@unrdf/oxigraph';

const { namedNode, literal, triple } = dataFactory;
const ex = s => namedNode(`http://example.org/${s}`);
const foaf = s => namedNode(`http://xmlns.com/foaf/0.1/${s}`);
const rdfType = namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');

store.add(triple(ex('alice'), rdfType, foaf('Person')));
store.add(triple(ex('alice'), foaf('name'), literal('Alice Smith')));

const results = store.query('SELECT ?name WHERE { ?s <http://xmlns.com/foaf/0.1/name> ?name }');
console.log(results.length); // 1 — uncached (first call)

const results2 = store.query('SELECT ?name WHERE { ?s <http://xmlns.com/foaf/0.1/name> ?name }');
console.log(results2.length); // 1 — served from cache
```

The cache key is derived from the normalised query string plus the current `mutationVersion`. Every `add()`, `delete()`, `update()`, or `load()` call increments `mutationVersion`, so stale entries are never returned after a write.

## Step 3 — Read Cache Statistics

```javascript
const stats = store.getStats();

console.log(`Cache size:   ${stats.cache.size} / ${stats.cache.maxSize}`);
console.log(`Cache hits:   ${stats.cache.hits}`);
console.log(`Cache misses: ${stats.cache.misses}`);
console.log(`Hit rate:     ${(stats.cache.hitRate * 100).toFixed(1)}%`);
console.log(`Avg cached query:   ${stats.query.avgCachedTimeMs.toFixed(2)}ms`);
console.log(`Avg uncached query: ${stats.query.avgUncachedTimeMs.toFixed(2)}ms`);
```

`getStats()` returns:

```javascript
{
  cache: { size, maxSize, hits, misses, hitRate },
  query: { total, cached, uncached, totalTimeMs, cachedTimeMs,
           uncachedTimeMs, avgCachedTimeMs, avgUncachedTimeMs },
  patterns: <number of analysed patterns>,
  mutationVersion: <number>
}
```

## Step 4 — Inspect Query Patterns (Optional)

When `analyzePatterns` is `true`, you can retrieve the parsed pattern for any query that has been executed at least once:

```javascript
const query = 'SELECT ?name WHERE { ?s <http://xmlns.com/foaf/0.1/name> ?name }';
store.query(query);

const pattern = store.getQueryPattern(query);
console.log(pattern.type); // 'SELECT'
console.log(pattern.variables); // ['?name']
console.log(pattern.hasFilter); // false
```

Calling `getQueryPattern(query)` on a query that has not been executed yet analyses it immediately and caches the result.

## Step 5 — Use Prepared Queries for Repeated Execution

For queries that run many times with different variable bindings, use `PreparedQuery` to pre-normalise once and bind later:

```javascript
import { prepare } from '@unrdf/oxigraph/query-cache';

const personQuery = prepare(`
  SELECT ?name ?email WHERE {
    ?person <http://xmlns.com/foaf/0.1/name> ?name ;
            <http://schema.org/email>        ?email .
  }
`);

// Execute with bound variables — ?person is substituted at call time
const result = personQuery.execute(store, {
  person: 'http://example.org/alice',
});

console.log(`Executed ${personQuery.executions} time(s)`);
```

Binding substitutes `?variableName` with `<iri>` in the normalised query string. Values that already start with `<` are used as-is; all other values are wrapped: `http://example.org/alice` becomes `<http://example.org/alice>`.

## Step 6 — Manually Clear the Cache

If you need to force fresh results without mutating the store:

```javascript
store.clearQueryCache();
// All cached results and pattern analyses are removed.
```

To reset statistics counters only (cache entries are kept):

```javascript
store.resetStats();
```

## Troubleshooting

**Cache hit rate is 0% even for identical queries.**
Check that `cacheResults: true` is set. Also verify that a mutation (add/delete/update/load) is not occurring between each pair of calls — every mutation increments `mutationVersion`, which is part of the cache key.

**Results from the cache are stale after a load.**
`load()` calls `queryCache.clear()` and increments `mutationVersion`, so this should not occur unless you are bypassing the `CachedQueryStore` API and writing directly to the inner `this.store`.

**Prepared query bindings are not substituting correctly.**
`bind()` uses a regex replacement on the normalised query string. Ensure your variable names in the query match the keys in the bindings object exactly (without the leading `?`).

## See Also

- [reference/oxigraph-store-api.md — CachedQueryStore](../reference/oxigraph-store-api.md#cachedquerystore)
- [reference/configuration-options.md](../reference/configuration-options.md)
