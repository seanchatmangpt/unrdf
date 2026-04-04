# Configuration Options Reference

All options objects accepted by `@unrdf/oxigraph` APIs.

---

## `LoadOptions`

Passed to `store.load(data, options)`.

| Field           | Type        | Required | Default       | Description                                                                        |
| --------------- | ----------- | -------- | ------------- | ---------------------------------------------------------------------------------- |
| `format`        | `string`    | Yes      | —             | RDF serialization format. See [Format Strings](#format-strings).                   |
| `baseIri`       | `string`    | No       | —             | Base IRI for resolving relative IRIs in the document. Also accepted as `base_iri`. |
| `toNamedGraph`  | `NamedNode` | No       | default graph | Load all triples into this named graph instead of the default graph.               |
| `unchecked`     | `boolean`   | No       | `false`       | Disable per-triple validation. Use only for trusted, pre-validated data.           |
| `noTransaction` | `boolean`   | No       | `false`       | Disable transactional guarantees for the load operation. Faster but not atomic.    |

**Example:**

```javascript
store.load(turtleString, {
  format: 'text/turtle',
  baseIri: 'http://example.org/',
  toNamedGraph: dataFactory.namedNode('http://example.org/graphs/people'),
});
```

---

## `DumpOptions`

Passed to `store.dump(options)`.

| Field            | Type        | Required | Default | Description                                                                                |
| ---------------- | ----------- | -------- | ------- | ------------------------------------------------------------------------------------------ |
| `format`         | `string`    | Yes      | —       | RDF serialization format. See [Format Strings](#format-strings).                           |
| `fromNamedGraph` | `NamedNode` | No       | —       | Export only this named graph. When omitted, all triples in the default graph are exported. |

**Example:**

```javascript
// Export entire default graph
const turtle = store.dump({ format: 'text/turtle' });

// Export one named graph
const graph = store.dump({
  format: 'application/n-quads',
  fromNamedGraph: dataFactory.namedNode('http://example.org/graphs/people'),
});
```

---

## `QueryOptions`

Passed as the second argument to `store.query(query, options)`.

| Field                    | Type          | Required | Default | Description                                                                 |
| ------------------------ | ------------- | -------- | ------- | --------------------------------------------------------------------------- |
| `baseIri`                | `string`      | No       | —       | Base IRI for resolving relative IRIs in the query string.                   |
| `useDefaultGraphAsUnion` | `boolean`     | No       | `false` | Treat the default graph as a union of all named graphs.                     |
| `defaultGraph`           | `NamedNode[]` | No       | —       | Override the default graph(s) used when the query does not specify a graph. |
| `namedGraphs`            | `NamedNode[]` | No       | —       | Restrict the set of named graphs available to the query.                    |
| `resultsFormat`          | `string`      | No       | —       | Format string for result serialisation (e.g. `'json'`).                     |

---

## `UpdateOptions`

Passed as the second argument to `store.update(query, options)`.

| Field     | Type     | Required | Default | Description                                               |
| --------- | -------- | -------- | ------- | --------------------------------------------------------- |
| `baseIri` | `string` | No       | —       | Base IRI for resolving relative IRIs in the update query. |

---

## `CachedQueryStoreOptions`

Passed to `new CachedQueryStore(options)` or `createCachedStore(options)`.

| Field             | Type      | Required | Default | Description                                                                                                                 |
| ----------------- | --------- | -------- | ------- | --------------------------------------------------------------------------------------------------------------------------- |
| `cacheSize`       | `number`  | No       | `1000`  | Maximum number of query results to hold in the LRU cache. When the cache is full, the least-recently-used entry is evicted. |
| `cacheTtlMs`      | `number`  | No       | `60000` | Time-to-live in milliseconds for cache entries. Entries older than this are treated as cache misses.                        |
| `cacheResults`    | `boolean` | No       | `true`  | Whether to cache query results. Set to `false` to disable result caching while keeping pattern analysis.                    |
| `analyzePatterns` | `boolean` | No       | `true`  | Whether to analyse and cache `QueryPattern` objects for each unique query string.                                           |

**Example:**

```javascript
import { createCachedStore } from '@unrdf/oxigraph/query-cache';

const store = createCachedStore({
  cacheSize: 200,
  cacheTtlMs: 10_000, // 10 seconds
  cacheResults: true,
  analyzePatterns: false, // skip pattern analysis for maximum speed
});
```

---

## `QueryPattern`

Returned by `store.getQueryPattern(query)` and stored on `PreparedQuery.pattern`.

| Field            | Type                                             | Description                                                                 |
| ---------------- | ------------------------------------------------ | --------------------------------------------------------------------------- |
| `type`           | `'SELECT' \| 'ASK' \| 'CONSTRUCT' \| 'DESCRIBE'` | Query form detected by parsing the first keyword                            |
| `variables`      | `string[]`                                       | Variables projected in the SELECT clause (e.g. `['?name', '?age']`)         |
| `hasFilter`      | `boolean`                                        | `true` if the query contains at least one FILTER clause                     |
| `hasOptional`    | `boolean`                                        | `true` if the query contains at least one OPTIONAL block                    |
| `hasUnion`       | `boolean`                                        | `true` if the query contains at least one UNION                             |
| `hasAggregate`   | `boolean`                                        | `true` if the query uses COUNT, SUM, AVG, MIN, MAX, GROUP_CONCAT, or SAMPLE |
| `triplePatterns` | `number`                                         | Estimated number of triple patterns (rough heuristic)                       |
| `predicates`     | `string[]`                                       | Deduplicated list of IRI predicates appearing in angle brackets             |

---

## `StoreStats`

Returned by `CachedQueryStore.getStats()`.

```typescript
{
  cache: {
    size:     number,   // current number of cached entries
    maxSize:  number,   // configured maximum
    hits:     number,   // cache hit count since last reset
    misses:   number,   // cache miss count since last reset
    hitRate:  number,   // hits / (hits + misses), 0 when no queries
  },
  query: {
    total:              number,  // total query calls
    cached:             number,  // calls served from cache
    uncached:           number,  // calls that executed against the store
    totalTimeMs:        number,  // cumulative time for all queries
    cachedTimeMs:       number,  // cumulative time for cached queries
    uncachedTimeMs:     number,  // cumulative time for uncached queries
    avgCachedTimeMs:    number,  // cachedTimeMs / cached
    avgUncachedTimeMs:  number,  // uncachedTimeMs / uncached
  },
  patterns:        number,  // number of analysed QueryPattern entries
  mutationVersion: number,  // incremented on each add/delete/update/load
}
```

---

## Format Strings

Both MIME types and short aliases are accepted by `store.load()` and `store.dump()`.

| Format    | MIME type               | Short alias |
| --------- | ----------------------- | ----------- |
| Turtle    | `text/turtle`           | `ttl`       |
| TriG      | `application/trig`      | `trig`      |
| N-Triples | `application/n-triples` | `nt`        |
| N-Quads   | `application/n-quads`   | `nq`        |
| JSON-LD   | `application/ld+json`   | `jsonld`    |
| RDF/XML   | `application/rdf+xml`   | `rdf`       |

**Format capabilities:**

| Format    | Named graphs            | Blank node labels preserved | Human-readable |
| --------- | ----------------------- | --------------------------- | -------------- |
| Turtle    | No (default graph only) | Yes                         | Yes            |
| TriG      | Yes                     | Yes                         | Yes            |
| N-Triples | No                      | Yes                         | Verbose        |
| N-Quads   | Yes                     | Yes                         | Verbose        |
| JSON-LD   | Yes                     | Yes                         | Yes (JSON)     |
| RDF/XML   | No                      | Yes                         | Verbose        |

Use N-Quads or TriG when you need to preserve named graph assignments across a dump/load round-trip. Turtle and N-Triples drop the graph component.

---

## Zod Schemas (store-receipts)

Imported from `@unrdf/oxigraph/store-receipts`.

### `StoreConfigSchema`

```javascript
z.object({
  quads: z.array(z.any()).optional(),
  name: z.string().optional(),
  metadata: z.record(z.string(), z.any()).optional(),
});
```

### `QueryOptionsSchema`

```javascript
z.object({
  timeout: z.number().int().positive().optional(),
  limit: z.number().int().positive().optional(),
  offset: z.number().int().nonnegative().optional(),
});
```

### `QuadSchema`

```javascript
z.object({
  subject: z.any(),
  predicate: z.any(),
  object: z.any(),
  graph: z.any().optional(),
});
```
