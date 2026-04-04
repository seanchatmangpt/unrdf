# Persistence and Durability

This article explains what it means for `@unrdf/oxigraph` to be an "in-memory" store, how long data lives, and what patterns practitioners use to achieve durability.

## What "In-Memory" Means

`OxigraphStore` holds all quad data in the WASM module's linear memory. No file is written to disk automatically. When the Node.js process exits, all data is gone.

This is the same model as:

- A Redis instance with no persistence configured
- A SQLite `:memory:` database
- An N3.js `Store`

The term "in-memory" refers to storage location, not performance — on-disk stores can also be fast, and in-memory stores can be slow.

## Session Boundaries

Data in an `OxigraphStore` lives for one of three lifespans depending on how the application is structured:

| Lifespan    | How it happens                                       | Example                                                          |
| ----------- | ---------------------------------------------------- | ---------------------------------------------------------------- |
| **Request** | Store created and discarded per HTTP request         | API endpoint that loads a Turtle file, queries, and responds     |
| **Process** | Store created at startup and reused                  | Long-running server that accumulates triples during its lifetime |
| **Test**    | Store created in `beforeEach`, cleared between tests | Vitest test suite                                                |

None of these scopes provide durability across process restarts without explicit serialisation.

## How Durability Is Achieved in Practice

Because the store does not persist itself, durability requires one of three patterns:

### Pattern 1 — Load from source files on startup

```javascript
import { readFileSync } from 'node:fs';
import { createStore } from '@unrdf/oxigraph';

const store = createStore();
store.load(readFileSync('knowledge-base.ttl', 'utf8'), { format: 'text/turtle' });
// The store is now populated for the process lifetime.
```

The source files are the durable record. The in-memory store is a working copy. This is appropriate when the graph is read-mostly and can be rebuilt quickly from the source.

### Pattern 2 — Snapshot on shutdown

```javascript
import { writeFileSync } from 'node:fs';
import { createStore } from '@unrdf/oxigraph';

const store = createStore();
// ... application runs ...

process.on('SIGTERM', () => {
  writeFileSync('snapshot.nq', store.dump({ format: 'application/n-quads' }));
  process.exit(0);
});
```

N-Quads is the preferred format for snapshots because it preserves named graph assignments and is line-oriented (one quad per line), which is friendly to diff tools and append-only logs.

### Pattern 3 — Write-ahead log (WAL pattern)

For stores that accept writes during the process lifetime, append each mutation to a log file. On restart, replay the log to restore state.

```javascript
import { appendFileSync } from 'node:fs';
import { createStore, dataFactory } from '@unrdf/oxigraph';

const store = createStore();

function addAndLog(quad) {
  store.add(quad);
  // Serialize this quad to N-Triples (one triple per line) and append to log
  const { subject: s, predicate: p, object: o } = quad;
  const line = `<${s.value}> <${p.value}> "${o.value}" .\n`;
  appendFileSync('mutations.nt', line);
}
```

On startup, replay `mutations.nt` through `store.load()`. This pattern works well for stores with moderate write rates where individual mutations need to be auditable.

## Transactional Guarantees in `store.load()`

`store.load()` is atomic by default. If the parse fails midway through a large document, the store is unchanged — no partially loaded data is committed. This is the `noTransaction: false` default.

Setting `noTransaction: true` disables this guarantee and can improve throughput for large trusted datasets, at the cost of leaving the store in a partially loaded state if an error occurs.

## Named Graphs and Multi-Store Patterns

Named graphs are stored inside the same WASM memory as the default graph. They are not separate files or separate stores. The only durability boundary is the process.

If you want to give different named graphs different durability properties (e.g., one graph rebuilt from an external API on startup, another populated from user writes), use separate `OxigraphStore` instances and serialise them independently.

## What `CachedQueryStore` Does Not Persist

`CachedQueryStore` adds a query-result cache on top of `OxigraphStore`. The cache is also in-memory and is cleared when the process exits. It is not a durable structure — it is a performance optimisation. Do not treat its contents as a record of past queries.

## Comparison With Persistent SPARQL Endpoints

Tools like Apache Jena TDB, Blazegraph, and the Oxigraph server (separate from the npm package) write to disk. They survive process restarts natively. The `@unrdf/oxigraph` npm package does **not** use disk storage — it is the WASM-compiled engine only, not the Oxigraph server.

If you need a durable, disk-backed SPARQL endpoint, use the [Oxigraph server](https://oxigraph.org/oxigraph/1.0/) as a separate process and query it over HTTP (using Comunica with a SPARQL service source). The `@unrdf/oxigraph` npm package is designed for in-process, in-memory use.

## Summary

| Question                                    | Answer                                                         |
| ------------------------------------------- | -------------------------------------------------------------- |
| Does the store write to disk automatically? | No                                                             |
| What happens on process exit?               | All data is lost                                               |
| How do I survive a restart?                 | Load from a source file, or snapshot on shutdown               |
| Is `store.load()` atomic?                   | Yes, unless `noTransaction: true`                              |
| Does `CachedQueryStore` add persistence?    | No — the cache is also in-memory                               |
| Is this the same as the Oxigraph server?    | No — the npm package is engine-only, not a persistent database |

## See Also

- [how-to/03-bulk-load-turtle-file.md](../how-to/03-bulk-load-turtle-file.md) — loading from disk
- [reference/configuration-options.md — LoadOptions](../reference/configuration-options.md#loadoptions) — `noTransaction` and `unchecked` flags
- [explanation/01-why-oxigraph-backend.md](01-why-oxigraph-backend.md) — WASM architecture and performance tradeoffs
