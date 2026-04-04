# @unrdf/oxigraph Documentation

Rust-based SPARQL engine for UNRDF — a high-performance, in-memory RDF store compiled to WebAssembly. Provides the same quad-store interface as the core in-memory backend with a native-speed SPARQL execution engine.

## Package Identity

```
@unrdf/oxigraph  v26.4.4
wraps: oxigraph npm package (^0.5.2)
runtime: Node.js >=18.0.0
```

## Documentation Map

This documentation follows the [Diataxis](https://diataxis.fr/) framework.

### Tutorials — Learning by doing

Step-by-step guides for developers new to `@unrdf/oxigraph`. Follow them end-to-end and you will have a working store with real data.

- [tutorials/README.md](tutorials/README.md) — Index
- [tutorials/01-persistent-rdf-store.md](tutorials/01-persistent-rdf-store.md) — Build your first Oxigraph-backed store, run SPARQL, and export

### How-To Guides — Solving specific problems

Goal-oriented recipes for practitioners who already know the basics.

- [how-to/README.md](how-to/README.md) — Index
- [how-to/01-configure-persistent-backend.md](how-to/01-configure-persistent-backend.md) — Set store options: cache size, TTL, pattern analysis
- [how-to/02-switch-from-memory-to-oxigraph.md](how-to/02-switch-from-memory-to-oxigraph.md) — Drop-in swap of an existing in-memory store
- [how-to/03-bulk-load-turtle-file.md](how-to/03-bulk-load-turtle-file.md) — Load Turtle files, named graphs, and multi-format data

### Reference — Technical specification

Accurate, complete descriptions of all APIs and configuration options.

- [reference/README.md](reference/README.md) — Index
- [reference/oxigraph-store-api.md](reference/oxigraph-store-api.md) — Full `OxigraphStore` API, `dataFactory`, SPARQL-star utilities
- [reference/configuration-options.md](reference/configuration-options.md) — `CachedQueryStore` options, `LoadOptions`, `DumpOptions`, format strings

### Explanation — Understanding concepts

Background articles that explain design decisions and tradeoffs.

- [explanation/README.md](explanation/README.md) — Index
- [explanation/01-why-oxigraph-backend.md](explanation/01-why-oxigraph-backend.md) — Why Rust-compiled WASM vs pure-JavaScript engines
- [explanation/02-persistence-and-durability.md](explanation/02-persistence-and-durability.md) — What "in-memory" means, session boundaries, and durability patterns

## Module Exports at a Glance

| Export path                      | What it provides                                   |
| -------------------------------- | -------------------------------------------------- |
| `@unrdf/oxigraph`                | `createStore`, `dataFactory`, `OxigraphStore`      |
| `@unrdf/oxigraph/store`          | `OxigraphStore` class directly                     |
| `@unrdf/oxigraph/query-cache`    | `CachedQueryStore`, `PreparedQuery`, `prepare`     |
| `@unrdf/oxigraph/sparql-star`    | `SPARQLStarQueryBuilder`, `queryByConfidence`, etc |
| `@unrdf/oxigraph/store-receipts` | Receipt-wrapped `createStore`, `query`, `addQuad`  |
| `@unrdf/oxigraph/types`          | JSDoc typedefs (`LoadOptions`, `DumpOptions`, etc) |
