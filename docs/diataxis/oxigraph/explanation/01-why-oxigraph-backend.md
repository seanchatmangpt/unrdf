# Why the Oxigraph Backend

This article explains why `@unrdf/oxigraph` exists alongside `@unrdf/core`, what the Rust-WASM compilation chain means for JavaScript runtimes, and when to reach for each engine.

## The Two Engines in UNRDF

UNRDF ships with two quad-store backends:

| Package           | Engine      | SPARQL evaluation       | Language              |
| ----------------- | ----------- | ----------------------- | --------------------- |
| `@unrdf/core`     | N3.js Store | Comunica (async stream) | JavaScript            |
| `@unrdf/oxigraph` | Oxigraph    | Built-in Rust engine    | Rust compiled to WASM |

Both expose a compatible quad-store interface (`add`, `delete`, `has`, `match`, `query`). The difference is where query evaluation happens.

## What WASM Compilation Means

Oxigraph is a complete SPARQL 1.1 engine written in Rust. The `oxigraph` npm package ships it compiled with `wasm-pack` to a single WASM binary. When your JavaScript process imports the package:

1. Node.js loads the `.wasm` binary into a `WebAssembly.Module`.
2. The module is instantiated once and kept in memory for the process lifetime.
3. Subsequent `new Store()` calls (and all operations) run synchronously inside that module — no JavaScript bridge overhead per operation.

The consequence is that all `OxigraphStore` methods are **synchronous**. There is no async SPARQL evaluation pipeline; the query enters the WASM module and the result comes back as a JavaScript value. This is fundamentally different from Comunica, which evaluates queries as async streams.

## Performance Characteristics

The WASM boundary adds a fixed per-call overhead (roughly 0.01–0.1ms) but removes the per-result cost of a stream-based engine. For queries that produce small-to-moderate result sets, Oxigraph is generally faster. For queries that produce very large result sets where back-pressure and lazy evaluation matter, stream-based engines can be more memory-efficient.

Empirical observations from the test suite (`packages/oxigraph/test/comparison.test.mjs`):

- **Store initialisation** — Oxigraph reinitialises in under 1ms (WASM already loaded).
- **Add throughput** — 5,000 triples in well under 100ms. Oxigraph's Rust-optimised indexing typically runs 2–5× faster than N3.js at scale.
- **ASK queries** — Extremely fast (sub-millisecond for small stores) because the short-circuit evaluation stops at the first match.
- **Simple SELECT** — Typically 1–10ms for a 200-triple store. Comunica carries 100–500ms cold-start overhead for the first query; Oxigraph has none.
- **Complex queries with aggregates** — Both engines are competitive at moderate dataset sizes. Comunica has a more mature optimiser for federated or deeply nested queries.

## Why Not Always Use Oxigraph

1. **Comunica's federation**: If you need queries that span multiple remote SPARQL endpoints (SERVICE clauses), Comunica handles this natively. Oxigraph does not.

2. **Streaming large result sets**: If a query returns tens of thousands of bindings and you want to process them one at a time without holding all results in memory, Comunica's stream model is better. Oxigraph always returns a materialised array.

3. **N3 rules and Datalog**: The `@unrdf/hooks` package supports N3-rule and Datalog conditions, which require the N3.js ecosystem. These are not available against an Oxigraph store.

4. **WASM size**: The Oxigraph WASM binary adds approximately 2–4 MB to your deployment. For edge functions or browser bundles where payload size is critical, this matters.

## Why the Interface Is Compatible

`OxigraphStore` is designed to be a drop-in replacement for the core in-memory store. It implements the same method names (`add`, `delete`, `has`, `match`, `getQuads`, `addQuad`, `removeQuad`, `query`, `update`, `load`, `dump`, `size`, `clear`) for exactly this reason.

This means you can switch backends by changing one import line and re-running tests. No application logic needs to change if it does not depend on async iterators.

## The `dataFactory` Compatibility Layer

RDF terms created by the Oxigraph data factory are native WASM objects. They are not the same JavaScript objects that N3.js creates. The `@unrdf/oxigraph` package exports its own `dataFactory` that creates Oxigraph-native terms. When migrating from N3.js:

- Use `dataFactory` from `@unrdf/oxigraph` when constructing terms to be stored in `OxigraphStore`.
- Do not mix N3.js terms and Oxigraph terms in the same store operation — the comparison semantics differ.

If you need to copy quads from an N3.js store into an Oxigraph store, reconstruct them using the Oxigraph data factory:

```javascript
import { createStore, dataFactory } from '@unrdf/oxigraph';

const oxiStore = createStore();
for (const q of n3Store.getQuads(null, null, null, null)) {
  oxiStore.addQuad(
    dataFactory.namedNode(q.subject.value),
    dataFactory.namedNode(q.predicate.value),
    q.object.termType === 'Literal'
      ? dataFactory.literal(
          q.object.value,
          q.object.language || dataFactory.namedNode(q.object.datatype.value)
        )
      : dataFactory.namedNode(q.object.value)
  );
}
```

## Summary

Use `@unrdf/oxigraph` when:

- You need synchronous SPARQL execution (no async/await at query time)
- Your queries are single-store and do not use SERVICE federation
- Result sets fit comfortably in memory
- You value the performance characteristics of a native-compiled engine

Stay with `@unrdf/core` + Comunica when:

- You need federated queries across multiple endpoints
- You need lazy/streaming evaluation of very large result sets
- You are integrating with the N3.js rule ecosystem

## Further Reading

- [Oxigraph project](https://oxigraph.org/)
- [SPARQL 1.1 specification](https://www.w3.org/TR/sparql11-overview/)
- [how-to/02-switch-from-memory-to-oxigraph.md](../how-to/02-switch-from-memory-to-oxigraph.md) — practical migration steps
