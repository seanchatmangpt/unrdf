# Explanation: Storage Backends

`@unrdf/core` exposes two distinct store styles. Understanding why they exist prevents
confusion when reading code that uses one or the other.

## The two store styles

### 1. `UnrdfStore` (class-based, recommended for new code)

```javascript
import { createUnrdfStore } from '@unrdf/core';

const store = createUnrdfStore();
store.add(quad(...));
const rows = store.query(`SELECT * WHERE { ?s ?p ?o }`);
```

`UnrdfStore` wraps an `OxigraphStore` directly. It holds a persistent in-process Oxigraph
instance and calls `store.query()` synchronously. No data conversion happens on each query.

**When to use it:**

- New application code
- Reactive frameworks that require synchronous reads
- Code that needs `transaction()`, `bulkAdd()`, `bulkRemove()`, `load()`, or `dump()`

### 2. Functional API (backward-compatible, async-first)

```javascript
import { createStore, addQuad, getQuads, executeQuery } from '@unrdf/core';

const store = createStore();
addQuad(store, { subject: ..., predicate: ..., object: ..., graph: ... });
const rows = await executeQuery(store, `SELECT * WHERE { ?s ?p ?o }`);
```

The functional API was the original UNRDF store interface. It creates a raw Oxigraph store
(the object returned by `@unrdf/oxigraph`'s `createStore`) and operates on it via standalone
functions.

**When to use it:**

- Legacy code already using the functional pattern
- Quick scripts that prefer the functional style
- When you specifically need `iterateQuads` (a generator) rather than `match()`

### Why async wrappers call synchronous executors

The `executeQuery` / `executeSelect` / `executeAsk` / `executeConstruct` functions are `async`
functions that internally call `executeQuerySync`. They are _not_ async because the underlying
engine is asynchronous — Oxigraph's SPARQL execution is synchronous. They are async only to
preserve the function signatures expected by code written when the package used N3.js (which
had a callback-based query interface).

This means `await executeQuery(store, sparql)` and `store.query(sparql)` have effectively
identical performance. There is no I/O or microtask queueing involved; the promise resolves
in the same turn of the event loop.

## Why Oxigraph instead of N3.js

The earlier versions of `@unrdf/core` used the N3.js library as the in-memory store. N3.js
is a pure JavaScript implementation of the RDF/JS DatasetCore interface and is well-suited
for parsing Turtle/N3 formats. However, it does not execute SPARQL directly — queries
required converting the store to another format or using a separate SPARQL engine.

Oxigraph (via `@unrdf/oxigraph`, a WebAssembly build of the Rust Oxigraph library) provides:

1. **Integrated SPARQL execution** — no conversion step between storage and query.
2. **Synchronous query API** — the WebAssembly module exposes synchronous calls, enabling
   `computed()` patterns in reactive frameworks.
3. **SPARQL 1.1 compliance** — including named graphs, aggregates, property paths, and DESCRIBE.
4. **Load/dump serialization** — built-in support for multiple RDF serialization formats.

The trade-off is that Oxigraph is a WebAssembly module (~3 MB) rather than pure JavaScript.
For browser use, consider the `@unrdf/browser` package which provides a lighter-weight
configuration.

## The `_store` internal property

`UnrdfStore._store` holds the raw Oxigraph store instance. It is intentionally prefixed with
`_` to signal that it is an implementation detail. Code that accesses `store._store` directly
is bypassing the version tracking, schema validation, and transaction rollback logic.

## Version tracking

`UnrdfStore` maintains a `version` counter that is incremented on every mutation:
`add`, `delete`, `bulkAdd`, `bulkRemove`, `clear`, `update`, `load`. Read operations (`query`,
`match`, `has`, `size`) leave `version` unchanged.

This design supports reactive frameworks that track dependencies by comparing version numbers.
A `computed()` that reads `store.version` during its initial run will re-run whenever the
version changes — which happens exactly when the store content changes.

```javascript
// Vue 3 example (illustrative)
import { ref, computed } from 'vue';
import { createUnrdfStore } from '@unrdf/core';

const store = createUnrdfStore();
const version = ref(store.version);

const names = computed(() => {
  version.value; // subscribe to version changes
  return store
    .query(
      `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT ?name WHERE { ?s foaf:name ?name }
  `
    )
    .map(r => r.name.value);
});
```

## SHACL and the N3.js bridge

SHACL validation in `@unrdf/core` uses the `rdf-validate-shacl` library, which is built on
N3.js's `DatasetCore` interface. Because Oxigraph quads and N3.js quads are not the same
objects (different prototypes, different DataFactory), the `shacl-validator.mjs` layer
maintains a thin bridge: it creates an N3.js store, copies Oxigraph quads into it, and
passes that N3.js dataset to the SHACL validator.

This conversion is done per-`validateGraph` call and is O(n) in the number of data quads.
For large graphs (> 100K quads), consider pre-building a shared N3.js dataset rather than
re-converting on every validation.

## See also

- [RDF data model](./01-rdf-data-model.md) — what quads, terms, and graphs are.
- [Reference: Store API](../reference/store-api.md) — authoritative `UnrdfStore` signatures.
- [How-To: Bulk-add quads](../how-to/01-bulk-add-quads.md) — practical patterns.
