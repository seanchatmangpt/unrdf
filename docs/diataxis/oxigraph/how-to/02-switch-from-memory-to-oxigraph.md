# How to Switch from an In-Memory Store to Oxigraph

This guide shows how to replace an existing in-memory store (such as an N3.js `Store` or an `@unrdf/core` quad store) with `OxigraphStore` without changing the rest of your application.

## When to Do This

Switch to `@unrdf/oxigraph` when:

- SPARQL queries are taking more than a few milliseconds on a dataset that fits in memory
- You need a SPARQL 1.1 compliant engine with full SELECT/CONSTRUCT/DESCRIBE/ASK support
- You want near-native query performance from a WASM-compiled Rust engine

## Prerequisites

- Existing code that uses an in-memory RDF store
- `@unrdf/oxigraph` installed: `pnpm add @unrdf/oxigraph`

## Pattern A — Replace a Store Created Inline

Before (using N3.js `Store` directly):

```javascript
import { Store, DataFactory } from 'n3';

const store = new Store();
const { namedNode, literal, quad } = DataFactory;

store.addQuad(
  namedNode('http://example.org/alice'),
  namedNode('http://xmlns.com/foaf/0.1/name'),
  literal('Alice Smith')
);

const quads = store.getQuads(namedNode('http://example.org/alice'), null, null, null);
```

After (using `OxigraphStore`):

```javascript
import { createStore, dataFactory } from '@unrdf/oxigraph';

const store = createStore();
const { namedNode, literal, quad } = dataFactory;

store.addQuad(
  namedNode('http://example.org/alice'),
  namedNode('http://xmlns.com/foaf/0.1/name'),
  literal('Alice Smith')
);

const quads = store.getQuads(namedNode('http://example.org/alice'), null, null, null);
```

The method names `addQuad` and `getQuads` (alias for `match`) are identical. The only change is the import.

## Pattern B — Replace a Factory Function

If your codebase uses a factory function to create stores, update the factory:

Before:

```javascript
// store-factory.mjs
import { Store } from 'n3';

export function createGraphStore(initialQuads = []) {
  const store = new Store(initialQuads);
  return store;
}
```

After:

```javascript
// store-factory.mjs
import { createStore } from '@unrdf/oxigraph';

export function createGraphStore(initialQuads = []) {
  return createStore(initialQuads);
}
```

All callers of `createGraphStore` continue to work unchanged because `OxigraphStore` supports the same `add`, `delete`, `has`, `match`, and `getQuads` interface.

## Pattern C — Migrating SPARQL Queries

If your existing code uses Comunica or another async SPARQL engine, note that `OxigraphStore.query()` is **synchronous**.

Before (Comunica, async):

```javascript
import { QueryEngine } from '@comunica/query-sparql';

const engine = new QueryEngine();
const bindingsStream = await engine.queryBindings(
  'SELECT ?name WHERE { ?s <http://xmlns.com/foaf/0.1/name> ?name }',
  { sources: [store] }
);
const bindings = await bindingsStream.toArray();
```

After (Oxigraph, synchronous):

```javascript
import { createStore } from '@unrdf/oxigraph';

const store = createStore();
// ... add data to store ...

const bindings = store.query('SELECT ?name WHERE { ?s <http://xmlns.com/foaf/0.1/name> ?name }');
// bindings is already an array — no await needed
```

If your callers expect `async`/`Promise`, wrap the call:

```javascript
export async function sparqlSelect(store, query) {
  return store.query(query); // synchronous under the hood
}
```

## Pattern D — Carry Over Existing Quads

If you have quads in an existing store and want to move them into Oxigraph without re-parsing:

```javascript
import { createStore } from '@unrdf/oxigraph';

// existingQuads is an array of RDF/JS-compatible quad objects
const existingQuads = legacyStore.getQuads(null, null, null, null);

// Pass them directly to the constructor
const oxiStore = createStore(existingQuads);

console.log(`Migrated ${oxiStore.size} quads`);
```

The constructor accepts any iterable of objects with `subject`, `predicate`, `object`, and `graph` properties.

## Verify the Switch

After migrating, confirm your tests still pass:

```bash
pnpm exec vitest run
```

And spot-check store size and a representative SPARQL query to confirm data integrity.

## Method Compatibility Table

| Operation       | N3.js                     | OxigraphStore                                       | Notes             |
| --------------- | ------------------------- | --------------------------------------------------- | ----------------- |
| Add one quad    | `store.addQuad(q)`        | `store.addQuad(q)` or `store.add(q)`                | Both work         |
| Add: 4-arg form | `store.addQuad(s,p,o,g)`  | `store.addQuad(s,p,o,g)`                            | Both work         |
| Remove quad     | `store.removeQuad(q)`     | `store.removeQuad(q)` or `store.delete(q)`          | Both work         |
| Pattern match   | `store.getQuads(s,p,o,g)` | `store.getQuads(s,p,o,g)` or `store.match(s,p,o,g)` | Both work         |
| Existence check | `store.has(q)`            | `store.has(q)`                                      | Identical         |
| Count quads     | `store.size`              | `store.size`                                        | Identical         |
| Clear store     | `store.removeMatches()`   | `store.clear()`                                     | Different name    |
| SPARQL query    | Comunica (async)          | `store.query(sparql)` (sync)                        | Interface differs |

## See Also

- [reference/oxigraph-store-api.md](../reference/oxigraph-store-api.md) — full method reference
- [explanation/01-why-oxigraph-backend.md](../explanation/01-why-oxigraph-backend.md) — performance rationale
