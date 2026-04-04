# Reference: Store API

All functions and classes are exported from `@unrdf/core`.

---

## `UnrdfStore` (class)

Exported as `UnrdfStore`. Create instances with `createUnrdfStore()`.

### Constructor

```typescript
new UnrdfStore(quads?: Quad[], options?: StoreOptions)
```

| Parameter | Type           | Default | Description                     |
| --------- | -------------- | ------- | ------------------------------- |
| `quads`   | `Quad[]`       | `[]`    | Initial quads to populate store |
| `options` | `StoreOptions` | `{}`    | Store configuration             |

**StoreOptions**

| Field     | Type     | Description                                     |
| --------- | -------- | ----------------------------------------------- |
| `baseIri` | `string` | Base IRI for relative IRI resolution (optional) |

---

### `store.add(quad)`

Add a single quad to the store.

**Parameters**

| Param  | Type   | Description |
| ------ | ------ | ----------- |
| `quad` | `Quad` | Quad to add |

**Returns** `void`

**Throws** `TypeError: add: quad is required` if `quad` is falsy.

**Side effect** Increments `store.version` by 1.

---

### `store.delete(quad)`

Remove a single quad from the store.

**Parameters**

| Param  | Type   | Description    |
| ------ | ------ | -------------- |
| `quad` | `Quad` | Quad to remove |

**Returns** `void`

**Throws** `TypeError: delete: quad is required` if `quad` is falsy.

**Side effect** Increments `store.version` by 1.

---

### `store.has(quad)`

Check whether a quad is present in the store.

**Parameters**

| Param  | Type   | Description   |
| ------ | ------ | ------------- |
| `quad` | `Quad` | Quad to check |

**Returns** `boolean`

---

### `store.match(subject?, predicate?, object?, graph?)`

Return all quads matching the given pattern. Pass `null` or `undefined` for any component to
match any value.

**Parameters**

| Param       | Type           | Description      |
| ----------- | -------------- | ---------------- |
| `subject`   | `Term \| null` | Subject filter   |
| `predicate` | `Term \| null` | Predicate filter |
| `object`    | `Term \| null` | Object filter    |
| `graph`     | `Term \| null` | Graph filter     |

**Returns** `Quad[]`

---

### `store.size()`

Return the number of quads in the store.

**Returns** `number`

---

### `store.clear()`

Remove all quads from the store.

**Returns** `void`

**Side effect** Increments `store.version` by 1.

---

### `store.bulkAdd(quads)`

Insert an array of quads in a single operation. Version is incremented once.

**Parameters**

| Param   | Type     | Description     |
| ------- | -------- | --------------- |
| `quads` | `Quad[]` | Quads to insert |

**Returns** `void`

**Throws** `TypeError: bulkAdd: quads must be an array` if `quads` is not an array.

**Side effect** Increments `store.version` by 1.

---

### `store.bulkRemove(quads)`

Remove an array of quads in a single operation. Silently skips quads not in the store.
Version is incremented once.

**Parameters**

| Param   | Type     | Description     |
| ------- | -------- | --------------- |
| `quads` | `Quad[]` | Quads to remove |

**Returns** `void`

**Throws** `TypeError: bulkRemove: quads must be an array` if `quads` is not an array.

**Side effect** Increments `store.version` by 1.

---

### `store.transaction(fn)`

Execute operations atomically. If `fn` throws, the store is rolled back to its pre-transaction
state by clearing and re-inserting a snapshot of all quads.

**Parameters**

| Param | Type                          | Description                                     |
| ----- | ----------------------------- | ----------------------------------------------- |
| `fn`  | `(store: UnrdfStore) => void` | Transaction function; receives the store itself |

**Returns** `void`

**Throws**

- `TypeError: transaction: fn must be a function` if `fn` is not a function.
- `Error: Transaction failed: <original message>` if `fn` throws. Store is rolled back.

---

### `store.query(sparql, options?)`

Execute a SPARQL query synchronously. Auto-detects query type (SELECT, ASK, CONSTRUCT,
DESCRIBE) from the query string.

**Parameters**

| Param     | Type           | Description           |
| --------- | -------------- | --------------------- |
| `sparql`  | `string`       | SPARQL query string   |
| `options` | `QueryOptions` | Optional query config |

**QueryOptions**

| Field           | Type                              | Description                              |
| --------------- | --------------------------------- | ---------------------------------------- |
| `baseIri`       | `string`                          | Base IRI for relative IRI resolution     |
| `defaultGraph`  | `string`                          | Default graph IRI                        |
| `namedGraphs`   | `string[]`                        | Named graph IRIs                         |
| `resultsFormat` | `'json' \| 'bindings' \| 'quads'` | Override result format for SELECT        |
| `timeout`       | `number`                          | Query timeout in milliseconds (positive) |

**Returns**

| Query type                       | Return type         |
| -------------------------------- | ------------------- |
| SELECT (default)                 | `BindingRow[]`      |
| SELECT + `resultsFormat: 'json'` | `SparqlJsonResults` |
| ASK                              | `boolean`           |
| CONSTRUCT                        | `Quad[]`            |
| DESCRIBE                         | `Quad[]`            |

**`BindingRow`** — a plain object keyed by variable name. Each value is a term object:

```typescript
{
  type: string;     // 'NamedNode' | 'Literal' | 'BlankNode'
  value: string;    // the IRI or literal string
  language?: string;
  datatype?: string;
}
```

**`SparqlJsonResults`** — W3C SPARQL 1.1 JSON Results format:

```typescript
{
  head: { vars: string[] };
  results: { bindings: BindingRow[] };
}
```

**Throws**

- `TypeError: query: sparql must be a string` if `sparql` is not a string.
- `Error` if query execution fails.

---

### `store.queryAsync(sparql, options?)`

Async wrapper around `store.query()`. Returns `Promise<...>` with the same result types.

---

### `store.update(sparql, options?)`

Execute a SPARQL UPDATE query.

**Parameters**

| Param     | Type                   | Description          |
| --------- | ---------------------- | -------------------- |
| `sparql`  | `string`               | SPARQL UPDATE string |
| `options` | `{ baseIri?: string }` | Optional config      |

**Returns** `void`

**Throws**

- `TypeError: update: sparql must be a string` if `sparql` is not a string.
- `Error` if the update fails.

**Side effect** Increments `store.version` by 1.

---

### `store.load(data, options)`

Load serialized RDF data into the store.

**Parameters**

| Param            | Type     | Description             |
| ---------------- | -------- | ----------------------- |
| `data`           | `string` | Serialized RDF content  |
| `options.format` | `string` | MIME type of the format |

Common format strings: `'text/turtle'`, `'application/n-triples'`,
`'application/n-quads'`, `'application/trig'`.

**Returns** `void`

**Throws** `Error` if the data is malformed.

**Side effect** Increments `store.version` by 1.

---

### `store.dump(options)`

Serialize the entire store to a string.

**Parameters**

| Param            | Type     | Description               |
| ---------------- | -------- | ------------------------- |
| `options.format` | `string` | Target MIME type / format |

**Returns** `string`

**Throws** `Error` if format is unsupported or serialization fails.

---

### `store.version` (getter)

A monotonically incrementing integer. Incremented by `add`, `delete`, `bulkAdd`, `bulkRemove`,
`clear`, `update`, `load`. Read operations (`query`, `match`, `has`, `size`) do not change it.

**Returns** `number`

---

## `createUnrdfStore(quads?, options?)`

Factory function. Equivalent to `new UnrdfStore(quads, options)`.

```typescript
function createUnrdfStore(quads?: Quad[], options?: StoreOptions): UnrdfStore;
```

---

## Functional API (legacy)

These functions use the Oxigraph store returned by the functional `createStore()`, not
`UnrdfStore`. They exist for backward compatibility with code written before `UnrdfStore`
was introduced.

### `createStore()`

```typescript
function createStore(): OxigraphStore;
```

Returns a raw Oxigraph store. Does not have `.query()`, `.bulkAdd()`, etc. Use
`createUnrdfStore()` for new code.

### `addQuad(store, quadData)`

```typescript
function addQuad(store: OxigraphStore, quadData: QuadLike): void;
```

Validates `quadData` against `QuadSchema` (Zod) before adding.

**Throws** `TypeError` if store or quadData is missing. `ZodError` if quadData structure is
invalid.

### `removeQuad(store, quadData)`

```typescript
function removeQuad(store: OxigraphStore, quadData: QuadLike): void;
```

### `getQuads(store, subject?, predicate?, object?, graph?)`

```typescript
function getQuads(
  store: OxigraphStore,
  subject?: Term | null,
  predicate?: Term | null,
  object?: Term | null,
  graph?: Term | null
): Quad[];
```

Returns all matching quads as an array.

### `iterateQuads(store)`

```typescript
function* iterateQuads(store: OxigraphStore): IterableIterator<Quad>
```

Generator; avoids materialising the full array.

### `countQuads(store)`

```typescript
function countQuads(store: OxigraphStore): number;
```

Returns `store.size`.

---

## Canonicalization utilities

### `canonicalize(store, options?)`

```typescript
async function canonicalize(
  store: OxigraphStore,
  options?: {
    algorithm?: string; // default 'URDNA2015'
    produceGeneralizedRdf?: boolean; // default false
    timeoutMs?: number; // default 30000
  }
): Promise<string>;
```

Returns the URDNA2015 canonical N-Quads string. Returns `''` for an empty store.

**Throws** `Error: Canonicalization failed: …` on parse or execution error, or timeout.

### `toNTriples(quads)`

```typescript
async function toNTriples(quads: Quad[]): Promise<string>;
```

Converts an array of quads to N-Triples format (graph component stripped).

### `sortQuads(quads)`

```typescript
function sortQuads(quads: Quad[]): Quad[];
```

Returns a new sorted array ordered by subject, predicate, object, then graph.

### `isIsomorphic(store1, store2, options?)`

```typescript
async function isIsomorphic(
  store1: OxigraphStore,
  store2: OxigraphStore,
  options?: CanonicalizationOptions
): Promise<boolean>;
```

---

## Validation utilities

### `validateQuad(quad)`

```typescript
function validateQuad(quad: unknown): boolean;
```

Returns `true` if quad matches `QuadSchema`, `false` otherwise. Never throws.

**QuadSchema** requires `subject.value`, `predicate.value`, `object.value` to be strings.
`graph.value` is optional.

### `validateStore(store)`

```typescript
function validateStore(store: unknown): boolean;
```

Returns `true` if store has a `getQuads` function (i.e. implements the functional store
interface). Returns `false` for null, plain objects, or `UnrdfStore` instances (which use
`match` not `getQuads`).

---

## DataFactory functions

Re-exported from Oxigraph's `dataFactory`. All are synchronous.

| Function           | Signature                                                    | Returns        |
| ------------------ | ------------------------------------------------------------ | -------------- |
| `namedNode(iri)`   | `(iri: string) => NamedNode`                                 | `NamedNode`    |
| `literal(v, dt?)`  | `(value: string, datatype?: NamedNode \| string) => Literal` | `Literal`      |
| `literal(v, lang)` | `(value: string, language: string) => Literal`               | `Literal`      |
| `blankNode(id?)`   | `(id?: string) => BlankNode`                                 | `BlankNode`    |
| `variable(name)`   | `(name: string) => Variable`                                 | `Variable`     |
| `defaultGraph()`   | `() => DefaultGraph`                                         | `DefaultGraph` |
| `quad(s,p,o,g?)`   | `(s, p, o, g?) => Quad`                                      | `Quad`         |

---

## Namespace constants

Each namespace export is a proxy object created by `@rdfjs/namespace`. Access any property to
get the corresponding `NamedNode`.

```javascript
RDF.type.value; // 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'
FOAF.Person.value; // 'http://xmlns.com/foaf/0.1/Person'
XSD.integer.value; // 'http://www.w3.org/2001/XMLSchema#integer'
```

| Constant          | Base IRI                                      |
| ----------------- | --------------------------------------------- |
| `RDF`             | `http://www.w3.org/1999/02/22-rdf-syntax-ns#` |
| `RDFS`            | `http://www.w3.org/2000/01/rdf-schema#`       |
| `OWL`             | `http://www.w3.org/2002/07/owl#`              |
| `XSD`             | `http://www.w3.org/2001/XMLSchema#`           |
| `FOAF`            | `http://xmlns.com/foaf/0.1/`                  |
| `DCTERMS`         | `http://purl.org/dc/terms/`                   |
| `SKOS`            | `http://www.w3.org/2004/02/skos/core#`        |
| `COMMON_PREFIXES` | plain string map of the above                 |

---

## Term utility functions

### `createTerms(data)`

```typescript
function createTerms(data: Record<string, unknown>): Record<string, Term>;
```

Converts plain JS values to RDF terms based on type detection:

| JS value         | RDF term                   |
| ---------------- | -------------------------- |
| `'http://...'`   | `NamedNode`                |
| `'urn:...'`      | `NamedNode`                |
| `'_:...'`        | `BlankNode`                |
| `'?...'`         | `Variable`                 |
| other `string`   | `Literal` (plain string)   |
| `number` (int)   | `Literal` (`xsd:integer`)  |
| `number` (float) | `Literal` (`xsd:double`)   |
| `boolean`        | `Literal` (`xsd:boolean`)  |
| `Date`           | `Literal` (`xsd:dateTime`) |

### `createLiteral(value, datatype?, language?)`

```typescript
function createLiteral(
  value: string,
  datatype?: NamedNode | string | null,
  language?: string | null
): Literal;
```

### `createNamedNode(iri)` / `createBlankNode(id?)` / `createVariable(name)` / `createQuad(s,p,o,g?)`

Thin wrappers around the DataFactory functions. `createQuad` defaults `graph` to the default
graph when omitted.
