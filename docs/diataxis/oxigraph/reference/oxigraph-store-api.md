# OxigraphStore API Reference

Complete specification of all classes, functions, and utilities exported by `@unrdf/oxigraph`.

---

## `createStore(quads?)`

Factory function. The preferred way to create a store.

**Import:** `import { createStore } from '@unrdf/oxigraph'`

**Signature:**

```typescript
function createStore(quads?: Quad[]): OxigraphStore;
```

**Parameters:**

| Parameter | Type     | Required | Description                                                                               |
| --------- | -------- | -------- | ----------------------------------------------------------------------------------------- |
| `quads`   | `Quad[]` | No       | Initial quads to populate the store. Accepts any array of RDF/JS-compatible quad objects. |

**Returns:** `OxigraphStore`

**Throws:** Never. The constructor itself does not validate quads; invalid quads will cause errors when queried.

**Example:**

```javascript
import { createStore, dataFactory } from '@unrdf/oxigraph';

const empty = createStore();

const preloaded = createStore([
  dataFactory.quad(
    dataFactory.namedNode('http://example.org/alice'),
    dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'),
    dataFactory.literal('Alice')
  ),
]);
```

---

## `OxigraphStore`

Class that wraps the `oxigraph.Store` WASM object. All methods are synchronous.

**Import:** `import { OxigraphStore } from '@unrdf/oxigraph'`

### Constructor

```typescript
new OxigraphStore(quads?: Quad[])
```

Prefer `createStore(quads)` over calling `new OxigraphStore(quads)` directly.

---

### `add(quad)`

Add a quad to the store.

```typescript
add(quad: Quad): void
```

**Throws:** `Error('Quad is required')` when `quad` is `null` or `undefined`.

---

### `addQuad(quadOrSubject, predicate?, object?, graph?)`

Compatibility alias. Supports two call signatures:

```typescript
// Form 1: pass a complete quad object
addQuad(quad: Quad): void

// Form 2: pass four separate terms
addQuad(subject: Term, predicate: Term, object: Term, graph?: Term): void
```

When `predicate` is provided, the method constructs a quad internally using `oxigraph.quad(subject, predicate, object, graph || oxigraph.defaultGraph())` and delegates to `add()`.

---

### `delete(quad)`

Remove a quad from the store. No-op if the quad does not exist.

```typescript
delete(quad: Quad): void
```

**Throws:** `Error('Quad is required')` when `quad` is `null` or `undefined`.

---

### `removeQuad(quadOrSubject, predicate?, object?, graph?)`

Compatibility alias for `delete`. Supports the same two call signatures as `addQuad`.

---

### `has(quad)`

Return `true` if the quad exists in the store.

```typescript
has(quad: Quad): boolean
```

**Throws:** `Error('Quad is required')` when `quad` is `null` or `undefined`.

---

### `match(subject?, predicate?, object?, graph?)`

Return all quads matching the given pattern. Pass `null` or `undefined` for any component to match any value.

```typescript
match(
  subject?:   Subject | null,
  predicate?: Predicate | null,
  object?:    RDFObject | null,
  graph?:     Graph | null
): Quad[]
```

**Returns:** `Quad[]` — always an array, never `null`. Empty array when no quads match.

**Throws:** `Error('Match operation failed: ...')` if the underlying WASM call rejects.

**Examples:**

```javascript
// All quads
store.match();

// All quads about Alice
store.match(dataFactory.namedNode('http://example.org/alice'), null, null);

// All names (any subject)
store.match(null, dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'), null);
```

---

### `getQuads(subject?, predicate?, object?, graph?)`

Alias for `match()`. Identical signature and return type.

---

### `query(sparqlQuery, options?)`

Execute a SPARQL 1.1 query. The query type determines the return value.

```typescript
query(query: string, options?: QueryOptions): Binding[] | boolean | Quad[]
```

**Parameters:**

| Parameter | Type           | Required | Description                                               |
| --------- | -------------- | -------- | --------------------------------------------------------- |
| `query`   | `string`       | Yes      | SPARQL 1.1 query string                                   |
| `options` | `QueryOptions` | No       | See [QueryOptions](configuration-options.md#queryoptions) |

**Return type by query form:**

| Query form  | Return type | Notes                                                       |
| ----------- | ----------- | ----------------------------------------------------------- |
| `SELECT`    | `Binding[]` | Each `Binding` is a `Map`-like: use `.get('varName').value` |
| `ASK`       | `boolean`   | `true` if at least one match exists                         |
| `CONSTRUCT` | `Quad[]`    | Array of constructed quads                                  |
| `DESCRIBE`  | `Quad[]`    | Array of quads describing the resource                      |

**Throws:**

- `Error('Query must be a non-empty string')` — `query` is empty or not a string
- `Error('Query execution failed: ...')` — SPARQL parse or execution error

**Examples:**

```javascript
// SELECT
const bindings = store.query('SELECT ?name WHERE { ?s <http://xmlns.com/foaf/0.1/name> ?name }');
bindings.forEach(b => console.log(b.get('name').value));

// ASK
const exists = store.query('ASK { ?s ?p ?o }'); // boolean

// CONSTRUCT
const quads = store.query('CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }'); // Quad[]
```

---

### `update(sparqlUpdateQuery, options?)`

Execute a SPARQL 1.1 Update operation (INSERT DATA, DELETE WHERE, etc.).

```typescript
update(query: string, options?: UpdateOptions): void
```

**Throws:**

- `Error('Query must be a non-empty string')` — `query` is empty or not a string
- `Error('Update execution failed: ...')` — SPARQL Update parse or execution error

---

### `load(data, options)`

Parse serialised RDF and insert all triples into the store. Synchronous.

```typescript
load(data: string, options: LoadOptions): void
```

**Throws:**

- `Error('Data must be a non-empty string')` — `data` is empty or not a string
- `Error('Format option is required')` — `options.format` is missing
- `Error('Load operation failed: ...')` — parse error or invalid format string

See [LoadOptions](configuration-options.md#loadoptions) for the full options reference.

---

### `dump(options)`

Serialise the store (or one named graph) to a string.

```typescript
dump(options: DumpOptions): string
```

**Throws:**

- `Error('Format option is required')` — `options.format` is missing
- `Error('Dump operation failed: ...')` — invalid format string

See [DumpOptions](configuration-options.md#dumpoptions).

---

### `size` (getter)

Return the total number of quads in the store.

```typescript
get size(): number
```

Implemented as `this.match().length`. Not O(1) — avoid calling in tight loops on large stores.

---

### `clear()`

Remove all quads from the store. Iterates `match()` and calls `delete()` on each.

```typescript
clear(): void
```

---

### `OxigraphStore.getDataFactory()` (static)

Return a dataFactory object. Prefer the named export `dataFactory` from the main entry point.

```typescript
static getDataFactory(): DataFactory
```

---

## `dataFactory`

Named export from `@unrdf/oxigraph`. An object containing RDF term factory functions.

**Import:** `import { dataFactory } from '@unrdf/oxigraph'`

### `dataFactory.namedNode(iri)`

```typescript
namedNode(iri: string): NamedNode
```

Create an IRI term. `iri` must be a valid absolute IRI string.

### `dataFactory.blankNode(id?)`

```typescript
blankNode(id?: string): BlankNode
```

Create a blank node. When `id` is omitted, a unique identifier is generated.

### `dataFactory.literal(value, languageOrDatatype?)`

```typescript
literal(value: string, languageOrDatatype?: string | NamedNode): Literal
```

Create a literal value.

| `languageOrDatatype`             | Effect                                        |
| -------------------------------- | --------------------------------------------- |
| Omitted                          | Plain string literal (datatype: `xsd:string`) |
| String (e.g. `'en'`)             | Language-tagged literal                       |
| `NamedNode` (e.g. `xsd:integer`) | Typed literal                                 |

**Example:**

```javascript
dataFactory.literal('Alice'); // plain
dataFactory.literal('Bonjour', 'fr'); // language-tagged
dataFactory.literal('30', dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#integer'));
```

### `dataFactory.defaultGraph()`

```typescript
defaultGraph(): DefaultGraph
```

Return the default graph term. Use as the fourth argument to `quad()` to place a triple in the default graph.

### `dataFactory.quad(subject, predicate, object, graph?)`

```typescript
quad(
  subject:   Subject,
  predicate: Predicate,
  object:    RDFObject,
  graph?:    Graph
): Quad
```

`graph` defaults to `defaultGraph()` when omitted.

### `dataFactory.triple(subject, predicate, object)`

```typescript
triple(subject: Subject, predicate: Predicate, object: RDFObject): Quad
```

Convenience alias for `quad(subject, predicate, object, defaultGraph())`.

---

## `CachedQueryStore`

Extends `OxigraphStore` with an LRU result cache and query-pattern analysis.

**Import:** `import { CachedQueryStore } from '@unrdf/oxigraph/query-cache'`

### Constructor

```typescript
new CachedQueryStore(options?: CachedQueryStoreOptions)
```

See [CachedQueryStoreOptions](configuration-options.md#cachedquerystoreoptions).

### Additional Methods

#### `ask(query)`

```typescript
ask(query: string): boolean
```

Thin wrapper around `query()` for ASK queries. Included for readability.

#### `executePrepared(normalizedQuery, bindings?)`

```typescript
executePrepared(normalizedQuery: string, bindings?: Record<string, string>): any
```

Execute a pre-normalised query with variable substitution. Bindings map variable names (without `?`) to IRI strings.

#### `clearQueryCache()`

```typescript
clearQueryCache(): void
```

Clear all cached results and pattern analyses.

#### `getQueryPattern(query)`

```typescript
getQueryPattern(query: string): QueryPattern
```

Return (or compute and cache) the `QueryPattern` for the given query string. See [QueryPattern](configuration-options.md#querypattern).

#### `getStats()`

```typescript
getStats(): StoreStats
```

Return current cache and query statistics. See [StoreStats](configuration-options.md#storestats).

#### `resetStats()`

```typescript
resetStats(): void
```

Reset all counters in `queryStats` and zero out cache hit/miss counters. Cache entries are not evicted.

### Overridden Methods That Invalidate the Cache

The following methods call `super` and then increment `mutationVersion` (or clear the cache):

| Method                | Cache effect                                        |
| --------------------- | --------------------------------------------------- |
| `add(quad)`           | Increments `mutationVersion`                        |
| `delete(quad)`        | Increments `mutationVersion`                        |
| `update(query)`       | Clears cache entirely, increments `mutationVersion` |
| `load(data, options)` | Clears cache entirely, increments `mutationVersion` |

---

## `createCachedStore(options?)`

Factory function for `CachedQueryStore`.

**Import:** `import { createCachedStore } from '@unrdf/oxigraph/query-cache'`

```typescript
function createCachedStore(options?: CachedQueryStoreOptions): CachedQueryStore;
```

---

## `PreparedQuery`

Pre-normalised query object for efficient repeated execution.

**Import:** `import { PreparedQuery, prepare } from '@unrdf/oxigraph/query-cache'`

### Constructor

```typescript
new PreparedQuery(query: string)
```

Normalises and analyses the query at construction time. Subsequent `bind()` and `execute()` calls use the cached normalised form.

### Properties

| Property            | Type           | Description                                                      |
| ------------------- | -------------- | ---------------------------------------------------------------- |
| `originalQuery`     | `string`       | The raw query string as passed to the constructor                |
| `normalizedQuery`   | `string`       | Whitespace-normalised, comment-stripped, keyword-uppercased form |
| `pattern`           | `QueryPattern` | Analysed structure of the query                                  |
| `bindableVariables` | `string[]`     | Variables that appear in the SELECT clause (with `?` prefix)     |
| `executions`        | `number`       | Count of `execute()` calls since construction                    |

### `bind(bindings?)`

```typescript
bind(bindings?: Record<string, string>): string
```

Substitute variables in the normalised query. Returns the bound query string without executing it.

Keys are variable names without `?`. Values are IRI strings. If a value already starts with `<`, it is used as-is; otherwise it is wrapped: `value` → `<value>`.

### `execute(store, bindings?)`

```typescript
execute(store: CachedQueryStore, bindings?: Record<string, string>): any
```

Bind variables, then call `store.query()` with the resulting string. Increments `executions`.

---

## `prepare(query)`

Factory function for `PreparedQuery`.

```typescript
function prepare(query: string): PreparedQuery;
```

---

## SPARQL-star Utilities

**Import:** `import { ... } from '@unrdf/oxigraph/sparql-star'`

### `SPARQLStarQueryBuilder`

Fluent builder for RDF-star annotated SPARQL queries.

#### Constructor

```typescript
new SPARQLStarQueryBuilder();
```

Pre-registers prefixes: `rdf`, `rdfs`, `xsd`, `rdfstar`.

#### Builder Methods

All methods return `this` for chaining.

| Method                                      | Signature                          | Description                                      |
| ------------------------------------------- | ---------------------------------- | ------------------------------------------------ |
| `addPrefix(prefix, iri)`                    | `(string, string) → this`          | Register a namespace prefix                      |
| `select(vars)`                              | `(string[]) → this`                | Set projected variables. `?` prefix is optional. |
| `where(s, p, o)`                            | `(string, string, string) → this`  | Add a regular triple pattern                     |
| `whereQuoted(s, p, o)`                      | `(string, string, string) → this`  | Add an RDF-star quoted triple `<<s p o>>`        |
| `whereAnnotation(variable, type, pattern?)` | `(string, string, string?) → this` | Annotate the last quoted triple                  |
| `filter(condition)`                         | `(string) → this`                  | Add a `FILTER(condition)` clause                 |
| `confidenceThreshold(threshold)`            | `(number) → this`                  | Filter by `rdfstar:confidence >= threshold`      |
| `temporalFilter(timestamp)`                 | `(string) → this`                  | Filter by `rdfstar:validFrom`/`validTo` window   |
| `orderBy(variable, direction?)`             | `(string, 'ASC'\|'DESC') → this`   | Add ORDER BY clause. Default direction: `'ASC'`  |
| `limit(n)`                                  | `(number) → this`                  | Add LIMIT clause                                 |
| `offset(n)`                                 | `(number) → this`                  | Add OFFSET clause                                |

#### `build()`

```typescript
build(): string
```

Return the complete SPARQL-star query string.

### `createSPARQLStarBuilder()`

```typescript
function createSPARQLStarBuilder(): SPARQLStarQueryBuilder;
```

Alias for `new SPARQLStarQueryBuilder()`.

### `executeSPARQLStar(store, query, options?)`

```typescript
function executeSPARQLStar(store: OxigraphStore, query: string, options?: object): any;
```

Execute a SPARQL-star query on any store that has a `query()` method.

**Throws:**

- `Error('Invalid store: must have a query method')`
- `Error('Invalid query: must be a non-empty string')`
- `Error('SPARQL-star query failed: ...')`

### `queryByConfidence(store, threshold, options?)`

```typescript
function queryByConfidence(store: OxigraphStore, threshold: number, options?: object): any[];
```

Return all triples annotated with `rdfstar:confidence >= threshold`, ordered by confidence descending.

### `queryBySource(store, sourceIRI, options?)`

```typescript
function queryBySource(store: OxigraphStore, sourceIRI: string, options?: object): any[];
```

Return all triples annotated with `rdfstar:source = <sourceIRI>`.

### `queryByTemporal(store, timestamp, options?)`

```typescript
function queryByTemporal(store: OxigraphStore, timestamp: string, options?: object): any[];
```

Return all triples that are temporally valid at `timestamp` (ISO 8601 string), based on `rdfstar:validFrom` and `rdfstar:validTo` annotations.

---

## RDF Term Types

All term types implement `equals(other: Term): boolean`.

| Type           | `termType`       | Key fields                                                 |
| -------------- | ---------------- | ---------------------------------------------------------- |
| `NamedNode`    | `'NamedNode'`    | `value: string` (IRI)                                      |
| `BlankNode`    | `'BlankNode'`    | `value: string` (identifier)                               |
| `Literal`      | `'Literal'`      | `value: string`, `language: string`, `datatype: NamedNode` |
| `DefaultGraph` | `'DefaultGraph'` | `value: ''` (always empty string)                          |

**Type aliases:**

| Alias       | Union                                               |
| ----------- | --------------------------------------------------- |
| `Term`      | `NamedNode \| BlankNode \| Literal \| DefaultGraph` |
| `Subject`   | `NamedNode \| BlankNode`                            |
| `Predicate` | `NamedNode`                                         |
| `RDFObject` | `NamedNode \| BlankNode \| Literal`                 |
| `Graph`     | `NamedNode \| BlankNode \| DefaultGraph`            |
