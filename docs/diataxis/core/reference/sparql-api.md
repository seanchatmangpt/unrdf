# Reference: SPARQL API

All functions are exported from `@unrdf/core`.

---

## Synchronous executors

### `executeQuerySync(store, sparql, options?)`

Execute any SPARQL query synchronously. Auto-detects type from the query string.

**Parameters**

| Param     | Type                          | Description         |
| --------- | ----------------------------- | ------------------- |
| `store`   | `UnrdfStore \| OxigraphStore` | Store to query      |
| `sparql`  | `string`                      | SPARQL query string |
| `options` | `QuerySyncOptions`            | Optional config     |

**QuerySyncOptions**

| Field    | Type          | Description         |
| -------- | ------------- | ------------------- |
| `limit`  | `number`      | Result count limit  |
| `offset` | `number`      | Result skip count   |
| `signal` | `AbortSignal` | Cancellation signal |

**Returns**

| Query type | Return value                                                        |
| ---------- | ------------------------------------------------------------------- |
| SELECT     | `BindingRow[]` with `.type === 'select'` and `.rows` property added |
| CONSTRUCT  | `Quad[]` with `.type === 'construct'` and `.quads` property added   |
| DESCRIBE   | `Quad[]` with `.type === 'describe'` and `.quads` property added    |
| ASK        | `boolean`                                                           |

**Note on the `.rows` / `.quads` property** — The returned array has both the array elements
and a `rows` (or `quads`) property that point back to the same array. This exists for
backward-compatibility. Normal iteration (`for...of`, indexing) works on the array directly.

**Throws**

- `TypeError: store is required` if `store` is falsy.
- `TypeError: sparql must be a string` if `sparql` is not a string.
- `Error: SPARQL query execution failed: …` wrapping the underlying error.

---

### `executeSelectSync(store, sparql, options?)`

Typed wrapper for SELECT queries. Throws if the query is not a SELECT.

**Returns** `BindingRow[]`

**Throws** `Error: Query is not a SELECT query` if `sparql` is not a SELECT.

---

### `executeAskSync(store, sparql, options?)`

Typed wrapper for ASK queries. Throws if the query is not an ASK.

**Returns** `boolean`

**Throws** `Error: Query is not an ASK query` if `sparql` is not an ASK.

---

### `executeConstructSync(store, sparql, options?)`

Typed wrapper for CONSTRUCT queries. Accepts DESCRIBE as well.

**Returns** `Quad[]`

**Throws** `Error: Query is not a CONSTRUCT query` if `sparql` is neither CONSTRUCT nor DESCRIBE.

---

### `prepareQuerySync(sparql)`

Parse and inspect a SPARQL query without executing it.

**Parameters**

| Param    | Type     | Description         |
| -------- | -------- | ------------------- |
| `sparql` | `string` | SPARQL query string |

**Returns**

```typescript
{
  type: string;        // lowercase query type: 'select', 'ask', 'construct', 'describe'
  queryType: string;   // uppercase: 'SELECT', 'ASK', 'CONSTRUCT', 'DESCRIBE'
  variables: string[]; // all ?variable names found in the string (including subject vars)
  prefixes: Record<string, string>; // PREFIX bindings from the query
  query: string;       // the original query string
  sparql: string;      // alias for query
}
```

**Throws**

- `TypeError: prepareQuerySync: sparql must be a string` if not a string.
- `Error: Query preparation failed: …` on unexpected errors.

---

## Async executors (legacy)

These are async wrappers that delegate to the synchronous executors. They exist for
backward-compatibility. For new code, prefer `store.query()` or the sync executors.

### `executeQuery(store, sparql, options?)`

```typescript
async function executeQuery(store, sparql, options?): Promise<BindingRow[] | boolean | Quad[]>;
```

### `executeSelect(store, sparql, options?)`

```typescript
async function executeSelect(store, sparql, options?): Promise<BindingRow[]>;
```

### `executeAsk(store, sparql, options?)`

```typescript
async function executeAsk(store, sparql, options?): Promise<boolean>;
```

### `executeConstruct(store, sparql, options?)`

```typescript
async function executeConstruct(store, sparql, options?): Promise<Quad[]>;
```

### `prepareQuery(sparql)`

```typescript
async function prepareQuery(sparql): Promise<QueryMetadata>;
```

---

## `QueryBuilder` (fluent SPARQL construction)

Exported as `QueryBuilder` (class) and `sparql` (factory function) from
`@unrdf/core/sparql/query-builder.mjs` and re-exported from `@unrdf/core/sparql/index.mjs`.

### `sparql(options?)`

Factory function. Returns a new `QueryBuilder`.

**Options**

| Field      | Type                     | Description                |
| ---------- | ------------------------ | -------------------------- |
| `prefixes` | `Record<string, string>` | Initial prefix map         |
| `baseIRI`  | `string`                 | Base IRI for relative URIs |

All `QueryBuilder` methods return `this` for chaining, except `build()`.

---

### `.prefix(prefix, uri)`

Add a PREFIX declaration.

```javascript
builder.prefix('foaf', 'http://xmlns.com/foaf/0.1/');
```

**Throws** `ZodError` if prefix or uri is invalid.

---

### `.base(iri)`

Set the BASE IRI.

```javascript
builder.base('http://example.org/');
```

---

### `.select(...variables)`

Start a SELECT query. Variables may include or omit the `?` prefix. Pass `'*'` for
`SELECT *`.

```javascript
builder.select('?name', '?email');
builder.select('*');
```

---

### `.distinct()`

Add the `DISTINCT` modifier to SELECT.

---

### `.where(pattern)`

Add a WHERE clause triple pattern.

```javascript
builder.where('?person foaf:name ?name');
```

Multiple calls add multiple patterns to the same WHERE block.

---

### `.optional(pattern)`

Add an OPTIONAL block.

```javascript
builder.optional('?person foaf:mbox ?email');
```

---

### `.filter(expression)`

Add a FILTER clause. The expression is wrapped in `FILTER(…)`.

```javascript
builder.filter('REGEX(?email, "@example.com")');
```

---

### `.orderBy(...variables)`

Add ORDER BY. Prefix a variable with `-` for descending order.

```javascript
builder.orderBy('?name', '-?age');
// ORDER BY ?name DESC(?age)
```

---

### `.groupBy(...variables)`

Add GROUP BY.

---

### `.having(expression)`

Add a HAVING clause.

---

### `.limit(n)`

Add `LIMIT n`.

---

### `.offset(n)`

Add `OFFSET n`.

---

### `.construct(...patterns)`

Start a CONSTRUCT query. Patterns are triple patterns as strings.

```javascript
builder.construct('?s foaf:name ?name').where('?s foaf:name ?name');
```

---

### `.insert(...patterns)` / `.delete(...patterns)`

Start an INSERT/DELETE query.

---

### `.graph(iri, fn)`

Scope patterns to a named graph.

```javascript
builder.graph('http://example.org/graph1', b => {
  b.where('?s ?p ?o');
});
```

---

### `.union(fn)`

Add a UNION block.

---

### `.build()`

Return the final SPARQL query string.

**Throws** `Error` if the query type has not been set (no `.select()`, `.construct()`, etc.).

---

## `BindingRow` type

The binding row object returned by SELECT queries:

```typescript
interface BindingRow {
  [variableName: string]: {
    type: string; // 'NamedNode', 'Literal', 'BlankNode'
    value: string; // IRI string or literal value
    language?: string; // present for language-tagged literals
    datatype?: string; // present for typed literals (IRI string)
  };
  // Row also has a .get(key) method for backward compatibility
  get(key: string): TermObject | undefined;
}
```

---

## `QueryMetadata` type (from `prepareQuerySync`)

```typescript
interface QueryMetadata {
  type: string; // lowercase: 'select', 'ask', ...
  queryType: string; // uppercase: 'SELECT', 'ASK', ...
  variables: string[]; // e.g. ['name', 's', 'email']
  prefixes: Record<string, string>;
  query: string;
  sparql: string;
}
```
