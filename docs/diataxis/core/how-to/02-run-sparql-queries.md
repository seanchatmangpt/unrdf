# How-To: Run SPARQL Queries

## Choose the right executor

| Situation                                              | Use                                                           |
| ------------------------------------------------------ | ------------------------------------------------------------- |
| You have an `UnrdfStore` and want synchronous results  | `store.query(sparql)`                                         |
| You need async/await in a pipeline                     | `store.queryAsync(sparql)` or `executeQuery(store, sparql)`   |
| You want typed wrappers that throw on wrong query kind | `executeSelectSync`, `executeAskSync`, `executeConstructSync` |
| You want to build queries programmatically             | `sparql()` QueryBuilder                                       |

---

## Run a SELECT query

```javascript
import { createUnrdfStore, namedNode, literal, quad, FOAF, RDF } from '@unrdf/core';

const store = createUnrdfStore();
store.add(quad(namedNode('http://example.org/alice'), FOAF.name, literal('Alice')));
store.add(quad(namedNode('http://example.org/bob'), FOAF.name, literal('Bob')));

const rows = store.query(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?person ?name WHERE { ?person foaf:name ?name }
  ORDER BY ?name
`);

for (const row of rows) {
  // row.name and row.person are term objects with .value
  console.log(row.person.value, row.name.value);
}
```

## Request W3C SPARQL JSON results format

Pass `{ resultsFormat: 'json' }` to get the SPARQL 1.1 JSON Results wire format:

```javascript
const json = store.query(
  `PREFIX foaf: <http://xmlns.com/foaf/0.1/>
   SELECT ?name WHERE { ?s foaf:name ?name }`,
  { resultsFormat: 'json' }
);

// json.head.vars === ['name']
// json.results.bindings === [{ name: { type: 'Literal', value: 'Alice' } }, ...]
console.log(json.head.vars);
console.log(json.results.bindings);
```

## Run an ASK query

```javascript
const exists = store.query(`ASK { <http://example.org/alice> ?p ?o }`);
// exists is a boolean
if (exists) {
  console.log('Found');
}
```

Use the typed helper to guard against accidentally passing a non-ASK query:

```javascript
import { executeAskSync } from '@unrdf/core';

const result = executeAskSync(store, `ASK { ?s ?p ?o }`);
// Throws if the query string is not an ASK query
```

## Run a CONSTRUCT query

```javascript
const quads = store.query(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  CONSTRUCT { ?s foaf:name ?name }
  WHERE    { ?s foaf:name ?name }
`);

// quads is an array of RDF quad objects
for (const q of quads) {
  console.log(q.subject.value, '->', q.object.value);
}
```

## Run a DESCRIBE query

```javascript
const description = store.query(`DESCRIBE <http://example.org/alice>`);
// Returns an array of quads describing alice
```

## Use the QueryBuilder for programmatic construction

The `sparql()` factory builds SPARQL strings via method chaining. This is useful when clauses
are determined at runtime.

```javascript
import { sparql } from '@unrdf/core/sparql/query-builder';
// or from the SPARQL index
import { sparql } from '@unrdf/core';
```

> Note: `sparql` is exported from `@unrdf/core/sparql/index.mjs`. Import it directly if your
> bundler resolves subpath exports.

```javascript
const query = sparql()
  .prefix('foaf', 'http://xmlns.com/foaf/0.1/')
  .select('?person', '?name', '?email')
  .where('?person a foaf:Person')
  .where('?person foaf:name ?name')
  .where('?person foaf:mbox ?email')
  .filter('REGEX(?email, "@example.com")')
  .orderBy('?name')
  .limit(20)
  .offset(0)
  .build();

const rows = store.query(query);
```

### SELECT DISTINCT

```javascript
const query = sparql().select('?name').distinct().where('?s foaf:name ?name').build();
```

### CONSTRUCT with QueryBuilder

```javascript
const query = sparql()
  .prefix('foaf', 'http://xmlns.com/foaf/0.1/')
  .construct('?s foaf:name ?name')
  .where('?s foaf:name ?name')
  .build();

const quads = store.query(query);
```

## Use the async functional API (legacy)

The functional `executeQuery` / `executeSelect` / `executeAsk` / `executeConstruct` are async
wrappers that delegate to the synchronous executor. They exist for backward compatibility.

```javascript
import { createStore, addQuad, namedNode, literal, FOAF, executeSelect } from '@unrdf/core';

const store = createStore();
addQuad(store, {
  subject: namedNode('http://example.org/alice'),
  predicate: FOAF.name,
  object: literal('Alice'),
  graph: { value: '', termType: 'DefaultGraph' },
});

const rows = await executeSelect(
  store,
  `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name WHERE { ?s foaf:name ?name }
`
);
```

## Prepare and inspect a query without executing it

```javascript
import { prepareQuerySync } from '@unrdf/core';

const meta = prepareQuerySync(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name ?email WHERE { ?s foaf:name ?name ; foaf:mbox ?email }
`);

console.log(meta.queryType); // 'SELECT'
console.log(meta.variables); // ['name', 'email', 's']
console.log(meta.prefixes); // { foaf: 'http://xmlns.com/foaf/0.1/' }
```

## See also

- Reference: [SPARQL API](../reference/sparql-api.md) — complete signatures.
- Reference: [`UnrdfStore` API](../reference/store-api.md) — `query`, `queryAsync`, `update`.
- How-To: [Export to Turtle/N-Quads](./03-export-to-turtle.md).
