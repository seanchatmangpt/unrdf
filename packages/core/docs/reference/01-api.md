# API Reference: @unrdf/core

Complete reference for all functions exported by @unrdf/core.

---

## Store Creation

### createUnrdfStore(options?)

Create a synchronous in-memory RDF store.

**Signature:**
```javascript
function createUnrdfStore(options?: UnrdfStoreOptions): UnrdfStore
```

**Parameters:**
- `options.indexing?` (string) - Index strategy: `'predicate'` | `'object'` | `'none'`
- `options.maxSize?` (number) - Maximum number of quads (default: unlimited)
- `options.autoGC?` (boolean) - Automatic garbage collection (default: true)

**Returns:** UnrdfStore instance

**Example:**
```javascript
import { createUnrdfStore } from '@unrdf/core';

const store = createUnrdfStore({
  indexing: 'predicate',
  maxSize: 1000000
});
```

### createStore(options?)

Create an asynchronous RDF store (legacy API).

**Signature:**
```javascript
async function createStore(options?: StoreOptions): AsyncStore
```

**Parameters:**
- `options` - Same as createUnrdfStore

**Returns:** Promise<AsyncStore>

**Example:**
```javascript
import { createStore } from '@unrdf/core';

const store = await createStore();
```

---

## Store Methods (Synchronous)

### store.addQuad(quad)

Add a single quad to the store.

**Signature:**
```javascript
store.addQuad(quad: Quad): void
```

**Parameters:**
- `quad.subject` - NamedNode or BlankNode
- `quad.predicate` - NamedNode
- `quad.object` - NamedNode, Literal, or BlankNode
- `quad.graph?` - NamedNode (default: default graph)

**Throws:** Error if quad is invalid

**Example:**
```javascript
store.addQuad({
  subject: namedNode('http://example.com/alice'),
  predicate: namedNode('http://xmlns.com/foaf/0.1/knows'),
  object: namedNode('http://example.com/bob')
});
```

### store.removeQuad(quad)

Remove a quad from the store.

**Signature:**
```javascript
store.removeQuad(quad: Quad): boolean
```

**Returns:** True if quad was removed, false if not found

**Example:**
```javascript
const removed = store.removeQuad(quad);
if (removed) console.log('Removed');
```

### store.match(subject?, predicate?, object?, graph?)

Find all quads matching a pattern.

**Signature:**
```javascript
store.match(
  subject?: Term,
  predicate?: Term,
  object?: Term,
  graph?: Term
): Quad[]
```

**Parameters:** All optional; omit to match anything

**Returns:** Array of matching quads

**Example:**
```javascript
// All quads with subject = alice
const quads = store.match(
  namedNode('http://example.com/alice')
);

// All quads about anyone who knows anyone
const knowsQuads = store.match(
  undefined,
  namedNode('http://xmlns.com/foaf/0.1/knows'),
  undefined
);
```

### store.size

Get the total number of quads in the store.

**Signature:**
```javascript
store.size: number
```

**Example:**
```javascript
console.log(`Store has ${store.size} quads`);
```

---

## Query Execution (Synchronous)

### executeQuerySync(store, query, options?)

Execute a SPARQL query synchronously.

**Signature:**
```javascript
function executeQuerySync(
  store: UnrdfStore,
  query: string,
  options?: QueryOptions
): Binding[]
```

**Parameters:**
- `query` - SPARQL query string
- `options.timeout?` - Maximum query time in milliseconds
- `options.base?` - Base IRI for relative references

**Returns:** Array of variable bindings (objects)

**Throws:** Error on syntax or execution error

**Example:**
```javascript
import { executeQuerySync } from '@unrdf/core';

const results = executeQuerySync(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name WHERE {
    ?person foaf:name ?name .
  }
`);

results.forEach(binding => {
  console.log(binding.name.value);
});
```

### executeSelectSync(store, query, options?)

Execute a SPARQL SELECT query.

**Signature:**
```javascript
function executeSelectSync(
  store: UnrdfStore,
  query: string,
  options?: QueryOptions
): Binding[]
```

**Returns:** Array of bindings

**Example:**
```javascript
const people = executeSelectSync(store, `
  SELECT ?name { ?x foaf:name ?name }
`);
```

### executeAskSync(store, query, options?)

Execute a SPARQL ASK query (returns boolean).

**Signature:**
```javascript
function executeAskSync(
  store: UnrdfStore,
  query: string,
  options?: QueryOptions
): boolean
```

**Returns:** True if any matches, false otherwise

**Example:**
```javascript
const hasAlice = executeAskSync(store, `
  PREFIX ex: <http://example.com/>
  ASK { ex:alice ?p ?o }
`);
```

### executeConstructSync(store, query, options?)

Execute a SPARQL CONSTRUCT query (returns quads).

**Signature:**
```javascript
function executeConstructSync(
  store: UnrdfStore,
  query: string,
  options?: QueryOptions
): Quad[]
```

**Returns:** Array of constructed quads

**Example:**
```javascript
const derived = executeConstructSync(store, `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  CONSTRUCT { ?x foaf:knows ?y }
  WHERE { ?x foaf:knows ?y }
`);
```

### prepareQuerySync(query, options?)

Pre-compile a query for repeated execution.

**Signature:**
```javascript
function prepareQuerySync(
  query: string,
  options?: QueryOptions
): PreparedQuery
```

**Returns:** PreparedQuery object with execute() method

**Example:**
```javascript
const prepared = prepareQuerySync(`
  SELECT ?name WHERE { ?x foaf:name ?name }
`);

// Execute multiple times
const results1 = prepared.execute(store1);
const results2 = prepared.execute(store2);
```

---

## Term Creation

### namedNode(value)

Create a named node (IRI).

**Signature:**
```javascript
function namedNode(value: string): NamedNode
```

**Example:**
```javascript
import { namedNode } from '@unrdf/core';

const alice = namedNode('http://example.com/alice');
```

### literal(value, datatype?)

Create a literal value.

**Signature:**
```javascript
function literal(
  value: string,
  datatype?: NamedNode | string
): Literal
```

**Parameters:**
- `value` - String value
- `datatype` - Optional data type (e.g., 'http://www.w3.org/2001/XMLSchema#integer')

**Example:**
```javascript
import { literal, namedNode } from '@unrdf/core';

// String literal
const name = literal('Alice');

// Typed literal
const age = literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer'));

// With language tag
const greeting = literal('Hello', 'en');
```

### blankNode(id?)

Create a blank node.

**Signature:**
```javascript
function blankNode(id?: string): BlankNode
```

**Parameters:**
- `id?` - Optional identifier (auto-generated if omitted)

**Example:**
```javascript
import { blankNode } from '@unrdf/core';

const bn = blankNode('node1');
// or
const autoId = blankNode();  // _:b12345
```

### variable(name)

Create a SPARQL variable.

**Signature:**
```javascript
function variable(name: string): Variable
```

**Example:**
```javascript
import { variable } from '@unrdf/core';

const ?person = variable('person');
```

### defaultGraph()

Get the default graph.

**Signature:**
```javascript
function defaultGraph(): DefaultGraph
```

---

## Utility Functions

### canonicalize(quads)

Canonicalize quads (normalize representation).

**Signature:**
```javascript
function canonicalize(quads: Quad[]): Quad[]
```

**Returns:** Canonicalized quads

**Example:**
```javascript
import { canonicalize } from '@unrdf/core';

const canonical = canonicalize(quads);
// Useful for comparing RDF graphs
if (isIsomorphic(quads1, quads2)) {
  console.log('Graphs are equivalent');
}
```

### toNTriples(quads)

Convert quads to N-Triples string format.

**Signature:**
```javascript
function toNTriples(quads: Quad[]): string
```

**Returns:** N-Triples formatted string

**Example:**
```javascript
import { toNTriples } from '@unrdf/core';

const nTriples = toNTriples(quads);
fs.writeFileSync('output.nt', nTriples);
```

### sortQuads(quads)

Sort quads in canonical order.

**Signature:**
```javascript
function sortQuads(quads: Quad[]): Quad[]
```

**Returns:** Sorted quads

**Example:**
```javascript
const sorted = sortQuads(quads);
```

### isIsomorphic(quads1, quads2)

Check if two sets of quads are graph-isomorphic.

**Signature:**
```javascript
function isIsomorphic(
  quads1: Quad[],
  quads2: Quad[]
): boolean
```

**Returns:** True if graphs are equivalent

**Example:**
```javascript
if (isIsomorphic(graph1, graph2)) {
  console.log('Graphs are equivalent');
}
```

---

## Constants

### RDF, RDFS, OWL, XSD, FOAF, DCTERMS, SKOS

Namespace objects for common vocabularies.

**Example:**
```javascript
import { RDF, FOAF } from '@unrdf/core';

// Creates appropriate NamedNodes
RDF.type       // <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>
FOAF.name      // <http://xmlns.com/foaf/0.1/name>
```

### COMMON_PREFIXES

Object containing all common namespace prefix declarations.

**Example:**
```javascript
import { COMMON_PREFIXES } from '@unrdf/core';

const prefixDeclarations = Object.entries(COMMON_PREFIXES)
  .map(([prefix, uri]) => `PREFIX ${prefix}: <${uri}>`)
  .join('\n');
```

---

## Types

### Binding

Result of a SELECT query.

```typescript
type Binding = {
  [variableName: string]: Term
}
```

**Example:**
```javascript
// Query: SELECT ?name ?age
// Result binding:
{
  name: { type: 'Literal', value: 'Alice' },
  age: { type: 'Literal', value: '30' }
}
```

### Quad

Complete RDF statement.

```typescript
type Quad = {
  subject: NamedNode | BlankNode
  predicate: NamedNode
  object: NamedNode | Literal | BlankNode
  graph?: NamedNode | DefaultGraph
}
```

### Term

Any RDF term (can be Subject, Predicate, Object, or Graph).

```typescript
type Term = NamedNode | BlankNode | Literal | Variable | DefaultGraph
```

### NamedNode

IRI/URI reference.

```typescript
type NamedNode = {
  type: 'NamedNode'
  value: string  // Full IRI
}
```

### Literal

Text or typed value.

```typescript
type Literal = {
  type: 'Literal'
  value: string
  datatype?: NamedNode
  language?: string
}
```

### BlankNode

Anonymous resource.

```typescript
type BlankNode = {
  type: 'BlankNode'
  value: string  // Local identifier
}
```

---

## Error Handling

All query functions throw Error on failure:

```javascript
try {
  const results = executeQuerySync(store, invalidQuery);
} catch (error) {
  console.error('Query failed:', error.message);
}
```

### Common Errors

| Message | Cause | Solution |
|---------|-------|----------|
| "Unexpected token" | SPARQL syntax error | Check query syntax |
| "Unknown variable" | Variable not in SELECT | Add to WHERE clause |
| "Cannot convert" | Type mismatch | Cast types explicitly |
| "Timeout" | Query took too long | Add LIMIT or optimize |

---

## Async API (Legacy)

### createStore(options?)

Create async store.

```javascript
const store = await createStore();
const quads = await addQuad(store, quad);
```

### Async Functions

- `createStore()` - Create async store
- `addQuad(store, quad)` - Add quad asynchronously
- `removeQuad(store, quad)` - Remove quad
- `getQuads(store, ...)` - Get quads
- `iterateQuads(store, ...)` - Iterate through quads
- `countQuads(store, ...)` - Count matching quads
- `executeQuery(store, query)` - Execute query asynchronously
- `executeSelect(store, query)` - SELECT asynchronously
- `executeAsk(store, query)` - ASK asynchronously
- `executeConstruct(store, query)` - CONSTRUCT asynchronously

See tutorials for examples.

---

## Next Reading

- **TYPES.md** (Reference) - Detailed type definitions
- **CONFIGURATION.md** (Reference) - Configuration options
- **optimize-sparql-queries** (How-To) - Make queries faster
