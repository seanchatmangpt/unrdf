# @unrdf/core API Reference

Complete API documentation for the RDF core substrate.

## Table of Contents

- [Store Operations](#store-operations)
- [SPARQL Execution](#sparql-execution)
- [Types and Constants](#types-and-constants)
- [Examples](#examples)

## Store Operations

### `createStore()`

Creates a new in-memory RDF store.

```javascript
import { createStore } from '@unrdf/core'

const store = createStore()
```

**Returns**: `Store` - New RDF store instance

---

### `addQuad(store, quad)`

Adds an RDF quad to the store.

```javascript
import { createStore, addQuad, namedNode, literal } from '@unrdf/core'

const store = createStore()
addQuad(store, {
  subject: namedNode('http://example.com/alice'),
  predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
  object: literal('Alice'),
  graph: defaultGraph()
})
```

**Parameters**:
- `store` - RDF store instance
- `quad` - Quad object with subject, predicate, object, graph

**Returns**: `void`

---

### `removeQuad(store, quad)`

Removes an RDF quad from the store.

```javascript
removeQuad(store, quad)
```

**Parameters**:
- `store` - RDF store instance
- `quad` - Quad to remove

**Returns**: `void`

---

### `getQuads(store, subject, predicate, object, graph)`

Queries quads from the store.

```javascript
import { getQuads } from '@unrdf/core'

const quads = getQuads(store, null, null, null)
// Returns all quads

const bySubject = getQuads(store, subject, null, null)
// Returns quads with matching subject
```

**Parameters**:
- `store` - RDF store instance
- `subject` - Optional subject to match (null = any)
- `predicate` - Optional predicate to match (null = any)
- `object` - Optional object to match (null = any)
- `graph` - Optional graph to match (null = any)

**Returns**: `Array<Quad>` - Matching quads

---

### `iterateQuads(store)`

Iterates over all quads in the store.

```javascript
import { iterateQuads } from '@unrdf/core'

for (const quad of iterateQuads(store)) {
  console.log(quad.subject.value)
}
```

**Parameters**:
- `store` - RDF store instance

**Returns**: `Iterable<Quad>` - Iterator over quads

---

## SPARQL Execution

### `executeQuery(store, sparqlQuery, options?)`

Executes a SPARQL query against the store.

```javascript
import { executeQuery } from '@unrdf/core'

const results = await executeQuery(store, `
  SELECT ?name WHERE {
    ?s foaf:name ?name
  }
`)

console.log(results)
// [
//   { name: { termType: 'Literal', value: 'Alice' } },
//   { name: { termType: 'Literal', value: 'Bob' } }
// ]
```

**Parameters**:
- `store` - RDF store instance
- `sparqlQuery` - SPARQL query string
- `options` - Optional query options

**Returns**: `Promise<Array>` - Query results

---

### `prepareQuery(sparqlQuery)`

Parses a SPARQL query without executing it.

```javascript
import { prepareQuery } from '@unrdf/core'

const parsed = prepareQuery('SELECT ?name WHERE { ?s foaf:name ?name }')
```

**Parameters**:
- `sparqlQuery` - SPARQL query string

**Returns**: `ParsedQuery` - Parsed query object

---

## Types and Constants

### `createTerms()`

Creates RDF term constructors.

```javascript
import { createTerms } from '@unrdf/core'

const { namedNode, literal, blankNode, variable } = createTerms()

const name = namedNode('http://xmlns.com/foaf/0.1/name')
const alice = literal('Alice')
```

**Returns**: `Object` with term constructors

---

### Common Namespaces

Pre-defined namespace constants:

```javascript
import { RDF, RDFS, OWL, XSD, FOAF } from '@unrdf/core'

// Usage
const typeQuad = {
  predicate: RDF.type,
  object: FOAF.Person
}
```

**Available**:
- `RDF` - RDF namespace
- `RDFS` - RDFS namespace
- `OWL` - OWL namespace
- `XSD` - XML Schema namespace
- `FOAF` - Friend of a Friend namespace

---

## Examples

See `examples/` directory for complete examples:
- `basic-operations.mjs` - Store CRUD operations
- `sparql-queries.mjs` - SPARQL query examples
- `canonicalization.mjs` - RDF canonicalization

---

## Further Reading

- [RDF Concepts](https://www.w3.org/TR/rdf11-concepts/)
- [SPARQL 1.1 Query Language](https://www.w3.org/TR/sparql11-query/)
- [RDF-JS Data Model](https://rdf.js.org/)
