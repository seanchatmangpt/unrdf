# Tutorial 01: Store and Query RDF Data

By the end of this tutorial you will have a working script that builds a small knowledge graph
of people and organisations, queries it with SPARQL, and inspects the results.

## What you will build

A script that:

1. Creates an RDF store
2. Adds quads describing three people
3. Runs a SELECT query to find all names
4. Runs an ASK query to check whether a person exists
5. Runs a CONSTRUCT query to extract a subgraph

## Step 1 — Import the building blocks

```javascript
// people-graph.mjs
import { createUnrdfStore, namedNode, literal, quad, FOAF, RDF } from '@unrdf/core';
```

`createUnrdfStore` returns an `UnrdfStore` instance backed by Oxigraph. `namedNode`, `literal`,
and `quad` are RDF/JS DataFactory functions. `FOAF` and `RDF` are pre-built namespace helpers
so you do not have to type full IRIs everywhere.

## Step 2 — Create a store and add quads

An RDF quad has four components: subject, predicate, object, and graph. Subjects and predicates
are named nodes (IRIs). Objects can be named nodes or literals. The graph defaults to the
default graph when omitted.

```javascript
const store = createUnrdfStore();

// Quad helper: quad(subject, predicate, object)
store.add(quad(namedNode('http://example.org/alice'), RDF.type, FOAF.Person));

store.add(quad(namedNode('http://example.org/alice'), FOAF.name, literal('Alice')));

store.add(
  quad(
    namedNode('http://example.org/alice'),
    FOAF.age,
    literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
  )
);

store.add(quad(namedNode('http://example.org/bob'), RDF.type, FOAF.Person));

store.add(quad(namedNode('http://example.org/bob'), FOAF.name, literal('Bob')));

store.add(
  quad(namedNode('http://example.org/bob'), FOAF.knows, namedNode('http://example.org/alice'))
);

console.log('Store size:', store.size()); // 6
```

## Step 3 — Run a SELECT query

`store.query()` is synchronous. It auto-detects the query type from the SPARQL keyword.

```javascript
const rows = store.query(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?person ?name WHERE {
    ?person foaf:name ?name .
  }
  ORDER BY ?name
`);

// rows is an array of plain objects keyed by variable name
for (const row of rows) {
  console.log(row.name.value);
}
// Alice
// Bob
```

Each row is a plain object. Variable bindings are accessed by variable name. Each binding
object has a `type` and a `value` field, and optionally `language` or `datatype`.

## Step 4 — Run an ASK query

ASK returns a boolean directly:

```javascript
const aliceExists = store.query(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  ASK { <http://example.org/alice> foaf:name ?name }
`);

console.log('Alice exists:', aliceExists); // true

const charlieExists = store.query(`
  ASK { <http://example.org/charlie> ?p ?o }
`);

console.log('Charlie exists:', charlieExists); // false
```

## Step 5 — Run a CONSTRUCT query

CONSTRUCT returns an array of quads:

```javascript
const constructedQuads = store.query(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  CONSTRUCT {
    ?person foaf:name ?name .
  }
  WHERE {
    ?person a foaf:Person ;
            foaf:name ?name .
  }
`);

console.log('Constructed quads:', constructedQuads.length); // 2
for (const q of constructedQuads) {
  console.log(q.subject.value, '->', q.object.value);
}
// http://example.org/alice -> Alice
// http://example.org/bob -> Bob
```

## Step 6 — Check membership and remove a quad

```javascript
const q = quad(namedNode('http://example.org/bob'), FOAF.name, literal('Bob'));

console.log('Has Bob name:', store.has(q)); // true

store.delete(q);
console.log('Has Bob name after delete:', store.has(q)); // false
console.log('Store size:', store.size()); // 5
```

## Complete working script

```javascript
// people-graph.mjs
import { createUnrdfStore, namedNode, literal, quad, FOAF, RDF } from '@unrdf/core';

const store = createUnrdfStore();

// Add people
store.add(quad(namedNode('http://example.org/alice'), RDF.type, FOAF.Person));
store.add(quad(namedNode('http://example.org/alice'), FOAF.name, literal('Alice')));
store.add(quad(namedNode('http://example.org/bob'), RDF.type, FOAF.Person));
store.add(quad(namedNode('http://example.org/bob'), FOAF.name, literal('Bob')));
store.add(
  quad(namedNode('http://example.org/bob'), FOAF.knows, namedNode('http://example.org/alice'))
);

console.log('Store has', store.size(), 'quads');

// SELECT - find all names
const names = store.query(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name WHERE { ?s foaf:name ?name }
  ORDER BY ?name
`);
console.log(
  'Names:',
  names.map(r => r.name.value)
);
// Names: [ 'Alice', 'Bob' ]

// ASK - does alice exist?
const hasAlice = store.query(`ASK { <http://example.org/alice> ?p ?o }`);
console.log('Has alice:', hasAlice); // true

// CONSTRUCT - extract name triples only
const nameTriples = store.query(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  CONSTRUCT { ?s foaf:name ?name } WHERE { ?s foaf:name ?name }
`);
console.log('Name triples:', nameTriples.length); // 2
```

## Next steps

- Tutorial 02: [Validate a graph with SHACL](./02-validate-with-shacl.md) — define shape
  constraints and generate a validation report.
- How-To: [Bulk-add quads](../how-to/01-bulk-add-quads.md) — load thousands of quads
  efficiently in a single operation.
- How-To: [Run SPARQL queries](../how-to/02-run-sparql-queries.md) — advanced query patterns,
  the QueryBuilder, and options like `resultsFormat`.
