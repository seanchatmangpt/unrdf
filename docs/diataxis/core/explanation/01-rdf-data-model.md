# Explanation: The RDF Data Model

Understanding the RDF data model helps you use `@unrdf/core` correctly and avoid common
mistakes around blank nodes, named graphs, and typed literals.

## Triples and quads

The core unit of RDF data is a _triple_: a statement of the form `subject predicate object`.

```
<http://example.org/alice>  <http://xmlns.com/foaf/0.1/name>  "Alice"
```

A _quad_ (also called an _RDF statement in a named graph_) adds a fourth component — the
graph IRI — to indicate which graph the triple belongs to:

```
subject     predicate     object     graph
<alice>     foaf:name     "Alice"    <http://example.org/myGraph>
```

`@unrdf/core` always uses quads internally, even when you work with "triples": quads without
an explicit graph component are placed in the _default graph_, represented as a term with
`termType: 'DefaultGraph'` and `value: ''`.

```javascript
import { quad, namedNode, literal, defaultGraph } from '@unrdf/core';

// These two calls produce the same quad:
const q1 = quad(namedNode('http://s'), namedNode('http://p'), literal('o'));
const q2 = quad(namedNode('http://s'), namedNode('http://p'), literal('o'), defaultGraph());
```

## The four term types

Every component of a quad is an _RDF term_, one of four types:

| `termType`       | Describes                               | Example                                          |
| ---------------- | --------------------------------------- | ------------------------------------------------ |
| `'NamedNode'`    | A resource identified by an IRI         | `namedNode('http://example.org/alice')`          |
| `'Literal'`      | A data value (string, integer, date, …) | `literal('Alice')`, `literal('30', XSD.integer)` |
| `'BlankNode'`    | An anonymous resource (no global IRI)   | `blankNode('b0')`                                |
| `'DefaultGraph'` | The unnamed default graph               | `defaultGraph()`                                 |

There is also `'Variable'` (`variable('name')`), used in SPARQL query patterns rather than
stored data.

## Literals and datatypes

A literal has a string value and optionally a _datatype IRI_ or a _language tag_.

```javascript
import { literal, XSD } from '@unrdf/core';

literal('Alice'); // plain string, datatype xsd:string
literal('30', XSD.integer); // typed literal
literal('Bonjour', 'fr'); // language-tagged literal (no datatype)
```

Plain `literal('Alice')` is shorthand for `xsd:string`. Language-tagged literals have no
datatype — the language tag `'fr'` is separate.

Typed literals carry the datatype IRI as a named node:

```javascript
const age = literal('30', XSD.integer);
console.log(age.value); // '30'  (always a string)
console.log(age.datatype.value); // 'http://www.w3.org/2001/XMLSchema#integer'
```

The `value` of a literal is always a string, regardless of datatype. It is your application's
responsibility to parse `'30'` as a number when needed.

## Named graphs and the default graph

An RDF dataset is a collection of graphs. Most stores have one _default graph_ and zero or
more _named graphs_. Named graphs let you partition data, track provenance, or apply different
SHACL shapes to different graph regions.

```javascript
const personGraph = namedNode('http://example.org/persons');

store.add(
  quad(
    namedNode('http://example.org/alice'),
    FOAF.name,
    literal('Alice'),
    personGraph // <-- this triple lives in the named graph
  )
);
```

Querying with SPARQL uses `GRAPH` clauses to target named graphs:

```sparql
SELECT ?name WHERE {
  GRAPH <http://example.org/persons> {
    ?person foaf:name ?name .
  }
}
```

If you do not specify a graph when adding quads, they go into the default graph and are
accessible from queries without a GRAPH clause.

## Blank nodes

A blank node identifies a resource within a document or store without giving it a global IRI.
Blank nodes are scoped: the identifier `_:b0` in one document has no relationship to `_:b0`
in another.

```javascript
const person = blankNode(); // auto-generates an ID
store.add(quad(person, FOAF.name, literal('Anonymous')));
```

Blank nodes are preserved across `dump` and `load` cycles within the same store but are
re-labelled during canonicalization (URDNA2015 replaces blank node IDs with deterministic
canonical names). Two stores that look structurally identical but use different blank node
IDs will therefore produce the same canonical N-Quads string, and `isIsomorphic` will return
`true`.

## Why SPARQL bindings return term objects, not raw values

When a SELECT query returns `row.name`, the value is a term object like
`{ type: 'Literal', value: 'Alice', datatype: 'http://www.w3.org/2001/XMLSchema#string' }`,
not the plain string `'Alice'`.

This design preserves full term information — type, language, and datatype — which is often
needed when writing further queries, storing results back as quads, or distinguishing IRIs
from literals. If you only need the string value, access `.value`:

```javascript
const name = row.name.value; // 'Alice'
const iri = row.person.value; // 'http://example.org/alice'
```

## SHACL and the RDF data model

SHACL shapes are themselves expressed as RDF quads (in the SHACL vocabulary). The
`ShapeBuilder.build()` method returns an array of quads, not a JSON object. This means:

1. You can store shapes in a named graph inside the same store as your data.
2. You can query the shapes with SPARQL (e.g. to enumerate all constraints).
3. You can serialise them with `store.dump()` and reload them from a file.

The `rdf-validate-shacl` library that `@unrdf/core` wraps uses N3 quads internally. The
`shacl-validator.mjs` layer handles the conversion between Oxigraph and N3 representations
transparently.

## See also

- [Storage backends](./02-storage-backends.md) — how the data model maps to the two store
  implementations.
- [Reference: Store API](../reference/store-api.md) — authoritative signatures.
