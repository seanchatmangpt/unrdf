# Tutorial 02: Validate a Graph with SHACL

By the end of this tutorial you will have a script that defines a SHACL shape for a `Person`
class, validates an RDF graph against it, and interprets the resulting report.

## What you will build

A script that:

1. Builds a SHACL shape using the fluent `ShapeBuilder` API
2. Loads some RDF data — one conforming person and one non-conforming person
3. Validates the data against the shape
4. Reads the validation report

## Background: what is SHACL?

SHACL (Shapes Constraint Language) is a W3C standard for defining rules that RDF graphs must
satisfy. A _shape_ targets nodes in the graph (for example, all resources typed `foaf:Person`)
and declares constraints on their properties (for example, `foaf:name` must appear at least
once and be a string).

`@unrdf/core` provides two SHACL utilities:

- **`ShapeBuilder`** — a fluent API for building SHACL shapes in JavaScript without writing
  raw Turtle.
- **`createValidator` / `validateGraph`** — runs rdf-validate-shacl against a data graph and
  returns a structured report.

## Step 1 — Import the SHACL utilities

```javascript
// validate-people.mjs
import {
  createUnrdfStore,
  namedNode,
  literal,
  quad,
  RDF,
  shacl,
  createValidator,
  validateGraph,
} from '@unrdf/core';
```

## Step 2 — Define a shape with ShapeBuilder

The `shacl()` factory returns a `ShapeBuilder`. Call `.targetClass()` to declare which
resources the shape applies to, then use `.property()` to open a `PropertyBuilder` for each
property constraint.

```javascript
// Define a shape: every foaf:Person must have exactly one foaf:name (xsd:string)
// and an optional foaf:age that, if present, must be a non-negative integer.
const shape = shacl('http://example.org/PersonShape')
  .targetClass('http://xmlns.com/foaf/0.1/Person')
  .property('http://xmlns.com/foaf/0.1/name')
  .minCount(1)
  .maxCount(1)
  .datatype('xsd:string')
  .property('http://xmlns.com/foaf/0.1/age')
  .datatype('xsd:integer')
  .minInclusive(0)
  .build(); // returns an array of RDF quads
```

`shape` is now an array of quads that encodes the SHACL constraints in RDF.

## Step 3 — Create the data graph

```javascript
// Conforming person: has exactly one name
const alice = [
  quad(
    namedNode('http://example.org/alice'),
    namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    namedNode('http://xmlns.com/foaf/0.1/Person')
  ),
  quad(
    namedNode('http://example.org/alice'),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal('Alice')
  ),
];

// Non-conforming person: missing the required foaf:name
const bob = [
  quad(
    namedNode('http://example.org/bob'),
    namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    namedNode('http://xmlns.com/foaf/0.1/Person')
  ),
  // foaf:name is absent — this violates the minCount(1) constraint
];

const dataQuads = [...alice, ...bob];
```

## Step 4 — Create a validator and validate

`createValidator` accepts the shape as a Turtle string, a quad array, or an Oxigraph store.
Here we pass the quad array built by `ShapeBuilder.build()`.

```javascript
const validator = await createValidator(shape);
const report = await validateGraph(dataQuads, shape);
```

## Step 5 — Read the report

The report object follows `ValidationReportSchema`:

```javascript
console.log('Conforms:', report.conforms);
// Conforms: false

for (const result of report.results) {
  console.log('Violation:', result.message);
  console.log('  Focus node:', result.focusNode);
  console.log('  Property path:', result.path);
}
// Violation: Required property missing
//   Focus node: http://example.org/bob
//   Property path: http://xmlns.com/foaf/0.1/name
```

A conforming graph produces `report.conforms === true` and `report.results === []`.

## Step 6 — Try the Turtle shorthand

You can also pass a Turtle string directly to `createValidator` if you prefer to write SHACL
in its native syntax:

```javascript
const shapesTurtle = `
  @prefix sh:   <http://www.w3.org/ns/shacl#> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  @prefix ex:   <http://example.org/> .

  ex:PersonShape a sh:NodeShape ;
    sh:targetClass foaf:Person ;
    sh:property [
      sh:path foaf:name ;
      sh:minCount 1 ;
      sh:maxCount 1 ;
      sh:datatype xsd:string ;
    ] .
`;

const validatorFromTurtle = await createValidator(shapesTurtle);
```

## Complete working script

```javascript
// validate-people.mjs
import { namedNode, literal, quad, shacl, validateGraph } from '@unrdf/core';

// 1. Define shape
const shape = shacl('http://example.org/PersonShape')
  .targetClass('http://xmlns.com/foaf/0.1/Person')
  .property('http://xmlns.com/foaf/0.1/name')
  .minCount(1)
  .maxCount(1)
  .datatype('xsd:string')
  .build();

// 2. Build data
const data = [
  // Alice: conforming
  quad(
    namedNode('http://example.org/alice'),
    namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    namedNode('http://xmlns.com/foaf/0.1/Person')
  ),
  quad(
    namedNode('http://example.org/alice'),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal('Alice')
  ),
  // Bob: missing name — non-conforming
  quad(
    namedNode('http://example.org/bob'),
    namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    namedNode('http://xmlns.com/foaf/0.1/Person')
  ),
];

// 3. Validate
const report = await validateGraph(data, shape);

console.log('Conforms:', report.conforms); // false
for (const v of report.results) {
  console.log('FAIL:', v.focusNode, '-', v.message);
}
// FAIL: http://example.org/bob - Required property missing
```

## Next steps

- Reference: [SHACL API](../reference/shacl-api.md) — `createValidator`, `validateGraph`,
  `ShapeBuilder`, `PropertyBuilder`, and `ConstraintType` in full detail.
- How-To: [Run SPARQL queries](../how-to/02-run-sparql-queries.md) — query the data graph
  before or after validation.
- Explanation: [RDF data model](../explanation/01-rdf-data-model.md) — why SHACL is expressed
  as RDF and what that means for your toolchain.
