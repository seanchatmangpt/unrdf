# DataFactory API Reference

**Version**: 5.0.0+
**Package**: `@unrdf/core`, `@unrdf/oxigraph`
**Stability**: Stable
**Standard**: [RDF/JS Data Model Specification](https://rdf.js.org/data-model-spec/)

---

## Overview

UNRDF provides RDF term constructors via the `dataFactory` object from `@unrdf/oxigraph`. These factories create RDF/JS-compliant terms (NamedNode, Literal, BlankNode, etc.) for use in quad operations.

**Import Options**:

```javascript
// Option 1: Individual imports (RECOMMENDED)
import { namedNode, literal, blankNode } from '@unrdf/core';

// Option 2: DataFactory object
import { dataFactory } from '@unrdf/oxigraph';
const { namedNode, literal } = dataFactory;

// Option 3: Legacy N3 (DEPRECATED)
import { UnrdfDataFactory } from '@unrdf/core/rdf/n3-justified-only';
```

---

## Table of Contents

1. [namedNode](#namednode)
2. [literal](#literal)
3. [blankNode](#blanknode)
4. [variable](#variable)
5. [defaultGraph](#defaultgraph)
6. [quad](#quad)
7. [triple](#triple)
8. [Term Types](#term-types)
9. [Migration Guide](#migration-guide)

---

## namedNode

Create a Named Node (IRI/URI reference).

**Signature**:
```javascript
function namedNode(iri: string): NamedNode
```

**Parameters**:

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `iri` | `string` | Yes | Full IRI (not relative) |

**Returns**: `NamedNode` object

**Properties**:
- `termType`: `'NamedNode'`
- `value`: IRI string
- `equals(other)`: Equality check

**Example**:
```javascript
import { namedNode } from '@unrdf/core';

const alice = namedNode('http://example.org/alice');
console.log(alice.termType); // 'NamedNode'
console.log(alice.value); // 'http://example.org/alice'

const foafName = namedNode('http://xmlns.com/foaf/0.1/name');
console.log(foafName.value); // 'http://xmlns.com/foaf/0.1/name'
```

**Validation**:
```javascript
// ✅ Valid
namedNode('http://example.org/resource');
namedNode('https://example.org/resource');
namedNode('urn:isbn:0-123-45678-9');

// ❌ Invalid (will still create but violates RDF spec)
namedNode('example'); // Relative IRI
namedNode('http://example.org/spaces are bad');
```

**Version**: 5.0.0 | **Stability**: Stable

---

## literal

Create a Literal value (string, number, date, etc.).

**Signature**:
```javascript
function literal(
  value: string | number | boolean,
  languageOrDatatype?: string | NamedNode
): Literal
```

**Parameters**:

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `value` | `string \| number \| boolean` | Yes | Literal value |
| `languageOrDatatype` | `string \| NamedNode` | No | Language tag OR datatype IRI |

**Returns**: `Literal` object

**Properties**:
- `termType`: `'Literal'`
- `value`: String representation
- `language`: Language tag (if any)
- `datatype`: Datatype NamedNode
- `equals(other)`: Equality check

**Examples**:

### Plain Literal
```javascript
import { literal } from '@unrdf/core';

const name = literal('Alice');
console.log(name.termType); // 'Literal'
console.log(name.value); // 'Alice'
console.log(name.datatype.value); // 'http://www.w3.org/2001/XMLSchema#string'
```

### Language-Tagged Literal
```javascript
import { literal } from '@unrdf/core';

const greeting = literal('Bonjour', 'fr');
console.log(greeting.value); // 'Bonjour'
console.log(greeting.language); // 'fr'
console.log(greeting.datatype.value); // 'http://www.w3.org/1999/02/22-rdf-syntax-ns#langString'

const hello = literal('Hello', 'en-US');
console.log(hello.language); // 'en-US'
```

### Typed Literal
```javascript
import { literal, namedNode } from '@unrdf/core';

// Integer
const age = literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer'));
console.log(age.datatype.value); // 'http://www.w3.org/2001/XMLSchema#integer'

// Boolean
const active = literal('true', namedNode('http://www.w3.org/2001/XMLSchema#boolean'));
console.log(active.value); // 'true'

// Date
const birthdate = literal('1990-01-01', namedNode('http://www.w3.org/2001/XMLSchema#date'));
console.log(birthdate.value); // '1990-01-01'
```

### Numeric Literals
```javascript
import { literal } from '@unrdf/core';

const count = literal(42);
console.log(count.value); // '42'
console.log(count.datatype.value); // 'http://www.w3.org/2001/XMLSchema#integer'

const price = literal(19.99);
console.log(price.value); // '19.99'
console.log(price.datatype.value); // 'http://www.w3.org/2001/XMLSchema#decimal'
```

**Common Datatypes**:

| Type | IRI |
|------|-----|
| String | `http://www.w3.org/2001/XMLSchema#string` |
| Integer | `http://www.w3.org/2001/XMLSchema#integer` |
| Decimal | `http://www.w3.org/2001/XMLSchema#decimal` |
| Boolean | `http://www.w3.org/2001/XMLSchema#boolean` |
| Date | `http://www.w3.org/2001/XMLSchema#date` |
| DateTime | `http://www.w3.org/2001/XMLSchema#dateTime` |

**Version**: 5.0.0 | **Stability**: Stable

---

## blankNode

Create a Blank Node (anonymous resource).

**Signature**:
```javascript
function blankNode(label?: string): BlankNode
```

**Parameters**:

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `label` | `string` | No | Blank node identifier (auto-generated if omitted) |

**Returns**: `BlankNode` object

**Properties**:
- `termType`: `'BlankNode'`
- `value`: Blank node label (e.g., `'b0'`, `'b1'`)
- `equals(other)`: Equality check

**Examples**:

### Auto-Generated Label
```javascript
import { blankNode } from '@unrdf/core';

const blank1 = blankNode();
const blank2 = blankNode();

console.log(blank1.value); // 'b0' (or similar)
console.log(blank2.value); // 'b1'
console.log(blank1.equals(blank2)); // false
```

### Named Blank Node
```javascript
import { blankNode } from '@unrdf/core';

const person = blankNode('person1');
console.log(person.value); // 'person1'

const address = blankNode('addr');
console.log(address.value); // 'addr'
```

**Use Cases**:
- Intermediate resources without IRIs
- Complex nested structures
- N-ary relationships

**Example (Blank Node for Address)**:
```javascript
import { blankNode, namedNode, literal, createStore } from '@unrdf/core';

const store = createStore();
const alice = namedNode('http://example.org/alice');
const address = blankNode('addr');

// Alice has address (blank node)
store.add(alice, namedNode('http://example.org/hasAddress'), address);

// Address properties
store.add(address, namedNode('http://example.org/street'), literal('123 Main St'));
store.add(address, namedNode('http://example.org/city'), literal('Springfield'));
store.add(address, namedNode('http://example.org/zip'), literal('12345'));
```

**Version**: 5.0.0 | **Stability**: Stable

---

## variable

Create a SPARQL variable.

**Signature**:
```javascript
function variable(name: string): Variable
```

**Parameters**:

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `name` | `string` | Yes | Variable name (without `?` prefix) |

**Returns**: `Variable` object

**Properties**:
- `termType`: `'Variable'`
- `value`: Variable name

**Example**:
```javascript
import { variable } from '@unrdf/core';

const subject = variable('s');
const predicate = variable('p');
const object = variable('o');

console.log(subject.value); // 's'
console.log(subject.termType); // 'Variable'
```

**Note**: Variables are primarily for internal SPARQL processing. Application code rarely needs to create Variable objects directly.

**Version**: 5.0.0 | **Stability**: Stable

---

## defaultGraph

Create a Default Graph term.

**Signature**:
```javascript
function defaultGraph(): DefaultGraph
```

**Returns**: `DefaultGraph` object (singleton)

**Properties**:
- `termType`: `'DefaultGraph'`
- `value`: `''` (empty string)

**Example**:
```javascript
import { defaultGraph, quad, namedNode, literal } from '@unrdf/core';

const q = quad(
  namedNode('http://example.org/alice'),
  namedNode('http://xmlns.com/foaf/0.1/name'),
  literal('Alice'),
  defaultGraph() // Explicitly in default graph
);

console.log(q.graph.termType); // 'DefaultGraph'
console.log(q.graph.value); // ''
```

**Note**: Default graph is implicit. Most quad operations use default graph if graph parameter is omitted.

**Version**: 5.0.0 | **Stability**: Stable

---

## quad

Create a Quad (RDF statement with optional graph).

**Signature**:
```javascript
function quad(
  subject: NamedNode | BlankNode,
  predicate: NamedNode,
  object: Term,
  graph?: NamedNode | BlankNode | DefaultGraph
): Quad
```

**Parameters**:

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `subject` | `NamedNode \| BlankNode` | Yes | Subject term |
| `predicate` | `NamedNode` | Yes | Predicate term |
| `object` | `Term` | Yes | Object term (any RDF term) |
| `graph` | `NamedNode \| BlankNode \| DefaultGraph` | No | Graph term (default graph if omitted) |

**Returns**: `Quad` object

**Properties**:
- `subject`: Subject term
- `predicate`: Predicate term
- `object`: Object term
- `graph`: Graph term
- `equals(other)`: Equality check

**Examples**:

### Simple Triple
```javascript
import { quad, namedNode, literal } from '@unrdf/core';

const triple = quad(
  namedNode('http://example.org/alice'),
  namedNode('http://xmlns.com/foaf/0.1/name'),
  literal('Alice')
);

console.log(triple.subject.value); // 'http://example.org/alice'
console.log(triple.predicate.value); // 'http://xmlns.com/foaf/0.1/name'
console.log(triple.object.value); // 'Alice'
console.log(triple.graph.termType); // 'DefaultGraph'
```

### Quad with Named Graph
```javascript
import { quad, namedNode, literal } from '@unrdf/core';

const q = quad(
  namedNode('http://example.org/alice'),
  namedNode('http://xmlns.com/foaf/0.1/age'),
  literal(30),
  namedNode('http://example.org/graph/users')
);

console.log(q.graph.value); // 'http://example.org/graph/users'
```

### Quad with Blank Node
```javascript
import { quad, namedNode, blankNode, literal } from '@unrdf/core';

const person = blankNode('person1');

const q = quad(
  person,
  namedNode('http://xmlns.com/foaf/0.1/name'),
  literal('Alice')
);

console.log(q.subject.termType); // 'BlankNode'
```

**Version**: 5.0.0 | **Stability**: Stable

---

## triple

Create a Triple (shorthand for quad with default graph).

**Signature**:
```javascript
function triple(
  subject: NamedNode | BlankNode,
  predicate: NamedNode,
  object: Term
): Quad
```

**Example**:
```javascript
import { triple, namedNode, literal } from '@unrdf/core';

const t = triple(
  namedNode('http://example.org/alice'),
  namedNode('http://xmlns.com/foaf/0.1/name'),
  literal('Alice')
);

// Equivalent to:
// quad(subject, predicate, object, defaultGraph())
```

**Version**: 5.0.0 | **Stability**: Stable

---

## Term Types

### Type Hierarchy

```
Term (base interface)
├── NamedNode (IRI/URI)
├── BlankNode (anonymous resource)
├── Literal (value with optional language/datatype)
├── Variable (SPARQL variable)
└── DefaultGraph (default graph singleton)
```

### Common Properties

All terms have:
- `termType`: String identifying term type
- `value`: String representation
- `equals(other)`: Equality comparison

### Type Guards

```javascript
function isNamedNode(term) {
  return term.termType === 'NamedNode';
}

function isLiteral(term) {
  return term.termType === 'Literal';
}

function isBlankNode(term) {
  return term.termType === 'BlankNode';
}
```

---

## Migration Guide

### From N3 DataFactory

**Before (N3)**:
```javascript
import { DataFactory } from 'n3';

const subject = DataFactory.namedNode('http://example.org/alice');
const object = DataFactory.literal('Alice');
```

**After (UNRDF v5)**:
```javascript
import { namedNode, literal } from '@unrdf/core';

const subject = namedNode('http://example.org/alice');
const object = literal('Alice');
```

### From @rdfjs/data-model

**Before (@rdfjs/data-model)**:
```javascript
import { namedNode, literal } from '@rdfjs/data-model';

const subject = namedNode('http://example.org/alice');
```

**After (UNRDF v5)**:
```javascript
import { namedNode, literal } from '@unrdf/core';

const subject = namedNode('http://example.org/alice');
```

**Note**: Both use the same RDF/JS standard, so API is identical. Only import path changes.

---

## Best Practices

### 1. Use Constants for Common IRIs

```javascript
import { namedNode } from '@unrdf/core';
import { RDF, FOAF } from '@unrdf/core';

// ❌ Repetitive
const type1 = namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
const type2 = namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');

// ✅ Use constants
const type1 = RDF.type;
const type2 = RDF.type;
```

### 2. Create Factories for Domain Models

```javascript
import { namedNode, literal } from '@unrdf/core';

const EX = 'http://example.org/';

function createPerson(id, name, age) {
  return {
    subject: namedNode(`${EX}${id}`),
    quads: [
      quad(namedNode(`${EX}${id}`), RDF.type, namedNode(`${EX}Person`)),
      quad(namedNode(`${EX}${id}`), FOAF.name, literal(name)),
      quad(namedNode(`${EX}${id}`), namedNode(`${EX}age`), literal(age))
    ]
  };
}

const alice = createPerson('alice', 'Alice', 30);
```

### 3. Validate IRIs Before Creating NamedNodes

```javascript
import { namedNode } from '@unrdf/core';

function safeNamedNode(iri) {
  if (!iri.match(/^https?:\/\//)) {
    throw new Error(`Invalid IRI: ${iri}`);
  }
  return namedNode(iri);
}
```

---

## Related Documentation

- [Core API](/docs/reference/core-rdf-api.md)
- [Store API](/docs/reference/oxigraph-store-api.md)
- [RDF/JS Spec](https://rdf.js.org/data-model-spec/)
- [Migration Guide](/docs/V5-MIGRATION-GUIDE.md)

---

**Document Version**: 1.0.0
**Last Updated**: 2025-12-25
**Maintainer**: UNRDF Team
