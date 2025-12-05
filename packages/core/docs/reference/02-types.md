# Types Reference: @unrdf/core

Complete type definitions for @unrdf/core with examples.

---

## Term Types

### NamedNode

Represents an IRI/URI.

```typescript
type NamedNode = {
  type: 'NamedNode'
  value: string
}
```

**Created by:**
```javascript
import { namedNode } from '@unrdf/core';
const person = namedNode('http://example.com/alice');
```

**Usage:**
- Subject in quads
- Predicate in quads
- Object in quads
- Graph identifier

---

### Literal

Represents a text or typed value.

```typescript
type Literal = {
  type: 'Literal'
  value: string
  datatype?: NamedNode
  language?: string
}
```

**Created by:**
```javascript
import { literal } from '@unrdf/core';

// Plain literal
const name = literal('Alice');

// Typed literal
import { XSD } from '@unrdf/core';
const age = literal('30', XSD.integer);

// Language-tagged literal
const greeting = literal('Bonjour', 'fr');
```

**Common Types:**
| Type | URI | Values |
|------|-----|--------|
| string | xsd:string | "text" |
| integer | xsd:integer | "123" |
| boolean | xsd:boolean | "true" |
| decimal | xsd:decimal | "3.14" |
| double | xsd:double | "3.14159e+0" |
| dateTime | xsd:dateTime | "2024-01-01T00:00:00Z" |
| date | xsd:date | "2024-01-01" |

---

### BlankNode

Represents an anonymous resource.

```typescript
type BlankNode = {
  type: 'BlankNode'
  value: string
}
```

**Created by:**
```javascript
import { blankNode } from '@unrdf/core';

const node = blankNode('person123');  // _:person123
const autoId = blankNode();           // _:b1234567
```

**When to use:**
- Resources without IRIs
- Intermediate results in CONSTRUCT queries
- Aggregated data structures

---

### Variable

Represents a SPARQL variable.

```typescript
type Variable = {
  type: 'Variable'
  value: string
}
```

**Created by:**
```javascript
import { variable } from '@unrdf/core';

const personVar = variable('person');
```

**Only used in:**
- Query construction
- Prepared queries

---

### DefaultGraph

Represents the default graph.

```typescript
type DefaultGraph = {
  type: 'DefaultGraph'
}
```

**Created by:**
```javascript
import { defaultGraph } from '@unrdf/core';

const dg = defaultGraph();
```

**Usage:**
- Graph property when not specified
- Implicit graph in named triples

---

## Quad Type

RDF statement (subject-predicate-object-graph).

```typescript
type Quad = {
  subject: NamedNode | BlankNode
  predicate: NamedNode
  object: NamedNode | Literal | BlankNode
  graph?: NamedNode | DefaultGraph
}
```

**Example:**
```javascript
const quad = {
  subject: namedNode('http://example.com/alice'),
  predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
  object: literal('Alice'),
  graph: defaultGraph()
};
```

---

## Store Types

### UnrdfStore

Synchronous RDF store.

```typescript
interface UnrdfStore {
  addQuad(quad: Quad): void
  removeQuad(quad: Quad): boolean
  match(s?: Term, p?: Term, o?: Term, g?: Term): Quad[]
  size: number
}
```

**Example:**
```javascript
import { createUnrdfStore } from '@unrdf/core';

const store = createUnrdfStore();
store.addQuad(quad);
const matches = store.match(subject);
```

### AsyncStore

Async version of UnrdfStore (legacy API).

```typescript
interface AsyncStore {
  addQuad(quad: Quad): Promise<void>
  removeQuad(quad: Quad): Promise<boolean>
  // ... other async methods
}
```

---

## Binding Type

Result of SPARQL SELECT query.

```typescript
type Binding = {
  [variableName: string]: Term
}
```

**Example:**
```javascript
// Query: SELECT ?name ?age
// Result:
[
  {
    name: { type: 'Literal', value: 'Alice' },
    age: { type: 'Literal', value: '30' }
  },
  {
    name: { type: 'Literal', value: 'Bob' },
    age: { type: 'Literal', value: '25' }
  }
]
```

**Access:**
```javascript
results.forEach(binding => {
  const name = binding.name.value;
  const age = binding.age.value;
});
```

---

## Option Types

### UnrdfStoreOptions

Options for creating a store.

```typescript
interface UnrdfStoreOptions {
  indexing?: 'predicate' | 'object' | 'none'
  maxSize?: number
  autoGC?: boolean
}
```

**Example:**
```javascript
const store = createUnrdfStore({
  indexing: 'predicate',
  maxSize: 1000000,
  autoGC: true
});
```

### QueryOptions

Options for executing queries.

```typescript
interface QueryOptions {
  timeout?: number  // milliseconds
  base?: string     // Base IRI for relative references
}
```

**Example:**
```javascript
const results = executeQuerySync(store, query, {
  timeout: 5000,  // 5 seconds
  base: 'http://example.com/'
});
```

---

## Prepared Query Type

Pre-compiled query for reuse.

```typescript
interface PreparedQuery {
  execute(store: UnrdfStore): Binding[]
  variables: string[]
}
```

**Example:**
```javascript
const prepared = prepareQuerySync(`
  SELECT ?name WHERE { ?x foaf:name ?name }
`);

console.log(prepared.variables);  // ['name']

const results = prepared.execute(store);
```

---

## Serialization Types

### Term Union

Any single term.

```typescript
type Term = NamedNode | BlankNode | Literal | Variable | DefaultGraph
```

### RDF Term

Term that can be in an RDF triple (no variables).

```typescript
type RDFTerm = NamedNode | BlankNode | Literal
```

### Subject Term

Valid subject (can't be literal).

```typescript
type SubjectTerm = NamedNode | BlankNode
```

### Predicate Term

Must be NamedNode.

```typescript
type PredicateTerm = NamedNode
```

### Object Term

Any RDF term.

```typescript
type ObjectTerm = NamedNode | BlankNode | Literal
```

---

## Type Guards

Check term types:

```javascript
function isNamedNode(term) {
  return term && term.type === 'NamedNode';
}

function isLiteral(term) {
  return term && term.type === 'Literal';
}

function isBlankNode(term) {
  return term && term.type === 'BlankNode';
}

// Usage
if (isNamedNode(quad.object)) {
  const iri = quad.object.value;
}
```

---

## Namespace Types

Convenience objects for common vocabularies.

```typescript
const RDF: NamespaceObject
const RDFS: NamespaceObject
const OWL: NamespaceObject
const XSD: NamespaceObject
const FOAF: NamespaceObject
const DCTERMS: NamespaceObject
const SKOS: NamespaceObject
```

**Example:**
```javascript
import { RDF, FOAF, XSD } from '@unrdf/core';

RDF.type           // NamedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type')
FOAF.name          // NamedNode('http://xmlns.com/foaf/0.1/name')
XSD.integer        // NamedNode('http://www.w3.org/2001/XMLSchema#integer')
```

---

## Common Type Patterns

```javascript
// Type check
function validateTerm(term) {
  if (!term || !term.type) throw new Error('Invalid term');
  if (!['NamedNode', 'BlankNode', 'Literal', 'Variable', 'DefaultGraph'].includes(term.type)) {
    throw new Error('Unknown term type');
  }
}

// Build quad safely
function buildQuad(subject, predicate, object) {
  return {
    subject: validateSubject(subject),
    predicate: validatePredicate(predicate),
    object: validateObject(object)
  };
}

// Extract values
function extractValue(term) {
  if (term.type === 'Literal') {
    return term.value;
  }
  if (term.type === 'NamedNode') {
    return term.value;
  }
  return null;
}
```

---

## Next Reading

- **API.md** (Reference) - Function signatures
- **CONFIGURATION.md** (Reference) - Configuration options
- **architecture** (Explanation) - How types fit into the system
