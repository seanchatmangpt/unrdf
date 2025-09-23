# Utilities API Reference

unrdf provides comprehensive utility functions that cover the 80/20 "dark matter" of RDF operations. These utilities are designed to handle common patterns and edge cases that would otherwise require boilerplate code.

## Term Utilities

Utilities for RDF term manipulation and conversion.

### `asNamedNode(value)`
Convert a value to a NamedNode.

**Parameters:**
- `value` (string|Term): The value to convert

**Returns:** `NamedNode`

**Example:**
```javascript
import { asNamedNode } from 'unrdf';

const node = asNamedNode("http://example.org/foo");
```

### `asLiteral(value, datatype?, language?)`
Convert a value to a Literal.

**Parameters:**
- `value` (any): The literal value
- `datatype` (string, optional): The datatype IRI
- `language` (string, optional): The language tag

**Returns:** `Literal`

**Example:**
```javascript
import { asLiteral } from 'unrdf';

const lit = asLiteral("hello");
const typedLit = asLiteral(42, "http://www.w3.org/2001/XMLSchema#integer");
const langLit = asLiteral("hello", null, "en");
```

### `asBlankNode(id?)`
Convert a value to a BlankNode.

**Parameters:**
- `id` (string, optional): The blank node identifier

**Returns:** `BlankNode`

**Example:**
```javascript
import { asBlankNode } from 'unrdf';

const bnode = asBlankNode();
const namedBnode = asBlankNode("person1");
```

### `quadToJSON(quad)`
Convert a quad to a JSON representation.

**Parameters:**
- `quad` (Quad): The quad to convert

**Returns:** `Object` with properties:
- `subject`: Subject as JSON
- `predicate`: Predicate as JSON
- `object`: Object as JSON
- `graph`: Graph as JSON

**Example:**
```javascript
import { quadToJSON } from 'unrdf';

const json = quadToJSON(quad);
console.log(json.subject.value); // "http://example.org/person"
```

### `jsonToQuad(json)`
Convert a JSON representation to a quad.

**Parameters:**
- `json` (Object): The JSON representation

**Returns:** `Quad`

**Example:**
```javascript
import { jsonToQuad } from 'unrdf';

const quad = jsonToQuad({
  subject: { termType: "NamedNode", value: "http://example.org/person" },
  predicate: { termType: "NamedNode", value: "http://xmlns.com/foaf/0.1/name" },
  object: { termType: "Literal", value: "John Doe" },
  graph: { termType: "DefaultGraph", value: "" }
});
```

### `isIRI(term)`
Check if a term is an IRI (NamedNode).

**Parameters:**
- `term` (Term): The term to check

**Returns:** `boolean`

### `isLiteral(term)`
Check if a term is a Literal.

**Parameters:**
- `term` (Term): The term to check

**Returns:** `boolean`

### `isBlankNode(term)`
Check if a term is a BlankNode.

**Parameters:**
- `term` (Term): The term to check

**Returns:** `boolean`

### `isVariable(term)`
Check if a term is a Variable.

**Parameters:**
- `term` (Term): The term to check

**Returns:** `boolean`

### `getValue(term)`
Get the value of a term.

**Parameters:**
- `term` (Term): The term

**Returns:** `string|number|boolean`

### `getDatatype(term)`
Get the datatype of a literal term.

**Parameters:**
- `term` (Term): The literal term

**Returns:** `string|null`

### `getLanguage(term)`
Get the language of a literal term.

**Parameters:**
- `term` (Term): The literal term

**Returns:** `string|null`

### `toObject(term)`
Convert a term to a plain object.

**Parameters:**
- `term` (Term): The term to convert

**Returns:** `Object`

### `fromObject(obj)`
Convert a plain object to a term.

**Parameters:**
- `obj` (Object): The object to convert

**Returns:** `Term`

### `common(term1, term2)`
Find common properties between two terms.

**Parameters:**
- `term1` (Term): First term
- `term2` (Term): Second term

**Returns:** `Object` with common properties

## Graph Utilities

Utilities for querying and manipulating RDF stores.

### `pluck(store, predicate)`
Extract all objects for a given predicate.

**Parameters:**
- `store` (Store): The RDF store
- `predicate` (string|Term): The predicate

**Returns:** `Array<Term>` (objects)

**Example:**
```javascript
import { pluck } from 'unrdf';

const labels = pluck(store, "http://www.w3.org/2000/01/rdf-schema#label");
```

### `indexByPredicate(store, predicate)`
Create an index mapping subjects to objects for a predicate.

**Parameters:**
- `store` (Store): The RDF store
- `predicate` (string|Term): The predicate

**Returns:** `Map<Term, Term>` (subject to object mapping)

**Example:**
```javascript
import { indexByPredicate } from 'unrdf';

const labelMap = indexByPredicate(store, "http://www.w3.org/2000/01/rdf-schema#label");
```

### `getSubjectsByType(store, type)`
Get all subjects of a specific type.

**Parameters:**
- `store` (Store): The RDF store
- `type` (string|Term): The type

**Returns:** `Array<Term>` (subjects)

**Example:**
```javascript
import { getSubjectsByType } from 'unrdf';

const persons = getSubjectsByType(store, "http://xmlns.com/foaf/0.1/Person");
```

### `hasType(store, subject, type)`
Check if a subject has a specific type.

**Parameters:**
- `store` (Store): The RDF store
- `subject` (string|Term): The subject
- `type` (string|Term): The type

**Returns:** `boolean`

**Example:**
```javascript
import { hasType } from 'unrdf';

const isPerson = hasType(store, "http://example.org/foo", "http://xmlns.com/foaf/0.1/Person");
```

### `getOne(store, subject, predicate)`
Get a single object for a subject-predicate pair.

**Parameters:**
- `store` (Store): The RDF store
- `subject` (string|Term): The subject
- `predicate` (string|Term): The predicate

**Returns:** `Term|null`

**Example:**
```javascript
import { getOne } from 'unrdf';

const name = getOne(store, "http://example.org/foo", "http://xmlns.com/foaf/0.1/name");
```

### `getAll(store, subject, predicate)`
Get all objects for a subject-predicate pair.

**Parameters:**
- `store` (Store): The RDF store
- `subject` (string|Term): The subject
- `predicate` (string|Term): The predicate

**Returns:** `Array<Term>`

**Example:**
```javascript
import { getAll } from 'unrdf';

const names = getAll(store, "http://example.org/foo", "http://xmlns.com/foaf/0.1/name");
```

### `getSubjects(store, predicate, object)`
Get all subjects for a predicate-object pair.

**Parameters:**
- `store` (Store): The RDF store
- `predicate` (string|Term): The predicate
- `object` (string|Term): The object

**Returns:** `Array<Term>`

**Example:**
```javascript
import { getSubjects } from 'unrdf';

const subjects = getSubjects(store, "http://xmlns.com/foaf/0.1/name", "John Doe");
```

### `getPredicates(store, subject, object)`
Get all predicates for a subject-object pair.

**Parameters:**
- `store` (Store): The RDF store
- `subject` (string|Term): The subject
- `object` (string|Term): The object

**Returns:** `Array<Term>`

**Example:**
```javascript
import { getPredicates } from 'unrdf';

const predicates = getPredicates(store, "http://example.org/foo", "John Doe");
```

### `findByProperty(store, predicate, value)`
Find subjects by property value.

**Parameters:**
- `store` (Store): The RDF store
- `predicate` (string|Term): The predicate
- `value` (string|Term): The value to match

**Returns:** `Array<Term>`

**Example:**
```javascript
import { findByProperty } from 'unrdf';

const subjects = findByProperty(store, "http://xmlns.com/foaf/0.1/name", "John Doe");
```

### `getTypes(store, subject)`
Get all types for a subject.

**Parameters:**
- `store` (Store): The RDF store
- `subject` (string|Term): The subject

**Returns:** `Array<Term>`

**Example:**
```javascript
import { getTypes } from 'unrdf';

const types = getTypes(store, "http://example.org/foo");
```

### `getProperties(store, subject)`
Get all properties for a subject.

**Parameters:**
- `store` (Store): The RDF store
- `subject` (string|Term): The subject

**Returns:** `Array<Term>`

**Example:**
```javascript
import { getProperties } from 'unrdf';

const properties = getProperties(store, "http://example.org/foo");
```

### `getObjects(store, subject, predicate)`
Get all objects for a subject-predicate pair.

**Parameters:**
- `store` (Store): The RDF store
- `subject` (string|Term): The subject
- `predicate` (string|Term): The predicate

**Returns:** `Array<Term>`

**Example:**
```javascript
import { getObjects } from 'unrdf';

const objects = getObjects(store, "http://example.org/foo", "http://xmlns.com/foaf/0.1/name");
```

## Serialization Utilities

Utilities for debugging and inspecting RDF serializations.

### `debugTurtle(store)`
Serialize a store to Turtle for debugging.

**Parameters:**
- `store` (Store): The RDF store

**Returns:** `Promise<string>` (Turtle string)

**Example:**
```javascript
import { debugTurtle } from 'unrdf';

const turtle = await debugTurtle(store);
console.log(turtle);
```

### `debugNQuads(store)`
Serialize a store to N-Quads for debugging.

**Parameters:**
- `store` (Store): The RDF store

**Returns:** `Promise<string>` (N-Quads string)

**Example:**
```javascript
import { debugNQuads } from 'unrdf';

const nquads = await debugNQuads(store);
console.log(nquads);
```

### `debugJSONLD(store)`
Serialize a store to JSON-LD for debugging.

**Parameters:**
- `store` (Store): The RDF store

**Returns:** `Promise<Object>` (JSON-LD document)

**Example:**
```javascript
import { debugJSONLD } from 'unrdf';

const jsonld = await debugJSONLD(store);
console.log(JSON.stringify(jsonld, null, 2));
```

## Validation Utilities

Utilities for validating RDF terms, quads, and stores.

### `validateIRI(iri)`
Validate an IRI string.

**Parameters:**
- `iri` (string): The IRI to validate

**Returns:** `boolean`

**Example:**
```javascript
import { validateIRI } from 'unrdf';

const isValid = validateIRI("http://example.org/foo");
```

### `validateTerm(term)`
Validate an RDF term.

**Parameters:**
- `term` (Term): The term to validate

**Returns:** `Object` with properties:
- `valid`: Whether the term is valid
- `errors`: Array of error messages

**Example:**
```javascript
import { validateTerm } from 'unrdf';

const result = validateTerm(term);
if (!result.valid) {
  console.log(result.errors);
}
```

### `validateQuad(quad)`
Validate an RDF quad.

**Parameters:**
- `quad` (Quad): The quad to validate

**Returns:** `Object` with properties:
- `valid`: Whether the quad is valid
- `errors`: Array of error messages

**Example:**
```javascript
import { validateQuad } from 'unrdf';

const result = validateQuad(quad);
if (!result.valid) {
  console.log(result.errors);
}
```

### `validateStore(store)`
Validate an RDF store.

**Parameters:**
- `store` (Store): The store to validate

**Returns:** `Object` with properties:
- `valid`: Whether the store is valid
- `errors`: Array of error messages
- `stats`: Store statistics

**Example:**
```javascript
import { validateStore } from 'unrdf';

const result = validateStore(store);
if (!result.valid) {
  console.log(result.errors);
}
console.log(result.stats);
```

## ID Utilities

Utilities for generating and managing identifiers.

### `generateId(prefix?)`
Generate a unique identifier.

**Parameters:**
- `prefix` (string, optional): Prefix for the ID

**Returns:** `string`

**Example:**
```javascript
import { generateId } from 'unrdf';

const id = generateId("person");
// Returns something like "person_1234567890"
```

### `isValidId(id)`
Check if an ID is valid.

**Parameters:**
- `id` (string): The ID to check

**Returns:** `boolean`

### `normalizeId(id)`
Normalize an ID to a standard format.

**Parameters:**
- `id` (string): The ID to normalize

**Returns:** `string`

## IO Utilities

Utilities for input/output operations.

### `readFile(filePath)`
Read a file asynchronously.

**Parameters:**
- `filePath` (string): Path to the file

**Returns:** `Promise<string>` (file contents)

**Example:**
```javascript
import { readFile } from 'unrdf';

const content = await readFile('./data.ttl');
```

### `writeFile(filePath, content)`
Write content to a file asynchronously.

**Parameters:**
- `filePath` (string): Path to the file
- `content` (string): Content to write

**Returns:** `Promise<void>`

**Example:**
```javascript
import { writeFile } from 'unrdf';

await writeFile('./output.ttl', turtleContent);
```

### `fileExists(filePath)`
Check if a file exists.

**Parameters:**
- `filePath` (string): Path to the file

**Returns:** `Promise<boolean>`

### `listFiles(directory, pattern?)`
List files in a directory.

**Parameters:**
- `directory` (string): Directory path
- `pattern` (string, optional): File pattern (glob)

**Returns:** `Promise<Array<string>>`

## Quad Utilities

Utilities for working with RDF quads.

### `quadToString(quad)`
Convert a quad to a string representation.

**Parameters:**
- `quad` (Quad): The quad to convert

**Returns:** `string`

### `stringToQuad(str)`
Convert a string representation to a quad.

**Parameters:**
- `str` (string): The string representation

**Returns:** `Quad`

### `quadEquals(quad1, quad2)`
Check if two quads are equal.

**Parameters:**
- `quad1` (Quad): First quad
- `quad2` (Quad): Second quad

**Returns:** `boolean`

### `quadHash(quad)`
Generate a hash for a quad.

**Parameters:**
- `quad` (Quad): The quad

**Returns:** `string`

## Debug Utilities

Utilities for debugging RDF operations.

### `debugStore(store, options?)`
Debug information about a store.

**Parameters:**
- `store` (Store): The RDF store
- `options` (Object, optional): Debug options

**Returns:** `Object` with debug information

**Example:**
```javascript
import { debugStore } from 'unrdf';

const debug = debugStore(store, { verbose: true });
console.log(debug);
```

### `debugQuad(quad)`
Debug information about a quad.

**Parameters:**
- `quad` (Quad): The quad

**Returns:** `Object` with debug information

### `debugTerm(term)`
Debug information about a term.

**Parameters:**
- `term` (Term): The term

**Returns:** `Object` with debug information

### `traceOperation(operation, context?)`
Trace an operation for debugging.

**Parameters:**
- `operation` (Function): The operation to trace
- `context` (Object, optional): Context information

**Returns:** `any` (result of the operation)

## Namespace Utilities

Utilities for working with RDF namespaces.

### `getNamespace(iri)`
Extract the namespace from an IRI.

**Parameters:**
- `iri` (string): The IRI

**Returns:** `string`

### `getLocalName(iri)`
Extract the local name from an IRI.

**Parameters:**
- `iri` (string): The IRI

**Returns:** `string`

### `isInNamespace(iri, namespace)`
Check if an IRI is in a specific namespace.

**Parameters:**
- `iri` (string): The IRI
- `namespace` (string): The namespace

**Returns:** `boolean`

## Merge Utilities

Utilities for merging RDF data.

### `mergeStores(stores)`
Merge multiple stores into one.

**Parameters:**
- `stores` (Array<Store>): Array of stores to merge

**Returns:** `Store` (merged store)

### `mergeQuads(quads)`
Merge multiple quads, removing duplicates.

**Parameters:**
- `quads` (Array<Quad>): Array of quads to merge

**Returns:** `Array<Quad>` (merged quads)

## Quality Utilities

Utilities for assessing RDF data quality.

### `assessQuality(store)`
Assess the quality of RDF data.

**Parameters:**
- `store` (Store): The RDF store

**Returns:** `Object` with quality metrics

### `findIssues(store)`
Find potential issues in RDF data.

**Parameters:**
- `store` (Store): The RDF store

**Returns:** `Array<Object>` (list of issues)

## SPARQL Utilities

Utilities for working with SPARQL queries.

### `parseQuery(query)`
Parse a SPARQL query.

**Parameters:**
- `query` (string): The SPARQL query

**Returns:** `Object` (parsed query)

### `validateQuery(query)`
Validate a SPARQL query.

**Parameters:**
- `query` (string): The SPARQL query

**Returns:** `Object` with validation results

### `optimizeQuery(query)`
Optimize a SPARQL query.

**Parameters:**
- `query` (string): The SPARQL query

**Returns:** `string` (optimized query)

## Transform Utilities

Utilities for transforming RDF data.

### `transformStore(store, transformer)`
Transform a store using a transformer function.

**Parameters:**
- `store` (Store): The RDF store
- `transformer` (Function): The transformer function

**Returns:** `Store` (transformed store)

### `mapTerms(store, mapper)`
Map terms in a store using a mapper function.

**Parameters:**
- `store` (Store): The RDF store
- `mapper` (Function): The mapper function

**Returns:** `Store` (mapped store)

### `filterQuads(store, predicate)`
Filter quads in a store using a predicate function.

**Parameters:**
- `store` (Store): The RDF store
- `predicate` (Function): The predicate function

**Returns:** `Store` (filtered store)
