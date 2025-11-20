# Utilities API Reference

UNRDF provides utility functions for common RDF operations, error handling, validation, and debugging.

## Parsing & Serialization

### `parseTurtle(ttl, options)`

Parse Turtle string into RDF quads.

**Parameters:**
- `ttl` (string): Turtle/TriG string (required)
- `options` (Object, optional): Parse options
  - `baseIRI` (string): Base IRI for resolving relative URIs
  - `blankNodePrefix` (string): Prefix for blank node identifiers
  - `format` (string): Format ('Turtle', 'TriG', 'N-Triples', 'N-Quads')

**Returns:** `Store` - N3 Store containing parsed quads

**Throws:** `Error` - If parsing fails

**Example:**
```javascript
import { parseTurtle } from 'unrdf/utils';

const ttl = `
  @prefix ex: <http://example.org/> .
  ex:alice ex:knows ex:bob .
`;

const store = parseTurtle(ttl, {
  baseIRI: 'http://example.org/'
});

console.log('Parsed', store.size, 'quads');
```

---

### `serializeTurtle(store, options)`

Serialize RDF quads to Turtle string.

**Parameters:**
- `store` (Store|Array): N3 Store or array of quads (required)
- `options` (Object, optional): Serialization options
  - `prefixes` (Object): Namespace prefix mappings
  - `format` (string): Output format ('Turtle', 'TriG', 'N-Triples', 'N-Quads')

**Returns:** `string` - Serialized Turtle/TriG string

**Example:**
```javascript
import { serializeTurtle } from 'unrdf/utils';

const turtle = serializeTurtle(store, {
  prefixes: {
    ex: 'http://example.org/',
    foaf: 'http://xmlns.com/foaf/0.1/'
  },
  format: 'Turtle'
});

console.log(turtle);
```

**Output:**
```turtle
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:alice foaf:knows ex:bob .
```

---

### `parseNQuads(nquads, options)`

Parse N-Quads string into RDF quads.

**Parameters:**
- `nquads` (string): N-Quads string (required)
- `options` (Object, optional): Parse options

**Returns:** `Store` - N3 Store containing parsed quads

**Example:**
```javascript
import { parseNQuads } from 'unrdf/utils';

const nquads = `
  <http://example.org/alice> <http://xmlns.com/foaf/0.1/knows> <http://example.org/bob> .
`;

const store = parseNQuads(nquads);
```

---

### `serializeNQuads(store)`

Serialize RDF quads to N-Quads string.

**Parameters:**
- `store` (Store|Array): N3 Store or array of quads (required)

**Returns:** `string` - Serialized N-Quads string

**Example:**
```javascript
import { serializeNQuads } from 'unrdf/utils';

const nquads = serializeNQuads(store);
console.log(nquads);
```

---

## Validation

### `validateQuad(quad)`

Validate that an object is a valid RDF quad.

**Parameters:**
- `quad` (Object): Object to validate (required)

**Returns:** `Object` - Validation result
- `valid` (boolean): Is valid quad
- `errors` (Array<string>): Validation errors

**Example:**
```javascript
import { validateQuad } from 'unrdf/utils';
import { DataFactory } from 'n3';

const { namedNode, literal, quad } = DataFactory;

const q = quad(
  namedNode('http://example.org/alice'),
  namedNode('http://xmlns.com/foaf/0.1/name'),
  literal('Alice')
);

const result = validateQuad(q);
console.log('Valid:', result.valid);
console.log('Errors:', result.errors);
```

---

### `validateStore(store)`

Validate that a store is properly formatted.

**Parameters:**
- `store` (Store): N3 Store to validate (required)

**Returns:** `Object` - Validation result
- `valid` (boolean): Is valid store
- `errors` (Array<string>): Validation errors
- `warnings` (Array<string>): Validation warnings
- `stats` (Object): Store statistics

**Example:**
```javascript
import { validateStore } from 'unrdf/utils';

const result = validateStore(store);

if (!result.valid) {
  console.error('Invalid store:', result.errors);
} else {
  console.log('Valid store with', result.stats.quads, 'quads');
}
```

---

### `validateTurtle(ttl)`

Validate Turtle syntax without parsing.

**Parameters:**
- `ttl` (string): Turtle string to validate (required)

**Returns:** `Object` - Validation result
- `valid` (boolean): Is valid Turtle
- `errors` (Array<Object>): Syntax errors with line/column info

**Example:**
```javascript
import { validateTurtle } from 'unrdf/utils';

const ttl = `
  @prefix ex: <http://example.org/> .
  ex:alice ex:knows ex:bob .
`;

const result = validateTurtle(ttl);

if (!result.valid) {
  result.errors.forEach(err => {
    console.error(`Line ${err.line}, column ${err.column}: ${err.message}`);
  });
}
```

---

## Term Utilities

### `isNamedNode(term)`

Check if a term is a named node.

**Parameters:**
- `term` (Term): RDF term to check (required)

**Returns:** `boolean` - True if named node

**Example:**
```javascript
import { isNamedNode } from 'unrdf/utils';
import { DataFactory } from 'n3';

const { namedNode, literal } = DataFactory;

console.log(isNamedNode(namedNode('http://example.org/alice'))); // true
console.log(isNamedNode(literal('Alice'))); // false
```

---

### `isLiteral(term)`

Check if a term is a literal.

**Parameters:**
- `term` (Term): RDF term to check (required)

**Returns:** `boolean` - True if literal

**Example:**
```javascript
import { isLiteral } from 'unrdf/utils';

console.log(isLiteral(literal('Alice'))); // true
console.log(isLiteral(namedNode('http://example.org/alice'))); // false
```

---

### `isBlankNode(term)`

Check if a term is a blank node.

**Parameters:**
- `term` (Term): RDF term to check (required)

**Returns:** `boolean` - True if blank node

**Example:**
```javascript
import { isBlankNode } from 'unrdf/utils';
import { DataFactory } from 'n3';

const { blankNode } = DataFactory;

console.log(isBlankNode(blankNode('b1'))); // true
```

---

### `termEquals(term1, term2)`

Check if two terms are equal.

**Parameters:**
- `term1` (Term): First term (required)
- `term2` (Term): Second term (required)

**Returns:** `boolean` - True if terms are equal

**Example:**
```javascript
import { termEquals } from 'unrdf/utils';
import { DataFactory } from 'n3';

const { namedNode } = DataFactory;

const alice1 = namedNode('http://example.org/alice');
const alice2 = namedNode('http://example.org/alice');
const bob = namedNode('http://example.org/bob');

console.log(termEquals(alice1, alice2)); // true
console.log(termEquals(alice1, bob)); // false
```

---

### `getLiteralValue(literal)`

Get the string value of a literal.

**Parameters:**
- `literal` (Literal): Literal term (required)

**Returns:** `string` - Literal value

**Example:**
```javascript
import { getLiteralValue } from 'unrdf/utils';
import { DataFactory } from 'n3';

const { literal } = DataFactory;

const name = literal('Alice');
console.log(getLiteralValue(name)); // "Alice"

const age = literal('30', 'http://www.w3.org/2001/XMLSchema#integer');
console.log(getLiteralValue(age)); // "30"
```

---

### `getLiteralDatatype(literal)`

Get the datatype IRI of a literal.

**Parameters:**
- `literal` (Literal): Literal term (required)

**Returns:** `string` - Datatype IRI

**Example:**
```javascript
import { getLiteralDatatype } from 'unrdf/utils';

const age = literal('30', 'http://www.w3.org/2001/XMLSchema#integer');
console.log(getLiteralDatatype(age));
// => "http://www.w3.org/2001/XMLSchema#integer"
```

---

## Quad Utilities

### `quadEquals(quad1, quad2)`

Check if two quads are equal.

**Parameters:**
- `quad1` (Quad): First quad (required)
- `quad2` (Quad): Second quad (required)

**Returns:** `boolean` - True if quads are equal

**Example:**
```javascript
import { quadEquals } from 'unrdf/utils';

const q1 = quad(alice, knows, bob);
const q2 = quad(alice, knows, bob);

console.log(quadEquals(q1, q2)); // true
```

---

### `cloneQuad(quad)`

Create a deep copy of a quad.

**Parameters:**
- `quad` (Quad): Quad to clone (required)

**Returns:** `Quad` - Cloned quad

**Example:**
```javascript
import { cloneQuad } from 'unrdf/utils';

const original = quad(alice, knows, bob);
const copy = cloneQuad(original);

console.log(quadEquals(original, copy)); // true
console.log(original === copy); // false
```

---

### `matchesPattern(quad, pattern)`

Check if a quad matches a pattern.

**Parameters:**
- `quad` (Quad): Quad to check (required)
- `pattern` (Object): Pattern to match
  - `subject` (Term|null): Subject pattern
  - `predicate` (Term|null): Predicate pattern
  - `object` (Term|null): Object pattern
  - `graph` (Term|null): Graph pattern

**Returns:** `boolean` - True if quad matches pattern

**Example:**
```javascript
import { matchesPattern } from 'unrdf/utils';

const q = quad(alice, knows, bob);

// Match specific predicate
const matches = matchesPattern(q, {
  subject: null,
  predicate: knows,
  object: null,
  graph: null
});

console.log('Matches:', matches); // true
```

---

## Store Utilities

### `mergeStores(...stores)`

Merge multiple stores into one.

**Parameters:**
- `...stores` (Store[]): Stores to merge (required)

**Returns:** `Store` - New store containing all quads

**Example:**
```javascript
import { mergeStores } from 'unrdf/utils';

const store1 = new Store([quad1, quad2]);
const store2 = new Store([quad3, quad4]);

const merged = mergeStores(store1, store2);
console.log('Merged size:', merged.size);
```

---

### `cloneStore(store)`

Create a deep copy of a store.

**Parameters:**
- `store` (Store): Store to clone (required)

**Returns:** `Store` - Cloned store

**Example:**
```javascript
import { cloneStore } from 'unrdf/utils';

const original = new Store([quad1, quad2]);
const copy = cloneStore(original);

console.log('Copy size:', copy.size);
console.log('Same reference:', original === copy); // false
```

---

### `filterStore(store, predicate)`

Filter a store based on a predicate function.

**Parameters:**
- `store` (Store): Store to filter (required)
- `predicate` (Function): Filter function `(quad) => boolean` (required)

**Returns:** `Store` - New store with filtered quads

**Example:**
```javascript
import { filterStore } from 'unrdf/utils';

const filtered = filterStore(store, quad => {
  return quad.predicate.value === 'http://xmlns.com/foaf/0.1/knows';
});

console.log('Filtered size:', filtered.size);
```

---

### `mapStore(store, mapper)`

Map quads in a store using a mapper function.

**Parameters:**
- `store` (Store): Store to map (required)
- `mapper` (Function): Map function `(quad) => quad` (required)

**Returns:** `Store` - New store with mapped quads

**Example:**
```javascript
import { mapStore } from 'unrdf/utils';

// Replace all blank nodes with named nodes
const mapped = mapStore(store, quad => {
  if (quad.subject.termType === 'BlankNode') {
    return quad({
      subject: namedNode(`http://example.org/${quad.subject.value}`),
      predicate: quad.predicate,
      object: quad.object,
      graph: quad.graph
    });
  }
  return quad;
});
```

---

## Prefix Utilities

### `expandPrefix(prefixed, prefixes)`

Expand a prefixed URI to full IRI.

**Parameters:**
- `prefixed` (string): Prefixed URI (e.g., 'ex:alice') (required)
- `prefixes` (Object): Prefix mappings (required)

**Returns:** `string` - Expanded IRI

**Example:**
```javascript
import { expandPrefix } from 'unrdf/utils';

const prefixes = {
  ex: 'http://example.org/',
  foaf: 'http://xmlns.com/foaf/0.1/'
};

const expanded = expandPrefix('ex:alice', prefixes);
console.log(expanded); // "http://example.org/alice"
```

---

### `compactIRI(iri, prefixes)`

Compact an IRI to prefixed form.

**Parameters:**
- `iri` (string): Full IRI (required)
- `prefixes` (Object): Prefix mappings (required)

**Returns:** `string` - Prefixed URI or original IRI

**Example:**
```javascript
import { compactIRI } from 'unrdf/utils';

const prefixes = {
  ex: 'http://example.org/',
  foaf: 'http://xmlns.com/foaf/0.1/'
};

const compacted = compactIRI('http://example.org/alice', prefixes);
console.log(compacted); // "ex:alice"
```

---

### `loadPrefixes(format)`

Load common prefix mappings.

**Parameters:**
- `format` (string, optional): Format ('rdf', 'rdfs', 'owl', 'foaf', 'schema', 'all')

**Returns:** `Object` - Prefix mappings

**Example:**
```javascript
import { loadPrefixes } from 'unrdf/utils';

// Load all common prefixes
const prefixes = loadPrefixes('all');

// Load specific ontology
const rdfPrefixes = loadPrefixes('rdf');
```

---

## Error Handling

### `isRDFError(error)`

Check if an error is an RDF-specific error.

**Parameters:**
- `error` (Error): Error to check (required)

**Returns:** `boolean` - True if RDF error

**Example:**
```javascript
import { isRDFError } from 'unrdf/utils';

try {
  parseTurtle('invalid turtle');
} catch (error) {
  if (isRDFError(error)) {
    console.error('RDF Error:', error.message);
  }
}
```

---

### `formatError(error, options)`

Format an error for display.

**Parameters:**
- `error` (Error): Error to format (required)
- `options` (Object, optional): Format options
  - `verbose` (boolean): Include stack trace
  - `color` (boolean): Use ANSI colors

**Returns:** `string` - Formatted error message

**Example:**
```javascript
import { formatError } from 'unrdf/utils';

try {
  // Some operation
} catch (error) {
  console.error(formatError(error, { verbose: true, color: true }));
}
```

---

## Debugging Utilities

### `inspectQuad(quad, options)`

Inspect a quad with detailed information.

**Parameters:**
- `quad` (Quad): Quad to inspect (required)
- `options` (Object, optional): Inspect options
  - `depth` (number): Inspection depth
  - `colors` (boolean): Use colors

**Returns:** `string` - Formatted quad information

**Example:**
```javascript
import { inspectQuad } from 'unrdf/utils';

const q = quad(alice, knows, bob);
console.log(inspectQuad(q, { colors: true }));
```

**Output:**
```
Quad {
  subject: NamedNode { value: 'http://example.org/alice' },
  predicate: NamedNode { value: 'http://xmlns.com/foaf/0.1/knows' },
  object: NamedNode { value: 'http://example.org/bob' },
  graph: DefaultGraph {}
}
```

---

### `inspectStore(store, options)`

Inspect a store with statistics and sample quads.

**Parameters:**
- `store` (Store): Store to inspect (required)
- `options` (Object, optional): Inspect options
  - `sampleSize` (number): Number of sample quads to show. Default: 5
  - `showStats` (boolean): Show statistics. Default: true

**Returns:** `string` - Formatted store information

**Example:**
```javascript
import { inspectStore } from 'unrdf/utils';

console.log(inspectStore(store, { sampleSize: 10 }));
```

**Output:**
```
Store {
  size: 1,234 quads
  subjects: 234
  predicates: 12
  objects: 456
  graphs: 3

  Sample quads:
    1. ex:alice foaf:knows ex:bob
    2. ex:bob foaf:name "Bob"
    ...
}
```

---

### `diffStores(store1, store2)`

Compare two stores and show differences.

**Parameters:**
- `store1` (Store): First store (required)
- `store2` (Store): Second store (required)

**Returns:** `Object` - Difference information
- `added` (Store): Quads in store2 but not store1
- `removed` (Store): Quads in store1 but not store2
- `common` (Store): Quads in both stores

**Example:**
```javascript
import { diffStores } from 'unrdf/utils';

const diff = diffStores(store1, store2);

console.log('Added:', diff.added.size);
console.log('Removed:', diff.removed.size);
console.log('Common:', diff.common.size);
```

---

## Performance Utilities

### `measureOperation(fn, label)`

Measure the execution time of an operation.

**Parameters:**
- `fn` (Function): Function to measure (required)
- `label` (string, optional): Label for the operation

**Returns:** `Promise<Object>` - Result and timing
- `result` (any): Function result
- `duration` (number): Duration in milliseconds

**Example:**
```javascript
import { measureOperation } from 'unrdf/utils';

const { result, duration } = await measureOperation(
  async () => {
    return parseTurtle(largeTurtleData);
  },
  'Parse large Turtle file'
);

console.log(`Parsed in ${duration}ms`);
```

---

### `benchmark(operations, iterations)`

Benchmark multiple operations.

**Parameters:**
- `operations` (Object): Operations to benchmark `{ name: function }` (required)
- `iterations` (number, optional): Number of iterations. Default: 100

**Returns:** `Object` - Benchmark results

**Example:**
```javascript
import { benchmark } from 'unrdf/utils';

const results = await benchmark({
  'Parse Turtle': () => parseTurtle(ttlData),
  'Parse N-Quads': () => parseNQuads(nquadData),
  'Serialize Turtle': () => serializeTurtle(store)
}, 1000);

console.table(results);
```

**Output:**
```
┌───────────────────┬──────────┬──────────┬──────────┐
│ Operation         │ Average  │ Min      │ Max      │
├───────────────────┼──────────┼──────────┼──────────┤
│ Parse Turtle      │ 12.3ms   │ 10.1ms   │ 15.2ms   │
│ Parse N-Quads     │ 8.7ms    │ 7.2ms    │ 11.3ms   │
│ Serialize Turtle  │ 15.4ms   │ 13.8ms   │ 18.9ms   │
└───────────────────┴──────────┴──────────┴──────────┘
```

---

## Best Practices

### Error Handling Pattern

```javascript
import { parseTurtle, isRDFError, formatError } from 'unrdf/utils';

try {
  const store = parseTurtle(turtleData);
} catch (error) {
  if (isRDFError(error)) {
    console.error('RDF parsing failed:', formatError(error));
  } else {
    console.error('Unexpected error:', error.message);
  }
}
```

### Validation Pattern

```javascript
import { validateStore, validateQuad } from 'unrdf/utils';

// Validate before operations
const storeValidation = validateStore(store);
if (!storeValidation.valid) {
  throw new Error(`Invalid store: ${storeValidation.errors.join(', ')}`);
}

// Validate individual quads
for (const quad of quads) {
  const quadValidation = validateQuad(quad);
  if (!quadValidation.valid) {
    console.warn(`Skipping invalid quad: ${quadValidation.errors.join(', ')}`);
  }
}
```

### Performance Monitoring

```javascript
import { measureOperation, benchmark } from 'unrdf/utils';

// Measure critical operations
const { result, duration } = await measureOperation(
  () => complexOperation(),
  'Complex Operation'
);

if (duration > 1000) {
  console.warn(`Operation took ${duration}ms - consider optimization`);
}

// Benchmark alternatives
const results = await benchmark({
  'Approach A': () => approachA(),
  'Approach B': () => approachB()
});
```
