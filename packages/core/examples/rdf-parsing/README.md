# RDF Parsing Examples

This example demonstrates parsing RDF data from various serialization formats using UNRDF.

## What You'll Learn

- Parsing Turtle format RDF
- Parsing N-Triples format RDF
- Parsing N-Quads format (with named graphs)
- Merging multiple RDF graphs
- Canonicalizing RDF output
- Validating RDF syntax
- Converting between formats

## Running the Example

```bash
# Install dependencies
pnpm install

# Run the example
pnpm start

# Run tests
pnpm test
```

## RDF Formats

### Turtle (Terse RDF Triple Language)

Human-readable format with prefixes:

```turtle
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:alice foaf:name "Alice" ;
         foaf:knows ex:bob .
```

### N-Triples

Simple line-based format:

```ntriples
<http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" .
<http://example.org/alice> <http://xmlns.com/foaf/0.1/knows> <http://example.org/bob> .
```

### N-Quads

N-Triples with graph names:

```nquads
<http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" <http://example.org/graph1> .
```

## Parsing Examples

### Parse Turtle

```javascript
import { parseTurtle } from './index.mjs';

const turtle = `
  @prefix ex: <http://example.org/> .
  ex:alice ex:name "Alice" .
`;

const store = await parseTurtle(turtle);
console.log(`Parsed ${store.size} triples`);
```

### Parse N-Triples

```javascript
import { parseNTriples } from './index.mjs';

const ntriples = `
  <http://example.org/alice> <http://example.org/name> "Alice" .
`;

const store = await parseNTriples(ntriples);
```

### Parse N-Quads

```javascript
import { parseNQuads } from './index.mjs';

const nquads = `
  <http://example.org/alice> <http://example.org/name> "Alice" <http://example.org/graph1> .
`;

const store = await parseNQuads(nquads);
const graphs = getGraphs(store);
console.log(`Found graphs: ${graphs.join(', ')}`);
```

## Advanced Operations

### Merge Multiple Stores

```javascript
const store1 = await parseTurtle(turtle1);
const store2 = await parseTurtle(turtle2);
const merged = mergeStores([store1, store2]);
```

### Canonicalize RDF

```javascript
const canonical = canonicalize(store);
// Returns sorted N-Triples for consistent comparison
```

### Validate Syntax

```javascript
const result = await validateRDF(rdfString, 'turtle');
if (result.valid) {
  console.log(`Valid! ${result.tripleCount} triples`);
} else {
  console.log(`Errors: ${result.errors.join(', ')}`);
}
```

### Convert Formats

```javascript
const ntriples = await convertFormat(
  turtleString,
  'turtle',
  'ntriples'
);
```

## Working with Graphs

### Extract Graph Names

```javascript
const graphs = getGraphs(store);
// Returns: ['http://example.org/graph1', 'http://example.org/graph2']
```

### Query Specific Graph

```javascript
import { namedNode } from 'n3';

const quads = store.getQuads(
  null,
  null,
  null,
  namedNode('http://example.org/graph1')
);
```

## Error Handling

All parsing functions throw descriptive errors:

```javascript
try {
  const store = await parseTurtle(invalidTurtle);
} catch (error) {
  console.error(`Parsing failed: ${error.message}`);
}
```

## Key Concepts

### RDF Serialization

Different formats for representing the same RDF data:
- **Turtle**: Human-readable, supports prefixes and abbreviations
- **N-Triples**: Simple, one triple per line
- **N-Quads**: N-Triples with graph names (fourth element)

### Named Graphs

N-Quads can represent multiple graphs:
```
<s1> <p1> <o1> <graph1> .
<s2> <p2> <o2> <graph2> .
```

### Canonicalization

Sorting triples alphabetically creates a canonical form for:
- Comparing RDF graphs
- Hashing/signing RDF content
- Testing RDF equality

## Next Steps

- Explore the basic store operations example
- Try SPARQL queries on parsed data
- Check out advanced UNRDF features in the documentation


## Testing

Run the test suite:

```bash
pnpm test
```

Run tests in watch mode:

```bash
pnpm test:watch
```

Generate coverage report:

```bash
pnpm test:coverage
```

Test coverage: 80%+ (minimum requirement)
