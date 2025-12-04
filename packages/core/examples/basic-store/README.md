# Basic RDF Store Operations

This example demonstrates the fundamental operations of working with RDF data using UNRDF's core store functionality.

## What You'll Learn

- Creating an RDF store
- Adding triples (quads) to the store
- Querying the store with patterns
- Removing data from the store
- Exporting store data to N-Triples format
- Analyzing store statistics

## Running the Example

```bash
# Install dependencies
pnpm install

# Run the example
pnpm start

# Run tests
pnpm test
```

## Code Overview

### Creating a Store

```javascript
import { Store, DataFactory } from 'n3';
const { namedNode, literal } = DataFactory;

const store = new Store();
```

### Adding Triples

```javascript
store.addQuad(
  namedNode('http://example.org/alice'),
  namedNode('http://xmlns.com/foaf/0.1/name'),
  literal('Alice')
);
```

### Querying the Store

```javascript
// Query all quads
const allQuads = store.getQuads(null, null, null, null);

// Query by subject
const aliceQuads = store.getQuads(
  namedNode('http://example.org/alice'),
  null,
  null,
  null
);
```

### Removing Data

```javascript
// Find and remove matching quads
const toRemove = store.getQuads(null, predicate, null, null);
toRemove.forEach(quad => store.removeQuad(quad));
```

### Exporting Data

```javascript
// Export to N-Triples format
const ntriples = exportToNTriples(store);
console.log(ntriples);
```

## Key Concepts

### RDF Triples

An RDF triple consists of:
- **Subject**: The resource being described (URI or blank node)
- **Predicate**: The property or relationship (URI)
- **Object**: The value (URI, blank node, or literal)

### Store Patterns

Query patterns use `null` as wildcards:
- `(alice, null, null)` - All properties of Alice
- `(null, name, null)` - All name properties
- `(null, null, "Alice")` - All triples with "Alice" as object

### N-Triples Format

Simple line-based RDF serialization:
```
<http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" .
```

## Next Steps

- Explore SPARQL queries in the `sparql-queries` example
- Learn about parsing different RDF formats in the `rdf-parsing` example
- Check out the UNRDF documentation for advanced features


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
