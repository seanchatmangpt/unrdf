# How-To: Use Composables

**Problem**: You need a high-level, ergonomic API for RDF operations without dealing with low-level Store manipulation.

## Solution

UNRDF composables provide context-aware functions that simplify common RDF tasks. Use `initStore()` to create a root context, then access graph, parsing, and query functions via composables.

### Initialize Store Context

Start every workflow with `initStore()`:

```javascript
import { initStore, useGraph } from 'unrdf';

// Create root context with empty store
await initStore();

// Access graph operations
const graph = useGraph();

console.log(`Initial size: ${graph.size}`);
```

### Add and Query Data

Use `useGraph()` for all graph operations:

```javascript
import { initStore, useGraph, DataFactory } from 'unrdf';

await initStore();
const graph = useGraph();

// Create terms
const { namedNode, literal } = DataFactory;

// Add quads
graph.add(
  namedNode('http://example.org/alice'),
  namedNode('http://schema.org/name'),
  literal('Alice')
);

graph.add(
  namedNode('http://example.org/alice'),
  namedNode('http://schema.org/age'),
  literal(30, namedNode('http://www.w3.org/2001/XMLSchema#integer'))
);

// Query with SPARQL
const results = graph.select(`
  PREFIX schema: <http://schema.org/>
  SELECT ?name ?age WHERE {
    ?person schema:name ?name ;
            schema:age ?age .
  }
`);

console.log(results[0].name.value);  // "Alice"
console.log(results[0].age.value);   // "30"
```

### Parse and Serialize

Use `useTurtle()` for format conversion:

```javascript
import { initStore, useTurtle, useGraph } from 'unrdf';

await initStore();
const turtle = useTurtle();
const graph = useGraph();

// Parse Turtle
const ttl = `
  @prefix ex: <http://example.org/> .
  ex:alice a ex:Person ;
    ex:name "Alice" .
`;

const store = turtle.parse(ttl);

// Add to graph
store.forEach(quad => graph.add(quad));

// Serialize back
const output = turtle.serialize(graph.store);
console.log(output);
```

### Namespaces and Terms

Use `useTerms()` and `usePrefixes()` for ergonomic term creation:

```javascript
import { initStore, useTerms, usePrefixes } from 'unrdf';

await initStore();
const { namedNode, literal, quad } = useTerms();
const prefixes = usePrefixes();

// Register namespaces
prefixes.register('ex', 'http://example.org/');
prefixes.register('schema', 'http://schema.org/');

// Create terms with prefixes
const alice = namedNode(prefixes.expand('ex:alice'));
const name = namedNode(prefixes.expand('schema:name'));

// Create quad
const q = quad(alice, name, literal('Alice'));

// Compact back to prefixed form
const prefixedIRI = prefixes.compact(alice.value);
console.log(prefixedIRI);  // "ex:alice"
```

### Validation and Reasoning

Use specialized composables for validation:

```javascript
import { initStore, useGraph, useValidator, useReasoner } from 'unrdf';

await initStore();
const graph = useGraph();
const validator = useValidator();
const reasoner = useReasoner();

// Add data
graph.update(`
  INSERT DATA {
    ex:alice a schema:Person ; schema:name "Alice" .
  }
`);

// SHACL validation
const report = await validator.validate(graph.store, shapesTtl);
if (!report.conforms) {
  console.error('Validation failed');
}

// N3 reasoning
const inferred = await reasoner.reason(graph.store, rulesTtl);
console.log(`Inferred ${inferred.size} new quads`);
```

### Graph Statistics

Use `stats()` for graph metrics:

```javascript
import { initStore, useGraph } from 'unrdf';

await initStore();
const graph = useGraph();

// Add data
graph.update(`
  INSERT DATA {
    ex:alice a schema:Person .
    ex:bob a schema:Person .
    ex:alice schema:knows ex:bob .
  }
`);

// Get statistics
const stats = graph.stats();
console.log(`Quads: ${stats.quadCount}`);
console.log(`Subjects: ${stats.subjectCount}`);
console.log(`Predicates: ${stats.predicateCount}`);
console.log(`Objects: ${stats.objectCount}`);
console.log(`Graphs: ${stats.graphCount}`);
```

### Boolean Checks

Use `ask()` for existence checks:

```javascript
const graph = useGraph();

// Check if pattern exists
const hasAlice = graph.ask(`
  ASK { ex:alice a schema:Person }
`);

if (hasAlice) {
  console.log('Alice exists in graph');
}

// Complex conditions
const hasFriends = graph.ask(`
  ASK {
    ?person schema:knows ?friend .
  }
`);
```

### Transform Graphs

Use `construct()` to reshape data:

```javascript
const graph = useGraph();

// Transform schema.org → FOAF
const foafStore = graph.construct(`
  PREFIX schema: <http://schema.org/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>

  CONSTRUCT {
    ?person a foaf:Person ;
            foaf:name ?name .
  }
  WHERE {
    ?person a schema:Person ;
            schema:name ?name .
  }
`);

console.log(`Transformed to ${foafStore.size} quads`);
```

### Canonicalization

Use `useCanon()` for graph comparison:

```javascript
import { initStore, useGraph, useCanon } from 'unrdf';

await initStore();
const graph = useGraph();
const canon = useCanon();

// Add data
graph.add(...quads1);

// Canonicalize
const canonical = await canon.canonicalize(graph.store);

// Compare graphs
const store2 = /* another store */;
const isomorphic = await canon.isIsomorphic(graph.store, store2);

if (isomorphic) {
  console.log('Graphs are equivalent');
}

// Get content hash
const hash = await canon.getCanonicalHash(graph.store);
console.log(`Content hash: ${hash}`);
```

## Variations

### Load from Files

Combine composables with file I/O:

```javascript
import { initStore, useGraph, useTurtle } from 'unrdf';
import { readFile } from 'fs/promises';

await initStore();
const graph = useGraph();
const turtle = useTurtle();

// Load Turtle file
const ttl = await readFile('./data.ttl', 'utf-8');
const store = turtle.parse(ttl);

// Add to graph
store.forEach(quad => graph.add(quad));

// Query loaded data
const results = graph.select(`SELECT * WHERE { ?s ?p ?o } LIMIT 10`);
```

### Multiple Contexts

Create isolated contexts for different datasets:

```javascript
import { createStoreContext, useStoreContext, useGraph } from 'unrdf';

// Context 1
const ctx1 = createStoreContext([], { label: 'dataset1' });
await ctx1.call(async () => {
  const graph = useGraph();
  graph.add(...quads1);
});

// Context 2
const ctx2 = createStoreContext([], { label: 'dataset2' });
await ctx2.call(async () => {
  const graph = useGraph();
  graph.add(...quads2);
});

// Each context has isolated data
```

### Runtime Validation

Use `useZod()` for type safety:

```javascript
import { initStore, useZod } from 'unrdf';

await initStore();
const zod = useZod();

// Validate quad structure
const quad = { subject: {...}, predicate: {...}, object: {...} };
const valid = zod.validateQuad(quad);

if (!valid) {
  throw new Error('Invalid quad structure');
}

// Validate SPARQL query
const query = `SELECT * WHERE { ?s ?p ?o }`;
const validQuery = zod.validateSPARQLQuery(query);
```

### Delta Tracking

Use `useDelta()` for change detection:

```javascript
import { initStore, useGraph, useDelta } from 'unrdf';

await initStore();
const graph = useGraph();
const delta = useDelta();

// Take snapshot
const snapshot1 = delta.snapshot(graph.store);

// Make changes
graph.add(...newQuads);
graph.remove(...oldQuads);

// Compute diff
const snapshot2 = delta.snapshot(graph.store);
const diff = delta.diff(snapshot1, snapshot2);

console.log(`Added: ${diff.additions.length}`);
console.log(`Removed: ${diff.removals.length}`);
```

### Chain Operations

Compose multiple operations fluently:

```javascript
await initStore();
const graph = useGraph();

// Parse → Query → Transform → Serialize
const result = await graph
  .update(`INSERT DATA { ex:alice a schema:Person }`)
  .select(`SELECT ?person WHERE { ?person a schema:Person }`)
  .then(results => {
    const transformed = graph.construct(`
      CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }
    `);

    return useTurtle().serialize(transformed);
  });

console.log(result);
```

## Related Guides

- [Tutorial: Getting Started](../tutorials/01-getting-started.md) - Composables walkthrough
- [How-To: Parse RDF Formats](./parse-rdf-formats.md) - Parsing with useTurtle()
- [How-To: Query with SPARQL](./query-with-sparql.md) - useGraph() queries
- [Explanation: Context System](../explanation/architecture/context-system.md) - unctx architecture
