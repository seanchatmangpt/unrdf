# Composables API Reference

Complete API reference for UNRDF composables - Vue-inspired functions for RDF operations.

## Overview

UNRDF composables provide a consistent, functional interface for RDF operations. Inspired by Vue.js composables, they enable reactive, context-aware graph manipulation.

**Core Principles:**

- 🎯 **Single Responsibility** - Each composable does one thing well
- 🔄 **Composability** - Combine composables for complex workflows
- 📦 **Context-Aware** - Automatically uses store context (unctx)
- ✅ **Type-Safe** - JSDoc + Zod runtime validation
- ⚡ **Performance** - Optimized for large graphs

## Store Context

### initStore(quads, options)

Initialize the RDF store context.

**Parameters:**

- `quads` (Array): Initial quads to add (optional)
- `options` (Object): Store options
  - `baseIRI` (string): Base IRI for the store
  - `prefixes` (Object): Prefix mappings

**Returns:** Function to run code within store context

**Example:**

```javascript
import { initStore } from 'unrdf';

const runApp = initStore([], {
  baseIRI: 'http://example.org/',
  prefixes: {
    ex: 'http://example.org/',
    foaf: 'http://xmlns.com/foaf/0.1/'
  }
});

await runApp(async () => {
  // Code runs with store context
  const graph = useGraph();
  // ...
});
```

### useStoreContext()

Access the current store context.

**Returns:** StoreContext object

**Example:**

```javascript
import { useStoreContext } from 'unrdf';

const ctx = useStoreContext();
console.log('Store size:', ctx.store.size);
console.log('Base IRI:', ctx.baseIRI);
```

## Graph Operations

### useGraph()

Main interface for RDF graph operations.

**Returns:** Graph operations object

**Example:**

```javascript
import { useGraph } from 'unrdf';

const graph = useGraph();

// SPARQL query
const results = graph.select(`
  PREFIX ex: <http://example.org/>
  SELECT ?name WHERE { ?person ex:name ?name }
`);

// Validate
const validation = graph.validate(shaclShapes);

// Serialize
const turtle = graph.serialize({ format: 'Turtle' });
```

### Methods

#### query(sparql, options)

Execute any SPARQL query.

**Parameters:**

- `sparql` (string): SPARQL query
- `options` (Object): Query options
  - `limit` (number): Result limit
  - `signal` (AbortSignal): Abort signal

**Returns:** Query result object

```typescript
{
  type: 'select' | 'ask' | 'construct' | 'update';
  results?: Array<Object>;  // For SELECT
  boolean?: boolean;        // For ASK
  store?: Store;            // For CONSTRUCT
}
```

**Example:**

```javascript
const result = graph.query(`
  PREFIX ex: <http://example.org/>
  SELECT ?s ?p ?o WHERE { ?s ?p ?o }
  LIMIT 10
`);

console.log('Type:', result.type);
console.log('Results:', result.results);
```

#### select(sparql)

Execute SPARQL SELECT query.

**Returns:** Array of result bindings

**Example:**

```javascript
const results = graph.select(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name ?email WHERE {
    ?person foaf:name ?name ;
            foaf:mbox ?email .
  }
`);

results.forEach(row => {
  console.log(`${row.name.value}: ${row.email.value}`);
});
```

#### ask(sparql)

Execute SPARQL ASK query.

**Returns:** boolean

**Example:**

```javascript
const exists = graph.ask(`
  PREFIX ex: <http://example.org/>
  ASK { ?s a ex:Person }
`);

console.log('Has persons:', exists);
```

#### construct(sparql)

Execute SPARQL CONSTRUCT query.

**Returns:** New Store with constructed triples

**Example:**

```javascript
const newStore = graph.construct(`
  PREFIX ex: <http://example.org/>
  CONSTRUCT { ?person ex:hasEmail ?email }
  WHERE { ?person foaf:mbox ?email }
`);

console.log('Constructed triples:', newStore.size);
```

#### update(sparql)

Execute SPARQL UPDATE query.

**Returns:** Update result object

**Example:**

```javascript
await graph.update(`
  PREFIX ex: <http://example.org/>
  INSERT DATA {
    ex:Alice a ex:Person ;
      ex:name "Alice Smith" .
  }
`);
```

#### validate(shapesInput)

Validate graph against SHACL shapes.

**Parameters:**

- `shapesInput` (string|Store): SHACL shapes as Turtle or Store

**Returns:** Validation report

```typescript
{
  conforms: boolean;
  results: Array<{
    focusNode: string;
    resultPath: string;
    message: string;
    severity: 'Violation' | 'Warning' | 'Info';
  }>;
}
```

**Example:**

```javascript
const shapes = `
  PREFIX ex: <http://example.org/>
  PREFIX sh: <http://www.w3.org/ns/shacl#>

  ex:PersonShape a sh:NodeShape ;
    sh:targetClass ex:Person ;
    sh:property [
      sh:path ex:name ;
      sh:datatype xsd:string ;
      sh:minCount 1 ;
    ] .
`;

const report = graph.validate(shapes);

if (!report.conforms) {
  console.log('Validation failed:');
  report.results.forEach(r => {
    console.log(`- ${r.message}`);
  });
}
```

#### validateOrThrow(shapesInput)

Validate and throw on failure.

**Throws:** Error if validation fails

**Example:**

```javascript
try {
  graph.validateOrThrow(shapes);
  console.log('Validation passed!');
} catch (error) {
  console.error('Validation error:', error.message);
}
```

#### serialize(options)

Serialize graph to string.

**Parameters:**

- `options` (Object):
  - `format` (string): 'Turtle' | 'N-Quads' (default: 'Turtle')
  - `prefixes` (Object): Prefix mappings

**Returns:** Serialized string

**Example:**

```javascript
const turtle = graph.serialize({
  format: 'Turtle',
  prefixes: {
    ex: 'http://example.org/',
    foaf: 'http://xmlns.com/foaf/0.1/'
  }
});

console.log(turtle);
```

#### stats()

Get graph statistics.

**Returns:** Statistics object

```typescript
{
  quadCount: number;
  subjectCount: number;
  predicateCount: number;
  objectCount: number;
  blankNodeCount: number;
  namedGraphCount: number;
}
```

**Example:**

```javascript
const stats = graph.stats();
console.log(`Quads: ${stats.quadCount}`);
console.log(`Subjects: ${stats.subjectCount}`);
```

#### isIsomorphic(otherGraph)

Check if graphs are isomorphic.

**Parameters:**

- `otherGraph` (Object|Store): Another graph or Store

**Returns:** boolean

**Example:**

```javascript
const graph1 = useGraph();
const graph2 = useGraph();

const isEqual = graph1.isIsomorphic(graph2);
console.log('Graphs are isomorphic:', isEqual);
```

#### union(...otherGraphs)

Create union of graphs.

**Returns:** New graph with union

**Example:**

```javascript
const combined = graph1.union(graph2, graph3);
console.log('Combined size:', combined.size);
```

#### difference(otherGraph)

Create difference of graphs.

**Returns:** New graph with difference

**Example:**

```javascript
const diff = graph1.difference(graph2);
console.log('Difference size:', diff.size);
```

#### intersection(otherGraph)

Create intersection of graphs.

**Returns:** New graph with intersection

**Example:**

```javascript
const common = graph1.intersection(graph2);
console.log('Common quads:', common.size);
```

#### skolemize(baseIRI)

Skolemize blank nodes.

**Parameters:**

- `baseIRI` (string): Base IRI for skolemization (optional)

**Returns:** New graph with skolemized nodes

**Example:**

```javascript
const skolemized = graph.skolemize('http://example.org/.well-known/genid/');
```

#### toJSONLD(options)

Convert to JSON-LD.

**Parameters:**

- `options` (Object):
  - `context` (Object): JSON-LD context
  - `frame` (Object): JSON-LD frame

**Returns:** JSON-LD document

**Example:**

```javascript
const jsonld = graph.toJSONLD({
  context: {
    '@vocab': 'http://example.org/',
    'foaf': 'http://xmlns.com/foaf/0.1/'
  }
});
```

### Properties

#### size

Number of quads in the graph.

**Type:** number (read-only)

**Example:**

```javascript
console.log('Graph has', graph.size, 'quads');
```

## Turtle Operations

### useTurtle()

Parse and serialize Turtle format.

**Returns:** Turtle operations object

**Example:**

```javascript
import { useTurtle } from 'unrdf';

const turtle = useTurtle();

// Parse
const quads = await turtle.parse(`
  @prefix ex: <http://example.org/> .
  ex:Alice a ex:Person ;
    ex:name "Alice" .
`);

// Serialize
const turtleString = await turtle.serialize(quads, {
  prefixes: { ex: 'http://example.org/' }
});
```

### Methods

#### parse(input, options)

Parse Turtle to quads.

**Parameters:**

- `input` (string): Turtle string
- `options` (Object):
  - `baseIRI` (string): Base IRI
  - `blankNodePrefix` (string): Blank node prefix

**Returns:** Promise<Array<Quad>>

#### serialize(quads, options)

Serialize quads to Turtle.

**Parameters:**

- `quads` (Array): Array of quads
- `options` (Object):
  - `prefixes` (Object): Prefix mappings

**Returns:** Promise<string>

## Delta Operations

### useDelta()

Detect and compute graph changes.

**Returns:** Delta operations object

**Example:**

```javascript
import { useDelta } from 'unrdf';

const delta = useDelta();

const diff = delta.compute(store1, store2);

console.log('Additions:', diff.additions.length);
console.log('Removals:', diff.removals.length);
```

### Methods

#### compute(before, after)

Compute delta between two stores.

**Returns:** Delta object

```typescript
{
  additions: Array<Quad>;
  removals: Array<Quad>;
  unchanged: Array<Quad>;
}
```

#### apply(store, delta)

Apply delta to store.

**Parameters:**

- `store` (Store): Target store
- `delta` (Object): Delta to apply

**Returns:** Modified store

#### inverse(delta)

Compute inverse delta.

**Returns:** Inverted delta object

**Example:**

```javascript
const inverseDelta = delta.inverse({
  additions: [/* quads */],
  removals: [/* quads */]
});

// Undo changes
delta.apply(store, inverseDelta);
```

## Canonicalization

### useCanon()

Canonicalize RDF graphs (URDNA2015).

**Returns:** Canon operations object

**Example:**

```javascript
import { useCanon } from 'unrdf';

const canon = useCanon();

const { hash, canonicalQuads } = await canon.canonicalize(store);

console.log('Canonical hash:', hash);
```

### Methods

#### canonicalize(store, options)

Canonicalize a store.

**Parameters:**

- `store` (Store): Store to canonicalize
- `options` (Object):
  - `algorithm` (string): 'URDNA2015' | 'RDFC-1.0'
  - `format` (string): Output format

**Returns:** Promise<CanonResult>

```typescript
{
  hash: string;
  canonicalQuads: Array<Quad>;
  normalizedDocument: string;
}
```

#### hash(store)

Get canonical hash without quads.

**Returns:** Promise<string>

**Example:**

```javascript
const hash1 = await canon.hash(store1);
const hash2 = await canon.hash(store2);

console.log('Graphs equal:', hash1 === hash2);
```

## Term Utilities

### useTerms()

Create and manipulate RDF terms.

**Returns:** Term utilities object

**Example:**

```javascript
import { useTerms } from 'unrdf';

const terms = useTerms();

const iri = terms.namedNode('http://example.org/Alice');
const literal = terms.literal('Alice', 'en');
const blank = terms.blankNode();
```

### Methods

#### namedNode(iri)

Create named node.

**Returns:** NamedNode

#### literal(value, languageOrDatatype)

Create literal.

**Parameters:**

- `value` (string): Literal value
- `languageOrDatatype` (string|NamedNode): Language tag or datatype

**Returns:** Literal

**Example:**

```javascript
const name = terms.literal('Alice', 'en');
const age = terms.literal('30', terms.namedNode('http://www.w3.org/2001/XMLSchema#integer'));
```

#### blankNode(id)

Create blank node.

**Parameters:**

- `id` (string): Blank node ID (optional)

**Returns:** BlankNode

#### quad(subject, predicate, object, graph)

Create quad.

**Returns:** Quad

**Example:**

```javascript
const quad = terms.quad(
  terms.namedNode('http://example.org/Alice'),
  terms.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
  terms.namedNode('http://example.org/Person')
);
```

#### defaultGraph()

Get default graph node.

**Returns:** DefaultGraph

## Reasoning

### useReasoner()

Apply reasoning rules (Eye reasoner).

**Returns:** Reasoner operations object

**Example:**

```javascript
import { useReasoner } from 'unrdf';

const reasoner = useReasoner();

const inferred = await reasoner.reason(store, rules);

console.log('Inferred triples:', inferred.size);
```

### Methods

#### reason(store, rules, options)

Apply reasoning rules.

**Parameters:**

- `store` (Store): Input store
- `rules` (string|Store): N3 reasoning rules
- `options` (Object): Reasoning options

**Returns:** Promise<Store>

**Example:**

```javascript
const rules = `
  @prefix ex: <http://example.org/> .
  { ?x a ex:Person } => { ?x a ex:Agent } .
`;

const inferred = await reasoner.reason(store, rules);
```

## Zod Validation

### useZod()

Runtime validation with Zod schemas.

**Returns:** Zod utilities object

**Example:**

```javascript
import { useZod } from 'unrdf';

const zod = useZod();

const PersonSchema = zod.object({
  name: zod.string(),
  age: zod.number().positive()
});

const result = PersonSchema.safeParse({ name: 'Alice', age: 30 });
console.log('Valid:', result.success);
```

## Best Practices

### 1. Always Use Store Context

```javascript
// Good: Use store context
const runApp = initStore();
await runApp(() => {
  const graph = useGraph();
  // ...
});

// Bad: No context
const graph = useGraph();  // Throws error
```

### 2. Batch Operations

```javascript
// Good: Single update
await graph.update(`
  INSERT DATA {
    ex:Alice a ex:Person .
    ex:Bob a ex:Person .
    ex:Charlie a ex:Person .
  }
`);

// Bad: Multiple updates
await graph.update(`INSERT DATA { ex:Alice a ex:Person }`);
await graph.update(`INSERT DATA { ex:Bob a ex:Person }`);
await graph.update(`INSERT DATA { ex:Charlie a ex:Person }`);
```

### 3. Use Prefixes

```javascript
// Good: Readable prefixes
const graph = useGraph();
graph.select(`
  PREFIX ex: <http://example.org/>
  SELECT ?name WHERE { ?person ex:name ?name }
`);

// Bad: Full IRIs
graph.select(`
  SELECT ?name WHERE { ?person <http://example.org/name> ?name }
`);
```

### 4. Validate Data

```javascript
// Good: Validate before use
const report = graph.validate(shapes);
if (report.conforms) {
  // Process data
}

// Better: Throw on invalid
graph.validateOrThrow(shapes);
// Only reaches here if valid
```

## See Also

- [Knowledge Hooks API](/docs/api/knowledge-hooks.md)
- [CLI Reference](/docs/api/cli-reference.md)
- [Examples](/examples)
- [Core Concepts](/docs/core-concepts.md)
