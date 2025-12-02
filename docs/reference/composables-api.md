# Composables API Reference

**Version**: v4.1.1
**Package**: `unrdf/composables`
**Module**: `composables`

This reference documents the high-level composable functions that provide an opinionated, ergonomic API for RDF operations with async context management.

---

## Table of Contents

- [Context Management](#context-management)
- [Graph Operations](#graph-operations)
- [Turtle Operations](#turtle-operations)
- [Term Creation](#term-creation)
- [Validation & Reasoning](#validation--reasoning)
- [Change Tracking](#change-tracking)

---

## Context Management

### initStore

**Signature**: `initStore(initialQuads?: Quad[], options?: Object): Function`

**Description**: Initializes the root store context for your application. This should be called at the root level to enable context propagation to all composables.

**Parameters**:
- `initialQuads` (Quad[], optional) - Initial quads to populate the store (default: `[]`)
- `options` (Object, optional) - Store configuration options
  - `baseIRI` (string, optional) - Base IRI for the store

**Returns**: Function to call with your application logic

**Example**:
```javascript
import { initStore, useGraph } from 'unrdf/composables';

// At the root of your application
const runApp = initStore([], { baseIRI: 'http://example.org/' });

runApp(async () => {
  // Your application code here
  const graph = useGraph();
  graph.add(/* ... */);
  // All composables will use the same store context
});
```

**See Also**: [setStoreContext](#setstorecontext), [useStoreContext](#usestorecontext)

**Since**: v4.1.1

---

### setStoreContext

**Signature**: `setStoreContext(initialQuads?: Quad[], options?: Object): StoreContext`

**Description**: Sets the store context for the current execution context. Useful when you need to set the context outside of initStore.

**Parameters**:
- `initialQuads` (Quad[], optional) - Initial quads (default: `[]`)
- `options` (Object, optional) - Store configuration options

**Returns**: The created StoreContext

**Example**:
```javascript
import { setStoreContext, useGraph } from 'unrdf/composables';

// Set context manually
const context = setStoreContext([], { baseIRI: 'http://example.org/' });

// Now composables will use this context
const graph = useGraph();
```

**See Also**: [initStore](#initstore), [useStoreContext](#usestorecontext)

**Since**: v4.1.1

---

### useStoreContext

**Signature**: `useStoreContext(): StoreContext`

**Description**: Accesses the current store context. Throws an error if context is not initialized.

**Returns**: Current StoreContext object with methods:
- `store` (Store) - The underlying N3 Store
- `add(...quads)` - Add quads to store
- `remove(...quads)` - Remove quads from store
- `clear()` - Clear all quads
- `namedNode(value)` - Create named node
- `literal(value, datatype)` - Create literal
- `blankNode(value)` - Create blank node
- `quad(s, p, o, g)` - Create quad
- `serialize(options)` - Serialize store
- `stats()` - Get store statistics
- `query(sparql, options)` - Execute SPARQL query
- `canonicalize(options)` - Canonicalize store
- `isIsomorphic(store1, store2)` - Check isomorphism
- `hash(options)` - Generate canonical hash

**Throws**:
- `Error` if store context is not initialized

**Example**:
```javascript
import { initStore, useStoreContext } from 'unrdf/composables';

const runApp = initStore();

runApp(() => {
  const ctx = useStoreContext();
  console.log('Store size:', ctx.store.size);
});
```

**See Also**: [initStore](#initstore), [setStoreContext](#setstorecontext)

**Since**: v4.1.1

---

## Graph Operations

### useGraph

**Signature**: `useGraph(): GraphAPI`

**Description**: Primary composable for graph operations. Provides high-level methods for adding, querying, and manipulating RDF data.

**Returns**: GraphAPI object with methods:

#### add

**Signature**: `add(...quads: Quad[]): GraphAPI`

**Description**: Adds quads to the graph.

**Parameters**:
- `...quads` (Quad[]) - Quads to add

**Returns**: GraphAPI for chaining

**Example**:
```javascript
import { useGraph, DataFactory } from 'unrdf/composables';

const { namedNode, literal, quad } = DataFactory;
const graph = useGraph();

graph.add(
  quad(
    namedNode('http://example.org/alice'),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal('Alice')
  )
);
```

---

#### remove

**Signature**: `remove(...quads: Quad[]): GraphAPI`

**Description**: Removes quads from the graph.

**Parameters**:
- `...quads` (Quad[]) - Quads to remove

**Returns**: GraphAPI for chaining

**Example**:
```javascript
const graph = useGraph();
graph.remove(quad1, quad2);
```

---

#### select

**Signature**: `select(sparql: string, options?: Object): Promise<Array<Object>>`

**Description**: Executes a SPARQL SELECT query.

**Parameters**:
- `sparql` (string) - SPARQL SELECT query
- `options` (Object, optional) - Query options

**Returns**: Promise resolving to array of binding objects

**Example**:
```javascript
const graph = useGraph();

const results = await graph.select(`
  SELECT ?name WHERE {
    ?person <http://xmlns.com/foaf/0.1/name> ?name .
  }
`);

console.log(results); // [{ name: 'Alice' }, { name: 'Bob' }]
```

---

#### ask

**Signature**: `ask(sparql: string, options?: Object): Promise<boolean>`

**Description**: Executes a SPARQL ASK query.

**Parameters**:
- `sparql` (string) - SPARQL ASK query
- `options` (Object, optional) - Query options

**Returns**: Promise resolving to boolean

**Example**:
```javascript
const graph = useGraph();

const hasData = await graph.ask(`
  ASK WHERE {
    ?s ?p ?o .
  }
`);

console.log('Has data:', hasData);
```

---

#### construct

**Signature**: `construct(sparql: string, options?: Object): Promise<Store>`

**Description**: Executes a SPARQL CONSTRUCT query.

**Parameters**:
- `sparql` (string) - SPARQL CONSTRUCT query
- `options` (Object, optional) - Query options

**Returns**: Promise resolving to a new Store with constructed quads

**Example**:
```javascript
const graph = useGraph();

const constructed = await graph.construct(`
  CONSTRUCT {
    ?person a <http://example.org/Person> .
  } WHERE {
    ?person <http://xmlns.com/foaf/0.1/name> ?name .
  }
`);

console.log('Constructed', constructed.size, 'quads');
```

---

#### describe

**Signature**: `describe(sparql: string, options?: Object): Promise<Store>`

**Description**: Executes a SPARQL DESCRIBE query.

**Parameters**:
- `sparql` (string) - SPARQL DESCRIBE query
- `options` (Object, optional) - Query options

**Returns**: Promise resolving to a new Store with described quads

**Example**:
```javascript
const graph = useGraph();

const described = await graph.describe(`
  DESCRIBE <http://example.org/alice>
`);

console.log('Described', described.size, 'quads');
```

---

#### update

**Signature**: `update(sparql: string, options?: Object): Promise<Store>`

**Description**: Executes a SPARQL UPDATE operation.

**Parameters**:
- `sparql` (string) - SPARQL UPDATE query
- `options` (Object, optional) - Update options

**Returns**: Promise resolving to the updated Store

**Example**:
```javascript
const graph = useGraph();

await graph.update(`
  INSERT DATA {
    <http://example.org/alice> <http://example.org/age> "31" .
  }
`);
```

---

#### stats

**Signature**: `stats(): Object`

**Description**: Retrieves statistics about the graph.

**Returns**: Statistics object with:
- `quads` (number) - Total number of quads
- `subjects` (number) - Unique subjects count
- `predicates` (number) - Unique predicates count
- `objects` (number) - Unique objects count
- `graphs` (number) - Named graphs count

**Example**:
```javascript
const graph = useGraph();
const stats = graph.stats();

console.log('Graph statistics:', stats);
```

---

#### size

**Description**: Property that returns the number of quads in the graph.

**Type**: `number`

**Example**:
```javascript
const graph = useGraph();
console.log('Quads in graph:', graph.size);
```

---

**Complete Example**:
```javascript
import { initStore, useGraph, DataFactory } from 'unrdf/composables';

const { namedNode, literal, quad } = DataFactory;

const runApp = initStore();

runApp(async () => {
  const graph = useGraph();

  // Add data
  graph.add(
    quad(
      namedNode('http://example.org/alice'),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal('Alice')
    ),
    quad(
      namedNode('http://example.org/alice'),
      namedNode('http://example.org/age'),
      literal('30')
    )
  );

  // Query data
  const results = await graph.select(`
    SELECT ?name ?age WHERE {
      ?person <http://xmlns.com/foaf/0.1/name> ?name ;
               <http://example.org/age> ?age .
    }
  `);

  console.log('Results:', results);

  // Get statistics
  const stats = graph.stats();
  console.log('Graph has', stats.quads, 'quads');
});
```

**See Also**: [initStore](#initstore), [useTurtle](#useturtle)

**Since**: v4.1.1

---

## Turtle Operations

### useTurtle

**Signature**: `useTurtle(): TurtleAPI`

**Description**: Composable for Turtle serialization and parsing operations.

**Returns**: TurtleAPI object with methods:

#### parse

**Signature**: `parse(ttl: string, baseIRI?: string): Promise<Store>`

**Description**: Parses a Turtle string into a Store.

**Parameters**:
- `ttl` (string) - Turtle string to parse
- `baseIRI` (string, optional) - Base IRI (default: 'http://example.org/')

**Returns**: Promise resolving to N3 Store

**Example**:
```javascript
import { useTurtle } from 'unrdf/composables';

const turtle = useTurtle();

const ttl = `
  @prefix ex: <http://example.org/> .
  ex:alice ex:knows ex:bob .
`;

const store = await turtle.parse(ttl, 'http://example.org/');
console.log('Parsed', store.size, 'quads');
```

---

#### serialize

**Signature**: `serialize(store: Store, options?: Object): Promise<string>`

**Description**: Serializes a Store to Turtle format.

**Parameters**:
- `store` (Store) - N3 Store to serialize
- `options` (Object, optional) - Serialization options
  - `prefixes` (Object, optional) - Prefix mappings
  - `baseIRI` (string, optional) - Base IRI

**Returns**: Promise resolving to Turtle string

**Example**:
```javascript
import { useGraph, useTurtle } from 'unrdf/composables';

const graph = useGraph();
const turtle = useTurtle();

// Add some data
graph.add(/* ... */);

// Serialize to Turtle
const ttl = await turtle.serialize(graph.store, {
  prefixes: { ex: 'http://example.org/' }
});

console.log(ttl);
```

---

**See Also**: [useGraph](#usegraph), [parseTurtle](./core-rdf-api.md#parseturtle)

**Since**: v4.1.1

---

## Term Creation

### useTerms

**Signature**: `useTerms(): TermsAPI`

**Description**: Composable for creating RDF terms (nodes, literals, quads).

**Returns**: TermsAPI object with methods:

#### namedNode

**Signature**: `namedNode(iri: string): NamedNode`

**Description**: Creates a named node (IRI).

**Parameters**:
- `iri` (string) - IRI value

**Returns**: NamedNode

**Example**:
```javascript
import { useTerms } from 'unrdf/composables';

const terms = useTerms();
const alice = terms.namedNode('http://example.org/alice');
```

---

#### literal

**Signature**: `literal(value: string, datatype?: string): Literal`

**Description**: Creates a literal value.

**Parameters**:
- `value` (string) - Literal value
- `datatype` (string, optional) - Datatype IRI

**Returns**: Literal

**Example**:
```javascript
const terms = useTerms();
const name = terms.literal('Alice');
const age = terms.literal('30', 'http://www.w3.org/2001/XMLSchema#integer');
```

---

#### blankNode

**Signature**: `blankNode(id?: string): BlankNode`

**Description**: Creates a blank node.

**Parameters**:
- `id` (string, optional) - Blank node identifier

**Returns**: BlankNode

**Example**:
```javascript
const terms = useTerms();
const bn = terms.blankNode();
const namedBn = terms.blankNode('b1');
```

---

#### quad

**Signature**: `quad(subject: Term, predicate: Term, object: Term, graph?: Term): Quad`

**Description**: Creates a quad (triple with optional graph).

**Parameters**:
- `subject` (Term) - Subject term
- `predicate` (Term) - Predicate term
- `object` (Term) - Object term
- `graph` (Term, optional) - Graph term

**Returns**: Quad

**Example**:
```javascript
const terms = useTerms();

const q = terms.quad(
  terms.namedNode('http://example.org/alice'),
  terms.namedNode('http://xmlns.com/foaf/0.1/name'),
  terms.literal('Alice')
);
```

---

**Complete Example**:
```javascript
import { useTerms, useGraph } from 'unrdf/composables';

const terms = useTerms();
const graph = useGraph();

// Create terms
const alice = terms.namedNode('http://example.org/alice');
const foafName = terms.namedNode('http://xmlns.com/foaf/0.1/name');
const name = terms.literal('Alice');

// Create and add quad
const triple = terms.quad(alice, foafName, name);
graph.add(triple);
```

**See Also**: [useGraph](#usegraph), [DataFactory](./core-rdf-api.md#datafactory)

**Since**: v4.1.1

---

### usePrefixes

**Signature**: `usePrefixes(): PrefixesAPI`

**Description**: Composable for namespace and prefix management.

**Returns**: PrefixesAPI object with methods for managing namespace prefixes

**Example**:
```javascript
import { usePrefixes } from 'unrdf/composables';

const prefixes = usePrefixes();

// Add prefix
prefixes.add('ex', 'http://example.org/');

// Expand prefixed name
const iri = prefixes.expand('ex:alice');
console.log(iri); // 'http://example.org/alice'

// Compress IRI
const prefixed = prefixes.compress('http://example.org/alice');
console.log(prefixed); // 'ex:alice'
```

**See Also**: [useTerms](#useterms)

**Since**: v4.1.1

---

## Validation & Reasoning

### useValidator

**Signature**: `useValidator(): ValidatorAPI`

**Description**: Composable for SHACL validation operations.

**Returns**: ValidatorAPI object with methods:

#### validate

**Signature**: `validate(store: Store, shapes: Store | string, options?: Object): ValidationReport`

**Description**: Validates a store against SHACL shapes.

**Parameters**:
- `store` (Store) - Store to validate
- `shapes` (Store | string) - SHACL shapes
- `options` (Object, optional) - Validation options

**Returns**: Validation report

**Example**:
```javascript
import { useValidator, useGraph } from 'unrdf/composables';

const graph = useGraph();
const validator = useValidator();

// Add data
graph.add(/* ... */);

// Validate
const shapes = `
  @prefix sh: <http://www.w3.org/ns/shacl#> .
  @prefix ex: <http://example.org/> .

  ex:PersonShape a sh:NodeShape ;
    sh:targetClass ex:Person ;
    sh:property [
      sh:path ex:name ;
      sh:minCount 1
    ] .
`;

const report = validator.validate(graph.store, shapes);
console.log('Conforms:', report.conforms);
```

**See Also**: [validateShacl](./core-rdf-api.md#validateshacl)

**Since**: v4.1.1

---

### useReasoner

**Signature**: `useReasoner(): ReasonerAPI`

**Description**: Composable for N3 reasoning operations.

**Returns**: ReasonerAPI object with methods:

#### reason

**Signature**: `reason(store: Store, rules: Store | string, options?: Object): Promise<Store>`

**Description**: Executes N3 reasoning on a store.

**Parameters**:
- `store` (Store) - Store to reason over
- `rules` (Store | string) - N3 rules
- `options` (Object, optional) - Reasoning options

**Returns**: Promise resolving to reasoned Store

**Example**:
```javascript
import { useReasoner, useGraph } from 'unrdf/composables';

const graph = useGraph();
const reasoner = useReasoner();

// Add data
graph.add(/* ... */);

// Apply rules
const rules = `
  @prefix ex: <http://example.org/> .
  { ?x ex:parent ?y } => { ?y ex:child ?x } .
`;

const reasoned = await reasoner.reason(graph.store, rules);
console.log('Reasoned store has', reasoned.size, 'quads');
```

**See Also**: [reason](./core-rdf-api.md#reason)

**Since**: v4.1.1

---

### useCanon

**Signature**: `useCanon(): CanonAPI`

**Description**: Composable for RDF canonicalization operations.

**Returns**: CanonAPI object with methods:

#### canonicalize

**Signature**: `canonicalize(store: Store, options?: Object): Promise<string>`

**Description**: Canonicalizes a store.

**Parameters**:
- `store` (Store) - Store to canonicalize
- `options` (Object, optional) - Canonicalization options

**Returns**: Promise resolving to canonical N-Quads string

**Example**:
```javascript
import { useCanon, useGraph } from 'unrdf/composables';

const graph = useGraph();
const canon = useCanon();

// Add data with blank nodes
graph.add(/* ... */);

// Canonicalize
const canonical = await canon.canonicalize(graph.store);
console.log('Canonical form:', canonical);
```

---

#### isIsomorphic

**Signature**: `isIsomorphic(store1: Store, store2: Store): Promise<boolean>`

**Description**: Checks if two stores are isomorphic.

**Parameters**:
- `store1` (Store) - First store
- `store2` (Store) - Second store

**Returns**: Promise resolving to true if isomorphic

**Example**:
```javascript
const canon = useCanon();

const iso = await canon.isIsomorphic(store1, store2);
console.log('Stores are isomorphic:', iso);
```

**See Also**: [canonicalize](./core-rdf-api.md#canonicalize), [isIsomorphic](./core-rdf-api.md#isisomorphic)

**Since**: v4.1.1

---

### useZod

**Signature**: `useZod(): ZodAPI`

**Description**: Composable for runtime Zod validation of RDF data.

**Returns**: ZodAPI object with Zod schema validators

**Example**:
```javascript
import { useZod } from 'unrdf/composables';

const zod = useZod();

// Validate hook definition
const validation = zod.validateKnowledgeHook(hookDef);
if (!validation.success) {
  console.error('Validation errors:', validation.errors);
}
```

**See Also**: [Schemas Reference](./schemas.md)

**Since**: v4.1.1

---

## Change Tracking

### useDelta

**Signature**: `useDelta(): DeltaAPI`

**Description**: Composable for tracking and computing graph changes (deltas).

**Returns**: DeltaAPI object with methods:

#### compute

**Signature**: `compute(storeBefore: Store, storeAfter: Store): Delta`

**Description**: Computes the delta between two stores.

**Parameters**:
- `storeBefore` (Store) - Store before changes
- `storeAfter` (Store) - Store after changes

**Returns**: Delta object with:
- `additions` (Quad[]) - Quads added
- `removals` (Quad[]) - Quads removed

**Example**:
```javascript
import { useDelta, Store } from 'unrdf/composables';

const delta = useDelta();

const before = new Store();
const after = new Store();

// ... modify stores

const changes = delta.compute(before, after);
console.log('Added:', changes.additions.length);
console.log('Removed:', changes.removals.length);
```

---

#### apply

**Signature**: `apply(store: Store, delta: Delta): Store`

**Description**: Applies a delta to a store.

**Parameters**:
- `store` (Store) - Store to modify
- `delta` (Delta) - Delta to apply

**Returns**: Modified Store

**Example**:
```javascript
const delta = useDelta();

const changes = {
  additions: [quad1, quad2],
  removals: [quad3]
};

delta.apply(store, changes);
console.log('Store updated');
```

**See Also**: [TransactionManager](./knowledge-hooks-api.md#transactionmanager)

**Since**: v4.1.1

---

## Related Documentation

- [Core RDF API Reference](./core-rdf-api.md) - Low-level RDF operations
- [Knowledge Hooks API Reference](./knowledge-hooks-api.md) - Hook system
- [Utilities API Reference](./utilities-api.md) - Utility functions
- [Getting Started Tutorial](../tutorials/01-getting-started.md) - Introduction to composables
