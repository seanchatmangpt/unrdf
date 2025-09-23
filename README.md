# unrdf

**Opinionated composable framework for RDF knowledge operations**

unrdf is the opinionated RDF framework for JavaScript. It makes the RDF universe accessible through a single composable surface — powered by N3.js, Comunica, SHACL, and Zod.

## Philosophy

**No TypeScript. Ever.** TypeScript is an illusion of safety that collapses at runtime. unrdf guarantees correctness at the only level that matters: execution.

**JSDoc is the source of truth.** Documentation, type hints, and developer experience are delivered directly via JSDoc, keeping the codebase minimal and expressive.

**Zod is the contract.** Runtime validation ensures that what you think your data is, and what it actually is, are always in sync.

**Composables everywhere.** Every aspect of RDF — graphs, queries, validation, reasoning, serialization — is accessible through consistent composable functions.

## Installation

```bash
pnpm add unrdf
```

## Quick Start

```javascript
import { useStore, useGraph, useValidator, useZod } from 'unrdf';

// Create a store
const store = useStore();

// Add some data
const quad = store.quad(
  store.namedNode("http://example.org/Person"),
  store.namedNode("http://xmlns.com/foaf/0.1/name"),
  store.literal("John Doe")
);
store.add(quad);

// Create a graph interface
const graph = useGraph(store.store);

// Query with SPARQL
const results = await graph.select(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name WHERE {
    ?person foaf:name ?name .
  }
`);

// Validate with Zod
const zod = useZod();
const PersonSchema = z.object({
  name: z.string()
});

const validation = await zod.validateResults(results, PersonSchema);
console.log(validation.validated); // [{ name: "John Doe" }]
```

## Core Composables

### useStore
The foundation of all unrdf operations. Creates and manages N3.Store instances.

```javascript
const store = useStore();
store.add(quad);
const stats = store.stats();
const turtle = await store.serialize();
```

### useGraph
High-level RDF operations including SPARQL queries and set operations.

```javascript
const graph = useGraph(store.store);
const results = await graph.select("SELECT ?s ?p ?o WHERE { ?s ?p ?o }");
const exists = await graph.ask("ASK WHERE { ?s a ex:Person }");
```

### useValidator
SHACL validation for RDF graphs.

```javascript
const validator = useValidator();
const report = await validator.validate(dataStore, shapesStore);
await validator.validateOrThrow(dataStore, shapesStore);
```

### useReasoner
EYE-based reasoning over RDF data.

```javascript
const reasoner = useReasoner();
const inferred = await reasoner.reason(dataStore, rulesStore);
```

### useCanon
Canonicalization and isomorphism checking using URDNA2015.

```javascript
const canon = useCanon();
const canonical = await canon.canonicalize(store);
const isIsomorphic = await canon.isIsomorphic(store1, store2);
```

### useZod
Runtime validation for RDF-derived data.

```javascript
const zod = useZod();
const validation = await zod.validateResults(sparqlResults, schema);
```

### useTurtle
File system operations for Turtle files.

```javascript
const turtle = await useTurtle('./graph');
await turtle.loadAll();
await turtle.save('my-graph', store);
```

## Utilities

unrdf includes comprehensive utility functions that cover the 80/20 "dark matter" of RDF operations:

### Term Utilities
```javascript
import { asNamedNode, asLiteral, asBlankNode, quadToJSON, jsonToQuad } from 'unrdf';

const node = asNamedNode("http://example.org/foo");
const lit = asLiteral("hello");
const bnode = asBlankNode();
const json = quadToJSON(quad);
const quad = jsonToQuad(json);
```

### Graph Utilities
```javascript
import { pluck, indexByPredicate, getSubjectsByType, hasType, getOne, getAll } from 'unrdf';

const labels = pluck(store, "http://www.w3.org/2000/01/rdf-schema#label");
const labelMap = indexByPredicate(store, "http://www.w3.org/2000/01/rdf-schema#label");
const persons = getSubjectsByType(store, "http://xmlns.com/foaf/0.1/Person");
const isPerson = hasType(store, "http://example.org/foo", "http://xmlns.com/foaf/0.1/Person");
const name = getOne(store, "http://example.org/foo", "http://xmlns.com/foaf/0.1/name");
const names = getAll(store, "http://example.org/foo", "http://xmlns.com/foaf/0.1/name");
```

### Serialization Utilities
```javascript
import { debugTurtle, debugNQuads, debugJSONLD } from 'unrdf';

const turtle = await debugTurtle(store);
const nquads = await debugNQuads(store);
const jsonld = await debugJSONLD(store);
```

### Validation Utilities
```javascript
import { validateIRI, validateTerm, validateQuad, validateStore } from 'unrdf';

const isValidIRI = validateIRI("http://example.org/foo");
const isValidTerm = validateTerm(term);
const isValidQuad = validateQuad(quad);
const result = validateStore(store);
```

## Opinionated Design

unrdf enforces a single, opinionated path through the RDF universe:

- **One Store**: N3.Store is the only memory model
- **One Query Engine**: Comunica is the only SPARQL engine
- **One Validator**: SHACL is the only validation method
- **One Reasoner**: EYE is the only reasoning engine
- **One Canonicalization**: URDNA2015 is the only canonicalization method
- **One Serialization**: Turtle and N-Quads are the primary formats
- **One Validation**: Zod is the only runtime validation

This eliminates choice paralysis and ensures consistency across all RDF operations.

## Why unrdf?

The RDF ecosystem has matured into a diverse set of libraries, but this diversity has created fragmentation. A typical project may mix N3 for parsing, Comunica for SPARQL, rdf-ext for datasets, rdf-validate-shacl for constraints, and eyereasoner for inference. Each library is useful in isolation, but together they form a patchwork of styles, APIs, and stores.

unrdf addresses this by enforcing a single opinionated path. The framework selects a canonical implementation for each layer and wraps them in a composable API pattern. The result is not a new ontology language or reasoner but a reduction of cognitive overhead for practitioners.

## License

MIT

## Contributing

This project follows the opinionated design philosophy. Contributions should align with the single-path approach and maintain the composable API pattern.