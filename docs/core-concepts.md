# Core Concepts

Understanding unrdf's philosophy and design principles.

## The "Reactive Knowledge Graphs" Philosophy

unrdf is built on the principle of **reactive knowledge graphs**. Instead of providing multiple options and configurations, it makes one choice for each concern and sticks to it. This eliminates the "dark matter" of RDF development—the 80% of boilerplate glue code that developers typically write.

**🎯 Knowledge Hooks** are the crown jewel of this philosophy - they transform static knowledge graphs into intelligent, reactive systems that respond to changes with deterministic, auditable actions.

### Why Opinionated?

RDF development is plagued by choice paralysis and "dark matter" - the 80% of boilerplate glue code needed to connect different libraries:

- Which RDF store to use? (rdf-ext, N3, rdflib, etc.)
- Which query engine? (Comunica, SPARQL.js, etc.)
- Which validator? (SHACL, ShEx, custom)
- Which reasoner? (EYE, OWL reasoners, custom)
- Which serialization format? (Turtle, JSON-LD, RDF/XML, etc.)
- **How to build reactive triggers?** (Custom event systems, polling, etc.)

unrdf eliminates these choices by making one decision for each concern, with **Knowledge Hooks** as the crown jewel:

| Concern | unrdf's Choice | Why |
|---------|----------------|-----|
| **🎯 Knowledge Hooks** | Built-in reactive triggers | Eliminates custom event systems, provides cryptographic provenance |
| **Store** | N3.Store | Fast, memory-efficient, well-maintained |
| **Query Engine** | @comunica/query-sparql | Standards-compliant, performant |
| **Validator** | rdf-validate-shacl | Industry standard, comprehensive |
| **Reasoner** | eyereasoner | N3 rules, fast, reliable |
| **Canonization** | rdf-canonize (URDNA2015) | W3C standard, deterministic |
| **JSON-LD** | jsonld | Normalized to N3 internally |
| **Traversal** | Clownface | Bound to N3.Store, intuitive API |
| **Config** | Turtle only | Human-readable, RDF-native |

## Knowledge Hooks Philosophy

**Knowledge Hooks** represent the pinnacle of unrdf's opinionated design. They solve the fundamental problem of RDF reactivity:

### The Problem
Traditional RDF applications are static - they require external event systems, polling mechanisms, or custom trigger logic to respond to changes. This creates:

- **Fragmented architectures** with event buses, message queues, or polling loops
- **Lost provenance** - changes aren't cryptographically tracked
- **Complex state management** - maintaining change history and audit trails
- **Inconsistent behavior** - different systems handle changes differently

### The Solution
Knowledge Hooks provide a **unified, declarative approach** to RDF reactivity:

```javascript
// Define what to monitor and how to respond
const healthHook = defineHook({
  id: 'ex:ServiceHealthMonitor',
  select: 'SELECT ?service ?errorRate WHERE { ?service ex:errorRate ?errorRate }',
  predicates: [
    { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.02 } }
  ],
  combine: 'OR'
});

// Get cryptographically signed receipts for all evaluations
const receipt = await evaluateHook(healthHook);
```

### Key Principles

1. **Declarative Triggers**: Define *what* to monitor, not *how* to monitor it
2. **Cryptographic Provenance**: Every evaluation is signed and auditable
3. **Pure Functions**: Hooks are deterministic and side-effect free
4. **Composable Logic**: Complex behaviors through simple predicate combination
5. **Performance First**: Sub-millisecond evaluation with optimized SPARQL

## Dual API Architecture

unrdf provides **two complementary APIs** that work together:

### 🎯 Knowledge Hooks (Primary API)
The primary interface for **reactive knowledge graphs**:

```javascript
// Define reactive triggers declaratively
const healthHook = defineHook({
  id: 'ex:ServiceHealthMonitor',
  select: 'SELECT ?service ?errorRate WHERE { ?service ex:errorRate ?errorRate }',
  predicates: [
    { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.02 } }
  ]
});

// Get cryptographically signed receipts
const receipt = await evaluateHook(healthHook);
```

### Foundation Composables (Secondary API)
The underlying composable functions that power Knowledge Hooks:

```javascript
// Each composable has a single responsibility
const store = useStore();           // Store management
const turtle = useTurtle();         // Turtle I/O
const graph = useGraph(store);      // SPARQL operations
const validator = useValidator();   // SHACL validation
const reasoner = useReasoner();     // N3 reasoning
const canon = useCanon();           // Canonicalization
const jsonld = useJsonLd();         // JSON-LD operations
const pointer = usePointer(store);  // Graph traversal
```

### Benefits of Dual Architecture

1. **Knowledge Hooks**: High-level, declarative reactive programming
2. **Composables**: Low-level, granular control when needed
3. **Composability**: Mix and match as needed
4. **Testability**: Easy to test in isolation
5. **Maintainability**: Clear boundaries and interfaces

## The "Dark Matter" Problem

RDF development typically involves writing a lot of boilerplate code:

### Traditional Approach (80% "Dark Matter")
```javascript
// Traditional RDF development - lots of "dark matter"
import { Store } from 'n3';
import { QueryEngine } from '@comunica/query-sparql';
import { Parser } from 'n3';
import { Writer } from 'n3';
import { validate } from 'rdf-validate-shacl';
import { reason } from 'eyereasoner';
import { canonize } from 'rdf-canonize';
import { expand, compact } from 'jsonld';
import { cf } from '@zazuko/env';

// 80% of your code is glue between these libraries
const store = new Store();
const engine = new QueryEngine();
const parser = new Parser();
const writer = new Writer();

// Plus custom event system for reactivity...
class RDFEventBus {
  constructor() { /* complex event handling */ }
  onChange(callback) { /* polling logic */ }
}
// ... more setup code
```

### unrdf Solution (90% Less "Dark Matter")
unrdf eliminates this by providing a unified interface with built-in reactivity:

```javascript
// unrdf - minimal "dark matter" with built-in reactivity
import { initStore, defineHook, evaluateHook } from 'unrdf';

// Initialize context once
const runApp = initStore();

// Define reactive hooks declaratively
const healthHook = defineHook({
  id: 'ex:ServiceHealthMonitor',
  select: 'SELECT ?service ?errorRate WHERE { ?service ex:errorRate ?errorRate }',
  predicates: [
    { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.02 } }
  ]
});

// That's it - everything just works together with cryptographic provenance
const receipt = await evaluateHook(healthHook);
```

## Type Safety Strategy

unrdf uses a two-tier type safety approach:

### 1. JSDoc for Development
```javascript
/**
 * Create a terms composable for RDF term creation
 * @param {Object} [options] - Terms options
 * @param {string} [options.baseIRI] - Base IRI for relative IRIs
 * @returns {Object} Terms creation interface
 */
export function useTerms(options = {}) {
  // Implementation
}
```

### 2. Zod for Runtime Validation
```javascript
import { useZod } from 'unrdf';
import { z } from 'zod';

const zod = useZod();
const PersonSchema = z.object({
  name: z.string(),
  age: z.number().optional()
});

// Runtime validation at application boundaries
const result = zod.validate(PersonSchema, userInput);
```

### Why This Approach?

- **JSDoc**: Provides IDE support and documentation without TypeScript complexity
- **Zod**: Ensures runtime safety and data validation
- **No TypeScript**: Avoids compilation complexity and type system overhead

## Memory Model: N3.Store Only

unrdf uses N3.Store as the single memory model for all RDF data:

```javascript
// All data flows through N3.Store
const store = useStore();
const turtle = useTurtle();
const jsonld = useJsonLd();

// Turtle → N3.Store
const store1 = await turtle.parse(turtleData);

// JSON-LD → N3.Store
const store2 = await jsonld.fromJSONLD(jsonldData);

// N3.Store → JSON-LD
const jsonldData = await jsonld.toJSONLD(store1);
```

### Benefits

- **Consistency**: One data structure for all operations
- **Performance**: N3.Store is optimized for RDF operations
- **Compatibility**: Works with all RDF libraries that support N3
- **Simplicity**: No need to convert between different data structures

## Query Engine: Comunica Only

All SPARQL operations use @comunica/query-sparql:

```javascript
const graph = useGraph(store);

// All these use Comunica internally
const selectResults = await graph.select('SELECT * WHERE { ?s ?p ?o }');
const askResult = await graph.ask('ASK { ?s a ex:Person }');
const constructResult = await graph.construct('CONSTRUCT { ?s a ex:Person } WHERE { ?s a ex:Person }');
const updateResult = await graph.update('INSERT { ex:new a ex:Person } WHERE {}');
```

### Why Comunica?

- **Standards Compliant**: Implements SPARQL 1.1 fully
- **Performance**: Optimized query execution
- **Maintenance**: Actively maintained and updated
- **Features**: Supports all SPARQL 1.1 features

## Validation: SHACL Only

All validation uses rdf-validate-shacl:

```javascript
const validator = useValidator();

// SHACL shapes
const shapes = `
  @prefix sh: <http://www.w3.org/ns/shacl#> .
  ex:PersonShape a sh:NodeShape ;
    sh:targetClass ex:Person ;
    sh:property [
      sh:path ex:name ;
      sh:minCount 1
    ] .
`;

const validation = await validator.validate(store, shapes);
```

### Why SHACL?

- **Industry Standard**: W3C recommendation
- **Comprehensive**: Supports complex validation rules
- **RDF Native**: Works directly with RDF data
- **Tooling**: Good ecosystem support

## Reasoning: EYE Only

All reasoning operations use eyereasoner:

```javascript
const reasoner = useReasoner();

// N3 rules
const rules = `
  @prefix ex: <http://example.org/> .
  { ?person ex:parent ?parent } => { ?parent ex:child ?person } .
`;

const reasonedStore = await reasoner.reason(store, rules);
```

### Why EYE?

- **N3 Rules**: Supports N3 rule language
- **Performance**: Fast reasoning engine
- **Reliability**: Well-tested and stable
- **Integration**: Works well with N3.Store

## Canonicalization: URDNA2015 Only

All canonicalization uses rdf-canonize (URDNA2015):

```javascript
const canon = useCanon();

// Canonicalize a store
const canonicalStore = await canon.canonicalize(store);

// Check if two stores are isomorphic
const areIsomorphic = await canon.isomorphic(store1, store2);
```

### Why URDNA2015?

- **W3C Standard**: Official W3C recommendation
- **Deterministic**: Always produces the same result
- **Interoperable**: Works across different implementations
- **Reliable**: Well-specified algorithm

## JSON-LD: Normalized to N3

JSON-LD operations are normalized to N3.Store:

```javascript
const jsonld = useJsonLd();

// JSON-LD → N3.Store → JSON-LD
const store = await jsonld.fromJSONLD(jsonldData);
const normalizedJsonld = await jsonld.toJSONLD(store);
```

### Why This Approach?

- **Consistency**: All data flows through N3.Store
- **Performance**: N3.Store is optimized for RDF operations
- **Simplicity**: No need to manage multiple data structures
- **Reliability**: N3.Store is well-tested and stable

## Graph Traversal: Clownface Bound to N3.Store

All graph traversal uses Clownface, always bound to N3.Store:

```javascript
const pointer = usePointer(store);

// Clownface operations
const personPointer = pointer.node('ex:person');
const name = personPointer.out('foaf:name').value;
const friends = personPointer.out('foaf:knows').toArray();
```

### Why Clownface?

- **Intuitive API**: Easy to understand and use
- **Powerful**: Supports complex traversal patterns
- **Performance**: Optimized for graph operations
- **Integration**: Works seamlessly with N3.Store

## Configuration: Turtle Only

All configuration is done in Turtle format:

```javascript
// No JSON/YAML configs - just Turtle
const config = `
  @prefix unrdf: <http://unrdf.org/config#> .
  
  unrdf:config unrdf:baseIRI "http://example.org/" ;
    unrdf:defaultPrefix "ex" ;
    unrdf:strictMode true .
`;
```

### Why Turtle?

- **RDF Native**: Uses RDF syntax
- **Human Readable**: Easy to read and write
- **Extensible**: Can add custom properties
- **Consistent**: Same format as data

## Error Handling Strategy

unrdf uses a consistent error handling strategy:

1. **Validation Errors**: Use Zod for input validation
2. **RDF Errors**: Use SHACL for graph validation
3. **Runtime Errors**: Throw descriptive errors with context
4. **Recovery**: Provide graceful degradation where possible

```javascript
// Input validation with Zod
const result = zod.validate(PersonSchema, userInput);
if (!result.success) {
  throw new Error(`Invalid input: ${result.errors.join(', ')}`);
}

// RDF validation with SHACL
const validation = await validator.validate(store, shapes);
if (!validation.valid) {
  throw new Error(`RDF validation failed: ${validation.errors.join(', ')}`);
}

// Runtime errors with context
if (!store || typeof store.getQuads !== 'function') {
  throw new Error('[useGraph] Store is required and must have getQuads method');
}
```

## Performance Considerations

unrdf is designed for performance:

1. **Single Memory Model**: N3.Store is optimized for RDF operations
2. **Minimal Overhead**: Composables have minimal performance impact
3. **Caching**: Built-in caching for expensive operations
4. **Metrics**: Performance monitoring built-in
5. **Concurrency**: Supports concurrent operations

```javascript
// Performance monitoring
const metrics = useMetrics();
const wrappedOperation = metrics.wrap('expensive-operation', async () => {
  return await expensiveRDFOperation();
});

// Caching
const cache = useCache();
const result = await cache.get('query-result', async () => {
  return await graph.select('SELECT * WHERE { ?s ?p ?o }');
});
```

## Testing Strategy

unrdf includes comprehensive testing:

1. **Unit Tests**: Each composable tested in isolation
2. **Integration Tests**: End-to-end workflows
3. **Edge Case Tests**: Boundary conditions and error cases
4. **Performance Tests**: Benchmarking and optimization
5. **Concurrency Tests**: Multi-threaded operations

The test suite covers:
- Normal operation paths
- Error conditions
- Edge cases and boundary values
- Performance characteristics
- Concurrent operations
- Memory usage patterns

## Migration from Other RDF Libraries

unrdf is designed to be easy to migrate to:

1. **Gradual Migration**: Can be adopted incrementally
2. **Compatibility**: Works with existing RDF data
3. **Clear API**: Intuitive interface for RDF developers
4. **Documentation**: Comprehensive migration guides

See the [Migration Guide](./migration.md) for detailed instructions on moving from other RDF libraries.