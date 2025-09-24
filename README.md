# unrdf

**Opinionated composable framework for RDF knowledge operations**

unrdf is the opinionated RDF framework for JavaScript. It makes the RDF universe accessible through a single composable surface — powered by N3.js, Comunica, SHACL, and Zod.

## Philosophy

**No TypeScript. Ever.** TypeScript is an illusion of safety that collapses at runtime. unrdf guarantees correctness at the only level that matters: execution.

**JSDoc is the source of truth.** Documentation, type hints, and developer experience are delivered directly via JSDoc, keeping the codebase minimal and expressive.

**Zod is the contract.** Runtime validation ensures that what you think your data is, and what it actually is, are always in sync.

**Context is everything.** unrdf enforces the "One Store Rule" through a global context system that ensures all composables share the same RDF engine and store instance.

**Composables everywhere.** Every aspect of RDF — graphs, queries, validation, reasoning, serialization — is accessible through consistent composable functions.

## Installation

```bash
pnpm add unrdf
```

## Quick Start

### Context-Based Architecture (The unrdf Way)

unrdf uses [unctx](https://github.com/unjs/unctx) for global store management, ensuring there's only one store by default:

```javascript
import { initStore, useStore, useGraph, useValidator, useZod } from 'unrdf';

// Initialize the store context at the root of your application
const runApp = initStore([], { baseIRI: 'http://example.org/' });

runApp(() => {
  // All composables now share the same store automatically
  const store = useStore();
  const graph = useGraph();
  const validator = useValidator();
  const zod = useZod();
  
  // Create RDF data
  const subject = store.namedNode('http://example.org/person1');
  const predicate = store.namedNode('http://example.org/name');
  const object = store.literal('John Doe');
  const quad = store.quad(subject, predicate, object);
  
  // Add to the shared store
  store.add(quad);
  
  // Query the data
  const results = await graph.select(`
    PREFIX ex: <http://example.org/>
    SELECT ?s ?p ?o WHERE { ?s ?p ?o }
  `);
  
  console.log('Query results:', results);
  
  // Validate with SHACL
  const report = await validator.validate(shapesStore);
  console.log('Validation report:', report);
  
  // Type-safe validation with Zod
  const PersonSchema = z.object({
    id: z.string().url(),
    name: z.string(),
    age: z.number().int().min(0)
  });
  
  const validation = await zod.validateResults(results, PersonSchema);
  console.log('Zod validation:', validation);
});
```

### CLI Usage

```bash
# Install globally
pnpm install -g unrdf

# Initialize a new project
unrdf init my-knowledge-graph

# Parse RDF data
unrdf parse data.ttl --stats

# Query with SPARQL  
unrdf query data.ttl --query "SELECT * WHERE { ?s ?p ?o } LIMIT 10"

# Validate against SHACL shapes
unrdf validate data.ttl shapes.ttl

# Convert between formats
unrdf convert data.ttl --to json-ld
```

## Core Composables

### Context Management

#### initStore
Initialize the global store context for your application.

```javascript
import { initStore } from 'unrdf';

// Initialize with empty store
const runApp = initStore();

// Initialize with existing data
const runApp = initStore(quads, { baseIRI: 'http://example.org/' });

// Run your application code
runApp(() => {
  // All composables share the same context here
  const store = useStore();
  const graph = useGraph();
  // ... rest of your code
});
```

#### useStore
Access the shared store instance from context.

```javascript
const store = useStore();

// Add quads
store.add(quad);

// Get statistics
const stats = store.stats();

// Serialize to Turtle
const turtle = await store.serialize();
```

### RDF Operations

#### useTerms
RDF term creation and manipulation. Enforces the "One Terms Rule" - N3 DataFactory is the only term creation method.

```javascript
const terms = useTerms();
const subject = terms.iri("http://example.org/person");
const name = terms.lit("John Doe");
const age = terms.lit(30, "http://www.w3.org/2001/XMLSchema#integer");
const bnode = terms.bnode("person1");
const statement = terms.quad(subject, terms.iri("http://example.org/name"), name);
```

#### usePrefixes
Prefix management and CURIE operations. Enforces the "One Prefix Rule" - centralized prefix management.

```javascript
const prefixes = usePrefixes({
  "ex": "http://example.org/",
  "foaf": "http://xmlns.com/foaf/0.1/"
});

// Register new prefixes
prefixes.register({ "dc": "http://purl.org/dc/terms/" });

// Expand CURIEs
const fullIRI = prefixes.expand("ex:Person");

// Shrink IRIs
const curie = prefixes.shrink("http://example.org/Person");

// List all prefixes
const allPrefixes = prefixes.list();
```

#### useLists
RDF list operations for reading and writing linked lists in RDF. Enforces the "One List Rule" - standard rdf:List format.

```javascript
const lists = useLists();

// Read a list
const items = lists.read(store, listHead);

// Write a list
const head = lists.write(store, ["item1", "item2", "item3"]);

// Convert to strings
const strings = lists.toStrings(store, listHead);

// Create from strings
const head = lists.fromStrings(store, ["item1", "item2", "item3"]);
```

### Query & Reasoning

#### useGraph
High-level RDF operations including SPARQL queries and set operations.

```javascript
const graph = useGraph();

// SPARQL SELECT queries
const results = await graph.select(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name WHERE {
    ?person foaf:name ?name .
  }
`);

// SPARQL ASK queries
const exists = await graph.ask(`
  PREFIX ex: <http://example.org/>
  ASK WHERE { ?s a ex:Person }
`);

// Get store statistics
const stats = graph.getStats();
```

#### useReasoner
EYE-based reasoning over RDF data.

```javascript
const reasoner = useReasoner();

// Reason over data with rules
const inferred = await reasoner.reason(dataStore, rulesStore);

// Check if reasoning would produce new triples
const wouldProduceNew = await reasoner.wouldProduceNewTriples(dataStore, rulesStore);

// Get reasoning statistics
const stats = reasoner.getStats(originalStore, inferredStore);
```

### Validation & Canonicalization

#### useValidator
SHACL validation for RDF graphs.

```javascript
const validator = useValidator();

// Validate against SHACL shapes
const report = await validator.validate(shapesStore);

// Validate and throw on failure
await validator.validateOrThrow(shapesTurtle);

// Summarize validation results
const summary = validator.summarize(report);

// Filter by severity
const violations = validator.filterBySeverity(report, "http://www.w3.org/ns/shacl#Violation");
```

#### useCanon
Canonicalization and isomorphism checking using URDNA2015.

```javascript
const canon = useCanon();

// Canonicalize a store
const canonical = await canon.canonicalize(store);

// Check if two stores are isomorphic
const isIsomorphic = await canon.isIsomorphic(store1, store2);

// Generate canonical hash
const hash = await canon.hash(store);
```

#### useZod
Runtime validation for RDF-derived data.

```javascript
const zod = useZod();

const PersonSchema = z.object({
  name: z.string(),
  age: z.number().int().min(0)
});

const validation = await zod.validateResults(sparqlResults, PersonSchema);
console.log(validation.validated); // [{ name: "John Doe", age: 30 }]
```

### I/O Operations

#### useTurtleFS
File system operations for Turtle files.

```javascript
const turtleFS = await useTurtleFS('./graph');

// Load all .ttl files
await turtleFS.loadAll();

// Save a specific graph
await turtleFS.save('my-graph', store);

// Load a specific file
const store = await turtleFS.load('my-graph');

// List all files
const files = await turtleFS.list();
```

#### useNQuads
N-Quads parsing and serialization. Enforces the "One N-Quads Rule" - standard N-Quads format only.

```javascript
const nquads = useNQuads();

// Parse N-Quads
const store = nquads.parse(nquadsString);

// Serialize to N-Quads
const nquadsString = await nquads.serialize(store);

// Validate N-Quads
const validation = nquads.validate(nquadsString);

// Convert to Turtle
const turtle = await nquads.toTurtle(nquadsString);
```

### Graph Traversal

#### usePointer
Clownface-based graph traversal. Enforces the "One Pointer Rule" - Clownface is the only traversal method.

```javascript
const pointer = usePointer();

// Get pointer to specific node
const nodePointer = pointer.node("person1");

// Get nodes of specific type
const persons = pointer.ofType("foaf:Person");

// Get nodes with specific property
const namedNodes = pointer.withProperty("foaf:name");

// Get nodes with specific property value
const johnNodes = pointer.withValue("foaf:name", "John Doe");

// Get underlying Clownface instance
const clownface = pointer.getClownface();
```

### Utility Composables

#### useIRIs
IRI resolution and management.

```javascript
const iris = useIRIs();

// Resolve relative IRIs
const absolute = iris.resolve("person1", "http://example.org/");

// Map prefixes to paths
iris.map("ex", "/api/");

// Check if URI is absolute
const isAbsolute = iris.isAbsolute("http://example.org/foo");
```

#### useCache
Caching for expensive operations.

```javascript
const cache = useCache();

// Cache function results
const cachedFunction = cache.wrap(expensiveFunction);

// Set and get cached values
cache.set("key", value, { ttl: 60000 });
const value = cache.get("key");

// Get cache statistics
const stats = cache.getStats();
```

#### useMetrics
Performance metrics and timing.

```javascript
const metrics = useMetrics();

// Wrap functions with metrics
const timedFunction = metrics.wrap(myFunction, "operation-name");

// Create timers
const timer = metrics.timer("operation");
// ... do work
timer.end();

// Get metrics
const allMetrics = metrics.getAll();
const summary = metrics.getSummary();
```

#### useDelta
Change tracking and diff operations.

```javascript
const delta = useDelta();

// Calculate difference between stores
const changes = delta.diff(store1, store2);

// Apply changes to a store
const patchedStore = delta.patch(store, changes);

// Get statistics about changes
const stats = delta.getStats(changes);
```

## The Context Architecture

unrdf enforces the "One Store Rule" through a sophisticated context system:

### How It Works

1. **Initialize Context**: `initStore()` creates a global context with a single RDF engine and store
2. **Run Application**: `runApp()` executes your code within the context
3. **Access Composables**: All composables automatically use the shared context
4. **Consistent State**: Every operation works on the same store instance

### Benefits

- **No Store Confusion**: Impossible to accidentally work with different stores
- **Automatic Sharing**: All composables share the same engine configuration
- **Clean APIs**: No need to pass stores between composables
- **Testable**: Easy to isolate tests with fresh contexts

### Example: Multiple Composables Working Together

```javascript
const runApp = initStore([], { baseIRI: 'http://example.org/' });

runApp(async () => {
  // All these composables share the same store automatically
  const store = useStore();
  const graph = useGraph();
  const validator = useValidator();
  const reasoner = useReasoner();
  const canon = useCanon();
  
  // Load data
  const turtleFS = await useTurtleFS('./data');
  await turtleFS.loadAll();
  
  // Query the data
  const results = await graph.select(`
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT ?person ?name WHERE {
      ?person a foaf:Person ;
               foaf:name ?name .
    }
  `);
  
  // Validate against shapes
  const report = await validator.validate(shapesStore);
  
  // Reason over the data
  const inferred = await reasoner.reason(null, rulesStore);
  
  // Canonicalize for comparison
  const canonical = await canon.canonicalize(store);
  
  console.log('All operations completed on the same store!');
});
```

## Opinionated Design

unrdf enforces a single, opinionated path through the RDF universe:

- **One Store**: N3.Store is the only memory model, managed through context
- **One Engine**: Single RdfEngine instance shared across all composables
- **One Terms**: N3 DataFactory is the only term creation method
- **One Prefixes**: Centralized prefix management
- **One Lists**: Standard rdf:List format
- **One Query Engine**: Comunica is the only SPARQL engine
- **One Validator**: SHACL is the only validation method
- **One Reasoner**: EYE is the only reasoning engine
- **One Canonicalization**: URDNA2015 is the only canonicalization method
- **One Serialization**: Turtle and N-Quads are the primary formats
- **One Pointer**: Clownface is the only traversal method
- **One Validation**: Zod is the only runtime validation
- **One Context**: Global context system ensures consistency

This eliminates choice paralysis and ensures consistency across all RDF operations.

## Why unrdf?

The RDF ecosystem has matured into a diverse set of libraries, but this diversity has created fragmentation. A typical project may mix N3 for parsing, Comunica for SPARQL, rdf-ext for datasets, rdf-validate-shacl for constraints, and eyereasoner for inference. Each library is useful in isolation, but together they form a patchwork of styles, APIs, and stores.

unrdf addresses this by enforcing a single opinionated path with a context-based architecture. The framework selects a canonical implementation for each layer, wraps them in a composable API pattern, and ensures they all work together through a shared context system. The result is not a new ontology language or reasoner but a reduction of cognitive overhead for practitioners.

## Migration from v0.x

If you're upgrading from unrdf v0.x, the main change is the introduction of the context system:

### Before (v0.x)
```javascript
const store = useStore();
const graph = useGraph(store.store);
const validator = useValidator();
```

### After (v1.0)
```javascript
const runApp = initStore();
runApp(() => {
  const store = useStore();
  const graph = useGraph();
  const validator = useValidator();
});
```

The context system ensures all composables work together seamlessly while maintaining the same API surface.

## License

MIT

## Contributing

This project follows the opinionated design philosophy. Contributions should align with the single-path approach, maintain the composable API pattern, and respect the context-based architecture.