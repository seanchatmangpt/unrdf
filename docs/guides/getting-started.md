# Getting Started with unrdf

This guide will help you get up and running with unrdf quickly. We'll cover installation, basic concepts, and your first RDF operations.

## Installation

unrdf is available as an npm package. Install it using your preferred package manager:

```bash
# Using pnpm (recommended)
pnpm add unrdf

# Using npm
npm install unrdf

# Using yarn
yarn add unrdf
```

## Basic Concepts

unrdf is built around a few core concepts:

### Composables
Composables are focused, single-responsibility functions that provide specific RDF operations. Each composable follows the "One Rule" principle - there's one canonical way to do each operation.

### The "One Rule" Philosophy
unrdf eliminates choice paralysis by enforcing a single opinionated path:

- **One Store**: N3.Store is the only memory model
- **One Terms**: N3 DataFactory is the only term creation method
- **One Query Engine**: Comunica is the only SPARQL engine
- **One Validator**: SHACL is the only validation method
- **One Reasoner**: EYE is the only reasoning engine

### JSDoc + Zod
Instead of TypeScript, unrdf uses JSDoc for type hints and Zod for runtime validation. This provides type safety where it matters - at runtime.

## Your First RDF Operation

Let's start with a simple example that creates some RDF data and queries it:

```javascript
import { useStore, useTerms, useGraph } from 'unrdf';

// Create a store
const store = useStore();

// Create terms
const terms = useTerms({
  baseIRI: "http://example.org/"
});

// Add some data
const person = terms.iri("person1");
const name = terms.lit("John Doe");
const age = terms.lit(30, "http://www.w3.org/2001/XMLSchema#integer");

// Create quads
const nameQuad = terms.quad(
  person,
  terms.iri("name"),
  name
);

const ageQuad = terms.quad(
  person,
  terms.iri("age"),
  age
);

// Add to store
store.add(nameQuad);
store.add(ageQuad);

// Query the data
const graph = useGraph(store.store);
const results = await graph.select(`
  PREFIX ex: <http://example.org/>
  SELECT ?name ?age WHERE {
    ?person ex:name ?name ;
            ex:age ?age .
  }
`);

console.log(results);
// Output: [{ name: "John Doe", age: 30 }]
```

## Working with Prefixes

Prefixes make IRIs more readable and manageable:

```javascript
import { usePrefixes } from 'unrdf';

// Create a prefix manager
const prefixes = usePrefixes({
  "ex": "http://example.org/",
  "foaf": "http://xmlns.com/foaf/0.1/"
});

// Register additional prefixes
prefixes.register({
  "dc": "http://purl.org/dc/terms/"
});

// Expand CURIEs to full IRIs
const fullIRI = prefixes.expand("ex:Person");
console.log(fullIRI); // "http://example.org/Person"

// Shrink full IRIs to CURIEs
const curie = prefixes.shrink("http://example.org/Person");
console.log(curie); // "ex:Person"
```

## Loading and Saving Data

unrdf provides several ways to load and save RDF data:

### Turtle Files

```javascript
import { useTurtle } from 'unrdf';

const turtle = useTurtle();

// Parse Turtle string
const store = await turtle.parse(`
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  
  ex:person1 a foaf:Person ;
    foaf:name "John Doe" ;
    foaf:age 30 .
`);

// Serialize to Turtle
const turtleString = await turtle.serialize(store);
console.log(turtleString);
```

### N-Quads Files

```javascript
import { useNQuads } from 'unrdf';

const nquads = useNQuads();

// Parse N-Quads string
const store = nquads.parse(`
  <http://example.org/person1> <http://xmlns.com/foaf/0.1/name> "John Doe" .
  <http://example.org/person1> <http://xmlns.com/foaf/0.1/age> "30"^^<http://www.w3.org/2001/XMLSchema#integer> .
`);

// Serialize to N-Quads
const nquadsString = await nquads.serialize(store);
console.log(nquadsString);
```

### File System Operations

```javascript
import { useTurtleFS } from 'unrdf';

// Create a Turtle file system interface
const turtleFS = await useTurtleFS('./data', {
  baseIRI: "http://example.org/",
  autoLoad: true
});

// Load all Turtle files in the directory
await turtleFS.loadAll();

// Save a store to a file
await turtleFS.save('my-data', store);

// Load a specific file
const loadedStore = await turtleFS.load('my-data.ttl');
```

## Working with Lists

RDF lists are a common pattern for ordered collections:

```javascript
import { useLists } from 'unrdf';
import { Store } from 'n3';

const store = new Store();
const lists = useLists(store);
const terms = useTerms();

// Create a list
const items = [
  terms.lit("apple"),
  terms.lit("banana"),
  terms.lit("cherry")
];

const listHead = lists.write(items);

// Read the list
const readItems = lists.read(listHead);
console.log(readItems); // [Literal("apple"), Literal("banana"), Literal("cherry")]

// Check if something is a list
const isList = lists.isList(listHead);
console.log(isList); // true

// Get list length
const length = lists.length(listHead);
console.log(length); // 3
```

## Graph Traversal with Clownface

Clownface provides a powerful way to traverse RDF graphs:

```javascript
import { usePointer } from 'unrdf';

const pointer = usePointer(store);

// Get a specific node
const personNode = pointer.node("ex:person1");

// Traverse properties
const name = personNode.out("foaf:name").value;
console.log(name); // "John Doe"

// Get all friends
const friends = personNode.out("foaf:knows").toArray();
console.log(friends);

// Find all people
const people = pointer.ofType("foaf:Person");
console.log(people);

// Find nodes with a specific property
const namedNodes = pointer.withProperty("foaf:name");
console.log(namedNodes);
```

## Validation with SHACL

SHACL (Shapes Constraint Language) provides a way to validate RDF data:

```javascript
import { useValidator } from 'unrdf';

const validator = useValidator();

// Define shapes (this would typically be loaded from a file)
const shapesStore = await turtle.parse(`
  @prefix sh: <http://www.w3.org/ns/shacl#> .
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  
  ex:PersonShape a sh:NodeShape ;
    sh:targetClass foaf:Person ;
    sh:property [
      sh:path foaf:name ;
      sh:datatype xsd:string ;
      sh:minCount 1 ;
      sh:maxCount 1
    ] ;
    sh:property [
      sh:path foaf:age ;
      sh:datatype xsd:integer ;
      sh:minInclusive 0
    ] .
`);

// Validate data
const report = await validator.validate(store, shapesStore);

if (report.conforms) {
  console.log("Data is valid!");
} else {
  console.log("Validation errors:", report.results);
}
```

## Reasoning with EYE

EYE is a logic-based reasoner that can infer new facts from existing data:

```javascript
import { useReasoner } from 'unrdf';

const reasoner = useReasoner();

// Define rules (this would typically be loaded from a file)
const rulesStore = await turtle.parse(`
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  
  { ?person foaf:age ?age . ?age > 18 } => { ?person ex:isAdult true } .
`);

// Perform reasoning
const inferredStore = await reasoner.reason(store, rulesStore);

// The inferred store now contains additional facts
console.log("Original quads:", store.size());
console.log("Inferred quads:", inferredStore.size());
```

## Runtime Validation with Zod

Zod provides runtime validation for data derived from RDF:

```javascript
import { useZod } from 'unrdf';
import { z } from 'zod';

const zod = useZod();

// Define a schema
const PersonSchema = z.object({
  name: z.string(),
  age: z.number().min(0).max(150),
  email: z.string().email().optional()
});

// Query for person data
const results = await graph.select(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name ?age ?email WHERE {
    ?person foaf:name ?name ;
            foaf:age ?age .
    OPTIONAL { ?person foaf:mbox ?email }
  }
`);

// Validate the results
const validation = await zod.validateResults(results, PersonSchema);

if (validation.valid) {
  console.log("Valid data:", validation.validated);
} else {
  console.log("Validation errors:", validation.errors);
}
```

## Canonicalization

Canonicalization ensures that equivalent RDF graphs have the same representation:

```javascript
import { useCanon } from 'unrdf';

const canon = useCanon();

// Canonicalize a store
const canonical = await canon.canonicalize(store);
console.log(canonical);

// Check if two stores are isomorphic
const isIsomorphic = await canon.isIsomorphic(store1, store2);
console.log("Stores are equivalent:", isIsomorphic);
```

## Error Handling

unrdf provides comprehensive error handling with descriptive error messages:

```javascript
import { useTerms } from 'unrdf';

const terms = useTerms();

try {
  // This will throw an error
  const invalidIRI = terms.iri(123); // IRI must be a string
} catch (error) {
  console.error("Error:", error.message);
  // Output: "[useTerms] IRI must be a string"
}

try {
  // This will also throw an error
  const invalidLiteral = terms.lit(null); // Literal value cannot be null
} catch (error) {
  console.error("Error:", error.message);
  // Output: "[useTerms] Literal value cannot be null or undefined"
}
```

## Best Practices

### 1. Use Composables Consistently
Always use the provided composables rather than accessing underlying libraries directly:

```javascript
// ✅ Good
const terms = useTerms();
const node = terms.iri("http://example.org/foo");

// ❌ Avoid
import { DataFactory } from 'n3';
const node = DataFactory.namedNode("http://example.org/foo");
```

### 2. Handle Errors Gracefully
Always wrap operations in try-catch blocks:

```javascript
try {
  const result = await graph.select(query);
  // Process results
} catch (error) {
  console.error("Query failed:", error.message);
  // Handle error appropriately
}
```

### 3. Use Prefixes for Readability
Register common prefixes to make your code more readable:

```javascript
const prefixes = usePrefixes({
  "ex": "http://example.org/",
  "foaf": "http://xmlns.com/foaf/0.1/",
  "dc": "http://purl.org/dc/terms/"
});

// Use CURIEs in queries
const results = await graph.select(`
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name WHERE {
    ?person foaf:name ?name .
  }
`);
```

### 4. Validate Data Early
Use Zod schemas to validate data as early as possible:

```javascript
const PersonSchema = z.object({
  name: z.string(),
  age: z.number()
});

// Validate input data
const validation = await zod.validateResults(results, PersonSchema);
if (!validation.valid) {
  throw new Error("Invalid person data");
}
```

## Next Steps

Now that you have the basics down, explore these areas:

1. **[API Reference](../api/composables.md)** - Complete documentation of all composables
2. **[Utilities](../api/utilities.md)** - Helper functions for common operations
3. **[Examples](../examples/)** - Real-world usage patterns
4. **[CLI Documentation](../cli/)** - Command-line interface usage

## Getting Help

- **GitHub Issues**: [Report bugs or request features](https://github.com/gitvan/unrdf/issues)
- **Discussions**: [Ask questions or share ideas](https://github.com/gitvan/unrdf/discussions)
- **Documentation**: [Complete API reference](https://github.com/gitvan/unrdf#readme)

Happy coding with unrdf! 🚀