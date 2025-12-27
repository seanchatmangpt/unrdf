# Composables API Reference

The composables API provides modular, reusable functions for RDF operations. Each composable is designed to work independently while being composable with others.

## Overview

Composables are the core building blocks of UNRDF:

- **`useStore()`** - RDF data storage
- **`useGraph()`** - Graph operations and SPARQL queries
- **`useTurtle()`** - Turtle format handling
- **`useValidator()`** - SHACL validation
- **`useReasoner()`** - OWL reasoning
- **`useZod()`** - Type-safe operations
- **`useCanon()`** - RDF canonicalization

## useStore()

Creates and manages an RDF store for storing and retrieving RDF data.

### Signature

```javascript
function useStore(options?: StoreOptions): RdfStore
```

### Parameters

- `options` (optional): Configuration options for the store

### Returns

`RdfStore` - An RDF store instance

### Options

```javascript
interface StoreOptions {
  // Maximum number of quads to store
  maxQuads?: number;
  
  // Enable indexing for faster queries
  enableIndexing?: boolean;
  
  // Custom index configuration
  indexes?: IndexConfig[];
  
  // Memory management options
  memory?: {
    // Enable automatic cleanup
    autoCleanup?: boolean;
    
    // Cleanup threshold
    threshold?: number;
  };
}
```

### Methods

#### `addQuads(quads: Quad[]): Promise<void>`

Adds quads to the store.

```javascript
const store = useStore();
const quads = await turtle.parse(turtleData);
await store.addQuads(quads);
```

#### `removeQuads(quads: Quad[]): Promise<void>`

Removes quads from the store.

```javascript
await store.removeQuads(quadsToRemove);
```

#### `getQuads(): Quad[]`

Gets all quads from the store.

```javascript
const allQuads = store.getQuads();
```

#### `getQuadsByPattern(pattern: QuadPattern): Quad[]`

Gets quads matching a specific pattern.

```javascript
const pattern = {
  subject: { termType: 'NamedNode', value: 'http://example.org/john' }
};
const matchingQuads = store.getQuadsByPattern(pattern);
```

#### `clear(): void`

Clears all quads from the store.

```javascript
store.clear();
```

#### `getStats(): StoreStats`

Gets statistics about the store.

```javascript
const stats = store.getStats();
console.log(`Store contains ${stats.quadCount} quads`);
```

### Example

```javascript
import { useStore, useTurtle } from 'unrdf';

const store = useStore({
  enableIndexing: true,
  memory: {
    autoCleanup: true,
    threshold: 10000
  }
});

const turtle = useTurtle();
const quads = await turtle.parse(`
  @prefix ex: <http://example.org/> .
  ex:john a ex:Person ;
    ex:name "John Doe" .
`);

await store.addQuads(quads);
console.log(`Added ${quads.length} quads to store`);
```

## useGraph()

Creates a graph interface for querying and manipulating RDF data.

### Signature

```javascript
function useGraph(store: RdfStore, options?: GraphOptions): RdfGraph
```

### Parameters

- `store`: The RDF store to operate on
- `options` (optional): Configuration options for the graph

### Returns

`RdfGraph` - A graph interface instance

### Options

```javascript
interface GraphOptions {
  // Default query timeout in milliseconds
  queryTimeout?: number;
  
  // Enable query optimization
  enableOptimization?: boolean;
  
  // Custom query handlers
  queryHandlers?: QueryHandler[];
  
  // Graph name for named graphs
  graphName?: string;
}
```

### Methods

#### `query(sparql: string): AsyncIterable<Binding>`

Executes a SPARQL query on the graph.

```javascript
const graph = useGraph(store);
const results = await graph.query(`
  SELECT ?name WHERE {
    ?person a ex:Person ;
      ex:name ?name .
  }
`);

for await (const binding of results) {
  console.log(binding.get('name').value);
}
```

#### `addQuads(quads: Quad[]): Promise<void>`

Adds quads to the graph.

```javascript
await graph.addQuads(newQuads);
```

#### `removeQuads(quads: Quad[]): Promise<void>`

Removes quads from the graph.

```javascript
await graph.removeQuads(quadsToRemove);
```

#### `getQuads(): Quad[]`

Gets all quads from the graph.

```javascript
const allQuads = graph.getQuads();
```

#### `contains(pattern: QuadPattern): boolean`

Checks if the graph contains a specific pattern.

```javascript
const hasPerson = graph.contains({
  predicate: { termType: 'NamedNode', value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
  object: { termType: 'NamedNode', value: 'http://example.org/Person' }
});
```

#### `size(): number`

Gets the number of quads in the graph.

```javascript
const quadCount = graph.size();
```

### Example

```javascript
import { useStore, useGraph, useTurtle } from 'unrdf';

const store = useStore();
const graph = useGraph(store, {
  queryTimeout: 5000,
  enableOptimization: true
});

const turtle = useTurtle();
const quads = await turtle.parse(turtleData);
await graph.addQuads(quads);

// Query for all persons
const results = await graph.query(`
  PREFIX ex: <http://example.org/>
  SELECT ?person ?name WHERE {
    ?person a ex:Person ;
      ex:name ?name .
  }
`);

for await (const binding of results) {
  console.log(`${binding.get('name').value} is a person`);
}
```

## useTurtle()

Provides Turtle RDF format parsing and serialization.

### Signature

```javascript
function useTurtle(options?: TurtleOptions): TurtleHandler
```

### Parameters

- `options` (optional): Configuration options for Turtle handling

### Returns

`TurtleHandler` - A Turtle format handler

### Options

```javascript
interface TurtleOptions {
  // Default prefixes to use
  prefixes?: Record<string, string>;
  
  // Enable strict parsing
  strict?: boolean;
  
  // Custom error handling
  onError?: (error: ParseError) => void;
  
  // Serialization options
  serialization?: {
    // Indent level
    indent?: number;
    
    // Use compact format
    compact?: boolean;
    
    // Include prefixes
    includePrefixes?: boolean;
  };
}
```

### Methods

#### `parse(turtle: string): Promise<Quad[]>`

Parses a Turtle string into RDF quads.

```javascript
const turtle = useTurtle();
const quads = await turtle.parse(`
  @prefix ex: <http://example.org/> .
  ex:john a ex:Person .
`);
```

#### `serialize(quads: Quad[]): Promise<string>`

Serializes RDF quads to Turtle format.

```javascript
const turtleString = await turtle.serialize(quads);
```

#### `parseWithPrefixes(turtle: string, prefixes: Record<string, string>): Promise<Quad[]>`

Parses Turtle with custom prefixes.

```javascript
const customPrefixes = {
  'foaf': 'http://xmlns.com/foaf/0.1/',
  'ex': 'http://example.org/'
};
const quads = await turtle.parseWithPrefixes(turtleData, customPrefixes);
```

#### `serializeWithPrefixes(quads: Quad[], prefixes: Record<string, string>): Promise<string>`

Serializes quads with custom prefixes.

```javascript
const turtleString = await turtle.serializeWithPrefixes(quads, customPrefixes);
```

### Example

```javascript
import { useTurtle } from 'unrdf';

const turtle = useTurtle({
  prefixes: {
    'foaf': 'http://xmlns.com/foaf/0.1/',
    'ex': 'http://example.org/'
  },
  strict: true
});

// Parse Turtle data
const quads = await turtle.parse(`
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  @prefix ex: <http://example.org/> .
  
  ex:john a foaf:Person ;
    foaf:name "John Doe" ;
    foaf:age 30 .
`);

// Serialize back to Turtle
const serialized = await turtle.serialize(quads);
console.log(serialized);
```

## useValidator()

Provides RDF validation using SHACL shapes.

### Signature

```javascript
function useValidator(options?: ValidatorOptions): RdfValidator
```

### Parameters

- `options` (optional): Configuration options for validation

### Returns

`RdfValidator` - A validation handler

### Options

```javascript
interface ValidatorOptions {
  // Enable strict validation
  strict?: boolean;
  
  // Custom validation rules
  customRules?: ValidationRule[];
  
  // Error reporting options
  reporting?: {
    // Include detailed error information
    detailed?: boolean;
    
    // Maximum number of errors to report
    maxErrors?: number;
  };
}
```

### Methods

#### `validate(data: Quad[], shapes: Quad[]): Promise<ValidationResult>`

Validates RDF data against SHACL shapes.

```javascript
const validator = useValidator();
const result = await validator.validate(dataQuads, shapeQuads);

if (!result.conforms) {
  console.log('Validation failed:', result.results);
}
```

#### `validateWithConstraints(data: Quad[], constraints: Constraint[]): Promise<ValidationResult>`

Validates data with custom constraints.

```javascript
const constraints = [
  {
    path: 'http://example.org/name',
    minCount: 1,
    datatype: 'http://www.w3.org/2001/XMLSchema#string'
  }
];

const result = await validator.validateWithConstraints(dataQuads, constraints);
```

#### `createValidator(shape: Quad[]): CustomValidator`

Creates a custom validator for a specific shape.

```javascript
const customValidator = validator.createValidator(shapeQuads);
const result = await customValidator.validate(dataQuads);
```

### Example

```javascript
import { useValidator, useTurtle } from 'unrdf';

const validator = useValidator({
  strict: true,
  reporting: {
    detailed: true,
    maxErrors: 100
  }
});

const turtle = useTurtle();

// Load data and shapes
const dataQuads = await turtle.parse(dataTurtle);
const shapeQuads = await turtle.parse(shapeTurtle);

// Validate
const result = await validator.validate(dataQuads, shapeQuads);

if (result.conforms) {
  console.log('✅ Data is valid');
} else {
  console.log(`❌ Validation failed with ${result.results.length} errors`);
  for (const error of result.results) {
    console.log(`- ${error.resultMessage[0].value}`);
  }
}
```

## useReasoner()

Provides OWL reasoning and inference capabilities.

### Signature

```javascript
function useReasoner(options?: ReasonerOptions): RdfReasoner
```

### Parameters

- `options` (optional): Configuration options for reasoning

### Returns

`RdfReasoner` - A reasoning engine

### Options

```javascript
interface ReasonerOptions {
  // Enable OWL reasoning
  enableOWL?: boolean;
  
  // Custom reasoning rules
  customRules?: ReasoningRule[];
  
  // Reasoning timeout
  timeout?: number;
  
  // Enable transitive closure
  transitiveClosure?: boolean;
}
```

### Methods

#### `reason(quads: Quad[]): Promise<Quad[]>`

Performs OWL reasoning on the given quads.

```javascript
const reasoner = useReasoner();
const inferredQuads = await reasoner.reason(originalQuads);
```

#### `applyRules(quads: Quad[], rules: Rule[]): Promise<Quad[]>`

Applies custom reasoning rules.

```javascript
const rules = [
  {
    name: 'PersonRule',
    condition: (quads) => /* ... */,
    inference: (quads) => /* ... */
  }
];

const inferred = await reasoner.applyRules(quads, rules);
```

#### `getInferred(quads: Quad[]): Promise<Quad[]>`

Gets only the inferred triples.

```javascript
const inferred = await reasoner.getInferred(quads);
```

#### `isConsistent(quads: Quad[]): Promise<boolean>`

Checks if the data is consistent.

```javascript
const consistent = await reasoner.isConsistent(quads);
```

### Example

```javascript
import { useReasoner, useTurtle } from 'unrdf';

const reasoner = useReasoner({
  enableOWL: true,
  transitiveClosure: true,
  timeout: 30000
});

const turtle = useTurtle();
const quads = await turtle.parse(ontologyTurtle);

// Perform reasoning
const reasonedQuads = await reasoner.reason(quads);
console.log(`Inferred ${reasonedQuads.length} new triples`);

// Check consistency
const consistent = await reasoner.isConsistent(quads);
console.log(`Data is ${consistent ? 'consistent' : 'inconsistent'}`);
```

## useZod()

Provides type-safe RDF operations using Zod schemas.

### Signature

```javascript
function useZod(options?: ZodOptions): ZodHelper
```

### Parameters

- `options` (optional): Configuration options for Zod integration

### Returns

`ZodHelper` - A Zod integration helper

### Options

```javascript
interface ZodOptions {
  // Default namespace for generated IRIs
  defaultNamespace?: string;
  
  // Custom type mappings
  typeMappings?: Record<string, string>;
  
  // Enable strict mode
  strict?: boolean;
}
```

### Methods

#### `createShape(schema: ZodSchema): RdfShape`

Creates an RDF shape from a Zod schema.

```javascript
const zodHelper = useZod();
const shape = zodHelper.createShape(PersonSchema);
```

#### `toRdf(data: any, schema: ZodSchema): Quad[]`

Converts structured data to RDF quads.

```javascript
const personData = {
  id: 'http://example.org/john',
  name: 'John Doe',
  age: 30
};

const quads = zodHelper.toRdf(personData, PersonSchema);
```

#### `fromRdf(quads: Quad[], schema: ZodSchema): any`

Converts RDF quads to structured data.

```javascript
const personData = zodHelper.fromRdf(quads, PersonSchema);
```

#### `validate(data: any, schema: ZodSchema): ValidationResult`

Validates data against a Zod schema.

```javascript
const result = zodHelper.validate(personData, PersonSchema);
```

### Example

```javascript
import { useZod } from 'unrdf';
import { z } from 'zod';

const zodHelper = useZod({
  defaultNamespace: 'http://example.org/',
  strict: true
});

// Define schema
const PersonSchema = z.object({
  id: z.string().url(),
  name: z.string().min(1),
  age: z.number().int().min(0).max(150).optional()
});

// Convert to RDF
const personData = {
  id: 'http://example.org/john',
  name: 'John Doe',
  age: 30
};

const quads = zodHelper.toRdf(personData, PersonSchema);

// Convert back from RDF
const reconstructed = zodHelper.fromRdf(quads, PersonSchema);
```

## useCanon()

Provides RDF canonicalization capabilities.

### Signature

```javascript
function useCanon(options?: CanonOptions): RdfCanon
```

### Parameters

- `options` (optional): Configuration options for canonicalization

### Returns

`RdfCanon` - A canonicalization handler

### Options

```javascript
interface CanonOptions {
  // Canonicalization algorithm
  algorithm?: 'rdf-canonize' | 'custom';
  
  // Custom canonicalization rules
  customRules?: CanonRule[];
}
```

### Methods

#### `canonicalize(quads: Quad[]): Promise<string>`

Canonicalizes RDF quads to a normalized string.

```javascript
const canon = useCanon();
const canonical = await canon.canonicalize(quads);
```

#### `compare(quads1: Quad[], quads2: Quad[]): Promise<boolean>`

Compares two sets of quads for semantic equality.

```javascript
const equal = await canon.compare(quads1, quads2);
```

### Example

```javascript
import { useCanon, useTurtle } from 'unrdf';

const canon = useCanon();
const turtle = useTurtle();

const quads1 = await turtle.parse(turtleData1);
const quads2 = await turtle.parse(turtleData2);

// Check if semantically equal
const equal = await canon.compare(quads1, quads2);
console.log(`Datasets are ${equal ? 'equal' : 'different'}`);

// Get canonical representation
const canonical = await canon.canonicalize(quads1);
```

## Composing Composables

Composables are designed to work together:

```javascript
import { 
  useStore, 
  useGraph, 
  useTurtle, 
  useValidator, 
  useReasoner, 
  useZod 
} from 'unrdf';

// Create the pipeline
const store = useStore();
const graph = useGraph(store);
const turtle = useTurtle();
const validator = useValidator();
const reasoner = useReasoner();
const zodHelper = useZod();

// Parse and validate data
const quads = await turtle.parse(turtleData);
const validationResult = await validator.validate(quads, shapeQuads);

if (validationResult.conforms) {
  // Add to graph
  await graph.addQuads(quads);
  
  // Perform reasoning
  const reasonedQuads = await reasoner.reason(quads);
  await graph.addQuads(reasonedQuads);
  
  // Query the enhanced graph
  const results = await graph.query(sparqlQuery);
  
  // Convert to structured data
  const structuredData = zodHelper.fromRdf(quads, PersonSchema);
}
```

## Best Practices

1. **Use appropriate composables** for your use case
2. **Configure options** to match your requirements
3. **Handle errors** properly in async operations
4. **Compose functions** for complex workflows
5. **Use type safety** with Zod integration
6. **Validate data** before processing
7. **Manage memory** efficiently with large datasets