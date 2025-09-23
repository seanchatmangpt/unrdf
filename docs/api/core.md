# Core API Reference

The core API provides the fundamental building blocks for RDF operations in UNRDF.

## Overview

The core API consists of:

- **Store Management** - RDF data storage and retrieval
- **Graph Operations** - SPARQL queries and graph manipulation
- **Format Handling** - RDF serialization and parsing
- **Validation** - SHACL shape validation
- **Reasoning** - OWL inference and reasoning
- **Type Safety** - Zod integration for structured data

## Core Functions

### `useStore()`

Creates and manages an RDF store for storing and retrieving RDF data.

```javascript
import { useStore } from 'unrdf';

const store = useStore();
```

**Returns:** `RdfStore` - An RDF store instance

**Features:**
- Quad storage and retrieval
- Memory-efficient data structures
- Transaction support
- Index management

### `useGraph(store)`

Creates a graph interface for querying and manipulating RDF data.

```javascript
import { useStore, useGraph } from 'unrdf';

const store = useStore();
const graph = useGraph(store);
```

**Parameters:**
- `store` (RdfStore) - The RDF store to operate on

**Returns:** `RdfGraph` - A graph interface instance

**Features:**
- SPARQL query execution
- Quad addition and removal
- Graph traversal
- Pattern matching

### `useTurtle()`

Provides Turtle RDF format parsing and serialization.

```javascript
import { useTurtle } from 'unrdf';

const turtle = useTurtle();
```

**Returns:** `TurtleHandler` - A Turtle format handler

**Features:**
- Turtle parsing
- Turtle serialization
- Prefix management
- Error handling

### `useValidator()`

Provides RDF validation using SHACL shapes.

```javascript
import { useValidator } from 'unrdf';

const validator = useValidator();
```

**Returns:** `RdfValidator` - A validation handler

**Features:**
- SHACL shape validation
- Constraint checking
- Error reporting
- Custom validators

### `useReasoner()`

Provides OWL reasoning and inference capabilities.

```javascript
import { useReasoner } from 'unrdf';

const reasoner = useReasoner();
```

**Returns:** `RdfReasoner` - A reasoning engine

**Features:**
- OWL reasoning
- Rule-based inference
- Transitive closure
- Custom rules

### `useZod()`

Provides type-safe RDF operations using Zod schemas.

```javascript
import { useZod } from 'unrdf';

const zodHelper = useZod();
```

**Returns:** `ZodHelper` - A Zod integration helper

**Features:**
- Schema definition
- RDF conversion
- Type validation
- Data transformation

## Core Classes

### `RdfStore`

The main RDF data storage class.

```javascript
class RdfStore {
  // Add quads to the store
  addQuads(quads: Quad[]): Promise<void>
  
  // Remove quads from the store
  removeQuads(quads: Quad[]): Promise<void>
  
  // Get all quads
  getQuads(): Quad[]
  
  // Get quads matching a pattern
  getQuadsByPattern(pattern: QuadPattern): Quad[]
  
  // Clear the store
  clear(): void
  
  // Get store statistics
  getStats(): StoreStats
}
```

### `RdfGraph`

The graph interface for querying and manipulation.

```javascript
class RdfGraph {
  // Execute SPARQL query
  query(sparql: string): AsyncIterable<Binding>
  
  // Add quads to the graph
  addQuads(quads: Quad[]): Promise<void>
  
  // Remove quads from the graph
  removeQuads(quads: Quad[]): Promise<void>
  
  // Get all quads
  getQuads(): Quad[]
  
  // Check if graph contains pattern
  contains(pattern: QuadPattern): boolean
  
  // Get graph size
  size(): number
}
```

### `TurtleHandler`

Handles Turtle RDF format operations.

```javascript
class TurtleHandler {
  // Parse Turtle string to quads
  parse(turtle: string): Promise<Quad[]>
  
  // Serialize quads to Turtle
  serialize(quads: Quad[]): Promise<string>
  
  // Parse with custom prefixes
  parseWithPrefixes(turtle: string, prefixes: Record<string, string>): Promise<Quad[]>
  
  // Serialize with custom prefixes
  serializeWithPrefixes(quads: Quad[], prefixes: Record<string, string>): Promise<string>
}
```

### `RdfValidator`

Handles RDF validation operations.

```javascript
class RdfValidator {
  // Validate data against SHACL shapes
  validate(data: Quad[], shapes: Quad[]): Promise<ValidationResult>
  
  // Validate with custom constraints
  validateWithConstraints(data: Quad[], constraints: Constraint[]): Promise<ValidationResult>
  
  // Create custom validator
  createValidator(shape: Quad[]): CustomValidator
}
```

### `RdfReasoner`

Handles OWL reasoning and inference.

```javascript
class RdfReasoner {
  // Perform OWL reasoning
  reason(quads: Quad[]): Promise<Quad[]>
  
  // Apply custom rules
  applyRules(quads: Quad[], rules: Rule[]): Promise<Quad[]>
  
  // Get inferred triples
  getInferred(quads: Quad[]): Promise<Quad[]>
  
  // Check consistency
  isConsistent(quads: Quad[]): Promise<boolean>
}
```

### `ZodHelper`

Provides Zod integration for type-safe operations.

```javascript
class ZodHelper {
  // Create RDF shape from Zod schema
  createShape(schema: ZodSchema): RdfShape
  
  // Convert structured data to RDF
  toRdf(data: any, schema: ZodSchema): Quad[]
  
  // Convert RDF to structured data
  fromRdf(quads: Quad[], schema: ZodSchema): any
  
  // Validate data against schema
  validate(data: any, schema: ZodSchema): ValidationResult
}
```

## Data Types

### `Quad`

Represents an RDF quad (subject, predicate, object, graph).

```javascript
interface Quad {
  subject: Term;
  predicate: Term;
  object: Term;
  graph: Term;
}
```

### `Term`

Represents an RDF term (IRI, literal, blank node).

```javascript
interface Term {
  termType: 'NamedNode' | 'Literal' | 'BlankNode';
  value: string;
  datatype?: string;
  language?: string;
}
```

### `Binding`

Represents a SPARQL query result binding.

```javascript
interface Binding {
  get(variable: string): Term | undefined;
  has(variable: string): boolean;
  keys(): string[];
  values(): Term[];
}
```

### `ValidationResult`

Represents the result of RDF validation.

```javascript
interface ValidationResult {
  conforms: boolean;
  results: ValidationReport[];
}
```

### `ValidationReport`

Represents a single validation report.

```javascript
interface ValidationReport {
  focusNode: Term;
  resultPath?: Term;
  resultMessage: Term[];
  severity: Term;
  sourceConstraintComponent: Term;
}
```

## Error Handling

UNRDF provides comprehensive error handling:

```javascript
import { UnrdfError, ValidationError, ParseError } from 'unrdf';

try {
  const quads = await turtle.parse(invalidTurtle);
} catch (error) {
  if (error instanceof ParseError) {
    console.log('Parse error:', error.message);
    console.log('Line:', error.line);
    console.log('Column:', error.column);
  }
}
```

### Error Types

- **`UnrdfError`** - Base error class
- **`ParseError`** - RDF parsing errors
- **`ValidationError`** - Validation errors
- **`QueryError`** - SPARQL query errors
- **`ReasoningError`** - Reasoning errors

## Configuration

UNRDF can be configured globally:

```javascript
import { configure } from 'unrdf';

configure({
  // Default prefixes
  prefixes: {
    'foaf': 'http://xmlns.com/foaf/0.1/',
    'ex': 'http://example.org/'
  },
  
  // Validation options
  validation: {
    strict: true,
    reportDetails: true
  },
  
  // Reasoning options
  reasoning: {
    enableOWL: true,
    customRules: []
  }
});
```

## Performance Considerations

### Memory Management

- Use streaming for large datasets
- Clear stores when no longer needed
- Use efficient data structures

### Query Optimization

- Use appropriate indexes
- Limit result sets
- Use efficient SPARQL patterns

### Validation Performance

- Cache validation results
- Use incremental validation
- Optimize SHACL shapes

## Best Practices

1. **Use Type Safety**: Leverage Zod schemas for type-safe operations
2. **Handle Errors**: Always wrap operations in try-catch blocks
3. **Manage Memory**: Clear stores and streams when done
4. **Optimize Queries**: Use efficient SPARQL patterns
5. **Validate Data**: Always validate RDF data before processing
6. **Use Composables**: Combine functions for complex operations

## Examples

### Basic Store Operations

```javascript
import { useStore, useGraph, useTurtle } from 'unrdf';

const store = useStore();
const graph = useGraph(store);
const turtle = useTurtle();

// Parse and add data
const quads = await turtle.parse(`
  @prefix ex: <http://example.org/> .
  ex:john a ex:Person .
`);

await graph.addQuads(quads);

// Query data
const results = await graph.query('SELECT ?s WHERE { ?s a ex:Person }');
for await (const binding of results) {
  console.log(binding.get('s').value);
}
```

### Validation Example

```javascript
import { useValidator, useTurtle } from 'unrdf';

const validator = useValidator();
const turtle = useTurtle();

const data = await turtle.parse(dataTurtle);
const shapes = await turtle.parse(shapeTurtle);

const result = await validator.validate(data, shapes);
if (!result.conforms) {
  console.log('Validation errors:', result.results);
}
```

### Reasoning Example

```javascript
import { useReasoner, useTurtle } from 'unrdf';

const reasoner = useReasoner();
const turtle = useTurtle();

const quads = await turtle.parse(ontologyTurtle);
const inferred = await reasoner.reason(quads);

console.log(`Inferred ${inferred.length} new triples`);
```
