# Composables Documentation

This directory contains detailed documentation for each unrdf composable.

## Core Composables

### [useStore](./useStore.md)
Manage N3.Store instances with a clean, composable interface.

### [useGraph](./useGraph.md)
High-level graph operations including SPARQL queries and set operations.

### [useTurtle](./useTurtle.md)
Load, parse, and save Turtle files with GitVan integration.

### [useValidator](./useValidator.md)
SHACL validation with comprehensive error reporting.

### [useReasoner](./useReasoner.md)
N3 rule-based reasoning using the EYE reasoner.

### [useCanon](./useCanon.md)
Canonicalization and isomorphism checking using URDNA2015.

### [useZod](./useZod.md)
Runtime validation of JavaScript objects with Zod schemas.

## Extended Composables

### [usePrefixes](./usePrefixes.md)
Manage RDF prefixes and CURIE expansion/shrinking.

### [useTerms](./useTerms.md)
Create and manipulate RDF terms (named nodes, literals, blank nodes).

### [usePointer](./usePointer.md)
Clownface-based graph traversal and manipulation.

### [useJsonLd](./useJsonLd.md)
JSON-LD processing and conversion.

### [useNQuads](./useNQuads.md)
N-Quads parsing and serialization.

### [useTurtleFS](./useTurtleFS.md)
File system operations for Turtle files.

### [useCache](./useCache.md)
Caching for expensive operations.

### [useDelta](./useDelta.md)
Delta operations for tracking changes between stores.

### [useMetrics](./useMetrics.md)
Performance metrics and monitoring.

### [useLists](./useLists.md)
RDF list operations and utilities.

## Composable Patterns

### Basic Usage
```javascript
import { useStore, useGraph } from 'unrdf';

// Create composable instances
const store = useStore();
const graph = useGraph(store);

// Use composable methods
store.add(quad);
const results = await graph.select(sparql);
```

### Configuration
```javascript
import { useReasoner } from 'unrdf';

// Configure composable
const reasoner = useReasoner({
  timeoutMs: 60000,
  onMetric: (metric) => console.log(metric)
});
```

### Error Handling
```javascript
import { useTurtle } from 'unrdf';

try {
  const turtle = await useTurtle();
  const store = await turtle.parse(turtleData);
} catch (error) {
  console.error('Turtle parsing failed:', error.message);
}
```

### Chaining Operations
```javascript
import { useStore, useGraph, useCanon } from 'unrdf';

const store = useStore();
const graph = useGraph(store);
const canon = useCanon();

// Chain operations
const results = await graph.select(sparql);
const canonical = await canon.canonicalize(store);
```

## Best Practices

### 1. Use Composables Consistently
```javascript
// Good: Use composables for all operations
const store = useStore();
const graph = useGraph(store);

// Avoid: Direct engine usage
const engine = new RdfEngine();
const results = await engine.query(store, sparql);
```

### 2. Handle Errors Gracefully
```javascript
try {
  const result = await composable.operation();
} catch (error) {
  console.error('Operation failed:', error.message);
  // Provide fallback or rethrow
}
```

### 3. Use Appropriate Configuration
```javascript
// Set timeouts based on operation complexity
const quickOp = useGraph(store, { timeoutMs: 5000 });
const slowOp = useReasoner({ timeoutMs: 60000 });
```

### 4. Validate Data
```javascript
// Validate inputs and outputs
const schema = zod.z.object({ name: zod.z.string() });
const validation = zod.validateResults(results, schema);
if (!validation.validated) {
  throw new Error('Invalid data structure');
}
```

## Common Patterns

### Store Management
```javascript
const store = useStore();
store.add(quad1, quad2, quad3);
store.remove(quad1);
store.clear();
```

### Graph Operations
```javascript
const graph = useGraph(store);
const results = await graph.select(sparql);
const isValid = await graph.ask(sparql);
const constructed = await graph.construct(sparql);
```

### Validation
```javascript
const validator = useValidator();
const report = await validator.validate(dataStore, shapesStore);
if (!report.conforms) {
  console.error('Validation failed:', report.results);
}
```

### Reasoning
```javascript
const reasoner = useReasoner();
const inferred = await reasoner.reason(dataStore, rulesStore);
const newTriples = reasoner.getNewTriples(originalStore, inferred);
```

### Canonicalization
```javascript
const canon = useCanon();
const canonical = await canon.canonicalize(store);
const isIsomorphic = await canon.isIsomorphic(store1, store2);
```

### Runtime Validation
```javascript
const zod = useZod();
const schema = zod.z.object({ name: zod.z.string() });
const validation = zod.validateResults(results, schema);
```

## Performance Considerations

### Timeout Management
```javascript
// Set appropriate timeouts
const reasoner = useReasoner({ timeoutMs: 30000 });
const graph = useGraph(store, { timeoutMs: 5000 });
```

### Memory Management
```javascript
// Clear stores when done
store.clear();

// Use streaming for large datasets
const results = await graph.select(sparql, { limit: 1000 });
```

### Caching
```javascript
// Cache expensive operations
const cache = useCache();
const result = await cache.getOrSet('expensive-operation', async () => {
  return await expensiveOperation();
});
```

## Error Handling

### Common Error Types
- **Parse Errors**: Invalid input format
- **Query Errors**: Invalid SPARQL syntax
- **Validation Errors**: SHACL validation failures
- **Reasoning Errors**: N3 reasoning failures
- **Timeout Errors**: Operation timeouts

### Error Recovery
```javascript
try {
  const result = await composable.operation();
} catch (error) {
  if (error.message.includes('timeout')) {
    // Retry with longer timeout
    const retry = useComposable({ timeoutMs: 60000 });
    return await retry.operation();
  }
  throw error;
}
```

## Testing

### Unit Testing
```javascript
import { describe, it, expect } from 'vitest';
import { useStore } from 'unrdf';

describe('useStore', () => {
  it('should create a new store', () => {
    const store = useStore();
    expect(store.size).toBe(0);
  });
});
```

### Integration Testing
```javascript
import { useStore, useGraph } from 'unrdf';

describe('Store and Graph Integration', () => {
  it('should work together', async () => {
    const store = useStore();
    const graph = useGraph(store);
    
    store.add(quad);
    const results = await graph.select(sparql);
    
    expect(results.results).toHaveLength(1);
  });
});
```

## Contributing

When adding new composables:

1. Follow the established patterns
2. Include comprehensive JSDoc documentation
3. Add proper error handling
4. Include unit tests
5. Update this documentation

See [CONTRIBUTING.md](../../CONTRIBUTING.md) for more details.