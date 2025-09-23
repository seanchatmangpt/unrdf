# Engines Documentation

This directory contains detailed documentation for unrdf's engine layer.

## Overview

The engine layer provides the low-level RDF operations that power all unrdf composables. It enforces unrdf's "One Path" philosophy by using a single, opinionated implementation for each RDF operation.

## Core Engine

### [RdfEngine](./RdfEngine.md)
The core RDF engine that powers all unrdf operations.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                        unrdf                                │
├─────────────────────────────────────────────────────────────┤
│  Composables Layer                                          │
│  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐          │
│  │useStore │ │useGraph │ │useTurtle│ │useValidator│        │
│  └─────────┘ └─────────┘ └─────────┘ └─────────┘          │
├─────────────────────────────────────────────────────────────┤
│  Engine Layer                                               │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │                    RdfEngine                            │ │
│  │  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐      │ │
│  │  │   N3    │ │ Comunica│ │  SHACL  │ │   EYE   │      │ │
│  │  └─────────┘ └─────────┘ └─────────┘ └─────────┘      │ │
│  └─────────────────────────────────────────────────────────┘ │
├─────────────────────────────────────────────────────────────┤
│  External Libraries                                         │
│  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐          │
│  │   N3    │ │ Comunica│ │  SHACL  │ │   EYE   │          │
│  └─────────┘ └─────────┘ └─────────┘ └─────────┘          │
└─────────────────────────────────────────────────────────────┘
```

## Engine Components

### N3.js Integration
- **Store Management**: N3.Store as the only in-memory RDF store
- **Parsing**: Turtle and N-Quads parsing
- **Serialization**: Turtle and N-Quads serialization
- **Term Creation**: Named nodes, literals, blank nodes, quads

### Comunica Integration
- **SPARQL Queries**: SELECT, ASK, CONSTRUCT, DESCRIBE
- **SPARQL Updates**: INSERT, DELETE, LOAD, CREATE, DROP, CLEAR, MOVE, COPY, ADD
- **Streaming**: Efficient handling of large result sets
- **Timeout Management**: Configurable timeouts for long-running queries

### SHACL Integration
- **Validation**: Data validation against SHACL shapes
- **Error Reporting**: Comprehensive validation error reporting
- **Shape Processing**: Support for complex SHACL shapes
- **Performance**: Optimized validation for large datasets

### EYE Integration
- **N3 Reasoning**: Rule-based reasoning with N3 rules
- **Inference**: Forward chaining inference
- **Rule Processing**: Support for complex N3 rules
- **Performance**: Optimized reasoning for large datasets

### rdf-canonize Integration
- **Canonicalization**: URDNA2015 canonicalization
- **Isomorphism**: Graph isomorphism checking
- **Deterministic**: Consistent canonical forms
- **Fallback**: Graceful handling of canonicalization failures

### jsonld Integration
- **JSON-LD Processing**: Conversion between RDF and JSON-LD
- **Context Handling**: JSON-LD context processing
- **Framing**: JSON-LD framing operations
- **Normalization**: JSON-LD normalization

### Clownface Integration
- **Graph Traversal**: Clownface-based graph traversal
- **Pointer Operations**: Efficient pointer operations
- **Dataset Binding**: Always bound to N3.Store
- **Performance**: Optimized traversal operations

## Configuration

### Engine Options

```javascript
import { RdfEngine } from 'unrdf';

const engine = new RdfEngine({
  baseIRI: 'https://example.org/',
  deterministic: true,
  timeoutMs: 30000,
  onMetric: (metric) => console.log(metric),
  logger: customLogger
});
```

### Configuration Options

- **baseIRI**: Base IRI for parsing operations
- **deterministic**: Enable deterministic operations
- **timeoutMs**: Default timeout for operations
- **onMetric**: Metrics callback function
- **logger**: Logger instance

## Performance Considerations

### Memory Management

```javascript
// Clear stores when done
const store = engine.createStore();
// ... use store
store.clear();
```

### Timeout Management

```javascript
// Set appropriate timeouts
const engine = new RdfEngine({ timeoutMs: 60000 });
```

### Deterministic Operations

```javascript
// Enable deterministic mode for consistent results
const engine = new RdfEngine({ deterministic: true });
```

### Streaming

```javascript
// Use streaming for large datasets
const results = await engine.query(store, sparql, { limit: 1000 });
```

## Error Handling

### Common Error Types

- **Parse Errors**: Invalid input format
- **Query Errors**: Invalid SPARQL syntax
- **Validation Errors**: SHACL validation failures
- **Reasoning Errors**: N3 reasoning failures
- **Timeout Errors**: Operation timeouts
- **Canonicalization Errors**: URDNA2015 failures

### Error Recovery

```javascript
try {
  const result = await engine.operation();
} catch (error) {
  if (error.message.includes('timeout')) {
    // Retry with longer timeout
    const retry = new RdfEngine({ timeoutMs: 60000 });
    return await retry.operation();
  }
  throw error;
}
```

## Best Practices

### 1. Use Engine Consistently

```javascript
// Good: Use engine for all operations
const engine = new RdfEngine();
const store = engine.createStore();
const results = await engine.query(store, sparql);

// Avoid: Mixing direct library usage
const store = new Store();
const engine = new QueryEngine();
```

### 2. Configure Appropriately

```javascript
// Set appropriate timeouts
const engine = new RdfEngine({ timeoutMs: 30000 });

// Enable deterministic operations
const engine = new RdfEngine({ deterministic: true });
```

### 3. Handle Errors Gracefully

```javascript
try {
  const result = await engine.operation();
} catch (error) {
  console.error('Operation failed:', error.message);
  // Provide fallback or rethrow
}
```

### 4. Monitor Performance

```javascript
const engine = new RdfEngine({
  onMetric: (metric) => {
    console.log(`${metric.event}: ${metric.durMs}ms`);
  }
});
```

## Common Patterns

### Engine Factory

```javascript
function createEngine(options = {}) {
  return new RdfEngine({
    baseIRI: 'https://example.org/',
    deterministic: true,
    timeoutMs: 30000,
    ...options
  });
}

const engine = createEngine({ timeoutMs: 60000 });
```

### Operation Wrapper

```javascript
async function safeOperation(engine, operation, ...args) {
  try {
    return await engine[operation](...args);
  } catch (error) {
    console.error(`${operation} failed:`, error.message);
    throw error;
  }
}

const result = await safeOperation(engine, 'query', store, sparql);
```

### Metrics Collection

```javascript
const metrics = [];

const engine = new RdfEngine({
  onMetric: (metric) => {
    metrics.push(metric);
  }
});

// ... use engine

console.log('Performance metrics:', metrics);
```

## Testing

### Unit Testing

```javascript
import { describe, it, expect } from 'vitest';
import { RdfEngine } from 'unrdf';

describe('RdfEngine', () => {
  it('should create engine instance', () => {
    const engine = new RdfEngine();
    expect(engine).toBeDefined();
  });
});
```

### Integration Testing

```javascript
import { RdfEngine } from 'unrdf';

describe('RdfEngine Integration', () => {
  it('should work with all operations', async () => {
    const engine = new RdfEngine();
    const store = engine.createStore();
    
    // Test all operations
    const results = await engine.query(store, sparql);
    expect(results).toBeDefined();
  });
});
```

## Troubleshooting

### Common Issues

#### Engine Creation
```javascript
// Error: Cannot create engine
// Solution: Check dependencies
pnpm install unrdf
```

#### Operation Failures
```javascript
// Error: Operation failed
// Solution: Check error message and retry
try {
  const result = await engine.operation();
} catch (error) {
  console.error('Operation failed:', error.message);
}
```

#### Performance Issues
```javascript
// For large datasets, use streaming
const results = await engine.query(store, sparql, { limit: 1000 });
```

### Getting Help

1. Check the [API Reference](../api-reference.md)
2. Look at [Examples](../examples/)
3. Review [Core Concepts](../core-concepts.md)
4. Check the [GitHub Issues](https://github.com/gitvan/unrdf/issues)

## Contributing

When modifying engines:

1. Follow the established patterns
2. Include comprehensive JSDoc documentation
3. Add proper error handling
4. Include unit tests
5. Update this documentation

See [CONTRIBUTING.md](../../CONTRIBUTING.md) for more details.
