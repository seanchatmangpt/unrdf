# Troubleshooting

Common issues and solutions when using unrdf.

## Installation Issues

### Package Manager Conflicts

**Problem**: Conflicts between npm, yarn, and pnpm

**Solution**: Use pnpm exclusively as specified in the project rules:

```bash
# Remove other package managers
rm -rf node_modules package-lock.json yarn.lock

# Use pnpm
pnpm install
```

### Node.js Version Issues

**Problem**: Compatibility issues with Node.js versions

**Solution**: Ensure you're using Node.js 18 or higher:

```bash
# Check Node.js version
node --version

# Use nvm to switch versions
nvm use 18
```

### Dependency Installation Failures

**Problem**: Dependencies fail to install

**Solution**: Clear cache and reinstall:

```bash
# Clear pnpm cache
pnpm store prune

# Remove node_modules and reinstall
rm -rf node_modules
pnpm install
```

## Runtime Issues

### Store Not Found

**Problem**: `useStore` returns undefined or throws error

**Solution**: Ensure proper import and initialization:

```javascript
// ❌ Incorrect
import { useStore } from 'unrdf';
const store = useStore(); // Missing import

// ✅ Correct
import { useStore } from 'unrdf';
const store = useStore();
```

### Graph Operations Fail

**Problem**: Graph operations throw errors

**Solution**: Ensure store is properly initialized:

```javascript
// ❌ Incorrect
import { useGraph } from 'unrdf';
const graph = useGraph(); // Missing store parameter

// ✅ Correct
import { useStore, useGraph } from 'unrdf';
const store = useStore();
const graph = useGraph(store);
```

### Turtle Parsing Errors

**Problem**: Turtle parsing fails with syntax errors

**Solution**: Check Turtle syntax and prefixes:

```javascript
// ❌ Incorrect - missing prefix
const turtle = `
  ex:person ex:name "John" .
`;

// ✅ Correct - with prefix
const turtle = `
  @prefix ex: <http://example.org/> .
  ex:person ex:name "John" .
`;
```

### SPARQL Query Errors

**Problem**: SPARQL queries fail or return empty results

**Solution**: Check query syntax and data:

```javascript
// ❌ Incorrect - missing prefix
const query = `
  SELECT ?name WHERE {
    ?person ex:name ?name .
  }
`;

// ✅ Correct - with prefix
const query = `
  PREFIX ex: <http://example.org/>
  SELECT ?name WHERE {
    ?person ex:name ?name .
  }
`;
```

## Performance Issues

### Slow Query Performance

**Problem**: SPARQL queries are slow

**Solution**: Optimize queries and use caching:

```javascript
import { useCache } from 'unrdf';

const cache = useCache();
const graph = useGraph(store);

// Cache expensive queries
const results = await cache.get('expensive-query', async () => {
  return await graph.select(expensiveSparql);
});
```

### Memory Usage Issues

**Problem**: High memory usage with large datasets

**Solution**: Use streaming and batch processing:

```javascript
// Process data in batches
const batchSize = 1000;
for (let i = 0; i < data.length; i += batchSize) {
  const batch = data.slice(i, i + batchSize);
  await processBatch(batch);
}
```

### Slow Turtle Parsing

**Problem**: Large Turtle files parse slowly

**Solution**: Use streaming parser for large files:

```javascript
import { createReadStream } from 'fs';
import { useTurtle } from 'unrdf';

const turtle = useTurtle();
const stream = createReadStream('large-file.ttl');

// Parse in chunks
const chunks = [];
stream.on('data', chunk => chunks.push(chunk));
stream.on('end', async () => {
  const content = chunks.join('');
  const store = await turtle.parse(content);
});
```

## Validation Issues

### SHACL Validation Failures

**Problem**: SHACL validation returns unexpected results

**Solution**: Check shapes and data structure:

```javascript
// Ensure shapes are properly defined
const shapes = `
  @prefix sh: <http://www.w3.org/ns/shacl#> .
  @prefix ex: <http://example.org/> .
  
  ex:PersonShape a sh:NodeShape ;
    sh:targetClass ex:Person ;
    sh:property [
      sh:path ex:name ;
      sh:minCount 1 ;
      sh:datatype xsd:string
    ] .
`;
```

### Zod Validation Errors

**Problem**: Zod validation fails

**Solution**: Check schema definition and data:

```javascript
import { z } from 'zod';
import { useZod } from 'unrdf';

const zod = useZod();
const schema = z.object({
  name: z.string(),
  age: z.number()
});

// Validate data
const result = zod.validate(schema, { name: "John", age: 30 });
```

## Development Issues

### Test Failures

**Problem**: Tests fail unexpectedly

**Solution**: Check test setup and mocks:

```javascript
// Ensure proper test setup
import { describe, it, expect, beforeEach } from 'vitest';

describe('useStore', () => {
  let store;
  
  beforeEach(() => {
    store = useStore();
  });
  
  it('should create store', () => {
    expect(store).toBeDefined();
  });
});
```

### Linting Errors

**Problem**: ESLint or Prettier errors

**Solution**: Fix formatting and style issues:

```bash
# Fix linting issues
pnpm lint:fix

# Check specific files
pnpm lint src/composables/useStore.mjs
```

### Build Failures

**Problem**: Build process fails

**Solution**: Check build configuration and dependencies:

```bash
# Clean and rebuild
rm -rf dist
pnpm build

# Check for missing dependencies
pnpm install
```

## Configuration Issues

### Base IRI Problems

**Problem**: Relative IRIs not resolving correctly

**Solution**: Set proper base IRI:

```javascript
// Set base IRI in options
const terms = useTerms({ baseIRI: 'http://example.org/' });
const iri = terms.iri('person/123'); // Resolves to http://example.org/person/123
```

### Prefix Resolution Issues

**Problem**: Prefixes not expanding correctly

**Solution**: Register prefixes properly:

```javascript
const prefixes = usePrefixes();
prefixes.register({
  ex: 'http://example.org/',
  foaf: 'http://xmlns.com/foaf/0.1/'
});

const expanded = prefixes.expand('ex:person'); // http://example.org/person
```

## Error Messages

### Common Error Messages

#### `[useStore] Quads must be an array`
**Cause**: Passing non-array to `add()` method
**Solution**: Ensure quads are in an array:

```javascript
// ❌ Incorrect
store.add(quad);

// ✅ Correct
store.add([quad]);
```

#### `[useGraph] Store is required`
**Cause**: Not passing store to `useGraph()`
**Solution**: Pass store instance:

```javascript
// ❌ Incorrect
const graph = useGraph();

// ✅ Correct
const store = useStore();
const graph = useGraph(store);
```

#### `[useTerms] IRI must be a string`
**Cause**: Passing non-string to `iri()` method
**Solution**: Pass string value:

```javascript
// ❌ Incorrect
terms.iri(123);

// ✅ Correct
terms.iri('http://example.org/person');
```

#### `[useValidator] Data is required`
**Cause**: Not passing data to validator
**Solution**: Pass store or data:

```javascript
// ❌ Incorrect
validator.validate();

// ✅ Correct
validator.validate(store, shapes);
```

## Debugging Tips

### Enable Debug Logging

```javascript
// Set debug environment variable
process.env.DEBUG = 'unrdf:*';

// Or enable specific debug namespaces
process.env.DEBUG = 'unrdf:store,unrdf:graph';
```

### Use Performance Metrics

```javascript
import { useMetrics } from 'unrdf';

const metrics = useMetrics();

// Wrap expensive operations
const wrappedOperation = metrics.wrap('operation', async () => {
  return await expensiveOperation();
});

// Check performance
const lastMetric = metrics.last();
console.log('Operation took:', lastMetric.duration, 'ms');
```

### Debug Turtle Output

```javascript
import { debugTurtle } from 'unrdf/utils';

// Debug store contents
const debugOutput = debugTurtle(store, { 
  prefixes: { ex: 'http://example.org/' },
  indent: 2
});
console.log(debugOutput);
```

### Validate Data Structure

```javascript
import { validateQuadJSON } from 'unrdf/utils';

// Validate quad JSON
const quadJson = { subject: '...', predicate: '...', object: '...' };
const validation = validateQuadJSON(quadJson);

if (!validation.valid) {
  console.error('Validation errors:', validation.errors);
}
```

## Getting Help

### Community Resources

- **GitHub Issues**: Report bugs and request features
- **GitHub Discussions**: Ask questions and share ideas
- **Documentation**: Check existing documentation
- **Examples**: Look at usage examples

### Reporting Issues

When reporting issues, include:

1. **Version Information**: unrdf version, Node.js version
2. **Environment**: Operating system, package manager
3. **Reproduction Steps**: Steps to reproduce the issue
4. **Expected Behavior**: What should happen
5. **Actual Behavior**: What actually happens
6. **Code Example**: Minimal code that reproduces the issue
7. **Error Messages**: Full error messages and stack traces

### Example Issue Report

```markdown
## Bug Report

**Version**: unrdf 1.0.0
**Node.js**: 18.17.0
**OS**: macOS 13.0
**Package Manager**: pnpm 8.0.0

**Description**: Store.add() throws error when adding single quad

**Steps to Reproduce**:
1. Create store with `useStore()`
2. Create quad with `useTerms()`
3. Call `store.add(quad)`

**Expected**: Quad should be added to store
**Actual**: Throws error "[useStore] Quads must be an array"

**Code**:
```javascript
import { useStore, useTerms } from 'unrdf';

const store = useStore();
const terms = useTerms();
const quad = terms.quad(
  terms.iri('http://example.org/s'),
  terms.iri('http://example.org/p'),
  terms.lit('o')
);

store.add(quad); // Error here
```

**Error Message**:
```
Error: [useStore] Quads must be an array
    at Store.add (/path/to/unrdf/src/composables/useStore.mjs:45:11)
```
```

## See Also

- [Getting Started](./getting-started.md) - Basic usage guide
- [Core Concepts](./core-concepts.md) - Understanding unrdf's philosophy
- [Examples](./examples.md) - Usage examples and patterns
- [API Reference](./api-reference.md) - Complete API documentation
- [Contributing](./CONTRIBUTING.md) - How to contribute
