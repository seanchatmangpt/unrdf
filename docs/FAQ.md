# Frequently Asked Questions

Common questions and answers about unrdf.

## General Questions

### What is unrdf?

unrdf is an opinionated, composable framework for RDF knowledge operations in JavaScript. It provides a "one true path" through the RDF universe, making explicit choices for every component to eliminate "dark matter" – the boilerplate glue code that often plagues RDF development.

### Why should I use unrdf instead of other RDF libraries?

unrdf eliminates the "dark matter" of RDF development by providing:
- **Opinionated choices**: No decision fatigue about which libraries to use
- **Composable architecture**: Focused, reusable functions for specific concerns
- **Consistent API**: All composables follow the same patterns
- **Performance optimization**: Optimized integrations between chosen libraries
- **Comprehensive testing**: Extensive test coverage including edge cases

### What does "opinionated" mean in the context of unrdf?

unrdf makes explicit choices for its core components:
- **RDF Store**: N3.Store only
- **SPARQL Query Engine**: @comunica/query-sparql only
- **SHACL Validator**: rdf-validate-shacl only
- **N3 Reasoner**: eyereasoner only
- **RDF Canonicalization**: rdf-canonize (URDNA2015) only
- **JSON-LD Processing**: jsonld (normalized to N3) only
- **Graph Traversal**: Clownface (bound to N3.Store) only

### What is "dark matter" in RDF development?

"Dark matter" refers to the often-invisible, repetitive, and error-prone glue code that developers write to integrate various libraries in a typical RDF application. unrdf aims to eliminate this by providing canonical integrations and high-level APIs.

## Technical Questions

### Why does unrdf use .mjs files instead of TypeScript?

unrdf follows the project rule: "No TypeScript. Ever. JSDoc is the source of truth. Zod is the runtime contract." This approach provides:
- **Simplicity**: No compilation step required
- **Runtime safety**: Zod provides runtime validation
- **Type safety**: JSDoc provides type information for IDEs
- **Performance**: Direct execution without TypeScript overhead

### How does unrdf handle type safety without TypeScript?

unrdf uses a combination of:
- **JSDoc**: For type information and IDE support
- **Zod**: For runtime validation and type safety
- **Runtime checks**: Built-in validation in composables
- **Error handling**: Descriptive error messages for type issues

### What is the difference between useStore and useGraph?

- **useStore**: Low-level N3.Store operations (add, remove, get quads)
- **useGraph**: High-level graph operations (SPARQL queries, validation, serialization)

### How do I handle large datasets with unrdf?

For large datasets, use:
- **Streaming**: Process data in chunks
- **Batch operations**: Add/remove quads in batches
- **Caching**: Use useCache for expensive operations
- **Performance monitoring**: Use useMetrics to identify bottlenecks

### Can I use unrdf in the browser?

Yes, unrdf is designed to work in both Node.js and browser environments. However, some composables (like useTurtleFS) are Node.js specific.

### How does unrdf handle memory management?

unrdf uses N3.Store for in-memory storage, which is optimized for performance. For large datasets, consider:
- **Streaming operations**: Process data in chunks
- **Lazy loading**: Load data as needed
- **Memory monitoring**: Use useMetrics to track memory usage

## Usage Questions

### How do I get started with unrdf?

1. Install unrdf: `pnpm add unrdf`
2. Import composables: `import { useStore, useGraph } from 'unrdf'`
3. Create a store: `const store = useStore()`
4. Add data: `store.add([quad1, quad2])`
5. Query data: `const results = await graph.select(sparql)`

### What's the difference between useTurtle and useTurtleFS?

- **useTurtle**: Parses and serializes Turtle strings
- **useTurtleFS**: Manages Turtle files on the filesystem

### How do I validate RDF data with unrdf?

Use the useValidator composable with SHACL shapes:

```javascript
import { useValidator } from 'unrdf';

const validator = useValidator();
const validation = await validator.validate(store, shapes);

if (!validation.valid) {
  console.error('Validation errors:', validation.errors);
}
```

### How do I perform reasoning with unrdf?

Use the useReasoner composable with N3 rules:

```javascript
import { useReasoner } from 'unrdf';

const reasoner = useReasoner();
const reasonedStore = await reasoner.reason(store, rules);
```

### How do I convert between RDF and JSON-LD?

Use the useJsonLd composable:

```javascript
import { useJsonLd } from 'unrdf';

const jsonld = useJsonLd();

// RDF to JSON-LD
const jsonldDoc = await jsonld.toJSONLD(store);

// JSON-LD to RDF
const rdfStore = await jsonld.fromJSONLD(jsonldDoc);
```

### How do I handle prefixes in unrdf?

Use the usePrefixes composable:

```javascript
import { usePrefixes } from 'unrdf';

const prefixes = usePrefixes();
prefixes.register({
  ex: 'http://example.org/',
  foaf: 'http://xmlns.com/foaf/0.1/'
});

const expanded = prefixes.expand('ex:person'); // http://example.org/person
```

## Performance Questions

### How can I improve performance with unrdf?

- **Use caching**: Cache expensive operations with useCache
- **Monitor performance**: Use useMetrics to identify bottlenecks
- **Optimize queries**: Write efficient SPARQL queries
- **Batch operations**: Process data in batches
- **Use streaming**: For large datasets, use streaming operations

### Does unrdf support parallel processing?

Yes, unrdf is designed to work with parallel processing:
- **Concurrent operations**: Multiple composables can run concurrently
- **Async/await**: All operations are asynchronous
- **Worker support**: Can be used with Web Workers (planned)

### How does unrdf handle memory usage?

unrdf uses N3.Store which is optimized for memory usage. For large datasets:
- **Streaming**: Process data in chunks
- **Lazy loading**: Load data as needed
- **Memory monitoring**: Use useMetrics to track usage

## Integration Questions

### Can I use unrdf with existing RDF libraries?

unrdf is designed to replace the need for multiple RDF libraries by providing a unified API. However, you can still use other libraries alongside unrdf if needed.

### How does unrdf integrate with databases?

unrdf is primarily designed for in-memory operations. For database integration:
- **Import data**: Load data from databases into unrdf stores
- **Export data**: Export unrdf stores to databases
- **Custom composables**: Create custom composables for database operations

### Can I use unrdf with GraphQL?

Yes, unrdf can be used with GraphQL:
- **Data source**: Use unrdf as a data source for GraphQL
- **Schema generation**: Generate GraphQL schemas from RDF data
- **Query translation**: Translate GraphQL queries to SPARQL

### How does unrdf work with React/Vue/Angular?

unrdf is framework-agnostic and can be used with any JavaScript framework:
- **React**: Use in components and hooks
- **Vue**: Use in components and composables
- **Angular**: Use in services and components

## Development Questions

### How do I contribute to unrdf?

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests
5. Update documentation
6. Submit a pull request

See [CONTRIBUTING.md](./CONTRIBUTING.md) for detailed guidelines.

### How do I test unrdf?

unrdf uses Vitest for testing:
- **Unit tests**: Test individual composables
- **Integration tests**: Test composable interactions
- **Edge cases**: Test boundary conditions
- **Performance tests**: Test performance characteristics

### How do I debug unrdf?

- **Debug logging**: Enable debug environment variables
- **Performance metrics**: Use useMetrics to monitor performance
- **Error handling**: Check error messages and stack traces
- **Validation**: Use validation utilities to check data

### How do I extend unrdf?

You can extend unrdf by:
- **Creating custom composables**: Follow the composable pattern
- **Adding utilities**: Add helper functions to utils
- **Contributing**: Submit pull requests for new features
- **Plugins**: Create plugins for specific use cases (planned)

## Troubleshooting Questions

### Why are my tests failing?

Common causes:
- **Missing mocks**: Ensure all dependencies are mocked
- **Async operations**: Use proper async/await in tests
- **Data setup**: Ensure test data is properly initialized
- **Environment**: Check test environment configuration

### Why is my SPARQL query not working?

Common issues:
- **Missing prefixes**: Ensure all prefixes are defined
- **Syntax errors**: Check SPARQL syntax
- **Data structure**: Ensure data matches query expectations
- **Case sensitivity**: Check for case sensitivity issues

### Why is my Turtle parsing failing?

Common causes:
- **Syntax errors**: Check Turtle syntax
- **Missing prefixes**: Ensure all prefixes are defined
- **Encoding issues**: Check file encoding
- **Line endings**: Check for line ending issues

### Why is my validation failing?

Common issues:
- **Shape definition**: Check SHACL shape definitions
- **Data structure**: Ensure data matches shape expectations
- **Namespace issues**: Check namespace declarations
- **Constraint violations**: Review constraint definitions

## See Also

- [Getting Started](./getting-started.md) - Basic usage guide
- [Core Concepts](./core-concepts.md) - Understanding unrdf's philosophy
- [Examples](./examples.md) - Usage examples and patterns
- [API Reference](./api-reference.md) - Complete API documentation
- [Troubleshooting](./TROUBLESHOOTING.md) - Common issues and solutions
- [Contributing](./CONTRIBUTING.md) - How to contribute
