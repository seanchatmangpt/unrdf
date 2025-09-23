# Examples

This directory contains real-world examples demonstrating how to use unrdf in various scenarios.

## Basic Examples

### [Hello World](./hello-world.mjs)
Simple introduction to unrdf basics.

### [SPARQL Queries](./sparql-queries.mjs)
Comprehensive SPARQL query examples.

### [Turtle Processing](./turtle-processing.mjs)
Loading, parsing, and saving Turtle files.

### [SHACL Validation](./shacl-validation.mjs)
Data validation with SHACL shapes.

### [N3 Reasoning](./n3-reasoning.mjs)
Rule-based reasoning with N3.

### [Canonicalization](./canonicalization.mjs)
Graph canonicalization and comparison.

### [Zod Validation](./zod-validation.mjs)
Runtime validation with Zod schemas.

## Advanced Examples

### [Knowledge Graph](./knowledge-graph.mjs)
Building a complete knowledge graph.

### [Data Pipeline](./data-pipeline.mjs)
ETL pipeline for RDF data.

### [API Integration](./api-integration.mjs)
Integrating with external APIs.

### [Performance Optimization](./performance-optimization.mjs)
Optimizing for large datasets.

### [Error Handling](./error-handling.mjs)
Comprehensive error handling patterns.

### [Testing](./testing.mjs)
Testing strategies for RDF applications.

## Domain-Specific Examples

### [FOAF Profiles](./foaf-profiles.mjs)
Working with FOAF (Friend of a Friend) data.

### [SKOS Vocabularies](./skos-vocabularies.mjs)
Managing SKOS vocabularies and thesauri.

### [DCAT Catalogs](./dcat-catalogs.mjs)
Data catalogs with DCAT.

### [PROV Provenance](./prov-provenance.mjs)
Provenance tracking with PROV.

### [Schema.org](./schema-org.mjs)
Structured data with Schema.org.

## Integration Examples

### [Node.js Server](./nodejs-server.mjs)
Building a Node.js server with unrdf.

### [Express API](./express-api.mjs)
REST API with Express and unrdf.

### [GraphQL](./graphql.mjs)
GraphQL integration with unrdf.

### [Database Integration](./database-integration.mjs)
Integrating with various databases.

### [File Processing](./file-processing.mjs)
Batch processing of RDF files.

## Performance Examples

### [Large Datasets](./large-datasets.mjs)
Handling large RDF datasets.

### [Streaming](./streaming.mjs)
Streaming processing for memory efficiency.

### [Caching](./caching.mjs)
Caching strategies for performance.

### [Parallel Processing](./parallel-processing.mjs)
Parallel processing of RDF data.

## Testing Examples

### [Unit Testing](./unit-testing.mjs)
Unit testing with Vitest.

### [Integration Testing](./integration-testing.mjs)
Integration testing strategies.

### [Performance Testing](./performance-testing.mjs)
Performance testing and benchmarking.

### [Mock Data](./mock-data.mjs)
Generating mock RDF data for testing.

## Deployment Examples

### [Docker](./docker.mjs)
Containerizing unrdf applications.

### [Kubernetes](./kubernetes.mjs)
Deploying to Kubernetes.

### [AWS](./aws.mjs)
Deploying to AWS services.

### [Azure](./azure.mjs)
Deploying to Azure services.

## How to Run Examples

### Prerequisites

```bash
# Install dependencies
pnpm install

# Build the project
pnpm build
```

### Running Examples

```bash
# Run a specific example
node docs/examples/hello-world.mjs

# Run all examples
pnpm run examples
```

### Example Structure

Each example follows this structure:

```javascript
#!/usr/bin/env node

/**
 * Example: Hello World
 * 
 * This example demonstrates basic unrdf usage.
 */

import { useStore, useGraph } from 'unrdf';

async function main() {
  try {
    // Example code here
    console.log('Example completed successfully!');
  } catch (error) {
    console.error('Example failed:', error.message);
    process.exit(1);
  }
}

// Run the example
main();
```

## Contributing Examples

When adding new examples:

1. Follow the established structure
2. Include comprehensive comments
3. Add error handling
4. Test the example
5. Update this README

### Example Template

```javascript
#!/usr/bin/env node

/**
 * Example: [Title]
 * 
 * [Description of what this example demonstrates]
 */

import { useStore, useGraph, useTurtle, useValidator, useReasoner, useCanon, useZod } from 'unrdf';

async function main() {
  try {
    console.log('Starting [Title] example...');
    
    // Example code here
    
    console.log('Example completed successfully!');
  } catch (error) {
    console.error('Example failed:', error.message);
    process.exit(1);
  }
}

// Run the example
main();
```

## Best Practices

### 1. Error Handling

```javascript
try {
  const result = await operation();
} catch (error) {
  console.error('Operation failed:', error.message);
  process.exit(1);
}
```

### 2. Progress Reporting

```javascript
console.log('Processing data...');
const result = await processData();
console.log(`Processed ${result.count} items`);
```

### 3. Resource Cleanup

```javascript
const store = useStore();
try {
  // Use store
} finally {
  store.clear();
}
```

### 4. Configuration

```javascript
const config = {
  timeoutMs: 30000,
  batchSize: 1000,
  // ... other options
};
```

## Common Patterns

### Data Loading

```javascript
import { useTurtle } from 'unrdf';

const turtle = await useTurtle();
const store = await turtle.load('data.ttl');
```

### Query Processing

```javascript
import { useGraph } from 'unrdf';

const graph = useGraph(store);
const results = await graph.select(sparql);
```

### Validation

```javascript
import { useValidator } from 'unrdf';

const validator = useValidator();
const report = await validator.validate(dataStore, shapesStore);
```

### Reasoning

```javascript
import { useReasoner } from 'unrdf';

const reasoner = useReasoner();
const inferred = await reasoner.reason(dataStore, rulesStore);
```

### Canonicalization

```javascript
import { useCanon } from 'unrdf';

const canon = useCanon();
const canonical = await canon.canonicalize(store);
```

### Runtime Validation

```javascript
import { useZod } from 'unrdf';

const zod = useZod();
const schema = zod.z.object({ name: zod.z.string() });
const validation = zod.validateResults(results, schema);
```

## Troubleshooting

### Common Issues

#### Import Errors
```javascript
// Error: Cannot resolve module 'unrdf'
// Solution: Check package.json and node_modules
pnpm install unrdf
```

#### Async/Await Issues
```javascript
// Error: Cannot use await in non-async function
// Solution: Make function async
async function processData() {
  const results = await graph.select(sparql);
}
```

#### Memory Issues
```javascript
// For large datasets, use streaming
const results = await graph.select(sparql, { limit: 1000 });
```

### Getting Help

1. Check the [API Reference](../api-reference.md)
2. Look at [Core Concepts](../core-concepts.md)
3. Review [Composables](../composables/)
4. Check the [GitHub Issues](https://github.com/gitvan/unrdf/issues)
