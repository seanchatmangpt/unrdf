# E2E Tests with Testcontainers

This directory contains end-to-end tests for the KGC JS sidecar using Testcontainers to provide realistic testing environments with containerized services.

## Overview

The E2E tests use Testcontainers to spin up real services (PostgreSQL, Redis, MinIO, Apache Jena Fuseki) in Docker containers, providing a production-like environment for testing the knowledge engine.

## Test Structure

### Core Files

- **`testcontainer-setup.mjs`** - Testcontainers configuration and utilities
- **`knowledge-engine-e2e.test.mjs`** - Core knowledge engine E2E tests
- **`browser-e2e.test.mjs`** - Browser compatibility E2E tests
- **`integration-e2e.test.mjs`** - Multi-service integration tests

### Test Categories

1. **Knowledge Graph Operations**
   - Data loading and querying
   - Reasoning and inference
   - SHACL validation
   - Transaction management

2. **Knowledge Hooks Integration**
   - Hook execution on data changes
   - Hook failure handling
   - Policy pack management

3. **Multi-Service Integration**
   - PostgreSQL for RDF storage
   - Redis for caching
   - MinIO for object storage
   - Apache Jena Fuseki for SPARQL endpoints

4. **Browser Compatibility**
   - Web Worker-based effect execution
   - Browser storage persistence
   - Memory constraint handling
   - Cross-browser compatibility

5. **Performance and Scalability**
   - Large dataset handling
   - Concurrent operations
   - Memory pressure management
   - High-throughput scenarios

## Prerequisites

### Docker

Testcontainers requires Docker to be running:

```bash
# Check Docker is running
docker --version
docker ps
```

### Node.js Dependencies

```bash
pnpm install
```

## Running Tests

### Run All E2E Tests

```bash
pnpm test test/e2e/
```

### Run Specific Test Suites

```bash
# Knowledge engine E2E tests
pnpm test test/e2e/knowledge-engine-e2e.test.mjs

# Browser E2E tests
pnpm test test/e2e/browser-e2e.test.mjs

# Integration E2E tests
pnpm test test/e2e/integration-e2e.test.mjs
```

### Run with Verbose Output

```bash
pnpm test test/e2e/ --reporter=verbose
```

## Test Environment

### Services Used

| Service            | Container                 | Purpose         | Port       |
| ------------------ | ------------------------- | --------------- | ---------- |
| PostgreSQL         | `postgres:15-alpine`      | RDF storage     | 5432       |
| Redis              | `redis:7-alpine`          | Caching         | 6379       |
| MinIO              | `minio/minio:latest`      | Object storage  | 9000, 9001 |
| Apache Jena Fuseki | `stain/jena-fuseki:4.9.0` | SPARQL endpoint | 3030       |
| Node.js App        | `node:18-alpine`          | KGC sidecar     | 3000       |

### Configuration

The test environment is configured in `testcontainer-setup.mjs`:

```javascript
export const TEST_CONFIG = {
  STARTUP_TIMEOUT: 30000,
  SHUTDOWN_TIMEOUT: 10000,
  TEST_DATA_DIR: join(tmpdir(), 'unrdf-e2e-tests'),
  PORTS: {
    POSTGRES: 5432,
    REDIS: 6379,
    MINIO: 9000,
    MINIO_CONSOLE: 9001,
    FUSEKI: 3030,
  },
};
```

## Test Data

### Sample Knowledge Graph

```javascript
{
  '@context': {
    '@vocab': 'https://example.org/',
    'schema': 'https://schema.org/'
  },
  '@graph': [
    {
      '@id': 'https://example.org/alice',
      '@type': 'schema:Person',
      'schema:name': 'Alice Doe',
      'schema:email': 'alice@example.org',
      'schema:knows': { '@id': 'https://example.org/bob' }
    },
    {
      '@id': 'https://example.org/bob',
      '@type': 'schema:Person',
      'schema:name': 'Bob Smith',
      'schema:email': 'bob@example.org'
    }
  ]
}
```

### Sample Policy Pack

```javascript
{
  meta: {
    name: 'test-policy-pack',
    version: '1.0.0',
    description: 'Test policy pack for E2E testing'
  },
  hooks: [
    {
      id: 'test-hook-1',
      name: 'Test Hook 1',
      description: 'A test hook for validation',
      when: {
        kind: 'sparql-ask',
        query: 'ASK WHERE { ?s a schema:Person }'
      },
      then: {
        kind: 'javascript',
        code: `
          return {
            success: true,
            message: 'Test hook executed successfully',
            timestamp: new Date().toISOString()
          };
        `
      }
    }
  ]
}
```

## Writing E2E Tests

### Basic Test Structure

```javascript
import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { E2ETestEnvironment, TestDataManager } from './testcontainer-setup.mjs';

describe('My E2E Tests', () => {
  let testEnv;
  let dataManager;

  beforeAll(async () => {
    testEnv = new E2ETestEnvironment();
    dataManager = new TestDataManager();
    await testEnv.startServices();
    await testEnv.waitForServices();
  }, 60000);

  afterAll(async () => {
    if (testEnv) {
      await testEnv.cleanup();
    }
  });

  it('should test something', async () => {
    // Your test logic here
  });
});
```

### Accessing Services

```javascript
// Get service configurations
const postgres = testEnv.getService('postgres');
const redis = testEnv.getService('redis');
const minio = testEnv.getService('minio');
const fuseki = testEnv.getService('fuseki');

// Use service endpoints
const connectionString = postgres.connectionString;
const cacheEndpoint = redis.connectionString;
const objectStorage = minio.config;
const sparqlEndpoint = fuseki.sparqlEndpoint;
```

### Creating Test Data

```javascript
// Create sample data
dataManager.createTestData('my-dataset', {
  '@context': { '@vocab': 'https://example.org/' },
  '@graph': [
    /* your data */
  ],
});

// Retrieve test data
const myData = dataManager.getTestData('my-dataset');
```

## Troubleshooting

### Common Issues

1. **Docker not running**

   ```
   Error: Docker is not running
   ```

   Solution: Start Docker Desktop or Docker daemon

2. **Port conflicts**

   ```
   Error: Port 5432 is already in use
   ```

   Solution: Stop conflicting services or change ports in config

3. **Container startup timeout**

   ```
   Error: Container did not start within timeout
   ```

   Solution: Increase `STARTUP_TIMEOUT` or check container logs

4. **Memory issues**
   ```
   Error: Out of memory
   ```
   Solution: Increase Docker memory limits or reduce test data size

### Debugging

Enable verbose logging:

```bash
DEBUG=testcontainers* pnpm test test/e2e/
```

Check container logs:

```javascript
const container = testEnv.getService('postgres');
console.log(await container.container.logs());
```

### Performance Tuning

For faster test execution:

1. **Use smaller datasets** for development
2. **Increase Docker resources** (CPU, memory)
3. **Run tests in parallel** where possible
4. **Cache container images** locally

## CI/CD Integration

### GitHub Actions

```yaml
name: E2E Tests
on: [push, pull_request]

jobs:
  e2e-tests:
    runs-on: ubuntu-latest
    services:
      docker:
        image: docker:latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-node@v3
        with:
          node-version: '18'
      - run: pnpm install
      - run: pnpm test test/e2e/
```

### Local Development

```bash
# Run E2E tests in watch mode
pnpm test test/e2e/ --watch

# Run specific test file
pnpm test test/e2e/knowledge-engine-e2e.test.mjs --watch
```

## Best Practices

1. **Isolation**: Each test should be independent
2. **Cleanup**: Always clean up containers after tests
3. **Timeouts**: Set appropriate timeouts for container operations
4. **Data**: Use realistic test data that mirrors production
5. **Performance**: Monitor test execution time and optimize
6. **Error Handling**: Test both success and failure scenarios
7. **Documentation**: Document test scenarios and expected outcomes

## Contributing

When adding new E2E tests:

1. Follow the existing test structure
2. Use descriptive test names
3. Include proper setup and teardown
4. Add documentation for new test scenarios
5. Ensure tests are deterministic and repeatable
6. Consider performance impact of new tests
