# AtomVM Integration Test Guide

This guide documents the integration tests for `@unrdf/atomvm` with ecosystem packages.

## Overview

The integration tests verify that `@unrdf/atomvm` works correctly with:

- `@unrdf/oxigraph` - RDF store operations and SPARQL queries
- `@unrdf/core` - Core RDF operations and error handling
- `@unrdf/streaming` - Change feeds and stream processing

## Test Files

### `test/integration-core.test.mjs` (550+ LOC)

**Purpose:** Comprehensive tests for AtomVM with core ecosystem packages.

**Test Suites:**

| Suite | Tests | Description |
|-------|-------|-------------|
| AtomVM + Oxigraph Store | 5 | Store creation, quad operations, roundtrip |
| AtomVM + SPARQL Queries | 5 | SELECT, ASK, CONSTRUCT, FILTER, errors |
| AtomVM + Circuit Breaker | 3 | Failure recovery, state management |
| AtomVM + Message Validation | 3 | RPC message validation patterns |
| E2E Multi-Agent Scenario | 1 | Full workflow with 5 agents |
| Performance Benchmarks | 4 | Throughput, latency, concurrency, memory |
| Cross-Package Export Verification | 4 | Import/export compatibility |

### `test/integration-streaming.test.mjs` (350+ LOC)

**Purpose:** Tests for AtomVM with streaming and change feed operations.

**Test Suites:**

| Suite | Tests | Description |
|-------|-------|-------------|
| AtomVM + Change Feeds | 3 | Event emission, ordering |
| AtomVM + Stream Batching | 3 | Batch processing, timeout flush |
| AtomVM + Async Stream Source | 2 | Async iterator consumption |
| Streaming Performance | 2 | Throughput, backpressure |
| Change Feed -> Batcher Pipeline | 2 | Full pipeline integration |
| Streaming Export Verification | 2 | Utility compatibility |

## Running Tests

```bash
# Run all integration tests
cd packages/atomvm
pnpm test test/integration-core.test.mjs test/integration-streaming.test.mjs

# Run with verbose output
npx vitest run --reporter=verbose test/integration-core.test.mjs

# Run specific suite
npx vitest run -t "AtomVM + SPARQL"
```

## Performance SLAs

The integration tests verify the following SLA targets:

| Metric | Target | Actual (Measured) |
|--------|--------|-------------------|
| Triple insertion throughput | >1,000/sec | 20,868/sec |
| SPARQL query latency (P50) | <10ms | 0.13ms |
| SPARQL query latency (P95) | <100ms | 0.53ms |
| Concurrent operations | >1,000/sec | 22,404/sec |
| Streaming throughput | >1,000/sec | 9,776/sec |
| Memory per triple | <5KB | 2.02KB |

## Test Coverage

### 1. Oxigraph Integration

```javascript
import { createStore, dataFactory } from '@unrdf/oxigraph';

const store = createStore();
const { namedNode, literal, quad } = dataFactory;

// Add triples
store.add(quad(namedNode('http://ex.org/s'), namedNode('http://ex.org/p'), literal('value')));

// Query with SPARQL
const results = store.query('SELECT ?s WHERE { ?s ?p ?o }');
```

### 2. Circuit Breaker Pattern

```javascript
import { CircuitBreaker } from '@unrdf/atomvm';

const breaker = new CircuitBreaker({
  failureThreshold: 3,
  resetTimeout: 5000
});

const result = await breaker.call(async () => {
  return await riskyOperation();
});
```

### 3. Change Feed Integration

```javascript
const store = createStore();
const changeStream = new ChangeStream(store);

changeStream.on('change', (event) => {
  console.log(`${event.type}: ${event.sequence}`);
});

store.add(quad(s, p, o)); // Emits 'add' event
```

### 4. Stream Batching

```javascript
const batcher = new TripleStreamBatcher({ batchSize: 100 });

batcher.on('batch', (batch, meta) => {
  batch.forEach(triple => store.add(triple));
});

for (const triple of source) {
  batcher.add(triple);
}
batcher.close();
```

## Multi-Agent E2E Scenario

The E2E test simulates a complete workflow:

1. **Agent 1 (Graph Creator):** Creates social network graph
2. **Agent 2 (Query Agent):** Executes SPARQL queries
3. **Agent 3 (Stream Agent):** Processes updates
4. **Agent 6 (Cache Agent):** Manages result caching
5. **Agent 8 (Validator):** Checks data integrity
6. **Agent 9 (SLA Monitor):** Measures performance

## Error Handling

Tests verify proper error propagation:

```javascript
// Errors from Oxigraph propagate with clear messages
expect(() => store.add(null)).toThrow('Quad is required');
expect(() => store.query(null)).toThrow('Query must be a non-empty string');
expect(() => store.query('INVALID')).toThrow('Query execution failed');
```

## Test Isolation

Each test:
- Uses fresh store instances (`beforeEach`)
- Cleans up resources (`afterEach`)
- Has no dependencies on other tests
- Completes independently

## OTEL Integration

Performance metrics are logged for observability:

```
Triple insertion: 20868 triples/sec (47.92ms for 1000)
SPARQL latency: P50=0.13ms, P95=0.53ms
Concurrent ops: 100 in 4.46ms (22404 ops/sec)
Streaming throughput: 9776 triples/sec (102.29ms)
Memory: +19.27MB for 10k triples (2021 bytes/triple)
```

## Adding New Tests

Follow this pattern:

```javascript
describe('Integration: AtomVM + NewFeature', () => {
  let store;

  beforeEach(() => {
    store = createStore();
  });

  afterEach(() => {
    store = null;
  });

  it('should verify feature works', () => {
    // ARRANGE
    const input = createTestInput();

    // ACT
    const result = featureUnderTest(input);

    // ASSERT
    expect(result).toMatchExpected();
  });
});
```

## Troubleshooting

### Missing Dependencies

```bash
pnpm install
```

### Test Timeouts

Increase timeout in test or vitest.config.mjs:

```javascript
it('slow test', async () => { ... }, { timeout: 30000 });
```

### Import Errors

Ensure workspace dependencies are linked:

```json
{
  "dependencies": {
    "@unrdf/oxigraph": "workspace:*"
  }
}
```
