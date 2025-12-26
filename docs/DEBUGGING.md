# UNRDF Debugging Guide

Complete guide to debugging UNRDF applications, understanding errors, and profiling performance.

## Table of Contents

- [Common Errors and Solutions](#common-errors-and-solutions)
- [Enabling Debug Mode](#enabling-debug-mode)
- [Reading OTEL Traces](#reading-otel-traces)
- [Performance Profiling](#performance-profiling)
- [Reproducing Issues](#reproducing-issues)
- [Error Recovery Patterns](#error-recovery-patterns)

---

## Common Errors and Solutions

### ERR_INVALID_QUAD_SUBJECT

**Error Message**: "Invalid quad subject: must be NamedNode or BlankNode"

**Cause**: The subject of an RDF quad must be either a NamedNode or BlankNode, not a Literal or other term type.

**Solution**:
```javascript
import { namedNode, blankNode, literal } from '@unrdf/core';

// ❌ WRONG - Literal as subject
const quad = {
  subject: literal('Alice'),  // Invalid!
  predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
  object: literal('Alice')
};

// ✅ CORRECT - NamedNode or BlankNode as subject
const quad = {
  subject: namedNode('http://example.org/alice'),
  predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
  object: literal('Alice')
};
```

### ERR_INVALID_SPARQL

**Error Message**: "Invalid SPARQL query syntax"

**Cause**: SPARQL query has syntax errors or uses undefined variables.

**Solution**:
```javascript
// ❌ WRONG - Undefined variable ?age
const query = `
  SELECT ?name ?age WHERE {
    ?person foaf:name ?name .
  }
`;

// ✅ CORRECT - All variables in SELECT are bound in WHERE
const query = `
  SELECT ?name ?age WHERE {
    ?person foaf:name ?name ;
            foaf:age ?age .
  }
`;
```

**Debug**: Enable query debugging to see parsed query:
```bash
DEBUG=unrdf:query node your-script.mjs
```

### ERR_STORE_NOT_FOUND

**Error Message**: "Store not found or not initialized"

**Cause**: Attempting to use a store before creating it.

**Solution**:
```javascript
import { createStore, addQuad, namedNode, literal } from '@unrdf/core';

// ❌ WRONG - Using undefined store
addQuad(undefined, { subject: namedNode('...'), ... });

// ✅ CORRECT - Create store first
const store = createStore();
addQuad(store, { subject: namedNode('...'), ... });
```

### ERR_QUERY_TIMEOUT

**Error Message**: "Query execution timeout"

**Cause**: Query is too complex or operates on too much data without proper limits.

**Solution**:
```javascript
// ❌ WRONG - No LIMIT on large dataset
const query = `
  SELECT ?s ?p ?o WHERE {
    ?s ?p ?o .
  }
`;

// ✅ CORRECT - Add LIMIT and OFFSET for pagination
const query = `
  SELECT ?s ?p ?o WHERE {
    ?s ?p ?o .
  }
  LIMIT 100
  OFFSET 0
`;

// Or increase timeout for complex queries
import { executeQuery } from '@unrdf/core';
const results = await executeQuery(store, query, {
  timeout: 30000  // 30 seconds
});
```

### ERR_NETWORK_REQUEST / ERR_FEDERATION_TIMEOUT

**Error Message**: "Network request failed" or "Federation query timeout"

**Cause**: Network issues or remote endpoint unavailable.

**Solution**: Use retry and fallback patterns:
```javascript
import { retry, fallback, withTimeout } from '@unrdf/core/recovery';

// Retry with exponential backoff
const data = await retry(
  async () => fetch('https://dbpedia.org/sparql'),
  { maxAttempts: 5, initialDelay: 200 }
);

// Fallback to cached data
const results = await fallback(
  async () => federatedQuery(remoteEndpoint, query),
  async () => getCachedResults(query)
);

// Timeout wrapper
const data = await withTimeout(
  async () => federatedQuery(endpoint, query),
  10000,  // 10 second timeout
  'Federation query timeout'
);
```

---

## Enabling Debug Mode

UNRDF uses the `DEBUG` environment variable for fine-grained logging.

### Basic Usage

```bash
# Enable all UNRDF debug logs
DEBUG=unrdf:* node your-script.mjs

# Enable specific namespace
DEBUG=unrdf:store node your-script.mjs

# Enable multiple namespaces
DEBUG=unrdf:store,unrdf:query node your-script.mjs
```

### Available Namespaces

| Namespace | Description |
|-----------|-------------|
| `unrdf:*` | All UNRDF debug logs |
| `unrdf:store` | Store operations (add, remove, count) |
| `unrdf:query` | SPARQL query execution |
| `unrdf:hooks` | Hook lifecycle and execution |
| `unrdf:workflow` | Workflow orchestration |
| `unrdf:federation` | Federated query execution |
| `unrdf:cache` | Cache operations |
| `unrdf:perf` | Performance metrics |

### Programmatic Usage

```javascript
import { createDebugger } from '@unrdf/core/debug';

const debug = createDebugger('store');

// Simple logging
debug.log('Adding quad', { quad });

// Error logging
debug.error('Failed to add quad', error);

// Warning logging
debug.warn('Duplicate quad detected', { quad });

// Performance timing
debug.time('addQuad');
// ... operation
debug.timeEnd('addQuad');  // Logs duration

// Memory tracking
debug.memory('Before query');
// ... operation
debug.memory('After query');
```

### Example Debug Output

```
[2025-12-25T10:30:45.123Z] [unrdf:store] Adding quad {
  "subject": "http://example.org/alice",
  "predicate": "http://xmlns.com/foaf/0.1/name",
  "object": "Alice"
}
[2025-12-25T10:30:45.125Z] [unrdf:store] Timer started: addQuad
[2025-12-25T10:30:45.127Z] [unrdf:store] Timer ended: addQuad {
  "durationMs": "2.34"
}
```

---

## Reading OTEL Traces

UNRDF integrates with OpenTelemetry for distributed tracing.

### Running OTEL Validation

```bash
# Run comprehensive validation
node validation/run-all.mjs comprehensive

# Check validation score (must be ≥80/100)
grep "Score:" validation-output.log

# Check for failures
grep "FAILED\|Error" validation-output.log
```

### Understanding OTEL Spans

Each major operation creates a span with:
- **Name**: Operation identifier (e.g., `addQuad`, `executeQuery`)
- **Duration**: Time taken in milliseconds
- **Attributes**: Operation-specific metadata
- **Status**: Success or error

### Example OTEL Trace Analysis

```javascript
// View traces in console
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('unrdf');

const span = tracer.startSpan('myOperation');
span.setAttribute('quad.count', 100);
span.setAttribute('query.type', 'SELECT');

try {
  // ... operation
  span.setStatus({ code: SpanStatusCode.OK });
} catch (error) {
  span.setStatus({
    code: SpanStatusCode.ERROR,
    message: error.message
  });
} finally {
  span.end();
}
```

### Common OTEL Patterns

**Check operation duration**:
```bash
# Find slow operations (>1 second)
grep "duration" validation-output.log | awk '$2 > 1000'
```

**Identify error patterns**:
```bash
# Count errors by type
grep "ERROR" validation-output.log | sort | uniq -c
```

---

## Performance Profiling

### Built-in Performance Tracker

```javascript
import { perfTracker } from '@unrdf/core/debug';

// Track operation
const id = perfTracker.start('bulkImport');

// ... perform operation

const metrics = perfTracker.end(id);
console.log(metrics);
// {
//   operation: 'bulkImport',
//   durationMs: '1234.56',
//   memoryDeltaMB: '45.23',
//   timestamp: '2025-12-25T10:30:45.123Z'
// }
```

### Trace Function Execution

```javascript
import { trace } from '@unrdf/core/debug';

// Wrap function
const addQuadTraced = trace(addQuad, 'addQuad');

// Use normally - automatically logs timing
addQuadTraced(store, quad);
```

### System Information Snapshot

```javascript
import { dumpDebugSnapshot } from '@unrdf/core/debug';

// Dump complete debug info to console
dumpDebugSnapshot();
```

**Output**:
```
=== UNRDF Debug Snapshot ===

System Info: {
  "node": "v18.19.0",
  "platform": "linux",
  "arch": "x64",
  "memory": {
    "rss": "123.45 MB",
    "heapTotal": "78.90 MB",
    "heapUsed": "56.78 MB",
    "external": "12.34 MB"
  },
  "uptime": "15.23 minutes",
  "env": {
    "DEBUG": "unrdf:*",
    "NODE_ENV": "development"
  }
}

Active Performance Metrics: [
  {
    "id": "bulkImport_1735123845123_abc123",
    "operation": "bulkImport",
    "startTime": 1735123845123,
    "startMemory": 59234567,
    "elapsedMs": "567.89"
  }
]

=== End Snapshot ===
```

### Node.js Built-in Profiler

```bash
# CPU profiling
node --cpu-prof your-script.mjs

# Heap profiling
node --heap-prof your-script.mjs

# Inspect with Chrome DevTools
node --inspect-brk your-script.mjs
# Open chrome://inspect in browser
```

---

## Reproducing Issues

### Creating Minimal Reproductions

**1. Isolate the problem**:
```javascript
// Create minimal test case
import { createStore, addQuad, namedNode, literal } from '@unrdf/core';

const store = createStore();

// Minimal failing example
const quad = {
  subject: namedNode('http://example.org/alice'),
  predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
  object: literal('Alice')
};

addQuad(store, quad);  // Where does it fail?
```

**2. Enable maximum debugging**:
```bash
DEBUG=unrdf:* NODE_ENV=development node reproduce.mjs 2>&1 | tee debug.log
```

**3. Capture full environment**:
```javascript
import { getSystemInfo } from '@unrdf/core/debug';

console.log('Environment:', JSON.stringify(getSystemInfo(), null, 2));

// Add to bug report
```

**4. Include test data**:
```javascript
// Export failing data
import { toNTriples } from '@unrdf/core';

const quads = getQuads(store);
const ntriples = toNTriples(quads);
console.log('Data:', ntriples);
```

### Issue Report Template

```markdown
## Bug Report

**Environment**:
- Node.js version: [from getSystemInfo()]
- UNRDF version: [from package.json]
- Platform: [from getSystemInfo()]

**Debug Output**:
```
[Paste debug.log content]
```

**Minimal Reproduction**:
```javascript
[Paste minimal code]
```

**Expected Behavior**:
[What should happen]

**Actual Behavior**:
[What actually happens]

**Test Data**:
```turtle
[N-Triples format data]
```
```

---

## Error Recovery Patterns

### Retry with Exponential Backoff

```javascript
import { retry } from '@unrdf/core/recovery';

const result = await retry(
  async () => {
    // Operation that might fail
    return await fetchRemoteData(url);
  },
  {
    maxAttempts: 5,
    initialDelay: 100,      // Start with 100ms
    maxDelay: 5000,         // Cap at 5 seconds
    backoffMultiplier: 2,   // Double each time
    shouldRetry: (error) => {
      // Custom retry logic
      return error.code === 'ETIMEDOUT';
    },
    onRetry: async (error, attempt, delay) => {
      console.log(`Retry ${attempt} in ${delay}ms:`, error.message);
    }
  }
);
```

### Circuit Breaker Pattern

```javascript
import { CircuitBreaker } from '@unrdf/core/recovery';

const breaker = new CircuitBreaker({
  failureThreshold: 5,     // Open after 5 failures
  successThreshold: 2,     // Close after 2 successes
  timeout: 60000,          // Try again after 60 seconds
  onStateChange: (oldState, newState, state) => {
    console.log(`Circuit ${oldState} -> ${newState}`, state);
  }
});

// Use for risky operations
const result = await breaker.execute(async () => {
  return await queryRemoteEndpoint(sparql);
});

// Check circuit state
console.log(breaker.getState());
// { state: 'closed', failures: 0, successes: 2, openedAt: null }
```

### Graceful Fallback

```javascript
import { fallback } from '@unrdf/core/recovery';

// Fallback to cache
const data = await fallback(
  async () => fetchFromRemote(url),
  async () => fetchFromCache(url)
);

// Static fallback value
const results = await fallback(
  async () => executeQuery(store, query),
  []  // Empty array if query fails
);
```

### Timeout Wrapper

```javascript
import { withTimeout } from '@unrdf/core/recovery';

const result = await withTimeout(
  async () => {
    // Long-running operation
    return await complexQuery(store, query);
  },
  5000,  // 5 second timeout
  'Query timeout - try simpler query or add LIMIT'
);
```

### Bulk Operations with Error Handling

```javascript
import { bulkOperation } from '@unrdf/core/recovery';

const quads = [/* ... many quads ... */];

const { successes, failures } = await bulkOperation(
  quads,
  async (quad) => addQuad(store, quad),
  {
    continueOnError: true,  // Don't stop on individual failures
    concurrency: 10          // Process 10 at a time
  }
);

console.log(`Added ${successes.length} quads`);
console.log(`Failed ${failures.length} quads`);

// Retry failures
if (failures.length > 0) {
  console.log('Retrying failures...');
  const failedQuads = failures.map(f => f.item);
  await bulkOperation(failedQuads, async (quad) => addQuad(store, quad));
}
```

### Rate Limiting

```javascript
import { RateLimiter } from '@unrdf/core/recovery';

const limiter = new RateLimiter({
  maxOperations: 10,  // 10 operations
  windowMs: 1000      // per second
});

// Process items respecting rate limit
for (const item of items) {
  await limiter.execute(async () => {
    return await processItem(item);
  });
}

// Check status
console.log(limiter.getStatus());
// { current: 8, max: 10, windowMs: 1000, available: 2 }
```

### Combined Recovery Strategy

```javascript
import { withRecovery, CircuitBreaker } from '@unrdf/core/recovery';

const breaker = new CircuitBreaker({ failureThreshold: 3 });

const robustFetch = withRecovery(
  async (url) => fetch(url).then(r => r.json()),
  {
    retry: {
      maxAttempts: 3,
      initialDelay: 200
    },
    circuitBreaker: breaker,
    timeout: 5000,
    fallback: async () => getCachedData()
  }
);

// Use as normal function
const data = await robustFetch('https://api.example.com/data');
```

---

## Quick Reference

### Debug Checklist

- [ ] Enable debug mode: `DEBUG=unrdf:*`
- [ ] Check OTEL validation: `≥80/100`
- [ ] Review error context and solutions
- [ ] Capture system info snapshot
- [ ] Create minimal reproduction
- [ ] Run with profiler if needed

### Error Handling Checklist

- [ ] Use specific error classes (`ValidationError`, `QueryError`, etc.)
- [ ] Include context in errors
- [ ] Implement retry for network operations
- [ ] Add circuit breakers for external services
- [ ] Provide fallback values/strategies
- [ ] Set timeouts on all async operations
- [ ] Handle bulk operations with individual error tracking

### Performance Checklist

- [ ] Use `perfTracker` for critical paths
- [ ] Enable performance debugging: `DEBUG=unrdf:perf`
- [ ] Monitor memory with `debug.memory()`
- [ ] Add LIMIT to SPARQL queries
- [ ] Profile with Node.js `--cpu-prof`
- [ ] Check OTEL spans for slow operations

---

## Additional Resources

- **UNRDF Documentation**: https://unrdf.dev/docs
- **Error Code Reference**: https://unrdf.dev/errors
- **SPARQL Reference**: https://www.w3.org/TR/sparql11-query/
- **OpenTelemetry Guide**: https://opentelemetry.io/docs/
- **Node.js Profiling**: https://nodejs.org/en/docs/guides/simple-profiling/

---

**Remember**: The Adversarial PM question applies to debugging too:
- ❓ Did I RUN the debug command or just read the docs?
- ❓ Did I VERIFY the error message matches?
- ❓ Can I REPRODUCE the issue from scratch?
- ❓ What's the EVIDENCE this fix works?

Self-deception is the enemy. Measure, don't assume.
