# How-To 03: Measure Query Performance

**Objective:** Measure and optimize SPARQL query performance using UNRDF's built-in profiling and OTEL integration.

**Audience:** Intermediate - developers optimizing production systems

**Prerequisites:**
- Understanding of SPARQL queries
- Familiarity with performance profiling
- **Capability Atoms:** `@unrdf/core` (query), `@unrdf/observability` (OTEL)

**Estimated Time:** 15 minutes

---

## Problem

You need to:
- Measure query execution time
- Identify slow queries
- Optimize query patterns
- Track performance regressions
- Monitor production query performance

---

## Solution

Use UNRDF's OpenTelemetry integration and query profiler.

---

## Step-by-Step Guide

### 1. Enable Query Profiling

**[Placeholder - Content to be filled]**

```javascript
import { createStore } from '@unrdf/oxigraph';
import { enableProfiling } from '@unrdf/observability';

const store = createStore();
enableProfiling(store, {
  captureQueries: true,
  threshold: 100 // ms
});
```

**Evidence:** Profiling at `/home/user/unrdf/packages/observability/src/profiling.mjs`

---

### 2. Measure Query Execution

**[Placeholder - Content to be filled]**

```javascript
import { measureQuery } from '@unrdf/observability';

const result = await measureQuery(store, {
  query: `SELECT * WHERE { ?s ?p ?o }`,
  context: { name: 'all-triples-query' }
});

console.log('Execution time:', result.duration, 'ms');
console.log('Results:', result.bindings.length);
console.log('Stats:', result.stats);
```

**Evidence:** Measurement at `/home/user/unrdf/packages/observability/src/measure-query.mjs`

---

### 3. Identify Slow Queries

**[Placeholder - Content to be filled]**

```javascript
// Get slow query report
const slowQueries = await store.profiler.getSlowQueries({
  threshold: 100, // ms
  limit: 10
});

slowQueries.forEach(q => {
  console.log(`${q.duration}ms: ${q.query.substring(0, 50)}...`);
  console.log('Called:', q.count, 'times');
  console.log('Avg:', q.avgDuration, 'ms');
});
```

**Evidence:** Slow query detection at `/home/user/unrdf/packages/observability/src/slow-queries.mjs`

---

### 4. Optimize Query Patterns

**[Placeholder - Content to be filled]**

```javascript
// Get optimization suggestions
const suggestions = await store.profiler.suggestOptimizations({
  query: mySlowQuery
});

suggestions.forEach(s => {
  console.log('Suggestion:', s.description);
  console.log('Expected improvement:', s.impact);
  console.log('Optimized query:', s.optimizedQuery);
});
```

**Evidence:** Optimization at `/home/user/unrdf/packages/core/src/query-optimizer.mjs`

---

### 5. Monitor with OTEL

**[Placeholder - Content to be filled]**

```javascript
// OTEL spans for queries
import { trace } from '@opentelemetry/api';

const span = trace.getTracer('unrdf').startSpan('sparql-query', {
  attributes: {
    'query.pattern': 'SELECT',
    'query.complexity': 'high'
  }
});

const results = await store.query(sparqlQuery);
span.end();
```

**Evidence:** OTEL integration at `/home/user/unrdf/packages/observability/src/otel-query.mjs`

---

## Complete Example

**[Placeholder - Link to complete example]**

**Evidence:** Full example at `/home/user/unrdf/examples/query-performance.mjs`

---

## Common Issues

**[Placeholder - Troubleshooting]**

- Issue: "Profiler overhead too high"
- Issue: "OTEL spans not appearing"
- Issue: "Query timeout"

---

## Related Guides

- **[How-To 04: Integrate with Existing Graphs](./04-integrate-with-existing-graphs.md)** - Integration patterns
- **[Explanation 04: Performance Tradeoffs](../explanation/performance-tradeoffs.md)** - Performance deep dive
- **[Reference: Hook API](../reference/hook-api.md)** - Hook performance

---

**Questions?** Check [TROUBLESHOOTING.md](/home/user/unrdf/docs/TROUBLESHOOTING.md) or file an issue.
