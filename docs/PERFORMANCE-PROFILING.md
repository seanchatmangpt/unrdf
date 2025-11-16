# Performance Profiling Guide

**UNRDF v3.1.0** - Built-in Performance Profiler

**Last Updated:** March 15, 2026

---

## 📋 Table of Contents

1. [Overview](#overview)
2. [Getting Started](#getting-started)
3. [Latency Profiling](#latency-profiling)
4. [Memory Profiling](#memory-profiling)
5. [CPU Profiling](#cpu-profiling-nodejs-only)
6. [Interpreting Metrics](#interpreting-metrics)
7. [Optimization Recommendations](#optimization-recommendations)
8. [Performance Budgets](#performance-budgets)
9. [Monitoring Integration](#monitoring-integration)
10. [Best Practices](#best-practices)

---

## Overview

UNRDF v3.1.0 includes a **built-in performance profiler** providing real-time insights into:

- ✅ **Latency** - p50/p95/p99 percentiles for all operations
- ✅ **Memory** - Heap usage, GC pressure, leak detection
- ✅ **CPU** - Hot path identification, flamegraphs (Node.js only)
- ✅ **Cache** - Hit rates, eviction stats
- ✅ **Queries** - Slow query detection and analysis
- ✅ **Hooks** - Effect execution timing
- ✅ **OTEL Integration** - Export to Prometheus/Jaeger

### Why Built-in Profiling?

**Problem:** External profiling tools require setup, have overhead, and don't understand RDF operations.

**Solution:** Built-in profiler with zero-config, minimal overhead, and RDF-aware metrics.

**Benefits:**
- 📊 **Real-time insights** - No need to reproduce issues
- 🎯 **RDF-specific metrics** - SPARQL latency, triple counts, etc.
- ⚡ **Low overhead** - <2% performance impact
- 🔧 **Zero config** - Works out of the box
- 📈 **OTEL export** - Integrate with existing monitoring

---

## Getting Started

### Basic Setup

```javascript
import { createDarkMatterCore } from 'unrdf';

const system = await createDarkMatterCore({
  profiling: {
    enabled: true,  // Enable profiler
    sampleRate: 0.1  // Sample 10% of operations (reduce overhead)
  }
});

// Use system normally
await system.query({ query: 'SELECT * WHERE { ?s ?p ?o }', type: 'sparql-select' });

// Get performance profile
const profile = await system.getPerformanceProfile();
console.log(profile);
```

### Configuration Options

```javascript
const system = await createDarkMatterCore({
  profiling: {
    // Basic options
    enabled: true,  // Enable/disable profiler
    sampleRate: 0.1,  // Sample rate (0.1 = 10%)

    // Metrics to collect
    metrics: ['latency', 'memory', 'cache', 'cpu'],  // Default: all

    // Slow query detection
    slowQueryThreshold: 100,  // ms - Log queries slower than this
    onSlowQuery: (query, duration) => {
      console.warn(`Slow query (${duration}ms): ${query.query.slice(0, 100)}`);
    },

    // Continuous profiling
    exportInterval: 60000,  // Export metrics every 60s
    output: 'console',  // 'console', 'file', 'otel'

    // File output options
    filePath: './profiles/profile-{timestamp}.json',
    fileRotation: 'daily',  // 'hourly', 'daily', 'weekly'

    // OTEL integration
    otel: {
      exporter: prometheusExporter,  // OpenTelemetry exporter
      metrics: ['latency', 'memory', 'cache'],
      interval: 30000  // Export every 30s
    }
  }
});
```

---

## Latency Profiling

### Collecting Latency Metrics

```javascript
const system = await createDarkMatterCore({
  profiling: {
    enabled: true,
    metrics: ['latency']
  }
});

// Run operations
for (let i = 0; i < 1000; i++) {
  await system.query({
    query: 'SELECT * WHERE { ?s ?p ?o } LIMIT 10',
    type: 'sparql-select'
  });
}

// Get latency profile
const profile = await system.getPerformanceProfile();

console.log(profile.latency);
// {
//   p50: 12.5,   // Median (50th percentile)
//   p95: 45.2,   // 95th percentile
//   p99: 120.8,  // 99th percentile
//   max: 350.2,  // Maximum
//   min: 5.1,    // Minimum
//   mean: 18.7,  // Average
//   stddev: 15.3 // Standard deviation
// }
```

### Percentile Explanation

**p50 (Median):** Half of operations complete faster than this
**p95:** 95% of operations complete faster than this
**p99:** 99% of operations complete faster than this (tail latency)

**Example:**
- p50 = 10ms → Most operations are fast
- p95 = 50ms → Typical user experience
- p99 = 200ms → Worst 1% of requests (outliers)

### Latency by Operation Type

```javascript
const profile = await system.getPerformanceProfile();

console.log(profile.latencyByOperation);
// {
//   'sparql-select': { p50: 12.5, p95: 45.2, p99: 120.8 },
//   'sparql-ask': { p50: 5.2, p95: 15.1, p99: 35.4 },
//   'sparql-construct': { p50: 25.3, p95: 80.5, p99: 200.1 },
//   'transaction-commit': { p50: 8.7, p95: 25.3, p99: 60.2 },
//   'hook-execution': { p50: 3.2, p95: 10.5, p99: 25.8 },
//   'shacl-validation': { p50: 18.4, p95: 55.7, p99: 150.3 }
// }
```

### Real-time Latency Monitoring

```javascript
// Get current latency (updated every second)
const metrics = system.getMetrics();
console.log(`Current latency: ${metrics.currentLatency}ms`);

// Set up real-time monitoring
system.on('latency-spike', (event) => {
  if (event.latency > 100) {
    console.error(`Latency spike: ${event.latency}ms for ${event.operation}`);
  }
});
```

### Slow Query Detection

```javascript
const system = await createDarkMatterCore({
  profiling: {
    enabled: true,
    slowQueryThreshold: 100,  // Log queries > 100ms
    onSlowQuery: async (query, duration, metadata) => {
      console.warn(`
        SLOW QUERY DETECTED
        Duration: ${duration}ms
        Query: ${query.query}
        Type: ${query.type}
        Timestamp: ${new Date(metadata.timestamp).toISOString()}
      `);

      // Optional: Store for analysis
      await storeSlowQuery({
        query: query.query,
        duration,
        timestamp: metadata.timestamp,
        stackTrace: metadata.stackTrace
      });
    }
  }
});

// Get all slow queries
const profile = await system.getPerformanceProfile();
console.log(profile.slowQueries);
// [
//   {
//     query: 'SELECT * WHERE { ?s ?p ?o } ORDER BY ?s',
//     duration: 350.2,
//     timestamp: 1710518400000,
//     type: 'sparql-select'
//   },
//   ...
// ]
```

---

## Memory Profiling

### Collecting Memory Metrics

```javascript
const system = await createDarkMatterCore({
  profiling: {
    enabled: true,
    metrics: ['memory']
  }
});

// Get memory profile
const profile = await system.getPerformanceProfile();

console.log(profile.memory);
// {
//   heapUsed: 45678912,      // Bytes used on heap
//   heapTotal: 67108864,     // Total heap size
//   external: 1234567,       // External memory (buffers, etc.)
//   rss: 98765432,           // Resident set size (total memory)
//   arrayBuffers: 567890,    // ArrayBuffer memory
//   gcPressure: 0.15,        // GC pressure (0-1, lower is better)
//   leakScore: 0.02          // Leak detection score (0-1, 0 = no leak)
// }
```

### Human-Readable Format

```javascript
import { formatBytes } from 'unrdf';

const profile = await system.getPerformanceProfile();
const mem = profile.memory;

console.log(`
  Heap Used:    ${formatBytes(mem.heapUsed)}      (${(mem.heapUsed / mem.heapTotal * 100).toFixed(1)}% of heap)
  Heap Total:   ${formatBytes(mem.heapTotal)}
  External:     ${formatBytes(mem.external)}
  RSS:          ${formatBytes(mem.rss)}
  GC Pressure:  ${(mem.gcPressure * 100).toFixed(1)}%
  Leak Score:   ${(mem.leakScore * 100).toFixed(2)}%
`);

// Output:
// Heap Used:    43.5 MB      (68.0% of heap)
// Heap Total:   64.0 MB
// External:     1.2 MB
// RSS:          94.2 MB
// GC Pressure:  15.0%
// Leak Score:   2.00%
```

### Memory Leak Detection

```javascript
const system = await createDarkMatterCore({
  profiling: {
    enabled: true,
    memoryLeakDetection: {
      enabled: true,
      sampleInterval: 60000,  // Check every 60s
      threshold: 0.1,  // Alert if leak score > 10%
      onLeakDetected: (leak) => {
        console.error(`
          MEMORY LEAK DETECTED
          Leak Score: ${(leak.score * 100).toFixed(2)}%
          Growth Rate: ${leak.growthRate} MB/min
          Suspected Source: ${leak.suspectedSource}
        `);
      }
    }
  }
});

// Manually trigger leak check
const leakReport = await system.checkForMemoryLeaks();
if (leakReport.leak) {
  console.error('Memory leak detected:', leakReport);
}
```

### GC Pressure Monitoring

```javascript
// Monitor garbage collection pressure
const profile = await system.getPerformanceProfile();

if (profile.memory.gcPressure > 0.3) {
  console.warn('High GC pressure detected');

  // Recommendation: Reduce memory usage or increase heap
  // node --max-old-space-size=4096 your-app.js
}

// Get detailed GC stats
console.log(profile.gc);
// {
//   collections: 45,          // Total GC collections
//   pauseTime: 125.5,         // Total pause time (ms)
//   avgPauseTime: 2.8,        // Average pause time (ms)
//   maxPauseTime: 15.3,       // Maximum pause time (ms)
//   youngGenCollections: 40,  // Young generation GCs
//   oldGenCollections: 5      // Old generation (full) GCs
// }
```

### Memory Usage by Component

```javascript
const profile = await system.getPerformanceProfile();

console.log(profile.memoryByComponent);
// {
//   'rdf-store': 25.6,         // MB
//   'query-cache': 8.3,        // MB
//   'hook-manager': 2.1,       // MB
//   'transaction-log': 5.4,    // MB
//   'sandbox-isolates': 12.8,  // MB
//   'other': 3.2               // MB
// }
```

---

## CPU Profiling (Node.js only)

**Note:** CPU profiling is only available in Node.js (uses v8 module). Not available in browser.

### Collecting CPU Profiles

```javascript
const system = await createDarkMatterCore({
  profiling: {
    enabled: true,
    metrics: ['cpu']
  }
});

// Run operations
for (let i = 0; i < 1000; i++) {
  await system.query({ query: '...', type: 'sparql-select' });
}

// Get CPU profile
const profile = await system.getPerformanceProfile();

console.log(profile.cpu);
// {
//   totalTime: 5420,  // Total CPU time (ms)
//   userTime: 4850,   // User CPU time (ms)
//   systemTime: 570,  // System CPU time (ms)
//   utilization: 0.65 // CPU utilization (0-1)
// }
```

### Hot Path Identification

```javascript
const profile = await system.getPerformanceProfile();

console.log(profile.hotPaths);
// [
//   {
//     function: 'executeHook',
//     file: 'hook-executor.mjs',
//     line: 125,
//     count: 1250,           // Number of calls
//     totalTime: 850.5,      // Total time spent (ms)
//     avgTime: 0.68,         // Average time per call (ms)
//     percentage: 15.7       // % of total CPU time
//   },
//   {
//     function: 'parseQuery',
//     file: 'query.mjs',
//     line: 45,
//     count: 3200,
//     totalTime: 720.3,
//     avgTime: 0.23,
//     percentage: 13.3
//   },
//   ...
// ]
```

### Flamegraph Generation

```javascript
import { generateFlamegraph } from 'unrdf';

// Generate flamegraph (requires Node.js)
const flamegraph = await system.generateFlamegraph({
  duration: 30000,  // Profile for 30 seconds
  outputPath: './profiles/flamegraph.svg',
  format: 'svg'  // 'svg', 'html', 'json'
});

console.log(`Flamegraph saved to: ${flamegraph.path}`);
// Open flamegraph.svg in browser to visualize hot paths
```

**Flamegraph interpretation:**
- Width = Time spent in function
- Height = Call stack depth
- Color = Hot (red) to cold (blue)

### Continuous CPU Profiling

```javascript
// Start continuous profiling
const profiler = system.startContinuousProfiling({
  interval: 10000,  // Sample every 10s
  output: 'file',
  filePath: './profiles/cpu-{timestamp}.json'
});

// Run application...

// Stop profiling
await profiler.stop();

// Analyze collected profiles
const profiles = await profiler.getProfiles();
console.log(`Collected ${profiles.length} CPU profiles`);
```

---

## Interpreting Metrics

### Latency Targets

| Operation | p50 Target | p95 Target | p99 Target | Status |
|-----------|------------|------------|------------|--------|
| SPARQL SELECT (simple) | <10ms | <30ms | <100ms | ✅ Excellent |
| SPARQL SELECT (complex) | <50ms | <200ms | <500ms | ✅ Good |
| SPARQL ASK | <5ms | <15ms | <50ms | ✅ Excellent |
| SPARQL CONSTRUCT | <30ms | <100ms | <300ms | ✅ Good |
| Transaction Commit | <20ms | <50ms | <150ms | ✅ Excellent |
| Hook Execution | <10ms | <30ms | <100ms | ✅ Excellent |
| SHACL Validation | <30ms | <100ms | <300ms | ✅ Good |

### Memory Thresholds

| Metric | Healthy | Warning | Critical |
|--------|---------|---------|----------|
| **Heap Usage** | <70% | 70-85% | >85% |
| **GC Pressure** | <0.2 | 0.2-0.4 | >0.4 |
| **Leak Score** | <0.05 | 0.05-0.15 | >0.15 |
| **RSS Growth** | <10 MB/hour | 10-50 MB/hour | >50 MB/hour |

### CPU Utilization

| Utilization | Status | Action |
|-------------|--------|--------|
| **<30%** | 🟢 Underutilized | Consider scaling down |
| **30-70%** | ✅ Healthy | Optimal range |
| **70-90%** | ⚠️ High | Monitor closely |
| **>90%** | 🔴 Critical | Scale up or optimize |

### Cache Performance

| Hit Rate | Status | Action |
|----------|--------|--------|
| **>80%** | ✅ Excellent | No action needed |
| **60-80%** | 🟡 Good | Consider cache tuning |
| **40-60%** | ⚠️ Fair | Review cache strategy |
| **<40%** | 🔴 Poor | Cache not effective, consider disabling |

---

## Optimization Recommendations

### Latency Optimization

#### 1. Slow SPARQL Queries

**Symptom:** High p95/p99 latency for SELECT queries

**Diagnosis:**
```javascript
const profile = await system.getPerformanceProfile();
const slowQueries = profile.slowQueries.filter(q => q.type === 'sparql-select');
console.log(`Found ${slowQueries.length} slow SELECT queries`);
```

**Solutions:**

**A. Add indexes (if using backend store):**
```javascript
await system.addIndex({ predicate: 'http://xmlns.com/foaf/0.1/name' });
```

**B. Optimize query:**
```sparql
-- ❌ Slow: Cartesian product
SELECT ?person ?friend ?friendOfFriend
WHERE {
  ?person foaf:knows ?friend .
  ?friend foaf:knows ?friendOfFriend .
}

-- ✅ Fast: Add filter to reduce joins
SELECT ?person ?friend ?friendOfFriend
WHERE {
  ?person a foaf:Person .
  ?person foaf:knows ?friend .
  ?friend foaf:knows ?friendOfFriend .
  FILTER(?person != ?friendOfFriend)
}
```

**C. Enable query cache:**
```javascript
const system = await createDarkMatterCore({
  cache: {
    enabled: true,
    maxSize: 1000,  // Cache 1000 queries
    ttl: 300000     // 5 minutes
  }
});
```

#### 2. Slow Hook Execution

**Symptom:** High p95 latency for hook-execution

**Diagnosis:**
```javascript
const profile = await system.getPerformanceProfile();
const slowHooks = profile.hotPaths.filter(p => p.function.includes('Hook'));
console.log('Slow hooks:', slowHooks);
```

**Solutions:**

**A. Enable hook batching:**
```javascript
const system = await createDarkMatterCore({
  hooks: {
    batching: {
      enabled: true,
      maxBatchSize: 10,
      maxWaitTime: 50  // ms
    }
  }
});
```

**B. Increase isolate pool:**
```javascript
const system = await createDarkMatterCore({
  sandbox: {
    poolSize: 20,  // Reuse isolates
    maxConcurrent: 40
  }
});
```

### Memory Optimization

#### 1. High Heap Usage

**Symptom:** Heap usage >80%

**Diagnosis:**
```javascript
const profile = await system.getPerformanceProfile();
if (profile.memory.heapUsed / profile.memory.heapTotal > 0.8) {
  console.log('Memory by component:', profile.memoryByComponent);
}
```

**Solutions:**

**A. Reduce cache size:**
```javascript
const system = await createDarkMatterCore({
  cache: {
    maxSize: 500  // Reduce from 1000
  }
});
```

**B. Vacuum transaction log:**
```javascript
await system.vacuum({
  retentionDays: 7,  // Keep only last 7 days
  compressOld: true
});
```

**C. Increase heap size:**
```bash
node --max-old-space-size=4096 your-app.js  # 4GB heap
```

#### 2. Memory Leak

**Symptom:** Continuous memory growth

**Diagnosis:**
```javascript
const leak = await system.checkForMemoryLeaks();
if (leak.detected) {
  console.log('Leak source:', leak.suspectedSource);
  console.log('Growth rate:', leak.growthRate, 'MB/min');
}
```

**Solutions:**

**A. Check for unclosed resources:**
```javascript
// ❌ Bad: Not cleaning up
const system = await createDarkMatterCore();
// ... use system
// (forgot to call system.cleanup())

// ✅ Good: Always cleanup
try {
  const system = await createDarkMatterCore();
  // ... use system
} finally {
  await system.cleanup();
}
```

**B. Review hook effect code:**
```javascript
// ❌ Bad: Accumulating in closure
let cache = [];
const hook = defineHook({
  run: async (event) => {
    cache.push(event);  // Memory leak!
    return { success: true };
  }
});

// ✅ Good: Bounded cache
const cache = new LRUCache({ max: 100 });
const hook = defineHook({
  run: async (event) => {
    cache.set(event.id, event);  // Automatically evicts old entries
    return { success: true };
  }
});
```

### CPU Optimization

#### 1. High CPU Utilization

**Symptom:** CPU >80%

**Diagnosis:**
```javascript
const profile = await system.getPerformanceProfile();
console.log('CPU utilization:', (profile.cpu.utilization * 100).toFixed(1) + '%');
console.log('Hot paths:', profile.hotPaths.slice(0, 5));
```

**Solutions:**

**A. Optimize hot functions:**
```javascript
// Identify hot path from flamegraph
const flamegraph = await system.generateFlamegraph({ duration: 30000 });
// Review flamegraph.svg, optimize widest functions
```

**B. Use Web Workers (browser) or worker threads (Node.js):**
```javascript
const system = await createDarkMatterCore({
  workers: {
    enabled: true,
    maxWorkers: 8  // Parallelize work
  }
});
```

**C. Enable WASM parsing (browser):**
```javascript
const system = await createDarkMatterCore({
  parsing: {
    useWasm: true  // 38% faster parsing
  }
});
```

---

## Performance Budgets

### Setting Budgets

```javascript
const system = await createDarkMatterCore({
  profiling: {
    enabled: true,
    budgets: {
      'sparql-select': {
        p50: 15,  // ms
        p95: 50,
        p99: 150
      },
      'transaction-commit': {
        p50: 25,
        p95: 75,
        p99: 200
      },
      'hook-execution': {
        p50: 10,
        p95: 30,
        p99: 100
      }
    },
    onBudgetExceeded: (operation, metric, actual, budget) => {
      console.error(`
        PERFORMANCE BUDGET EXCEEDED
        Operation: ${operation}
        Metric: ${metric}
        Actual: ${actual}ms
        Budget: ${budget}ms
      `);

      // Alert team, create incident, etc.
    }
  }
});
```

### Monitoring Budget Compliance

```javascript
const profile = await system.getPerformanceProfile();
const compliance = profile.budgetCompliance;

console.log(compliance);
// {
//   'sparql-select': {
//     p50: { budget: 15, actual: 12.5, compliant: true },
//     p95: { budget: 50, actual: 55.2, compliant: false },  // ❌ Exceeded
//     p99: { budget: 150, actual: 120.8, compliant: true }
//   },
//   ...
// }

// Overall compliance score
console.log(`Budget compliance: ${(compliance.overallScore * 100).toFixed(1)}%`);
```

---

## Monitoring Integration

### Prometheus Integration

```javascript
import { PrometheusExporter } from '@opentelemetry/exporter-prometheus';

const exporter = new PrometheusExporter({ port: 9090 });

const system = await createDarkMatterCore({
  profiling: {
    enabled: true,
    otel: {
      exporter,
      metrics: ['latency', 'memory', 'cache', 'errors'],
      interval: 30000  // Export every 30s
    }
  }
});

// Metrics exposed at http://localhost:9090/metrics
```

**Prometheus scrape config:**
```yaml
# prometheus.yml
scrape_configs:
  - job_name: 'unrdf'
    static_configs:
      - targets: ['localhost:9090']
    scrape_interval: 30s
```

**Grafana dashboard queries:**
```promql
# p95 latency
histogram_quantile(0.95, rate(unrdf_operation_duration_bucket[5m]))

# Memory usage
unrdf_memory_heap_used_bytes / unrdf_memory_heap_total_bytes

# Cache hit rate
rate(unrdf_cache_hits_total[5m]) / (rate(unrdf_cache_hits_total[5m]) + rate(unrdf_cache_misses_total[5m]))
```

### Jaeger Integration

```javascript
import { JaegerExporter } from '@opentelemetry/exporter-jaeger';

const exporter = new JaegerExporter({
  endpoint: 'http://localhost:14268/api/traces'
});

const system = await createDarkMatterCore({
  profiling: {
    enabled: true,
    otel: {
      exporter,
      tracing: true
    }
  }
});

// View traces at http://localhost:16686
```

### Custom Metrics Export

```javascript
const system = await createDarkMatterCore({
  profiling: {
    enabled: true,
    exportInterval: 60000,
    onExport: async (metrics) => {
      // Send to custom backend
      await fetch('https://metrics.example.com/ingest', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(metrics)
      });
    }
  }
});
```

---

## Best Practices

### 1. Always Enable Profiling in Production

```javascript
const system = await createDarkMatterCore({
  profiling: {
    enabled: true,
    sampleRate: 0.05,  // 5% sampling in production (minimal overhead)
    slowQueryThreshold: 200  // Higher threshold for production
  }
});
```

### 2. Use Sampling to Reduce Overhead

```javascript
// Development: 100% sampling for detailed insights
const devSystem = await createDarkMatterCore({
  profiling: { enabled: true, sampleRate: 1.0 }
});

// Production: 5-10% sampling for low overhead
const prodSystem = await createDarkMatterCore({
  profiling: { enabled: true, sampleRate: 0.05 }
});
```

### 3. Set Performance Budgets

```javascript
const system = await createDarkMatterCore({
  profiling: {
    budgets: {
      'sparql-select': { p95: 100 },
      'transaction-commit': { p95: 150 }
    },
    onBudgetExceeded: (op, metric, actual, budget) => {
      // Alert team immediately
    }
  }
});
```

### 4. Export to OTEL

```javascript
// Integrate with existing monitoring stack
const system = await createDarkMatterCore({
  profiling: {
    enabled: true,
    otel: {
      exporter: prometheusExporter,
      metrics: ['latency', 'memory', 'cache', 'errors']
    }
  }
});
```

### 5. Review Profiles Regularly

```bash
# Generate weekly performance report
node scripts/performance-report.mjs --period weekly

# Review flamegraphs monthly
node scripts/generate-flamegraph.mjs --output reports/
```

### 6. Optimize Based on Data

```javascript
// Collect data first
const profile = await system.getPerformanceProfile();

// Identify bottlenecks
const bottlenecks = profile.hotPaths.slice(0, 10);

// Optimize top 3 bottlenecks (80/20 rule)
for (const bottleneck of bottlenecks.slice(0, 3)) {
  console.log(`Optimize: ${bottleneck.function} (${bottleneck.percentage}% of CPU)`);
}
```

---

## Summary

### Key Takeaways

✅ **Enable profiling** - Always on in production (with sampling)
✅ **Monitor latency** - p95/p99 are key metrics
✅ **Track memory** - Watch for leaks and GC pressure
✅ **Identify hot paths** - Use flamegraphs
✅ **Set budgets** - Enforce performance SLOs
✅ **Export to OTEL** - Integrate with monitoring stack

### Performance Profiler Features

| Feature | Node.js | Browser | Overhead |
|---------|---------|---------|----------|
| **Latency profiling** | ✅ | ✅ | <1% |
| **Memory profiling** | ✅ | ✅ | <1% |
| **CPU profiling** | ✅ | ❌ | <2% |
| **Flamegraphs** | ✅ | ❌ | <5% |
| **Slow query detection** | ✅ | ✅ | <1% |
| **OTEL export** | ✅ | ✅ | <1% |

---

## Resources

- [Release Notes](./v3.1.0-RELEASE-NOTES.md)
- [Migration Guide](./MIGRATION-v3.0-to-v3.1.md)
- [Performance Examples](../examples/v3.1.0/performance-profiling.mjs)
- [OpenTelemetry Docs](https://opentelemetry.io/docs/)

---

**Optimize with confidence!** 📊

Questions? Open an issue: https://github.com/unrdf/unrdf/issues
