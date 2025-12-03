# UNRDF Performance Profiling Guide

## Overview

UNRDF v3.1.0 includes comprehensive performance profiling tools for measuring latency, memory usage, and CPU performance. This guide covers all profiling capabilities and best practices.

## Quick Start

```javascript
import { createProfiler } from './src/profiling/profiler.mjs';

const profiler = createProfiler();

const { result, profile } = await profiler.profile('my-operation', async () => {
  // Your code here
  return performWork();
});

console.log('Latency:', profile.latency.duration, 'ms');
console.log('Memory delta:', profile.memory.heapUsedDelta, 'bytes');
```

## Architecture

### Core Components

1. **Main Profiler** (`profiler.mjs`)
   - Orchestrates all profiling activities
   - Integrates with OpenTelemetry
   - Manages profile history and statistics

2. **Latency Profiler** (`latency-profiler.mjs`)
   - High-resolution timing
   - Percentile calculations (p50, p75, p90, p95, p99, p999)
   - Histogram generation
   - Checkpoint support

3. **Memory Profiler** (`memory-profiler.mjs`)
   - Heap usage tracking
   - Memory leak detection
   - Trend analysis
   - Periodic snapshots

4. **CPU Profiler** (`cpu-profiler.mjs`)
   - V8 Inspector integration (Node.js only)
   - Hot function analysis
   - CPU time measurements
   - Profile export

5. **Reporter** (`reporter.mjs`)
   - JSON, HTML, and terminal output
   - Profile comparison
   - Regression detection

## Usage Patterns

### 1. Basic Profiling

```javascript
import { createProfiler } from './src/profiling/profiler.mjs';

const profiler = createProfiler({
  enableLatency: true,
  enableMemory: true,
  enableCpu: false,
  enableOtel: true
});

const { result, profile } = await profiler.profile('operation-name', async () => {
  // Code to profile
  return await myAsyncOperation();
});

console.log('Duration:', profile.latency.duration, 'ms');
console.log('p95 latency:', profile.latency.p95, 'ms');
console.log('Memory delta:', profile.memory.heapUsedDelta, 'bytes');
```

### 2. Multiple Operations with Statistics

```javascript
const profiler = createProfiler();

// Profile same operation multiple times
for (let i = 0; i < 100; i++) {
  await profiler.profile('api-call', async () => {
    return await fetch('/api/data');
  });
}

// Get aggregate statistics
const stats = profiler.getStats('api-call');
console.log('Mean latency:', stats.latency.mean, 'ms');
console.log('p95 latency:', stats.latency.p95, 'ms');
console.log('p99 latency:', stats.latency.p99, 'ms');
```

### 3. Performance Budget Enforcement

```javascript
import { LatencyProfiler } from './src/profiling/latency-profiler.mjs';

const latencyProfiler = new LatencyProfiler();

const { metrics } = await latencyProfiler.profileAsync('database-query', async () => {
  return await db.query('SELECT * FROM users');
});

// Define performance budget
const budget = {
  p50: 50,   // 50ms target
  p95: 100,  // 100ms acceptable
  p99: 200   // 200ms max
};

const budgetCheck = latencyProfiler.checkBudget(metrics, budget);

if (!budgetCheck.passed) {
  console.error('Performance budget violated!');
  budgetCheck.violations.forEach(v => {
    console.error(`${v.metric}: ${v.actual}ms (exceeded by ${v.exceeded}ms)`);
  });
}
```

### 4. Memory Leak Detection

```javascript
import { MemoryProfiler } from './src/profiling/memory-profiler.mjs';

const memoryProfiler = new MemoryProfiler();

const { metrics } = await memoryProfiler.profile('memory-test', async () => {
  // Operation that might leak memory
  return await processLargeDataset();
});

console.log('Memory trend:', metrics.trend.direction);
console.log('Growth rate:', metrics.trend.growthRate, 'bytes/sec');

if (metrics.leakDetected) {
  console.warn('⚠️  Potential memory leak detected!');
  console.warn('Growth rate:', metrics.trend.growthRate, 'bytes/sec');
}
```

### 5. CPU Profiling (Node.js)

```javascript
import { CpuProfiler } from './src/profiling/cpu-profiler.mjs';

if (CpuProfiler.isAvailable()) {
  const cpuProfiler = new CpuProfiler({
    sampleInterval: 100,  // microseconds
    saveProfiles: true,
    profileDir: '.profiles'
  });

  const { metrics } = await cpuProfiler.profile('cpu-intensive', async () => {
    return await heavyComputation();
  });

  console.log('Total CPU time:', metrics.totalTime / 1000, 'ms');
  console.log('Hot functions:');
  metrics.hotFunctions.slice(0, 10).forEach((fn, i) => {
    console.log(`${i + 1}. ${fn.name}`);
    console.log(`   ${fn.selfTimePercent.toFixed(2)}% | ${fn.hitCount} samples`);
  });
}
```

### 6. Profile Reporting

```javascript
import { Reporter } from './src/profiling/reporter.mjs';

// Terminal output
console.log(Reporter.toTerminal(profile));

// Save as JSON
Reporter.save(profile, 'profile.json', {
  format: 'json',
  dir: './reports'
});

// Save as HTML dashboard
Reporter.save(profile, 'profile.html', {
  format: 'html',
  dir: './reports',
  title: 'API Performance Profile'
});

// Compare profiles for regression detection
const comparison = Reporter.compare(baselineProfile, currentProfile);

if (comparison.regression) {
  console.error('Performance regression detected!');
  comparison.regressions.forEach(r => {
    console.error(`${r.metric}: ${r.change.toFixed(2)}% slower`);
  });
}
```

### 7. Quick Profiling Helper

```javascript
import { quickProfile } from './src/profiling/profiler.mjs';

// One-off profiling without managing profiler instance
const { result, profile } = await quickProfile('quick-test', async () => {
  return await someOperation();
});

console.log('Duration:', profile.latency.duration, 'ms');
```

## UNRDF Performance Targets

UNRDF v3.1.0 has the following performance targets:

| Operation | Target (p95) | Description |
|-----------|--------------|-------------|
| Parse operations | < 50ms | RDF parsing |
| Query operations | < 100ms | SPARQL queries |
| Hook execution | < 80ms | Knowledge hook execution |
| Transaction commit | < 120ms | Transaction commits |

### Validating Against Targets

```javascript
const profiler = createProfiler();

// Define UNRDF operations with targets
const operations = [
  { name: 'parse', target: 50 },
  { name: 'query', target: 100 },
  { name: 'hook-execution', target: 80 },
  { name: 'transaction-commit', target: 120 }
];

for (const op of operations) {
  const { profile } = await profiler.profile(op.name, async () => {
    return await performUnrdfOperation(op.name);
  });

  const passed = profile.latency.p95 < op.target;
  console.log(`${passed ? '✓' : '✗'} ${op.name}: ${profile.latency.p95}ms (target: ${op.target}ms)`);
}
```

## OpenTelemetry Integration

The profiler automatically exports metrics to OpenTelemetry:

```javascript
const profiler = createProfiler({
  enableOtel: true,
  labels: ['production', 'api-v1']
});

await profiler.profile('operation', async () => {
  // Metrics automatically exported to OTEL
  return await work();
});
```

### Exported OTEL Metrics

- `profiler.operation.latency` - Latency histogram
- `profiler.memory.heap_used` - Memory gauge
- `profiler.operations.total` - Operation counter
- `profiler.operations.errors` - Error counter

## Best Practices

### 1. Profile in Production

```javascript
// Enable profiling in production with sampling
const profiler = createProfiler({
  enableLatency: true,
  enableMemory: process.env.PROFILE_MEMORY === 'true',
  enableCpu: false, // Too expensive for production
  enableOtel: true
});

// Sample 10% of requests
if (Math.random() < 0.1) {
  await profiler.profile('api-request', async () => {
    return await handleRequest();
  });
}
```

### 2. Set Performance Budgets

```javascript
const budgets = {
  'api-get-user': { p95: 50, p99: 100 },
  'api-create-post': { p95: 100, p99: 200 },
  'db-query': { p95: 30, p99: 50 }
};

// Validate after profiling
const budget = budgets[operationName];
if (budget) {
  const check = latencyProfiler.checkBudget(metrics, budget);
  if (!check.passed) {
    alertSlackChannel('Performance budget violated', check.violations);
  }
}
```

### 3. Regular Regression Testing

```javascript
// Load baseline profiles
const baselines = loadBaselineProfiles();

// Profile current implementation
const current = await profileAllOperations();

// Compare and report
for (const [op, baseline] of Object.entries(baselines)) {
  const comparison = Reporter.compare(baseline, current[op]);
  if (comparison.regression) {
    reportRegression(op, comparison);
  }
}
```

### 4. Memory Leak Detection in CI

```javascript
// In CI pipeline
const memoryProfiler = new MemoryProfiler();

for (let i = 0; i < 100; i++) {
  await memoryProfiler.profile('operation', async () => {
    return await operation();
  });
}

const metrics = memoryProfiler.stop(sessionId);

if (metrics.leakDetected) {
  console.error('Memory leak detected in CI!');
  process.exit(1);
}
```

### 5. CPU Profiling for Optimization

```javascript
// Profile CPU-intensive operations during development
if (process.env.NODE_ENV === 'development') {
  const cpuProfiler = new CpuProfiler({ saveProfiles: true });

  const { metrics } = await cpuProfiler.profile('heavy-operation', async () => {
    return await complexCalculation();
  });

  // Profiles saved to .profiles/ directory
  // Load in Chrome DevTools for analysis
  console.log('Profile saved:', metrics.profilePath);
}
```

## Examples

See `/examples/profiling-example.mjs` for complete working examples:

```bash
node examples/profiling-example.mjs
```

## Testing

Run profiler tests:

```bash
npm test test/profiling/profiler.test.mjs
```

## Performance Characteristics

### Overhead

- **Latency profiling**: ~0.1-0.5% overhead
- **Memory profiling**: ~1-2% overhead (snapshots every 100ms)
- **CPU profiling**: ~5-10% overhead (Node.js Inspector)

### Recommendations

- **Development**: Enable all profilers
- **Staging**: Enable latency + memory profiling
- **Production**: Enable latency profiling with sampling

## Troubleshooting

### CPU Profiling Not Available

CPU profiling requires Node.js Inspector API:

```javascript
if (!CpuProfiler.isAvailable()) {
  console.warn('CPU profiling not available');
  // Continue without CPU profiling
}
```

### Memory Profiler False Positives

If you get false positive leak detection:

```javascript
const memoryProfiler = new MemoryProfiler();
memoryProfiler.leakThreshold = 5 * 1024 * 1024; // 5 MB/s threshold
```

### High Performance Overhead

Reduce snapshot frequency:

```javascript
const memoryProfiler = new MemoryProfiler();
memoryProfiler.snapshotInterval = 500; // 500ms instead of 100ms
```

## API Reference

See individual module files for detailed JSDoc API documentation:

- `/src/profiling/profiler.mjs` - Main profiler API
- `/src/profiling/latency-profiler.mjs` - Latency profiling API
- `/src/profiling/memory-profiler.mjs` - Memory profiling API
- `/src/profiling/cpu-profiler.mjs` - CPU profiling API
- `/src/profiling/reporter.mjs` - Reporting API

## Contributing

When adding new profiling capabilities:

1. Add JSDoc documentation
2. Include OTEL integration
3. Add tests to `/test/profiling/`
4. Update this guide
5. Add examples to `/examples/profiling-example.mjs`

## License

Same as UNRDF project license.
