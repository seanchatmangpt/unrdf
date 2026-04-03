---
description: Profile and benchmark UNRDF package performance, identifying bottlenecks and optimization opportunities
---

# Performance Profiler

## User Input

```text
$ARGUMENTS
```

You **MUST** consider the user input before proceeding (if not empty).

## Purpose

Measure, profile, and optimize performance of UNRDF packages against defined SLAs.

## Performance SLAs

| Operation              | Target | Maximum |
| ---------------------- | ------ | ------- |
| Single package assess  | <5s    | 10s     |
| Full 21-package report | <30s   | 60s     |
| SPARQL query           | <100ms | 500ms   |
| RDF file load          | <1s    | 3s      |
| CLI startup            | <500ms | 1s      |

## Quick Benchmarks

```bash
# Measure CLI startup time
time timeout 5s node packages/cli/dist/index.mjs --help

# Measure package assessment
time timeout 30s node packages/cli/dist/index.mjs maturity assess core

# Measure full report generation
time timeout 120s node packages/cli/dist/index.mjs maturity report
```

## Profiling Methods

### 1. Node.js Built-in Profiler

```bash
# Generate CPU profile
node --prof packages/cli/dist/index.mjs maturity assess core

# Process the profile
node --prof-process isolate-*.log > profile.txt

# Analyze hot functions
grep -E "^\s+\d+\s+\d+\.\d+%" profile.txt | head -20
```

### 2. Time Command Analysis

```bash
# Detailed timing
/usr/bin/time -v node packages/cli/dist/index.mjs maturity report 2>&1 | tee timing.log

# Parse results
grep -E "Elapsed|Maximum resident|CPU" timing.log
```

### 3. Memory Profiling

```bash
# Run with memory tracking
node --expose-gc --trace-gc packages/cli/dist/index.mjs maturity report 2>&1 | grep -E "Mark-sweep|Scavenge"

# Heap snapshot
node --inspect packages/cli/dist/index.mjs maturity assess core
# Connect Chrome DevTools to take heap snapshot
```

### 4. OTEL Metrics

```javascript
// Enable OTEL metrics collection
import { metrics } from '@opentelemetry/api';

const meter = metrics.getMeter('unrdf-performance');

const assessDuration = meter.createHistogram('maturity.assess.duration', {
  description: 'Time to assess package maturity',
  unit: 'ms',
});

// Record measurement
const start = performance.now();
await assessPackage(name);
assessDuration.record(performance.now() - start, { package: name });
```

## Benchmark Suite

### 1. RDF Store Operations

```javascript
// benchmark-store.mjs
import { createStore } from '@unrdf/oxigraph';
import { performance } from 'perf_hooks';

async function benchmarkStore() {
  const store = createStore();
  const results = [];

  // Benchmark: Load 1000 triples
  const data = generateTriples(1000);
  const loadStart = performance.now();
  store.load(data, { format: 'text/turtle' });
  results.push({
    operation: 'load-1000',
    duration: performance.now() - loadStart,
  });

  // Benchmark: SPARQL query
  const queryStart = performance.now();
  store.query('SELECT * WHERE { ?s ?p ?o } LIMIT 100');
  results.push({
    operation: 'query-100',
    duration: performance.now() - queryStart,
  });

  return results;
}
```

### 2. CLI Command Benchmarks

```bash
#!/bin/bash
# benchmark-cli.sh

echo "=== CLI Benchmarks ==="

# Warmup
node packages/cli/dist/index.mjs --help > /dev/null 2>&1

# Benchmark each command
for cmd in "maturity assess core" "maturity report" "maturity synergy"; do
  echo -n "$cmd: "
  start=$(date +%s%N)
  timeout 60s node packages/cli/dist/index.mjs $cmd > /dev/null 2>&1
  end=$(date +%s%N)
  duration=$(( (end - start) / 1000000 ))
  echo "${duration}ms"
done
```

### 3. Parallel vs Sequential

```javascript
// Compare parallel execution
const packages = ['core', 'streaming', 'federation', 'validation'];

// Sequential
const seqStart = performance.now();
for (const pkg of packages) {
  await assess(pkg);
}
const seqDuration = performance.now() - seqStart;

// Parallel
const parStart = performance.now();
await Promise.all(packages.map(assess));
const parDuration = performance.now() - parStart;

console.log(`Sequential: ${seqDuration}ms`);
console.log(`Parallel: ${parDuration}ms`);
console.log(`Speedup: ${(seqDuration / parDuration).toFixed(2)}x`);
```

## Bottleneck Detection

### Common UNRDF Bottlenecks

| Symptom       | Likely Cause      | Fix              |
| ------------- | ----------------- | ---------------- |
| Slow startup  | Large imports     | Lazy loading     |
| Memory growth | Store not cleaned | Explicit dispose |
| Query timeout | Full scan         | Add indexes      |
| Slow I/O      | Sync operations   | Use async        |

### Detection Commands

```bash
# Find sync operations
grep -r "readFileSync\|writeFileSync" packages/*/src/ --include="*.mjs" -l

# Find large imports at top level
head -50 packages/cli/src/commands/*.mjs | grep "^import"

# Check bundle sizes
for pkg in packages/*/; do
  name=$(basename "$pkg")
  size=$(du -sh "$pkg/dist" 2>/dev/null | cut -f1)
  echo "$name: $size"
done
```

## Output Format

````markdown
## Performance Profile Report

**Date**: [timestamp]
**Environment**: Node.js [version], [OS]

### Summary

| Metric        | Value | Target | Status |
| ------------- | ----- | ------ | ------ |
| CLI Startup   | Xms   | <500ms | ✅/❌  |
| Single Assess | Xs    | <5s    | ✅/❌  |
| Full Report   | Xs    | <30s   | ✅/❌  |
| Peak Memory   | XMB   | <512MB | ✅/❌  |

### Benchmark Results

| Operation       | P50 | P95 | P99 | Max |
| --------------- | --- | --- | --- | --- |
| assess core     | Xms | Xms | Xms | Xms |
| query simple    | Xms | Xms | Xms | Xms |
| load 1K triples | Xms | Xms | Xms | Xms |

### Hot Paths

1. [function/module] - X% of time
2. [function/module] - X% of time

### Memory Profile

- Initial: XMB
- Peak: XMB
- After GC: XMB

### Recommendations

1. [HIGH] [optimization opportunity]
2. [MEDIUM] [optimization opportunity]

### Verification

```bash
# Commands to verify improvements
time node packages/cli/dist/index.mjs maturity assess core
```
````

```

## Optimization Checklist

- [ ] Lazy load heavy dependencies
- [ ] Use async file operations
- [ ] Implement result caching
- [ ] Batch database operations
- [ ] Use streaming for large data
- [ ] Dispose stores after use
- [ ] Profile hot paths
- [ ] Measure before/after

End Command ---
```
