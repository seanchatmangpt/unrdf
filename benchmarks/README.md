# Knowledge Hooks Performance Benchmarks

Comprehensive benchmark suite for measuring Knowledge Hooks performance with OpenTelemetry integration.

## Overview

This benchmark suite implements the **80/20 principle**: 5 core benchmarks that measure 80% of performance characteristics:

1. **Hook Registration** - Measure hook registration speed and memory overhead
2. **Hook Execution** - Measure execution latency under varying complexity
3. **Hook Validation** - Measure Zod schema validation throughput
4. **Memory Profiling** - Measure peak memory usage and GC pressure
5. **Concurrent Execution** - Measure throughput with parallel workers

## Quick Start

### Run All Benchmarks

```bash
# Run complete benchmark suite
node benchmark/examples/hook-registration.benchmark.mjs

# Run with OTEL export
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318 \
  node benchmark/examples/hook-registration.benchmark.mjs
```

### Run Specific Benchmark

```bash
# Hook Registration Benchmark
node benchmark/examples/hook-registration.benchmark.mjs

# Memory Profiling (with GC enabled)
node --expose-gc benchmark/examples/memory-profiling.benchmark.mjs
```

### Run Tests

```bash
# Run validation tests
pnpm test benchmarks/validation.test.mjs

# Run integration tests
pnpm test benchmarks/integration.test.mjs

# Run all benchmark tests
pnpm test benchmarks/
```

## Installation

### Install Dependencies

```bash
# Install benchmark dependencies
pnpm add -D tinybench @opentelemetry/api @opentelemetry/sdk-node
```

### Setup OTEL Collector (Optional)

```bash
# Run OTEL collector for trace export
docker run -p 4318:4318 otel/opentelemetry-collector:latest
```

## Running Benchmarks

### Basic Usage

```javascript
import { runHookRegistrationBenchmark } from './benchmark/examples/hook-registration.benchmark.mjs';

// Run benchmark
const results = await runHookRegistrationBenchmark();

console.log('Results:', results);
console.log('Status:', results.validation.status);
console.log('Targets Met:', results.validation.targetsMet, '/', results.validation.targetsTotal);
```

### With Custom Configuration

```javascript
const BENCHMARK_CONFIG = {
  id: 'hook-registration',
  scenario: 'large-batch',
  parameters: {
    hookCount: 10000,
    complexity: 'complex',
    warmupRuns: 10,
    measurementRuns: 100,
    timeout: 30000
  },
  expectedTargets: {
    avgLatency: 1,
    p95Latency: 5,
    throughput: 1000,
    memoryOverhead: 100
  }
};
```

### Enable Garbage Collection

For accurate memory measurements, run benchmarks with GC exposed:

```bash
node --expose-gc benchmark/examples/hook-registration.benchmark.mjs
```

## Interpreting Results

### Result Structure

```json
{
  "benchmarkId": "hook-registration",
  "scenario": "medium-batch",
  "timestamp": "2025-12-04T10:30:00Z",
  "results": {
    "totalOps": 1000,
    "totalDuration": 850.5,
    "meanLatency": 0.85,
    "p50Latency": 0.78,
    "p95Latency": 1.8,
    "p99Latency": 3.2,
    "throughput": 1176,
    "memory": {
      "baseline": 15.2,
      "current": 37.7,
      "overhead": 22.5,
      "per1kHooks": 22.5
    }
  },
  "validation": {
    "status": "PASS",
    "targetsMet": 5,
    "targetsTotal": 5,
    "failures": []
  }
}
```

### Key Metrics Explained

#### Latency Metrics

- **Mean Latency**: Average operation time
- **p50 (Median)**: 50% of operations complete faster
- **p95**: 95% of operations complete faster (important for SLAs)
- **p99**: 99% of operations complete faster (tail latency)

#### Throughput Metrics

- **Throughput**: Operations per second
- **Total Duration**: Total benchmark execution time
- **Total Ops**: Number of operations measured

#### Memory Metrics

- **Baseline**: Memory usage before benchmark
- **Current**: Memory usage after benchmark
- **Overhead**: Memory increase during benchmark
- **Per 1k Hooks**: Memory used per 1000 hooks (normalized)

### Validation Status

- ✅ **PASS**: All baseline targets met
- ❌ **FAIL**: One or more targets not met

Check `validation.failures` array for specific metrics that didn't meet targets.

## Baseline Targets

### Hook Registration

| Scenario | Hooks | Avg Latency | p95 Latency | Throughput | Memory |
|----------|-------|-------------|-------------|------------|--------|
| Small | 100 | < 0.5ms | < 1ms | > 2000/sec | < 5MB |
| Medium | 1,000 | < 1ms | < 2ms | > 1000/sec | < 25MB |
| Large | 10,000 | < 1ms | < 5ms | > 1000/sec | < 100MB |

### Hook Execution

| Complexity | p50 Latency | p99 Latency | Throughput |
|------------|-------------|-------------|------------|
| Simple | < 1ms | < 10ms | > 1000/sec |
| Medium | < 3ms | < 15ms | > 200/sec |
| Complex | < 10ms | < 50ms | > 50/sec |

### Validation

| Operation | Throughput | Avg Latency | Accuracy |
|-----------|------------|-------------|----------|
| Schema | > 10,000/sec | < 0.1ms | > 99.9% |
| Runtime | > 5,000/sec | < 0.2ms | > 99.5% |

### Memory Profiling

| Test | Peak Memory | GC Pause (p95) | Memory Leak |
|------|-------------|----------------|-------------|
| Under Load | < 50MB/1k hooks | < 10ms | < 1MB/min |
| Stress | < 500MB | < 50ms | < 1MB/min |

### Concurrent Execution

| Workers | Total Throughput | p99 Latency | Contention |
|---------|------------------|-------------|------------|
| 10 | > 500/sec | < 100ms | < 5% |
| 100 | > 2000/sec | < 300ms | < 15% |
| 1000 | > 1000/sec | < 1000ms | < 30% |

## Regression Detection

### Thresholds

Benchmarks automatically detect performance regressions:

| Metric | Warning | Critical |
|--------|---------|----------|
| Latency | +10% | +20% |
| Throughput | -10% | -20% |
| Memory | +15% | +30% |
| Error Rate | +50% | +100% |

### Baseline Comparison

```bash
# Generate baseline
node benchmark/examples/hook-registration.benchmark.mjs > baseline.json

# Compare against baseline
node benchmark/examples/hook-registration.benchmark.mjs --compare baseline.json
```

## CI/CD Integration

### GitHub Actions

```yaml
name: Performance Benchmarks

on:
  pull_request:
    branches: [main]

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: pnpm/action-setup@v2
      - uses: actions/setup-node@v4
        with:
          node-version: '20'
          cache: 'pnpm'

      - run: pnpm install
      - run: pnpm test benchmarks/

      - name: Run Benchmarks
        run: |
          node --expose-gc benchmark/examples/hook-registration.benchmark.mjs \
            > benchmark-results.json

      - name: Compare Against Baseline
        run: |
          node scripts/compare-benchmarks.mjs \
            baseline.json \
            benchmark-results.json

      - name: Comment on PR
        uses: actions/github-script@v7
        with:
          script: |
            const results = require('./benchmark-results.json');
            const comment = `## Benchmark Results

            Status: ${results.validation.status}
            Targets Met: ${results.validation.targetsMet}/${results.validation.targetsTotal}

            | Metric | Value | Target | Status |
            |--------|-------|--------|--------|
            | Mean Latency | ${results.results.meanLatency}ms | < 1ms | ✅ |
            | p95 Latency | ${results.results.p95Latency}ms | < 2ms | ✅ |
            | Throughput | ${results.results.throughput}/sec | > 1000/sec | ✅ |
            `;

            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: comment
            });

      - name: Fail on Regression
        run: |
          if [ $(jq '.validation.status' benchmark-results.json) == '"FAIL"' ]; then
            echo "Benchmark regression detected!"
            exit 1
          fi
```

## OTEL Integration

### Span Structure

Every benchmark creates:

```
benchmark.{benchmark-id}          [Root Span]
  ├─ benchmark.setup              [Setup Phase]
  ├─ benchmark.warmup             [Warmup Phase]
  ├─ benchmark.measure            [Measurement Phase]
  ├─ memory.measure               [Memory Measurement]
  └─ benchmark.validation         [Validation Phase]
```

### Required Span Attributes

```javascript
{
  "benchmark.suite.name": "Knowledge Hooks Performance Benchmark Suite",
  "benchmark.suite.version": "1.0.0",
  "benchmark.id": "hook-registration",
  "benchmark.name": "Hook Registration Benchmark",
  "benchmark.timestamp": "2025-12-04T10:30:00Z",
  "benchmark.latency.mean.ms": 0.85,
  "benchmark.latency.p50.ms": 0.78,
  "benchmark.latency.p95.ms": 1.8,
  "benchmark.latency.p99.ms": 3.2,
  "benchmark.throughput.ops_per_sec": 1176,
  "memory.overhead.mb": 22.5,
  "validation.status": "PASS",
  "system.platform": "linux",
  "process.nodeVersion": "v20.10.0"
}
```

### Exporting Traces

```bash
# Export to OTLP endpoint
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318 \
OTEL_SERVICE_NAME=knowledge-hooks-benchmark \
  node benchmark/examples/hook-registration.benchmark.mjs

# Export to file
OTEL_TRACES_EXPORTER=file \
OTEL_EXPORTER_FILE_PATH=traces.json \
  node benchmark/examples/hook-registration.benchmark.mjs
```

## Troubleshooting

### Memory Measurements Inaccurate

**Problem**: Memory measurements show inconsistent results

**Solution**: Run with `--expose-gc` flag to enable manual garbage collection:

```bash
node --expose-gc benchmark/examples/hook-registration.benchmark.mjs
```

### Benchmarks Taking Too Long

**Problem**: Benchmarks exceed timeout

**Solution**: Reduce measurement runs or increase timeout:

```javascript
const BENCHMARK_CONFIG = {
  parameters: {
    measurementRuns: 10, // Reduce from 50
    timeout: 30000 // Increase to 30s
  }
};
```

### High Variance in Results

**Problem**: Results vary significantly between runs

**Solution**: Increase warmup runs and measurement runs:

```javascript
const BENCHMARK_CONFIG = {
  parameters: {
    warmupRuns: 10, // Increase warmup
    measurementRuns: 100 // Increase measurements
  }
};
```

### OTEL Spans Not Created

**Problem**: OTEL traces are empty

**Solution**: Ensure OTEL is properly initialized:

```javascript
import { trace } from '@opentelemetry/api';
import { NodeTracerProvider } from '@opentelemetry/sdk-trace-node';

const provider = new NodeTracerProvider();
provider.register();

const tracer = trace.getTracer('benchmark', '1.0.0');
```

### Regression Detected in CI

**Problem**: CI fails with regression error

**Solution**: Review baseline and update if change is expected:

```bash
# Regenerate baseline after intentional changes
node benchmark/examples/hook-registration.benchmark.mjs > baseline.json
git add baseline.json
git commit -m "Update benchmark baseline"
```

## Common Issues

### Issue: `global.gc is not a function`

Run with `--expose-gc` flag.

### Issue: `Cannot find module 'tinybench'`

Install dependencies: `pnpm install`

### Issue: Benchmark hangs

Increase timeout in configuration or reduce number of operations.

### Issue: Out of memory

Reduce `hookCount` or `measurementRuns` in configuration.

## Advanced Usage

### Custom Percentile Calculation

```javascript
function calculatePercentile(values, percentile) {
  const sorted = [...values].sort((a, b) => a - b);
  const index = Math.ceil((percentile / 100) * sorted.length) - 1;
  return sorted[Math.max(0, index)];
}

const p50 = calculatePercentile(samples, 50);
const p95 = calculatePercentile(samples, 95);
const p99 = calculatePercentile(samples, 99);
```

### Memory Leak Detection

```javascript
function detectMemoryLeak(measurements, threshold = 1) {
  // measurements: array of memory usage over time (MB)
  const first = measurements[0];
  const last = measurements[measurements.length - 1];
  const duration = measurements.length; // minutes

  const growthRate = (last - first) / duration;
  return growthRate > threshold; // MB per minute
}
```

### Baseline Generation

```javascript
// Generate baseline from multiple runs
async function generateBaseline(runs = 10) {
  const results = [];

  for (let i = 0; i < runs; i++) {
    const result = await runBenchmark();
    results.push(result);
  }

  // Calculate statistical baseline
  return {
    meanLatency: average(results.map(r => r.meanLatency)),
    p95Latency: average(results.map(r => r.p95Latency)),
    throughput: average(results.map(r => r.throughput)),
    memoryOverhead: average(results.map(r => r.memory.overhead))
  };
}
```

## References

- [Benchmark Specification](../benchmark/specs/BENCHMARK-SPECIFICATION.md)
- [Example Implementation](../benchmark/examples/hook-registration.benchmark.mjs)
- [OpenTelemetry Docs](https://opentelemetry.io/docs/instrumentation/js/)
- [tinybench Documentation](https://github.com/tinylibs/tinybench)

## Support

For issues or questions:

- **Issues**: https://github.com/unrdf/unrdf/issues
- **Discussions**: https://github.com/unrdf/unrdf/discussions
- **Documentation**: https://unrdf.github.io/docs/benchmarks

---

## Benchmark Infrastructure Utilities

The `/benchmarks/utils/` directory provides reusable utilities for all benchmark implementations:

### Utility Modules

#### 1. OtelCollector (`utils/otel-collector.mjs`)

Lightweight OTEL-compatible span collector for benchmark validation.

```javascript
import { OtelCollector } from './utils/otel-collector.mjs';

const collector = new OtelCollector('my-benchmark');
const span = collector.startSpan('operation', { param: 'value' });
// ... perform operation ...
collector.endSpan(span, true, { result: 'success' });

const summary = collector.getSummary();
const validation = collector.validateSpans();
```

#### 2. PercentileCalculator (`utils/percentile-calculator.mjs`)

Statistical analysis for latency measurements.

```javascript
import { PercentileCalculator } from './utils/percentile-calculator.mjs';

const calc = new PercentileCalculator();
measurements.forEach(m => calc.addMeasurement(m));

const stats = calc.getStats();
console.log(`p50: ${stats.p50}ms, p95: ${stats.p95}ms, p99: ${stats.p99}ms`);
```

#### 3. MemoryProfiler (`utils/memory-profiler.mjs`)

GC-aware memory measurement and leak detection.

```javascript
import { MemoryProfiler } from './utils/memory-profiler.mjs';

// Measure operation memory
const result = await MemoryProfiler.measureMemory(async () => {
  return await myOperation();
});

console.log(`Delta: ${MemoryProfiler.formatBytes(result.deltaHeapUsed)}`);

// Detect memory leaks
const leak = await MemoryProfiler.detectLeak(baseline, cleanup, 0.05);
if (leak.isLeak) console.warn('Memory leak detected!');
```

**Requires**: Run with `--expose-gc` for accurate measurements

#### 4. MetricsAggregator (`utils/metrics-aggregator.mjs`)

Results aggregation and regression detection.

```javascript
import { MetricsAggregator } from './utils/metrics-aggregator.mjs';

const aggregator = MetricsAggregator.loadBaseline('./baselines/baseline.json');

aggregator.recordResult('hook-registration', 'simple', 'latency-avg', 0.8, 'ms');
aggregator.recordResult('hook-registration', 'simple', 'throughput', 1200, 'ops/sec');

const { regressions, warnings } = aggregator.checkRegressions();
console.log(aggregator.generateReport());
aggregator.exportJSON('./results/results.json');
```

#### 5. Test Fixtures (`utils/test-fixtures.mjs`)

Reusable test data for consistent benchmarking.

```javascript
import {
  createSimpleHook,
  createHookArray,
  createEvent,
  createStore
} from './utils/test-fixtures.mjs';

const hooks = createHookArray(1000, 'bench-hook');
const event = createEvent('test:event');
const store = createStore(100);
```

### Baseline Configuration

Performance targets are defined in `/benchmarks/baselines/baseline.json`:

```json
{
  "benchmarks": {
    "hook-registration": {
      "latency-avg": { "target": 1, "unit": "ms", "priority": "P1" },
      "throughput": { "target": 1000, "unit": "ops/sec", "priority": "P1" }
    }
  },
  "regressionThresholds": {
    "critical": { "latency": 20, "throughput": 20, "memory": 30 },
    "warning": { "latency": 10, "throughput": 10, "memory": 15 }
  }
}
```

### Verification

Verify all utilities are working:

```bash
node benchmarks/verify-utils.mjs
```

---

**Note**: Always run benchmarks in a consistent environment for reliable results. Avoid running other processes during benchmarking.
