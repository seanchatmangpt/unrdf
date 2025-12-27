# Knowledge Hooks Benchmarking

This directory contains comprehensive performance benchmarking specifications and implementations for the UNRDF knowledge hooks system.

## Quick Start

```bash
# Run all benchmarks
npm run benchmark

# Run specific benchmark
npm run benchmark:registration
npm run benchmark:execution
npm run benchmark:validation
npm run benchmark:memory
npm run benchmark:concurrent

# Run with OTEL validation
npm run benchmark -- --validate-otel

# Compare against baseline
npm run benchmark -- --compare-baseline

# Generate HTML report
npm run benchmark -- --report-html
```

## 📁 Directory Structure

```
benchmark/
├── specs/                              # Benchmark specifications
│   ├── knowledge-hooks-benchmark.spec.json    # Full JSON spec
│   └── BENCHMARK-SPECIFICATION.md             # Human-readable spec
├── results/                            # Benchmark results (gitignored)
│   ├── {timestamp}-{benchmark-id}.json
│   ├── {timestamp}-{benchmark-id}.md
│   └── {timestamp}-{benchmark-id}.html
├── baseline.json                       # Baseline for regression detection
├── knowledge-hooks.benchmark.mjs       # Main benchmark implementation
└── README.md                           # This file
```

## 🎯 Benchmark Suite Overview

Based on **80/20 principles**, we focus on **5 core benchmarks** that measure 80% of performance characteristics:

### 1. Hook Registration (P1)
- **What:** Speed of registering hooks to knowledge engine
- **Target:** < 1ms per hook
- **Why:** Fast registration enables dynamic hook management

### 2. Hook Execution (P1)
- **What:** Execution latency under various conditions
- **Target:** < 10ms p99
- **Why:** Tail latency impacts user experience most

### 3. Hook Validation (P2)
- **What:** Schema validation throughput and accuracy
- **Target:** > 10,000 ops/sec
- **Why:** Validation is on critical path

### 4. Memory Profiling (P1)
- **What:** Peak memory and GC pressure
- **Target:** < 50MB per 1000 hooks
- **Why:** Memory must scale linearly

### 5. Concurrent Execution (P1)
- **What:** Throughput with parallel executions
- **Target:** > 500 ops/sec @ 10 workers
- **Why:** Real-world workloads are concurrent

## 📊 Key Metrics

### Latency (Always use percentiles, not just average!)
- **p50 (Median):** Typical case performance
- **p95:** 95% of requests are faster than this
- **p99:** Tail latency - critical for UX
- **Max:** Worst case scenario

### Throughput
- **ops/sec:** Operations per second
- **Total ops:** Total operations completed
- **Duration:** Time to complete benchmark

### Memory
- **Peak:** Maximum memory usage
- **Average:** Typical memory usage
- **GC frequency:** Garbage collection frequency
- **GC pause:** GC pause duration (p95, p99)

### Reliability
- **Error rate:** Percentage of failed operations
- **Timeout rate:** Percentage of timed out operations

## 🔍 OTEL Validation

Every benchmark result is validated using OpenTelemetry spans. This ensures:

1. ✅ **Correctness:** Benchmarks measure what they claim
2. ✅ **Completeness:** All operations are accounted for
3. ✅ **Reliability:** No silent failures or incorrect measurements

### Required OTEL Spans

```
benchmark.{benchmark-id}                    # Root span
├─ benchmark.measure                        # Measurement span
│   ├─ operation.{operation-name}          # Individual operations
│   └─ latency.measure                     # Latency measurement
└─ validation                              # Validation span
```

### Validation Checks

Before accepting results:
- [ ] Root span exists with correct attributes
- [ ] All child spans present
- [ ] Span status is OK (no errors)
- [ ] All required metrics recorded
- [ ] Baseline comparison complete

## 🎯 Baseline Targets

| Metric | Target | Priority |
|--------|--------|----------|
| Hook registration | < 1ms avg | P1 |
| Hook execution | < 10ms p99 | P1 |
| Memory per 1k hooks | < 50MB | P1 |
| Validation | > 10k ops/sec | P2 |
| Error rate | < 0.1% | P1 |

## 🚨 Regression Detection

Benchmarks automatically detect performance regressions by comparing to baseline:

| Metric | Warning | Critical |
|--------|---------|----------|
| **Latency** | +10% | +20% |
| **Throughput** | -10% | -20% |
| **Memory** | +15% | +30% |
| **Error Rate** | +50% | +100% |

**Example:**
```
⚠️  WARNING: Regression detected!
  - Hook execution p99: 12.5ms (was 9.8ms) +27.6% ⚠️
  - Registration throughput: 850 ops/sec (was 1100) -22.7% ⚠️
```

## 📈 Reports

Each benchmark run generates three report formats:

### 1. JSON Report (Machine-readable)
```json
{
  "benchmarkId": "hook-execution",
  "timestamp": "2025-12-04T10:30:00Z",
  "results": { ... },
  "validation": { "status": "PASS", ... },
  "regression": { "detected": false, ... }
}
```

### 2. Markdown Report (Human-readable)
```markdown
# Hook Execution Benchmark Results

**Status:** ✅ PASS
**Duration:** 1250ms
**Timestamp:** 2025-12-04 10:30:00

## Results
- p50 Latency: 2.8ms ✅ (target: < 3ms)
- p99 Latency: 9.2ms ✅ (target: < 10ms)
...
```

### 3. HTML Report (Interactive with charts)
- Latency distribution histogram
- Throughput over time line chart
- Memory usage over time area chart
- Baseline comparison bar chart

## 🛠️ Implementation Guide

### 1. Setup

```bash
# Install dependencies
pnpm install

# Create baseline (first run)
npm run benchmark -- --create-baseline
```

### 2. Running Benchmarks

```javascript
// Simple usage
import { runBenchmarks } from './knowledge-hooks.benchmark.mjs';

const results = await runBenchmarks({
  benchmarks: ['hook-registration', 'hook-execution'],
  compareBaseline: true,
  validateOtel: true,
  exportResults: true
});
```

### 3. Custom Benchmark

```javascript
import { Bench } from 'tinybench';
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('my-benchmark');

const bench = new Bench({ time: 1000 });

bench.add('my-operation', () => {
  const span = tracer.startActiveSpan('benchmark.my-operation', (span) => {
    // Your operation here
    const result = myOperation();

    // Record metrics
    span.setAttribute('benchmark.result', result);
    span.end();

    return result;
  });
});

await bench.run();
```

## 🎓 Best Practices

### DO ✅
- Use percentiles (p50, p95, p99) instead of just average
- Force GC between benchmarks to prevent interference
- Warm up JIT compiler before measurements
- Record full system context (CPU, memory, Node version)
- Compare against baseline to detect regressions
- Export OTEL traces for validation
- Use statistical significance tests (p-value < 0.05)

### DON'T ❌
- Don't rely on just average latency (hides tail latency)
- Don't run benchmarks on busy systems (skews results)
- Don't skip warmup runs (JIT not optimized)
- Don't ignore memory profiling (can cause OOM in prod)
- Don't skip OTEL validation (catches measurement errors)
- Don't benchmark in production (use staging/dev)

## 📚 References

- **Full Specification:** [BENCHMARK-SPECIFICATION.md](./specs/BENCHMARK-SPECIFICATION.md)
- **JSON Spec:** [knowledge-hooks-benchmark.spec.json](./specs/knowledge-hooks-benchmark.spec.json)
- **OTEL Documentation:** https://opentelemetry.io/docs/
- **Tinybench:** https://github.com/tinylibs/tinybench
- **Vitest:** https://vitest.dev/

## 🔧 Troubleshooting

### Benchmark failing with "Insufficient measurements"
- Increase `measurementRuns` in spec
- Decrease operation complexity
- Check if timeout is too short

### High variance in results
- Force GC between runs: `if (global.gc) global.gc()`
- Run with `--expose-gc` flag: `node --expose-gc benchmark.mjs`
- Increase warmup runs
- Close other applications

### OTEL validation failing
- Check span status: should be "OK"
- Verify all required attributes present
- Check trace export configuration
- Review OTEL collector logs

### Memory leak detected
- Profile with Chrome DevTools: `node --inspect benchmark.mjs`
- Check for event listener leaks
- Review closure retention
- Use weak references where appropriate

## 📞 Support

For issues or questions:
1. Check [BENCHMARK-SPECIFICATION.md](./specs/BENCHMARK-SPECIFICATION.md)
2. Review benchmark implementation
3. Check OTEL traces for errors
4. Open GitHub issue with benchmark results

---

**Remember:** Benchmarks lie. Always validate with OTEL spans and real-world testing! 🎯
