# Knowledge Hooks Benchmark Specification

**Version:** 1.0.0
**Principle:** 80/20 - Focus on 5 core benchmarks that measure 80% of performance characteristics

## Overview

This specification defines comprehensive performance benchmarking for knowledge hooks with OTEL integration. The suite focuses on 5 critical benchmarks that capture the essential performance characteristics of the knowledge hooks system.

## Core Benchmark Suite

### 1. Hook Registration Benchmark (P1)

**Purpose:** Measure the speed of registering N hooks to the knowledge engine

**Scenarios:**
- **Small Batch**: 100 hooks
  - Target: < 0.5ms per hook, > 2000 hooks/sec
  - Memory: < 5MB overhead

- **Medium Batch**: 1,000 hooks
  - Target: < 1ms per hook (avg), < 2ms (p95)
  - Memory: < 25MB overhead

- **Large Batch**: 10,000 hooks
  - Target: < 1ms per hook (avg), < 5ms (p99)
  - Memory: < 100MB overhead

**OTEL Spans:**
```
benchmark.hook-registration
  ├─ hook.register
  ├─ hook.validate
  └─ memory.measure
```

**Key Metrics:**
- `registration_latency_ms` (p50, p95, p99)
- `registration_throughput_ops_per_sec`
- `memory_peak_mb`
- `gc_count` / `gc_duration_ms`

---

### 2. Hook Execution Benchmark (P1)

**Purpose:** Measure execution latency of hooks under various complexity levels

**Scenarios:**
- **Simple Hook**: Validate + log operations
  - Target: < 1ms (p50), < 10ms (p99)
  - Throughput: > 1000 ops/sec

- **Medium Hook**: Validate + transform + store operations
  - Target: < 3ms (p50), < 15ms (p99)
  - Throughput: > 200 ops/sec

- **Complex Hook**: Async fetch + transform + transaction + notify
  - Target: < 10ms (p50), < 50ms (p99)
  - Throughput: > 50 ops/sec

**OTEL Spans:**
```
benchmark.hook-execution
  ├─ hook.execute
  ├─ hook.validate
  ├─ hook.transform
  ├─ store.operation
  └─ latency.measure
```

**Key Metrics:**
- `execution_latency_p50_ms`
- `execution_latency_p95_ms`
- `execution_latency_p99_ms`
- `execution_throughput_ops_per_sec`
- `execution_error_count`

---

### 3. Hook Validation Benchmark (P2)

**Purpose:** Measure validation throughput and accuracy for hook schemas

**Scenarios:**
- **Schema Validation**: 10,000 Zod schema validations
  - Target: > 10,000 ops/sec
  - Latency: < 0.1ms (avg), < 1ms (p99)
  - Accuracy: > 99.9%

- **Runtime Validation**: 5,000 payload validations
  - Target: > 5,000 ops/sec
  - Latency: < 0.2ms (avg), < 2ms (p99)
  - Accuracy: > 99.5%

**OTEL Spans:**
```
benchmark.hook-validation
  ├─ validation.schema
  ├─ validation.runtime
  └─ validation.accuracy.measure
```

**Key Metrics:**
- `validation_throughput_ops_per_sec`
- `validation_accuracy_percent`
- `validation_false_positive_count`
- `validation_false_negative_count`

---

### 4. Memory Profiling Benchmark (P1)

**Purpose:** Measure peak memory usage and GC pressure during operations

**Scenarios:**
- **Memory Under Load**: 1,000 hooks + 10,000 executions
  - Target: < 50MB peak per 1000 hooks
  - GC: < 10 GC/sec, < 10ms pause (p95)

- **Memory Stress Test**: 10,000 hooks + 100,000 executions
  - Target: < 500MB peak
  - Memory leak: < 1MB/min
  - GC: < 20 GC/sec, < 50ms pause (p99)

**OTEL Spans:**
```
benchmark.memory-profiling
  ├─ memory.measure
  ├─ memory.gc.track
  └─ memory.leak.detect
```

**Key Metrics:**
- `memory_peak_mb`
- `memory_average_mb`
- `memory_growth_rate_mb_per_min`
- `gc_pause_p95_ms`
- `memory_leak_detected`

---

### 5. Concurrent Execution Benchmark (P1)

**Purpose:** Measure throughput with multiple parallel hook executions

**Scenarios:**
- **Low Concurrency**: 10 concurrent workers
  - Target: > 500 ops/sec total
  - Latency: < 20ms (p50), < 100ms (p99)
  - Contention: < 5%

- **Medium Concurrency**: 100 concurrent workers
  - Target: > 2000 ops/sec total
  - Latency: < 50ms (p50), < 300ms (p99)
  - Contention: < 15%

- **High Concurrency**: 1000 concurrent workers (stress)
  - Target: > 1000 ops/sec total
  - Latency: < 100ms (p50), < 1000ms (p99)
  - Contention: < 30%

**OTEL Spans:**
```
benchmark.concurrent-execution
  ├─ concurrency.worker.spawn
  ├─ concurrency.execution
  └─ concurrency.contention.measure
```

**Key Metrics:**
- `concurrent_throughput_total_ops_per_sec`
- `concurrent_throughput_per_worker_ops_per_sec`
- `concurrent_latency_p95_ms`
- `concurrent_contention_rate`

---

## Baseline Targets (80/20 Focus)

### 5 Critical Metrics

| Metric | Target | Priority | Reasoning |
|--------|--------|----------|-----------|
| **Hook Registration** | < 1ms avg | P1 | Fast registration enables dynamic hook management |
| **Hook Execution** | < 10ms p99 | P1 | Tail latency impacts user experience most |
| **Memory Efficiency** | < 50MB per 1000 hooks | P1 | Memory overhead must scale linearly |
| **Validation Throughput** | > 10,000 ops/sec | P2 | Schema validation is on critical path |
| **Error Rate** | < 0.1% | P1 | Reliability is non-negotiable for production |

### Regression Thresholds

| Metric | Warning | Critical |
|--------|---------|----------|
| Latency | +10% | +20% |
| Throughput | -10% | -20% |
| Memory | +15% | +30% |
| Error Rate | +50% | +100% |

---

## OTEL Integration Requirements

### Required Span Attributes (All Benchmarks)

**Common Attributes:**
```javascript
{
  "benchmark.suite.name": "Knowledge Hooks Performance Benchmark Suite",
  "benchmark.suite.version": "1.0.0",
  "benchmark.id": "hook-registration",
  "benchmark.name": "Hook Registration Benchmark",
  "benchmark.category": "registration",
  "benchmark.timestamp": "2025-12-04T10:30:00Z",
  "benchmark.duration.ms": 1250,
  "benchmark.status": "OK",
  "system.hostname": "localhost",
  "system.platform": "linux",
  "system.arch": "x64",
  "process.pid": 12345,
  "process.nodeVersion": "v20.10.0"
}
```

### Span Structure

Every benchmark must create:
1. **Root Span**: `benchmark.{benchmark-id}`
2. **Child Spans**: Operation-specific spans (validate, execute, measure)
3. **Status**: All spans must have status "OK" for passing benchmarks
4. **Attributes**: All required attributes must be present

### Validation Checks

Before accepting benchmark results:
1. ✅ Root span exists for each benchmark
2. ✅ All required attributes present
3. ✅ Span status is OK (no errors)
4. ✅ Child spans present for major operations
5. ✅ Metrics exported correctly
6. ✅ Trace sampling is consistent

---

## Implementation Guidelines

### Benchmark Execution Flow

**1. Setup Phase**
```javascript
// Initialize OTEL tracer and meter
const tracer = trace.getTracer('knowledge-hooks-benchmark');
const meter = metrics.getMeter('knowledge-hooks-benchmark');

// Create root span
const span = tracer.startActiveSpan('benchmark.hook-registration', (span) => {
  // Set common attributes
  span.setAttribute('benchmark.id', 'hook-registration');
  span.setAttribute('benchmark.timestamp', new Date().toISOString());

  // Setup test data
  const hooks = generateTestHooks(1000);

  // Warm up JIT
  for (let i = 0; i < 10; i++) {
    registerHook(hooks[i]);
  }

  // Force initial GC
  if (global.gc) global.gc();

  // Record baseline memory
  const baselineMemory = process.memoryUsage();
});
```

**2. Execution Phase**
```javascript
// Start measurement span
const measureSpan = tracer.startActiveSpan('benchmark.measure', (span) => {
  const bench = new Bench({ time: 1000 });

  bench.add('hook-registration', () => {
    registerHook(testHook);
  });

  await bench.run();

  // Calculate percentiles
  const p50 = calculatePercentile(results, 0.50);
  const p95 = calculatePercentile(results, 0.95);
  const p99 = calculatePercentile(results, 0.99);

  // Record metrics
  span.setAttribute('benchmark.latency.p50.ms', p50);
  span.setAttribute('benchmark.latency.p95.ms', p95);
  span.setAttribute('benchmark.latency.p99.ms', p99);
});
```

**3. Teardown Phase**
```javascript
// Force GC
if (global.gc) global.gc();

// Record final memory
const finalMemory = process.memoryUsage();
span.setAttribute('benchmark.memory.peak.mb', finalMemory.heapUsed / 1024 / 1024);

// Validate against baseline
const regressionDetected = compareToBaseline(results, baseline);
span.setAttribute('validation.regressionDetected', regressionDetected);

// Close span
span.setStatus({ code: SpanStatusCode.OK });
span.end();

// Export traces
await tracer.forceFlush();
```

### Best Practices

1. **Use tinybench** for accurate microbenchmarking
2. **Isolate benchmarks** in separate processes to prevent interference
3. **Force GC** between benchmarks to prevent memory contamination
4. **Use percentiles** (p50, p95, p99) instead of just average
5. **Measure hot and cold paths** separately
6. **Include regression detection** against baseline
7. **Record full context** (CPU, memory, Node version, git commit)
8. **Export OTEL traces** for validation
9. **Generate reports** in multiple formats (JSON, Markdown, HTML)

---

## Validation Criteria

### Benchmark Pass Criteria

A benchmark passes if:
- ✅ All baseline targets are met
- ✅ All required OTEL spans exist with OK status
- ✅ No critical regressions vs baseline (< 20%)
- ✅ Minimum 100 measurements per scenario
- ✅ Statistical significance (p-value < 0.05)

### Report Requirements

Every benchmark run must generate:
1. **JSON results file** with all metrics
2. **Markdown summary** with pass/fail status
3. **Comparison table** vs baseline
4. **Latency distribution charts**
5. **Throughput over time charts**
6. **Memory profile visualization**
7. **OTEL trace export file**
8. **System information** (OS, CPU, memory, Node version)

---

## Example Benchmark Result

```json
{
  "benchmarkId": "hook-registration",
  "scenario": "medium-batch",
  "timestamp": "2025-12-04T10:30:00Z",
  "duration": 1250,
  "results": {
    "hookCount": 1000,
    "avgLatency": 0.85,
    "p95Latency": 1.8,
    "p99Latency": 3.2,
    "throughput": 1176,
    "memoryPeak": 22.5,
    "errorRate": 0.05
  },
  "targets": {
    "avgLatency": "< 1ms",
    "p95Latency": "< 2ms",
    "throughput": "> 1000 hooks/sec",
    "memoryOverhead": "< 25MB",
    "errorRate": "< 0.1%"
  },
  "validation": {
    "status": "PASS",
    "targetsMet": 5,
    "targetsTotal": 5,
    "regressionDetected": false,
    "baselineComparison": {
      "latency": "+2.4%",
      "throughput": "+5.1%",
      "memory": "-3.2%"
    }
  },
  "otel": {
    "traceId": "a1b2c3d4e5f6...",
    "rootSpanId": "12345678...",
    "spanCount": 4,
    "spanStatus": "OK"
  }
}
```

---

## Implementation Checklist

- [ ] Setup tinybench + vitest environment
- [ ] Implement OTEL tracer and meter
- [ ] Create test data generators for each scenario
- [ ] Implement 5 core benchmarks
- [ ] Add latency percentile calculation
- [ ] Add memory profiling with GC tracking
- [ ] Implement baseline comparison
- [ ] Create regression detection
- [ ] Export OTEL traces
- [ ] Generate JSON reports
- [ ] Generate Markdown reports
- [ ] Generate HTML reports with charts
- [ ] Add CI/CD integration
- [ ] Document usage and interpretation

---

## Files

- **Specification:** `/home/user/unrdf/benchmark/specs/knowledge-hooks-benchmark.spec.json`
- **Documentation:** `/home/user/unrdf/benchmark/specs/BENCHMARK-SPECIFICATION.md`
- **Implementation:** `/home/user/unrdf/benchmark/knowledge-hooks.benchmark.mjs` (to be created)
- **Results:** `/home/user/unrdf/benchmark/results/` (output directory)
- **Baseline:** `/home/user/unrdf/benchmark/baseline.json` (to be created)
