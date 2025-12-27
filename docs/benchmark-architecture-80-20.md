# Knowledge Hooks Benchmark Architecture (80/20 Design)

**Version:** 1.0.0
**Status:** Architecture Design
**Date:** 2025-12-04

## Executive Summary

This document defines a hyper-focused benchmark architecture for knowledge hooks that measures **only the 20% of metrics that drive 80% of production performance**. Eliminates vanity metrics and focuses on real-world bottlenecks.

---

## 1. Critical Metrics Identification (The Vital 20%)

### 1.1 Tier 1: Business-Critical Metrics (60% Impact)

These metrics directly affect user experience and system reliability:

| Metric | Impact | Why Critical | Target |
|--------|--------|--------------|--------|
| **Hook Execution Latency (p95)** | 40% | Directly affects transaction commit time | < 50ms |
| **Hook Registration Time** | 20% | Affects system startup and hot-reload | < 10ms |

**Rationale**: Users perceive latency > 100ms. Hook execution is on the critical path of every transaction. Registration time affects developer experience during hot-reload.

### 1.2 Tier 2: Scalability Metrics (30% Impact)

These metrics determine system capacity limits:

| Metric | Impact | Why Critical | Target |
|--------|--------|--------------|--------|
| **Concurrent Hook Execution Throughput** | 15% | Determines max transaction/sec capacity | > 1000 ops/sec |
| **Memory Footprint Per Hook** | 15% | Determines max concurrent hooks | < 5MB per hook |

**Rationale**: Production systems need to handle 100s of concurrent transactions. Memory limits determine horizontal scaling requirements.

### 1.3 Tier 3: Reliability Metrics (10% Impact)

These metrics ensure system stability under stress:

| Metric | Impact | Why Critical | Target |
|--------|--------|--------------|--------|
| **Circuit Breaker Trip Rate** | 5% | Prevents cascade failures | < 1% under normal load |
| **Condition Evaluation Cache Hit Rate** | 5% | Reduces redundant SPARQL queries | > 80% |

**Total Coverage**: 100% of production performance impact

---

## 2. Benchmark Suite Design (5-8 Core Benchmarks)

### 2.1 Benchmark Inventory

Only 6 benchmarks needed to cover all critical metrics:

```
benchmarks/
├── 01-hook-registration.bench.mjs          # Registration time
├── 02-hook-execution-latency.bench.mjs     # Execution p50/p95/p99
├── 03-concurrent-execution.bench.mjs       # Throughput under load
├── 04-memory-footprint.bench.mjs           # Memory per hook
├── 05-condition-evaluation.bench.mjs       # Cache effectiveness
└── 06-circuit-breaker.bench.mjs            # Reliability under failure
```

### 2.2 Benchmark Specifications

#### Benchmark 1: Hook Registration Performance

**File**: `benchmarks/01-hook-registration.bench.mjs`

**Purpose**: Measure overhead of `addKnowledgeHook()`

**Scenario**:
```javascript
// Register 100 hooks sequentially
// Measure: time per registration, memory delta
for (let i = 0; i < 100; i++) {
  manager.addKnowledgeHook(createTestHook(i));
}
```

**OTEL Spans**:
- `benchmark.hook-registration.start`
- `benchmark.hook-registration.iteration` (per hook)
- `benchmark.hook-registration.end`

**Metrics Collected**:
- `registration.duration.p50/p95/p99` (ms)
- `registration.memory.delta` (bytes)
- `registration.throughput` (ops/sec)

**Acceptance Criteria**:
- p95 < 10ms per registration
- Memory delta < 100KB per hook
- No memory leaks (final memory ~= 100 * hook size)

---

#### Benchmark 2: Hook Execution Latency

**File**: `benchmarks/02-hook-execution-latency.bench.mjs`

**Purpose**: Measure p50/p95/p99 latency of hook execution

**Scenario**:
```javascript
// Execute 1000 hooks sequentially
// Covers all lifecycle phases: before, condition, run, after
for (let i = 0; i < 1000; i++) {
  await manager.executeKnowledgeHook('test-hook', event);
}
```

**OTEL Spans**:
- `benchmark.hook-execution.start`
- `hook.evaluate` (instrumented in hook-executor.mjs)
- `hook.result` (instrumented in hook-executor.mjs)
- `benchmark.hook-execution.end`

**Metrics Collected**:
- `execution.latency.p50/p95/p99/max` (ms)
- `execution.phase.duration` (before, condition, run, after)
- `execution.error.rate` (%)

**Acceptance Criteria**:
- p50 < 20ms
- p95 < 50ms
- p99 < 100ms
- Error rate < 0.1%

---

#### Benchmark 3: Concurrent Execution Throughput

**File**: `benchmarks/03-concurrent-execution.bench.mjs`

**Purpose**: Measure max throughput under concurrent load

**Scenario**:
```javascript
// Spawn 100 concurrent hook executions
// Measure: ops/sec, latency under load, backpressure
await Promise.all(
  Array.from({ length: 100 }, () =>
    manager.executeKnowledgeHook('test-hook', event)
  )
);
```

**OTEL Spans**:
- `benchmark.concurrent.start`
- `benchmark.concurrent.wave` (per batch)
- `hook.evaluate` (per hook execution)
- `benchmark.concurrent.end`

**Metrics Collected**:
- `concurrent.throughput` (ops/sec)
- `concurrent.latency.p95` (ms under load)
- `concurrent.queue_depth.max`
- `concurrent.memory.peak` (bytes)

**Acceptance Criteria**:
- Throughput > 1000 ops/sec
- p95 latency < 100ms under full load
- No queue depth > 1000 (high watermark)
- Memory growth < 100MB for 10,000 operations

---

#### Benchmark 4: Memory Footprint

**File**: `benchmarks/04-memory-footprint.bench.mjs`

**Purpose**: Measure memory usage per hook and detect leaks

**Scenario**:
```javascript
// Register 1000 hooks, execute each 10 times, unregister all
// Force GC between phases
// Measure: heap growth, RSS growth, leak detection

// Phase 1: Register
for (let i = 0; i < 1000; i++) {
  manager.addKnowledgeHook(createTestHook(i));
}
const memAfterRegister = process.memoryUsage();

// Phase 2: Execute
for (let i = 0; i < 10; i++) {
  await manager.executeAllKnowledgeHooks(event);
}
const memAfterExecute = process.memoryUsage();

// Phase 3: Cleanup
manager.clearKnowledgeHooks();
global.gc(); // Require --expose-gc flag
const memAfterCleanup = process.memoryUsage();
```

**OTEL Spans**:
- `benchmark.memory.phase.register`
- `benchmark.memory.phase.execute`
- `benchmark.memory.phase.cleanup`

**Metrics Collected**:
- `memory.per_hook.average` (bytes)
- `memory.leak.detected` (boolean)
- `memory.rss.peak` (bytes)
- `memory.heap.used.peak` (bytes)

**Acceptance Criteria**:
- < 5MB per hook (average)
- Heap after cleanup < heap after register + 10%
- No memory leaks detected (cleanup returns to baseline ±5%)

---

#### Benchmark 5: Condition Evaluation Performance

**File**: `benchmarks/05-condition-evaluation.bench.mjs`

**Purpose**: Measure condition evaluation speed and cache effectiveness

**Scenario**:
```javascript
// Execute 1000 hooks with SPARQL condition
// 80% repeat conditions (should hit cache)
// 20% unique conditions (should miss cache)

for (let i = 0; i < 1000; i++) {
  const conditionId = i % 200; // 80% repeat
  const hook = createHookWithSparqlCondition(conditionId);
  await manager.executeKnowledgeHook(hook, event);
}
```

**OTEL Spans**:
- `benchmark.condition.start`
- `condition.evaluate` (per evaluation)
- `condition.cache.hit` / `condition.cache.miss`
- `benchmark.condition.end`

**Metrics Collected**:
- `condition.eval.duration.p95` (ms)
- `condition.cache.hit_rate` (%)
- `condition.cache.size` (entries)
- `condition.throughput` (evals/sec)

**Acceptance Criteria**:
- Cache hit: < 5ms (p95)
- Cache miss: < 50ms (p95)
- Cache hit rate > 80% (for 80/20 scenario)
- No cache evictions for working set

---

#### Benchmark 6: Circuit Breaker Behavior

**File**: `benchmarks/06-circuit-breaker.bench.mjs`

**Purpose**: Validate circuit breaker prevents cascade failures

**Scenario**:
```javascript
// Execute hooks with 10% error rate
// Measure: circuit breaker trip time, recovery time, error containment

const executor = createHookExecutor({
  circuitBreakerThreshold: 5,
  circuitBreakerResetMs: 1000,
});

// Phase 1: Inject failures until circuit opens
for (let i = 0; i < 100; i++) {
  const hook = createFailingHook(i % 10 === 0); // 10% failure
  await executor.execute(hook, event);
}

// Phase 2: Verify circuit open
const status = executor.getCircuitBreakerStatus();
assert(status.state === 'open');

// Phase 3: Wait for reset and verify recovery
await sleep(1000);
await executor.execute(successfulHook, event);
assert(status.state === 'half-open');
```

**OTEL Spans**:
- `benchmark.circuit-breaker.inject-failures`
- `benchmark.circuit-breaker.verify-open`
- `benchmark.circuit-breaker.verify-recovery`

**Metrics Collected**:
- `circuit_breaker.trip_time` (ms from first failure)
- `circuit_breaker.recovery_time` (ms)
- `circuit_breaker.error_containment_rate` (% of errors prevented)
- `circuit_breaker.false_positive_rate` (%)

**Acceptance Criteria**:
- Circuit opens within 5 failures (threshold)
- Circuit recovers within 1000ms (resetMs)
- Error containment > 95% when open
- False positive rate < 1%

---

## 3. Benchmark Structure

### 3.1 File Organization

```
benchmarks/
├── 01-hook-registration.bench.mjs
├── 02-hook-execution-latency.bench.mjs
├── 03-concurrent-execution.bench.mjs
├── 04-memory-footprint.bench.mjs
├── 05-condition-evaluation.bench.mjs
├── 06-circuit-breaker.bench.mjs
├── fixtures/
│   ├── test-hooks.mjs              # Reusable test hooks
│   ├── test-events.mjs             # Reusable test events
│   └── test-stores.mjs             # Reusable test stores
├── utils/
│   ├── otel-collector.mjs          # OTEL span collection
│   ├── metrics-aggregator.mjs      # Aggregate metrics from spans
│   ├── percentile-calculator.mjs   # Calculate p50/p95/p99
│   └── memory-profiler.mjs         # Memory tracking utilities
└── runner.mjs                      # Main benchmark runner
```

### 3.2 Benchmark File Template

```javascript
/**
 * @file [Benchmark Name]
 * @benchmark [benchmark-id]
 */

import { trace } from '@opentelemetry/api';
import { createOtelCollector } from './utils/otel-collector.mjs';
import { calculatePercentiles } from './utils/percentile-calculator.mjs';

const tracer = trace.getTracer('unrdf-benchmark');

export async function run() {
  const collector = createOtelCollector();

  return tracer.startActiveSpan('benchmark.[name].start', async (span) => {
    try {
      // Setup
      const setup = setupBenchmark();

      // Warmup (10% of iterations)
      await warmup(setup);

      // Actual benchmark
      const results = await runBenchmark(setup);

      // Collect OTEL spans
      const spans = collector.getSpans();

      // Calculate metrics
      const metrics = calculateMetrics(spans, results);

      // Validate acceptance criteria
      const validation = validateCriteria(metrics);

      span.setAttributes({
        'benchmark.name': '[name]',
        'benchmark.iterations': results.iterations,
        'benchmark.duration_ms': results.duration,
        'benchmark.passed': validation.passed,
      });

      span.setStatus({ code: validation.passed ? 1 : 2 });

      return {
        benchmark: '[name]',
        metrics,
        validation,
        rawData: { spans, results },
      };
    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: 2, message: error.message });
      throw error;
    } finally {
      span.end();
    }
  });
}

async function warmup(setup) {
  // 10% of main iterations, discarded
}

async function runBenchmark(setup) {
  // Main benchmark logic
}

function calculateMetrics(spans, results) {
  // Extract metrics from OTEL spans
}

function validateCriteria(metrics) {
  // Check acceptance criteria
}
```

---

## 4. Metrics Collection Approach

### 4.1 OTEL Span-Based Collection

**Pattern**: All benchmarks emit OTEL spans, not console logs.

```javascript
// ❌ WRONG: Console logging
console.time('hook-execution');
await hook.run(event);
console.timeEnd('hook-execution');

// ✅ RIGHT: OTEL spans
tracer.startActiveSpan('hook.execution', async (span) => {
  const start = Date.now();
  await hook.run(event);
  span.setAttribute('hook.duration_ms', Date.now() - start);
  span.setStatus({ code: SpanStatusCode.OK });
  span.end();
});
```

### 4.2 Span Schema

All benchmark spans follow this schema:

```javascript
{
  name: 'benchmark.[suite].[operation]',
  attributes: {
    'benchmark.suite': string,          // e.g., 'hook-registration'
    'benchmark.operation': string,      // e.g., 'register'
    'benchmark.iteration': number,      // e.g., 42
    'benchmark.duration_ms': number,    // e.g., 12.5
    'benchmark.memory_delta_bytes': number,
    'benchmark.success': boolean,
    'benchmark.error': string | null,
  },
  status: {
    code: SpanStatusCode.OK | SpanStatusCode.ERROR,
    message: string | undefined,
  },
  timestamp: number,
}
```

### 4.3 Metrics Aggregation Pipeline

```
OTEL Spans → Span Collector → Metrics Aggregator → Percentile Calculator → Validation
```

1. **Span Collector**: Captures all spans emitted during benchmark
2. **Metrics Aggregator**: Groups spans by suite, calculates totals
3. **Percentile Calculator**: Computes p50/p95/p99 from span durations
4. **Validation**: Compares metrics against acceptance criteria

---

## 5. Reporting Format

### 5.1 Console Output (Human-Readable)

```
🚀 Knowledge Hooks Benchmark Suite
   Running 6 core benchmarks...

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
📊 Benchmark 1: Hook Registration Performance
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Iterations: 100 hooks
Duration: 456ms

Metrics:
  ├─ Registration p50:    3.2ms  ✅ (target: < 10ms)
  ├─ Registration p95:    7.1ms  ✅ (target: < 10ms)
  ├─ Registration p99:    9.3ms  ✅ (target: < 10ms)
  ├─ Memory delta:       89KB    ✅ (target: < 100KB)
  └─ Throughput:         219/s   ✅ (target: > 100/s)

Status: ✅ PASSED (5/5 criteria)

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
📊 Benchmark 2: Hook Execution Latency
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Iterations: 1000 hooks
Duration: 12.3s

Metrics:
  ├─ Execution p50:      8.7ms   ✅ (target: < 20ms)
  ├─ Execution p95:     34.2ms   ✅ (target: < 50ms)
  ├─ Execution p99:     78.9ms   ✅ (target: < 100ms)
  ├─ Error rate:         0.0%    ✅ (target: < 0.1%)
  └─ Throughput:         81/s    ✅ (target: > 50/s)

Phase Breakdown:
  ├─ Before:             1.2ms (14%)
  ├─ Condition:          3.4ms (39%)
  ├─ Run:                3.5ms (40%)
  └─ After:              0.6ms (7%)

Status: ✅ PASSED (5/5 criteria)

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

... [4 more benchmarks]

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
🎯 Overall Results
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Total Duration: 45.6s
Benchmarks: 6/6 passed ✅
Criteria: 28/30 passed (93.3%) ✅

Critical Metrics Summary:
  ├─ Hook Execution p95:    34.2ms  ✅ (< 50ms)
  ├─ Hook Registration:      7.1ms  ✅ (< 10ms)
  ├─ Concurrent Throughput:  1234/s ✅ (> 1000/s)
  ├─ Memory Per Hook:        4.8MB  ✅ (< 5MB)
  ├─ Circuit Breaker:        0.3%   ✅ (< 1%)
  └─ Cache Hit Rate:         85.2%  ✅ (> 80%)

Performance Grade: A+ (93.3%)

✅ All critical benchmarks passed. System is production-ready.
```

### 5.2 JSON Output (Machine-Readable)

```json
{
  "version": "1.0.0",
  "timestamp": "2025-12-04T18:30:00Z",
  "environment": {
    "node": "v20.10.0",
    "platform": "linux",
    "cpus": 8,
    "memory": "16GB"
  },
  "summary": {
    "total_benchmarks": 6,
    "passed": 6,
    "failed": 0,
    "duration_ms": 45600,
    "score": 93.3
  },
  "benchmarks": [
    {
      "id": "hook-registration",
      "name": "Hook Registration Performance",
      "passed": true,
      "duration_ms": 456,
      "iterations": 100,
      "metrics": {
        "registration.p50": 3.2,
        "registration.p95": 7.1,
        "registration.p99": 9.3,
        "registration.memory_delta_kb": 89,
        "registration.throughput": 219
      },
      "validation": {
        "total_criteria": 5,
        "passed": 5,
        "failed": 0,
        "details": [
          { "criterion": "p95 < 10ms", "actual": 7.1, "target": 10, "passed": true },
          { "criterion": "memory < 100KB", "actual": 89, "target": 100, "passed": true }
        ]
      }
    }
  ],
  "critical_metrics": {
    "hook_execution_p95_ms": 34.2,
    "hook_registration_p95_ms": 7.1,
    "concurrent_throughput": 1234,
    "memory_per_hook_mb": 4.8,
    "circuit_breaker_trip_rate": 0.003,
    "cache_hit_rate": 0.852
  },
  "grade": "A+",
  "production_ready": true
}
```

---

## 6. Acceptance Criteria

### 6.1 Global Criteria

All benchmarks must meet these global requirements:

| Criterion | Target | Blocker |
|-----------|--------|---------|
| All benchmarks pass | 6/6 | Yes |
| Critical metrics pass | 6/6 | Yes |
| Overall score | > 80% | Yes |
| No memory leaks | 0 detected | Yes |
| No regression | < 10% slower than baseline | No |

### 6.2 Per-Benchmark Criteria

Defined in each benchmark specification above.

### 6.3 OTEL Span Requirements

All benchmarks must emit OTEL spans with:

- ✅ Correct span naming: `benchmark.[suite].[operation]`
- ✅ Required attributes: `benchmark.suite`, `benchmark.duration_ms`, `benchmark.success`
- ✅ Proper status codes: OK for success, ERROR for failure
- ✅ Exception recording for failures
- ✅ Parent-child relationships for nested operations

### 6.4 Regression Detection Thresholds

Benchmarks fail if metrics regress by more than:

| Metric Type | Threshold | Action |
|-------------|-----------|--------|
| Latency (p95) | +20% | Block CI |
| Throughput | -15% | Block CI |
| Memory | +30% | Block CI |
| Error rate | +5% (absolute) | Block CI |

---

## 7. Architecture Patterns

### 7.1 Concurrent Benchmark Execution

**Pattern**: Run benchmarks sequentially to prevent resource contention.

```javascript
// ❌ WRONG: Parallel execution (resource contention)
await Promise.all(benchmarks.map(b => b.run()));

// ✅ RIGHT: Sequential execution (clean measurements)
for (const benchmark of benchmarks) {
  await benchmark.run();
  await sleep(1000); // Cool-down period
}
```

**Rationale**: Parallel benchmarks pollute each other's measurements (CPU, memory, I/O contention).

### 7.2 Memory Profiling Approach

**Pattern**: Force GC between phases and measure delta.

```javascript
// Setup
global.gc(); // Require --expose-gc
const baseline = process.memoryUsage();

// Operation
await runOperations();

// Measure
global.gc(); // Force GC to collect garbage
const afterOp = process.memoryUsage();

const delta = {
  rss: afterOp.rss - baseline.rss,
  heapUsed: afterOp.heapUsed - baseline.heapUsed,
  heapTotal: afterOp.heapTotal - baseline.heapTotal,
};
```

**OTEL Span**:
```javascript
span.setAttributes({
  'memory.baseline.rss': baseline.rss,
  'memory.after.rss': afterOp.rss,
  'memory.delta.rss': delta.rss,
  'memory.baseline.heap_used': baseline.heapUsed,
  'memory.after.heap_used': afterOp.heapUsed,
  'memory.delta.heap_used': delta.heapUsed,
});
```

### 7.3 Results Aggregation

**Pattern**: OTEL spans → in-memory collector → aggregation → validation.

```javascript
import { InMemorySpanExporter } from '@opentelemetry/sdk-trace-base';

// Setup
const spanExporter = new InMemorySpanExporter();
const collector = {
  exporter: spanExporter,
  getSpans: () => spanExporter.getFinishedSpans(),
  reset: () => spanExporter.reset(),
};

// Benchmark
await runBenchmark();

// Aggregate
const spans = collector.getSpans();
const durations = spans
  .filter(s => s.name === 'hook.evaluate')
  .map(s => s.attributes['hook.duration_ms']);

const metrics = {
  p50: calculatePercentile(durations, 0.50),
  p95: calculatePercentile(durations, 0.95),
  p99: calculatePercentile(durations, 0.99),
};
```

### 7.4 CI/CD Integration Points

**GitHub Actions Workflow**:

```yaml
# .github/workflows/benchmarks.yml
name: Benchmarks

on:
  pull_request:
  push:
    branches: [main]
  schedule:
    - cron: '0 0 * * *' # Daily at midnight

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: '20'

      - name: Install dependencies
        run: pnpm install

      - name: Run benchmarks
        run: node --expose-gc benchmarks/runner.mjs --mode=ci --output=json

      - name: Upload results
        uses: actions/upload-artifact@v3
        with:
          name: benchmark-results
          path: benchmark-results.json

      - name: Compare against baseline
        run: node benchmarks/compare-baseline.mjs

      - name: Comment PR
        if: github.event_name == 'pull_request'
        uses: actions/github-script@v6
        with:
          script: |
            const fs = require('fs');
            const results = JSON.parse(fs.readFileSync('benchmark-results.json'));
            const comment = generateBenchmarkComment(results);
            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: comment
            });
```

**Benchmark Runner CLI**:

```bash
# Local development
node --expose-gc benchmarks/runner.mjs

# CI mode (JSON output)
node --expose-gc benchmarks/runner.mjs --mode=ci --output=json

# Single benchmark
node --expose-gc benchmarks/runner.mjs --suite=hook-registration

# Compare against baseline
node benchmarks/runner.mjs --compare=baseline-2025-12-04.json

# Generate baseline
node benchmarks/runner.mjs --save-baseline=baseline-$(date +%Y-%m-%d).json
```

---

## 8. Implementation Plan

### 8.1 Phase 1: Infrastructure (Week 1)

- [ ] Create `benchmarks/` directory structure
- [ ] Implement `utils/otel-collector.mjs`
- [ ] Implement `utils/metrics-aggregator.mjs`
- [ ] Implement `utils/percentile-calculator.mjs`
- [ ] Implement `utils/memory-profiler.mjs`
- [ ] Implement `benchmarks/runner.mjs`
- [ ] Create test fixtures in `benchmarks/fixtures/`

### 8.2 Phase 2: Core Benchmarks (Week 2)

- [ ] Implement `01-hook-registration.bench.mjs`
- [ ] Implement `02-hook-execution-latency.bench.mjs`
- [ ] Implement `03-concurrent-execution.bench.mjs`
- [ ] Implement `04-memory-footprint.bench.mjs`
- [ ] Implement `05-condition-evaluation.bench.mjs`
- [ ] Implement `06-circuit-breaker.bench.mjs`

### 8.3 Phase 3: CI/CD Integration (Week 3)

- [ ] Create GitHub Actions workflow
- [ ] Implement baseline comparison
- [ ] Implement PR comment generation
- [ ] Set up nightly benchmark runs
- [ ] Create benchmark dashboard (optional)

### 8.4 Phase 4: Documentation & Training (Week 4)

- [ ] Write benchmark developer guide
- [ ] Create example benchmarks
- [ ] Document OTEL span schema
- [ ] Create troubleshooting guide

---

## 9. Success Metrics

### 9.1 Benchmark Suite Quality

- ✅ All 6 benchmarks implemented
- ✅ 100% OTEL span coverage
- ✅ < 1 minute total execution time
- ✅ Zero false positives (flaky tests)

### 9.2 Production Impact

- ✅ Catch 95% of performance regressions before merge
- ✅ Reduce production incidents by 50%
- ✅ Enable confident refactoring (green benchmarks = safe)

### 9.3 Developer Experience

- ✅ < 5 minutes to understand benchmark results
- ✅ < 10 minutes to add new benchmark
- ✅ Zero manual interpretation needed (pass/fail is obvious)

---

## 10. Antipatterns to Avoid

### 10.1 Vanity Metrics

❌ **DON'T**: Measure code coverage of benchmarks
❌ **DON'T**: Count number of assertions
❌ **DON'T**: Measure test execution time (benchmarks ARE timing)
❌ **DON'T**: Create benchmarks for trivial operations (e.g., object creation)

✅ **DO**: Measure only production-critical metrics
✅ **DO**: Focus on real-world scenarios
✅ **DO**: Validate acceptance criteria, not implementation details

### 10.2 Synthetic Benchmarks

❌ **DON'T**: Benchmark empty hooks
❌ **DON'T**: Benchmark with tiny stores (1 triple)
❌ **DON'T**: Use unrealistic concurrency (10,000 parallel ops)

✅ **DO**: Use realistic hook complexity
✅ **DO**: Use realistic store sizes (1,000-10,000 triples)
✅ **DO**: Use realistic concurrency (10-100 parallel ops)

### 10.3 Flaky Benchmarks

❌ **DON'T**: Use wall-clock time without warmup
❌ **DON'T**: Ignore garbage collection
❌ **DON'T**: Run benchmarks in parallel

✅ **DO**: Warm up before measurement (10% of iterations)
✅ **DO**: Force GC before memory measurements
✅ **DO**: Run benchmarks sequentially with cool-down

---

## 11. Future Enhancements (v2.0)

### 11.1 Advanced Profiling

- [ ] CPU profiling with V8 profiler
- [ ] Flamegraphs for hot paths
- [ ] Event loop lag detection
- [ ] Async stack traces

### 11.2 Distributed Benchmarking

- [ ] Multi-node concurrent execution
- [ ] Network latency simulation
- [ ] Cross-platform benchmarks (Linux, macOS, Windows)

### 11.3 Continuous Benchmarking

- [ ] Real-time dashboard
- [ ] Automatic baseline updates
- [ ] Trend analysis (weekly/monthly)
- [ ] Anomaly detection (ML-based)

---

## Appendix A: OTEL Span Schema Reference

```javascript
// Benchmark lifecycle spans
'benchmark.[suite].start'              // Suite started
'benchmark.[suite].warmup'             // Warmup phase
'benchmark.[suite].iteration'          // Single iteration
'benchmark.[suite].aggregate'          // Aggregating results
'benchmark.[suite].validate'           // Validating criteria
'benchmark.[suite].end'                // Suite completed

// Memory profiling spans
'memory.baseline'                      // Baseline measurement
'memory.operation'                     // Operation under test
'memory.cleanup'                       // Cleanup phase
'memory.gc'                            // Garbage collection

// Concurrent execution spans
'concurrent.wave'                      // Batch of concurrent ops
'concurrent.op'                        // Single concurrent op

// Required attributes
{
  'benchmark.suite': string,           // Suite name
  'benchmark.iteration': number,       // Iteration number
  'benchmark.duration_ms': number,     // Operation duration
  'benchmark.memory_delta_bytes': number,
  'benchmark.success': boolean,
  'benchmark.error': string | null,
}
```

---

## Appendix B: Percentile Calculation Reference

```javascript
/**
 * Calculate percentile from sorted array
 * @param {number[]} values - Sorted array of values
 * @param {number} percentile - Percentile (0-1)
 * @returns {number} Percentile value
 */
function calculatePercentile(values, percentile) {
  if (values.length === 0) return 0;

  const sorted = [...values].sort((a, b) => a - b);
  const index = Math.ceil(sorted.length * percentile) - 1;

  return sorted[Math.max(0, index)];
}

// Example usage
const latencies = [10, 15, 12, 8, 50, 9, 11, 13, 45, 14];
const p50 = calculatePercentile(latencies, 0.50); // 12.5
const p95 = calculatePercentile(latencies, 0.95); // 50
const p99 = calculatePercentile(latencies, 0.99); // 50
```

---

## Appendix C: Memory Leak Detection

```javascript
/**
 * Detect memory leaks by comparing baseline to cleanup
 * @param {Object} baseline - Baseline memory usage
 * @param {Object} cleanup - Cleanup memory usage
 * @param {number} [tolerance=0.05] - Acceptable growth (5%)
 * @returns {Object} Leak detection result
 */
function detectMemoryLeak(baseline, cleanup, tolerance = 0.05) {
  const heapGrowth = cleanup.heapUsed - baseline.heapUsed;
  const heapGrowthPercent = heapGrowth / baseline.heapUsed;

  const leaked = heapGrowthPercent > tolerance;

  return {
    leaked,
    heapGrowth,
    heapGrowthPercent,
    tolerance,
    message: leaked
      ? `Memory leak detected: ${(heapGrowthPercent * 100).toFixed(2)}% growth`
      : 'No memory leak detected',
  };
}
```

---

**End of Document**
