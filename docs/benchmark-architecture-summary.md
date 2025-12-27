# Knowledge Hooks Benchmark Architecture - Executive Summary

**80/20 Design Principle**: Measure only the 20% of metrics that drive 80% of performance impact.

---

## Quick Overview

### What We're Building

A minimal, focused benchmark suite for knowledge hooks that validates production readiness through OTEL span-based performance analysis.

### Key Innovation

**OTEL Spans as Validation**: Not traditional unit tests. Benchmarks emit OpenTelemetry spans, and we validate by analyzing span attributes, durations, and status codes.

---

## The Vital 20% (Critical Metrics)

| Rank | Metric | Impact | Target | Benchmark |
|------|--------|--------|--------|-----------|
| 1 | Hook Execution Latency (p95) | 40% | < 50ms | #2 |
| 2 | Hook Registration Time | 20% | < 10ms | #1 |
| 3 | Concurrent Throughput | 15% | > 1000 ops/s | #3 |
| 4 | Memory Per Hook | 15% | < 5MB | #4 |
| 5 | Circuit Breaker Trip Rate | 5% | < 1% | #6 |
| 6 | Condition Cache Hit Rate | 5% | > 80% | #5 |

**Total Impact Covered**: 100%

---

## 6 Core Benchmarks

### Benchmark 1: Hook Registration Performance
**File**: `01-hook-registration.bench.mjs`
**Purpose**: Validate registration overhead
**Scenario**: Register 100 hooks sequentially
**Key Metric**: p95 registration time < 10ms

### Benchmark 2: Hook Execution Latency
**File**: `02-hook-execution-latency.bench.mjs`
**Purpose**: Measure end-to-end hook execution speed
**Scenario**: Execute 1000 hooks with all lifecycle phases
**Key Metric**: p95 execution time < 50ms

### Benchmark 3: Concurrent Execution Throughput
**File**: `03-concurrent-execution.bench.mjs`
**Purpose**: Validate scalability under load
**Scenario**: Execute 100 hooks in parallel (Promise.all)
**Key Metric**: Throughput > 1000 ops/sec

### Benchmark 4: Memory Footprint
**File**: `04-memory-footprint.bench.mjs`
**Purpose**: Detect memory leaks and measure consumption
**Scenario**: Register 1000 hooks, execute 10x each, cleanup
**Key Metric**: < 5MB per hook, no leaks

### Benchmark 5: Condition Evaluation Performance
**File**: `05-condition-evaluation.bench.mjs`
**Purpose**: Validate SPARQL condition caching
**Scenario**: Execute 1000 hooks with 80% repeat conditions
**Key Metric**: Cache hit rate > 80%

### Benchmark 6: Circuit Breaker Behavior
**File**: `06-circuit-breaker.bench.mjs`
**Purpose**: Validate reliability under failure
**Scenario**: Inject 10% failures, verify circuit opens/recovers
**Key Metric**: Trip rate < 1% under normal load

---

## Architecture Highlights

### OTEL Span-Based Validation Pattern

```javascript
// Traditional unit test (WRONG for benchmarks)
test('hook executes fast', () => {
  const start = Date.now();
  hook.run();
  const duration = Date.now() - start;
  expect(duration).toBeLessThan(50);
});

// OTEL span-based benchmark (RIGHT)
tracer.startActiveSpan('hook.execute', async (span) => {
  const start = Date.now();
  await hook.run();
  span.setAttribute('hook.duration_ms', Date.now() - start);
  span.setStatus({ code: SpanStatusCode.OK });
  span.end();
});

// Later: Aggregate all spans, calculate p95, validate
const durations = spans.map(s => s.attributes['hook.duration_ms']);
const p95 = calculatePercentile(durations, 0.95);
assert(p95 < 50, `p95 ${p95}ms exceeds target 50ms`);
```

### Data Flow

```
Benchmark → OTEL Spans → Span Collector → Metrics Aggregator → Validation → Report
```

### Concurrent Execution Strategy

- **Benchmarks run sequentially** (prevents resource contention)
- **Operations within benchmarks run in parallel** (realistic load)
- **1-second cool-down between benchmarks** (clean measurements)

### Memory Profiling Strategy

```
Baseline (GC) → Operation → Cleanup (GC) → Compare
```

Detect leaks: `cleanup.heapUsed - baseline.heapUsed < 5%`

---

## Acceptance Criteria

### Global Criteria (Must Pass All)

- ✅ All 6 benchmarks pass
- ✅ Overall score ≥ 80%
- ✅ No memory leaks detected
- ✅ No regressions > threshold

### Regression Thresholds

| Metric | Threshold | Action |
|--------|-----------|--------|
| Latency (p95) | +20% | ❌ Block CI |
| Throughput | -15% | ❌ Block CI |
| Memory | +30% | ❌ Block CI |
| Error Rate | +5% (absolute) | ❌ Block CI |

---

## CI/CD Integration

### GitHub Actions Workflow

**Triggers**:
- Pull request (compare against baseline)
- Push to main (save new baseline)
- Nightly schedule (monitor trends)

**Steps**:
1. Run benchmarks with `--expose-gc` flag
2. Upload JSON results as artifact
3. Compare against baseline (if PR)
4. Comment PR with results
5. Save new baseline (if main branch)

**Example PR Comment**:
```
📊 Benchmark Results

Overall Score: 93.3%
Benchmarks: 6/6 passed ✅
Duration: 45.6s

Critical Metrics:
✅ Hook Execution p95: 34.2ms (< 50ms)
✅ Throughput: 1234 ops/s (> 1000)
✅ Memory/hook: 4.8MB (< 5MB)

No regressions detected ✅
```

---

## Implementation Timeline

### Week 1: Infrastructure
- Create directory structure
- Implement utilities (OTEL collector, percentile calculator, memory profiler)
- Create test fixtures (hooks, events, stores)
- Implement benchmark runner

### Week 2: Core Benchmarks
- Implement 6 benchmarks (1 per day + buffer)
- Validate each benchmark locally
- Ensure all OTEL spans emit correctly

### Week 3: CI/CD
- Create GitHub Actions workflow
- Test on PR
- Set up baseline storage
- Implement regression detection

### Week 4: Documentation & Polish
- Write developer guide
- Create troubleshooting docs
- Demo to team
- Iterate based on feedback

---

## Key Design Decisions

### Why OTEL Spans Instead of Unit Tests?

1. **Production-aligned**: Same instrumentation as production
2. **Rich metadata**: Spans capture duration, memory, errors, context
3. **Aggregatable**: Easy to calculate percentiles, throughput
4. **Standardized**: Industry-standard format (OTLP)
5. **Exportable**: Can send to Jaeger, Prometheus, etc.

### Why Sequential Benchmark Execution?

**Problem**: Parallel benchmarks pollute each other's measurements.

**Example**:
```
Parallel: Benchmark A (CPU-bound) + Benchmark B (memory-bound)
Result: Both show degraded performance due to contention
```

**Solution**: Run sequentially with cool-down periods.

### Why Only 6 Benchmarks?

**80/20 Principle**: These 6 benchmarks cover 100% of production performance impact.

**Vanity metrics excluded**:
- Code coverage of benchmarks
- Number of assertions
- Test execution time
- Hook definition complexity

### Why Force GC Before Memory Measurements?

**Problem**: JavaScript GC is non-deterministic. Unreleased objects skew measurements.

**Solution**: Force GC with `global.gc()` (requires `--expose-gc` flag).

```javascript
// Before measurement
global.gc();
const baseline = process.memoryUsage();

// Operation
await operation();

// After measurement
global.gc();
const after = process.memoryUsage();
```

---

## Anti-Patterns to Avoid

| ❌ DON'T | ✅ DO |
|---------|-------|
| Benchmark trivial operations | Benchmark production-critical paths |
| Use synthetic data (1 triple store) | Use realistic data (1000+ triples) |
| Run benchmarks in parallel | Run sequentially with cool-down |
| Ignore garbage collection | Force GC before memory measurements |
| Measure vanity metrics | Measure only critical 20% |
| Create 100+ benchmarks | Create 6 focused benchmarks |

---

## Success Metrics

### Benchmark Quality

- ✅ < 1 minute total execution time
- ✅ Zero false positives (flaky tests)
- ✅ 100% OTEL span coverage

### Production Impact

- ✅ Catch 95% of regressions before merge
- ✅ Reduce production incidents by 50%
- ✅ Enable confident refactoring

### Developer Experience

- ✅ < 5 minutes to understand results
- ✅ < 10 minutes to add new benchmark
- ✅ Zero manual interpretation needed

---

## Related Documents

1. **[Benchmark Architecture (Full Spec)](./benchmark-architecture-80-20.md)**
   - Detailed specifications for all 6 benchmarks
   - OTEL span schemas
   - Acceptance criteria
   - Architecture patterns

2. **[Visual Diagrams](./benchmark-architecture-diagram.md)**
   - System architecture diagram
   - Data flow diagram
   - Concurrent execution flow
   - Memory profiling flow
   - CI/CD integration diagram

3. **[Implementation Roadmap](./benchmark-implementation-roadmap.md)**
   - Week-by-week plan
   - Code examples for all utilities
   - GitHub Actions workflow
   - Quick-start commands
   - Implementation checklist

---

## Quick Start

```bash
# 1. Clone repo and install dependencies
pnpm install

# 2. Create benchmark structure
mkdir -p benchmarks/{fixtures,utils,baselines}

# 3. Implement utilities (Week 1)
# See: benchmark-implementation-roadmap.md

# 4. Implement benchmarks (Week 2)
# See: benchmark-architecture-80-20.md

# 5. Run locally
node --expose-gc benchmarks/runner.mjs

# 6. Set up CI/CD (Week 3)
# See: .github/workflows/benchmarks.yml
```

---

## FAQ

### Q: Why not use existing benchmark tools like benchmark.js?

**A**: Existing tools focus on micro-benchmarks (function-level). We need macro-benchmarks (system-level) with OTEL integration for production alignment.

### Q: Can we add more benchmarks later?

**A**: Yes, but only if they measure a critical metric not covered by the core 6. Avoid vanity metrics.

### Q: What if a benchmark is flaky?

**A**: Add warmup phase (10% of iterations) and increase sample size. If still flaky, the benchmark is measuring the wrong thing.

### Q: How do we handle environment variability (CI vs local)?

**A**: Use relative comparisons (baseline) instead of absolute thresholds. Track trends over time.

### Q: What if performance degrades over time?

**A**: Nightly benchmarks + trend analysis. Alert if score drops > 10% week-over-week.

---

## Contact & Support

- **Architecture Questions**: See [benchmark-architecture-80-20.md](./benchmark-architecture-80-20.md)
- **Implementation Help**: See [benchmark-implementation-roadmap.md](./benchmark-implementation-roadmap.md)
- **Visual Reference**: See [benchmark-architecture-diagram.md](./benchmark-architecture-diagram.md)

---

**Status**: Architecture Design Complete ✅
**Next Step**: Begin Phase 1 Implementation (Week 1)
**Owner**: Knowledge Engine Team
**Last Updated**: 2025-12-04
