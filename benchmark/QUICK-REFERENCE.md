# Knowledge Hooks Benchmark - Quick Reference Card

## 🎯 5 Core Benchmarks

| # | Benchmark | Target | Priority |
|---|-----------|--------|----------|
| 1 | Hook Registration | < 1ms | P1 |
| 2 | Hook Execution | < 10ms p99 | P1 |
| 3 | Hook Validation | > 10k ops/sec | P2 |
| 4 | Memory Profiling | < 50MB / 1k hooks | P1 |
| 5 | Concurrent Execution | > 500 ops/sec @ 10 workers | P1 |

## 📊 Key Metrics Cheatsheet

### Latency (always use percentiles!)
```
p50 (median)  → Typical performance
p95           → 95% of requests faster than this
p99           → Tail latency (critical for UX)
max           → Worst case
```

### Throughput
```
ops/sec       → Operations per second
total ops     → Total operations completed
duration      → Time to complete benchmark
```

### Memory
```
peak          → Maximum memory usage
average       → Typical memory usage
growth rate   → Memory increase over time
GC pause      → Garbage collection pause duration
```

### Reliability
```
error rate    → % of failed operations
timeout rate  → % of timed out operations
success rate  → % of successful operations
```

## 🚨 Regression Thresholds

| Metric | Warning | Critical | Action |
|--------|---------|----------|--------|
| Latency | +10% | +20% | Investigate / Block |
| Throughput | -10% | -20% | Investigate / Block |
| Memory | +15% | +30% | Investigate / Block |
| Errors | +50% | +100% | Block immediately |

## ✅ OTEL Validation Checklist

Every benchmark must have:
- [ ] Root span: `benchmark.{benchmark-id}`
- [ ] Child spans for major operations
- [ ] All spans status: OK
- [ ] Required attributes present
- [ ] Metrics recorded correctly
- [ ] Trace exported successfully

## 🛠️ Common Commands

```bash
# Run example benchmark
node benchmark/examples/hook-registration.benchmark.mjs

# Run with GC enabled (recommended)
node --expose-gc benchmark/examples/hook-registration.benchmark.mjs

# Run full suite (when implemented)
npm run benchmark

# Run specific benchmark
npm run benchmark:registration
npm run benchmark:execution

# Compare against baseline
npm run benchmark -- --compare-baseline

# Create baseline
npm run benchmark -- --create-baseline

# Generate HTML report
npm run benchmark -- --report-html

# CI/CD mode (fail on regression)
npm run benchmark -- --compare-baseline --fail-on-regression
```

## 📝 Benchmark Implementation Template

```javascript
import { Bench } from 'tinybench';
import { trace, SpanStatusCode } from '@opentelemetry/api';

const tracer = trace.getTracer('benchmark', 'latest');

export async function runBenchmark() {
  return await tracer.startActiveSpan('benchmark.{id}', async (span) => {
    // 1. SETUP
    span.setAttribute('benchmark.id', '{id}');
    const testData = generateTestData();
    const baselineMemory = measureMemory();

    // 2. WARMUP
    for (let i = 0; i < WARMUP_RUNS; i++) {
      operation(testData[i]);
    }

    // 3. MEASURE
    const bench = new Bench({ time: 1000 });
    bench.add('operation', () => operation(testData));
    await bench.run();

    // 4. CALCULATE METRICS
    const results = bench.tasks[0].result;
    const p50 = calculatePercentile(results.samples, 50);
    const p95 = calculatePercentile(results.samples, 95);
    const p99 = calculatePercentile(results.samples, 99);

    // 5. RECORD IN SPAN
    span.setAttribute('latency.p50.ms', p50);
    span.setAttribute('latency.p95.ms', p95);
    span.setAttribute('latency.p99.ms', p99);

    // 6. VALIDATE
    const passed = p99 < TARGET_P99;
    span.setAttribute('validation.status', passed ? 'PASS' : 'FAIL');
    span.setStatus({ code: passed ? SpanStatusCode.OK : SpanStatusCode.ERROR });
    span.end();

    return { p50, p95, p99, passed };
  });
}
```

## 🎓 Best Practices Cheatsheet

### DO ✅
- Use percentiles (p50, p95, p99)
- Force GC between benchmarks
- Warm up JIT compiler
- Record system context
- Export OTEL traces
- Compare to baseline

### DON'T ❌
- Rely on just average
- Run on busy systems
- Skip warmup
- Ignore memory
- Skip OTEL validation
- Benchmark in production

## 📐 Scenario Parameters Quick Lookup

### Hook Registration
```
Small:  100 hooks   | < latestms | < 5MB
Medium: 1,000 hooks | < 1ms   | < 25MB
Large:  10,000 hooks| < 1ms   | < 100MB
```

### Hook Execution
```
Simple:  validate + log              | < 1ms p50  | < 10ms p99
Medium:  validate + transform + store| < 3ms p50  | < 15ms p99
Complex: async + transaction + notify| < 10ms p50 | < 50ms p99
```

### Hook Validation
```
Schema:  10,000 validations | > 10k ops/sec | < latestms avg
Runtime: 5,000 validations  | > 5k ops/sec  | < latestms avg
```

### Memory Profiling
```
Load:   1,000 hooks + 10,000 executions  | < 50MB peak  | < 10 GC/sec
Stress: 10,000 hooks + 100,000 executions| < 500MB peak | < 20 GC/sec
```

### Concurrent Execution
```
Low:    10 workers   | > 500 ops/sec  | < 100ms p99  | < 5% contention
Medium: 100 workers  | > 2k ops/sec   | < 300ms p99  | < 15% contention
High:   1000 workers | > 1k ops/sec   | < 1000ms p99 | < 30% contention
```

## 🔍 Debugging Failed Benchmarks

### Latency Too High
```bash
# Check system load
top -n 1

# Force GC
node --expose-gc benchmark.mjs

# Increase warmup runs
WARMUP_RUNS=50 node benchmark.mjs

# Profile with Chrome DevTools
node --inspect benchmark.mjs
```

### Throughput Too Low
```bash
# Check for contention
# Review OTEL traces for blocking operations

# Optimize hot path
# Use profiler to find bottlenecks

# Check for memory pressure
node --trace-gc benchmark.mjs
```

### Memory Leak
```bash
# Take heap snapshots
node --inspect benchmark.mjs
# Open chrome://inspect, take snapshots

# Track allocations
node --trace-gc --trace-gc-verbose benchmark.mjs

# Check for event listener leaks
# Review closure retention
```

### OTEL Validation Failed
```bash
# Check span status
grep "span.status" otel-traces.log

# Verify attributes
grep "span.setAttribute" benchmark.mjs

# Check trace export
# Review OTEL collector logs
```

## 📊 Result Interpretation Guide

### Example Result
```json
{
  "p50": latest,   // 50% of requests < latestms ✅
  "p95": latest,   // 95% of requests < latestms ✅
  "p99": latest,  // 99% of requests < latestms ❌ (target: < 10ms)
  "max": latest   // Worst case: latestms (investigate!)
}
```

**Interpretation:**
- ✅ Median and p95 look great
- ❌ p99 exceeds target by 25% → regression
- ⚠️  Max is latestx p99 → outliers present → investigate

### Regression Example
```
Baseline:  p99 = latestms
Current:   p99 = latestms
Change:    +latest% (critical threshold: +20%)
Status:    🚨 CRITICAL REGRESSION
```

## 📁 File Locations

```
/home/user/unrdf/benchmark/
├── specs/knowledge-hooks-benchmark.spec.json    # Full spec
├── specs/BENCHMARK-SPECIFICATION.md             # Detailed docs
├── examples/hook-registration.benchmark.mjs     # Reference impl
├── baseline.json                                # Baseline targets
├── README.md                                    # Usage guide
├── SUMMARY.md                                   # Overview
└── QUICK-REFERENCE.md                           # This file
```

## 🚀 Getting Started (30 seconds)

```bash
# 1. Review reference implementation
cat benchmark/examples/hook-registration.benchmark.mjs

# 2. Run it
node --expose-gc benchmark/examples/hook-registration.benchmark.mjs

# 3. Check output
# Expected: ✅ PASS - 4/4 targets met

# 4. Review detailed spec
cat benchmark/specs/BENCHMARK-SPECIFICATION.md
```

## 🎯 Success Criteria Summary

| Criteria | Target | Status |
|----------|--------|--------|
| All 5 benchmarks implemented | 5/5 | ⏳ In progress |
| All scenarios covered | 14/14 | ⏳ In progress |
| OTEL integration working | Yes | ⏳ In progress |
| Baseline established | Yes | ⏳ In progress |
| CI/CD integration | Yes | ⏳ In progress |
| < 5% variance | < 5% | ⏳ To validate |
| Error rate < latest% | < latest% | ⏳ To validate |

---

**Quick Tips:**
- Always use `--expose-gc` flag for accurate memory measurements
- Percentiles > average (p99 matters more than mean!)
- OTEL validation prevents silent failures
- Regression thresholds are your friends
- When in doubt, check the reference implementation

**Remember:** Fast benchmarks are useless if they're wrong. OTEL validates correctness! 🎯
