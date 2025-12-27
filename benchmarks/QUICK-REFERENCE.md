# Benchmarks Quick Reference

Fast reference for running and interpreting Knowledge Hooks benchmarks.

## Quick Commands

```bash
# Run all benchmarks
node benchmark/examples/hook-registration.benchmark.mjs

# Run with GC enabled (for accurate memory measurements)
node --expose-gc benchmark/examples/hook-registration.benchmark.mjs

# Run validation tests
pnpm test benchmarks/validation.test.mjs

# Run integration tests
pnpm test benchmarks/integration.test.mjs

# Export OTEL traces
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318 \
  node benchmark/examples/hook-registration.benchmark.mjs
```

## Key Metrics

### Latency (Lower is Better)

| Metric | Description | Target |
|--------|-------------|--------|
| **p50** | Median latency (50% complete faster) | < 1ms |
| **p95** | 95th percentile (tail latency) | < 2ms |
| **p99** | 99th percentile (worst case) | < 5ms |
| **Mean** | Average latency | < 1ms |

### Throughput (Higher is Better)

| Benchmark | Target |
|-----------|--------|
| Hook Registration | > 1000 ops/sec |
| Hook Execution (Simple) | > 1000 ops/sec |
| Hook Execution (Medium) | > 200 ops/sec |
| Hook Execution (Complex) | > 50 ops/sec |
| Validation | > 10,000 ops/sec |

### Memory (Lower is Better)

| Test | Target |
|------|--------|
| Per 1000 Hooks | < 25MB |
| Peak Usage | < 50MB (load), < 500MB (stress) |
| Growth Rate | < 1MB/min |
| GC Pause (p95) | < 10ms |

## Baseline Targets at a Glance

### Hook Registration

```
Small (100 hooks):    < 0.5ms avg, > 2000/sec, < 5MB
Medium (1k hooks):    < 1ms avg, > 1000/sec, < 25MB
Large (10k hooks):    < 1ms avg, > 1000/sec, < 100MB
```

### Hook Execution

```
Simple:   < 1ms p50,  < 10ms p99,  > 1000/sec
Medium:   < 3ms p50,  < 15ms p99,  > 200/sec
Complex:  < 10ms p50, < 50ms p99,  > 50/sec
```

## Regression Thresholds

| Metric | ⚠️ Warning | ❌ Critical |
|--------|----------|-----------|
| Latency | +10% | +20% |
| Throughput | -10% | -20% |
| Memory | +15% | +30% |
| Error Rate | +50% | +100% |

## Result Status Codes

| Code | Meaning | Action |
|------|---------|--------|
| ✅ **PASS** | All targets met | Ship it! |
| ⚠️ **WARNING** | Approaching threshold | Monitor closely |
| ❌ **FAIL** | Targets not met | Fix regression |

## Common Issues & Fixes

### Issue: High Variance in Results

```bash
# Fix: Increase warmup and measurement runs
node --expose-gc benchmark/examples/hook-registration.benchmark.mjs
```

### Issue: Memory Measurements Inaccurate

```bash
# Fix: Enable GC
node --expose-gc benchmark/examples/hook-registration.benchmark.mjs
```

### Issue: Benchmarks Taking Too Long

```javascript
// Fix: Reduce measurement runs or increase timeout
const BENCHMARK_CONFIG = {
  parameters: {
    measurementRuns: 10, // Reduce from 50
    timeout: 30000 // Increase to 30s
  }
};
```

### Issue: OTEL Spans Not Created

```javascript
// Fix: Initialize OTEL properly
import { trace } from '@opentelemetry/api';
const tracer = trace.getTracer('benchmark', '1.0.0');
```

## Interpreting Results

### Example Output

```
📊 BENCHMARK RESULTS: ✅ PASS
Targets Met: 5/5

Mean Latency:    0.85ms  (target: < 1ms)     ✅
P95 Latency:     1.8ms   (target: < 2ms)     ✅
Throughput:      1176/s  (target: > 1000/s)  ✅
Memory per 1k:   22.5MB  (target: < 25MB)    ✅
Error Rate:      0.05%   (target: < 0.1%)    ✅
```

### What to Look For

- **All targets met**: Ready to merge ✅
- **1-2 targets missed**: Investigate before merge ⚠️
- **3+ targets missed**: Do not merge, fix regression ❌
- **High p99 latency**: Check for outliers or contention
- **High memory growth**: Check for memory leaks
- **Low throughput**: Check for bottlenecks

## OTEL Span Attributes

### Required Attributes (All Benchmarks)

```javascript
{
  "benchmark.suite.name": "Knowledge Hooks Performance Benchmark Suite",
  "benchmark.id": "hook-registration",
  "benchmark.latency.mean.ms": 0.85,
  "benchmark.latency.p95.ms": 1.8,
  "benchmark.throughput.ops_per_sec": 1176,
  "memory.overhead.mb": 22.5,
  "validation.status": "PASS"
}
```

## CI/CD Integration

### Block Merge on Critical Regression

```yaml
- name: Run Benchmarks
  run: node --expose-gc benchmark/examples/hook-registration.benchmark.mjs > results.json

- name: Check for Regression
  run: |
    if [ $(jq '.validation.status' results.json) == '"FAIL"' ]; then
      echo "Critical regression detected!"
      exit 1
    fi
```

## Percentile Cheat Sheet

| Percentile | Meaning | Use Case |
|------------|---------|----------|
| p50 (Median) | Half are faster, half slower | Average user experience |
| p90 | 90% of requests faster | Good user experience |
| p95 | 95% of requests faster | SLA target |
| p99 | 99% of requests faster | Tail latency / worst case |
| p99.9 | 99.9% of requests faster | Extreme outliers |

**Rule of Thumb**: Optimize for p95, monitor p99.

## Memory Profiling Tips

```bash
# Enable GC for accurate measurements
node --expose-gc script.mjs

# Track memory over time
node --trace-gc script.mjs 2>&1 | grep "Scavenge" | wc -l

# Profile memory allocation
node --heap-prof script.mjs

# Analyze heap snapshot
node --inspect script.mjs
# Then use Chrome DevTools: chrome://inspect
```

## Benchmark File Structure

```
benchmarks/
├── validation.test.mjs           # Unit tests for utilities
├── integration.test.mjs          # Integration tests
├── README.md                     # Full documentation
├── QUICK-REFERENCE.md            # This file
└── test-output/                  # Test results

benchmark/
├── examples/
│   └── hook-registration.benchmark.mjs   # Example implementation
└── specs/
    └── BENCHMARK-SPECIFICATION.md        # Full spec
```

## Resources

- [Full Documentation](./README.md)
- [Benchmark Specification](../benchmark/specs/BENCHMARK-SPECIFICATION.md)
- [Example Implementation](../benchmark/examples/hook-registration.benchmark.mjs)
- [OpenTelemetry Docs](https://opentelemetry.io/docs/)

---

**Pro Tips**:
- Always run with `--expose-gc` for memory benchmarks
- Run benchmarks 3+ times and compare results
- Use p95 for SLA targets, not mean
- Monitor p99 to catch tail latency issues
- Update baseline after intentional performance changes
