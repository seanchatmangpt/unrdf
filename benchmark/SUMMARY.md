# Knowledge Hooks Benchmark Suite - Quick Summary

## 📋 What Was Created

A comprehensive performance benchmarking specification for UNRDF knowledge hooks following **80/20 principles** - 5 core benchmarks that measure 80% of performance characteristics.

## 📁 Files Created

```
/home/user/unrdf/benchmark/
├── specs/
│   ├── knowledge-hooks-benchmark.spec.json     # Full JSON specification
│   └── BENCHMARK-SPECIFICATION.md              # Human-readable specification
├── examples/
│   └── hook-registration.benchmark.mjs         # Reference implementation
├── baseline.json                               # Baseline targets for regression detection
├── README.md                                   # Usage guide
└── SUMMARY.md                                  # This file
```

## 🎯 The 5 Core Benchmarks

### 1. **Hook Registration** (P1)
- **What:** Speed of registering hooks to knowledge engine
- **Target:** < 1ms per hook
- **Scenarios:** 100 / 1,000 / 10,000 hooks
- **Why:** Fast registration enables dynamic hook management

### 2. **Hook Execution** (P1)
- **What:** Execution latency under various conditions
- **Target:** < 10ms p99
- **Scenarios:** Simple / Medium / Complex hooks
- **Why:** Tail latency impacts user experience most

### 3. **Hook Validation** (P2)
- **What:** Schema validation throughput and accuracy
- **Target:** > 10,000 ops/sec
- **Scenarios:** Schema / Runtime validation
- **Why:** Validation is on critical path

### 4. **Memory Profiling** (P1)
- **What:** Peak memory and GC pressure
- **Target:** < 50MB per 1000 hooks
- **Scenarios:** Normal load / Stress test
- **Why:** Memory must scale linearly

### 5. **Concurrent Execution** (P1)
- **What:** Throughput with parallel executions
- **Target:** > 500 ops/sec @ 10 workers
- **Scenarios:** 10 / 100 / 1000 workers
- **Why:** Real-world workloads are concurrent

## 📊 Key Innovation: OTEL-Based Validation

Unlike traditional benchmarks that just measure performance, this suite **validates correctness** using OpenTelemetry spans:

```
✅ Required OTEL Spans:
   ├─ Root span with benchmark metadata
   ├─ Child spans for each operation
   ├─ Span status must be OK
   └─ All required attributes present

❌ Benchmark FAILS if:
   - Missing required spans
   - Span status is ERROR
   - Missing required attributes
   - Performance targets not met
```

This prevents:
- ❌ Silent failures in benchmarks
- ❌ Incorrect measurements
- ❌ Missing operations
- ❌ Unreliable results

## 🎯 Baseline Targets (Quick Reference)

| Benchmark | Key Metric | Target | Priority |
|-----------|-----------|--------|----------|
| Registration | Avg latency | < 1ms | P1 |
| Execution | P99 latency | < 10ms | P1 |
| Validation | Throughput | > 10k ops/sec | P2 |
| Memory | Per 1k hooks | < 50MB | P1 |
| Concurrent | Throughput @ 10 workers | > 500 ops/sec | P1 |

## 🚨 Regression Detection

Automatically detects performance regressions:

| Metric | Warning | Critical | Action |
|--------|---------|----------|--------|
| **Latency** | +10% | +20% | Investigate / Block merge |
| **Throughput** | -10% | -20% | Investigate / Block merge |
| **Memory** | +15% | +30% | Investigate / Block merge |
| **Errors** | +50% | +100% | Block merge immediately |

**Example:**
```
⚠️  REGRESSION DETECTED:
  Hook execution p99: 12.5ms (baseline: 9.8ms) +27.6% 🚨
  Status: CRITICAL - blocks merge to main
```

## 🛠️ How to Use

### 1. Quick Start
```bash
# Run example benchmark
node benchmark/examples/hook-registration.benchmark.mjs

# Expected output:
# ✅ PASS - 4/4 targets met
```

### 2. Implement Full Suite
```bash
# Create implementations for all 5 benchmarks
# Use benchmark/examples/hook-registration.benchmark.mjs as template
```

### 3. Create Baseline
```bash
# Run benchmarks to establish baseline
npm run benchmark -- --create-baseline
```

### 4. CI/CD Integration
```bash
# Run benchmarks and detect regressions
npm run benchmark -- --compare-baseline --fail-on-regression
```

## 📐 Specification Details

### Benchmark Parameters

Each benchmark defines:
- **Sample sizes:** 100, 1000, 10000 operations
- **Concurrency levels:** 1, 10, 100 workers
- **Complexity levels:** Simple, Medium, Complex
- **Timeout values:** 5s, 10s, 30s, 60s, 120s
- **Warmup runs:** JIT optimization phase
- **Measurement runs:** Actual data collection

### Metrics Collected

**Latency:**
- p50 (median)
- p95 (95th percentile)
- p99 (99th percentile)
- max (worst case)

**Throughput:**
- ops/sec (operations per second)
- total ops completed

**Memory:**
- Peak (maximum)
- Average (typical)
- Growth rate (MB/min)
- GC frequency and pause duration

**Reliability:**
- Error rate (%)
- Timeout rate (%)
- Success rate (%)

## 🎓 Best Practices

### DO ✅
- Use percentiles (p50, p95, p99) not just average
- Force GC between benchmarks
- Warm up JIT compiler first
- Record system context
- Compare against baseline
- Export OTEL traces
- Use statistical tests

### DON'T ❌
- Don't rely on just average (hides tail latency)
- Don't run on busy systems
- Don't skip warmup
- Don't ignore memory
- Don't skip OTEL validation
- Don't benchmark in production

## 📊 Example Results Format

```json
{
  "benchmarkId": "hook-registration",
  "scenario": "medium-batch",
  "timestamp": "2025-12-04T10:30:00Z",
  "results": {
    "avgLatency": 0.85,
    "p95Latency": 1.8,
    "p99Latency": 3.2,
    "throughput": 1176,
    "memoryPeak": 22.5
  },
  "validation": {
    "status": "PASS",
    "targetsMet": 5,
    "targetsTotal": 5,
    "regressionDetected": false
  },
  "otel": {
    "traceId": "a1b2c3...",
    "spanCount": 4,
    "spanStatus": "OK"
  }
}
```

## 🔍 OTEL Validation Example

**Required Span Structure:**
```
benchmark.hook-registration
├─ benchmark.setup
│  └─ [attributes: hookCount, baselineMemory]
├─ benchmark.warmup
│  └─ [attributes: runs]
├─ benchmark.measure
│  └─ [attributes: latency.p50, latency.p95, throughput]
├─ memory.measure
│  └─ [attributes: peak, overhead, per1kHooks]
└─ benchmark.validation
   └─ [attributes: status, targetsMet, failures]
```

**Validation Checks:**
```javascript
✅ Root span exists
✅ 5 child spans present
✅ All spans status: OK
✅ 25 required attributes present
✅ Trace exported successfully
```

## 🚀 Next Steps

1. **Review Specification**
   - Read `/home/user/unrdf/benchmark/specs/BENCHMARK-SPECIFICATION.md`
   - Understand all 5 benchmarks and their scenarios

2. **Study Reference Implementation**
   - Review `/home/user/unrdf/benchmark/examples/hook-registration.benchmark.mjs`
   - Understand OTEL integration pattern
   - Note validation logic

3. **Implement Remaining Benchmarks**
   - Hook Execution (3 scenarios)
   - Hook Validation (2 scenarios)
   - Memory Profiling (2 scenarios)
   - Concurrent Execution (3 scenarios)

4. **Establish Baseline**
   - Run all benchmarks on target system
   - Record results in `baseline.json`
   - Document system specifications

5. **CI/CD Integration**
   - Add benchmark step to CI pipeline
   - Configure regression detection
   - Set up automated reporting

## 📚 Reference Documentation

- **Full Specification:** `/home/user/unrdf/benchmark/specs/BENCHMARK-SPECIFICATION.md`
- **JSON Spec:** `/home/user/unrdf/benchmark/specs/knowledge-hooks-benchmark.spec.json`
- **Usage Guide:** `/home/user/unrdf/benchmark/README.md`
- **Reference Implementation:** `/home/user/unrdf/benchmark/examples/hook-registration.benchmark.mjs`
- **Baseline Targets:** `/home/user/unrdf/benchmark/baseline.json`

## 🎯 Success Criteria

This benchmark suite is successful when:

✅ **Completeness**
- All 5 core benchmarks implemented
- All scenarios covered (14 total)
- OTEL integration working
- Baseline established

✅ **Reliability**
- < 5% variance between runs
- Consistent OTEL trace export
- Accurate regression detection
- No false positives

✅ **Usability**
- < 5 minutes to run full suite
- Clear pass/fail status
- Actionable regression reports
- Easy CI/CD integration

✅ **Production-Ready**
- Performance targets validated
- No memory leaks detected
- Error rates < 0.1%
- Scalability confirmed

---

**Remember:** Benchmarks are only as good as their validation. Always verify with OTEL spans! 🎯
