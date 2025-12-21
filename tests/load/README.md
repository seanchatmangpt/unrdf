# UNRDF Load Testing Suite

Comprehensive load testing infrastructure for memory stability, performance benchmarking, and sustained workload validation.

## Quick Start

```bash
# 1. Quick validation (10 seconds)
node tests/load/smoke-test.mjs

# 2. Memory leak detection (2 minutes)
timeout 150s node tests/load/slow-stable-test.mjs

# 3. Check results
cat tests/load/slow-stable-*.json
```

## Test Suite Overview

| Script | Purpose | Duration | Status |
|--------|---------|----------|--------|
| **smoke-test.mjs** | Quick functionality validation | 10s | ✅ PASS |
| **slow-stable-test.mjs** | Memory leak detection (5 ops/sec) | 2 min | ❌ Leak found |
| **memory-stable-test.mjs** | Fixed working set test (100 ops/sec) | 5 min | ❌ Leak found |
| **sustained-load-5min.mjs** | Accelerated 24h simulation (480 ops/sec) | 5 min | ❌ Leak found |
| **baseline-benchmark.mjs** | Performance baseline across data sizes | 5 min | ⏳ Not run |
| **memory-profiler.mjs** | Advanced heap analysis with snapshots | 10 min | ⏳ Not run |
| **run-all-load-tests.mjs** | Orchestrate full suite | 75 min | ⏳ Not run |

## Key Findings

### ⚠️ Critical Memory Leak Detected

**Severity**: 🔴 HIGH
**Impact**: Blocks production use for sustained workloads (>1 minute)

**Evidence**:
- 84.72% memory growth at **5 ops/sec** (query-only) over 2 minutes
- 748% growth at 100 ops/sec with fixed working set
- 1123% growth at 480 ops/sec

**Root Cause**: Likely in Oxigraph's iterator/query execution path (not data storage)

**See**: [LOAD-TEST-REPORT.md](./LOAD-TEST-REPORT.md) for full analysis

## Test Results Summary

### Smoke Test ✅

```
Throughput: 26,355 ops/sec (peak)
Memory: 90.69 MB growth (acceptable for 10s burst)
Latency: p50=4.76ms, p99=18.89ms
Verdict: PASSED
```

### Slow Stable Test ❌

```
Rate: 5 ops/sec (query-only)
Duration: 2 minutes
Memory Growth: 84.72% (7.9 MB → 14.6 MB)
Verdict: FAILED - Memory leak confirmed
```

### Memory Stable Test ❌

```
Rate: 100 ops/sec
Working Set: 10,000 quads (fixed)
Memory Growth: 748% in 31 seconds
Verdict: FAILED - Leak persists with fixed dataset
```

## Usage Examples

### Basic Smoke Test

```bash
node tests/load/smoke-test.mjs
```

**Output**:
```
💨 Smoke Test (10s)
1. Creating store... ✅
2. Inserting 1000 quads... ✅
3. Querying data... ✅ 100 results
4. Deleting quads... ✅ 100 deleted
5. Running 5s sustained load... ✅ 131774 ops (26355 ops/sec)
6. Memory: 90.69 MB growth

✅ Smoke test PASSED
```

### Memory Leak Detection

```bash
timeout 150s node tests/load/slow-stable-test.mjs
```

**Output**:
```
🐌 Slow Stable Test (5 ops/sec for 2 min)
[0s]   Baseline: 7.9 MB
[15s]  10.1 MB (+27.7%)
[30s]  9.9 MB  (+25.5%)
[120s] 14.6 MB (+84.7%)

❌ Growth: 84.72%
```

### With Heap Snapshots

```bash
node tests/load/memory-stable-test.mjs
```

Generates:
- `heap-stable-initial-*.heapsnapshot`
- `heap-stable-33pct-*.heapsnapshot`
- `heap-stable-67pct-*.heapsnapshot`
- `heap-stable-100pct-*.heapsnapshot`

Analyze in Chrome DevTools:
1. Open Chrome → DevTools → Memory
2. Load Snapshot → Select `.heapsnapshot` file
3. Compare snapshots to find retained objects

### With Memory Profiling

```bash
node --expose-gc tests/load/memory-profiler.mjs
```

Enables:
- Manual GC triggering
- Precise memory measurements
- GC effectiveness analysis

## Interpreting Results

### Success Criteria

| Metric | Threshold | Current | Status |
|--------|-----------|---------|--------|
| Memory Growth | <5% | **84%** | ❌ FAIL |
| Latency p99/p50 | <2x | 3.14/0.64 = 4.9x | ⚠️ WARNING |
| Throughput | >10 ops/sec | 26,355 ops/sec | ✅ PASS |
| No Crashes | 0 | 0 | ✅ PASS |

### Memory Growth Classification

| Growth | Duration | Severity | Action |
|--------|----------|----------|--------|
| <5% | Any | ✅ Acceptable | Monitor |
| 5-20% | >1 hour | ⚠️ Warning | Investigate |
| >20% | <1 hour | 🔴 Critical | Fix immediately |
| >100% | <5 min | 🚨 Emergency | Block deployment |

**Current Status**: 🚨 **EMERGENCY** - 84% growth in 2 minutes

## Mitigation Strategies

### Immediate Workarounds

**See**: [MEMORY-LEAK-MITIGATION.md](./MEMORY-LEAK-MITIGATION.md)

1. **Process Restart**: Auto-restart when heap >500MB
2. **Manual GC**: Force GC after query batches (15% improvement)
3. **Query Limits**: Bound result sets to <1000 quads
4. **Store Pooling**: Rotate instances every 60 seconds

### Long-term Fixes

1. Analyze heap snapshots in Chrome DevTools
2. Profile with Clinic.js
3. Fix iterator cleanup in Oxigraph
4. Contribute upstream or migrate to N3.js

## Performance Baseline

Before memory leak impacts performance:

| Operation | Throughput | Latency p50 | Latency p99 |
|-----------|------------|-------------|-------------|
| Query | 26,355 ops/sec | 0.64 ms | 3.14 ms |
| Insert | 26,355 ops/sec | 0.013 ms | 0.404 ms |
| Delete | N/A | 0.006 ms | 0.032 ms |

**Note**: Excellent performance until GC pressure kicks in (~30 seconds)

## Files Generated

### Test Scripts

- `smoke-test.mjs` - 10s validation
- `slow-stable-test.mjs` - Leak detection (5 ops/sec)
- `memory-stable-test.mjs` - Fixed working set (100 ops/sec)
- `sustained-load-5min.mjs` - 5min simulation (480 ops/sec)
- `baseline-benchmark.mjs` - Comprehensive perf baseline
- `memory-profiler.mjs` - Advanced heap analysis
- `run-all-load-tests.mjs` - Full suite orchestrator

### Test Outputs

- `slow-stable-*.json` - Test results (JSON)
- `heap-*.heapsnapshot` - Chrome DevTools snapshots (5.9 MB each)
- `LOAD-TEST-REPORT.md` - Comprehensive analysis
- `MEMORY-LEAK-MITIGATION.md` - Fix strategies

## Advanced Usage

### Custom Test Durations

```javascript
// Edit test script
const DURATION_MS = 10 * 60 * 1000; // 10 minutes
const OPS_PER_SEC = 50; // Custom rate
```

### With Inspector

```bash
node --inspect tests/load/memory-profiler.mjs
# Open chrome://inspect in Chrome
```

### With Clinic.js

```bash
npm install -g clinic

# Memory leak detection
clinic doctor -- node tests/load/slow-stable-test.mjs

# Heap profiling
clinic heapprofiler -- node tests/load/slow-stable-test.mjs

# Open report
clinic doctor --open
```

### With Trace GC

```bash
node --trace-gc --trace-gc-verbose tests/load/slow-stable-test.mjs > gc.log 2>&1
grep "Scavenge\|Mark-sweep" gc.log
```

## Continuous Integration

### GitHub Actions Example

```yaml
name: Load Tests

on: [push, pull_request]

jobs:
  load-test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-node@v3
        with:
          node-version: '24'
      - run: npm install
      - name: Smoke Test
        run: timeout 30s node tests/load/smoke-test.mjs
      - name: Memory Leak Detection
        run: timeout 180s node tests/load/slow-stable-test.mjs
      - name: Check Results
        run: |
          GROWTH=$(grep memoryGrowthPercent tests/load/slow-stable-*.json | tail -1 | cut -d'"' -f4)
          echo "Memory growth: $GROWTH%"
          [ $(echo "$GROWTH < 10" | bc) -eq 1 ] || exit 1
```

### Docker Example

```dockerfile
FROM node:24-slim

WORKDIR /app
COPY package*.json ./
RUN npm install

COPY . .

# Run smoke test on build
RUN timeout 30s node tests/load/smoke-test.mjs

CMD ["node", "server.js"]
```

## Monitoring in Production

### Memory Alerts

```javascript
// Add to app startup
setInterval(() => {
  const heapMB = process.memoryUsage().heapUsed / 1024 / 1024;

  if (heapMB > 500) {
    console.error(`🚨 Memory alert: ${heapMB.toFixed(0)}MB`);
    // Trigger restart
    process.exit(1); // PM2/Docker will restart
  }
}, 60000); // Check every minute
```

### OTEL Integration

```javascript
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('memory-monitor');

setInterval(() => {
  const mem = process.memoryUsage();
  const span = tracer.startSpan('memory.check');

  span.setAttribute('heap.used.mb', mem.heapUsed / 1024 / 1024);
  span.setAttribute('heap.total.mb', mem.heapTotal / 1024 / 1024);

  span.end();
}, 30000); // Every 30 seconds
```

## FAQ

### Q: Why do tests fail?

**A**: The tests **correctly detected a memory leak** in the Oxigraph store. This is a success for the testing methodology.

### Q: Can I use this in production?

**A**: Not for sustained workloads (>1 minute) until the leak is fixed. Use workarounds from [MEMORY-LEAK-MITIGATION.md](./MEMORY-LEAK-MITIGATION.md).

### Q: How do I fix the leak?

**A**: See mitigation guide. Short-term: process restarts + GC forcing. Long-term: fix Oxigraph or migrate to N3.js.

### Q: What's the root cause?

**A**: Likely iterator retention in Oxigraph's JS-to-native bindings. Heap snapshot analysis needed.

### Q: Should I skip these tests?

**A**: No. Run them to validate any fixes and prevent regressions.

## Contributing

To add new load tests:

1. Create `tests/load/my-test.mjs`
2. Follow the pattern from `smoke-test.mjs`
3. Add to `run-all-load-tests.mjs`
4. Document in this README

## License

MIT - See LICENSE file

---

**Status**: ⚠️ Memory leak detected - Fix in progress
**Last Updated**: 2025-12-20
**Node Version**: v24.11.1
**Platform**: macOS (darwin)
