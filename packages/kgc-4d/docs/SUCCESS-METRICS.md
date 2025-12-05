# Success Metrics: How to Know Deployment is Working

**Post-deployment validation: measure these 4 metrics**

---

## Core Success Criteria

After deployment, **ALL 4 must be true**:

| Metric | Target | Measurement |
|--------|--------|-------------|
| **Query Latency (P95)** | <Target SLA | `monitor:latency` |
| **Error Rate** | <1% | `metrics:errors` |
| **Memory Stability** | No unbounded growth | `monitor:memory` |
| **Time-Travel Success** | >99% reconstructions | `metrics:time-travel` |

---

## Metric 1: Query Latency

### Definition
Time from query submission to result return (wall-clock time, including all overhead)

### Targets by Operation Count

| Operation Count | SLA Target | Acceptable Range |
|---|---|---|
| <1K | <500ms | <50ms (with hooks) |
| 1K-10K | <2s | <2s (with optimization) |
| >10K | <5s | <5s (after full optimization) |

### How to Measure

```bash
# Option 1: Built-in monitoring (if available)
npm run monitor:latency

# Option 2: Manual measurement
const start = Date.now();
const results = store.match(subject, predicate, null);
const latency = Date.now() - start;
console.log(`Query latency: ${latency}ms`);

# Option 3: Histogram (P95, P99)
# Track last 1000 queries
# P95 = 95th percentile
# P99 = 99th percentile
```

### Success Indicators ✅
- P95 latency < SLA target
- P99 latency < 2× SLA target (acceptable outliers)
- No latency degradation over time
- Latency consistent across query types

### Failure Indicators ❌
- P95 > SLA target → Optimization needed
- P99 >> 2× SLA → Outlier spike investigation
- Increasing trend over time → Memory leak or index fragmentation
- High variance → GC pauses or contention

### Actions if Failing

```
1. Check operation count
   └─ Verify you're in right SLA bracket

2. Profile current latency
   └─ npm run bench:hooks (see actual numbers)

3. Identify bottleneck
   └─ Hooks? Memory? Index? (see BENCHMARKS.md section 4)

4. Apply optimization
   └─ Follow BENCHMARKS.md optimization roadmap

5. Re-measure
   └─ Did latency improve?

6. If still failing
   └─ Escalate to architecture team
```

---

## Metric 2: Error Rate

### Definition
Percentage of operations that fail (crash, return error, or timeout)

### Targets

| Metric | Target |
|--------|--------|
| Overall error rate | <1% (strict) |
| Critical queries (time-travel) | 0% |
| Add/remove operations | <0.1% |
| Read queries | <1% |

### How to Measure

```javascript
// Option 1: Application instrumentation
class ErrorCounter {
  constructor() {
    this.total = 0;
    this.errors = 0;
  }

  recordOperation(success) {
    this.total++;
    if (!success) this.errors++;
  }

  getErrorRate() {
    return (this.errors / this.total) * 100;
  }
}

const counter = new ErrorCounter();

// In operation handler
try {
  const results = store.match(...);
  counter.recordOperation(true);
} catch (err) {
  counter.recordOperation(false);
  logger.error(`Query failed: ${err.message}`);
}

// Report
setInterval(() => {
  const rate = counter.getErrorRate();
  console.log(`Error rate: ${rate.toFixed(2)}%`);
  if (rate > 1) {
    alert(`High error rate: ${rate}%`);
  }
}, 60000);

// Option 2: OTEL metrics (if configured)
// npm run metrics:errors
```

### Success Indicators ✅
- Error rate <1% consistently
- Critical operations (time-travel) 100% success
- Errors traceable to root cause
- Error frequency stable (not increasing)

### Failure Indicators ❌
- Error rate >1% → Investigation required
- Time-travel reconstruction failures → State corruption risk
- Increasing error trend → System degradation
- Errors without clear logs → Monitoring blind spot

### Actions if Failing

```
1. Get error details
   └─ Check logs (what errors? when? how often?)

2. Is it expected?
   └─ User error? Bad input? (fix docs/validation)
   └─ System error? Bug? (investigate)

3. Are specific operations failing?
   └─ All time-travel? → Snapshot issue
   └─ All adds? → Hook validation issue
   └─ Random? → GC pause or memory pressure

4. Apply fix
   └─ Based on error type (see TROUBLESHOOTING.md)

5. Verify recovery
   └─ Error rate drops? ✓
```

---

## Metric 3: Memory Stability

### Definition
RAM usage over time should be stable (not growing unbounded)

### Targets

| Measurement | Target |
|---|---|
| **Baseline** | (Your application RAM) |
| **Growth rate** | <10MB per hour |
| **Max threshold** | Baseline + 50% |
| **GC pause time** | <50ms (P95) |

### How to Measure

```javascript
// Option 1: Process memory tracking
function monitorMemory() {
  const used = process.memoryUsage().heapUsed / 1024 / 1024;
  const total = process.memoryUsage().heapTotal / 1024 / 1024;

  return {
    used: used.toFixed(2) + 'MB',
    total: total.toFixed(2) + 'MB',
    percent: ((used / total) * 100).toFixed(1) + '%'
  };
}

// Log periodically
setInterval(() => {
  const mem = monitorMemory();
  console.log(`Memory: ${mem.used} / ${mem.total} (${mem.percent})`);
}, 60000);

// Option 2: OTEL memory metrics
// npm run monitor:memory

// Option 3: GC events
// Enable: node --expose-gc app.js
// Track: GC duration + frequency
```

### Success Indicators ✅
- Memory stable (no growth trend)
- GC pauses <50ms
- Heap utilization <80%
- No memory spikes after operations

### Failure Indicators ❌
- Steadily increasing memory → Memory leak
- Sudden spike → Unbounded allocation
- GC pause >50ms → Performance impact
- Approaching max → Risk of OOM crash

### Actions if Failing

```
MEMORY LEAK (Steadily increasing):
  1. Identify cause
     └─ Hooks not cleaning? Cache growing?
  2. Check TROUBLESHOOTING.md (Memory issues section)
  3. Implement fix (cache cleanup, GC tuning)
  4. Monitor for improvement

MEMORY SPIKE (Sudden increase):
  1. What happened?
     └─ Large import? Time-travel to old date?
  2. Is it temporary?
     └─ Yes → Expect return to normal after GC
     └─ No → Investigate leak
  3. Take action

GC PAUSE (>50ms):
  1. Reduce allocation pressure
     └─ Process smaller batches
     └─ Limit concurrent operations
  2. Tune Node.js GC settings
     └─ Increase heap size
     └─ Adjust GC parameters
  3. Re-measure
```

---

## Metric 4: Time-Travel Success Rate

### Definition
Percentage of time-travel reconstructions that complete successfully

### Targets

| Operation | Target | Tolerance |
|---|---|---|
| Reconstruction success | >99% | Can fail <1% (edge cases) |
| State accuracy | 100% | Zero tolerance (must match) |
| Reconstruction speed | <100ms | (if using snapshots) |

### How to Measure

```javascript
// Track reconstruction success
const reconstructionMetrics = {
  attempts: 0,
  successes: 0,
  failures: 0,
  times: []
};

async function recordReconstructionResult(targetTime) {
  const start = Date.now();
  try {
    const state = await store.reconstructState({ targetTime });
    const duration = Date.now() - start;

    reconstructionMetrics.successes++;
    reconstructionMetrics.times.push(duration);

    return { success: true, duration };
  } catch (err) {
    reconstructionMetrics.failures++;
    return { success: false, error: err.message };
  } finally {
    reconstructionMetrics.attempts++;
  }
}

// Report metrics
function getReconstructionMetrics() {
  const successRate = (reconstructionMetrics.successes /
    reconstructionMetrics.attempts) * 100;
  const avgTime = reconstructionMetrics.times.length > 0
    ? reconstructionMetrics.times.reduce((a, b) => a + b) /
      reconstructionMetrics.times.length
    : 0;

  return {
    successRate: successRate.toFixed(2) + '%',
    attempts: reconstructionMetrics.attempts,
    avgLatency: avgTime.toFixed(0) + 'ms',
    failures: reconstructionMetrics.failures
  };
}

// Alert on failures
setInterval(() => {
  const metrics = getReconstructionMetrics();
  if (parseFloat(metrics.successRate) < 99) {
    alert(`Time-travel failures: ${metrics.failures} / ${metrics.attempts}`);
  }
}, 3600000);  // Every hour
```

### Success Indicators ✅
- Success rate >99%
- Rare failures traceable to edge cases
- Reconstructions <100ms (with snapshots)
- State accuracy 100% (verified by tests)

### Failure Indicators ❌
- Success rate <99% → Corruption risk
- State accuracy <100% → Data inconsistency
- Reconstructions >1s → Missing snapshots
- Increasing failure rate → Degradation

### Actions if Failing

```
TIME-TRAVEL FAILURES:
  1. Analyze error message
     └─ "Snapshot not found" → Add more snapshots
     └─ "Event log corrupted" → Restore from backup
     └─ "Timeout" → Snapshots too old

  2. Check snapshots
     └─ Are they recent? (daily minimum)
     └─ Are they valid? (verify checksums)

  3. Check event log
     └─ Is it growing? (should append-only)
     └─ Any corruption signs? (gaps, duplicates)

  4. Implement fix

STATE ACCURACY FAILURES:
  1. CRITICAL: State doesn't match
     └─ This is data corruption risk
     └─ Investigate immediately

  2. Verify
     └─ Run reference/COMPLETION-SUMMARY.md Phase 1 tests
     └─ Do reconstruction tests pass?

  3. Restore if needed
     └─ Restore from known-good snapshot
     └─ Investigate corruption cause

  4. Post-mortum
     └─ How did corruption happen?
     └─ Did FMEA guards fail? (should have caught it)
```

---

## Dashboard Template

Create a monitoring dashboard showing all 4 metrics:

```
╔═══════════════════════════════════════════════════════════════╗
║           KGC 4D PRODUCTION MONITORING DASHBOARD              ║
╠═══════════════════════════════════════════════════════════════╣
║                                                               ║
║ LATENCY (P95)                          ERROR RATE            ║
║  ████████████░░░░░░░░░░░░░░░░░░░░░  │  █░░░░░░░░░░░░░░░  │
║  32ms / 500ms target        ✅ OK    │  0.3% / 1% target   ✅ │
║                                     │                     │
║ MEMORY USAGE                          TIME-TRAVEL SUCCESS   ║
║  ████████████░░░░░░░░░░░░░░░░░░░░░  │  ██████████░░░░░░  │
║  2.1GB / 2.5GB max         ✅ OK    │  99.7% / 99% target ✅ │
║                                                               ║
╚═══════════════════════════════════════════════════════════════╝
```

---

## Weekly Success Report

Every Friday, confirm:

```
WEEK OF: December 1-7, 2025

✅ Query Latency
   P95: 45ms (target: <500ms)
   Trend: Stable (no degradation)
   Note: Validation caching working ✓

✅ Error Rate
   Total: 10,000 ops
   Failures: 15 (0.15%)
   Target: <1%
   Note: Most failures user input errors ✓

✅ Memory
   Baseline: 800MB
   Current: 920MB
   Growth: +2% (stable)
   Trend: Flat (no leak detected)

✅ Time-Travel
   Attempts: 100
   Successes: 100 (100%)
   Avg latency: 45ms (with snapshots)
   Note: Daily snapshots working ✓

DECISION:
  ✅ All metrics healthy
  ✅ System stable for next week
  ✅ No escalation needed
```

---

## When to Escalate

**Escalate immediately if**:
- Error rate >5% (something is broken)
- Time-travel success <99% (data corruption risk)
- Memory growth >50MB/hour (leak running away)
- P95 latency >5× SLA target (service degraded)

**Escalate within 24 hours if**:
- Latency > SLA target (trending up)
- Memory approaching max (approaching limit)
- GC pause >100ms (performance impacting)
- Any time-travel failures (investigate cause)

---

## Related Documentation

- **Deployment**: DEPLOYMENT-CHECKLIST.md (Phase 10: Post-deployment)
- **Monitoring**: DEPLOYMENT-CHECKLIST.md (Phase 4: Monitoring setup)
- **Performance**: BENCHMARKS.md (section 3.1: Safe operating ranges)
- **Troubleshooting**: TROUBLESHOOTING.md (performance issues section)

---

**Summary**: Monitor these 4 metrics continuously. All healthy = deployment successful. Any failure = investigate and fix per troubleshooting guide.

---

Last updated: December 5, 2025 | Status: Production Monitoring Guide ✅
