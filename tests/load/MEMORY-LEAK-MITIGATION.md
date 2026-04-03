# Memory Leak Mitigation Guide

## Immediate Workarounds (Use Now)

### 1. Process Restart Strategy

For production deployments, implement automatic process recycling:

```javascript
// Add to your server/app startup
const MEMORY_THRESHOLD_MB = 500;
const CHECK_INTERVAL_MS = 60000; // 1 minute

setInterval(() => {
  const heapUsedMB = process.memoryUsage().heapUsed / 1024 / 1024;

  if (heapUsedMB > MEMORY_THRESHOLD_MB) {
    console.warn(`Memory threshold exceeded: ${heapUsedMB.toFixed(0)}MB`);

    // Graceful shutdown
    server.close(() => {
      console.log('Restarting due to memory pressure');
      process.exit(0); // PM2 or Docker will restart
    });
  }
}, CHECK_INTERVAL_MS);
```

### 2. Manual GC Forcing

If running with `--expose-gc`, force garbage collection after heavy operations:

```javascript
import { createStore } from '@unrdf/oxigraph';

const store = createStore();

// After heavy query batch
async function queryBatch() {
  const results = [];

  for (const quad of store.match()) {
    results.push(quad);
  }

  // Force GC after iteration
  if (global.gc) {
    global.gc();
  }

  return results;
}
```

**Warning**: This only helps ~15% based on test results. Not a full solution.

### 3. Limit Query Scope

Reduce memory accumulation by limiting result sets:

```javascript
// ❌ BAD: Unbounded iteration
for (const quad of store.match()) {
  // Process all quads (leaks)
}

// ✅ GOOD: Bounded iteration
let count = 0;
const MAX_RESULTS = 1000;

for (const quad of store.match()) {
  if (count++ >= MAX_RESULTS) break;
  // Process limited quads
}
```

### 4. Store Instance Pooling

Create new store instances periodically instead of reusing:

```javascript
class StorePool {
  constructor(maxAge = 60000) { // 1 minute max age
    this.current = createStore();
    this.created = Date.now();
    this.maxAge = maxAge;
  }

  getStore() {
    const age = Date.now() - this.created;

    if (age > this.maxAge) {
      // Rotate to new store
      console.log('Rotating store instance');
      this.current = createStore();
      this.created = Date.now();

      if (global.gc) global.gc();
    }

    return this.current;
  }
}

const pool = new StorePool();

// Use
const store = pool.getStore();
```

---

## Investigation Actions

### 1. Analyze Heap Snapshots

```bash
# Take snapshots at intervals
node --expose-gc tests/load/memory-profiler.mjs

# Open in Chrome DevTools
# 1. Open Chrome
# 2. DevTools → Memory tab
# 3. Load Snapshot → Select heap-*.heapsnapshot files
# 4. Compare snapshots to find retained objects
```

**Look for**:
- Oxigraph native objects
- Detached DOM nodes (if used in browser context)
- Retained closures
- Event listeners

### 2. Profile with Clinic.js

```bash
npm install -g clinic

# Memory leak detection
clinic doctor -- node tests/load/slow-stable-test.mjs

# Heap profiling
clinic heapprofiler -- node tests/load/slow-stable-test.mjs

# Open reports
clinic doctor --open
```

### 3. Trace Allocations

```bash
# Run with allocation profiler
node --trace-gc --trace-gc-verbose tests/load/slow-stable-test.mjs > gc.log 2>&1

# Analyze GC patterns
grep "Scavenge\|Mark-sweep" gc.log
```

---

## Code Fixes to Try

### Fix 1: Explicit Iterator Cleanup

```javascript
// Current (leaking)
for (const quad of store.match()) {
  // Process
}

// Try: Manual iterator control
const iterator = store.match();
try {
  for (const quad of iterator) {
    // Process
  }
} finally {
  // Explicit cleanup (if Oxigraph supports it)
  if (iterator.return) iterator.return();
}
```

### Fix 2: WASM Memory Management

Check if Oxigraph exposes memory management:

```javascript
import oxigraph from 'oxigraph';

// After operations
if (oxigraph.__wbindgen_free) {
  oxigraph.__wbindgen_free(); // Hypothetical
}
```

### Fix 3: Alternative Store Implementation

Fallback to N3.js (no known memory leaks):

```javascript
// File: packages/oxigraph/src/store-n3-fallback.mjs
import { Store } from 'n3';

export function createStoreFallback() {
  return new Store();
}

// Use in app
import { createStoreFallback } from '@unrdf/oxigraph/store-n3-fallback';

const store = createStoreFallback(); // Uses N3.js instead
```

---

## Monitoring & Alerts

### Production Monitoring

```javascript
// Add to observability setup
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('memory-monitor');

setInterval(() => {
  const mem = process.memoryUsage();
  const heapUsedMB = mem.heapUsed / 1024 / 1024;

  const span = tracer.startSpan('memory.check');
  span.setAttribute('heap.used.mb', heapUsedMB);
  span.setAttribute('heap.total.mb', mem.heapTotal / 1024 / 1024);
  span.setAttribute('rss.mb', mem.rss / 1024 / 1024);

  if (heapUsedMB > 500) {
    span.setAttribute('memory.alert', true);
    console.error(`🚨 Memory alert: ${heapUsedMB.toFixed(0)}MB`);
  }

  span.end();
}, 30000); // Every 30 seconds
```

### Alerting Thresholds

| Metric | Warning | Critical | Action |
|--------|---------|----------|--------|
| Heap Used | >300 MB | >500 MB | Restart process |
| Heap Growth Rate | >10 MB/min | >50 MB/min | Emergency restart |
| RSS | >1 GB | >2 GB | Scale down/restart |

---

## Testing the Fix

After applying any mitigation, re-run validation:

```bash
# 1. Quick smoke test
timeout 15s node tests/load/smoke-test.mjs

# 2. Leak detection test
timeout 150s node tests/load/slow-stable-test.mjs

# 3. Check for <5% growth
grep "memoryGrowthPercent" tests/load/slow-stable-*.json
# Should show: "memoryGrowthPercent": "4.2" (or less)
```

**Success Criteria**:
- ✅ Slow stable test: <10% growth over 2 minutes
- ✅ Memory stable test: <5% growth over 5 minutes
- ✅ Heap snapshots show no retained objects

---

## Upstream Contribution

If you fix the leak, contribute back to Oxigraph:

1. **Create minimal reproduction**:
   ```bash
   # Minimal test case
   node tests/load/slow-stable-test.mjs > reproduction.log
   ```

2. **Attach heap snapshots**:
   ```bash
   tar -czf heap-snapshots.tar.gz tests/load/heap-*.heapsnapshot
   ```

3. **File issue**: https://github.com/oxigraph/oxigraph/issues
   - Title: "Memory leak in JS bindings during iterator usage"
   - Attach: reproduction.log, heap-snapshots.tar.gz
   - Environment: Node v24.11.1, oxigraph version X.Y.Z

---

## Alternative Solutions

### Option 1: Migrate to N3.js

**Pros**:
- No known memory leaks
- Pure JavaScript (easier debugging)
- Good performance for <1M triples

**Cons**:
- Slower than Oxigraph for large datasets
- No SPARQL query support

### Option 2: Use External SPARQL Endpoint

**Pros**:
- Process isolation (leak contained)
- Can restart endpoint independently
- Industry-standard solution

**Cons**:
- Additional infrastructure
- Network latency
- Operational complexity

### Option 3: Hybrid Approach

```javascript
// Use Oxigraph for small queries, N3 for sustained
class AdaptiveStore {
  constructor() {
    this.oxiStore = createStore(); // Fast but leaky
    this.n3Store = new Store();    // Slow but stable
    this.oxiLastUsed = Date.now();
  }

  match(subject, predicate, object) {
    // Use N3 for sustained workloads
    if (Date.now() - this.oxiLastUsed > 60000) {
      return this.n3Store.match(subject, predicate, object);
    }

    // Use Oxigraph for bursts
    this.oxiLastUsed = Date.now();
    return this.oxiStore.match(subject, predicate, object);
  }
}
```

---

## Summary

**Immediate Action**: Use process restart + GC forcing as band-aid
**Short-term**: Implement store pooling + query limits
**Long-term**: Fix upstream or migrate to alternative

**Test After Each Change**:
```bash
node tests/load/slow-stable-test.mjs && echo "✅ PASS" || echo "❌ FAIL"
```

---

**Last Updated**: 2025-12-20
**Status**: Workarounds available, fix needed
**Priority**: 🔴 HIGH - Blocks production use >1 minute
