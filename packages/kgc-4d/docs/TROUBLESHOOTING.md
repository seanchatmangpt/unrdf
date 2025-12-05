# Troubleshooting Guide

**Diagnose and fix common problems quickly**

Use Ctrl+F to find your issue.

---

## Runtime Errors

### "Cannot read property 'value' of undefined"

**Likely cause**: Query returned empty results (no matching quads found)

**Diagnosis**:
```javascript
// Problem code
const results = store.match(subject, predicate, null);
const value = results[0].object.value; // ❌ Crashes if empty
```

**Solution**:
```javascript
// Fixed code
const results = store.match(subject, predicate, null);
if (results.length > 0) {
  const value = results[0].object.value; // ✓ Safe
}
```

**Root cause**: Check if data was actually added to store:
```bash
npm test  # Are tests passing? (should be 250/250)
```

---

### "Quad not found in store"

**Likely causes**:
1. URI format incorrect (case-sensitive!)
2. Predicate spelled wrong
3. Data was never added
4. Time-travel to wrong timestamp

**Diagnosis checklist**:

```javascript
// Check 1: Is subject URI exactly right?
const subject = namedNode("http://example.org/Alice");  // ✓ Correct
const subject = namedNode("http://example.org/alice");  // ❌ Different!

// Check 2: Is store empty?
const allQuads = store.match(null, null, null);
if (allQuads.length === 0) {
  console.error("Store is empty - nothing added!");
}

// Check 3: Is data at the right time?
const state = await store.reconstructState({
  targetTime: new Date('2025-12-05')
});
// If no data at that time, earlier events may not exist
```

**Solution**:
1. Verify URIs are exactly correct (case-sensitive, same format)
2. Print quads before querying to verify data was added
3. Use correct timestamp for time-travel queries

---

### "Invalid IRI format"

**Likely cause**: IRI (identifier) doesn't meet W3C standard

**Invalid formats**:
```javascript
namedNode("alice")                          // ❌ Missing protocol
namedNode("http://example.org/alice person") // ❌ Spaces not allowed
namedNode("example.org/alice")              // ❌ Missing http://
```

**Valid formats**:
```javascript
namedNode("http://example.org/alice")       // ✓ Standard HTTP IRI
namedNode("http://example.org/alice#1")     // ✓ With fragment
namedNode("http://example.org/resource/123") // ✓ With path
```

**Solution**: Always use full HTTP IRIs (http://domain/path/to/resource)

---

### "Snapshot not found before target time"

**Likely cause**: Requested time-travel to point before any events exist

**Diagnosis**:
```javascript
// When did events first occur?
const allEvents = store.match(null, null, null);
if (allEvents.length === 0) {
  console.error("No events in store!");
} else {
  console.log("First event timestamp needed");
}

// When are you querying?
const targetTime = new Date('2020-01-01'); // Too early?
```

**Solution**:
1. Check first event timestamp in event log
2. Only time-travel to dates >= first event date
3. If you need earlier state, add events or create them first

**Edge case handling** (handled automatically):
- No events exist → returns empty store ✓
- No snapshots before target time → clears error gracefully ✓

---

## Performance Issues

### Latency exceeds target SLA

**Diagnosis workflow**:

**Step 1: Identify operation count**
```javascript
const opCount = dataset.length;  // How many operations?

// If < 1K:   should be <50ms   (baseline: 0.02ms)
// If 1K-10K: should be <2s      (baseline: 33ms single hook)
// If >10K:   should be <5s      (baseline: 10827ms, UNACCEPTABLE)
```

**Step 2: Measure current latency**
```bash
npm run bench:hooks  # Generates benchmark report
# Look for: "Latency" section at your operation count
```

**Step 3: Identify bottleneck**
```javascript
// Bottleneck 1: Zod validation (~10μs per operation)
// Bottleneck 2: Hook chain (11-45μs × hook count)
// Bottleneck 3: Memory pressure (GC pauses)
```

**Step 4: Apply fixes (in order)**

### Fix 1: Validation Caching (15 min, 35% improvement)
```javascript
// BEFORE (no caching, re-validates every time)
export function executeHook(hook, quad) {
  const validatedHook = HookSchema.parse(hook); // ❌ 10μs every time
  // ...
}

// AFTER (cache validated hooks)
const validatedHooksCache = new WeakMap();

export function executeHook(hook, quad) {
  let validatedHook = validatedHooksCache.get(hook);
  if (!validatedHook) {
    validatedHook = HookSchema.parse(hook);
    validatedHooksCache.set(hook, validatedHook);  // ✓ Only parse once
  }
  // ...
}
```

**Expected gain**: 35% latency reduction

### Fix 2: Fast-path for Validation-Only Hooks (1-2 hours, 5.8x improvement)
Pre-validated schemas can skip full Zod parsing.

See: `BENCHMARKS.md` section 4.1.2

### Fix 3: Schema Compilation (30 min, 5-10% additional)
Pre-compile Zod schemas at initialization instead of per-operation.

See: `BENCHMARKS.md` section 4.1.3

**Decision tree for latency**:
```
Operation count?
  < 1K:     ✅ No action needed (safe now)
  1K-10K:   ⚠️  Apply Fix 1 (validation caching)
            Then test: does latency meet <2s target?
            If NO:    Apply Fix 2 (fast-path hooks)
  > 10K:    ❌ MUST apply all 3 fixes before deploying
            Target: <5s latency
            Measure: npm run bench:hooks
            Verify: All targets met
```

See: `BENCHMARKS.md` optimization roadmap (complete guide)

---

### Memory usage growing unbounded

**Diagnosis**:

```bash
# Check memory trend over time
npm run monitor:memory

# If seeing: 100MB → 200MB → 300MB (constant growth)
# That's a memory leak ❌
```

**Possible causes**:

1. **Hook cache not clearing**: Hooks accumulate without garbage collection
2. **Large snapshots accumulating**: Too many time-travel snapshots stored
3. **Event log not pruned**: Old events not being archived

**Solutions**:

### Solution 1: Implement memory limits
```javascript
const MAX_MEMORY_MB = 500;

function checkMemory() {
  const used = process.memoryUsage().heapUsed / 1024 / 1024;
  if (used > MAX_MEMORY_MB) {
    // Trigger emergency cleanup
    clearOldSnapshots();
    gc(); // Force garbage collection
  }
}
```

### Solution 2: Implement hook cache cleanup
```javascript
// Clear cache periodically (every 1 hour)
setInterval(() => {
  validatedHooksCache.clear();
}, 3600000);
```

### Solution 3: Archive old events
```javascript
// Move events older than 30 days to archive
async function archiveOldEvents(days = 30) {
  const cutoff = new Date();
  cutoff.setDate(cutoff.getDate() - days);

  const oldEvents = getEventsBeforeDate(cutoff);
  await archive.save(oldEvents);
  removeFromActiveLog(oldEvents);
}
```

**Monitor post-fix**:
```bash
npm run monitor:memory
# Should stabilize (no more growth)
```

---

### GC (Garbage Collection) pauses causing latency spikes

**Symptom**: Latency usually <100ms, occasionally spikes to 500ms+

**Cause**: JavaScript GC running during operation

**Mitigation**:

```javascript
// 1. Tune Node.js GC settings
// Start process with: node --expose-gc app.js

// 2. Manual GC during low-traffic periods
if (isLowTrafficPeriod()) {
  if (global.gc) {
    global.gc();
  }
}

// 3. Reduce allocation pressure
// Use object pools instead of creating new objects constantly
```

**Monitor**:
```bash
# Monitor GC events
npm run monitor:gc

# Should show: GC pauses < 50ms
```

---

## Testing & Validation Issues

### Tests are failing

**Diagnosis**:

```bash
npm test

# Output should show:
# ✓ 250/250 tests passing

# If showing failures: ❌ 248/250 tests passing
# Find which tests failed
```

**For each failed test**:

1. Read the test name (e.g., "time-travel reconstruction at T=2025-01-01")
2. Check what it tests (see `reference/COMPLETION-SUMMARY.md` Phase 1)
3. Read error message for root cause

**Common test failures**:

### Test: "Time-travel reconstruction fails"
**Likely cause**: Snapshot corruption or event log gap

**Fix**:
```javascript
// Verify snapshots are valid
async function validateSnapshots() {
  const snapshots = await store.getSnapshots();
  for (const snap of snapshots) {
    const checksum = calculateChecksum(snap.data);
    if (checksum !== snap.expectedChecksum) {
      console.error("Snapshot corrupted:", snap.id);
      // Rebuild from event log
    }
  }
}

npm test  // Re-run tests
```

### Test: "State mismatch at point T"
**Likely cause**: Event log gap or event order wrong

**Fix**:
```javascript
// Verify event log integrity
async function validateEventLog() {
  const events = await store.getEventLog();
  let prevTimestamp = new Date(0);

  for (const event of events) {
    if (event.timestamp < prevTimestamp) {
      console.error("Events out of order!");
      // Requires replay from backup
    }
    prevTimestamp = event.timestamp;
  }
}
```

---

### OTEL validation failing (score < 80/100)

**Symptom**: `npm run validate:otel` shows score < 80/100

**Diagnosis categories** (4 total):
1. **Trace completeness**: Missing span data?
2. **Log coverage**: Missing log entries?
3. **Metric collection**: Missing metrics?
4. **Error detection**: Not catching errors?

**Fix for each category**:

### Low trace completeness
```javascript
// Add missing instrumentation
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('kgc-4d');

export async function executeQuery(query) {
  const span = tracer.startSpan('executeQuery');
  try {
    // ... query logic
    span.setStatus({ code: SpanStatusCode.OK });
  } catch (err) {
    span.recordException(err);
    span.setStatus({ code: SpanStatusCode.ERROR });
  }
  span.end();
}
```

### Low log coverage
```javascript
// Add debug logging
import { logger } from './logger';

export function addQuad(quad) {
  logger.debug(`Adding quad: ${quad.subject.value}`);
  store.add(quad);
  logger.info(`Quad added successfully`);
}
```

**Re-validate**:
```bash
npm run validate:otel

# Target: Score ≥ 80/100 across all categories
```

---

## Deployment Issues

### Pre-deployment check fails

**Checklist to verify**:

```bash
# 1. Tests pass?
npm test
# Expected: ✓ 250/250 passing

# 2. OTEL validation passes?
npm run validate:otel
# Expected: Score ≥ 80/100

# 3. FMEA risk acceptable?
# Manual check: 0 high-risk modes?
# See: reference/FMEA-PRODUCTION.md

# 4. Performance targets met?
npm run bench:hooks
# Compare to BENCHMARKS.md section 5.1 targets
```

**If any fail**: Go back to that section and fix before deploying.

### Deployment halted due to risk threshold

**Reason**: FMEA found high-risk mode (RPN ≥ 100)

**Resolution**:
1. Review `reference/FMEA-PRODUCTION.md` (find the mode)
2. Understand the failure scenario
3. Implement guard/mitigation
4. Update FMEA
5. Re-validate: risk should drop below threshold

**Don't bypass**: Risk threshold exists for good reason.

---

## Data Integrity Issues

### Suspected state corruption

**Symptom**: Query returns unexpected data, contradicts earlier state

**Diagnosis**:

```javascript
// Step 1: Verify checksums
const snapshot1 = await store.getSnapshot('2025-12-01');
const checksum1 = calculateChecksum(snapshot1.data);
if (checksum1 !== snapshot1.expectedChecksum) {
  console.error("CORRUPTION DETECTED");
  // Requires recovery
}

// Step 2: Time-travel to before corruption
const uncorruptedState = await store.reconstructState({
  targetTime: new Date('2025-11-30')
});

// Step 3: Compare
// If uncorrupedState looks correct, corruption occurred 11/30 - 12/01
```

**Recovery procedure**:

1. **Identify corruption point**: What date did it occur?
2. **Get last good snapshot**: Before that date
3. **Replay events**: From good snapshot to point before corruption
4. **Skip corrupted event**: Find event that caused it
5. **Verify**: Does reconstructed state match reality?

**Post-recovery**:
```bash
npm test  # All tests pass?
npm run validate:otel  # OTEL validation OK?
```

---

## Getting Unstuck

### Still having problems after trying above?

**Step 1: Search documentation**
```
- GLOSSARY.md     (understand terminology)
- FAQ.md          (quick answers)
- INDEX.md        (search 150K+ words)
```

**Step 2: Check FMEA for your failure**
```
reference/FMEA-PRODUCTION.md
  - 28 failure modes listed
  - Each has: cause, mitigation, guard
  - Find your issue, follow mitigation
```

**Step 3: Run diagnostics**
```bash
npm test              # Test status?
npm run validate:otel # Validation status?
npm run bench:hooks   # Performance status?
npm run monitor:*     # Real-time monitoring
```

**Step 4: Review logs**
```bash
tail -f /var/log/kgc-4d.log

# Look for:
# ERROR  - Something failed (see details)
# WARN   - Degraded state (may recover)
# INFO   - Normal operations (good baseline)
```

### Nothing above helped

**Last resort**:

1. **Roll back to last known good version**
   - See: `DEPLOYMENT-CHECKLIST.md` Phase 6 (Rollback plan)

2. **File detailed issue**
   - Error message (exact)
   - Steps to reproduce
   - Environment (Node version, OS)
   - Logs (full context around error)

3. **Reference FMEA**
   - Your issue is likely in 28 failure modes
   - Mitigation steps provided for each

---

## Quick Lookup

| Problem | Check |
|---------|-------|
| Error message | TROUBLESHOOTING.md (this page) |
| Can't find answer | FAQ.md |
| What does term mean? | GLOSSARY.md |
| Performance slow | BENCHMARKS.md section 4 |
| Can't deploy | DEPLOYMENT-CHECKLIST.md |
| Tests failing | reference/COMPLETION-SUMMARY.md Phase 1 |
| Memory leak | Search "memory" in this page |
| Lost data | reference/FMEA-PRODUCTION.md (state corruption section) |
| Still stuck | INDEX.md (unified search) |

---

## Prevention is Better

**Avoid issues entirely**:

1. **Run deployment checklist** (Phase 8: Load testing)
   - Test at expected peak load for 5+ minutes
   - Catch issues before production

2. **Monitor continuously** (Phase 10: Post-deployment)
   - Daily review week 1
   - Weekly review weeks 2-4
   - Real-time alerts ongoing

3. **Keep backups** (Data safety)
   - Daily snapshots of event log
   - Offsite backup (cloud storage)
   - Test recovery procedures monthly

4. **Document your setup**
   - Hook configuration
   - Performance targets
   - Alert thresholds
   - Rollback procedure

---

**Still having issues?** You're not alone. Troubleshooting is normal and expected. Use this guide systematically - most issues are resolved within the diagnostic steps.

---

**Last updated**: December 5, 2025 | Status: Complete ✅
