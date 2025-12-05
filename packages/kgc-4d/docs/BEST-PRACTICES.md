# Best Practices: How to Use KGC 4D Correctly

**Proactive guidance: do this to prevent 80% of problems**

Not "what breaks and how to fix it" (see TROUBLESHOOTING.md for that), but "what to do from day one to avoid problems entirely."

---

## 1. Data Design Practices

### ✅ DO: Use Consistent URIs

```javascript
// GOOD: Consistent, canonical URIs
const alice = namedNode("http://example.org/people/alice");
const bob = namedNode("http://example.org/people/bob");
const knows = namedNode("http://xmlns.com/foaf/0.1/knows");

store.add(dataFactory.quad(alice, knows, bob));
```

**Why**: Consistency allows reliable queries. Different representations of same entity breaks time-travel.

### ❌ DON'T: Mix URI Formats

```javascript
// BAD: Same entity, different URIs
store.add(dataFactory.quad(
  namedNode("http://example.org/alice"),  // Lowercase
  // ...
));
store.add(dataFactory.quad(
  namedNode("http://example.org/Alice"),  // Uppercase!
  // ...
));
// These are treated as different entities
```

**Problem**: Time-travel queries fail (entity appears/disappears unexpectedly)

---

### ✅ DO: Use Namespace Prefixes

```javascript
// GOOD: Organized with prefixes
const EX = "http://example.org/";
const FOAF = "http://xmlns.com/foaf/0.1/";

const alice = namedNode(EX + "people/alice");
const name = namedNode(FOAF + "name");
const knows = namedNode(FOAF + "knows");

// Clear, reusable, maintainable
```

### ❌ DON'T: Hardcode Full URIs Everywhere

```javascript
// BAD: Duplicated, error-prone
store.add(dataFactory.quad(
  namedNode("http://example.org/people/alice"),
  namedNode("http://xmlns.com/foaf/0.1/name"),
  literal("Alice")
));
// Repeat 100 times with typo risk: "http://example.org/people/Alice" somewhere
```

---

## 2. Hook Configuration Practices

### ✅ DO: Validate at Boundaries, Not Internally

```javascript
// GOOD: Validate input ONCE at API boundary
export async function addPerson(personData) {
  // Validate external input
  const validated = PersonSchema.parse(personData);

  // Trust validated data internally
  const quad = createQuadFromPerson(validated);
  store.add(quad);
}
```

**Why**: Single validation point, no repeated overhead, clear flow.

### ❌ DON'T: Add Hooks for Every Operation Type

```javascript
// BAD: Validates repeatedly for same data
store.registerHook('validate-person', PersonSchema);
store.registerHook('validate-name', NameSchema);
store.registerHook('validate-email', EmailSchema);
store.registerHook('validate-uri-format', URISchema);

// Each operation triggers all hooks (cumulative overhead)
```

**Problem**: 44μs × 4 hooks = 176μs overhead per operation (11,700% degradation at 10K ops)

### ✅ DO: Use <1K Quads Per Batch

```javascript
// GOOD: Process in batches
const BATCH_SIZE = 500; // Safe, no optimization needed

async function importLargeDataset(file) {
  let batch = [];
  for await (const line of readLines(file)) {
    const quad = parseQuad(line);
    batch.push(quad);

    if (batch.length >= BATCH_SIZE) {
      await storeBatch(batch);  // <500ms per batch ✓
      batch = [];
    }
  }
  if (batch.length > 0) {
    await storeBatch(batch);
  }
}
```

**Why**: <1K ops = no performance issues, deployment-ready.

### ❌ DON'T: Process 100K Operations in One Call

```javascript
// BAD: Massive single operation
const allQuads = await loadEntireDataset();  // 100K quads
store.addMany(allQuads);  // 49+ seconds, unacceptable
```

**Problem**: Exceeds performance SLA, deployment blocked, requires optimization.

---

## 3. Time-Travel Practices

### ✅ DO: Create Snapshots at Key Points

```javascript
// GOOD: Snapshots for fast reconstruction
async function processImport(file) {
  // Import phase 1
  const phase1Quads = await phase1Import();
  store.addMany(phase1Quads);
  await store.createSnapshot('after-phase-1');  // ✓ Save state

  // Import phase 2
  const phase2Quads = await phase2Import();
  store.addMany(phase2Quads);
  await store.createSnapshot('after-phase-2');  // ✓ Save state

  // Verification
  const phase1State = await store.reconstructState({
    snapshotName: 'after-phase-1'
  });
  verify(phase1State);  // Fast: starts from snapshot
}
```

**Why**: O(1) reconstruction (instant) vs O(n) (replay all events)

### ❌ DON'T: Reconstruct to Far Past Without Snapshots

```javascript
// BAD: Replays 6 months of events
const stateJan1 = await store.reconstructState({
  targetTime: new Date('2025-01-01')  // 6 months ago, no snapshot nearby
});
// Replays 26M events: 10+ seconds
```

**Problem**: Slow queries, bad user experience.

### ✅ DO: Schedule Regular Snapshots

```javascript
// GOOD: Automatic snapshots (daily, weekly, or after major changes)
setInterval(async () => {
  const date = new Date().toISOString().split('T')[0];
  await store.createSnapshot(`daily-${date}`);
}, 24 * 60 * 60 * 1000);  // Every 24 hours
```

### ❌ DON'T: Never Create Snapshots

```javascript
// BAD: No snapshots ever
// Every time-travel query replays entire event log
// 100K events = 10+ seconds per query
```

---

## 4. Schema & Validation Practices

### ✅ DO: Use Zod for Input Validation

```javascript
// GOOD: Strong input validation
import { z } from 'zod';

const PersonSchema = z.object({
  name: z.string().min(1),
  email: z.string().email(),
  age: z.number().int().min(0).max(150)
});

export function createPerson(data: unknown) {
  const valid = PersonSchema.parse(data);  // Throws if invalid
  // Rest of code can trust `valid`
}
```

**Why**: Fail fast at boundary, no garbage data in store.

### ❌ DON'T: Trust External Input Without Validation

```javascript
// BAD: No validation
export function createPerson(data) {
  store.add(quad(
    person(data.name),      // What if data.name = null?
    email,                   // What if data.email is invalid?
    literal(data.email)
  ));
  // Bad data in store → breaks time-travel, state corruption
}
```

---

## 5. Monitoring & Alerting Practices

### ✅ DO: Monitor Four Key Metrics

```javascript
// 1. Latency (P95, P99)
setInterval(() => {
  const p95 = latencyHistogram.percentile(95);
  if (p95 > LATENCY_SLA) {
    alert(`Latency degraded: ${p95}ms (target: ${LATENCY_SLA}ms)`);
  }
}, 60000);

// 2. Memory (growing trend)
setInterval(() => {
  const used = process.memoryUsage().heapUsed / 1024 / 1024;
  if (used > MAX_MEMORY_MB) {
    alert(`Memory spike: ${used}MB (max: ${MAX_MEMORY_MB}MB)`);
  }
}, 60000);

// 3. Error Rate
setInterval(() => {
  const errorRate = errorCounter.value / totalCounter.value;
  if (errorRate > ERROR_THRESHOLD) {
    alert(`Error rate high: ${(errorRate * 100).toFixed(2)}%`);
  }
}, 60000);

// 4. Time-Travel Reconstruction Success Rate
setInterval(() => {
  const successRate = successCount / (successCount + failCount);
  if (successRate < 0.99) {  // <99% = problem
    alert(`Reconstruction failures: ${(successRate * 100).toFixed(2)}%`);
  }
}, 3600000);  // Every hour
```

**Why**: Catch problems before users notice.

### ❌ DON'T: Set Alerts for Everything

```javascript
// BAD: Alert fatigue
registerAlert('quad-added');      // Fires millions/day
registerAlert('memory-increased'); // Noise
registerAlert('gc-running');       // Normal operation

// Team ignores all alerts (alert fatigue)
// Real problem happens, no one responds
```

---

## 6. Deployment Practices

### ✅ DO: Follow Deployment Phases Exactly

See: `DEPLOYMENT-CHECKLIST.md` (10 phases, non-negotiable)

**Phase 2 is critical**: Know your operation count
- <1K: Safe now
- 1K-10K: Optimize first
- >10K: MUST optimize

**Phase 8 is critical**: Load test at actual expected load
- 5+ minutes sustained
- Watch latency, memory, errors
- Verify all metrics acceptable

### ✅ DO: Have Rollback Plan BEFORE Deploying

```javascript
// GOOD: Documented rollback
1. Deployment fails? → Immediate rollback
2. Error spike detected? → Automated rollback
3. Memory leak? → Manual rollback within 5 min
4. State corruption? → Restore from backup
```

### ❌ DON'T: Deploy Without Load Testing

```javascript
// BAD: "Looks good in dev, let's ship it"
// Deploy to production
// 1 hour later: Users report 10x latency
// Can't explain why (never measured at scale)
```

### ❌ DON'T: Deploy Without Rollback Plan

```javascript
// BAD: "We'll figure it out if something breaks"
// Something breaks
// Takes 2+ hours to rollback (no documented procedure)
// Users impacted entire time
```

---

## 7. Testing Practices

### ✅ DO: Test at Expected Scale

```javascript
// GOOD: Realistic test
async function testTimeTravel() {
  // Add 1000 quads (expected load)
  const quads = generateTestQuads(1000);
  await store.addMany(quads);

  // Reconstruct at various points
  for (const days = [1, 7, 30]) {
    const past = new Date();
    past.setDate(past.getDate() - days);

    const state = await store.reconstructState({
      targetTime: past
    });

    expect(state.length).toBe(0);  // No quads yet at that time
  }
}
```

**Why**: Tests pass at 100 quads, fail at 100K (discover problems early).

### ❌ DON'T: Test with Tiny Datasets

```javascript
// BAD: Toy test
async function testTimeTravel() {
  const quads = [
    dataFactory.quad(a, knows, b)  // 1 quad
  ];
  store.add(quads);

  const state = await store.reconstructState({
    targetTime: new Date('2025-01-01')
  });

  // ✓ Passes (too simple to fail)
  // Later, 100K quads → fails due to performance
}
```

---

## 8. Operational Practices

### ✅ DO: Document Everything

```javascript
// GOOD: Clear documentation
/*
 * TIME-TRAVEL RECONSTRUCTION
 *
 * How it works:
 * 1. Find nearest snapshot before targetTime
 * 2. Replay events from snapshot to targetTime
 * 3. Return state at that moment
 *
 * Performance:
 * - With snapshot: <100ms (O(1) lookup + replay delta)
 * - Without snapshot: 10+ seconds (replay all events, O(n))
 *
 * Production constraints:
 * - Create snapshots daily (24-hour replay worst case)
 * - Reconstruct only within last 1 year (data retention policy)
 * - Maximum 5 concurrent reconstructions (prevent GC spike)
 */
export async function reconstructState(options) {
  // ...
}
```

### ✅ DO: Version Your Schemas

```javascript
// GOOD: Versioned schemas
const PersonSchemaV1 = z.object({
  name: z.string(),
  email: z.string()
});

const PersonSchemaV2 = PersonSchemaV1.extend({
  phone: z.string().optional()  // Added in v2
});

// In code: use CURRENT_PERSON_VERSION
const validatePerson = (version) => {
  if (version === 1) return PersonSchemaV1;
  if (version === 2) return PersonSchemaV2;
  throw new Error(`Unknown schema version: ${version}`);
};
```

**Why**: Allows schema evolution without breaking existing data.

### ❌ DON'T: Assume Data Never Changes

```javascript
// BAD: No versioning
const PersonSchema = z.object({
  name: z.string(),
  email: z.string()
});

// 6 months later: "We need phone field"
// Add it to schema
// Now: Old data (no phone) fails validation
// Time-travel queries break
```

---

## 9. Quick Reference: The 10 Rules

1. **Use consistent URIs** → Reliable queries
2. **Validate at boundaries** → No garbage data
3. **Batch <1K operations** → Performance predictable
4. **Create snapshots regularly** → Time-travel fast
5. **Monitor 4 key metrics** → Catch problems early
6. **Follow deployment phases** → No surprises
7. **Have rollback plan** → Recover quickly
8. **Test at expected scale** → Discover real problems
9. **Document everything** → Team knows how to operate
10. **Version schemas** → Evolve without breaking

---

## 10. Decision Flow: "Should I Do This?"

```
Thinking about...

├─ Changing data structure?
│  └─ STOP: Document schema version first
│     Then: Code for migration path
│
├─ Adding performance feature?
│  └─ Check: BENCHMARKS.md (what's bottleneck?)
│     If not measured: MEASURE FIRST
│     Then: Optimize only bottleneck
│
├─ Deploying to production?
│  └─ Check: DEPLOYMENT-CHECKLIST.md (all 10 phases?)
│     If any skip: DO NOT DEPLOY
│     Then: Follow each phase exactly
│
├─ Adding new hook/validation?
│  └─ Check: Operation count <1K?
│     If YES: Safe to add
│     If 1K-10K: Optimize existing first
│     If >10K: Must optimize before adding
│
├─ Querying old data (time-travel)?
│  └─ Check: How old? (>7 days?)
│     If YES: Use snapshot (faster)
│     If NO: Direct reconstruction OK
│
└─ Unsure about something?
   └─ See: FAQ.md (quick answer)
      Or: GLOSSARY.md (define term)
      Or: TROUBLESHOOTING.md (fix problem)
```

---

## 11. Anti-Patterns: What Never Works

| Anti-Pattern | Problem | Solution |
|---|---|---|
| No URI consistency | Time-travel broken | Use canonical URIs |
| Hook on every operation | >49s for 100K ops | Validate at boundary |
| No snapshots | 10+ sec time-travel | Create snapshots daily |
| No monitoring | Problems invisible | Monitor 4 key metrics |
| Skip deployment phases | Production failures | Follow all 10 phases |
| Test with 1 quad | Misses real problems | Test at expected scale |
| No schema versioning | Data breaks on schema change | Version and migrate |
| No documentation | Team confusion | Document everything |
| All-or-nothing optimization | Wasted effort | Measure first, optimize bottleneck |
| Ignore FMEA guards | Preventable failures | Implement 24 guards |

---

## 12. Success Patterns: What Always Works

| Pattern | Why | How |
|---|---|---|
| Consistent URIs | Reliable queries | Use namespace prefixes |
| Validate at boundaries | No garbage data | Zod schemas at API entry |
| Small batches (<1K) | Performance guaranteed | Process data in chunks |
| Regular snapshots | Fast time-travel | Daily snapshot schedule |
| Monitor 4 metrics | Problems visible | Latency, memory, errors, reconstruction |
| Follow phases exactly | No surprises | DEPLOYMENT-CHECKLIST.md |
| Test at scale | Discover real issues | 1000 quads minimum |
| Schema versioning | Smooth evolution | V1, V2, migration path |
| Document operations | Team ready | Comments in code, runbooks |
| Implement guards | Prevent failures | 24 poka-yoke controls |

---

## 13. Common Decisions Made Wrong

### Decision: "Should we add validation hooks?"

❌ **Common wrong answer**: "Yes, validate everything"
- Result: 49s for 100K ops, deployment blocked, team frustrated

✅ **Correct answer**:
1. Check operation count
2. If <1K: Safe to add hooks
3. If 1K-10K: Optimize existing hooks first
4. If >10K: Profile bottleneck before adding anything
5. Always validate at API boundary, not internally

---

### Decision: "How often should we create snapshots?"

❌ **Common wrong answer**: "Never, we have event log"
- Result: 10+ seconds per time-travel query, users complain

✅ **Correct answer**:
1. Daily snapshots minimum (24-hour replay worst case)
2. After major imports/changes (faster reconstruction)
3. At version boundaries (support schema migrations)
4. Expected: <100ms reconstruction with snapshots

---

### Decision: "Is performance acceptable?"

❌ **Common wrong answer**: "Feels fast to me"
- Result: Fails load testing, shocked team, delayed deployment

✅ **Correct answer**:
1. Know your operation count (measure!)
2. Check BENCHMARKS.md section 3.1 (safe ranges)
3. Load test at that count for 5+ minutes
4. Verify latency, memory, errors all acceptable
5. Document baseline (for comparison later)

---

## 14. When to Escalate

**Get help from FMEA if**:
- New failure mode discovered → reference/FMEA-PRODUCTION.md
- Guard not working → implement from FMEA specification
- Risk threshold exceeded → risk mitigation required

**Get help from BENCHMARKS if**:
- Performance degrading → performance analysis section
- Optimization needed → BENCHMARKS.md section 4 roadmap
- Targets not met → BENCHMARKS.md section 5.1 SLAs

**Get help from DEPLOYMENT-CHECKLIST if**:
- Pre-deployment check fails → phase-by-phase diagnosis
- Risk concern → FMEA reference in checklist
- Monitoring setup unclear → Phase 4 detailed

---

**Summary**: Follow these best practices and you'll prevent 80% of problems before they occur. The other 20% of issues? See TROUBLESHOOTING.md.

---

Last updated: December 5, 2025 | Status: Production Guide ✅
