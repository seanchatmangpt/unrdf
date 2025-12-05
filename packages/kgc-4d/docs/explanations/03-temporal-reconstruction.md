# Temporal Reconstruction

This explanation covers how KGC 4D can reconstruct the state of your knowledge graph at any point in time.

## The Zero-Information Invariant

KGC 4D is built on a single fundamental principle:

**All state at any point in time is reconstructible from:**
- **Event Log** (immutable history of all mutations)
- **Git Snapshots** (optional optimizations)

This means:
- You never need to store state separately
- State is always consistent
- You can travel to any point in history
- You can prove the reconstruction is correct

## Why This Matters

### Traditional Approach

```
Database:
  Current state in primary table
  History in separate audit table (if present)

Problem: State and history can diverge
         If primary table is corrupted,
         you might not have consistent history to restore from

Example:
  Primary: Alice's balance = $50
  Audit log: [+$100, -$50, +$20, -$30]
  Reconstructed: $100 - $50 + $20 - $30 = $40

  Question: Is it $50 or $40? Which is correct?
  Answer: If primary is corrupted, you don't know!
```

### KGC 4D Approach

```
EventLog: [CREATE Alice, SET balance $100, SET balance $50]

Current state? Reconstruct from EventLog:
  1. CREATE Alice (she exists)
  2. SET balance $100 (balance = $100)
  3. SET balance $50 (balance = $50)
  Result: balance = $50 ✓

State at time T2? Reconstruct from EventLog[0..T2]:
  1. CREATE Alice
  2. SET balance $100
  Result: balance = $100 ✓

No separate "current state" table needed!
EventLog is the authoritative source.
```

## Reconstruction Algorithm

### Simple Case: No Snapshots

```
reconstructState(store, git, targetTime):
  1. Start with empty knowledge graph
  2. Read entire EventLog from store
  3. Filter events where tNs ≤ targetTime
  4. Sort events by tNs (already sorted)
  5. Replay each mutation in order
  6. Return reconstructed store
```

**Example:**

```javascript
const store = new KGCStore();

// Current state after events e1, e2, e3
// Alice's name has been set 3 times: "Alice" → "Alicia" → "Alice"
// Current: name = "Alice"

// Now reconstruct at time T2 (between e2 and e3)
const pastStore = await reconstructState(store, git, T2);

// EventLog has: e1, e2, e3
// Filter: e1, e2 (e3 is after T2)
// Replay: e1 (create Alice), e2 (set name to "Alicia")
// Result: pastStore has name = "Alicia" ✓
```

**Time complexity:** O(e) where e = events after target time
**Space complexity:** O(q) where q = quads at target time

### Optimized Case: With Snapshots

```
reconstructState(store, git, targetTime):
  1. Find newest snapshot where snapshot.tNs ≤ targetTime
  2. Load snapshot from Git
  3. Create new store with snapshot as initial state
  4. Get EventLog after snapshot time
  5. Replay only events > snapshot.tNs
  6. Return reconstructed store
```

**Performance improvement:**

```
Without snapshots:
  10,000 events total
  Target time = 5,000 events ago
  Need to replay: all 10,000 events

With snapshot at event 5,000:
  Snapshot at tNs = 5000
  Target time = 5,001 (just after snapshot)
  Need to replay: 1 event only

  Speed improvement: 10,000x faster!
```

### Snapshot Strategy

**When to create snapshots:**

```
Option 1: Time-based
  Every 1 hour, create snapshot
  Pro: Predictable replay time (max 1 hour of events)
  Con: Might create unnecessary snapshots

Option 2: Event-based
  After every 1,000 events, create snapshot
  Pro: Proportional to activity
  Con: Variable times between snapshots

Option 3: Hybrid
  Create snapshot every 1 hour OR every 1,000 events (whichever comes first)
  Pro: Balanced approach

Option 4: On-demand
  Create snapshots only when needed
  Pro: Minimal storage
  Con: First query is slow, subsequent ones are cached
```

**KGC 4D recommendation:** Option 3 (hybrid) for balanced performance.

## Determinism: The Key Property

### Why Determinism Matters

Reconstruction MUST be deterministic. Given the same EventLog, the same targetTime must always produce identical state.

```javascript
const pastStore1 = await reconstructState(store, git, T);
const results1 = pastStore1.querySync(query);

const pastStore2 = await reconstructState(store, git, T);
const results2 = pastStore2.querySync(query);

// Must be identical!
assert(results1 === results2);
```

### How KGC 4D Ensures Determinism

1. **EventLog is immutable** - Can't change past events
2. **Events are ordered by tNs** - Deterministic order
3. **Mutations are applied in order** - No randomness
4. **RDF semantics are deterministic** - Same triples = same meaning
5. **Snapshot export is canonical** - Same order every time

**Result:** Reconstruction is a pure function:

```
reconstruct(eventLog, targetTime) → state
  This function ALWAYS returns the same state for same inputs.
```

## Temporal Queries

### Point-in-Time Queries

"What was true at exact time T?"

```javascript
const pastStore = await reconstructState(store, git, timestamp);
const results = pastStore.querySync(`
  SELECT ?person ?name
  WHERE {
    GRAPH <kgc:Universe> { ?person ex:name ?name }
  }
`);
```

### Range Queries

"What was true between T1 and T2?"

```javascript
const store1 = await reconstructState(store, git, T1);
const store2 = await reconstructState(store, git, T2);

const results1 = store1.querySync(query);
const results2 = store2.querySync(query);

// Compare to find what changed
const changed = findDifferences(results1, results2);
```

### Interval Queries

"Find all people who had name='Alice' anytime between T1 and T2"

```javascript
// Sample at regular intervals
const interval = BigInt(1000000000); // 1 second
const times = [];
for (let t = T1; t <= T2; t += interval) {
  times.push(t);
}

const allStores = await Promise.all(
  times.map(t => reconstructState(store, git, t))
);

const allResults = allStores.map(s => s.querySync(query));

// Find union of all results
const allPeople = new Set();
allResults.forEach(result => {
  result.forEach(row => {
    allPeople.add(row.get('person').value);
  });
});
```

## Example: Audit Trail

Find when Alice's email changed:

```javascript
async function auditTrail(store, git, entity) {
  // Query EventLog for all changes to this entity
  const changes = store.querySync(`
    PREFIX kgc: <kgc:>
    PREFIX ex: <http://example.org/>

    SELECT ?timestamp ?mutation
    WHERE {
      GRAPH <kgc:EventLog> {
        ?event kgc:T_NS ?timestamp .
        ?event kgc:subject <${entity}> ;
               kgc:predicate ex:email .
      }
    }
    ORDER BY ?timestamp
  `);

  // For each change, reconstruct state before and after
  const timeline = [];
  for (const row of changes) {
    const ts = BigInt(row.get('timestamp').value);

    // Before change
    const before = await reconstructState(store, git, ts - 1n);
    const email1 = queryEmail(before, entity);

    // After change
    const after = await reconstructState(store, git, ts);
    const email2 = queryEmail(after, entity);

    timeline.push({
      timestamp: ts,
      before: email1,
      after: email2,
    });
  }

  return timeline;
}

// Usage:
const audit = await auditTrail(store, git, 'http://example.org/alice');
console.table(audit);
// Shows every email change with before/after values and exact timestamp
```

## Consistency Guarantees

### Strong Consistency

Within a single store, temporal reconstruction provides **strong consistency**:

```
Event e1 at T1: "Set X = 10"
Event e2 at T2 > T1: "Set X = 20"

For any time T:
  T < T1: X doesn't exist or has previous value
  T1 ≤ T < T2: X = 10
  T ≥ T2: X = 20

Guaranteed, no ambiguity.
```

### Causality Respecting

Reconstruction respects causal relationships:

```
Event A (vc = {1, 0}): "Create Alice"
Event B (vc = {1, 1}): "Set Alice's age = 30"

If A → B (causally):
  Then any time >= B's timestamp will include both
  Any time between A and B will have A but not B

This is guaranteed by vector clocks.
```

## Practical Considerations

### Memory Usage

```javascript
// Small store (1000 quads)
pastStore = await reconstructState(store, git, T);
// Memory: ~10 MB

// Large store (100,000,000 quads)
pastStore = await reconstructState(store, git, T);
// Memory: ~10 GB

// Solution: Don't load entire history into memory
// Instead, query incrementally
```

### Time for Reconstruction

```
Without snapshots:
  Time = time_to_replay_events + time_to_load_eventlog
  For 1M events: ~30-60 seconds

With snapshots every 100K events:
  Find nearest snapshot: fast
  Replay at most 100K events: ~3-6 seconds

  Speed improvement: 5-20x
```

### Snapshot Cleanup

Old snapshots can be pruned to save space:

```javascript
async function pruneSnapshots(store, git, keepDays) {
  const cutoff = now() - BigInt(keepDays) * BigInt(86_400_000_000_000);

  // Only keep snapshots newer than cutoff
  // EventLog is authoritative, so no data loss

  // Can restore ANY point in time from EventLog alone
}
```

## Edge Cases

### What if an Event is Very Old?

```
Target time = 100 years ago
EventLog still has all events from then
Reconstruction still works!

Replaying 100 years of events might be slow, but it's possible.
This is why snapshots are important.
```

### What if EventLog is Corrupted?

```
Can't reconstruct - error thrown

Solution: Keep snapshots in Git
         Each snapshot is a Git commit
         Can manually restore from Git commit

Result: Redundancy protects against EventLog corruption
```

### What if Someone Modifies an Event?

```
EventLog is stored in Oxigraph (immutable in KGC 4D)
Changing an event would require:
  1. Modifying the database file directly (hacky)
  2. Re-computing all downstream events

BLAKE3 hash of snapshot would mismatch
Git history would show the change
Compromised snapshot would be detected!

Result: Modification is detectable via Git/BLAKE3
```

## Summary

- **Reconstruction** = Snapshot + Replay
- **Deterministic** = Same inputs always give same output
- **Immutable** = EventLog is the authoritative source
- **Temporal** = Can query any point in time
- **Efficient** = Snapshots optimize replay distance
- **Complete** = Entire history always available

This is the core of the "zero-information invariant": state is never stored separately—it's always reconstructed from events.
