# Tutorial: Time Travel to Past States

**Objective:** Learn how to reconstruct historical state using snapshots and event replay.

**Time:** 20 minutes  
**Level:** Intermediate

---

## Prerequisites

**Capabilities Needed:**
- Capability: "State reconstruction from snapshots"
- Capability: "Event replay from EventLog"
- Capability: "Snapshot caching (O(1) lookup)"

**Prerequisites:**
- [Tutorial 02: Create and Freeze Universe](./02-create-freeze-universe.md)
- Understanding of event sourcing (helpful)

---

## What You'll Learn

1. Reconstruct state at a specific timestamp
2. Use snapshot + delta replay for efficiency
3. Verify time-traveled state matches expectations
4. Understand snapshot caching optimization

---

## Step 1: Create Multi-Event Timeline

```javascript
import { KGCStore, GitBackbone, freezeUniverse, EVENT_TYPES } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

const store = new KGCStore();
const git = new GitBackbone('./time-travel-repo');

// Event 1: Add Alice
await store.appendEvent(
  { type: EVENT_TYPES.CREATE, payload: { subject: 'Alice' } },
  [{
    type: 'add',
    subject: dataFactory.namedNode('http://example.org/Alice'),
    predicate: dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'),
    object: dataFactory.literal('Alice'),
  }]
);

// Freeze checkpoint 1
const snapshot1 = await freezeUniverse(store, git);
console.log('Snapshot 1:', snapshot1.timestamp_iso);

// Event 2: Add Bob
await store.appendEvent(
  { type: EVENT_TYPES.CREATE, payload: { subject: 'Bob' } },
  [{
    type: 'add',
    subject: dataFactory.namedNode('http://example.org/Bob'),
    predicate: dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'),
    object: dataFactory.literal('Bob'),
  }]
);

// Freeze checkpoint 2
const snapshot2 = await freezeUniverse(store, git);
console.log('Snapshot 2:', snapshot2.timestamp_iso);
```

---

## Step 2: Time Travel to Snapshot 1

```javascript
import { reconstructState } from '@unrdf/kgc-4d';

// Reconstruct state at snapshot1 time
const targetTime = BigInt(snapshot1.t_ns);
const pastStore = await reconstructState(store, git, targetTime);

// pastStore now contains ONLY Alice (state before Bob was added)
console.log('Time-traveled to:', snapshot1.timestamp_iso);
```

**What happened:**
1. Found nearest snapshot <= targetTime (snapshot1)
2. Loaded snapshot from Git
3. Replayed events between snapshot and targetTime (none in this case)
4. Returned new KGCStore with historical state

---

## Step 3: Verify Time-Traveled State

```javascript
import { GRAPHS } from '@unrdf/kgc-4d';

const universeGraph = dataFactory.namedNode(GRAPHS.UNIVERSE);

// Query Universe in pastStore
const pastQuads = [...pastStore.match(null, null, null, universeGraph)];
console.log('Quads in past state:', pastQuads.length);

// Should only see Alice, not Bob
const hasAlice = pastQuads.some(q => q.subject.value.includes('Alice'));
const hasBob = pastQuads.some(q => q.subject.value.includes('Bob'));

console.assert(hasAlice, 'Alice exists in past state');
console.assert(!hasBob, 'Bob does NOT exist in past state');
console.log('Verification: Time travel successful!');
```

---

## Step 4: Complete Working Example

```javascript
import {
  KGCStore,
  GitBackbone,
  freezeUniverse,
  reconstructState,
  EVENT_TYPES,
  GRAPHS,
} from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

async function main() {
  const store = new KGCStore();
  const git = new GitBackbone('./tutorial-03-repo');

  // Build timeline with snapshots
  const foaf = 'http://xmlns.com/foaf/0.1/';
  
  // T1: Add Alice
  await store.appendEvent(
    { type: EVENT_TYPES.CREATE, payload: { subject: 'Alice' } },
    [{
      type: 'add',
      subject: dataFactory.namedNode('http://example.org/Alice'),
      predicate: dataFactory.namedNode(foaf + 'name'),
      object: dataFactory.literal('Alice'),
    }]
  );
  const snap1 = await freezeUniverse(store, git);

  // T2: Add Bob
  await store.appendEvent(
    { type: EVENT_TYPES.CREATE, payload: { subject: 'Bob' } },
    [{
      type: 'add',
      subject: dataFactory.namedNode('http://example.org/Bob'),
      predicate: dataFactory.namedNode(foaf + 'name'),
      object: dataFactory.literal('Bob'),
    }]
  );
  const snap2 = await freezeUniverse(store, git);

  // T3: Add Charlie
  await store.appendEvent(
    { type: EVENT_TYPES.CREATE, payload: { subject: 'Charlie' } },
    [{
      type: 'add',
      subject: dataFactory.namedNode('http://example.org/Charlie'),
      predicate: dataFactory.namedNode(foaf + 'name'),
      object: dataFactory.literal('Charlie'),
    }]
  );

  console.log('Current state has 3 people (Alice, Bob, Charlie)');
  console.log('Snapshot 1 time:', snap1.timestamp_iso);
  console.log('Snapshot 2 time:', snap2.timestamp_iso);

  // Time travel to snap1 (only Alice)
  const past1 = await reconstructState(store, git, BigInt(snap1.t_ns));
  const universeGraph = dataFactory.namedNode(GRAPHS.UNIVERSE);
  const quads1 = [...past1.match(null, null, null, universeGraph)];
  console.log('\nTime-traveled to snap1:',quads1.length, 'quads');

  // Time travel to snap2 (Alice + Bob)
  const past2 = await reconstructState(store, git, BigInt(snap2.t_ns));
  const quads2 = [...past2.match(null, null, null, universeGraph)];
  console.log('Time-traveled to snap2:', quads2.length, 'quads');

  return { store, git, snap1, snap2, past1, past2 };
}

main();
```

---

## Verification

Run the complete example:

```bash
node tutorial-03-time-travel.mjs
```

Expected output:
```
Current state has 3 people (Alice, Bob, Charlie)
Snapshot 1 time: 2025-12-27T10:30:00.100Z
Snapshot 2 time: 2025-12-27T10:30:00.200Z

Time-traveled to snap1: 1 quads
Time-traveled to snap2: 2 quads
```

---

## Evidence

**Source Code:**
- reconstructState: `/home/user/unrdf/packages/kgc-4d/src/freeze.mjs` (reconstruct function)
- Snapshot caching: `/home/user/unrdf/packages/kgc-4d/src/snapshot-cache.mjs`

**Tests:**
- Time travel tests: `/home/user/unrdf/packages/kgc-4d/test/4d-time-travel-validation.test.mjs`

**Examples:**
- Time travel example: `/home/user/unrdf/packages/kgc-4d/examples/basic-usage.mjs:54-92`
- Mission-critical: `/home/user/unrdf/packages/kgc-4d/examples/mission-critical.mjs:56-95`

---

## Key Takeaways

1. **Snapshot + Delta:** Time travel uses nearest snapshot + event replay (efficient)
2. **O(1) lookup:** Snapshot cache finds nearest freeze in constant time
3. **Immutable:** Past states are reconstructed, not mutated
4. **Verifiable:** Reconstruct state matches cryptographic receipt

---

## Advanced: Event Replay

If targetTime is between snapshots, events are replayed:

```javascript
// Snapshot at T1 (Alice)
// Event at T2 (Add Bob)
// Event at T3 (Add Charlie)
// Snapshot at T4

// Time travel to T2.5 (between T2 and T3):
// 1. Load snapshot at T1
// 2. Replay event at T2 (Add Bob)
// 3. Stop (T3 is after targetTime)
// Result: State has Alice + Bob
```

---

## Next Steps

**Continue Learning:**
- [Tutorial 04: Query Event Logs](./04-query-event-logs.md) - Query the event history
- [How-To 02: Implement Time Travel](../how-to/02-implement-time-travel.md) - Production patterns
- [How-To 05: Optimize Performance](../how-to/05-optimize-performance.md) - Snapshot strategies

**Deep Dive:**
- [Explanation 02: How Time Travel Works](../explanation/02-how-time-travel-works.md) - Algorithm details
- [Explanation 03: Zero-Information Invariant](../explanation/03-zero-info-invariant.md) - Why this works

**Reference:**
- [Time API](../reference/time-api.md) - Timestamp functions
- [Receipt Schema](../reference/receipt-schema.md) - Snapshot metadata

---

**Navigate:** [← Previous](./02-create-freeze-universe.md) | [Tutorials](./README.md) | [Next →](./04-query-event-logs.md)
