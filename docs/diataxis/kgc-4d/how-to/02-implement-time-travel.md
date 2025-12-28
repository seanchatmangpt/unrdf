# How-To: Implement Time Travel in Your App

**Problem:** You need to add state rollback and historical reconstruction to your application.

**Solution:** Use `reconstructState()` with snapshot caching for efficient time travel to any historical point.

**Time:** 20 minutes

---

## Prerequisites

- [Tutorial 03: Time Travel](../tutorials/03-time-travel.md)
- Application with KGC-4D event log

---

## Steps

### 1. Design Snapshot Strategy

**Rule of thumb:** Snapshot every N events or every T time interval.

```javascript
const SNAPSHOT_INTERVAL_EVENTS = 100; // Snapshot every 100 events
const SNAPSHOT_INTERVAL_TIME = 3600_000_000_000n; // Every hour in nanoseconds
```

### 2. Implement Auto-Snapshotting

```javascript
import { freezeUniverse, now, duration } from '@unrdf/kgc-4d';

let lastSnapshotTime = now();
let lastSnapshotEventCount = 0;

async function maybeSnapshot(store, git) {
  const currentCount = store.getEventCount();
  const currentTime = now();

  const eventDelta = currentCount - lastSnapshotEventCount;
  const timeDelta = duration(lastSnapshotTime, currentTime);

  if (eventDelta >= SNAPSHOT_INTERVAL_EVENTS || timeDelta >= SNAPSHOT_INTERVAL_TIME) {
    const receipt = await freezeUniverse(store, git);
    lastSnapshotTime = currentTime;
    lastSnapshotEventCount = currentCount;
    console.log('Auto-snapshot:', receipt.id);
    return receipt;
  }

  return null;
}
```

### 3. Implement Time Travel API

```javascript
import { reconstructState } from '@unrdf/kgc-4d';

async function timeTravelTo(store, git, targetTime) {
  console.log('Time-traveling to:', targetTime);
  const pastStore = await reconstructState(store, git, BigInt(targetTime));
  return pastStore;
}
```

### 4. Add Rollback Functionality

```javascript
async function rollbackToReceipt(store, git, receipt) {
  const pastStore = await reconstructState(store, git, BigInt(receipt.t_ns));
  
  // Option A: Replace current store (destructive)
  // store = pastStore;
  
  // Option B: Return new store (non-destructive, recommended)
  return pastStore;
}
```

---

## Complete Code

```javascript
import {
  KGCStore,
  GitBackbone,
  freezeUniverse,
  reconstructState,
  now,
  duration,
  EVENT_TYPES,
} from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

class TimeTravelStore {
  constructor(repoPath) {
    this.store = new KGCStore();
    this.git = new GitBackbone(repoPath);
    this.snapshots = [];
    this.snapshotInterval = 100; // events
  }

  async appendEvent(event, deltas) {
    const receipt = await this.store.appendEvent(event, deltas);
    
    // Auto-snapshot if needed
    if (receipt.receipt.event_count % this.snapshotInterval === 0) {
      const snapshot = await freezeUniverse(this.store, this.git);
      this.snapshots.push(snapshot);
      console.log('Auto-snapshot:', snapshot.id);
    }

    return receipt;
  }

  async timeTravelTo(targetTime) {
    return await reconstructState(this.store, this.git, BigInt(targetTime));
  }

  async rollbackToSnapshot(snapshotId) {
    const snapshot = this.snapshots.find(s => s.id === snapshotId);
    if (!snapshot) throw new Error('Snapshot not found');
    return await this.timeTravelTo(snapshot.t_ns);
  }

  listSnapshots() {
    return this.snapshots.map(s => ({
      id: s.id,
      time: s.timestamp_iso,
      eventCount: s.event_count,
    }));
  }
}

// Usage
async function main() {
  const ttStore = new TimeTravelStore('./time-travel-app');

  // Add events (auto-snapshots every 100 events)
  for (let i = 0; i < 250; i++) {
    await ttStore.appendEvent(
      { type: EVENT_TYPES.CREATE, payload: { index: i } },
      []
    );
  }

  console.log('Snapshots:', ttStore.listSnapshots());

  // Time travel
  const snapshot = ttStore.snapshots[1];
  const pastStore = await ttStore.rollbackToSnapshot(snapshot.id);
  console.log('Rolled back to:', snapshot.timestamp_iso);

  return ttStore;
}

main();
```

---

## Evidence

**Source:** `/home/user/unrdf/packages/kgc-4d/src/freeze.mjs` (reconstructState)  
**Tests:** `/home/user/unrdf/packages/kgc-4d/test/4d-time-travel-validation.test.mjs`  
**Example:** `/home/user/unrdf/packages/kgc-4d/examples/mission-critical.mjs:56-95`

---

## Troubleshooting

**Q: Time travel is slow?**  
A: Increase snapshot frequency. See [How-To 05: Optimize Performance](./05-optimize-performance.md).

**Q: Memory issues?**  
A: Don't keep all snapshots in memory. Store receipts in database and load on-demand.

---

## Related

- [Tutorial 03: Time Travel](../tutorials/03-time-travel.md) - Basics
- [Explanation: How Time Travel Works](../explanation/02-how-time-travel-works.md) - Algorithm
