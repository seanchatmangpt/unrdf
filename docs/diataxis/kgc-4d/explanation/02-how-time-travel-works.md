# Explanation: How Time Travel Reconstruction Works

**Question:** How does KGC-4D reconstruct historical state efficiently?

---

## The Algorithm

Time travel uses **snapshot + delta replay**:

1. Find nearest snapshot **<=** target time
2. Load snapshot from Git
3. Replay events between snapshot and target time
4. Return reconstructed store

---

## Example Timeline

```
T1: Event (Add Alice)
T2: SNAPSHOT [Alice]
T3: Event (Add Bob)
T4: Event (Update Alice age)
T5: SNAPSHOT [Alice (age=30), Bob]
T6: Event (Add Charlie)
```

**Time travel to T4:**
1. Find nearest snapshot <= T4 → T2
2. Load T2 snapshot (Alice only)
3. Replay T3 (Add Bob)
4. Replay T4 (Update Alice)
5. Return store with [Alice (age=30), Bob]

**Key insight:** Don't replay from beginning (slow). Start from nearest snapshot.

---

## Snapshot Caching

KGC-4D caches last snapshot pointer in System graph for O(1) lookup:

```javascript
// Cached in System graph
{
  subject: <kgc:LastSnapshot>,
  predicate: <kgc:git_ref>,
  object: "a1b2c3d4...",
  graph: <kgc:System>
}
```

**No full EventLog scan needed** to find snapshots.

---

## Rationale

### Why Snapshot + Replay?

**Alternative 1: Full Replay from Genesis**
- Replay every event from T0 to T_target
- Slow for long histories (O(N) events)

**Alternative 2: Store Every State**
- Store full Universe after every event
- Fast reads (O(1)) but huge storage (O(N) states)

**Snapshot + Replay:**
- Balance: O(log N) snapshots, O(N/log N) events between snapshots
- Tunable: More snapshots = faster replay, larger Git repo

---

## Performance

**Snapshot every 100 events:**

| Events | Snapshots | Avg Replay | Max Replay |
|--------|-----------|------------|------------|
| 100    | 1         | 50         | 100        |
| 1000   | 10        | 50         | 100        |
| 10000  | 100       | 50         | 100        |

**Constant average replay cost regardless of total history length.**

---

## Evidence

**Source:** `/home/user/unrdf/packages/kgc-4d/src/freeze.mjs` (reconstructState function)  
**Tests:** `/home/user/unrdf/packages/kgc-4d/test/4d-time-travel-validation.test.mjs`

---

## Related

- [Tutorial 03: Time Travel](../tutorials/03-time-travel.md)
- [How-To 05: Optimize Performance](../how-to/05-optimize-performance.md)
