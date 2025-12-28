# How-To: Optimize Snapshot Performance

**Problem:** Freeze/replay is slow for large knowledge graphs.

**Solution:** Tune snapshot frequency, use caching, and optimize event replay.

**Time:** 10 minutes

---

## Optimization Strategies

### 1. Snapshot Frequency Tuning

**Too few snapshots:** Slow replay (many events)  
**Too many snapshots:** Slow freeze, large Git repo

**Recommended:**
- Small graphs (<10K quads): Every 1000 events
- Medium graphs (10K-100K): Every 500 events
- Large graphs (>100K): Every 100 events

### 2. Use Snapshot Caching

KGC-4D automatically caches last snapshot pointer in System graph (O(1) lookup).

### 3. Batch Event Replay

Replay events in batches for better performance.

---

## Evidence

**Source:** `/home/user/unrdf/packages/kgc-4d/src/snapshot-cache.mjs`  
**Tests:** `/home/user/unrdf/packages/kgc-4d/test/snapshot-cache.test.mjs`

---

## Related

- [Explanation: Performance Tradeoffs](../../explanation/performance-tradeoffs.md)
