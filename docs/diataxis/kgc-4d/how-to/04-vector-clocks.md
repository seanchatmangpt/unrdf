# How-To: Use Vector Clocks for Distributed Systems

**Problem:** You need to coordinate causality across multiple distributed nodes.

**Solution:** Use VectorClock for increment/merge/compare operations on each node.

**Time:** 30 minutes

---

## Prerequisites

- [Tutorial 05: Vector Clocks](../tutorials/05-vector-clocks.md)
- Distributed system architecture

---

## Pattern: Distributed Event Coordination

### Node Implementation

```javascript
import { VectorClock } from '@unrdf/kgc-4d';

class DistributedNode {
  constructor(nodeId) {
    this.nodeId = nodeId;
    this.clock = new VectorClock(nodeId);
  }

  localEvent() {
    this.clock.increment();
    return this.clock.snapshot();
  }

  sendMessage() {
    this.clock.increment();
    return {
      senderId: this.nodeId,
      clock: this.clock.snapshot(),
      payload: { /* ... */ },
    };
  }

  receiveMessage(message) {
    this.clock.merge(message.clock);
    this.clock.increment();
  }

  detectCausality(otherClock) {
    return this.clock.compare(otherClock);
  }
}
```

---

## Evidence

**Source:** `/home/user/unrdf/packages/kgc-4d/src/time.mjs:210` (VectorClock)  
**Example:** `/home/user/unrdf/packages/kgc-4d/examples/local-first-collaboration.mjs`

---

## Related

- [Reference: VectorClock API](../reference/vector-clock-api.md)
- [Explanation: Monotonic Clocks](../explanation/06-monotonic-clocks.md)
