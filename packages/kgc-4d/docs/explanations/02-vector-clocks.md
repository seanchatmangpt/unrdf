# Causality and Vector Clocks

This explanation dives deep into how KGC 4D tracks causality in distributed systems using vector clocks.

## The Problem: Ordering Events in Distributed Systems

### The Challenge

Imagine two independent servers creating events:

```
Server A: 14:30:00.000 - "Transfer $50 from Alice"
Server B: 14:30:00.001 - "Transfer $50 from Alice to Carol"
Server A: 14:30:00.002 - "Check balance"
```

**Question:** Did the balance check happen after both transfers, or in between?

**Naive answer:** Use timestamps - 14:30:00.002 is after both, so yes.

**Problem:** What if servers have clock skew?

```
Server A clock: 2 seconds fast
Server B clock: correct
Server A: 14:30:02.000 - "Transfer $50 from Alice"
Server B: 14:30:00.001 - "Transfer $50 to Carol"

Timestamps say A happened after B
Reality: B happened first (servers not connected when B executed)
```

Timestamps alone **cannot determine causality**. You need vector clocks.

## What Are Vector Clocks?

A vector clock is a data structure that each node maintains:

```
Node A: vc_A = {A: 0, B: 0}
Node B: vc_B = {A: 0, B: 0}

Event 1 at A: "send message to B"
  vc_A = {A: 1, B: 0}

Message sent to B with vc_A

Event 2 at B: "receive message from A"
  vc_B = max(vc_B, vc_A) = {A: 1, B: 0}
  vc_B = {A: 1, B: 1}  // B increments its own counter

Event 3 at B: "do something"
  vc_B = {A: 1, B: 2}

Event 4 at A: "hear about B's event"
  vc_A = max(vc_A, message.vc) = {A: 1, B: 2}
  vc_A = {A: 2, B: 2}
```

## Why Vector Clocks Work

### Partial Ordering

Vector clocks create a **partial order** on events:

```
vc(A) < vc(B) ⟺ A happened before B (A causally precedes B)
vc(A) = vc(B) ⟺ A and B are the same event
vc(A) || vc(B) ⟺ A and B are concurrent (neither caused the other)
```

**Example:**

```
vc_A = {A: 3, B: 1}
vc_B = {A: 3, B: 3}

Is A < B?
  A's value for A: 3 ≤ B's value for A: 3 ✓
  A's value for B: 1 ≤ B's value for B: 3 ✓
  At least one is strict <: B's B is 3 > 1 ✓

Result: A < B, so A happened before B
```

### Detecting Concurrency

```
vc_A = {A: 2, B: 1}
vc_B = {A: 1, B: 2}

Is A < B?
  A's A: 2 > B's A: 1 ✗ (not ≤)
  So A ≮ B

Is B < A?
  B's B: 2 > A's B: 1 ✗ (not ≤)
  So B ≮ A

Result: A and B are concurrent (neither caused the other)
```

## The Algorithm

### Receiving an Event Locally

```javascript
class VectorClock {
  constructor(nodeId) {
    this.nodeId = nodeId;
    this.clock = new Map();  // { nodeId: counter, ... }
  }

  increment() {
    // Local event: increment own counter
    const current = this.clock.get(this.nodeId) || 0;
    this.clock.set(this.nodeId, current + 1);
    return new Map(this.clock);
  }
}

// Usage:
const clock = new VectorClock('node-a');
const vc1 = clock.increment();  // {node-a: 1}
const vc2 = clock.increment();  // {node-a: 2}
```

### Sending a Message

Include the vector clock with the message:

```javascript
// Node A wants to send to Node B
const message = {
  data: "important info",
  vectorClock: clock.increment()  // Include current clock
};

// Send to B (via network, queue, etc.)
sendToB(message);
```

### Receiving a Message

```javascript
function receiveMessage(message) {
  // Update clock: take element-wise maximum
  const received = message.vectorClock;

  for (const [node, count] of received.entries()) {
    const current = this.clock.get(node) || 0;
    this.clock.set(node, Math.max(current, count));
  }

  // Then increment own counter
  const myCount = this.clock.get(this.nodeId) || 0;
  this.clock.set(this.nodeId, myCount + 1);

  return new Map(this.clock);
}
```

### Full Example

```
Network: A ---> B ---> C

Initial:
  vc_A = {A: 0, B: 0, C: 0}
  vc_B = {A: 0, B: 0, C: 0}
  vc_C = {A: 0, B: 0, C: 0}

Event 1: A creates event e1
  vc_A = {A: 1, B: 0, C: 0}

Event 2: A sends e1 to B
  Message includes vc_A = {A: 1, B: 0, C: 0}

Event 3: B receives e1
  vc_B = max(vc_B, {A: 1, B: 0, C: 0}) = {A: 1, B: 0, C: 0}
  vc_B = {A: 1, B: 1, C: 0}

Event 4: B creates event e2
  vc_B = {A: 1, B: 2, C: 0}

Event 5: B sends e1 and e2 to C
  Message includes vc_B = {A: 1, B: 2, C: 0}

Event 6: C receives from B
  vc_C = max(vc_C, {A: 1, B: 2, C: 0}) = {A: 1, B: 2, C: 0}
  vc_C = {A: 1, B: 2, C: 1}

Now we can determine:
  e1 → e3 → e4 → e5 → e6  (all causally ordered)
  All events have:
    e1.vc = {A: 1, B: 0, C: 0}
    e3.vc = {A: 1, B: 1, C: 0}
    e4.vc = {A: 1, B: 2, C: 0}
    e6.vc = {A: 1, B: 2, C: 1}

  Each e1 < e3 < e4 < e6 (provably)
```

## Properties of Vector Clocks

### Correctness

**If A → B (A caused B), then vc(A) < vc(B)**

Proof: B must either:
1. Receive a message from A (updates B's clock to include A's)
2. Or learn about A through transitive messages
In both cases, vc(B) > vc(A)

**If vc(A) < vc(B), then A → B**

Follows from how clocks are updated.

### Completeness

**If A and B are concurrent, then vc(A) || vc(B)**

If neither vc(A) < vc(B) nor vc(B) < vc(A), they're concurrent.

### Space Complexity

For a system with N nodes, each vector clock has N values.

```
N = 3 nodes: clock = {A: 3, B: 5, C: 2} (3 values)
N = 100 nodes: clock has 100 values (could be large!)

Optimization: Only store non-zero entries
             or use interval tree clocks for large N
```

## Use Cases in KGC 4D

### Tracking Event Causality

```javascript
const event1 = await store.appendEvent(
  { type: 'CREATE' },
  mutations
);
// event1.vectorClock = {node-1: 1, node-2: 0}

const event2 = await store.appendEvent(
  { type: 'UPDATE' },
  mutations
);
// event2.vectorClock = {node-1: 2, node-2: 0}

// Event1 causally precedes Event2
// Provable from clocks: {1,0} < {2,0}
```

### Detecting Conflicts

```
Distributed e-commerce:
  Warehouse-A: "Sell item to customer1"  (vc_A = {A: 1, B: 0})
  Warehouse-B: "Sell same item to customer2"  (vc_B = {A: 0, B: 1})

Vector clocks: {1,0} || {0,1} (concurrent!)
Meaning: These sales didn't know about each other
         Both tried to sell the same item simultaneously

Result: Conflict detected
        (Real-world: need resolution strategy)
```

### Determining Read Causality

```javascript
// Client reads from server at time T1 with vc_server = {A: 5, B: 3}
const read1 = await fetchData();

// Client later reads at time T2 with vc_server = {A: 5, B: 5}
const read2 = await fetchData();

// Knowing vc(read1) < vc(read2), we know:
// - read2 definitely includes all changes from read1
// - No need to re-fetch data already in read1
```

## Comparison with Alternatives

### Lamport Clocks

```
Single counter per system:
  Lamport_1 = 5
  Lamport_2 = 5

Problem: Can't tell if events are causal or concurrent
         Both have clock value 5, so we don't know their relationship

Vector Clock:
  vc_1 = {A: 5, B: 3}
  vc_2 = {A: 3, B: 5}

Solution: vc_1 and vc_2 are concurrent (neither ordered)
```

### Timestamp-based Ordering

```
Timestamp approach:
  Event A: 14:30:00.000
  Event B: 14:30:00.001

Problem: Clock skew makes B appear after A even if B happened first

Vector Clock approach:
  Event A: {A: 5, B: 3}
  Event B: {A: 3, B: 5}

Solution: Clocks show causality regardless of wall-clock time
```

### Wall-Clock Time vs Vector Clocks in KGC 4D

```
Wall-clock (t_ns):
  - Answers: "When did this happen?" (absolute time)
  - Subject to clock skew
  - Useful for compliance and auditing

Vector Clocks:
  - Answers: "Did A cause B?" (relative causality)
  - Immune to clock skew
  - Useful for conflict detection and ordering

KGC 4D uses BOTH:
  - t_ns for timeline queries
  - Vector clocks for causality
  - Together they're complete
```

## Practical Implications

### Consistency Models

Vector clocks enable **eventual consistency**:

```
Update on Node A: event1
Update on Node B: event2 (concurrent with event1)

Question: Do all nodes eventually agree on order?
Answer: No! They're concurrent (legitimately)

Solution: Use conflict resolution
  - Last-write-wins
  - Merge function
  - Require human resolution
```

### Strong Eventual Consistency

```
If all nodes eventually receive all messages:
  → All nodes will eventually agree on vector clocks
  → All nodes will agree on which events are causal
  → All nodes will agree on total order of causal chains

This is guaranteed by vector clock properties.
```

### Availability vs Consistency Trade-off

```
With vector clocks, KGC 4D can:
- Continue operating during network partitions
- Detect conflicts when nodes reconnect
- Merge updates deterministically

Without vector clocks:
- Would need to choose: availability OR consistency
- Or use quorum (reduces availability)
```

## Implementation in KGC 4D

```javascript
import { VectorClock } from '@unrdf/kgc-4d';

// Create clock for this node
const clock = new VectorClock({ 'node-1': 0, 'node-2': 0 });

// Increment on local event
clock.increment('node-1');

// Merge on receiving message
clock.merge({ 'node-1': 5, 'node-2': 3 });

// Check causality
const isBefore = clock.happensBefore(other);
```

## Summary

- **Vector clocks** track causality in distributed systems
- **vc(A) < vc(B)** means A happened before B
- **vc(A) || vc(B)** means A and B are concurrent
- KGC 4D uses them to track which events depend on which
- Combined with wall-clock time (t_ns), they provide complete temporal ordering
- No single point of failure or central clock needed
