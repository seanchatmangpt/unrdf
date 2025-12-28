# Tutorial: Use Vector Clocks for Causality

**Objective:** Learn how to track causal relationships in distributed systems using vector clocks.

**Time:** 20 minutes  
**Level:** Advanced

---

## Prerequisites

**Capabilities Needed:**
- Capability: "Vector clock increment/merge/compare"
- Capability: "Causal ordering detection"
- Capability: "Distributed event coordination"

**Prerequisites:**
- [Tutorial 01: Nanosecond Timestamps](./01-nanosecond-timestamps.md)
- Understanding of distributed systems (helpful)

---

## What You'll Learn

1. Create and increment vector clocks
2. Merge vector clocks from multiple nodes
3. Detect causal ordering (happened-before relationships)
4. Identify concurrent events

---

## Step 1: Create a Vector Clock

```javascript
import { VectorClock } from '@unrdf/kgc-4d';

// Create vector clock for node 'alice'
const clock = new VectorClock('alice');

console.log('Initial clock:', clock.toString());
// Output: alice:0
```

**What is a vector clock?** A vector clock is a map of {nodeId: counter} that tracks causality in distributed systems.

---

## Step 2: Increment Clock on Events

```javascript
// Alice performs actions
clock.increment();
console.log('After event 1:', clock.toString());
// Output: alice:1

clock.increment();
console.log('After event 2:', clock.toString());
// Output: alice:2
```

**Rule:** Increment your node's counter on every local event.

---

## Step 3: Merge Clocks (Receive Message)

```javascript
// Bob has his own clock
const bobClock = new VectorClock('bob');
bobClock.increment(); // bob:1

// Alice receives message from Bob
clock.merge(bobClock);
console.log('Alice after merge:', clock.toString());
// Output: alice:2,bob:1
```

**Rule:** When receiving a message, merge sender's clock with yours.

---

## Step 4: Detect Causal Relationships

```javascript
const clock1 = new VectorClock('alice');
clock1.increment(); // alice:1

const clock2 = new VectorClock('alice');
clock2.increment(); // alice:1
clock2.increment(); // alice:2

// Compare
const comparison = clock1.compare(clock2);
console.log('Comparison:', comparison);
// Output: -1 (clock1 happened-before clock2)

if (comparison === -1) {
  console.log('clock1 → clock2 (causally ordered)');
} else if (comparison === 1) {
  console.log('clock2 → clock1');
} else if (comparison === 0) {
  console.log('clock1 == clock2 (same event)');
} else {
  console.log('clock1 || clock2 (concurrent)');
}
```

---

## Step 5: Complete Working Example

```javascript
import { VectorClock } from '@unrdf/kgc-4d';

async function main() {
  // Three nodes in distributed system
  const alice = new VectorClock('alice');
  const bob = new VectorClock('bob');
  const charlie = new VectorClock('charlie');

  console.log('=== Distributed Event Scenario ===\n');

  // Alice: Event 1
  alice.increment();
  console.log('Alice event 1:', alice.toString());

  // Bob: Event 1
  bob.increment();
  console.log('Bob event 1:  ', bob.toString());

  // Alice sends message to Bob
  console.log('\n[Alice → Bob message]');
  bob.merge(alice);
  bob.increment(); // Bob processes message
  console.log('Bob after receive:', bob.toString());

  // Charlie: Event 1 (independent)
  charlie.increment();
  console.log('Charlie event 1:', charlie.toString());

  // Bob sends to Charlie
  console.log('\n[Bob → Charlie message]');
  charlie.merge(bob);
  charlie.increment();
  console.log('Charlie after receive:', charlie.toString());

  // Detect causality
  console.log('\n=== Causality Analysis ===');
  const aliceVsBob = alice.compare(bob);
  const bobVsCharlie = bob.compare(charlie);
  const aliceVsCharlie = alice.compare(charlie);

  console.log('Alice vs Bob:', aliceVsBob === -1 ? 'Alice → Bob' : 'concurrent');
  console.log('Bob vs Charlie:', bobVsCharlie === -1 ? 'Bob → Charlie' : 'concurrent');
  console.log('Alice vs Charlie:', aliceVsCharlie === -1 ? 'Alice → Charlie' : aliceVsCharlie === null ? 'concurrent' : 'other');

  return { alice, bob, charlie };
}

main();
```

---

## Verification

Run the complete example:

```bash
node tutorial-05-vector-clocks.mjs
```

Expected output:
```
=== Distributed Event Scenario ===

Alice event 1: alice:1
Bob event 1:   bob:1

[Alice → Bob message]
Bob after receive: alice:1,bob:2

Charlie event 1: charlie:1

[Bob → Charlie message]
Charlie after receive: alice:1,bob:2,charlie:2

=== Causality Analysis ===
Alice vs Bob: Alice → Bob
Bob vs Charlie: Bob → Charlie
Alice vs Charlie: Alice → Charlie (transitive)
```

---

## Evidence

**Source Code:**
- VectorClock class: `/home/user/unrdf/packages/kgc-4d/src/time.mjs:210` (VectorClock implementation)

**Tests:**
- Vector clock tests: `/home/user/unrdf/packages/kgc-4d/test/time.test.mjs` (VectorClock tests)

**Examples:**
- Local-first collaboration: `/home/user/unrdf/packages/kgc-4d/examples/local-first-collaboration.mjs`

---

## Key Takeaways

1. **Increment:** Increment your counter on every local event
2. **Merge:** Merge received clocks when receiving messages
3. **Compare:** Detect happened-before (-1), happened-after (1), equal (0), or concurrent (null)
4. **Partial order:** Vector clocks provide partial ordering in distributed systems

---

## Advanced: Concurrent Events

```javascript
const alice = new VectorClock('alice');
alice.increment(); // alice:1

const bob = new VectorClock('bob');
bob.increment(); // bob:1

// alice:1 vs bob:1
const result = alice.compare(bob);
console.log(result); // null (concurrent - no causal relationship)
```

Concurrent events cannot be ordered causally and may need conflict resolution.

---

## Next Steps

**Continue Learning:**
- [How-To 04: Vector Clocks for Distribution](../how-to/04-vector-clocks.md) - Production patterns
- [How-To 05: Optimize Performance](../how-to/05-optimize-performance.md) - Clock pruning

**Deep Dive:**
- [Explanation 06: Monotonic Clock Guarantees](../explanation/06-monotonic-clocks.md) - Clock semantics
- [Explanation 04: Why Git Backing](../explanation/04-why-git-backing.md) - Distributed architecture

**Reference:**
- [VectorClock API](../reference/vector-clock-api.md) - Complete API

---

**Navigate:** [← Previous](./04-query-event-logs.md) | [Tutorials](./README.md) | [Main Diataxis](../README.md)
