# Explanations

Explanations are **understanding-oriented guides** that help you grasp the concepts and principles behind KGC 4D. Unlike tutorials and how-tos, these dive into the *why* rather than the *how*.

## Available Explanations

1. **[Why 4 Dimensions?](./01-four-dimensions.md)**
   - Observable state (O) - the data
   - Nanosecond time (t_ns) - when it happened
   - Vector causality (V) - why it happened
   - Git references (G) - cryptographic proof

2. **[Causality and Vector Clocks](./02-vector-clocks.md)**
   - What is causality in distributed systems?
   - How vector clocks track ordering
   - Happened-before relationships
   - Detecting concurrent events

3. **[Temporal Reconstruction](./03-temporal-reconstruction.md)**
   - The zero-information invariant
   - How snapshots optimize replays
   - Time travel algorithms
   - Deterministic state reconstruction

4. **[Git as Immutable History](./04-git-backbone.md)**
   - Why Git, not a database?
   - Content-addressed snapshots
   - BLAKE3 cryptographic hashing
   - Verifying snapshot integrity

5. **[Event Sourcing Architecture](./05-event-sourcing.md)**
   - The event log pattern
   - Atomic transaction semantics
   - Eventual consistency models
   - Comparing with CQRS and other patterns

6. **[FMEA and Mistake-Proofing](./06-poka-yoke.md)**
   - What is FMEA analysis?
   - Poka-yoke principles
   - How 24 guards prevent defects
   - Building robust systems

---

## Learning Progression

Start with **Why 4 Dimensions?** to understand the big picture:

```
Why 4 Dimensions?
    ↓
Causality and Vector Clocks
    ↓
Temporal Reconstruction
    ↓
Git as Immutable History
    ↓
Event Sourcing Architecture
    ↓
FMEA and Mistake-Proofing
```

Or jump directly to topics of interest.

---

## By Question

| Question | Read... |
|----------|---------|
| What problem does KGC 4D solve? | Why 4 Dimensions? |
| How does time travel work? | Temporal Reconstruction |
| How do distributed systems stay consistent? | Causality and Vector Clocks |
| Why is Git used for snapshots? | Git as Immutable History |
| How are mutations tracked? | Event Sourcing Architecture |
| How are defects prevented? | FMEA and Mistake-Proofing |

---

## Key Concepts

### Observable State (O)
**What:** RDF triples representing facts
**When:** Current moment in time
**Stored:** Universe named graph

### Nanosecond Time (t_ns)
**What:** Precise timestamps with no floating-point loss
**Why:** Financial transactions, audit compliance, scientific accuracy
**Implementation:** BigInt nanoseconds with monotonic enforcement

### Vector Causality (V)
**What:** Logical clocks tracking which events caused which
**Why:** Ordering events in distributed systems without central clock
**Example:** Event A happened before Event B (provable via clock values)

### Git References (G)
**What:** Snapshots stored as immutable Git commits
**Why:** Cryptographic proof that snapshots haven't changed
**Benefit:** Anyone can verify authenticity without trusting us

---

## Theoretical Foundations

### ACID Properties

KGC 4D provides **strong ACID semantics**:

- **Atomic** - All mutations in an event succeed or all fail
- **Consistent** - Store always in valid RDF state
- **Isolated** - Events don't interfere with each other
- **Durable** - Events persisted in EventLog

### Causality Models

KGC 4D uses **logical clocks** (vector clocks) rather than:

- **Synchronized clocks** - Requires NTP, complex
- **Lamport clocks** - Lose information about concurrent events
- **Hybrid clocks** - More complex than vector clocks for our use case

### Temporal Databases

KGC 4D implements a **temporal database** with:

- **Point-in-time queries** - "What was true at timestamp T?"
- **Range queries** - "What was true between T1 and T2?"
- **Temporal constraints** - "Find all changes since last Tuesday"

---

## Philosophy

### Explicitness Over Implicitness

Every change is recorded as an event. No hidden state.

```javascript
// ✓ Explicit: Every change is an event
await store.appendEvent({ type: 'CREATE' }, mutations);

// ✗ Implicit: Hidden changes
store.addQuad(quad); // What created this quad?
```

### Verification Over Trust

Don't trust us; verify with BLAKE3 hashing and Git.

```javascript
// Anyone can verify snapshot authenticity
const isValid = await verifyReceipt(snapshot, git, store);
// Requires no trust in us, Git, or anyone else
```

### Immutability Over Mutability

History never changes; only append to EventLog.

```javascript
// ✓ Immutable: EventLog is append-only
EventLog ← [Event1, Event2, Event3, ...]
// Can't change or delete past events

// ✗ Mutable: Dangerous
History ← [Event1, Event2, Event3, ...]
// If Event2 changes, what does past reconstruction show?
```

### Deterministic Over Probabilistic

Given the same events, always get identical state.

```javascript
// ✓ Deterministic: Same EventLog = same state
reconstructState(store, git, time) // Always identical result

// ✗ Probabilistic: Order matters
if (event1 && event2) { /* result depends on order */ }
```

---

## Further Reading

Each explanation includes:
- Conceptual diagrams
- Mathematical foundations
- Practical examples
- Comparison with alternatives
- Historical context

Dive in to understand not just *how* to use KGC 4D, but *why* it works the way it does.
