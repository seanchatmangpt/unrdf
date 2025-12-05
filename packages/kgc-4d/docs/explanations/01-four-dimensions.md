# Why 4 Dimensions?

KGC 4D is "four-dimensional" because it tracks state along four axes. This section explains why all four dimensions are necessary and how they work together.

## The Four Dimensions

### 1. Observable State (O) - The Data

**Dimension:** What facts are true?

Observable state consists of RDF triples representing the current knowledge graph:

```
(subject, predicate, object)
("Alice", "hasName", "Alicia")
("Alice", "hasAge", "30")
("Bob", "knows", "Alice")
```

**Stored in:** Universe named graph

**Characteristics:**
- Mutable (can add/remove triples)
- Current only (no history)
- Queryable via SPARQL
- Single version

**Why RDF?**
- Graphs are universally understood
- Machines and humans can read the same format
- Standardized query language (SPARQL)
- Extensible to any domain

**Problem it solves:**
"What's true about my data *right now*?"

---

### 2. Nanosecond Time (t_ns) - When It Happened

**Dimension:** When did each fact become true?

Every event has a precise timestamp:

```
Event 1: t_ns = 1701734400000000000 (Dec 5, 2023, 00:00:00.000000000)
         "Alice created"
Event 2: t_ns = 1701734400000000100 (100 nanoseconds later)
         "Alice's age set to 30"
Event 3: t_ns = 1701734400000000200
         "Bob created"
```

**Why nanoseconds?**

| Granularity | Worst Case | Use Case |
|-------------|-----------|----------|
| Seconds | 2 events at 1:00:59 | Basic logging |
| Milliseconds | 2 events at 1:00:59.999 | Most applications |
| Microseconds | Fast algorithms | Microsecond timing |
| **Nanoseconds** | **Sub-microsecond events** | **Financial trades, atomic operations** |

Most databases use milliseconds (IEEE 754 floats), losing precision:

```javascript
// Milliseconds (float-based) - loses precision
const ts1 = 1701734400000.5;  // Can't represent
const ts2 = 1701734400000.9;  // Loses digits

// Nanoseconds (BigInt) - perfect precision
const ts1 = 1701734400000000500n;  // Exact
const ts2 = 1701734400000000900n;  // Exact
```

**Why monotonic?**
Time never goes backward. If system clock resets, KGC 4D uses the last valid timestamp + 1 nanosecond. This ensures causal ordering:

```
Event 1: t_ns = 1000n
Event 2: t_ns = 1001n (always > previous)
Event 3: t_ns = 1002n (even if system clock resets)
```

**Why BigInt?**
- No floating-point rounding errors
- Exact to the nanosecond
- Supports dates from year -271,821 to +275,760 (beyond human history)
- Native JavaScript support (native in Node.js, browsers)

**Problem it solves:**
"What was true on December 1st at 3:45:12.123456789?"

---

### 3. Vector Causality (V) - Why It Happened

**Dimension:** Which events caused which other events?

In a distributed system, you can't use a single global clock. Instead, use **vector clocks** that track causal relationships:

```
Node1: Event1 (vector clock: {n1: 1, n2: 0})
         ↓ tells Node2
Node2: Event2 (vector clock: {n1: 1, n2: 1})  ← caused by Event1
         ↓ tells Node1
Node1: Event3 (vector clock: {n1: 2, n2: 1})  ← caused by Event2
```

**Happened-Before Relationship:**
Event A happened before Event B if and only if Event A's vector clock is strictly less than Event B's:

```
vc(A) < vc(B) ⟺ A happened before B
{1, 0} < {1, 1} ✓ (A happened before B)
{1, 0} < {0, 1} ✗ (A and B are concurrent)
```

**Why not just use timestamps?**

Scenario: Two nodes, no network connection:

```
Node1 @ time T1: Event A created
Node2 @ time T2: Event B created (T2 > T1 due to clock skew)

Question: Did A cause B, or are they concurrent?
Answer: Timestamps alone can't tell!
        - If nodes are connected: maybe A caused B
        - If nodes are isolated: they're concurrent

Vector clocks answer this definitively.
```

**Why vectors?**
Vector clocks use one counter per node:

```
Single counter (Lamport clock):
  Node1: L=5
  Node2: L=5
  Problem: Can't tell if events are causal or concurrent

Vector clock:
  Node1: {n1: 5, n2: 3}  ← Node1 is at 5, knows about Node2 at 3
  Node2: {n1: 3, n2: 5}  ← Node2 is at 5, knows about Node1 at 3
  Solution: Can determine exact causal ordering
```

**Problem it solves:**
"In a distributed system with multiple nodes, which events depend on which?"

**Real-world example:**

```
Alice's account balance: $100
Server1 receives: "Send $50 to Bob"  (Event A)
Server2 receives: "Send $50 to Carol" (Event B)

Timestamps say: A and B happened simultaneously
Vector clocks: Also show A and B are concurrent

Result: BOTH transfers are attempted
       $100 - $50 - $50 = -$100 ✗ OVERDRAFT

This is why vector clocks matter for financial systems.
```

---

### 4. Git References (G) - Cryptographic Proof

**Dimension:** How do we prove nothing was tampered with?

Snapshots are stored as Git commits with BLAKE3 hashing:

```
Universe State at Time T:
  100,000 RDF triples
         ↓
  Export to canonical N-Quads
         ↓
  Compute BLAKE3 hash
  hash = "a1b2c3d4e5f6..."
         ↓
  Create Git commit
  commit = abc123def456...
         ↓
  Store in Git repository
```

**Why Git?**

1. **Immutable commits** - Once created, can't change history
2. **Content-addressed** - Commit hash identifies content exactly
3. **Decentralized** - Can verify without contacting original creator
4. **Auditable** - Complete chain of commits visible
5. **Distributed** - Clone repository to any number of places

**Why BLAKE3?**

```
MD5: Broken (can forge collisions)
SHA1: Weakened (feasible collision attacks)
SHA-256: Secure but slow
BLAKE3: Secure AND fast
        - 3x faster than SHA-256 in software
        - Cryptographically sound
        - Parallelizable
```

**Verification Process:**

```
Frozen snapshot receipt contains:
- gitRef: abc123... (Git commit)
- hash: a1b2c3d4... (BLAKE3)

To verify:
1. Fetch commit from Git
2. Recompute BLAKE3 hash
3. Compare: computed == stored?
4. If match: snapshot is authentic
5. If mismatch: snapshot was tampered with

All without trusting anyone else!
```

**Why not database checksum?**

```
Database checksum:
  hash = db.computeChecksum()
  Problem: DB can be modified AND checksum updated secretly
           No external verification possible

Git + BLAKE3:
  hash = blake3(snapshot)
  Git commits can be verified on any machine
  Problem: Impossible to fake without access to private key
           (if using signed commits)

Result: Git provides EXTERNAL verification
```

**Problem it solves:**
"How do we prove a snapshot hasn't been modified since we created it?"

---

## How the Dimensions Work Together

### A Concrete Example

Let's trace through a financial transaction:

```
Alice sends $50 to Bob

Step 1: Observable State (O)
        "Alice's balance" = $100
        "Bob's balance" = $0

Step 2: Nanosecond Time (t_ns)
        Event timestamp: 2023-12-05 14:30:00.000000000

Step 3: Vector Causality (V)
        Vector clock: {alice_node: 5, bob_node: 3}
        (Alice at event 5, knows about Bob at event 3)

Step 4: Git Reference (G)
        1. State before transfer frozen to Git
           Git commit: abc123... (BLAKE3: x1y2z3...)

        2. Append transfer event

        3. State after transfer frozen to Git
           Git commit: def456... (BLAKE3: a9b8c7...)
```

**Later, someone asks: "Was Alice's account really at $50 after the transfer?"**

Answer using all 4 dimensions:

1. **Observable State** - Current state shows $50
2. **Nanosecond Time** - At exactly 2023-12-05 14:30:00.000000000
3. **Vector Causality** - After all events that causally preceded it
4. **Git Reference** - Verified via BLAKE3 hash in commit def456...

**Proof is cryptographic and unchallengeable.**

---

## Why Not Just 1D or 2D?

### Just Observable State (1D)

```javascript
// Only current data
store.addQuad(quad);

Problem: No history
  - Can't ask "What was the balance yesterday?"
  - Can't detect what changed
  - No audit trail
```

### Observable State + Time (2D)

```javascript
// Current data + timestamps
event.timestamp = now();
store.addQuad(quad);

Problem: No causality
  - In distributed system, timestamp alone doesn't prove ordering
  - Concurrent events look ordered by clock skew
  - Clock resets break everything
```

### Observable State + Time + Causality (3D)

```javascript
// Current data + timestamps + vector clocks
event.timestamp = now();
event.vectorClock = {...};
store.addQuad(quad);

Problem: No external proof
  - Anyone could claim "I have the right history"
  - Can't verify without trusting us
  - Snapshots could be modified after the fact
```

### All 4 Dimensions (4D) ✓

```javascript
// Observable state + time + causality + Git verification
event.timestamp = now();
event.vectorClock = {...};
snapshot.gitRef = commitHash;
snapshot.hash = blake3(nquads);

Result:
  - Know current state ✓
  - Know exactly when it happened ✓
  - Know causality across systems ✓
  - Can prove it wasn't tampered with ✓
```

---

## Mathematical Formalism

### The 4D State Space

KGC 4D state can be represented as:

```
State(t) = (O, t_ns, V, G)

Where:
  O = Observable state (RDF triples)
  t_ns = Nanosecond timestamp
  V = Vector clock
  G = Git references

Invariant: State is fully reconstructible from (t_ns, V) + EventLog + Git
```

### Reconstruction Formula

```
State(target_time) =
  Snapshot(latest_s where s.t_ns ≤ target_time)
  + Replay(EventLog[s.t_ns .. target_time])
```

This is **deterministic** - same inputs always produce same output.

---

## Comparison with Other Systems

| System | O | t_ns | V | G |
|--------|---|------|---|---|
| Relational DB | ✓ | ✗ | ✗ | ✗ |
| Temporal DB | ✓ | ✓ | ✗ | ✗ |
| Event Sourcing | ✓ | ✓ | Partial | ✗ |
| Blockchains | ✓ | ✓ | ✓ | ✓ (cryptographic) |
| **KGC 4D** | ✓ | ✓ | ✓ | ✓ (Git-based) |

KGC 4D combines all four at scale without blockchain overhead.

---

## Summary

Each dimension solves a specific problem:

| Dimension | Problem | Solution |
|-----------|---------|----------|
| **Observable (O)** | What's true? | RDF triples |
| **Temporal (t_ns)** | When was it true? | Nanosecond timestamps |
| **Causal (V)** | Why is it in this order? | Vector clocks |
| **Cryptographic (G)** | Can we prove it? | Git + BLAKE3 |

Together, they form a **complete, verifiable, temporal knowledge graph** that answers:
- **What?** (observable)
- **When?** (temporal)
- **Why?** (causal)
- **Prove it!** (cryptographic)
