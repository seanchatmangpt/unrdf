# Architecture Overview

## System Design

KGC 4D combines four dimensions:

```
┌─────────────────────────────────────────────┐
│        4D Knowledge Graph (KGC 4D)          │
├─────────────────────────────────────────────┤
│ Observable State (O)                        │
│ ├─ Current RDF triples (Universe graph)     │
│ ├─ Queryable via SPARQL                     │
│ └─ Mutable only via events                  │
├─────────────────────────────────────────────┤
│ Nanosecond Time (t_ns)                      │
│ ├─ BigInt timestamps (nanosecond precision) │
│ ├─ Monotonic enforcement                    │
│ └─ Both Node.js and browser                 │
├─────────────────────────────────────────────┤
│ Vector Causality (V)                        │
│ ├─ Logical clocks for causality             │
│ ├─ Distributed ordering                     │
│ └─ Happens-before relationships             │
├─────────────────────────────────────────────┤
│ Git References (G)                          │
│ ├─ Content-addressed snapshots              │
│ ├─ BLAKE3 cryptographic hashing             │
│ └─ Immutable commit history                 │
└─────────────────────────────────────────────┘
```

## Named Graph Partitioning

Three logical RDF graphs within a single Oxigraph store:

### Universe (`<kgc:Universe>`)

**Current observable state.** Contains all active RDF triples representing the knowledge graph's current state.

```sparql
PREFIX ex: <http://example.org/>
SELECT ?person ?name
WHERE {
  GRAPH <kgc:Universe> {
    ?person ex:name ?name .
  }
}
```

- **Purpose**: Answer "What's true now?"
- **Queryable**: Yes (SPARQL SELECT, CONSTRUCT, etc.)
- **Mutable**: Yes, but only through event appending
- **Retention**: Keeps current state only

### EventLog (`<kgc:EventLog>`)

**Immutable append-only history.** Contains every mutation as an event record.

```sparql
PREFIX kgc: <kgc:>
SELECT ?eventId ?type ?timestamp
WHERE {
  GRAPH <kgc:EventLog> {
    ?eventId kgc:type ?type ;
             kgc:T_NS ?timestamp .
  }
}
```

- **Purpose**: Answer "What changed and when?"
- **Queryable**: Yes (full SPARQL access)
- **Mutable**: No, append-only
- **Retention**: Keeps complete history

### System (`<kgc:System>`)

**Metadata and configuration.** Contains vector clocks, cached snapshots, and system state.

```sparql
PREFIX kgc: <kgc:>
SELECT ?vectorClock
WHERE {
  GRAPH <kgc:System> {
    <kgc:State> kgc:vectorClock ?vectorClock .
  }
}
```

- **Purpose**: Internal state and optimization
- **Queryable**: Yes
- **Mutable**: Yes (system operations)
- **Retention**: Current metadata only

## Data Flow

### Write Path: Appending an Event

```
User calls appendEvent()
         ↓
    Parse mutations
         ↓
   Generate event ID
         ↓
  Get current timestamp (t_ns)
         ↓
   Run poka-yoke guards (24 checks)
         ↓
  Create event record in EventLog
         ↓
   Apply mutations to Universe
         ↓
  Increment vector clock in System
         ↓
   Return receipt with:
   - eventId
   - tNs
   - vectorClock
   - mutations
```

### Read Path: Querying

```
User calls querySync(sparql)
         ↓
Parse SPARQL query
         ↓
  Select graph (Universe/EventLog/System)
         ↓
Execute query via Oxigraph
         ↓
Return result bindings
         ↓
(No side effects, entirely read-only)
```

### Freeze Path: Creating Snapshot

```
User calls freezeUniverse(store, git)
         ↓
Export Universe as N-Quads
         ↓
Canonicalize order (deterministic)
         ↓
Compute BLAKE3 hash
         ↓
Create Git commit with N-Quads
         ↓
Record metadata in System graph
         ↓
Return FrozenSnapshot:
- snapshotId
- gitRef
- hash
- tNs
```

### Time Travel Path: Reconstruction

```
User calls reconstructState(store, git, targetTime)
         ↓
Find nearest snapshot ≤ targetTime
         ↓
Load snapshot from Git
         ↓
Create new KGCStore
         ↓
Load snapshot into new store
         ↓
Replay all events after snapshot
         ↓
Return reconstructed store
         ↓
User queries the historical store
```

## Zero-Information Invariant

**Fundamental principle:** All state at any time is reconstructible from:

```
State(t) = Snapshot(closest t_snap where t_snap ≤ t) +
           Replay(EventLog between t_snap and t)
```

**Implications:**

1. **No state duplication** - Never store state separately
2. **Complete audit trail** - EventLog is the source of truth
3. **Temporal queries** - Can query any point in time
4. **Verification** - Hash of snapshot + Git history proves integrity
5. **Recovery** - Never need special recovery procedures

**Example:**

```javascript
// What was the state on December 1st?
const pastState = reconstructState(store, git, targetDate);

// This is GUARANTEED to be identical to what would have
// been computed on that date because:
// - EventLog is append-only (immutable)
// - Snapshots are cryptographically verified
// - Replay is deterministic
```

## Event Model

### Event Structure

```javascript
{
  type: 'CREATE' | 'UPDATE' | 'DELETE' | 'SNAPSHOT' | 'HOOK_EXECUTION',
  payload: { /* arbitrary JSON metadata */ },
  mutations: [
    { type: 'add', subject, predicate, object },
    { type: 'delete', subject, predicate, object },
  ],
  // Recorded by system:
  eventId: 'uuid',
  tNs: BigInt nanoseconds,
  vectorClock: { nodeId: count, ... },
}
```

### Atomic Guarantees

All mutations in an event succeed or all fail together:

```javascript
// Atomic: Both mutations succeed or both fail
await store.appendEvent(
  { type: 'UPDATE', payload: { description: 'Transfer' } },
  [
    { type: 'delete', subject: alice, predicate: balance, object: '100' },
    { type: 'add', subject: bob, predicate: balance, object: '100' },
  ]
);
// Either both succeed (atomically) or error thrown
```

## Causality Tracking

Vector clocks track causal relationships:

```javascript
// Initial clock: { node1: 0, node2: 0 }

// Node1 appends event
event1.vectorClock = { node1: 1, node2: 0 };

// Node2 appends event (independent)
event2.vectorClock = { node1: 0, node2: 1 };

// Node1 appends after learning about Node2's event
event3.vectorClock = { node1: 2, node2: 1 };

// Now we can determine ordering:
event1.vectorClock.happensBefore(event3) // true
event2.vectorClock.happensBefore(event3) // true
event1.vectorClock.happensBefore(event2) // false (concurrent)
event2.vectorClock.happensBefore(event1) // false (concurrent)
```

## Snapshot Strategy

### When to Snapshot

- **Trade-off**: Frequent snapshots = faster reconstruction but larger storage
- **Typical**: Create snapshots at logical boundaries (daily, after major updates, etc.)
- **No requirement**: Events are authoritative; snapshots are optimization only

### Snapshot Storage

Snapshots stored in Git as:
- **Content**: Canonical N-Quads format
- **Format**: Plain text, version-controlled
- **Verification**: BLAKE3 hash in metadata
- **Accessibility**: Can be read directly from Git

## Performance Characteristics

| Operation | Time Complexity | Space |
|-----------|-----------------|-------|
| appendEvent() | O(m) | m mutations |
| querySync() | O(n) | n matches |
| freezeUniverse() | O(q) | q quads |
| reconstructState() | O(e + q) | e events + q quads |
| verifyReceipt() | O(q) | q quads in snapshot |

Where:
- **m** = mutations per event
- **n** = query result count
- **q** = quads in universe
- **e** = events between snapshot and target time

## Implementation Details

### Event IDs

Generated as UUIDs to ensure uniqueness across distributed systems:

```javascript
eventId = uuid() // e.g., "550e8400-e29b-41d4-a716-446655440000"
```

### Timestamps

BigInt nanoseconds with monotonic enforcement:

```javascript
// Current time never goes backward
const t1 = now(); // e.g., 1701734400000000000n
const t2 = now(); // e.g., 1701734400000000001n (always ≥ t1)
```

### Canonicalization

N-Quads exported in deterministic order:

1. Subjects sorted lexicographically
2. For each subject: predicates sorted
3. For each predicate: objects sorted
4. Results in identical hash for identical data

### Storage

- **Universe**: Active quads in memory (Oxigraph)
- **EventLog**: Persisted in Oxigraph store
- **Snapshots**: Immutable Git commits
- **System**: Metadata in memory

## Consistency Model

**Strong consistency** within a single store:
- Transactions are atomic
- Queries see committed events

**Eventual consistency** across distributed stores:
- Snapshots replicated asynchronously
- Vector clocks establish causality order

## Error Recovery

### Guard Violations

24 poka-yoke guards prevent common mistakes (detailed in Guards reference):

```javascript
try {
  await store.appendEvent(event, mutations);
} catch (error) {
  // Guard caught invalid state or mutation
  // Event was NOT applied
  // Store remains consistent
}
```

### Git Failures

If Git operations fail during freeze:

```javascript
try {
  const frozen = await freezeUniverse(store, git);
} catch (error) {
  // Event log remains intact
  // Retry freeze or use different repository
  // No data loss
}
```

### Time Failures

Monotonic clock enforcement prevents time anomalies:

```javascript
// If system clock moves backward, KGC 4D detects and uses
// last valid timestamp + 1 nanosecond
const t1 = now(); // System time
// System clock resets
const t2 = now(); // Still ≥ t1 (guarded)
```

## Design Principles

1. **Immutability**: EventLog and snapshots never change
2. **Atomicity**: Mutations in an event are all-or-nothing
3. **Verifiability**: BLAKE3 hashing enables cryptographic proof
4. **Isomorphism**: Identical behavior in Node.js and browsers
5. **Explicitness**: No implicit state; everything recorded as events
6. **Simplicity**: Three graphs, deterministic rules, no magic

## Comparison with Other Systems

| Feature | KGC 4D | Event Sourcing | Time-Series DB | Knowledge Graphs |
|---------|--------|----------------|-----------------|-----------------|
| **RDF Triples** | ✓ | ✗ | ✗ | ✓ |
| **Temporal Queries** | ✓ | ✓ | ✓ | Limited |
| **Immutable History** | ✓ | ✓ | ✓ | ✗ |
| **Cryptographic Proof** | ✓ (Git+BLAKE3) | ✗ | ✗ | ✗ |
| **Causality Tracking** | ✓ (VectorClock) | Limited | ✗ | ✗ |
| **Browser Support** | ✓ | No | No | Limited |
