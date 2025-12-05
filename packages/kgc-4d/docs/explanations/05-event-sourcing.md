# Event Sourcing Architecture

This explanation covers event sourcing—the architectural pattern at the core of KGC 4D.

## What is Event Sourcing?

Event sourcing is an architectural pattern where:

```
Traditional approach:
  Store current state
  Update state in place
  Discard old values

Event sourcing approach:
  Store all events (changes)
  Derive current state from events
  Keep complete history
```

### Example

```
Traditional:
  Users table:
    Alice: balance = $50

Event sourcing:
  Event log:
    1. CREATE Alice
    2. SET balance = $100
    3. SET balance = $50

  Current state = Replay events
               = Alice: balance = $50

  Historical state = Replay events 1-2
                   = Alice: balance = $100
```

## The Event Log

### Structure

Each event contains:

```
{
  eventId: "uuid",           // Unique identifier
  type: "CREATE|UPDATE|DELETE|SNAPSHOT|HOOK_EXECUTION",
  tNs: BigInt,               // Timestamp (nanoseconds)
  payload: { ... },          // Metadata (user who made change, etc.)
  vectorClock: { ... },      // Causality tracking
  mutations: [               // Actual changes
    { type: "add", subject, predicate, object },
    { type: "delete", subject, predicate, object },
  ]
}
```

### Properties

```
Append-only:
  Events can be added, never deleted or modified
  Once recorded, history is immutable

Ordered:
  By timestamp (t_ns)
  Monotonically increasing

Complete:
  Every change is recorded
  No hidden modifications

Deterministic:
  Same events in same order always produce same state
```

## State Derivation

### The Formula

```
State(t) = Replay(EventLog[0 .. t])

Where:
  EventLog[0 .. t] = all events with timestamp ≤ t
  Replay = apply all mutations in order

Result: state at time t (deterministic)
```

### Example

```
Timeline:
  t=1ns: Event1 (CREATE Alice, SET age=30)
  t=2ns: Event2 (SET age=31)
  t=3ns: Event3 (DELETE Alice)
  t=4ns: Event4 (CREATE Alice, SET age=25)

Current state (t=4ns):
  Replay all events: CREATE → SET age=30 → SET age=31 → DELETE → CREATE → SET age=25
  Result: Alice exists, age=25 ✓

State at t=2ns:
  Replay events 1-2: CREATE → SET age=30 → SET age=31
  Result: Alice exists, age=31 ✓

State at t=3ns:
  Replay events 1-3: CREATE → SET age=30 → SET age=31 → DELETE
  Result: Alice doesn't exist ✓
```

## Benefits Over Traditional Approach

### Complete Audit Trail

```
Traditional:
  User changes age from 30 to 31
  Old database: age = 30 (lost)
  New database: age = 31

  Question: Who changed it? When? Why?
  Answer: Unknown (no history)

Event sourcing:
  EventLog:
    Event_5: type=UPDATE, tNs=..., userId="alice", description="Birthday"
            mutation: {delete: age=30, add: age=31}

  Answer: Alice changed it at this exact time, reason: Birthday
```

### Time Travel

```
Traditional:
  Want to know state at January 1st?
  Answer: Can't (only have current state)
  Option: Restore from backup (if exists, if has correct date)

Event sourcing:
  Want to know state at January 1st?
  Answer: Replay events up to January 1st
  Works for ANY point in time
```

### Debugging

```
Traditional:
  User reports: "My balance seems wrong"
  Investigation: Check current balance in database
  Problem: No way to see what happened

Event sourcing:
  User reports: "My balance seems wrong"
  Investigation: Get full event history
  See: CREATE → ADD $100 → TRANSFER $50 → TRANSFER $30
  Can verify exactly what happened and when
```

### Consistency

```
Traditional:
  Database state != Audit log
  Inconsistency possible if they diverge

Event sourcing:
  State is derived from events
  No possibility of divergence
  Events are the source of truth
```

## Atomic Transactions

In KGC 4D, events provide ACID semantics:

```
Atomic:
  All mutations in an event succeed or all fail
  No partial updates

Consistent:
  Store always in valid RDF state
  All quads follow RDF rules

Isolated:
  Events don't interfere with each other
  No race conditions

Durable:
  Events persisted in EventLog
  Survive system failures
```

### Example: Financial Transfer

```javascript
// Traditional approach (risky):
balance_alice -= 50;  // What if crash here?
balance_bob += 50;    // Never executes
// Result: Lost $50!

// Event sourcing (safe):
await store.appendEvent(
  { type: 'UPDATE', payload: { reason: 'transfer' } },
  [
    { type: 'delete', subject: alice, predicate: balance, object: '100' },
    { type: 'delete', subject: bob, predicate: balance, object: '0' },
    { type: 'add', subject: alice, predicate: balance, object: '50' },
    { type: 'add', subject: bob, predicate: balance, object: '50' },
  ]
);
// Either ALL mutations succeed or event is rejected
// No possibility of partial update
```

## Comparison with CQRS

### CQRS Pattern

```
CQRS = Command Query Responsibility Segregation

Command side (write):
  Handle state changes
  Generate events

Query side (read):
  Build read-optimized views
  Subscribe to events

Example:
  Command: "Increase balance by $50"
  → Event: {type: 'increase_balance', amount: 50}
  → Query reads event and updates read view
```

### KGC 4D vs CQRS

```
CQRS:
  - Command and Query sides separated
  - Potential eventual consistency issues
  - Requires synchronization between sides

KGC 4D:
  - Single command interface (appendEvent)
  - Single query interface (querySync)
  - Always consistent (query reads from store)
  - Simpler (less infrastructure)

Result: KGC 4D uses event sourcing but not full CQRS
        Simpler model without sacrificing auditability
```

## Idempotency

Events can be replayed safely:

```
Event: {type: 'SET', predicate: 'age', value: '30'}

Replay 1: Alice age = 30
Replay 2: Alice age = 30
Replay 3: Alice age = 30

Result is always the same (idempotent)

This is why time travel works!
```

## Event Sourcing Challenges

### Storage Growth

```
Problem: EventLog grows without bound
         Every change becomes an event
         Over years, millions of events

Solution:
  Snapshots: Periodically freeze state, use as starting point
           Replay only events after snapshot

  Pruning: Delete very old events (if compliant with retention)
          Keep snapshots for archival
```

### Complex Queries

```
Traditional:
  SELECT AVG(age) FROM users;
  Direct computation on current state

Event sourcing:
  No direct query on events
  Must replay events to compute
  Slower for complex aggregations

Solution:
  Materialized views: Build read-optimized tables
                     Subscribe to events
                     Update views incrementally

  (KGC 4D: Use SPARQL for queries, handled by Oxigraph)
```

### Event Versioning

```
Problem: Old events use old schema
         Can't replay if schema changed

Example:
  Old event: {type: 'SET_AGE', value_decades: 3}
  New schema: {type: 'SET_AGE', value_years: 30}

Solution:
  1. Version events: {version: 2, type: 'SET_AGE', ...}
  2. Replay handles version conversion
  3. Gradual migration to new schema

(KGC 4D: RDF is schema-flexible, handles this naturally)
```

## Event Sourcing in Distributed Systems

### Benefits

```
Multiple nodes:
  Each can have full EventLog
  Each can replay to compute current state
  Natural replication (send events, not state)

Offline operation:
  Accumulate events locally
  Sync when connection available
  No conflicts if events are commutative

Causality:
  Vector clocks in events track dependencies
  Can determine ordering in distributed system
```

### Challenges

```
Event ordering:
  What if two nodes create events simultaneously?
  Vector clocks answer: they're concurrent
  Need conflict resolution strategy

Consistency:
  All nodes will eventually see same events
  But might see them in different order initially
  Eventual consistency model
```

## Event Sourcing Best Practices

### 1. Make Events Immutable

```javascript
// ✓ Good
await store.appendEvent({
  type: 'CREATE',
  payload: { ... }
}, mutations);
// Can't modify after creation

// ✗ Bad
event.mutation.push(newMutation);  // Modifying events!
```

### 2. Include Context in Payload

```javascript
// ✓ Good
{
  type: 'UPDATE',
  payload: {
    userId: 'alice@example.com',
    reason: 'user-edit',
    source: 'web-api',
    timestamp_iso: '2023-12-05T14:30:00Z'
  }
}

// ✗ Minimal
{
  type: 'UPDATE',
  payload: {}
}
```

### 3. Keep Events Granular

```javascript
// ✓ Good (each fact separately)
[
  { type: 'add', subject: alice, predicate: name, object: 'Alice' },
  { type: 'add', subject: alice, predicate: age, object: '30' },
  { type: 'add', subject: alice, predicate: city, object: 'NYC' },
]

// ✗ Coarse (bundled)
[
  { type: 'add', subject: alice, predicate: metadata, object: 'name=Alice&age=30&city=NYC' }
]
// Hard to query and reason about
```

### 4. Avoid Commands in Events

```javascript
// ✓ Good (what happened)
{
  type: 'UPDATE',
  payload: { field: 'balance' },
  mutations: [
    { type: 'delete', object: '100' },
    { type: 'add', object: '150' }
  ]
}

// ✗ Bad (how it should happen)
{
  type: 'COMMAND',
  command: 'transfer',
  from: 'alice',
  to: 'bob',
  amount: 50
}
```

## Summary

Event sourcing is an architectural pattern where:
- All changes are recorded as immutable events
- Current state is derived from events (not stored separately)
- Complete audit trail is maintained automatically
- Time travel queries become possible
- Distributed systems can naturally replicate

KGC 4D is built on event sourcing principles, combined with:
- RDF for flexible data representation
- Nanosecond timestamps for precision
- Vector clocks for causality tracking
- Git for immutable snapshots
