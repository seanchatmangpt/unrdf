# References

Reference documentation provides **information-oriented** details about KGC 4D's API, architecture, and systems.

## Available References

1. **[API Reference](./01-api.md)**
   - Complete API for KGCStore, GitBackbone, VectorClock
   - All public functions and classes
   - Parameter types and return values
   - Usage examples for each API

2. **[Architecture Overview](./02-architecture.md)**
   - System design and named graphs
   - Data flow and event processing
   - Zero-information invariant explained
   - Core design principles

3. **[Poka-Yoke Guards](./03-guards.md)**
   - 24 mistake-proofing mechanisms
   - FMEA analysis for each guard
   - Guard groupings by subsystem
   - How guards prevent defects

4. **[Constants](./04-constants.md)**
   - RDF graph URIs
   - Event type constants
   - Predicate definitions
   - System namespaces

---

## Quick API Lookup

| Class | Purpose |
|-------|---------|
| **KGCStore** | Extended RDF store with events and atomic operations |
| **GitBackbone** | Pure JS Git for snapshot storage and verification |
| **VectorClock** | Logical clock for causality tracking |

| Function | Purpose |
|----------|---------|
| **freezeUniverse()** | Create snapshot and store in Git |
| **reconstructState()** | Time-travel to historical state |
| **verifyReceipt()** | Cryptographically verify snapshot |
| **appendEvent()** | Atomically append event with mutations |

| Constant | Value |
|----------|-------|
| **GRAPHS.UNIVERSE** | `<kgc:Universe>` |
| **GRAPHS.EVENT_LOG** | `<kgc:EventLog>` |
| **GRAPHS.SYSTEM** | `<kgc:System>` |

---

## By Use Case

| I need to... | Read... |
|--------------|---------|
| Append data | API Reference - appendEvent() |
| Freeze state | API Reference - freezeUniverse() |
| Time travel | API Reference - reconstructState() |
| Verify data | API Reference - verifyReceipt() |
| Query events | API Reference - querySync() |
| Understand design | Architecture Overview |
| Find a constant | Constants reference |
| Learn about guards | Poka-Yoke Guards |

---

## API Methods Quick Start

```javascript
// Create store
const store = new KGCStore();

// Append event atomically
const receipt = await store.appendEvent(
  { type: 'CREATE', payload: { ... } },
  mutations
);

// Query current state
const results = store.querySync(sparqlQuery);

// Create snapshot
const frozen = await freezeUniverse(store, git);

// Travel back in time
const pastStore = await reconstructState(store, git, timestamp);

// Verify integrity
const isValid = await verifyReceipt(frozen, git, store);
```

---

## Named Graphs (The Big Three)

KGC 4D uses three named RDF graphs:

| Graph | Purpose | Queryable | Mutable |
|-------|---------|-----------|---------|
| **Universe** | Current observable state | ✓ | ✓ (via events) |
| **EventLog** | Immutable history | ✓ | ✗ (append-only) |
| **System** | Metadata and clocks | ✓ | ✓ |

---

## Error Handling

```javascript
try {
  await store.appendEvent(event, mutations);
} catch (error) {
  if (error.message.includes('Guard violation')) {
    console.log('Invalid operation detected:', error.message);
  }
}
```

Common errors documented in guards reference.
