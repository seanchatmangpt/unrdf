# API Reference

Complete reference for all public APIs in KGC 4D.

## Classes

### KGCStore

Extended RDF store with event sourcing and atomic operations.

#### Constructor

```javascript
const store = new KGCStore();
```

Properties:
- `UNIVERSE` - Named node for the Universe graph
- `EVENT_LOG` - Named node for the EventLog graph
- `SYSTEM` - Named node for the System graph

#### Methods

##### appendEvent(eventDesc, mutations)

Atomically append an event with mutations to the store.

```javascript
const receipt = await store.appendEvent(
  {
    type: 'CREATE', // 'CREATE' | 'UPDATE' | 'DELETE' | 'SNAPSHOT' | 'HOOK_EXECUTION'
    payload: { description: 'User action', userId: '123' }, // arbitrary JSON
  },
  [
    { type: 'add', subject, predicate, object },
    { type: 'delete', subject, predicate, object },
  ]
);

// Returns:
// {
//   eventId: '...uuid...',
//   tNs: BigInt nanoseconds,
//   vectorClock: { ... },
//   mutations: [...],
// }
```

**Parameters:**
- `eventDesc.type` - Event type constant
- `eventDesc.payload` - Arbitrary metadata object (will be JSON stringified)
- `mutations` - Array of mutation objects
  - `mutation.type` - 'add' or 'delete'
  - `mutation.subject` - RDF NamedNode
  - `mutation.predicate` - RDF NamedNode
  - `mutation.object` - RDF Term (NamedNode, Literal, etc.)

**Returns:** Receipt object with event metadata

**Throws:** On guard violations or invalid input

##### querySync(sparql)

Execute SPARQL query synchronously (read-only).

```javascript
const results = store.querySync(`
  PREFIX ex: <http://example.org/>
  SELECT ?person ?name
  WHERE {
    GRAPH <kgc:Universe> {
      ?person a ex:Person ;
              ex:name ?name .
    }
  }
`);

// Returns array of bindings
// Each binding is a Map-like object
results.forEach(binding => {
  console.log(binding.get('person'));
  console.log(binding.get('name'));
});
```

**Parameters:**
- `sparql` - SPARQL query string (SELECT, ASK, CONSTRUCT, DESCRIBE)

**Returns:** Array of result bindings (for SELECT), boolean (for ASK), or quads (for CONSTRUCT/DESCRIBE)

**Notes:**
- Only reads from store, doesn't modify
- Queries can target UNIVERSE, EVENT_LOG, or SYSTEM graphs
- Use `<kgc:Universe>`, `<kgc:EventLog>`, `<kgc:System>` graph names

##### addQuad(quad)

Add a single quad directly (not recommended—use appendEvent for atomicity).

```javascript
const quad = dataFactory.quad(
  subject,
  predicate,
  object,
  store.UNIVERSE
);

store.addQuad(quad);
```

**Parameters:**
- `quad` - RDF Quad object

##### getQuads(pattern)

Get quads matching a pattern.

```javascript
const quads = store.getQuads(
  subject,     // or null for any
  predicate,   // or null for any
  object,      // or null for any
  graph        // or null for any
);

quads.forEach(quad => {
  console.log(quad.subject, quad.predicate, quad.object);
});
```

**Returns:** Array of matching quads

---

### GitBackbone

Pure JavaScript Git operations for snapshot storage.

#### Constructor

```javascript
const git = new GitBackbone('./repo-path');
```

**Parameters:**
- `path` - Repository path (Node.js) or name (browser)

#### Methods

##### commitSnapshot(nquads, message)

Create a Git commit with N-Quads content.

```javascript
const ref = await git.commitSnapshot(nquadsString, 'Monthly snapshot');

// Returns: Git commit reference (hash)
console.log('Snapshot committed:', ref);
```

**Parameters:**
- `nquads` - N-Quads formatted string
- `message` - Commit message

**Returns:** Git commit reference

##### readSnapshot(ref)

Read snapshot N-Quads from Git.

```javascript
const nquads = await git.readSnapshot('abc123...');

console.log(nquads);
// <http://example.org/alice> <http://...> "Alice" <kgc:Universe> .
```

**Parameters:**
- `ref` - Git commit reference

**Returns:** N-Quads string

##### getCommitTime(ref)

Get the commit timestamp.

```javascript
const timestamp = await git.getCommitTime('abc123...');
```

**Returns:** Commit timestamp (ISO string)

---

### VectorClock

Logical clock for tracking causality in distributed systems.

#### Constructor

```javascript
const vc = new VectorClock({ node1: 1, node2: 2 });
```

**Parameters:**
- Object with node IDs as keys and clock values as integers

#### Methods

##### increment(nodeId)

Increment the clock for a node.

```javascript
vc.increment('node1');
// Now: { node1: 2, node2: 2 }
```

##### merge(other)

Merge two vector clocks.

```javascript
const vc1 = new VectorClock({ a: 1, b: 2 });
const vc2 = new VectorClock({ a: 2, b: 1 });

vc1.merge(vc2);
// Result: { a: 2, b: 2 }
```

##### happensBefore(other)

Check if this clock happened before another.

```javascript
const vc1 = new VectorClock({ a: 1, b: 1 });
const vc2 = new VectorClock({ a: 2, b: 2 });

console.log(vc1.happensBefore(vc2)); // true
```

**Returns:** boolean

---

## Functions

### freezeUniverse(store, git)

Create a deterministic snapshot and store in Git.

```javascript
const frozen = await freezeUniverse(store, git);

// Returns:
// {
//   snapshotId: 'uuid...',
//   gitRef: 'abc123...',
//   hash: 'BLAKE3 hash...',
//   tNs: BigInt nanoseconds,
// }
```

**Parameters:**
- `store` - KGCStore instance
- `git` - GitBackbone instance

**Returns:** Frozen snapshot object with:
- `snapshotId` - Unique snapshot identifier
- `gitRef` - Git commit reference
- `hash` - BLAKE3 hash of N-Quads
- `tNs` - Snapshot timestamp

**Process:**
1. Export Universe graph to N-Quads (canonical order)
2. Compute BLAKE3 hash
3. Create Git commit with N-Quads
4. Record metadata in System graph

### reconstructState(store, git, targetTime)

Time-travel: reconstruct state at a specific point.

```javascript
const pastStore = await reconstructState(store, git, targetTimestamp);

// pastStore is a new KGCStore with historical state
const results = pastStore.querySync(sparqlQuery);
```

**Parameters:**
- `store` - Current KGCStore
- `git` - GitBackbone instance
- `targetTime` - BigInt nanoseconds

**Returns:** New KGCStore with state at targetTime

**Algorithm:**
1. Find nearest snapshot before targetTime
2. Load snapshot from Git
3. Replay all events between snapshot and targetTime
4. Return reconstructed store

### verifyReceipt(frozen, git, store)

Cryptographically verify a snapshot's integrity.

```javascript
const isValid = await verifyReceipt(frozen, git, store);

if (isValid) {
  console.log('✓ Snapshot is authentic');
} else {
  console.log('✗ Snapshot has been tampered with');
}
```

**Parameters:**
- `frozen` - Frozen snapshot object
- `git` - GitBackbone instance
- `store` - KGCStore (for context)

**Returns:** boolean

**Process:**
1. Fetch snapshot from Git using gitRef
2. Recompute BLAKE3 hash
3. Compare with stored hash
4. Return true if match, false if mismatch

### now()

Get current time as BigInt nanoseconds.

```javascript
const timestamp = now();
// BigInt: 1701734400000000000n

// Works in both Node.js and browser
// Node.js: process.hrtime.bigint()
// Browser: performance.now() * 1_000_000n
```

**Returns:** BigInt nanoseconds

### toISO(bigIntNs)

Convert BigInt nanoseconds to ISO 8601 string.

```javascript
const isoString = toISO(BigInt('1701734400000000000'));
// '2023-12-05T00:00:00.000000000Z'
```

**Parameters:**
- `bigIntNs` - BigInt nanoseconds

**Returns:** ISO 8601 string with nanosecond precision

### fromISO(isoString)

Convert ISO 8601 string to BigInt nanoseconds.

```javascript
const ns = fromISO('2023-12-05T00:00:00.000000000Z');
// BigInt: 1701734400000000000n
```

**Parameters:**
- `isoString` - ISO 8601 string

**Returns:** BigInt nanoseconds

### addNanoseconds(time, ns)

Add nanoseconds to a timestamp.

```javascript
const oneHourLater = addNanoseconds(timestamp, BigInt(3_600_000_000_000));
```

**Parameters:**
- `time` - BigInt nanoseconds
- `ns` - BigInt nanoseconds to add

**Returns:** BigInt result

### duration(start, end)

Calculate duration between two timestamps.

```javascript
const elapsed = duration(startTime, endTime);
// Returns BigInt nanoseconds
```

**Parameters:**
- `start` - BigInt nanoseconds
- `end` - BigInt nanoseconds

**Returns:** BigInt nanoseconds (end - start)

---

## Constants

### GRAPHS

Named graph URIs:

```javascript
import { GRAPHS } from '@unrdf/kgc-4d';

GRAPHS.UNIVERSE   // <kgc:Universe>
GRAPHS.EVENT_LOG  // <kgc:EventLog>
GRAPHS.SYSTEM     // <kgc:System>
```

### EVENT_TYPES

Event type constants:

```javascript
import { EVENT_TYPES } from '@unrdf/kgc-4d';

EVENT_TYPES.CREATE           // 'CREATE'
EVENT_TYPES.UPDATE           // 'UPDATE'
EVENT_TYPES.DELETE           // 'DELETE'
EVENT_TYPES.SNAPSHOT         // 'SNAPSHOT'
EVENT_TYPES.HOOK_EXECUTION   // 'HOOK_EXECUTION'
```

### PREDICATES

RDF predicate URIs:

```javascript
import { PREDICATES } from '@unrdf/kgc-4d';

PREDICATES.T_NS              // Timestamp predicate
PREDICATES.TYPE              // Event type
PREDICATES.GIT_REF           // Git reference
PREDICATES.PAYLOAD           // Event payload
PREDICATES.VECTOR_CLOCK      // Vector clock
```

---

## Type Definitions

### EventDescription

```typescript
type EventDescription = {
  type: 'CREATE' | 'UPDATE' | 'DELETE' | 'SNAPSHOT' | 'HOOK_EXECUTION',
  payload?: Record<string, unknown>,
};
```

### Mutation

```typescript
type Mutation = {
  type: 'add' | 'delete',
  subject: NamedNode,
  predicate: NamedNode,
  object: Term,
};
```

### Receipt

```typescript
type Receipt = {
  eventId: string,
  tNs: bigint,
  vectorClock: Record<string, number>,
  mutations: Mutation[],
};
```

### FrozenSnapshot

```typescript
type FrozenSnapshot = {
  snapshotId: string,
  gitRef: string,
  hash: string,
  tNs: bigint,
};
```

---

## Error Handling

KGC 4D throws errors for:

- **Guard violations** - Poka-yoke mistakes detected
- **Invalid input** - Type or structure errors
- **Git errors** - Repository issues
- **Query errors** - SPARQL syntax or execution
- **Time errors** - Monotonic clock violations

Example:

```javascript
try {
  await store.appendEvent(event, mutations);
} catch (error) {
  if (error.message.includes('Guard')) {
    console.error('Poka-yoke guard caught mistake:', error);
  } else {
    console.error('Unexpected error:', error);
  }
}
```
