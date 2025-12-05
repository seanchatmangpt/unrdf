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

Logical clock for tracking causality in distributed systems. Each node maintains a counter that increments on local events. On receiving a message from another node, the clock merges (takes max of each component) and increments the local counter.

#### Constructor

```javascript
const vc = new VectorClock('node-1');

console.log(vc.nodeId);     // 'node-1'
console.log(vc.counters);   // Map { 'node-1' => 0n }
```

**Parameters:**
- `nodeId` (string) - Unique identifier for this node

**Throws:**
- `TypeError` if nodeId is not a non-empty string

#### increment()

Increment the local counter on a local event. Each local operation increments this node's logical clock value.

```javascript
const vc = new VectorClock('alice');

vc.increment();
console.log(vc.counters.get('alice'));  // 1n

vc.increment();
console.log(vc.counters.get('alice'));  // 2n

// Returns this for chaining
vc.increment().increment().increment();
console.log(vc.counters.get('alice'));  // 5n
```

**Returns:** `this` (for method chaining)

#### merge(other)

Merge with a clock from another node. Takes the maximum of each component and increments local counter.

```javascript
const vc1 = new VectorClock('node-a');
vc1.increment();  // { node-a: 1n }

const vc2 = new VectorClock('node-b');
vc2.increment();
vc2.increment();  // { node-b: 2n }

vc1.merge(vc2);
// Result: { node-a: 1n, node-b: 2n, node-a: 2n } (merged and local incremented)
console.log(vc1.counters.get('node-a'));  // 2n (was 1, merged with nothing, then +1)
console.log(vc1.counters.get('node-b'));  // 2n (merged from other)
```

**Parameters:**
- `other` (VectorClock) - Another vector clock to merge

**Returns:** `this` (for method chaining)

**Throws:**
- `TypeError` if other is not a VectorClock instance

#### compare(other)

Compare this clock with another for causal ordering. Determines the happened-before relationship.

```javascript
const vc1 = new VectorClock('alice');
vc1.increment();           // { alice: 1n }

const vc2 = new VectorClock('bob');
vc2.increment();
vc2.increment();           // { bob: 2n }

// Merge to establish causality
vc1.merge(vc2);            // { alice: 2n, bob: 2n }

const vc3 = new VectorClock('alice');
vc3.increment();
vc3.increment();
vc3.increment();           // { alice: 3n }
vc3.merge(vc1);            // { alice: 3n, bob: 2n }

console.log(vc1.compare(vc3));  // -1 (vc1 happened before vc3)
console.log(vc3.compare(vc1));  //  1 (vc3 happened after vc1)

const vc4 = new VectorClock('charlie');
const vc5 = new VectorClock('charlie');
console.log(vc4.compare(vc5));  //  0 (concurrent - neither before other)
```

**Parameters:**
- `other` (VectorClock) - Clock to compare with

**Returns:**
- `-1` if this happened-before other
- `0` if concurrent (neither before the other)
- `1` if this happened-after other

**Throws:**
- `TypeError` if other is not a VectorClock instance

#### happensBefore(other)

Check if this clock happened-before another clock (causal predecessor).

```javascript
const vc1 = new VectorClock('node-a');
vc1.increment();

const vc2 = new VectorClock('node-b');
vc2.increment().increment();

vc1.merge(vc2);  // Establish causality

console.log(vc1.happensBefore(vc2));  // false (different branches)
console.log(vc2.happensBefore(vc1));  // false
console.log(vc1.happensBefore(vc1));  // false (not before itself)
```

**Parameters:**
- `other` (VectorClock) - Clock to check against

**Returns:** `boolean` - true if this happened-before other

#### isConcurrentWith(other)

Check if two events are concurrent (neither happened-before the other).

```javascript
const vc1 = new VectorClock('alice');
vc1.increment();  // { alice: 1n }

const vc2 = new VectorClock('bob');
vc2.increment();  // { bob: 1n }

// No merge = no causal relationship = concurrent
console.log(vc1.isConcurrentWith(vc2));  // true

// After merge, they have causal relationship
vc1.merge(vc2);
const vc3 = new VectorClock('alice');
vc3.increment().increment();
vc3.merge(vc1);

console.log(vc1.isConcurrentWith(vc3));  // false (established order)
```

**Parameters:**
- `other` (VectorClock) - Clock to check against

**Returns:** `boolean` - true if concurrent (compare() === 0)

#### toJSON()

Serialize vector clock to JSON-compatible object. Converts BigInt counters to strings (JSON safe).

```javascript
const vc = new VectorClock('node-x');
vc.increment();
vc.increment();

const json = vc.toJSON();
console.log(json);
// {
//   nodeId: 'node-x',
//   counters: { 'node-x': '2' }
// }

// Safe to send over network or store
JSON.stringify(json);
```

**Returns:**
```javascript
{
  nodeId: string,
  counters: { [nodeId]: string }  // BigInt values as strings
}
```

#### static fromJSON(json)

Reconstruct vector clock from JSON object.

```javascript
const json = {
  nodeId: 'node-x',
  counters: { 'node-x': '2', 'node-y': '5' }
};

const vc = VectorClock.fromJSON(json);
console.log(vc.nodeId);                    // 'node-x'
console.log(vc.counters.get('node-x'));   // 2n (restored as BigInt)
console.log(vc.counters.get('node-y'));   // 5n

// Can continue using the clock
vc.increment();
console.log(vc.counters.get('node-x'));   // 3n
```

**Parameters:**
- `json` - Object with nodeId and counters (from toJSON())

**Returns:** `VectorClock` instance

**Throws:**
- `Error` if json is invalid or missing nodeId/counters

#### clone()

Create a deep copy of this vector clock.

```javascript
const vc1 = new VectorClock('node-a');
vc1.increment().increment();

const vc2 = vc1.clone();

// Modifying clone doesn't affect original
vc2.increment();

console.log(vc1.counters.get('node-a'));  // 2n (unchanged)
console.log(vc2.counters.get('node-a'));  // 3n (incremented)

// They are independent objects
console.log(vc1 === vc2);  // false
console.log(vc1.compare(vc2));  // 0 (concurrent after clone)
```

**Returns:** `VectorClock` - Deep copy of this clock

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

## Guard Functions

KGC 4D implements 32 poka-yoke mistake-proofing guards across 6 subsystems. Guards throw descriptive errors immediately to surface bugs during development.

See [Poka-Yoke Guards Reference](./03-guards.md) for conceptual explanations and FMEA analysis.

### Time Guards (T1-T5)

#### guardMonotonicOrdering(current, lastTime)

Enforce monotonic clock ordering. Prevents clock going backwards due to system clock drift.

```javascript
import { guardMonotonicOrdering } from '@unrdf/kgc-4d';

const result = guardMonotonicOrdering(100n, 99n);
// Returns: 100n (forwards, unchanged)

const wrapped = guardMonotonicOrdering(99n, 100n);
// Returns: 101n (backwards detected, auto-incremented)
```

**Parameters:**
- `current` (BigInt) - Current timestamp
- `lastTime` (BigInt) - Previous timestamp

**Returns:** BigInt (current or auto-incremented)

**Throws:** TypeError if not BigInt

---

#### guardTimeEnvironment()

Validate time environment (Node.js vs browser).

```javascript
import { guardTimeEnvironment } from '@unrdf/kgc-4d';

const isNode = guardTimeEnvironment();
// Returns: true if process.hrtime.bigint available
```

**Parameters:** None

**Returns:** boolean (true if Node.js environment)

**Throws:** Error if no valid time source

---

#### guardISOFormat(iso)

Validate ISO 8601 date format.

```javascript
import { guardISOFormat } from '@unrdf/kgc-4d';

guardISOFormat('2025-01-15T10:30:00.000Z');
// Returns: true

guardISOFormat('invalid');
// Throws: Error with format details
```

**Parameters:**
- `iso` (string) - ISO 8601 string

**Returns:** boolean

**Throws:** TypeError or Error if invalid format

---

#### guardBigIntRange(t_ns)

Validate BigInt timestamp within safe range.

```javascript
import { guardBigIntRange } from '@unrdf/kgc-4d';

guardBigIntRange(1701734400000000000n);
// Returns: true

guardBigIntRange(-1n);
// Throws: RangeError
```

**Parameters:**
- `t_ns` (BigInt) - Timestamp in nanoseconds

**Returns:** boolean

**Throws:** TypeError or RangeError if out of bounds

---

#### guardBigIntPrecision(t_ns)

Validate BigInt-to-Number precision preservation.

```javascript
import { guardBigIntPrecision } from '@unrdf/kgc-4d';

guardBigIntPrecision(1701734400000000000n);
// Returns: true (conversion safe)
```

**Parameters:**
- `t_ns` (BigInt) - Timestamp in nanoseconds

**Returns:** boolean

**Throws:** TypeError or RangeError if conversion not finite

---

### Store Guards (S1-S6)

#### guardEventIdGeneration(eventId)

Validate event ID (UUID or fallback format).

```javascript
import { guardEventIdGeneration } from '@unrdf/kgc-4d';

guardEventIdGeneration('550e8400-e29b-41d4-a716-446655440000');
// Returns: true (valid UUID)

guardEventIdGeneration('');
// Throws: Error (empty)
```

**Parameters:**
- `eventId` (string) - Event identifier

**Returns:** boolean

**Throws:** TypeError or Error if invalid format

---

#### guardPayloadJSON(payload)

Validate event payload JSON serializability.

```javascript
import { guardPayloadJSON } from '@unrdf/kgc-4d';

guardPayloadJSON({ userId: '123', action: 'create' });
// Returns: true

guardPayloadJSON(undefined);
// Returns: true (null payload allowed)
```

**Parameters:**
- `payload` (any) - Event metadata

**Returns:** boolean

**Throws:** Error if not JSON-serializable

---

#### guardQuadStructure(quad)

Validate RDF quad structure.

```javascript
import { guardQuadStructure } from '@unrdf/kgc-4d';

guardQuadStructure({
  subject: { value: 'http://example.org/alice' },
  predicate: { value: 'http://example.org/name' },
  object: { value: 'Alice' },
  graph: { value: 'kgc:Universe' }
});
// Returns: true

guardQuadStructure({ subject: null });
// Throws: Error (missing subject.value)
```

**Parameters:**
- `quad` (object) - RDF quad with subject, predicate, object, graph

**Returns:** boolean

**Throws:** TypeError or Error if quad malformed

---

#### guardDeltaType(deltaType)

Validate mutation type (add or delete only).

```javascript
import { guardDeltaType } from '@unrdf/kgc-4d';

guardDeltaType('add');
// Returns: true

guardDeltaType('update');
// Throws: Error (not in whitelist)
```

**Parameters:**
- `deltaType` (string) - Mutation type

**Returns:** boolean

**Throws:** TypeError or Error if not 'add' or 'delete'

---

#### guardEventCountOverflow(count)

Validate event count doesn't overflow.

```javascript
import { guardEventCountOverflow } from '@unrdf/kgc-4d';

guardEventCountOverflow(1000);
// Returns: true

guardEventCountOverflow(-1);
// Throws: RangeError (negative)
```

**Parameters:**
- `count` (number | BigInt) - Event count

**Returns:** boolean

**Throws:** TypeError or RangeError if overflow risk

---

#### guardGraphsExport(graphs)

Validate GRAPHS constant exports.

```javascript
import { guardGraphsExport } from '@unrdf/kgc-4d';
import { GRAPHS } from '@unrdf/kgc-4d';

guardGraphsExport(GRAPHS);
// Returns: true
```

**Parameters:**
- `graphs` (object) - GRAPHS export with UNIVERSE, EVENT_LOG, SYSTEM

**Returns:** boolean

**Throws:** TypeError or Error if missing required keys

---

### Git Guards (G1-G6)

#### guardGitRepository(dir)

Validate Git repository directory.

```javascript
import { guardGitRepository } from '@unrdf/kgc-4d';

guardGitRepository('./repo');
// Returns: true (if .git exists)

guardGitRepository('');
// Throws: Error (empty path)
```

**Parameters:**
- `dir` (string) - Repository directory path

**Returns:** boolean

**Throws:** TypeError or Error if directory invalid or not a Git repo

---

#### guardSnapshotWrite(snapshotPath)

Validate snapshot file can be written.

```javascript
import { guardSnapshotWrite } from '@unrdf/kgc-4d';

guardSnapshotWrite('./snapshots/snapshot.nq');
// Returns: true (if path valid and writable)
```

**Parameters:**
- `snapshotPath` (string) - Path where snapshot will be written

**Returns:** boolean

**Throws:** Error if path invalid or not writable

---

#### guardCommitHash(hash)

Validate Git commit hash format.

```javascript
import { guardCommitHash } from '@unrdf/kgc-4d';

guardCommitHash('abc123def456');
// Returns: true (valid hex)

guardCommitHash('not-hex');
// Throws: Error (invalid format)
```

**Parameters:**
- `hash` (string) - Git commit hash

**Returns:** boolean

**Throws:** Error if not valid hex format

---

#### guardSnapshotExists(hash)

Validate snapshot exists in Git.

```javascript
import { guardSnapshotExists } from '@unrdf/kgc-4d';

guardSnapshotExists('abc123def456');
// Returns: true (or throws if not found)
```

**Parameters:**
- `hash` (string) - Git commit hash

**Returns:** boolean

**Throws:** Error if snapshot not found

---

#### guardCommitMessageSafety(message)

Prevent command injection in commit messages.

```javascript
import { guardCommitMessageSafety } from '@unrdf/kgc-4d';

guardCommitMessageSafety('Snapshot created');
// Returns: true

guardCommitMessageSafety('$(rm -rf /)');
// Throws: Error (dangerous characters)
```

**Parameters:**
- `message` (string) - Commit message

**Returns:** boolean

**Throws:** Error if dangerous characters detected

---

### Freeze Guards (F1-F5)

#### guardNQuadsEncoding(nquads)

Validate N-Quads UTF-8 encoding.

```javascript
import { guardNQuadsEncoding } from '@unrdf/kgc-4d';

guardNQuadsEncoding('<http://example.org/s> <http://example.org/p> "o" .');
// Returns: true (valid UTF-8)
```

**Parameters:**
- `nquads` (string) - N-Quads content

**Returns:** boolean

**Throws:** Error if encoding invalid

---

#### guardEmptyUniverseFreeze(quads)

Prevent freezing empty universe.

```javascript
import { guardEmptyUniverseFreeze } from '@unrdf/kgc-4d';

guardEmptyUniverseFreeze([quad1, quad2]);
// Returns: true

guardEmptyUniverseFreeze([]);
// Throws: Error (no quads to freeze)
```

**Parameters:**
- `quads` (array) - Quads in universe

**Returns:** boolean

**Throws:** Error if universe empty

---

#### guardBLAKE3Hash(hash)

Validate BLAKE3 hash format.

```javascript
import { guardBLAKE3Hash } from '@unrdf/kgc-4d';

guardBLAKE3Hash('a1b2c3d4e5f6...');
// Returns: true (valid hex)
```

**Parameters:**
- `hash` (string) - BLAKE3 hash in hex

**Returns:** boolean

**Throws:** Error if not valid hex format

---

#### guardGitRefIntegrity(gitRef)

Validate Git reference integrity.

```javascript
import { guardGitRefIntegrity } from '@unrdf/kgc-4d';

guardGitRefIntegrity('abc123def456');
// Returns: true (or throws if corrupted)
```

**Parameters:**
- `gitRef` (string) - Git commit reference

**Returns:** boolean

**Throws:** Error if reference corrupted

---

#### guardReceiptSchema(receipt)

Validate snapshot receipt structure.

```javascript
import { guardReceiptSchema } from '@unrdf/kgc-4d';

guardReceiptSchema({
  snapshotId: 'uuid',
  gitRef: 'abc123',
  hash: 'a1b2c3...',
  tNs: 1701734400000000000n
});
// Returns: true
```

**Parameters:**
- `receipt` (object) - Frozen snapshot receipt

**Returns:** boolean

**Throws:** Error if schema invalid

---

### API Guards (A1-A5)

#### guardTimeGap(targetTime, snapshotTime)

Detect time anomalies (target before snapshot).

```javascript
import { guardTimeGap } from '@unrdf/kgc-4d';

guardTimeGap(1000n, 500n);
// Returns: true (target after snapshot)

guardTimeGap(500n, 1000n);
// Throws: Error (target before snapshot)
```

**Parameters:**
- `targetTime` (BigInt) - Target reconstruction time
- `snapshotTime` (BigInt) - Snapshot time

**Returns:** boolean

**Throws:** Error if target before snapshot

---

#### guardArgumentType(value, expectedType, argName)

Type-check function arguments.

```javascript
import { guardArgumentType } from '@unrdf/kgc-4d';

guardArgumentType(store, 'object', 'store');
// Returns: true

guardArgumentType('not-a-store', 'object', 'store');
// Throws: TypeError
```

**Parameters:**
- `value` (any) - Value to check
- `expectedType` (string) - Expected type
- `argName` (string) - Argument name for error message

**Returns:** boolean

**Throws:** TypeError if type mismatch

---

#### guardNotNull(value, argName)

Prevent null/undefined parameters.

```javascript
import { guardNotNull } from '@unrdf/kgc-4d';

guardNotNull(store, 'store');
// Returns: true (if store not null)

guardNotNull(null, 'store');
// Throws: Error (null not allowed)
```

**Parameters:**
- `value` (any) - Value to check
- `argName` (string) - Parameter name

**Returns:** boolean

**Throws:** Error if null or undefined

---

#### guardArgumentShape(value, expectedShape, argName)

Validate object structure (required keys).

```javascript
import { guardArgumentShape } from '@unrdf/kgc-4d';

guardArgumentShape(
  { type: 'CREATE', payload: {} },
  { type: 'string', payload: 'object' },
  'eventDesc'
);
// Returns: true
```

**Parameters:**
- `value` (object) - Object to validate
- `expectedShape` (object) - Expected shape (key -> type)
- `argName` (string) - Parameter name

**Returns:** boolean

**Throws:** Error if shape invalid

---

#### guardModuleExports(moduleExports, required)

Verify module exports.

```javascript
import { guardModuleExports } from '@unrdf/kgc-4d';

guardModuleExports(module.exports, ['KGCStore', 'GitBackbone']);
// Returns: true (if all exports present)
```

**Parameters:**
- `moduleExports` (object) - Module exports
- `required` (array) - Required export names

**Returns:** boolean

**Throws:** Error if exports missing

---

#### guardPublicAPI(module, expectedExports)

Check API contracts.

```javascript
import { guardPublicAPI } from '@unrdf/kgc-4d';

guardPublicAPI(kgcModule, ['freezeUniverse', 'reconstructState']);
// Returns: true (if all functions exported)
```

**Parameters:**
- `module` (object) - Module to check
- `expectedExports` (array) - Expected functions

**Returns:** boolean

**Throws:** Error if API contract broken

---

### Concurrency Guards (C1-C4)

#### guardAtomicWrite(filePath)

Ensure atomic file writes.

```javascript
import { guardAtomicWrite } from '@unrdf/kgc-4d';

guardAtomicWrite('./snapshot.nq');
// Returns: true (if write atomic)
```

**Parameters:**
- `filePath` (string) - File path

**Returns:** boolean

**Throws:** Error if atomic write not possible

---

#### guardEventIDUniqueness(eventId, existingIds)

Prevent ID collisions in concurrent scenario.

```javascript
import { guardEventIDUniqueness } from '@unrdf/kgc-4d';

guardEventIDUniqueness('new-id', new Set(['id-1', 'id-2']));
// Returns: true

guardEventIDUniqueness('id-1', new Set(['id-1', 'id-2']));
// Throws: Error (duplicate detected)
```

**Parameters:**
- `eventId` (string) - New event ID
- `existingIds` (Set | Array) - Existing IDs

**Returns:** boolean

**Throws:** Error if ID already exists

---

#### guardTimeStateEncapsulation()

Protect time state from external mutation.

```javascript
import { guardTimeStateEncapsulation } from '@unrdf/kgc-4d';

guardTimeStateEncapsulation();
// Returns: true (if time state protected)
```

**Parameters:** None

**Returns:** boolean

**Throws:** Error if time state compromised

---

#### guardEventCountConsistency(memoryCount, storeCount)

Verify event count consistency across layers.

```javascript
import { guardEventCountConsistency } from '@unrdf/kgc-4d';

guardEventCountConsistency(100, 100);
// Returns: true (counts match)

guardEventCountConsistency(100, 99);
// Throws: Error (counts diverged)
```

**Parameters:**
- `memoryCount` (number) - Count in memory
- `storeCount` (number) - Count in store

**Returns:** boolean

**Throws:** Error if counts diverge

---

### allGuards

Collection of all guard functions for testing and validation.

```javascript
import { allGuards } from '@unrdf/kgc-4d';

console.log(allGuards.length); // 32
console.log(allGuards.map(g => g.name));
// [guardMonotonicOrdering, guardTimeEnvironment, ...]
```

**Type:** Array<Function>

**Contains:** All 32 guard functions indexed and accessible

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
