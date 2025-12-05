# KGC 4D Engine - API Reference

## Core Classes

### KGCStore

Extended UnrdfStore with atomic event appending and named graph support.

#### Constructor

```javascript
const store = new KGCStore(options?: Object)
```

**Options** (passed to UnrdfStore):
- `backend`: Store backend (defaults to oxigraph)
- Other UnrdfStore options

#### Methods

##### appendEvent(eventData, deltas)

Atomically appends event to EventLog and applies deltas to Universe.

```javascript
const { receipt } = await store.appendEvent(
  {
    type: 'CREATE',
    payload: { description: 'Added Alice' }
  },
  [
    {
      type: 'add',
      subject: namedNode('http://example.org/Alice'),
      predicate: namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      object: namedNode('http://example.org/Person')
    }
  ]
);
```

**Parameters**:
- `eventData` (Object): Event metadata
  - `type` (string): EVENT_TYPES.CREATE, UPDATE, DELETE, SNAPSHOT
  - `payload` (Object): Application-specific metadata
  - `git_ref` (string, optional): Git commit hash
- `deltas` (Array): State changes to apply
  - Each delta: `{ type: 'add'|'delete', subject, predicate, object }`

**Returns**: Promise<{receipt}>
- `receipt.id` (string): UUID for event
- `receipt.t_ns` (string): BigInt nanosecond timestamp as string
- `receipt.timestamp_iso` (string): ISO 8601 formatted timestamp
- `receipt.event_count` (number): Cumulative events in this session

##### queryEventLog(sparql)

SPARQL query on EventLog named graph.

```javascript
const results = await store.queryEventLog(`
  SELECT ?event ?type WHERE {
    GRAPH <http://kgc.io/EventLog> {
      ?event <http://kgc.io/type> ?type .
    }
  }
`);
```

**Parameters**:
- `sparql` (string): SPARQL query string

**Returns**: Promise<Array> of results

##### queryUniverse(sparql)

SPARQL query on Universe named graph.

```javascript
const results = await store.queryUniverse(`
  SELECT ?person ?name WHERE {
    GRAPH <http://kgc.io/Universe> {
      ?person <http://xmlns.com/foaf/0.1/name> ?name .
    }
  }
`);
```

**Parameters**:
- `sparql` (string): SPARQL query string

**Returns**: Promise<Array> of results

##### getEventCount()

Returns the number of events appended in this session.

```javascript
const count = store.getEventCount();
// Example output: 42
```

**Returns**: number

---

### GitBackbone

Git-backed content-addressed store for snapshots.

#### Constructor

```javascript
const git = new GitBackbone(dir?: string)
```

**Parameters**:
- `dir` (string): Repository directory (default: '.')

#### Methods

##### commitSnapshot(nquads, message)

Persists N-Quads to Git, returns commit hash.

```javascript
const commitHash = await git.commitSnapshot(
  `<http://example.org/Alice> <http://example.org/age> "30" .`,
  'Snapshot at 2024-12-04T15:16:00.123Z'
);
```

**Parameters**:
- `nquads` (string): N-Quads formatted RDF
- `message` (string): Commit message

**Returns**: Promise<string> - Git commit hash (abbreviated)

**Node.js Behavior**: Uses Git CLI (`git add`, `git commit`)

**Browser Behavior**: Uses isomorphic-git with lightning-fs

##### readSnapshot(hash)

Fetches snapshot content from Git by commit hash.

```javascript
const nquads = await git.readSnapshot('abc123def456');
```

**Parameters**:
- `hash` (string): Git commit hash or abbreviated hash

**Returns**: Promise<string> - N-Quads content

---

## Functions

### freezeUniverse(store, gitBackbone)

Creates a frozen snapshot of the universe and records receipt.

```javascript
const receipt = await freezeUniverse(store, gitBackbone);

// Output:
// {
//   id: "550e8400-e29b-41d4-a716-446655440000",
//   t_ns: "1733314560123456789",
//   timestamp_iso: "2024-12-04T15:16:00.123Z",
//   universe_hash: "blake3_hash_...",
//   git_ref: "abc123def456",
//   event_count: 42,
//   nquad_count: 156
// }
```

**Parameters**:
- `store` (KGCStore): The store to freeze
- `gitBackbone` (GitBackbone): Git backend for persistence

**Returns**: Promise<Receipt>
- `id`: Event UUID
- `t_ns`: Timestamp as BigInt string
- `timestamp_iso`: ISO 8601 string
- `universe_hash`: BLAKE3 hash of N-Quads
- `git_ref`: Git commit hash
- `event_count`: Cumulative events
- `nquad_count`: Number of triples

---

### reconstructState(store, gitBackbone, targetTime)

Reconstructs universe state at a specific point in time.

```javascript
const pastStore = await reconstructState(
  store,
  gitBackbone,
  BigInt(receipt.t_ns)  // Time travel to frozen receipt
);

// pastStore is a new KGCStore with state from targetTime
```

**Parameters**:
- `store` (KGCStore): Source store (EventLog queried)
- `gitBackbone` (GitBackbone): Git backend for snapshot retrieval
- `targetTime` (BigInt): Nanosecond timestamp to reconstruct

**Returns**: Promise<KGCStore> - New store with reconstructed state

**Behavior**:
1. Queries EventLog for nearest SNAPSHOT event before targetTime
2. Loads snapshot N-Quads from Git
3. Creates new temp store, loads N-Quads into Universe graph
4. Queries for events between snapshot and targetTime
5. Replays events (applies deltas)
6. Returns reconstructed store

---

### verifyReceipt(receipt, gitBackbone, store)

Cryptographically verifies a frozen receipt.

```javascript
const verification = await verifyReceipt(receipt, gitBackbone, store);

if (verification.valid) {
  console.log(`✓ Receipt verified`);
  console.log(`✓ Hash matches Git commit`);
} else {
  console.log(`✗ Verification failed: ${verification.reason}`);
}
```

**Parameters**:
- `receipt` (Receipt): Frozen receipt to verify
- `gitBackbone` (GitBackbone): Git backend
- `store` (KGCStore): Store (optional, for additional validation)

**Returns**: Promise<{valid, reason?, receipt_id?, timestamp?, universe_hash?, git_commit?}>
- If `valid === true`:
  - `receipt_id`: UUID from receipt
  - `timestamp`: ISO timestamp
  - `universe_hash`: Verified BLAKE3 hash
  - `git_commit`: Verified Git hash
- If `valid === false`:
  - `reason`: Human-readable error message

---

## Time Functions

### now()

Get current time in nanoseconds (BigInt).

```javascript
const t_ns = now();
// Example: 1733314560123456789n
```

**Returns**: BigInt - Nanoseconds since epoch

**Behavior**:
- Node.js: `process.hrtime.bigint()` (true nanosecond precision)
- Browser: `performance.now() * 1_000_000` (millisecond-based approximation)
- Enforces monotonic ordering (never decreases)

---

### toISO(t_ns)

Convert BigInt nanoseconds to ISO 8601 string.

```javascript
const iso = toISO(1733314560123456789n);
// Output: "2024-12-04T15:16:00.123Z"
```

**Parameters**:
- `t_ns` (BigInt): Nanosecond timestamp

**Returns**: string - ISO 8601 formatted datetime

---

### fromISO(iso)

Parse ISO 8601 string to BigInt nanoseconds.

```javascript
const t_ns = fromISO("2024-12-04T15:16:00.123Z");
// Output: 1733314560123000000n
```

**Parameters**:
- `iso` (string): ISO 8601 datetime string

**Returns**: BigInt - Nanoseconds

**Throws**: Error if invalid ISO string

---

### addNanoseconds(t_ns, delta)

Add nanoseconds to a timestamp.

```javascript
const future = addNanoseconds(now(), BigInt(1_000_000_000));  // +1 second
```

**Parameters**:
- `t_ns` (BigInt): Base timestamp
- `delta` (BigInt | number): Nanoseconds to add

**Returns**: BigInt - New timestamp

---

### duration(start_ns, end_ns)

Calculate duration between two timestamps.

```javascript
const elapsed = duration(start, end);
// Returns BigInt nanoseconds
```

**Parameters**:
- `start_ns` (BigInt): Start time
- `end_ns` (BigInt): End time

**Returns**: BigInt - Nanoseconds between timestamps

---

## Constants

### GRAPHS

Named graph URIs:

```javascript
import { GRAPHS } from '@unrdf/kgc-4d';

GRAPHS.UNIVERSE   // "http://kgc.io/Universe"
GRAPHS.EVENT_LOG  // "http://kgc.io/EventLog"
GRAPHS.SYSTEM     // "http://kgc.io/System"
```

### EVENT_TYPES

Event type constants:

```javascript
import { EVENT_TYPES } from '@unrdf/kgc-4d';

EVENT_TYPES.CREATE     // "CREATE"
EVENT_TYPES.UPDATE     // "UPDATE"
EVENT_TYPES.DELETE     // "DELETE"
EVENT_TYPES.SNAPSHOT   // "SNAPSHOT"
```

### PREDICATES

RDF predicate URIs for event serialization:

```javascript
import { PREDICATES } from '@unrdf/kgc-4d';

PREDICATES.T_NS        // "http://kgc.io/t_ns"
PREDICATES.TYPE        // "http://kgc.io/type"
PREDICATES.GIT_REF     // "http://kgc.io/git_ref"
PREDICATES.PAYLOAD     // "http://kgc.io/payload"
```

---

## Types (JSDoc)

### Receipt

```javascript
/**
 * @typedef {Object} Receipt
 * @property {string} id - Event UUID
 * @property {string} t_ns - Nanosecond timestamp as string
 * @property {string} timestamp_iso - ISO 8601 datetime
 * @property {string} universe_hash - BLAKE3 hash of N-Quads
 * @property {string} git_ref - Git commit hash
 * @property {number} event_count - Cumulative events
 * @property {number} nquad_count - Number of triples
 */
```

### EventData

```javascript
/**
 * @typedef {Object} EventData
 * @property {string} type - EVENT_TYPES constant
 * @property {Object} payload - Application metadata
 * @property {string} [git_ref] - Optional Git reference
 */
```

### Delta

```javascript
/**
 * @typedef {Object} Delta
 * @property {'add'|'delete'} type - Add or delete
 * @property {*} subject - RDF subject (namedNode, etc.)
 * @property {*} predicate - RDF predicate
 * @property {*} object - RDF object
 */
```

---

## Error Handling

All functions throw on error. Catch with try-catch:

```javascript
try {
  const receipt = await freezeUniverse(store, git);
} catch (error) {
  console.error('Freeze failed:', error.message);
}
```

**Common Errors**:
- `"Failed to freeze universe: ..."` - Dump or hash failed
- `"Git commit failed: ..."` - Git operation failed
- `"No snapshot found before time ..."` - Time-travel target pre-dates all snapshots
- `"Hash mismatch: ..."` - Receipt verification failed

---

## Performance Characteristics

| Operation | Target |
|-----------|--------|
| `now()` | <1μs |
| `appendEvent()` | <5ms (Oxigraph transaction overhead) |
| `freezeUniverse()` | <1s (for <100K quads) |
| `reconstructState()` | <2s (includes replay) |
| `verifyReceipt()` | <100ms (hash recompute) |

---

## Examples

See `examples/basic-usage.mjs` for complete working examples including:
- Basic freeze and time-travel
- Event log querying
- Multi-event sequences with snapshots
- Receipt verification

---

## Related Documentation

- **ARD** - Architecture requirements and design principles
- **Basic Usage** - Practical examples and workflows
