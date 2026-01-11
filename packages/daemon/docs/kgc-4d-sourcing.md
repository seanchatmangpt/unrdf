# KGC-4D Event Sourcing for Daemon Operations

## Overview

The `DaemonEventStore` implements event sourcing for background daemon operations with KGC-4D temporal semantics. This provides:

- **Append-only operation logs** with nanosecond precision timestamps
- **Universe freeze snapshots** at operation boundaries for deterministic state capture
- **BLAKE3 hash chains** for immutability verification and tamper detection
- **Temporal queries** supporting point-in-time state reconstruction and replay
- **Merkle tree proofs** for cryptographic audit trail verification

## Architecture

### Five-Layer Event Flow

```
Operation Execution
        ↓
Event Append → Hash Chain Link
        ↓
Status Updates (enqueued → started → success/failure)
        ↓
Universe Freeze → Snapshot + Merkle Root
        ↓
Temporal Queries & State Reconstruction
```

### Core Components

#### 1. Event Log (Append-Only)

Every daemon operation generates an immutable event entry:

```javascript
{
  id: UUID,                    // Unique event identifier
  timestamp: BigInt,           // Nanosecond precision (KGC-4D)
  operationId: string,         // Links to operation being tracked
  operationType: string,       // Category (create-task, update-config, etc.)
  status: enum,               // enqueued | started | success | failure
  payload: Object,            // Operation data
  previousHash: string,       // BLAKE3 hash of prior event
  currentHash: string,        // BLAKE3 hash of this event
  metadata: Object,           // Optional context (priority, source, etc.)
}
```

#### 2. Hash Chain

Each event links to the previous via BLAKE3 hash (ARD-mandated fastest WASM implementation):

```
Event 0: hash_0 = blake3(payload_0)
Event 1: hash_1 = blake3(payload_1), previousHash = hash_0
Event 2: hash_2 = blake3(payload_2), previousHash = hash_1
```

**Tamper Detection**: If any event is modified, its hash changes, breaking the chain immediately.

#### 3. Universe Freeze Snapshots

At operation boundaries, capture immutable state snapshot:

```javascript
{
  freezeId: UUID,                    // Snapshot identifier
  timestamp: BigInt,                 // When snapshot taken (ns precision)
  freezeTimestampISO: string,       // ISO 8601 representation
  eventCount: number,               // Events up to this point
  stateHash: string,                // BLAKE3 of current state
  merkleRoot: string,               // Root of Merkle tree over all events
  previousFreezeId: UUID | null,    // Chain of snapshots
  operations: [                      // Snapshot of all operations
    {
      operationId: string,
      status: enum,
      operationType: string,
    }
  ]
}
```

#### 4. Temporal Reconstruction

Replay events up to specific timestamp to reconstruct state at any point in time:

```javascript
const state = await store.reconstructState(targetTimestamp);
// state.events      → Events occurred by that time
// state.merkleRoot  → Merkle root at that point
// state.stateHash   → Hash chain head at that point
```

**Use Cases**:
- Point-in-time audit queries
- Root cause analysis (what was state when failure occurred?)
- Compliance reporting (state as of specific date)

#### 5. Merkle Tree Proofs

Cryptographic proofs that events belong to snapshot:

```javascript
const proof = await store.generateMerkleProof(eventIndex);
// proof.leafIndex  → Position in log
// proof.leafHash   → Individual event hash
// proof.proof      → Path to root
// proof.merkleRoot → Snapshot root

const isValid = await store.verifyProof(proof);
```

**Verification**: Recompute hash path, ensure it reaches expected root.

## Usage Examples

### Basic Event Sourcing Workflow

```javascript
import { DaemonEventStore } from '@unrdf/daemon/integrations/kgc-4d-sourcing';

// Create and initialize store
const store = new DaemonEventStore({ logger: console });
await store.initialize();

// Append operation
const event = await store.appendEvent('create-task', {
  taskId: 'task-123',
  priority: 'high',
});

// Update status
await store.updateEventStatus(event.operationId, 'started');
await store.updateEventStatus(event.operationId, 'success', {
  duration: '2.5ms',
  result: 'completed',
});

// Freeze universe at checkpoint
const snapshot = await store.freezeUniverse();
console.log(`${snapshot.eventCount} events frozen at ${snapshot.freezeTimestampISO}`);
```

### Temporal Queries

```javascript
// Query all 'task-complete' operations
const taskOps = await store.queryEvents({
  operationType: 'task-complete',
});

// Query recent successful operations
const now = now();  // KGC-4D time
const yesterday = now - 86_400_000_000_000n;  // 1 day in nanoseconds

const recent = await store.queryEvents({
  fromTimestamp: yesterday,
  toTimestamp: now,
  status: 'success',
});

// Query specific operation lifecycle
const opId = event.operationId;
const lifecycle = await store.queryEvents({ operationId: opId });
// Shows: enqueued → started → success progression
```

### Time-Travel Replay

```javascript
// Capture state at operation start
const checkpointTime = event.timestamp;

// Make more operations...
await store.appendEvent('update-task', {});
await store.appendEvent('delete-task', {});

// Reconstruct state as it was at checkpoint
const stateAtCheckpoint = await store.reconstructState(checkpointTime);
console.log(`${stateAtCheckpoint.eventCount} events at checkpoint time`);
// Output: 1 (only the initial operation)
```

### Audit Trail Verification

```javascript
// Generate Merkle proof for compliance report
const proof = await store.generateMerkleProof(0);

// Verify proof independently
const isValid = await store.verifyProof(proof);
if (!isValid) {
  throw new Error('Audit trail tampered with');
}

// Include in compliance report
report.auditProof = {
  event: store.eventLog[0],
  proof,
  verified: isValid,
};
```

### Hash Chain Integrity

```javascript
// Each event maintains chain
const event1 = await store.appendEvent('op1', {});
const event2 = await store.appendEvent('op2', {});

// Verify chain
console.assert(event2.previousHash === event1.currentHash);
// If false: event1 or event2 was modified

// Current head
const stats = store.getStats();
console.log(`Hash chain head: ${stats.currentHash}`);
```

## Proof Verification Workflow

### Scenario: Auditor Verifies Operation

1. **Auditor receives event log snapshot**
   ```javascript
   const logSnapshot = await store.reconstructState(auditTimestamp);
   ```

2. **Auditor requests Merkle proof for specific event**
   ```javascript
   const proof = await store.generateMerkleProof(eventIndex);
   // Contains: leaf hash, proof path, merkle root
   ```

3. **Auditor verifies proof independently**
   ```javascript
   // Can be done offline in isolated environment
   const isValid = await store.verifyProof(proof);
   ```

4. **Auditor checks snapshot signed by authority**
   ```javascript
   // Snapshot includes merkleRoot
   // If proof.merkleRoot === snapshot.merkleRoot → proof valid
   // If not → event was modified after snapshot
   ```

## Universe Freeze Semantics

### Empty Universe Freeze (Genesis Snapshot)

Freezing an empty event log (0 events) creates a **genesis snapshot**:

```javascript
const store = new DaemonEventStore();
await store.initialize();

// No events yet
const genesisSnapshot = await store.freezeUniverse();
// genesisSnapshot.eventCount === 0
// genesisSnapshot.merkleRoot = blake3('')  // Empty tree hash
// genesisSnapshot.previousFreezeId === null
```

**Use Cases**:
- Establishes baseline state before any operations
- Enables time-travel to "before any events" using genesis as anchor
- Subsequent freezes build on genesis point

### Multi-Freeze History

```javascript
// Freeze 1: After first batch of operations
const snap1 = await store.freezeUniverse();

// More operations...
await store.appendEvent('op1', {});

// Freeze 2: After additional operations
const snap2 = await store.freezeUniverse();
console.assert(snap2.previousFreezeId === snap1.freezeId);

// Freeze history forms chain
const history = store.getFreezeHistory();
// [snap1, snap2, ...]
```

## State Reconstruction Guarantee

**Deterministic**: Given same event log and timestamp, reconstruction always produces identical state.

```javascript
const state1 = await store.reconstructState(timestamp);
const state2 = await store.reconstructState(timestamp);

console.assert(state1.merkleRoot === state2.merkleRoot);
console.assert(state1.stateHash === state2.stateHash);
// Same input → identical output (no randomness)
```

## Performance Characteristics

### Event Appending
- **O(1) hash**: BLAKE3 constant time for fixed payload sizes
- **O(1) log insert**: Array append
- **Total**: <1μs per event on typical hardware

### Temporal Queries
- **O(n) scan**: Linear scan through event log
- **Filtering**: In-memory via array methods
- **Total**: <1ms for 10K events

### Merkle Proof Generation
- **Tree construction**: O(n log n) for n events
- **Proof path**: O(log n) nodes from leaf to root
- **Total**: <10ms for 10K events

### State Reconstruction
- **Replay**: O(n) events from genesis to target time
- **Merkle recompute**: O(n log n)
- **Total**: <100ms for 10K events with modern hardware

## Audit Requirements

### What Evidence Must Be Preserved

1. **Event log**: Append-only records (never modify past events)
2. **Hash chain**: Previous hash of each event
3. **Freeze snapshots**: Immutable checkpoints with Merkle roots
4. **Merkle proofs**: Generated independently for audit

### What Auditor Verifies

```
✓ Event log integrity (hash chain unbroken)
✓ Merkle proofs recompute to frozen root
✓ Status transitions follow valid state machine (enqueued → started → success/failure)
✓ Timestamps are monotonically increasing
✓ Operation IDs are unique
✓ Payloads match recorded hashes
```

### What Cannot Be Forged

- **Tampered event**: Hash mismatch in chain
- **Inserted event**: Merkle proof fails
- **Deleted event**: Merkle root mismatch
- **Reordered event**: Timestamp order violation
- **Modified status**: Hash chain break

## Integration with Daemon

### Event Types

The daemon generates events for:

```javascript
await store.appendEvent('task-scheduled', {
  taskId,
  nextRun,
  trigger,
});

await store.appendEvent('task-execution-started', {
  taskId,
  executionId,
  executorNode,
});

await store.appendEvent('task-execution-completed', {
  taskId,
  executionId,
  result,
  duration,
});

// With status progression
await store.updateEventStatus(executionId, 'started');
await store.updateEventStatus(executionId, 'success', { outputHash });
// or
await store.updateEventStatus(executionId, 'failure', { error });
```

### Query Patterns

```javascript
// Find all task executions for investigation
const executions = await store.queryEvents({
  operationType: 'task-execution-completed',
  status: 'failure',
  fromTimestamp: dayAgo,
  toTimestamp: now,
});

// Time-travel to understand state at failure point
for (const exec of executions) {
  const state = await store.reconstructState(exec.timestamp);
  console.log(`At ${toISO(exec.timestamp)}: ${state.eventCount} events`);
}
```

## Configuration

```javascript
const store = new DaemonEventStore({
  logger: {
    log: (msg) => console.log(`[EventStore] ${msg}`),
    error: (msg) => console.error(`[EventStore] ${msg}`),
  },
});

await store.initialize();
```

## API Reference

### DaemonEventStore

#### `constructor(options)`
- `options.logger` - Custom logger (default: console)

#### `async initialize()`
Initialize hash chain genesis (must call before using store)

#### `async appendEvent(operationType, payload, metadata)`
Append event to log with hash chain linking

#### `async updateEventStatus(operationId, status, result)`
Update operation status (enqueued → started → success/failure)

#### `async freezeUniverse()`
Create immutable snapshot with Merkle root

#### `async reconstructState(targetTimestamp)`
Replay events to specific point in time

#### `async queryEvents(query)`
Filter events by timestamp range, type, ID, status

#### `async generateMerkleProof(eventIndex)`
Generate cryptographic proof for event at index

#### `async verifyProof(proof)`
Verify Merkle proof is valid for frozen root

#### `getStats()`
Get aggregate statistics (event count, freeze count, hash head)

#### `getFreezeHistory()`
Get all universe freeze snapshots in order

## See Also

- [@unrdf/kgc-4d](../../../kgc-4d/) - Time model and freeze engine
- [@unrdf/daemon](../README.md) - Main daemon package
- [@unrdf/v6-core](../../v6-core/) - Control plane and delta contracts
