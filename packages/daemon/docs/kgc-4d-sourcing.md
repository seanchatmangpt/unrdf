# KGC-4D daemon event sourcing

`DaemonEventStore` records daemon-operation state as an immutable transition ledger plus a derived current view. It is available through the public package subpath:

```javascript
import { DaemonEventStore } from '@unrdf/daemon/integrations/kgc-4d-sourcing';
```

Independent Merkle operations are available through:

```javascript
import {
  buildMerkleTree,
  getMerkleProofPath,
  verifyMerkleProof,
} from '@unrdf/daemon/integrations/kgc-4d-merkle';
```

## State model

The store keeps two distinct projections:

- `transitionLog`: every immutable `enqueued`, `started`, `success`, or `failure` transition in global hash-chain order.
- `eventLog`: the latest transition for each operation, used as the current operation view.

`updateEventStatus` appends a new transition. It never rewrites the prior transition. Each transition binds:

```text
schema
transition id
nanosecond timestamp
operation id and type
status
payload and metadata
previous global hash
previous transition id for the same operation
```

The legal lifecycle is:

```text
enqueued -> started -> success | failure
enqueued -> success | failure
```

A terminal operation refuses later transitions. Duplicate `started` transitions are refused.

## Evidence admission

Payloads, metadata, and results cross a deterministic admission boundary before any state change. Admitted values are:

- `null`
- strings and booleans
- finite numbers, including a distinct encoding for negative zero
- `BigInt`
- dense arrays of admitted values
- plain records with enumerable data properties and admitted values

The store refuses non-finite numbers, `undefined`, functions, symbols, cyclic structures, sparse arrays, accessors, non-enumerable properties, `Date`, `Map`, `Set`, and class instances. A refusal leaves the current view, transition ledger, index, and head hash unchanged.

## Basic lifecycle

```javascript
const store = new DaemonEventStore({ logger: console });
await store.initialize();

const enqueued = await store.appendEvent('create-task', {
  taskId: 'task-123',
  priority: 'high',
});

await store.updateEventStatus(enqueued.operationId, 'started');
await store.updateEventStatus(enqueued.operationId, 'success', {
  result: 'completed',
});
```

## Current view and lifecycle history

Queries return the current view by default:

```javascript
const current = await store.queryEvents({
  operationId: enqueued.operationId,
});
// One latest transition for the operation.
```

Request the append-only lifecycle explicitly:

```javascript
const lifecycle = await store.queryEvents({
  operationId: enqueued.operationId,
  includeHistory: true,
});
// enqueued -> started -> success
```

`reconstructState(timestamp)` replays transitions through the admitted timestamp and returns both current operation state and transition evidence:

```javascript
const state = await store.reconstructState(checkpointTimestamp);

console.log(state.eventCount);       // Operations visible at the checkpoint.
console.log(state.transitionCount);  // Transitions visible at the checkpoint.
console.log(state.events);           // Latest transition per operation.
console.log(state.transitions);      // Complete admitted history through the checkpoint.
```

## Chain verification

`verifyTransitionChain()` verifies the transition ledger and its derived projections:

```javascript
const receipt = await store.verifyTransitionChain();

if (!receipt.valid) {
  throw new Error(`${receipt.reason} at transition ${receipt.index}`);
}
```

Verification covers:

- strictly monotonic transition timestamps
- the global `previousHash` chain
- canonical transition-hash recomputation
- initial `enqueued` status
- per-operation `previousEventId` ancestry
- terminal and duplicate-started refusals
- current-view cardinality and identity
- operation-index cardinality and identity
- equality between the verified ledger head and `store.previousHash`

Failure reasons remain typed, including `CURRENT_HASH_MISMATCH`, `PREVIOUS_HASH_MISMATCH`, `PREVIOUS_EVENT_ID_MISMATCH`, `POST_TERMINAL_TRANSITION`, `CURRENT_VIEW_MISMATCH`, and `CURRENT_INDEX_MISMATCH`.

## Universe freezes

`freezeUniverse()` captures immutable current-view and transition commitments:

```javascript
const snapshot = await store.freezeUniverse();

console.log(snapshot.eventCount);
console.log(snapshot.transitionCount);
console.log(snapshot.merkleRoot);
console.log(snapshot.transitionMerkleRoot);
console.log(snapshot.stateHash);
```

Merkle roots are domain-separated and bind the leaf count. The empty-tree root is therefore a protocol commitment, not the raw BLAKE3 digest of an empty string.

## Merkle proofs

Current-view proofs:

```javascript
const proof = await store.generateMerkleProof(eventIndex);
const valid = await store.verifyProof(proof);
```

Transition-history proofs:

```javascript
const proof = await store.generateTransitionProof(transitionIndex);
const valid = await store.verifyProof(proof);
```

A proof binds `leafIndex`, `leafCount`, `leafHash`, the canonical sibling path, and `merkleRoot`. Verification refuses incorrect left/right positions, invalid duplicated-tail evidence, and surplus or missing path steps.

## Bounded standing

This implementation establishes in-memory transition admission, hash-chain verification, current-view projection, temporal reconstruction, universe snapshots, and Merkle membership proofs. It does **not** by itself establish durable persistence, multi-process synchronization, authority signatures, external timestamp authority, crash recovery, or replication. Those capabilities require separate storage, consensus, authority, and replay receipts.
