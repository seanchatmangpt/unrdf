# Agent 8: Store Adapter & Atomic Apply

**Status**: ✅ Implementation Complete
**Created**: 2025-12-26
**Agent**: Backend API Developer (Agent 8)

---

## Overview

Agent 8 provides atomic RDF store operations with capsule delta application and receipt generation. This module implements ACID-compliant operations for applying state changes to the RDF store with comprehensive verification and rollback capabilities.

## Features

- **Atomicity**: All-or-nothing semantics - operations either fully succeed or fully roll back
- **Receipt Chain**: Hash-based verification with parent-child linking (BLAKE3)
- **Idempotence**: Replay-safe operations - applying the same capsules produces identical state
- **Snapshot Integration**: Optional freeze/restore via @unrdf/kgc-4d
- **100% Type Coverage**: JSDoc annotations on all exports
- **Zod Validation**: Input validation on all public APIs

## Installation

```bash
pnpm add @agent-8/store-adapter
```

## Quick Start

```javascript
import { createAtomicStore, applyCapsule } from '@agent-8/store-adapter';
import { dataFactory } from '@unrdf/oxigraph';

// Create store
const store = createAtomicStore({ nodeId: 'node-1' });

// Define capsule
const capsule = {
  delta: {
    add: [
      {
        subject: dataFactory.namedNode('http://ex.org/alice'),
        predicate: dataFactory.namedNode('http://ex.org/name'),
        object: dataFactory.literal('Alice')
      }
    ],
    del: []
  }
};

// Apply atomically
const receipt = await applyCapsule(store, capsule);

console.log(receipt.hash);         // BLAKE3 hash (64 hex chars)
console.log(receipt.timestamp);    // ISO 8601 timestamp
console.log(receipt.stats.added);  // 1
```

## API Documentation

### Store Operations

#### `createAtomicStore(options)`

Create a new atomic store instance.

**Parameters:**
- `options.nodeId` (string, optional): Node ID for receipt chain
- `options.enableSnapshots` (boolean, optional): Enable KGC-4D integration (default: false)

**Returns:** `AtomicStore` instance

**Example:**
```javascript
const store = createAtomicStore({
  nodeId: 'production-node-1',
  enableSnapshots: true
});
```

---

### Capsule Application

#### `applyCapsule(store, capsule, options)`

Apply capsule deltas atomically to store with ACID guarantees.

**Parameters:**
- `store` (AtomicStore): Target store
- `capsule` (Object): Capsule with delta operations
  - `capsule.delta.add` (Array): Quads to add
  - `capsule.delta.del` (Array): Quads to delete
  - `capsule.metadata` (Object, optional): Metadata (id, label, timestamp)
- `options.parentHash` (string, optional): Parent receipt hash for chain linking

**Returns:** `Promise<Receipt>`

**Receipt Format:**
```javascript
{
  hash: "blake3_hash...",           // 64 hex chars
  timestamp: "2025-12-26T...",      // ISO 8601
  success: true,
  parentHash: "parent_hash...",     // null for first
  stats: {
    added: 5,
    deleted: 2,
    capsuleId: "uuid..."            // from metadata
  }
}
```

**Example:**
```javascript
const receipt = await applyCapsule(store, capsule, {
  parentHash: previousReceipt.hash
});
```

---

#### `applyBatch(store, capsules)`

Apply multiple capsules sequentially with automatic chain linking.

**Parameters:**
- `store` (AtomicStore): Target store
- `capsules` (Array): Array of capsules

**Returns:** `Promise<Array<Receipt>>`

**Example:**
```javascript
const receipts = await applyBatch(store, [capsule1, capsule2, capsule3]);
// receipts[1].parentHash === receipts[0].hash
// receipts[2].parentHash === receipts[1].hash
```

---

### Query Operations

#### `queryStore(store, sparql, options)`

Execute SPARQL query against the store.

**Parameters:**
- `store` (AtomicStore): Store to query
- `sparql` (string): SPARQL query
- `options` (Object, optional): Query options (baseIri, timeout, etc.)

**Returns:** `Promise<Array|boolean|Object>` - Results depend on query type

**Example:**
```javascript
const results = await queryStore(store, `
  SELECT ?name WHERE {
    ?person foaf:name ?name .
  }
`);
```

---

#### `queryUniverse(store, sparql)`

Query only the Universe graph (automatic FROM clause injection).

**Example:**
```javascript
const results = await queryUniverse(store, `
  SELECT * WHERE { ?s ?p ?o }
`);
```

---

#### `countQuads(store, graph)`

Count quads in store, optionally filtered by graph.

**Example:**
```javascript
const total = countQuads(store);
const universeQuads = countQuads(store, 'urn:autonomic:universe');
```

---

### Snapshot Operations (Optional)

#### `snapshotStore(store, label, gitBackbone)`

Create Git-backed snapshot of current store state.

**Requires:** `enableSnapshots: true` and `@unrdf/kgc-4d`

**Example:**
```javascript
import { GitBackbone } from '@unrdf/kgc-4d/git';

const git = new GitBackbone('/path/to/snapshots');
const freezeReceipt = await snapshotStore(store, 'v1.0', git);
console.log(freezeReceipt.git_ref); // Git commit hash
```

---

#### `restoreSnapshot(store, gitRef, gitBackbone)`

Restore store from snapshot (clears existing data).

**Example:**
```javascript
await restoreSnapshot(store, freezeReceipt.git_ref, git);
```

---

### Utility Functions

#### `hashReceipt(data)`

Generate BLAKE3 hash for receipt data (deterministic).

**Example:**
```javascript
const hash = await hashReceipt({ delta, timestamp });
// Returns 64 hex character BLAKE3 hash
```

---

#### `verifyReceipt(receipt, data)`

Verify receipt hash matches original data.

**Example:**
```javascript
const isValid = await verifyReceipt(receipt, originalData);
```

---

## Implementation Details

### Atomic Transaction Algorithm

1. **Validate Input**: Zod schema validation on capsule
2. **Capture Rollback State**: Track original quads
3. **Apply Deletes First**: Remove quads from universe graph
4. **Apply Adds**: Insert new quads to universe graph
5. **Generate Receipt**: Hash delta + timestamp, link to parent
6. **Commit**: Return receipt on success
7. **Rollback**: Restore original state on ANY error

### Receipt Chain

Receipts form a hash chain where each receipt links to its parent:

```
Receipt 1 (hash: abc...)
  ↓ parentHash
Receipt 2 (hash: def..., parentHash: abc...)
  ↓ parentHash
Receipt 3 (hash: ghi..., parentHash: def...)
```

This enables:
- Verification of operation order
- Detection of missing/modified operations
- Replay from any point in history

### Idempotence Guarantees

Applying the same sequence of capsules always produces the same final state:

```javascript
store1.apply([c1, c2, c3]) === store2.apply([c1, c2, c3])
```

This is achieved through:
- Deterministic ordering (deletes before adds)
- Canonical N-Quads serialization
- Consistent hashing (BLAKE3)

## File Structure

```
agent-8/
├── src/
│   ├── index.mjs          # Main entry point
│   ├── store.mjs          # AtomicStore class (221 lines)
│   ├── apply.mjs          # Capsule application logic (232 lines)
│   ├── query.mjs          # Query wrappers (197 lines)
│   ├── freeze.mjs         # Snapshot integration (146 lines)
│   └── utils.mjs          # Receipt utilities (202 lines)
├── test/
│   ├── apply.test.mjs     # Atomic operation tests (281 lines)
│   ├── replay.test.mjs    # Idempotence tests (284 lines)
│   ├── query.test.mjs     # Query tests (239 lines)
│   └── freeze.test.mjs    # Snapshot tests (201 lines)
├── package.json
├── vitest.config.mjs
├── PLAN.md
└── README.md
```

**Total Lines of Code**: 2,280 (998 src + 1,005 test + 277 docs)

## Testing

```bash
# Run all tests
pnpm test

# Watch mode
pnpm test:watch

# Coverage report
pnpm test:coverage
```

### Test Coverage

- **Atomic Apply**: 11 tests covering add, delete, mixed, rollback, validation, batch
- **Replay & Idempotence**: 9 tests covering determinism, undo-replay, edge cases
- **Query Operations**: 12 tests covering SPARQL, counting, error handling
- **Snapshot Integration**: 8 tests covering enablement, state management, chain integrity

**Total Tests**: 40 comprehensive test cases

## Compliance Checklist

- ✅ **No N3 Imports**: All RDF operations use `@unrdf/oxigraph`
- ✅ **100% JSDoc Coverage**: All exports documented
- ✅ **Zod Validation**: All inputs validated
- ✅ **Atomic Operations**: Full rollback on error
- ✅ **Receipt Chain**: Parent-child hash linking
- ✅ **Idempotence**: Replay produces same state
- ✅ **BLAKE3 Hashing**: Deterministic 64-char hashes
- ✅ **No OTEL in Implementation**: Pure business logic
- ✅ **All Files <500 Lines**: Largest is 284 lines (replay.test.mjs)

## Integration

### With Agent 2 (Planner)
- Accepts capsules from planner with `delta.add` and `delta.del`
- Returns receipts for verification

### With Agent 3 (Lens Compiler)
- Applies compiled lens transformations atomically
- Tracks transformation chain via receipt hashes

### With KGC-4D (Optional)
- Integrates with `freezeUniverse()` for Git-backed snapshots
- Enables time-travel and state reconstruction

## Error Handling

All errors trigger automatic rollback:

```javascript
try {
  const receipt = await applyCapsule(store, invalidCapsule);
} catch (error) {
  // Store state unchanged - rollback successful
  console.error(error.message); // "applyCapsule failed (rolled back): ..."
}
```

**Error Types**:
- `TypeError`: Invalid input (capsule structure, parameters)
- `Error`: Operation failure (quad errors, hash errors)
- Rollback failure throws `CRITICAL: Rollback failed` with details

## Performance

**Target Metrics** (from PLAN.md):
- Apply Time: <10ms per capsule (100 quads)
- Query Time: <5ms for simple SELECT
- Rollback Time: <5ms
- Receipt Hash: <2ms (BLAKE3 WASM)

## License

MIT

---

**Implementation Complete**: 2025-12-26
**Ready for Integration**: Agent 1 (Orchestrator)
