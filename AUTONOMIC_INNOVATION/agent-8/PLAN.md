# Agent 8: Store Adapter & Atomic Apply

**Role**: Implement atomic RDF store operations with capsule delta application and receipt generation

**Status**: Design Phase
**Created**: 2025-12-26
**Agent**: Backend API Developer (Agent 8)

---

## Overview

This module provides atomic application of capsule deltas to the RDF store with:
- **Atomicity**: All-or-nothing semantics (ACID compliance)
- **Receipts**: Hash-based verification with parent chain linking
- **Idempotence**: Replay-safe operations
- **Snapshot Integration**: Optional freeze/restore via @unrdf/kgc-4d

---

## Files to Create

### Core Implementation (`./AUTONOMIC_INNOVATION/agent-8/src/`)

#### 1. `store.mjs` - Store Abstraction
Wraps Oxigraph with atomic operations and capsule-aware methods.

**Exports**:
```javascript
/**
 * AtomicStore - Extends UnrdfStore with capsule application
 * @class
 */
export class AtomicStore extends UnrdfStore {
  constructor(options)
  async applyCapsule(capsule) → receipt
  async query(sparql, options) → results
  async insert(quads) → void
  async delete(quads) → void
  async snapshot(label) → freezeData
  getReceipts() → receipt[]
  getLastReceipt() → receipt | null
}

/**
 * Create atomic store instance
 * @param {Object} [options] - Store configuration
 * @param {string} [options.nodeId] - Node ID for receipt chain
 * @param {boolean} [options.enableSnapshots=false] - Enable KGC-4D integration
 * @returns {AtomicStore}
 */
export function createAtomicStore(options = {})
```

**Key Features**:
- Inherits from `UnrdfStore` (from `@unrdf/core`)
- Tracks receipt chain (parent hash → child hash)
- Optional Git-backed snapshots via `@unrdf/kgc-4d`
- Named graphs: `<urn:autonomic:universe>` for current state

---

#### 2. `apply.mjs` - Atomic Capsule Application

**Exports**:
```javascript
/**
 * Apply capsule deltas atomically to store
 * @param {AtomicStore} store - Target store
 * @param {Object} capsule - Capsule from Agent 2/3
 * @param {Object} capsule.delta - Delta operations
 * @param {Array} capsule.delta.add - Quads to add
 * @param {Array} capsule.delta.del - Quads to delete
 * @param {Object} [capsule.metadata] - Optional metadata
 * @param {Object} [options] - Apply options
 * @param {string} [options.parentHash] - Parent receipt hash for chain
 * @returns {Promise<Object>} Receipt { hash, timestamp, success, parentHash, stats }
 * @throws {Error} If operation fails (with rollback)
 */
export async function applyCapsule(store, capsule, options = {})

/**
 * Apply multiple capsules sequentially (batched)
 * @param {AtomicStore} store - Target store
 * @param {Array<Object>} capsules - Array of capsules
 * @returns {Promise<Array<Object>>} Array of receipts
 */
export async function applyBatch(store, capsules)

/**
 * Validate capsule structure before apply
 * @param {Object} capsule - Capsule to validate
 * @returns {boolean} True if valid
 * @throws {TypeError} If capsule is malformed
 */
export function validateCapsule(capsule)
```

**Algorithm** (Atomic Apply):
1. **Validate Input**: Check capsule structure (Zod schema)
2. **Begin Transaction**: Capture rollback state (quad snapshots)
3. **Apply Deletes**: Remove `delta.del` quads from Universe graph
4. **Apply Adds**: Insert `delta.add` quads to Universe graph
5. **Generate Receipt**:
   - Timestamp (nanosecond precision via `@unrdf/kgc-4d/time`)
   - Hash deltas + timestamp (BLAKE3 via `hash-wasm`)
   - Link to parent hash (if provided)
   - Store metadata (quad counts, capsule ID)
6. **Commit**: If no errors, return receipt
7. **Rollback**: On ANY error, restore state and throw

**Receipt Format**:
```javascript
{
  hash: "blake3_hash_of_operation",
  timestamp: "2025-12-26T12:00:00.123456789Z",
  success: true,
  parentHash: "previous_receipt_hash",
  stats: {
    added: 5,
    deleted: 2,
    capsuleId: "capsule_uuid"
  }
}
```

---

#### 3. `query.mjs` - Query Wrapper

**Exports**:
```javascript
/**
 * Query store with SPARQL
 * @param {AtomicStore} store - Store to query
 * @param {string} sparql - SPARQL query string
 * @param {Object} [options] - Query options
 * @returns {Promise<Array>} Query results
 */
export async function queryStore(store, sparql, options = {})

/**
 * Query with automatic graph context
 * @param {AtomicStore} store - Store to query
 * @param {string} sparql - SPARQL query (FROM clause auto-injected)
 * @returns {Promise<Array>} Results from Universe graph only
 */
export async function queryUniverse(store, sparql)

/**
 * Count quads in store
 * @param {AtomicStore} store - Store to query
 * @param {string} [graph] - Optional graph filter
 * @returns {number} Quad count
 */
export function countQuads(store, graph)
```

---

#### 4. `freeze.mjs` - Snapshot Integration (Optional)

**Exports**:
```javascript
/**
 * Create snapshot of current store state
 * @param {AtomicStore} store - Store to snapshot
 * @param {string} label - Human-readable label
 * @param {Object} gitBackbone - GitBackbone from @unrdf/kgc-4d
 * @returns {Promise<Object>} Freeze receipt from KGC-4D
 */
export async function snapshotStore(store, label, gitBackbone)

/**
 * Restore store from snapshot
 * @param {AtomicStore} store - Target store (will be cleared)
 * @param {string} gitRef - Git reference from freeze receipt
 * @param {Object} gitBackbone - GitBackbone instance
 * @returns {Promise<void>}
 */
export async function restoreSnapshot(store, gitRef, gitBackbone)

/**
 * Check if snapshots are enabled
 * @param {AtomicStore} store - Store to check
 * @returns {boolean}
 */
export function snapshotsEnabled(store)
```

**Integration Notes**:
- Uses `@unrdf/kgc-4d/freeze` for `freezeUniverse()` and `reconstructState()`
- Requires `gitBackbone` instance (from `@unrdf/kgc-4d/git`)
- Only enabled if `options.enableSnapshots = true` in `createAtomicStore()`

---

#### 5. `index.mjs` - Main Entry Point

**Exports**:
```javascript
// Store
export { AtomicStore, createAtomicStore } from './store.mjs';

// Apply
export {
  applyCapsule,
  applyBatch,
  validateCapsule
} from './apply.mjs';

// Query
export {
  queryStore,
  queryUniverse,
  countQuads
} from './query.mjs';

// Freeze (optional)
export {
  snapshotStore,
  restoreSnapshot,
  snapshotsEnabled
} from './freeze.mjs';

// Utilities
export { hashReceipt, verifyReceipt } from './utils.mjs';
```

---

#### 6. `utils.mjs` - Receipt Utilities

**Exports**:
```javascript
/**
 * Hash receipt data (deterministic)
 * @param {Object} data - Data to hash
 * @returns {Promise<string>} BLAKE3 hash
 */
export async function hashReceipt(data)

/**
 * Verify receipt hash matches data
 * @param {Object} receipt - Receipt to verify
 * @param {Object} data - Original data
 * @returns {Promise<boolean>} True if valid
 */
export async function verifyReceipt(receipt, data)

/**
 * Serialize delta for hashing (canonical)
 * @param {Object} delta - Capsule delta
 * @returns {string} Canonical N-Quads
 */
export function serializeDelta(delta)

/**
 * Deserialize delta from N-Quads
 * @param {string} nquads - N-Quads string
 * @returns {Object} Delta object
 */
export function deserializeDelta(nquads)
```

---

### Tests (`./AUTONOMIC_INNOVATION/agent-8/test/`)

#### 1. `apply.test.mjs` - Atomic Apply Tests

**Test Cases**:
```javascript
import { describe, it, expect } from 'vitest';
import { createAtomicStore, applyCapsule } from '../src/index.mjs';
import { dataFactory } from '@unrdf/oxigraph';

describe('Atomic Apply', () => {
  it('applies simple add-only capsule', async () => {
    // Create store
    const store = createAtomicStore();

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

    // Apply
    const receipt = await applyCapsule(store, capsule);

    // Verify
    expect(receipt.success).toBe(true);
    expect(receipt.hash).toBeTruthy();
    expect(receipt.stats.added).toBe(1);
    expect(store.size()).toBe(1);
  });

  it('applies delete operations', async () => {
    // Setup: add quad first
    const store = createAtomicStore();
    const quad = dataFactory.quad(
      dataFactory.namedNode('http://ex.org/bob'),
      dataFactory.namedNode('http://ex.org/age'),
      dataFactory.literal('30', dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#integer'))
    );
    store.add(quad);

    // Define delete capsule
    const capsule = {
      delta: {
        add: [],
        del: [quad]
      }
    };

    // Apply
    const receipt = await applyCapsule(store, capsule);

    // Verify deletion
    expect(receipt.success).toBe(true);
    expect(receipt.stats.deleted).toBe(1);
    expect(store.size()).toBe(0);
  });

  it('rolls back on error (atomicity)', async () => {
    const store = createAtomicStore();

    // Invalid capsule (missing predicate)
    const badCapsule = {
      delta: {
        add: [{ subject: dataFactory.namedNode('http://ex.org/bad') }],
        del: []
      }
    };

    // Expect failure
    await expect(applyCapsule(store, badCapsule)).rejects.toThrow();

    // Verify no partial state
    expect(store.size()).toBe(0);
  });

  it('generates hash chain with parent', async () => {
    const store = createAtomicStore();

    const capsule1 = {
      delta: { add: [/* quad1 */], del: [] }
    };
    const capsule2 = {
      delta: { add: [/* quad2 */], del: [] }
    };

    const receipt1 = await applyCapsule(store, capsule1);
    const receipt2 = await applyCapsule(store, capsule2, {
      parentHash: receipt1.hash
    });

    expect(receipt2.parentHash).toBe(receipt1.hash);
  });
});
```

**Coverage Requirements**:
- Simple add operations
- Simple delete operations
- Mixed add + delete
- Rollback on error (atomicity)
- Receipt hash chain
- Invalid capsule handling

---

#### 2. `replay.test.mjs` - Replay & Idempotence Tests

**Test Cases**:
```javascript
describe('Replay & Idempotence', () => {
  it('replays capsules deterministically', async () => {
    const store1 = createAtomicStore();
    const store2 = createAtomicStore();

    const capsules = [
      { delta: { add: [quad1], del: [] } },
      { delta: { add: [quad2], del: [] } },
      { delta: { add: [], del: [quad1] } }
    ];

    // Apply to both stores
    for (const capsule of capsules) {
      await applyCapsule(store1, capsule);
      await applyCapsule(store2, capsule);
    }

    // Compare final states (serialize + hash)
    const state1 = await serializeStore(store1);
    const state2 = await serializeStore(store2);

    expect(state1).toEqual(state2);
  });

  it('handles undo-replay sequence', async () => {
    const store = createAtomicStore();

    // Add quad
    const addCapsule = {
      delta: { add: [quad1], del: [] }
    };
    await applyCapsule(store, addCapsule);
    expect(store.size()).toBe(1);

    // Undo (delete)
    const undoCapsule = {
      delta: { add: [], del: [quad1] }
    };
    await applyCapsule(store, undoCapsule);
    expect(store.size()).toBe(0);

    // Replay (add again)
    await applyCapsule(store, addCapsule);
    expect(store.size()).toBe(1);
  });

  it('concurrent capsules (sequential apply)', async () => {
    const store = createAtomicStore();

    // Apply 3 capsules in sequence
    const capsules = [
      { delta: { add: [quad1], del: [] } },
      { delta: { add: [quad2], del: [] } },
      { delta: { add: [quad3], del: [] } }
    ];

    const receipts = await applyBatch(store, capsules);

    // Verify receipt chain
    expect(receipts[0].parentHash).toBeUndefined();
    expect(receipts[1].parentHash).toBe(receipts[0].hash);
    expect(receipts[2].parentHash).toBe(receipts[1].hash);

    expect(store.size()).toBe(3);
  });
});
```

---

#### 3. `query.test.mjs` - Query Operations

**Test Cases**:
```javascript
describe('Query Operations', () => {
  it('queries after apply', async () => {
    const store = createAtomicStore();

    const capsule = {
      delta: {
        add: [
          dataFactory.quad(
            dataFactory.namedNode('http://ex.org/alice'),
            dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'),
            dataFactory.literal('Alice')
          )
        ],
        del: []
      }
    };

    await applyCapsule(store, capsule);

    // Query
    const results = await queryUniverse(store, `
      SELECT ?name WHERE {
        ?person <http://xmlns.com/foaf/0.1/name> ?name .
      }
    `);

    expect(results).toHaveLength(1);
    expect(results[0].name.value).toBe('Alice');
  });

  it('counts quads correctly', async () => {
    const store = createAtomicStore();

    const capsule = {
      delta: {
        add: [quad1, quad2, quad3],
        del: []
      }
    };

    await applyCapsule(store, capsule);

    expect(countQuads(store)).toBe(3);
  });
});
```

---

#### 4. `freeze.test.mjs` - Snapshot Integration (Optional)

**Test Cases**:
```javascript
describe('Snapshot Integration', () => {
  it('creates snapshot after apply', async () => {
    const store = createAtomicStore({ enableSnapshots: true });
    const git = new GitBackbone('/tmp/agent8-test');

    // Apply capsule
    const capsule = { delta: { add: [quad1], del: [] } };
    await applyCapsule(store, capsule);

    // Snapshot
    const freezeReceipt = await snapshotStore(store, 'v1', git);

    expect(freezeReceipt.git_ref).toBeTruthy();
    expect(freezeReceipt.universe_hash).toBeTruthy();
  });

  it('restores from snapshot', async () => {
    const store = createAtomicStore({ enableSnapshots: true });
    const git = new GitBackbone('/tmp/agent8-restore');

    // Setup + snapshot
    await applyCapsule(store, { delta: { add: [quad1], del: [] } });
    const freeze = await snapshotStore(store, 'v1', git);

    // Clear store
    store.clear();
    expect(store.size()).toBe(0);

    // Restore
    await restoreSnapshot(store, freeze.git_ref, git);

    expect(store.size()).toBe(1);
  });
});
```

---

## Data Structures

### Capsule Schema (Zod)

```javascript
import { z } from 'zod';

// RDF Term schemas
const NamedNodeSchema = z.object({
  termType: z.literal('NamedNode'),
  value: z.string().url()
});

const BlankNodeSchema = z.object({
  termType: z.literal('BlankNode'),
  value: z.string()
});

const LiteralSchema = z.object({
  termType: z.literal('Literal'),
  value: z.string(),
  language: z.string().optional(),
  datatype: NamedNodeSchema.optional()
});

const RdfTermSchema = z.union([
  NamedNodeSchema,
  BlankNodeSchema,
  LiteralSchema
]);

const QuadSchema = z.object({
  subject: z.union([NamedNodeSchema, BlankNodeSchema]),
  predicate: NamedNodeSchema,
  object: RdfTermSchema,
  graph: NamedNodeSchema.optional()
});

export const CapsuleSchema = z.object({
  delta: z.object({
    add: z.array(QuadSchema),
    del: z.array(QuadSchema)
  }),
  metadata: z.object({
    id: z.string().uuid().optional(),
    label: z.string().optional(),
    timestamp: z.string().optional()
  }).optional()
});
```

---

## Dependencies

### Required (from existing packages)
- `@unrdf/core` - UnrdfStore base class, RDF operations
- `@unrdf/oxigraph` - createStore, dataFactory, OxigraphStore
- `hash-wasm` - BLAKE3 hashing (deterministic)
- `zod` - Schema validation

### Optional (for snapshots)
- `@unrdf/kgc-4d` - freezeUniverse, reconstructState, GitBackbone
- `@unrdf/kgc-4d/time` - now(), toISO() for nanosecond timestamps

### Dev Dependencies
- `vitest` - Testing framework
- `@unrdf/test-utils` - Test fixtures and helpers

---

## Implementation Strategy

### Phase 1: Core Store & Apply (Essential)
1. **Day 1**: Implement `AtomicStore` class and `createAtomicStore()`
2. **Day 1**: Implement `applyCapsule()` with transaction + rollback
3. **Day 1**: Write 4 core tests (apply, delete, rollback, chain)

### Phase 2: Query & Utilities (Essential)
4. **Day 2**: Implement `queryStore()` and `queryUniverse()`
5. **Day 2**: Implement receipt hashing utilities
6. **Day 2**: Write replay and query tests

### Phase 3: Snapshot Integration (Optional)
7. **Day 3**: Implement `snapshotStore()` and `restoreSnapshot()`
8. **Day 3**: Write snapshot tests
9. **Day 3**: Integration test with KGC-4D

---

## Atomic Apply Algorithm (Detailed)

```javascript
async function applyCapsule(store, capsule, options = {}) {
  // 1. VALIDATE INPUT
  const validated = CapsuleSchema.parse(capsule);

  // 2. CAPTURE ROLLBACK STATE
  const addedQuads = [];
  const deletedQuads = [];
  const originalSize = store.size();

  try {
    // 3. APPLY DELETES FIRST (order matters for idempotence)
    for (const delta of validated.delta.del) {
      const quad = dataFactory.quad(
        delta.subject,
        delta.predicate,
        delta.object,
        dataFactory.namedNode('<urn:autonomic:universe>')
      );

      // Track before delete
      const exists = [...store.match(
        quad.subject,
        quad.predicate,
        quad.object,
        quad.graph
      )];

      if (exists.length > 0) {
        deletedQuads.push(...exists);
        store.delete(quad);
      }
    }

    // 4. APPLY ADDS
    for (const delta of validated.delta.add) {
      const quad = dataFactory.quad(
        delta.subject,
        delta.predicate,
        delta.object,
        dataFactory.namedNode('<urn:autonomic:universe>')
      );

      store.add(quad);
      addedQuads.push(quad);
    }

    // 5. GENERATE RECEIPT
    const timestamp = toISO(now());
    const deltaHash = await hashDelta(validated.delta, timestamp);

    const receipt = {
      hash: deltaHash,
      timestamp: timestamp,
      success: true,
      parentHash: options.parentHash || null,
      stats: {
        added: addedQuads.length,
        deleted: deletedQuads.length,
        capsuleId: validated.metadata?.id || null
      }
    };

    // 6. STORE RECEIPT IN STORE (for chain tracking)
    store._receipts = store._receipts || [];
    store._receipts.push(receipt);

    return receipt;

  } catch (error) {
    // 7. ROLLBACK ON ERROR
    // Re-add deleted quads
    for (const quad of deletedQuads) {
      try {
        store.add(quad);
      } catch {}
    }

    // Remove added quads
    for (const quad of addedQuads) {
      try {
        store.delete(quad);
      } catch {}
    }

    // Verify rollback
    if (store.size() !== originalSize) {
      throw new Error(
        `CRITICAL: Rollback failed - store size ${store.size()} != original ${originalSize}`
      );
    }

    throw new Error(`applyCapsule failed (rolled back): ${error.message}`);
  }
}

// Hash delta deterministically
async function hashDelta(delta, timestamp) {
  // Serialize to N-Quads (canonical ordering)
  const addNQuads = delta.add.map(quadToNQuad).sort().join('\n');
  const delNQuads = delta.del.map(quadToNQuad).sort().join('\n');

  const canonical = JSON.stringify({
    add: addNQuads,
    del: delNQuads,
    timestamp: timestamp
  });

  return await blake3(canonical);
}
```

---

## Test Coverage Requirements

### Minimum Coverage: 80%
- **Core Apply**: 100% (critical path)
- **Query**: 80%
- **Freeze**: 60% (optional feature)
- **Utils**: 90%

### Critical Test Scenarios
1. ✅ Simple add operation
2. ✅ Simple delete operation
3. ✅ Mixed add + delete
4. ✅ Rollback on error (atomicity)
5. ✅ Receipt hash chain
6. ✅ Replay determinism
7. ✅ Query after apply
8. ✅ Snapshot + restore (if enabled)

---

## Named Graphs Strategy

### Graph URIs
- **Universe**: `<urn:autonomic:universe>` - Current state (all applied capsules)
- **Receipts**: `<urn:autonomic:receipts>` - Receipt metadata (optional, for SPARQL query)
- **Metadata**: `<urn:autonomic:meta>` - Store config and pointers

### Why Separate Graphs?
- **Isolation**: Universe state separate from metadata
- **Query Efficiency**: Filter by graph for faster queries
- **Cleanup**: Clear metadata without affecting universe
- **KGC-4D Compatibility**: Aligns with GRAPHS.UNIVERSE pattern

---

## Success Criteria

### Functional
- [ ] `applyCapsule()` works with add/delete operations
- [ ] Rollback on error (no partial state)
- [ ] Receipt hash chain validated
- [ ] Query returns correct results after apply
- [ ] Replay produces identical state

### Non-Functional
- [ ] All tests pass (4+ minimum)
- [ ] 80%+ code coverage
- [ ] JSDoc on all exports (100%)
- [ ] Zod validation on inputs
- [ ] No imports from `'n3'` (use `@unrdf/oxigraph` only)

### Integration
- [ ] Works with Agent 2 capsules (plan)
- [ ] Works with Agent 3 lenses (compile)
- [ ] Works with KGC-4D freeze (optional)
- [ ] Exports align with Agent 1 orchestrator

---

## Performance Targets

- **Apply Time**: <10ms per capsule (100 quads)
- **Query Time**: <5ms for simple SELECT (SPARQL)
- **Rollback Time**: <5ms (restore state)
- **Receipt Hash**: <2ms (BLAKE3 via WASM)

---

## Error Handling

### Error Types
1. **Validation Errors**: Invalid capsule structure (Zod)
   - Throw `TypeError` with detailed message
2. **Store Errors**: Oxigraph failures (quad operations)
   - Rollback + throw `Error`
3. **Receipt Errors**: Hash generation failures
   - Rollback + throw `Error`
4. **Snapshot Errors**: Git/KGC-4D failures (optional)
   - Throw `Error` (no rollback, snapshot is separate)

### Error Messages
- **Clear**: Include operation name and reason
- **Actionable**: Suggest fix if possible
- **Safe**: Never expose internal state in production

---

## Adversarial Validation Checklist

### Before Declaring Complete
- [ ] Did I RUN all tests? (not just write them)
- [ ] Did I verify rollback works? (show test output)
- [ ] Did I check receipt hash determinism? (run twice, compare)
- [ ] Did I verify KGC-4D integration? (if enabled)
- [ ] Can I replay capsules and get identical state? (evidence)

### Evidence Required
- Test output showing ✅ for all tests
- Coverage report showing ≥80%
- Receipt hash comparison (2 runs, same capsules → same hash)
- File count: `ls -1 src/*.mjs | wc -l` (expect 6)
- Import validation: `grep "from 'n3'" src/*.mjs` (expect 0 results)

---

## Open Questions (To Resolve)

1. **Capsule ID**: Does Agent 2 provide `capsule.id`? Or generate here?
   - **Decision**: Accept optional `metadata.id`, generate UUID if missing
2. **Graph Strategy**: Single Universe graph or multiple?
   - **Decision**: Single `<urn:autonomic:universe>` for simplicity
3. **Receipt Storage**: In-memory only or persist to RDF?
   - **Decision**: Both - in-memory array + optional RDF graph
4. **Snapshot Frequency**: When to trigger snapshots?
   - **Decision**: Manual only (via `snapshotStore()` call)

---

## References

- **KGC-4D Store Pattern**: `/home/user/unrdf/packages/kgc-4d/src/store.mjs`
- **KGC-4D Freeze Pattern**: `/home/user/unrdf/packages/kgc-4d/src/freeze.mjs`
- **UnrdfStore Base Class**: `/home/user/unrdf/packages/core/src/rdf/unrdf-store.mjs`
- **Agent 1 Orchestrator**: `/home/user/unrdf/AUTONOMIC_INNOVATION/agent-1/PLAN.md`
- **Inventory**: `/home/user/unrdf/AUTONOMIC_INNOVATION/INVENTORY.md`

---

## Next Steps (Implementation)

1. Create `src/` and `test/` directories
2. Implement `store.mjs` and `createAtomicStore()`
3. Implement `apply.mjs` with atomic transaction
4. Write 4 core tests and verify they pass
5. Implement `query.mjs` and utilities
6. (Optional) Implement `freeze.mjs` for snapshots
7. Run OTEL validation (≥80/100 score)
8. Submit to Agent 1 for integration

---

**End of Plan** - Ready for implementation.
