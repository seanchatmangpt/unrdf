# Agent 8: Store Adapter and Atomic Apply - Implementation Plan

## Mission
Integrate with @unrdf/oxigraph and @unrdf/kgc-4d to apply capsule deltas atomically with rollback guarantees, auditable receipts, and deterministic replay.

## Files to Create

### 1. `/home/user/unrdf/AUTONOMIC_INNOVATION/agent-8/PLAN.md`
This file - documents all files, functions, and test cases.

### 2. `/home/user/unrdf/AUTONOMIC_INNOVATION/agent-8/store-adapter.mjs`
Store adapter abstraction with transaction semantics.

**Functions**:
- `createStoreAdapter(storeImpl?)` → StoreAdapter
  - storeImpl: optional custom store (default: use @unrdf/oxigraph createStore())
  - Return methods: { addQuad, deleteQuad, queryQuads, transaction }
  - Provides unified interface for quad operations

- `transaction(capsule, adapter)` → { success: boolean, receipt, appliedAt }
  - Apply all quads in capsule.delta atomically
  - If any quad fails, entire transaction rolls back
  - Return receipt with timestamp & hash
  - Timestamp: nanosecond precision (use @unrdf/kgc-4d time.mjs now())
  - Hash: BLAKE3 (use hash-wasm)

- `replayFromReceipt(receipt, store)` → { quads, receipt }
  - Given a receipt, reconstruct which quads were applied
  - Useful for auditability and verification
  - Deterministic: same receipt → same quads always
  - Returns original receipt + reconstructed quads array

### 3. `/home/user/unrdf/AUTONOMIC_INNOVATION/agent-8/atomic.mjs`
Atomic application with KGC-4D store integration.

**Functions**:
- `atomicApply(capsule, store)` → { receipt, appliedAt, hash, error? }
  - Apply capsule.delta to store atomically
  - Use @unrdf/oxigraph store interface (add/delete)
  - Record receipt with @unrdf/kgc-4d freeze utilities
  - If store is frozen, receipt becomes immutable proof
  - Return: { receipt, appliedAt (nanoseconds), hash, error }
  - On error: rollback + return error field

- `verifyAtomicity(capsule, receipt, store)` → boolean
  - Query store for all quads matching receipt
  - Verify: stored quads match capsule.delta exactly
  - Return true if match, false if divergence
  - Check quad count, subject/predicate/object values

### 4. `/home/user/unrdf/AUTONOMIC_INNOVATION/agent-8/integration.mjs`
Integration with KGC-4D freeze and Oxigraph store.

**Functions**:
- `integrateWithKGC4D(capsule)` → { frozenSnapshot, hash }
  - Use @unrdf/kgc-4d freeze.mjs to create immutable snapshot
  - Create receipt object from capsule application
  - Return frozen state & hash
  - Does NOT mutate store (read-only snapshot)

- `integrateWithOxigraph(capsule, store)` → StoreResult
  - Use @unrdf/oxigraph createStore() for store instance
  - Convert capsule.delta to oxigraph quads
  - Apply quads with rollback on failure
  - Return: { success, error?, quadCount }

### 5. `/home/user/unrdf/AUTONOMIC_INNOVATION/agent-8/index.mjs`
Public API exports.

**Exports**:
- All functions from store-adapter.mjs
- All functions from atomic.mjs
- All functions from integration.mjs

### 6. `/home/user/unrdf/AUTONOMIC_INNOVATION/agent-8/test.mjs`
Comprehensive test suite using Node.js built-in test runner.

**Test Cases**:
1. **Atomic apply - all or nothing**
   - Capsule with 10 quads → all applied or none
   - Verify quad count matches capsule.delta length
   - Check store state before/after

2. **Replay from receipt**
   - Apply capsule, get receipt
   - Replay from receipt → exact same quads
   - Deterministic: run 3 times → identical result

3. **Atomicity verification**
   - After apply, verify store contains exact capsule.delta
   - No extra quads, no missing quads
   - Subject/predicate/object values match exactly

4. **Large transaction performance**
   - Capsule with 1000 quads → applied atomically in <500ms
   - Measure with performance.now() or process.hrtime()
   - Verify all quads present after apply

5. **Deterministic receipt hash**
   - Same capsule applied twice → same receipt hash
   - Hash stability (run 100 times → identical)
   - Receipt includes nanosecond timestamp

6. **Rollback on failure**
   - Capsule with invalid quad (e.g., malformed URI)
   - Transaction fails and rolls back
   - Store state unchanged after rollback

7. **Integration with KGC-4D**
   - Apply capsule to KGCStore instance
   - Verify receipt recorded in EventLog graph
   - Check nanosecond timestamp precision

## Core Invariants

1. **Atomicity**: All deltas applied or none (ACID transaction)
2. **Rollback**: On failure, store returns to pre-transaction state
3. **Determinism**: Same capsule → same receipt hash (always)
4. **Auditability**: Receipt enables full reconstruction of applied quads
5. **Performance**: 1000 quads applied in <500ms
6. **Immutability**: Receipts are tamper-evident (hash verification)

## Atomicity Guarantees

### Transaction Semantics (ACID)
- **Atomicity**: All deltas succeed or all fail
- **Consistency**: Store invariants maintained
- **Isolation**: No partial states visible
- **Durability**: Receipt provides proof of application

### Rollback Mechanism
1. Track all added quads during transaction
2. Track all deleted quads during transaction
3. On error: reverse all operations in LIFO order
4. Restore store to exact pre-transaction state

### Receipt Structure
```javascript
{
  id: string,           // Unique receipt ID (UUID)
  t_ns: string,         // Nanosecond timestamp (BigInt as string)
  timestamp_iso: string, // ISO 8601 timestamp
  hash: string,         // BLAKE3 hash of applied deltas
  quadCount: number,    // Number of quads applied
  capsuleHash: string,  // Hash of source capsule
}
```

## Replay Mechanisms

### Deterministic Replay
- Receipt contains hash of applied deltas
- Hash enables verification of stored quads
- Replay reconstructs exact quad array from receipt
- Same receipt → same quads (always)

### Audit Trail
- Receipt ID enables lookup in EventLog
- Timestamp enables time-travel to exact state
- Hash enables tamper detection
- Quad count enables quick validation

## Dependencies

- `@unrdf/oxigraph` - Store implementation (createStore, dataFactory)
- `@unrdf/kgc-4d` - Time utilities (now, toISO), freeze utilities
- `hash-wasm` - BLAKE3 hashing (deterministic)
- `zod` - Schema validation (if needed)
- Node.js built-in `test` and `assert` for testing

## Integration Points

**For Agent 2 (Capsule IR)**:
- Receives capsule with delta array
- Applies deltas atomically
- Returns receipt for capsule application

**For Agent 1 (Orchestration)**:
- Provides atomicApply() for execution
- Provides verifyAtomicity() for validation
- Integrates with KGCStore for event logging

## Success Criteria

- [ ] All 3 modules implemented (store-adapter, atomic, integration)
- [ ] All 7 core functions implemented
- [ ] Test suite with 7+ test cases
- [ ] 100% test pass rate (timeout 5s)
- [ ] Atomicity verified (all-or-nothing)
- [ ] Rollback verified (state restored on failure)
- [ ] Deterministic receipt hashing (100 iterations identical)
- [ ] Large transaction <500ms (1000 quads)
- [ ] Zero OTEL in implementation (pure functions only)
- [ ] Full JSDoc type hints (100% coverage)

## Execution Timeline

Single-pass implementation:
1. Store adapter (store-adapter.mjs)
2. Atomic operations (atomic.mjs)
3. Integration layer (integration.mjs)
4. Public API (index.mjs)
5. Test suite (test.mjs)
6. Verification (run tests, check output)

Estimated: 1 message execution, verified completion.
