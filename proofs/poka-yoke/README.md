# Poka-Yoke Proofs - UNRDF v6 Mistake-Proofing Demonstrations

This directory contains **runnable proof tests** that demonstrate v6 poka-yoke (mistake-proofing) patterns. Each proof validates a specific pattern that makes invalid states impossible.

## Quick Start

```bash
# Run all proofs
cd /home/user/unrdf/proofs/poka-yoke
node 01-sealed-universe.test.mjs
node 02-receipt-immutability.test.mjs
node 03-branded-ids.test.mjs
node 04-builder-pattern.test.mjs
node 05-atomic-delta.test.mjs

# Or run all at once
for test in *.test.mjs; do node "$test"; done
```

**Expected Output**: All tests pass in <350ms total

---

## Proofs

### 01. Sealed Universe State Machine
**File**: `01-sealed-universe.test.mjs`  
**Pattern**: State machine enforcement (MUTABLE → FROZEN → SEALED)  
**Proves**: Terminal SEALED state blocks all mutations  
**Runtime**: <100ms

**What it demonstrates**:
- MUTABLE state allows `appendEvent()`, `admit()`
- FROZEN state blocks mutations, allows time-travel
- SEALED state is terminal (no operations allowed)
- Invalid transitions throw immediately (cannot freeze twice, cannot skip states)

**Key Learning**:
```javascript
const sm = new UniverseStateMachine('FROZEN');
sm.guardMutableOperation('appendEvent');  // ← Throws: Universe is FROZEN
```

---

### 02. Receipt Immutability
**File**: `02-receipt-immutability.test.mjs`  
**Pattern**: Immutability by default (Object.freeze)  
**Proves**: Frozen objects prevent tampering  
**Runtime**: <50ms

**What it demonstrates**:
- Unfrozen objects allow tampering (vulnerability)
- `Object.freeze()` prevents property mutation
- Strict mode throws `TypeError` on mutation attempts
- Deep freeze required for nested objects

**Key Learning**:
```javascript
const receipt = Object.freeze({ universe_hash: 'abc123' });
receipt.universe_hash = 'TAMPERED';  // ← Silently fails (or throws in strict mode)
assert.strictEqual(receipt.universe_hash, 'abc123');  // ← Still original
```

---

### 03. Branded ID Types
**File**: `03-branded-ids.test.mjs`  
**Pattern**: Type-level guards (Zod `.brand()`)  
**Proves**: Branded types prevent ID confusion  
**Runtime**: <50ms

**What it demonstrates**:
- ReceiptId, EventId, UniverseId are distinct types
- Cannot pass ReceiptId where EventId expected (compile error in TS)
- Zod validates at parse boundary
- Factory functions return branded IDs

**Key Learning**:
```javascript
const ReceiptIdSchema = z.string().uuid().brand('ReceiptId');
const EventIdSchema = z.string().uuid().brand('EventId');

const receiptId = ReceiptIdSchema.parse('550e8400-...');
// receiptId and eventId are DIFFERENT types (TS catches mix-up)
```

---

### 04. Builder Pattern
**File**: `04-builder-pattern.test.mjs`  
**Pattern**: Constrained construction  
**Proves**: Builder enforces required fields + immutability  
**Runtime**: <50ms

**What it demonstrates**:
- Cannot build without required fields
- Each field validated at entry (fail-fast)
- Builder produces frozen (immutable) objects
- Fluent API (chainable methods)

**Key Learning**:
```javascript
const receipt = new ReceiptBuilder()
  .withId(id)
  .withTimestamp(t_ns)
  .withUniverseHash(hash)
  .withPreviousHash(prevHash)
  .build();  // ← Frozen, validated

// Cannot construct receipt without all required fields
```

---

### 05. Atomic Delta Application
**File**: `05-atomic-delta.test.mjs`  
**Pattern**: Transaction semantics with rollback  
**Proves**: All-or-none delta application  
**Runtime**: <100ms

**What it demonstrates**:
- Successful deltas apply ALL operations
- Failed deltas roll back ALL operations (no partial state)
- Transaction wrapper enforces atomicity
- Receipt issued on both success and denial

**Key Learning**:
```javascript
const snapshot = new Set(store);
try {
  for (const op of delta.operations) {
    applyOperation(store, op);
  }
} catch (error) {
  // ROLLBACK: No partial state ever exists
  store.clear();
  snapshot.forEach(item => store.add(item));
}
```

---

## V6 Contract (Guarantees)

These proofs demonstrate the **v6 contract** - the guarantees UNRDF v6 provides:

### 1. Type Safety
- [ ] **Immutability**: All receipts, deltas are `Readonly<T>` + `Object.freeze()`
- [ ] **State Validity**: Invalid state transitions won't compile (e.g., `frozenUniverse.appendEvent()` is a type error)
- [ ] **Branded IDs**: ReceiptId, EventId, UniverseId are distinct types

### 2. Runtime Safety
- [ ] **Atomic Operations**: All deltas are all-or-none (rollback on any failure)
- [ ] **No Partial State**: Store is always consistent
- [ ] **Monotonic Time**: Timestamps never go backward

### 3. Construction Safety
- [ ] **Builder Pattern**: Cannot construct invalid objects
- [ ] **Required Fields**: Builder enforces at build time
- [ ] **Validation**: Each field validated at entry

---

## Pattern Summary

| Pattern | Purpose | Proof File | Impact |
|---------|---------|-----------|--------|
| State Machine | Enforce valid transitions | 01 | Eliminates "invalid operation on frozen universe" |
| Object.freeze() | Prevent tampering | 02 | Eliminates receipt mutation footgun |
| Branded Types | Prevent ID confusion | 03 | Eliminates "wrong ID type" bugs |
| Builder | Constrained construction | 04 | Eliminates "missing required field" errors |
| Atomic Transactions | No partial state | 05 | Eliminates inconsistent store state |

---

## Running Individual Tests

Each proof is a standalone Node.js test file using the built-in `node:test` module (Node.js 18+):

```bash
# Run with detailed output
node --test 01-sealed-universe.test.mjs

# Run with TAP output
node --test-reporter=tap 01-sealed-universe.test.mjs
```

---

## Integration with Main Test Suite

These proofs can be integrated into the main vitest suite:

```javascript
// In test/poka-yoke-integration.test.mjs
import { execSync } from 'node:child_process';
import { test } from 'vitest';

test('Poka-Yoke Proofs Pass', () => {
  const proofs = [
    'proofs/poka-yoke/01-sealed-universe.test.mjs',
    'proofs/poka-yoke/02-receipt-immutability.test.mjs',
    // ... etc
  ];
  
  for (const proof of proofs) {
    execSync(`node ${proof}`, { stdio: 'inherit' });
  }
});
```

---

## Expected Output

When all proofs pass, you should see:

```
✅ Proof 01 PASSED: State machine prevents invalid transitions
   - MUTABLE → FROZEN → SEALED enforced
   - Invalid transitions impossible (throw immediately)
   - Terminal SEALED state blocks all mutations

✅ Proof 02 PASSED: Object.freeze() prevents tampering
   - Frozen objects are immutable
   - Must use deepFreeze for nested objects
   - V6 Contract: ALL receipts must be Object.freeze()d

✅ Proof 03 PASSED: Branded types prevent ID confusion
   - ReceiptId, EventId, UniverseId are distinct types
   - Validation at parse time prevents wrong ID type
   - V6 Contract: Use branded types for all IDs

✅ Proof 04 PASSED: Builder pattern prevents invalid construction
   - Required fields enforced at build time
   - Each field validated immediately
   - Output is immutable (Object.freeze)
   - V6 Contract: Use builders for all complex objects

✅ Proof 05 PASSED: Atomic delta application (all-or-none)
   - Successful deltas apply all operations
   - Failed deltas roll back ALL operations
   - No partial state ever exists
   - V6 Contract: All mutations are atomic transactions
```

---

## Next Steps

1. **Review** the analysis document: `/home/user/unrdf/docs/poka-yoke-v6-analysis/POKA-YOKE-ANALYSIS.md`
2. **Prioritize** v6 improvements (see Recommendations section)
3. **Implement** high-priority patterns (Builder, Branded IDs, Type-Level State Guards)
4. **Measure** success (compile-time error rate, runtime error rate, developer experience)

---

**Generated**: 2025-12-28  
**Author**: Claude (Poka-Yoke Engineer)  
**Purpose**: Demonstrate v6 mistake-proofing patterns with runnable proofs
