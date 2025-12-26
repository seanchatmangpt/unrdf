# Agent 2: Capsule IR Primitive - Implementation Plan

## Mission
Create a portable, content-addressed change program (Capsule) with deterministic hashing and tamper-evident receipt chains.

## Files to Create

### 1. `/home/user/unrdf/AUTONOMIC_INNOVATION/agent-2/PLAN.md`
This file - documents all files, functions, and test cases.

### 2. `/home/user/unrdf/AUTONOMIC_INNOVATION/agent-2/schema.mjs`
Zod schemas for Capsule data structure.

**Exports**:
- `IntentOpSchema` - Schema for intent operations
- `DeltaOpSchema` - Schema for RDF delta operations
- `GuardSchema` - Schema for guard constraints
- `ReceiptSchema` - Schema for tamper-evident receipt
- `CapsuleSchema` - Complete Capsule schema

### 3. `/home/user/unrdf/AUTONOMIC_INNOVATION/agent-2/capsule.mjs`
Core capsule operations (pure functions, no side effects).

**Functions**:
- `planCapsule(intent, profile?)` → Capsule
  - Accept intent operations array
  - Validate against profile if provided
  - Return capsule with intent and guard, empty delta & receipt

- `compileCapsuleToDeltas(capsule, lensMap)` → Capsule
  - Translate intent operations to RDF deltas using lens mappings
  - Deterministically order delta array
  - Return new capsule with delta field populated

- `verifyCapsule(capsule)` → { valid: boolean, errors: string[] }
  - Validate schema using Zod
  - Verify receipt chain (parent hashes match)
  - Check guard invariants not violated

- `applyCapsule(capsule)` → { receipt, appliedAt, hash }
  - Does NOT mutate capsule (pure function)
  - Generate new receipt object
  - Return receipt metadata for Agent 8 store application

### 4. `/home/user/unrdf/AUTONOMIC_INNOVATION/agent-2/canonicalization.mjs`
Deterministic canonicalization and cryptographic hashing.

**Functions**:
- `canonicalizeCapsule(capsule)` → string
  - Order intent array by (op, target, value) lexicographically
  - Order delta array by (subject, predicate, object, op) lexicographically
  - Flatten guard to deterministic key-value string
  - Use JSON.stringify with replacer for consistent encoding

- `hashCapsule(capsule)` → string (hex)
  - Compute sha256(canonicalizeCapsule(capsule))
  - Use hash-wasm library for deterministic hashing
  - Return hex-encoded hash string

- `hashReceipt(receipt, parents?)` → string (hex)
  - Create deterministic hash chain: sha256(receipt.hash + parent1 + parent2 + ...)
  - Build receipt object: { hash, parents: [...], timestamp, signer? }
  - Return hex-encoded hash

### 5. `/home/user/unrdf/AUTONOMIC_INNOVATION/agent-2/index.mjs`
Public API exports.

**Exports**:
- All functions from capsule.mjs
- All functions from canonicalization.mjs
- CapsuleSchema from schema.mjs

### 6. `/home/user/unrdf/AUTONOMIC_INNOVATION/agent-2/test.mjs`
Comprehensive test suite using Node.js built-in test runner.

**Test Cases**:
1. **Capsule schema validation**
   - Valid capsule with all required fields passes
   - Invalid capsule (missing fields) fails with error
   - Invalid operations rejected

2. **Deterministic canonicalization**
   - Same intent/delta → identical canonical string (run twice)
   - Different order inputs → same canonical output
   - Empty arrays handled correctly

3. **Idempotence of hashing**
   - hashCapsule(capsule) called 100 times → identical hash every time
   - Hash stability across process restarts (deterministic)

4. **Receipt chain verification**
   - Valid parent chain passes verification
   - Parent hash mismatch detected and rejected
   - Empty parent array allowed (genesis capsule)

5. **Capsule immutability**
   - planCapsule() does NOT modify input intent array
   - compileCapsuleToDeltas() does NOT mutate input capsule
   - applyCapsule() does NOT mutate input capsule
   - Deep freeze verification

## Core Invariants

1. **Purity**: All functions are pure (no side effects, no mutations)
2. **Determinism**: Same input → same output, always
3. **Immutability**: Capsules are never mutated after creation
4. **Content-Addressing**: Hash uniquely identifies capsule content
5. **Tamper-Evidence**: Receipt chain detects any modification

## Dependencies

- `zod` - Schema validation
- `hash-wasm` - Deterministic SHA-256 hashing
- Node.js built-in `crypto` (fallback)
- Node.js built-in `test` and `assert` for testing

## Integration Points

**For Agent 1 (Intent DSL)**:
- Receives intent operations from DSL parser
- Returns validated Capsule structure

**For Agent 8 (Store Application)**:
- Provides applyCapsule() for receipt generation
- Provides verifyCapsule() for pre-application validation
- Capsule remains immutable during store application

## Success Criteria

- [ ] All Zod schemas defined and exported
- [ ] All 4 core functions implemented (planCapsule, compileCapsuleToDeltas, verifyCapsule, applyCapsule)
- [ ] All 3 hashing functions implemented (canonicalizeCapsule, hashCapsule, hashReceipt)
- [ ] Test suite with 5+ test cases
- [ ] 100% test pass rate (timeout 5s)
- [ ] Deterministic hashing verified (100 iterations identical)
- [ ] Immutability verified (no input mutations)
- [ ] Zero OTEL in implementation (pure functions only)
- [ ] Full JSDoc type hints (100% coverage)

## Execution Timeline

Single-pass implementation:
1. Schema definition (schema.mjs)
2. Canonicalization & hashing (canonicalization.mjs)
3. Core operations (capsule.mjs)
4. Public API (index.mjs)
5. Test suite (test.mjs)
6. Verification (run tests, check output)

Estimated: 1 message execution, verified completion.
