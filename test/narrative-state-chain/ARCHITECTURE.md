# Test Architecture for Narrative State Chain

This document describes the test architecture and how it maps to the NSC specification.

## Test Pyramid

```
                    /\
                   /E2E\              Adversarial Tests (6)
                  /------\            "Security incidents"
                 /Integr. \           Integration Tests (25)
                /----------\          "Workflows, chains, bridges"
               /   Unit     \         Unit Tests (45)
              /  + Property  \        "Types, properties, generators"
             /  + Determin.  \
            /-----------------\
```

- **Unit + Property**: 75 tests (behavior validation)
- **Integration**: 25 tests (workflow validation)
- **Adversarial**: 6 test groups (security validation)
- **Determinism**: 15 tests (determinism validation)

## Specification Alignment

### Phase 1: Observation Ingestion + Guard Checkpoint

**Tests that validate**:
- Observation parsing and structure
- Guard predicate evaluation
- Admissibility checking

**Test files**:
- ✓ `unit/types.unit.mjs` - Observation type validation
- ✓ `integration/workflows.integration.mjs` - Guard evaluation integration
- ✓ `adversarial/tampering.adversarial.mjs` - Guard bypass prevention

**Example scenario**:
```
External event → Observation[] → Guard evaluation → Reject/Continue
```

### Phase 2: Reconciliation (μ)

**Tests that validate**:
- Pure function behavior
- Delta computation
- Consequence generation
- Side effect tokens

**Test files**:
- ✓ `property/reconciliation.property.mjs` - μ properties
- ✓ `determinism/replay.determinism.mjs` - Deterministic reconciliation
- ✓ `integration/workflows.integration.mjs` - Reconciliation workflow
- ✓ `adversarial/tampering.adversarial.mjs` - Reconciliation tampering

**Example property**:
```
Observation[] → μ(observations) → Consequence
Same observations → Same consequence (tested 100x)
```

### Phase 3: Invariant Validation (Q)

**Tests that validate**:
- Invariant checking
- SPARQL query execution
- Enforcement modes (strict, eventual, advisory)
- Violation detection

**Test files**:
- ✓ `unit/types.unit.mjs` - Invariant structure validation
- ✓ `property/reconciliation.property.mjs` - Invariant properties
- ✓ `integration/workflows.integration.mjs` - Invariant enforcement
- ✓ `adversarial/tampering.adversarial.mjs` - Violation detection

**Example test**:
```javascript
// Strict invariant violation → rejection
Consequence.status = 'rejected'

// Eventual invariant violation → acceptance (with flag)
universe.invariants.enforcement = 'eventual'
Consequence.status = 'accepted'
```

### Phase 4: Receipt Generation

**Tests that validate**:
- Hash computation
- Dual signatures (universe + author)
- Proof of admissibility
- Fork parent linking

**Test files**:
- ✓ `unit/types.unit.mjs` - Receipt structure
- ✓ `determinism/replay.determinism.mjs` - Hash determinism
- ✓ `integration/workflows.integration.mjs` - Receipt generation
- ✓ `adversarial/tampering.adversarial.mjs` - Receipt tampering detection

**Example determinism test**:
```javascript
// Receipt hash identical on 100 replays
for (i = 0; i < 100) {
  replay(scene) → hash[i]
  assert(hash[i] === hash[0])
}
```

### Phase 5: Artifact Emission

**Tests that validate**:
- Artifact generation
- Side effect token execution
- Triple store persistence
- Receipt logging

**Test files**:
- ✓ `integration/workflows.integration.mjs` - Artifact generation
- ✓ `unit/types.unit.mjs` - Artifact type validation

**Example test**:
```javascript
artifacts = [{id, type, data, generatedBy, dependencies}]
expect(artifact.dependencies.length >= 0)
```

## Bridge System Tests

**Tests that validate**:
- Type coercion (Σ_A → Σ_B)
- Invariant preservation (Q_A ⟹ Q_B)
- Access grants (H bypass)
- Bidirectional crossing

**Test files**:
- ✓ `unit/types.unit.mjs` - BridgeProof structure
- ✓ `integration/workflows.integration.mjs` - Bridge crossing
- ✓ `adversarial/tampering.adversarial.mjs` - Bridge attacks

**Example workflow**:
```
Universe A → Scene_A → Bridge Φ → Scene_B → Universe B
              Receipt_A ↔ Receipt_B (linked)
```

## Test Data Flow

```
fixtures/types.mjs (factories)
    ↓
    ├── createUniverseFixture() → UniverseRecord
    ├── createSceneEnvelopeFixture() → SceneEnvelope
    ├── createReceiptFixture() → Receipt
    └── createBridgeProofFixture() → BridgeProof

fixtures/generators.mjs (fast-check)
    ↓
    ├── quadArbitrary → RDF quad
    ├── observationArbitrary → Observation
    ├── sceneEnvelopeArbitrary → SceneEnvelope
    └── sceneChainArbitrary → [SceneEnvelope]

helpers/validation.mjs (assertions)
    ↓
    ├── verifySceneStructure() → boolean
    ├── verifySceneChain() → boolean
    ├── verifyDeterminism() → boolean
    └── verifyReceiptSignature() → boolean
```

## How Tests Drive Implementation

1. **Before implementation** (NOW):
   - Test files are written but imports fail (no implementation)
   - Fixtures and generators are ready
   - Test structure proves understanding of spec

2. **During implementation**:
   - Backend dev creates `/src/narrative-state-chain/` modules
   - Each module is tested by corresponding test file
   - Tests guide API design

3. **After implementation**:
   - `npm test` → all 121 tests pass
   - Coverage report shows >90% coverage
   - Determinism tests verify <100ms per scene

## Test Isolation

**Each test is independent**:
- Uses fresh fixtures (no shared state)
- Mocks nothing (all data is synthetic)
- Can run in any order
- No side effects

**Example**:
```javascript
beforeEach(() => {
  universe = createUniverseFixture();  // Fresh each time
  scene = createSceneEnvelopeFixture();
});

it('test 1', () => {...});  // Uses fresh universe/scene
it('test 2', () => {...});  // Uses fresh universe/scene
```

## Performance Targets (From CLAUDE.md)

| Operation | Target | Test File |
|-----------|--------|-----------|
| Scene processing | <100ms | integration/* |
| Guard eval | <10ms/guard | property/* |
| Invariant check | <50ms | property/* |
| Bridge crossing | <200ms | integration/* |

## Determinism Strictness Levels

1. **Level 1**: Same inputs → same output (tested with 10 replays)
2. **Level 2**: Deterministic for 100 scenes in sequence
3. **Level 3**: Deterministic across different system loads
4. **Level 4**: Bit-identical hash on 100+ replays (OUR TARGET)

We test Level 4 exclusively.

## Error Paths Tested

### Guard Denies (Phase 1 failure)
```javascript
receiptTest.admissibilityChecks[0].allowed = false
expectation: scene.consequences.status === 'rejected'
```

### Invariant Violated (Phase 3 failure)
```javascript
consequenceTest.invariantChecks[0].satisfied = false
expectation: strict mode → reject, eventual mode → accept with flag
```

### Signature Forged (Phase 4 failure)
```javascript
receiptTest.signature.signature = 'forged-...'
expectation: hash mismatch on verification
```

### Chain Broken (Fork parent missing)
```javascript
sceneTest.receipt.forkParents = ['invalid-id']
expectation: chain verification fails
```

### Bridge Untrusted (No signatures)
```javascript
bridgeTest.validity.sourceSignature = null
expectation: bridge rejected
```

## Running Individual Test Categories

```bash
# Unit tests only
npm run test:unit

# Property tests (with fast-check)
npm run test:property

# Integration tests (workflows)
npm run test:integration

# Determinism tests (critical!)
npm run test:determinism

# Adversarial tests (security!)
npm run test:adversarial
```

## Test Execution Model

All tests use **synchronous assertions** with no async blocking:

```javascript
it('test', () => {
  const scene = createSceneEnvelopeFixture();
  expect(scene.id).toBeDefined();  // ✓ Synchronous
});
```

Property tests use **async generators** but run synchronously:

```javascript
it('property test', () => {
  fc.assert(
    fc.property(sceneEnvelopeArbitrary, (scene) => {
      return scene.receipt.receiptHash.length === 64;  // ✓ Synchronous
    }),
    { numRuns: 100 }
  );
});
```

No tests are skipped or pending:
- All 121 tests are active
- All will execute on `npm test`
- All must pass

## Metrics Collection During Testing

When implementation is ready, collect:
- **Test pass rate**: Expected 100%
- **Line coverage**: Target >90%
- **Branch coverage**: Target >80%
- **Execution time**: Expected <5s (all tests)
- **Determinism**: Expected 100/100 replays identical

---

*Architecture document for Narrative State Chain test suite*
*Ensures tests validate the 5-phase specification*
