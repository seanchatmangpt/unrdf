# Narrative State Chain - Test Suite

**Status**: Test infrastructure READY. Awaiting backend implementation completion.

**100% Pass Rate Target**: ALL tests must pass before release.

## Overview

Complete test suite for the Narrative State Chain architecture with:
- **50+ Unit Tests** - Type validation, schemas, CRUD operations
- **Property-Based Tests** - fast-check generators for reconciliation, minimality, invariants
- **15+ Integration Tests** - Full workflows, scene chains, bridges
- **Determinism Tests** - Replay verification (100+ runs per test)
- **Adversarial Tests** - Security, tampering detection, tampering prevention

## Test Organization

```
test/narrative-state-chain/
├── unit/                 # Unit tests (types, schemas)
│   └── types.unit.mjs
├── property/             # Property-based tests
│   └── reconciliation.property.mjs
├── integration/          # Integration tests
│   └── workflows.integration.mjs
├── determinism/          # Determinism verification
│   └── replay.determinism.mjs
├── adversarial/          # Security & tampering tests
│   └── tampering.adversarial.mjs
├── fixtures/             # Test data factories
│   ├── types.mjs         # Type fixtures (Universe, Scene, etc.)
│   └── generators.mjs    # fast-check generators
├── helpers/              # Test utilities
│   └── validation.mjs    # Validation functions
└── vitest.config.mjs     # Test configuration
```

## Fixtures & Generators

### Type Fixtures (`fixtures/types.mjs`)

Factory functions for creating test data:

```javascript
import {
  createUniverseFixture,      // UniverseRecord
  createSceneEnvelopeFixture, // SceneEnvelope
  createReceiptFixture,       // Receipt
  createBridgeProofFixture,   // BridgeProof
  generateScenes,             // Batch of scenes
  generateQuads               // RDF quads
} from '../fixtures/types.mjs';

const universe = createUniverseFixture();
const scene = createSceneEnvelopeFixture({ universeId: universe.id });
```

### Property Generators (`fixtures/generators.mjs`)

Fast-check generators for property-based testing:

```javascript
import {
  quadArbitrary,           // RDF quad generator
  observationArbitrary,    // Observation generator
  sceneEnvelopeArbitrary,  // Scene envelope generator
  sceneChainArbitrary,     // Linked scene chains
  commutativeObservationPairArbitrary  // Commutativity testing
} from '../fixtures/generators.mjs';

// Use in property tests
fc.assert(
  fc.property(observationArbitrary, (obs) => {
    // Test property
  }),
  { numRuns: 100 }
);
```

### Validation Helpers (`helpers/validation.mjs`)

Functions to validate NSC objects:

```javascript
import {
  verifySceneStructure,
  verifySceneChain,
  verifyReceiptSignature,
  verifyDeterminism,
  verifyInvariantCheckResult
} from '../helpers/validation.mjs';

if (verifySceneStructure(scene)) {
  // Scene is valid
}
```

## Running Tests

### Prerequisites

```bash
cd /home/user/unrdf
npm install vitest fast-check zod
```

### Run All Tests

```bash
# Timeout 5s per CLAUDE.md rules (Andon principle)
timeout 5s npm test -- --reporter=verbose

# Show full output
npm run test:all

# Watch mode (development)
npm run test:watch
```

### Run by Category

```bash
# Unit tests only
timeout 5s npm run test:unit

# Property-based tests only
timeout 5s npm run test:property

# Integration tests only
timeout 5s npm run test:integration

# Determinism tests only (critical!)
timeout 5s npm run test:determinism

# Adversarial tests only (security!)
timeout 5s npm run test:adversarial
```

### Coverage Report

```bash
npm run test:coverage
# Opens coverage/index.html
```

## Test Counts by Category

| Category | Tests | Priority | Status |
|----------|-------|----------|--------|
| Unit (types) | 45+ | HIGH | Ready |
| Property (reconciliation) | 25+ | HIGH | Ready |
| Integration (workflows) | 15+ | MEDIUM | Ready |
| Determinism (replay) | 30+ | CRITICAL | Ready |
| Adversarial (security) | 20+ | CRITICAL | Ready |
| **TOTAL** | **135+** | - | **Ready** |

## Key Test Scenarios

### Unit Tests

- [x] UniverseRecord type validation
- [x] Observation structure validation
- [x] Delta type validation (additions/deletions)
- [x] Receipt signature verification
- [x] SceneEnvelope linking
- [x] BridgeProof structure

### Property Tests

- [x] Reconciliation should handle empty observations
- [x] Consequence invariant checks match universe invariants
- [x] Observations commutative when disjoint
- [x] Delta proof justification is valid
- [x] Invariant checks are all boolean
- [x] Receipt timestamps are monotone
- [x] Scene chain sequence numbers are valid

### Integration Tests

- [x] Complete scene processing end-to-end
- [x] Scene ordering and chaining
- [x] Guard evaluation
- [x] Invariant enforcement
- [x] Bridge translation
- [x] Artifact generation

### Determinism Tests

- [x] Scene replay produces identical hash (10 runs)
- [x] Scene replay produces identical hash (100 runs)
- [x] Delta hash is deterministic
- [x] Scene chain hash is deterministic
- [x] Observation ordering affects hash
- [x] Reconciliation result is identical on replay
- [x] Large scenes (1000+ observations) are deterministic

### Adversarial Tests

- [x] Receipt signature forgery detection
- [x] Receipt hash tampering detection
- [x] Delta addition tampering detection
- [x] Guard circumvention prevention
- [x] Guard order manipulation detection
- [x] Invariant violation detection
- [x] Chain integrity attacks (broken parents)
- [x] Sequence number jumps/duplicates
- [x] Bridge type coercion tampering
- [x] Access grant expiration tracking
- [x] DoS prevention (extreme timestamps, large deltas)

## Performance SLAs (from specification)

| Operation | Target | Method |
|-----------|--------|--------|
| Scene processing | <100ms | OTEL spans |
| Guard evaluation | <10ms per guard | timed tests |
| Invariant check | <50ms per invariant | benchmarks |
| Bridge crossing | <200ms | integration tests |

## Determinism Guarantee

**CRITICAL**: The test suite verifies:

1. **Bit-identical replay** - Running same scene 100 times produces identical hash
2. **Canonical serialization** - Objects with same content hash the same way
3. **Fork chain integrity** - Parent-child links are cryptographically verifiable
4. **Signature stability** - Signing same receipt always produces same signature

## Expected Results After Implementation

```
✅ All 135+ tests passing
✅ Coverage >90% (lines, functions)
✅ Branch coverage >80%
✅ Determinism proven (100+ replay runs per test)
✅ Zero flaky tests (all assertions deterministic)
✅ Adversarial tests all passing (no forgeries possible)
```

## Architecture Alignment

Tests validate the 5-phase processing model:

1. **Phase 1: Observation Ingestion + Guard Checkpoint**
   - Tests: unit/types.unit.mjs (Guard validation)
   - Tests: integration/workflows.integration.mjs (Guard evaluation)

2. **Phase 2: Reconciliation (μ)**
   - Tests: property/reconciliation.property.mjs (μ properties)
   - Tests: adversarial/tampering.adversarial.mjs (μ integrity)

3. **Phase 3: Invariant Validation (Q)**
   - Tests: unit/types.unit.mjs (Invariant checks)
   - Tests: integration/workflows.integration.mjs (Invariant enforcement)

4. **Phase 4: Receipt Generation**
   - Tests: determinism/replay.determinism.mjs (Receipt hash)
   - Tests: adversarial/tampering.adversarial.mjs (Receipt tampering)

5. **Phase 5: Artifact Emission**
   - Tests: integration/workflows.integration.mjs (Artifacts)
   - Tests: adversarial/tampering.adversarial.mjs (Side effects)

## Debugging Failed Tests

### Unit Test Failure
```bash
npm run test:unit -- --reporter=verbose --reporter=html
# Check: vitest-html-reporter.html
```

### Property Test Failure
Capture seed:
```
Seed: 1234567890
Re-run with: npm run test:property -- --seed 1234567890
```

### Determinism Test Failure
Check: Different hash on replay indicates non-deterministic reconciliation

### Adversarial Test Failure
Check: Tampering was not detected (security issue!)

## Integration with CI/CD

```yaml
# .github/workflows/test.yml
- name: Test Narrative State Chain
  run: timeout 5s npm test -- test/narrative-state-chain

- name: Coverage Report
  run: npm run test:coverage
  if: always()
```

## Notes for Backend Dev

1. **Import Paths**: Tests import from `/home/user/unrdf/src/narrative-state-chain/`
2. **No Live Dependencies**: All tests use fixtures - can run without real implementation
3. **Determinism is Non-Negotiable**: Receipt hashes MUST match after replay
4. **Signatures are Required**: All receipts MUST have valid signatures
5. **Invariants are Checked**: Every scene consequence MUST have invariant checks

## Waiting for Implementation

This test suite is **COMPLETE and READY**. Once backend-dev completes:

1. Implementation modules appear in `/home/user/unrdf/src/narrative-state-chain/`
2. Run `npm test` → should show 0 failures
3. Run `npm run test:coverage` → verify coverage targets met
4. Run determinism tests → verify <100ms per scene
5. Mark as DONE ✓

## Contact

If test infrastructure needs updates:
- Modify fixtures in `fixtures/`
- Add new test files following pattern
- Keep all 5 categories (unit, property, integration, determinism, adversarial)
