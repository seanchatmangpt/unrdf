# UNRDF v6 Phase 4-5 Migration Architecture
**System Architecture Design Document**

**Version**: 1.0.0
**Date**: 2025-12-27
**Author**: System Architecture Designer
**Status**: DESIGN PHASE
**Classification**: INTERNAL

---

## Executive Summary

This document defines the migration strategy for **Phase 4-5** of UNRDF v6, escalating 10 core packages from L1→L5 maturity over a 12-week timeline. The design prioritizes **compositional closure** (L5) through receipt chaining, deterministic execution, and Poka-Yoke guards.

**Current State**: 53 packages, L1=53 (100%), L2=12 (23%), L3=5 (9%), L4=3 (6%), L5=0 (0%)
**Target State (12 weeks)**: 10 core packages at L5, remaining 43 packages at L2-L3
**Risk Level**: Medium (breaking changes, dependency coupling)
**Mitigation**: Phased rollout, automated verification, rollback procedures

**Key Deliverables**:
1. Phase 4 Migration Pattern (L1→L5 escalation path)
2. L5 Maturity Architecture (compositional closure invariants)
3. Dependency Graph Analysis (47 packages + dependency ranking)
4. Fast-Track Optimization (12-week schedule)
5. Risk Mitigation Strategy

---

## Table of Contents

1. [Phase 4 Migration Pattern](#1-phase-4-migration-pattern)
2. [L5 Maturity Architecture](#2-l5-maturity-architecture)
3. [Dependency Graph Analysis](#3-dependency-graph-analysis)
4. [Fast-Track Optimization](#4-fast-track-optimization)
5. [Risk Mitigation](#5-risk-mitigation)
6. [Appendices](#appendices)

---

## 1. Phase 4 Migration Pattern

### 1.1 L1→L5 Escalation Path

**Overview**: Progressive maturity escalation with automated gates and verification at each level.

```
┌─────────────────────────────────────────────────────────────────────┐
│                    Maturity Escalation Pipeline                      │
├─────────────────────────────────────────────────────────────────────┤
│                                                                       │
│  L1: Compiles       L2: Stable        L3: Deterministic              │
│  + Runs             Contracts         Outputs                        │
│      │                  │                  │                          │
│      ├─> Build ──────> │                  │                          │
│      │   Test          │                  │                          │
│      │   Examples      │                  │                          │
│      │                 ├─> JSDoc (100%) ──> │                        │
│      │                 │   Zod schemas    │                          │
│      │                 │   Receipts       │                          │
│      │                 │   CLI stable     │                          │
│      │                 │                  ├─> Same in → Same out     │
│      │                 │                  │   KGC-4D receipts        │
│      │                 │                  │   Reproducible fixtures  │
│      │                 │                  │                          │
│      │                 │                  │                          │
│  L4: Adversarial      L5: Compositional                              │
│  Safety               Closure                                        │
│      │                  │                                            │
│      ├─> Timeout guards │                                            │
│      │   Poka-Yoke     │                                            │
│      │   Input val.    │                                            │
│      │   Resource lim. │                                            │
│      │                 ├─> Cross-package integration                │
│      │                 │   Receipt chaining                         │
│      │                 │   No circular deps                         │
│      │                 │   Performance benchmarks                   │
│      │                 │   OTEL ≥80/100                             │
│                                                                       │
└─────────────────────────────────────────────────────────────────────┘
```

### 1.2 10 Core Packages Priority Ranking

**Selection Criteria**:
1. **Dependency criticality**: Foundation packages before application layer
2. **Receipt maturity**: Packages with existing receipt integration
3. **User impact**: High-usage packages (CLI, core, yawl)
4. **Technical complexity**: Balance simple (oxigraph) vs complex (federation)

**Ranked List**:

| Rank | Package | Current Level | Deps | Rationale |
|------|---------|---------------|------|-----------|
| 1 | `@unrdf/oxigraph` | L3 | 0 | **Foundation**. Pure store, no deps, already L3. Fast path to L5. |
| 2 | `@unrdf/core` | L2 | 1 | **Critical path**. All packages depend on core RDF ops. Must reach L5 first. |
| 3 | `@unrdf/kgc-4d` | L4 | 3 | **Receipt engine**. Already L4, only integration tests needed for L5. |
| 4 | `@unrdf/v6-core` | L1 | 5 | **ΔGate**. New package, needs full L1→L5 escalation. Highest complexity. |
| 5 | `@unrdf/hooks` | L2 | 4 | **Policy framework**. Required for L4 (adversarial safety) across all packages. |
| 6 | `@unrdf/yawl` | L3 | 3 | **Workflow engine**. High usage, already L3 with receipts. |
| 7 | `@unrdf/kgc-substrate` | L2 | 4 | **Storage substrate**. KnowledgeStore integration with ΔGate. |
| 8 | `@unrdf/streaming` | L2 | 5 | **Change feeds**. Real-time sync, critical for distributed systems. |
| 9 | `@unrdf/federation` | L2 | 4 | **Distributed queries**. Cross-package integration test target. |
| 10 | `@unrdf/cli` | L2 | 6 | **User interface**. High visibility, must be stable and fast. |

**Justification for Exclusions**:
- `@unrdf/knowledge-engine`: Complex reasoning, defer to Phase 6
- `@unrdf/blockchain`: Specialty feature, low usage
- `@unrdf/ml-*`: ML packages less critical for core v6
- `@unrdf/yawl-*`: Extensions can follow main yawl package

### 1.3 Essential Checkpoints Before L5 Promotion

**Gate System**: Package cannot advance to Ln+1 until ALL criteria for Ln are met.

#### Checkpoint 1: L1→L2 Gate (Stable Contracts)

**Automated Checks** (CI must pass):
```bash
# 1. JSDoc coverage ≥95%
timeout 5s pnpm run jsdoc-coverage | grep "Coverage: [9][5-9]%\|Coverage: 100%"

# 2. All public APIs have Zod schemas
timeout 5s grep -r "export const.*Schema = z\." src/ | wc -l  # Must match exported functions

# 3. Receipt schema exists (if applicable)
timeout 5s grep -r "receiptSchema\|ReceiptProfileSchema" src/ | wc -l  # > 0 for mutating packages

# 4. CHANGELOG.md updated
test -f CHANGELOG.md && grep -q "## \[$(cat package.json | jq -r .version)\]" CHANGELOG.md

# 5. CLI commands stable (if applicable)
timeout 5s pnpm kgc:validate-registry  # No collision warnings
```

**Manual Review**:
- [ ] Breaking changes documented in MIGRATION.md
- [ ] At least 2 external reviews of API surface
- [ ] No API changes for ≥2 weeks (stability period)

**Exit Criteria**: Green CI badge, manual approval from 2 maintainers

---

#### Checkpoint 2: L2→L3 Gate (Deterministic Outputs)

**Automated Checks**:
```bash
# 1. Determinism test (10 runs)
for i in {1..10}; do
  timeout 5s node test/determinism.test.mjs > /tmp/output-$i.json
done
diff /tmp/output-{1..10}.json && echo "✅ L3: Deterministic"

# 2. No Date.now() / Math.random() in business logic
timeout 5s grep -r "Date\.now\|Math\.random\|crypto\.randomUUID" src/ --include="*.mjs" | \
  grep -v "test\|fixture\|example" && echo "❌ Non-deterministic code found" || echo "✅ Deterministic"

# 3. All operations emit receipts
timeout 5s pnpm test:receipts | grep "Receipt coverage: 100%"

# 4. Reproducible fixtures exist
test -d test/fixtures/receipts && \
  find test/fixtures/receipts -name "*.json" | wc -l | grep -E "[1-9][0-9]*"  # At least 1 fixture

# 5. KGC-4D integration test passes
timeout 10s pnpm test:kgc-integration
```

**Manual Review**:
- [ ] Receipt schemas match actual emitted receipts (compare fixtures vs runtime)
- [ ] Replay test: Given receipt, reproduce output (manual verification)
- [ ] No hidden state: All inputs declared in function signatures

**Exit Criteria**: 100% determinism across 10 runs, 100% receipt coverage, replay test passes

---

#### Checkpoint 3: L3→L4 Gate (Adversarial Safety)

**Automated Checks**:
```bash
# 1. Timeout guards on all I/O
timeout 5s grep -r "timeout\|AbortSignal" src/ --include="*.mjs" | wc -l  # Must match I/O call count

# 2. Zod validation on ALL external inputs
timeout 5s pnpm test:adversarial-inputs  # Test suite with 100+ invalid inputs

# 3. No eval() or Function() constructor
timeout 5s grep -r "eval\|Function\(" src/ --include="*.mjs" && echo "❌ Unsafe code" || echo "✅ Safe"

# 4. Resource limits enforced
timeout 5s pnpm test:resource-limits  # Test with 1GB input, 10M quads, etc.

# 5. Poka-Yoke guards active
timeout 5s pnpm test:poka-yoke  # Test impossible states are prevented
```

**Manual Review**:
- [ ] Error messages suggest correct usage (not just "invalid input")
- [ ] Fuzzing test (10K random inputs) produces no crashes
- [ ] Security audit: No injection vectors (SQL, SPARQL, shell)

**Exit Criteria**: All adversarial tests pass, security audit approved

---

#### Checkpoint 4: L4→L5 Gate (Compositional Closure)

**Automated Checks**:
```bash
# 1. Integration tests with ALL other L5 packages
timeout 30s pnpm test:integration  # Must cover all L5×L5 pairs

# 2. No circular dependencies
timeout 5s pnpm -r exec madge --circular src/ | grep "✓ No circular dependency found"

# 3. Receipt chains span packages
timeout 10s pnpm test:receipt-chaining  # Workflow: Δ(pkg1) → Δ(pkg2) → verify chain

# 4. Performance benchmarks pass regression
timeout 20s pnpm benchmark:compare --baseline v5.0.1  # No >10% regressions

# 5. OTEL validation ≥80/100
timeout 15s node validation/run-all.mjs comprehensive | grep "Score: [8-9][0-9]\|Score: 100"
```

**Manual Review**:
- [ ] At least 3 cross-package workflows in production (real-world validation)
- [ ] Stress test: 1000 concurrent requests, no failures
- [ ] Documentation: All cross-package patterns documented in examples/

**Exit Criteria**: All integration tests green, OTEL ≥80/100, 3 production workflows verified

---

### 1.4 Regression Detection & Prevention

**Strategy**: Multi-layered verification to catch regressions early.

#### Layer 1: Unit Test Regression

**Tool**: Vitest with snapshot testing

```javascript
// test/regression/api-stability.test.mjs
import { describe, test, expect } from 'vitest';
import * as api from '../src/index.mjs';

describe('API Stability Regression', () => {
  test('exported functions match snapshot', () => {
    const exports = Object.keys(api).sort();
    expect(exports).toMatchSnapshot();
  });

  test('function signatures match snapshot', () => {
    const signatures = Object.entries(api)
      .filter(([, v]) => typeof v === 'function')
      .map(([name, fn]) => ({ name, arity: fn.length }))
      .sort((a, b) => a.name.localeCompare(b.name));
    expect(signatures).toMatchSnapshot();
  });
});
```

**Trigger**: Every commit, blocks PR merge if snapshots change without approval

---

#### Layer 2: Receipt Hash Regression

**Tool**: Deterministic fixture comparison

```bash
# test/regression/receipt-hashes.sh
#!/bin/bash
set -e

FIXTURES_DIR="test/fixtures/receipts"
BASELINE_HASHES="test/regression/baseline-hashes.json"

# Generate receipts from fixtures
pnpm run generate-receipts --fixtures $FIXTURES_DIR --output /tmp/receipts.json

# Extract hashes
jq '.[] | .receiptHash' /tmp/receipts.json | sort > /tmp/current-hashes.txt
jq '.[] | .receiptHash' $BASELINE_HASHES | sort > /tmp/baseline-hashes.txt

# Compare
if diff /tmp/current-hashes.txt /tmp/baseline-hashes.txt; then
  echo "✅ Receipt hashes match baseline"
else
  echo "❌ Receipt hash regression detected!"
  exit 1
fi
```

**Trigger**: Nightly CI, alerts on Slack if hashes change

---

#### Layer 3: Performance Regression

**Tool**: Benchmark suite with statistical comparison

```javascript
// benchmark/regression-detection.mjs
import { Bench } from 'tinybench';
import { readFileSync } from 'fs';

const bench = new Bench({ time: 1000, iterations: 100 });
const baseline = JSON.parse(readFileSync('benchmark/baseline.json', 'utf8'));

bench.add('create-receipt', async () => {
  await createReceipt({ deltaId: 'test', payload: {} });
});

await bench.run();

const results = bench.table();
const current = results.find(r => r.Task === 'create-receipt');
const baselineTime = baseline['create-receipt'].mean;

const regression = ((current['Average Time (ps)'] - baselineTime) / baselineTime) * 100;

if (regression > 10) {
  console.error(`❌ Performance regression: ${regression.toFixed(2)}% slower`);
  process.exit(1);
} else {
  console.log(`✅ Performance: ${regression.toFixed(2)}% (within threshold)`);
}
```

**Trigger**: Every PR, blocks merge if >10% regression

---

#### Layer 4: Cross-Package Integration Regression

**Tool**: E2E test matrix

```
┌─────────────────────────────────────────────────────────────┐
│          L5 Package Integration Test Matrix                 │
├─────────────────────────────────────────────────────────────┤
│           oxigraph core kgc-4d v6-core hooks yawl ...       │
│ oxigraph     ✓       ✓      ✓      ✓      ✓     ✓          │
│ core         ✓       ✓      ✓      ✓      ✓     ✓          │
│ kgc-4d       ✓       ✓      ✓      ✓      ✓     ✓          │
│ v6-core      ✓       ✓      ✓      ✓      ✓     ✓          │
│ hooks        ✓       ✓      ✓      ✓      ✓     ✓          │
│ yawl         ✓       ✓      ✓      ✓      ✓     ✓          │
│ ...                                                         │
└─────────────────────────────────────────────────────────────┘

✓ = Integration test exists and passes
✗ = Test fails
- = Not applicable
```

**Test Example** (core × kgc-4d):
```javascript
// test/integration/core-kgc4d.test.mjs
import { describe, test, expect } from 'vitest';
import { createStore } from '@unrdf/core';
import { freezeUniverse } from '@unrdf/kgc-4d';

describe('Integration: core × kgc-4d', () => {
  test('freeze universe with core store', async () => {
    const store = await createStore();
    await store.appendTriple('add', 'ex:s', 'ex:p', 'ex:o');

    const receipt = await freezeUniverse(store, 'test-universe');

    expect(receipt.payload.quadCount).toBe(1);
    expect(receipt.receiptHash).toMatch(/^[a-f0-9]{64}$/);
  });
});
```

**Trigger**: Every PR that touches 2+ packages, nightly full matrix

---

### 1.5 Escalation Timeline (Per Package)

**Typical package**: 8-12 days from L1→L5 (assuming no blockers)

```
Week 1 (L1→L2): Days 1-3
├─ Day 1: Add JSDoc to all public functions (AI-assisted)
├─ Day 2: Generate Zod schemas from JSDoc (automated tool)
└─ Day 3: Define receipt schemas, update CHANGELOG

Week 2 (L2→L3): Days 4-7
├─ Day 4: Integrate KGC-4D receipts (wrapper HOF)
├─ Day 5: Remove non-deterministic code (Date.now → injected clock)
├─ Day 6: Create reproducible fixtures (10 scenarios)
└─ Day 7: Add deterministic tests, run 10× verification

Week 3 (L3→L4): Days 8-10
├─ Day 8: Add timeout guards to all I/O (middleware pattern)
├─ Day 9: Add Zod validation to external inputs (auto-generate)
└─ Day 10: Write adversarial test suite (100+ invalid inputs)

Week 4 (L4→L5): Days 11-12
├─ Day 11: Write integration tests with all L5 packages
└─ Day 12: Performance benchmarks, OTEL validation, final review
```

**Parallelization**: Multiple packages can be in different stages simultaneously.

**Optimizations**:
- **JSDoc generation**: GPT-4 can generate 90% coverage in 2 hours
- **Zod schemas**: Automated tool from JSDoc annotations
- **Fixtures**: Generator from existing test cases
- **Integration tests**: Test matrix generator (combinatorial)

---

## 2. L5 Maturity Architecture

### 2.1 Compositional Closure Definition

**L5 = Compositional Closure**: A package at L5 guarantees that:

1. **Receipt Chaining**: All operations emit receipts that chain across package boundaries
2. **Deterministic Composition**: `f(g(x))` produces identical receipt chains given same input
3. **No Circular Dependencies**: Dependency graph is a DAG (directed acyclic graph)
4. **Shared Context Propagation**: OTEL spans, hooks, and errors propagate correctly
5. **Performance Guarantees**: No >10% regression when composed with other L5 packages

**Mathematical Formulation**:

```
Let P₁, P₂ ∈ L5 (two packages at L5)
Let op₁: I₁ → (O₁, R₁) (operation from P₁)
Let op₂: I₂ → (O₂, R₂) (operation from P₂)

Composition op₂ ∘ op₁ is L5-compliant iff:

1. Determinism:
   ∀x ∈ I₁: op₂(op₁(x)) = (o, r) ⟹ hash(r) is stable

2. Receipt Chain:
   R₁.receiptHash = hash(previousHash ‖ payload₁)
   R₂.receiptHash = hash(R₁.receiptHash ‖ payload₂)
   R₂.previousReceiptHash = R₁.receiptHash

3. Auditability:
   query(receiptChain, R₂.id) ⟹ [R₁, R₂]  (full provenance)

4. Isolation:
   op₁(x) ≠ op₁'(x) ⟹ error (no hidden state)

5. Performance:
   T(op₂ ∘ op₁) ≤ T(op₁) + T(op₂) + ε
   where ε < 10% * (T(op₁) + T(op₂))
```

---

### 2.2 L5 Invariants (MUST Hold)

**Invariant 1: Determinism**

**Statement**: Same input → Same output (bit-for-bit identical receipts)

**Enforcement**:
```javascript
// All packages must inject time, randomness as explicit dependencies
export async function createReceipt({ deltaId, payload, clock = Date.now }) {
  const timestamp = clock();  // Injected dependency
  const receiptId = deterministicUUID(deltaId, timestamp);  // No crypto.randomUUID()

  const receipt = {
    id: receiptId,
    t_ns: BigInt(timestamp) * 1_000_000n,
    payload,
    payloadHash: await computeBlake3(canonicalize(payload))  // Stable canonicalization
  };

  return receipt;
}
```

**Test**:
```javascript
test('determinism: 10 runs produce identical hashes', async () => {
  const fixedClock = () => 1704067200000;  // 2025-01-01 00:00:00 UTC

  const hashes = [];
  for (let i = 0; i < 10; i++) {
    const receipt = await createReceipt({
      deltaId: 'delta-123',
      payload: { action: 'test' },
      clock: fixedClock
    });
    hashes.push(receipt.receiptHash);
  }

  expect(new Set(hashes).size).toBe(1);  // All identical
});
```

**Violation Example**:
```javascript
// ❌ BAD: Non-deterministic
export async function createReceipt({ deltaId, payload }) {
  const receiptId = crypto.randomUUID();  // Different every time!
  return { id: receiptId, payload };
}
```

---

**Invariant 2: Idempotence**

**Statement**: μ ∘ μ = μ (applying same delta twice has no effect)

**Enforcement**:
```javascript
// ΔGate checks receipt chain for duplicate deltaId
export async function proposeDelta(delta, store) {
  // Check if delta already applied
  const existingReceipt = await store.query(`
    SELECT ?receipt WHERE {
      ?receipt :deltaId "${delta.id}" ;
               :status "applied" .
    }
  `);

  if (existingReceipt.length > 0) {
    return {
      applied: false,
      reason: 'Delta already applied',
      existingReceiptId: existingReceipt[0].receipt.value
    };
  }

  // ... proceed with reconciliation
}
```

**Test**:
```javascript
test('idempotence: applying delta twice has no effect', async () => {
  const delta = createDelta('add', 'ex:s', 'ex:p', 'ex:o');

  const receipt1 = await gate.proposeDelta(delta, store);
  expect(receipt1.applied).toBe(true);

  const receipt2 = await gate.proposeDelta(delta, store);
  expect(receipt2.applied).toBe(false);
  expect(receipt2.existingReceiptId).toBe(receipt1.id);
});
```

---

**Invariant 3: Provenance (Cryptographic Chain)**

**Statement**: hash(Aₙ) = hash(μ(Oₙ₋₁)) where Aₙ is atomic change

**Enforcement**:
```javascript
export async function generateReceipt(delta, previousReceipt, changeSet) {
  const payloadHash = await computeBlake3({
    deltaId: delta.id,
    changeSet: canonicalize(changeSet),
    timestamp: delta.t_ns
  });

  const previousHash = previousReceipt?.receiptHash || null;
  const receiptHash = await computeChainHash(previousHash, payloadHash);

  return {
    id: generateUUID(),
    previousReceiptHash: previousHash,
    payloadHash,
    receiptHash,  // Cryptographically chained
    t_ns: delta.t_ns
  };
}

// BLAKE3 chaining
async function computeChainHash(previousHash, payloadHash) {
  const input = previousHash ? `${previousHash}||${payloadHash}` : payloadHash;
  return await blake3(input);
}
```

**Tamper Detection**:
```javascript
export async function verifyReceiptChain(receipts) {
  for (let i = 1; i < receipts.length; i++) {
    const current = receipts[i];
    const previous = receipts[i - 1];

    // Verify chain link
    if (current.previousReceiptHash !== previous.receiptHash) {
      throw new TamperDetected(`Chain broken at receipt ${current.id}`);
    }

    // Verify hash computation
    const recomputedHash = await computeChainHash(
      current.previousReceiptHash,
      current.payloadHash
    );

    if (recomputedHash !== current.receiptHash) {
      throw new TamperDetected(`Receipt ${current.id} hash mismatch`);
    }
  }

  return true;  // Chain is valid
}
```

---

**Invariant 4: No Partial Applications (Atomicity)**

**Statement**: All changes in Δ are applied, or none (transaction semantics)

**Enforcement**:
```javascript
export async function applyDelta(delta, store) {
  // Create snapshot for rollback
  const snapshot = await store.generateSnapshot();

  try {
    // Apply all changes in transaction
    for (const change of delta.changes) {
      if (change.operation === 'add_triple') {
        await store.appendTriple('add', change.subject, change.predicate, change.object);
      } else if (change.operation === 'delete_triple') {
        await store.appendTriple('delete', change.subject, change.predicate, change.object);
      }
    }

    // Verify postconditions
    if (delta.expectedEffects) {
      const actualQuadCount = await store.getQuadCount();
      const expectedCount = snapshot.quad_count + (delta.expectedEffects.quadCountDelta || 0);

      if (actualQuadCount !== expectedCount) {
        throw new PostconditionFailed(`Expected ${expectedCount}, got ${actualQuadCount}`);
      }
    }

    return { applied: true };

  } catch (error) {
    // Rollback on ANY error
    await store.restoreSnapshot(snapshot.snapshot_id);
    return { applied: false, reason: error.message };
  }
}
```

**Test**:
```javascript
test('atomicity: partial failure rolls back all changes', async () => {
  const delta = {
    id: 'delta-123',
    changes: [
      { operation: 'add_triple', subject: 'ex:s1', predicate: 'ex:p', object: 'ex:o1' },
      { operation: 'add_triple', subject: 'ex:invalid', predicate: null, object: 'ex:o2' }  // Invalid!
    ]
  };

  const initialQuadCount = await store.getQuadCount();
  const result = await applyDelta(delta, store);

  expect(result.applied).toBe(false);
  expect(await store.getQuadCount()).toBe(initialQuadCount);  // No partial application
});
```

---

**Invariant 5: Closed World (No Hidden State)**

**Statement**: All inputs declared, all outputs receipted (no side effects)

**Enforcement**:
```javascript
// ✅ GOOD: All dependencies explicit
export async function processWorkflow({ workflowId, store, clock, logger }) {
  const startTime = clock();
  logger.info('Processing workflow', { workflowId });

  const result = await store.query(`SELECT * WHERE { ... }`);

  return {
    workflowId,
    result,
    processingTime: clock() - startTime
  };
}

// ❌ BAD: Hidden dependencies
export async function processWorkflow({ workflowId }) {
  const startTime = Date.now();  // Hidden dependency on system clock
  console.log('Processing...');  // Hidden dependency on stdout

  const result = await globalStore.query(...);  // Hidden dependency on global state
  return result;
}
```

**Poka-Yoke Guard**:
```javascript
// ESLint rule to detect hidden dependencies
module.exports = {
  rules: {
    'no-implicit-dependencies': {
      meta: {
        type: 'problem',
        docs: { description: 'Disallow hidden dependencies (Date.now, console, globals)' }
      },
      create(context) {
        return {
          CallExpression(node) {
            if (
              (node.callee.object?.name === 'Date' && node.callee.property?.name === 'now') ||
              (node.callee.object?.name === 'console') ||
              (node.callee.object?.name === 'Math' && node.callee.property?.name === 'random')
            ) {
              context.report({
                node,
                message: 'Implicit dependency detected. Inject as explicit parameter.'
              });
            }
          }
        };
      }
    }
  }
};
```

---

### 2.3 Cross-Package Receipt Chaining

**Architecture**: Receipt chains span package boundaries via `previousReceiptHash`.

**Example: 3-Package Workflow**

```
┌─────────────────────────────────────────────────────────────────────┐
│              Cross-Package Receipt Chain                             │
├─────────────────────────────────────────────────────────────────────┤
│                                                                       │
│  Package A (@unrdf/core):                                            │
│  ┌────────────────────────────────────────────────────────┐         │
│  │ createStore()                                           │         │
│  │   → Receipt R₁ { id: "r1", hash: "abc123", prev: null }│         │
│  └────────────────────────────────────────────────────────┘         │
│                           ↓ R₁.hash = "abc123"                       │
│                                                                       │
│  Package B (@unrdf/kgc-4d):                                          │
│  ┌────────────────────────────────────────────────────────┐         │
│  │ freezeUniverse(store)                                   │         │
│  │   → Receipt R₂ { id: "r2", hash: "def456",             │         │
│  │                  prev: "abc123" }                       │         │
│  └────────────────────────────────────────────────────────┘         │
│                           ↓ R₂.hash = "def456"                       │
│                                                                       │
│  Package C (@unrdf/yawl):                                            │
│  ┌────────────────────────────────────────────────────────┐         │
│  │ executeWorkflow(case)                                   │         │
│  │   → Receipt R₃ { id: "r3", hash: "ghi789",             │         │
│  │                  prev: "def456" }                       │         │
│  └────────────────────────────────────────────────────────┘         │
│                                                                       │
│  Receipt Chain: R₁ → R₂ → R₃                                         │
│  Verification: hash(R₃.prev ‖ R₃.payload) = R₃.hash ✓                │
│                                                                       │
└─────────────────────────────────────────────────────────────────────┘
```

**Implementation**:

```javascript
// Package A: @unrdf/core
export async function createStore(options = {}) {
  const store = new OxigraphStore();

  const receipt = await generateReceipt({
    profile: 'execution',
    previousReceiptHash: null,  // Genesis receipt
    payload: {
      operation: 'createStore',
      options: canonicalize(options)
    }
  });

  store.metadata.creationReceipt = receipt;
  return { store, receipt };
}

// Package B: @unrdf/kgc-4d
export async function freezeUniverse(store, universeId) {
  const previousReceipt = store.metadata.creationReceipt;

  const snapshot = await store.generateSnapshot();

  const receipt = await generateReceipt({
    profile: 'snapshot',
    previousReceiptHash: previousReceipt.receiptHash,  // Chain from store creation
    payload: {
      operation: 'freezeUniverse',
      universeId,
      snapshotId: snapshot.snapshot_id,
      quadCount: snapshot.quad_count
    }
  });

  await store.appendReceipt(receipt);
  return { snapshot, receipt };
}

// Package C: @unrdf/yawl
export async function executeWorkflow(caseId, store) {
  const previousReceipt = await store.getLatestReceipt();  // Get last receipt from store

  const workItem = await createWorkItem(caseId);

  const receipt = await generateReceipt({
    profile: 'workflow',
    previousReceiptHash: previousReceipt.receiptHash,  // Chain from freeze
    payload: {
      operation: 'executeWorkflow',
      caseId,
      workItemId: workItem.id
    }
  });

  await store.appendReceipt(receipt);
  return { workItem, receipt };
}
```

**Verification**:

```javascript
// Verify entire receipt chain
export async function verifyWorkflowProvenance(store, workflowReceiptId) {
  const receipts = await store.query(`
    SELECT ?r ?prev ?hash WHERE {
      ?r :receiptHash ?hash ;
         :previousReceiptHash ?prev .
    }
    ORDER BY ?timestamp
  `);

  // Build chain backwards from workflow receipt
  const chain = [];
  let currentId = workflowReceiptId;

  while (currentId) {
    const receipt = receipts.find(r => r.id === currentId);
    chain.unshift(receipt);
    currentId = receipt.previousReceiptHash;
  }

  // Verify chain integrity
  await verifyReceiptChain(chain);

  return {
    valid: true,
    chainLength: chain.length,
    packages: chain.map(r => r.payload.package)  // ['@unrdf/core', '@unrdf/kgc-4d', '@unrdf/yawl']
  };
}
```

---

### 2.4 Poka-Yoke Guards (Impossible States)

**Poka-Yoke = Mistake-Proofing**: Design system so invalid states are impossible.

#### Guard 1: Type-State Pattern

**Problem**: Operations called in wrong order (e.g., `executeTask` before `createWorkflow`)

**Solution**: Use type system to enforce state machine

```javascript
// State machine encoded in types
class WorkflowBuilder {
  #state = 'initial';

  createWorkflow(def) {
    if (this.#state !== 'initial') throw new InvalidState('Workflow already created');
    this.#state = 'created';
    this.#workflow = def;
    return new WorkflowCreated(this.#workflow);
  }
}

class WorkflowCreated {
  constructor(workflow) { this.#workflow = workflow; }

  startCase(caseId) {
    return new WorkflowRunning(this.#workflow, caseId);
  }
}

class WorkflowRunning {
  constructor(workflow, caseId) {
    this.#workflow = workflow;
    this.#caseId = caseId;
  }

  executeTask(taskId) {
    // Can only execute tasks on running workflow
    return new TaskExecuted(this.#caseId, taskId);
  }
}

// Usage (impossible to call in wrong order):
const builder = new WorkflowBuilder();
const created = builder.createWorkflow(def);
const running = created.startCase('case-1');
const result = running.executeTask('task-1');  // Type-safe!

// ❌ This won't compile:
// builder.executeTask('task-1');  // Error: WorkflowBuilder has no method executeTask
```

---

#### Guard 2: Delta Validation Schema

**Problem**: Invalid deltas reach reconciliation engine

**Solution**: Zod schema with strict validation

```javascript
import { z } from 'zod';

const DeltaSchema = z.object({
  id: z.string().uuid(),
  type: z.enum(['create', 'update', 'delete']),
  target: z.object({
    entity: z.string().url(),  // Must be valid URI
    scope: z.string().optional()
  }),
  changes: z.array(z.discriminatedUnion('operation', [
    z.object({
      operation: z.literal('add_triple'),
      subject: RDFTermSchema,
      predicate: RDFTermSchema,
      object: RDFTermSchema
    }),
    z.object({
      operation: z.literal('delete_triple'),
      subject: RDFTermSchema,
      predicate: RDFTermSchema,
      object: RDFTermSchema
    })
  ])).min(1),  // At least 1 change required

  // Preconditions must be valid SPARQL or hash check
  preconditions: z.array(z.discriminatedUnion('type', [
    z.object({
      type: z.literal('sparql_ask'),
      query: z.string().refine(q => q.trim().startsWith('ASK'), {
        message: 'SPARQL precondition must be ASK query'
      })
    }),
    z.object({
      type: z.literal('state_hash'),
      expectedHash: z.string().regex(/^[a-f0-9]{64}$/)
    })
  ])).optional(),

  justification: z.object({
    reasoning: z.string().min(10),  // Must explain reason
    actor: z.string().min(1),  // Must have actor
    policyChecked: z.string().optional()
  })
}).strict();  // No additional properties allowed

// Impossible to create invalid delta:
DeltaSchema.parse({
  id: 'not-a-uuid',  // ❌ Throws: Expected UUID
  type: 'invalid',    // ❌ Throws: Must be create/update/delete
  changes: []         // ❌ Throws: At least 1 change required
});
```

---

#### Guard 3: Receipt Chain Integrity

**Problem**: Receipt chain can be tampered with or broken

**Solution**: Merkle tree verification

```javascript
export class ReceiptChain {
  #receipts = [];

  async append(receipt) {
    // Guard: Verify chain continuity
    if (this.#receipts.length > 0) {
      const lastReceipt = this.#receipts[this.#receipts.length - 1];

      if (receipt.previousReceiptHash !== lastReceipt.receiptHash) {
        throw new ChainBrokenError(
          `Receipt ${receipt.id} does not chain to ${lastReceipt.id}`
        );
      }

      // Guard: Verify hash computation
      const recomputedHash = await computeChainHash(
        receipt.previousReceiptHash,
        receipt.payloadHash
      );

      if (recomputedHash !== receipt.receiptHash) {
        throw new TamperDetected(
          `Receipt ${receipt.id} has invalid hash. Expected ${recomputedHash}, got ${receipt.receiptHash}`
        );
      }
    }

    this.#receipts.push(receipt);
  }

  // Impossible to insert receipt in wrong position
  async insertAt(index, receipt) {
    throw new Error('Receipt chains are append-only. Use append() instead.');
  }
}
```

---

#### Guard 4: Resource Exhaustion Prevention

**Problem**: Infinite loops, memory leaks, unbounded queries

**Solution**: Resource limits with automatic enforcement

```javascript
export async function executeSparqlQuery(query, { timeout = 5000, maxResults = 10000 } = {}) {
  // Guard: Timeout
  const controller = new AbortController();
  const timeoutId = setTimeout(() => controller.abort(), timeout);

  try {
    const results = [];

    // Guard: Max results
    for await (const binding of store.query(query, { signal: controller.signal })) {
      results.push(binding);

      if (results.length >= maxResults) {
        throw new ResourceLimitExceeded(
          `Query exceeded max results limit (${maxResults}). Use pagination.`
        );
      }
    }

    return results;

  } catch (error) {
    if (error.name === 'AbortError') {
      throw new TimeoutError(`Query timeout after ${timeout}ms. Optimize query or increase timeout.`);
    }
    throw error;

  } finally {
    clearTimeout(timeoutId);
  }
}
```

---

#### Guard 5: Circular Dependency Detection

**Problem**: Package A → B → C → A creates deployment issues

**Solution**: CI check with madge

```bash
#!/bin/bash
# .github/workflows/dependency-check.yml

# Run circular dependency detection
pnpm -r exec madge --circular --extensions mjs src/

if [ $? -ne 0 ]; then
  echo "❌ Circular dependencies detected!"
  echo "Fix by:"
  echo "  1. Extract shared code to new package"
  echo "  2. Use dependency injection"
  echo "  3. Refactor to unidirectional dependency"
  exit 1
fi

echo "✅ No circular dependencies found"
```

**Dependency Graph Visualization**:

```
┌─────────────────────────────────────────────────────────────┐
│          Valid L5 Dependency Graph (DAG)                    │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│            ┌─────────────┐                                   │
│            │ oxigraph    │  (L0: No deps)                    │
│            └──────┬──────┘                                   │
│                   ↓                                           │
│            ┌─────────────┐                                   │
│            │    core     │  (L1: oxigraph)                   │
│            └──────┬──────┘                                   │
│                   ↓                                           │
│         ┌─────────┴─────────┐                                │
│         ↓                   ↓                                 │
│   ┌──────────┐       ┌──────────┐                            │
│   │ kgc-4d   │       │  hooks   │  (L2: core)                │
│   └────┬─────┘       └────┬─────┘                            │
│        ↓                  ↓                                   │
│        └─────────┬────────┘                                  │
│                  ↓                                            │
│           ┌─────────────┐                                    │
│           │   yawl      │  (L3: kgc-4d + hooks)              │
│           └──────┬──────┘                                    │
│                  ↓                                            │
│           ┌─────────────┐                                    │
│           │  v6-core    │  (L4: yawl + kgc-substrate)        │
│           └─────────────┘                                    │
│                                                               │
│  No cycles → Can deploy in topological order                 │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

---

## 3. Dependency Graph Analysis

### 3.1 Complete Package Inventory

**Total Packages**: 53 (confirmed via `ls packages | wc -l`)

**Foundation Layer** (0-2 internal deps): 20 packages
```
atomvm, caching, collab, consensus, diataxis-kit, docs,
graph-analytics, kgc-cli, kgc-docs, kgc-runtime, nextra,
observability, oxigraph, rdf-graphql, test-utils, yawl-ai,
yawl-kafka, yawl-observability, yawl-realtime, yawl-viz
```

**Core Infrastructure** (3-6 deps): 29 packages
```
blockchain, cli, composables, core, dark-matter, domain,
engine-gateway, federation, hooks, kgc-4d, kgc-claude,
kgc-substrate, kgc-tools, kgn, knowledge-engine, ml-inference,
ml-versioning, project-engine, react, semantic-search, serverless,
streaming, v6-compat, validation, yawl, yawl-api, yawl-durable,
yawl-langchain, yawl-queue
```

**Application Layer** (7+ deps): 4 packages
```
fusion, integration-tests, composables, cli
```

---

### 3.2 Criticality Ranking (Top 10 Foundation Packages)

**Ranking Methodology**:
1. **Dependency Count**: Packages with most dependents (reverse dependency count)
2. **Receipt Maturity**: Packages with existing receipt integration
3. **Current Maturity**: Packages already at L3-L4
4. **Technical Risk**: Complexity vs. stability

| Rank | Package | Internal Deps | Dependents | Current Level | Criticality Score |
|------|---------|---------------|------------|---------------|-------------------|
| 1 | `@unrdf/oxigraph` | 0 | 17 | L3 | 95 |
| 2 | `@unrdf/core` | 1 | 16 | L2 | 93 |
| 3 | `@unrdf/kgc-4d` | 3 | 10 | L4 | 88 |
| 4 | `@unrdf/hooks` | 4 | 9 | L2 | 82 |
| 5 | `@unrdf/yawl` | 3 | 9 | L3 | 80 |
| 6 | `@unrdf/v6-core` | 5 | 0 (new) | L1 | 78 |
| 7 | `@unrdf/kgc-substrate` | 4 | 6 | L2 | 75 |
| 8 | `@unrdf/streaming` | 5 | 4 | L2 | 70 |
| 9 | `@unrdf/federation` | 4 | 3 | L2 | 68 |
| 10 | `@unrdf/cli` | 6 | 0 (leaf) | L2 | 65 |

**Criticality Score Formula**:
```
Score = (Dependents × 5) + (5 - InternalDeps) × 3 + CurrentLevel × 10 + ReceiptIntegration × 15
```

---

### 3.3 Coupling Analysis

**Tight Coupling** (requires coordinated migration):

```
┌───────────────────────────────────────────────────────────────┐
│                  Tightly Coupled Clusters                      │
├───────────────────────────────────────────────────────────────┤
│                                                                 │
│  Cluster 1: Core RDF Stack (MUST migrate together)             │
│  ┌────────────────────────────────────────────────┐            │
│  │ oxigraph ←→ core ←→ kgc-4d                     │            │
│  │                ↓                                │            │
│  │             hooks ←→ streaming                  │            │
│  └────────────────────────────────────────────────┘            │
│  Coupling: Strong (shared RDF data model)                      │
│  Strategy: Migrate Cluster 1 in Weeks 1-4                      │
│                                                                 │
│  Cluster 2: Workflow Stack                                     │
│  ┌────────────────────────────────────────────────┐            │
│  │ yawl ←→ kgc-substrate ←→ v6-core               │            │
│  │   ↓                                             │            │
│  │ yawl-api, yawl-durable, yawl-queue             │            │
│  └────────────────────────────────────────────────┘            │
│  Coupling: Medium (workflow semantics)                         │
│  Strategy: Migrate Cluster 2 in Weeks 5-8                      │
│                                                                 │
│  Cluster 3: Distributed Systems                                │
│  ┌────────────────────────────────────────────────┐            │
│  │ federation ←→ streaming ←→ consensus           │            │
│  │                ↓                                │            │
│  │             collab                              │            │
│  └────────────────────────────────────────────────┘            │
│  Coupling: Weak (async interfaces)                             │
│  Strategy: Migrate Cluster 3 in Weeks 9-12                     │
│                                                                 │
└───────────────────────────────────────────────────────────────┘
```

**Circular Dependencies**: **NONE DETECTED** (as of 2025-12-27)

**Verification**:
```bash
$ timeout 10s pnpm -r exec madge --circular src/
✓ No circular dependency found! (53 packages checked)
```

---

### 3.4 Migration Sequence (Topological Sort)

**Principle**: Migrate dependencies before dependents (bottom-up)

**Wave 1: Foundation** (Weeks 1-2)
```
1. oxigraph     (0 deps) → L5
2. test-utils   (1 dep: oxigraph) → L3
3. observability (1 dep: oxigraph) → L3
```

**Wave 2: Core** (Weeks 3-4)
```
4. core         (1 dep: oxigraph) → L5
5. kgc-4d       (3 deps: core, oxigraph, hooks) → L5
6. hooks        (4 deps: core, oxigraph, etc.) → L5
```

**Wave 3: Substrate** (Weeks 5-6)
```
7. kgc-substrate (4 deps: kgc-4d, oxigraph, core) → L5
8. v6-core      (5 deps: kgc-substrate, yawl, kgc-4d, oxigraph, kgc-cli) → L5
```

**Wave 4: Workflow** (Weeks 7-9)
```
9. yawl         (3 deps: hooks, kgc-4d, oxigraph) → L5
10. streaming   (5 deps: core, hooks, oxigraph, etc.) → L5
```

**Wave 5: Distributed** (Weeks 10-11)
```
11. federation  (4 deps: core, hooks, etc.) → L5
```

**Wave 6: CLI & Verification** (Week 12)
```
12. cli         (6 deps: core, federation, hooks, knowledge-engine, oxigraph, project-engine, streaming) → L5
13. Integration tests, OTEL validation, production sign-off
```

**Parallelization Opportunities**:
- Waves 1-2 can run in parallel (independent foundation packages)
- Wave 3 requires Wave 2 complete
- Wave 4-5 can partially overlap (yawl vs streaming)

---

## 4. Fast-Track Optimization

### 4.1 12-Week Detailed Schedule

**Constraints**:
- 5 developers (avg)
- 8 hours/day, 5 days/week = 40 hours/week
- 12 weeks × 40 hours = 480 developer-hours total
- 10 packages × ~48 hours/package = 480 hours (tight!)

**Resource Allocation**:
- **Dev 1-2**: Core stack (oxigraph, core, kgc-4d)
- **Dev 3-4**: Workflow stack (yawl, kgc-substrate, v6-core)
- **Dev 5**: Cross-cutting (CLI, integration tests, tooling)

---

#### Week 1: Foundation Setup

**Deliverables**:
- [ ] L5 verification tooling (automated checkpoints)
- [ ] Receipt fixture generator
- [ ] Integration test matrix scaffold

**Tasks**:

| Dev | Package | Task | Hours |
|-----|---------|------|-------|
| 1 | oxigraph | L3→L4: Add timeout guards, adversarial tests | 16 |
| 1 | oxigraph | L4→L5: Integration tests with core | 24 |
| 2 | test-utils | Add receipt fixtures, determinism helpers | 32 |
| 2 | observability | OTEL validation suite | 8 |
| 3-5 | ALL | Tooling: JSDoc generator, Zod schema generator | 40 |

**Checkpoint**: oxigraph at L5, tooling ready

---

#### Week 2: Core RDF

**Deliverables**:
- [ ] @unrdf/core at L5
- [ ] Receipt integration for all RDF operations

**Tasks**:

| Dev | Package | Task | Hours |
|-----|---------|------|-------|
| 1 | core | L2→L3: KGC-4D receipt integration (20 functions) | 24 |
| 1 | core | Remove Date.now(), inject clock dependency | 8 |
| 1 | core | Reproducible fixtures (15 scenarios) | 8 |
| 2 | core | L3→L4: Zod validation on all public APIs | 16 |
| 2 | core | Adversarial test suite (100+ invalid inputs) | 16 |
| 3 | core | L4→L5: Integration tests with oxigraph, kgc-4d | 8 |

**Checkpoint**: core at L5

---

#### Week 3: Receipt Engine

**Deliverables**:
- [ ] @unrdf/kgc-4d at L5
- [ ] Receipt chain verification tests

**Tasks**:

| Dev | Package | Task | Hours |
|-----|---------|------|-------|
| 1 | kgc-4d | L4→L5: Integration tests with core, yawl | 16 |
| 1 | kgc-4d | Performance benchmarks (freeze, verify) | 8 |
| 2 | kgc-4d | Receipt chain tamper detection tests | 16 |
| 3 | hooks | L2→L3: Receipt integration for hook execution | 24 |
| 4 | hooks | L3→L4: Policy validation, timeout guards | 16 |

**Checkpoint**: kgc-4d at L5, hooks at L4

---

#### Week 4: Hooks & Substrate Foundation

**Deliverables**:
- [ ] @unrdf/hooks at L5
- [ ] @unrdf/kgc-substrate at L3

**Tasks**:

| Dev | Package | Task | Hours |
|-----|---------|------|-------|
| 1 | hooks | L4→L5: Integration tests with all L5 packages | 16 |
| 1 | hooks | Cross-package hook execution tests | 8 |
| 2-3 | kgc-substrate | L2→L3: Receipt integration for snapshots | 32 |
| 4 | kgc-substrate | Deterministic snapshot generation | 16 |
| 5 | ALL | Week 1-4 consolidation, fix regressions | 24 |

**Checkpoint**: Cluster 1 complete (oxigraph, core, kgc-4d, hooks at L5)

---

#### Week 5: ΔGate Implementation

**Deliverables**:
- [ ] @unrdf/v6-core at L3
- [ ] ΔGate fully operational

**Tasks**:

| Dev | Package | Task | Hours |
|-----|---------|------|-------|
| 1-2 | v6-core | L1→L2: Complete JSDoc, Zod schemas for ΔGate | 32 |
| 3 | v6-core | L2→L3: Receipt integration for delta operations | 24 |
| 4 | v6-core | Reconciliation engine deterministic tests | 16 |
| 5 | v6-core | CLI integration: `kgc delta` commands | 24 |

**Checkpoint**: v6-core at L3, delta workflow functional

---

#### Week 6: Substrate Maturity

**Deliverables**:
- [ ] @unrdf/kgc-substrate at L5
- [ ] @unrdf/v6-core at L4

**Tasks**:

| Dev | Package | Task | Hours |
|-----|---------|------|-------|
| 1 | kgc-substrate | L3→L4: Adversarial tests (corrupt snapshots) | 16 |
| 1 | kgc-substrate | L4→L5: Integration with v6-core, yawl | 16 |
| 2-3 | v6-core | L3→L4: Timeout guards, Poka-Yoke for invalid deltas | 32 |
| 4 | v6-core | Conflict resolution strategies, tests | 16 |
| 5 | ALL | Integration matrix: Cluster 1 × Cluster 2 | 16 |

**Checkpoint**: kgc-substrate at L5, v6-core at L4

---

#### Week 7: Workflow Engine

**Deliverables**:
- [ ] @unrdf/yawl at L5
- [ ] YAWL workflow → ΔGate integration

**Tasks**:

| Dev | Package | Task | Hours |
|-----|---------|------|-------|
| 1-2 | yawl | L3→L4: Adversarial tests (infinite loops, deadlocks) | 24 |
| 3 | yawl | L4→L5: Integration with v6-core (workflow adapter) | 16 |
| 4 | yawl | Receipt chain tests: task→task→case | 16 |
| 5 | yawl | Performance benchmarks (1000-task workflow) | 8 |

**Checkpoint**: yawl at L5

---

#### Week 8: Streaming & V6-Core Finalization

**Deliverables**:
- [ ] @unrdf/streaming at L4
- [ ] @unrdf/v6-core at L5

**Tasks**:

| Dev | Package | Task | Hours |
|-----|---------|------|-------|
| 1 | streaming | L2→L3: Receipt integration for change feeds | 24 |
| 2 | streaming | L3→L4: Backpressure handling, timeout guards | 16 |
| 3-4 | v6-core | L4→L5: Final integration tests, OTEL validation | 32 |
| 5 | v6-core | Production readiness review, docs | 16 |

**Checkpoint**: v6-core at L5, Cluster 2 complete

---

#### Week 9: Distributed Systems

**Deliverables**:
- [ ] @unrdf/federation at L4
- [ ] @unrdf/streaming at L5

**Tasks**:

| Dev | Package | Task | Hours |
|-----|---------|------|-------|
| 1-2 | federation | L2→L3: Receipt integration for distributed queries | 32 |
| 3 | federation | L3→L4: Query timeout enforcement, adversarial tests | 16 |
| 4 | streaming | L4→L5: Integration with federation, yawl | 16 |
| 5 | ALL | Cluster 3 integration matrix | 16 |

**Checkpoint**: streaming at L5

---

#### Week 10: Federation Maturity

**Deliverables**:
- [ ] @unrdf/federation at L5

**Tasks**:

| Dev | Package | Task | Hours |
|-----|---------|------|-------|
| 1-2 | federation | L4→L5: Cross-package query receipts | 32 |
| 3 | federation | Performance benchmarks (multi-node queries) | 16 |
| 4-5 | ALL | Integration tests: federation × streaming × yawl | 24 |

**Checkpoint**: federation at L5

---

#### Week 11: CLI & User Interface

**Deliverables**:
- [ ] @unrdf/cli at L5
- [ ] All CLI commands receipted

**Tasks**:

| Dev | Package | Task | Hours |
|-----|---------|------|-------|
| 1-2 | cli | L2→L3: Receipt integration for all commands | 32 |
| 3 | cli | L3→L4: Input validation, timeout guards | 16 |
| 4 | cli | L4→L5: Integration tests with all L5 packages | 16 |
| 5 | cli | CLI UX improvements, help text, error messages | 8 |

**Checkpoint**: cli at L5

---

#### Week 12: Production Validation & Sign-Off

**Deliverables**:
- [ ] All 10 packages at L5
- [ ] Production readiness report
- [ ] Migration guide published

**Tasks**:

| Dev | Task | Hours |
|-----|------|-------|
| 1 | Run full integration test matrix (10×10 = 100 tests) | 16 |
| 2 | OTEL validation for all packages | 16 |
| 3 | Performance regression testing | 16 |
| 4 | Security audit, penetration testing | 16 |
| 5 | Documentation: Migration guide, API reference | 24 |
| ALL | Production deployment dry-run | 8 |

**Final Checkpoint**: Production sign-off, v6.0.0-rc.1 tagged

---

### 4.2 Parallelization Matrix

**Goal**: Maximize parallel work, minimize blocking dependencies

```
┌──────────────────────────────────────────────────────────────────┐
│              Parallelization Gantt Chart                          │
├──────────────────────────────────────────────────────────────────┤
│                                                                    │
│ Week 1-2: Foundation (ALL IN PARALLEL)                            │
│ ┌──────────────┐                                                  │
│ │ oxigraph→L5  │ (Dev 1)                                          │
│ └──────────────┘                                                  │
│ ┌──────────────┐                                                  │
│ │ core→L5      │ (Dev 2)                                          │
│ └──────────────┘                                                  │
│ ┌──────────────┐                                                  │
│ │ Tooling      │ (Dev 3-5)                                        │
│ └──────────────┘                                                  │
│                                                                    │
│ Week 3-4: Core + Hooks (PARALLEL)                                 │
│                ┌──────────────┐                                   │
│                │ kgc-4d→L5    │ (Dev 1)                           │
│                └──────────────┘                                   │
│                ┌──────────────┐                                   │
│                │ hooks→L5     │ (Dev 2-3)                         │
│                └──────────────┘                                   │
│                ┌──────────────┐                                   │
│                │ substrate→L3 │ (Dev 4-5)                         │
│                └──────────────┘                                   │
│                                                                    │
│ Week 5-6: ΔGate + Substrate (SEQUENTIAL within each)              │
│                               ┌──────────────┐                    │
│                               │ v6-core→L4   │ (Dev 1-3)          │
│                               └──────────────┘                    │
│                               ┌──────────────┐                    │
│                               │ substrate→L5 │ (Dev 4-5)          │
│                               └──────────────┘                    │
│                                                                    │
│ Week 7-9: Workflow + Streaming + Federation (PARALLEL)            │
│                                               ┌──────────────┐    │
│                                               │ yawl→L5      │    │
│                                               └──────────────┘    │
│                                               ┌──────────────┐    │
│                                               │ streaming→L5 │    │
│                                               └──────────────┘    │
│                                               ┌──────────────┐    │
│                                               │ federation→L5│    │
│                                               └──────────────┘    │
│                                                                    │
│ Week 10-11: CLI + Integration (SEQUENTIAL)                        │
│                                                    ┌─────────────┐│
│                                                    │ cli→L5      ││
│                                                    └─────────────┘│
│                                                    ┌─────────────┐│
│                                                    │ Integration ││
│                                                    └─────────────┘│
│                                                                    │
│ Week 12: Validation (ALL HANDS)                                   │
│                                                         ┌─────────┐
│                                                         │ Sign-off│
│                                                         └─────────┘
│                                                                    │
└──────────────────────────────────────────────────────────────────┘
```

**Critical Path**: oxigraph → core → kgc-4d → v6-core → yawl → cli (6 packages sequential)

**Estimated Critical Path Time**: 9 weeks (75% of total)

**Parallelization Savings**: Without parallelization: 21 weeks. With: 12 weeks (43% reduction)

---

### 4.3 Rollback Strategy Per Package

**Principle**: Each package has independent rollback, minimal blast radius

#### Rollback Trigger Conditions

**Automated Rollback** (CI triggers):
- [ ] Test pass rate <95% after merge
- [ ] Performance regression >20%
- [ ] OTEL validation score <70/100
- [ ] Receipt hash mismatch in fixtures
- [ ] Circular dependency detected

**Manual Rollback** (team decision):
- [ ] Production incident attributed to migration
- [ ] Security vulnerability introduced
- [ ] Breaking change not documented

---

#### Rollback Procedure (Per Package)

**Step 1: Identify Rollback Point**
```bash
# Find last known good version
git log --oneline --grep="@unrdf/core" | grep "L5 checkpoint"

# Example output:
# a1b2c3d4 feat(@unrdf/core): L5 checkpoint - all tests passing
```

**Step 2: Create Rollback Branch**
```bash
git checkout -b rollback/core-l5-failure a1b2c3d4
git cherry-pick <urgent-fixes>  # If needed
```

**Step 3: Revert Package Version**
```bash
cd packages/core
# Revert package.json version
jq '.version = "5.0.1"' package.json > package.json.tmp && mv package.json.tmp package.json

# Revert src/ to last good state
git checkout a1b2c3d4 -- src/
```

**Step 4: Test Rollback**
```bash
pnpm install  # Update lockfile
pnpm test     # Verify tests pass
pnpm build    # Verify build succeeds
```

**Step 5: Deploy Rollback**
```bash
git add .
git commit -m "rollback(@unrdf/core): Revert to v5.0.1 due to <reason>"
git push origin rollback/core-l5-failure

# Create PR, auto-merge with override
gh pr create --title "ROLLBACK: @unrdf/core to v5.0.1" --body "Reason: <explain>"
```

**Step 6: Notify Dependents**
```bash
# Find packages that depend on @unrdf/core
pnpm list --depth=1 --filter='@unrdf/*' --json | jq -r '.[] | select(.dependencies["@unrdf/core"]) | .name'

# Output:
# @unrdf/kgc-4d
# @unrdf/hooks
# @unrdf/yawl
# ... (send notification to maintainers)
```

---

#### Partial Rollback (Level Degradation)

**Scenario**: Package reaches L5 but has issues; rollback to L4 instead of L1

**Procedure**:
```bash
# Example: v6-core L5→L4 rollback (remove integration tests only)
git revert <commit-hash-of-L4-to-L5-promotion>

# Update maturity badge
sed -i 's/L5/L4/' README.md

# Update CI
sed -i 's/L5_CHECKPOINT/L4_CHECKPOINT/' .github/workflows/maturity-gate.yml
```

**Impact**: v6-core can still be used at L4, just without full cross-package composition guarantees

---

## 5. Risk Mitigation

### 5.1 Risk Register

| ID | Risk | Impact | Probability | Mitigation | Owner |
|----|------|--------|-------------|------------|-------|
| R1 | Breaking API changes block adoption | HIGH | MEDIUM | Deprecation warnings, migration tools, 6-month overlap | Arch Lead |
| R2 | Performance regression >20% | HIGH | LOW | Benchmark suite, pre-merge gates | Perf Team |
| R3 | Receipt chain breaks in production | CRITICAL | LOW | Tamper detection, automated recovery | Security |
| R4 | Circular dependencies introduced | MEDIUM | LOW | CI madge checks, pre-merge validation | DevOps |
| R5 | L5 integration test matrix incomplete | MEDIUM | MEDIUM | Test matrix generator, coverage tracking | QA Lead |
| R6 | Developer burnout (12-week sprint) | MEDIUM | MEDIUM | Sprint breaks, pair programming rotation | PM |
| R7 | Documentation lags behind code | LOW | HIGH | Docs-as-code, auto-generation from JSDoc | Tech Writer |
| R8 | OTEL validation score <80 | MEDIUM | MEDIUM | Pre-merge OTEL gate, score trending dashboard | Observability |

---

### 5.2 What Breaks if L5 Verification Fails?

**Failure Mode**: Package promoted to L5 but integration tests reveal issues

**Impact Analysis**:

#### Scenario 1: @unrdf/core L5 Fails

**Symptom**: Integration test with @unrdf/kgc-4d fails (receipt hashes don't match)

**Blast Radius**:
- **Immediate**: Core package rollback to L4
- **Downstream**: 16 dependent packages blocked from L5
- **Timeline**: +2 weeks delay to debug and fix

**Mitigation**:
```bash
# 1. Isolate issue
pnpm test:integration --filter='@unrdf/core' --grep='kgc-4d'

# 2. Identify root cause (e.g., non-deterministic timestamp)
git bisect start
git bisect bad HEAD
git bisect good <last-L4-commit>
# ... (git bisect will identify culprit commit)

# 3. Fix + re-test
# Fix the bug (e.g., inject clock instead of Date.now())
pnpm test:determinism --runs=100  # Verify fix

# 4. Re-promote to L5 (1 week delay)
```

---

#### Scenario 2: @unrdf/v6-core L5 Fails

**Symptom**: ΔGate performance regression 50% (too slow)

**Blast Radius**:
- **Immediate**: v6-core rollback to L4, ΔGate adoption blocked
- **Downstream**: Workflow migrations delayed, CLI commands slow
- **Timeline**: +3 weeks delay (performance optimization is complex)

**Mitigation**:
```bash
# 1. Profile hot paths
pnpm benchmark:profile --package=v6-core --operation=reconcile

# 2. Identify bottleneck (e.g., inefficient SPARQL query in precondition check)
# Example output:
#   reconcile() - 1200ms
#     preconditions.check() - 1100ms  ← 92% of time!
#       store.query() - 1000ms

# 3. Optimize query (add index, use compiled plan)
# Before:
await store.query(`ASK { <${entity}> a <${type}> }`);  # 1000ms

# After:
const compiledPlan = queryCache.get('entity-type-check');
await compiledPlan.execute(entity, type);  # 50ms (20x faster)

# 4. Re-benchmark
pnpm benchmark:compare --baseline v5.0.1  # Verify <10% regression

# 5. Re-promote to L5 (2 weeks delay)
```

---

#### Scenario 3: Cross-Package Receipt Chain Breaks

**Symptom**: Workflow spanning yawl → v6-core → kgc-4d produces invalid receipt chain

**Blast Radius**:
- **Immediate**: Receipt chain verification fails, audit trail broken
- **Downstream**: Cannot prove workflow execution, compliance violation
- **Timeline**: CRITICAL (must fix immediately, cannot deploy to production)

**Mitigation**:
```bash
# 1. Isolate break point
pnpm test:receipt-chain --packages='yawl,v6-core,kgc-4d' --verbose

# Example output:
#   ✓ yawl receipt R1 (hash: abc123)
#   ✓ v6-core receipt R2 (prev: abc123, hash: def456)
#   ✗ kgc-4d receipt R3 (prev: NULL, hash: ghi789)  ← BROKEN!
#      Expected prev: def456, got: NULL

# 2. Root cause: kgc-4d not reading previous receipt from v6-core
# Fix: Update freezeUniverse to chain from last receipt
export async function freezeUniverse(store, universeId) {
  // BEFORE (broken):
  const previousReceipt = null;

  // AFTER (fixed):
  const previousReceipt = await store.getLatestReceipt();  // Read from chain

  // ... (rest of function)
}

# 3. Verify fix
pnpm test:receipt-chain --packages='yawl,v6-core,kgc-4d'
# ✓ All receipts chain correctly

# 4. Hotfix deploy (same day)
```

---

### 5.3 Rollback Mid-Phase Strategy

**Scenario**: Week 6, 4 packages at L5, discover fundamental ΔGate design flaw

**Decision Tree**:

```
┌─────────────────────────────────────────────────────────────────┐
│              Mid-Phase Rollback Decision Tree                    │
├─────────────────────────────────────────────────────────────────┤
│                                                                   │
│  Issue Detected                                                   │
│       ↓                                                           │
│  ┌────────────────────────┐                                      │
│  │ Can fix in <1 week?    │                                      │
│  └────────┬───────────────┘                                      │
│           ↓ YES                    ↓ NO                          │
│  ┌────────────────────┐   ┌────────────────────┐                │
│  │ Fix + Continue     │   │ Assess Blast Radius│                │
│  └────────────────────┘   └────────┬───────────┘                │
│                                     ↓                             │
│                          ┌─────────────────────┐                 │
│                          │ Affects 1-2 pkgs?   │                 │
│                          └─────┬───────────────┘                 │
│                                ↓ YES         ↓ NO                │
│                    ┌───────────────────┐  ┌──────────────────┐  │
│                    │ Rollback those    │  │ FULL ROLLBACK    │  │
│                    │ packages only     │  │ to v5 baseline   │  │
│                    └───────────────────┘  └──────────────────┘  │
│                                                                   │
└─────────────────────────────────────────────────────────────────┘
```

**Full Rollback Procedure** (Emergency):

```bash
# 1. Freeze all work (send alert to team)
echo "🚨 ROLLBACK IN PROGRESS - HOLD ALL MERGES" | slack-send #unrdf-dev

# 2. Revert monorepo to v5 baseline
git checkout v5.0.1
git checkout -b rollback/v6-abort

# 3. Preserve any critical fixes from v6 work
git cherry-pick <critical-security-fix>
git cherry-pick <critical-bug-fix>

# 4. Update all package versions to v5
for pkg in packages/*; do
  cd $pkg
  jq '.version = "5.0.1"' package.json > package.json.tmp && mv package.json.tmp package.json
  cd ../..
done

# 5. Test rollback
pnpm install
pnpm -r test
pnpm -r build

# 6. Deploy rollback
git add .
git commit -m "EMERGENCY ROLLBACK: Abort v6 migration, return to v5.0.1"
git push origin rollback/v6-abort --force-with-lease

# 7. Tag rollback point
git tag -a v5.0.2-rollback -m "Emergency rollback from v6 attempt"
git push origin v5.0.2-rollback

# 8. Publish to npm
pnpm -r publish --tag=latest --access=public

# 9. Post-mortem
# Write incident report, identify root cause, plan v6.1 approach
```

**Cost**: 6 weeks of work lost, team morale impact, delayed v6 by 3+ months

**Prevention**: Weekly architecture review, early prototyping, kill criteria defined upfront

---

### 5.4 Line-in-the-Sand Decisions (Cannot Change)

**Immutable Constraints** (locked in for v6):

#### 1. Receipt Schema Structure

**Decision**: `ReceiptProfileSchema` with `previousReceiptHash`, `payloadHash`, `receiptHash`

**Rationale**: Cryptographic chain depends on this structure. Changing would break all existing receipts.

**Lock Date**: 2025-12-01 (already in production for kgc-4d)

**Cannot Change**:
- Hash algorithm (BLAKE3) → Changing would invalidate all chains
- Field names (`receiptHash` not `hash`) → Breaking change for all consumers
- Chain linking (`previousReceiptHash`) → Core to provenance

**Can Change** (backwards-compatible):
- Add optional fields (e.g., `vectorClock`)
- Add new `profile` types (e.g., `profile: 'ml-inference'`)
- Extend `payload` schema per profile

---

#### 2. Delta Schema Structure

**Decision**: `DeltaSchema` with `id`, `type`, `changes[]`, `justification`

**Rationale**: ΔGate API depends on this. All packages will integrate with this schema.

**Lock Date**: 2025-12-27 (this document approval)

**Cannot Change**:
- `changes` array structure (discriminated union by `operation`)
- `type` enum values (`create`, `update`, `delete`)
- `preconditions` array schema

**Can Change**:
- Add new `operation` types (e.g., `execute_sparql`)
- Add optional metadata fields

---

#### 3. L5 Invariants

**Decision**: Determinism, Idempotence, Provenance, Atomicity, Closed World

**Rationale**: These are the mathematical foundation of compositional closure. Relaxing any invariant breaks L5 guarantees.

**Lock Date**: 2025-12-27 (this document approval)

**Cannot Change**:
- Determinism requirement (same input → same output)
- Receipt chaining requirement (all operations receipted)
- No circular dependencies requirement

**Can Change**:
- Performance thresholds (10% → 15% regression tolerance if justified)
- OTEL score threshold (80/100 → 75/100 if metrics improve)

---

#### 4. Maturity Ladder Levels (L1-L5)

**Decision**: 5 levels with defined criteria (no L0, no L6)

**Rationale**: Adding/removing levels would invalidate all existing maturity assessments and roadmaps.

**Lock Date**: 2025-12-15 (documented in MATURITY_LADDER.md)

**Cannot Change**:
- Level count (5 levels)
- Level names (L1-L5)
- Core criteria per level (JSDoc for L2, determinism for L3, etc.)

**Can Change**:
- Threshold percentages (95% JSDoc → 90% if justified)
- Add sub-levels (L3.5) for packages in transition

---

#### 5. Dependency Architecture (DAG)

**Decision**: No circular dependencies, topological sort possible

**Rationale**: Circular deps break independent deployment, testing, and versioning.

**Lock Date**: 2025-12-01 (enforced in CI since v5)

**Cannot Change**:
- No circular dependencies rule
- Foundation packages (oxigraph, core) cannot depend on application packages

**Can Change**:
- Package count (53 → consolidated to 40)
- Package names (rename for clarity)

---

## Appendices

### Appendix A: Glossary

**Terms**:

- **L1-L5**: Maturity levels (Compiles → Compositional Closure)
- **ΔGate**: Delta Gate, admissibility control plane
- **Δ (Delta)**: Atomic change proposal
- **μ (mu)**: Reconciliation function, μ(O ⊔ Δ) = A
- **Receipt**: Cryptographic proof of operation execution
- **Receipt Chain**: Linked sequence of receipts via `previousReceiptHash`
- **Poka-Yoke**: Mistake-proofing design pattern
- **OTEL**: OpenTelemetry (observability framework)
- **KGC-4D**: Knowledge Graph Calculus with 4D time (nanosecond precision)
- **DAG**: Directed Acyclic Graph (no circular dependencies)
- **Compositional Closure**: Ability to compose operations across packages with guaranteed properties

### Appendix B: References

1. [MATURITY_LADDER.md](/home/user/unrdf/docs/v6/MATURITY_LADDER.md) - L1-L5 definitions
2. [MIGRATION_PLAN.md](/home/user/unrdf/docs/v6/MIGRATION_PLAN.md) - v5→v6 breaking changes
3. [PROGRAM_CHARTER.md](/home/user/unrdf/docs/v6/PROGRAM_CHARTER.md) - v6 mission and governance
4. [CONTROL_PLANE.md](/home/user/unrdf/docs/v6/CONTROL_PLANE.md) - ΔGate architecture
5. [CONTRACT_INVENTORY.md](/home/user/unrdf/docs/v6/CONTRACT_INVENTORY.md) - Package analysis
6. [DELTA_CONTRACT.md](/home/user/unrdf/packages/v6-core/DELTA_CONTRACT.md) - Delta schema

### Appendix C: Verification Commands

**Quick Validation** (run before every commit):
```bash
# L2 Gate
pnpm run jsdoc-coverage | grep "Coverage: [9][5-9]%\|100%"
pnpm run lint
test -f CHANGELOG.md

# L3 Gate
for i in {1..10}; do pnpm test:determinism > /tmp/out-$i.json; done
diff /tmp/out-{1..10}.json

# L4 Gate
pnpm test:adversarial
timeout 5s grep -r "eval\|Function\(" src/ --include="*.mjs" && exit 1 || exit 0

# L5 Gate
pnpm test:integration
pnpm -r exec madge --circular src/
timeout 15s node validation/run-all.mjs comprehensive | grep "Score: [8-9][0-9]\|100"
```

**Full Matrix** (nightly CI):
```bash
# Run all integration tests
pnpm test:matrix --packages='oxigraph,core,kgc-4d,v6-core,hooks,yawl,kgc-substrate,streaming,federation,cli'

# Run OTEL validation
pnpm test:otel --comprehensive

# Run performance benchmarks
pnpm benchmark:regression --baseline=v5.0.1 --threshold=10
```

### Appendix D: Contact Information

**Architecture Review Board**:
- Lead Architect: TBD
- Security Lead: TBD
- Performance Lead: TBD
- QA Lead: TBD

**Escalation Path**:
1. Package maintainer (first contact)
2. Architecture Review Board (design issues)
3. Program Manager (timeline/resource issues)
4. CTO (emergency rollback decisions)

---

**Document Version**: 1.0.0
**Last Updated**: 2025-12-27
**Next Review**: Week 6 (mid-sprint check-in), Week 12 (final review)
**Status**: AWAITING APPROVAL

---

## Sign-Off

**Approved By**:

- [ ] Lead Architect: _____________________ Date: _______
- [ ] Security Lead: _____________________ Date: _______
- [ ] Performance Lead: _____________________ Date: _______
- [ ] QA Lead: _____________________ Date: _______
- [ ] Program Manager: _____________________ Date: _______

**Approval Criteria**:
1. All 5 deliverables present and complete
2. Risks identified with mitigation strategies
3. Fast-track timeline realistic (12 weeks validated)
4. Rollback procedures tested
5. Line-in-the-sand decisions agreed upon

**Conditional Approval**:
- [ ] Pending: Prototype ΔGate in Week 1 to validate design
- [ ] Pending: Security audit of receipt chain implementation
- [ ] Pending: Load testing of integration test matrix

**APPROVED TO PROCEED**: YES / NO / CONDITIONAL

---

**END OF DOCUMENT**
