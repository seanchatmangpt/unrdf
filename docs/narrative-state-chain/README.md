# Narrative State Chain Documentation

**Evidence-Based Documentation for UNRDF's Narrative State Chain Architecture**

## What is Narrative State Chain?

The **Narrative State Chain** is UNRDF's architecture for managing state mutations through:

- **Universe (Σ)**: Hash-addressed RDF knowledge graphs with frozen snapshots
- **Scene**: Delta operations that propose changes with receipts as proof
- **Reconciliation (μ)**: Deterministic merge function μ(O ⊔ Δ) for state evolution
- **Guards (H)**: Admissibility policies enforcing invariants
- **Receipts**: Cryptographic proof of every state transition
- **Bridges (Φ)**: Type-preserving translators between domains

**Key Invariants:**
- All state changes flow through Δ (Delta) — no direct mutations
- Every Δ produces a receipt (success or denial)
- Hash-addressed identity prevents tampering
- Deterministic replay guarantees reproducibility

---

## Choose Your Learning Path

### New to Narrative State Chain?
Start here:
1. **[Tutorial: Your First Scene](tutorial/01-hello-world.md)** — Create a universe, add a delta, verify receipt (10 min)
2. **[Tutorial: Freeze & Time Travel](tutorial/02-freeze-and-replay.md)** — Snapshot state and reconstruct history (15 min)

### Solving Specific Problems?
**How-To Guides:**
- [Define a Universe Schema](how-to/define-universe.md) — Set up Σ with RDF constraints
- [Write Reconciliation Logic](how-to/write-reconciliation.md) — Implement μ(O ⊔ Δ) with conflict resolution
- [Enforce Guards](how-to/enforce-guards.md) — Add admissibility policies (H)
- [Create Bridges](how-to/create-bridges.md) — Build Φ translators between domains
- [Verify Receipts](how-to/verify-receipts.md) — Validate cryptographic proofs
- [Handle Failed Admissibility](how-to/handle-failures.md) — Recover from policy violations

### Looking Up API Details?
**Reference:**
- [Universe API](reference/api-universe.md) — KGCStore operations (add, match, freeze)
- [Scene API](reference/api-scene.md) — Delta schema and operations
- [Bridge API](reference/api-bridge.md) — Adapter patterns (workflow, resource, GraphQL)
- [Receipt API](reference/api-receipt.md) — Receipt schema and validation
- [Error Codes](reference/error-codes.md) — Standard error taxonomy
- [Data Shapes](reference/data-shapes.md) — RDF schemas (TTL examples)

### Understanding Design Decisions?
**Explanations:**
- [Why Hash-Addressed Identity?](explanation/identity-architecture.md) — Tamper detection via Merkle anchoring
- [Why Deterministic Reconciliation?](explanation/why-determinism.md) — μ as pure function
- [Guard Semantics](explanation/guard-semantics.md) — Composable, auditable policies
- [Bridge Proofs](explanation/bridge-proofs.md) — Type-preserving translation
- [Invariant Design](explanation/invariant-design.md) — Trade-offs between strict vs permissive evolution

---

## Key Concepts (Quick Reference)

| Concept | Symbol | Implementation | Evidence |
|---------|--------|----------------|----------|
| **Universe** | Σ | `KGCStore` | [packages/kgc-4d/src/store.mjs](/home/user/unrdf/packages/kgc-4d/src/store.mjs) |
| **Scene** | Δ | `Delta` operations | [packages/v6-core/src/delta/schema.mjs](/home/user/unrdf/packages/v6-core/src/delta/schema.mjs) |
| **Reconciliation** | μ | `reconcile(store, delta)` | [packages/v6-core/src/delta/reconcile.mjs](/home/user/unrdf/packages/v6-core/src/delta/reconcile.mjs) |
| **Guards** | H | `DeltaGate` + policies | [packages/v6-core/src/delta/gate.mjs](/home/user/unrdf/packages/v6-core/src/delta/gate.mjs) |
| **Receipt** | R | `DeltaReceipt` | [packages/v6-core/src/delta/schema.mjs](/home/user/unrdf/packages/v6-core/src/delta/schema.mjs):137 |
| **Bridge** | Φ | Adapters | [packages/v6-core/src/delta/adapters/](/home/user/unrdf/packages/v6-core/src/delta/adapters/) |
| **Freeze** | — | `freezeUniverse()` | [packages/kgc-4d/src/freeze.mjs](/home/user/unrdf/packages/kgc-4d/src/freeze.mjs):35 |
| **Time Travel** | — | `reconstructState()` | [packages/kgc-4d/src/freeze.mjs](/home/user/unrdf/packages/kgc-4d/src/freeze.mjs) |

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                      UNIVERSE (Σ)                           │
│                   Hash-Addressed KGC Store                  │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       ▼
              ┌────────────────┐
              │  SCENE (Δ)     │  ← Propose changes
              │  Delta Ops     │
              └────────┬───────┘
                       │
                       ▼
              ┌────────────────┐
              │  GUARDS (H)    │  ← Admissibility check
              │  ΔGate Policy  │
              └────────┬───────┘
                       │
                       ▼
              ┌────────────────┐
              │  μ(O ⊔ Δ)     │  ← Reconciliation
              │  Merge Logic   │
              └────────┬───────┘
                       │
                       ▼
              ┌────────────────┐
              │  RECEIPT (R)   │  ← Cryptographic proof
              │  Hash + Proof  │
              └────────────────┘
```

**Flow:**
1. Propose Δ (scene) with operations (add/delete/update)
2. Check H (guards) via admissibility policies
3. If lawful → μ(O ⊔ Δ) (reconcile)
4. Atomic commit → Generate R (receipt)
5. If unlawful → Denial receipt with reason

---

## Evidence Trail

Every claim in this documentation maps to:
- **Code**: Absolute file paths to implementation
- **Tests**: Proof of correctness (pass/fail)
- **Examples**: Runnable demonstrations

**Verification Command:**
```bash
# Run all narrative-state-chain tests
npm test -- packages/v6-core/src/delta packages/kgc-4d

# Run specific proof (freeze + replay)
node packages/kgc-4d/test/4d-time-travel-validation.test.mjs
```

**No speculation. Only proven capabilities.**

---

## Quick Start (5 minutes)

```javascript
import { createStore } from '@unrdf/core';
import { DeltaGate, createDelta } from '@unrdf/v6-core/delta';

// 1. Create universe (Σ)
const store = createStore();
const gate = new DeltaGate();

// 2. Propose scene (Δ)
const delta = createDelta('add',
  'http://example.org/Alice',
  'http://example.org/name',
  'Alice Smith',
  { package: '@my-app', actor: 'user-1' }
);

// 3. Apply via gate (H + μ)
const receipt = await gate.proposeDelta(delta, store);

// 4. Verify receipt (R)
console.assert(receipt.applied, 'Delta was applied');
console.assert(receipt.stateHash, 'Has state commitment');
console.log('Operations applied:', receipt.operationsApplied);
```

**Next:** [Tutorial: Your First Scene](tutorial/01-hello-world.md)

---

## License

MIT (same as UNRDF)
