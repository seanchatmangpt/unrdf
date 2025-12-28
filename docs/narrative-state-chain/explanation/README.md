# Explanation: Understanding Design Decisions

**Deep dives into the "why" behind narrative state chain.**

## Available Explanations

### Skeleton (To Be Completed)

All explanation docs are placeholders pending completion.

---

### Architectural Decisions

1. **[Why Hash-Addressed Identity?](identity-architecture.md)**
   - Tamper detection via BLAKE3
   - Merkle anchoring for proofs
   - Content-addressable storage benefits
   - **Evidence:** [packages/kgc-4d/src/freeze.mjs](/home/user/unrdf/packages/kgc-4d/src/freeze.mjs):35

2. **[Why Deterministic Reconciliation?](why-determinism.md)**
   - μ as pure function
   - Replay guarantees
   - Time-travel correctness
   - **Evidence:** [packages/v6-core/src/delta/reconcile.mjs](/home/user/unrdf/packages/v6-core/src/delta/reconcile.mjs)

3. **[Guard Semantics](guard-semantics.md)**
   - Composable policies
   - Auditable denials
   - Fail-fast vs fail-safe
   - **Evidence:** [packages/v6-core/src/delta/gate.mjs](/home/user/unrdf/packages/v6-core/src/delta/gate.mjs)

4. **[Bridge Proofs](bridge-proofs.md)**
   - Type-preserving translation
   - Domain adapter correctness
   - Proof composition
   - **Evidence:** [packages/v6-core/src/delta/adapters/](/home/user/unrdf/packages/v6-core/src/delta/adapters/)

5. **[Invariant Design](invariant-design.md)**
   - Strict vs permissive evolution
   - Zod schema validation
   - Guard vs invariant trade-offs
   - **Evidence:** [packages/kgc-4d/src/guards.mjs](/home/user/unrdf/packages/kgc-4d/src/guards.mjs)

---

## Key Questions Answered

| Question | Explanation |
|----------|-------------|
| Why can't I mutate state directly? | Hash-addressed identity requires immutable transitions |
| Why does every delta produce a receipt? | Auditability + proof-based admission |
| Why are guards separate from reconciliation? | Separation of concerns: admission control ≠ merge logic |
| Why hash-based identity instead of UUIDs? | Content addressing prevents tampering |
| Why deterministic replay? | Time-travel + reproducibility guarantees |

---

## Design Principles

1. **Proof-Oriented Programming**
   - Receipts as cryptographic proof
   - No mutations without evidence
   - Replay as verification

2. **Separation of Concerns**
   - Guards (H) → Admission control
   - Reconciliation (μ) → Merge logic
   - Receipts (R) → Proof generation

3. **Composability**
   - Policies compose via constraints
   - Bridges compose via adapters
   - Snapshots compose via Merkle chains

4. **Fail-Fast**
   - Validate early (schema → guards → reconcile)
   - Reject unlawful deltas before state change
   - All-or-none atomicity

---

**Back to:** [Main Documentation](../README.md)
