# How-To Guides: Task-Focused Solutions

**Solve specific problems with narrative state chain.**

## Available Guides

### Complete

1. **[Enforce Guards](enforce-guards.md)** — Add admissibility policies (H)
   - Define policies
   - Register with gate
   - Handle rejections
   - **Evidence:** [packages/v6-core/src/delta/gate.mjs](/home/user/unrdf/packages/v6-core/src/delta/gate.mjs)

---

### Skeleton (To Be Completed)

2. **Define a Universe Schema** — Set up Σ with RDF constraints
   - **Evidence:** [packages/kgc-4d/src/store.mjs](/home/user/unrdf/packages/kgc-4d/src/store.mjs)

3. **Write Reconciliation Logic** — Implement μ(O ⊔ Δ) with conflict resolution
   - **Evidence:** [packages/v6-core/src/delta/reconcile.mjs](/home/user/unrdf/packages/v6-core/src/delta/reconcile.mjs)

4. **Create Bridges** — Build Φ translators between domains
   - **Evidence:** [packages/v6-core/src/delta/adapters/](/home/user/unrdf/packages/v6-core/src/delta/adapters/)

5. **Verify Receipts** — Validate cryptographic proofs
   - **Evidence:** [packages/v6-core/src/delta/schema.mjs](/home/user/unrdf/packages/v6-core/src/delta/schema.mjs):137

6. **Handle Failed Admissibility** — Recover from policy violations
   - **Evidence:** [packages/v6-core/src/delta/gate.mjs](/home/user/unrdf/packages/v6-core/src/delta/gate.mjs):141

---

## Quick Index

| Problem | Guide |
|---------|-------|
| Need to control who can modify state? | [Enforce Guards](enforce-guards.md) |
| Delta rejected, need to debug? | Handle Failed Admissibility (skeleton) |
| Want deterministic conflict resolution? | Write Reconciliation Logic (skeleton) |
| Translating between domains? | Create Bridges (skeleton) |
| Need to validate state transitions? | Verify Receipts (skeleton) |

---

**Next:** [Reference Documentation](../reference/README.md)
