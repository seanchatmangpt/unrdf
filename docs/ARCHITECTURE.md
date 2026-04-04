# Architecture: O\* Innovations 4-6

---

## System Architecture

```
┌────────────────────────────────────────────────────────┐
│ O* INNOVATIONS LAYER (NEW)                             │
├────────────────────────────────────────────────────────┤
│                                                        │
│  ┌─────────────────┐  ┌─────────────────┐             │
│  │ Innovation 4    │  │ Innovation 5    │             │
│  │ Federation      │  │ Hooks Market    │             │
│  │ Quorum Voting   │  │ place Compose   │             │
│  │                 │  │                 │             │
│  │ M-of-N votes    │  │ RDF norms +     │             │
│  │ + BLAKE3        │  │ N3 deps +       │             │
│  │ receipts        │  │ SHACL soft-fail │             │
│  └────────┬────────┘  └────────┬────────┘             │
│           │                    │                       │
│  ┌────────▼────────────────────▼────────┐             │
│  │ Innovation 6: Streaming Admission    │             │
│  │ Delta receipts + chaining            │             │
│  │ Input/output/delta BLAKE3 hashes     │             │
│  └────────┬─────────────────────────────┘             │
│           │                                            │
├───────────┼────────────────────────────────────────────┤
│ HOOKS SYSTEM (EXISTING)                                │
│                                                        │
│  Condition Evaluator (SPARQL, SHACL, N3, Datalog)    │
│  Effect Executor (functions, SPARQL CONSTRUCT)        │
│  Receipt Chaining (from v6-core)                      │
│                                                        │
├──────────────────────────────────────────────────────┤
│ RDF FOUNDATION                                         │
│                                                        │
│  Oxigraph (SPARQL + RDF store)                        │
│  EYE Reasoner (N3 forward-chaining)                   │
│  Zod Validation (schemas)                             │
└────────────────────────────────────────────────────────┘
```

---

## Innovation 4: Federation Quorum Data Flow

```
Input: proposalId, approvals { id → boolean }
   │
   ├─→ Validate approvals against validator config (Zod)
   │
   ├─→ Compute quorum:
   │    • Count approving validators OR sum weights
   │    • Compare to requiredVotes OR requiredWeight
   │
   ├─→ Generate receipt:
   │    • canonicalize(proposalId + approvals)
   │    • inputHash = BLAKE3(canonical)
   │    • outputHash = BLAKE3(decision result)
   │    • receiptHash = BLAKE3(receipt object)
   │    • previousReceiptHash = link to prior
   │
Output: { approved, receipt, approvingValidators }
```

### Determinism Guarantee

Same input (proposalId + same approvals) produces same receipt hashes across multiple runs:

- Canonical JSON serialization (sorted keys)
- Deterministic BLAKE3 hashing
- No random state

---

## Innovation 5: Hooks Marketplace Data Flow

```
Input: Hook definition (JSON/YAML)
   │
   ├─→ SPARQL CONSTRUCT Normalization
   │    • Parse hook name, version, dependencies
   │    • Generate RDF triples:
   │      ?hook a hook:Hook
   │      ?hook hook:name "payment-validator"
   │      ?hook hook:hasCondition ?cond
   │      ?hook hook:hasEffect ?effect
   │      ?hook hook:dependsOn "basic-validator"
   │    • Store in Oxigraph RDF store
   │
   ├─→ N3 Forward-Chaining Dependency Resolution
   │    • N3 rules: { ?h1 hook:dependsOn ?h2 }
   │              => { ?h1 hook:allDeps ?h2 }
   │    • Transitive closure: A→B→C gives A allDeps [B,C]
   │    • Circular detection: Any node with self-path? Reject.
   │
   ├─→ SHACL Soft-Fail Validation (annotate mode)
   │    • Evaluate against hook shape
   │    • If violations exist:
   │      - Generate SHACL report as RDF triples
   │      - Store violations in marketplace store
   │      - DO NOT block admission (soft-fail)
   │    • Violations queryable via SPARQL
   │
Output: { admitted=true, violations=[], resolvedDeps=[] }
```

### Key Property: Soft-Fail

```javascript
// Even if SHACL validation fails:
const result = await marketplace.admit(badHook);
// result.admitted = true  ← Always admits
// result.violations = []  ← Captures failures as RDF
```

Admin can decide later if it's worth fixing based on audit trail.

---

## Innovation 6: Streaming Admission Data Flow

```
Input: deltas (RDF quads to add/remove), condition (optional)
   │
   ├─→ Phase 1: Before
   │    • inputHash = getStoreStateHash(store)
   │      [BLAKE3 of all canonical quads]
   │
   ├─→ Phase 2: Apply (tentatively)
   │    • Apply all deltas to store in memory
   │
   ├─→ Phase 3: After
   │    • outputHash = getStoreStateHash(store)
   │      [BLAKE3 of store with deltas applied]
   │
   ├─→ Phase 4: Compute Delta Hash
   │    • deltaHash = BLAKE3(canonical delta bytes)
   │
   ├─→ Phase 5: Generate Receipt
   │    • receipt.inputHash = hash from Phase 1
   │    • receipt.outputHash = hash from Phase 3
   │    • receipt.deltaHash = hash from Phase 4
   │    • receipt.receiptHash = BLAKE3(receipt object)
   │    • receipt.previousReceiptHash = link to prior
   │
   ├─→ Phase 6: Evaluate Delta Condition (if provided)
   │    • Condition checks: receipt.outputHash == expected?
   │    • If fails: ROLLBACK to pre-Phase 2 state, return admitted=false
   │    • If passes OR no condition: keep deltas, return admitted=true
   │
Output: { admitted, receipt with 4 hashes + chain link }
```

### Determinism Guarantee

Same delta applied to same store state produces identical hashes:

- inputHash(S) + deltaHash(Δ) → outputHash(S+Δ)
- Reproducible from receipt alone

---

## Determinism Verification

All 3 innovations verify determinism:

```javascript
// Test: Run same operation twice
const result1 = await quorum.decide(proposal, approvals);
const result2 = await quorum.decide(proposal, approvals);

// Assertion: Same receipt hash
assert(result1.receipt.receiptHash === result2.receipt.receiptHash);
```

---

## Receipt Chaining Across Innovations

```
Time progression:

T1: Federation Quorum vote
    receipt1 = BLAKE3(proposal + votes)
    receipt1.receiptHash = hash1
    ↓

T2: Hooks Marketplace admission
    receipt2.previousReceiptHash = hash1
    receipt2.receiptHash = hash2
    ↓

T3: Streaming delta admission
    receipt3.previousReceiptHash = hash2
    receipt3.receiptHash = hash3

Complete chain: hash1 → hash2 → hash3
Proof: Each decision links to the previous one
```

---

## Integration Points

### With @unrdf/hooks

- Condition evaluator: Innovations 5 & 6 use existing conditions
- Effect executor: Innovations 5 uses sparql-construct effects
- Receipt pattern: All use withReceipt() from v6-core

### With @unrdf/oxigraph

- Store: Marketplace uses OxigraphStore
- SPARQL: Marketplace queries use store.query()
- getStoreStateHash(): Streaming uses for input/output hashes

### With @unrdf/v6-core

- Receipt pattern: All use withReceipt, createContext, blake3Hash
- Canonicalize: All use canonical JSON for determinism

---

## Performance Characteristics

| Component         | Bottleneck          | Optimization                  |
| ----------------- | ------------------- | ----------------------------- |
| Federation Quorum | Zod validation      | In-memory, <1ms               |
| Marketplace       | N3 forward-chaining | Floyd-Warshall O(n³) for deps |
| Streaming         | BLAKE3 hashing      | Parallel hash computation     |

---

**Version**: 26.4.4 | **Date**: April 3, 2026
