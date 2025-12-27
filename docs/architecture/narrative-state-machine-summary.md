# Narrative State Machine - Executive Summary

**Document Type**: Architecture Decision Summary
**Status**: DRAFT - Awaiting Consensus
**Date**: 2025-12-27
**Author**: System Architecture Designer

---

## Critical Questions Answered

### Q1: Where does RDF fit?

**Answer**: RDF is the **universal data format** throughout the entire system.

**Specifics**:
- **Observations**: RDF quads (Turtle, JSON-LD, N-Triples as input formats)
- **Delta**: Arrays of RDF quads (`additions[]`, `deletions[]`)
- **Schema (Σ)**: SHACL shapes (RDF-based constraints)
- **Invariants (Q)**: SPARQL ASK queries (RDF query language)
- **Universe state**: Stored in `@unrdf/oxigraph` triple store
- **Scenes**: Persisted as N-Quads files (one file per scene)
- **Receipts**: JSON-LD (RDF-compatible + human-readable)
- **Artifacts**: Can be RDF (derived ontologies) OR binary (PDFs, etc.)

**Key Implementation Detail**:
```javascript
// Scene storage format
scene-sha256-abc123.nq:
  <urn:scene:abc123> rdf:type ex:Scene .
  <urn:scene:abc123> ex:observation _:obs1 .
  _:obs1 ex:subject <urn:agent:alice> .
  _:obs1 ex:predicate ex:votedFor .
  _:obs1 ex:object "option-A" .
```

**Content Addressing**: All RDF content hashed using **canonical N-Quads** (sorted, deterministic).

---

### Q2: Is μ a pure function or can it have side effects?

**Answer**: μ is **strictly pure** (deterministic, no I/O, no randomness).

**Rationale**:
1. **Auditability**: Replaying scenes must yield identical receipts
2. **Formal verification**: Minimality proofs require determinism
3. **Testing**: Pure functions are trivially testable

**Side Effects Handled Via**:
```javascript
// μ returns side effect TOKENS (not executions)
async function reconcile(observations) {
  // ✅ Pure computation only
  const delta = computeDiff(currentState, observations);

  return {
    status: 'accepted',
    resultingGraph: applyDelta(currentState, delta),
    sideEffectTokens: [  // Opaque instructions for executor
      {type: 'email', to: 'alice@example.com', ...},
      {type: 'webhook', url: 'https://...', payload: {...}}
    ]
  };
}

// ❌ FORBIDDEN (side effect inside μ)
async function reconcile(observations) {
  await fetch('https://api.example.com');  // VIOLATES PURITY
  return {...};
}
```

**Execution Model**:
1. μ runs → generates tokens
2. Receipt signed (state committed)
3. **Separate executor** processes tokens asynchronously
4. If side effect fails → logged but receipt remains valid (state already committed)

**Guarantee**: Scene processing is **synchronous** (deterministic), side effects are **asynchronous** (best-effort).

---

### Q3: Can invariants Q be violated temporarily?

**Answer**: Yes, with **enforcement mode = 'eventual'**.

**Three Enforcement Modes**:

| Mode | Behavior | Use Case |
|------|----------|----------|
| **strict** (default) | MUST hold before AND after transition. Violation → immediate rejection | Balance checks, vote limits |
| **eventual** | Can violate for up to `MAX_SCENES` (config: 10). Must converge | Multi-step atomic operations |
| **advisory** | Logged but not enforced. For metrics/warnings only | Performance hints, style checks |

**Example (Multi-Step Workflow)**:
```
Invariant: "sum(accounts) = CONSTANT"

Scene 1: Debit account A (-$100)
  → Invariant VIOLATED (sum decreased)
  → Mode = 'eventual' → Accept but flag as "in-repair"

Scene 2: Credit account B (+$100)
  → Invariant RESTORED
  → Clear "in-repair" flag

If Scene 2 doesn't arrive within MAX_SCENES (10):
  → Universe enters 'halted' state
  → Manual intervention required
```

**Tracking**: Each receipt includes `invariantViolationDepth` counter (0 = all satisfied).

**Safety Mechanism**: If `eventual` invariant violated >MAX_SCENES → system halts and alerts.

---

### Q4: Who signs receipts?

**Answer**: **Dual signature** (universe authority + scene author).

**Structure**:
```javascript
{
  sceneId: 'sha256:9f3a...',
  timestamp: 1703674801,
  admissibilityChecks: [...],
  minimalityProof: {...},
  signatures: {
    universe: {
      algorithm: 'ed25519',
      publicKey: 'uni_pk_base58',
      signature: 'uni_sig_base64',
      signerId: 'did:web:governance.example'
    },
    author: {
      algorithm: 'ed25519',
      publicKey: 'author_pk_base58',
      signature: 'author_sig_base64',
      signerId: 'did:key:alice...'
    }
  },
  receiptHash: 'sha256:4d1e...'  // Hash of all above fields
}
```

**What Each Signature Proves**:

| Signer | Signs | Proves |
|--------|-------|--------|
| **Universe** | `hash(sceneId + guardResults + invariantChecks + timestamp)` | Guards and invariants were correctly evaluated |
| **Author** | `hash(observations + delta)` | Author claims responsibility for this change |

**Trust Model**:
- **Only universe signed**: Universe could fabricate scenes (author can't verify)
- **Only author signed**: Author could claim invalid scenes were accepted
- **Both signed**: ✅ Non-repudiation + authenticity

**Optional Third Signatures**:
- **Witnesses**: Third-party validators (e.g., 3-of-5 multisig for high-stakes universes)
- **Bridge authorities**: When scene crosses universes

**Verification**:
```javascript
function verifyReceipt(receipt) {
  const universeValid = ed25519.verify(
    receipt.signatures.universe.signature,
    receipt.signatures.universe.publicKey,
    computeUniversePayload(receipt)
  );

  const authorValid = ed25519.verify(
    receipt.signatures.author.signature,
    receipt.signatures.author.publicKey,
    computeAuthorPayload(receipt)
  );

  return universeValid && authorValid;
}
```

---

## Design Principles Summary

### 1. Content-Addressable Everything
**Decision**: All entities identified by hash of canonical content.

**Implications**:
- Universes: `sha256(canonical(schema + invariants + guards))`
- Scenes: `sha256(canonical(observations + delta + universe_id))`
- Receipts: `sha256(canonical(all_fields))`
- Artifacts: `sha256(data)`

**Benefits**: Tamper-evident, deduplication, Git-like model

---

### 2. Deterministic State Transitions
**Decision**: Same observations → same receipt hash (across all replicas).

**Requirements**:
- μ is pure function
- Canonical RDF serialization (sorted N-Quads)
- No timestamps inside μ (use observation metadata)
- No external data sources (all context in universe state)

**Verification**: Replay from genesis → identical final state

---

### 3. Separation of Concerns

```
┌─────────────────────────────────────────────────────────────┐
│  LAYER           RESPONSIBILITY         PURE/IMPURE         │
├─────────────────────────────────────────────────────────────┤
│  Guards (H)      Access control         Pure predicate      │
│  Reconcile (μ)   State computation      Pure function       │
│  Invariants (Q)  Constraint checking    Pure SPARQL query   │
│  Receipt Gen     Proof generation       Pure (signing OK)   │
│  Side Effects    External actions       Impure (async)      │
└─────────────────────────────────────────────────────────────┘
```

**Impure operations** (I/O, randomness) **ONLY** in side effect executor (after receipt signed).

---

### 4. Bridge Trust Model

**Problem**: How to translate scenes across incompatible universes?

**Solution**: Bridge Φ is a **smart contract** trusted by both universes.

**Guarantees**:
1. **Type safety**: `Σ_A → Σ_B` mapping is well-typed
2. **Invariant preservation**: `Q_A ⟹ Q_B` (formal proof required)
3. **Access control**: Explicit grants to bypass incompatible guards
4. **Traceability**: `Receipt_A ↔ Receipt_B` linked in bridge log

**Trust Requirement**: Bridge must be **multi-sig approved** by both universe authorities.

---

## Implementation Roadmap

### Phase 1: Core Types (Week 1)
**Deliverable**: `/src/types/narrative-state-machine.d.ts`

- [ ] JSDoc type definitions (all 20+ types)
- [ ] Zod validation schemas
- [ ] Unit tests for type constraints
- [ ] OTEL validation: ≥80/100

**Files**:
```
src/types/
  ├── universe.d.ts
  ├── scene.d.ts
  ├── receipt.d.ts
  ├── bridge.d.ts
  └── guards.d.ts
```

---

### Phase 2: Universe Management (Week 2)
**Deliverable**: `/src/core/universe.mjs`

- [ ] UniverseRecord CRUD operations
- [ ] Schema validation (SHACL integration)
- [ ] Invariant engine (SPARQL ASK execution)
- [ ] Guard registry (pluggable predicates)
- [ ] Tests: 100% pass, coverage ≥80%
- [ ] OTEL validation: ≥80/100

**Key Functions**:
```javascript
createUniverse({schema, invariants, guards})
loadUniverse(id)
validateSchema(quads, schema)
checkInvariants(graph, invariants)
evaluateGuards(context, guards)
```

---

### Phase 3: Scene Processing (Week 3)
**Deliverable**: `/src/core/scene.mjs`

- [ ] Observation ingestion (RDF parsing)
- [ ] Delta computation (graph diff algorithm)
- [ ] Reconciliation engine (μ executor)
- [ ] Receipt generation (dual signing)
- [ ] Triple store integration (@unrdf/oxigraph)
- [ ] Tests: 100% pass, <100ms p95 latency
- [ ] OTEL validation: ≥80/100

**Key Functions**:
```javascript
ingestObservations(observations, universe)
computeDelta(currentState, observations)
reconcile(delta, universe) // μ function
generateReceipt(scene, guardResults, invariantChecks)
commitScene(scene, receipt)
```

---

### Phase 4: Bridge System (Week 4)
**Deliverable**: `/src/core/bridge.mjs`

- [ ] Type coercion rules engine
- [ ] Invariant preservation proofs
- [ ] Cross-universe scene translation
- [ ] Bridge registry and discovery
- [ ] Tests: 100% pass, multi-universe scenarios
- [ ] OTEL validation: ≥80/100

**Key Functions**:
```javascript
createBridge({sourceUniverse, targetUniverse, typeMapping, invariantProofs})
translateScene(scene, bridge)
verifyBridgeProof(proof)
executeBridgeCrossing(receipt_A, bridge) → receipt_B
```

---

### Phase 5: Production Hardening (Week 5)
**Deliverable**: Production-ready system

- [ ] OTEL instrumentation (spans for each phase)
- [ ] Performance benchmarks (target: <100ms per scene)
- [ ] Failure recovery (rollback, compensation)
- [ ] Audit trail export (JSON-LD, CSV)
- [ ] Documentation (API docs, examples, runbook)
- [ ] Integration tests: 1000+ scenes, 10+ universes
- [ ] OTEL validation: ≥95/100 (production threshold)

---

## Open Questions for Team Review

### 1. Concurrency Model
**Question**: How to handle concurrent scenes targeting same universe?

**Options**:
- **A. Serial execution**: Simple, slow (100-500 scenes/sec)
- **B. Optimistic locking**: Fast, requires conflict detection and retry
- **C. CRDT-based**: Automatic merging, complex to implement

**Recommendation**: Start with **A** (serial), migrate to **B** (optimistic) if throughput insufficient.

**Decision needed by**: End of Week 1

---

### 2. Bridge Creation Authority
**Question**: Who can create bridges? How to prevent malicious translation?

**Options**:
- **A. Permissionless**: Anyone can create, users choose which to trust
- **B. Multi-sig approval**: Both universe authorities must sign bridge
- **C. Community vetting**: Public registry with reputation system

**Recommendation**: **B** (multi-sig) for MVP, add **C** (community vetting) later.

**Decision needed by**: End of Week 2

---

### 3. Receipt Storage Format
**Question**: Append-only log vs content-addressed store?

**Options**:
- **A. Append-only log**: Simple, fast append, harder to query
- **B. Content-addressed store**: Git-like, efficient dedup, requires indexing
- **C. Hybrid**: Daily logs + content-addressed index

**Recommendation**: **C** (hybrid) for best of both worlds.

**Decision needed by**: End of Week 3

---

### 4. Minimality Algorithm
**Question**: Which algorithm to prove delta minimality?

**Options**:
- **A. Set-cover approximation**: NP-hard, 2x optimal with greedy algorithm
- **B. Graph edit distance**: Polynomial, less semantic
- **C. Domain-specific**: Custom logic per universe type (e.g., voting)

**Recommendation**: **C** (domain-specific) with **A** (set-cover) as fallback.

**Decision needed by**: End of Week 2

---

### 5. Side Effect Failure Handling
**Question**: What if email fails but receipt is signed?

**Options**:
- **A. Best-effort**: Log failure, no retry
- **B. Exponential backoff**: Retry up to N times (e.g., 5)
- **C. Dead letter queue**: Store failed tokens for manual resolution

**Recommendation**: **B** (exponential backoff) with **C** (DLQ) for persistent failures.

**Decision needed by**: End of Week 4

---

## Acceptance Criteria

Before declaring MVP complete, the system MUST satisfy:

### Functional Requirements
- [ ] **Determinism**: 1000 scenes replayed → identical receipt hashes (100% match)
- [ ] **Auditability**: Every state transition traceable to signed receipt (0 gaps)
- [ ] **Minimality**: Delta proven minimal via formal algorithm (not heuristic)
- [ ] **Composability**: At least 1 working bridge between 2 universes
- [ ] **Correctness**: Invariants hold with p≥0.999 (extensive testing or formal verification)

### Non-Functional Requirements
- [ ] **Performance**: <100ms median scene processing (p95 OTEL-verified)
- [ ] **Throughput**: ≥200 scenes/sec per universe (single-threaded)
- [ ] **Test coverage**: ≥80% static coverage, 100% pass rate
- [ ] **OTEL validation**: ≥95/100 comprehensive score
- [ ] **Documentation**: Complete API docs, 5+ worked examples

### Integration Test (Reference Implementation)
**Scenario**: GovernanceUniverse with 3 invariants, 5 guards

1. Create universe with schema, invariants, guards
2. Process 1000 votes from 100 unique agents
3. Verify:
   - All receipts cryptographically valid
   - No invariant violations (Q held throughout)
   - Replay from genesis yields identical final state
   - Cross-universe bridge to ArchivalUniverse preserves vote counts

**Pass Criteria**: 100% of verifications succeed, <100ms p95 latency.

---

## Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| **SPARQL performance degradation on large graphs** | High | High | Materialized views, query optimization, indexing |
| **Signing overhead exceeds latency budget** | Medium | High | Batch signing, hardware acceleration (HSM) |
| **Bridge trust model too restrictive** | Low | Medium | Start strict (multi-sig), relax later |
| **Side effect failures impact UX** | Medium | Medium | Exponential backoff + DLQ + user notifications |
| **Determinism violated by μ implementation bugs** | Medium | Critical | Extensive testing, formal verification for critical universes |
| **Receipt storage grows unbounded** | High | Medium | Archive old receipts, prune with retention policy |

---

## Success Metrics

### Week 1 (Types)
- [ ] All JSDoc types defined
- [ ] Zod validation passing
- [ ] OTEL: ≥80/100

### Week 2 (Universe)
- [ ] Universe CRUD working
- [ ] SHACL validation integrated
- [ ] SPARQL invariants executing
- [ ] OTEL: ≥80/100

### Week 3 (Scenes)
- [ ] Scene processing end-to-end
- [ ] Receipts signed and stored
- [ ] <100ms p95 latency
- [ ] OTEL: ≥80/100

### Week 4 (Bridges)
- [ ] Cross-universe translation working
- [ ] Invariant preservation proven
- [ ] OTEL: ≥80/100

### Week 5 (Production)
- [ ] Integration test passing (1000 scenes)
- [ ] Documentation complete
- [ ] OTEL: ≥95/100

---

## Conclusion

**This architecture provides**:
- ✅ Determinism (pure functions, canonical serialization)
- ✅ Auditability (cryptographic receipts, dual signatures)
- ✅ Flexibility (pluggable guards, invariants, μ functions)
- ✅ Composability (bridges with formal proofs)
- ✅ Performance (<100ms median latency target)
- ✅ Security (multi-sig, content-addressing, guard enforcement)

**Next Actions**:
1. **Team review** of this document (target: 2 business days)
2. **Consensus** on open questions 1-5 (target: end of Week 1)
3. **Kick off Phase 1** (types implementation)

**Questions?** See:
- Full spec: `/home/user/unrdf/docs/architecture/narrative-state-machine.md`
- Diagrams: `/home/user/unrdf/docs/architecture/narrative-state-machine-diagrams.md`
- This summary: `/home/user/unrdf/docs/architecture/narrative-state-machine-summary.md`

---

**END OF SUMMARY**

*Ready for consensus. Implementation begins after approval.*
