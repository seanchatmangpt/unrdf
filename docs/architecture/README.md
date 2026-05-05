# Narrative State Machine Architecture

**Post-Cyberpunk Governance System - Design Documentation**

Status: DRAFT | Version: latest | Date: 2025-12-27

---

## Quick Navigation

### 📋 [Executive Summary](./narrative-state-machine-summary.md)
**Start here** - Answers to critical questions, design principles, roadmap

- ✅ Where RDF fits (answer: everywhere)
- ✅ Is μ pure? (answer: yes, strictly)
- ✅ Can invariants be violated? (answer: yes, with 'eventual' mode)
- ✅ Who signs receipts? (answer: dual signature - universe + author)
- 5-week implementation roadmap
- Acceptance criteria and success metrics

### 📖 [Full Specification](./narrative-state-machine.md)
**Complete technical details** - Type system, ADRs, data flow

- JSDoc type definitions (20+ types)
- Architecture Decision Records (ADR-001 through ADR-005)
- State transition flow diagrams
- Data flow examples (governance vote walkthrough)
- Glossary and references

### 📊 [Visual Architecture](./narrative-state-machine-diagrams.md)
**Diagrams and visual models** - C4, sequence, component diagrams

- C4 context and container diagrams
- Component diagram (reconciliation engine detail)
- Sequence diagram (complete scene processing)
- Bridge architecture (cross-universe communication)
- Entity-relationship model
- Storage layout (file system structure)
- Performance characteristics and security model

---

## Core Concepts (1-Minute Overview)

```
Universe U = ⟨Σ, μ, Q, H⟩
  │
  ├─ Σ (Schema): RDF type constraints (SHACL shapes)
  ├─ μ (Reconcile): Pure function (Observations → Consequence)
  ├─ Q (Invariants): SPARQL ASK queries that MUST hold
  └─ H (Guards): Access control predicates (agent, action, target) → bool

Scene = ⟨O, Δ, μ(O), A, Receipt⟩
  │
  ├─ O (Observations): New RDF quads (raw input)
  ├─ Δ (Delta): Minimal change (additions + deletions)
  ├─ μ(O) (Consequence): Result of reconciliation
  ├─ A (Artifacts): Deterministic outputs
  └─ Receipt: Cryptographic proof ⟨hash, signatures, admissibility⟩

Bridge Φ = Proof(meaning preserves across universes)
  │
  ├─ Type coercion: Σ_A → Σ_B
  ├─ Invariant preservation: Q_A ⟹ Q_B
  ├─ Access grants: Bypass incompatible guards
  └─ Validity: Multi-sig from both universes
```

---

## State Machine Flow (ASCII)

```
Observation → Guard Check → μ Reconciliation → Invariant Check → Receipt Generation → Artifact Output
     │            │              │                    │                  │                   │
     │            │              │                    │                  │                   │
   RDF quads   Authz/Authz   Pure function        SPARQL ASK        Dual signature      Persist state
     │            │              │                    │                  │                   │
     │            ▼              ▼                    ▼                  ▼                   ▼
     │        Pass/Deny     Compute Δ            Satisfied?         Universe +          Triple store
     │                      (minimal)              (Q holds)          Author              + Receipts
     │                                                 │               keys                + Artifacts
     │                                                 │                                      │
     │                                                 │                                      │
     └─────────────────────────────────────────────────┴──────────────────────────────────────┘
                                  Immutable audit trail (content-addressed)
```

---

## Implementation Status

### ✅ Completed
- [x] Conceptual design (this document)
- [x] Type system definition (JSDoc)
- [x] Architecture diagrams (C4, sequence, ER)
- [x] ADRs (5 core decisions)

### 🔄 In Progress
- [ ] Team review and consensus
- [ ] Open questions resolution (5 items)

### 📅 Planned (5-Week Roadmap)
- [ ] **Week 1**: Core types + Zod validation
- [ ] **Week 2**: Universe management
- [ ] **Week 3**: Scene processing
- [ ] **Week 4**: Bridge system
- [ ] **Week 5**: Production hardening

---

## Key Design Decisions

### ADR-001: RDF as Native Format
All observations, deltas, and state are RDF quads. Schema is SHACL. Invariants are SPARQL.

**Why?** Universal interoperability, rich query capabilities, content-addressable via canonical N-Quads.

### ADR-002: Pure Reconciliation Function
μ is strictly pure (deterministic, no side effects). Side effects via tokens executed AFTER receipt.

**Why?** Replaying scenes must yield identical results. Formal verification requires determinism.

### ADR-003: Eventual Invariants
Invariants support three modes: strict, eventual, advisory.

**Why?** Multi-step workflows require temporary violations (e.g., debit then credit).

### ADR-004: Dual Signature
Receipts signed by BOTH universe authority AND scene author.

**Why?** Non-repudiation. Neither party can forge or deny scenes.

### ADR-005: Content-Addressable IDs
All entities identified by hash of canonical content.

**Why?** Tamper-evident, deduplication, Git-like model.

---

## Quick Reference

### File Paths

```
/home/user/unrdf/docs/architecture/
├── README.md                              (this file)
├── narrative-state-machine.md             (full specification)
├── narrative-state-machine-diagrams.md    (visual architecture)
└── narrative-state-machine-summary.md     (executive summary)
```

### Type Definitions

```javascript
// Core types (see full spec for details)
UniverseRecord  = {id, schema, reconcile, invariants, guards, metadata}
SceneEnvelope   = {id, universeId, observations, delta, consequences, artifacts, receipt, metadata}
Receipt         = {sceneId, timestamp, admissibilityChecks, minimalityProof, forkParents, signatures}
BridgeProof     = {id, sourceUniverseId, targetUniverseId, typeCoercion, invariantPreservation, validity}
```

### Performance Targets (OTEL)

```
Operation                Latency (p95)    Throughput
─────────────────────────────────────────────────────
Guard evaluation         <1ms             10K/sec
Reconciliation (μ)       <50ms            1K/sec
Invariant check          <10ms            1K/sec
Receipt generation       <15ms            2K/sec
─────────────────────────────────────────────────────
END-TO-END              <100ms            500/sec
```

### Key Guarantees

✅ **Determinism**: Same observations → same receipt hash
✅ **Auditability**: Every transition has signed receipt
✅ **Minimality**: Delta proven minimal by formal algorithm
✅ **Composability**: Bridges enable cross-universe workflows
✅ **Security**: Multi-sig, content-addressing, guard enforcement

---

## Open Questions (Awaiting Consensus)

1. **Concurrency model**: Serial vs optimistic locking vs CRDT?
2. **Bridge authority**: Permissionless vs multi-sig vs community vetting?
3. **Receipt storage**: Append-only log vs content-addressed vs hybrid?
4. **Minimality algorithm**: Set-cover vs edit distance vs domain-specific?
5. **Side effect failures**: Best-effort vs retry vs dead letter queue?

**Decision deadline**: End of Week 1 (2025-01-03)

---

## Acceptance Test (Reference Implementation)

**Scenario**: GovernanceUniverse voting system

1. Create universe with 3 invariants, 5 guards
2. Process 1000 votes from 100 unique agents
3. Verify:
   - All receipts cryptographically valid ✓
   - No invariant violations ✓
   - Replay from genesis → identical state ✓
   - Bridge to ArchivalUniverse preserves counts ✓

**Pass criteria**: 100% success, <100ms p95 latency

---

## Related Documentation

- **Project README**: `/home/user/unrdf/README.md`
- **CLAUDE.md**: `/home/user/unrdf/CLAUDE.md` (execution patterns)
- **API Docs**: TBD (generated after implementation)
- **Examples**: TBD (created during Week 5)

---

## Contact & Review

**Architecture Designer**: System Architecture Agent
**Review Status**: DRAFT - Awaiting team consensus
**Next Action**: Team review meeting (schedule within 2 business days)

**Feedback**: Open issues in repo or comment on this document.

---

## Version History

| Version | Date       | Changes                                      |
|---------|------------|----------------------------------------------|
| latest   | 2025-12-27 | Initial draft (full spec + diagrams + summary)|

---

**END OF README**

*This is the blueprint. Implementation begins after consensus.*
