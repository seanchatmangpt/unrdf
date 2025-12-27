# UNRDF v6 Program Charter: Unification Release

**Version**: 6.0.0-alpha
**Status**: Charter
**Last Updated**: 2025-12-27

---

## Mission

**v6 "Unification Release"** establishes one substrate, one control plane, one UX surface across the UNRDF monorepo (47 packages). The release unifies fragmented abstractions into a coherent, deterministic, receipt-driven architecture.

**Core Principle**: Eliminate duplication. Establish contracts. Enforce guarantees.

---

## Primary Control Plane: ΔGate

**ΔGate** is the central admissibility gate and receipt enforcement mechanism. All state changes flow through ΔGate, which provides:

1. **Admissibility checking**: Proposed Δ (delta) validated against policies, schemas, and invariants
2. **Reconciliation**: μ(O ⊔ Δ) → A (merge operation O with delta Δ to produce atomic change A)
3. **Receipt generation**: Cryptographic proof of change using BLAKE3 hash chains
4. **Atomic application**: All-or-none Δ application with rollback on failure

**ΔGate Guarantees** (v6 Core Invariants):

| Invariant | Definition | Enforcement |
|-----------|------------|-------------|
| **Determinism** | Same O → Same A | Hash-stable canonicalization (lexicographic ordering) |
| **Idempotence** | μ∘μ = μ | Receipt chain prevents duplicate application via hash checking |
| **Provenance** | hash(A) = hash(μ(O)) | Every change carries cryptographic receipt |
| **No Partials** | All-or-none Δ | Transaction boundaries with rollback on any failure |
| **Closed World** | No external hidden state | All inputs declared, all outputs receipted |

**Architecture**:

```
┌─────────────────────────────────────────────────────────────┐
│                         ΔGate                                │
│  ┌──────────┐   ┌──────────┐   ┌──────────┐   ┌──────────┐ │
│  │ Propose  │ → │ Reconcile│ → │ Receipt  │ → │  Apply   │ │
│  │   Δ      │   │ μ(O⊔Δ)   │   │  BLAKE3  │   │ Atomic A │ │
│  └──────────┘   └──────────┘   └──────────┘   └──────────┘ │
└─────────────────────────────────────────────────────────────┘
         ↓                ↓               ↓              ↓
    ┌─────────┐    ┌──────────┐   ┌───────────┐  ┌──────────┐
    │ Hooks   │    │ KGStore  │   │ Receipt   │  │  Event   │
    │ (Policy)│    │(Substrate)│   │  Chain    │  │  Log     │
    └─────────┘    └──────────┘   └───────────┘  └──────────┘
```

**Integration Points**:

- **Hooks**: Policy validation before reconciliation (pre-Δ, post-Δ, on-failure)
- **KnowledgeStore**: Substrate for append-only log and deterministic snapshots
- **ReceiptChain**: Cryptographic proof chain with merkle tree chaining
- **Event Log**: KGC-4D event logging for audit and replay
- **CLI**: Noun-verb interface for Δ proposal and inspection
- **Workflows**: YAWL integration for complex multi-step Δ sequences

---

## The Four Unifications

### Unification 1: One Contract for "Receipts"

**Problem**: Multiple receipt formats across packages (execution, allocation, compile, verification).

**Solution**: Single `ReceiptProfile` schema that unifies all receipt types.

**Schema** (Zod):

```javascript
const ReceiptProfileSchema = z.object({
  // Identity
  id: z.string().uuid(),
  profile: z.enum(['execution', 'allocation', 'compile', 'verification', 'workflow', 'delta']),

  // Cryptographic proof
  previousReceiptHash: z.string().length(64).nullable(),
  payloadHash: z.string().length(64),
  receiptHash: z.string().length(64),

  // Timestamp
  t_ns: z.bigint(),
  timestamp_iso: z.string(),

  // Context
  context: z.object({
    caseId: z.string().optional(),
    taskId: z.string().optional(),
    deltaId: z.string().optional(),
    nodeId: z.string(),
  }),

  // Profile-specific payload
  payload: z.any(), // Validated by profile-specific schema

  // KGC integration
  kgcEventId: z.string().optional(),
  gitRef: z.string().optional(),
  vectorClock: z.any().optional(),
});
```

**Impact**:

- All packages emit compatible receipts
- Cross-package verification works uniformly
- Receipt chains span package boundaries
- Audit trails are queryable via SPARQL

**Maturity Target**: L2 (stable public contracts)

---

### Unification 2: One Contract for "Δ" (Delta)

**Problem**: Changes applied via ad-hoc mutations, no standardized delta representation.

**Solution**: Single `Delta` schema that represents all legal state changes.

**Schema** (Zod):

```javascript
const DeltaSchema = z.object({
  // Identity
  id: z.string().uuid(),
  type: z.enum(['create', 'update', 'delete', 'composite']),

  // Target
  target: z.object({
    entity: z.string(), // URI or identifier
    scope: z.string().optional(), // Graph, context, namespace
  }),

  // Change specification
  changes: z.array(z.object({
    operation: z.enum(['add_triple', 'remove_triple', 'replace_value', 'execute_sparql']),
    subject: z.any().optional(),
    predicate: z.any().optional(),
    object: z.any().optional(),
    sparql: z.string().optional(),
  })),

  // Justification
  justification: z.object({
    reasoning: z.string(),
    actor: z.string(),
    hookValidated: z.string().optional(),
    policyChecked: z.string().optional(),
  }).optional(),

  // Preconditions (must hold for Δ to be admissible)
  preconditions: z.array(z.object({
    type: z.enum(['sparql_ask', 'state_hash', 'receipt_exists']),
    query: z.string().optional(),
    expectedHash: z.string().optional(),
    receiptId: z.string().optional(),
  })).optional(),

  // Expected effects (postconditions for verification)
  expectedEffects: z.object({
    quadCountDelta: z.number().optional(),
    affectedEntities: z.array(z.string()).optional(),
  }).optional(),
});
```

**ΔGate Workflow**:

1. **Propose Δ**: Client submits delta with justification
2. **Validate Preconditions**: All preconditions must pass
3. **Reconcile**: μ(O ⊔ Δ) computes atomic change set A
4. **Generate Receipt**: BLAKE3 hash of (Δ, A, timestamp)
5. **Apply Atomic**: Execute A as transaction, rollback on failure
6. **Verify Effects**: Check postconditions match expectations
7. **Emit Event**: Log to KGC-4D for audit trail

**Impact**:

- All state changes are visible, auditable, replayable
- No hidden mutations
- Receipt chain proves causality
- Δ can be serialized, transmitted, queued

**Maturity Target**: L3 (deterministic outputs + replayability)

---

### Unification 3: One Noun-Verb CLI Ontology

**Problem**: Inconsistent command structures across packages.

**Solution**: Extend existing `kgc-cli` Registry with standardized noun-verb ontology.

**Canonical Nouns** (v6 core):

| Noun | Description | Source Package |
|------|-------------|----------------|
| `delta` | Propose, apply, verify state changes | @unrdf/v6-core |
| `receipt` | Inspect, verify, chain receipts | @unrdf/kgc-substrate |
| `snapshot` | Create, restore deterministic snapshots | @unrdf/kgc-substrate |
| `hook` | Register, execute, validate hooks | @unrdf/hooks |
| `workflow` | Start, monitor YAWL cases | @unrdf/yawl |
| `query` | Execute SPARQL queries | @unrdf/core |
| `store` | Initialize, status of KnowledgeStore | @unrdf/kgc-substrate |

**Canonical Verbs** (uniform across nouns):

| Verb | Semantics | Receipt Profile |
|------|-----------|-----------------|
| `create` | Create new entity | execution |
| `apply` | Apply delta/change | delta |
| `verify` | Cryptographic verification | verification |
| `inspect` | Read-only query | N/A (no receipt) |
| `list` | Enumerate entities | N/A (no receipt) |
| `restore` | Restore from snapshot | execution |

**Example Commands**:

```bash
# Propose and apply delta
kgc delta create --file delta.json --dry-run
kgc delta apply --id delta-123 --receipt-chain

# Inspect receipts
kgc receipt inspect --id receipt-456
kgc receipt verify --chain --from genesis --to receipt-789

# Snapshot operations
kgc snapshot create --tag v6-alpha-1
kgc snapshot restore --hash abc123def456

# Hook management
kgc hook list --package @unrdf/hooks
kgc hook validate --id pre-delta-hook --delta delta-123
```

**Impact**:

- Consistent UX across all packages
- Discoverable via `kgc <noun> --help`
- Deterministic JSON output via `--json` flag
- Receipt emitted for all mutating operations

**Maturity Target**: L2 (stable public contracts)

---

### Unification 4: One Documentation Pipeline

**Problem**: Fragmented docs across Markdown, JSDoc, ADRs, examples.

**Solution**: Diataxis-driven documentation with deterministic LaTeX→PDF compilation.

**Documentation Structure** (Diataxis quadrants):

```
docs/
├── v6/                          # v6-specific architecture
│   ├── PROGRAM_CHARTER.md       # This document
│   ├── CONTROL_PLANE.md         # ΔGate design
│   └── ADRs/                    # Architecture Decision Records
├── tutorials/                   # Learning-oriented
│   ├── 01-getting-started.md
│   ├── 02-first-delta.md
│   └── 03-receipt-chains.md
├── how-to/                      # Task-oriented
│   ├── apply-delta.md
│   ├── verify-receipts.md
│   └── create-snapshots.md
├── reference/                   # Information-oriented
│   ├── api/                     # Generated from JSDoc
│   ├── schemas/                 # Zod schemas as JSON Schema
│   └── cli/                     # Generated from Registry
├── explanation/                 # Understanding-oriented
│   ├── why-deltagate.md
│   ├── receipt-cryptography.md
│   └── determinism-guarantees.md
└── latex/                       # PDF compilation
    ├── main.tex
    ├── preamble.tex
    └── Makefile                 # Deterministic build
```

**Build Pipeline**:

1. **Extract**: JSDoc → JSON → Markdown (via @unrdf/kgc-cli)
2. **Validate**: All cross-references resolved, no dead links
3. **Transform**: Markdown → LaTeX (via Pandoc with custom templates)
4. **Compile**: LaTeX → PDF (deterministic, same input → same hash)
5. **Receipt**: PDF hash stored in ReceiptChain

**Commands**:

```bash
# Generate docs
kgc docs generate --format markdown --output docs/reference/api
kgc docs generate --format latex --output docs/latex

# Build PDF
kgc docs build --format pdf --deterministic --receipt

# Verify build reproducibility
kgc docs verify --hash abc123def456 --rebuild
```

**Impact**:

- Single source of truth
- Docs as code (version controlled, reviewed)
- Reproducible builds (PDF hash in receipt chain)
- Diataxis structure improves discoverability

**Maturity Target**: L4 (adversarial misuse safety via comprehensive docs)

---

## Maturity Ladder

Each package in v6 must progress through these levels:

### L1: Compiles, Runs, Minimal Examples

**Requirements**:

- `npm run build` succeeds (0 errors)
- `npm test` runs (may have failures)
- At least 1 working example in `examples/`
- README with installation and basic usage

**Exit Criteria**: CI passes build step

---

### L2: Stable Public Contracts

**Requirements**:

- All public APIs have Zod schemas
- CLI commands registered in Registry
- JSDoc coverage ≥90% for public exports
- Breaking changes documented in CHANGELOG
- Semantic versioning enforced

**Exit Criteria**: API contracts stable for ≥2 weeks, no breaking changes

---

### L3: Deterministic Outputs + Replayability

**Requirements**:

- All mutating operations emit receipts
- Receipts chained with BLAKE3
- Snapshots are hash-stable (same input → same hash)
- Event log replay produces identical state
- Integration tests verify determinism

**Exit Criteria**: 100% of mutating operations receipted, replay tests pass

---

### L4: Adversarial Misuse Safety

**Requirements**:

- Poka-yoke guards on all Δ proposals
- Schema validation before side effects
- Hooks prevent invalid state transitions
- Error messages suggest correct usage
- Docs include "common mistakes" section

**Exit Criteria**: Adversarial test suite passes (fuzzing, invalid inputs, race conditions)

---

### L5: Cross-Package Compositional Closure

**Requirements**:

- Δ can be composed across packages
- Receipt chains span package boundaries
- Workflows orchestrate multi-package operations
- Integration tests cover cross-package scenarios
- Performance benchmarks for compositions

**Exit Criteria**: At least 3 cross-package workflows in production use

---

## v6 Delivery Phases

### Phase 1: Foundation (Weeks 1-2)

**Deliverables**:

- [ ] `@unrdf/v6-core` package scaffolded
- [ ] `ΔGate` core implementation (propose, reconcile, receipt, apply)
- [ ] `DeltaSchema` and `ReceiptProfileSchema` validated
- [ ] Integration with `@unrdf/kgc-substrate` complete
- [ ] Unit tests for ΔGate workflow (≥80% coverage)

**Success Criteria**: Can propose, apply, verify simple Δ with receipt

---

### Phase 2: CLI Integration (Weeks 3-4)

**Deliverables**:

- [ ] `kgc delta` commands registered in Registry
- [ ] `kgc receipt` commands registered in Registry
- [ ] `--json` output for all commands
- [ ] Receipt emitted for all mutating operations
- [ ] Help text and error messages reviewed

**Success Criteria**: CLI commands work end-to-end, receipts verifiable

---

### Phase 3: Documentation Pipeline (Weeks 5-6)

**Deliverables**:

- [ ] Diataxis structure implemented
- [ ] JSDoc → Markdown extraction working
- [ ] LaTeX → PDF pipeline deterministic
- [ ] Cross-references validated
- [ ] `kgc docs` commands implemented

**Success Criteria**: Docs build reproducibly, PDF hash matches

---

### Phase 4: Package Migration (Weeks 7-10)

**Deliverables**:

- [ ] 10 packages migrated to L2 (stable contracts)
- [ ] 5 packages migrated to L3 (deterministic outputs)
- [ ] 2 packages migrated to L4 (adversarial safety)
- [ ] Migration guide published
- [ ] Breaking changes documented

**Success Criteria**: Core packages emit v6-compatible receipts and deltas

---

### Phase 5: Cross-Package Workflows (Weeks 11-12)

**Deliverables**:

- [ ] 3 cross-package workflows implemented (YAWL)
- [ ] Receipt chains span packages
- [ ] Integration tests cover workflows
- [ ] Performance benchmarks baseline
- [ ] Production readiness review

**Success Criteria**: Workflows execute reliably, receipts verify end-to-end

---

## Governance

### Decision Authority

| Decision Type | Authority | Review Required |
|---------------|-----------|-----------------|
| Core invariants | Program Charter (this doc) | Architecture review |
| Schema changes | Package maintainer | Schema board review |
| CLI commands | CLI working group | Registry collision check |
| Maturity level | Package maintainer + CI | Automated checks |
| Breaking changes | Requires RFC | Community feedback (7 days) |

### RFC Process (for breaking changes)

1. **Proposal**: Submit RFC to `docs/v6/rfcs/`
2. **Discussion**: 7-day comment period
3. **Decision**: Recorded in ADR
4. **Implementation**: After approval only
5. **Communication**: Announced in CHANGELOG and release notes

---

## Success Metrics

### Quantitative

| Metric | Baseline (v5) | Target (v6.0) | Measurement |
|--------|---------------|---------------|-------------|
| Receipt coverage | 0% | 100% | % of mutating ops receipted |
| Deterministic builds | 60% | 100% | % of packages with hash-stable outputs |
| CLI consistency | 40% | 100% | % of commands following noun-verb |
| Cross-package Δ | 0 | 10 | Count of cross-package deltas |
| Docs coverage | 60% | 95% | % of public APIs documented |

### Qualitative

- **Developer experience**: Onboarding time <1 hour (vs. 1 day in v5)
- **Debugging**: Receipt chains reduce bug investigation time by 50%
- **Compliance**: Audit trail meets regulatory requirements (GDPR, SOC2)
- **Confidence**: Determinism enables fearless refactoring

---

## Risks and Mitigation

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| Breaking changes block adoption | High | Medium | Deprecation path, migration tools |
| Performance regression | Medium | Medium | Benchmark suite, optimization budget |
| Schema drift across packages | High | Low | Automated schema validation in CI |
| Receipt chain breaks | High | Low | Tamper detection, recovery procedures |
| Docs out of sync | Medium | High | Docs as code, generated from source |

---

## Appendix A: Prior Art

v6 builds on proven patterns:

- **Git**: Merkle tree chaining, content-addressable storage
- **Blockchain**: Hash chains, immutability, consensus
- **Event Sourcing**: Append-only log, replay, CQRS
- **CRDTs**: Convergent reconciliation, commutativity
- **Capability Security**: Least privilege, object capabilities

---

## Appendix B: References

- [ADR-001: Why ΔGate](docs/v6/ADRs/001-deltagate-rationale.md)
- [Control Plane Design](docs/v6/CONTROL_PLANE.md)
- [Receipt Cryptography](docs/explanation/receipt-cryptography.md)
- [Determinism Guarantees](docs/explanation/determinism-guarantees.md)

---

**Charter Approved**: [TBD]
**Charter Version**: 1.0.0
**Next Review**: Q2 2026
