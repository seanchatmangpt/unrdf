# Governed Ontology Substrate Architecture

**A formally-grounded, closed-world ontology substrate with admission control, deterministic receipts, and cryptographic provenance.**

## Core Mathematical Model

```
A = μ(O)
```

Where:
- **O**: Universe (partitioned RDF state)
- **μ**: Projection operator (deterministic compilation)
- **A**: Artifacts (schemas, documentation, validation rules)

### Core Law

At any epoch τ:

```
μ ∘ μ = μ              (Idempotence)
O ⊨ Σ                  (Typing: O satisfies schema Σ)
Λ is ≺-total          (Total order on partitions)
Π is ⊕-monoid         (Partition merging is monoid)
preserve(Q)           (Invariants preserved)
μ ⊣ H                 (Guard denies forbidden operations)
hash(A) = hash(μ(O))  (Deterministic hashing)
```

## Universe Structure (O)

The universe partitions RDF state into **6 ordered, governed layers**:

### 1. Industrial Substrate (O₀) - Read-Only

**Purpose**: Foundational, allow-listed industry ontologies

**Contents**:
- 7 W3C Standard Ontologies (version-pinned, content-hashed)
  - PROV-O (W3C Provenance)
  - ODRL (W3C Open Digital Rights Language)
  - SKOS (W3C Simple Knowledge Organization System)
  - OWL-Time (W3C Time Ontology)
  - DCAT (W3C Data Catalog Vocabulary)
  - ORG (W3C Organization Ontology)
  - OA (W3C Open Annotation)

**Properties**:
- `readOnly = true` (enforced via partition.mjs)
- `maxTriples = 10,000` (complexity bound)
- Deterministic ingestion (SHA256 content hash)
- Version tracking with distribution URLs

**Invariants**:
- Cannot be edited (O₀ is fixed)
- All contained ontologies must be in allow-list
- All IRIs must be valid RFC 3987 URIs

### 2. Corporate Canon (O_corp) - Read-Only

**Purpose**: Enterprise-wide data models and standards

**Examples**:
- Global entity taxonomies (Products, Services, Locations)
- Standard data types and constraints
- Company-wide naming conventions
- Cross-organizational ontologies

**Properties**:
- `readOnly = true` (enforced)
- `maxTriples = 50,000` (complexity bound)
- Built atop Industrial Substrate (O₀ ⊆ O_corp)
- Globally governed (cannot be weakened by overlays)

**Invariants**:
- Cannot be edited directly (protected)
- Overlays can only add, never weaken constraints
- All terms must be consistent with O₀

### 3. Business Unit Overlays (O_bu) - Read-Write

**Purpose**: Business unit-specific extensions (additive only)

**Examples**:
- Studios: Film metadata, production scheduling
- Streaming: Content catalogs, licensing rules
- Parks: Attractions, visitor experiences
- Consumer Products: Merchandise, inventory

**Properties**:
- `readOnly = false` (writable)
- `maxTriples = 100,000` per BU
- Must be additive (cannot redefine substrate or canon)
- Can extend corporate canon with BU-specific terms

**Invariants**:
- No redefinition of protected namespaces
- No collision with industrial substrate IRIs
- No weakening of corporate canon constraints
- Overlay ⊆ (O₀ ⊔ O_corp ⊔ O_bu)

### 4. Regional Overlays (O_region) - Read-Write

**Purpose**: Territory-specific customizations

**Examples**:
- EMEA: European regulations, data localization
- APAC: Asia-Pacific business rules, cultural adaptations
- AMER: Americas-specific taxonomies

**Properties**:
- `readOnly = false` (writable)
- `maxTriples = 200,000` per region
- Additive only (cannot modify BU or corporate)
- Typically restrictions and localizations

**Invariants**:
- Same as BU overlays
- Additive to all upstream layers

### 5. Execution Ledger (O_exec) - Append-Only

**Purpose**: Immutable audit trail and receipts

**Contents**:
- Receipt journal (every operation)
- Validation reports
- Projection artifacts
- Decision logs

**Properties**:
- `readOnly = false` (append-only)
- `maxTriples = unbounded` (with archival)
- Immutable once written (receipts cannot be modified)
- Hash chain for integrity verification

**Invariants**:
- Cannot delete or modify existing entries
- New entries must reference previous (hash chain)
- All entries must be valid receipts (RDF objects)

### 6. System Policy (O_sys) - Read-Only

**Purpose**: Governance rules and policy enforcement

**Contents**:
- Protected namespace registry (8 W3C standards)
- Forbidden operations (H) definitions
- Complexity bounds (Λ)
- Release channel policies
- Invariant definitions (Q)

**Properties**:
- `readOnly = true` (immutable policy)
- `maxTriples = 10,000` (small, fixed)
- Engine-enforced (not user-modifiable)
- Defines guard conditions and invariants

**Invariants**:
- Cannot be edited (system-owned)
- All policies must be valid RDF

## Admission Gate (μ)

All changes to O flow through the **Admission Gate**, which enforces:

### Step 1: Forbidden Operations Guard (H)

Check if proposed Δ implies any forbidden operations:

```
H: (O, Δ) → {allow, deny} × Reason
```

**Three Categories of Forbidden Operations**:

#### H₁: Edit Industrial Substrate
- **Definition**: Attempt to add/remove/modify triples in O₀
- **Rationale**: Substrate is foundational, immutable by design
- **Guard**: `guardEditIndustrialSubstrate(delta)`
- **Detection**: Target partition type is IndustrialSubstrate

#### H₂: Redefine Protected Terms
- **Definition**: Redefine terms in protected namespaces
- **Protected Namespaces** (8 total):
  - RDF (http://www.w3.org/1999/02/22-rdf-syntax-ns#)
  - RDFS (http://www.w3.org/2000/01/rdf-schema#)
  - OWL (http://www.w3.org/2002/07/owl#)
  - SHACL (http://www.w3.org/ns/shacl#)
  - SKOS (http://www.w3.org/2004/02/skos/core#)
  - Dublin Core (http://purl.org/dc/elements/1.1/)
  - Dublin Core Terms (http://purl.org/dc/terms/)
  - FOAF (http://xmlns.com/foaf/0.1/)
- **Guard**: `guardRedefineProtectedTerm(delta)`
- **Detection**: Quad subject IRI in protected namespace

#### H₃: Weaken Corporate Canon
- **Definition**: Remove constraints from corporate canon
- **Rationale**: Corporate standards must be globally enforced
- **Guard**: `guardWeakenCorporateCanon(delta)`
- **Detection**: Quad deletion from corporate canon partition with constraint predicate

**Guard Evaluation**:

```javascript
async evaluateGuard(universe, delta) {
  const checks = [
    checkForbiddenOperation(delta, H₁),  // EditIndustrialSubstrate
    checkForbiddenOperation(delta, H₂),  // RedefineProtectedTerm
    checkForbiddenOperation(delta, H₃),  // WeakenCorporateCanon
  ];

  for (const check of checks) {
    if (!check.passed) {
      return { allowed: false, blockedBy: check.operationId };
    }
  }
  return { allowed: true };
}
```

### Step 2: Invariant Checking (Q)

If guard passes, verify **6 invariants** (Q):

#### Q₁: Typing (O ⊨ Σ)
- **Definition**: All quads conform to partition schema
- **Checks**:
  - RDF/JS quad structure validity
  - Predicate must be NamedNode (not Literal, BlankNode)
  - Subject never Literal
  - IRIs valid RFC 3987 format
- **Failure**: Deny with type error details

#### Q₂: Non-Collision
- **Definition**: No IRI collisions with protected namespaces
- **Checks**:
  - Extract namespace from subject IRI
  - Check against protected namespace set
  - Report any collisions
- **Failure**: Deny with collision details

#### Q₃: Monotone (Additive Only)
- **Definition**: Overlays cannot redefine base triples
- **Checks**:
  - For each quad in Δ where (subject, predicate) exists in base
  - If object differs → conflict (not allowed)
  - Retraction check: no deletion of base triples
- **Failure**: Deny with conflict details

#### Q₄: Determinism
- **Definition**: Same inputs → same outputs
- **Checks**:
  - Hash(Δ) computed deterministically
  - Recompute and verify
  - No randomness, no timestamps in logic
- **Failure**: Deny with hash mismatch

#### Q₅: Provenance
- **Definition**: Receipts bind inputs/outputs
- **Checks**:
  - Receipt structure valid
  - Input hashes present (ontologies, delta)
  - Output hash populated
  - Agent and timestamp recorded
- **Failure**: Deny with missing provenance

#### Q₆: Bounds
- **Definition**: Complexity within limits
- **Bounds**:
  - Max quads per partition: 100,000
  - Max import depth: 10 levels
  - Max namespaces: 50 per partition
  - Max quads per subject: 1,000
- **Failure**: Deny with bound violation

**Invariant Orchestration**:

```javascript
async checkAllInvariants(delta, options = {}) {
  const invariants = [
    Q_typing(delta, options),
    Q_noncollision(delta, options),
    Q_monotone(delta, options),
    Q_determinism(delta, options),
    Q_provenance(delta, options),
    Q_bounds(delta, options),
  ];

  const results = await Promise.all(invariants);
  return {
    passed: results.every(r => r.passed),
    failures: results.filter(r => !r.passed),
    decision: results.every(r => r.passed) ? 'allow' : 'deny'
  };
}
```

### Step 3: Receipt Emission

If all checks pass → **ALLOW**, emit receipt

If any check fails → **DENY**, emit denial receipt

```javascript
const receipt = {
  id: 'urn:receipt:' + uuidv5(...),
  inputHashes: {
    ontologyReleases: [...],
    deltaCapsule: hash(delta)
  },
  decision: 'allow' | 'deny',
  epoch: 'τ_2025_12_26_1430_123',
  outputHash: hash(universeAfterAdmission),
  toolchainVersion: { node: 'v18.0.0', packages: {} },
  guardDecision: { allowed, blockedBy, reason },
  invariantResults: [{ name, passed, reason }, ...],
  beforeHash: previousReceipt.receiptHash || null,
  receiptHash: hash(receipt),
  generatedAtTime: ISO8601timestamp
};
```

## Projection Operator (μ)

Deterministically generates **artifacts** from universe O:

```
μ: O → A
```

### Projection Steps

1. **Topological Sort**: Order partitions by dependency
2. **Merge Triples**: Accumulate from O₀ → O_corp → O_bu → O_region
3. **Generate Schemas**: Extract OWL/SHACL → JSON Schema, SHACL artifacts
4. **Generate Documentation**: Extract rdfs:comment → Markdown
5. **Generate Validation Rules**: Extract constraints → SPARQL queries
6. **Generate APIs**: Map classes → OpenAPI/GraphQL
7. **Compute Manifest**: Hash each artifact, create Merkle root

### Artifacts (A)

#### Schemas
- **JSON Schema** (from OWL classes)
- **SHACL Shapes** (from property constraints)
- **ShEx** (optional Shape Expressions)

#### Documentation
- **Markdown** (from rdfs:comment, rdfs:label)
- **HTML** (rendered from Markdown)
- **Examples** (from rdfs:seeAlso references)

#### Validation Rules
- **SPARQL Queries** (for constraint checking)
- **SHACL Rules** (for derived properties)
- **Custom Functions** (JavaScript validators)

#### APIs
- **OpenAPI 3.0** (REST endpoints from classes)
- **GraphQL Schema** (from RDF classes)
- **Protocol Buffers** (optional gRPC services)

#### Manifest
```json
{
  "artifacts": [
    { "id": "schema-person", "format": "json-schema", "hash": "..." },
    { "id": "doc-person", "format": "markdown", "hash": "..." }
  ],
  "merkleRoot": "...",
  "generatedAt": "2025-12-26T14:30:00Z",
  "toolchainVersion": "..."
}
```

### Properties

**Determinism**: `hash(O) = hash(O') ⟹ μ(O) = μ(O')`

**Idempotence**: `μ(μ(O)) = μ(O)`

**Compositionality**: `μ(P₁ ⊔ P₂) = μ(P₁) ⊕ μ(P₂)` (monoid merge)

## Receipt System (R)

Cryptographic proof objects binding operations to universe state:

### Receipt Structure

```
Receipt = ⟨H(O_in), H(A_out), τ, decision, toolchain, checks⟩
```

### Receipt Properties

- **Deterministic**: Same operation → same hash
- **Immutable**: Object.freeze() applied
- **Chainable**: previousHash → receiptHash link
- **Batchable**: Multiple receipts → Merkle root
- **Verifiable**: Can re-compute hash and validate signature

### Hash Chaining

Receipts form an **immutable append-only log**:

```
r₀ → r₁ → r₂ → ... → rₙ

Where rᵢ.beforeHash = hash(rᵢ₋₁)
```

**Chain Verification**:
1. Load receipt rᵢ
2. Compute hash of rᵢ₋₁
3. Verify rᵢ.beforeHash = hash(rᵢ₋₁)
4. Verify epochs monotonically increasing

### Merkle Tree Batching

For efficiency, receipts batched in Merkle tree:

```
         Root
        /    \
      H(r₁,r₂) H(r₃,r₄)
      /  \    /  \
    H(r₁) H(r₂) H(r₃) H(r₄)
```

**Properties**:
- Single root hash represents batch
- Individual receipt verifiable with log(n) hashes (Merkle proof)
- Efficient for 1000s of receipts

## Data Flow

```
┌──────────────────┐
│   User Request   │
│  (CLI command)   │
└────────┬─────────┘
         │
         ▼
┌──────────────────────────────┐
│ 1. Parse & Build Operation   │
└────────┬─────────────────────┘
         │
         ▼
┌──────────────────────────────┐
│ 2. Load Universe O           │
│    (6 partitions)            │
└────────┬─────────────────────┘
         │
         ▼
┌──────────────────────────────┐
│ 3. Evaluate Guard H          │
│    (H₁, H₂, H₃)              │
└────┬───────────────────┬─────┘
     │ DENY              │ ALLOW
     │                   ▼
     │          ┌──────────────────────────────┐
     │          │ 4. Check Invariants Q        │
     │          │ (Q₁-Q₆)                      │
     │          └────┬───────────────────┬─────┘
     │               │ FAIL              │ PASS
     │               │                   ▼
     │               │          ┌──────────────────────────────┐
     │               │          │ 5. Project μ(O)              │
     │               │          │ (Schemas, Docs, APIs)        │
     │               │          └────────┬─────────────────────┘
     │               │                   │
     │               ▼                   ▼
     │          ┌──────────────────────────────┐
     └─────────▶│ 6. Emit Receipt (DENY/ALLOW) │
                │ (Hash, Decision, Epoch τ)    │
                └────────┬─────────────────────┘
                         │
                         ▼
              ┌──────────────────────────────┐
              │ 7. Append to Ledger          │
              │    (Hash chain)              │
              └────────┬─────────────────────┘
                       │
                       ▼
              ┌──────────────────────────────┐
              │ 8. Persist O' & A            │
              │    (Write artifacts)         │
              └────────┬─────────────────────┘
                       │
                       ▼
              ┌──────────────────────────────┐
              │ Return Success + Receipt ID  │
              └──────────────────────────────┘
```

## Implementation Layers

```
Layer 4: Commands (CLI interface)
├── validate.mjs
├── propose.mjs
├── admit.mjs
└── project.mjs

Layer 3: Admission Engine
├── admission-engine.mjs
├── invariants.mjs
└── forbidden-operations.mjs

Layer 2: Universe & Receipts
├── universe.mjs
├── partition.mjs
├── receipt.mjs
├── receipt-chain.mjs
└── receipt-generator.mjs

Layer 1: Utilities
├── rdf-utils.mjs
├── registry.mjs
├── ontology-release.mjs
└── delta-capsule.mjs

Layer 0: Dependencies
└── @unrdf/oxigraph (RDF store)
```

## Deployment Model

### Single Machine
```
universe.ttl ──┐
policy.ttl    ├──> CLI ──> Admission ──> Receipts ──> Artifacts
overlays/*.ttl─┘
```

### Distributed
```
Master Universe          Regional Cache
    │                        │
    ├──> Receipt Chain  ────▶├──> Validation
    │                        │
    └──> Artifact Manifest ──┴──> Artifact Distribution
```

## Performance Targets

| Operation | SLA | Typical |
|-----------|-----|---------|
| Validate | < 5s | ~100ms |
| Propose | < 5s | ~75ms |
| Admit | < 5s | ~150ms |
| Project | < 10s | ~200ms |

## Security Model

### Threat Model

1. **Corrupt Developer**: Attempts to edit protected namespace
   - **Mitigation**: Guard blocks (H₁, H₂)

2. **Malicious BU**: Attempts to weaken corporate standards
   - **Mitigation**: Q_monotone invariant

3. **Tampered Receipt**: Receipt modified after creation
   - **Mitigation**: Hash chain integrity, Object.freeze()

4. **Replay Attack**: Same Δ applied twice
   - **Mitigation**: Receipts are time-ordered, impossible to replay

### Guarantees

- **Integrity**: No undetected modifications (hash chain)
- **Authenticity**: Deterministic hashing (reproducible)
- **Compliance**: Forbidden operations non-representable
- **Auditability**: Complete receipt trail (immutable)

## Future Enhancements

1. **Cryptographic Signatures**: Ed25519 sign receipts
2. **Merkle Anchoring**: Batch receipts in blockchain
3. **SPARQL Endpoints**: Direct RDF query interface
4. **Time-Travel Queries**: Query universe at past epoch
5. **Conflict Resolution**: Multi-writer reconciliation
6. **Performance Optimization**: Incremental hashing, caching

---

**Version**: 1.0.0
**Status**: Production
**Last Updated**: 2025-12-26
