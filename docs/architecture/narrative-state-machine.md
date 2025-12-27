# Narrative State Machine Architecture
## Post-Cyberpunk Governance System - Core Conceptual Model

**Status**: Draft
**Date**: 2025-12-27
**Version**: 0.1.0

---

## Executive Summary

This document defines the formal architecture for a narrative state machine where:
- **Universes** are isolated rulesets with their own schema, reconciliation logic, invariants, and guards
- **Scenes** are atomic state transitions with cryptographic receipts
- **Bridges** provide provably-correct cross-universe communication
- **Guards** enforce admissibility via pure predicates
- **Receipts** create immutable audit trails

**Core Guarantee**: Every state transition is deterministic, auditable, and provably minimal.

---

## 1. Type System (JSDoc)

```javascript
/**
 * @typedef {Object} UniverseRecord
 * @property {string} id - Hash-addressed identity (sha256 of canonical schema)
 * @property {UniverseSchema} schema - RDF type constraints (Σ)
 * @property {ReconciliationFunction} reconcile - Pure function μ: Observations → Consequences
 * @property {InvariantSet} invariants - Properties that MUST hold (Q)
 * @property {GuardMap} guards - Access control rules (H)
 * @property {UniverseMetadata} metadata - Lineage tracking
 */

/**
 * @typedef {Object} UniverseSchema
 * @property {string[]} requiredTypes - RDF classes that must exist (e.g., ['ex:Agent', 'ex:Action'])
 * @property {Object<string, PropertyConstraint>} properties - Predicate → constraint mapping
 * @property {string[]} closedWorld - Properties that cannot be extended
 * @property {string} namespace - Base IRI for this universe
 */

/**
 * @typedef {Object} PropertyConstraint
 * @property {string} range - Expected RDF type (rdfs:range)
 * @property {number} minCardinality - Minimum occurrences
 * @property {number} maxCardinality - Maximum occurrences (Infinity = unbounded)
 * @property {boolean} functional - True if max 1 value allowed
 */

/**
 * @typedef {function(Observation[]): Promise<Consequence>} ReconciliationFunction
 * Pure async function that computes state changes.
 * MUST be deterministic: same input → same output.
 * Side effects (I/O, randomness) are FORBIDDEN.
 */

/**
 * @typedef {Object} InvariantSet
 * @property {Invariant[]} rules - Logical predicates that must hold
 * @property {string} enforcement - 'strict' | 'eventual' | 'advisory'
 */

/**
 * @typedef {Object} Invariant
 * @property {string} id - Unique identifier
 * @property {string} sparql - SPARQL ASK query (returns true if valid)
 * @property {string} description - Human-readable explanation
 * @property {string} severity - 'error' | 'warning'
 */

/**
 * @typedef {Map<string, GuardPredicate>} GuardMap
 * Map of guard_id → predicate function
 */

/**
 * @typedef {function(GuardContext): GuardResult} GuardPredicate
 * Pure function that determines if an action is admissible.
 */

/**
 * @typedef {Object} GuardContext
 * @property {UniverseRecord} universe - Current universe
 * @property {string} agentId - DID or public key of actor
 * @property {string} action - Action type (e.g., 'create', 'update', 'delete')
 * @property {Quad[]} target - RDF quads being modified
 * @property {SceneEnvelope|null} parentScene - Previous scene in chain
 */

/**
 * @typedef {Object} GuardResult
 * @property {boolean} allowed - True if admissible
 * @property {string|null} denyReason - Explanation if rejected
 * @property {string} guardId - Which guard evaluated this
 * @property {number} timestamp - When check occurred
 */

/**
 * @typedef {Object} UniverseMetadata
 * @property {number} created - Unix timestamp
 * @property {number} updated - Last modification time
 * @property {string[]} parentUniverses - IDs of universes this forked from
 * @property {string} creator - DID of universe author
 * @property {number} version - Semantic version counter
 */

/**
 * @typedef {Object} SceneEnvelope
 * @property {string} id - Hash of (observations + delta + universe_id)
 * @property {string} universeId - Which universe contains this scene
 * @property {Observation[]} observations - New RDF quads (raw input)
 * @property {Delta} delta - Computed minimal change
 * @property {Consequence} consequences - Result of μ(observations)
 * @property {Artifact[]} artifacts - Deterministic outputs
 * @property {Receipt} receipt - Cryptographic proof of admissibility
 * @property {SceneMetadata} metadata - Timing and lineage
 */

/**
 * @typedef {Object} Observation
 * @property {Quad} quad - RDF statement (subject, predicate, object, graph)
 * @property {string} source - Where this came from (e.g., 'agent:alice', 'sensor:temp01')
 * @property {number} timestamp - When observed
 * @property {number} confidence - 0.0-1.0 certainty score
 */

/**
 * @typedef {Object} Delta
 * @property {Quad[]} additions - Quads to add
 * @property {Quad[]} deletions - Quads to remove
 * @property {string} hash - sha256(canonical(additions + deletions))
 * @property {MinimalityProof} proof - Why this is smallest valid change
 */

/**
 * @typedef {Object} MinimalityProof
 * @property {string} algorithm - 'set-cover' | 'graph-diff' | 'custom'
 * @property {number} alternativeCount - How many other deltas were considered
 * @property {string} justification - Why this delta is minimal
 */

/**
 * @typedef {Object} Consequence
 * @property {string} status - 'accepted' | 'rejected' | 'deferred'
 * @property {TripleStore} resultingGraph - New state after applying delta
 * @property {InvariantCheckResult[]} invariantChecks - Q validation results
 * @property {string[]} sideEffectTokens - Opaque handles for external actions (e.g., 'notify:alice')
 */

/**
 * @typedef {Object} InvariantCheckResult
 * @property {string} invariantId - Which invariant was checked
 * @property {boolean} satisfied - True if holds
 * @property {string} query - SPARQL that was executed
 * @property {Object} bindings - Variable bindings if violated
 */

/**
 * @typedef {Object} Artifact
 * @property {string} id - Content-addressed (hash of data)
 * @property {string} type - MIME type or RDF class
 * @property {string|Buffer} data - Actual content
 * @property {string} generatedBy - Which μ function produced this
 * @property {string[]} dependencies - Other artifact IDs this depends on
 */

/**
 * @typedef {Object} Receipt
 * @property {string} sceneId - Which scene this certifies
 * @property {number} timestamp - When receipt was generated
 * @property {GuardResult[]} admissibilityChecks - All guard evaluations
 * @property {MinimalityProof} minimalityProof - Why delta is minimal
 * @property {string[]} forkParents - Scene IDs this builds on
 * @property {Signature} signature - Cryptographic seal
 * @property {string} receiptHash - sha256(canonical(all above fields))
 */

/**
 * @typedef {Object} Signature
 * @property {string} algorithm - 'ed25519' | 'secp256k1' | 'bls12-381'
 * @property {string} publicKey - Signer's public key (base58)
 * @property {string} signature - Signed hash (base64)
 * @property {string} signerId - DID of signer
 */

/**
 * @typedef {Object} SceneMetadata
 * @property {number} created - When scene was committed
 * @property {string} author - DID of scene creator
 * @property {number} sequenceNumber - Total order within universe
 * @property {string[]} tags - Optional categorization
 */

/**
 * @typedef {Object} BridgeProof
 * @property {string} id - Hash of proof content
 * @property {string} sourceUniverseId - Origin universe
 * @property {string} targetUniverseId - Destination universe
 * @property {TypeCoercionRules} typeCoercion - How Σ_src maps to Σ_target
 * @property {InvariantPreservation[]} invariantPreservation - Q_src ⟹ Q_target proofs
 * @property {AccessGrant[]} accessGrants - Which H rules are waived
 * @property {ProofOfValidity} validity - Cryptographic chain
 * @property {BridgeMetadata} metadata - Context
 */

/**
 * @typedef {Object} TypeCoercionRules
 * @property {Map<string, string>} classMapping - Source class → target class
 * @property {Map<string, string>} propertyMapping - Source property → target property
 * @property {TransformFunction[]} transforms - Custom conversion logic
 * @property {string} defaultNamespace - Fallback for unmapped terms
 */

/**
 * @typedef {function(Quad[]): Quad[]} TransformFunction
 * Pure function that rewrites quads during bridge crossing.
 */

/**
 * @typedef {Object} InvariantPreservation
 * @property {string} sourceInvariantId - Invariant from source universe
 * @property {string} targetInvariantId - Corresponding invariant in target
 * @property {string} proofType - 'logical-implication' | 'constraint-relaxation' | 'manual-verification'
 * @property {string} proof - Formal proof or human justification
 */

/**
 * @typedef {Object} AccessGrant
 * @property {string} guardId - Which guard is being bypassed
 * @property {string} reason - Why bypass is necessary
 * @property {string} grantor - DID of entity authorizing bypass
 * @property {number} expiresAt - Unix timestamp (null = permanent)
 */

/**
 * @typedef {Object} ProofOfValidity
 * @property {Signature} sourceSignature - Signed by source universe authority
 * @property {Signature} targetSignature - Signed by target universe authority
 * @property {string[]} witnessSignatures - Third-party attestations
 * @property {string} merkleRoot - Root of proof tree
 */

/**
 * @typedef {Object} BridgeMetadata
 * @property {number} created - When bridge was established
 * @property {string} creator - DID of bridge author
 * @property {boolean} bidirectional - Can translate both ways?
 * @property {number} usageCount - How many scenes have crossed
 */

/**
 * @typedef {import('@rdfjs/types').Quad} Quad
 * Standard RDF quad (subject, predicate, object, graph).
 */

/**
 * @typedef {Object} TripleStore
 * In-memory or persistent RDF graph.
 * Implementation: @unrdf/oxigraph Store
 */
```

---

## 2. State Transition Flow

```
┌─────────────────────────────────────────────────────────────────────┐
│                         NARRATIVE STATE MACHINE                      │
└─────────────────────────────────────────────────────────────────────┘

Phase 1: OBSERVATION INGESTION
┌──────────────┐
│  External    │
│  Event       │  (sensor, agent, API)
└──────┬───────┘
       │ Observation[] (RDF quads + metadata)
       ▼
┌──────────────────────────────────────────────────────────────────────┐
│  GUARD CHECKPOINT (H)                                                │
│  ┌────────────────────────────────────────────────────────────────┐  │
│  │  For each observation:                                         │  │
│  │    1. Extract (agent, action, target)                          │  │
│  │    2. Evaluate ALL guards in H                                 │  │
│  │    3. Collect GuardResult[]                                    │  │
│  │    4. If ANY deny → REJECT with receipt                        │  │
│  └────────────────────────────────────────────────────────────────┘  │
└──────────────────────┬───────────────────────────────────────────────┘
                       │ Admissible observations
                       ▼
Phase 2: RECONCILIATION (μ)
┌──────────────────────────────────────────────────────────────────────┐
│  μ: Observations → Consequence                                       │
│  ┌────────────────────────────────────────────────────────────────┐  │
│  │  1. Load current universe state (triple store)                 │  │
│  │  2. Compute minimal delta (graph diff algorithm)               │  │
│  │  3. Apply delta to get new_state                               │  │
│  │  4. Check invariants Q on new_state                            │  │
│  │  5. Generate artifacts (deterministic outputs)                 │  │
│  │  6. Return Consequence{status, graph, checks, tokens}          │  │
│  └────────────────────────────────────────────────────────────────┘  │
└──────────────────────┬───────────────────────────────────────────────┘
                       │ Consequence (accepted/rejected)
                       ▼
Phase 3: INVARIANT VALIDATION (Q)
┌──────────────────────────────────────────────────────────────────────┐
│  INVARIANT CHECKER                                                   │
│  ┌────────────────────────────────────────────────────────────────┐  │
│  │  For each invariant in Q:                                      │  │
│  │    1. Execute SPARQL ASK query on new_state                    │  │
│  │    2. If false AND severity='error' → ROLLBACK                 │  │
│  │    3. If false AND severity='warning' → LOG but continue       │  │
│  │    4. Record InvariantCheckResult                              │  │
│  └────────────────────────────────────────────────────────────────┘  │
└──────────────────────┬───────────────────────────────────────────────┘
                       │ Validated consequence
                       ▼
Phase 4: RECEIPT GENERATION
┌──────────────────────────────────────────────────────────────────────┐
│  RECEIPT BUILDER                                                     │
│  ┌────────────────────────────────────────────────────────────────┐  │
│  │  1. Hash scene content (observations + delta)                  │  │
│  │  2. Attach guard results                                       │  │
│  │  3. Attach invariant checks                                    │  │
│  │  4. Prove minimality of delta                                  │  │
│  │  5. Link to fork parents (previous scenes)                     │  │
│  │  6. Sign with universe key + agent key                         │  │
│  │  7. Compute receipt hash                                       │  │
│  └────────────────────────────────────────────────────────────────┘  │
└──────────────────────┬───────────────────────────────────────────────┘
                       │ Signed receipt
                       ▼
Phase 5: ARTIFACT EMISSION
┌──────────────────────────────────────────────────────────────────────┐
│  ARTIFACT OUTPUT                                                     │
│  ┌────────────────────────────────────────────────────────────────┐  │
│  │  - Persist updated triple store                               │  │
│  │  - Emit artifacts (files, notifications, etc.)                 │  │
│  │  - Append receipt to immutable log                             │  │
│  │  - Trigger side effect tokens (async, non-blocking)            │  │
│  └────────────────────────────────────────────────────────────────┘  │
└──────────────────────┬───────────────────────────────────────────────┘
                       │
                       ▼
                  ┌─────────┐
                  │  DONE   │
                  └─────────┘

REJECTION PATH (any phase fails):
┌──────────────────────────────────────────────────────────────────────┐
│  Generate rejection receipt with:                                   │
│    - Which guard/invariant failed                                   │
│    - Detailed failure reason                                        │
│    - Partial delta (if computed)                                    │
│    - Signature proving rejection is authentic                       │
└──────────────────────────────────────────────────────────────────────┘
```

---

## 3. Bridge Φ: Cross-Universe Translation

```
Universe A                Bridge Φ                Universe B
┌────────────┐          ┌──────────┐           ┌────────────┐
│  Schema_A  │          │  Type    │           │  Schema_B  │
│  (Σ_A)     │◄────────►│  Coercion│◄─────────►│  (Σ_B)     │
│            │          │  Rules   │           │            │
│ ex:Person  │          │ ex:Person│           │ foaf:Person│
│  :name     │          │  →       │           │  foaf:name │
│  :age      │          │ foaf:... │           │  foaf:age  │
└────────────┘          └──────────┘           └────────────┘
       │                      │                       │
       │ Scene_A              │                       │ Scene_B
       │ (observations)       │                       │ (transformed)
       ▼                      ▼                       ▼
┌────────────┐          ┌──────────┐           ┌────────────┐
│ Invariants │          │ Invariant│           │ Invariants │
│   Q_A      │          │ Preserv- │           │   Q_B      │
│            │          │ ation    │           │            │
│ "Age ≥ 0"  ├─────────►│  Proof   ├──────────►│ "Age ≥ 0"  │
└────────────┘          └──────────┘           └────────────┘
       │                      │                       │
       │                      │                       │
       ▼                      ▼                       ▼
┌────────────┐          ┌──────────┐           ┌────────────┐
│  Guards    │          │ Access   │           │  Guards    │
│   H_A      │          │ Grants   │           │   H_B      │
│            │          │          │           │            │
│ "Owner     │◄────────►│ Bypass   │◄─────────►│ "Public    │
│  only"     │          │ Owner    │           │  read OK"  │
└────────────┘          │ check    │           └────────────┘
                        └──────────┘

BRIDGE EXECUTION:
1. Scene exits Universe A → Receipt_A generated
2. Bridge Φ receives Receipt_A
3. Validate Receipt_A signature
4. Apply type coercion: Σ_A quads → Σ_B quads
5. Check Q_A ⟹ Q_B (invariant preservation)
6. Apply access grants (bypass incompatible H_B guards)
7. Submit transformed scene to Universe B
8. Universe B generates Receipt_B
9. Link Receipt_A ↔ Receipt_B in bridge log

TRUST MODEL:
- Bridge must be trusted by BOTH universes
- Bridge signature = third-party attestation
- Merkle tree proves transform was deterministic
```

---

## 4. Architectural Decisions (ADRs)

### ADR-001: RDF as Native Observation Format

**Decision**: All observations are RDF quads stored as TTL/N-Quads.

**Rationale**:
- RDF provides universal data model (subject-predicate-object)
- SPARQL enables expressive invariant checking
- Content-addressable via canonical N-Quads hashing
- Aligns with UNRDF project's core mission

**Consequences**:
- ✅ Universal interoperability via bridges
- ✅ Rich query capabilities (SPARQL)
- ❌ Performance overhead vs binary formats
- ❌ Requires RDF literacy from universe designers

**Implementation**: Use `@unrdf/oxigraph` for storage, canonical N-Quads for hashing.

---

### ADR-002: μ is Pure Function

**Decision**: Reconciliation function μ MUST be pure (deterministic, no side effects).

**Rationale**:
- Replaying scenes must yield identical results (auditing requirement)
- Enables formal verification of minimality proofs
- Side effects go in `sideEffectTokens`, executed AFTER receipt generation

**Consequences**:
- ✅ Deterministic state transitions
- ✅ Testable in isolation
- ❌ Cannot directly call APIs/databases inside μ
- Workaround: Return tokens like `{type: 'http_post', url: '...', body: '...'}`, executor handles async

**Example**:
```javascript
// ✅ CORRECT
async function reconcile(observations) {
  const delta = computeDiff(observations);
  return {
    status: 'accepted',
    resultingGraph: applyDelta(currentGraph, delta),
    sideEffectTokens: ['notify:alice@example.com']
  };
}

// ❌ WRONG (side effect inside μ)
async function reconcile(observations) {
  await fetch('https://api.example.com/notify'); // FORBIDDEN
  return {...};
}
```

---

### ADR-003: Invariants Can Violate During Transaction (Eventual Consistency)

**Decision**: Invariants support three enforcement modes:
1. `strict`: MUST hold before AND after every scene
2. `eventual`: Can violate temporarily, must converge within N scenes
3. `advisory`: Logged but not enforced

**Rationale**:
- Some invariants (e.g., "total votes = sum of ballots") require multi-step updates
- Strict mode would require complex compensation logic
- Eventual mode allows natural multi-scene workflows

**Consequences**:
- ✅ Flexibility for complex state machines
- ❌ Complexity in proving eventual convergence
- ❌ Risk of infinite violation cycles

**Safeguard**: If `eventual` invariant violated for >MAX_SCENES (config param), system halts and alerts.

---

### ADR-004: Dual Signature for Receipts

**Decision**: Receipts signed by BOTH universe authority AND scene author.

**Rationale**:
- Universe signature: Proves admissibility (guards passed, invariants held)
- Author signature: Proves authorship (non-repudiation)
- Dual signature prevents universe from fabricating scenes OR author from claiming universe accepted invalid scene

**Consequences**:
- ✅ Strong non-repudiation
- ✅ Detects tampering by either party
- ❌ Requires key management for both roles
- ❌ Offline signing requires pre-authorization tokens

**Format**:
```javascript
{
  signatures: {
    universe: {algorithm: 'ed25519', publicKey: 'uni_pk', signature: 'uni_sig'},
    author: {algorithm: 'ed25519', publicKey: 'author_pk', signature: 'author_sig'}
  }
}
```

---

### ADR-005: Content-Addressable Everything

**Decision**: All entities (universes, scenes, artifacts, receipts) identified by hash of canonical content.

**Rationale**:
- Immutability by design (changing content changes ID)
- Enables merkle trees for efficient proofs
- Git-like model for forking/merging universes

**Consequences**:
- ✅ Tamper-evident (any change detected)
- ✅ Deduplication (identical content = same ID)
- ❌ Requires canonical serialization (e.g., sorted N-Quads)
- ❌ Versioning requires explicit parent links

**Hash Algorithm**: SHA-256 for now, upgradable via version prefix (e.g., `sha3-256:abcd...`).

---

## 5. Critical Questions Answered

### Q1: Where does RDF fit?

**Answer**: RDF is the **native format** for:
- **Observations**: Ingested as RDF quads (Turtle, JSON-LD, N-Triples)
- **Delta**: Stored as `additions` and `deletions` quad arrays
- **Universe Schema (Σ)**: Defined using SHACL or custom RDF vocabulary
- **Invariants (Q)**: Expressed as SPARQL ASK queries
- **Artifacts**: Can be RDF (e.g., derived ontology) OR binary (e.g., PDF)

**Storage**:
- Scenes stored as **N-Quads files** (one file per scene)
- Triple store (`@unrdf/oxigraph`) holds current universe state
- Receipts stored as **JSON-LD** (semantic + machine-readable)

**Example Scene File** (`scene-abc123.nq`):
```nquads
<urn:scene:abc123> <rdf:type> <ex:Scene> .
<urn:scene:abc123> <ex:observation> _:obs1 .
_:obs1 <ex:subject> <urn:agent:alice> .
_:obs1 <ex:predicate> <ex:voted> .
_:obs1 <ex:object> "option-A" .
_:obs1 <ex:timestamp> "2025-12-27T10:00:00Z"^^xsd:dateTime .
```

---

### Q2: Is μ a pure function or can it have side effects?

**Answer**: μ is **strictly pure** (see ADR-002).

**Side effects handled via**:
1. **sideEffectTokens**: Opaque strings/objects in `Consequence`
2. **Executor service**: Reads tokens AFTER receipt is signed, performs async actions
3. **Audit trail**: Each side effect logged with receipt_id

**Example**:
```javascript
// μ returns tokens
{
  status: 'accepted',
  sideEffectTokens: [
    {type: 'email', to: 'alice@example.com', subject: 'Vote recorded'},
    {type: 'webhook', url: 'https://analytics.example.com', payload: {...}}
  ]
}

// Executor processes tokens
for (const token of consequence.sideEffectTokens) {
  await sideEffectExecutor.handle(token);
  auditLog.append({receiptId, token, timestamp, result});
}
```

**Guarantee**: Even if side effects fail, receipt remains valid (state transition already committed).

---

### Q3: Can invariants Q be violated temporarily?

**Answer**: Yes, with **enforcement mode = 'eventual'** (see ADR-003).

**Strict mode** (default):
- Invariant MUST hold before transition
- Invariant MUST hold after transition
- Violation → immediate rejection

**Eventual mode**:
- Invariant can be violated for up to `MAX_SCENES` transitions (config: default 10)
- Scene N violates → flagged as "in-repair"
- Scene N+k must restore invariant (k ≤ MAX_SCENES)
- If not restored → universe enters `halted` state

**Use case**: Multi-step atomic operations
```
Scene 1: Debit account A ($100)     → Violates "sum(accounts) = constant"
Scene 2: Credit account B ($100)    → Restores invariant ✓
```

**Tracking**: Each scene's receipt includes `invariantViolationDepth` counter.

---

### Q4: Who signs receipts?

**Answer**: **Dual signature** (see ADR-004):

1. **Universe Authority** (system key):
   - Signs: `hash(sceneId + guardResults + invariantChecks + timestamp)`
   - Proves: Guards and invariants were correctly evaluated
   - Public key: Published in universe metadata

2. **Scene Author** (user/agent key):
   - Signs: `hash(observations + delta)`
   - Proves: Author claims responsibility for this change
   - Public key: Linked to DID in observation metadata

**Optional third signatures**:
- **Witnesses**: Third-party validators (for high-stakes universes)
- **Bridge authorities**: When scene crosses universes

**Verification**:
```javascript
function verifyReceipt(receipt, universe, author) {
  const universeValid = verifySignature(
    receipt.signature.universe,
    universe.publicKey
  );
  const authorValid = verifySignature(
    receipt.signature.author,
    author.publicKey
  );
  return universeValid && authorValid;
}
```

**Trust model**:
- If ONLY universe signed → Universe could fabricate scenes
- If ONLY author signed → Author could claim invalid scenes were accepted
- BOTH signed → Non-repudiation + authenticity

---

## 6. Data Flow Example (Governance Vote)

```
SCENARIO: Alice votes "yes" on proposal #42 in GovernanceUniverse

Step 1: OBSERVATION
───────────────────
Alice's wallet submits:
{
  quad: {
    subject: 'did:key:alice',
    predicate: 'gov:votedFor',
    object: 'proposal:42#yes',
    graph: 'vote-session:2025-12-27'
  },
  source: 'wallet:alice',
  timestamp: 1703674800,
  confidence: 1.0
}

Step 2: GUARD CHECK
───────────────────
Guards evaluate:
✓ G1 (isRegisteredVoter): Alice in voters list
✓ G2 (hasNotVotedYet): No prior vote from Alice on #42
✓ G3 (withinVotingPeriod): Current time < deadline
✓ G4 (signatureValid): Wallet signature matches Alice's DID

All guards pass → Admissible

Step 3: RECONCILIATION (μ)
──────────────────────────
Current state (triple store):
  proposal:42 gov:voteCount "5"^^xsd:integer .
  proposal:42 gov:yesVotes "2"^^xsd:integer .

Delta computation:
  additions: [
    <did:key:alice> <gov:votedFor> <proposal:42#yes> .
  ]
  deletions: [
    <proposal:42> <gov:yesVotes> "2"^^xsd:integer .
  ]
  additions: [
    <proposal:42> <gov:yesVotes> "3"^^xsd:integer .
    <proposal:42> <gov:voteCount> "6"^^xsd:integer .
  ]

Minimality proof:
  "Graph edit distance = 3 (minimal by set-cover algorithm)"

Step 4: INVARIANT CHECK
───────────────────────
Q1: "voteCount = yesVotes + noVotes"
  SPARQL ASK: SELECT (sum(?yes) + sum(?no) = ?total) WHERE {...}
  Result: TRUE ✓

Q2: "Each voter voted at most once"
  SPARQL ASK: SELECT ?voter (COUNT(?vote) as ?c) WHERE {...} HAVING (?c > 1)
  Result: No matches ✓

Step 5: RECEIPT GENERATION
──────────────────────────
{
  sceneId: 'sha256:9f3a...',
  timestamp: 1703674801,
  admissibilityChecks: [
    {guardId: 'G1', allowed: true, ...},
    {guardId: 'G2', allowed: true, ...},
    {guardId: 'G3', allowed: true, ...},
    {guardId: 'G4', allowed: true, ...}
  ],
  minimalityProof: {
    algorithm: 'set-cover',
    alternativeCount: 7,
    justification: '...'
  },
  forkParents: ['sha256:7e2a...'],
  signature: {
    universe: {publicKey: 'uni_pk', signature: 'uni_sig'},
    author: {publicKey: 'alice_pk', signature: 'alice_sig'}
  },
  receiptHash: 'sha256:4d1e...'
}

Step 6: ARTIFACT OUTPUT
───────────────────────
1. Update triple store with delta
2. Emit artifacts:
   - vote-ledger.ttl (append-only log)
   - metrics.json (vote tallies)
3. Append receipt to receipts.ndjson
4. Execute side effects:
   - {type: 'notify', user: 'alice', msg: 'Vote recorded'}
   - {type: 'webhook', url: 'https://dashboard.gov/update'}

DONE: Alice's vote is now immutably recorded with cryptographic proof.
```

---

## 7. Implementation Roadmap

### Phase 1: Core Types (Week 1)
- [ ] Implement JSDoc types in `/src/types/narrative-state-machine.d.ts`
- [ ] Create validation schemas using Zod
- [ ] Unit tests for type constraints

### Phase 2: Universe Management (Week 2)
- [ ] UniverseRecord CRUD operations
- [ ] Schema validation (SHACL integration)
- [ ] Invariant engine (SPARQL ASK execution)
- [ ] Guard system with pluggable predicates

### Phase 3: Scene Processing (Week 3)
- [ ] Observation ingestion
- [ ] Delta computation (graph diff algorithm)
- [ ] Reconciliation engine (μ executor)
- [ ] Receipt generation and signing

### Phase 4: Bridge System (Week 4)
- [ ] Type coercion rules engine
- [ ] Invariant preservation proofs
- [ ] Cross-universe scene translation
- [ ] Bridge registry and discovery

### Phase 5: Production Hardening (Week 5)
- [ ] OTEL instrumentation (spans for each phase)
- [ ] Performance benchmarks (target: <100ms per scene)
- [ ] Failure recovery (rollback, compensation)
- [ ] Audit trail export (JSON-LD, CSV)

---

## 8. Open Questions for Review

1. **Concurrency**: How do we handle concurrent scenes targeting same universe?
   - Option A: Serial execution (simple, slow)
   - Option B: Optimistic locking with conflict detection
   - Option C: CRDT-based automatic merging

2. **Bridge Trust**: Who can create bridges? How to prevent malicious translation?
   - Require N-of-M multi-sig from both universe authorities?
   - Public bridge registry with community vetting?

3. **Receipt Storage**: Append-only log vs content-addressed store?
   - Log: Simpler, faster append, harder to query
   - CAS: Git-like, efficient dedup, requires indexing

4. **Minimality Algorithm**: Set-cover vs graph edit distance vs custom?
   - Set-cover: NP-hard, approximation OK?
   - Edit distance: Polynomial but less semantic
   - Custom: Domain-specific (e.g., voting has known patterns)

5. **Side Effect Failure**: What if email fails but receipt is signed?
   - Retry with exponential backoff?
   - Dead letter queue?
   - Mark scene as "partially-executed" (but state is committed)?

---

## 9. Validation Criteria

Before implementation begins, this design must satisfy:

- [ ] **Determinism**: Same observations → same receipt hash (tested with 1000 replays)
- [ ] **Auditability**: Every state transition traceable to signed receipt
- [ ] **Minimality**: Delta proven minimal (via formal algorithm, not heuristic)
- [ ] **Composability**: Bridges enable cross-universe workflows
- [ ] **Performance**: <100ms median scene processing (OTEL-verified)
- [ ] **Correctness**: Invariants hold with p>0.999 (formal verification or extensive testing)

**Acceptance Test**: Implement simple GovernanceUniverse with 3 invariants, 5 guards, process 1000 votes, verify:
- All receipts cryptographically valid
- No invariant violations
- Replay from genesis yields identical final state
- Cross-universe bridge to "ArchivalUniverse" preserves vote counts

---

## Appendices

### A. Glossary

- **Universe**: Isolated context with own rules (schema, guards, invariants)
- **Scene**: Atomic state transition with cryptographic receipt
- **Observation**: Raw input (RDF quads) from external source
- **Delta**: Minimal set of quad additions/deletions
- **Consequence**: Result of applying delta (new graph state)
- **Guard**: Access control predicate (who can do what)
- **Invariant**: Logical constraint that must hold
- **Receipt**: Signed proof of admissibility and minimality
- **Bridge**: Translator between universes (with proof of correctness)
- **Artifact**: Deterministic output (file, notification, etc.)

### B. References

- [RDF 1.1 Concepts](https://www.w3.org/TR/rdf11-concepts/)
- [SPARQL 1.1 Query Language](https://www.w3.org/TR/sparql11-query/)
- [SHACL - Shapes Constraint Language](https://www.w3.org/TR/shacl/)
- [Content Addressing](https://en.wikipedia.org/wiki/Content-addressable_storage)
- [Merkle Trees](https://en.wikipedia.org/wiki/Merkle_tree)
- [Ed25519 Signatures](https://ed25519.cr.yp.to/)

### C. Notation

- Σ (Sigma): Schema (type system)
- μ (mu): Reconciliation function
- Q: Invariant set
- H: Guard map
- Φ (Phi): Bridge proof
- O: Observations
- Δ (Delta): Change set
- ⟨...⟩: Tuple/record
- →: Function/mapping
- ⟹: Logical implication
- ∈: Element of set

---

**END OF DOCUMENT**

*This architecture is DRAFT. Implementation begins after team review and consensus on open questions.*
