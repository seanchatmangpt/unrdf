# Admission Engine - RDF Change Gatekeeper

**Location**: `/home/user/unrdf/src/admission/`

**Purpose**: Gate all RDF store modifications via Δ (delta) capsules and invariants Q, enforcing forbidden operations H.

---

## Overview

The Admission Engine is the **central gatekeeper** for all changes to the RDF store. Every modification MUST go through admission control to ensure:

1. **Substrate Protection**: Industrial substrate (RDF, RDFS, OWL) cannot be modified
2. **Canon Preservation**: Canonical terms retain their fixed semantics
3. **Data Quality**: All changes meet RDF validity and provenance requirements
4. **Complexity Bounds**: Changes stay within tractability limits

---

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    Δ Capsule                            │
│  ┌───────────────────────────────────────────────────┐  │
│  │ Partition: overlay-1                              │  │
│  │ Changes: [add 100 quads]                          │  │
│  │ Invariants: [Q_typing, Q_noncollision, ...]      │  │
│  │ Provenance: {agent: "alice", timestamp: ...}     │  │
│  └───────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────┐
│              Admission Engine                           │
│  ┌─────────────────────────────────────────────────┐   │
│  │  1. Forbidden Operations (H) - Hard Blocks      │   │
│  │     ├─ EditIndustrialSubstrate                  │   │
│  │     ├─ RedefineProtectedTerm                    │   │
│  │     └─ WeakenCorporateCanon                     │   │
│  └─────────────────────────────────────────────────┘   │
│                         │                               │
│                         ▼                               │
│  ┌─────────────────────────────────────────────────┐   │
│  │  2. Invariants (Q) - Quality Checks             │   │
│  │     ├─ Q_typing      (RDF syntax valid)         │   │
│  │     ├─ Q_noncollision (No namespace conflicts)  │   │
│  │     ├─ Q_monotone    (Overlay additive)         │   │
│  │     ├─ Q_determinism (Hash check)               │   │
│  │     ├─ Q_provenance  (Audit trail complete)     │   │
│  │     └─ Q_bounds      (Complexity in limits)     │   │
│  └─────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────┘
                         │
                         ▼
            ┌─────────────────────────┐
            │   Decision: ALLOW/DENY   │
            │   Reason: "..."          │
            │   Details: {...}         │
            └─────────────────────────┘
```

---

## Core Components

### 1. Delta Capsule (`delta-capsule.mjs`)

**Purpose**: Represents a proposed batch of changes.

**Structure**:
```javascript
{
  id: "uuid",
  partition: {
    namespace: "http://example.org/overlay/",
    name: "user-extensions",
    protected: false
  },
  changes: [{
    operation: "add",  // or "delete", "update"
    quads: [/* RDF quads */]
  }],
  invariants: [
    { name: "Q_typing", enabled: true, strictness: "error" }
  ],
  provenance: {
    agent: "alice",
    timestamp: "2025-12-26T10:00:00Z",
    source: "ui-form",
    justification: "User profile update"
  },
  metadata: { /* custom metadata */ }
}
```

**Key Methods**:
- `getQuadCount()` - Total number of proposed quads
- `isAdditiveOnly()` - Check if only additions (no deletes/updates)
- `getNamespaces()` - Extract all namespaces used
- `getHash()` - SHA-256 hash for determinism checking

**Example**:
```javascript
import { DeltaCapsule } from './delta-capsule.mjs';

const delta = new DeltaCapsule({
  partition: { namespace: 'http://example.org/', name: 'test' },
  changes: [{ operation: 'add', quads: [...] }],
  invariants: [{ name: 'Q_typing', enabled: true }],
  provenance: { agent: 'test', timestamp: new Date().toISOString() }
});

console.log(delta.getQuadCount());  // e.g., 42
console.log(delta.isAdditiveOnly()); // true
```

---

### 2. Forbidden Operations (`forbidden-operations.mjs`)

**Purpose**: Define operations that are NEVER allowed.

**Guards**:

#### H1: EditIndustrialSubstrate
- **Blocks**: DELETE/UPDATE on protected namespaces
- **Protected Namespaces**:
  - `http://www.w3.org/1999/02/22-rdf-syntax-ns#` (RDF)
  - `http://www.w3.org/2000/01/rdf-schema#` (RDFS)
  - `http://www.w3.org/2002/07/owl#` (OWL)
  - `http://www.w3.org/ns/shacl#` (SHACL)
  - `http://www.w3.org/2004/02/skos/core#` (SKOS)
  - `http://purl.org/dc/elements/1.1/` (Dublin Core)
  - `http://xmlns.com/foaf/0.1/` (FOAF)

#### H2: RedefineProtectedTerm
- **Blocks**: Modifying canonical terms (rdf:type, rdfs:Class, etc.)
- **Allowed**: Only safe metadata predicates (rdfs:label, rdfs:comment)

#### H3: WeakenCorporateCanon
- **Blocks**: Deleting type assertions for canonical terms
- **Blocks**: Deleting structural relationships (rdfs:subClassOf, etc.)

**Example**:
```javascript
import { guardEditIndustrialSubstrate } from './forbidden-operations.mjs';

const result = guardEditIndustrialSubstrate(delta);

if (!result.allowed) {
  console.log(result.reason);  // "Attempted to delete protected industrial substrate..."
  console.log(result.violatingQuads);  // Array of problematic quads
}
```

---

### 3. Invariants (`invariants.mjs`)

**Purpose**: Define checkable properties that must hold.

**Invariant Set**:

#### Q_typing
- **Checks**: RDF syntax validity
- **Violations**: Literal subjects, non-IRI predicates, malformed IRIs
- **Example**: `{ s: "literal", p: "ex:p", o: "ex:o" }` → FAIL

#### Q_noncollision
- **Checks**: No IRI collisions with protected namespaces
- **Violations**: Creating terms in `owl:*`, `rdf:*`, etc.
- **Example**: `{ s: "owl:myClass", ... }` → FAIL

#### Q_monotone
- **Checks**: Overlays don't redefine substrate
- **Violations**: Non-additive changes, canonical term modifications
- **Example**: Deleting `rdf:type rdf:type rdf:Property` → FAIL

#### Q_determinism
- **Checks**: Same Δ → same hash
- **Violations**: Hash mismatch, non-deterministic hash
- **Example**: Hash recomputation differs → FAIL

#### Q_provenance
- **Checks**: Complete audit trail metadata
- **Violations**: Missing agent, invalid timestamp
- **Example**: `{ agent: "", timestamp: "..." }` → FAIL

#### Q_bounds
- **Checks**: Complexity within limits
- **Violations**: >10K quads, >10 nesting depth, >50 namespaces
- **Example**: 15,000 quads → FAIL

**Example**:
```javascript
import { Q_typing, Q_noncollision } from './invariants.mjs';

const typingResult = Q_typing(delta);
if (!typingResult.passed) {
  console.log(typingResult.reason);     // "RDF syntax violations detected..."
  console.log(typingResult.violations); // Array of violation details
}

const collisionResult = Q_noncollision(delta);
// Check result...
```

---

### 4. Admission Engine (`admission-engine.mjs`)

**Purpose**: Orchestrate guard and invariant checks, make ALLOW/DENY decision.

**Configuration**:
```javascript
import { AdmissionEngine } from './admission-engine.mjs';

const engine = new AdmissionEngine({
  strictMode: true,        // ANY failure → DENY
  allowWarnings: true,     // Allow warning-level issues
  customBounds: {          // Override default bounds
    maxQuads: 5000
  },
  auditLog: true          // Enable audit logging
});
```

**Usage**:
```javascript
const decision = await engine.admitCapsule(delta);

console.log(decision.decision);  // 'ALLOW' or 'DENY'
console.log(decision.reason);    // Human-readable reason

if (decision.allowed) {
  // Apply changes to store
  applyChanges(delta);
} else {
  // Log rejection
  console.error('Denied:', decision.reason);
  console.error('Blocked by:', decision.checks.forbiddenOperations.blockedBy);
  console.error('Failed invariants:', decision.checks.invariants.failedInvariants);
}
```

**Statistics & Audit**:
```javascript
const stats = engine.getStats();
console.log(stats);
// {
//   totalProcessed: 100,
//   allowed: 85,
//   denied: 15,
//   deniedByGuards: 8,
//   deniedByInvariants: 7,
//   allowRate: "85.00%",
//   denyRate: "15.00%"
// }

const auditLog = engine.getAuditLog(10);  // Last 10 entries
auditLog.forEach(entry => {
  console.log(`[${entry.timestamp}] ${entry.decision} - ${entry.reason}`);
});
```

**Dry-Run Validation**:
```javascript
// Validate without affecting stats
const result = await engine.validateCapsule(delta);

if (result.valid) {
  console.log('✅ Would be admitted');
} else {
  console.log('❌ Would be denied:', result.decision.reason);
}
```

---

## File Inventory

```
/home/user/unrdf/src/admission/
├── delta-capsule.mjs           # Δ capsule data structure + validation
├── forbidden-operations.mjs    # Guards (H1, H2, H3)
├── invariants.mjs              # Invariants (Q_typing, Q_noncollision, ...)
├── admission-engine.mjs        # Main admission engine
├── admission-engine.test.mjs   # Comprehensive test suite (vitest)
├── run-tests.mjs               # Standalone test runner (Node.js)
├── DECISION-MATRIX.md          # Decision patterns documentation
└── README.md                   # This file
```

**Total**: 8 files, ~3,500 LoC

---

## Decision Examples

See [DECISION-MATRIX.md](./DECISION-MATRIX.md) for comprehensive examples.

**Quick Reference**:

| Scenario | Decision | Primary Reason |
|----------|----------|----------------|
| Add quads to overlay namespace | ✅ ALLOW | Clean additive extension |
| Delete RDF substrate triple | ❌ DENY | H1: EditIndustrialSubstrate |
| Redefine rdf:type | ❌ DENY | H2: RedefineProtectedTerm |
| Create owl:myClass | ❌ DENY | Q_noncollision |
| Literal as subject | ❌ DENY | Q_typing |
| 15K quad batch | ❌ DENY | Q_bounds |
| Missing provenance | ❌ DENY | Q_provenance |

---

## Testing

### Run Tests (Requires Dependencies)

```bash
# Using vitest (if installed in workspace)
pnpm test src/admission/admission-engine.test.mjs

# Using standalone runner (no dependencies)
node src/admission/run-tests.mjs
```

### Test Coverage

- ✅ DeltaCapsule creation and validation
- ✅ All 3 forbidden operation guards
- ✅ All 6 invariant checks
- ✅ Admission engine ALLOW scenarios
- ✅ Admission engine DENY scenarios (guards)
- ✅ Admission engine DENY scenarios (invariants)
- ✅ Statistics tracking
- ✅ Audit logging
- ✅ Dry-run validation
- ✅ Batch processing

**Total Test Scenarios**: 8 core scenarios + edge cases

---

## Integration

### With RDF Store

```javascript
import { createStore } from '@unrdf/oxigraph';
import { AdmissionEngine } from './admission-engine.mjs';
import { DeltaCapsule } from './delta-capsule.mjs';

const store = createStore();
const engine = new AdmissionEngine({ strictMode: true });

async function applyChanges(delta) {
  const decision = await engine.admitCapsule(delta);

  if (!decision.allowed) {
    throw new Error(`Admission denied: ${decision.reason}`);
  }

  // Apply admitted changes to store
  for (const change of delta.changes) {
    if (change.operation === 'add') {
      for (const quad of change.quads) {
        store.add(quad);
      }
    }
    // Handle delete/update similarly
  }

  return {
    success: true,
    applied: delta.getQuadCount(),
    decision
  };
}

// Usage
const delta = new DeltaCapsule({
  partition: { namespace: 'http://example.org/', name: 'overlay' },
  changes: [{ operation: 'add', quads: [quad1, quad2] }],
  invariants: [
    { name: 'Q_typing', enabled: true },
    { name: 'Q_noncollision', enabled: true }
  ],
  provenance: { agent: 'user-123', timestamp: new Date().toISOString() }
});

const result = await applyChanges(delta);
console.log(`Applied ${result.applied} quads`);
```

---

## API Reference

### DeltaCapsule

```typescript
class DeltaCapsule {
  constructor(config: DeltaCapsuleConfig)

  // Getters
  getAllQuads(): Quad[]
  getQuadsByOperation(operation: 'add' | 'delete' | 'update'): Quad[]
  getQuadCount(): number
  getNamespaces(): Set<string>
  getHash(): string

  // Checks
  isAdditiveOnly(): boolean
  hasInvariant(name: string): boolean
  getInvariant(name: string): InvariantConfig | null

  // Serialization
  toJSON(): object
  static fromJSON(json: object): DeltaCapsule
  static validate(obj: object): ValidationResult
}
```

### AdmissionEngine

```typescript
class AdmissionEngine {
  constructor(config?: AdmissionConfig)

  // Main API
  async admitCapsule(capsule: DeltaCapsule, options?: object): Promise<DecisionResult>
  async admitBatch(capsules: DeltaCapsule[], options?: object): Promise<DecisionResult[]>
  async validateCapsule(capsule: DeltaCapsule): Promise<ValidationResult>

  // Statistics
  getStats(): Statistics
  resetStats(): void

  // Audit
  getAuditLog(limit?: number): AuditEntry[]
  clearAuditLog(): void
}
```

### Invariant Functions

```typescript
function Q_typing(capsule: DeltaCapsule, options?: object): InvariantResult
function Q_noncollision(capsule: DeltaCapsule, options?: object): InvariantResult
function Q_monotone(capsule: DeltaCapsule, options?: object): InvariantResult
function Q_determinism(capsule: DeltaCapsule, options?: object): InvariantResult
function Q_provenance(capsule: DeltaCapsule, options?: object): InvariantResult
function Q_bounds(capsule: DeltaCapsule, options?: object): InvariantResult

function checkAllInvariants(capsule: DeltaCapsule, options?: object): CombinedResult
```

### Guard Functions

```typescript
function guardEditIndustrialSubstrate(capsule: DeltaCapsule): GuardResult
function guardRedefineProtectedTerm(capsule: DeltaCapsule): GuardResult
function guardWeakenCorporateCanon(capsule: DeltaCapsule): GuardResult

function checkForbiddenOperations(capsule: DeltaCapsule): CombinedGuardResult
```

---

## Design Principles

1. **Defense in Depth**: Guards block dangerous patterns, invariants ensure quality
2. **Deterministic**: Same Δ → same decision (via hash checking)
3. **Auditable**: Full provenance tracking and audit logs
4. **Fail-Safe**: Strict mode denies on any uncertainty
5. **Extensible**: Custom invariants and bounds configurable
6. **Efficient**: Guards run before expensive invariant checks

---

## Performance Characteristics

- **Guard Checking**: O(n) where n = number of quads
- **Invariant Checking**: O(n) to O(n log n) depending on invariant
- **Hash Computation**: O(n log n) for canonical sorting
- **Memory**: O(n) for quad storage in capsule

**Benchmarks** (estimated for 1000 quads):
- Guard checks: <5ms
- Invariant checks: <20ms
- Total decision: <30ms

---

## Future Enhancements

- [ ] SHACL shape validation integration
- [ ] OWL reasoning pre-checks
- [ ] Parallel invariant execution
- [ ] Streaming admission for large batches
- [ ] Machine learning anomaly detection
- [ ] Distributed admission consensus

---

## References

- **CLAUDE.md**: Project coding standards
- **DECISION-MATRIX.md**: Comprehensive decision examples
- **Source Code**: All files JSDoc-documented

---

## License

MIT (see repository root LICENSE file)

---

## Author

Generated by Claude Code (Sonnet 4.5) for the UNRDF project.

**Date**: 2025-12-26
