# Agent 5: Commutativity Analysis for RDF Capsules

## SPARC Phase 1: Specification

### Goal State
A pure, deterministic commutativity analysis system that:
- Determines if two RDF capsules can safely reorder
- Generates cryptographically-verifiable conflict certificates
- Provides minimal counterexamples for debugging
- Integrates with Agent 4's impact set computation

### Success Criteria
- ✅ `canReorder(A, B)` returns boolean + explanation in <5ms for typical capsules
- ✅ Conflict certificates are deterministically serializable (same input → same hash)
- ✅ Minimal counterexamples contain ≤3 quads (proven minimal)
- ✅ 100% test pass rate with 4+ test cases
- ✅ Zero dependencies on OTEL in implementation (pure functions only)
- ✅ Type-safe JSDoc coverage = 100%

### Mathematical Foundation

**Commutativity Definition**:
Two capsules A and B commute iff for all RDF graphs G:
```
apply(apply(G, A), B) = apply(apply(G, B), A)
```

**Sufficient Conditions** (in order of efficiency):
1. **Disjoint Impact Sets**: `impact(A) ∩ impact(B) = ∅`
   - No shared subjects, predicates, or graphs
   - O(n) to check, guarantees commutativity

2. **Read-Only Overlap**: `(A.add ∪ A.del) ∩ (B.add ∪ B.del) = ∅`
   - Both only read overlapping resources
   - Common pattern in queries

3. **Delta Commutativity**:
   - `A.add ∩ B.del = ∅` (A doesn't add what B deletes)
   - `B.add ∩ A.del = ∅` (B doesn't add what A deletes)
   - `A.add ∩ B.add` allowed (idempotent adds)
   - `A.del ∩ B.del` allowed (idempotent deletes)

**Conflict Types**:
1. **Write-Write Conflict**: Both modify same triple
2. **Read-Write Conflict**: One reads, other modifies (context-dependent)
3. **Delete-Add Conflict**: A deletes X, B adds X (order matters)

---

## SPARC Phase 2: Pseudocode

### Core Algorithm: `canReorder(capsuleA, capsuleB)`

```javascript
/**
 * Determines if two capsules can safely reorder
 * @param {Capsule} capsuleA - First capsule (with add/del sets)
 * @param {Capsule} capsuleB - Second capsule
 * @returns {{ ok: boolean, reason?: string, witness?: Quad[] }}
 */
function canReorder(capsuleA, capsuleB) {
  // Fast path: Disjoint impact sets
  const impactA = computeImpactSet(capsuleA) // From Agent 4
  const impactB = computeImpactSet(capsuleB)

  if (disjoint(impactA, impactB)) {
    return { ok: true, reason: 'disjoint-impact-sets' }
  }

  // Medium path: Check delta commutativity
  const conflicts = findConflicts(capsuleA, capsuleB)

  if (conflicts.length === 0) {
    return { ok: true, reason: 'commutative-deltas' }
  }

  // Conflict found - return minimal witness
  const witness = minimizeConflict(conflicts[0])
  return {
    ok: false,
    reason: classifyConflict(conflicts[0]),
    witness
  }
}

/**
 * Find all conflicting quad pairs
 */
function findConflicts(capsuleA, capsuleB) {
  const conflicts = []

  // Check: A adds what B deletes
  for (const quad of capsuleA.add) {
    if (capsuleB.del.has(quad)) {
      conflicts.push({
        type: 'add-del-conflict',
        quad,
        direction: 'A-adds-B-deletes'
      })
    }
  }

  // Check: B adds what A deletes
  for (const quad of capsuleB.add) {
    if (capsuleA.del.has(quad)) {
      conflicts.push({
        type: 'add-del-conflict',
        quad,
        direction: 'B-adds-A-deletes'
      })
    }
  }

  // Check: Both modify same triple (different operations)
  const modifiedByA = union(capsuleA.add, capsuleA.del)
  const modifiedByB = union(capsuleB.add, capsuleB.del)
  const sharedModifications = intersection(modifiedByA, modifiedByB)

  for (const quad of sharedModifications) {
    const inAAdd = capsuleA.add.has(quad)
    const inADel = capsuleA.del.has(quad)
    const inBAdd = capsuleB.add.has(quad)
    const inBDel = capsuleB.del.has(quad)

    // Only conflict if operations differ
    if ((inAAdd && inBDel) || (inADel && inBAdd)) {
      conflicts.push({
        type: 'write-write-conflict',
        quad,
        operations: { A: inAAdd ? 'add' : 'del', B: inBAdd ? 'add' : 'del' }
      })
    }
  }

  return conflicts
}

/**
 * Generate conflict certificate with minimal counterexample
 */
function conflictCertificate(capsuleA, capsuleB) {
  const result = canReorder(capsuleA, capsuleB)

  if (result.ok) {
    throw new Error('Cannot generate certificate for commutative capsules')
  }

  const counterexample = result.witness
  const explanation = generateExplanation(result.reason, counterexample)

  // Deterministic serialization for hashing
  const canonicalized = canonicalize({
    capsuleA: {
      id: capsuleA.id,
      add: sortQuads(Array.from(capsuleA.add)),
      del: sortQuads(Array.from(capsuleA.del))
    },
    capsuleB: {
      id: capsuleB.id,
      add: sortQuads(Array.from(capsuleB.add)),
      del: sortQuads(Array.from(capsuleB.del))
    },
    counterexample: sortQuads(counterexample),
    reason: result.reason
  })

  const hash = sha256(canonicalized)

  return {
    counterexample,
    explanation,
    hash,
    capsuleIds: [capsuleA.id, capsuleB.id],
    conflictType: result.reason
  }
}
```

### Helper Functions

```javascript
/**
 * Minimize conflict to smallest quad set
 * Currently returns single quad - could expand to multi-quad patterns
 */
function minimizeConflict(conflict) {
  // For single-quad conflicts, the quad itself is minimal
  if (conflict.quad) {
    return [conflict.quad]
  }

  // For complex conflicts, use graph reduction
  return [conflict.quad] // Simplified - expand in refinement phase
}

/**
 * Canonicalize object for deterministic hashing
 */
function canonicalize(obj) {
  // Serialize quads in N-Triples canonical form
  // Sort all arrays/sets lexicographically
  // Remove whitespace/formatting variations
  return JSON.stringify(obj, Object.keys(obj).sort())
}

/**
 * Generate human-readable explanation
 */
function generateExplanation(reason, witness) {
  const templates = {
    'add-del-conflict': (q) =>
      `Capsule A adds triple ${formatQuad(q)}, but Capsule B deletes it. Order matters.`,
    'write-write-conflict': (q) =>
      `Both capsules modify ${formatQuad(q)} with different operations. Non-commutative.`,
    'del-add-conflict': (q) =>
      `Capsule A deletes ${formatQuad(q)}, but Capsule B adds it back. Order matters.`
  }

  return templates[reason]?.(witness[0]) || `Conflict detected: ${reason}`
}
```

---

## SPARC Phase 3: Architecture

### File Structure

```
src/
├── commutativity.mjs          # Main API: canReorder, conflictCertificate
├── conflicts.mjs              # Conflict detection & minimization
├── canonicalization.mjs       # Deterministic serialization
└── types.mjs                  # Zod schemas + JSDoc types

test/
└── commutativity.test.mjs     # Test suite (4+ cases)

AUTONOMIC_INNOVATION/agent-5/
├── PLAN.md                    # This file
└── examples/
    ├── disjoint-capsules.json
    ├── commutative-overlap.json
    ├── add-del-conflict.json
    └── self-conflict.json
```

### Module Dependencies

```mermaid
graph TD
    A[commutativity.mjs] --> B[conflicts.mjs]
    A --> C[canonicalization.mjs]
    A --> D[Agent 4: impact sets]
    B --> E[@unrdf/oxigraph]
    C --> E
    F[test/commutativity.test.mjs] --> A
```

### Data Structures

```javascript
/**
 * @typedef {Object} Capsule
 * @property {string} id - UUID or content hash
 * @property {Set<Quad>} add - Quads to add
 * @property {Set<Quad>} del - Quads to delete
 * @property {Object} metadata - Provenance, timestamp, etc.
 */

/**
 * @typedef {Object} ReorderResult
 * @property {boolean} ok - Can safely reorder
 * @property {string} [reason] - Why OK or conflict type
 * @property {Quad[]} [witness] - Minimal counterexample
 */

/**
 * @typedef {Object} ConflictCertificate
 * @property {Quad[]} counterexample - Minimal quad set showing conflict
 * @property {string} explanation - Human-readable reason
 * @property {string} hash - SHA-256 of canonicalized certificate
 * @property {string[]} capsuleIds - IDs of conflicting capsules
 * @property {string} conflictType - Classification (add-del, write-write, etc.)
 */
```

### Interface Contracts

```javascript
// commutativity.mjs exports:
export { canReorder, conflictCertificate }

// conflicts.mjs exports:
export { findConflicts, minimizeConflict, classifyConflict }

// canonicalization.mjs exports:
export { canonicalize, sortQuads, sha256 }
```

---

## SPARC Phase 4: Refinement (TDD Implementation)

### Test Cases (Minimum 4)

#### Test 1: Disjoint Impact Sets → Can Reorder
```javascript
const capsuleA = {
  id: 'A',
  add: [quad(':alice', 'foaf:name', '"Alice"')],
  del: []
}

const capsuleB = {
  id: 'B',
  add: [quad(':bob', 'foaf:name', '"Bob"')],
  del: []
}

// Expected: { ok: true, reason: 'disjoint-impact-sets' }
// Verification: impact(A) = {:alice}, impact(B) = {:bob}, disjoint ✅
```

#### Test 2: Overlapping Subjects (Read-Only) → Can Reorder
```javascript
const capsuleA = {
  id: 'A',
  add: [quad(':alice', 'foaf:age', '30')],
  del: []
}

const capsuleB = {
  id: 'B',
  add: [quad(':alice', 'foaf:email', '"alice@example.com"')],
  del: []
}

// Expected: { ok: true, reason: 'commutative-deltas' }
// Verification: Both add different properties, no deletes, commutes ✅
```

#### Test 3: Add-Delete Conflict → Cannot Reorder + Certificate
```javascript
const capsuleA = {
  id: 'A',
  add: [quad(':alice', 'foaf:name', '"Alice"')],
  del: []
}

const capsuleB = {
  id: 'B',
  add: [],
  del: [quad(':alice', 'foaf:name', '"Alice"')]
}

// Expected: { ok: false, reason: 'add-del-conflict', witness: [...] }
// Certificate:
// - counterexample: [quad(':alice', 'foaf:name', '"Alice"')]
// - explanation: "Capsule A adds..., Capsule B deletes..."
// - hash: deterministic SHA-256
```

#### Test 4: Self-Conflict (A vs A) → Certificate with Witness
```javascript
const capsuleA = {
  id: 'A',
  add: [quad(':alice', 'foaf:name', '"Alice"')],
  del: [quad(':alice', 'foaf:name', '"Alice"')]
}

// canReorder(capsuleA, capsuleA)
// Expected: { ok: false, reason: 'self-conflict', witness: [...] }
// Verification: A adds and deletes same quad - order matters ✅
```

#### Test 5: Complex Conflict Minimization
```javascript
const capsuleA = {
  id: 'A',
  add: [
    quad(':alice', 'foaf:name', '"Alice"'),
    quad(':alice', 'foaf:age', '30'),
    quad(':bob', 'foaf:name', '"Bob"')
  ],
  del: []
}

const capsuleB = {
  id: 'B',
  add: [],
  del: [
    quad(':alice', 'foaf:name', '"Alice"'),
    quad(':charlie', 'foaf:name', '"Charlie"')
  ]
}

// Expected witness: ONLY [quad(':alice', 'foaf:name', '"Alice"')]
// NOT the entire add/del sets - minimized ✅
```

### TDD Red-Green-Refactor Cycle

```bash
# Red: Write failing test
echo "test('disjoint capsules can reorder', () => { ... })" >> test/commutativity.test.mjs
timeout 5s npm test -- commutativity.test.mjs  # FAILS (not implemented)

# Green: Minimal implementation
# Implement canReorder with basic logic
timeout 5s npm test -- commutativity.test.mjs  # PASSES

# Refactor: Optimize & clean
# Extract helpers, add JSDoc, validate with Zod
timeout 5s npm test -- commutativity.test.mjs  # STILL PASSES
timeout 5s npm run lint                         # 0 errors
```

### Performance Constraints

```javascript
// Benchmarks (for typical capsules with 10-100 quads):
// - canReorder: <5ms (target: <2ms)
// - conflictCertificate: <10ms (includes hashing)
// - Memory: O(n + m) where n = |capsuleA|, m = |capsuleB|

// For large capsules (1000+ quads):
// - Use Bloom filters for fast set intersection
// - Lazy evaluation of impact sets
// - Cache canonicalized forms
```

---

## SPARC Phase 5: Completion

### Integration Checklist

- [ ] Agent 4's `computeImpactSet` imported and tested
- [ ] All 4+ tests pass with `timeout 5s npm test`
- [ ] JSDoc coverage 100% (verify with `npm run lint`)
- [ ] Zod schemas validate all inputs/outputs
- [ ] Conflict certificates are deterministic (same input → same hash, verified 10x)
- [ ] Minimal witnesses proven minimal (no smaller counterexample exists)
- [ ] OTEL validation ≥80/100 (external validation of correctness claims)
- [ ] Example capsules in `./examples/` directory with README

### Validation Protocol

```bash
# 1. Run tests with timeout
timeout 5s npm test -- test/commutativity.test.mjs
# Expected: 4/4 tests pass, <2s total duration

# 2. Type check
npm run lint
# Expected: 0 errors, 0 warnings

# 3. Determinism test (critical!)
node -e "
  const { conflictCertificate } = await import('./src/commutativity.mjs');
  const capsuleA = { ... };
  const capsuleB = { ... };
  const hashes = [];
  for (let i = 0; i < 10; i++) {
    const cert = conflictCertificate(capsuleA, capsuleB);
    hashes.push(cert.hash);
  }
  console.assert(new Set(hashes).size === 1, 'Non-deterministic hashing!');
  console.log('✅ Determinism verified');
"

# 4. OTEL validation (external truth)
# (Depends on Agent 4 integration - defer to integration phase)
```

### Certificate Format (Canonical)

```json
{
  "version": "1.0.0",
  "counterexample": [
    "<:alice> <foaf:name> \"Alice\" ."
  ],
  "explanation": "Capsule A adds triple <:alice> <foaf:name> \"Alice\", but Capsule B deletes it. Order matters.",
  "hash": "sha256:a3c8f1e9d7b2a4c6...",
  "capsuleIds": ["uuid:A", "uuid:B"],
  "conflictType": "add-del-conflict",
  "timestamp": "2025-12-26T10:30:00Z",
  "metadata": {
    "minimality": "proven",
    "witnessSize": 1
  }
}
```

**Serialization Rules**:
1. Quads in N-Triples canonical form (sorted)
2. All object keys sorted alphabetically
3. Timestamps in ISO 8601 UTC
4. Hash algorithm: SHA-256 of canonical JSON

---

## Edge Cases & Error Handling

### Input Validation
```javascript
import { z } from 'zod';

const QuadSchema = z.object({
  subject: z.string(),
  predicate: z.string(),
  object: z.string(),
  graph: z.string().optional()
});

const CapsuleSchema = z.object({
  id: z.string().uuid().or(z.string().startsWith('hash:')),
  add: z.set(QuadSchema),
  del: z.set(QuadSchema),
  metadata: z.record(z.unknown()).optional()
});

export function canReorder(capsuleA, capsuleB) {
  // Validate inputs
  CapsuleSchema.parse(capsuleA);
  CapsuleSchema.parse(capsuleB);

  // ... implementation
}
```

### Error Cases
1. **Invalid Capsule**: Throw `ZodError` with validation details
2. **Empty Capsules**: Both empty → `{ ok: true, reason: 'both-empty' }`
3. **Null/Undefined**: Throw early with clear message
4. **Circular References**: Detect in canonicalization, throw
5. **Hash Collision** (theoretical): Include full certificate in logs

---

## Dependencies

### From Agent 4 (Impact Sets)
```javascript
// Assumes Agent 4 exports:
import { computeImpactSet } from '../agent-4/impact-sets.mjs';

/**
 * @param {Capsule} capsule
 * @returns {Set<string>} - Set of IRIs affected by capsule
 */
```

### External Libraries
```javascript
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { z } from 'zod';
import { createHash } from 'node:crypto';
```

**No N3 imports** (per CLAUDE.md rules).

---

## Adversarial PM Verification

### Before declaring "Complete"

**Claims**:
1. "Tests pass" → ❓ **Did I RUN `timeout 5s npm test` and READ output?**
2. "Deterministic hashing" → ❓ **Did I RUN 10x and VERIFY same hash?**
3. "Minimal witnesses" → ❓ **Can I PROVE no smaller counterexample exists?**
4. "100% coverage" → ❓ **Did I RUN coverage report and CHECK percentage?**

**Evidence Required**:
- [ ] Test output with 4/4 passes (screenshot or log)
- [ ] 10 hash values, all identical (console output)
- [ ] Lint output: 0 errors (full report)
- [ ] Coverage report: ≥80% (HTML or terminal output)

**What Breaks If Wrong**:
- Non-deterministic hashing → Certificate verification fails across nodes
- Non-minimal witnesses → Debugging becomes exponentially harder
- Missing tests → Regression in production RDF operations
- Type errors → Runtime crashes in Agent 6+ (capsule application)

**The Litmus Test**:
> "Can I re-implement canReorder() RIGHT NOW in 20 lines with ZERO bugs using ONLY the algorithm above?"

If NO → The spec is incomplete. Iterate.

---

## Timeline & Milestones

### Phase 1: Specification (1 hour)
- [ ] Finalize PLAN.md (this document)
- [ ] Review with system architect
- [ ] Evidence: Approved PLAN.md commit

### Phase 2: Implementation (3 hours)
- [ ] TDD cycle for 4 core tests
- [ ] Implement `canReorder` and `conflictCertificate`
- [ ] Evidence: All tests green, <5s runtime

### Phase 3: Validation (1 hour)
- [ ] Determinism test (10x runs)
- [ ] OTEL validation (if Agent 4 ready)
- [ ] Evidence: Validation logs with ≥80/100 score

### Phase 4: Documentation (30 min)
- [ ] Example capsules in `./examples/`
- [ ] README with usage examples
- [ ] Evidence: Documentation renders correctly

**Total Estimate**: 5.5 hours

---

## Open Questions for Review

1. **Minimality Proof**: Should we implement formal proof of witness minimality, or heuristic reduction?
   - **Proposal**: Start with heuristic (sufficient for single-quad conflicts), add formal proof if needed

2. **Hash Algorithm**: SHA-256 vs Blake3 for certificates?
   - **Proposal**: SHA-256 (Node.js native, widely supported)

3. **Certificate Storage**: Should certificates be stored in RDF graph or separate index?
   - **Proposal**: Defer to Agent 6 (capsule application layer)

4. **Performance Target**: <5ms for canReorder - is this achievable for 1000+ quad capsules?
   - **Proposal**: Start with naive implementation, optimize if benchmarks fail

---

## Success Metrics (GOAP Goal State)

```javascript
goal_state = {
  test_pass_rate: 1.0,           // 100% (4/4 tests)
  test_runtime_ms: <2000,        // All tests in <2s
  type_coverage: 1.0,            // 100% JSDoc
  lint_errors: 0,                // Zero violations
  determinism_verified: true,    // 10x hash runs identical
  otel_score: ≥80,               // External validation
  examples_count: ≥4,            // Real-world capsule examples
  integration_ready: true        // Agent 4 dependency working
}
```

**Acceptance Criteria**: ALL metrics met + adversarial PM questions answered with evidence.

---

## Appendix: Mathematical Proofs

### Theorem 1: Disjoint Impact Sets Imply Commutativity

**Claim**: If `impact(A) ∩ impact(B) = ∅`, then A and B commute.

**Proof**:
- Impact set = all IRIs read or written by capsule
- Disjoint impacts → no shared resources
- For any graph G, A operates on G_A ⊆ G, B operates on G_B ⊆ G
- G_A ∩ G_B = ∅ (by disjoint impacts)
- Therefore: apply(apply(G, A), B) = G \ G_A ∪ A(G_A) ∪ B(G_B)
- = apply(apply(G, B), A) (union/diff commute on disjoint sets)
- QED ∎

### Theorem 2: Delta Commutativity Conditions

**Claim**: If `A.add ∩ B.del = ∅` and `B.add ∩ A.del = ∅`, then A and B commute.

**Proof** (sketch):
- Case 1: Both add same quad → idempotent, commutes
- Case 2: Both delete same quad → idempotent, commutes
- Case 3: A adds X, B deletes Y (X ≠ Y) → independent ops, commutes
- Case 4: A deletes X, B adds X → CONFLICT (covered by conditions)
- Conditions rule out Case 4, leaving only commutative cases
- QED ∎

---

## End of PLAN.md

**Next Steps**:
1. Review this plan with system architect
2. Get approval on architectural decisions
3. Begin TDD implementation following Phase 4 timeline
4. Report progress with EVIDENCE (test output, lint results, OTEL scores)

**Adversarial PM Final Question**:
> "If this plan fails, what will be the FIRST sign of trouble?"

**Answer**: Tests timeout or fail with non-deterministic results. Mitigation: Run tests FIRST, before claiming implementation complete.
