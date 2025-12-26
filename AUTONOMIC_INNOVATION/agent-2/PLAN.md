# Agent 2: Capsule IR Implementation Plan

**Role**: Design and implement the Capsule IR - a stable, content-addressed format for portable change programs.

**Philosophy**: Capsules are immutable, deterministic change programs. Like Git commits, they're identified by content hash. Like Bitcoin transactions, they're tamper-evident and form parent chains.

---

## 1. Overview & Data Model

### Capsule Structure (Zod Schema)
```javascript
{
  id: string,                     // Content hash (BLAKE3) of canonical capsule
  version: 'capsule/v1',          // Schema version

  intent: {                       // High-level operations
    ops: [
      { type: 'set', subject, predicate, object },
      { type: 'create', subject, graph },
      { type: 'link', subject, predicate, target },
      { type: 'unlink', subject, predicate, target }
    ]
  },

  delta: {                        // Compiled quad operations
    add: [quad, quad, ...],       // Additions (canonically sorted)
    del: [quad, quad, ...]        // Deletions (canonically sorted)
  },

  guard: {                        // Safety constraints
    limits: {
      maxQuads: number,           // Max delta size
      maxDepth: number,           // Max graph depth
      timeout: number             // Max execution time (ms)
    },
    profiles: [string, ...]       // Required convention profiles
  },

  receipt: {                      // Provenance & integrity
    hash: string,                 // BLAKE3 content hash
    parents: [hash, ...],         // Parent capsule hashes (chronological)
    timestamp: string,            // ISO 8601 with nanosecond precision
    signer?: string               // Optional hook signature
  }
}
```

### Design Principles
1. **Determinism First**: Same intent + store → same capsule ID
2. **Content Addressing**: Hash is computed from canonical serialization
3. **Parent Chains**: Capsules reference previous capsules (like Git commits)
4. **Guard Rails**: Profiles enforce conventions, limits prevent abuse
5. **Idempotence**: `planCapsule(planCapsule(x)) === planCapsule(x)`

---

## 2. Files to Create

### Core Modules (`./src/`)
1. **`capsule.mjs`** - Capsule schema, constructors, validation
2. **`canonicalize.mjs`** - Deterministic canonicalization (stable JSON + quad sorting)
3. **`hash.mjs`** - BLAKE3 hashing with parent chain tracking
4. **`verify.mjs`** - Verification and tamper detection
5. **`planner.mjs`** - Intent → Delta compilation (uses store to compute changes)
6. **`index.mjs`** - Public API exports

### Tests (`./test/`)
1. **`capsule.test.mjs`** - Capsule creation and serialization
2. **`determinism.test.mjs`** - Hash stability (run twice, same hash)
3. **`planner.test.mjs`** - Intent compilation tests
4. **`verification.test.mjs`** - Tamper detection and parent chain verification
5. **`idempotence.test.mjs`** - Idempotence guarantees

---

## 3. Module Details & Pseudocode

### 3.1 `capsule.mjs` - Schema & Constructors

**Purpose**: Define Capsule data structure with Zod validation and factory functions.

**Zod Schemas**:
```javascript
import { z } from 'zod';

// Intent operation types
const IntentOpSchema = z.discriminatedUnion('type', [
  z.object({ type: z.literal('set'), subject: z.string(), predicate: z.string(), object: z.any() }),
  z.object({ type: z.literal('create'), subject: z.string(), graph: z.string() }),
  z.object({ type: z.literal('link'), subject: z.string(), predicate: z.string(), target: z.string() }),
  z.object({ type: z.literal('unlink'), subject: z.string(), predicate: z.string(), target: z.string() }),
]);

// Quad representation (serialized for determinism)
const QuadSchema = z.object({
  subject: z.string(),
  subjectType: z.enum(['NamedNode', 'BlankNode']),
  predicate: z.string(),
  object: z.object({
    value: z.string(),
    type: z.enum(['NamedNode', 'BlankNode', 'Literal']),
    datatype: z.string().optional(),
    language: z.string().optional(),
  }),
  graph: z.string(),
});

const CapsuleSchema = z.object({
  id: z.string(),
  version: z.literal('capsule/v1'),
  intent: z.object({ ops: z.array(IntentOpSchema) }),
  delta: z.object({
    add: z.array(QuadSchema),
    del: z.array(QuadSchema),
  }),
  guard: z.object({
    limits: z.object({
      maxQuads: z.number().int().positive(),
      maxDepth: z.number().int().positive(),
      timeout: z.number().int().positive(),
    }),
    profiles: z.array(z.string()),
  }),
  receipt: z.object({
    hash: z.string(),
    parents: z.array(z.string()),
    timestamp: z.string(),
    signer: z.string().optional(),
  }),
});
```

**Functions**:
```javascript
/**
 * Create a new Capsule from intent and delta
 * @param {Object} intent - High-level operations
 * @param {Object} delta - Compiled quad changes
 * @param {Object} guard - Safety constraints
 * @param {Array<string>} parents - Parent capsule hashes
 * @returns {Object} Validated Capsule
 */
export function createCapsule(intent, delta, guard, parents = []) {
  // 1. Canonicalize delta quads (deterministic sorting)
  const canonicalDelta = {
    add: canonicalizeQuads(delta.add),
    del: canonicalizeQuads(delta.del),
  };

  // 2. Generate timestamp (nanosecond precision from @unrdf/kgc-4d)
  const timestamp = toISO(now());

  // 3. Build capsule (without hash/id yet)
  const capsule = {
    version: 'capsule/v1',
    intent,
    delta: canonicalDelta,
    guard,
    receipt: {
      hash: '', // Computed next
      parents: parents.sort(), // Chronological order
      timestamp,
      signer: undefined,
    },
  };

  // 4. Compute content hash
  const hash = hashCapsule(capsule);
  capsule.id = hash;
  capsule.receipt.hash = hash;

  // 5. Validate with Zod
  return CapsuleSchema.parse(capsule);
}
```

**Exports**:
- `CapsuleSchema` - Zod validator
- `createCapsule(intent, delta, guard, parents)` - Factory
- `validateCapsule(capsule)` - Validation function

---

### 3.2 `canonicalize.mjs` - Deterministic Canonicalization

**Purpose**: Ensure identical capsules produce identical hashes via canonical ordering.

**Algorithm**:
```javascript
/**
 * Canonicalize a capsule for hashing
 * Steps:
 * 1. Sort intent ops by type, then subject, then predicate
 * 2. Sort delta quads using RDF canonical ordering
 * 3. Sort parent hashes lexicographically
 * 4. Normalize all strings to UTF-8 NFC
 * 5. Serialize to JSON with sorted keys
 *
 * @param {Object} capsule - Capsule to canonicalize
 * @returns {string} Canonical JSON string
 */
export function canonicalizeCapsule(capsule) {
  // 1. Deep clone to avoid mutation
  const canonical = JSON.parse(JSON.stringify(capsule));

  // 2. Sort intent operations
  canonical.intent.ops.sort((a, b) => {
    // Compare type first
    if (a.type !== b.type) return a.type.localeCompare(b.type);
    // Then subject
    if (a.subject !== b.subject) return a.subject.localeCompare(b.subject);
    // Then predicate (if exists)
    const aPred = a.predicate || '';
    const bPred = b.predicate || '';
    return aPred.localeCompare(bPred);
  });

  // 3. Sort delta quads (RDF canonical order: S-P-O)
  canonical.delta.add = canonicalizeQuads(canonical.delta.add);
  canonical.delta.del = canonicalizeQuads(canonical.delta.del);

  // 4. Sort parent hashes
  canonical.receipt.parents.sort();

  // 5. UTF-8 NFC normalization (for Unicode stability)
  const normalized = normalizeStrings(canonical);

  // 6. JSON stringify with sorted keys
  return JSON.stringify(normalized, Object.keys(normalized).sort());
}

/**
 * Canonicalize quad array using RDF spec ordering
 * @param {Array<Object>} quads - Serialized quads
 * @returns {Array<Object>} Sorted quads
 */
export function canonicalizeQuads(quads) {
  return quads.sort((a, b) => {
    // Subject comparison
    const sCompare = a.subject < b.subject ? -1 : a.subject > b.subject ? 1 : 0;
    if (sCompare !== 0) return sCompare;

    // Predicate comparison
    const pCompare = a.predicate < b.predicate ? -1 : a.predicate > b.predicate ? 1 : 0;
    if (pCompare !== 0) return pCompare;

    // Object value comparison
    const oCompare = a.object.value < b.object.value ? -1 : a.object.value > b.object.value ? 1 : 0;
    if (oCompare !== 0) return oCompare;

    // Graph comparison
    return a.graph < b.graph ? -1 : a.graph > b.graph ? 1 : 0;
  });
}

/**
 * Normalize all strings in object to UTF-8 NFC
 * @param {Object} obj - Object with string values
 * @returns {Object} Normalized object
 */
function normalizeStrings(obj) {
  if (typeof obj === 'string') {
    return obj.normalize('NFC');
  }
  if (Array.isArray(obj)) {
    return obj.map(normalizeStrings);
  }
  if (obj && typeof obj === 'object') {
    const result = {};
    for (const [key, value] of Object.entries(obj)) {
      result[key.normalize('NFC')] = normalizeStrings(value);
    }
    return result;
  }
  return obj;
}
```

**Exports**:
- `canonicalizeCapsule(capsule)` → string
- `canonicalizeQuads(quads)` → Array<Quad>
- `serializeQuad(quad)` → Object (for storage)
- `deserializeQuad(obj)` → Quad (for reconstruction)

---

### 3.3 `hash.mjs` - BLAKE3 Hashing

**Purpose**: Compute deterministic content hash using hash-wasm (BLAKE3).

**Algorithm**:
```javascript
import { blake3 } from 'hash-wasm';
import { canonicalizeCapsule } from './canonicalize.mjs';

/**
 * Compute BLAKE3 hash of capsule
 * @param {Object} capsule - Capsule to hash
 * @returns {Promise<string>} BLAKE3 hex digest
 */
export async function hashCapsule(capsule) {
  // 1. Canonicalize capsule (deterministic serialization)
  const canonical = canonicalizeCapsule(capsule);

  // 2. Compute BLAKE3 hash (via hash-wasm)
  const hash = await blake3(canonical);

  return hash;
}

/**
 * Compute hash with parent chain verification
 * Ensures parent hashes exist and are valid
 *
 * @param {Object} capsule - Capsule to hash
 * @param {Map<string, Object>} parentCapsules - Map of parent hash → capsule
 * @returns {Promise<{ hash: string, parentChain: Array<string> }>}
 */
export async function hashWithParentChain(capsule, parentCapsules = new Map()) {
  // 1. Verify all parent hashes exist
  for (const parentHash of capsule.receipt.parents) {
    if (!parentCapsules.has(parentHash)) {
      throw new Error(`Parent capsule not found: ${parentHash}`);
    }

    // 2. Verify parent hash is correct
    const parentCapsule = parentCapsules.get(parentHash);
    const recomputedHash = await hashCapsule(parentCapsule);
    if (recomputedHash !== parentHash) {
      throw new Error(`Parent hash mismatch: expected ${parentHash}, got ${recomputedHash}`);
    }
  }

  // 3. Compute capsule hash
  const hash = await hashCapsule(capsule);

  // 4. Build full parent chain (recursive)
  const parentChain = [];
  for (const parentHash of capsule.receipt.parents) {
    parentChain.push(parentHash);
    const parent = parentCapsules.get(parentHash);
    if (parent.receipt.parents.length > 0) {
      const { parentChain: grandparents } = await hashWithParentChain(parent, parentCapsules);
      parentChain.push(...grandparents);
    }
  }

  return { hash, parentChain };
}
```

**Exports**:
- `hashCapsule(capsule)` → Promise<string>
- `hashWithParentChain(capsule, parentMap)` → Promise<{ hash, parentChain }>

---

### 3.4 `verify.mjs` - Verification & Tamper Detection

**Purpose**: Verify capsule integrity and detect tampering.

**Algorithm**:
```javascript
import { hashCapsule } from './hash.mjs';
import { CapsuleSchema } from './capsule.mjs';

/**
 * Verify capsule integrity
 * Checks:
 * 1. Schema validation (Zod)
 * 2. Hash correctness (recompute and compare)
 * 3. Parent chain integrity
 * 4. Timestamp validity
 *
 * @param {Object} capsule - Capsule to verify
 * @param {Array<string>} [expectedParents] - Expected parent hashes
 * @returns {Promise<{ ok: boolean, reason?: string }>}
 */
export async function verifyCapsule(capsule, expectedParents = null) {
  try {
    // 1. Schema validation
    CapsuleSchema.parse(capsule);
  } catch (err) {
    return { ok: false, reason: `Schema validation failed: ${err.message}` };
  }

  // 2. Recompute hash
  const recomputedHash = await hashCapsule(capsule);
  if (recomputedHash !== capsule.id) {
    return {
      ok: false,
      reason: `Hash mismatch: expected ${capsule.id}, got ${recomputedHash}`,
    };
  }

  // 3. Verify receipt hash matches capsule ID
  if (capsule.receipt.hash !== capsule.id) {
    return {
      ok: false,
      reason: `Receipt hash mismatch: ${capsule.receipt.hash} !== ${capsule.id}`,
    };
  }

  // 4. Verify parent hashes (if expected parents provided)
  if (expectedParents !== null) {
    const actualParents = capsule.receipt.parents.slice().sort();
    const expectedSorted = expectedParents.slice().sort();

    if (JSON.stringify(actualParents) !== JSON.stringify(expectedSorted)) {
      return {
        ok: false,
        reason: `Parent mismatch: expected [${expectedSorted}], got [${actualParents}]`,
      };
    }
  }

  // 5. Verify timestamp is valid ISO 8601
  try {
    fromISO(capsule.receipt.timestamp);
  } catch (err) {
    return { ok: false, reason: `Invalid timestamp: ${err.message}` };
  }

  // 6. Verify guard limits are reasonable
  const { maxQuads, maxDepth, timeout } = capsule.guard.limits;
  if (maxQuads < 1 || maxDepth < 1 || timeout < 1) {
    return { ok: false, reason: 'Guard limits must be positive integers' };
  }

  return { ok: true };
}

/**
 * Detect tampering by comparing stored vs computed hash
 * @param {Object} capsule - Capsule to check
 * @returns {Promise<boolean>} True if tampered
 */
export async function detectTampering(capsule) {
  const { ok } = await verifyCapsule(capsule);
  return !ok;
}
```

**Exports**:
- `verifyCapsule(capsule, expectedParents?)` → Promise<{ ok, reason? }>
- `detectTampering(capsule)` → Promise<boolean>

---

### 3.5 `planner.mjs` - Intent → Delta Compilation

**Purpose**: Compile high-level intent operations into low-level quad deltas.

**Algorithm**:
```javascript
import { dataFactory, createStore } from '@unrdf/oxigraph';
import { createCapsule } from './capsule.mjs';

/**
 * Plan a capsule by compiling intent into delta
 * Steps:
 * 1. Parse intent operations
 * 2. Compute delta by comparing before/after states
 * 3. Apply guard profile constraints
 * 4. Generate capsule with receipt
 *
 * @param {Object} intent - Intent with ops array
 * @param {Object} store - Current RDF store (for delta computation)
 * @param {Object} profile - Convention profile with guards
 * @param {Array<string>} [parents] - Parent capsule hashes
 * @returns {Promise<Object>} Planned capsule
 */
export async function planCapsule(intent, store, profile, parents = []) {
  // 1. Create temporary store with current state
  const tempStore = createStore();

  // Copy current store state
  for (const quad of store.match(null, null, null, null)) {
    tempStore.add(quad);
  }

  // 2. Apply intent operations to temp store
  const delta = { add: [], del: [] };

  for (const op of intent.ops) {
    switch (op.type) {
      case 'set': {
        // Delete old values for (subject, predicate)
        const oldQuads = [...tempStore.match(
          dataFactory.namedNode(op.subject),
          dataFactory.namedNode(op.predicate),
          null,
          null
        )];

        for (const quad of oldQuads) {
          tempStore.delete(quad);
          delta.del.push(serializeQuad(quad));
        }

        // Add new value
        const newQuad = dataFactory.quad(
          dataFactory.namedNode(op.subject),
          dataFactory.namedNode(op.predicate),
          createObject(op.object),
          dataFactory.defaultGraph()
        );
        tempStore.add(newQuad);
        delta.add.push(serializeQuad(newQuad));
        break;
      }

      case 'create': {
        // Add subject to graph (minimal: rdf:type)
        const quad = dataFactory.quad(
          dataFactory.namedNode(op.subject),
          dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
          dataFactory.namedNode(op.graph),
          dataFactory.defaultGraph()
        );
        tempStore.add(quad);
        delta.add.push(serializeQuad(quad));
        break;
      }

      case 'link': {
        const quad = dataFactory.quad(
          dataFactory.namedNode(op.subject),
          dataFactory.namedNode(op.predicate),
          dataFactory.namedNode(op.target),
          dataFactory.defaultGraph()
        );
        tempStore.add(quad);
        delta.add.push(serializeQuad(quad));
        break;
      }

      case 'unlink': {
        const quad = dataFactory.quad(
          dataFactory.namedNode(op.subject),
          dataFactory.namedNode(op.predicate),
          dataFactory.namedNode(op.target),
          dataFactory.defaultGraph()
        );
        tempStore.delete(quad);
        delta.del.push(serializeQuad(quad));
        break;
      }

      default:
        throw new Error(`Unknown intent operation type: ${op.type}`);
    }
  }

  // 3. Apply guard constraints
  const totalQuads = delta.add.length + delta.del.length;
  if (totalQuads > profile.limits.maxQuads) {
    throw new Error(`Delta exceeds maxQuads: ${totalQuads} > ${profile.limits.maxQuads}`);
  }

  // 4. Create capsule
  return createCapsule(intent, delta, profile, parents);
}

/**
 * Helper: Create RDF object term from value
 */
function createObject(value) {
  if (typeof value === 'string' && value.startsWith('http')) {
    return dataFactory.namedNode(value);
  }
  return dataFactory.literal(String(value));
}

/**
 * Helper: Serialize quad for storage
 */
function serializeQuad(quad) {
  return {
    subject: quad.subject.value,
    subjectType: quad.subject.termType,
    predicate: quad.predicate.value,
    object: {
      value: quad.object.value,
      type: quad.object.termType,
      datatype: quad.object.datatype?.value,
      language: quad.object.language,
    },
    graph: quad.graph.value,
  };
}
```

**Idempotence Guarantee**:
```javascript
/**
 * Ensure idempotence: planCapsule(planCapsule(x)) === planCapsule(x)
 * By computing delta from ORIGINAL store, not modified store
 */
export async function planCapsuleIdempotent(intent, originalStore, profile, parents = []) {
  // Always compute delta relative to original store
  return planCapsule(intent, originalStore, profile, parents);
}
```

**Exports**:
- `planCapsule(intent, store, profile, parents?)` → Promise<Capsule>
- `planCapsuleIdempotent(...)` → Promise<Capsule>

---

## 4. Test Strategy

### 4.1 `capsule.test.mjs` - Basic Creation
```javascript
import { test, expect } from 'vitest';
import { createCapsule } from '../src/capsule.mjs';

test('creates valid capsule with all required fields', () => {
  const intent = { ops: [{ type: 'set', subject: 'ex:s', predicate: 'ex:p', object: 'value' }] };
  const delta = { add: [], del: [] };
  const guard = { limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 }, profiles: [] };

  const capsule = createCapsule(intent, delta, guard);

  expect(capsule).toHaveProperty('id');
  expect(capsule.version).toBe('capsule/v1');
  expect(capsule.receipt.hash).toBe(capsule.id);
});
```

### 4.2 `determinism.test.mjs` - Hash Stability
```javascript
test('produces identical hash for identical capsules', async () => {
  const intent = { ops: [{ type: 'create', subject: 'ex:s1', graph: 'ex:Class' }] };
  const delta = { add: [], del: [] };
  const guard = { limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 }, profiles: [] };

  // Create twice
  const capsule1 = createCapsule(intent, delta, guard);
  const capsule2 = createCapsule(intent, delta, guard);

  // Hashes should differ (different timestamps)
  // BUT: If we freeze timestamp, hashes should match
  capsule2.receipt.timestamp = capsule1.receipt.timestamp;

  const hash1 = await hashCapsule(capsule1);
  const hash2 = await hashCapsule(capsule2);

  expect(hash1).toBe(hash2);
});
```

### 4.3 `planner.test.mjs` - Intent Compilation
```javascript
test('compiles set intent into delta', async () => {
  const store = createStore();
  const intent = { ops: [{ type: 'set', subject: 'ex:s', predicate: 'ex:p', object: 'v1' }] };
  const profile = { limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 }, profiles: [] };

  const capsule = await planCapsule(intent, store, profile);

  expect(capsule.delta.add).toHaveLength(1);
  expect(capsule.delta.del).toHaveLength(0);
});
```

### 4.4 `verification.test.mjs` - Tamper Detection
```javascript
test('detects tampered delta', async () => {
  const capsule = createCapsule(intent, delta, guard);

  // Tamper with delta
  capsule.delta.add.push({ subject: 'ex:evil', predicate: 'ex:p', object: { value: 'tampered' } });

  const result = await verifyCapsule(capsule);

  expect(result.ok).toBe(false);
  expect(result.reason).toContain('Hash mismatch');
});
```

### 4.5 `idempotence.test.mjs` - Idempotence Guarantee
```javascript
test('planCapsule is idempotent', async () => {
  const store = createStore();
  const intent = { ops: [{ type: 'set', subject: 'ex:s', predicate: 'ex:p', object: 'v' }] };
  const profile = { limits: { maxQuads: 100, maxDepth: 5, timeout: 5000 }, profiles: [] };

  const capsule1 = await planCapsule(intent, store, profile);

  // Plan again with ORIGINAL store (not modified)
  const capsule2 = await planCapsule(intent, store, profile);

  // Deltas should be identical (different timestamps, but same operations)
  expect(capsule1.delta.add).toEqual(capsule2.delta.add);
  expect(capsule1.delta.del).toEqual(capsule2.del);
});
```

---

## 5. Dependencies & Imports

### External Dependencies (from monorepo)
```javascript
// From @unrdf/oxigraph
import { createStore, dataFactory } from '@unrdf/oxigraph';

// From @unrdf/kgc-4d
import { now, toISO, fromISO } from '@unrdf/kgc-4d';

// From root dependencies
import { blake3 } from 'hash-wasm';
import { z } from 'zod';
```

### Internal Dependencies (within agent-2)
```javascript
// capsule.mjs imports
import { hashCapsule } from './hash.mjs';
import { canonicalizeCapsule, canonicalizeQuads } from './canonicalize.mjs';

// planner.mjs imports
import { createCapsule, serializeQuad } from './capsule.mjs';

// verify.mjs imports
import { CapsuleSchema } from './capsule.mjs';
import { hashCapsule } from './hash.mjs';
```

---

## 6. Public API (`./src/index.mjs`)

```javascript
// Capsule creation
export { createCapsule, CapsuleSchema, validateCapsule } from './capsule.mjs';

// Planning
export { planCapsule, planCapsuleIdempotent } from './planner.mjs';

// Hashing
export { hashCapsule, hashWithParentChain } from './hash.mjs';

// Canonicalization
export { canonicalizeCapsule, canonicalizeQuads, serializeQuad, deserializeQuad } from './canonicalize.mjs';

// Verification
export { verifyCapsule, detectTampering } from './verify.mjs';
```

---

## 7. Determinism Guarantees

### Sources of Non-Determinism (and Fixes)
1. **Timestamps**: Use `now()` from @unrdf/kgc-4d (nanosecond precision, monotonic)
2. **Quad Order**: Canonical RDF ordering (S-P-O comparison, no localeCompare)
3. **JSON Key Order**: Sorted keys in JSON.stringify
4. **Parent Order**: Lexicographic sort of parent hashes
5. **Unicode**: UTF-8 NFC normalization

### Verification Protocol
```bash
# Run tests twice, compare hashes
npm test -- capsule.test.mjs > run1.txt
npm test -- capsule.test.mjs > run2.txt
diff run1.txt run2.txt  # Should be identical (except timestamps)
```

---

## 8. Success Criteria

### Functional Requirements
- [ ] All Zod schemas pass validation
- [ ] `createCapsule()` generates valid capsules
- [ ] `planCapsule()` compiles intent → delta
- [ ] `hashCapsule()` produces BLAKE3 hashes
- [ ] `verifyCapsule()` detects tampering
- [ ] All 5 test suites pass (100% coverage)

### Non-Functional Requirements
- [ ] Deterministic hashing (same input → same hash)
- [ ] Idempotence (planCapsule is idempotent)
- [ ] Parent chain integrity (recursive verification)
- [ ] No npm dependencies beyond monorepo packages
- [ ] JSDoc coverage 100%
- [ ] Exports from `./src/index.mjs` work correctly

### Integration Requirements
- [ ] Imports from @unrdf/oxigraph work
- [ ] Imports from @unrdf/kgc-4d work
- [ ] hash-wasm BLAKE3 integration works
- [ ] Zod validation works
- [ ] Can be imported by Agent 1 (orchestrator)

---

## 9. Implementation Order

1. **Phase 1**: Core data structures
   - `capsule.mjs` (Zod schemas + createCapsule)
   - `capsule.test.mjs` (basic validation)

2. **Phase 2**: Canonicalization
   - `canonicalize.mjs` (quad sorting + JSON)
   - `determinism.test.mjs` (hash stability)

3. **Phase 3**: Hashing
   - `hash.mjs` (BLAKE3 integration)
   - Update `capsule.mjs` to use hashing

4. **Phase 4**: Planning
   - `planner.mjs` (intent compilation)
   - `planner.test.mjs` + `idempotence.test.mjs`

5. **Phase 5**: Verification
   - `verify.mjs` (tamper detection)
   - `verification.test.mjs`

6. **Phase 6**: Integration
   - `index.mjs` (public API)
   - Integration tests with Agent 1

---

## 10. Edge Cases & Error Handling

### Edge Cases to Handle
1. **Empty Intent**: `{ ops: [] }` → Valid capsule with empty delta
2. **Empty Delta**: No changes → Valid capsule (idempotent)
3. **Circular Parents**: Detect and reject
4. **Malformed Quads**: Zod validation catches
5. **Oversized Delta**: Guard limits enforce
6. **Invalid Timestamps**: `fromISO()` validates

### Error Messages
```javascript
// Guard violation
"Delta exceeds maxQuads: 150 > 100"

// Tamper detection
"Hash mismatch: expected abc123, got def456"

// Parent verification
"Parent capsule not found: xyz789"

// Schema validation
"Schema validation failed: intent.ops must be array"
```

---

## Summary

This plan implements a **deterministic, content-addressed Capsule IR** using:
- **Zod** for schema validation
- **BLAKE3** (via hash-wasm) for content hashing
- **@unrdf/kgc-4d** for nanosecond timestamps and receipts
- **@unrdf/oxigraph** for RDF store operations
- **Canonical ordering** for deterministic serialization

The result is a **tamper-evident, idempotent change program format** suitable for distributed RDF systems.

**Key Innovation**: Like Git commits, Capsules form immutable chains. Like Bitcoin transactions, they're content-addressed and verifiable. Unlike both, they carry semantic RDF operations with guard rails.

**Next Steps**: Implement Phase 1 (core data structures) and validate with tests before proceeding to canonicalization and hashing.
