# Tutorial: Your First Scene

**Objective:** Learn how to create a universe, propose a delta operation, and verify the receipt.

**Time:** 10 minutes  
**Prerequisites:** 
- Node.js 18+ installed
- Basic understanding of RDF triples (subject, predicate, object)
- UNRDF packages installed (`npm install`)

---

## What You'll Build

A simple narrative state chain that:
1. Creates a **Universe (Σ)** — an RDF knowledge graph
2. Proposes a **Scene (Δ)** — a delta operation to add data
3. Applies via **Guards (H)** — admissibility check
4. Executes **Reconciliation (μ)** — merge the change
5. Receives a **Receipt (R)** — cryptographic proof

---

## Step 1: Import Core Modules

```javascript
/**
 * Tutorial 01: Your First Scene
 * 
 * @example
 * node docs/narrative-state-chain/tutorial/01-hello-world.mjs
 */

import { createStore } from '@unrdf/core';
import { DeltaGate, createDelta } from '@unrdf/v6-core/delta';

console.log('=== Tutorial 01: Your First Scene ===\n');
```

**What's happening:**
- `createStore()` — Factory for creating a Universe (KGCStore)
- `DeltaGate` — Guards enforcement (admissibility policies)
- `createDelta()` — Helper to construct Delta operations

---

## Step 2: Create the Universe (Σ)

```javascript
// Create an empty universe (hash-addressed RDF store)
const store = createStore();
console.log('✅ Created Universe (Σ)\n');

// Verify initial state
console.log(`Initial size: ${store.size} triples`);
// Expected: 0 (empty universe)
```

**Evidence:** [packages/kgc-4d/src/store.mjs](/home/user/unrdf/packages/kgc-4d/src/store.mjs)

**Key Properties:**
- Hash-addressed identity (every state has a BLAKE3 hash)
- Append-only event log
- Time-travel capability via snapshots

---

## Step 3: Create the Guard (H)

```javascript
// Create a delta gate with no policies (lawful by default)
const gate = new DeltaGate({
  strict: false, // Allow operations without explicit policies
});
console.log('✅ Created Delta Gate (H)\n');
```

**What's a Guard?**
Guards are admissibility policies that answer: *"Is this delta lawful?"*

**Default behavior:**
- No policies → All deltas are lawful
- Strict mode → Requires explicit policies

**Evidence:** [packages/v6-core/src/delta/gate.mjs](/home/user/unrdf/packages/v6-core/src/delta/gate.mjs):32

---

## Step 4: Propose a Scene (Δ)

```javascript
// Create a delta: Add a triple about Alice
const delta = createDelta(
  'add',                          // Operation type
  'http://example.org/Alice',     // Subject
  'http://example.org/name',      // Predicate
  'Alice Smith',                  // Object
  {
    package: '@tutorial',         // Source package
    actor: 'tutorial-user'        // Actor (optional)
  }
);

console.log('✅ Created Delta (Δ)');
console.log('Delta ID:', delta.id);
console.log('Operations:', delta.operations.length);
console.log();
```

**Delta Structure:**
```typescript
{
  id: string;              // UUID
  timestamp_iso: string;   // ISO 8601
  t_ns: bigint;           // Nanosecond timestamp
  operations: [{
    op: 'add' | 'delete' | 'update';
    subject: string;
    predicate: string;
    object: string;
    graph?: string;
  }];
  source: {
    package: string;
    actor?: string;
  };
}
```

**Evidence:** [packages/v6-core/src/delta/schema.mjs](/home/user/unrdf/packages/v6-core/src/delta/schema.mjs):110

---

## Step 5: Apply via Gate (H + μ)

```javascript
// Propose delta to the gate
// Flow: Δ → H (admissibility) → μ(O ⊔ Δ) (reconcile) → R (receipt)
const receipt = await gate.proposeDelta(delta, store);

console.log('✅ Delta Applied');
console.log('Applied:', receipt.applied);
console.log('State Hash:', receipt.stateHash);
console.log('Operations Applied:', receipt.operationsApplied);
console.log();
```

**What happened internally:**

1. **Schema Validation:** Zod validates delta structure
2. **Admissibility Check (H):** Gate checks policies (none in this case → lawful)
3. **Reconciliation (μ):** `reconcile(store, delta)` merges change
4. **Receipt Generation (R):** Cryptographic proof with state hash

**Evidence:** [packages/v6-core/src/delta/gate.mjs](/home/user/unrdf/packages/v6-core/src/delta/gate.mjs):67

---

## Step 6: Verify the Receipt (R)

```javascript
// Verify receipt properties
console.assert(receipt.applied === true, 'Delta must be applied');
console.assert(receipt.stateHash, 'Must have state hash');
console.assert(receipt.operationsApplied === 1, 'Must apply 1 operation');
console.assert(receipt.deltaId === delta.id, 'Receipt must reference delta ID');

console.log('✅ Receipt Verified\n');
```

**Receipt Schema:**
```typescript
{
  deltaId: string;         // UUID reference to delta
  applied: boolean;        // Success flag
  timestamp_ns: bigint;    // Processing timestamp
  stateHash: string;       // BLAKE3 hash of new state
  operationsApplied: number; // Count of ops
  reason?: string;         // Denial reason (if applied=false)
}
```

**Evidence:** [packages/v6-core/src/delta/schema.mjs](/home/user/unrdf/packages/v6-core/src/delta/schema.mjs):137

---

## Step 7: Query the Universe

```javascript
// Verify the triple was added
const aliceTriples = store.getQuads(
  store.namedNode('http://example.org/Alice'),
  null,
  null
);

console.log('--- Alice\'s Data ---');
for (const quad of aliceTriples) {
  console.log(`${quad.predicate.value}: ${quad.object.value}`);
}

console.log('\n--- Final State ---');
console.log(`Total triples: ${store.size}`);
console.log('Expected: 1 (the triple we added)');
```

**Verification:**
```bash
node docs/narrative-state-chain/tutorial/01-hello-world.mjs
```

**Expected Output:**
```
=== Tutorial 01: Your First Scene ===

✅ Created Universe (Σ)
✅ Created Delta Gate (H)
✅ Created Delta (Δ)
✅ Delta Applied
Applied: true
State Hash: blake3_hash_here
Operations Applied: 1
✅ Receipt Verified

--- Alice's Data ---
http://example.org/name: Alice Smith

--- Final State ---
Total triples: 1
```

---

## Complete Code

```javascript
/**
 * Tutorial 01: Your First Scene - Complete
 */
import { createStore } from '@unrdf/core';
import { DeltaGate, createDelta } from '@unrdf/v6-core/delta';

async function main() {
  // 1. Create universe
  const store = createStore();
  
  // 2. Create gate
  const gate = new DeltaGate({ strict: false });
  
  // 3. Propose delta
  const delta = createDelta(
    'add',
    'http://example.org/Alice',
    'http://example.org/name',
    'Alice Smith',
    { package: '@tutorial', actor: 'tutorial-user' }
  );
  
  // 4. Apply via gate
  const receipt = await gate.proposeDelta(delta, store);
  
  // 5. Verify receipt
  console.assert(receipt.applied, 'Delta applied');
  console.assert(receipt.stateHash, 'Has state hash');
  
  // 6. Query result
  console.log('Final size:', store.size);
  console.log('State hash:', receipt.stateHash);
}

main().catch(console.error);
```

---

## Key Takeaways

1. **All mutations flow through Δ:** No direct `store.add()` — always propose a delta
2. **Every Δ produces R:** Success or denial, always get a receipt
3. **Hash-addressed state:** `receipt.stateHash` is cryptographic commitment
4. **Atomic operations:** All-or-none (no partial applications)

---

## What NOT to Do

### Anti-Pattern 1: Direct Mutations
```javascript
// ❌ WRONG: Bypasses guards and receipts
store.addQuad(subject, predicate, object);
```

```javascript
// ✅ CORRECT: Propose via delta
const delta = createDelta('add', subject, predicate, object, { package: '@my-app' });
const receipt = await gate.proposeDelta(delta, store);
```

### Anti-Pattern 2: Ignoring Receipts
```javascript
// ❌ WRONG: Assuming success
await gate.proposeDelta(delta, store);
// What if it failed?
```

```javascript
// ✅ CORRECT: Check receipt
const receipt = await gate.proposeDelta(delta, store);
if (!receipt.applied) {
  throw new Error(`Delta rejected: ${receipt.reason}`);
}
```

### Anti-Pattern 3: Mutating Delta After Creation
```javascript
// ❌ WRONG: Delta is immutable
const delta = createDelta(...);
delta.operations.push({ op: 'add', ... }); // Breaks hash integrity
```

```javascript
// ✅ CORRECT: Create new delta
const delta = createDelta('add', ...);
const delta2 = createDelta('add', ...); // Separate delta
```

---

## Next Steps

1. **[Tutorial 02: Freeze & Time Travel](02-freeze-and-replay.md)** — Snapshot state and reconstruct history
2. **[How-To: Enforce Guards](../how-to/enforce-guards.md)** — Add admissibility policies
3. **[Reference: Scene API](../reference/api-scene.md)** — Full delta operation reference

---

## Evidence & Proofs

**Code:**
- Universe: [/home/user/unrdf/packages/kgc-4d/src/store.mjs](/home/user/unrdf/packages/kgc-4d/src/store.mjs)
- Delta: [/home/user/unrdf/packages/v6-core/src/delta/schema.mjs](/home/user/unrdf/packages/v6-core/src/delta/schema.mjs)
- Gate: [/home/user/unrdf/packages/v6-core/src/delta/gate.mjs](/home/user/unrdf/packages/v6-core/src/delta/gate.mjs)

**Tests:**
- Gate tests: [/home/user/unrdf/packages/v6-core/test/delta-gate.test.mjs](/home/user/unrdf/packages/v6-core/test/delta-gate.test.mjs)
- Reconciliation tests: [/home/user/unrdf/packages/v6-core/test/delta-reconcile.test.mjs](/home/user/unrdf/packages/v6-core/test/delta-reconcile.test.mjs)

**Verification:**
```bash
# Run delta contract tests
npm test -- packages/v6-core/src/delta
```
