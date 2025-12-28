# Pattern 2: Delta Contract Pattern

**Version**: v6.0.0
**Maturity Target**: L5 (Stable contracts, deterministic, adversarial-safe, composable)
**Copy-Exact Template**: Yes - Use AS-IS for all P0+P1 packages

---

## Overview

**Problem**: Version transitions need explicit proposals showing BEFORE → DELTA → AFTER with integrity proofs.

**Solution**: Delta Contract = Explicit change carrier with Merkle proof of integrity. All mutations flow through Δ.

**Invariant**: Δ is the ONLY way to mutate state. No direct mutations allowed.

**Formula**: `μ(O ⊔ Δ) → O'` (Reconcile ontology O with delta Δ to produce new state O')

---

## Core Concepts

### 1. Delta = Explicit Change Proposal

A Delta is a **change proposal**, not a mutation. It contains:

```
Delta = {
  id: UUID,
  timestamp: ISO + nanoseconds,
  operations: [DeltaOperation], // What to change
  source: { package, actor, context }, // Who/what proposes
  admissibility: { policyId, constraints, preConditions } // Policy metadata
}
```

### 2. Three Delta Operations

```javascript
// Add: Insert new triple
{ op: 'add', subject: 's', predicate: 'p', object: 'o', graph: 'g' }

// Delete: Remove existing triple
{ op: 'delete', subject: 's', predicate: 'p', object: 'o', graph: 'g' }

// Update: Atomic replace (delete old + add new)
{ op: 'update', subject: 's', predicate: 'p', oldObject: 'old', newObject: 'new', graph: 'g' }
```

### 3. Delta → Receipt Flow

```
Propose Δ → Admissibility Check → Apply μ(O ⊔ Δ) → Receipt (success/deny)
```

Every delta produces a receipt (success or denial). No partial applications.

---

## Copy-Exact Templates

### 1. Delta Schema (Zod)

```javascript
import { z } from 'zod';

/**
 * Delta Operation Schema - Individual RDF operations
 */
export const DeltaOperationSchema = z.discriminatedUnion('op', [
  z.object({
    op: z.literal('add'),
    subject: z.string().min(1),
    predicate: z.string().min(1),
    object: z.string().min(1),
    graph: z.string().optional(),
  }),
  z.object({
    op: z.literal('delete'),
    subject: z.string().min(1),
    predicate: z.string().min(1),
    object: z.string().min(1),
    graph: z.string().optional(),
  }),
  z.object({
    op: z.literal('update'),
    subject: z.string().min(1),
    predicate: z.string().min(1),
    oldObject: z.string().min(1),
    newObject: z.string().min(1),
    graph: z.string().optional(),
  }),
]);

/**
 * Delta Source Schema
 */
export const DeltaSourceSchema = z.object({
  package: z.string().min(1), // E.g., '@unrdf/yawl'
  actor: z.string().optional(), // E.g., 'workflow-engine'
  context: z.any().optional(), // Domain-specific context
});

/**
 * Delta Admissibility Schema
 */
export const DeltaAdmissibilitySchema = z.object({
  policyId: z.string().optional(), // Policy identifier
  constraints: z.array(z.string()).optional(), // Constraint IDs
  preConditions: z.array(z.string()).optional(), // Pre-condition IDs
}).optional();

/**
 * Delta Schema - The ONLY legal change carrier in v6
 */
export const DeltaSchema = z.object({
  id: z.string().uuid(),
  timestamp_iso: z.string().datetime(),
  t_ns: z.bigint().nonnegative(),
  operations: z.array(DeltaOperationSchema).min(1),
  source: DeltaSourceSchema,
  admissibility: DeltaAdmissibilitySchema,
});

/**
 * Delta Receipt Schema - Result of delta application
 */
export const DeltaReceiptSchema = z.object({
  deltaId: z.string().uuid(),
  applied: z.boolean(),
  timestamp_ns: z.bigint().nonnegative(),
  stateHash: z.string().min(1).optional(), // Post-application hash (if applied)
  reason: z.string().optional(), // Denial reason (if not applied)
  operationsApplied: z.number().int().nonnegative().optional(),
  errors: z.array(z.string()).optional(),
});

/**
 * Validate Delta
 * @param {any} data
 * @returns {Delta}
 */
export function validateDelta(data) {
  return DeltaSchema.parse(data);
}

/**
 * Validate Delta Receipt
 * @param {any} data
 * @returns {DeltaReceipt}
 */
export function validateDeltaReceipt(data) {
  return DeltaReceiptSchema.parse(data);
}
```

### 2. Delta Builder (Factory)

```javascript
/**
 * Create Delta - Factory for building deltas
 *
 * @param {Object} options
 * @param {Array<DeltaOperation>} options.operations - Operations to apply
 * @param {string} options.package - Source package (e.g., '@unrdf/yawl')
 * @param {string} [options.actor] - Actor performing operation
 * @param {any} [options.context] - Domain-specific context
 * @param {string} [options.policyId] - Admissibility policy ID
 * @param {Array<string>} [options.constraints] - Constraint IDs
 * @param {Array<string>} [options.preConditions] - Pre-condition IDs
 * @returns {Delta} Validated delta
 *
 * @example
 * const delta = createDelta({
 *   operations: [
 *     { op: 'add', subject: 'http://ex.org/Alice', predicate: 'http://ex.org/age', object: '30' }
 *   ],
 *   package: '@unrdf/my-package',
 *   actor: 'system',
 *   policyId: 'require-approval',
 * });
 */
export function createDelta(options) {
  const {
    operations,
    package: pkg,
    actor,
    context,
    policyId,
    constraints,
    preConditions,
  } = options;

  const delta = {
    id: crypto.randomUUID(),
    timestamp_iso: new Date().toISOString(),
    t_ns: BigInt(Date.now()) * 1_000_000n, // Milliseconds to nanoseconds
    operations,
    source: {
      package: pkg,
      actor,
      context,
    },
    admissibility: policyId || constraints || preConditions
      ? { policyId, constraints, preConditions }
      : undefined,
  };

  return validateDelta(delta);
}
```

### 3. Delta Gate (Admissibility Enforcer)

```javascript
/**
 * Delta Gate - Admissibility enforcer
 *
 * Guards the boundary between proposed changes and applied changes.
 * Ensures only lawful deltas mutate state.
 *
 * @class
 *
 * @example
 * const gate = new DeltaGate({ policies: myPolicies });
 * const receipt = await gate.proposeDelta(delta, store);
 */
export class DeltaGate {
  /**
   * @param {Object} [options]
   * @param {Object} [options.policies] - Policy enforcement rules
   * @param {boolean} [options.strict] - Strict mode (default: true)
   * @param {Function} [options.conflictResolver] - Conflict resolution strategy
   */
  constructor(options = {}) {
    this.policies = options.policies || {};
    this.strict = options.strict !== false;
    this.conflictResolver = options.conflictResolver || this._defaultConflictResolver;
  }

  /**
   * Propose delta for application
   *
   * @param {Delta} delta - Delta to propose
   * @param {Object} store - Knowledge store instance
   * @returns {Promise<DeltaReceipt>}
   *
   * @example
   * const receipt = await gate.proposeDelta(delta, store);
   * if (receipt.applied) {
   *   console.log('Applied:', receipt.stateHash);
   * } else {
   *   console.error('Rejected:', receipt.reason);
   * }
   */
  async proposeDelta(delta, store) {
    try {
      // 1. Validate delta schema
      const validatedDelta = validateDelta(delta);

      // 2. Check admissibility
      const admissibilityCheck = await this._checkAdmissibility(validatedDelta, store);

      if (!admissibilityCheck.lawful) {
        return this.rejectDelta(validatedDelta, admissibilityCheck.reason);
      }

      // 3. Apply delta atomically
      return await this.applyDelta(validatedDelta, store);
    } catch (error) {
      return this.rejectDelta(delta, `Validation failed: ${error.message}`);
    }
  }

  /**
   * Apply delta atomically
   *
   * @param {Delta} delta - Validated delta
   * @param {Object} store - Knowledge store instance
   * @returns {Promise<DeltaReceipt>}
   */
  async applyDelta(delta, store) {
    try {
      // Reconcile: μ(O ⊔ Δ)
      const reconcileResult = await reconcile(store, delta, this.conflictResolver);

      if (!reconcileResult.applied) {
        return this.rejectDelta(delta, reconcileResult.reason || 'Reconciliation failed');
      }

      // Success receipt
      const receipt = {
        deltaId: delta.id,
        applied: true,
        timestamp_ns: BigInt(Date.now()) * 1_000_000n,
        stateHash: reconcileResult.stateHash,
        operationsApplied: delta.operations.length,
      };

      return validateDeltaReceipt(receipt);
    } catch (error) {
      return this.rejectDelta(delta, `Application failed: ${error.message}`);
    }
  }

  /**
   * Reject delta with denial receipt
   *
   * @param {Delta} delta
   * @param {string} reason
   * @returns {DeltaReceipt}
   */
  rejectDelta(delta, reason) {
    const receipt = {
      deltaId: delta.id || 'unknown',
      applied: false,
      timestamp_ns: BigInt(Date.now()) * 1_000_000n,
      reason,
      operationsApplied: 0,
    };

    return validateDeltaReceipt(receipt);
  }

  /**
   * Check admissibility policies
   * @private
   */
  async _checkAdmissibility(delta, store) {
    if (!delta.admissibility) {
      return { lawful: true };
    }

    const { policyId, constraints, preConditions } = delta.admissibility;

    // Check policy
    if (policyId && this.policies[policyId]) {
      const policy = this.policies[policyId];
      const result = await policy(delta, store);

      if (!result.allowed) {
        return { lawful: false, reason: `Policy ${policyId} denied: ${result.reason}` };
      }
    }

    // Check constraints
    if (constraints) {
      for (const constraintId of constraints) {
        const constraint = this.policies[constraintId];
        if (!constraint) {
          return { lawful: false, reason: `Unknown constraint: ${constraintId}` };
        }

        const result = await constraint(delta, store);
        if (!result.allowed) {
          return { lawful: false, reason: `Constraint ${constraintId} violated: ${result.reason}` };
        }
      }
    }

    // Check pre-conditions
    if (preConditions) {
      for (const preConditionId of preConditions) {
        const preCondition = this.policies[preConditionId];
        if (!preCondition) {
          return { lawful: false, reason: `Unknown pre-condition: ${preConditionId}` };
        }

        const result = await preCondition(delta, store);
        if (!result.satisfied) {
          return { lawful: false, reason: `Pre-condition ${preConditionId} not satisfied: ${result.reason}` };
        }
      }
    }

    return { lawful: true };
  }

  /**
   * Default conflict resolver: delta-wins
   * @private
   */
  _defaultConflictResolver(conflict) {
    return 'delta-wins';
  }

  /**
   * Add policy
   *
   * @param {string} id - Policy identifier
   * @param {Function} policy - Policy function
   *
   * @example
   * gate.addPolicy('no-deletes', async (delta, store) => {
   *   const hasDeletes = delta.operations.some(op => op.op === 'delete');
   *   return {
   *     allowed: !hasDeletes,
   *     reason: hasDeletes ? 'Delete operations not allowed' : undefined
   *   };
   * });
   */
  addPolicy(id, policy) {
    if (typeof policy !== 'function') {
      throw new TypeError('Policy must be a function');
    }
    this.policies[id] = policy;
  }

  /**
   * Remove policy
   * @param {string} id
   * @returns {boolean}
   */
  removePolicy(id) {
    if (this.policies[id]) {
      delete this.policies[id];
      return true;
    }
    return false;
  }

  /**
   * List policies
   * @returns {string[]}
   */
  listPolicies() {
    return Object.keys(this.policies);
  }
}
```

### 4. Reconcile Function (μ(O ⊔ Δ))

```javascript
/**
 * Reconcile - Apply delta to store
 *
 * Implements μ(O ⊔ Δ): Reconcile ontology O with delta Δ.
 *
 * @param {Object} store - Knowledge store (RDF store)
 * @param {Delta} delta - Validated delta
 * @param {Function} conflictResolver - Conflict resolution strategy
 * @returns {Promise<{applied: boolean, stateHash?: string, reason?: string}>}
 */
export async function reconcile(store, delta, conflictResolver) {
  try {
    // Begin transaction (all-or-none atomicity)
    const transaction = store.createTransaction();

    for (const operation of delta.operations) {
      if (operation.op === 'add') {
        // Add new triple
        transaction.add({
          subject: operation.subject,
          predicate: operation.predicate,
          object: operation.object,
          graph: operation.graph || 'http://kgc.io/Universe',
        });
      } else if (operation.op === 'delete') {
        // Remove existing triple
        transaction.delete({
          subject: operation.subject,
          predicate: operation.predicate,
          object: operation.object,
          graph: operation.graph || 'http://kgc.io/Universe',
        });
      } else if (operation.op === 'update') {
        // Atomic update: delete old + add new
        transaction.delete({
          subject: operation.subject,
          predicate: operation.predicate,
          object: operation.oldObject,
          graph: operation.graph || 'http://kgc.io/Universe',
        });
        transaction.add({
          subject: operation.subject,
          predicate: operation.predicate,
          object: operation.newObject,
          graph: operation.graph || 'http://kgc.io/Universe',
        });
      }
    }

    // Commit transaction
    await transaction.commit();

    // Compute state hash (Merkle root of all triples)
    const stateHash = await computeStoreHash(store);

    return {
      applied: true,
      stateHash,
    };
  } catch (error) {
    return {
      applied: false,
      reason: `Reconciliation failed: ${error.message}`,
    };
  }
}

/**
 * Compute store state hash (Merkle root)
 *
 * @param {Object} store
 * @returns {Promise<string>}
 */
async function computeStoreHash(store) {
  const quads = store.getQuads(null, null, null, null);
  const serialized = deterministicSerialize(quads);
  return await computeBlake3(serialized);
}
```

---

## Delta Chaining (Composability)

**Question**: Can deltas chain?

**Answer**: YES. Deltas compose via state hashing.

```javascript
// Delta 1: Add Alice's age
const delta1 = createDelta({
  operations: [
    { op: 'add', subject: 'http://ex.org/Alice', predicate: 'http://ex.org/age', object: '30' }
  ],
  package: '@unrdf/my-package',
});

const receipt1 = await gate.proposeDelta(delta1, store);
console.assert(receipt1.applied);
const stateHash1 = receipt1.stateHash;

// Delta 2: Update Alice's age (chains from delta 1)
const delta2 = createDelta({
  operations: [
    { op: 'update', subject: 'http://ex.org/Alice', predicate: 'http://ex.org/age', oldObject: '30', newObject: '31' }
  ],
  package: '@unrdf/my-package',
});

const receipt2 = await gate.proposeDelta(delta2, store);
console.assert(receipt2.applied);
const stateHash2 = receipt2.stateHash;

// Proof: State hashes form chain
console.assert(stateHash1 !== stateHash2);

// Merkle proof: Can verify state transition
const proof = {
  before: stateHash1,
  delta: delta2,
  after: stateHash2,
};
```

---

## Merkle Proof of Integrity

### 1. Single Delta Proof

```javascript
/**
 * Prove Delta Integrity
 *
 * @param {Delta} delta
 * @param {string} beforeHash - State hash before delta
 * @param {string} afterHash - State hash after delta
 * @returns {Object} Merkle proof
 */
export function proveDeltaIntegrity(delta, beforeHash, afterHash) {
  return {
    deltaId: delta.id,
    operations: delta.operations.map(op => ({
      op: op.op,
      subject: op.subject,
      predicate: op.predicate,
      object: op.object || op.newObject,
    })),
    beforeStateHash: beforeHash,
    afterStateHash: afterHash,
    timestamp: delta.timestamp_iso,
    source: delta.source,
  };
}
```

### 2. Delta Chain Proof

```javascript
/**
 * Prove Delta Chain Integrity
 *
 * @param {Array<{delta: Delta, beforeHash: string, afterHash: string}>} chain
 * @returns {Object} Chain proof
 */
export function proveDeltaChain(chain) {
  const proofs = chain.map(({delta, beforeHash, afterHash}) =>
    proveDeltaIntegrity(delta, beforeHash, afterHash)
  );

  // Verify chain: afterHash[i] === beforeHash[i+1]
  for (let i = 0; i < proofs.length - 1; i++) {
    if (proofs[i].afterStateHash !== proofs[i + 1].beforeStateHash) {
      throw new Error(`Chain broken at index ${i}: ${proofs[i].afterStateHash} !== ${proofs[i + 1].beforeStateHash}`);
    }
  }

  return {
    chain: proofs,
    genesisHash: proofs[0].beforeStateHash,
    finalHash: proofs[proofs.length - 1].afterStateHash,
    deltaCount: proofs.length,
  };
}
```

---

## Common Pitfalls

### Pitfall 1: Direct State Mutation

```javascript
// ❌ WRONG - Direct mutation bypasses delta system
store.add({ subject: 's', predicate: 'p', object: 'o' }); // NO DELTA!

// ✅ CORRECT - Mutation via delta
const delta = createDelta({
  operations: [{ op: 'add', subject: 's', predicate: 'p', object: 'o' }],
  package: '@unrdf/my-package',
});
await gate.proposeDelta(delta, store);
```

### Pitfall 2: Partial Application

```javascript
// ❌ WRONG - Partial application (some operations succeed, some fail)
for (const operation of delta.operations) {
  try {
    store.add(operation); // If one fails, others already applied!
  } catch (error) {
    console.error('Failed:', error);
  }
}

// ✅ CORRECT - All-or-none transaction
const transaction = store.createTransaction();
for (const operation of delta.operations) {
  transaction.add(operation);
}
await transaction.commit(); // Atomic: all succeed or all fail
```

### Pitfall 3: Forgetting Admissibility

```javascript
// ❌ WRONG - No admissibility check
const delta = createDelta({ operations: [...] });
await gate.applyDelta(delta, store); // Bypasses admissibility check!

// ✅ CORRECT - Propose delta (admissibility checked)
const receipt = await gate.proposeDelta(delta, store);
```

### Pitfall 4: Non-Deterministic Delta IDs

```javascript
// ❌ WRONG - Random UUID (non-deterministic)
const delta = {
  id: crypto.randomUUID(), // ❌ Random!
  // ...
};

// ✅ CORRECT - Deterministic ID from content hash
const deltaContent = { operations, source, timestamp_iso };
const contentHash = await computeBlake3(deltaContent);
const delta = {
  id: contentHash.slice(0, 36), // First 36 chars = deterministic ID
  // ...
};
```

---

## Testing Template

```javascript
import { describe, it, expect } from 'vitest';
import { DeltaGate, createDelta } from './delta-gate.mjs';

describe('Delta Contract Pattern', () => {
  it('applies delta successfully', async () => {
    const store = createTestStore();
    const gate = new DeltaGate();

    const delta = createDelta({
      operations: [
        { op: 'add', subject: 'http://ex.org/Alice', predicate: 'http://ex.org/age', object: '30' }
      ],
      package: '@unrdf/test',
    });

    const receipt = await gate.proposeDelta(delta, store);

    expect(receipt.applied).toBe(true);
    expect(receipt.stateHash).toBeDefined();
    expect(receipt.operationsApplied).toBe(1);
  });

  it('rejects delta with policy violation', async () => {
    const store = createTestStore();
    const gate = new DeltaGate();

    gate.addPolicy('no-deletes', async (delta) => {
      const hasDeletes = delta.operations.some(op => op.op === 'delete');
      return { allowed: !hasDeletes, reason: hasDeletes ? 'Deletes not allowed' : undefined };
    });

    const delta = createDelta({
      operations: [
        { op: 'delete', subject: 'http://ex.org/Alice', predicate: 'http://ex.org/age', object: '30' }
      ],
      package: '@unrdf/test',
      policyId: 'no-deletes',
    });

    const receipt = await gate.proposeDelta(delta, store);

    expect(receipt.applied).toBe(false);
    expect(receipt.reason).toContain('Deletes not allowed');
  });

  it('chains deltas correctly', async () => {
    const store = createTestStore();
    const gate = new DeltaGate();

    const delta1 = createDelta({
      operations: [{ op: 'add', subject: 's', predicate: 'p', object: 'v1' }],
      package: '@unrdf/test',
    });

    const receipt1 = await gate.proposeDelta(delta1, store);
    const stateHash1 = receipt1.stateHash;

    const delta2 = createDelta({
      operations: [{ op: 'update', subject: 's', predicate: 'p', oldObject: 'v1', newObject: 'v2' }],
      package: '@unrdf/test',
    });

    const receipt2 = await gate.proposeDelta(delta2, store);
    const stateHash2 = receipt2.stateHash;

    expect(stateHash1).not.toBe(stateHash2);
  });
});
```

---

## Decision Matrix: When to Use Delta Pattern

| Scenario | Use Delta? | Rationale |
|----------|------------|-----------|
| State mutation (add/delete/update triples) | ✅ YES | MUST use delta (only legal mutation path) |
| Batch operations | ✅ YES | Single delta with multiple operations |
| Read-only query | ❌ NO | No mutation, no delta needed |
| External API sync | ✅ YES | API changes → deltas → store |
| Version control | ✅ YES | Each commit → delta with Merkle proof |

---

## Copy-Paste Checklist

- [ ] Copy `delta-schema.mjs` with Zod schemas
- [ ] Copy `delta-gate.mjs` with DeltaGate class
- [ ] Copy `reconcile.mjs` with reconciliation logic
- [ ] Copy `delta-gate.test.mjs` tests
- [ ] Configure policies for your domain
- [ ] Ban direct store mutations (ESLint rule)
- [ ] Document admissibility policies in README

---

## References

- Existing Implementation: `/home/user/unrdf/packages/v6-core/src/delta/`
- Delta Schema: `/home/user/unrdf/packages/v6-core/src/delta/schema.mjs`
- Delta Gate: `/home/user/unrdf/packages/v6-core/src/delta/gate.mjs`
- Reconcile: `/home/user/unrdf/packages/v6-core/src/delta/reconcile.mjs`
