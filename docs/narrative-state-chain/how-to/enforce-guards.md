# How-To: Enforce Guards

**Goal:** Add admissibility policies (H) to control which deltas are lawful.

**Use Case:** You need to prevent unauthorized mutations, enforce business rules, or validate preconditions before applying changes.

**Time:** 15 minutes  
**Prerequisites:** 
- Completed [Tutorial 01: Your First Scene](../tutorial/01-hello-world.md)
- Understanding of Delta operations

---

## Problem Statement

By default, `DeltaGate` allows all deltas (non-strict mode). But in production, you need:

1. **Authorization:** Only specific actors can modify certain predicates
2. **Invariant enforcement:** Prevent operations that violate constraints
3. **Precondition checks:** Ensure required state exists before mutation
4. **Audit trails:** Log who did what and why

Guards (H) enforce these rules **before** reconciliation (μ), ensuring only lawful deltas modify state.

---

## Solution Overview

```javascript
// Create gate with policies
const gate = new DeltaGate({
  policies: {
    'policy-id': async (delta, store) => ({
      allowed: boolean,    // Is delta lawful?
      reason?: string      // Why denied (if !allowed)
    })
  },
  strict: true  // Require explicit admissibility
});

// Deltas specify which policies to check
const delta = {
  // ... delta fields
  admissibility: {
    policyId: 'policy-id',           // Primary policy
    constraints: ['constraint-1'],    // Additional checks
    preConditions: ['precondition-1'] // Required state
  }
};
```

**Evidence:** [packages/v6-core/src/delta/gate.mjs](/home/user/unrdf/packages/v6-core/src/delta/gate.mjs):164

---

## Implementation Steps

### Step 1: Define Guard Policies

```javascript
/**
 * Guard Policy: No Delete Operations
 * 
 * Prevents any delta containing delete operations.
 * Use case: Append-only audit log
 */
async function noDeletesPolicy(delta, store) {
  const hasDeletes = delta.operations.some(op => op.op === 'delete');
  
  return {
    allowed: !hasDeletes,
    reason: hasDeletes ? 'Delete operations are not allowed' : undefined
  };
}

/**
 * Guard Policy: Require Actor
 * 
 * Ensures all deltas have an actor (for audit trail).
 */
async function requireActorPolicy(delta, store) {
  const hasActor = !!delta.source.actor;
  
  return {
    allowed: hasActor,
    reason: !hasActor ? 'Actor is required for all deltas' : undefined
  };
}

/**
 * Guard Policy: Namespace Authorization
 * 
 * Only specific actors can modify certain namespaces.
 */
async function namespaceAuthPolicy(delta, store) {
  const actor = delta.source.actor;
  const operations = delta.operations;
  
  // Admin actors can modify any namespace
  if (actor === 'admin' || actor === 'system') {
    return { allowed: true };
  }
  
  // Check if any operations touch admin namespace
  const touchesAdminNS = operations.some(op =>
    op.subject.startsWith('http://admin.example.org/')
  );
  
  if (touchesAdminNS) {
    return {
      allowed: false,
      reason: `Actor '${actor}' cannot modify admin namespace`
    };
  }
  
  return { allowed: true };
}
```

**Policy Contract:**
```typescript
/**
 * Guard policy function
 * 
 * @param {Delta} delta - Proposed delta
 * @param {KGCStore} store - Current universe state
 * @returns {Promise<{allowed: boolean, reason?: string}>}
 */
type GuardPolicy = (
  delta: Delta,
  store: KGCStore
) => Promise<{ allowed: boolean; reason?: string }>;
```

---

### Step 2: Register Policies with Gate

```javascript
import { DeltaGate } from '@unrdf/v6-core/delta';

// Option A: Policies in constructor
const gate = new DeltaGate({
  policies: {
    'no-deletes': noDeletesPolicy,
    'require-actor': requireActorPolicy,
    'namespace-auth': namespaceAuthPolicy
  },
  strict: true  // Reject deltas without admissibility
});

// Option B: Add policies dynamically
const gate = new DeltaGate({ strict: false });
gate.addPolicy('no-deletes', noDeletesPolicy);
gate.addPolicy('require-actor', requireActorPolicy);
gate.addPolicy('namespace-auth', namespaceAuthPolicy);

console.log('Registered policies:', gate.listPolicies());
// ['no-deletes', 'require-actor', 'namespace-auth']
```

**Evidence:** [packages/v6-core/src/delta/gate.mjs](/home/user/unrdf/packages/v6-core/src/delta/gate.mjs):245

---

### Step 3: Specify Admissibility in Deltas

```javascript
import { createDelta } from '@unrdf/v6-core/delta';

// Create delta with admissibility requirements
const delta = {
  id: crypto.randomUUID(),
  timestamp_iso: new Date().toISOString(),
  t_ns: BigInt(Date.now()) * 1_000_000n,
  operations: [
    {
      op: 'add',
      subject: 'http://example.org/Alice',
      predicate: 'http://example.org/name',
      object: 'Alice Smith'
    }
  ],
  source: {
    package: '@my-app',
    actor: 'user-123'  // Required by 'require-actor' policy
  },
  admissibility: {
    policyId: 'require-actor',              // Primary check
    constraints: ['no-deletes'],            // Additional constraint
    preConditions: []                       // No preconditions
  }
};

// Propose delta
const receipt = await gate.proposeDelta(delta, store);

if (receipt.applied) {
  console.log('✅ Delta lawful and applied');
  console.log('State hash:', receipt.stateHash);
} else {
  console.error('❌ Delta rejected:', receipt.reason);
}
```

**Evidence:** [packages/v6-core/src/delta/schema.mjs](/home/user/unrdf/packages/v6-core/src/delta/schema.mjs):76

---

### Step 4: Handle Rejections

```javascript
/**
 * Propose delta with error handling
 */
async function proposeDeltaSafely(gate, delta, store) {
  try {
    const receipt = await gate.proposeDelta(delta, store);
    
    if (!receipt.applied) {
      // Log denial for audit
      console.error('[DENIED]', {
        deltaId: receipt.deltaId,
        reason: receipt.reason,
        timestamp: receipt.timestamp_ns,
        actor: delta.source.actor
      });
      
      // Throw or return error
      throw new Error(`Delta denied: ${receipt.reason}`);
    }
    
    // Success: log and return
    console.log('[APPLIED]', {
      deltaId: receipt.deltaId,
      stateHash: receipt.stateHash,
      operations: receipt.operationsApplied
    });
    
    return receipt;
    
  } catch (error) {
    // Validation error (schema failure)
    console.error('[ERROR]', {
      deltaId: delta.id,
      error: error.message
    });
    throw error;
  }
}

// Usage
const receipt = await proposeDeltaSafely(gate, delta, store);
```

---

## Advanced: Stateful Preconditions

```javascript
/**
 * Guard Policy: Require Entity Exists
 * 
 * Prevents updating an entity that doesn't exist.
 * Queries the store to check precondition.
 */
async function requireEntityExistsPolicy(delta, store) {
  for (const op of delta.operations) {
    if (op.op === 'update') {
      // Check if subject exists in store
      const exists = store.getQuads(
        store.namedNode(op.subject),
        null,
        null
      ).length > 0;
      
      if (!exists) {
        return {
          allowed: false,
          reason: `Cannot update non-existent entity: ${op.subject}`
        };
      }
    }
  }
  
  return { allowed: true };
}

// Register as precondition
gate.addPolicy('entity-exists', requireEntityExistsPolicy);

// Use in delta
const delta = {
  // ... delta fields
  admissibility: {
    preConditions: ['entity-exists']  // Check before reconciliation
  }
};
```

**Why use preConditions vs constraints?**

| Type | When Checked | Purpose | Example |
|------|--------------|---------|---------|
| **Policy** | First | Primary authorization | Actor has permission |
| **Constraints** | After policy | Validate operations | No deletes allowed |
| **PreConditions** | After constraints | State requirements | Entity must exist |

**Evidence:** [packages/v6-core/src/delta/gate.mjs](/home/user/unrdf/packages/v6-core/src/delta/gate.mjs):198

---

## Advanced: Composable Policies

```javascript
/**
 * Compose multiple policies into one
 */
function composePolicies(...policies) {
  return async (delta, store) => {
    for (const policy of policies) {
      const result = await policy(delta, store);
      if (!result.allowed) {
        return result;  // Short-circuit on first denial
      }
    }
    return { allowed: true };
  };
}

// Create composed policy
const strictPolicy = composePolicies(
  requireActorPolicy,
  noDeletesPolicy,
  namespaceAuthPolicy
);

// Register composed policy
gate.addPolicy('strict-all', strictPolicy);

// Use in delta
const delta = {
  // ... delta fields
  admissibility: {
    policyId: 'strict-all'  // Checks all 3 policies
  }
};
```

---

## Testing Guard Policies

```javascript
import { describe, test, expect } from 'vitest';
import { DeltaGate, createDelta } from '@unrdf/v6-core/delta';
import { createStore } from '@unrdf/core';

describe('Guard Policies', () => {
  test('no-deletes policy rejects delete operations', async () => {
    const gate = new DeltaGate({
      policies: { 'no-deletes': noDeletesPolicy },
      strict: false
    });
    const store = createStore();
    
    const delta = createDelta(
      'delete',
      'http://example.org/Alice',
      'http://example.org/name',
      'Alice Smith',
      { package: '@test' }
    );
    
    delta.admissibility = { policyId: 'no-deletes' };
    
    const receipt = await gate.proposeDelta(delta, store);
    
    expect(receipt.applied).toBe(false);
    expect(receipt.reason).toContain('Delete operations are not allowed');
  });
  
  test('require-actor policy rejects deltas without actor', async () => {
    const gate = new DeltaGate({
      policies: { 'require-actor': requireActorPolicy }
    });
    const store = createStore();
    
    const delta = createDelta(
      'add',
      'http://example.org/Alice',
      'http://example.org/name',
      'Alice Smith',
      { package: '@test' }  // No actor!
    );
    
    delta.admissibility = { policyId: 'require-actor' };
    
    const receipt = await gate.proposeDelta(delta, store);
    
    expect(receipt.applied).toBe(false);
    expect(receipt.reason).toContain('Actor is required');
  });
  
  test('namespace-auth policy allows admin actors', async () => {
    const gate = new DeltaGate({
      policies: { 'namespace-auth': namespaceAuthPolicy }
    });
    const store = createStore();
    
    const delta = createDelta(
      'add',
      'http://admin.example.org/setting',
      'http://example.org/value',
      'secret',
      { package: '@test', actor: 'admin' }  // Admin actor
    );
    
    delta.admissibility = { policyId: 'namespace-auth' };
    
    const receipt = await gate.proposeDelta(delta, store);
    
    expect(receipt.applied).toBe(true);
  });
});
```

**Run tests:**
```bash
npm test -- how-to/enforce-guards.test.mjs
```

---

## Complete Example

```javascript
/**
 * Complete Example: Enforce Guards
 */
import { createStore } from '@unrdf/core';
import { DeltaGate, createDelta } from '@unrdf/v6-core/delta';

// 1. Define policies
async function noDeletesPolicy(delta, store) {
  const hasDeletes = delta.operations.some(op => op.op === 'delete');
  return {
    allowed: !hasDeletes,
    reason: hasDeletes ? 'Delete operations not allowed' : undefined
  };
}

async function requireActorPolicy(delta, store) {
  return {
    allowed: !!delta.source.actor,
    reason: !delta.source.actor ? 'Actor required' : undefined
  };
}

// 2. Create gate with policies
const gate = new DeltaGate({
  policies: {
    'no-deletes': noDeletesPolicy,
    'require-actor': requireActorPolicy
  },
  strict: true
});

const store = createStore();

// 3. Lawful delta (passes policies)
const lawfulDelta = createDelta(
  'add',
  'http://example.org/Alice',
  'http://example.org/name',
  'Alice Smith',
  { package: '@my-app', actor: 'user-123' }
);
lawfulDelta.admissibility = {
  constraints: ['no-deletes', 'require-actor']
};

const receipt1 = await gate.proposeDelta(lawfulDelta, store);
console.log('Lawful delta:', receipt1.applied); // true

// 4. Unlawful delta (violates no-deletes)
const unlawfulDelta = createDelta(
  'delete',
  'http://example.org/Alice',
  'http://example.org/name',
  'Alice Smith',
  { package: '@my-app', actor: 'user-123' }
);
unlawfulDelta.admissibility = {
  constraints: ['no-deletes', 'require-actor']
};

const receipt2 = await gate.proposeDelta(unlawfulDelta, store);
console.log('Unlawful delta:', receipt2.applied); // false
console.log('Reason:', receipt2.reason); // 'Constraint no-deletes violated: Delete operations not allowed'
```

---

## Key Takeaways

1. **Guards run BEFORE reconciliation:** Prevent unlawful state from ever being computed
2. **Policies are composable:** Combine multiple checks via constraints/preConditions
3. **Every denial produces a receipt:** Audit trail for rejected operations
4. **Stateful checks are possible:** Query store in policy functions
5. **Strict mode enforces discipline:** Require explicit admissibility declarations

---

## Common Patterns

### Pattern 1: Role-Based Access Control (RBAC)
```javascript
async function rbacPolicy(delta, store) {
  const actor = delta.source.actor;
  const role = await getUserRole(actor, store);
  const requiredRole = delta.admissibility?.context?.requiredRole || 'user';
  
  const allowed = hasPermission(role, requiredRole);
  return {
    allowed,
    reason: !allowed ? `Role '${role}' cannot perform this operation` : undefined
  };
}
```

### Pattern 2: Rate Limiting
```javascript
const rateLimits = new Map(); // actor → {count, resetTime}

async function rateLimitPolicy(delta, store) {
  const actor = delta.source.actor;
  const now = Date.now();
  const limit = rateLimits.get(actor) || { count: 0, resetTime: now + 60000 };
  
  if (now > limit.resetTime) {
    limit.count = 0;
    limit.resetTime = now + 60000;
  }
  
  if (limit.count >= 10) {
    return {
      allowed: false,
      reason: 'Rate limit exceeded (10 ops/minute)'
    };
  }
  
  limit.count++;
  rateLimits.set(actor, limit);
  return { allowed: true };
}
```

### Pattern 3: Schema Validation
```javascript
import { z } from 'zod';

const UserSchema = z.object({
  subject: z.string().url(),
  name: z.string().min(1),
  email: z.string().email()
});

async function schemaValidationPolicy(delta, store) {
  for (const op of delta.operations) {
    if (op.subject.includes('/users/')) {
      try {
        UserSchema.parse({
          subject: op.subject,
          name: op.object,  // Simplified for example
          email: '...'
        });
      } catch (err) {
        return {
          allowed: false,
          reason: `Schema validation failed: ${err.message}`
        };
      }
    }
  }
  return { allowed: true };
}
```

---

## Next Steps

1. **[How-To: Write Reconciliation Logic](write-reconciliation.md)** — Implement μ(O ⊔ Δ) with conflict resolution
2. **[How-To: Verify Receipts](verify-receipts.md)** — Validate cryptographic proofs
3. **[Reference: Receipt API](../reference/api-receipt.md)** — Receipt schema details

---

## Evidence & Proofs

**Code:**
- DeltaGate: [/home/user/unrdf/packages/v6-core/src/delta/gate.mjs](/home/user/unrdf/packages/v6-core/src/delta/gate.mjs)
- Admissibility schema: [/home/user/unrdf/packages/v6-core/src/delta/schema.mjs](/home/user/unrdf/packages/v6-core/src/delta/schema.mjs):76

**Tests:**
- Gate policy tests: [/home/user/unrdf/packages/v6-core/test/delta-gate.test.mjs](/home/user/unrdf/packages/v6-core/test/delta-gate.test.mjs)

**Verification:**
```bash
# Run guard policy tests
npm test -- packages/v6-core/test/delta-gate.test.mjs
```
