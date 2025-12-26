# Tutorial 04: Implement Policy Gates

**Objective:** Learn how to implement policy-controlled hooks that act as gates for knowledge graph operations.

**Audience:** Intermediate - requires understanding of hooks and policies

**Prerequisites:**
- Completed [Tutorial 01: Create and Freeze Universe](./01-create-and-freeze-universe.md) (required)
- Completed [Tutorial 03: Generate and Verify Receipts](./03-generate-and-verify-receipts.md) (recommended)
- Understanding of reactive programming concepts
- **Capability Atoms:** `@unrdf/hooks` (hooks), `@unrdf/validation` (policies)

**Estimated Time:** 25-30 minutes

---

## What You'll Build

By the end of this tutorial, you will:
1. Define policy predicates for knowledge operations
2. Implement a policy gate hook
3. Configure before/run/after lifecycle
4. Test policy enforcement
5. Build an audit trail for policy decisions

**Final Result:** A working policy gate system that controls access to knowledge graph operations.

---

## Step 1: Define Policy Predicates

**[Placeholder - Content to be filled]**

```javascript
// Policy predicate definition
const policy = {
  name: 'require-approval',
  predicate: `
    ASK {
      ?operation :hasApproval ?approval .
      ?approval :approvedBy ?user .
      FILTER(?user = :admin)
    }
  `
};
```

**Evidence:** Policy predicates at `/home/user/unrdf/packages/validation/src/policy-predicates.mjs`

---

## Step 2: Implement Policy Gate Hook

**[Placeholder - Content to be filled]**

```javascript
// Policy gate hook
import { defineHook } from '@unrdf/hooks';

const policyGate = defineHook({
  meta: {
    name: 'policy-gate',
    description: 'Enforce approval policy'
  },
  when: {
    kind: 'sparql-ask',
    ref: {
      uri: 'policy://require-approval',
      sha256: '...',
      mediaType: 'application/sparql-query'
    }
  },
  before: async (context) => {
    // Pre-check logic
  },
  run: async (context) => {
    // Main gate logic
  },
  after: async (context) => {
    // Audit trail
  }
});
```

**Evidence:** Hook definition at `/home/user/unrdf/packages/hooks/src/define-hook.mjs`

---

## Step 3: Configure Lifecycle Handlers

**[Placeholder - Content to be filled]**

```javascript
// Before: Validate input
before: async (context) => {
  if (!context.operation) {
    throw new Error('Operation required');
  }
}

// Run: Check policy
run: async (context) => {
  const approved = await checkPolicy(context);
  if (!approved) {
    return { veto: true, reason: 'Policy not met' };
  }
  return { veto: false };
}

// After: Audit
after: async (context) => {
  await auditLog(context);
}
```

**Evidence:** Lifecycle handlers at `/home/user/unrdf/packages/hooks/src/lifecycle.mjs`

---

## Step 4: Test Policy Enforcement

**[Placeholder - Content to be filled]**

```javascript
// Test policy gate
const result = await executeOperation({
  type: 'add-triple',
  triple: ['ex:Alice', 'ex:knows', 'ex:Bob']
});

// Should be vetoed if no approval
assert(result.vetoed === true);
assert(result.reason === 'Policy not met');
```

**Evidence:** Testing utilities at `/home/user/unrdf/packages/test-utils/src/hook-testing.mjs`

---

## Step 5: Build Audit Trail

**[Placeholder - Content to be filled]**

```javascript
// Audit trail implementation
import { createAuditLog } from '@unrdf/kgc-4d';

const auditLog = createAuditLog({
  anchor: 'git-notes',
  includeReceipts: true
});

await auditLog.record({
  operation: 'policy-check',
  result: 'vetoed',
  reason: 'No approval found'
});
```

**Evidence:** Audit logging at `/home/user/unrdf/packages/kgc-4d/src/audit-log.mjs`

---

## Example Code

**Complete Working Example:**

[Link to proof artifact - policy gate demo]

**Evidence:** Full example at `/home/user/unrdf/examples/policy-gate.mjs`

---

## Common Issues

**[Placeholder - Troubleshooting tips]**

- Issue: "Policy predicate syntax error"
- Issue: "Hook not triggered"
- Issue: "Infinite veto loop"
- Issue: "Audit log permission denied"

---

## What You've Learned

- ✅ How to define policy predicates
- ✅ How to implement policy gate hooks
- ✅ Understanding of before/run/after lifecycle
- ✅ How to test policy enforcement
- ✅ How to build audit trails
- ✅ Understanding of hook vetoing mechanism

---

## Next Steps

**Continue Learning:**
- **[Tutorial 01: Create and Freeze Universe](./01-create-and-freeze-universe.md)** - Review basics

**Solve Specific Problems:**
- **[How-To 01: Validate Policy Packs](../how-to/01-validate-policy-packs.md)** - Complex policy validation
- **[How-To 02: Audit Decision Trail](../how-to/02-audit-decision-trail.md)** - Advanced auditing

**Understand the Design:**
- **[Explanation 02: Proof-Based Admission vs Editing](../explanation/proof-based-admission-vs-editing.md)** - Governance philosophy

**API Reference:**
- **[Policy Predicate Syntax Reference](../reference/policy-predicate-syntax.md)** - Complete syntax
- **[Hook API Reference](../reference/hook-api.md)** - All hook options

---

**Questions?** Check [TROUBLESHOOTING.md](/home/user/unrdf/docs/TROUBLESHOOTING.md) or file an issue.
