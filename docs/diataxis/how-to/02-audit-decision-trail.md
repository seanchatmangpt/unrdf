# How-To 02: Audit Decision Trail

**Objective:** Build a complete audit trail for all policy decisions and hook executions in your knowledge graph.

**Audience:** Intermediate - developers implementing compliance systems

**Prerequisites:**
- Understanding of audit logging concepts
- Familiarity with git-notes or similar anchoring
- **Capability Atoms:** `@unrdf/kgc-4d` (audit, receipts), `@unrdf/hooks` (hook tracking)

**Estimated Time:** 20 minutes

---

## Problem

You need to:
- Track every policy decision (approved/denied)
- Record hook execution history
- Maintain immutable audit logs
- Query audit trail for compliance reports
- Prove audit log integrity

---

## Solution

Use UNRDF's KGC-4D audit trail system with receipt-based verification.

---

## Step-by-Step Guide

### 1. Initialize Audit System

**[Placeholder - Content to be filled]**

```javascript
import { createAuditTrail } from '@unrdf/kgc-4d';

const auditTrail = createAuditTrail({
  anchor: 'git-notes',
  autoReceipt: true,
  retention: '7y' // 7 years
});
```

**Evidence:** Audit initialization at `/home/user/unrdf/packages/kgc-4d/src/audit-trail.mjs`

---

### 2. Record Policy Decisions

**[Placeholder - Content to be filled]**

```javascript
// In your policy hook
after: async (context) => {
  await auditTrail.record({
    type: 'policy-decision',
    policy: 'require-approval',
    decision: context.vetoed ? 'denied' : 'approved',
    reason: context.reason,
    actor: context.user,
    timestamp: new Date()
  });
}
```

**Evidence:** Record implementation at `/home/user/unrdf/packages/kgc-4d/src/record-audit.mjs`

---

### 3. Query Audit Trail

**[Placeholder - Content to be filled]**

```javascript
// Query audit trail
const decisions = await auditTrail.query({
  type: 'policy-decision',
  dateRange: {
    start: '2025-01-01',
    end: '2025-12-31'
  },
  policy: 'require-approval'
});

// Generate compliance report
const report = auditTrail.generateReport(decisions);
```

**Evidence:** Query API at `/home/user/unrdf/packages/kgc-4d/src/query-audit.mjs`

---

### 4. Verify Audit Integrity

**[Placeholder - Content to be filled]**

```javascript
// Verify entire audit trail
const verification = await auditTrail.verify({
  checkReceipts: true,
  checkChain: true
});

if (!verification.valid) {
  console.error('Audit trail tampered:', verification.violations);
}
```

**Evidence:** Verification at `/home/user/unrdf/packages/kgc-4d/src/verify-audit.mjs`

---

### 5. Export for Compliance

**[Placeholder - Content to be filled]**

```javascript
// Export audit trail
await auditTrail.export({
  format: 'json-ld',
  destination: './audit-export.jsonld',
  includeReceipts: true,
  signedBy: 'compliance-officer'
});
```

**Evidence:** Export at `/home/user/unrdf/packages/kgc-4d/src/export-audit.mjs`

---

## Complete Example

**[Placeholder - Link to complete example]**

**Evidence:** Full example at `/home/user/unrdf/examples/audit-trail.mjs`

---

## Common Issues

**[Placeholder - Troubleshooting]**

- Issue: "Audit log write permission denied"
- Issue: "Receipt verification failed"
- Issue: "Audit chain broken"

---

## Related Guides

- **[How-To 01: Validate Policy Packs](./01-validate-policy-packs.md)** - Policy validation
- **[Tutorial 03: Generate and Verify Receipts](../tutorials/03-generate-and-verify-receipts.md)** - Receipt basics
- **[Reference: Receipt Schema](../reference/receipt-schema.md)** - Receipt structure

---

**Questions?** Check [TROUBLESHOOTING.md](/home/user/unrdf/docs/TROUBLESHOOTING.md) or file an issue.
