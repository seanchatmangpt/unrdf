# Tutorial 03: Generate and Verify Receipts

**Objective:** Learn how to generate tamper-proof receipts for knowledge graph operations and verify their integrity.

**Audience:** Beginners - no cryptography knowledge required

**Prerequisites:**
- Completed [Tutorial 01: Create and Freeze Universe](./01-create-and-freeze-universe.md) (required)
- Understanding of knowledge graph operations
- **Capability Atoms:** `@unrdf/kgc-4d` (receipts), `@unrdf/core` (hashing)

**Estimated Time:** 20-25 minutes

---

## What You'll Build

By the end of this tutorial, you will:
1. Generate receipts for knowledge graph operations
2. Understand receipt structure and fields
3. Verify receipt integrity using SHA-256
4. Detect tampered receipts
5. Anchor receipts to git-notes for audit trails

**Final Result:** A working receipt generation and verification system with tamper detection.

---

## Step 1: Understanding Receipts

**[Placeholder - Content to be filled]**

```javascript
// Receipt structure overview
const receipt = {
  operation: 'freeze',
  timestamp: '2025-12-26T...',
  hash: 'sha256:...',
  anchor: 'git-notes:refs/notes/receipts'
};
```

**Evidence:** Receipt schema at `/home/user/unrdf/packages/kgc-4d/src/receipt-schema.mjs`

---

## Step 2: Generate a Receipt

**[Placeholder - Content to be filled]**

```javascript
// Receipt generation code
import { generateReceipt } from '@unrdf/kgc-4d';

const receipt = await generateReceipt({
  operation: 'add-triple',
  data: store,
  reason: 'User action',
  anchor: 'git-notes'
});
```

**Evidence:** Generation implementation at `/home/user/unrdf/packages/kgc-4d/src/receipt-generation.mjs`

---

## Step 3: Verify Receipt Integrity

**[Placeholder - Content to be filled]**

```javascript
// Verification code
import { verifyReceipt } from '@unrdf/kgc-4d';

const isValid = await verifyReceipt(receipt, {
  checkAnchor: true,
  strictMode: true
});

if (!isValid) {
  console.error('Receipt has been tampered with!');
}
```

**Evidence:** Verification at `/home/user/unrdf/packages/kgc-4d/src/receipt-verification.mjs`

---

## Step 4: Detect Tampering

**[Placeholder - Content to be filled]**

```javascript
// Tamper detection example
// Simulate tampering
receipt.timestamp = '2025-01-01T00:00:00Z';

// Verify detects the change
const result = await verifyReceipt(receipt);
// result.valid = false
// result.reason = 'Hash mismatch'
```

**Evidence:** Tamper detection in `/home/user/unrdf/packages/kgc-4d/src/integrity-check.mjs`

---

## Step 5: Anchor to Git Notes

**[Placeholder - Content to be filled]**

```javascript
// Git-notes anchoring
import { anchorToGit } from '@unrdf/kgc-4d';

await anchorToGit(receipt, {
  ref: 'refs/notes/receipts',
  commit: 'HEAD'
});
```

**Evidence:** Git anchoring at `/home/user/unrdf/packages/kgc-4d/src/git-anchor.mjs`

---

## Example Code

**Complete Working Example:**

[Link to proof artifact - receipt generation demo]

**Evidence:** Full example at `/home/user/unrdf/examples/receipt-verification.mjs`

---

## Common Issues

**[Placeholder - Troubleshooting tips]**

- Issue: "Git notes permission denied"
- Issue: "Hash algorithm not supported"
- Issue: "Receipt anchor not found"

---

## What You've Learned

- ✅ How to generate tamper-proof receipts
- ✅ Understanding of receipt structure and fields
- ✅ How to verify receipt integrity
- ✅ How to detect tampering attempts
- ✅ How to anchor receipts to git-notes
- ✅ Understanding of cryptographic hashing in UNRDF

---

## Next Steps

**Continue Learning:**
- **[Tutorial 04: Implement Policy Gates](./04-implement-policy-gates.md)** - Policy-controlled hooks

**Solve Specific Problems:**
- **[How-To 02: Audit Decision Trail](../how-to/02-audit-decision-trail.md)** - Build audit systems

**Understand the Design:**
- **[Explanation 02: Proof-Based Admission vs Editing](../explanation/proof-based-admission-vs-editing.md)** - Governance model

**API Reference:**
- **[Receipt Schema Reference](../reference/receipt-schema.md)** - Complete schema details
- **[Hook API Reference](../reference/hook-api.md)** - Hook integration with receipts

---

**Questions?** Check [TROUBLESHOOTING.md](/home/user/unrdf/docs/TROUBLESHOOTING.md) or file an issue.
