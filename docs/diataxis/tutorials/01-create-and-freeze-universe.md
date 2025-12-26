# Tutorial 01: Create and Freeze a Knowledge Universe

**Objective:** Learn how to create a partitioned knowledge universe, add data, and freeze it for immutability using KGC-4D.

**Audience:** Beginners - no prior UNRDF experience required

**Prerequisites:**
- Node.js >= 18.0.0 installed
- Basic understanding of RDF triples (subject-predicate-object)
- Familiarity with JavaScript/ESM syntax
- **Capability Atoms:** `@unrdf/core` (parse, store), `@unrdf/kgc-4d` (universe, freeze)

**Estimated Time:** 10-15 minutes

---

## What You'll Build

By the end of this tutorial, you will:
1. Create a new partitioned knowledge universe
2. Add RDF triples to the universe
3. Freeze the universe to create an immutable snapshot
4. Verify the freeze operation using receipt validation

**Final Result:** A working knowledge graph with tamper-proof freeze capability.

---

## Step 1: Set Up Your Project

**[Placeholder - Content to be filled]**

```javascript
// Setup code will go here
import { createStore } from '@unrdf/oxigraph';
import { createUniverse, freezeUniverse } from '@unrdf/kgc-4d';
```

**Evidence:** See implementation in `/home/user/unrdf/packages/kgc-4d/src/universe.mjs`

---

## Step 2: Create a Universe

**[Placeholder - Content to be filled]**

```javascript
// Universe creation code will go here
const universe = createUniverse({
  name: 'my-first-universe',
  description: 'Tutorial example universe'
});
```

**Evidence:** Universe API at `/home/user/unrdf/packages/kgc-4d/src/universe.mjs:15-45`

---

## Step 3: Add RDF Data

**[Placeholder - Content to be filled]**

```javascript
// Add triples to universe
universe.add(/* ... */);
```

**Evidence:** Data operations in `/home/user/unrdf/packages/core/src/store-operations.mjs`

---

## Step 4: Freeze the Universe

**[Placeholder - Content to be filled]**

```javascript
// Freeze operation
const receipt = await freezeUniverse(universe, {
  reason: 'Tutorial completion',
  anchor: 'git-notes'
});
```

**Evidence:** Freeze implementation at `/home/user/unrdf/packages/kgc-4d/src/freeze.mjs`

---

## Step 5: Verify the Freeze

**[Placeholder - Content to be filled]**

```javascript
// Verification code
const isValid = await verifyReceipt(receipt);
console.log('Receipt valid:', isValid);
```

**Evidence:** Receipt verification in `/home/user/unrdf/packages/kgc-4d/src/receipt-verification.mjs`

---

## Example Code

**Complete Working Example:**

[Link to proof artifact from agent 3-6 outputs - to be filled]

**Evidence:** Full example at `/home/user/unrdf/examples/freeze-universe.mjs`

---

## Common Issues

**[Placeholder - Troubleshooting tips]**

- Issue: "Universe already frozen"
- Issue: "Invalid receipt anchor"
- Issue: "Permission denied on git-notes"

---

## What You've Learned

- ✅ How to create a partitioned knowledge universe
- ✅ How to add RDF data to a universe
- ✅ How to freeze a universe for immutability
- ✅ How to verify freeze receipts
- ✅ Understanding of KGC-4D's governance model

---

## Next Steps

**Continue Learning:**
- **[Tutorial 02: Parse RDF in Browser](./02-parse-rdf-in-browser.md)** - Cross-runtime RDF parsing
- **[Tutorial 03: Generate and Verify Receipts](./03-generate-and-verify-receipts.md)** - Deep dive into receipts

**Solve Specific Problems:**
- **[How-To 01: Validate Policy Packs](../how-to/01-validate-policy-packs.md)** - Add validation rules

**Understand the Design:**
- **[Explanation 01: Why Partitioned Universes](../explanation/why-partitioned-universes.md)** - Design rationale

**API Reference:**
- **[Hook API Reference](../reference/hook-api.md)** - Complete hook documentation
- **[Receipt Schema](../reference/receipt-schema.md)** - Receipt structure details

---

**Questions?** Check [TROUBLESHOOTING.md](/home/user/unrdf/docs/TROUBLESHOOTING.md) or file an issue.
