# How-To: Freeze and Verify Universe Snapshots

**Problem:** You need to create cryptographically verifiable snapshots of your knowledge graph for audit trails or compliance.

**Solution:** Use `freezeUniverse()` to create Git-backed snapshots with BLAKE3 hashing, then verify with `verifyReceipt()`.

**Time:** 10 minutes

---

## Prerequisites

- [Tutorial 02: Create and Freeze Universe](../tutorials/02-create-freeze-universe.md)
- Running KGC-4D store with events

---

## Steps

### 1. Freeze the Universe

```javascript
import { KGCStore, GitBackbone, freezeUniverse } from '@unrdf/kgc-4d';

const store = new KGCStore();
const git = new GitBackbone('./audit-repo');

// ... add events to store ...

const receipt = await freezeUniverse(store, git);
console.log('Frozen:', receipt.universe_hash);
console.log('Git ref:', receipt.git_ref);
```

### 2. Store Receipt for Later Verification

```javascript
import { writeFile } from 'fs/promises';

// Persist receipt (e.g., to database or file)
await writeFile(
  `./receipts/${receipt.id}.json`,
  JSON.stringify(receipt, null, 2)
);
```

### 3. Verify Receipt Cryptographically

```javascript
import { verifyReceipt } from '@unrdf/kgc-4d';

const verification = await verifyReceipt(receipt, git, store);

if (verification.valid) {
  console.log('Receipt VALID');
  console.log('Hash matches Git commit:', verification.receipt_id);
} else {
  console.error('Receipt INVALID:', verification.reason);
}
```

---

## Complete Code

```javascript
import {
  KGCStore,
  GitBackbone,
  freezeUniverse,
  verifyReceipt,
  EVENT_TYPES,
} from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';
import { writeFile } from 'fs/promises';

async function main() {
  const store = new KGCStore();
  const git = new GitBackbone('./freeze-verify-demo');

  // Add some data
  await store.appendEvent(
    { type: EVENT_TYPES.CREATE, payload: { subject: 'Alice' } },
    [{
      type: 'add',
      subject: dataFactory.namedNode('http://example.org/Alice'),
      predicate: dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'),
      object: dataFactory.literal('Alice'),
    }]
  );

  // Freeze
  const receipt = await freezeUniverse(store, git);
  console.log('Frozen:', receipt.universe_hash.substring(0, 16) + '...');

  // Persist receipt
  await writeFile(`./receipt-${receipt.id}.json`, JSON.stringify(receipt));

  // Verify
  const verification = await verifyReceipt(receipt, git, store);
  console.log('Verification:', verification.valid ? 'PASSED' : 'FAILED');

  return { store, git, receipt, verification };
}

main();
```

---

## Verification

Run: `node freeze-verify.mjs`

Expected output:
```
Frozen: blake3_abc123de...
Verification: PASSED
```

---

## Evidence

**Source:** `/home/user/unrdf/packages/kgc-4d/src/freeze.mjs:35` (freezeUniverse)  
**Tests:** `/home/user/unrdf/packages/kgc-4d/test/freeze.test.mjs`  
**Example:** `/home/user/unrdf/packages/kgc-4d/examples/mission-critical.mjs:22-51`

---

## Troubleshooting

**Q: Verification fails with "Hash mismatch"?**  
A: The universe state changed between freeze and verification. Ensure you verify against the exact frozen state.

**Q: Git ref not found?**  
A: Check that GitBackbone points to the same repository used during freeze.

---

## Related

- [Reference: Receipt Schema](../reference/receipt-schema.md) - Receipt structure
- [Explanation: Why BLAKE3](../explanation/05-why-blake3.md) - Hash algorithm choice
- [How-To 03: Query Event History](./03-query-event-history.md) - Audit queries
