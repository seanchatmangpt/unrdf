# How-To: Verify Receipt Chain Integrity

**Time**: ~20 minutes
**Difficulty**: Intermediate
**Prerequisites**: Understanding of [receipts](../tutorials/01-getting-started-v6.md#your-first-receipt)

---

## What is a Receipt Chain?

A receipt chain is a **cryptographic chain of custody** linking operations in sequence. Each receipt contains:

1. **Hash of current operation** (SHA-256)
2. **Hash of parent receipt** (chain link)
3. **Merkle proof** (anchors to immutable tree)
4. **Digital signature** (authenticity)

**Think**: Blockchain for knowledge graph operations.

---

## Receipt Structure

```json
{
  "hash": "sha256:current-hash",
  "timestamp": 1704067200000,
  "operation": "delta.apply",
  "inputs": { "deltaId": "delta-001", "universeId": "universe-abc" },
  "outputs": { "newStateHash": "sha256:state-after" },
  "proof": {
    "parentReceipts": ["sha256:parent-hash"],
    "merkleRoot": "sha256:tree-root",
    "merklePath": [...],
    "signature": "ed25519:sig..."
  },
  "metadata": {
    "package": "@unrdf/v6-core",
    "agent": "alice@example.org",
    "version": "6.0.0-alpha.1"
  }
}
```

---

## Step 1: Generate a Receipt Chain

Execute multiple operations to build a chain:

```javascript
import { withReceipt } from '@unrdf/v6-core/receipts';
import { createStore } from '@unrdf/oxigraph';

// Operation 1: Create store
const createStoreWithReceipt = withReceipt(async function createStoreOp() {
  return await createStore();
});

const { value: store, receipt: receipt1 } = await createStoreWithReceipt();
console.log('Receipt 1:', receipt1.hash);

// Operation 2: Add triple (chains to receipt1)
const addTripleWithReceipt = withReceipt(async function addTripleOp(store, subject, predicate, object) {
  await store.add({ subject, predicate, object });
  return store;
}, { parentReceipt: receipt1.hash });

const { value: updatedStore, receipt: receipt2 } = await addTripleWithReceipt(
  store,
  'http://example.org/alice',
  'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
  'http://xmlns.com/foaf/0.1/Person'
);
console.log('Receipt 2:', receipt2.hash);
console.log('Parent:', receipt2.proof.parentReceipts); // [receipt1.hash]

// Operation 3: Query (chains to receipt2)
const queryWithReceipt = withReceipt(async function queryOp(store, sparql) {
  return await store.query(sparql);
}, { parentReceipt: receipt2.hash });

const { value: results, receipt: receipt3 } = await queryWithReceipt(
  updatedStore,
  'SELECT * WHERE { ?s ?p ?o }'
);
console.log('Receipt 3:', receipt3.hash);
console.log('Chain:', receipt3.proof.parentReceipts); // [receipt2.hash, receipt1.hash]
```

**Receipt Chain**:
```
receipt1 ← receipt2 ← receipt3
  (create)  (add)     (query)
```

---

## Step 2: Verify Individual Receipt

```bash
npx kgc receipt verify --hash sha256:receipt3-hash
```

**Checks:**
- ✅ Hash matches content (deterministic)
- ✅ Parent receipts exist
- ✅ Merkle proof valid
- ✅ Signature valid
- ✅ Timestamp within acceptable range

**Output:**
```
✅ Receipt sha256:receipt3-hash is valid
  - Hash: verified
  - Parent receipts: 1 found
  - Merkle proof: valid
  - Signature: ed25519 verified
  - Chain depth: 3
```

---

## Step 3: Verify Entire Chain

```bash
npx kgc receipt chain --hash sha256:receipt3-hash --verify
```

**Output:**
```
Receipt Chain (depth 3):
  1. sha256:receipt1-hash (createStoreOp)
  2. sha256:receipt2-hash (addTripleOp)
  3. sha256:receipt3-hash (queryOp)

Verification:
  ✅ All hashes verified
  ✅ All parent links valid
  ✅ Merkle proofs valid
  ✅ Signatures valid
  ✅ Chain of custody intact
```

---

## Programmatic Verification

```javascript
import { verifyReceiptChain, getReceiptChain } from '@unrdf/v6-core/receipts';

// Get full chain starting from a receipt
const chain = await getReceiptChain(receipt3.hash);
console.log('Chain length:', chain.length); // 3

// Verify entire chain
const { valid, errors } = await verifyReceiptChain(chain);

if (valid) {
  console.log('✅ Chain verified!');
  console.log('Operations:', chain.map(r => r.operation));
} else {
  console.error('❌ Chain verification failed:', errors);
}
```

---

## Step 4: Detect Tampering

Receipts are **tamper-evident**. Any modification invalidates the chain.

### Example: Tampered Receipt

```javascript
import { verifyReceipt } from '@unrdf/v6-core/receipts';

const tamperedReceipt = {
  ...receipt2,
  outputs: { differentValue: 'malicious' } // Modified!
};

const { valid, reason } = await verifyReceipt(tamperedReceipt);
console.log('Valid?', valid); // false
console.log('Reason:', reason); // "Hash mismatch: expected sha256:..., got sha256:..."
```

**Security**: Even a single bit change invalidates the hash.

---

## Step 5: Anchor Receipts to Blockchain

For maximum security, anchor receipt merkle roots to a blockchain:

```bash
npx kgc receipt anchor --hash sha256:receipt3-hash --blockchain ethereum
```

**Process:**
1. Compute Merkle root of receipt chain
2. Submit root to Ethereum smart contract
3. Return transaction hash as proof

**Verification** (later):
```bash
npx kgc receipt verify --hash sha256:receipt3-hash --blockchain ethereum --tx 0xabc123...
```

**Output:**
```
✅ Receipt sha256:receipt3-hash verified
  - Merkle root: sha256:root-hash
  - Anchored to Ethereum: block 18234567
  - Transaction: 0xabc123...
  - Timestamp on-chain: 2024-01-01 12:00:00 UTC
```

---

## Receipt Chain Export

Export the entire chain for archival or sharing:

```bash
npx kgc receipt chain --hash sha256:receipt3-hash --export chain.json
```

**Output** (`chain.json`):
```json
{
  "chainId": "chain-abc123",
  "receipts": [
    { "hash": "sha256:receipt1-hash", ... },
    { "hash": "sha256:receipt2-hash", ... },
    { "hash": "sha256:receipt3-hash", ... }
  ],
  "merkleRoot": "sha256:root-hash",
  "verification": {
    "verified": true,
    "timestamp": 1704067200000,
    "verifiedBy": "@unrdf/v6-core@6.0.0-alpha.1"
  }
}
```

Share `chain.json` with auditors or regulators for verification.

---

## Receipt Replay

Receipts enable **deterministic replay** - reproduce operations from receipts:

```bash
npx kgc receipt replay --file chain.json --universe universe-replay
```

**Process:**
1. Create new universe `universe-replay`
2. Replay operations from receipts in order
3. Verify final state hash matches original

**Output:**
```
Replaying 3 operations...
  1. createStoreOp (sha256:receipt1-hash) ✅
  2. addTripleOp (sha256:receipt2-hash) ✅
  3. queryOp (sha256:receipt3-hash) ✅

Final state hash: sha256:state-after
Original state hash: sha256:state-after
✅ Replay successful! States match.
```

---

## Advanced: Multi-Package Receipt Chains

Receipts can chain across packages:

```javascript
import { withReceipt as yawlReceipt } from '@unrdf/yawl';
import { withReceipt as hooksReceipt } from '@unrdf/hooks';

// YAWL operation
const { receipt: yawlReceiptData } = await yawlReceipt(startWorkflow)();

// Hooks operation (chains to YAWL)
const { receipt: hooksReceiptData } = await hooksReceipt(
  activateHook,
  { parentReceipt: yawlReceiptData.hash }
)();

console.log('Cross-package chain:', hooksReceiptData.proof.parentReceipts);
// ['sha256:yawl-receipt-hash']
```

Verify cross-package chain:
```bash
npx kgc receipt chain --hash sha256:hooks-receipt-hash --verify
```

**Output:**
```
Receipt Chain (cross-package):
  1. sha256:yawl-receipt-hash (@unrdf/yawl)
  2. sha256:hooks-receipt-hash (@unrdf/hooks)

✅ Cross-package chain verified
  - Packages: @unrdf/yawl, @unrdf/hooks
  - Total operations: 2
```

---

## Security Best Practices

1. **Store receipts separately** from data (e.g., `.kgc/receipts/`)
2. **Anchor to blockchain** for high-value operations
3. **Verify periodically** (e.g., daily cron job)
4. **Export chains** for compliance audits
5. **Use hardware signing** for production (HSM, TPM)

---

## Troubleshooting

### "Parent receipt not found"

**Cause**: Missing receipt in chain

**Solution**: Restore from backup or blockchain anchor:
```bash
npx kgc receipt restore --hash sha256:parent-hash --from blockchain
```

### "Merkle proof verification failed"

**Cause**: Receipt tampered or incomplete

**Solution**: Re-export chain from source:
```bash
npx kgc receipt chain --hash sha256:root-hash --export chain-verified.json
```

### "Timestamp out of acceptable range"

**Cause**: Receipt created in future or past (clock skew)

**Solution**: Adjust verification tolerance:
```bash
npx kgc receipt verify --hash sha256:hash --tolerance 3600
# Allows ±1 hour clock skew
```

---

## Summary

✅ Receipt chains provide cryptographic chain of custody
✅ Each receipt links to parent via hash
✅ Verify individual receipts or entire chains
✅ Detect tampering via hash mismatches
✅ Anchor to blockchain for immutability
✅ Export chains for audits
✅ Replay operations deterministically

**Next**: [How-To: Implement L5 Maturity for Custom Package](./04-implement-l5-maturity.md)
