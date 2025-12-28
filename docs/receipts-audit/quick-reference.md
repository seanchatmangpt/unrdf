# Receipt Quick Reference Guide

**UNRDF v6.0.0 Receipt System**

---

## Create a Receipt

### Execution Receipt
```javascript
import { createReceipt } from '@unrdf/v6-core/receipts';

const receipt = await createReceipt('execution', {
  eventType: 'TASK_COMPLETED',
  caseId: 'case-123',
  taskId: 'approval',
  payload: {
    decision: 'APPROVE',
    justification: { reasoning: 'All conditions met' }
  }
});

console.log(receipt.id);           // UUID
console.log(receipt.receiptHash);  // 64-char BLAKE3 hex
```

### Allocation Receipt
```javascript
const receipt = await createReceipt('allocation', {
  eventType: 'RESOURCE_ALLOCATED',
  resourceId: 'res-456',
  poolId: 'pool-789',
  allocationPeriod: { start: '2025-01-01', end: '2025-01-02' },
  capacity: { total: 100, available: 80, allocated: 20, unit: 'hours' },
  payload: { action: 'ALLOCATE' }
});
```

### Compile Receipt
```javascript
const receipt = await createReceipt('compile', {
  eventType: 'GRAMMAR_COMPILED',
  inputHashes: ['abc...', 'def...'],
  outputHash: 'ghi...',
  compilerVersion: '1.0.0',
  grammarType: 'SPARQL',
  payload: { result: 'SUCCESS' }
});
```

### Verification Receipt
```javascript
const receipt = await createReceipt('verification', {
  eventType: 'MERKLE_PROOF_VERIFIED',
  verifiedHash: 'abc...',
  merkleRoot: 'def...',
  proofPath: [{ hash: 'ghi...', position: 'left' }],
  payload: { result: 'VALID' }
});
```

---

## Chain Receipts

### Create Chain
```javascript
import { createReceipt } from '@unrdf/v6-core/receipts';

// Genesis receipt (previousReceipt = null)
const receipt1 = await createReceipt('execution', event1, null);

// Chained receipt
const receipt2 = await createReceipt('execution', event2, receipt1);

// Verify chain link
console.log(receipt2.previousHash === receipt1.receiptHash); // true
```

### Use Receipt Chain Class
```javascript
import { ReceiptChain } from '@unrdf/observability/receipts';

const chain = new ReceiptChain('audit-chain-1');

await chain.append({
  operation: 'admit',
  payload: { delta: 'delta_001' },
  actor: 'system'
});

await chain.append({
  operation: 'freeze',
  payload: { universe_hash: 'abc...' },
  actor: 'governance'
});

console.log(chain.length); // 2
console.log(chain.getLatest());
```

---

## Verify Receipts

### Verify Single Receipt
```javascript
import { verifyReceipt } from '@unrdf/v6-core/receipts';

const result = await verifyReceipt(receipt);

console.log(result.valid);           // true/false
console.log(result.checks);          // { payloadHashValid, chainHashValid }
console.log(result.error);           // Error message if invalid
```

### Verify Chain
```javascript
import { TamperDetector } from '@unrdf/observability/receipts';

const detector = new TamperDetector();
const result = await detector.verifyChain([receipt1, receipt2, receipt3]);

console.log(result.valid);    // true/false
console.log(result.errors);   // Array of error messages
```

### Verify Chain Link
```javascript
import { verifyChainLink } from '@unrdf/v6-core/receipts';

const result = await verifyChainLink(receipt2, receipt1);

console.log(result.valid);    // true/false
console.log(result.error);    // Error message if invalid
```

---

## Merkle Trees

### Build Merkle Tree
```javascript
import { buildMerkleTree } from '@unrdf/v6-core/receipts/merkle';

// Receipts must have 'hash' field (use receiptHash)
const receiptsWithHash = receipts.map(r => ({ ...r, hash: r.receiptHash }));

const tree = await buildMerkleTree(receiptsWithHash);

console.log(tree.root);       // Merkle root hash
console.log(tree.depth);      // Tree depth (log₂ N)
console.log(tree.leafCount);  // Number of leaves
```

### Generate Merkle Proof
```javascript
import { getProofPath } from '@unrdf/v6-core/receipts/merkle';

const proof = await getProofPath(tree, 'receipt-123', receipts);

console.log(proof.leaf);      // Receipt hash
console.log(proof.proof);     // Array of { hash, position }
console.log(proof.root);      // Merkle root
console.log(proof.index);     // Leaf index
```

### Verify Merkle Inclusion
```javascript
import { verifyInclusion } from '@unrdf/v6-core/receipts/merkle';

const isValid = await verifyInclusion(tree.root, receipt, proof);

console.log(isValid); // true/false
```

---

## KGC-4D Freeze

### Freeze Universe
```javascript
import { freezeUniverse } from '@unrdf/kgc-4d';
import { KGCStore } from '@unrdf/kgc-4d';
import { GitBackbone } from '@unrdf/kgc-4d';

const store = new KGCStore();
const git = new GitBackbone('/path/to/repo');

// Add data to universe
await store.appendEvent({ type: 'CREATE' }, [
  { type: 'add', subject, predicate, object }
]);

// Freeze
const receipt = await freezeUniverse(store, git);

console.log(receipt.id);              // Receipt ID
console.log(receipt.universe_hash);   // BLAKE3 hash of universe
console.log(receipt.git_ref);         // Git commit SHA
console.log(receipt.t_ns);            // Nanosecond timestamp
```

### Verify Freeze Receipt
```javascript
import { verifyReceipt } from '@unrdf/kgc-4d';

const result = await verifyReceipt(receipt, git);

console.log(result.valid);         // true/false
console.log(result.universe_hash); // Recomputed hash
console.log(result.reason);        // Error reason if invalid
```

### Time-Travel (Reconstruct State)
```javascript
import { reconstructState } from '@unrdf/kgc-4d';

const targetTime = BigInt('1704110400000000000'); // Nanoseconds
const reconstructed = await reconstructState(store, git, targetTime);

console.log(reconstructed.size());  // Quad count at targetTime
```

---

## withReceipt HOF (Wrap Functions)

### Basic Usage
```javascript
import { withReceipt } from '@unrdf/v6-core/receipts';

const processData = withReceipt(
  (data) => data.map(x => x * 2),
  {
    operation: 'processData',
    getTimestamp: () => 1704110400000000000n // Deterministic
  }
);

const { result, receipt } = await processData([1, 2, 3]);

console.log(result);           // [2, 4, 6]
console.log(receipt.id);       // Receipt ID
console.log(receipt.receiptHash); // Receipt hash
```

### Chained Receipts
```javascript
import { createReceiptChain } from '@unrdf/v6-core/receipts';

const chain = await createReceiptChain([
  { fn: step1, context: { operation: 'step1' } },
  { fn: step2, context: { operation: 'step2' } },
  { fn: step3, context: { operation: 'step3' } },
]);

// chain[0].receipt.previousHash === null
// chain[1].receipt.previousHash === chain[0].receipt.receiptHash
// chain[2].receipt.previousHash === chain[1].receipt.receiptHash
```

### Verify Idempotency
```javascript
import { verifyIdempotency } from '@unrdf/v6-core/receipts';

const wrapped = withReceipt(
  (x) => x * 2,
  { operation: 'double', getTimestamp: () => 1704110400000000000n }
);

const check = await verifyIdempotency(wrapped, [5]);

console.log(check.idempotent);   // true
console.log(check.hashMatch);    // true
```

---

## SPARQL Queries

### Find All Receipts
```sparql
PREFIX kgc: <http://kgc.io/>

SELECT ?receipt ?time ?hash WHERE {
  GRAPH <http://kgc.io/receipts> {
    ?receipt kgc:t_ns ?time ;
             kgc:receiptHash ?hash .
  }
}
ORDER BY ?time
```

### Get Receipts for Case
```sparql
PREFIX kgc: <http://kgc.io/>

SELECT ?receipt ?operation ?timestamp WHERE {
  GRAPH <http://kgc.io/receipts> {
    ?receipt kgc:caseId "case-123" ;
             kgc:operation ?operation ;
             kgc:timestamp_iso ?timestamp .
  }
}
ORDER BY ?timestamp
```

### Find Snapshots
```sparql
PREFIX kgc: <http://kgc.io/>

SELECT ?event ?time ?hash ?gitRef WHERE {
  GRAPH <http://kgc.io/event_log> {
    ?event kgc:type "SNAPSHOT" ;
           kgc:t_ns ?time ;
           kgc:git_ref ?gitRef ;
           kgc:payload ?payload .
    
    BIND(json_extract(?payload, "$.universe_hash") AS ?hash)
  }
}
ORDER BY ?time
```

### Detect Temporal Anomalies
```sparql
PREFIX kgc: <http://kgc.io/>

SELECT ?r1 ?r2 ?t1 ?t2 WHERE {
  ?r1 kgc:t_ns ?t1 .
  ?r2 kgc:previousHash ?r1 ;
      kgc:t_ns ?t2 .
  
  FILTER(?t2 <= ?t1)  # Temporal violation
}
```

---

## Common Patterns

### Pattern 1: Generate → Verify
```javascript
const receipt = await createReceipt('execution', event);
const result = await verifyReceipt(receipt);
console.assert(result.valid);
```

### Pattern 2: Chain → Batch → Anchor
```javascript
// 1. Generate chain
const receipts = [];
for (const event of events) {
  const prev = receipts[receipts.length - 1] || null;
  const receipt = await createReceipt('execution', event, prev);
  receipts.push(receipt);
}

// 2. Build Merkle tree
const receiptsWithHash = receipts.map(r => ({ ...r, hash: r.receiptHash }));
const tree = await buildMerkleTree(receiptsWithHash);

// 3. Anchor merkle root
const anchorReceipt = await createReceipt('verification', {
  eventType: 'MERKLE_ROOT_ANCHORED',
  merkleRoot: tree.root,
  receiptCount: receipts.length,
  payload: { blockchain: 'ethereum', txHash: '0x...' }
});
```

### Pattern 3: Time-Travel → Verify
```javascript
// 1. Freeze at t1
const receipt1 = await freezeUniverse(store, git);

// 2. Modify universe
await store.appendEvent(...);

// 3. Freeze at t2
const receipt2 = await freezeUniverse(store, git);

// 4. Time-travel back to t1
const restored = await reconstructState(store, git, receipt1.t_ns);

// 5. Verify restored state matches receipt1
const verified = await verifyReceipt(receipt1, git);
console.assert(verified.valid);
```

---

## Performance Tips

1. **Batch receipts**: Create 1000 receipts, then Merkle batch
2. **Cache recent**: LRU cache for last 100 receipts
3. **Async verification**: Verify 10 receipts in parallel
4. **Prepared queries**: Pre-compile SPARQL queries
5. **Git batching**: Commit 1000 receipts at once

---

## Troubleshooting

### Receipt Verification Fails
```javascript
const result = await verifyReceipt(receipt);
if (!result.valid) {
  console.log('Checks:', result.checks);
  console.log('Error:', result.error);
  
  // Check payloadHash
  if (!result.checks.payloadHashValid) {
    console.log('Payload has been modified');
  }
  
  // Check chainHash
  if (!result.checks.chainHashValid) {
    console.log('Chain link broken');
  }
}
```

### Chain Verification Fails
```javascript
const detector = new TamperDetector();
const result = await detector.verifyChain(receipts);
if (!result.valid) {
  console.log('Errors:', result.errors);
  
  // Common issues:
  // - Genesis receipt has non-null previousHash
  // - Chain link broken (previousHash mismatch)
  // - Temporal ordering violated
}
```

### Merkle Verification Fails
```javascript
const isValid = await verifyInclusion(root, receipt, proof);
if (!isValid) {
  console.log('Receipt not in Merkle tree');
  console.log('Expected root:', root);
  console.log('Computed root:', /* recompute */);
  
  // Verify receipt is in original receipts array
  // Verify proof is for correct receipt
  // Verify tree was built correctly
}
```

---

**END OF QUICK REFERENCE**
