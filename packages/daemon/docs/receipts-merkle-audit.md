# Daemon Receipts Merkle Tree Audit Integration

> Cryptographic receipt generation with Merkle tree batching for daemon operations audit trails

## Overview

The `DaemonReceiptGenerator` provides cryptographic receipts with Merkle tree support for daemon background operations. Each operation generates a tamper-evident receipt with:

- **BLAKE3 hashing** for cryptographic integrity
- **Receipt chaining** where each receipt links to the previous one
- **Merkle tree batching** for efficient proof generation over operation batches
- **Proof verification** allowing auditors to verify operations without full chain
- **Tamper detection** identifying modified or tampered receipts

## Architecture

### Receipt Chain Structure

Each receipt in the chain references the previous receipt via `previousHash`, creating an immutable audit trail:

```
Receipt 1 (genesis)
  ├─ previousHash: null
  ├─ payloadHash: BLAKE3(operation_data)
  └─ receiptHash: BLAKE3(GENESIS:payloadHash)

Receipt 2
  ├─ previousHash: Receipt1.receiptHash  [chain link]
  ├─ payloadHash: BLAKE3(operation_data)
  └─ receiptHash: BLAKE3(previousHash:payloadHash)

Receipt 3
  ├─ previousHash: Receipt2.receiptHash  [chain link]
  ├─ payloadHash: BLAKE3(operation_data)
  └─ receiptHash: BLAKE3(previousHash:payloadHash)
```

Any modification to a receipt breaks the chain, making tampering immediately detectable.

### Merkle Tree Batching

Receipts are batched into Merkle trees for efficient proof generation:

```
Merkle Tree (8 receipts)
                Root
               /    \
             Node1  Node2
            /    \  /    \
          H1-H2 H3-H4 H5-H6 H7-H8
          / \   / \   / \   / \
         R1 R2 R3 R4 R5 R6 R7 R8
```

Where:
- **Leaves** are receipt hashes (merkleLeafHash)
- **Internal nodes** combine pairs: `BLAKE3(left_hash:right_hash)`
- **Root** is the top node, representing all 8 receipts

### Batch Proofs

A batch proof combines:
1. **Merkle root** - represents all receipts in the batch
2. **Receipts** - the actual receipt objects
3. **Tree structure** - for verification and export

## API Reference

### Creating a Generator

```javascript
import DaemonReceiptGenerator from '@unrdf/daemon/integrations/receipts-merkle';

// Default: batch size 100, max buffer 1000
const generator = new DaemonReceiptGenerator();

// Custom configuration
const generator = new DaemonReceiptGenerator({
  batchSize: 50,        // Operations per batch (10-100)
  maxBufferSize: 500    // Max buffered operations
});
```

### Generating Receipts

```javascript
// Single operation receipt
const receipt = await generator.generateReceipt({
  operationId: uuid(),
  operationType: 'task_executed',
  timestamp_ns: BigInt(Date.now() * 1_000_000),
  nodeId: 'node-1',
  daemonId: 'daemon-1',
  payload: {
    taskId: 'task-123',
    status: 'completed',
    duration_ms: 42
  }
});

console.log(receipt);
// {
//   id: 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx',
//   operationId: '...',
//   operationType: 'task_executed',
//   timestamp_ns: 1234567890000000n,
//   timestamp_iso: '2025-01-11T12:34:56.789Z',
//   payloadHash: '[64-char-hex]',
//   previousHash: '[64-char-hex] or null',
//   receiptHash: '[64-char-hex]',
//   batchIndex: 0,
//   merkleLeafHash: '[64-char-hex]'
// }
```

**Receipt Fields**:
- `id` - Unique receipt identifier (UUID v4)
- `operationId` - Reference to the operation
- `operationType` - Type of operation (task_scheduled, task_executed, task_failed, state_change, hook_triggered)
- `payloadHash` - BLAKE3 hash of operation payload
- `previousHash` - Hash of previous receipt (null for genesis)
- `receiptHash` - Hash of this receipt (BLAKE3(previousHash:payloadHash))
- `merkleLeafHash` - Hash used in Merkle tree
- `batchIndex` - Index in current batch

### Generating Batch Proofs

```javascript
// Buffer receipts
const operations = [...]; // Array of operations
for (const op of operations) {
  await generator.generateReceipt(op);
}

// Generate batch proof
const proof = await generator.generateBatchProof(50);
// or all buffered: await generator.generateBatchProof();

console.log(proof);
// {
//   batchId: 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx',
//   batchNumber: 0,
//   merkleRoot: '[64-char-hex]',
//   leafCount: 50,
//   treeDepth: 6,
//   timestamp_ns: 1234567890000000n,
//   receipts: [receipt1, receipt2, ...]
// }
```

### Verifying Receipts

```javascript
// Get inclusion proof for a receipt
const proof = await generator.getReceiptProof(receiptId, receipts);

// Verify proof
const isValid = await generator.verifyProof(proof);
console.log(isValid); // true or false
```

**Proof Structure**:
- `leafHash` - Receipt hash
- `leafIndex` - Position in tree
- `proofPath` - Array of sibling hashes with positions (left/right)
- `merkleRoot` - Tree root to verify against
- `batchSize` - Number of receipts in tree

**Verification Process**:
1. Start with leaf hash
2. Apply proof path steps: `BLAKE3(hash1:hash2)` or `BLAKE3(hash2:hash1)`
3. Compare final computed hash with merkleRoot

### Verifying Chains

```javascript
// Verify entire chain integrity
const result = await generator.verifyChain(receipts);

console.log(result);
// {
//   valid: true,
//   totalReceipts: 100,
//   validReceipts: 100,
//   tamperedReceipts: [],
//   merkleRootConsistent: true,
//   chainLinksValid: true
// }
```

**Verification Checks**:
1. **Genesis Receipt** - First receipt must have `previousHash: null`
2. **Chain Links** - Each receipt's `previousHash` must equal previous receipt's `receiptHash`
3. **Hash Integrity** - Receipt hash must validate: `BLAKE3(previousHash:payloadHash)`
4. **Merkle Consistency** - Tree root must be consistent across rebuilds

### Tamper Detection

```javascript
// Detect tampering in a batch
const tamperedReceipts = await generator.detectTampering(receipts);

if (tamperedReceipts.length > 0) {
  console.log('Tampering detected!');
  for (const tampered of tamperedReceipts) {
    console.log(`Receipt ${tampered.receiptId}: ${tampered.reason}`);
  }
}
```

### Exporting Merkle Trees

```javascript
// Export tree structure for audit/storage
const tree = await generator.exportMerkleTree(receipts);

// Save to file
import fs from 'fs/promises';
await fs.writeFile('audit-tree.json', JSON.stringify(tree, null, 2));

// Tree structure:
// {
//   root: '[64-char-hex]',
//   depth: 4,
//   leafCount: 16,
//   leaves: ['[hash1]', '[hash2]', ...],
//   treeStructure: {
//     root: '[hash]',
//     depth: 4,
//     leafCount: 16,
//     levels: [
//       { level: 0, nodeCount: 16, hashes: [...] },
//       { level: 1, nodeCount: 8, hashes: [...] },
//       ...
//     ]
//   }
// }
```

### Statistics

```javascript
const stats = generator.getStatistics();

console.log(stats);
// {
//   totalBatchesGenerated: 5,
//   bufferedOperations: 25,
//   totalReceiptsGenerated: 500,
//   totalBatches: 5,
//   lastReceiptHash: '[64-char-hex]'
// }
```

## Proof Verification Workflow for Auditors

### Scenario: Verify a batch of operations

1. **Obtain batch proof from daemon**
   ```javascript
   const proof = daemonProof; // From audit log
   ```

2. **Extract specific receipt to verify**
   ```javascript
   const receipt = proof.receipts[5];
   ```

3. **Generate Merkle inclusion proof**
   ```javascript
   const merkleProof = await generator.getReceiptProof(
     receipt.id,
     proof.receipts
   );
   ```

4. **Verify proof against published Merkle root**
   ```javascript
   const isValid = await generator.verifyProof(merkleProof);
   if (!isValid) {
     console.log('Receipt NOT in published tree - tampering suspected!');
   }
   ```

5. **Verify chain integrity**
   ```javascript
   const chainResult = await generator.verifyChain(proof.receipts);
   if (!chainResult.valid) {
     console.log('Chain broken at receipts:', chainResult.tamperedReceipts);
   }
   ```

## Tamper Detection Workflow

### Detecting Modified Receipts

Tampering can be detected at multiple levels:

1. **Single Receipt Verification**
   ```javascript
   // If receiptHash doesn't match BLAKE3(previousHash:payloadHash)
   // → Receipt has been modified
   ```

2. **Chain Verification**
   ```javascript
   const result = await generator.verifyChain(receipts);
   if (!result.chainLinksValid) {
     // Some receipts have broken chain links
     // Identify which: result.tamperedReceipts
   }
   ```

3. **Merkle Tree Verification**
   ```javascript
   const proof = await generator.getReceiptProof(receiptId, receipts);
   const isValid = await generator.verifyProof(proof);
   if (!isValid) {
     // Receipt not in published Merkle tree
     // Could indicate insertion, deletion, or modification
   }
   ```

### Common Tampering Patterns

| Tamper Type | Detection | Evidence |
|-------------|-----------|----------|
| **Payload modification** | receiptHash changes | Hash integrity check fails |
| **Receipt insertion** | chain breaks | previousHash doesn't link |
| **Receipt deletion** | chain breaks | nextReceipt.previousHash invalid |
| **Receipt modification** | multiple failures | Hash + chain both fail |
| **Tree manipulation** | Merkle proof fails | Proof doesn't reconstruct root |

## Performance Characteristics

### Batch Size Impact

| Batch Size | Tree Depth | Proof Path Length | Time to Verify |
|-----------|-----------|------------------|-----------------|
| 10 receipts | 4 | 4 steps | <1ms |
| 50 receipts | 6 | 6 steps | <5ms |
| 100 receipts | 7 | 7 steps | <10ms |
| 1,000 receipts | 10 | 10 steps | <50ms |

### Recommended Configuration

- **Default Daemon**: `batchSize: 100` (optimal balance)
- **High-throughput**: `batchSize: 100` (max efficient size)
- **Low-latency audit**: `batchSize: 25` (faster verification)

## Security Considerations

### Cryptographic Guarantees

- **BLAKE3**: 256-bit output (64 hex chars), collision-resistant
- **Chain links**: Unbreakable without knowing private operations
- **Merkle proofs**: Cannot forge proof without knowing pre-image

### Audit Trail Properties

1. **Immutability**: Modifying any receipt breaks chain
2. **Completeness**: All operations must be in chain
3. **Ordering**: Timestamps enforce chronological order
4. **Non-repudiation**: Node/daemon IDs recorded with operations

### Trust Model

```
Auditor
  ↓
[Receipt] ← Verified via chain link + hash
  ↓
[Merkle Proof] ← Verified via tree reconstruction
  ↓
[Published Root] ← Checked against audit log
  ↓
Trust Level: High (cryptographic proof)
```

## Integration with Daemon Operations

### Capturing Operations

```javascript
daemon.on('operation:execute', async (operation) => {
  const receipt = await receiptGenerator.generateReceipt({
    operationId: operation.id,
    operationType: 'task_executed',
    timestamp_ns: BigInt(Date.now() * 1_000_000),
    nodeId: daemon.nodeId,
    daemonId: daemon.config.daemonId,
    payload: operation.result
  });

  // Store receipt or publish to audit log
  auditLog.append(receipt);
});
```

### Periodic Batch Generation

```javascript
// Every 1000 receipts or 5 minutes
daemon.on('tick:5m', async () => {
  if (receiptGenerator.getStatistics().bufferedOperations > 1000) {
    const proof = await receiptGenerator.generateBatchProof();
    auditLog.publishBatch(proof);
  }
});
```

### Audit Queries

```javascript
// Query operations in date range
async function auditOperations(startTime, endTime) {
  const batches = auditLog.queryBatches({ startTime, endTime });

  for (const batch of batches) {
    const result = await generator.verifyChain(batch.receipts);
    if (!result.valid) {
      console.warn(`Batch ${batch.batchId}: tampering detected`);
      return { valid: false, tamperedReceipts: result.tamperedReceipts };
    }
  }

  return { valid: true, receipts: batches.flatMap(b => b.receipts) };
}
```

## Examples

### Example: Audit a daemon operation

```javascript
import DaemonReceiptGenerator from '@unrdf/daemon/integrations/receipts-merkle';

// Auditor creates generator
const generator = new DaemonReceiptGenerator();

// Retrieve batch from daemon audit log
const batchJSON = await fetch('/daemon/audit/batch/001').then(r => r.json());

// Verify batch integrity
const verifyResult = await generator.verifyChain(batchJSON.receipts);
console.log(`Batch valid: ${verifyResult.valid}`);
console.log(`Receipts: ${verifyResult.validReceipts}/${verifyResult.totalReceipts}`);

if (!verifyResult.valid) {
  console.log('Tampering detected in:');
  verifyResult.tamperedReceipts.forEach(t => {
    console.log(`  - ${t.receiptId}: ${t.reason}`);
  });
}

// Verify specific receipt in tree
const receipt = batchJSON.receipts[5];
const proof = await generator.getReceiptProof(receipt.id, batchJSON.receipts);
const isInTree = await generator.verifyProof(proof);
console.log(`Receipt in published tree: ${isInTree}`);
```

## Limitations

- **Single Merkle tree per batch**: Merkle trees cannot span batches
- **No timestamp verification**: Chain doesn't enforce temporal ordering
- **Synchronous hashing**: BLAKE3 is async, impacts throughput
- **In-memory storage**: Receipts stored in memory (use database for production)

## Future Enhancements

- Timestamp-based ordering enforcement
- Persistent receipt storage with indexing
- Distributed proof anchoring (blockchain)
- Incremental Merkle tree updates
- Receipt compression and archival
