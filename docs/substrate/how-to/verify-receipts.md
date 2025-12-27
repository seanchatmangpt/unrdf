# How to Verify Receipt Chains

Receipt verification ensures cryptographic integrity of operations, checkpoints, and workflow steps through BLAKE3 hash chains.

## Problem

You need to:

- Verify individual receipt integrity
- Validate hash chains across multiple receipts
- Detect tampering or corruption
- Audit complete operation history
- Ensure continuity of causality

## Solution

Use hash verification functions and chain validation to ensure receipts are authentic and unbroken.

## Step-by-Step

### 1. Verify Run Capsule Receipt

```javascript
import { blake3 } from 'hash-wasm';

async function verifyRunCapsule(capsule) {
  // Recompute hash from contents
  const hashContents = {
    id: capsule.id,
    parentRunId: capsule.parentRunId,
    deltaO: capsule.deltaO,
    deltaPi: capsule.deltaPi,
    deltaLambda: capsule.deltaLambda,
    deltaQ: capsule.deltaQ,
    toolTrace: capsule.toolTrace,
    artifacts: capsule.artifacts,
    t_ns: capsule.t_ns.toString(),
  };

  const serialized = JSON.stringify(hashContents, (key, value) =>
    typeof value === 'bigint' ? value.toString() : value
  );

  const recomputedHash = await blake3(serialized);

  return {
    valid: recomputedHash === capsule.runHash,
    expected: capsule.runHash,
    actual: recomputedHash,
  };
}

// Usage
const verification = await verifyRunCapsule(capsule);

if (verification.valid) {
  console.log('Run capsule verified');
} else {
  console.error('Hash mismatch:');
  console.error('Expected:', verification.expected);
  console.error('Actual:', verification.actual);
}
```

### 2. Verify Checkpoint Receipt

```javascript
import { verifyCheckpoint } from '@unrdf/kgc-claude';

async function checkCheckpointIntegrity(checkpoint, gitBackbone) {
  const result = await verifyCheckpoint(checkpoint, gitBackbone);

  if (result.valid) {
    console.log('Checkpoint verified');
    return true;
  } else {
    console.error('Checkpoint verification failed');
    console.error('Reason:', result.reason);
    return false;
  }
}

const valid = await checkCheckpointIntegrity(checkpoint, gitBackbone);
```

### 3. Verify Run Capsule Chain

```javascript
async function verifyRunCapsuleChain(capsules) {
  const results = [];

  for (let i = 0; i < capsules.length; i++) {
    const capsule = capsules[i];

    // Verify individual capsule
    const verification = await verifyRunCapsule(capsule);
    results.push({
      id: capsule.id,
      index: i,
      valid: verification.valid,
      hashMatch: verification.valid,
    });

    // Verify chain link
    if (i > 0) {
      const expected = capsules[i - 1].runHash;
      const actual = capsule.previousRunHash;

      if (expected !== actual) {
        results[i].chainBroken = true;
        results[i].expectedPrevious = expected;
        results[i].actualPrevious = actual;
        console.error(`Chain broken at index ${i}`);
      }
    }
  }

  const allValid = results.every(r => r.valid && !r.chainBroken);

  return { allValid, results };
}

// Usage
const runHistory = await getAllRunCapsules(store); // Get all runs
const chainVerification = await verifyRunCapsuleChain(runHistory);

if (chainVerification.allValid) {
  console.log('Entire chain verified');
} else {
  console.error('Chain verification failed');
  chainVerification.results.forEach((r, i) => {
    if (!r.valid || r.chainBroken) {
      console.error(`Issue at ${i}:`, r);
    }
  });
}
```

### 4. Verify Checkpoint Chain

```javascript
import { getCheckpointHistory } from '@unrdf/kgc-claude';

async function verifyCheckpointChain(gitBackbone) {
  const checkpoints = getCheckpointHistory();
  const results = [];

  for (let i = 0; i < checkpoints.length; i++) {
    const cp = checkpoints[i];

    // Verify individual checkpoint
    const verification = await verifyCheckpoint(cp, gitBackbone);
    results.push({
      id: cp.id,
      index: i,
      valid: verification.valid,
      reason: verification.reason,
    });

    // Verify chain link
    if (i > 0) {
      const expected = checkpoints[i - 1].checkpointHash;
      const actual = cp.previousCheckpointHash;

      if (expected !== actual) {
        results[i].chainBroken = true;
        console.error(`Checkpoint chain broken at ${i}`);
      }
    }
  }

  return results;
}

const cpVerification = await verifyCheckpointChain(gitBackbone);
console.log('Checkpoint chain verification:', cpVerification);
```

### 5. Verify WorkItem Receipt Chain

```javascript
import { getWorkItemReceipts } from '@unrdf/kgc-claude';

async function verifyWorkItemReceipts(workItemId) {
  const receipts = getWorkItemReceipts(workItemId);
  const results = [];

  for (let i = 0; i < receipts.length; i++) {
    const receipt = receipts[i];

    // Recompute receipt hash
    const hashContent = {
      id: receipt.id,
      workItemId: receipt.workItemId,
      executorId: receipt.executorId,
      t_ns: receipt.t_ns.toString(),
      status: receipt.status,
      progress: receipt.progress,
      output: receipt.output,
      error: receipt.error,
      previousReceiptHash: receipt.previousReceiptHash,
    };

    const recomputedHash = await blake3(JSON.stringify(hashContent));

    results.push({
      id: receipt.id,
      valid: recomputedHash === receipt.receiptHash,
      expected: receipt.receiptHash,
      actual: recomputedHash,
    });

    // Verify chain
    if (i > 0) {
      const expected = receipts[i - 1].receiptHash;
      const actual = receipt.previousReceiptHash;

      if (expected !== actual) {
        results[i].chainBroken = true;
      }
    }
  }

  return results;
}

const workItemVerification = await verifyWorkItemReceipts(workItem.id);
console.log('WorkItem receipt chain:', workItemVerification);
```

### 6. Batch Verification

```javascript
async function batchVerify(items, verifyFn) {
  const batchSize = 100;
  const results = [];

  for (let i = 0; i < items.length; i += batchSize) {
    const batch = items.slice(i, i + batchSize);

    const batchResults = await Promise.all(batch.map(item => verifyFn(item)));

    results.push(...batchResults);

    console.log(`Verified ${Math.min(i + batchSize, items.length)} / ${items.length}`);
  }

  return results;
}

// Verify all run capsules in batches
const allRuns = await getAllRunCapsules(store);
const verifications = await batchVerify(allRuns, verifyRunCapsule);

const failedCount = verifications.filter(v => !v.valid).length;
console.log(`Failed verifications: ${failedCount} / ${verifications.length}`);
```

## Advanced Patterns

### Merkle Tree Verification

```javascript
class MerkleReceiptTree {
  constructor(receipts) {
    this.receipts = receipts;
    this.tree = this.buildTree(receipts.map(r => r.receiptHash));
  }

  buildTree(hashes) {
    if (hashes.length === 0) return null;
    if (hashes.length === 1) return { hash: hashes[0], left: null, right: null };

    const mid = Math.floor(hashes.length / 2);
    const left = this.buildTree(hashes.slice(0, mid));
    const right = this.buildTree(hashes.slice(mid));

    const combined = left.hash + (right?.hash || '');
    const hash = blake3(combined);

    return { hash, left, right };
  }

  getRoot() {
    return this.tree?.hash;
  }

  async verify(receiptHash) {
    // Generate Merkle proof
    const proof = this.generateProof(this.tree, receiptHash, []);

    if (!proof) return { valid: false, reason: 'Receipt not in tree' };

    // Verify proof
    let current = receiptHash;
    for (const { sibling, position } of proof) {
      const combined = position === 'left' ? sibling + current : current + sibling;
      current = await blake3(combined);
    }

    return {
      valid: current === this.getRoot(),
      proof,
    };
  }

  generateProof(node, targetHash, proof) {
    if (!node) return null;
    if (node.hash === targetHash) return proof;

    // Search left
    if (node.left) {
      const leftProof = this.generateProof(node.left, targetHash, [
        ...proof,
        { sibling: node.right?.hash || '', position: 'right' },
      ]);
      if (leftProof) return leftProof;
    }

    // Search right
    if (node.right) {
      return this.generateProof(node.right, targetHash, [
        ...proof,
        { sibling: node.left?.hash || '', position: 'left' },
      ]);
    }

    return null;
  }
}

// Usage
const receipts = getWorkItemReceipts(workItem.id);
const merkleTree = new MerkleReceiptTree(receipts);

console.log('Merkle root:', merkleTree.getRoot());

const verification = await merkleTree.verify(receipts[0].receiptHash);
console.log('Merkle verification:', verification.valid);
```

### Continuous Verification Monitor

```javascript
class VerificationMonitor {
  constructor(interval = 60000) {
    this.interval = interval;
    this.failureCount = 0;
    this.successCount = 0;
  }

  start(store, gitBackbone) {
    this.timer = setInterval(async () => {
      await this.runVerification(store, gitBackbone);
    }, this.interval);
  }

  stop() {
    if (this.timer) {
      clearInterval(this.timer);
    }
  }

  async runVerification(store, gitBackbone) {
    console.log('Running verification cycle...');

    // Verify recent runs
    const recentRuns = await getRecentRunCapsules(store, 10);
    const runResults = await Promise.all(recentRuns.map(verifyRunCapsule));

    // Verify recent checkpoints
    const recentCheckpoints = getCheckpointHistory().slice(-5);
    const cpResults = await Promise.all(
      recentCheckpoints.map(cp => verifyCheckpoint(cp, gitBackbone))
    );

    const allValid = [...runResults, ...cpResults].every(r => r.valid);

    if (allValid) {
      this.successCount++;
      console.log(`Verification passed (${this.successCount} consecutive)`);
    } else {
      this.failureCount++;
      console.error(`Verification failed! (${this.failureCount} total failures)`);

      // Alert on failure
      this.onFailure(runResults, cpResults);
    }
  }

  onFailure(runResults, cpResults) {
    const failedRuns = runResults.filter(r => !r.valid);
    const failedCps = cpResults.filter(r => !r.valid);

    console.error('Failed runs:', failedRuns.length);
    console.error('Failed checkpoints:', failedCps.length);

    // Implement alerting (email, Slack, etc.)
  }
}

const monitor = new VerificationMonitor(60000); // Every minute
monitor.start(store, gitBackbone);
```

### Parallel Verification

```javascript
async function parallelVerify(items, verifyFn, concurrency = 10) {
  const results = [];
  const queue = [...items];

  async function worker() {
    while (queue.length > 0) {
      const item = queue.shift();
      if (item) {
        const result = await verifyFn(item);
        results.push(result);
      }
    }
  }

  // Spawn workers
  const workers = Array.from({ length: concurrency }, () => worker());
  await Promise.all(workers);

  return results;
}

// Verify 1000 receipts with 10 concurrent workers
const receipts = await getAllReceipts();
const results = await parallelVerify(receipts, verifyReceipt, 10);

console.log(`Verified ${results.length} receipts`);
console.log(`Valid: ${results.filter(r => r.valid).length}`);
console.log(`Invalid: ${results.filter(r => !r.valid).length}`);
```

## Best Practices

1. **Verify on ingestion**: Check receipts when loading from storage
2. **Periodic audits**: Run verification on schedule
3. **Alert on failures**: Immediate notification of integrity issues
4. **Batch for performance**: Verify in chunks, not one-by-one
5. **Store verification logs**: Audit trail of verification history
6. **Use Merkle trees for efficiency**: Logarithmic proof size
7. **Parallelize verification**: Utilize multiple cores

## Common Issues

**Issue**: Hash verification always fails

- **Cause**: Serialization differences (BigInt, key order)
- **Fix**: Use deterministic serialization function

**Issue**: Chain appears broken

- **Cause**: Receipts out of order
- **Fix**: Sort by timestamp before verification

**Issue**: Slow verification

- **Cause**: Large receipt count, serial processing
- **Fix**: Use parallel or batched verification

**Issue**: False positives

- **Cause**: Comparing wrong hash fields
- **Fix**: Verify you're comparing the right hash properties

## See Also

- [API Reference: Checkpoint Verification](../reference.md#checkpoint)
- [Explanation: Why Receipt Chains](../explanation.md#receipt-chains)
- [How-To: Create Checkpoints](./checkpoints.md)
