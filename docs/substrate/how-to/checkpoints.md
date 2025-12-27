# How to Use Checkpoints for State Management

Checkpoints provide universal freeze/thaw capabilities with cryptographic receipts, portable across all Claude surfaces (CLI, IDE, MCP).

## Problem

You need to:

- Create snapshots of universe state for rollback
- Verify snapshot integrity cryptographically
- Restore previous states after failures
- Track checkpoint history with hash chains

## Solution

Use the checkpoint module's `freeze` and `thaw` operations with Git-backed persistence.

## Step-by-Step

### 1. Create a Basic Checkpoint

```javascript
import { freeze } from '@unrdf/kgc-claude';

// Create checkpoint
const checkpoint = await freeze(store, gitBackbone);

console.log('Checkpoint created:');
console.log('ID:', checkpoint.id);
console.log('Snapshot hash:', checkpoint.snapshotHash);
console.log('Git ref:', checkpoint.gitRef);
console.log('Universe size:', checkpoint.universeSize);
console.log('Timestamp:', checkpoint.timestamp_iso);
```

### 2. Create Checkpoint with Run References

```javascript
// After persisting runs, checkpoint with references
const run1Capsule = await run1.seal();
await persistRunCapsule(store, run1Capsule);

const run2Capsule = await run2.seal();
await persistRunCapsule(store, run2Capsule);

const checkpoint = await freeze(store, gitBackbone, {
  runCapsuleIds: [run1Capsule.id, run2Capsule.id],
});

console.log('Checkpoint includes runs:', checkpoint.runCapsuleIds);
```

### 3. Restore from Checkpoint (Thaw)

```javascript
import { thaw } from '@unrdf/kgc-claude';

try {
  // Restore state
  const restoredStore = await thaw(store, gitBackbone, checkpoint.id);

  console.log('State restored to checkpoint:', checkpoint.id);
  console.log('Restored store:', restoredStore);
} catch (error) {
  console.error('Failed to restore:', error.message);
}
```

### 4. Verify Checkpoint Integrity

```javascript
import { verifyCheckpoint } from '@unrdf/kgc-claude';

const verification = await verifyCheckpoint(checkpoint, gitBackbone);

if (verification.valid) {
  console.log('Checkpoint verified successfully');
} else {
  console.error('Verification failed:', verification.reason);
  // Possible reasons:
  // - Snapshot hash mismatch
  // - Previous checkpoint not found
  // - Git snapshot corrupted
}
```

### 5. Checkpoint-Protected Execution

```javascript
import { withCheckpoint } from '@unrdf/kgc-claude';

try {
  const { result, checkpoint } = await withCheckpoint(store, gitBackbone, async ctx => {
    // Access pre-checkpoint
    console.log('Before checkpoint:', ctx.checkpoint.id);

    // Do risky operations
    const run = createRunCapsule();
    run.addToolCall({ name: 'Edit', input: { file: 'critical.mjs' } });

    const capsule = await run.seal();
    await persistRunCapsule(store, capsule);

    // Simulate potential failure
    if (Math.random() < 0.5) {
      throw new Error('Simulated failure');
    }

    // Return run IDs for checkpoint
    return { runCapsuleIds: [capsule.id] };
  });

  console.log('Operation succeeded');
  console.log('After checkpoint:', checkpoint.id);
} catch (error) {
  console.log('Operation failed, state rolled back');
  console.error(error.message);
}
```

### 6. View Checkpoint History

```javascript
import { getCheckpointHistory } from '@unrdf/kgc-claude';

const history = getCheckpointHistory();

console.log('Checkpoint history:');
history.forEach((cp, index) => {
  console.log(`${index + 1}. ${cp.id}`);
  console.log(`   Time: ${cp.timestamp_iso}`);
  console.log(`   Hash: ${cp.checkpointHash}`);
  console.log(`   Previous: ${cp.previousCheckpointHash || 'none'}`);
  console.log(`   Size: ${cp.universeSize} quads`);
});
```

### 7. Reconstruct Session State

```javascript
import { reconstructSession } from '@unrdf/kgc-claude';

// Reconstruct state at specific time
const targetTime = checkpoint.t_ns;
const reconstructedStore = await reconstructSession(store, gitBackbone, targetTime);

console.log('Session reconstructed at:', targetTime);
```

### 8. Calculate Drift from Ideal State

```javascript
import { calculateDrift } from '@unrdf/kgc-claude';

// Get expected state
const expectedStore = await reconstructSession(store, gitBackbone, checkpoint.t_ns);

// Compare to actual state
const drift = calculateDrift(store, expectedStore);

console.log('Drift score:', drift);
// 0 = perfect match
// >0 = divergence (higher = more drift)

if (drift > 0.1) {
  console.warn('Significant drift detected, consider rollback');
}
```

## Advanced Patterns

### Checkpoint Chain Verification

```javascript
async function verifyCheckpointChain(checkpoints, gitBackbone) {
  const results = [];

  for (let i = 0; i < checkpoints.length; i++) {
    const cp = checkpoints[i];
    const verification = await verifyCheckpoint(cp, gitBackbone);

    results.push({
      checkpoint: cp.id,
      valid: verification.valid,
      reason: verification.reason,
    });

    // Check chain link
    if (i > 0) {
      const expected = checkpoints[i - 1].checkpointHash;
      const actual = cp.previousCheckpointHash;

      if (expected !== actual) {
        results[i].chainBroken = true;
        console.error(`Chain broken at checkpoint ${i}: ${cp.id}`);
      }
    }
  }

  return results;
}

const history = getCheckpointHistory();
const chainVerification = await verifyCheckpointChain(history, gitBackbone);
console.log('Chain verification:', chainVerification);
```

### Periodic Auto-Checkpointing

```javascript
async function autoCheckpoint(store, gitBackbone, intervalMs) {
  let lastCheckpoint = null;

  setInterval(async () => {
    try {
      const checkpoint = await freeze(store, gitBackbone);
      console.log('Auto-checkpoint created:', checkpoint.id);
      lastCheckpoint = checkpoint;
    } catch (error) {
      console.error('Auto-checkpoint failed:', error.message);
    }
  }, intervalMs);

  return () => lastCheckpoint;
}

// Auto-checkpoint every 5 minutes
const getLastCheckpoint = autoCheckpoint(store, gitBackbone, 5 * 60 * 1000);

// Later, get last checkpoint
const last = getLastCheckpoint();
console.log('Last auto-checkpoint:', last?.id);
```

### Checkpoint Comparison

```javascript
async function compareCheckpoints(cp1, cp2, gitBackbone) {
  // Load both snapshots
  const snapshot1 = await gitBackbone.readSnapshot(cp1.gitRef);
  const snapshot2 = await gitBackbone.readSnapshot(cp2.gitRef);

  // Parse and compare
  const quads1 = parseNQuads(snapshot1);
  const quads2 = parseNQuads(snapshot2);

  const added = quads2.filter(q => !quads1.includes(q));
  const removed = quads1.filter(q => !quads2.includes(q));

  return {
    added: added.length,
    removed: removed.length,
    unchanged: quads2.filter(q => quads1.includes(q)).length,
    timeDiff: Number(cp2.t_ns - cp1.t_ns) / 1e9, // seconds
  };
}

const diff = await compareCheckpoints(checkpoint1, checkpoint2, gitBackbone);
console.log('Checkpoint diff:', diff);
```

### Conditional Rollback

```javascript
async function executeWithRollbackCondition(store, gitBackbone, operation, shouldRollback) {
  const beforeCheckpoint = await freeze(store, gitBackbone);

  try {
    const result = await operation();

    // Check if rollback is needed
    if (shouldRollback(result)) {
      console.log('Condition triggered rollback');
      await thaw(store, gitBackbone, beforeCheckpoint.id);
      return { rolledBack: true, reason: 'Condition failed' };
    }

    // Success, create after checkpoint
    const afterCheckpoint = await freeze(store, gitBackbone);
    return { rolledBack: false, checkpoint: afterCheckpoint };
  } catch (error) {
    // Error, always rollback
    await thaw(store, gitBackbone, beforeCheckpoint.id);
    throw error;
  }
}

// Example usage
const result = await executeWithRollbackCondition(
  store,
  gitBackbone,
  async () => {
    // Do work
    return { success: true, metrics: { filesChanged: 15 } };
  },
  result => {
    // Rollback if too many files changed
    return result.metrics.filesChanged > 10;
  }
);
```

### Checkpoint Pruning

```javascript
import { clearCheckpointHistory } from '@unrdf/kgc-claude';

function pruneOldCheckpoints(maxAge, maxCount) {
  const history = getCheckpointHistory();
  const now = BigInt(Date.now()) * 1000000n; // Convert to nanoseconds
  const maxAgeNs = BigInt(maxAge) * 1000000n;

  // Filter by age
  const recent = history.filter(cp => {
    return now - cp.t_ns < maxAgeNs;
  });

  // Keep only last N
  const toKeep = recent.slice(-maxCount);

  console.log(`Pruning checkpoints: ${history.length} → ${toKeep.length}`);

  // Clear and rebuild (warning: this clears in-memory history)
  clearCheckpointHistory();

  // Note: In production, you'd persist toKeep to durable storage
  return toKeep;
}

// Keep only checkpoints from last 24 hours, max 100
const kept = pruneOldCheckpoints(24 * 60 * 60 * 1000, 100);
```

## Best Practices

1. **Checkpoint before risky operations**: Always freeze before mutations
2. **Verify after creation**: Confirm integrity immediately
3. **Use withCheckpoint for automatic rollback**: Simplifies error handling
4. **Track checkpoint chains**: Maintain continuity for audit trails
5. **Prune old checkpoints**: Prevent unbounded growth
6. **Test restoration**: Regularly verify thaw operations work
7. **Monitor drift**: Track divergence from expected state

## Common Issues

**Issue**: Thaw fails with "Checkpoint not found"

- **Cause**: Checkpoint not in history map (ephemeral storage)
- **Fix**: Persist checkpoint receipts to durable storage

**Issue**: Verification fails with hash mismatch

- **Cause**: Git snapshot modified externally
- **Fix**: Protect `.git` directory, use read-only refs

**Issue**: Drift continuously increases

- **Cause**: Operations not going through proper channels
- **Fix**: Audit code for direct store mutations bypassing event log

**Issue**: Checkpoint creation is slow

- **Cause**: Large universe size
- **Fix**: Increase Git snapshot compression, consider incremental snapshots

## See Also

- [API Reference: Checkpoint](../reference.md#checkpoint)
- [Explanation: Why Universal Checkpointing](../explanation.md#universal-checkpointing)
- [Tutorial: Step 5-7](../tutorial.md#step-5-create-a-checkpoint)
