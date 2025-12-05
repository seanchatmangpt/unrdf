# How To: Verify Snapshots Cryptographically

**Problem:** You need to prove that a snapshot hasn't been tampered with and is authentic.

**Solution:** Use `verifyReceipt()` with BLAKE3 hashing to cryptographically verify integrity.

## When to Use This

- **Compliance**: Prove data hasn't changed since a transaction
- **Auditing**: Verify historical snapshots are authentic
- **Distribution**: Ensure snapshots received over the network are valid
- **Archiving**: Prove data integrity for long-term storage
- **Legal**: Provide cryptographic proof for disputes

## Basic Verification

```javascript
import { KGCStore, GitBackbone, freezeUniverse, verifyReceipt } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

const store = new KGCStore();
const git = new GitBackbone('./repo');

// Create data and freeze
await store.appendEvent(
  { type: 'CREATE', payload: { description: 'Important data' } },
  [/* mutations */]
);

const frozen = await freezeUniverse(store, git);

// Later, verify it
const isValid = await verifyReceipt(frozen, git, store);

if (isValid) {
  console.log('✓ Snapshot is authentic and unchanged');
} else {
  console.log('✗ WARNING: Snapshot has been tampered with!');
}
```

## Understanding the Verification Process

When you freeze the universe:

```javascript
const frozen = await freezeUniverse(store, git);
// frozen contains:
// - gitRef: Git commit reference
// - hash: BLAKE3 hash of the N-Quads
// - snapshotId: Unique identifier
// - tNs: Timestamp
```

The receipt includes a cryptographic commitment. To verify:

```javascript
// Step 1: Fetch the snapshot from Git
const snapshot = await git.readSnapshot(frozen.gitRef);

// Step 2: Recompute the BLAKE3 hash
const recomputedHash = await blake3Hash(snapshot);

// Step 3: Compare hashes
const isValid = recomputedHash === frozen.hash;
```

## Batch Verification

Verify multiple snapshots efficiently:

```javascript
async function verifySnapshots(snapshots, git, store) {
  const results = [];

  for (const snapshot of snapshots) {
    const isValid = await verifyReceipt(snapshot, git, store);
    results.push({
      snapshotId: snapshot.snapshotId,
      hash: snapshot.hash.slice(0, 16) + '...',
      valid: isValid,
      timestamp: new Date(Number(snapshot.tNs) / 1_000_000).toISOString(),
    });
  }

  return results;
}

// Verify all snapshots in your history
const verifications = await verifySnapshots(allFrozenSnapshots, git, store);
console.table(verifications);

// Check for failures
const failures = verifications.filter(r => !r.valid);
if (failures.length > 0) {
  console.error('⚠️  Found', failures.length, 'invalid snapshots');
}
```

## Verify Against Different Stores

Snapshots created from one store can be verified against another:

```javascript
// Original store that created the snapshot
const originalStore = new KGCStore();
const frozen = await freezeUniverse(originalStore, git);

// Later, in a different application instance
const newStore = new KGCStore();

// Verify the original snapshot still matches Git
const isValid = await verifyReceipt(frozen, git, newStore);
```

This is useful for:
- **Distributed systems**: Verify snapshots received from peers
- **Integration tests**: Ensure replicated data is consistent
- **Long-term archiving**: Verify old snapshots haven't degraded

## Build Verification Into Your Workflow

### Create and Verify Immediately

```javascript
async function safeFreeze(store, git) {
  // Create snapshot
  const frozen = await freezeUniverse(store, git);

  // Verify immediately
  const isValid = await verifyReceipt(frozen, git, store);

  if (!isValid) {
    throw new Error('Snapshot verification failed immediately after creation!');
  }

  return frozen;
}
```

### Periodic Audit

```javascript
async function auditSnapshots(store, git, snapshotList, interval) {
  setInterval(async () => {
    console.log(`[${new Date().toISOString()}] Auditing snapshots...`);

    for (const snapshot of snapshotList) {
      const isValid = await verifyReceipt(snapshot, git, store);

      if (!isValid) {
        console.error(`❌ ALERT: Snapshot ${snapshot.snapshotId} failed verification!`);
        // Alert, log, notify admin, etc.
      } else {
        console.log(`✓ Snapshot ${snapshot.snapshotId.slice(0, 8)} verified`);
      }
    }
  }, interval);
}

// Audit every hour
auditSnapshots(store, git, snapshots, 60 * 60 * 1000);
```

### Verify on Load

```javascript
async function loadAndVerifySnapshot(snapshotRef, git, store) {
  // Load snapshot metadata
  const frozen = await getSnapshotMetadata(snapshotRef);

  // Verify before using
  const isValid = await verifyReceipt(frozen, git, store);

  if (!isValid) {
    throw new Error(`Cannot use snapshot ${snapshotRef}: verification failed`);
  }

  // Reconstruct state from verified snapshot
  const reconstructed = await reconstructState(store, git, frozen.tNs);
  return reconstructed;
}
```

## Compliance Reports

Generate verification reports for audits:

```javascript
async function complianceReport(git, snapshots) {
  const report = {
    generatedAt: new Date().toISOString(),
    verifications: [],
    summary: {
      total: snapshots.length,
      valid: 0,
      invalid: 0,
    },
  };

  for (const snapshot of snapshots) {
    const isValid = await verifyReceipt(snapshot, git, store);

    report.verifications.push({
      snapshotId: snapshot.snapshotId,
      hash: snapshot.hash,
      verified: isValid,
      timestamp: new Date(Number(snapshot.tNs) / 1_000_000).toISOString(),
      gitRef: snapshot.gitRef,
    });

    if (isValid) {
      report.summary.valid++;
    } else {
      report.summary.invalid++;
    }
  }

  return report;
}

const report = await complianceReport(git, allSnapshots);
console.log(JSON.stringify(report, null, 2));
```

## Understanding BLAKE3

KGC 4D uses **BLAKE3** for hashing because:

| Property | Benefit |
|----------|---------|
| **Cryptographically secure** | Essentially impossible to forge collisions |
| **Fast** | Faster than SHA-256 in software |
| **Deterministic** | Same input always produces same hash |
| **Content-addressed** | Hash uniquely identifies content |

The BLAKE3 hash is computed from the **canonical N-Quads export**:

```javascript
// Step 1: Export Universe as N-Quads (canonical format)
const nquads = await store.exportQuads('N-Quads');

// Step 2: Hash the N-Quads
const hash = await blake3Hash(nquads);

// Step 3: This hash is the verification receipt
```

Because N-Quads format is deterministic, the same data always produces the same hash.

## Distributed Verification

Verify snapshots received from peers:

```javascript
async function verifyPeerSnapshot(peerSnapshot, localGit, localStore) {
  // Peer sends: snapshot data + hash
  // You have: Git with peer's commits

  // Verify that the hash matches the Git content
  const isValid = await verifyReceipt(peerSnapshot, localGit, localStore);

  if (isValid) {
    console.log('✓ Peer snapshot is authentic');
    // Safe to trust this snapshot
  } else {
    console.log('✗ Peer snapshot failed verification');
    // Reject, don't use, alert
  }
}
```

## Troubleshooting

**Q: "Git ref not found" error during verification**
A: The Git commit may have been garbage collected. Ensure Git repository is fully synchronized before verifying.

**Q: Hash mismatch immediately after freezing**
A: This should never happen. If it does, check for:
- Concurrent modifications to the store
- File system issues
- Memory corruption (very unlikely)

**Q: Verification is slow**
A: BLAKE3 computation is fast, but reading from Git might be slow on large snapshots. Use batch verification to parallelize.

**Q: Different hashes for the same data?**
A: Ensure the Universe graph is exported in the same order. Use canonical N-Quads format (which KGC 4D enforces).

## Summary

- Use `verifyReceipt(snapshot, git, store)` to verify authenticity
- BLAKE3 detects any tampering with the snapshot
- Verification works across different application instances
- Use periodic audits for compliance and security
- Build verification into your data loading workflow
