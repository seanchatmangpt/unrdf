# How To: Integrate with Git

**Problem:** You need to store KGC 4D snapshots in Git for versioning, backup, and audit trails.

**Solution:** Use `GitBackbone` to manage snapshot storage and verification.

## When to Use This

- **Version control**: Track history of knowledge graph changes
- **Backup**: Store snapshots as immutable Git commits
- **Distribution**: Share snapshots with other systems
- **Audit trails**: Prove who froze which snapshot when
- **Compliance**: Maintain chain of custody for data

## Initialize a Git Repository

```javascript
import { GitBackbone } from '@unrdf/kgc-4d';

// Create a new Git repository
const git = new GitBackbone('./my-kgc-repo');

// Or use an existing repository
const existingGit = new GitBackbone('/path/to/existing/repo');

console.log('✓ Git repository ready');
```

## Store Snapshots in Git

```javascript
import { KGCStore, GitBackbone, freezeUniverse } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

const store = new KGCStore();
const git = new GitBackbone('./snapshots');

// Create some data
await store.appendEvent(
  { type: 'CREATE', payload: { description: 'Important data' } },
  [/* mutations */]
);

// Freeze to Git
const frozen = await freezeUniverse(store, git);

console.log('✓ Snapshot frozen to Git');
console.log('  Git ref:', frozen.gitRef);
console.log('  Hash:', frozen.hash);
console.log('  Available at Git commit:', frozen.gitRef);
```

The snapshot is now stored as a Git commit containing the N-Quads export.

## Retrieve Snapshots from Git

```javascript
// Read a snapshot back from Git
const nquads = await git.readSnapshot(frozen.gitRef);
console.log('Snapshot N-Quads (first 500 chars):');
console.log(nquads.slice(0, 500));

// Verify integrity
const isValid = await verifyReceipt(frozen, git, store);
if (isValid) {
  console.log('✓ Snapshot matches original hash');
}
```

## Build Audit Trails

Record who froze which snapshot when:

```javascript
async function freezeWithAudit(store, git, metadata) {
  const frozen = await freezeUniverse(store, git);

  // Record audit information
  const auditEntry = {
    snapshotId: frozen.snapshotId,
    gitRef: frozen.gitRef,
    hash: frozen.hash,
    timestamp: frozen.tNs,
    frozenBy: metadata.userId,
    reason: metadata.reason,
    freezeTime: new Date().toISOString(),
  };

  // Store audit metadata (could also be in a separate system)
  console.log('Audit entry:', auditEntry);

  return { frozen, audit: auditEntry };
}

// Usage
const { frozen, audit } = await freezeWithAudit(store, git, {
  userId: 'alice@example.com',
  reason: 'Monthly backup',
});

console.table(audit);
```

## Sync With Remote Repository

Store snapshots in a remote Git repository:

```javascript
async function pushSnapshot(frozen, localGit, remoteUrl) {
  // The snapshot is already in localGit
  // Now push to remote

  // Initialize remote tracking (one time)
  await localGit.addRemote('origin', remoteUrl);

  // Push the snapshot commit
  await localGit.push('origin', 'main');

  console.log('✓ Snapshot pushed to', remoteUrl);
}

// Usage
await pushSnapshot(frozen, git, 'https://github.com/org/snapshots.git');
```

## Pull and Verify Remote Snapshots

```javascript
async function pullAndVerifySnapshot(remoteUrl, snapshotRef, localGit, store) {
  // Pull from remote
  await localGit.fetchRemote(remoteUrl);

  // Read the snapshot
  const nquads = await localGit.readSnapshot(snapshotRef);

  // Verify integrity
  const frozen = { gitRef: snapshotRef, hash: /* ... */ };
  const isValid = await verifyReceipt(frozen, localGit, store);

  if (isValid) {
    console.log('✓ Remote snapshot verified');
    return nquads;
  } else {
    throw new Error('Remote snapshot verification failed');
  }
}
```

## Distributed Snapshots

Share snapshots across multiple systems:

```javascript
// System A: Create and push snapshot
const storeA = new KGCStore();
const gitA = new GitBackbone('./repo-a');

await storeA.appendEvent(/* ... */);
const frozen = await freezeUniverse(storeA, gitA);
await gitA.push('origin', 'main');

// System B: Pull and use snapshot
const storeB = new KGCStore();
const gitB = new GitBackbone('./repo-b');

await gitB.fetchRemote('https://origin/repo-a.git');
const reconstructed = await reconstructState(storeB, gitB, frozen.tNs);

// Both systems now have identical state at frozen.tNs
const queryResult = reconstructed.querySync(/* ... */);
```

## Manage Snapshots Across Time

Create a timeline of snapshots:

```javascript
class SnapshotTimeline {
  constructor(store, git) {
    this.store = store;
    this.git = git;
    this.snapshots = [];
  }

  async addSnapshot(metadata) {
    const frozen = await freezeUniverse(this.store, this.git);

    const entry = {
      id: frozen.snapshotId,
      timestamp: frozen.tNs,
      hash: frozen.hash,
      gitRef: frozen.gitRef,
      metadata,
      created: new Date(),
    };

    this.snapshots.push(entry);
    return entry;
  }

  async getSnapshotsInRange(startTime, endTime) {
    return this.snapshots.filter(s =>
      s.timestamp >= startTime && s.timestamp <= endTime
    );
  }

  async verifyAll() {
    const results = [];
    for (const snapshot of this.snapshots) {
      const isValid = await verifyReceipt(
        { gitRef: snapshot.gitRef, hash: snapshot.hash },
        this.git,
        this.store
      );
      results.push({ id: snapshot.id, valid: isValid });
    }
    return results;
  }
}

// Usage
const timeline = new SnapshotTimeline(store, git);

await timeline.addSnapshot({ reason: 'Daily backup' });
await timeline.addSnapshot({ reason: 'After migration' });

const allSnapshots = await timeline.getSnapshotsInRange(startTime, endTime);
const verification = await timeline.verifyAll();

console.table(verification);
```

## Cleanup and Maintenance

### Remove Old Snapshots

```javascript
async function pruneOldSnapshots(timeline, keepDays) {
  const cutoffTime = now() - BigInt(keepDays) * BigInt(86_400_000_000_000);

  const toDelete = timeline.snapshots.filter(s => s.timestamp < cutoffTime);

  for (const snapshot of toDelete) {
    // Mark snapshot for deletion (implementation depends on Git setup)
    console.log(`Removing snapshot ${snapshot.id}`);
    // await git.deleteSnapshot(snapshot.gitRef);
  }

  console.log(`✓ Removed ${toDelete.length} snapshots older than ${keepDays} days`);
}
```

### Check Repository Size

```javascript
async function getRepositorySize(git) {
  // Git repositories include all snapshot history
  // Snapshots accumulate over time
  const stats = await git.getStats();

  console.log('Repository statistics:');
  console.log(`  Total snapshots: ${stats.commitCount}`);
  console.log(`  Size: ${stats.size} bytes`);
  console.log(`  Avg snapshot: ${stats.size / stats.commitCount} bytes`);

  return stats;
}
```

## Backup Strategy

### Regular Snapshot Backups

```javascript
async function setupRegularBackups(store, git, intervalMs) {
  let lastBackup = now();

  setInterval(async () => {
    const timeSinceLastBackup = now() - lastBackup;

    // Only backup if changes were made
    const eventCount = await countRecentEvents(store);
    if (eventCount > 0) {
      const frozen = await freezeUniverse(store, git);
      console.log(`✓ Backup created: ${frozen.snapshotId}`);

      // Push to remote for redundancy
      await git.push('origin', 'main');
      console.log('✓ Backup pushed to remote');

      lastBackup = now();
    }
  }, intervalMs);
}

// Backup every 6 hours
setupRegularBackups(store, git, 6 * 60 * 60 * 1000);
```

### Multi-Region Backup

```javascript
async function backupToMultipleRegions(frozen, regions) {
  const results = [];

  for (const region of regions) {
    try {
      const remote = `https://backup-${region}.example.com/snapshots`;
      await git.push(remote, 'main');

      results.push({ region, status: 'success' });
      console.log(`✓ Backed up to ${region}`);
    } catch (error) {
      results.push({ region, status: 'failed', error: error.message });
      console.error(`✗ Backup to ${region} failed`);
    }
  }

  return results;
}

// Backup to multiple regions
const backups = await backupToMultipleRegions(frozen, [
  'us-east',
  'us-west',
  'eu-central',
]);

console.table(backups);
```

## Troubleshooting

**Q: Git repository is very large**
A: Snapshots accumulate. Consider pruning old snapshots or using shallow clones for recent history only.

**Q: Snapshot push fails with network error**
A: Retry with exponential backoff. Network issues are common with large snapshots.

**Q: Different snapshots from same state?**
A: Ensure the Universe graph export is deterministic. Use canonical N-Quads order.

**Q: How do I restore from an old snapshot?**
A: Use `reconstructState()` with the snapshot's timestamp to rebuild the entire state.

## Summary

- Use `GitBackbone` to store snapshots in Git
- Snapshots are immutable Git commits with BLAKE3 verification
- Push snapshots to remote repositories for backup and distribution
- Use audit trails to track who froze which snapshot when
- Implement regular backups and verification checks
