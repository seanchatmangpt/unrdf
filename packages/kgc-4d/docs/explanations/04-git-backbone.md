# Git as Immutable History

This explanation covers why KGC 4D uses Git for snapshot storage and how it enables cryptographic verification.

## Why Not Just a Database?

### The Problem with Database Checksums

```
Traditional approach:
  Snapshot stored in database
  Checksum computed: hash = SHA256(state)
  Checksum stored in same database

Problem: If database is compromised:
  Attacker can modify snapshot AND checksum
  Checksum still matches (now worthless)
  No external verification possible

Example:
  Original: snapshot.data = "Alice: $100"
           checksum = abc123...

  Hacked:   snapshot.data = "Alice: $1000000"
           checksum = xyz789... (updated too)

  You can't detect the hack!
```

### The Git Solution

```
Git approach:
  Snapshot exported to N-Quads
  Snapshot committed to Git
  Git commit hash is deterministic

Git properties:
  1. Immutable commits - Can't change history
  2. Content-addressed - Hash identifies content
  3. Distributed - Anyone can have full history
  4. Auditable - All changes visible in log

Benefits:
  1. Verification is external
  2. Changing snapshot requires changing Git
  3. Changing Git requires updating all copies
  4. Tampering is immediately visible
```

## Content-Addressed Storage

### How Git Works

Every Git commit is identified by a hash:

```
Commit content:
  Tree: (list of file references)
  Author: "KGC 4D"
  Timestamp: "2023-12-05"
  Message: "Snapshot"
  Parent: (previous commit)

Commit hash = SHA1(entire commit object)
           = abc123def456...

Property: Same content always produces same hash
         Different content always produces different hash
```

### Content-Addressed in KGC 4D

```
Snapshot N-Quads:
  <http://example.org/alice> ...
  <http://example.org/bob> ...
  ...

BLAKE3 hash:
  hash = blake3(nquads_string)
       = a1b2c3d4e5f6...

Git commit:
  Message: hash = a1b2c3d4e5f6...
  Content: nquads_string
  Commit hash: abc123...

Verification:
  1. Get snapshot from Git
  2. Recompute BLAKE3 hash
  3. Compare with stored hash
  4. If different: snapshot was modified!
```

## BLAKE3 Hashing

### Why BLAKE3?

```
Cryptographic hash requirements:
  1. Collision-resistant - Can't find two inputs with same hash
  2. Pre-image resistant - Can't create input from hash
  3. Fast - Computable efficiently

Historical options:
  MD5: Broken (deliberate collisions possible)
  SHA1: Weakened (feasible collision attacks)
  SHA-256: Secure (but slower)
  BLAKE3: Secure + fast (3x faster than SHA-256)
```

### BLAKE3 Properties

```
Input: Arbitrary data (e.g., N-Quads file)
Output: 256-bit hash (32 bytes, 64 hex characters)

Example:
  Input: "<http://example.org/alice> ..."
  Output: a1b2c3d4e5f6g7h8i9j0k1l2m3n4o5p6q7r8s9t0u1v2w3x4y5z6

Deterministic:
  Same input → same output (always)
  Different input → different output (with overwhelming probability)

Cryptographic:
  - Can't reverse engineer input from hash
  - Can't find collision (two inputs with same hash)
  - Can't modify data without hash changing
```

## Verification Workflow

### Creating a Snapshot

```
Step 1: Freeze Universe
  store.querySync("SELECT ?s ?p ?o WHERE ...")
  → Export to canonical N-Quads format

Step 2: Compute BLAKE3 Hash
  hash = blake3(nquads)
  → a1b2c3d4e5f6...

Step 3: Create Git Commit
  git commit -m "Snapshot" --allow-empty
  → Commit hash: abc123...
  → Git stores N-Quads file

Step 4: Record Receipt
  frozen = {
    snapshotId: "uuid",
    hash: "a1b2c3d4e5f6...",
    gitRef: "abc123...",
    tNs: BigInt nanoseconds
  }

Result: Snapshot stored in Git with cryptographic proof
```

### Verifying a Snapshot

```
Step 1: Retrieve Git Commit
  nquads = git.readSnapshot(frozen.gitRef)
  → Read actual N-Quads from Git

Step 2: Recompute BLAKE3 Hash
  recomputed = blake3(nquads)
  → Compute on current hardware

Step 3: Compare Hashes
  if (recomputed === frozen.hash) {
    ✓ Snapshot is authentic and unchanged
  } else {
    ✗ Snapshot has been tampered with!
  }

Result: Cryptographic proof without trusting anyone
```

## Git Integrity

### Immutable History

```
Git commit structure:
  commit abc123...
    tree: (files)
    parent: def456...  ← Points to previous commit
    message: "Snapshot"

To change a commit:
  1. Modify the commit object
  2. Recompute hash (now different)
  3. Update parent pointer in next commit
  4. Now ALL subsequent commits have wrong hashes

Result: Changing history is immediately visible
        All commit hashes downstream change
        Impossible to hide modification
```

### Distributed Verification

```
You have snapshot in Git
You send snapshot to partner via email/network
Partner receives: N-Quads file + hash

Partner verifies:
  1. Recompute hash from N-Quads
  2. Compare with your provided hash
  3. No need to contact you
  4. No need to trust transmission

If hash matches: proof that N-Quads is authentic
```

## Snapshot Storage Format

### N-Quads Format

```
RDF triples exported as N-Quads (one quad per line):

<http://example.org/alice> <http://example.org/hasName> "Alicia" <kgc:Universe> .
<http://example.org/alice> <http://example.org/hasAge> "30"^^<http://www.w3.org/2001/XMLSchema#integer> <kgc:Universe> .
<http://example.org/bob> <http://example.org/hasName> "Robert" <kgc:Universe> .
...

Properties:
  - Plain text (human-readable)
  - Deterministic order (reproducible)
  - Self-contained (no external references)
  - Standard format (usable by any RDF tool)
```

### Canonical Ordering

Determinism requires canonical order:

```
Lexicographic sort:
  1. Sort all subjects
  2. For each subject, sort predicates
  3. For each predicate, sort objects
  4. For each object, sort graphs

Result: Same triples always export identically
       Same BLAKE3 hash
       Reproducible verification
```

## Comparison: Other Approaches

### Blockchain

```
Blockchain:
  - Distributed ledger
  - Cryptographically linked blocks
  - Consensus mechanism (Proof of Work, etc.)
  - Very slow and expensive

KGC 4D:
  - Git (already distributed if cloned)
  - BLAKE3 hashing
  - No consensus needed
  - Fast and free

Result: KGC 4D gets similar security without blockchain overhead
```

### Merkle Trees

```
Merkle Tree:
  - Hash of hashes forming tree structure
  - Allows proving one element's membership
  - Used in blockchains and file systems

KGC 4D:
  - Git already uses SHA1 (forms Merkle DAG)
  - BLAKE3 of entire snapshot (simpler)
  - Different use case

Result: Git's built-in Merkle structure sufficient
       Simpler model
```

### Database with Signatures

```
Database + signing key:
  - Store state in database
  - Sign each state with private key
  - Others verify with public key

Problem: Requires key management
         Keys can be compromised
         Single point of failure if key lost

Git + BLAKE3:
  - No key management
  - Verification via recomputation (always works)
  - Decentralized (anyone can verify)

Result: Git is simpler and more robust
```

## Practical Implementation

### Reading a Snapshot from Git

```javascript
import { GitBackbone } from '@unrdf/kgc-4d';

const git = new GitBackbone('./snapshots');

// Get N-Quads from Git
const nquads = await git.readSnapshot('abc123...');

// Result is raw N-Quads:
// <http://example.org/alice> ...
// <http://example.org/bob> ...
```

### Verifying with BLAKE3

```javascript
import { blake3 } from 'hash-wasm';

const nquads = await git.readSnapshot(gitRef);
const recomputed = await blake3(nquads);

if (recomputed === frozen.hash) {
  console.log('✓ Authentic');
} else {
  console.log('✗ Tampered!');
}
```

### Rebuilding from Snapshot

```javascript
async function rebuildFromSnapshot(frozen, git) {
  // 1. Verify integrity
  const isValid = await verifyReceipt(frozen, git, store);
  if (!isValid) throw new Error('Snapshot verification failed');

  // 2. Get N-Quads
  const nquads = await git.readSnapshot(frozen.gitRef);

  // 3. Parse and load into new store
  const newStore = new KGCStore();
  // (parse N-Quads and add quads to newStore)

  return newStore;
}
```

## Security Considerations

### What Git Verification Protects Against

```
✓ Detects if snapshot was modified after creation
✓ Detects if N-Quads file was corrupted
✓ Proves snapshot came from authoritative source
✓ Works even if Git server is compromised (verify locally)

✗ Doesn't prevent deletion of Git commits (can happen locally)
✗ Doesn't authenticate the creator (only verifies content)
✗ Doesn't prove who created the snapshot
```

### Signed Commits (Optional)

For authentication, use Git signed commits:

```javascript
// Sign commit with GPG key
git commit --gpg-sign -m "Snapshot"

// Others can verify:
// git verify-commit abc123...
// Proves creator's identity (if they have public key)
```

## Distributed Verification

### Clone and Verify

```
Scenario: You publish snapshots to GitHub
         Partner clones repository
         Partner verifies snapshot locally

Partner:
  git clone https://github.com/you/snapshots.git
  git log --oneline  # See commit history
  git show abc123    # See snapshot content

  # Recompute hash
  blake3(snapshot-file) === stored-hash?

  Result: ✓ Verified without contacting original source
```

### Multi-Party Verification

```
Snapshot created by System A
Snapshot sent to System B
System B sends to System C

Each step:
  Recompute BLAKE3 hash
  Verify match

If ANY system modifies snapshot:
  Hash changes
  Mismatch detected
  Downstream systems reject it

Result: Tampering detected regardless of channel
        No trust in network required
```

## Summary

- **Git** provides immutable, distributed history
- **BLAKE3** provides cryptographic verification
- **Content-addressed** storage means hash identifies content
- **Verification is external** - can verify without trusting creator
- **Distributed** - anyone can clone and verify
- **Simple** - no complex infrastructure needed
- **Proven** - Git is battle-tested for 15+ years
