# Explanation: Architecture Rationale - Why Git Backing?

**Question:** Why use Git for snapshots instead of database BLOBs or file system?

---

## The Decision

KGC-4D uses **isomorphic-git** to store Universe snapshots as Git commits.

---

## Rationale

### 1. Version Control for Knowledge Graphs

Git provides:
- **History:** `git log` shows all snapshots
- **Diff:** Compare universes across time
- **Branches:** Parallel universe branches
- **Merge:** Combine forked universes

**Knowledge graphs as code.**

### 2. Distributed by Default

Git is inherently distributed:
- Clone to other machines
- Push to remote backups
- Pull updates from peers

**No central server required.**

### 3. Content-Addressable Storage

Git uses SHA-1 hashes for commits:
- **Deduplication:** Identical snapshots share storage
- **Integrity:** Corrupted commits detected automatically
- **Verifiable:** Commit hash proves content

**Storage efficiency + tamper detection.**

### 4. Standard Tooling

Leverage existing Git ecosystem:
- GitHub/GitLab for hosting
- Git hooks for automation
- CI/CD pipelines for validation

**Don't reinvent version control.**

---

## Tradeoffs

**Advantages:**
- Version control semantics (branch, merge, diff)
- Distributed replication (clone, push, pull)
- Content-addressable (dedup + integrity)
- Standard tooling (GitHub, Git CLI)
- Works in Node + Browser (isomorphic-git)

**Disadvantages:**
- Larger repo size than compressed DB (Git stores full snapshots)
- No native SPARQL indexing (need to load snapshot first)
- SHA-1 collision risk (theoretical, Git moving to SHA-256)

---

## Alternatives Considered

### 1. Database BLOBs

Store snapshots in PostgreSQL BYTEA or similar.

**Rejected:**
- No built-in versioning
- Not distributed
- Proprietary export format

### 2. Object Storage (S3, MinIO)

**Rejected:**
- No versioning semantics (just files)
- No merge/diff
- Requires cloud/server infrastructure

### 3. IPFS

**Considered:**
- Content-addressable ✅
- Distributed ✅
- But: No Git-like versioning primitives

**Rejected:** Git already provides everything IPFS does + version control.

---

## Implementation: isomorphic-git

**Why isomorphic-git?**
- **Pure JavaScript:** No Git CLI dependency
- **Cross-platform:** Works in Node + Browser
- **Lightweight:** ~200KB bundled
- **ARD compliant:** No external CLI tools

**Browser pattern:**
```javascript
import LightningFS from '@isomorphic-git/lightning-fs';
const fs = new LightningFS('kgc-repo');
const git = new GitBackbone('./repo', { fs });
```

**Node pattern:**
```javascript
import fs from 'fs';
const git = new GitBackbone('./repo', { fs });
```

**Same code, different FS backend.**

---

## Evidence

**Source:** `/home/user/unrdf/packages/kgc-4d/src/git.mjs`  
**ARD:** `/home/user/unrdf/packages/kgc-4d/docs/ARD.md` (Git requirement)  
**Tests:** `/home/user/unrdf/packages/kgc-4d/test/freeze.test.mjs` (Git integration)

---

## Related

- [Explanation 03: Zero-Information Invariant](./03-zero-info-invariant.md)
- [Reference: GitBackbone API](../reference/git-backbone-api.md)
