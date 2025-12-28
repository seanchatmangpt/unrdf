# Explanation: The Zero-Information Invariant Principle

**Principle:** The entire universe at any time is fully reconstructible from EventLog + Git snapshots alone. No external database required.

---

## The Invariant

**Zero-Information Invariant:**

Given:
- EventLog (immutable RDF events)
- Git repository (snapshots)

You can reconstruct:
- Universe at time T
- Any historical state
- Complete provenance

**Without:**
- External database
- Cache
- Index

---

## Why This Matters

### 1. Disaster Recovery

If Universe graph corrupted:
```bash
# Rebuild entire universe from scratch
git clone backup-repo.git
node rebuild-from-eventlog.mjs
# Result: Identical universe state (verifiable via BLAKE3 hash)
```

### 2. Portability

Move knowledge graph by copying:
- Git repo (snapshots)
- EventLog quads (RDF dump)

No proprietary database exports.

### 3. Auditability

Anyone can verify:
1. Fetch Git commit
2. Replay EventLog
3. Compare hash

**Trust is verifiable, not assumed.**

---

## How It Works

### Storage Model

```
EventLog (immutable):
  - Event 1: {type: CREATE, payload: {...}, deltas: [...]}
  - Event 2: {type: UPDATE, ...}
  - Event N: {type: SNAPSHOT, git_ref: "a1b2c3d4..."}

Git (snapshots):
  - Commit a1b2c3d4: Universe N-Quads at time T

Universe (derived):
  - Rebuilt from EventLog + Git
  - Can be deleted and reconstructed identically
```

**Universe is a materialized view, not source of truth.**

---

## Rationale

### Source of Truth: EventLog

Why EventLog, not Universe?

**Universe (mutable):**
- Can be corrupted
- Lost on disk failure
- Hard to audit

**EventLog (immutable):**
- Append-only (safer)
- Git-backed (distributed)
- SPARQL-queryable (auditable)

**Conclusion:** EventLog + Git = single source of truth.

---

## Evidence

**Source:** `/home/user/unrdf/packages/kgc-4d/src/freeze.mjs` (reconstruction)  
**ARD:** `/home/user/unrdf/packages/kgc-4d/docs/ARD.md` (zero-info principle)

---

## Related

- [Explanation 02: How Time Travel Works](./02-how-time-travel-works.md)
- [Explanation 04: Why Git Backing](./04-why-git-backing.md)
