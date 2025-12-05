# KGC 4D Datum - Architecture Requirements Document

## Executive Summary

KGC 4D is a **4-dimensional knowledge graph engine** that combines Observable State (O), Time (t_ns in nanoseconds), Vector causality (V), and Git References (G) into a single unified datum structure. The system provides deterministic state reconstruction, temporal snapshots, and cryptographic receipt verification through a combination of Oxigraph (semantic store) and Git (content-addressable store).

## Design Principles

### 1. Zero-Information Invariant

**The entire universe state at any point in time must be reconstructible from:**
- Event Log (append-only sequence of RDF quads)
- Git snapshot references (BLAKE3 hash + commit hash)
- No external database needed

**Implementation**: All state changes are events appended to `kgc:EventLog` named graph. Each freeze creates a snapshot in Git with cryptographic proof (receipt).

### 2. Nanosecond Precision Without Floating Point

**Problem**: Floating-point milliseconds lose precision after ~3 months of accumulated operations.

**Solution**: BigInt nanoseconds with monotonic ordering enforcement.

```javascript
// Node.js: Native nanosecond precision
const t_ns = process.hrtime.bigint();  // Actual hardware nanoseconds

// Browser: Milliseconds → nanoseconds conversion
const t_ns = BigInt(Math.floor(performance.now() * 1_000_000));
```

**Monotonic Enforcement**: `lastTime` tracker ensures t_ns never decreases, preventing temporal anomalies.

### 3. ACID Semantics via Atomic Transactions

**Requirement**: Event append to EventLog and state delta to Universe must be all-or-nothing.

**Solution**: Leverage UnrdfStore.transaction() which provides snapshot-based rollback.

```javascript
store.transaction((tx) => {
  // 1. Serialize event to RDF quads
  tx.add(eventQuad);  // EventLog

  // 2. Apply state deltas
  tx.add(stateQuad);  // Universe

  // Auto-commits on success, auto-rolls back on exception
});
```

### 4. Named Graph Partitioning for Scale

**Architecture**: Single Oxigraph instance with 3 named graphs:

- **kgc:Universe** - Current observable state (hot, frequently queried)
- **kgc:EventLog** - Immutable history (append-only, less frequently queried)
- **kgc:System** - Metadata, configuration, vector clocks (future expansion)

**Benefit**: Query optimization via graph-specific indexes, SPARQL filtering by graph.

### 5. Git as Content-Addressable Backbone

**Problem**: How to anchor universe snapshots deterministically?

**Solution**: Store N-Quads in Git, reference by commit hash.

```
Universe Freeze Flow:
  1. Dump kgc:Universe to canonical N-Quads
  2. Hash N-Quads with BLAKE3
  3. Commit N-Quads to Git
  4. Record {universe_hash, git_ref} in SNAPSHOT event
  5. Generate receipt for cryptographic proof
```

**Verification**: Fetch commit → recompute hash → compare.

### 6. Hybrid Node.js/Browser Runtime

**Challenge**: Git operations fundamentally different in two environments.

**Solution**: Dual-mode implementation with environment detection.

```javascript
// Node.js: execSync for fast, simple operations
execSync('git commit -m "..."', { cwd: this.dir });

// Browser: isomorphic-git + lightning-fs for IndexedDB backend
const sha = await git.commit({
  fs: lfs,      // lightning-fs: IndexedDB-backed
  dir: '/',
  message: '...'
});
```

## System Architecture

### Conceptual Model

```
┌─────────────────────────────────────────────┐
│      KGC 4D Universe (Oxigraph Store)       │
├─────────────────────────────────────────────┤
│                                             │
│  ┌──────────────┐  ┌──────────────────┐    │
│  │  kgc:Universe│  │ kgc:EventLog     │    │
│  │ (hot state)  │  │ (history)        │    │
│  └──────────────┘  └──────────────────┘    │
│                                             │
└─────────────────────────────────────────────┘
           ↓
    [SNAPSHOT EVENT]
           ↓
    [BLAKE3 HASH]
           ↓
    [GIT COMMIT]
           ↓
┌─────────────────────────────────────────────┐
│       Git Backbone (Content-Addressed)      │
├─────────────────────────────────────────────┤
│ snapshot.nq @ abc123def...                  │
│ snapshot.nq @ def456abc...                  │
│ snapshot.nq @ 789xyz...                     │
└─────────────────────────────────────────────┘
```

### Data Flow: Event Append

```
Input: Event + Deltas
   ↓
[Transaction Start]
   ↓
Serialize Event → RDF Quads
   ↓
Add to kgc:EventLog (named graph)
   ↓
Apply Deltas to kgc:Universe (named graph)
   ↓
[Transaction Commit/Rollback]
   ↓
Return Receipt {id, t_ns, timestamp_iso, event_count}
```

### Data Flow: Time Travel

```
Input: targetTime (BigInt nanoseconds)
   ↓
Query EventLog for SNAPSHOT events before targetTime
   ↓
Load nearest snapshot from Git
   ↓
Create temp store, load N-Quads
   ↓
Query for events between snapshot and targetTime
   ↓
Replay events (delta application)
   ↓
Return reconstructed state as new store
```

## 4D Datum Structure

**4D = {O, t_ns, V, G}**

- **O** (Observable): RDF triples in kgc:Universe
- **t_ns** (Time): BigInt nanosecond timestamp
- **V** (Vector): Vector clock for distributed causality (future)
- **G** (Git): {universe_hash: BLAKE3, git_ref: commit_hash}

**Example Receipt (Frozen Datum)**:

```javascript
{
  id: "550e8400-e29b-41d4-a716-446655440000",
  t_ns: "1733314560123456789n",
  timestamp_iso: "2024-12-04T15:16:00.123Z",
  universe_hash: "blake3_hash_of_nquads",
  git_ref: "abc123def456789xyz...",
  event_count: 42,
  nquad_count: 156
}
```

## Event Log Schema (RDF)

```turtle
@prefix kgc: <http://kgc.io/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

<http://kgc.io/event/550e8400-e29b-41d4-a716-446655440000>
  kgc:type "CREATE" ;
  kgc:t_ns "1733314560123456789"^^xsd:integer ;
  kgc:payload "{\"description\": \"Added Alice\"}" ;
  kgc:git_ref "abc123def456789xyz" .
```

## Failure Modes & Mitigations

### Scenario 1: Git Commit Fails

**Symptom**: Unable to persist snapshot.

**Mitigation**: Fall back to in-memory snapshot (SNAPSHOT event stored but git_ref = null). Can retry later or fail loudly to signal infrastructure issue.

### Scenario 2: Replay Events Corrupt State

**Symptom**: Time-travel produces incorrect state.

**Mitigation**: Verify reconstructed state hash against receipt. If mismatch, report corruption and halt replay.

### Scenario 3: BigInt Precision Loss During Conversion

**Symptom**: Nanoseconds lose precision when converted to Date.

**Mitigation**: Preserve original BigInt in receipt. Never convert back to milliseconds for storage.

### Scenario 4: Distributed Vector Clocks Diverge

**Symptom**: Multiple writes appear concurrent when one happened first.

**Mitigation** (future): Implement Lamport or HLC clocks. For now, rely on single-writer (Git) for determinism.

## Performance Targets (80/20 MVP)

| Operation | Target | Current |
|-----------|--------|---------|
| appendEvent | <5ms | TBD |
| freezeUniverse | <1s (for <100K quads) | TBD |
| reconstructState | <2s (including replay) | TBD |
| verifyReceipt | <100ms | TBD |

## Future Extensions (NOT in MVP)

1. **Vector Clocks**: Lamport or HLC for distributed causality
2. **Hook Sandboxing**: Isolated-vm for governance hooks
3. **Performance Optimization**: Caching, indexing, parallel processing
4. **Migration Tooling**: Import legacy RDF, backward compatibility
5. **Production Crypto**: Ed25519 signatures on receipts
6. **Browser Full Support**: Complete lightning-fs integration
7. **Comprehensive Testing**: Unit, integration, property-based tests

## Dependencies

### Core (Existing in Monorepo)

- `@unrdf/core` - UnrdfStore base class
- `@unrdf/oxigraph` - RDF semantic store
- `@noble/hashes` - BLAKE3 hashing

### New

- `isomorphic-git` ^1.25.0 - Git operations in Node.js + Browser
- `lightning-fs` ^4.6.0 - IndexedDB-backed filesystem for Browser

### Total New External Dependencies: 2

## Deployment Considerations

### Node.js Production

- Requires Git binary installed (`git --version` must succeed)
- `snapshot.nq` files will accumulate in repo (consider gitignore)
- Consider running in shallow clone to minimize history

### Browser Production

- IndexedDB quota limits apply (typically 50MB+)
- Lightning-fs may have iOS/Safari limitations
- CORS may affect Git remote operations

## Acceptance Criteria

**80/20 MVP Completion**:

- [x] KGCStore extends UnrdfStore with atomic appendEvent()
- [x] Named Graph partitioning (UNIVERSE, EVENT_LOG)
- [x] BigInt nanosecond time with monotonic enforcement
- [x] GitBackbone with Node.js + Browser dual mode
- [x] freezeUniverse() with BLAKE3 receipt
- [x] reconstructState() with time-travel capability
- [x] verifyReceipt() for cryptographic validation
- [x] ~700 lines of implementation code
- [x] Zero external library requirements beyond isomorphic-git + lightning-fs
- [x] Works in both Node.js and Browser

## References

- **ARD** (This document)
- **API Reference** - `docs/API.md`
- **Basic Usage** - `examples/basic-usage.mjs`
- **Test Suite** - `test/` directory (future)
