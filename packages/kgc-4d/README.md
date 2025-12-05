# KGC 4D Engine

A **4-dimensional knowledge graph engine** combining Observable State, nanosecond-precision Time, Vector causality, and Git References into a unified datum structure.

## Features

- 🕐 **Nanosecond Precision**: BigInt timestamps with monotonic ordering (no floating-point loss)
- ⏸️ **Universe Freeze**: Create deterministic snapshots with BLAKE3 hashing and Git backing
- ⏰ **Time Travel**: Reconstruct state at any historical point via snapshot + event replay
- 🔐 **Cryptographic Receipts**: Verify frozen states with hash-based proofs
- 🔄 **ACID Semantics**: Atomic event append via transaction snapshots
- 📊 **RDF Queries**: SPARQL queries on both Universe (hot) and EventLog (history)
- 🌐 **Dual Runtime**: Works in Node.js (native nanoseconds) and Browser (IndexedDB)

## Quick Start

### Installation

```bash
# Install as workspace dependency
pnpm add @unrdf/kgc-4d

# Or directly
npm install @unrdf/kgc-4d
```

### Basic Usage

```javascript
import {
  KGCStore,
  GitBackbone,
  freezeUniverse,
  reconstructState,
  EVENT_TYPES
} from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

// 1. Initialize
const store = new KGCStore();
const git = new GitBackbone('./my-repo');

// 2. Add RDF data
const alice = dataFactory.namedNode('http://example.org/Alice');
const rdfType = dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
const person = dataFactory.namedNode('http://example.org/Person');

const aliceQuad = dataFactory.quad(alice, rdfType, person);

// 3. Append event atomically
const receipt = await store.appendEvent(
  {
    type: EVENT_TYPES.CREATE,
    payload: { description: 'Added Alice to universe' }
  },
  [{ type: 'add', ...aliceQuad }]
);

console.log(`Event appended at ${receipt.receipt.timestamp_iso}`);

// 4. Freeze universe to Git
const frozen = await freezeUniverse(store, git);
console.log(`Frozen at: ${frozen.timestamp_iso}`);
console.log(`Git commit: ${frozen.git_ref}`);
console.log(`Universe hash: ${frozen.universe_hash}`);

// 5. Modify state
const bob = dataFactory.namedNode('http://example.org/Bob');
const bobQuad = dataFactory.quad(bob, rdfType, person);

await store.appendEvent(
  { type: EVENT_TYPES.CREATE, payload: { description: 'Added Bob' } },
  [{ type: 'add', ...bobQuad }]
);

// 6. Time travel back to frozen state
const targetTime = BigInt(frozen.t_ns);
const pastStore = await reconstructState(store, git, targetTime);

// pastStore now contains only Alice (state from frozen time)
```

## Core Concepts

### Named Graphs

Single Oxigraph store with 3 logical partitions:

- **kgc:Universe** - Current observable state (hot)
- **kgc:EventLog** - Immutable event history (append-only)
- **kgc:System** - Metadata, configuration (future)

### Event Log

All state changes are immutable events in RDF. Query with SPARQL:

```javascript
const events = await store.queryEventLog(`
  SELECT ?event ?type ?timestamp WHERE {
    GRAPH <http://kgc.io/EventLog> {
      ?event <http://kgc.io/type> ?type ;
             <http://kgc.io/t_ns> ?t_ns .
    }
  }
  ORDER BY ?t_ns
`);
```

### Freeze & Receipt

A frozen receipt proves the exact state at a specific time:

```javascript
{
  id: "550e8400-e29b-41d4-a716-446655440000",
  t_ns: "1733314560123456789",
  timestamp_iso: "2024-12-04T15:16:00.123Z",
  universe_hash: "blake3_hash_of_nquads",
  git_ref: "abc123def456789xyz",
  event_count: 42,
  nquad_count: 156
}
```

**Verification**: Fetch Git commit → recompute BLAKE3 hash → compare.

### Time Travel

Reconstruct state at any historical point:

```javascript
const pastStore = await reconstructState(store, git, targetTime);
// Returns new KGCStore with state from targetTime
// Loaded from snapshot + replayed events
```

## API Overview

### Classes

- **KGCStore** - Extended UnrdfStore with `appendEvent()`, `queryEventLog()`, `queryUniverse()`
- **GitBackbone** - Git operations with `commitSnapshot()`, `readSnapshot()`

### Functions

- **freezeUniverse(store, gitBackbone)** - Create snapshot with receipt
- **reconstructState(store, gitBackbone, targetTime)** - Time-travel to past state
- **verifyReceipt(receipt, gitBackbone, store)** - Cryptographic verification
- **now()** - Get BigInt nanoseconds
- **toISO(t_ns)** - Convert to ISO 8601
- **fromISO(iso)** - Parse ISO 8601
- **addNanoseconds(t_ns, delta)** - BigInt arithmetic
- **duration(start_ns, end_ns)** - Calculate elapsed time

See [docs/API.md](docs/API.md) for complete reference.

## Examples

### Example 1: Basic Freeze

```bash
node examples/basic-usage.mjs
```

Output:
```
=== Example 1: Basic Freeze ===

📝 Adding Alice to universe...
  ✓ Event appended at 2024-12-04T15:16:00.123Z
  ✓ Event count: 1

❄️  Freezing universe...
  ✓ Frozen at: 2024-12-04T15:16:00.124Z
  ✓ Universe hash: blake3_hash_...
  ✓ Git commit: abc123def...
  ✓ N-Quads count: 5
```

### Example 2: Time Travel

See `examples/basic-usage.mjs` for full multi-event workflow with snapshots.

## Architecture

### Zero-Information Invariant

**The entire universe at any time is reconstructible from:**
- Event Log (RDF quads in kgc:EventLog)
- Git snapshots (N-Quads files)
- No external database required

### Time Model

**BigInt Nanoseconds** (not floating-point milliseconds):

```javascript
// Node.js: True nanosecond precision
const t_ns = process.hrtime.bigint();  // Hardware nanoseconds

// Browser: Milliseconds → nanoseconds
const t_ns = BigInt(Math.floor(performance.now() * 1_000_000));
```

**Monotonic Ordering**: t_ns never goes backward (enforced by `lastTime` tracking).

### Transactions

Events and state deltas are atomic via snapshot-based rollback:

```javascript
store.transaction((tx) => {
  tx.add(eventQuad);    // Add to EventLog
  tx.add(stateQuad);    // Add to Universe
  // Auto-commits on success, auto-rolls back on exception
});
```

### Git Backbone

Snapshots stored in Git, referenced by commit hash:

```
Universe Freeze:
  1. Dump kgc:Universe to N-Quads
  2. Hash with BLAKE3
  3. Commit to Git
  4. Record {hash, git_ref} in SNAPSHOT event
```

## Performance

| Operation | Target |
|-----------|--------|
| appendEvent | <5ms |
| freezeUniverse | <1s (for <100K quads) |
| reconstructState | <2s (includes replay) |
| verifyReceipt | <100ms |

(Benchmarks in progress)

## Environment Support

### Node.js

- ✅ Native `process.hrtime.bigint()` for true nanoseconds
- ✅ Git CLI via `execSync` (fast, simple)
- ✅ File system for snapshots

### Browser

- ⚠️ `performance.now() * 1_000_000` (millisecond approximation)
- ⚠️ `isomorphic-git` + `lightning-fs` (IndexedDB backend)
- ⚠️ Slower, but works offline

## Future Extensions (Not in MVP)

- Vector clocks for distributed causality
- Advanced hook sandboxing (isolated-vm)
- Performance optimization (caching, indexing)
- Migration tooling for legacy RDF
- Ed25519 signatures on receipts
- Comprehensive test suite
- CI/CD pipelines

## Dependencies

### Core (Monorepo)

- `@unrdf/core` - UnrdfStore base
- `@unrdf/oxigraph` - RDF semantic store
- `@noble/hashes` - BLAKE3

### New

- `isomorphic-git` ^1.25.0 - Git in Node/Browser
- `lightning-fs` ^4.6.0 - IndexedDB-backed FS

**Total new external deps: 2**

## Documentation

- [ARD.md](docs/ARD.md) - Architecture requirements & design principles
- [API.md](docs/API.md) - Complete API reference
- [examples/basic-usage.mjs](examples/basic-usage.mjs) - Working examples

## Testing

(Test suite in progress)

```bash
# Run tests
npm test

# Run specific test
npm test -- test/freeze.test.mjs
```

## License

MIT

## Contributing

See main UNRDF project contribution guidelines.

---

**Status**: 🚀 80/20 MVP (Nanosecond Time, Event Sourcing, Freeze/Replay)

**Not Included**: Vector clocks, advanced hooks, comprehensive tests, production crypto

**Quick Start**: See [examples/basic-usage.mjs](examples/basic-usage.mjs)
