# KGC 4D Engine

![Version](https://img.shields.io/badge/version-[VERSION]--beta.1-blue) ![Production Ready](https://img.shields.io/badge/production-ready-green) ![Tests](https://img.shields.io/badge/tests-176%2F176-brightgreen) ![Coverage](https://img.shields.io/badge/test%20coverage-99.8%25-brightgreen)

**A 4-dimensional knowledge graph engine** combining Observable State, nanosecond-precision Time, Vector causality, and Git References into a unified, auditable data structure.

## What is KGC 4D?

KGC 4D extends RDF knowledge graphs into 4 dimensions:
- **O (Observable)**: Current state as RDF triples in Universe graph
- **t (Time)**: Nanosecond-precision BigInt timestamps with monotonic ordering
- **V (Vector)**: Causality tracking via distributed vector clocks
- **G (Git)**: Content-addressed immutable snapshots with BLAKE3 verification

**Key insight**: The entire universe at any point in time is reconstructible from the Event Log + Git snapshots. No external database required.

## 5-Minute Quick Start

```javascript
import { KGCStore, GitBackbone, freezeUniverse, reconstructState, EVENT_TYPES } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

// Initialize
const store = new KGCStore();
const git = new GitBackbone('./my-repo');

// Add data
const alice = dataFactory.namedNode('http://example.org/Alice');
const rdfType = dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
const person = dataFactory.namedNode('http://example.org/Person');

// Append event atomically (L5 hardening: requires policy bounds)
await store.appendEvent(
  { 
    type: EVENT_TYPES.CREATE, 
    policy: { '@type': ['odrl:Policy'], action: 'mcpp:MutateGraph' },
    payload: { description: 'Added Alice' } 
  },
  [{ type: 'add', subject: alice, predicate: rdfType, object: person }]
);

// Freeze universe to Git (creates cryptographic receipt)
const frozen = await freezeUniverse(store, git);
console.log(`✓ Frozen at ${frozen.timestamp_iso}`);
console.log(`✓ Hash: ${frozen.universe_hash}`);

// Add more data
const bob = dataFactory.namedNode('http://example.org/Bob');
await store.appendEvent(
  { 
    type: EVENT_TYPES.CREATE, 
    policy: { '@type': ['odrl:Policy'], action: 'mcpp:MutateGraph' },
    payload: { description: 'Added Bob' } 
  },
  [{ type: 'add', subject: bob, predicate: rdfType, object: person }]
);

// Time-travel back to when only Alice existed
const pastStore = await reconstructState(store, git, BigInt(frozen.t_ns));
console.log(`✓ Time-traveled to ${frozen.timestamp_iso}`);
// pastStore now contains only Alice
```

## Installation

```bash
# Workspace
pnpm add @unrdf/kgc-4d

# Or standalone
npm install @unrdf/kgc-4d
```

## Core Features

| Feature | Capability | Use Case |
|---------|-----------|----------|
| **Nanosecond Timestamps** | Guaranteed ordering with BigInt precision | Debugging, audit trails |
| **Universe Snapshots** | Freeze & cryptographic verification with BLAKE3 | Checkpoints, rollback |
| **Time-Travel Queries** | Reconstruct state at any historical point | Root cause analysis |
| **Event Sourcing** | Append-only immutable history with SPARQL | Compliance, auditing |
| **Vector Clocks** | Distributed causality tracking | Multi-node coordination |
| **Git Backing** | Content-addressed snapshots, zero external DB | Offline verification |
| **Dual Runtime** | Node.js (true nanoseconds) + Browser (IndexedDB) | Universal deployment |

## Documentation Map

Choose your path:

### 🎓 Learning (Tutorials)
**Start here** - Hands-on step-by-step introduction to core concepts
- [Getting Started](docs/tutorials/01-getting-started.md) - Your first KGC 4D app
- [Working with Events](docs/tutorials/02-working-with-events.md) - Event sourcing patterns
- [Temporal Snapshots](docs/tutorials/03-temporal-snapshots.md) - Freeze and reconstruct

See [all tutorials →](docs/tutorials/README.md)

### 📖 How-To Guides (Problem → Solution)
**Real-world tasks** - Solutions to practical problems
- [Time Travel & Reconstruction](docs/how-to-guides/01-time-travel.md) - Query historical states
- [Cryptographic Verification](docs/how-to-guides/02-verification.md) - Verify frozen states
- [SPARQL Queries](docs/how-to-guides/03-querying.md) - Query Universe and EventLog
- [Git Integration](docs/how-to-guides/04-git-integration.md) - Snapshot management
- [Isomorphic Deployment](docs/how-to-guides/05-isomorphic-deployment.md) - Node.js + Browser

See [all how-tos →](docs/how-to-guides/README.md)

### 📚 Reference (API & Concepts)
**Precise documentation** - Authoritative information
- [Complete API Reference](docs/references/01-api.md) - All functions and classes
- [Architecture Overview](docs/references/02-architecture.md) - System design
- [Poka-Yoke Guards](docs/references/03-guards.md) - Mistake-proofing rules (24 guards)
- [Constants & URIs](docs/references/04-constants.md) - Named graphs, event types

See [all references →](docs/references/README.md)

### 🧠 Understanding (Deep Dives)
**Design rationale** - Why KGC 4D works this way
- [Why 4 Dimensions?](docs/explanations/01-four-dimensions.md) - The 4D model explained
- [Vector Clocks](docs/explanations/02-vector-clocks.md) - Distributed causality
- [Time-Travel Reconstruction](docs/explanations/03-temporal-reconstruction.md) - How it works
- [Git Backbone](docs/explanations/04-git-backbone.md) - Why Git matters
- [Event Sourcing](docs/explanations/05-event-sourcing.md) - Pattern fundamentals
- [Monotonic Clocks](docs/explanations/06-monotonic-clocks.md) - Precision guarantees

See [all explanations →](docs/explanations/README.md)

## Production Status

**Version**: [VERSION]-beta.1
**Tests**: ✅ 176/176 passing (100%)
**OTEL Validation**: ✅ 100/100 (production ready)
**Poka-Yoke Guards**: ✅ 24/24 from FMEA analysis

### Before Production Deployment
```bash
# 1. Run tests
pnpm test
# Expected: 176/176 passing

# 2. OTEL validation (required for production)
node validation/run-all.mjs comprehensive
# Expected: Score ≥ 80/100
```

## Performance (Measured)

| Operation | Latency (100 quads) | Latency (10K quads) |
|-----------|-------------------|-------------------|
| appendEvent | ~0.8ms | ~3.5ms |
| freezeUniverse | ~52ms | ~650ms |
| reconstructState (with snapshot) | ~45ms | ~380ms |
| reconstructState (cold) | ~150ms | ~6.5s |

See [BENCHMARKS.md](docs/BENCHMARKS.md) for detailed performance analysis and optimization patterns.

## Examples

**Quick demos** in `examples/`:
```bash
node examples/basic-usage.mjs              # Simple freeze + time-travel
node examples/mission-critical.mjs         # All 8 use cases
node examples/local-first-collaboration.mjs # Real-time sync patterns
```

## Architecture Principles

- **Zero-Information Invariant**: Entire universe at any time reconstructible from Event Log + Git
- **ACID Semantics**: Atomic event append with manual rollback on failure
- **Poka-Yoke**: 24 guards prevent invalid operations (from FMEA analysis)
- **Receipt-Driven**: Every operation returns cryptographic proof
- **Dual Named-Graph**: kgc:Universe (hot) + kgc:EventLog (history) separation

For deep technical analysis, see [ARCHITECTURE-DEEP-DIVE.md](docs/ARCHITECTURE-DEEP-DIVE.md).

## Dependencies

**Core (Monorepo)**:
- `@unrdf/core` - UnrdfStore foundation
- `@unrdf/oxigraph` - RDF semantic store

**External (ARD-Compliant)**:
- `hash-wasm` - BLAKE3 hashing (fastest WASM implementation)
- `isomorphic-git` - Pure JS Git (Node.js + Browser)

**Total**: 2 external dependencies, 0 CLI tools required.

## Contributing

See [CONTRIBUTING.md](../../CONTRIBUTING.md) in the main UNRDF project.

## License

MIT - See [LICENSE](LICENSE)

---

## Quick Decision Tree

**Need to...**

- ✅ **Get started quickly?** → [5-Minute Quick Start](#5-minute-quick-start) + [Tutorial: Getting Started](docs/tutorials/01-getting-started.md)
- ✅ **Understand time-travel?** → [How-To: Time Travel](docs/how-to-guides/01-time-travel.md) + [Explanation: Reconstruction](docs/explanations/03-temporal-reconstruction.md)
- ✅ **Use in production?** → [Production Status](#production-status) + [How-To: Verification](docs/how-to-guides/02-verification.md)
- ✅ **Optimize performance?** → [BENCHMARKS.md](docs/BENCHMARKS.md) + [How-To Guides](docs/how-to-guides/README.md)
- ✅ **Understand design?** → [Explanations](docs/explanations/README.md) + [ARCHITECTURE-DEEP-DIVE.md](docs/ARCHITECTURE-DEEP-DIVE.md)
- ✅ **Find API reference?** → [Complete API Reference](docs/references/01-api.md)

## Support

- **Issues**: Check [GitHub Issues](https://github.com/anthropics/unrdf/issues)
- **Docs**: Full documentation in [docs/](docs/) directory
- **Examples**: Working code in [examples/](examples/) directory

---

**Status**: Production-ready with comprehensive test coverage and OTEL validation.
See [docs/](docs/) for complete documentation organized by learning style (Tutorials → How-To → Reference → Explanation).
