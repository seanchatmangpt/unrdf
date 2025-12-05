# KGC 4D Documentation

Welcome to the KGC 4D (Knowledge Graph with Causality in 4 Dimensions) documentation. This guide is organized in four sections following the Diataxis framework:

## 📚 Documentation Sections

### [Tutorials](./tutorials/index.md)
**Learning-oriented guides** that teach you the fundamentals of KGC 4D by doing.

- **[Getting Started](./tutorials/01-getting-started.md)** - Set up your first KGC 4D store and perform basic operations
- **[Working with Events](./tutorials/02-working-with-events.md)** - Learn how to create, append, and manage events
- **[Temporal Snapshots](./tutorials/03-temporal-snapshots.md)** - Create and manage frozen universe snapshots

### [How-To Guides](./how-to-guides/index.md)
**Problem-oriented guides** for accomplishing specific tasks.

- **[Reconstruct State at a Point in Time](./how-to-guides/01-time-travel.md)** - Travel back to any moment in your knowledge graph's history
- **[Verify Snapshots Cryptographically](./how-to-guides/02-verification.md)** - Ensure integrity and authenticity of frozen universes
- **[Query Your Knowledge Graph](./how-to-guides/03-querying.md)** - Use SPARQL to explore events and state
- **[Integrate with Git](./how-to-guides/04-git-integration.md)** - Store snapshots and verify history with Git
- **[Distribute Across Node.js and Browser](./how-to-guides/05-isomorphic-deployment.md)** - Deploy KGC 4D in both server and client environments

### [References](./references/index.md)
**Information-oriented documentation** for understanding KGC 4D's architecture and API.

- **[API Reference](./references/01-api.md)** - Complete reference for all public classes, methods, and functions
- **[Architecture Overview](./references/02-architecture.md)** - System design, named graphs, and core concepts
- **[Poka-Yoke Guards](./references/03-guards.md)** - 24 mistake-proofing mechanisms and their purposes
- **[Constants](./references/04-constants.md)** - RDF URIs, event types, and system predicates

### [Explanations](./explanations/index.md)
**Understanding-oriented guides** for grasping the concepts and principles behind KGC 4D.

- **[Why 4 Dimensions?](./explanations/01-four-dimensions.md)** - Observable state, nanosecond time, vector causality, and Git references
- **[Causality and Vector Clocks](./explanations/02-vector-clocks.md)** - How logical clocks track causality in distributed systems
- **[Temporal Reconstruction](./explanations/03-temporal-reconstruction.md)** - The zero-information invariant and state reconstruction
- **[Git as Immutable History](./explanations/04-git-backbone.md)** - Why Git provides the foundation for cryptographic verification
- **[Event Sourcing Architecture](./explanations/05-event-sourcing.md)** - The event log pattern and atomic transaction semantics
- **[FMEA and Mistake-Proofing](./explanations/06-poka-yoke.md)** - How failure mode analysis and poka-yoke guards prevent defects

---

## Quick Navigation

**I want to...**

- Get started with a working example → [Getting Started Tutorial](./tutorials/01-getting-started.md)
- Understand how KGC 4D works → Start with [Why 4 Dimensions?](./explanations/01-four-dimensions.md)
- Look up an API method → [API Reference](./references/01-api.md)
- Travel back in time → [Time Travel Guide](./how-to-guides/01-time-travel.md)
- Deploy in a browser → [Isomorphic Deployment Guide](./how-to-guides/05-isomorphic-deployment.md)
- Verify data integrity → [Verification Guide](./how-to-guides/02-verification.md)

---

## Key Concepts at a Glance

| Concept | Explanation |
|---------|-------------|
| **Observable State (O)** | Current RDF triples in the Universe graph |
| **Nanosecond Time (t_ns)** | BigInt timestamps with monotonic ordering |
| **Vector Causality (V)** | Logical clocks tracking distributed events |
| **Git References (G)** | Content-addressed snapshots with BLAKE3 hashing |
| **Event Log** | Immutable append-only history of all mutations |
| **Freezing** | Creating a deterministic snapshot and storing in Git |
| **Time Travel** | Reconstructing state at any historical point |
| **Verification** | Cryptographically proving snapshot integrity |

---

## Important Files and Examples

- **[Basic Usage Example](../examples/basic-usage.mjs)** - 4 runnable examples
- **[Mission-Critical Examples](../examples/mission-critical.mjs)** - 8 real-world JTBD scenarios
- **[Architecture Requirements](./ARD.md)** - Formal design constraints
- **[Implementation Summary](./IMPLEMENTATION-SUMMARY.md)** - Technical decisions and rationale
