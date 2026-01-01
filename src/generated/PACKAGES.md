# UNRDF Packages Reference

> Auto-generated from RDF ontology via ggen

**Generated**: 2025-01-01T00:00:00Z
**Ontology**: schema/domain.ttl
**Total Packages**: 12+

---

## Package Tiers

### 🟢 Essential Tier (7 packages)

Core packages always needed for UNRDF operation.

#### @unrdf/core
- **Purpose**: RDF Graph Operations, SPARQL, Foundational Substrate
- **Version**: 6.0.0-rc.1
- **Dependencies**: None
- **Description**: Core functionality for working with RDF graphs, SPARQL query execution, and the foundational knowledge substrate layer

#### @unrdf/oxigraph
- **Purpose**: Oxigraph SPARQL engine binding (10-100x faster than N3)
- **Version**: 6.0.0-rc.1
- **Dependencies**: `@unrdf/core`
- **Description**: High-performance SPARQL query engine based on Oxigraph (Rust backend)

#### @unrdf/kgc-4d
- **Purpose**: KGC 4D Datum & Universe Freeze Engine
- **Version**: 6.0.0-rc.1
- **Dependencies**: `@unrdf/core`
- **Description**: Knowledge Graph Context with 4D temporal reasoning and universe freeze capabilities

#### @unrdf/hooks
- **Purpose**: Policy Definition and Execution Framework
- **Version**: 6.0.0-rc.1
- **Dependencies**: `@unrdf/core`
- **Description**: Event-driven hook system for policy definition and execution

#### @unrdf/streaming
- **Purpose**: Change Feeds and Real-time Synchronization
- **Version**: 6.0.0-rc.1
- **Dependencies**: `@unrdf/core`
- **Description**: Real-time change feeds and synchronization for distributed systems

#### @unrdf/v6-core
- **Purpose**: ΔGate control plane, unified receipts, delta contracts
- **Version**: 6.0.0-rc.1
- **Dependencies**: `@unrdf/core`
- **Description**: v6 control plane with delta contract support and unified receipt generation

#### @unrdf/yawl
- **Purpose**: Core YAWL workflow engine
- **Version**: 6.0.0-rc.1
- **Dependencies**: `@unrdf/core`
- **Description**: YAWL-based workflow and process execution engine

---

### 🔵 Extended Tier (8+ packages)

Commonly used packages for typical use cases.

#### @unrdf/federation
- **Purpose**: Distributed RDF Query with RAFT Consensus
- **Version**: 6.0.0-rc.1
- **Dependencies**: `@unrdf/core`
- **Description**: Distributed RDF query capabilities with RAFT consensus for consistency

#### @unrdf/knowledge-engine
- **Purpose**: Rule Engine, Inference, Pattern Matching
- **Version**: 6.0.0-rc.1
- **Dependencies**: `@unrdf/core`, `@unrdf/oxigraph`
- **Description**: Forward-chaining rule engine with inference and pattern matching

#### @unrdf/cli
- **Purpose**: Command-line Tools for Graph Operations
- **Version**: 6.0.0-rc.1
- **Dependencies**: `@unrdf/core`
- **Description**: Command-line interface for graph operations and administration

#### @unrdf/kgc-runtime
- **Purpose**: KGC governance runtime with Zod schemas
- **Version**: 6.0.0-rc.1
- **Dependencies**: `@unrdf/core`, `@unrdf/kgc-4d`
- **Description**: Runtime governance system with schema validation

#### @unrdf/kgc-substrate
- **Purpose**: Deterministic KnowledgeStore
- **Version**: 6.0.0-rc.1
- **Dependencies**: `@unrdf/core`, `@unrdf/kgc-4d`
- **Description**: Deterministic knowledge store substrate layer

#### @unrdf/receipts
- **Purpose**: Batch receipt generation with Merkle trees
- **Version**: 6.0.0-rc.1
- **Dependencies**: `@unrdf/core`
- **Description**: Cryptographic receipt generation with Merkle tree proofs

#### @unrdf/consensus
- **Purpose**: Production-grade Raft consensus
- **Version**: 6.0.0-rc.1
- **Dependencies**: `@unrdf/core`, `@unrdf/federation`
- **Description**: Raft consensus implementation for distributed systems

#### @unrdf/v6-compat
- **Purpose**: V5 to V6 migration bridge
- **Version**: 6.0.0-rc.1
- **Dependencies**: `@unrdf/core`, `@unrdf/v6-core`
- **Description**: Compatibility layer for migrating from v5 to v6

---

## Installation Quick Start

### Core Installation (All Essential packages)

```bash
pnpm add @unrdf/core @unrdf/oxigraph @unrdf/kgc-4d @unrdf/hooks @unrdf/streaming @unrdf/v6-core @unrdf/yawl
```

### Full Installation (Essential + Extended)

```bash
pnpm add -D \
  @unrdf/core \
  @unrdf/oxigraph \
  @unrdf/kgc-4d \
  @unrdf/hooks \
  @unrdf/streaming \
  @unrdf/v6-core \
  @unrdf/yawl \
  @unrdf/federation \
  @unrdf/knowledge-engine \
  @unrdf/cli \
  @unrdf/kgc-runtime \
  @unrdf/kgc-substrate \
  @unrdf/receipts \
  @unrdf/consensus \
  @unrdf/v6-compat
```

---

## Dependency Graph

```
@unrdf/core
  ├─→ @unrdf/oxigraph
  ├─→ @unrdf/kgc-4d
  │    └─→ @unrdf/kgc-runtime
  │    └─→ @unrdf/kgc-substrate
  ├─→ @unrdf/hooks
  ├─→ @unrdf/streaming
  ├─→ @unrdf/v6-core
  │    └─→ @unrdf/v6-compat
  ├─→ @unrdf/yawl
  ├─→ @unrdf/federation
  │    └─→ @unrdf/consensus
  ├─→ @unrdf/knowledge-engine
  ├─→ @unrdf/cli
  └─→ @unrdf/receipts
```

---

## Package Metadata

| Package Name | Version | Type | Tier |
|---|---|---|---|
| `@unrdf/core` | 6.0.0-rc.1 | ESM | Essential |
| `@unrdf/oxigraph` | 6.0.0-rc.1 | ESM | Essential |
| `@unrdf/kgc-4d` | 6.0.0-rc.1 | ESM | Essential |
| `@unrdf/hooks` | 6.0.0-rc.1 | ESM | Essential |
| `@unrdf/streaming` | 6.0.0-rc.1 | ESM | Essential |
| `@unrdf/v6-core` | 6.0.0-rc.1 | ESM | Essential |
| `@unrdf/yawl` | 6.0.0-rc.1 | ESM | Essential |
| `@unrdf/federation` | 6.0.0-rc.1 | ESM | Extended |
| `@unrdf/knowledge-engine` | 6.0.0-rc.1 | ESM | Extended |
| `@unrdf/cli` | 6.0.0-rc.1 | CLI | Extended |
| `@unrdf/kgc-runtime` | 6.0.0-rc.1 | ESM | Extended |
| `@unrdf/kgc-substrate` | 6.0.0-rc.1 | ESM | Extended |
| `@unrdf/receipts` | 6.0.0-rc.1 | ESM | Extended |
| `@unrdf/consensus` | 6.0.0-rc.1 | ESM | Extended |
| `@unrdf/v6-compat` | 6.0.0-rc.1 | ESM | Extended |

---

**Generated by**: [ggen](https://github.com/seanchatmangpt/ggen) v5.0.2
**Last Updated**: 2025-01-01
**Documentation**: See [docs/](../docs/) for full documentation
