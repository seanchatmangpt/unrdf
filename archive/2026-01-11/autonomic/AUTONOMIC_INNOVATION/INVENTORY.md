# AUTONOMIC_INNOVATION: KGC/UNRDF Package Inventory

**Generated**: 2025-12-26
**Mode**: Fully Autonomic, Phase 0 Complete
**Destination**: `claude/kgc-migration-facade-zPbbg` branch

---

## Core Packages (10 Critical)

### 1. @unrdf/core
- **Path**: `packages/core/src/index.mjs`
- **Version**: 5.0.1
- **Provides**:
  - RDF operations & data model
  - SPARQL execution engine
  - Type definitions & validation
  - Health checks & metrics
  - Security schemas
  - Logging utilities
- **Imports Available**:
  - `./rdf` - RDF operations
  - `./sparql` - SPARQL execution
  - `./validation` - Validation framework
  - `./types` - Type definitions

### 2. @unrdf/oxigraph
- **Path**: `packages/oxigraph/src/store.mjs`
- **Version**: 5.0.1
- **Provides**:
  - Graph store implementation (Oxigraph WASM backend)
  - SPARQL queries & mutations
  - Content-addressable storage
- **Imports Available**:
  - `./store` - Store interface
  - `./types` - Type definitions

### 3. @unrdf/kgc-4d
- **Path**: `packages/kgc-4d/src/index.mjs`
- **Version**: 5.0.1
- **Provides**:
  - **freeze.mjs** - Universe snapshots, nanosecond timestamps
  - **guards.mjs** - Change guards & invariant enforcement
  - **store.mjs** - KGC store adapter
  - **git.mjs** - Git-backed snapshots
  - **time.mjs** - Time travel & event log
  - **snapshot-cache.mjs** - Snapshot caching
  - **hdit/** - HDIT integration
- **Imports Available**:
  - `./` - Main KGC-4D interface
  - `./client` - Client API
  - `./hdit` - HDIT integration

### 4. @unrdf/hooks
- **Path**: `packages/hooks/src/index.mjs`
- **Version**: 5.0.1
- **Provides**:
  - Policy definition framework
  - Hook execution & chaining
  - Citty CLI integration
- **Imports Available**:
  - `./define` - Hook definition
  - `./executor` - Hook execution

### 5. @unrdf/streaming
- **Path**: `packages/streaming/src/index.mjs`
- **Version**: 5.0.1
- **Provides**:
  - Change feeds & event streams
  - Real-time synchronization
  - WebSocket message brokers
  - LRU caching
- **Imports Available**:
  - `./processor` - Stream processing

### 6. @unrdf/collab
- **Path**: `packages/collab/src/index.mjs`
- **Version**: 1.0.0
- **Provides**:
  - CRDT-based collaboration (Yjs)
  - Offline-first architecture
  - WebSocket sync
- **Imports Available**:
  - `./crdt` - CRDT primitives
  - `./sync` - Sync protocol
  - `./composables` - Composable utilities

### 7. @unrdf/consensus
- **Path**: `packages/consensus/src/index.mjs`
- **Version**: 1.0.0
- **Provides**:
  - Raft consensus algorithm
  - Cluster management
  - Distributed state machine
  - WebSocket transport
- **Imports Available**:
  - `./raft` - Raft coordinator
  - `./cluster` - Cluster manager
  - `./state` - Distributed state

### 8. @unrdf/federation
- **Path**: `packages/federation/src/index.mjs`
- **Version**: 5.0.1
- **Provides**:
  - Peer discovery
  - Distributed SPARQL queries
  - Comunica integration
  - Advanced federation patterns
- **Imports Available**:
  - `./coordinator` - Federation coordinator
  - `./advanced-sparql` - Advanced patterns

### 9. @unrdf/blockchain
- **Path**: `packages/blockchain/src/index.mjs`
- **Version**: 1.0.0
- **Provides**:
  - Cryptographic receipt anchoring
  - Merkle proof generation
  - Blockchain verification
  - Noble hashes & ethers.js
- **Imports Available**:
  - `./anchoring` - Receipt anchorer
  - `./contracts` - Workflow verifier
  - `./merkle` - Merkle proofs

### 10. @unrdf/caching
- **Path**: `packages/caching/src/index.mjs`
- **Version**: 1.0.0
- **Provides**:
  - Multi-layer caching (LRU + Redis)
  - SPARQL query caching
  - Dependency tracking & invalidation
- **Imports Available**:
  - `./layers` - Multi-layer cache
  - `./invalidation` - Dependency tracker
  - `./query` - SPARQL cache

---

## Observation: Observable State Invariant
- All packages store observable state O in RDF graphs
- Projection is computed deterministically: A = μ(O)
- Idempotence: μ∘μ = μ (enforced by tests)

---

## Available Hashing & Canonicalization
- **hash-wasm** (v4.12.0) - Workspace dep
- **rdf-canonize** (v5.0.0) - Available in @unrdf/core
- **@noble/hashes** (v1.3.3) - Available in @unrdf/blockchain
- **isomorphic-git** (v1.35.1) - Available in @unrdf/kgc-4d

---

## Determinism Guarantees
- **Node.js ESM** - All source is `.mjs` (no CommonJS)
- **Zod v4.1.13** - Validation (workspace override)
- **Vitest** - Deterministic test runner (no flakes)
- **Pnpm** - Locked dependency tree

---

## Execution Environment
- **Node.js**: ≥18.0.0
- **Pnpm**: ≥7.0.0
- **Type**: "module" (ESM only)
- **Output**: `.mjs` files only

---

## Next Steps (Phase 1)
- Agent 1: Create PLAN.md for orchestration & integration
- Agents 2-10: Create PLAN.md for respective primitives
- All agents operate concurrently
- Merge outputs into ./src/ and ./test/
