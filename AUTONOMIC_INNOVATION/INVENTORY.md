# UNRDF Autonomic Innovation - Package Inventory

**Generated**: 2025-12-26
**Total Packages**: 41
**Active KGC/UNRDF Packages**: 37

---

## Core Packages (Foundation Layer)

### @unrdf/core (v5.0.1)
- **Import Path**: `@unrdf/core`
- **Main Export**: `./src/index.mjs`
- **Provides**:
  - RDF graph operations
  - SPARQL query execution
  - Data validation (Zod schemas)
  - Security policies
  - Health checks & logging
  - Metrics collection
  - Types & constants
- **Key Dependencies**: `n3`, `rdf-canonize`, `@rdfjs/*`, `zod`
- **Purpose**: Foundational RDF substrate

### @unrdf/oxigraph (v5.0.1)
- **Import Path**: `@unrdf/oxigraph`
- **Main Export**: `./src/index.mjs`
- **Sub-exports**:
  - `./store` - Store implementation
  - `./types` - TypeScript definitions
- **Provides**:
  - High-performance SPARQL engine binding (Oxigraph)
  - RDF store operations
  - Graph database benchmarking
- **Key Dependencies**: `oxigraph`, `zod`
- **Purpose**: Efficient graph storage and querying

### @unrdf/knowledge-engine (v5.0.1)
- **Import Path**: `@unrdf/knowledge-engine`
- **Main Export**: `./src/index.mjs`
- **Sub-exports**:
  - `./query` - Query processing
  - `./canonicalize` - Canonical forms
  - `./parse` - Parsing utilities
  - `./ai-search` - AI-enhanced search
- **Provides**:
  - Rule engine & inference
  - Pattern matching
  - Canonicalization
  - AI-enhanced semantic search
  - Transformers integration
- **Key Dependencies**: `eyereasoner`, `@xenova/transformers`, `@noble/hashes`
- **Purpose**: Advanced reasoning & pattern discovery

---

## Time-Domain & Event Sourcing

### @unrdf/kgc-4d (v5.0.1)
- **Import Path**: `@unrdf/kgc-4d`
- **Main Export**: `./src/index.mjs`
- **Sub-exports**:
  - `./client` - Client interface
  - `./hdit` - HDIT (hash-based deterministic IT)
- **Key Modules**:
  - `freeze.mjs` - Universe freezing
  - `time.mjs` - Nanosecond precision timing
  - `snapshot-cache.mjs` - Snapshot caching
  - `git.mjs` - Git-backed snapshots
  - `guards.mjs` - Guard validation
  - `store.mjs` - Store interface
- **Provides**:
  - Datum & universe freeze engine
  - Nanosecond-precision event logging
  - Git-backed snapshots
  - Deterministic event replay
  - Receipt generation with parent chains
- **Key Dependencies**: `hash-wasm`, `isomorphic-git`, `@unrdf/core`, `@unrdf/oxigraph`
- **Purpose**: 4D event sourcing with time travel

---

## Change Propagation & Streaming

### @unrdf/streaming (v5.0.1)
- **Import Path**: `@unrdf/streaming`
- **Main Export**: `./src/index.mjs`
- **Sub-exports**:
  - `./processor` - Stream processor
- **Provides**:
  - Change feed generation
  - Real-time synchronization
  - Stream processing
  - WebSocket integration
  - LRU caching for efficiency
- **Key Dependencies**: `ws`, `lru-cache`, `@opentelemetry/api`, `@unrdf/core`, `@unrdf/hooks`, `@unrdf/oxigraph`
- **Purpose**: Real-time change propagation

### @unrdf/collab (v1.0.0)
- **Import Path**: `@unrdf/collab`
- **Main Export**: `./src/index.mjs`
- **Sub-exports**:
  - `./crdt` - CRDT implementations
  - `./sync` - Synchronization
  - `./composables` - Composable utilities
- **Provides**:
  - CRDT-based collaboration (Yjs)
  - Offline-first architecture
  - Real-time editing
  - IndexedDB persistence
- **Key Dependencies**: `yjs`, `y-websocket`, `y-indexeddb`, `lib0`, `ws`
- **Purpose**: Offline-capable collaborative RDF editing

---

## Policy & Execution

### @unrdf/hooks (v5.0.1)
- **Import Path**: `@unrdf/hooks`
- **Main Export**: `./src/index.mjs`
- **Sub-exports**:
  - `./define` - Hook definition
  - `./executor` - Hook execution
- **Provides**:
  - Hook policy definition
  - Hook execution framework
  - Policy validation (Zod)
  - CLI integration (citty)
- **Key Dependencies**: `@unrdf/core`, `@unrdf/oxigraph`, `citty`, `zod`
- **Purpose**: Event-driven policy execution

---

## Distributed & Consensus

### @unrdf/federation (v5.0.1)
- **Import Path**: `@unrdf/federation`
- **Main Export**: `./src/index.mjs`
- **Sub-exports**:
  - `./coordinator` - Query coordination
  - `./advanced-sparql` - Advanced SPARQL federation
- **Provides**:
  - Peer discovery
  - Distributed query execution
  - SPARQL federation (Comunica)
  - Metrics collection (Prometheus)
- **Key Dependencies**: `@comunica/query-sparql`, `prom-client`, `@opentelemetry/api`, `@unrdf/core`, `@unrdf/hooks`
- **Purpose**: Decentralized query federation

### @unrdf/consensus (v1.0.0)
- **Import Path**: `@unrdf/consensus`
- **Main Export**: `./src/index.mjs`
- **Sub-exports**:
  - `./raft` - Raft consensus coordinator
  - `./cluster` - Cluster membership manager
  - `./state` - Distributed state machine
  - `./transport` - WebSocket transport
- **Provides**:
  - Production-grade Raft consensus
  - Cluster membership
  - Distributed state machine
  - Fault tolerance
- **Key Dependencies**: `@unrdf/federation`, `msgpackr`, `ws`, `@opentelemetry/api`
- **Purpose**: Multi-node coordination via consensus

---

## Observability & Validation

### @unrdf/observability (v1.0.0)
- **Import Path**: `@unrdf/observability`
- **Main Export**: `./src/index.mjs`
- **Sub-exports**:
  - `./metrics` - Workflow metrics
  - `./exporters` - Grafana exporter
  - `./alerts` - Alert manager
- **Provides**:
  - Prometheus metrics collection
  - Grafana exporter
  - OTEL integration
  - Alert management
  - Observability dashboard
- **Key Dependencies**: `prom-client`, `@opentelemetry/*`, `@opentelemetry/sdk-metrics`, `express`
- **Purpose**: Production observability & alerting

### @unrdf/validation (v5.0.1)
- **Import Path**: `@unrdf/validation`
- **Provides**:
  - OTEL validation framework
  - Validation orchestration
  - Quality gate checking
- **Purpose**: Development-time validation (runs via `validation/run-all.mjs`)

---

## Domain & Specialized Packages

### @unrdf/domain (v5.0.1)
- **Purpose**: Domain-specific RDF patterns

### @unrdf/caching (v5.0.1)
- **Purpose**: Caching layer for graph operations

### @unrdf/composables (v5.0.1)
- **Purpose**: Reusable composition patterns

### @unrdf/ml-inference (v5.0.1)
- **Purpose**: ML model inference on graphs

### @unrdf/ml-versioning (v5.0.1)
- **Purpose**: Model versioning & provenance

### @unrdf/semantic-search (v5.0.1)
- **Purpose**: Semantic search capabilities

### @unrdf/graph-analytics (v5.0.1)
- **Purpose**: Graph analytics & metrics

### @unrdf/rdf-graphql (v5.0.1)
- **Purpose**: GraphQL→RDF bridging

---

## Infrastructure Packages

### @unrdf/cli (v5.0.1)
- **Purpose**: Command-line interface

### @unrdf/engine-gateway (v5.0.1)
- **Purpose**: API gateway for engine

### @unrdf/serverless (v5.0.1)
- **Purpose**: Serverless deployment support

### @unrdf/project-engine (v5.0.1)
- **Purpose**: Project lifecycle management

### @unrdf/atomvm (v5.0.1)
- **Purpose**: Atomic VM operations

### @unrdf/blockchain (v5.0.1)
- **Purpose**: Blockchain integration

### @unrdf/dark-matter (v5.0.1)
- **Purpose**: Advanced graph transformations

---

## Workflow & Queue Packages (YAWL Suite)

### @unrdf/yawl (v5.0.1)
- **Purpose**: Core workflow orchestration

### @unrdf/yawl-api (v5.0.1)
- **Purpose**: REST API for workflows

### @unrdf/yawl-queue (v5.0.1)
- **Purpose**: Message queue handling

### @unrdf/yawl-durable (v5.0.1)
- **Purpose**: Durable workflow execution

### @unrdf/yawl-kafka (v5.0.1)
- **Purpose**: Kafka integration

### @unrdf/yawl-langchain (v5.0.1)
- **Purpose**: LangChain integration

### @unrdf/yawl-observability (v5.0.1)
- **Purpose**: YAWL-specific observability

### @unrdf/yawl-ai (v5.0.1)
- **Purpose**: AI-powered workflow generation

### @unrdf/yawl-realtime (v5.0.1)
- **Purpose**: Real-time workflow updates

### @unrdf/yawl-viz (v5.0.1)
- **Purpose**: Workflow visualization

---

## Test & Documentation

### @unrdf/test-utils (v5.0.1)
- **Purpose**: Testing utilities & fixtures

### @unrdf/docs (v5.0.1)
- **Purpose**: Documentation generation

### @unrdf/nextra (v5.0.1)
- **Purpose**: Next.js documentation framework

---

## Critical Import Patterns

### For RDF Store Operations
```javascript
import { createStore } from '@unrdf/oxigraph';
// NOT: from 'n3' (direct use forbidden outside justified modules)
```

### For Canonicalization & Hashing
```javascript
import { freezeUniverse, snapSnapshot } from '@unrdf/kgc-4d';
import { hashWasm } from 'hash-wasm'; // Root dependency
```

### For Change Tracking
```javascript
import { registerChangeListener } from '@unrdf/streaming';
```

### For Policy Execution
```javascript
import { defineHook, executeHook } from '@unrdf/hooks';
```

### For Distributed Queries
```javascript
import { createFederator } from '@unrdf/federation';
```

### For Consensus
```javascript
import { createRaftCoordinator } from '@unrdf/consensus';
```

---

## Key Guarantees from Existing Packages

1. **Determinism**: @unrdf/kgc-4d provides hash-based determinism via Git snapshots
2. **Time**: Nanosecond-precision event timestamps
3. **Provenance**: Receipt chains with parent hashes
4. **Canonicalization**: `rdf-canonize` integration for stable representations
5. **Zod Validation**: All inputs validated via Zod schemas
6. **OTEL Instrumentation**: Built-in tracing via @opentelemetry/*
7. **Offline Capability**: @unrdf/collab enables offline-first collaboration
8. **Consensus**: Raft consensus for distributed coordination

---

## Autonomic Innovation Foundation

**Status**: Phase 0 Complete
**Next**: Agent 1 (Orchestrator) creates per-agent submodules and delegates to Agents 2-10
**Constraint**: All new code MUST use ONLY these packages - no npm additions allowed
