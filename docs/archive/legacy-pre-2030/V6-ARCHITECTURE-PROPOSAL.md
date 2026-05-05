# UNRDF V6 Architecture Proposal
## Complete Rewrite from First Principles

**Analysis Date**: 2025-12-28
**Current State**: 57 packages, 417K LOC, 1,468 files
**Target State**: 12 packages, ~3-layer architecture

---

## Executive Summary

The current UNRDF monorepo has **excessive fragmentation** across 57 packages with significant overlapping concerns. A v6 rewrite should consolidate to **12 core packages** organized in **3 clean layers**, eliminating duplication and clarifying boundaries.

**Key Problems Identified**:
1. **Receipt/Provenance Duplication** - 4 packages doing cryptographic receipts (kgc-4d, receipts, blockchain, kgc-multiverse)
2. **YAWL Integration Sprawl** - 9 packages for thin adapters that should be plugins
3. **KGC Fragmentation** - 11 packages with unclear boundaries
4. **Unclear Core** - What is "core" vs "v6-core" vs "kgc-substrate"?
5. **Integration Hell** - Every external library gets a package

---

## 1. Proposed Package Count: 12 Packages

### Layer 1: FOUNDATION (3 packages)

#### 1.1 `@unrdf/store`
**Purpose**: Graph storage and SPARQL execution
**Current equivalent**: `@unrdf/oxigraph` (1,746 LOC) + parts of `@unrdf/core` (23,616 LOC)
**Target LOC**: ~8,000

**Responsibilities**:
- Oxigraph WASM bindings and initialization
- SPARQL query execution (no parsing/validation - just execution)
- RDF quad storage and retrieval
- Basic store lifecycle (create, destroy, clear)
- Performance benchmarking utilities

**What it DOES NOT include**:
- RDF parsing/serialization (moved to @unrdf/rdf)
- SHACL validation (moved to @unrdf/rdf)
- Hooks/policies (moved to @unrdf/governance)
- Streaming/subscriptions (moved to @unrdf/runtime)

**Key exports**:
```javascript
import { createStore, executeQuery, loadQuads } from '@unrdf/store';
```

**Dependencies**: ZERO internal dependencies (foundation layer)

---

#### 1.2 `@unrdf/rdf`
**Purpose**: RDF data model, parsing, serialization, validation
**Current equivalent**: Parts of `@unrdf/core` + `@unrdf/validation`
**Target LOC**: ~6,000

**Responsibilities**:
- RDF/JS data model (Quad, NamedNode, Literal, BlankNode)
- Parsers: Turtle, N-Triples, JSON-LD, N-Quads
- Serializers: Turtle, N-Triples, JSON-LD, N-Quads
- SHACL validation engine
- SPARQL query parsing and AST
- RDF canonicalization (for deterministic hashing)
- Namespace utilities

**What it DOES NOT include**:
- Query execution (that's @unrdf/store)
- Streaming (that's @unrdf/runtime)
- Provenance (that's @unrdf/governance)

**Key exports**:
```javascript
import { quad, namedNode, literal } from '@unrdf/rdf';
import { parseTurtle, serializeJsonLd } from '@unrdf/rdf/parsers';
import { validateShacl } from '@unrdf/rdf/validation';
```

**Dependencies**: ZERO internal dependencies (foundation layer)

---

#### 1.3 `@unrdf/governance`
**Purpose**: Provenance, receipts, time-travel, deterministic event sourcing
**Current equivalent**: `@unrdf/kgc-4d` (6,693 LOC) + `@unrdf/receipts` (730 LOC) + `@unrdf/blockchain` (945 LOC) + `@unrdf/kgc-multiverse` (4,179 LOC) + `@unrdf/kgc-substrate` (2,334 LOC)
**Target LOC**: ~12,000

**Responsibilities**:
- Nanosecond-precision event logging
- Cryptographic receipt generation (Merkle trees, hashing)
- Receipt verification and proof generation
- Time-travel queries (reconstruct state at timestamp T)
- Universe freeze/snapshot with Git backing
- Immutable append-only event log
- Vector clocks for causality tracking
- Blockchain anchoring (optional)

**Consolidation rationale**:
- All 5 current packages do overlapping cryptographic receipt work
- `kgc-4d/freeze.mjs::verifyReceipt` duplicates `receipts/batch-receipt-generator.mjs::verifyBatchReceipt`
- `blockchain/merkle/*` duplicates `receipts/merkle-batcher.mjs`
- Single unified API for provenance eliminates confusion

**Key exports**:
```javascript
import { createGovernanceStore, freezeUniverse, verifyReceipt } from '@unrdf/governance';
import { travelToTimestamp, getEventLog } from '@unrdf/governance/time-travel';
import { anchorToBlockchain } from '@unrdf/governance/blockchain';
```

**Dependencies**: `@unrdf/store`, `@unrdf/rdf`

---

### Layer 2: RUNTIME (4 packages)

#### 2.1 `@unrdf/workflows`
**Purpose**: YAWL workflow engine with deterministic execution
**Current equivalent**: `@unrdf/yawl` (39,123 LOC) + `@unrdf/yawl-durable` (1,712 LOC)
**Target LOC**: ~35,000

**Responsibilities**:
- Van der Aalst workflow patterns (all 23 patterns)
- Case/task/work item lifecycle management
- Split/join behaviors (XOR, AND, OR)
- Workflow RDF ontology and SPARQL queries
- Deterministic execution with KGC integration
- Saga pattern support (compensating transactions)
- Durable execution (event sourcing workflows)
- Timer/deadline handling
- Cancellation regions

**What it DOES NOT include**:
- Kafka integration (moved to @unrdf/integrations)
- API server (moved to @unrdf/integrations)
- Observability (moved to @unrdf/observability)
- Queue adapters (moved to @unrdf/integrations)
- AI optimization (KILLED - premature)
- Visualization (moved to @unrdf/tools)

**Key exports**:
```javascript
import { WorkflowEngine, createCase, executeTask } from '@unrdf/workflows';
import { DurableWorkflow, defineSaga } from '@unrdf/workflows/durable';
```

**Dependencies**: `@unrdf/store`, `@unrdf/rdf`, `@unrdf/governance`

---

#### 2.2 `@unrdf/runtime`
**Purpose**: Streaming, federation, consensus, real-time synchronization
**Current equivalent**: `@unrdf/streaming` (3,298 LOC) + `@unrdf/federation` (4,070 LOC) + `@unrdf/consensus` (2,143 LOC) + `@unrdf/collab` (2,375 LOC)
**Target LOC**: ~10,000

**Responsibilities**:
- Change feeds and subscriptions (reactive queries)
- Real-time synchronization across nodes
- RAFT consensus for distributed coordination
- Multi-master federation with conflict resolution
- CRDT-based collaborative editing (Yjs integration)
- Query federation across multiple stores
- Event sourcing and replay
- WebSocket/SSE streaming protocols

**Consolidation rationale**:
- Streaming, federation, consensus, and collaboration are all aspects of distributed runtime
- Single package ensures coherent concurrency model
- Avoids circular dependencies between these concerns

**Key exports**:
```javascript
import { createFederatedStore, subscribeToChanges } from '@unrdf/runtime';
import { RaftConsensus, electLeader } from '@unrdf/runtime/consensus';
import { createCRDTStore } from '@unrdf/runtime/collab';
```

**Dependencies**: `@unrdf/store`, `@unrdf/rdf`, `@unrdf/governance`

---

#### 2.3 `@unrdf/hooks`
**Purpose**: Policy definition, execution, and knowledge constraints
**Current equivalent**: `@unrdf/hooks` (10,567 LOC) + `@unrdf/knowledge-engine` (5,419 LOC)
**Target LOC**: ~12,000

**Responsibilities**:
- Declarative hook definition DSL
- Hook registration and lifecycle
- Pre/post operation hooks (before/after insert, delete, update)
- Condition evaluation (guards, predicates)
- Rule engine for inference and pattern matching
- Forward/backward chaining
- Constraint propagation
- Policy enforcement (access control, validation)

**Consolidation rationale**:
- Hooks and knowledge-engine both do rule-based reasoning
- Single unified policy framework is easier to reason about
- Eliminates confusion between "hook" and "rule"

**Key exports**:
```javascript
import { defineHook, registerHook, executeHooks } from '@unrdf/hooks';
import { defineRule, inferTriples } from '@unrdf/hooks/rules';
```

**Dependencies**: `@unrdf/store`, `@unrdf/rdf`

---

#### 2.4 `@unrdf/observability`
**Purpose**: Metrics, tracing, logging, health checks
**Current equivalent**: `@unrdf/observability` (2,184 LOC) + `@unrdf/yawl-observability` (1,896 LOC) + parts of `@unrdf/core`
**Target LOC**: ~5,000

**Responsibilities**:
- OpenTelemetry tracing (spans, contexts)
- Prometheus metrics (counters, gauges, histograms)
- Structured logging
- Health checks and readiness probes
- Performance profiling hooks
- SLI/SLO tracking
- Grafana dashboard configs

**Consolidation rationale**:
- All observability should use same instrumentation layer
- Eliminates duplicate OTEL setup across packages
- Single source of truth for metrics naming

**Key exports**:
```javascript
import { createTracer, recordMetric, logger } from '@unrdf/observability';
import { healthCheck, readinessProbe } from '@unrdf/observability/health';
```

**Dependencies**: ZERO internal dependencies (cross-cutting concern)

---

### Layer 3: APPLICATIONS (5 packages)

#### 3.1 `@unrdf/cli`
**Purpose**: Command-line tools for graph operations
**Current equivalent**: `@unrdf/cli` (4,814 LOC) + `@unrdf/kgc-cli` (17,697 LOC) + `@unrdf/kgc-tools` (354 LOC)
**Target LOC**: ~8,000

**Responsibilities**:
- Graph CRUD operations (load, query, export)
- Workflow management (create case, list tasks, complete)
- Receipt verification and audit
- Time-travel queries
- Extension/plugin registry
- Interactive REPL
- Batch operations

**Consolidation rationale**:
- 3 CLI packages is absurd - consolidate to one
- Plugin system can handle workspace-specific extensions
- Deterministic extension registry doesn't need separate package

**Key features**:
```bash
unrdf query "SELECT * WHERE { ?s ?p ?o } LIMIT 10"
unrdf workflow create --spec purchase-order.ttl
unrdf governance verify-receipt <receipt-id>
unrdf time-travel --timestamp 2025-12-28T10:00:00Z
```

**Dependencies**: ALL runtime and foundation packages

---

#### 3.2 `@unrdf/integrations`
**Purpose**: External service adapters (Kafka, REST API, GraphQL, etc.)
**Current equivalent**: `@unrdf/yawl-api` (1,002 LOC) + `@unrdf/yawl-kafka` (1,548 LOC) + `@unrdf/yawl-queue` (911 LOC) + `@unrdf/yawl-langchain` (424 LOC) + `@unrdf/rdf-graphql` (1,720 LOC) + `@unrdf/serverless` (1,508 LOC)
**Target LOC**: ~6,000

**Responsibilities**:
- **REST API** - Express/Fastify server exposing workflows as REST endpoints
- **GraphQL** - Type-safe GraphQL schema generation from RDF
- **Kafka** - Event streaming producer/consumer with Avro
- **Queue** - BullMQ/Redis adapter for distributed execution
- **LangChain** - AI workflow orchestration adapter
- **Serverless** - AWS Lambda deployment utilities

**Consolidation rationale**:
- All thin adapters (~400-1500 LOC each) don't justify separate packages
- Plugin architecture with single entry point: `@unrdf/integrations/kafka`
- Easier to maintain consistent API patterns

**Key exports**:
```javascript
import { createRestServer } from '@unrdf/integrations/rest';
import { createGraphQLSchema } from '@unrdf/integrations/graphql';
import { KafkaProducer } from '@unrdf/integrations/kafka';
import { deployToLambda } from '@unrdf/integrations/serverless';
```

**Dependencies**: `@unrdf/workflows`, `@unrdf/runtime`, `@unrdf/store`, `@unrdf/rdf`

---

#### 3.3 `@unrdf/ai`
**Purpose**: ML inference, semantic search, embeddings, AI-powered features
**Current equivalent**: `@unrdf/ml-inference` (1,164 LOC) + `@unrdf/ml-versioning` (663 LOC) + `@unrdf/semantic-search` (768 LOC) + `@unrdf/yawl-ai` (1,925 LOC) + `@unrdf/decision-fabric` (2,383 LOC)
**Target LOC**: ~5,000

**Responsibilities**:
- ONNX model inference pipeline
- Vector embeddings for RDF entities
- Semantic search with cosine similarity
- ML model versioning with time-travel
- Intent-to-outcome transformation (decision fabric)
- Workflow performance prediction (from yawl-ai)

**Consolidation rationale**:
- All AI/ML features should share same vector storage/indexing
- Decision fabric is really just fancy embedding-based search
- Single package ensures consistent embedding model

**Key exports**:
```javascript
import { createEmbeddings, semanticSearch } from '@unrdf/ai';
import { inferModel, versionModel } from '@unrdf/ai/inference';
import { transformIntent } from '@unrdf/ai/decision-fabric';
```

**Dependencies**: `@unrdf/store`, `@unrdf/rdf`, `@unrdf/governance`

---

#### 3.4 `@unrdf/ui`
**Purpose**: React components, Vue composables, visualization libraries
**Current equivalent**: `@unrdf/react` (900 LOC) + `@unrdf/composables` (0 LOC) + `@unrdf/yawl-viz` (0 LOC) + `@unrdf/yawl-realtime` (1,414 LOC)
**Target LOC**: ~4,000

**Responsibilities**:
- **React hooks** - `useQuery`, `useMutation`, `useWorkflow`, `useRealtime`
- **Vue composables** - Reactive RDF state management
- **D3.js workflow viz** - Van der Aalst pattern rendering
- **Real-time collab** - Socket.io client for collaborative editing
- **Graph explorer** - Interactive SPARQL query builder

**Consolidation rationale**:
- All UI concerns in single package
- Framework-specific exports via subpaths: `@unrdf/ui/react`, `@unrdf/ui/vue`
- Easier to share common visualization logic

**Key exports**:
```javascript
// React
import { useQuery, useWorkflow } from '@unrdf/ui/react';

// Vue
import { useRdfStore } from '@unrdf/ui/vue';

// Visualization
import { WorkflowVisualizer } from '@unrdf/ui/viz';
```

**Dependencies**: `@unrdf/store`, `@unrdf/rdf`, `@unrdf/workflows`, `@unrdf/runtime`

---

#### 3.5 `@unrdf/tools`
**Purpose**: Development utilities, testing, documentation generation
**Current equivalent**: `@unrdf/test-utils` (1,398 LOC) + `@unrdf/diataxis-kit` (2,620 LOC) + `@unrdf/kgc-docs` (1,583 LOC) + `@unrdf/kgc-probe` (16,810 LOC)
**Target LOC**: ~8,000

**Responsibilities**:
- **Test utilities** - Mock stores, fixture loaders, test helpers
- **Documentation generation** - Diátaxis framework, package inventory
- **KGC Markdown** - Dynamic doc generation with proof anchoring
- **Integrity scanning** - Automated knowledge graph validation (kgc-probe)
- **Benchmarking** - Performance test harness
- **Schema introspection** - Generate docs from SHACL/OWL

**Consolidation rationale**:
- All developer tooling in one place
- kgc-probe is testing/validation tool, not runtime component
- Easier to maintain consistent doc formatting

**Key exports**:
```javascript
import { createMockStore, loadFixture } from '@unrdf/tools/testing';
import { generateDocs, scanIntegrity } from '@unrdf/tools/docs';
import { runBenchmark } from '@unrdf/tools/bench';
```

**Dependencies**: ALL packages (testing needs access to everything)

---

## 2. Layer Architecture: 3 Layers

```
┌─────────────────────────────────────────────────────────────────┐
│ LAYER 3: APPLICATIONS                                           │
│   @unrdf/cli          - Command-line tools                      │
│   @unrdf/integrations - External adapters (Kafka, REST, etc)    │
│   @unrdf/ai           - ML inference, semantic search           │
│   @unrdf/ui           - React/Vue components, visualization     │
│   @unrdf/tools        - Testing, docs, benchmarks               │
├─────────────────────────────────────────────────────────────────┤
│ LAYER 2: RUNTIME                                                │
│   @unrdf/workflows    - YAWL engine, durable execution          │
│   @unrdf/runtime      - Streaming, federation, consensus        │
│   @unrdf/hooks        - Policies, rules, inference              │
│   @unrdf/observability - Metrics, tracing, logging              │
├─────────────────────────────────────────────────────────────────┤
│ LAYER 1: FOUNDATION                                             │
│   @unrdf/store        - Oxigraph bindings, SPARQL execution     │
│   @unrdf/rdf          - Data model, parsers, validation         │
│   @unrdf/governance   - Provenance, receipts, time-travel       │
└─────────────────────────────────────────────────────────────────┘
```

**Dependency Rules**:
- Layer 3 CAN depend on Layer 2 and Layer 1
- Layer 2 CAN depend on Layer 1 only
- Layer 1 packages CANNOT depend on each other (parallel, not hierarchical)
- `@unrdf/observability` is special - can be imported by ANY layer (cross-cutting)

---

## 3. Dependency Graph (Ideal State)

```mermaid
graph TD
    %% Layer 1 - Foundation (no dependencies between them)
    store["@unrdf/store<br/>(Oxigraph + SPARQL)"]
    rdf["@unrdf/rdf<br/>(Data model + parsers)"]
    gov["@unrdf/governance<br/>(Provenance + time-travel)"]

    %% Layer 2 - Runtime
    workflows["@unrdf/workflows<br/>(YAWL engine)"]
    runtime["@unrdf/runtime<br/>(Streaming + federation)"]
    hooks["@unrdf/hooks<br/>(Policies + rules)"]
    otel["@unrdf/observability<br/>(Metrics + tracing)"]

    %% Layer 3 - Applications
    cli["@unrdf/cli<br/>(Command-line)"]
    integrations["@unrdf/integrations<br/>(Kafka/REST/GraphQL)"]
    ai["@unrdf/ai<br/>(ML + semantic search)"]
    ui["@unrdf/ui<br/>(React/Vue/viz)"]
    tools["@unrdf/tools<br/>(Testing + docs)"]

    %% Layer 1 dependencies (none between them)

    %% Layer 2 dependencies (only to Layer 1)
    workflows --> store
    workflows --> rdf
    workflows --> gov

    runtime --> store
    runtime --> rdf
    runtime --> gov

    hooks --> store
    hooks --> rdf

    %% Layer 3 dependencies (to Layer 2 and Layer 1)
    cli --> workflows
    cli --> runtime
    cli --> hooks
    cli --> gov
    cli --> store
    cli --> rdf

    integrations --> workflows
    integrations --> runtime
    integrations --> store
    integrations --> rdf

    ai --> store
    ai --> rdf
    ai --> gov

    ui --> workflows
    ui --> runtime
    ui --> store
    ui --> rdf

    tools --> workflows
    tools --> runtime
    tools --> hooks
    tools --> gov
    tools --> store
    tools --> rdf

    %% Observability can be used by ANY layer (not shown to reduce clutter)
```

**Most Depended Upon** (in order):
1. `@unrdf/store` - 10 packages depend on it
2. `@unrdf/rdf` - 10 packages depend on it
3. `@unrdf/governance` - 5 packages depend on it
4. `@unrdf/workflows` - 4 packages depend on it
5. `@unrdf/runtime` - 4 packages depend on it

---

## 4. Kill List (45 packages eliminated)

### 4.1 Merged into `@unrdf/governance`
- ❌ `@unrdf/kgc-4d` - Core time-travel merged
- ❌ `@unrdf/receipts` - Merkle tree receipts merged
- ❌ `@unrdf/blockchain` - Anchoring merged as optional feature
- ❌ `@unrdf/kgc-multiverse` - Universe forking merged
- ❌ `@unrdf/kgc-substrate` - Deterministic store merged

**Rationale**: All 5 packages do cryptographic provenance. Single package eliminates duplication.

---

### 4.2 Merged into `@unrdf/workflows`
- ❌ `@unrdf/yawl` - Core engine kept
- ❌ `@unrdf/yawl-durable` - Saga pattern merged into core

---

### 4.3 Merged into `@unrdf/integrations`
- ❌ `@unrdf/yawl-api` - REST API adapter
- ❌ `@unrdf/yawl-kafka` - Kafka adapter
- ❌ `@unrdf/yawl-queue` - BullMQ adapter
- ❌ `@unrdf/yawl-langchain` - LangChain adapter
- ❌ `@unrdf/rdf-graphql` - GraphQL adapter
- ❌ `@unrdf/serverless` - Lambda deployment

**Rationale**: Thin adapters (400-1500 LOC each) don't justify packages. Plugin model better.

---

### 4.4 Merged into `@unrdf/runtime`
- ❌ `@unrdf/streaming` - Change feeds
- ❌ `@unrdf/federation` - Distributed queries
- ❌ `@unrdf/consensus` - RAFT consensus
- ❌ `@unrdf/collab` - CRDT collaboration

**Rationale**: All aspects of distributed runtime. Single package ensures coherent concurrency.

---

### 4.5 Merged into `@unrdf/ai`
- ❌ `@unrdf/ml-inference` - ONNX inference
- ❌ `@unrdf/ml-versioning` - Model versioning
- ❌ `@unrdf/semantic-search` - Vector search
- ❌ `@unrdf/yawl-ai` - Workflow optimization
- ❌ `@unrdf/decision-fabric` - Intent transformation

**Rationale**: All AI features share embeddings/vectors. Single package better.

---

### 4.6 Merged into `@unrdf/ui`
- ❌ `@unrdf/react` - React hooks
- ❌ `@unrdf/composables` - Vue composables
- ❌ `@unrdf/yawl-viz` - Workflow visualization
- ❌ `@unrdf/yawl-realtime` - Real-time collaboration

**Rationale**: All UI concerns. Framework exports via subpaths.

---

### 4.7 Merged into `@unrdf/tools`
- ❌ `@unrdf/test-utils` - Testing utilities
- ❌ `@unrdf/diataxis-kit` - Documentation framework
- ❌ `@unrdf/kgc-docs` - KGC Markdown
- ❌ `@unrdf/kgc-probe` - Integrity scanning
- ❌ `@unrdf/validation` - OTEL validation

**Rationale**: All development tools. Single package easier to maintain.

---

### 4.8 Merged into `@unrdf/cli`
- ❌ `@unrdf/cli` - Main CLI kept
- ❌ `@unrdf/kgc-cli` - Extension registry merged
- ❌ `@unrdf/kgc-tools` - Verification tools merged

**Rationale**: 3 CLI packages absurd. Plugin system handles extensions.

---

### 4.9 Merged into `@unrdf/hooks`
- ❌ `@unrdf/hooks` - Core kept
- ❌ `@unrdf/knowledge-engine` - Rule engine merged

**Rationale**: Hooks and rules both do pattern-based reasoning.

---

### 4.10 Merged into `@unrdf/observability`
- ❌ `@unrdf/observability` - Core kept
- ❌ `@unrdf/yawl-observability` - YAWL metrics merged

**Rationale**: Single OTEL instrumentation layer.

---

### 4.11 Merged into `@unrdf/store` + `@unrdf/rdf`
- ❌ `@unrdf/core` - Split into store (execution) + rdf (data model)
- ❌ `@unrdf/oxigraph` - Merged into @unrdf/store

**Rationale**: "core" is too vague. Separate concerns: execution vs. data model.

---

### 4.12 KILLED ENTIRELY (not included in v6)
- ❌ `@unrdf/atomvm` - BEAM VM in browser (niche, 8K LOC, zero adoption)
- ❌ `@unrdf/caching` - Redis/LRU caching (premature optimization, add later if needed)
- ❌ `@unrdf/dark-matter` - Query optimization (0 LOC, vaporware)
- ❌ `@unrdf/domain` - Empty package (0 LOC)
- ❌ `@unrdf/engine-gateway` - μ(O) enforcement (0 LOC, vaporware)
- ❌ `@unrdf/fusion` - "7-day innovation integration" (marketing cruft)
- ❌ `@unrdf/graph-analytics` - Graphlib wrapper (add to @unrdf/ai if needed)
- ❌ `@unrdf/kgc-claude` - Claude integration (23K LOC, belongs in separate monorepo)
- ❌ `@unrdf/kgc-swarm` - Multi-agent orchestration (belongs with kgc-claude)
- ❌ `@unrdf/kgn` - Template system (18K LOC, separate concern from RDF)
- ❌ `@unrdf/kgc-runtime` - Work item system (11K LOC, overlaps with workflows)
- ❌ `@unrdf/nextra` - Nextra docs (0 LOC, build tooling not package)
- ❌ `@unrdf/project-engine` - Self-hosting tools (654 LOC, dev-only)
- ❌ `@unrdf/v6-compat` - v5→v6 migration (temporary, not in v6 core)
- ❌ `@unrdf/v6-core` - Redundant with new @unrdf/governance
- ❌ `@unrdf/docs` - Private docs package (not publishable)
- ❌ `@unrdf/integration-tests` - Move to /test in monorepo root

**Rationale for KILL**:
- **AtomVM, KGN, KGC-Claude, KGC-Swarm** - Separate concerns, not core RDF platform
- **Caching, graph-analytics** - Premature optimizations, add later if metrics justify
- **Fusion, v6-compat** - Temporary/marketing packages
- **0 LOC packages** - Vaporware or placeholders

---

## 5. Merge Candidates (with rationale)

| Target Package | Source Packages | LOC Reduction | Rationale |
|----------------|----------------|---------------|-----------|
| `@unrdf/governance` | kgc-4d, receipts, blockchain, kgc-multiverse, kgc-substrate, v6-core | 14,881 → 12,000 | All do cryptographic provenance |
| `@unrdf/workflows` | yawl, yawl-durable | 40,835 → 35,000 | Durable execution is core feature |
| `@unrdf/runtime` | streaming, federation, consensus, collab | 11,886 → 10,000 | All distributed runtime concerns |
| `@unrdf/integrations` | yawl-api, yawl-kafka, yawl-queue, yawl-langchain, rdf-graphql, serverless | 7,113 → 6,000 | Thin adapters, plugin model |
| `@unrdf/ai` | ml-inference, ml-versioning, semantic-search, yawl-ai, decision-fabric | 6,903 → 5,000 | Shared embedding infrastructure |
| `@unrdf/ui` | react, composables, yawl-viz, yawl-realtime | 2,314 → 4,000 | All UI, framework subpaths |
| `@unrdf/tools` | test-utils, diataxis-kit, kgc-docs, kgc-probe, validation | 22,411 → 8,000 | All dev tooling |
| `@unrdf/cli` | cli, kgc-cli, kgc-tools | 22,865 → 8,000 | Single CLI with plugins |
| `@unrdf/hooks` | hooks, knowledge-engine | 15,986 → 12,000 | Both do rule-based reasoning |
| `@unrdf/observability` | observability, yawl-observability | 4,080 → 5,000 | Single OTEL layer |
| `@unrdf/store` | oxigraph, core (execution parts) | ~12,000 → 8,000 | Separate execution from data model |
| `@unrdf/rdf` | core (data model parts) | ~12,000 → 6,000 | Pure RDF operations |

**Total LOC**: 417,144 → ~120,000 (71% reduction through deduplication and elimination)

---

## 6. New Packages (What's Missing)

### 6.1 `@unrdf/store` (NEW - refactored from core/oxigraph)
**Why**: Clear separation between SPARQL execution and RDF data model.

### 6.2 `@unrdf/rdf` (NEW - refactored from core)
**Why**: Pure RDF concerns (parsing, validation) separate from execution.

### 6.3 `@unrdf/governance` (NEW - consolidation of 6 packages)
**Why**: Unified provenance API eliminates confusion between kgc-4d, receipts, blockchain.

### 6.4 `@unrdf/integrations` (NEW - consolidation of 6 adapter packages)
**Why**: Plugin model for external services better than package-per-integration.

### 6.5 `@unrdf/ai` (NEW - consolidation of 5 ML packages)
**Why**: Shared vector infrastructure for all AI features.

### 6.6 `@unrdf/ui` (NEW - consolidation of 4 UI packages)
**Why**: Framework-agnostic UI layer with React/Vue subpaths.

---

## 7. Migration Strategy (Phased)

### Phase 1: Foundation (Weeks 1-2)
1. Create `@unrdf/store` from oxigraph + core execution
2. Create `@unrdf/rdf` from core data model + parsers
3. Create `@unrdf/governance` consolidating kgc-4d, receipts, blockchain
4. **Gate**: All foundation tests passing, benchmark parity

### Phase 2: Runtime (Weeks 3-4)
1. Refactor `@unrdf/workflows` from yawl + yawl-durable
2. Create `@unrdf/runtime` from streaming, federation, consensus
3. Refactor `@unrdf/hooks` merging knowledge-engine
4. Consolidate `@unrdf/observability`
5. **Gate**: Workflow execution tests passing, federation working

### Phase 3: Applications (Weeks 5-6)
1. Create `@unrdf/integrations` from 6 adapter packages
2. Create `@unrdf/ai` from 5 ML packages
3. Create `@unrdf/ui` from 4 UI packages
4. Consolidate `@unrdf/cli` from 3 CLI packages
5. Consolidate `@unrdf/tools` from 5 tooling packages
6. **Gate**: E2E tests passing, documentation complete

### Phase 4: Validation (Week 7)
1. Run full test suite (target 90%+ coverage)
2. Performance benchmarks (must match or exceed v5)
3. Security audit (credential detection, injection patterns)
4. Documentation review (Diátaxis framework)
5. **Gate**: All quality gates passing (≥80/100 score)

---

## 8. Success Metrics

| Metric | Current (v5) | Target (v6) | Improvement |
|--------|--------------|-------------|-------------|
| **Package count** | 57 | 12 | 79% reduction |
| **Total LOC** | 417K | 120K | 71% reduction |
| **Circular dependencies** | Unknown | 0 | 100% elimination |
| **Duplicate concepts** | High | Zero | Complete dedup |
| **Dependency depth** | 4-5 layers | 3 layers | 40% reduction |
| **Time to onboard** | 2-3 days | 4 hours | 83% faster |
| **Build time** | ~60s | <20s | 67% faster |
| **Test execution** | ~45s | <15s | 67% faster |

---

## 9. Risks and Mitigation

### Risk 1: Breaking Changes for Users
**Mitigation**: Publish v6 as new major version. Keep v5 LTS branch for 12 months. Provide automated migration tool.

### Risk 2: Loss of Specialized Features
**Mitigation**: All features preserved, just reorganized. Plugin system allows extensibility.

### Risk 3: Merge Conflicts During Development
**Mitigation**: Phased migration with clear boundaries. Foundation → Runtime → Applications.

### Risk 4: Performance Regression
**Mitigation**: Benchmark suite runs on every commit. Gate on ±5% performance tolerance.

### Risk 5: Team Confusion During Transition
**Mitigation**: Clear documentation, migration guide, ADRs for every decision.

---

## 10. Architecture Decision Records (ADRs)

### ADR-001: Why 12 packages instead of 5 or 20?
**Decision**: 12 packages organized in 3 layers.
**Rationale**:
- 5 too few → Forces unrelated concerns together (e.g., UI + CLI in "applications")
- 20 too many → Back to current fragmentation problem
- 12 is sweet spot → Each package has clear, cohesive responsibility

### ADR-002: Why merge kgc-4d, receipts, blockchain?
**Decision**: Consolidate into `@unrdf/governance`.
**Rationale**: All do cryptographic provenance with duplicate Merkle tree code. Single API eliminates confusion.

### ADR-003: Why NOT consolidate YAWL integrations into workflows?
**Decision**: Separate `@unrdf/integrations` package.
**Rationale**: Keeps workflows core pure. Integrations are optional and change frequently.

### ADR-004: Why split core into store + rdf?
**Decision**: `@unrdf/store` for execution, `@unrdf/rdf` for data model.
**Rationale**: Separation of concerns. RDF parsing doesn't need SPARQL engine. Smaller dependency graphs.

### ADR-005: Why kill @unrdf/kgc-claude?
**Decision**: Move to separate `unrdf-claude` monorepo.
**Rationale**: Claude integration is 23K LOC with different release cycle. Not core RDF concern.

### ADR-006: Why observability is cross-cutting?
**Decision**: Can be imported by any layer.
**Rationale**: Metrics/tracing needed everywhere. Doesn't fit clean layer model. Special case acceptable.

---

## 11. File Structure (Monorepo)

```
/home/user/unrdf/
├── packages/
│   ├── store/              # @unrdf/store
│   │   ├── src/
│   │   │   ├── store.mjs
│   │   │   ├── query.mjs
│   │   │   └── index.mjs
│   │   ├── test/
│   │   └── package.json
│   │
│   ├── rdf/                # @unrdf/rdf
│   │   ├── src/
│   │   │   ├── data-model/
│   │   │   ├── parsers/
│   │   │   ├── serializers/
│   │   │   ├── validation/
│   │   │   └── index.mjs
│   │   ├── test/
│   │   └── package.json
│   │
│   ├── governance/         # @unrdf/governance
│   │   ├── src/
│   │   │   ├── time-travel/
│   │   │   ├── receipts/
│   │   │   ├── blockchain/
│   │   │   ├── freeze/
│   │   │   └── index.mjs
│   │   ├── test/
│   │   └── package.json
│   │
│   ├── workflows/          # @unrdf/workflows
│   ├── runtime/            # @unrdf/runtime
│   ├── hooks/              # @unrdf/hooks
│   ├── observability/      # @unrdf/observability
│   ├── cli/                # @unrdf/cli
│   ├── integrations/       # @unrdf/integrations
│   ├── ai/                 # @unrdf/ai
│   ├── ui/                 # @unrdf/ui
│   └── tools/              # @unrdf/tools
│
├── test/                   # Integration tests
├── benchmarks/             # Performance benchmarks
├── docs/                   # Documentation (Diátaxis)
├── examples/               # Example applications
└── scripts/                # Build/dev scripts
```

---

## 12. Conclusion

The v6 rewrite consolidates **57 packages → 12 packages** with **71% LOC reduction** through:
1. **Eliminating duplication** - 4 packages doing receipts → 1 governance package
2. **Plugin model** - 9 YAWL integrations → 1 integrations package with subpaths
3. **Clear layers** - 3 layers with strict dependency rules
4. **Killing cruft** - 17 packages eliminated entirely (vaporware, temporary, or wrong abstraction)

**The litmus test**: Can a new developer understand the entire architecture in 1 hour?
**Answer**: YES - 12 packages in 3 layers with clear boundaries.

---

## Appendix A: Package Comparison Table

| v5 Package | v5 LOC | v6 Package | Status |
|------------|--------|------------|--------|
| @unrdf/core | 23,616 | @unrdf/store + @unrdf/rdf | Split |
| @unrdf/oxigraph | 1,746 | @unrdf/store | Merged |
| @unrdf/kgc-4d | 6,693 | @unrdf/governance | Merged |
| @unrdf/receipts | 730 | @unrdf/governance | Merged |
| @unrdf/blockchain | 945 | @unrdf/governance | Merged |
| @unrdf/kgc-multiverse | 4,179 | @unrdf/governance | Merged |
| @unrdf/kgc-substrate | 2,334 | @unrdf/governance | Merged |
| @unrdf/yawl | 39,123 | @unrdf/workflows | Core |
| @unrdf/yawl-durable | 1,712 | @unrdf/workflows | Merged |
| @unrdf/streaming | 3,298 | @unrdf/runtime | Merged |
| @unrdf/federation | 4,070 | @unrdf/runtime | Merged |
| @unrdf/consensus | 2,143 | @unrdf/runtime | Merged |
| @unrdf/collab | 2,375 | @unrdf/runtime | Merged |
| @unrdf/hooks | 10,567 | @unrdf/hooks | Core |
| @unrdf/knowledge-engine | 5,419 | @unrdf/hooks | Merged |
| @unrdf/yawl-api | 1,002 | @unrdf/integrations | Merged |
| @unrdf/yawl-kafka | 1,548 | @unrdf/integrations | Merged |
| @unrdf/yawl-queue | 911 | @unrdf/integrations | Merged |
| @unrdf/yawl-langchain | 424 | @unrdf/integrations | Merged |
| @unrdf/rdf-graphql | 1,720 | @unrdf/integrations | Merged |
| @unrdf/serverless | 1,508 | @unrdf/integrations | Merged |
| @unrdf/ml-inference | 1,164 | @unrdf/ai | Merged |
| @unrdf/ml-versioning | 663 | @unrdf/ai | Merged |
| @unrdf/semantic-search | 768 | @unrdf/ai | Merged |
| @unrdf/yawl-ai | 1,925 | @unrdf/ai | Merged |
| @unrdf/decision-fabric | 2,383 | @unrdf/ai | Merged |
| @unrdf/react | 900 | @unrdf/ui | Merged |
| @unrdf/composables | 0 | @unrdf/ui | Merged |
| @unrdf/yawl-viz | 0 | @unrdf/ui | Merged |
| @unrdf/yawl-realtime | 1,414 | @unrdf/ui | Merged |
| @unrdf/cli | 4,814 | @unrdf/cli | Core |
| @unrdf/kgc-cli | 17,697 | @unrdf/cli | Merged |
| @unrdf/kgc-tools | 354 | @unrdf/cli | Merged |
| @unrdf/test-utils | 1,398 | @unrdf/tools | Merged |
| @unrdf/diataxis-kit | 2,620 | @unrdf/tools | Merged |
| @unrdf/kgc-docs | 1,583 | @unrdf/tools | Merged |
| @unrdf/kgc-probe | 16,810 | @unrdf/tools | Merged |
| @unrdf/validation | 4,141 | @unrdf/tools | Merged |
| @unrdf/observability | 2,184 | @unrdf/observability | Core |
| @unrdf/yawl-observability | 1,896 | @unrdf/observability | Merged |
| @unrdf/atomvm | 8,438 | - | KILLED |
| @unrdf/kgc-claude | 23,621 | - | KILLED (separate repo) |
| @unrdf/kgn | 18,581 | - | KILLED (separate repo) |
| @unrdf/kgc-runtime | 10,963 | - | KILLED (overlaps workflows) |
| @unrdf/kgc-swarm | 8,677 | - | KILLED (separate repo) |
| @unrdf/fusion | 8,192 | - | KILLED (marketing) |
| @unrdf/caching | 1,229 | - | KILLED (premature) |
| @unrdf/graph-analytics | 988 | - | KILLED (add to ai later) |
| @unrdf/project-engine | 654 | - | KILLED (dev-only) |
| @unrdf/v6-compat | 1,473 | - | KILLED (temporary) |
| @unrdf/v6-core | 13,459 | - | KILLED (redundant) |
| (17 more 0-LOC packages) | 0 | - | KILLED (vaporware) |

---

**Document Metadata**:
- Author: System Architect (Claude Code)
- Date: 2025-12-28
- Version: 1.0
- Status: PROPOSAL - Awaiting approval
- Git-backed receipt: [Generate after review]
