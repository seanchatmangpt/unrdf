# UNRDF v5 Substrate VOC Analysis: Defining Core vs Composable

## Overview
This document synthesizes 7 synthetic Voices of the Customer (VOCs) to establish clear boundaries between the core substrate that UNRDF v5 provides and the composable extensions users must build themselves.

**Core Principle**: UNRDF v5 is the **building substrate** (RDF graph operations, Knowledge Hooks, federation primitives), not the **application framework** (Knowledge Engine, composables, domain models).

---

## 4 AI Agent/Swarm VOCs

### VOC-1: **Autonomous Knowledge Agent** (Swarm Coordinator)
**Persona**: Decentralized multi-agent system orchestrating RDF transformations across federated knowledge graphs

**Needs**:
- Low-level graph mutation primitives (add/remove quads, semantic reasoning)
- Streaming change feeds to coordinate state across agents
- Federation primitives to discover and query remote agents
- Knowledge Hooks as policy enforcement (not knowledge engine rules)
- Direct SPARQL execution without interpretive layers

**Does NOT need**:
- Knowledge Engine's rule engine (agents encode rules internally)
- Composables like `use-graph`, `use-validator` (agents wrap these)
- Dark Matter optimizations (agents implement own optimization)
- HTF framework integration (agents bring their own)

**Quote**: *"We need the raw RDF substrate and streaming primitives. We'll build our own coordination layer on top."*

---

### VOC-2: **Real-time Graph Sync Agent** (Stream Processor)
**Persona**: Agent maintaining consistency across multiple UNRDF instances in distributed systems

**Needs**:
- Streaming change feed with guaranteed delivery semantics
- Bidirectional sync primitives (delta application)
- Federation connection pools
- Knowledge Hooks for mutation policies only
- Memory-efficient streaming (small payloads)

**Does NOT need**:
- Query optimization (streaming is write-optimized)
- Caching layers (agents manage their own LRU)
- Performance monitoring (agents integrate OTEL themselves)
- AI semantic enrichment (agents control enrichment strategy)

**Quote**: *"Stream the deltas, give us federation primitives. Let us build the sync protocol."*

---

### VOC-3: **Autonomous ML Agent** (Pattern Learner)
**Persona**: Agent learning patterns from RDF data and applying them autonomously

**Needs**:
- SPARQL query execution (read-only or controlled write)
- Graph canonicalization for consistent pattern matching
- Knowledge Hooks for applying learned patterns as policies
- Streaming subscriptions to trigger re-learning
- Direct access to raw quads (no interpretation)

**Does NOT need**:
- Knowledge Engine's inference rules
- Domain models (agent learns own patterns)
- Composables or UI helpers
- Federation for their own data (federation is for external knowledge)

**Quote**: *"Give us SPARQL, Hooks for policy, and streaming triggers. We'll learn and apply our own patterns."*

---

### VOC-4: **Autonomous Audit Agent** (Compliance Monitor)
**Persona**: Agent ensuring RDF graphs maintain compliance policies continuously

**Needs**:
- Query capabilities (SPARQL with reasoning)
- Change feed subscriptions (react to mutations immediately)
- Knowledge Hooks for enforcement (halt on violation)
- Audit logging (who did what when)
- Direct quad access (trace compliance to source)

**Does NOT need**:
- Knowledge Engine rules (agent encodes compliance policies)
- Domain models (agent validates against standards)
- Dark Matter (agent defines own thresholds)
- Composables (agent is bare metal)

**Quote**: *"Stream us changes, let us subscribe to hooks, give us full traceability. Compliance enforcement is our job."*

---

## 3 Human Developer/Operator VOCs

### VOC-5: **RDF Data Engineer** (Data Pipeline Builder)
**Persona**: Builds ETL pipelines loading/transforming RDF data into UNRDF instances

**Needs**:
- Graph creation and CRUD (create, read, update, delete graphs)
- SPARQL query execution
- Federation for linking datasets
- Streaming for incremental loads
- Knowledge Hooks for validation during ingestion
- CLI tools for scripting pipelines

**Does NOT need**:
- Knowledge Engine (they bring their own orchestration)
- Composables (Python/Java scripts don't use these)
- Dark Matter (they optimize their own pipelines)
- AI semantic enrichment (external system handles that)

**Quote**: *"I need reliable graph operations, federation to connect sources, and hooks to validate. Keep it simple."*

---

### VOC-6: **Knowledge Graph Application Developer** (App Builder)
**Persona**: Builds web/mobile apps using UNRDF as a backend

**Needs**:
- Knowledge Hooks for client-side policy enforcement
- Composables like `use-graph`, `use-delta`, `use-terms`
- Browser SDK with IndexedDB storage
- Streaming for real-time UI updates
- Federation discovery and querying
- CLI for bootstrapping app projects

**Does NOT need**:
- Knowledge Engine (app brings its own business logic)
- Dark Matter (app optimizes its own critical paths)
- Project Engine (not building UNRDF itself)
- Audit logging (app logs what matters to them)

**Quote**: *"Give me composables, Hooks for validation, real-time streaming, and a solid browser SDK. The rest is my app."*

---

### VOC-7: **Infrastructure & Platform Operator** (DevOps)
**Persona**: Deploys, monitors, and operates UNRDF instances in production

**Needs**:
- Docker images, K8s manifests, Terraform configs
- OTEL instrumentation and metrics
- Federation for multi-instance deployments
- Streaming health checks and audit trails
- Knowledge Hooks for operational policies
- CLI tools for instance management
- Graceful degradation primitives

**Does NOT need**:
- Knowledge Engine's rule complexity (ops monitors graphs, not rules)
- Composables (they're not building apps)
- Dark Matter (ops cares about reliability, not optimization)
- Domain models (domain is operations, not knowledge)

**Quote**: *"Instrument everything, give us K8s/Docker/Terraform, and let us write policies. Keep the system observable and stable."*

---

## Summary: Core Substrate Boundaries

### ✅ KEEP IN v5 CORE SUBSTRATE
These are the **building blocks** users expect:
- RDF graph operations (N3 Store wrapping, canonicalization)
- SPARQL query execution (via Comunica)
- Knowledge Hooks (policy definition and execution)
- Federation primitives (connection pooling, query routing)
- Streaming (change feeds, subscriptions, real-time sync)
- Browser SDK (IndexedDB, shims, comps)
- CLI core (graph/context management, hook evaluation)
- Observability (OTEL instrumentation points)
- Security (validation, sandbox restrictions)

### ❌ MOVE OUT OF v5 CORE
These are **application-level patterns** users should build:
- **Knowledge Engine** → Community package for rule engines
- **Composables** (except browser essentials) → Use-graph-builder patterns docs
- **Dark Matter** → Performance optimization as separate lib
- **Project Engine** → Document self-hosting patterns
- **Domain Models** (paper, thesis) → Examples only
- **AI Semantic Integration** → Plugin architecture docs
- **Monitoring/Andon** → OTEL instrumentation points, users build watchers
- **HTF Framework** → Document hook testing patterns

### 🎯 DEFINE INTERFACES, NOT IMPLEMENTATIONS
Move to thin adapter layers:
```javascript
// KEEP: These are substrate interfaces
- queryStore(store, sparql)        // Comunica wrapper
- subscribeToChanges(store)        // Streaming interface
- applyPolicy(quad, hook)          // Hook executor
- federateTo(peers)                // Federation interface

// MOVE: These are application patterns
- inferPatterns(store)             // Knowledge Engine
- optimizeQuery(query, store)      // Dark Matter
- validateAgainstSpec(graph)       // Project Engine
- enrichWithAI(quad)               // Plugin system
```

---

## Implementation Priorities

### Phase 1: Preserve Core (v5.0)
1. RDF operations (N3 Store, quads, canonicalization)
2. SPARQL execution (Comunica)
3. Knowledge Hooks (simple policy execution)
4. Federation basics (peer discovery)
5. Streaming primitives (change feed)
6. CLI essential commands
7. Browser SDK essentials
8. OTEL instrumentation points

### Phase 2: Extract Libraries (v5.1+)
1. Extract Knowledge Engine → `@unrdf/knowledge-engine`
2. Extract Dark Matter → `@unrdf/dark-matter`
3. Extract Composables → `@unrdf/composables-web`
4. Extract Project Engine → `@unrdf/project-engine`
5. Extract Domain Models → Examples and docs

### Phase 3: Document Extensions (v5.2+)
1. Knowledge Engine plugin docs
2. Custom composables patterns
3. Federation protocol specs
4. Dark Matter optimization guide
5. Hook policy libraries (community)

---

## Why This Matters

**For VOCs 1-4 (AI Agents)**: They get raw substrate to build specialized coordination. Knowledge Engine constraints would prevent their use cases.

**For VOC-5 (Data Engineer)**: They need reliable operations, not framework features. Federation + Hooks + Streaming = their entire toolkit.

**For VOC-6 (App Developer)**: They need composables + Hooks + streaming. Knowledge Engine is overhead they'll replace with app logic anyway.

**For VOC-7 (DevOps)**: They need observable primitives, not complex rule engines. Observability points > opaque implementations.

**Result**: UNRDF v5 becomes the **RDF substrate platform** instead of a **RDF application framework**. Better for everyone.
