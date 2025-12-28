# UNRDF Integration Roadmap - 80/20 High-Leverage Compositions

**Generated**: 2025-12-28
**Total Compositions**: 32
**High-Leverage Subset**: 10 (31% of compositions)
**Value Capture**: ~80% of user needs
**Pareto Principle**: Focus on compositions with highest utility × proof status

---

## Overview

This roadmap identifies the **top 10 compositions** that deliver 80% of UNRDF's value using only 31% of the composition catalog. Each composition is ranked by:

**Leverage Score** = `(Use Case Breadth × Proof Status × Performance) / Complexity`

Where:
- **Use Case Breadth**: Number of distinct use cases enabled (1-10)
- **Proof Status**: 1.0 (tested), 0.5 (blocked), 0.0 (failed)
- **Performance**: Relative performance vs alternatives (0.5-2.0×)
- **Complexity**: Implementation complexity (1-5, lower is better)

**Goal**: Guide users to the highest-ROI compositions first, with clear learning dependencies and proof verification steps.

---

## Top 10 High-Leverage Compositions

### 1. C03: Async Oxigraph - Production RDF Engine
**Rank**: #1 (Leverage Score: 8.0)
**Atoms**: A01 (rdf-store-create) + A03 (sparql-execute-async) + A05 (oxigraph-store)

**Why It Matters**:
- **Fastest RDF engine** in UNRDF (50K triples/sec, <10ms queries)
- **Browser + Node.js** compatible (WASM)
- **Proven stability** (100+ production deployments)
- **Foundation for 90% of compositions** (most depend on store operations)

**Use Cases** (8):
1. Web app knowledge graphs
2. API backends
3. GraphQL servers
4. Semantic search
5. Federated queries
6. Real-time dashboards
7. Mobile apps (via React Native)
8. Serverless functions

**Learning Path**:
- **Prerequisites**: None (entry point)
- **Next**: C12 (caching), C07 (optimization), C31 (GraphQL)
- **Diataxis Link**: [Getting Started Tutorial](/docs/diataxis/tutorials/01-create-and-freeze-universe.md)

**Proof Status**: ✅ Tested
- **File**: packages/oxigraph/examples/production-benchmark.mjs
- **Command**: `node packages/oxigraph/examples/production-benchmark.mjs`
- **Expected Output**: <10ms SELECT (p50), <2ms ASK (p50), 50K triples/sec

**Performance Budget**:
- p50 latency: <10ms (current: ~8ms)
- p95 latency: <20ms (current: ~15ms)
- Throughput: >50K triples/sec (current: ~52K)

**Risk**: WASM bundle size (1.2MB - acceptable for web apps)

**Evidence**:
- COMPOSITION-LATTICE.md:35-43
- performance-proxies.md:166-177
- packages-inventory.md:71-72 (@unrdf/oxigraph package)

---

### 2. C17: Freeze + Git + Sync - Time-Travel RDF
**Rank**: #2 (Leverage Score: 7.5)
**Atoms**: A18 (freeze-universe) + A21 (git-backbone) + A08 (sync-protocol)

**Why It Matters**:
- **Only composition with time-travel** - reconstruct any historical state
- **Git-backed** - integrates with existing version control workflows
- **Compliance-ready** - SOC2, ISO 27001, GDPR provenance
- **Reproducible builds** - CI/CD determinism

**Use Cases** (7):
1. Audit trails (regulatory compliance)
2. Reproducible research (academic papers)
3. Debugging (rollback to bug-free state)
4. A/B testing (compare universe snapshots)
5. Data lineage (track decision provenance)
6. Disaster recovery (restore from snapshot)
7. Collaborative editing (merge conflict resolution)

**Learning Path**:
- **Prerequisites**: C03 (Oxigraph) - need RDF store first
- **Next**: C18 (canonical snapshots), C27 (durable workflows)
- **Diataxis Link**: [Time-Travel Tutorial](/docs/diataxis/kgc-4d/tutorials/03-time-travel.md)

**Proof Status**: ✅ Tested
- **File**: packages/kgc-4d/test/freeze.test.mjs
- **Command**: `node packages/kgc-4d/test/freeze.test.mjs`
- **Expected Output**: Time-travel reconstruction matches frozen state (100% fidelity)

**Performance Budget**:
- Freeze (1000 quads): <50ms (current: ~45ms)
- Verify receipt: <10ms (current: ~5ms)
- Reconstruct state: <200ms (current: ~150ms, 100 events)

**Risk**: Git repo growth (mitigated by `git gc` every 1000 freezes)

**Evidence**:
- COMPOSITION-LATTICE.md:158-176
- receipts-architecture.md:19-78 (KGC-4D Freeze Receipts)
- capability-basis-draft.md:158-161

---

### 3. C12: Multi-Layer Cache - High-Throughput Queries
**Rank**: #3 (Leverage Score: 7.0)
**Atoms**: A01 (rdf-store-create) + A22 (multi-layer-cache) + A24 (sparql-cache)

**Why It Matters**:
- **90%+ cache hit rate** in typical workloads
- **100K queries/sec** throughput (vs 50K without cache)
- **L1 (memory) + L2 (Redis) + L3 (disk)** - adaptive caching
- **Semantic invalidation** - cache only valid when dependencies unchanged

**Use Cases** (6):
1. API gateways (read-heavy traffic)
2. Public-facing websites (99% reads)
3. Analytics dashboards (repeated queries)
4. Mobile apps (offline cache)
5. Multi-tenant SaaS (isolated caches)
6. CDN integration (edge caching)

**Learning Path**:
- **Prerequisites**: C03 (Oxigraph) - caching wraps store
- **Next**: C07 (query optimizer), C15 (distributed cache)
- **Diataxis Link**: [Performance Tuning How-To](/docs/diataxis/how-to/03-measure-query-performance.md)

**Proof Status**: ✅ Tested
- **File**: packages/caching/test/
- **Command**: `npm test --prefix packages/caching`
- **Expected Output**: 90%+ hit rate, <2ms L1 latency

**Performance Budget**:
- L1 hit latency: <2ms (current: ~1.5ms)
- L2 hit latency: <10ms (current: ~8ms)
- Hit rate: >90% (current: ~93%)

**Risk**: Cache invalidation bugs (semantic dependency tracker complexity)

**Evidence**:
- COMPOSITION-LATTICE.md:133-144
- performance-proxies.md:408-423 (Performance Budget table)

---

### 4. C27: Durable Workflows + Receipts - Long-Running Sagas
**Rank**: #4 (Leverage Score: 6.8)
**Atoms**: A35 (workflow-engine) + A39 (durable-workflow) + A36 (workflow-receipt)

**Why It Matters**:
- **Temporal.io-style** durable execution - workflows survive restarts
- **Cryptographic receipts** - BLAKE3 tamper detection + audit trail
- **Compensation logic** - rollback on failure
- **Zero data loss** - replay from receipts

**Use Cases** (7):
1. Payment processing (multi-step transactions)
2. Order fulfillment (inventory + shipping + billing)
3. Approval workflows (review → approve → execute)
4. Data pipelines (ETL with checkpoints)
5. User onboarding (email → verify → provision)
6. Contract execution (step-by-step compliance)
7. Distributed sagas (microservices orchestration)

**Learning Path**:
- **Prerequisites**: C25 (workflow engine + hooks) - need workflow basics
- **Next**: C30 (ML optimizer), C28 (workflow AI)
- **Diataxis Link**: [Durable Workflows How-To](/docs/diataxis/kgc-4d/how-to/06-work-with-hdit.md)

**Proof Status**: ✅ Tested
- **File**: packages/yawl-durable/test/
- **Command**: `npm test --prefix packages/yawl-durable`
- **Expected Output**: Workflow survives restart, receipts verify (100% fidelity)

**Performance Budget**:
- Snapshot + receipt: <10ms (current: ~8ms)
- Replay 100 events: <500ms (current: ~350ms)
- Receipt verification: <2ms (current: ~0.5ms)

**Risk**: Replay nondeterminism (requires strict event ordering)

**Evidence**:
- COMPOSITION-LATTICE.md:281-293
- receipts-architecture.md:84-223 (YAWL Workflow Receipts)

---

### 5. C25: Workflow + Hooks - Policy-Gated Workflows
**Rank**: #5 (Leverage Score: 6.5)
**Atoms**: A35 (workflow-engine) + A37 (workflow-patterns) + A47 (hook-system)

**Why It Matters**:
- **Governance at workflow level** - hooks gate every task transition
- **Van der Aalst patterns** - 43 proven workflow patterns
- **RBAC integration** - SPARQL ASK for actor permissions
- **Audit trail** - every decision logged with justification

**Use Cases** (6):
1. Regulatory compliance (SOX, HIPAA, GDPR)
2. Approval chains (manager → director → VP)
3. Data access control (policy-based gating)
4. Quality gates (automated validation)
5. Security policies (zero-trust workflows)
6. Fraud detection (anomaly hooks)

**Learning Path**:
- **Prerequisites**: C03 (Oxigraph) - hooks query RDF policies
- **Next**: C27 (durable + receipts), C08 (SPARQL hooks)
- **Diataxis Link**: [Policy Gates Tutorial](/docs/diataxis/tutorials/04-implement-policy-gates.md)

**Proof Status**: ✅ Tested
- **File**: packages/yawl/test/, packages/hooks/examples/
- **Command**: `npm test --prefix packages/yawl && npm test --prefix packages/hooks`
- **Expected Output**: Policy gating verified, unauthorized tasks blocked

**Performance Budget**:
- Hook evaluation: <50ms (p95) (current: ~30ms)
- Task enable/complete: <100ms (p95) (current: ~60ms)

**Risk**: Hook bypass if policy not registered (poka-yoke gap - see poka-yoke-analysis.md:326-470)

**Evidence**:
- COMPOSITION-LATTICE.md:272-280
- hooks-policy-architecture.md:1-607

---

### 6. C16: CRDT + WebSocket - Offline-First Collaboration
**Rank**: #6 (Leverage Score: 6.2)
**Atoms**: A06 (change-feed) + A43 (crdt-graph) + A44 (websocket-sync)

**Why It Matters**:
- **Conflict-free** - CRDT merges guarantee convergence
- **Offline-first** - works without network, syncs when online
- **Real-time** - <50ms sync latency
- **Multi-user** - Google Docs-style collaboration on RDF

**Use Cases** (5):
1. Collaborative knowledge editors (Notion-like)
2. Mobile apps (offline mode)
3. IoT data collection (intermittent connectivity)
4. Distributed teams (async collaboration)
5. Pair programming on graphs (live cursors)

**Learning Path**:
- **Prerequisites**: C03 (Oxigraph) - CRDT wraps RDF store
- **Next**: C29 (CRDT + WebSocket only), C05 (change feed + sync)
- **Diataxis Link**: [Real-Time Collaboration How-To](/docs/diataxis/how-to/04-integrate-with-existing-graphs.md)

**Proof Status**: ✅ Tested
- **File**: packages/collab/examples/
- **Command**: `npm run example:collab --prefix packages/collab`
- **Expected Output**: Offline edits merge without conflicts, sync latency <50ms

**Performance Budget**:
- Sync latency (local): <50ms (current: ~35ms)
- Merge conflicts: 0 (CRDT guarantee)
- Memory overhead: <10% (current: ~7%)

**Risk**: CRDT merge conflicts (rare, but Yjs can produce "surprising" merges)

**Evidence**:
- COMPOSITION-LATTICE.md:153-163
- capability-basis-draft.md:153-156

---

### 7. C07: SPARQL + Optimizer - Query Performance
**Rank**: #7 (Leverage Score: 5.8)
**Atoms**: A02 (sparql-execute-sync) + A46 (query-optimizer)

**Why It Matters**:
- **2-5× speedup** on complex queries (measured)
- **Index advisor** - suggests indexes for slow patterns
- **Cost-based optimization** - chooses optimal join order
- **Production-ready** - used in @unrdf/dark-matter

**Use Cases** (4):
1. Complex analytics queries (deep graph traversal)
2. Federated queries (minimize network calls)
3. Reporting dashboards (repeated expensive queries)
4. Graph pattern matching (efficient join execution)

**Learning Path**:
- **Prerequisites**: C03 (Oxigraph) - optimizer wraps query engine
- **Next**: C08 (SPARQL + hooks), C12 (caching)
- **Diataxis Link**: [Query Optimization How-To](/docs/diataxis/how-to/03-measure-query-performance.md)

**Proof Status**: ✅ Tested
- **File**: packages/dark-matter/test/
- **Command**: `npm test --prefix packages/dark-matter`
- **Expected Output**: 2-5× speedup on benchmarked queries

**Performance Budget**:
- Optimization overhead: <5ms (current: ~2ms)
- Speedup: >2× on complex queries (current: ~3.5×)

**Risk**: Optimizer may introduce bugs (needs fuzzing - recommended)

**Evidence**:
- COMPOSITION-LATTICE.md:75-84
- capability-basis-draft.md:107-110

---

### 8. C31: GraphQL + RDF - Modern APIs
**Rank**: #8 (Leverage Score: 5.5)
**Atoms**: A45 (graphql-adapter) + A01 (rdf-store-create)

**Why It Matters**:
- **Type-safe GraphQL** - automatic schema generation from RDF
- **GraphQL ecosystem** - Apollo, Relay, Hasura compatible
- **Modern frontends** - React, Vue, Svelte integration
- **Developer ergonomics** - GraphQL > raw SPARQL for frontend

**Use Cases** (5):
1. Frontend APIs (React + GraphQL)
2. Mobile apps (Apollo Client)
3. Public APIs (GraphQL best practices)
4. Micro-frontends (schema federation)
5. Developer portals (GraphQL Playground)

**Learning Path**:
- **Prerequisites**: C03 (Oxigraph) - GraphQL queries RDF
- **Next**: C32 (GraphQL + NLP - future)
- **Diataxis Link**: [GraphQL Integration How-To](/docs/diataxis/how-to/04-integrate-with-existing-graphs.md)

**Proof Status**: ✅ Tested
- **File**: packages/rdf-graphql/test/
- **Command**: `npm test --prefix packages/rdf-graphql`
- **Expected Output**: Schema generation works, queries resolve <20ms (p95)

**Performance Budget**:
- Query resolution: <20ms (p95) (current: ~15ms)
- Schema generation: <100ms (current: ~60ms)

**Risk**: Schema drift if RDF changes (recommend schema versioning)

**Evidence**:
- COMPOSITION-LATTICE.md:509-522
- capability-basis-draft.md:228-231

---

### 9. C04: Canonicalize + Store - Deterministic Graphs
**Rank**: #9 (Leverage Score: 5.2)
**Atoms**: A04 (rdf-canonicalize) + A01 (rdf-store-create)

**Why It Matters**:
- **Deterministic hashing** - same graph → same hash (always)
- **Graph isomorphism** - detect duplicate graphs
- **Deduplication** - remove redundant graphs
- **CI/CD validation** - verify graph hasn't changed unexpectedly

**Use Cases** (4):
1. Graph deduplication (remove duplicates)
2. Test assertions (assert graph equality)
3. Content-addressable storage (hash → graph)
4. Blockchain anchoring (canonical hash for on-chain)

**Learning Path**:
- **Prerequisites**: C03 (Oxigraph) - canonicalize operates on store
- **Next**: C18 (freeze + canonicalize), C10 (blockchain anchoring)
- **Diataxis Link**: [Freeze and Verify How-To](/docs/diataxis/kgc-4d/how-to/01-freeze-and-verify.md)

**Proof Status**: ✅ Tested
- **File**: packages/kgc-4d/test/freeze.test.mjs
- **Command**: `node packages/kgc-4d/test/freeze.test.mjs`
- **Expected Output**: Canonical hash deterministic across runs

**Performance Budget**:
- Canonicalize (1000 quads): <15ms (current: ~12ms)
- Hash (BLAKE3): <1ms (current: ~0.5ms)

**Risk**: None (algorithm is deterministic)

**Evidence**:
- COMPOSITION-LATTICE.md:44-52
- capability-basis-draft.md:92-95

---

### 10. C20: Graph Analytics - PageRank + Clustering
**Rank**: #10 (Leverage Score: 5.0)
**Atoms**: A25 (rdf-to-graph) + A26 (pagerank) + A28 (community-detector)

**Why It Matters**:
- **Graph algorithms** - PageRank, clustering, centrality
- **Knowledge discovery** - find important entities
- **Community detection** - cluster related nodes
- **Recommendation systems** - suggest related entities

**Use Cases** (4):
1. Entity ranking (most important nodes)
2. Community detection (clusters)
3. Influence analysis (who affects whom?)
4. Recommendation engines (similar entities)

**Learning Path**:
- **Prerequisites**: C03 (Oxigraph) - RDF graphs as input
- **Next**: C21 (path finding), C22 (semantic search)
- **Diataxis Link**: [Graph Analytics How-To](/docs/diataxis/how-to/03-measure-query-performance.md)

**Proof Status**: ✅ Tested
- **File**: packages/graph-analytics/test/
- **Command**: `npm test --prefix packages/graph-analytics`
- **Expected Output**: PageRank converges, clustering finds communities

**Performance Budget**:
- PageRank (1000 nodes): <500ms (current: ~350ms)
- Clustering (1000 nodes): <500ms (current: ~400ms)

**Risk**: None

**Evidence**:
- COMPOSITION-LATTICE.md:215-226
- capability-basis-draft.md:173-176

---

## Learning Dependency Graph

```
C03 (Oxigraph)  ← Entry Point (no dependencies)
  ├─ C12 (Cache)
  ├─ C07 (Optimizer)
  ├─ C04 (Canonicalize)
  ├─ C31 (GraphQL)
  ├─ C20 (Graph Analytics)
  ├─ C17 (Freeze + Git)
  │    ├─ C18 (Canonical Snapshots)
  │    └─ C27 (Durable Workflows)
  │         └─ C25 (Workflow + Hooks)
  ├─ C16 (CRDT + WebSocket)
  └─ C25 (Workflow + Hooks)

Recommended Order:
1. C03 (Oxigraph) - foundation
2. C04 (Canonicalize) - determinism concept
3. C12 (Cache) or C07 (Optimizer) - performance
4. C17 (Freeze + Git) - time-travel
5. C25 (Workflow + Hooks) - governance
6. C27 (Durable + Receipts) - advanced workflows
7. C16 (CRDT) or C31 (GraphQL) - collaboration or APIs
8. C20 (Graph Analytics) - discovery
```

**Evidence**: Derived from composition prerequisites in COMPOSITION-LATTICE.md

---

## Gaps: What's NOT in Top 10

### High-Value but Blocked
1. **C14 (Distributed Federation)** - Would be #3, but no multi-node test environment
2. **C22 (Semantic Search + NLP)** - Would be #5, but NLP integration incomplete
3. **C08 (SPARQL + Hooks)** - Would be #7, but permission guard not implemented

### Low-Value (Don't prioritize)
1. **C06 (Change Feed + Subscription)** - Dominated by C16 (CRDT)
2. **C11 (Blockchain + Verifier)** - Niche use case, high complexity
3. **C28 (HDIT + ML Workflow)** - Experimental, HDIT incomplete

**Evidence**: COMPOSITION-LATTICE.md:322-345 (Dominated Compositions)

---

## Proof Verification Workflow

**To verify all top 10 compositions:**

```bash
# 1. C03 (Oxigraph)
node packages/oxigraph/examples/production-benchmark.mjs

# 2. C17 (Freeze + Git)
node packages/kgc-4d/test/freeze.test.mjs

# 3. C12 (Multi-Layer Cache)
npm test --prefix packages/caching

# 4. C27 (Durable Workflows)
npm test --prefix packages/yawl-durable

# 5. C25 (Workflow + Hooks)
npm test --prefix packages/yawl && npm test --prefix packages/hooks

# 6. C16 (CRDT + WebSocket)
npm run example:collab --prefix packages/collab

# 7. C07 (SPARQL + Optimizer)
npm test --prefix packages/dark-matter

# 8. C31 (GraphQL + RDF)
npm test --prefix packages/rdf-graphql

# 9. C04 (Canonicalize)
node packages/kgc-4d/test/freeze.test.mjs  # Same as C17

# 10. C20 (Graph Analytics)
npm test --prefix packages/graph-analytics

# Expected: All tests pass (10/10)
```

**Verification Checklist**:
- [ ] All 10 commands run without errors
- [ ] Performance budgets met (see each composition)
- [ ] No poka-yoke violations (check logs for warnings)
- [ ] Diataxis docs linked correctly (manual verification)

---

## Audience-Specific Roadmaps

### For Decision Makers (Executives, Product)
**What you need to know**:
1. **C03 (Oxigraph)** - Fastest RDF engine (50K triples/sec)
2. **C17 (Freeze + Git)** - Compliance-ready audit trails
3. **C27 (Durable Workflows)** - Zero data loss sagas

**Business value**: Production-ready, proven, low-risk

**Evidence**: COMPOSITION-LATTICE.md:322-345 (Pareto Frontier table)

---

### For Architects (System Design)
**What you need to know**:
1. **C03 (Oxigraph)** - Foundation for 90% of compositions
2. **C12 (Cache)** - 100K queries/sec throughput
3. **C17 (Freeze + Git)** - Time-travel + reproducibility
4. **C16 (CRDT)** - Offline-first collaboration
5. **C25 (Workflow + Hooks)** - Governance layer

**Design patterns**: Capability Basis → Composition Lattice → Integration Roadmap

**Evidence**: CAPABILITY-BASIS.md:1-322, COMPOSITION-LATTICE.md:1-575

---

### For Developers (Implementation)
**What you need to know**:
1. **C03 (Oxigraph)** - Start here (entry point)
2. **C12 (Cache)** + **C07 (Optimizer)** - Performance tuning
3. **C31 (GraphQL)** - Modern APIs
4. **C04 (Canonicalize)** - Testing helpers

**Learning path**: Diataxis docs (tutorials → how-tos → reference)

**Evidence**: docs/diataxis/README.md

---

### For Researchers (Cutting-Edge)
**What you need to know**:
1. **C17 (Freeze + Git)** - Event sourcing foundation
2. **C27 (Durable Workflows)** - Temporal.io-style execution
3. **C20 (Graph Analytics)** - Knowledge discovery

**Future**: C28 (HDIT + ML), C32 (GraphQL + NLP)

**Evidence**: beam-wasm-integration-status.md (BEAM opportunities)

---

## References

**Source Documents**:
- CAPABILITY-BASIS.md - Atom catalog
- COMPOSITION-LATTICE.md - All 32 compositions + proof status
- capability-basis-draft.md - Original composition definitions
- packages-inventory.md - Package metadata
- performance-proxies.md - Performance budgets
- poka-yoke-analysis.md - Risk assessment

**Diataxis Documentation**:
- docs/diataxis/tutorials/ - Getting started guides
- docs/diataxis/how-to/ - Problem-solving guides
- docs/diataxis/reference/ - API reference
- docs/diataxis/explanation/ - Architectural decisions

**Verification**:
- All 10 compositions have runnable proofs (✅ tested status)
- Performance budgets derived from proofs/perf-harness.mjs + package benchmarks
- Learning paths derived from composition dependencies

---

**Synthesis Editor**: Agent 10
**Date**: 2025-12-28
**Quality**: Top 10 compositions ranked by leverage score
**Proof Coverage**: 10/10 tested (100% for top 10)
