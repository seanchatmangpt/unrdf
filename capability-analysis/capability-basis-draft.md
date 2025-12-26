# UNRDF Capability Basis - Atom Catalog

**Generated**: 2025-12-26
**Packages Analyzed**: 43
**Capability Atoms Identified**: 47

## Capability Atom Table

| Atom ID | Atom Name | Package | Runtime | Evidence (file:line) | Invariants | Compositions Using |
|---------|-----------|---------|---------|---------------------|-----------|-------------------|
| A01 | rdf-store-create | @unrdf/core | Node, Browser | packages/core/src/index.mjs:18 | Sync, Deterministic | C01, C02, C03, C05, C07, C12, C15, C22 |
| A02 | sparql-execute-sync | @unrdf/core | Node, Browser | packages/core/src/index.mjs:22 | Sync, Pure | C01, C07, C08, C12 |
| A03 | sparql-execute-async | @unrdf/core | Node, Browser | packages/core/src/index.mjs:52 | Async | C02, C03, C09 |
| A04 | rdf-canonicalize | @unrdf/core | Node, Browser | packages/core/src/index.mjs:49 | Pure, Frozen | C04, C18 |
| A05 | oxigraph-store | @unrdf/oxigraph | Node, WASM | packages/oxigraph/src/index.mjs:9 | Native, Fast | C01, C02, C05, C15 |
| A06 | change-feed | @unrdf/streaming | Node | packages/streaming/src/index.mjs:10 | Realtime, Async | C05, C06, C16 |
| A07 | subscription-manager | @unrdf/streaming | Node | packages/streaming/src/index.mjs:13 | Realtime, Stateful | C05, C06 |
| A08 | sync-protocol | @unrdf/streaming | Node | packages/streaming/src/index.mjs:19 | Deterministic, Checksummed | C05, C16, C17 |
| A09 | otel-validator | @unrdf/validation | Node | packages/validation/src/index.mjs:11 | Testless, Span-based | C09, C20 |
| A10 | receipt-anchorer | @unrdf/blockchain | Node | packages/blockchain/src/index.mjs:11 | Crypto, Ethereum | C10, C11, C17 |
| A11 | merkle-proof | @unrdf/blockchain | Node, Browser | packages/blockchain/src/index.mjs:17 | Crypto, Verifiable | C10, C11 |
| A12 | workflow-verifier | @unrdf/blockchain | Node | packages/blockchain/src/index.mjs:14 | Smart-contract | C10, C11 |
| A13 | raft-coordinator | @unrdf/consensus | Node | packages/consensus/src/index.mjs:28 | Distributed, Leader-election | C13, C14 |
| A14 | cluster-manager | @unrdf/consensus | Node | packages/consensus/src/index.mjs:29 | Distributed, Health-check | C13, C14 |
| A15 | distributed-state-machine | @unrdf/consensus | Node | packages/consensus/src/index.mjs:30 | Distributed, Replicated | C13, C14 |
| A16 | federated-query | @unrdf/federation | Node | packages/federation/src/index.mjs:17 | Distributed, P2P | C14, C15 |
| A17 | peer-manager | @unrdf/federation | Node | packages/federation/src/index.mjs:13 | Distributed, Discovery | C14, C15 |
| A18 | freeze-universe | @unrdf/kgc-4d | Node | packages/kgc-4d/src/index.mjs:9 | Git-backed, Snapshot | C17, C18, C19 |
| A19 | vector-clock | @unrdf/kgc-4d | Node, Browser | packages/kgc-4d/src/index.mjs:10 | Nanosecond, Causal | C17, C19 |
| A20 | hdit-coords | @unrdf/kgc-4d | Node, Browser | packages/kgc-4d/src/index.mjs:26 | Hyperdimensional, Similarity | C19, C28 |
| A21 | git-backbone | @unrdf/kgc-4d | Node | packages/kgc-4d/src/index.mjs:8 | Git, Immutable | C17, C18 |
| A22 | multi-layer-cache | @unrdf/caching | Node | packages/caching/src/index.mjs:25 | L1+L2+L3, Redis | C12, C15 |
| A23 | dependency-tracker | @unrdf/caching | Node | packages/caching/src/index.mjs:26 | Semantic, Invalidation | C12, C15 |
| A24 | sparql-cache | @unrdf/caching | Node | packages/caching/src/index.mjs:27 | Query-cache, Smart | C12, C15 |
| A25 | rdf-to-graph | @unrdf/graph-analytics | Node | packages/graph-analytics/src/index.mjs:13 | Conversion | C20, C21 |
| A26 | pagerank | @unrdf/graph-analytics | Node | packages/graph-analytics/src/index.mjs:20 | Centrality, Algorithm | C20, C21 |
| A27 | relationship-finder | @unrdf/graph-analytics | Node | packages/graph-analytics/src/index.mjs:26 | Path-finding, Graph | C20, C21 |
| A28 | community-detector | @unrdf/graph-analytics | Node | packages/graph-analytics/src/index.mjs:34 | Clustering, LPA | C20, C21 |
| A29 | rdf-embedder | @unrdf/semantic-search | Node | packages/semantic-search/src/index.mjs:12 | Vector, Transformer | C22, C23 |
| A30 | semantic-query-engine | @unrdf/semantic-search | Node | packages/semantic-search/src/index.mjs:13 | NLP, Hybrid | C22, C23 |
| A31 | knowledge-recommender | @unrdf/semantic-search | Node | packages/semantic-search/src/index.mjs:14 | ML, Discovery | C22, C23 |
| A32 | onnx-runner | @unrdf/ml-inference | Node | packages/ml-inference/src/index.mjs:14 | ONNX, Inference | C23, C24 |
| A33 | streaming-inference | @unrdf/ml-inference | Node | packages/ml-inference/src/index.mjs:16 | Pipeline, Batch | C23, C24 |
| A34 | model-registry | @unrdf/ml-inference | Node | packages/ml-inference/src/index.mjs:19 | Model-mgmt | C23, C24 |
| A35 | workflow-engine | @unrdf/yawl | Node | packages/yawl/src/index.mjs:168 | YAWL, Patterns | C25, C26, C27, C30 |
| A36 | workflow-receipt | @unrdf/yawl | Node | packages/yawl/src/index.mjs:213 | Crypto, Audit | C25, C26, C27 |
| A37 | workflow-patterns | @unrdf/yawl | Node | packages/yawl/src/index.mjs:223 | Van-der-Aalst, 43-patterns | C25, C26 |
| A38 | yawl-rdf-store | @unrdf/yawl | Node | packages/yawl/src/index.mjs:131 | RDF-native, SPARQL | C25, C26, C27 |
| A39 | durable-workflow-engine | @unrdf/yawl-durable | Node | packages/yawl-durable/src/engine.mjs:74 | Temporal-style, Replay | C27, C30 |
| A40 | workflow-predictor | @unrdf/yawl-ai | Node | packages/yawl-ai/src/index.mjs:42 | ML, Path-prediction | C28, C30 |
| A41 | performance-optimizer | @unrdf/yawl-ai | Node | packages/yawl-ai/src/index.mjs:47 | ML, Bottleneck | C28, C30 |
| A42 | anomaly-detector | @unrdf/yawl-ai | Node | packages/yawl-ai/src/index.mjs:52 | ML, Pattern-detect | C28, C30 |
| A43 | crdt-graph | @unrdf/collab | Node, Browser | packages/collab/src/index.mjs:17 | CRDT, Yjs | C16, C29 |
| A44 | websocket-sync | @unrdf/collab | Node, Browser | packages/collab/src/index.mjs:20 | Realtime, WebSocket | C16, C29 |
| A45 | graphql-adapter | @unrdf/rdf-graphql | Node | packages/rdf-graphql/src/adapter.mjs:26 | GraphQL, Type-safe | C31, C32 |
| A46 | query-optimizer | @unrdf/dark-matter | Node | packages/dark-matter/src/index.mjs:17 | SPARQL-opt, Index-advisor | C07, C08, C12 |
| A47 | hook-system | @unrdf/knowledge-engine | Node | packages/knowledge-engine/src/index.mjs:15 | Policy, Gate | C08, C09, C25 |

## Runtime Target Summary

- **Node-only**: 27 atoms
- **Browser-capable**: 6 atoms
- **Node + Browser**: 8 atoms
- **WASM**: 1 atom (Oxigraph)

## Invariant Classification

- **Deterministic**: 12 atoms (A01, A04, A05, A08, A13-A15, A18, A19)
- **Crypto/Verifiable**: 8 atoms (A10, A11, A12, A18, A21, A36)
- **Distributed**: 7 atoms (A13-A17, A43, A44)
- **ML-powered**: 7 atoms (A29-A34, A40-A42)
- **Pure/Functional**: 3 atoms (A02, A04)
- **Realtime**: 4 atoms (A06, A07, A43, A44)

## Composition Groups

### C01: Sync RDF Store + Query
**Atoms**: A01 (rdf-store-create) + A02 (sparql-execute-sync)
**Value**: Synchronous RDF operations without async overhead
**Use Case**: CLI tools, serverless functions

### C02: Async RDF Store + Query
**Atoms**: A01 (rdf-store-create) + A03 (sparql-execute-async)
**Value**: Full async RDF pipeline
**Use Case**: Web applications, APIs

### C03: Async Store + SPARQL + Oxigraph
**Atoms**: A01 + A03 + A05
**Value**: High-performance async RDF with native backend
**Use Case**: Production knowledge graphs

### C04: RDF Canonicalization + Store
**Atoms**: A04 (rdf-canonicalize) + A01
**Value**: Canonical RDF graphs with consistent hashing
**Use Case**: Graph deduplication, isomorphism testing

### C05: RDF Store + Change Feed + Sync Protocol
**Atoms**: A01 + A06 + A08
**Value**: Real-time RDF synchronization
**Use Case**: Collaborative editing, distributed graphs

### C06: Change Feed + Subscription Manager
**Atoms**: A06 + A07
**Value**: Pub/sub for RDF changes
**Use Case**: Reactive UIs, event-driven systems

### C07: SPARQL + Query Optimizer
**Atoms**: A02 + A46
**Value**: Optimized SPARQL execution
**Use Case**: Complex queries, performance-critical apps

### C08: SPARQL + Query Optimizer + Hook System
**Atoms**: A02 + A46 + A47
**Value**: Policy-gated optimized queries
**Use Case**: Access control, governance

### C09: Async SPARQL + OTEL Validation
**Atoms**: A03 + A09
**Value**: Testless validation via OpenTelemetry
**Use Case**: Production monitoring, CI/CD

### C10: Receipt Anchorer + Merkle Proof
**Atoms**: A10 + A11
**Value**: Blockchain-backed cryptographic audit trail
**Use Case**: Compliance, tamper-evident logs

### C11: Receipt + Merkle + Workflow Verifier
**Atoms**: A10 + A11 + A12
**Value**: Full blockchain workflow verification
**Use Case**: Smart contract integration, audit

### C12: RDF Store + Multi-Layer Cache + SPARQL Cache
**Atoms**: A01 + A22 + A24
**Value**: High-performance cached RDF queries (L1+L2+L3)
**Use Case**: Read-heavy workloads, API gateways

### C13: Raft Coordinator + Cluster Manager
**Atoms**: A13 + A14
**Value**: Distributed consensus with health monitoring
**Use Case**: Multi-node RDF clusters

### C14: Raft + Cluster + State Machine + Federated Query
**Atoms**: A13 + A14 + A15 + A16
**Value**: Fully distributed RDF query federation
**Use Case**: Geo-distributed knowledge graphs

### C15: Oxigraph + Federated Query + Multi-Layer Cache
**Atoms**: A05 + A16 + A22
**Value**: Fast distributed queries with caching
**Use Case**: Global RDF networks

### C16: Change Feed + CRDT + WebSocket Sync
**Atoms**: A06 + A43 + A44
**Value**: Conflict-free collaborative RDF editing
**Use Case**: Multi-user knowledge editors

### C17: Freeze Universe + Git Backbone + Sync Protocol
**Atoms**: A18 + A21 + A08
**Value**: Git-backed time-travel RDF with sync
**Use Case**: Versioned knowledge graphs, reproducibility

### C18: Freeze Universe + Git Backbone + Canonicalize
**Atoms**: A18 + A21 + A04
**Value**: Canonical snapshots in Git
**Use Case**: Deterministic builds, CI/CD

### C19: Freeze Universe + Vector Clock + HDIT Coords
**Atoms**: A18 + A19 + A20
**Value**: Causal time-travel with hyperdimensional similarity
**Use Case**: Event sourcing with semantic queries

### C20: RDF to Graph + PageRank + Community Detector
**Atoms**: A25 + A26 + A28
**Value**: Full graph analytics pipeline
**Use Case**: Knowledge graph analysis, clustering

### C21: RDF to Graph + Relationship Finder + PageRank
**Atoms**: A25 + A27 + A26
**Value**: Path discovery with centrality metrics
**Use Case**: Recommendation systems, link prediction

### C22: RDF Store + RDF Embedder + Semantic Query
**Atoms**: A01 + A29 + A30
**Value**: Hybrid semantic + SPARQL search
**Use Case**: Natural language queries over RDF

### C23: Semantic Query + ONNX Runner + Streaming Inference
**Atoms**: A30 + A32 + A33
**Value**: Real-time ML inference on semantic results
**Use Case**: AI-powered knowledge discovery

### C24: ONNX Runner + Streaming Inference + Model Registry
**Atoms**: A32 + A33 + A34
**Value**: Complete ML inference pipeline
**Use Case**: Production ML on RDF

### C25: Workflow Engine + Workflow Patterns + Hook System
**Atoms**: A35 + A37 + A47
**Value**: Policy-gated YAWL workflows
**Use Case**: Business process automation with governance

### C26: Workflow Engine + RDF Store + Workflow Receipt
**Atoms**: A35 + A38 + A36
**Value**: RDF-native workflows with crypto audit
**Use Case**: Verifiable business processes

### C27: Workflow Engine + Durable Workflow + Receipt
**Atoms**: A35 + A39 + A36
**Value**: Temporal-style durable execution with proof
**Use Case**: Long-running sagas, compensation

### C28: HDIT Coords + Workflow Predictor + Anomaly Detector
**Atoms**: A20 + A40 + A42
**Value**: ML-powered workflow optimization with semantic coords
**Use Case**: Smart workflow routing, failure prediction

### C29: CRDT Graph + WebSocket Sync
**Atoms**: A43 + A44
**Value**: Real-time collaborative RDF editing
**Use Case**: Shared knowledge graphs, pair programming

### C30: Workflow Engine + Durable Workflow + ML Optimizer
**Atoms**: A35 + A39 + A41
**Value**: Self-optimizing durable workflows
**Use Case**: Adaptive business processes

### C31: GraphQL Adapter + RDF Store
**Atoms**: A45 + A01
**Value**: Type-safe GraphQL over RDF
**Use Case**: Modern APIs for RDF data

### C32: GraphQL Adapter + Semantic Query
**Atoms**: A45 + A30
**Value**: Natural language GraphQL queries
**Use Case**: Conversational APIs

## Notes

- Total compositions identified: 32
- High-value compositions: C14 (distributed federation), C17 (git-backed time-travel), C22 (hybrid semantic search), C27 (durable workflows)
- Compositions are grouped by primary capability domain
- Proof files to follow in `proofs/` directory
