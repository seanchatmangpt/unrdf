# UNRDF Packages Inventory

**Generated:** 2025-12-26
**Total Packages:** 43 (38 public, 5 private)
**Total LOC:** 157,345 lines of code

## Complete Package Listing

| # | Name | Description | Entry Point | Exports | Runtime | LOC | Internal Dependencies |
|---|------|-------------|-------------|---------|---------|-----|----------------------|
| 1 | @unrdf/atomvm | Run AtomVM (Erlang/BEAM VM) in browser and Node.js using WebAssembly | src/index.mjs | 2 | WASM | 1,977 | — |
| 2 | @unrdf/blockchain | Blockchain integration for UNRDF - Cryptographic receipt anchoring and audit trails | src/index.mjs | 4 | Node | 945 | kgc-4d, yawl |
| 3 | @unrdf/caching | Multi-layer caching system for RDF queries with Redis and LRU | src/index.mjs | 4 | Node | 1,229 | oxigraph |
| 4 | @unrdf/cli | UNRDF CLI - Command-line Tools for Graph Operations and Context Management | src/index.mjs | 2 | Node | 5,592 | core, federation, hooks, knowledge-engine, oxigraph, project-engine, streaming |
| 5 | @unrdf/collab | Real-time collaborative RDF editing using CRDTs (Yjs) with offline-first architecture | src/index.mjs | 4 | Node | 1,102 | core |
| 6 | @unrdf/composables | UNRDF Composables - Vue 3 Composables for Reactive RDF State (Optional Extension) | src/index.mjs | 3 | Node | 3,448 | core, streaming |
| 7 | @unrdf/consensus | Production-grade Raft consensus for distributed workflow coordination | src/index.mjs | 5 | Node | 2,143 | federation |
| 8 | @unrdf/core | UNRDF Core - RDF Graph Operations, SPARQL Execution, and Foundational Substrate | src/index.mjs | 14 | Node | 19,110 | oxigraph |
| 9 | @unrdf/dark-matter | UNRDF Dark Matter - Query Optimization and Performance Analysis (Optional Extension) | src/index.mjs | 3 | Node | 3,049 | core, oxigraph |
| 10 | @unrdf/diataxis-kit | Diátaxis documentation kit for monorepo package inventory and deterministic doc scaffold generation | src/index.mjs | 7 | Node | 2,620 | — |
| 11 | @unrdf/domain | Domain models and types for UNRDF | src/index.mjs | 1 | Node | 1,693 | — (private) |
| 12 | @unrdf/engine-gateway | μ(O) Engine Gateway - Enforcement layer for Oxigraph-first, N3-minimal RDF processing | src/index.mjs | 4 | Node | 476 | core, oxigraph |
| 13 | @unrdf/federation | UNRDF Federation - Peer Discovery and Distributed Query Execution | src/index.mjs | 3 | Node | 4,093 | core, hooks |
| 14 | @unrdf/graph-analytics | Advanced graph analytics for RDF knowledge graphs using graphlib | src/index.mjs | 5 | Node | 988 | — |
| 15 | @unrdf/hooks | UNRDF Knowledge Hooks - Policy Definition and Execution Framework | src/index.mjs | 3 | Node | 9,676 | core, oxigraph |
| 16 | @unrdf/integration-tests | Comprehensive integration tests for UNRDF multi-package workflows | src/index.mjs | 0 | Node | 0 | yawl, hooks, kgc-4d, federation, streaming, oxigraph, core (private) |
| 17 | @unrdf/kgc-4d | KGC 4D Datum & Universe Freeze Engine - Nanosecond-precision event logging with Git-backed snapshots | src/index.mjs | 3 | Node | 6,327 | core, oxigraph |
| 18 | @unrdf/kgn | Deterministic Nunjucks template system with custom filters and frontmatter support | src/index.js | 6 | Node | 18,581 | core, test-utils |
| 19 | @unrdf/knowledge-engine | UNRDF Knowledge Engine - Rule Engine, Inference, and Pattern Matching (Optional Extension) | src/index.mjs | 5 | Node | 23,650 | core, oxigraph, streaming |
| 20 | @unrdf/ml-inference | UNRDF ML Inference - High-performance ONNX model inference pipeline for RDF streams | src/index.mjs | 4 | Node | 1,164 | core, streaming, oxigraph |
| 21 | @unrdf/ml-versioning | ML Model Versioning System using TensorFlow.js and UNRDF KGC-4D time-travel capabilities | src/index.mjs | 3 | Node | 663 | kgc-4d, oxigraph, core |
| 22 | @unrdf/nextra-docs | UNRDF documentation with Nextra 4 - Developer-focused Next.js documentation | src/index.mjs | 0 | Node | 0 | — (private) |
| 23 | @unrdf/observability | Innovative Prometheus/Grafana observability dashboard for UNRDF distributed workflows | src/index.mjs | 4 | Node | 1,245 | — |
| 24 | @unrdf/oxigraph | UNRDF Oxigraph - Graph database benchmarking implementation using Oxigraph SPARQL engine | src/index.mjs | 3 | Node | 1,376 | — |
| 25 | @unrdf/project-engine | UNRDF Project Engine - Self-hosting Tools and Infrastructure (Development Only) | src/index.mjs | 1 | Node | 13,413 | core, knowledge-engine |
| 26 | @unrdf/rdf-graphql | Type-safe GraphQL interface for RDF knowledge graphs with automatic schema generation | src/adapter.mjs | 4 | Node | 1,720 | oxigraph |
| 27 | @unrdf/semantic-search | AI-powered semantic search over RDF knowledge graphs using vector embeddings | src/index.mjs | 4 | Node | 768 | oxigraph |
| 28 | @unrdf/serverless | UNRDF Serverless - One-click AWS deployment for RDF applications | src/index.mjs | 5 | Node | 1,508 | core, oxigraph |
| 29 | @unrdf/streaming | UNRDF Streaming - Change Feeds and Real-time Synchronization | src/index.mjs | 2 | Node | 1,962 | core, hooks, oxigraph |
| 30 | @unrdf/test-utils | Testing utilities for UNRDF development | src/index.mjs | 1 | Node | 1,398 | oxigraph (private) |
| 31 | @unrdf/validation | OTEL validation framework for UNRDF development | src/index.mjs | 1 | Node | 4,056 | knowledge-engine (private) |
| 32 | @unrdf/yawl | YAWL (Yet Another Workflow Language) engine with KGC-4D time-travel and receipt verification | src/index.mjs | 13 | Node | 39,022 | hooks, kgc-4d, oxigraph |
| 33 | @unrdf/yawl-ai | AI-powered workflow optimization using TensorFlow.js and YAWL patterns | src/index.mjs | 5 | Node | 1,925 | — |
| 34 | @unrdf/yawl-api | High-performance REST API framework that exposes YAWL workflows as RESTful APIs with OpenAPI documentation | src/server.mjs | 2 | Node | 1,002 | yawl, kgc-4d |
| 35 | @unrdf/yawl-durable | Durable execution framework inspired by Temporal.io using YAWL and KGC-4D | src/engine.mjs | 4 | Node | 1,712 | yawl, kgc-4d |
| 36 | @unrdf/yawl-kafka | Apache Kafka event streaming integration for YAWL workflows with Avro serialization | src/index.mjs | 4 | Node | 1,560 | core |
| 37 | @unrdf/yawl-langchain | LangChain integration for YAWL workflow engine - AI-powered workflow orchestration with RDF context | src/index.mjs | 3 | Node | 424 | kgc-4d, oxigraph, yawl |
| 38 | @unrdf/yawl-observability | Workflow observability framework with Prometheus metrics and OpenTelemetry tracing for YAWL | src/index.mjs | 4 | Node | 1,896 | yawl |
| 39 | @unrdf/yawl-queue | Distributed YAWL workflow execution using BullMQ and Redis | src/adapter.mjs | 3 | Node | 911 | yawl, kgc-4d |
| 40 | @unrdf/yawl-realtime | Real-time collaboration framework for YAWL workflows using Socket.io | src/index.mjs | 3 | Node | 1,414 | yawl |
| 41 | @unrdf/yawl-viz | Real-time D3.js visualization for YAWL workflows with Van der Aalst pattern rendering | src/visualizer.mjs | 1 | Node | 0 | yawl |
| 42 | docs | Nuxt documentation site | src/index.mjs | 0 | Node | 0 | — (private) |
| 43 | react | [Package not found] | — | — | — | — | — |

## Top 10 Packages by LOC

1. **@unrdf/yawl** - 39,022 LOC
2. **@unrdf/knowledge-engine** - 23,650 LOC
3. **@unrdf/core** - 19,110 LOC
4. **@unrdf/kgn** - 18,581 LOC
5. **@unrdf/project-engine** - 13,413 LOC
6. **@unrdf/hooks** - 9,676 LOC
7. **@unrdf/kgc-4d** - 6,327 LOC
8. **@unrdf/cli** - 5,592 LOC
9. **@unrdf/federation** - 4,093 LOC
10. **@unrdf/validation** - 4,056 LOC

## Architecture Layers

### Foundation Layer (Core Infrastructure)
- **@unrdf/oxigraph** - SPARQL graph database engine (1,376 LOC)
- **@unrdf/core** - RDF operations and foundational substrate (19,110 LOC)
- **@unrdf/domain** - Domain models and types (1,693 LOC)

### Workflow & Event Sourcing Layer
- **@unrdf/yawl** - Workflow engine with time-travel (39,022 LOC)
- **@unrdf/kgc-4d** - Event logging and Git-backed snapshots (6,327 LOC)
- **@unrdf/hooks** - Policy definition and execution (9,676 LOC)

### Integration Layer
- **@unrdf/federation** - Distributed query execution (4,093 LOC)
- **@unrdf/streaming** - Change feeds and real-time sync (1,962 LOC)
- **@unrdf/consensus** - Raft consensus coordination (2,143 LOC)

### AI/ML Layer
- **@unrdf/knowledge-engine** - Inference and pattern matching (23,650 LOC)
- **@unrdf/ml-inference** - ONNX model inference (1,164 LOC)
- **@unrdf/ml-versioning** - ML model versioning (663 LOC)
- **@unrdf/semantic-search** - Vector embeddings search (768 LOC)

### Developer Tools Layer
- **@unrdf/cli** - Command-line interface (5,592 LOC)
- **@unrdf/kgn** - Nunjucks template system (18,581 LOC)
- **@unrdf/diataxis-kit** - Documentation toolkit (2,620 LOC)
- **@unrdf/test-utils** - Testing utilities (1,398 LOC)

### Extension Modules
- **YAWL Extensions**: 9 packages (yawl-ai, yawl-api, yawl-durable, yawl-kafka, yawl-langchain, yawl-observability, yawl-queue, yawl-realtime, yawl-viz)
- **Performance**: @unrdf/caching, @unrdf/dark-matter
- **Infrastructure**: @unrdf/blockchain, @unrdf/observability, @unrdf/serverless
- **Interfaces**: @unrdf/rdf-graphql, @unrdf/composables, @unrdf/collab
- **Special**: @unrdf/atomvm (WASM)

## Verification

Entry points verified via file system for all 43 packages.
LOC counted using `find + wc -l` on `src/` directories.
Internal dependencies extracted from package.json dependencies fields.
