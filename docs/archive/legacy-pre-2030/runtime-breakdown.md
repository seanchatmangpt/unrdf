# UNRDF Runtime Environment Breakdown

**Generated:** 2025-12-26

## Runtime Categories

### Node.js Only (41 packages)

**Core Infrastructure (4)**
- @unrdf/core (19,110 LOC)
- @unrdf/oxigraph (1,376 LOC)
- @unrdf/domain (1,693 LOC)
- @unrdf/engine-gateway (476 LOC)

**Workflow & Orchestration (11)**
- @unrdf/yawl (39,022 LOC)
- @unrdf/kgc-4d (6,327 LOC)
- @unrdf/hooks (9,676 LOC)
- @unrdf/yawl-ai (1,925 LOC)
- @unrdf/yawl-api (1,002 LOC)
- @unrdf/yawl-durable (1,712 LOC)
- @unrdf/yawl-kafka (1,560 LOC)
- @unrdf/yawl-langchain (424 LOC)
- @unrdf/yawl-observability (1,896 LOC)
- @unrdf/yawl-queue (911 LOC)
- @unrdf/yawl-realtime (1,414 LOC)
- @unrdf/yawl-viz (0 LOC)

**AI/ML & Knowledge (4)**
- @unrdf/knowledge-engine (23,650 LOC)
- @unrdf/ml-inference (1,164 LOC)
- @unrdf/ml-versioning (663 LOC)
- @unrdf/semantic-search (768 LOC)

**Federation & Distributed Systems (3)**
- @unrdf/federation (4,093 LOC)
- @unrdf/consensus (2,143 LOC)
- @unrdf/blockchain (945 LOC)

**Streaming & Real-time (3)**
- @unrdf/streaming (1,962 LOC)
- @unrdf/collab (1,102 LOC)
- @unrdf/composables (3,448 LOC)

**Performance & Optimization (2)**
- @unrdf/caching (1,229 LOC)
- @unrdf/dark-matter (3,049 LOC)

**Developer Tools (5)**
- @unrdf/cli (5,592 LOC)
- @unrdf/kgn (18,581 LOC)
- @unrdf/diataxis-kit (2,620 LOC)
- @unrdf/project-engine (13,413 LOC)
- @unrdf/test-utils (1,398 LOC)

**Infrastructure & Deployment (3)**
- @unrdf/serverless (1,508 LOC)
- @unrdf/observability (1,245 LOC)
- @unrdf/validation (4,056 LOC)

**Interfaces & Integration (2)**
- @unrdf/rdf-graphql (1,720 LOC)
- @unrdf/graph-analytics (988 LOC)

**Documentation (2)**
- @unrdf/nextra-docs (0 LOC)
- docs (0 LOC)

**Tests (1)**
- @unrdf/integration-tests (0 LOC)

### WebAssembly (1 package)

**WASM Runtime**
- @unrdf/atomvm (1,977 LOC) - Erlang/BEAM VM in browser and Node.js via WASM

### Browser-Capable Packages

The following packages can run in both Node.js and browser environments:
- @unrdf/atomvm (WASM-based)
- @unrdf/composables (Vue 3 composables)

### Node.js Version Requirements

**Node ≥18.0.0 (Standard)**
- All packages except nextra-docs

**Node ≥20.0.0 (Higher Requirement)**
- @unrdf/nextra-docs (Next.js 16 requirement)

## Runtime Characteristics

### Pure Node.js Features
**Server-side only capabilities:**
- File system operations (KGC-4D Git snapshots)
- Native modules (ONNX runtime, TensorFlow.js)
- Database connections (Redis, Kafka, BullMQ)
- Network servers (Fastify, Socket.io, WebSocket)

### Browser-Compatible Features
**Client-side capable:**
- AtomVM WASM execution
- Vue 3 reactive RDF state management
- Client-side SPARQL queries (via Oxigraph WASM in theory)

### Deployment Targets

**Serverless (AWS Lambda)**
- @unrdf/serverless (1,508 LOC)
- Optimized for Lambda deployment with CDK

**Container/Docker**
- Most packages designed for containerized deployment
- Docker Swarm messaging support in @unrdf/atomvm experiments

**Edge/CDN**
- Potential: @unrdf/atomvm WASM module
- Potential: @unrdf/composables for edge rendering

## Package Manager Requirements

**pnpm ≥7.0.0** - All packages use pnpm workspace protocol
**npm/yarn** - Not supported (workspace protocol incompatible)

## Summary Statistics

| Runtime | Count | Total LOC | Percentage |
|---------|-------|-----------|------------|
| Node.js | 41 | 155,368 | 98.7% |
| WASM | 1 | 1,977 | 1.3% |
| Browser-capable | 2 | 5,425 | 3.4% |
| **Total** | **43** | **157,345** | **100%** |

**Notes:**
- Browser-capable count includes packages with dual runtime support
- Some packages (e.g., oxigraph) could theoretically support browser via WASM but don't currently expose browser builds
- All percentages based on total measurable LOC (157,345 lines)
