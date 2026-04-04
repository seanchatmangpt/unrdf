# UNRDF Package Maturity Matrix

> **Version**: 26.4.4 | **Packages**: 65 | **Updated**: 2026-04-03

This document classifies every package in the UNRDF monorepo by maturity level to help teams make informed adoption decisions.

---

## Maturity Levels

| Level            | Meaning                                                               | Safe for Production? |
| ---------------- | --------------------------------------------------------------------- | -------------------- |
| **Production**   | Stable API, high test coverage, documented. Core platform dependency. | Yes                  |
| **Beta**         | Functional and tested. API may change between minor versions.         | With caution         |
| **Alpha**        | Partially implemented, some test coverage. Breaking changes expected. | No — evaluation only |
| **Experimental** | Proof-of-concept or early research. Minimal testing.                  | No                   |

---

## Production (3 packages)

These packages form the foundation of UNRDF. They are stable, well-tested, and safe for production use.

| Package           | Description                                         |
| ----------------- | --------------------------------------------------- |
| `@unrdf/core`     | RDF storage, SPARQL execution, SHACL validation     |
| `@unrdf/oxigraph` | Rust-based persistent SPARQL backend                |
| `@unrdf/hooks`    | Policy definition and autonomous behavior framework |

---

## Beta (10 packages)

Functional packages with solid test suites. API surface may evolve but core behavior is reliable.

| Package                | Description                                                    |
| ---------------------- | -------------------------------------------------------------- |
| `@unrdf/daemon`        | Background task scheduler with security and clustering         |
| `@unrdf/cli`           | Command-line tools for graph operations                        |
| `@unrdf/streaming`     | Change feeds and real-time synchronization                     |
| `@unrdf/federation`    | Distributed RDF query with RAFT consensus                      |
| `@unrdf/yawl`          | YAWL workflow engine with KGC-4D time-travel                   |
| `@unrdf/kgc-4d`        | Nanosecond-precision event logging with Git-backed snapshots   |
| `@unrdf/kgc-runtime`   | KGC governance runtime with Zod schemas and work items         |
| `@unrdf/v6-core`       | ΔGate control plane, unified receipts, delta contracts         |
| `@unrdf/consensus`     | Production-grade Raft consensus for distributed coordination   |
| `@unrdf/kgc-substrate` | Deterministic, hash-stable KnowledgeStore with append-only log |

---

## Alpha (15 packages)

Packages with partial implementations and some test coverage. Suitable for evaluation and non-critical workloads.

| Package                         | Description                                                      |
| ------------------------------- | ---------------------------------------------------------------- |
| `@unrdf/atomvm`                 | AtomVM (Erlang/BEAM) in browser/Node via WebAssembly             |
| `@unrdf/kgc-probe`              | Automated knowledge graph integrity scanning                     |
| `@unrdf/kgc-swarm`              | Multi-agent template orchestration with receipts                 |
| `@unrdf/kgc-claude`             | Claude integration — run objects, checkpoints, bounded autonomy  |
| `@unrdf/fusion`                 | Unified integration layer for KGC-4D, blockchain, hooks, caching |
| `@unrdf/integration-tests`      | Cross-package integration and adversarial test suite             |
| `@unrdf/diataxis-kit`           | Diátaxis documentation scaffold generator                        |
| `@unrdf/blockchain`             | Cryptographic receipt anchoring and audit trails                 |
| `@unrdf/semantic-search`        | AI-powered semantic search over RDF via vector embeddings        |
| `@unrdf/graph-analytics`        | Advanced graph analytics using graphlib                          |
| `@unrdf/observability`          | Prometheus/Grafana observability dashboard                       |
| `@unrdf/self-healing-workflows` | Automatic error recovery using YAWL + Daemon + Hooks             |
| `@unrdf/event-automation`       | Event-driven automation with delta processing and receipts       |
| `@unrdf/validation`             | OTEL validation framework for development                        |
| `@unrdf/rdf-graphql`            | Type-safe GraphQL interface for RDF with auto schema generation  |

---

## Experimental (37 packages)

Research prototypes and early-stage explorations. Not recommended for any workload outside of experimentation.

### AI / ML (5)

| Package                    | Description                                                    |
| -------------------------- | -------------------------------------------------------------- |
| `@unrdf/ai-ml-innovations` | Novel AI/ML integration patterns for knowledge graphs          |
| `@unrdf/ml-inference`      | High-performance ONNX model inference pipeline for RDF streams |
| `@unrdf/ml-versioning`     | ML model versioning using TensorFlow.js and KGC-4D             |
| `@unrdf/yawl-ai`           | AI-powered workflow optimization using TensorFlow.js           |
| `@unrdf/yawl-langchain`    | LangChain integration for YAWL with RDF context                |

### YAWL Ecosystem (7)

| Package                     | Description                                                   |
| --------------------------- | ------------------------------------------------------------- |
| `@unrdf/yawl-api`           | REST API framework exposing YAWL workflows with OpenAPI docs  |
| `@unrdf/yawl-durable`       | Durable execution framework inspired by Temporal.io           |
| `@unrdf/yawl-kafka`         | Apache Kafka event streaming for YAWL with Avro serialization |
| `@unrdf/yawl-observability` | Prometheus metrics and OpenTelemetry tracing for YAWL         |
| `@unrdf/yawl-queue`         | Distributed YAWL execution using BullMQ and Redis             |
| `@unrdf/yawl-realtime`      | Real-time collaboration for YAWL via Socket.io                |
| `@unrdf/yawl-viz`           | Real-time D3.js visualization for YAWL workflows              |

### Blockchain / Cryptography (3)

| Package                  | Description                                                         |
| ------------------------ | ------------------------------------------------------------------- |
| `@unrdf/receipts`        | Batch receipt generation with Merkle tree and post-quantum crypto   |
| `@unrdf/zkp`             | Zero-knowledge SPARQL — privacy-preserving query proofs (zk-SNARKs) |
| `@unrdf/decision-fabric` | Hyperdimensional decision engine using μ-operators                  |

### Spatial / Temporal (3)

| Package                     | Description                                                |
| --------------------------- | ---------------------------------------------------------- |
| `@unrdf/spatial-kg`         | WebXR-enabled 3D visualization of RDF knowledge graphs     |
| `@unrdf/temporal-discovery` | Temporal pattern mining, anomaly detection, trend analysis |
| `@unrdf/geosparql`          | OGC GeoSPARQL standard compliance for spatial queries      |

### Data Processing (6)

| Package                 | Description                                              |
| ----------------------- | -------------------------------------------------------- |
| `@unrdf/caching`        | Multi-layer caching (Redis + LRU) for RDF queries        |
| `@unrdf/codegen`        | Code generation and metaprogramming tools                |
| `@unrdf/collab`         | Real-time collaborative RDF editing using CRDTs (Yjs)    |
| `@unrdf/engine-gateway` | μ(O) enforcement layer for Oxigraph-first RDF processing |
| `@unrdf/serverless`     | One-click AWS deployment for RDF applications            |
| `@unrdf/v6-compat`      | v5 to v6 migration bridge with adapters and lint rules   |

### KGC Tooling (4)

| Package            | Description                                                    |
| ------------------ | -------------------------------------------------------------- |
| `@unrdf/kgc-cli`   | Deterministic extension registry for workspace packages        |
| `@unrdf/kgc-docs`  | Markdown parser and dynamic doc generator with proof anchoring |
| `@unrdf/kgc-tools` | Verification, freeze, and replay utilities for KGC capsules    |
| `@unrdf/kgn`       | Deterministic Nunjucks template system with custom filters     |

### UI / Frontend (3)

| Package              | Description                                 |
| -------------------- | ------------------------------------------- |
| `@unrdf/react`       | AI semantic analysis tools for RDF (React)  |
| `@unrdf/composables` | Vue 3 composables for reactive RDF state    |
| `@unrdf/dark-matter` | Query optimization and performance analysis |

### Documentation / Internal (6)

| Package                   | Description                                                |
| ------------------------- | ---------------------------------------------------------- |
| `@unrdf/chatman-equation` | Chatman Equation documentation generation (Tera templates) |
| `@unrdf/domain`           | Domain models and types                                    |
| `@unrdf/nextra-docs`      | Nextra 4 documentation site (Next.js)                      |
| `@unrdf/project-engine`   | Self-hosting tools and infrastructure (dev only)           |
| `@unrdf/knowledge-engine` | Rule engine / inference (deprecated — see note below)      |
| `docs`                    | Interactive documentation with AI-powered search           |

---

## Deprecated Packages

| Package                   | Status                              | Guidance                                                               |
| ------------------------- | ----------------------------------- | ---------------------------------------------------------------------- |
| `@unrdf/knowledge-engine` | **Deprecated** — marked for removal | Use `@unrdf/core` equivalents. Recoverable from git history if needed. |

---

## Summary

| Level        | Count  | %        |
| ------------ | ------ | -------- |
| Production   | 3      | 4.6%     |
| Beta         | 10     | 15.4%    |
| Alpha        | 15     | 23.1%    |
| Experimental | 37     | 56.9%    |
| **Total**    | **65** | **100%** |

---

## Enterprise Adoption Guidance

**Starting a new project?** Use only **Production** packages (`core`, `oxigraph`, `hooks`). These have stable APIs, high test coverage, and are the foundation of the platform.

**Need workflows or scheduling?** The **Beta** tier (`daemon`, `yawl`, `cli`, `federation`, `streaming`) is functional and tested but expect minor API changes between releases.

**Evaluating capabilities?** **Alpha** packages demonstrate what's possible but should not be used in critical paths.

**Everything else** is research-grade — valuable for prototyping and exploration, not for production workloads.
