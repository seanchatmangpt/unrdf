# UNRDF Packages by Tier

## Essential Tier (7 packages)

### @unrdf/core

UNRDF Core - RDF Graph Operations, SPARQL Execution, and Foundational Substrate

**Version**: `latest.1`

**Dependencies**: OxigraphPackage

---

### @unrdf/hooks

UNRDF Knowledge Hooks - Policy Definition and Execution Framework

**Version**: `latest`

**Dependencies**: CorePackage, OxigraphPackage

---

### @unrdf/kgc-4d

KGC 4D Datum & Universe Freeze Engine - Nanosecond-precision event logging with Git-backed snapshots

**Version**: `latest`

**Dependencies**: CorePackage, OxigraphPackage

---

### @unrdf/oxigraph

UNRDF Oxigraph - Graph database benchmarking implementation using Oxigraph SPARQL engine

**Version**: `latest`

---

### @unrdf/streaming

UNRDF Streaming - Change Feeds and Real-time Synchronization

**Version**: `latest`

**Dependencies**: CorePackage, HooksPackage, OxigraphPackage

---

### @unrdf/v6-core

UNRDF v6 Core - ΔGate control plane, unified receipts, and delta contracts

**Version**: `latest.1`

**Dependencies**: KgcsubstratePackage, YawlPackage, KgccliPackage, Kgc4dPackage, HooksPackage, OxigraphPackage, BlockchainPackage

---

### @unrdf/yawl

YAWL (Yet Another Workflow Language) engine with KGC-4D time-travel and receipt verification

**Version**: `latest`

**Dependencies**: HooksPackage, Kgc4dPackage, OxigraphPackage

---

## Extended Tier (43 packages)

### @unrdf/atomvm

Run AtomVM (Erlang/BEAM VM) in browser and Node.js using WebAssembly

**Version**: `latest`

**Dependencies**: CorePackage, OxigraphPackage, StreamingPackage

---

### @unrdf/blockchain

Blockchain integration for UNRDF - Cryptographic receipt anchoring and audit trails

**Version**: `latest`

**Dependencies**: Kgc4dPackage, YawlPackage

---

### @unrdf/caching

Multi-layer caching system for RDF queries with Redis and LRU

**Version**: `latest`

**Dependencies**: OxigraphPackage

---

### @unrdf/cli

UNRDF CLI - Command-line Tools for Graph Operations and Context Management

**Version**: `latest`

**Dependencies**: CorePackage, DecisionfabricPackage, FederationPackage, HooksPackage, StreamingPackage

---

### @unrdf/collab

Real-time collaborative RDF editing using CRDTs (Yjs) with offline-first architecture

**Version**: `latest`

**Dependencies**: CorePackage

---

### @unrdf/composables

UNRDF Composables - Vue 3 Composables for Reactive RDF State (Optional Extension)

**Version**: `latest`

**Dependencies**: CorePackage, StreamingPackage

---

### @unrdf/consensus

Production-grade Raft consensus for distributed workflow coordination

**Version**: `latest`

**Dependencies**: FederationPackage

---

### @unrdf/dark-matter

UNRDF Dark Matter - Query Optimization and Performance Analysis (Optional Extension)

**Version**: `latest`

**Dependencies**: CorePackage, OxigraphPackage

---

### @unrdf/decision-fabric

Hyperdimensional Decision Fabric - Intent-to-Outcome transformation engine using μ-operators

**Version**: `latest`

**Dependencies**: CorePackage, HooksPackage, Kgc4dPackage, KnowledgeenginePackage, OxigraphPackage, StreamingPackage, ValidationPackage

---

### @unrdf/diataxis-kit

Diátaxis documentation kit for monorepo package inventory and deterministic doc scaffold generation

**Version**: `latest`

---

### @unrdf/domain

Domain models and types for UNRDF

**Version**: `latest`

---

### @unrdf/engine-gateway

μ(O) Engine Gateway - Enforcement layer for Oxigraph-first, N3-minimal RDF processing

**Version**: `latest`

**Dependencies**: CorePackage, OxigraphPackage

---

### @unrdf/federation

UNRDF Federation - Distributed RDF Query with RAFT Consensus and Multi-Master Replication

**Version**: `latest`

**Dependencies**: CorePackage, HooksPackage

---

### @unrdf/fusion

Unified integration layer for 7-day UNRDF innovation - KGC-4D, blockchain, hooks, caching

**Version**: `latest`

**Dependencies**: OxigraphPackage, Kgc4dPackage, BlockchainPackage, HooksPackage, CachingPackage, YawlPackage

---

### @unrdf/kgc-claude

KGC-Claude Substrate - Deterministic run objects, universal checkpoints, bounded autonomy, and multi-agent concurrency for Claude integration

**Version**: `latest`

**Dependencies**: CorePackage, OxigraphPackage, Kgc4dPackage, YawlPackage, HooksPackage

---

### @unrdf/kgc-cli

KGC CLI - Deterministic extension registry for ~40 workspace packages

**Version**: `latest`

---

### @unrdf/kgc-multiverse

KGC Multiverse - Universe branching, forking, and morphism algebra for knowledge graphs

**Version**: `latest`

**Dependencies**: CorePackage, OxigraphPackage, Kgc4dPackage, ReceiptsPackage

---

### @unrdf/kgc-probe

KGC Probe - Automated knowledge graph integrity scanning with 10 agents and artifact validation

**Version**: `latest`

**Dependencies**: KgcsubstratePackage, Kgc4dPackage, V6corePackage, OxigraphPackage, HooksPackage, YawlPackage

---

### @unrdf/kgc-runtime

KGC governance runtime with comprehensive Zod schemas and work item system

**Version**: `latest`

**Dependencies**: OxigraphPackage

---

### @unrdf/kgc-substrate

KGC Substrate - Deterministic, hash-stable KnowledgeStore with immutable append-only log

**Version**: `latest`

**Dependencies**: Kgc4dPackage, OxigraphPackage, CorePackage

---

### @unrdf/kgc-swarm

Multi-agent template orchestration with cryptographic receipts - KGC planning meets kgn rendering

**Version**: `latest`

**Dependencies**: CorePackage, OxigraphPackage, KgcsubstratePackage, KgnPackage, KnowledgeenginePackage, Kgc4dPackage

---

### @unrdf/kgc-tools

KGC Tools - Verification, freeze, and replay utilities for KGC capsules

**Version**: `latest`

**Dependencies**: Kgc4dPackage, KgcruntimePackage, CorePackage

---

### @unrdf/kgn

Deterministic Nunjucks template system with custom filters and frontmatter support

**Version**: `latest`

**Dependencies**: CorePackage, TestutilsPackage

---

### @unrdf/knowledge-engine

UNRDF Knowledge Engine - Rule Engine, Inference, and Pattern Matching (Optional Extension)

**Version**: `latest`

**Dependencies**: CorePackage, OxigraphPackage, StreamingPackage

---

### @unrdf/ml-inference

UNRDF ML Inference - High-performance ONNX model inference pipeline for RDF streams

**Version**: `latest`

**Dependencies**: CorePackage, StreamingPackage, OxigraphPackage

---

### @unrdf/ml-versioning

ML Model Versioning System using TensorFlow.js and UNRDF KGC-4D time-travel capabilities

**Version**: `latest`

**Dependencies**: Kgc4dPackage, OxigraphPackage, CorePackage

---

### @unrdf/observability

Innovative Prometheus/Grafana observability dashboard for UNRDF distributed workflows

**Version**: `latest`

---

### @unrdf/project-engine

UNRDF Project Engine - Self-hosting Tools and Infrastructure (Development Only)

**Version**: `latest`

**Dependencies**: CorePackage, KnowledgeenginePackage

---

### @unrdf/rdf-graphql

Type-safe GraphQL interface for RDF knowledge graphs with automatic schema generation

**Version**: `latest`

**Dependencies**: OxigraphPackage

---

### @unrdf/react

UNRDF React - AI Semantic Analysis Tools for RDF Knowledge Graphs (Optional Extension)

**Version**: `latest`

**Dependencies**: CorePackage, OxigraphPackage

---

### @unrdf/receipts

KGC Receipts - Batch receipt generation with Merkle tree verification for knowledge graph operations

**Version**: `latest`

**Dependencies**: CorePackage, OxigraphPackage, Kgc4dPackage, KgcmultiversePackage

---

### @unrdf/semantic-search

AI-powered semantic search over RDF knowledge graphs using vector embeddings

**Version**: `latest`

**Dependencies**: OxigraphPackage

---

### @unrdf/serverless

UNRDF Serverless - One-click AWS deployment for RDF applications

**Version**: `latest`

**Dependencies**: CorePackage, OxigraphPackage

---

### @unrdf/v6-compat

UNRDF v6 Compatibility Layer - v5 to v6 migration bridge with adapters and lint rules

**Version**: `latest.1`

**Dependencies**: CorePackage, Kgc4dPackage, OxigraphPackage, V6corePackage

---

### @unrdf/yawl-ai

AI-powered workflow optimization using TensorFlow.js and YAWL patterns

**Version**: `latest`

---

### @unrdf/yawl-api

High-performance REST API framework that exposes YAWL workflows as RESTful APIs with OpenAPI documentation

**Version**: `latest`

**Dependencies**: YawlPackage, Kgc4dPackage

---

### @unrdf/yawl-durable

Durable execution framework inspired by Temporal.io using YAWL and KGC-4D

**Version**: `latest`

**Dependencies**: YawlPackage, Kgc4dPackage

---

### @unrdf/yawl-kafka

Apache Kafka event streaming integration for YAWL workflows with Avro serialization

**Version**: `latest`

**Dependencies**: CorePackage

---

### @unrdf/yawl-langchain

LangChain integration for YAWL workflow engine - AI-powered workflow orchestration with RDF context

**Version**: `latest`

**Dependencies**: Kgc4dPackage, OxigraphPackage, YawlPackage

---

### @unrdf/yawl-observability

Workflow observability framework with Prometheus metrics and OpenTelemetry tracing for YAWL

**Version**: `latest`

**Dependencies**: YawlPackage

---

### @unrdf/yawl-queue

Distributed YAWL workflow execution using BullMQ and Redis

**Version**: `latest`

**Dependencies**: YawlPackage, Kgc4dPackage

---

### @unrdf/yawl-realtime

Real-time collaboration framework for YAWL workflows using Socket.io

**Version**: `latest`

**Dependencies**: YawlPackage

---

### @unrdf/yawl-viz

Real-time D3.js visualization for YAWL workflows with Van der Aalst pattern rendering

**Version**: `latest`

**Dependencies**: YawlPackage

---

## Internal Tier (5 packages)

### @unrdf/integration-tests

Phase 5: Comprehensive Integration & Adversarial Tests (75 tests)

**Version**: `latest`

**Dependencies**: YawlPackage, HooksPackage, Kgc4dPackage, KgcmultiversePackage, FederationPackage, StreamingPackage, OxigraphPackage, ReceiptsPackage, CorePackage

---

### @unrdf/kgc-docs

KGC Markdown parser and dynamic documentation generator with proof anchoring

**Version**: `latest`

---

### @unrdf/nextra-docs

UNRDF documentation with Nextra 4 - Developer-focused Next.js documentation

**Version**: `latest`

---

### @unrdf/test-utils

Testing utilities for UNRDF development

**Version**: `latest`

**Dependencies**: OxigraphPackage

---

### @unrdf/validation

OTEL validation framework for UNRDF development

**Version**: `latest`

**Dependencies**: KnowledgeenginePackage

---

## Optional Tier (1 packages)

### @unrdf/graph-analytics

Advanced graph analytics for RDF knowledge graphs using graphlib

**Version**: `latest`

---

