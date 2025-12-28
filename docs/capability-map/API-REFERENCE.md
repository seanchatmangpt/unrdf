# UNRDF API Reference

**Version**: 6.0.0
**Last Updated**: 2025-12-28
**Total Packages**: 40

---

## Overview

Complete API reference for all UNRDF packages. APIs are organized by use case and package category for easy navigation.

**Quick Links**:
- [OpenAPI Schema](./openapi-schema.json) - Machine-readable API specification
- [Quick Reference](./QUICK-REFERENCE.md) - One-page cheat sheet
- [Individual Package APIs](./reference/) - Detailed package documentation

---

## Package Categories


### RDF & Storage

#### ⏳ [@unrdf/caching](./reference/caching.md)

Multi-layer caching system for RDF queries with Redis and LRU

**Version**: 1.0.0 | **Maturity**: stable

**Exports**: src/index.mjs, src/layers/multi-layer-cache.mjs, src/invalidation/dependency-tracker.mjs...

*Full API documentation coming soon*

---

#### ⏳ [@unrdf/collab](./reference/collab.md)

Real-time collaborative RDF editing using CRDTs (Yjs) with offline-first architecture

**Version**: 1.0.0 | **Maturity**: mature

**Exports**: src/index.mjs, src/crdt/index.mjs, src/sync/index.mjs...

*Full API documentation coming soon*

---

#### ✅ [@unrdf/core](./reference/core.md)

UNRDF Core - RDF Graph Operations, SPARQL Execution, and Foundational Substrate

**Version**: 5.0.1 | **Maturity**: mature

**Key APIs**:
- `createStore()` - Function
- `UnrdfStore` - Class
- `addQuad(quad)` - Function
- `removeQuad(quad)` - Function
- `getQuads(pattern)` - Function

[View Full API →](./reference/core.md)

---

#### ⏳ [@unrdf/graph-analytics](./reference/graph-analytics.md)

Advanced graph analytics for RDF knowledge graphs using graphlib

**Version**: 1.0.0 | **Maturity**: mature

**Exports**: src/index.mjs, src/converter/rdf-to-graph.mjs, src/centrality/pagerank-analyzer.mjs...

*Full API documentation coming soon*

---

#### ✅ [@unrdf/oxigraph](./reference/oxigraph.md)

UNRDF Oxigraph - Graph database benchmarking implementation using Oxigraph SPARQL engine

**Version**: 5.0.1 | **Maturity**: mature

**Key APIs**:
- `createStore()` - Function
- `OxigraphStore` - Class
- `store.load(content, format)` - Method
- `store.dump(format, graph)` - Method
- `store.insert(quad)` - Method

[View Full API →](./reference/oxigraph.md)

---

#### ⏳ [@unrdf/rdf-graphql](./reference/rdf-graphql.md)

Type-safe GraphQL interface for RDF knowledge graphs with automatic schema generation

**Version**: 1.0.0 | **Maturity**: mature

**Exports**: src/adapter.mjs, src/schema-generator.mjs, src/query-builder.mjs...

*Full API documentation coming soon*

---

#### ⏳ [@unrdf/semantic-search](./reference/semantic-search.md)

AI-powered semantic search over RDF knowledge graphs using vector embeddings

**Version**: 1.0.0 | **Maturity**: mature

**Exports**: src/index.mjs, src/embeddings/index.mjs, src/search/index.mjs...

*Full API documentation coming soon*

---


### Governance & Policy

#### ⏳ [@unrdf/engine-gateway](./reference/engine-gateway.md)

μ(O) Engine Gateway - Enforcement layer for Oxigraph-first, N3-minimal RDF processing

**Version**: 5.0.1 | **Maturity**: mature

**Exports**: src/index.mjs, src/gateway.mjs, src/operation-detector.mjs...

*Full API documentation coming soon*

---

#### ⏳ [@unrdf/hooks](./reference/hooks.md)

UNRDF Knowledge Hooks - Policy Definition and Execution Framework

**Version**: 5.0.1 | **Maturity**: mature

**Exports**: src/index.mjs, src/define.mjs, src/executor.mjs...

*Full API documentation coming soon*

---


### Temporal & Events

#### ⏳ [@unrdf/blockchain](./reference/blockchain.md)

Blockchain integration for UNRDF - Cryptographic receipt anchoring and audit trails

**Version**: 1.0.0 | **Maturity**: mature

**Exports**: src/index.mjs, src/anchoring/receipt-anchorer.mjs, src/contracts/workflow-verifier.mjs...

*Full API documentation coming soon*

---

#### ⏳ [@unrdf/kgc-4d](./reference/kgc-4d.md)

KGC 4D Datum & Universe Freeze Engine - Nanosecond-precision event logging with Git-backed snapshots

**Version**: 5.0.1 | **Maturity**: mature

**Exports**: src/index.mjs, src/client.mjs, src/hdit/index.mjs...

*Full API documentation coming soon*

---

#### ⏳ [@unrdf/kgc-claude](./reference/kgc-claude.md)

KGC-Claude Substrate - Deterministic run objects, universal checkpoints, bounded autonomy, and multi-agent concurrency for Claude integration

**Version**: 5.0.0 | **Maturity**: stable

**Exports**: src/index.mjs, src/run-capsule.mjs, src/checkpoint.mjs...

*Full API documentation coming soon*

---

#### ⏳ [@unrdf/kgc-substrate](./reference/kgc-substrate.md)

KGC Substrate - Deterministic, hash-stable KnowledgeStore with immutable append-only log

**Version**: 1.0.0 | **Maturity**: documented

**Exports**: src/index.mjs, src/types.mjs, src/KnowledgeStore.mjs...

*Full API documentation coming soon*

---


### Streaming & Distribution

#### ⏳ [@unrdf/consensus](./reference/consensus.md)

Production-grade Raft consensus for distributed workflow coordination

**Version**: 1.0.0 | **Maturity**: mature

**Exports**: src/index.mjs, src/raft/raft-coordinator.mjs, src/membership/cluster-manager.mjs...

*Full API documentation coming soon*

---

#### ⏳ [@unrdf/federation](./reference/federation.md)

UNRDF Federation - Peer Discovery and Distributed Query Execution

**Version**: 5.0.1 | **Maturity**: mature

**Exports**: src/index.mjs, src/coordinator.mjs, src/advanced-sparql-federation.mjs...

*Full API documentation coming soon*

---

#### ⏳ [@unrdf/streaming](./reference/streaming.md)

UNRDF Streaming - Change Feeds and Real-time Synchronization

**Version**: 5.0.1 | **Maturity**: mature

**Exports**: src/index.mjs, src/processor.mjs, {

*Full API documentation coming soon*

---

#### ⏳ [@unrdf/yawl-kafka](./reference/yawl-kafka.md)

Apache Kafka event streaming integration for YAWL workflows with Avro serialization

**Version**: 1.0.0 | **Maturity**: mature

**Exports**: src/index.mjs, src/producer.mjs, src/consumer.mjs...

*Full API documentation coming soon*

---

#### ⏳ [@unrdf/yawl-queue](./reference/yawl-queue.md)

Distributed YAWL workflow execution using BullMQ and Redis

**Version**: 1.0.0 | **Maturity**: mature

**Exports**: src/adapter.mjs, src/examples/data-pipeline.mjs

*Full API documentation coming soon*

---


### Workflow & Orchestration

#### ⏳ [@unrdf/yawl](./reference/yawl.md)

YAWL (Yet Another Workflow Language) engine with KGC-4D time-travel and receipt verification

**Version**: 5.0.0 | **Maturity**: mature

**Exports**: src/index.mjs, src/api/workflow-api.mjs, src/api/graphql-api.mjs...

*Full API documentation coming soon*

---

#### ⏳ [@unrdf/yawl-ai](./reference/yawl-ai.md)

AI-powered workflow optimization using TensorFlow.js and YAWL patterns

**Version**: 1.0.0 | **Maturity**: stable

**Exports**: src/index.mjs, src/ml/workflow-predictor.mjs, src/ml/performance-optimizer.mjs...

*Full API documentation coming soon*

---

#### ⏳ [@unrdf/yawl-api](./reference/yawl-api.md)

High-performance REST API framework that exposes YAWL workflows as RESTful APIs with OpenAPI documentation

**Version**: 1.0.0 | **Maturity**: mature

**Exports**: src/server.mjs

*Full API documentation coming soon*

---

#### ⏳ [@unrdf/yawl-durable](./reference/yawl-durable.md)

Durable execution framework inspired by Temporal.io using YAWL and KGC-4D

**Version**: 0.1.0 | **Maturity**: mature

**Exports**: src/engine.mjs, src/saga.mjs, src/activity.mjs...

*Full API documentation coming soon*

---

#### ⏳ [@unrdf/yawl-langchain](./reference/yawl-langchain.md)

LangChain integration for YAWL workflow engine - AI-powered workflow orchestration with RDF context

**Version**: 1.0.0 | **Maturity**: mature

**Exports**: src/index.mjs, src/adapter.mjs, examples/code-review-workflow.mjs...

*Full API documentation coming soon*

---

#### ⏳ [@unrdf/yawl-observability](./reference/yawl-observability.md)

Workflow observability framework with Prometheus metrics and OpenTelemetry tracing for YAWL

**Version**: 1.0.0 | **Maturity**: mature

**Exports**: src/index.mjs, src/metrics.mjs, src/tracing.mjs...

*Full API documentation coming soon*

---

#### ⏳ [@unrdf/yawl-realtime](./reference/yawl-realtime.md)

Real-time collaboration framework for YAWL workflows using Socket.io

**Version**: 1.0.0 | **Maturity**: mature

**Exports**: src/index.mjs, src/server.mjs, src/client.mjs...

*Full API documentation coming soon*

---

#### ⏳ [@unrdf/yawl-viz](./reference/yawl-viz.md)

Real-time D3.js visualization for YAWL workflows with Van der Aalst pattern rendering

**Version**: 1.0.0 | **Maturity**: mature

**Exports**: src/visualizer.mjs

*Full API documentation coming soon*

---


### AI & ML

#### ⏳ [@unrdf/knowledge-engine](./reference/knowledge-engine.md)

UNRDF Knowledge Engine - Rule Engine, Inference, and Pattern Matching (Optional Extension)

**Version**: 5.0.1 | **Maturity**: mature

**Exports**: src/index.mjs, src/query.mjs, src/canonicalize.mjs...

*Full API documentation coming soon*

---

#### ⏳ [@unrdf/ml-inference](./reference/ml-inference.md)

UNRDF ML Inference - High-performance ONNX model inference pipeline for RDF streams

**Version**: 5.0.1 | **Maturity**: mature

**Exports**: src/index.mjs, src/runtime/onnx-runner.mjs, src/pipeline/streaming-inference.mjs...

*Full API documentation coming soon*

---

#### ⏳ [@unrdf/ml-versioning](./reference/ml-versioning.md)

ML Model Versioning System using TensorFlow.js and UNRDF KGC-4D time-travel capabilities

**Version**: 1.0.0 | **Maturity**: mature

**Exports**: src/index.mjs, src/version-store.mjs, src/examples/image-classifier.mjs...

*Full API documentation coming soon*

---

#### ⏳ [@unrdf/semantic-search](./reference/semantic-search.md)

AI-powered semantic search over RDF knowledge graphs using vector embeddings

**Version**: 1.0.0 | **Maturity**: mature

**Exports**: src/index.mjs, src/embeddings/index.mjs, src/search/index.mjs...

*Full API documentation coming soon*

---


### Infrastructure & Tools

#### ⏳ [@unrdf/atomvm](./reference/atomvm.md)

Run AtomVM (Erlang/BEAM VM) in browser and Node.js using WebAssembly

**Version**: 5.0.1 | **Maturity**: mature

**Exports**: src/index.mjs, src/service-worker-manager.mjs, {

*Full API documentation coming soon*

---

#### ⏳ [@unrdf/cli](./reference/cli.md)

UNRDF CLI - Command-line Tools for Graph Operations and Context Management

**Version**: 5.0.1 | **Maturity**: mature

**Exports**: src/index.mjs, src/commands/index.mjs

*Full API documentation coming soon*

---

#### ⏳ [@unrdf/diataxis-kit](./reference/diataxis-kit.md)

Diátaxis documentation kit for monorepo package inventory and deterministic doc scaffold generation

**Version**: 1.0.0 | **Maturity**: mature

**Exports**: src/index.mjs, src/inventory.mjs, src/evidence.mjs...

*Full API documentation coming soon*

---

#### ⏳ [@unrdf/kgc-cli](./reference/kgc-cli.md)

KGC CLI - Deterministic extension registry for ~40 workspace packages

**Version**: 5.0.1 | **Maturity**: mature

**Exports**: src/index.mjs, src/lib/registry.mjs, src/manifest/extensions.mjs...

*Full API documentation coming soon*

---

#### ⏳ [@unrdf/kgn](./reference/kgn.md)

Deterministic Nunjucks template system with custom filters and frontmatter support

**Version**: 5.0.1 | **Maturity**: mature

**Exports**: src/templates/*

*Full API documentation coming soon*

---

#### ⏳ [@unrdf/observability](./reference/observability.md)

Innovative Prometheus/Grafana observability dashboard for UNRDF distributed workflows

**Version**: 1.0.0 | **Maturity**: stable

**Exports**: src/index.mjs, src/metrics/workflow-metrics.mjs, src/exporters/grafana-exporter.mjs...

*Full API documentation coming soon*

---

#### ⏳ [@unrdf/serverless](./reference/serverless.md)

UNRDF Serverless - One-click AWS deployment for RDF applications

**Version**: 1.0.0 | **Maturity**: mature

**Exports**: src/index.mjs, src/cdk/index.mjs, src/deploy/index.mjs...

*Full API documentation coming soon*

---


## Common Usage Patterns

### Pattern 1: Create and Query RDF Store

```javascript
import { createStore, namedNode, literal } from '@unrdf/oxigraph';
import { executeSelect } from '@unrdf/core';

const store = createStore();
await store.insert(/* quads */);
const results = await executeSelect(store, 'SELECT ?s WHERE { ?s ?p ?o }');
```

### Pattern 2: Add Validation Hooks

```javascript
import { createStore } from '@unrdf/oxigraph';
import { defineHook, executeHook } from '@unrdf/hooks';

const store = createStore();
const hook = defineHook({
  trigger: 'before:insert',
  validate: (quad) => /* validation logic */
});

if (await executeHook(hook, quad)) {
  await store.insert(quad);
}
```

### Pattern 3: Workflow Orchestration

```javascript
import { createWorkflow, executeWorkflow } from '@unrdf/yawl';
import { createStore } from '@unrdf/kgc-4d';

const workflow = createWorkflow({
  tasks: [/* task definitions */]
});

const store = createStore();
await executeWorkflow(workflow, store);
```

---

## API Stability Levels

| Maturity | Description | Breaking Changes |
|----------|-------------|------------------|
| **mature** | Production-ready, stable API | Rare, with deprecation notices |
| **stable** | API finalized, minor changes possible | Only in major versions |
| **documented** | API defined but may evolve | Possible in minor versions |

---

## Support & Resources

- **Documentation**: [UNRDF Docs](../README.md)
- **GitHub**: [seanchatmangpt/unrdf](https://github.com/seanchatmangpt/unrdf)
- **Issues**: [GitHub Issues](https://github.com/seanchatmangpt/unrdf/issues)
- **Discussions**: [GitHub Discussions](https://github.com/seanchatmangpt/unrdf/discussions)

---

**Generated**: 2025-12-28T04:11:27.343Z
**Source**: Capability maps + package metadata
