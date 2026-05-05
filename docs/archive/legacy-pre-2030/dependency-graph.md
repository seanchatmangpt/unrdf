# UNRDF Dependency Graph

Generated: 2025-12-27T09:41:18.793Z

## Summary

- Total packages: 47
- Total internal dependencies: 84
- Leaf packages (no internal deps): 11
- Hub packages (3+ dependents): 8
- Circular dependencies: 0

## Leaf Packages (No Internal Dependencies)

- **@unrdf/atomvm**: Run AtomVM (Erlang/BEAM VM) in browser and Node.js using WebAssembly
- **@unrdf/diataxis-kit**: Diátaxis documentation kit for monorepo package inventory and deterministic doc scaffold generation
- **docs**:
- **@unrdf/domain**: Domain models and types for UNRDF
- **@unrdf/graph-analytics**: Advanced graph analytics for RDF knowledge graphs using graphlib
- **@unrdf/kgc-cli**: KGC CLI - Deterministic extension registry for ~40 workspace packages
- **@unrdf/nextra-docs**: UNRDF documentation with Nextra 4 - Developer-focused Next.js documentation
- **@unrdf/observability**: Innovative Prometheus/Grafana observability dashboard for UNRDF distributed workflows
- **@unrdf/oxigraph**: UNRDF Oxigraph - Graph database benchmarking implementation using Oxigraph SPARQL engine
- **react**: undefined
- **@unrdf/yawl-ai**: AI-powered workflow optimization using TensorFlow.js and YAWL patterns

## Hub Packages (Most Dependents)

| Package                 | Dependents | Description                                                                                          |
| ----------------------- | ---------- | ---------------------------------------------------------------------------------------------------- |
| @unrdf/oxigraph         | 21         | UNRDF Oxigraph - Graph database benchmarking implementation using Oxigraph SPARQL engine             |
| @unrdf/core             | 19         | UNRDF Core - RDF Graph Operations, SPARQL Execution, and Foundational Substrate                      |
| @unrdf/kgc-4d           | 11         | KGC 4D Datum & Universe Freeze Engine - Nanosecond-precision event logging with Git-backed snapshots |
| @unrdf/yawl             | 11         | YAWL (Yet Another Workflow Language) engine with KGC-4D time-travel and receipt verification         |
| @unrdf/hooks            | 7          | UNRDF Knowledge Hooks - Policy Definition and Execution Framework                                    |
| @unrdf/streaming        | 5          | UNRDF Streaming - Change Feeds and Real-time Synchronization                                         |
| @unrdf/federation       | 3          | UNRDF Federation - Peer Discovery and Distributed Query Execution                                    |
| @unrdf/knowledge-engine | 3          | UNRDF Knowledge Engine - Rule Engine, Inference, and Pattern Matching (Optional Extension)           |

## Circular Dependencies

None detected! Graph is acyclic.

## Full Dependency Tree

```
@unrdf/blockchain
  ← @unrdf/kgc-4d
  ← @unrdf/yawl
@unrdf/caching
  ← @unrdf/oxigraph
@unrdf/cli
  ← @unrdf/core
  ← @unrdf/federation
  ← @unrdf/hooks
  ← @unrdf/knowledge-engine
  ← @unrdf/oxigraph
  ← @unrdf/project-engine
  ← @unrdf/streaming
@unrdf/collab
  ← @unrdf/core
@unrdf/composables
  ← @unrdf/core
  ← @unrdf/streaming
@unrdf/consensus
  ← @unrdf/federation
@unrdf/core
  ← @unrdf/oxigraph
@unrdf/dark-matter
  ← @unrdf/core
  ← @unrdf/oxigraph
@unrdf/engine-gateway
  ← @unrdf/core
  ← @unrdf/oxigraph
@unrdf/federation
  ← @unrdf/core
  ← @unrdf/hooks
@unrdf/fusion
  ← @unrdf/oxigraph
  ← @unrdf/kgc-4d
  ← @unrdf/blockchain
  ← @unrdf/hooks
  ← @unrdf/caching
  ← @unrdf/yawl
@unrdf/hooks
  ← @unrdf/core
  ← @unrdf/oxigraph
@unrdf/integration-tests
  ← @unrdf/yawl
  ← @unrdf/hooks
  ← @unrdf/kgc-4d
  ← @unrdf/federation
  ← @unrdf/streaming
  ← @unrdf/oxigraph
  ← @unrdf/core
@unrdf/kgc-4d
  ← @unrdf/core
  ← @unrdf/oxigraph
@unrdf/kgc-claude
  ← @unrdf/core
  ← @unrdf/oxigraph
  ← @unrdf/kgc-4d
  ← @unrdf/yawl
  ← @unrdf/hooks
@unrdf/kgc-substrate
  ← @unrdf/kgc-4d
  ← @unrdf/oxigraph
  ← @unrdf/core
@unrdf/kgn
  ← @unrdf/core
  ← @unrdf/test-utils
@unrdf/knowledge-engine
  ← @unrdf/core
  ← @unrdf/oxigraph
  ← @unrdf/streaming
@unrdf/ml-inference
  ← @unrdf/core
  ← @unrdf/streaming
  ← @unrdf/oxigraph
@unrdf/ml-versioning
  ← @unrdf/kgc-4d
  ← @unrdf/oxigraph
  ← @unrdf/core
@unrdf/project-engine
  ← @unrdf/core
  ← @unrdf/knowledge-engine
@unrdf/rdf-graphql
  ← @unrdf/oxigraph
@unrdf/semantic-search
  ← @unrdf/oxigraph
@unrdf/serverless
  ← @unrdf/core
  ← @unrdf/oxigraph
@unrdf/streaming
  ← @unrdf/core
  ← @unrdf/hooks
  ← @unrdf/oxigraph
@unrdf/test-utils
  ← @unrdf/oxigraph
@unrdf/validation
  ← @unrdf/knowledge-engine
@unrdf/yawl
  ← @unrdf/hooks
  ← @unrdf/kgc-4d
  ← @unrdf/oxigraph
@unrdf/yawl-api
  ← @unrdf/yawl
  ← @unrdf/kgc-4d
@unrdf/yawl-durable
  ← @unrdf/yawl
  ← @unrdf/kgc-4d
@unrdf/yawl-kafka
  ← @unrdf/core
@unrdf/yawl-langchain
  ← @unrdf/kgc-4d
  ← @unrdf/oxigraph
  ← @unrdf/yawl
@unrdf/yawl-observability
  ← @unrdf/yawl
@unrdf/yawl-queue
  ← @unrdf/yawl
  ← @unrdf/kgc-4d
@unrdf/yawl-realtime
  ← @unrdf/yawl
@unrdf/yawl-viz
  ← @unrdf/yawl
```
