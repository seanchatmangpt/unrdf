# UNRDF Package Architecture Recommendations

Generated: 2025-12-27T09:44:08.330Z

## 1. Packages That Could Be Merged

### Rationale: Similar scope, low complexity

#### YAWL Extensions

The following YAWL extension packages could potentially be consolidated:

- **@unrdf/yawl-ai** (1930 LOC): AI-powered workflow optimization using TensorFlow.js and YAWL patterns
- **@unrdf/yawl-api** (1004 LOC): High-performance REST API framework that exposes YAWL workflows as RESTful APIs with OpenAPI documentation
- **@unrdf/yawl-durable** (1718 LOC): Durable execution framework inspired by Temporal.io using YAWL and KGC-4D
- **@unrdf/yawl-kafka** (1565 LOC): Apache Kafka event streaming integration for YAWL workflows with Avro serialization
- **@unrdf/yawl-langchain** (426 LOC): LangChain integration for YAWL workflow engine - AI-powered workflow orchestration with RDF context
- **@unrdf/yawl-observability** (1901 LOC): Workflow observability framework with Prometheus metrics and OpenTelemetry tracing for YAWL
- **@unrdf/yawl-queue** (913 LOC): Distributed YAWL workflow execution using BullMQ and Redis
- **@unrdf/yawl-realtime** (1418 LOC): Real-time collaboration framework for YAWL workflows using Socket.io

**Recommendation**: Consider consolidating into `@unrdf/yawl-extensions` with sub-exports.

#### Documentation Packages

- Current: 3 separate doc packages
- **Recommendation**: Consider unified `@unrdf/docs` with build-time variants

## 2. Packages Needing Better Exports

- **@unrdf/cli**: Has entry point but no detected exports
- **@unrdf/diataxis-kit**: Has entry point but no detected exports
- **@unrdf/graph-analytics**: Has entry point but no detected exports
- **@unrdf/yawl-langchain**: Has entry point but no detected exports

## 3. Breaking API Issues

### Version Fragmentation

- v5.x.x: 24 packages (mature)
- v1.x.x: 21 packages (stable)
- v0.x.x: 1 packages (experimental)

**Issue**: Version fragmentation may indicate API stability variance.

**Recommendation**:

- Move v0.x packages to v1.0 when stable
- Consider mono-version strategy for core packages

## 4. Dependency Complexity

### High Internal Dependency Count (≥3)

- **@unrdf/cli** (7 deps): @unrdf/core, @unrdf/federation, @unrdf/hooks, @unrdf/knowledge-engine, @unrdf/oxigraph, @unrdf/project-engine, @unrdf/streaming
- **@unrdf/integration-tests** (7 deps): @unrdf/yawl, @unrdf/hooks, @unrdf/kgc-4d, @unrdf/federation, @unrdf/streaming, @unrdf/oxigraph, @unrdf/core
- **@unrdf/fusion** (6 deps): @unrdf/oxigraph, @unrdf/kgc-4d, @unrdf/blockchain, @unrdf/hooks, @unrdf/caching, @unrdf/yawl
- **@unrdf/kgc-claude** (5 deps): @unrdf/core, @unrdf/oxigraph, @unrdf/kgc-4d, @unrdf/yawl, @unrdf/hooks
- **@unrdf/kgc-substrate** (3 deps): @unrdf/kgc-4d, @unrdf/oxigraph, @unrdf/core
- **@unrdf/knowledge-engine** (3 deps): @unrdf/core, @unrdf/oxigraph, @unrdf/streaming
- **@unrdf/ml-inference** (3 deps): @unrdf/core, @unrdf/streaming, @unrdf/oxigraph
- **@unrdf/ml-versioning** (3 deps): @unrdf/kgc-4d, @unrdf/oxigraph, @unrdf/core
- **@unrdf/streaming** (3 deps): @unrdf/core, @unrdf/hooks, @unrdf/oxigraph
- **@unrdf/yawl** (3 deps): @unrdf/hooks, @unrdf/kgc-4d, @unrdf/oxigraph
- **@unrdf/yawl-langchain** (3 deps): @unrdf/kgc-4d, @unrdf/oxigraph, @unrdf/yawl

**Risk**: High coupling may make refactoring difficult.

## 5. Architecture Patterns

### Strong Patterns

- **Layered architecture**: Core → KGC-4D → YAWL → Extensions
- **Module exports**: Consistent use of package.json exports field
- **Entry point convention**: Mostly `src/index.mjs`

### Improvement Opportunities

1. **Standardize entry points**: Some packages use `src/index.js`, others `src/index.mjs`
2. **Export consistency**: Some packages have many sub-exports, others none
3. **Runtime targets**: Clarify browser vs Node.js support

## 6. Quick Wins

1. **Fix `react` package**: Missing package.json
2. **Standardize versions**: Align v5 core packages to same patch version
3. **Document runtime targets**: Add explicit browser/node fields where applicable
4. **Reduce YAWL fragmentation**: 8 YAWL packages could be 2-3
