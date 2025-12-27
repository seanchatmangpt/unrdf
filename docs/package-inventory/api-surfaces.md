# UNRDF API Surface Documentation

Generated: 2025-12-27T09:41:18.799Z

This document catalogs all exported APIs from UNRDF packages.

## @unrdf/atomvm

**Version:** 5.0.1

**Description:** Run AtomVM (Erlang/BEAM VM) in browser and Node.js using WebAssembly

### Entry Points

- **main**: `src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./service-worker-manager]**: `./src/service-worker-manager.mjs`

### Exports

**Named Exports:**

- `AtomVMRuntime`
- `AtomVMNodeRuntime`
- `TerminalUI`
- `App`

### Code Metrics

- Source files: 10
- Total lines of code: 1987
- Avg lines per file: 199

---

## @unrdf/blockchain

**Version:** 1.0.0

**Description:** Blockchain integration for UNRDF - Cryptographic receipt anchoring and audit trails

### Entry Points

- **main**: `src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./anchoring]**: `./src/anchoring/receipt-anchorer.mjs`
- **exports[./contracts]**: `./src/contracts/workflow-verifier.mjs`
- **exports[./merkle]**: `./src/merkle/merkle-proof-generator.mjs`

### Exports

**Default Export:** `anonymous`

**Named Exports:**

- `ReceiptAnchorer`
- `AnchorResultSchema`
- `VerificationResultSchema`
- `WorkflowVerifier`
- `ContractStatsSchema`
- `estimateGasCosts`

### Code Metrics

- Source files: 4
- Total lines of code: 949
- Avg lines per file: 237

---

## @unrdf/caching

**Version:** 1.0.0

**Description:** Multi-layer caching system for RDF queries with Redis and LRU

### Entry Points

- **main**: `src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./layers]**: `./src/layers/multi-layer-cache.mjs`
- **exports[./invalidation]**: `./src/invalidation/dependency-tracker.mjs`
- **exports[./query]**: `./src/query/sparql-cache.mjs`

### Exports

**Default Export:** `anonymous`

**Named Exports:**

- `MultiLayerCache`
- `createMultiLayerCache`
- `DependencyTracker`
- `createDependencyTracker`
- `extractQuerySubjects`
- `SparqlCache`
- `createSparqlCache`
- `createCachingSystem`

### Code Metrics

- Source files: 4
- Total lines of code: 1233
- Avg lines per file: 308

---

## @unrdf/cli

**Version:** 5.0.1

**Description:** UNRDF CLI - Command-line Tools for Graph Operations and Context Management

### Entry Points

- **main**: `src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./commands]**: `./src/commands/index.mjs`

### Exports

### Code Metrics

- Source files: 33
- Total lines of code: 5625
- Avg lines per file: 170

---

## @unrdf/collab

**Version:** 1.0.0

**Description:** Real-time collaborative RDF editing using CRDTs (Yjs) with offline-first architecture

### Entry Points

- **main**: `src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./crdt]**: `./src/crdt/index.mjs`
- **exports[./sync]**: `./src/sync/index.mjs`
- **exports[./composables]**: `./src/composables/index.mjs`

### Exports

**Named Exports:**

- `CollaborativeRDFGraph`
- `WebSocketSync`
- `IndexedDBPersist`
- `useCollaboration`
- `usePresence`

### Code Metrics

- Source files: 16
- Total lines of code: 2391
- Avg lines per file: 149

---

## @unrdf/composables

**Version:** 5.0.1

**Description:** UNRDF Composables - Vue 3 Composables for Reactive RDF State (Optional Extension)

### Entry Points

- **main**: `src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./graph]**: `./src/graph.mjs`
- **exports[./delta]**: `./src/delta.mjs`

### Exports

**Named Exports:**

- `useGraph`
- `useQuery`
- `useDelta`
- `useTerms`
- `useSubscription`
- `useStreaming`

### Code Metrics

- Source files: 16
- Total lines of code: 3464
- Avg lines per file: 217

---

## @unrdf/consensus

**Version:** 1.0.0

**Description:** Production-grade Raft consensus for distributed workflow coordination

### Entry Points

- **main**: `src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./raft]**: `./src/raft/raft-coordinator.mjs`
- **exports[./cluster]**: `./src/membership/cluster-manager.mjs`
- **exports[./state]**: `./src/state/distributed-state-machine.mjs`
- **exports[./transport]**: `./src/transport/websocket-transport.mjs`

### Exports

**Named Exports:**

- `RaftCoordinator`
- `createRaftCoordinator`
- `ClusterManager`
- `createClusterManager`
- `NodeHealth`
- `WebSocketTransport`
- `createWebSocketTransport`

### Code Metrics

- Source files: 5
- Total lines of code: 2148
- Avg lines per file: 430

---

## @unrdf/core

**Version:** 5.0.1

**Description:** UNRDF Core - RDF Graph Operations, SPARQL Execution, and Foundational Substrate

### Entry Points

- **main**: `src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./rdf]**: `./src/rdf/index.mjs`
- **exports[./rdf/minimal-n3-integration]**: `./src/rdf/minimal-n3-integration.mjs`
- **exports[./rdf/n3-justified-only]**: `./src/rdf/n3-justified-only.mjs`
- **exports[./sparql]**: `./src/sparql/index.mjs`
- **exports[./types]**: `./src/types.mjs`
- **exports[./constants]**: `./src/constants.mjs`
- **exports[./validation]**: `./src/validation/index.mjs`
- **exports[./health]**: `./src/health.mjs`
- **exports[./logger]**: `./src/logger.mjs`
- **exports[./metrics]**: `./src/metrics.mjs`
- **exports[./security]**: `./src/security.mjs`
- **exports[./security-schemas]**: `./src/security-schemas.mjs`
- **exports[./utils/sparql-utils]**: `./src/utils/sparql-utils.mjs`

### Exports

**Named Exports:**

- `UnrdfStore`
- `createStore`
- `canonicalize`
- `toNTriples`
- `sortQuads`
- `isIsomorphic`
- `RDF`
- `RDFS`
- `OWL`
- `XSD`
- `FOAF`
- `DCTERMS`
- `SKOS`
- `COMMON_PREFIXES`

### Code Metrics

- Source files: 58
- Total lines of code: 19427
- Avg lines per file: 335

---

## @unrdf/dark-matter

**Version:** 5.0.1

**Description:** UNRDF Dark Matter - Query Optimization and Performance Analysis (Optional Extension)

### Entry Points

- **main**: `src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./optimizer]**: `./src/optimizer.mjs`
- **exports[./analyzer]**: `./src/analyzer.mjs`

### Exports

**Named Exports:**

- `createQueryOptimizer`
- `createIndexAdvisor`

### Code Metrics

- Source files: 9
- Total lines of code: 3058
- Avg lines per file: 340

---

## @unrdf/diataxis-kit

**Version:** 1.0.0

**Description:** Diátaxis documentation kit for monorepo package inventory and deterministic doc scaffold generation

### Entry Points

- **exports[.]**: `./src/index.mjs`
- **exports[./inventory]**: `./src/inventory.mjs`
- **exports[./evidence]**: `./src/evidence.mjs`
- **exports[./classify]**: `./src/classify.mjs`
- **exports[./scaffold]**: `./src/scaffold.mjs`
- **exports[./stable-json]**: `./src/stable-json.mjs`
- **exports[./hash]**: `./src/hash.mjs`

### Exports

### Code Metrics

- Source files: 9
- Total lines of code: 2629
- Avg lines per file: 292

---

## docs

**Version:** 5.0.1

**Description:**

### Code Metrics

- Source files: 0
- Total lines of code: 0
- Avg lines per file: 0

---

## @unrdf/domain

**Version:** 5.0.1

**Description:** Domain models and types for UNRDF

### Entry Points

- **main**: `./src/index.mjs`
- **exports[.]**: `./src/index.mjs`

### Exports

**Named Exports:**

- `Paper`
- `Thesis`
- `Config`

### Code Metrics

- Source files: 11
- Total lines of code: 1704
- Avg lines per file: 155

---

## @unrdf/engine-gateway

**Version:** 5.0.1

**Description:** μ(O) Engine Gateway - Enforcement layer for Oxigraph-first, N3-minimal RDF processing

### Entry Points

- **main**: `./src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./gateway]**: `./src/gateway.mjs`
- **exports[./operation-detector]**: `./src/operation-detector.mjs`
- **exports[./validators]**: `./src/validators.mjs`

### Exports

**Named Exports:**

- `EngineGateway`
- `detectOperationType`
- `N3_ONLY_OPS`
- `OXIGRAPH_OPS`
- `validateN3Usage`
- `validateOxigraphUsage`

### Code Metrics

- Source files: 4
- Total lines of code: 480
- Avg lines per file: 120

---

## @unrdf/federation

**Version:** 5.0.1

**Description:** UNRDF Federation - Peer Discovery and Distributed Query Execution

### Entry Points

- **main**: `src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./coordinator]**: `./src/coordinator.mjs`
- **exports[./advanced-sparql]**: `./src/advanced-sparql-federation.mjs`

### Exports

**Named Exports:**

- `createCoordinator`
- `createPeerManager`
- `PeerConfigSchema`
- `PeerInfoSchema`
- `CoordinatorConfigSchema`

### Code Metrics

- Source files: 11
- Total lines of code: 4104
- Avg lines per file: 373

---

## @unrdf/fusion

**Version:** 1.0.0

**Description:** Unified integration layer for 7-day UNRDF innovation - KGC-4D, blockchain, hooks, caching

### Entry Points

- **exports[.]**: `./src/index.mjs`

### Exports

**Default Export:** `anonymous`

**Named Exports:**

- `createStore`
- `dataFactory`
- `OxigraphStore`
- `createPolicyRegistry`
- `createEngine`
- `prove`

### Code Metrics

- Source files: 13
- Total lines of code: 7979
- Avg lines per file: 614

---

## @unrdf/graph-analytics

**Version:** 1.0.0

**Description:** Advanced graph analytics for RDF knowledge graphs using graphlib

### Entry Points

- **main**: `src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./converter]**: `./src/converter/rdf-to-graph.mjs`
- **exports[./centrality]**: `./src/centrality/pagerank-analyzer.mjs`
- **exports[./paths]**: `./src/paths/relationship-finder.mjs`
- **exports[./clustering]**: `./src/clustering/community-detector.mjs`

### Exports

### Code Metrics

- Source files: 5
- Total lines of code: 993
- Avg lines per file: 199

---

## @unrdf/hooks

**Version:** 5.0.1

**Description:** UNRDF Knowledge Hooks - Policy Definition and Execution Framework

### Entry Points

- **main**: `src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./define]**: `./src/define.mjs`
- **exports[./executor]**: `./src/executor.mjs`

### Exports

**Named Exports:**

- `QuadPool`
- `quadPool`
- `createPooledTransform`
- `isPooledQuad`
- `KnowledgeHookManager`

### Code Metrics

- Source files: 31
- Total lines of code: 9707
- Avg lines per file: 313

---

## @unrdf/integration-tests

**Version:** 5.0.0

**Description:** Comprehensive integration tests for UNRDF multi-package workflows

### Code Metrics

- Source files: 0
- Total lines of code: 0
- Avg lines per file: 0

---

## @unrdf/kgc-4d

**Version:** 5.0.1

**Description:** KGC 4D Datum & Universe Freeze Engine - Nanosecond-precision event logging with Git-backed snapshots

### Entry Points

- **main**: `./src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./client]**: `./src/client.mjs`
- **exports[./hdit]**: `./src/hdit/index.mjs`

### Exports

**Named Exports:**

- `KGCStore`
- `GitBackbone`
- `freezeUniverse`
- `reconstructState`
- `verifyReceipt`
- `now`
- `toISO`
- `fromISO`
- `addNanoseconds`
- `duration`
- `VectorClock`
- `hasClockJumpDetected`
- `resetClockJumpDetection`
- `GRAPHS`
- `EVENT_TYPES`
- `PREDICATES`
- `HookRegistry`
- `SSEClient`

### Code Metrics

- Source files: 24
- Total lines of code: 6362
- Avg lines per file: 265

---

## @unrdf/kgc-claude

**Version:** 5.0.0

**Description:** KGC-Claude Substrate - Deterministic run objects, universal checkpoints, bounded autonomy, and multi-agent concurrency for Claude integration

### Entry Points

- **main**: `./src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./run-capsule]**: `./src/run-capsule.mjs`
- **exports[./checkpoint]**: `./src/checkpoint.mjs`
- **exports[./autonomy-guard]**: `./src/autonomy-guard.mjs`
- **exports[./shard-merge]**: `./src/shard-merge.mjs`
- **exports[./async-workflow]**: `./src/async-workflow.mjs`
- **exports[./projection]**: `./src/projection.mjs`

### Exports

**Named Exports:**

- `GRAPHS`
- `PREDICATES`
- `RUN_STATUS`
- `WORK_ITEM_STATUS`
- `DENIAL_REASONS`
- `KGC_CLAUDE`
- `createSubstrate`

### Code Metrics

- Source files: 9
- Total lines of code: 3601
- Avg lines per file: 400

---

## @unrdf/kgc-cli

**Version:** 5.0.1

**Description:** KGC CLI - Deterministic extension registry for ~40 workspace packages

### Entry Points

- **main**: `src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./registry]**: `./src/lib/registry.mjs`
- **exports[./manifest]**: `./src/manifest/extensions.mjs`
- **exports[./latex]**: `./src/lib/latex/index.mjs`
- **exports[./latex/schemas]**: `./src/lib/latex/schemas.mjs`

### Exports

**Default Export:** `anonymous`

**Named Exports:**

- `Registry`
- `ExtensionSchema`
- `createEnvelope`
- `loadManifest`
- `extensions`
- `overrides`
- `getLoadOrder`
- `buildCittyTree`
- `initializeRegistry`

### Code Metrics

- Source files: 98
- Total lines of code: 18190
- Avg lines per file: 186

---

## @unrdf/kgc-substrate

**Version:** 1.0.0

**Description:** KGC Substrate - Deterministic, hash-stable KnowledgeStore with immutable append-only log

### Entry Points

- **main**: `./src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./types]**: `./src/types.mjs`
- **exports[./KnowledgeStore]**: `./src/KnowledgeStore.mjs`

### Exports

**Named Exports:**

- `KnowledgeStore`
- `ReceiptChain`
- `TamperDetector`

### Code Metrics

- Source files: 8
- Total lines of code: 2342
- Avg lines per file: 293

---

## @unrdf/kgn

**Version:** 5.0.1

**Description:** Deterministic Nunjucks template system with custom filters and frontmatter support

### Entry Points

- **main**: `src/index.js`
- **exports[.]**: `{"import":"./src/index.js"}`
- **exports[./engine]**: `{"import":"./src/engine/index.js"}`
- **exports[./filters]**: `{"import":"./src/filters/index.js"}`
- **exports[./renderer]**: `{"import":"./src/renderer/index.js"}`
- **exports[./linter]**: `{"import":"./src/linter/index.js"}`
- **exports[./templates/*]**: `./src/templates/*`

### Exports

**Named Exports:**

- `TemplateEngine`
- `EnhancedTemplateEngine`
- `TemplateInheritanceEngine`
- `createCustomFilters`
- `FrontmatterParser`
- `VariableExtractor`
- `TemplateLinter`
- `DeterministicRenderer`
- `KGenTemplateBase`
- `createEngine`
- `createInheritanceEngine`
- `createEnhancedEngine`
- `createInjectionEngine`

### Code Metrics

- Source files: 58
- Total lines of code: 18639
- Avg lines per file: 321

---

## @unrdf/knowledge-engine

**Version:** 5.0.1

**Description:** UNRDF Knowledge Engine - Rule Engine, Inference, and Pattern Matching (Optional Extension)

### Entry Points

- **main**: `src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./query]**: `./src/query.mjs`
- **exports[./canonicalize]**: `./src/canonicalize.mjs`
- **exports[./parse]**: `./src/parse.mjs`
- **exports[./ai-search]**: `./src/ai-enhanced-search.mjs`

### Exports

**Named Exports:**

- `KnowledgeHookManager`
- `TransactionManager`
- `defineHook`
- `createHookExecutor`
- `createConditionEvaluator`
- `LockchainWriter`
- `createLockchainWriter`
- `ResolutionLayer`
- `QueryOptimizer`
- `query`
- `select`
- `ask`
- `construct`
- `describe`
- `update`
- `getQueryStats`
- `parseTurtle`
- `toTurtle`
- `toNQuads`
- `parseJsonLd`
- `toJsonLd`
- `EffectSandbox`
- `PolicyPackManager`
- `PolicyPack`

**Re-exports (\*):**

- `./schemas.mjs`

### Code Metrics

- Source files: 57
- Total lines of code: 23105
- Avg lines per file: 405

---

## @unrdf/ml-inference

**Version:** 5.0.1

**Description:** UNRDF ML Inference - High-performance ONNX model inference pipeline for RDF streams

### Entry Points

- **main**: `src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./runtime]**: `./src/runtime/onnx-runner.mjs`
- **exports[./pipeline]**: `./src/pipeline/streaming-inference.mjs`
- **exports[./registry]**: `./src/registry/model-registry.mjs`

### Exports

**Named Exports:**

- `ONNXRunner`
- `createONNXRunner`
- `ModelRegistry`
- `createModelRegistry`
- `createInferenceStack`

### Code Metrics

- Source files: 5
- Total lines of code: 1169
- Avg lines per file: 234

---

## @unrdf/ml-versioning

**Version:** 1.0.0

**Description:** ML Model Versioning System using TensorFlow.js and UNRDF KGC-4D time-travel capabilities

### Entry Points

- **main**: `./src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./version-store]**: `./src/version-store.mjs`
- **exports[./examples/image-classifier]**: `./src/examples/image-classifier.mjs`

### Exports

**Named Exports:**

- `MLVersionStore`

### Code Metrics

- Source files: 3
- Total lines of code: 666
- Avg lines per file: 222

---

## @unrdf/nextra-docs

**Version:** 5.0.1

**Description:** UNRDF documentation with Nextra 4 - Developer-focused Next.js documentation

### Code Metrics

- Source files: 0
- Total lines of code: 0
- Avg lines per file: 0

---

## @unrdf/observability

**Version:** 1.0.0

**Description:** Innovative Prometheus/Grafana observability dashboard for UNRDF distributed workflows

### Entry Points

- **main**: `./src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./metrics]**: `./src/metrics/workflow-metrics.mjs`
- **exports[./exporters]**: `./src/exporters/grafana-exporter.mjs`
- **exports[./alerts]**: `./src/alerts/alert-manager.mjs`

### Exports

**Named Exports:**

- `WorkflowMetrics`
- `createWorkflowMetrics`
- `WorkflowStatus`
- `GrafanaExporter`
- `createGrafanaExporter`
- `AlertManager`
- `createAlertManager`
- `AlertSeverity`
- `createObservabilityStack`

### Code Metrics

- Source files: 5
- Total lines of code: 1371
- Avg lines per file: 274

---

## @unrdf/oxigraph

**Version:** 5.0.1

**Description:** UNRDF Oxigraph - Graph database benchmarking implementation using Oxigraph SPARQL engine

### Entry Points

- **main**: `src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./store]**: `./src/store.mjs`
- **exports[./types]**: `./src/types.mjs`

### Exports

**Default Export:** `anonymous`

**Named Exports:**

- `createStore`
- `dataFactory`
- `OxigraphStore`

### Code Metrics

- Source files: 5
- Total lines of code: 1381
- Avg lines per file: 276

---

## @unrdf/project-engine

**Version:** 5.0.1

**Description:** UNRDF Project Engine - Self-hosting Tools and Infrastructure (Development Only)

### Entry Points

- **main**: `src/index.mjs`
- **exports[.]**: `./src/index.mjs`

### Exports

**Named Exports:**

- `scanFileSystemToStore`
- `buildProjectModelFromFs`
- `detectStackFromFs`
- `classifyFiles`
- `diffProjectStructure`
- `materializeArtifacts`
- `ProjectStructureLens`
- `getProjectEngineConfig`
- `ProjectEngineConfigSchema`
- `buildProjectReport`
- `createProjectInitializationPipeline`
- `inferDomainModel`
- `inferDomainModelFromPath`
- `DomainModelLens`
- `analyzeHotspots`
- `scoreFeature`
- `findMissingRoles`
- `scoreMissingRole`
- `deriveLinterRules`
- `analyzeCodePatterns`
- `generateESLintConfig`
- `analyzeJsComplexity`
- `DocGenerationResultSchema`

### Code Metrics

- Source files: 37
- Total lines of code: 13450
- Avg lines per file: 364

---

## @unrdf/rdf-graphql

**Version:** 1.0.0

**Description:** Type-safe GraphQL interface for RDF knowledge graphs with automatic schema generation

### Entry Points

- **main**: `src/adapter.mjs`
- **exports[.]**: `./src/adapter.mjs`
- **exports[./schema]**: `./src/schema-generator.mjs`
- **exports[./query]**: `./src/query-builder.mjs`
- **exports[./resolver]**: `./src/resolver.mjs`

### Exports

**Named Exports:**

- `RDFGraphQLAdapter`
- `createAdapter`
- `RDFSchemaGenerator`
- `SPARQLQueryBuilder`
- `RDFResolverFactory`

### Code Metrics

- Source files: 5
- Total lines of code: 1725
- Avg lines per file: 345

---

## @unrdf/semantic-search

**Version:** 1.0.0

**Description:** AI-powered semantic search over RDF knowledge graphs using vector embeddings

### Entry Points

- **main**: `src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./embeddings]**: `./src/embeddings/index.mjs`
- **exports[./search]**: `./src/search/index.mjs`
- **exports[./discovery]**: `./src/discovery/index.mjs`

### Exports

**Default Export:** `anonymous`

**Named Exports:**

- `RDFEmbedder`
- `SemanticQueryEngine`
- `KnowledgeRecommender`

### Code Metrics

- Source files: 7
- Total lines of code: 775
- Avg lines per file: 111

---

## @unrdf/serverless

**Version:** 1.0.0

**Description:** UNRDF Serverless - One-click AWS deployment for RDF applications

### Entry Points

- **main**: `src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./cdk]**: `./src/cdk/index.mjs`
- **exports[./deploy]**: `./src/deploy/index.mjs`
- **exports[./api]**: `./src/api/index.mjs`
- **exports[./storage]**: `./src/storage/index.mjs`

### Exports

**Named Exports:**

- `UNRDFStack`
- `createUNRDFStack`
- `DynamoDBAdapter`
- `createAdapterFromEnv`
- `VERSION`
- `SUPPORTED_REGIONS`
- `DEFAULT_CONFIG`

### Code Metrics

- Source files: 9
- Total lines of code: 1517
- Avg lines per file: 169

---

## @unrdf/streaming

**Version:** 5.0.1

**Description:** UNRDF Streaming - Change Feeds and Real-time Synchronization

### Entry Points

- **main**: `src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./processor]**: `./src/processor.mjs`

### Exports

**Named Exports:**

- `createChangeFeed`
- `createSubscriptionManager`
- `createStreamProcessor`

### Code Metrics

- Source files: 9
- Total lines of code: 1971
- Avg lines per file: 219

---

## @unrdf/test-utils

**Version:** 5.0.1

**Description:** Testing utilities for UNRDF development

### Entry Points

- **main**: `./src/index.mjs`
- **exports[.]**: `./src/index.mjs`

### Exports

**Named Exports:**

- `TestScenario`
- `FluentAssertions`
- `TestContextBuilder`
- `TestHelpers`
- `scenario`
- `expect`
- `createTestContext`
- `createDefaultTestContext`

### Code Metrics

- Source files: 3
- Total lines of code: 1401
- Avg lines per file: 467

---

## @unrdf/validation

**Version:** 5.0.1

**Description:** OTEL validation framework for UNRDF development

### Entry Points

- **main**: `./src/index.mjs`
- **exports[.]**: `./src/index.mjs`

### Exports

**Named Exports:**

- `OTELValidator`
- `createOTELValidator`
- `defaultOTELValidator`
- `defaultValidationRunner`
- `defaultOTELValidator`
- `defaultValidationHelpers`

### Code Metrics

- Source files: 8
- Total lines of code: 4064
- Avg lines per file: 508

---

## @unrdf/yawl

**Version:** 5.0.0

**Description:** YAWL (Yet Another Workflow Language) engine with KGC-4D time-travel and receipt verification

### Entry Points

- **main**: `src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./api]**: `./src/api/workflow-api.mjs`
- **exports[./graphql-api]**: `./src/api/graphql-api.mjs`
- **exports[./ontology]**: `./src/ontology/yawl-ontology.mjs`
- **exports[./store]**: `./src/store/yawl-store.mjs`
- **exports[./types]**: `./src/types/yawl-types.mjs`
- **exports[./schemas]**: `./src/types/yawl-schemas.mjs`
- **exports[./hooks]**: `./src/hooks/yawl-hooks.mjs`
- **exports[./resources]**: `./src/resources/yawl-resources.mjs`
- **exports[./cancellation]**: `./src/cancellation/index.mjs`
- **exports[./receipt]**: `./src/receipt.mjs`
- **exports[./blockchain-receipts]**: `./src/blockchain-receipts.mjs`
- **exports[./visualization]**: `./src/visualization/live-workflow-viz.mjs`

### Exports

**Named Exports:**

- `YawlCase`
- `YawlResourcePool`

### Code Metrics

- Source files: 97
- Total lines of code: 39119
- Avg lines per file: 403

---

## @unrdf/yawl-ai

**Version:** 1.0.0

**Description:** AI-powered workflow optimization using TensorFlow.js and YAWL patterns

### Entry Points

- **main**: `src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./predictor]**: `./src/ml/workflow-predictor.mjs`
- **exports[./optimizer]**: `./src/ml/performance-optimizer.mjs`
- **exports[./anomaly]**: `./src/ml/anomaly-detector.mjs`
- **exports[./adapter]**: `./src/integration/yawl-adapter.mjs`

### Exports

**Default Export:** `anonymous`

**Named Exports:**

- `YAWLMLAdapter`
- `createAdapter`

### Code Metrics

- Source files: 5
- Total lines of code: 1930
- Avg lines per file: 386

---

## @unrdf/yawl-api

**Version:** 1.0.0

**Description:** High-performance REST API framework that exposes YAWL workflows as RESTful APIs with OpenAPI documentation

### Entry Points

- **main**: `src/server.mjs`
- **exports[.]**: `./src/server.mjs`
- **exports[./server]**: `./src/server.mjs`

### Exports

**Default Export:** `anonymous`

**Named Exports:**

- `YAWLAPIServer`
- `createYAWLAPIServer`

### Code Metrics

- Source files: 2
- Total lines of code: 1004
- Avg lines per file: 502

---

## @unrdf/yawl-durable

**Version:** 0.1.0

**Description:** Durable execution framework inspired by Temporal.io using YAWL and KGC-4D

### Entry Points

- **main**: `src/engine.mjs`
- **exports[.]**: `./src/engine.mjs`
- **exports[./saga]**: `./src/saga.mjs`
- **exports[./activity]**: `./src/activity.mjs`
- **exports[./replay]**: `./src/replay.mjs`

### Exports

**Named Exports:**

- `DurableWorkflowEngine`

### Code Metrics

- Source files: 6
- Total lines of code: 1718
- Avg lines per file: 286

---

## @unrdf/yawl-kafka

**Version:** 1.0.0

**Description:** Apache Kafka event streaming integration for YAWL workflows with Avro serialization

### Entry Points

- **main**: `src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./producer]**: `./src/producer.mjs`
- **exports[./consumer]**: `./src/consumer.mjs`
- **exports[./schemas]**: `./src/schemas.mjs`

### Exports

**Default Export:** `anonymous`

### Code Metrics

- Source files: 5
- Total lines of code: 1565
- Avg lines per file: 313

---

## @unrdf/yawl-langchain

**Version:** 1.0.0

**Description:** LangChain integration for YAWL workflow engine - AI-powered workflow orchestration with RDF context

### Entry Points

- **main**: `src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./adapter]**: `./src/adapter.mjs`
- **exports[./examples]**: `./examples/code-review-workflow.mjs`

### Exports

### Code Metrics

- Source files: 2
- Total lines of code: 426
- Avg lines per file: 213

---

## @unrdf/yawl-observability

**Version:** 1.0.0

**Description:** Workflow observability framework with Prometheus metrics and OpenTelemetry tracing for YAWL

### Entry Points

- **main**: `src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./metrics]**: `./src/metrics.mjs`
- **exports[./tracing]**: `./src/tracing.mjs`
- **exports[./sli]**: `./src/sli.mjs`

### Exports

**Named Exports:**

- `trace`
- `context`
- `SpanStatusCode`
- `SpanKind`

### Code Metrics

- Source files: 5
- Total lines of code: 1901
- Avg lines per file: 380

---

## @unrdf/yawl-queue

**Version:** 1.0.0

**Description:** Distributed YAWL workflow execution using BullMQ and Redis

### Entry Points

- **main**: `src/adapter.mjs`
- **exports[.]**: `./src/adapter.mjs`
- **exports[./adapter]**: `./src/adapter.mjs`
- **exports[./examples/data-pipeline]**: `./src/examples/data-pipeline.mjs`

### Exports

**Default Export:** `YAWLQueueAdapter`

**Named Exports:**

- `YAWLJobDataSchema`
- `AdapterConfigSchema`
- `YAWLQueueAdapter`

### Code Metrics

- Source files: 2
- Total lines of code: 913
- Avg lines per file: 457

---

## @unrdf/yawl-realtime

**Version:** 1.0.0

**Description:** Real-time collaboration framework for YAWL workflows using Socket.io

### Entry Points

- **main**: `src/index.mjs`
- **exports[.]**: `./src/index.mjs`
- **exports[./server]**: `./src/server.mjs`
- **exports[./client]**: `./src/client.mjs`

### Exports

**Default Export:** `anonymous`

### Code Metrics

- Source files: 4
- Total lines of code: 1418
- Avg lines per file: 355

---

## @unrdf/yawl-viz

**Version:** 1.0.0

**Description:** Real-time D3.js visualization for YAWL workflows with Van der Aalst pattern rendering

### Entry Points

- **main**: `src/visualizer.mjs`
- **exports[.]**: `./src/visualizer.mjs`

### Exports

**Default Export:** `anonymous`

**Named Exports:**

- `PATTERN_STYLES`
- `STATE_COLORS`
- `YAWLVisualizer`
- `createVisualizer`

### Code Metrics

- Source files: 1
- Total lines of code: 746
- Avg lines per file: 746

---
