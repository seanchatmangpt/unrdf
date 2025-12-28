# UNRDF API Capability Maps

**Generated**: 2025-12-28
**Purpose**: Comprehensive mapping of all @unrdf package APIs, capabilities, and capability atoms

---

## Table of Contents

- [Overview](#overview)
- [Core Packages](#core-packages)
  - [@unrdf/core](#unrdfcore)
  - [@unrdf/oxigraph](#unrdfoxigraph)
- [Runtime & Governance](#runtime--governance)
  - [@unrdf/hooks](#unrdfhooks)
  - [@unrdf/kgc-runtime](#unrdfkgc-runtime)
  - [@unrdf/kgc-4d](#unrdfkgc-4d)
- [Distribution & Streaming](#distribution--streaming)
  - [@unrdf/federation](#unrdffederation)
  - [@unrdf/streaming](#unrdfstreaming)
- [Workflow & Orchestration](#workflow--orchestration)
  - [@unrdf/yawl](#unrdfyawl)
- [Knowledge & Reasoning](#knowledge--reasoning)
  - [@unrdf/knowledge-engine](#unrdfknowledge-engine)
- [Capability Index](#capability-index)

---

## Overview

This document provides a comprehensive analysis of the UNRDF ecosystem's public APIs. Each package is analyzed for:

1. **Public API Surface**: All exported functions, classes, and types
2. **Key Capabilities**: High-level functionality categories
3. **Capability Atoms**: Smallest meaningful units of functionality
4. **Dependencies**: Internal and external dependencies
5. **Signatures**: Function parameters and return types

**Total Packages Analyzed**: 57
**Core Packages Documented**: 10+

---

## Core Packages

### @unrdf/core

**Version**: 6.0.0-alpha.1
**Description**: RDF Graph Operations, SPARQL Execution, and Foundational Substrate

#### Key Capabilities

1. **RDF Store Management**
2. **SPARQL Query Execution**
3. **RDF Canonicalization**
4. **Error Handling & Recovery**
5. **Validation & Type Safety**

#### Capability Atoms

##### Store Operations (Synchronous)

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `createUnrdfStore` | `() => UnrdfStore` | Create synchronous RDF store | `@unrdf/oxigraph` |
| `UnrdfStore.add` | `(quad: Quad) => void` | Add quad to store | - |
| `UnrdfStore.delete` | `(quad: Quad) => void` | Remove quad from store | - |
| `UnrdfStore.match` | `(s?, p?, o?, g?) => Iterator<Quad>` | Pattern-match quads | - |
| `UnrdfStore.size` | `number` | Get total quad count | - |

##### Store Operations (Async - Legacy)

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `createStore` | `() => Store` | Create async RDF store | `@unrdf/oxigraph` |
| `addQuad` | `(store: Store, quad: Quad) => void` | Add quad to store | `zod` (validation) |
| `removeQuad` | `(store: Store, quad: Quad) => void` | Remove quad from store | `zod` (validation) |
| `getQuads` | `(store, s?, p?, o?, g?) => Quad[]` | Retrieve quads by pattern | - |
| `iterateQuads` | `(store) => Iterator<Quad>` | Iterate all quads | - |
| `countQuads` | `(store, s?, p?, o?, g?) => number` | Count matching quads | - |

##### SPARQL Execution (Synchronous)

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `executeQuerySync` | `(store, query: string) => Results` | Execute SPARQL query (sync) | `oxigraph` |
| `executeSelectSync` | `(store, query: string) => Bindings[]` | Execute SELECT query (sync) | `oxigraph` |
| `executeAskSync` | `(store, query: string) => boolean` | Execute ASK query (sync) | `oxigraph` |
| `executeConstructSync` | `(store, query: string) => Quad[]` | Execute CONSTRUCT query (sync) | `oxigraph` |
| `prepareQuerySync` | `(query: string) => PreparedQuery` | Prepare query for reuse | `oxigraph` |

##### SPARQL Execution (Async - Legacy)

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `executeQuery` | `(store, query: string) => Promise<Results>` | Execute SPARQL query | `oxigraph` |
| `executeSelect` | `(store, query: string) => Promise<Bindings[]>` | Execute SELECT query | `oxigraph` |
| `executeAsk` | `(store, query: string) => Promise<boolean>` | Execute ASK query | `oxigraph` |
| `executeConstruct` | `(store, query: string) => Promise<Quad[]>` | Execute CONSTRUCT query | `oxigraph` |
| `prepareQuery` | `(query: string) => PreparedQuery` | Prepare query for reuse | `oxigraph` |

##### RDF Term Creation

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `namedNode` | `(uri: string) => NamedNode` | Create IRI/URI term | `@unrdf/oxigraph` |
| `literal` | `(value: string, lang?: string \| datatype?: NamedNode) => Literal` | Create literal term | `@unrdf/oxigraph` |
| `blankNode` | `(id?: string) => BlankNode` | Create blank node | `@unrdf/oxigraph` |
| `variable` | `(name: string) => Variable` | Create SPARQL variable | `@unrdf/oxigraph` |
| `defaultGraph` | `() => DefaultGraph` | Get default graph term | `@unrdf/oxigraph` |
| `quad` | `(s, p, o, g?) => Quad` | Create RDF quad | `@unrdf/oxigraph` |

##### Canonicalization

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `canonicalize` | `(dataset: Quad[]) => string` | Canonicalize RDF dataset | `rdf-canonize` |
| `toNTriples` | `(quads: Quad[]) => string` | Serialize to N-Triples | `@rdfjs/to-ntriples` |
| `sortQuads` | `(quads: Quad[]) => Quad[]` | Sort quads canonically | - |
| `isIsomorphic` | `(ds1: Quad[], ds2: Quad[]) => boolean` | Check graph isomorphism | `rdf-canonize` |

##### Error Handling

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `createError` | `(code, message, context?) => UnrdfError` | Create typed error | - |
| `wrapError` | `(error, context?) => UnrdfError` | Wrap external error | - |
| `assertError` | `(condition, message) => void` | Assert with error | - |
| `retry` | `(fn, options) => Promise<T>` | Retry failed operations | - |
| `CircuitBreaker` | `class` | Circuit breaker pattern | - |
| `withTimeout` | `(fn, timeout) => Promise<T>` | Add timeout to operation | - |
| `RateLimiter` | `class` | Rate limit operations | - |

##### Validation

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `validateQuad` | `(quad) => ValidationResult` | Validate quad structure | `zod` |
| `validateStore` | `(store) => ValidationResult` | Validate store state | `zod` |
| `QuadSchema` | `ZodSchema` | Quad validation schema | `zod` |
| `StoreSchema` | `ZodSchema` | Store validation schema | `zod` |

##### Debugging

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `createDebugger` | `(namespace: string) => Debugger` | Create debug logger | - |
| `PerformanceTracker` | `class` | Track performance metrics | - |
| `trace` | `(fn) => TracedFn` | Add tracing to function | - |
| `dumpDebugSnapshot` | `() => Snapshot` | Dump system state | - |

##### Constants

| Capability | Description |
|------------|-------------|
| `RDF`, `RDFS`, `OWL`, `XSD` | Common RDF vocabularies |
| `FOAF`, `DCTERMS`, `SKOS` | Common ontologies |
| `COMMON_PREFIXES` | Prefix mappings |
| `ERROR_CODES` | Error code constants |

---

### @unrdf/oxigraph

**Version**: 5.0.1
**Description**: Oxigraph SPARQL engine wrapper

#### Key Capabilities

1. **High-Performance RDF Store**
2. **Native SPARQL 1.1 Support**
3. **RDF Data Factory**

#### Capability Atoms

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `createStore` | `(quads?: Quad[]) => OxigraphStore` | Create Oxigraph store | `oxigraph` |
| `dataFactory.namedNode` | `(uri: string) => NamedNode` | Create IRI term | `oxigraph` |
| `dataFactory.literal` | `(value, lang?\|dt?) => Literal` | Create literal | `oxigraph` |
| `dataFactory.blankNode` | `(id?: string) => BlankNode` | Create blank node | `oxigraph` |
| `dataFactory.defaultGraph` | `() => DefaultGraph` | Get default graph | `oxigraph` |
| `dataFactory.quad` | `(s, p, o, g?) => Quad` | Create quad | `oxigraph` |
| `OxigraphStore.add` | `(quad) => void` | Add quad | `oxigraph` |
| `OxigraphStore.delete` | `(quad) => void` | Delete quad | `oxigraph` |
| `OxigraphStore.match` | `(s?, p?, o?, g?) => Iterator<Quad>` | Match quads | `oxigraph` |
| `OxigraphStore.query` | `(sparql: string) => Results` | Execute SPARQL | `oxigraph` |

---

## Runtime & Governance

### @unrdf/hooks

**Version**: 5.0.1
**Description**: Knowledge Hooks - Policy Definition and Execution Framework

#### Key Capabilities

1. **Policy-Based Governance**
2. **Hook Definition & Registration**
3. **Chain Execution with JIT Compilation**
4. **Quality Metrics (Lean Six Sigma)**
5. **Performance Optimization (Object Pooling)**

#### Capability Atoms

##### Hook Definition

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `defineHook` | `(config: HookConfig) => Hook` | Define knowledge hook | `zod` |
| `isValidHook` | `(hook) => boolean` | Validate hook structure | - |
| `getHookMetadata` | `(hook) => Metadata` | Extract hook metadata | - |

**HookConfig Schema**:
```typescript
{
  name: string,
  trigger: HookTrigger,  // 'before-add', 'after-add', etc.
  validate?: (quad: Quad) => boolean,
  transform?: (quad: Quad) => Quad,
  metadata?: Record<string, any>
}
```

**Trigger Types** (33 total):
- **CRUD**: `before-add`, `after-add`, `before-query`, `after-query`, `before-remove`, `after-remove`
- **Transaction**: `before-commit`, `after-commit`, `before-rollback`, `after-rollback`
- **Error**: `on-error`, `on-validation-fail`, `on-transform`, `on-timeout`, `on-circuit-open`
- **Async/IO**: `before-fetch`, `after-fetch`, `before-sync`, `after-sync`, `before-import`, `after-import`
- **Scheduled**: `on-schedule`, `on-interval`, `on-idle`, `on-startup`
- **Quality**: `quality-gate`, `defect-detection`, `continuous-improvement`, `spc-control`, `capability-analysis`, `root-cause`, `kaizen-event`, `audit-trail`

##### Hook Execution

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `executeHook` | `(hook, quad, context?) => HookResult` | Execute single hook | `@unrdf/core` |
| `executeHookChain` | `(hooks[], quad, context?) => ChainResult` | Execute hook chain | - |
| `executeHooksByTrigger` | `(trigger, quad, context?) => Results[]` | Execute all hooks for trigger | - |
| `wouldPassHooks` | `(hooks[], quad) => boolean` | Check if quad passes hooks | - |
| `validateOnly` | `(hooks[], quad) => boolean` | Run validation only | - |
| `executeBatch` | `(hooks[], quads[]) => Results[]` | Batch execute hooks | - |

##### Hook Chain Compilation (JIT)

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `compileHookChain` | `(hooks[]) => CompiledChain` | JIT compile hook chain | - |
| `compileValidationOnlyChain` | `(hooks[]) => CompiledChain` | Compile validation-only | - |
| `clearCompiledChainCache` | `() => void` | Clear compilation cache | - |
| `getCompilerStats` | `() => CompilerStats` | Get JIT statistics | - |

##### Hook Management

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `createHookRegistry` | `() => HookRegistry` | Create hook registry | `zod` |
| `registerHook` | `(registry, hook) => void` | Register hook | - |
| `unregisterHook` | `(registry, name) => boolean` | Unregister hook | - |
| `getHook` | `(registry, name) => Hook?` | Get hook by name | - |
| `listHooks` | `(registry) => Hook[]` | List all hooks | - |
| `getHooksByTrigger` | `(registry, trigger) => Hook[]` | Get hooks by trigger | - |

##### Built-in Hooks

| Capability | Description |
|------------|-------------|
| `validateSubjectIRI` | Ensure subject is IRI |
| `validatePredicateIRI` | Ensure predicate is IRI |
| `validateObjectLiteral` | Validate literal values |
| `validateIRIFormat` | Validate IRI format |
| `validateLanguageTag` | Validate language tags |
| `rejectBlankNodes` | Reject blank nodes |
| `normalizeNamespace` | Normalize namespace IRIs |
| `normalizeLanguageTag` | Normalize language tags |
| `trimLiterals` | Trim literal whitespace |

##### Object Pooling

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `QuadPool` | `class` | Quad object pool | - |
| `quadPool.acquire` | `() => Quad` | Get pooled quad | - |
| `quadPool.release` | `(quad) => void` | Return quad to pool | - |
| `createPooledTransform` | `(fn) => PooledTransform` | Create zero-alloc transform | - |

##### Quality Metrics

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `QualityMetricsCollector` | `class` | Collect quality metrics | `@unrdf/core` |
| `createQualityHooks` | `(config) => Hook[]` | Create quality hooks | - |

##### Policy Packs

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `PolicyPack` | `class` | Versioned policy pack | `zod` |
| `PolicyPackManager` | `class` | Manage policy packs | - |
| `createPolicyPackFromDirectory` | `(dir) => PolicyPack` | Load from directory | `fs` |

##### Hook Scheduler

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `HookScheduler` | `class` | Schedule hook execution | - |
| `createHookScheduler` | `() => HookScheduler` | Create scheduler | - |

---

### @unrdf/kgc-runtime

**Version**: 1.0.0
**Description**: KGC governance runtime with work items and receipt chains

#### Key Capabilities

1. **Admission Control**
2. **Work Item Execution**
3. **Receipt Chain Verification**
4. **Plugin Management**
5. **API Versioning**

#### Capability Atoms

##### Admission Control

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `AdmissionGate` | `class` | Control work item admission | `zod` |
| `AdmissionGate.admit` | `(workItem) => AdmissionResult` | Admit work item | - |

##### Work Items

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `WorkItemExecutor` | `class` | Execute async work items | `@unrdf/oxigraph` |
| `WorkItemExecutor.execute` | `(item) => Promise<Result>` | Execute work item | - |
| `WORK_ITEM_STATES` | `enum` | Work item states | - |

##### Receipts

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `generateReceipt` | `(event, prevHash?) => Receipt` | Generate cryptographic receipt | `hash-wasm` |
| `verifyReceiptHash` | `(receipt) => boolean` | Verify receipt hash | `hash-wasm` |
| `verifyReceiptChain` | `(receipts[]) => boolean` | Verify receipt chain | `hash-wasm` |
| `ReceiptStore` | `class` | Store receipts | - |

##### Capsules (Run Isolation)

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `RunCapsule` | `class` | Isolated execution capsule | `zod` |
| `storeCapsule` | `(capsule) => void` | Store capsule | - |
| `replayCapsule` | `(id) => Capsule` | Replay capsule | - |
| `listCapsules` | `() => Capsule[]` | List all capsules | - |

##### Merge & Conflict Resolution

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `shardMerge` | `(shards[]) => Merged` | Merge sharded data | - |
| `mergeCapsules` | `(capsules[]) => Capsule` | Merge capsules | - |
| `ConflictDetector` | `class` | Detect conflicts | - |
| `ConflictResolver` | `class` | Resolve conflicts | - |

##### Validation

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `validateReceiptChainIntegrity` | `(chain) => Result` | Validate chain integrity | `zod` |
| `validateTemporalConsistency` | `(events[]) => Result` | Validate temporal order | `zod` |
| `validateArtifactHash` | `(artifact) => Result` | Validate artifact hash | `hash-wasm` |
| `validateDependencyDAG` | `(deps) => Result` | Validate no cycles | `zod` |
| `detectCycle` | `(graph) => boolean` | Detect dependency cycles | - |

##### Plugin System

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `PluginManager` | `class` | Manage plugins | `zod` |
| `createPluginManager` | `() => PluginManager` | Create plugin manager | - |
| `PluginIsolation` | `class` | Isolate plugin execution | - |
| `createPluginIsolation` | `() => PluginIsolation` | Create isolation context | - |
| `createPublicAPI` | `(context) => API` | Create plugin API surface | - |

##### API Versioning

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `APIVersionManager` | `class` | Manage API versions | `zod` |
| `getVersionManager` | `() => APIVersionManager` | Get version manager | - |
| `isPluginCompatible` | `(plugin, version) => boolean` | Check compatibility | - |
| `validatePluginVersion` | `(plugin) => Result` | Validate plugin version | `zod` |

---

### @unrdf/kgc-4d

**Version**: 5.0.1
**Description**: 4D Datum & Universe Freeze Engine - Nanosecond-precision event logging

#### Key Capabilities

1. **Nanosecond-Precision Timestamping**
2. **Event Sourcing with Git Backend**
3. **Universe Snapshots (Freeze/Reconstruct)**
4. **Hyperdimensional Information Theory (HDIT)**
5. **Vector Clock Synchronization**

#### Capability Atoms

##### Core 4D Engine

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `KGCStore` | `class` | 4D event store | `@unrdf/oxigraph` |
| `GitBackbone` | `class` | Git-backed persistence | `isomorphic-git` |
| `freezeUniverse` | `(store, timestamp?) => Snapshot` | Freeze universe state | `hash-wasm` |
| `reconstructState` | `(snapshot) => Store` | Reconstruct from snapshot | `@unrdf/core` |
| `verifyReceipt` | `(receipt) => boolean` | Verify freeze receipt | `hash-wasm` |

##### Time Management

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `now` | `() => Nanoseconds` | Get current nanosecond time | - |
| `toISO` | `(ns: Nanoseconds) => string` | Convert to ISO string | - |
| `fromISO` | `(iso: string) => Nanoseconds` | Parse ISO to nanoseconds | - |
| `addNanoseconds` | `(ns, delta) => Nanoseconds` | Add nanoseconds | - |
| `duration` | `(start, end) => Nanoseconds` | Calculate duration | - |
| `VectorClock` | `class` | Distributed vector clock | - |
| `hasClockJumpDetected` | `() => boolean` | Detect clock jumps | - |

##### HDIT - Hyperdimensional Information Theory

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `coordsForEvent` | `(event, context?) => Vector` | Generate event coordinates | - |
| `batchCoordsForEvents` | `(events[], context?) => Vector[]` | Batch coordinate generation | - |
| `createUniverseContext` | `(config) => Context` | Create universe context | - |

**Distance & Similarity**:

| Capability | Signature | Description |
|------------|-----------|-------------|
| `cosineSimilarity` | `(v1, v2) => number` | Cosine similarity |
| `cosineDistance` | `(v1, v2) => number` | Cosine distance |
| `euclideanDistance` | `(v1, v2) => number` | Euclidean distance |
| `manhattanDistance` | `(v1, v2) => number` | Manhattan distance |
| `findKNearest` | `(target, vectors, k) => Vector[]` | K-nearest neighbors |
| `findWithinThreshold` | `(target, vectors, threshold) => Vector[]` | Threshold search |

**Projection & Visualization**:

| Capability | Signature | Description |
|------------|-----------|-------------|
| `projectPCA` | `(vectors, dims) => Projected[]` | PCA projection |
| `projectRandom` | `(vectors, dims) => Projected[]` | Random projection |
| `createVisualizationData` | `(events) => VizData` | Create viz data |
| `clusterProjection` | `(vectors, k) => Clusters` | K-means clustering |

**Guards & Validation**:

| Capability | Signature | Description |
|------------|-----------|-------------|
| `guardDimension` | `(dim) => void` | Validate dimension size |
| `guardMemory` | `(vectors, dim) => void` | Check memory bounds |
| `guardLatency` | `(vectors, dim, ops) => void` | Check latency budget |
| `suggestDimension` | `(n, latencyMs) => number` | Suggest optimal dimension |

**Constants**:

| Constant | Value | Description |
|----------|-------|-------------|
| `D_BROWSER` | 32 | Browser-safe dimension |
| `D_LIGHT` | 64 | Light workload |
| `D_MEDIUM` | 128 | Medium workload |
| `D_HEAVY` | 512 | Heavy workload |
| `D_NODE_MAX` | 2048 | Node.js maximum |
| `N_BROWSER_MAX` | 10000 | Browser entity limit |
| `LATENCY_BUDGET_MS` | 16.67 | 60fps latency budget |

##### Reusable Patterns

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `HookRegistry` | `class` | Hook registry pattern | - |
| `createDeltaSyncReducer` | `() => Reducer` | Delta sync state reducer | - |
| `SSEClient` | `class` | Server-sent events client | - |

---

## Distribution & Streaming

### @unrdf/federation

**Version**: 6.0.0
**Description**: Distributed RDF Query with RAFT Consensus

#### Key Capabilities

1. **Distributed SPARQL Execution**
2. **RAFT Consensus Protocol**
3. **Multi-Master Replication**
4. **Query Optimization & Planning**
5. **Advanced SPARQL Federation (Comunica)**

#### Capability Atoms

##### Core Federation

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `createCoordinator` | `(config) => Coordinator` | Create federation coordinator | `@unrdf/core` |
| `createPeerManager` | `(config) => PeerManager` | Manage federation peers | `zod` |

##### Distributed Query

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `executeFederatedQuery` | `(query, sources[]) => Promise<Results>` | Execute federated query | `@unrdf/core` |
| `executeDistributedQuery` | `(query, config) => Promise<Results>` | Execute distributed query | - |
| `aggregateResults` | `(results[]) => Results` | Aggregate query results | - |
| `routeQuery` | `(query, peers[]) => Peer[]` | Route query to peers | - |

##### Query Engine (V6)

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `createDistributedQueryEngine` | `(config) => Engine` | Create query engine | `@unrdf/core` |
| `DistributedQueryEngine.execute` | `(query) => Promise<Results>` | Execute with optimization | - |
| `DistributedQueryEngine.explain` | `(query) => QueryPlan` | Explain query plan | - |

**ExecutionStrategy**: `sequential`, `parallel`, `adaptive`
**PlanNodeType**: `source`, `join`, `filter`, `union`, `optional`

##### Consensus (RAFT)

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `createConsensusManager` | `(config) => ConsensusManager` | Create RAFT manager | `zod` |
| `ConsensusManager.appendEntry` | `(entry) => Promise<boolean>` | Append log entry | - |
| `ConsensusManager.requestVote` | `() => Promise<boolean>` | Request vote | - |
| `ConsensusManager.getState` | `() => NodeState` | Get node state | - |

**NodeState**: `follower`, `candidate`, `leader`

##### Replication

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `createDataReplicationManager` | `(config) => ReplicationManager` | Create replication manager | `@unrdf/core` |
| `ReplicationManager.replicate` | `(data) => Promise<void>` | Replicate data | - |
| `ReplicationManager.sync` | `() => Promise<void>` | Sync with peers | - |

**ReplicationTopology**: `star`, `ring`, `mesh`, `tree`
**ConflictResolution**: `last-write-wins`, `vector-clock`, `custom`
**ReplicationMode**: `eager`, `lazy`, `semi-sync`

##### Advanced SPARQL (Comunica)

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `createAdvancedFederationEngine` | `(config) => Engine` | Create Comunica engine | `@comunica/query-sparql` |
| `federatedQuery` | `(query, sources[]) => Promise<Results>` | Execute federated query | `@comunica/query-sparql` |
| `streamFederatedQuery` | `(query, sources[]) => AsyncIterator<Binding>` | Stream query results | `@comunica/query-sparql` |

##### Metrics

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `recordQuery` | `(query, duration) => void` | Record query metrics | `prom-client` |
| `recordError` | `(error) => void` | Record error | `prom-client` |
| `updatePeerMetrics` | `(peer, metrics) => void` | Update peer metrics | `prom-client` |
| `getMetricsState` | `() => Metrics` | Get metrics state | - |

---

### @unrdf/streaming

**Version**: 5.0.1
**Description**: Change Feeds and Real-time Synchronization

#### Key Capabilities

1. **Change Feed (EventTarget-based)**
2. **Real-time Validation**
3. **Stream Processing**
4. **Subscription Management**
5. **Sync Protocol**

#### Capability Atoms

##### Change Feed

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `createChangeFeed` | `(store?, config?) => ChangeFeed` | Create change feed | `@opentelemetry/api` |
| `ChangeFeed.emitChange` | `(event) => void` | Emit change event | - |
| `ChangeFeed.addEventListener` | `(type, handler) => void` | Subscribe to changes | - |
| `ChangeFeed.getHistory` | `() => Event[]` | Get change history | - |
| `ChangeFeed.replay` | `(from?) => void` | Replay changes | - |

**ChangeEvent Schema**:
```typescript
{
  type: 'add' | 'remove' | 'update',
  quad: Quad,
  timestamp: number,
  metadata?: Record<string, any>
}
```

##### Subscription Management

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `createSubscriptionManager` | `() => SubscriptionManager` | Create subscription manager | `ws` |
| `SubscriptionManager.subscribe` | `(pattern, handler) => Subscription` | Subscribe to pattern | - |
| `SubscriptionManager.unsubscribe` | `(id) => void` | Unsubscribe | - |

##### Stream Processing

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `createStreamProcessor` | `(config) => StreamProcessor` | Create stream processor | `@unrdf/hooks` |
| `StreamProcessor.process` | `(stream) => ProcessedStream` | Process RDF stream | - |

##### Real-time Validation

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `createRealTimeValidator` | `(config) => Validator` | Create real-time validator | `@unrdf/hooks` |
| `RealTimeValidator.validate` | `(quad) => Promise<Result>` | Validate quad | - |

**ValidationMode**: `strict`, `permissive`, `custom`

##### Sync Protocol

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `createSyncMessage` | `(changes[]) => SyncMessage` | Create sync message | `zod` |
| `parseSyncMessage` | `(msg) => SyncMessage` | Parse sync message | `zod` |
| `calculateChecksum` | `(quads[]) => string` | Calculate checksum | - |
| `mergeSyncMessages` | `(msgs[]) => SyncMessage` | Merge messages | - |
| `applySyncMessage` | `(store, msg) => void` | Apply to store | `@unrdf/core` |

##### RDF Stream Parser (V6)

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `createRDFStreamParser` | `(format?) => Parser` | Create stream parser | - |
| `parseRDFStream` | `(stream, format?) => AsyncIterator<Quad>` | Parse RDF stream | - |

##### Performance Monitoring

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `createPerformanceMonitor` | `() => Monitor` | Create performance monitor | `@opentelemetry/api` |
| `PerformanceMonitor.track` | `(metric, value) => void` | Track metric | - |

---

## Workflow & Orchestration

### @unrdf/yawl

**Version**: 6.0.0
**Description**: YAWL Workflow Engine with KGC-4D integration

#### Key Capabilities

1. **YAWL Workflow Patterns (Van der Aalst)**
2. **Petri Net Semantics**
3. **Resource Allocation**
4. **Event Sourcing with Receipts**
5. **GraphQL API**
6. **Live Visualization**

#### Capability Atoms

##### Workflow Engine

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `createWorkflowEngine` | `(config) => WorkflowEngine` | Create workflow engine | `@unrdf/kgc-4d` |
| `WorkflowEngine.start` | `() => Promise<void>` | Start engine | - |
| `WorkflowEngine.stop` | `() => Promise<void>` | Stop engine | - |
| `WorkflowEngine.createCase` | `(spec) => Promise<Case>` | Create workflow case | - |

##### Workflow Definition

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `createWorkflow` | `(spec) => Workflow` | Create workflow from spec | `zod` |
| `workflowToRDF` | `(workflow) => Quad[]` | Serialize to RDF | `@unrdf/core` |
| `workflowFromRDF` | `(quads[]) => Workflow` | Deserialize from RDF | `@unrdf/core` |

##### Case Management

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `createCase` | `(workflow, data?) => YawlCase` | Create case instance | `@unrdf/kgc-4d` |
| `YawlCase.enable` | `(taskId) => Promise<void>` | Enable task | - |
| `YawlCase.start` | `(taskId) => Promise<void>` | Start work item | - |
| `YawlCase.complete` | `(taskId, data) => Promise<void>` | Complete work item | - |
| `YawlCase.cancel` | `(taskId) => Promise<void>` | Cancel work item | - |

##### Task Management

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `createTaskDefinition` | `(def) => TaskDefinition` | Define task | `zod` |
| `createTaskInstance` | `(def, data) => TaskInstance` | Create task instance | - |

**TaskStatus**: `enabled`, `fired`, `allocated`, `started`, `completed`, `suspended`, `cancelled`, `failed`

##### Control Flow Patterns

| Capability | Description | Pattern # |
|------------|-------------|-----------|
| `sequence` | Sequential execution | WP-1 |
| `parallelSplit` | AND-split | WP-2 |
| `synchronization` | AND-join | WP-3 |
| `exclusiveChoice` | XOR-split | WP-4 |
| `simpleMerge` | XOR-join | WP-5 |
| `multiChoice` | OR-split | WP-6 |
| `structuredSyncMerge` | OR-join | WP-7 |
| `arbitraryCycle` | Loop | WP-10 |
| `deferredChoice` | Deferred choice | WP-16 |

**Split Types**: `XOR_Split`, `AND_Split`, `OR_Split`
**Join Types**: `XOR_Join`, `AND_Join`, `OR_Join`

##### Resource Management

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `createResourceManager` | `(config) => ResourceManager` | Create resource manager | `@unrdf/oxigraph` |
| `createParticipant` | `(data) => Participant` | Create participant | `zod` |
| `createRole` | `(data) => Role` | Create role | `zod` |
| `ResourceManager.allocate` | `(workItem) => Promise<Resource>` | Allocate resource | - |

**ResourceType**: `participant`, `tool`, `role`, `organizational-unit`

##### Event Sourcing

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `appendWorkflowEvent` | `(event) => Promise<void>` | Append event | `@unrdf/kgc-4d` |
| `reconstructCase` | `(caseId) => Promise<YawlCase>` | Reconstruct from events | `@unrdf/kgc-4d` |
| `getWorkflowAuditTrail` | `(caseId) => Promise<Event[]>` | Get audit trail | `@unrdf/kgc-4d` |

**Event Types**: `case-created`, `task-enabled`, `workitem-started`, `workitem-completed`, `case-completed`

##### Receipts & Verification

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `generateReceipt` | `(event, prevHash?) => Receipt` | Generate receipt | `@noble/ed25519` |
| `verifyReceipt` | `(receipt) => boolean` | Verify receipt | `@noble/ed25519` |
| `verifyChainLink` | `(receipt, prev?) => boolean` | Verify chain link | `hash-wasm` |
| `ProofChain` | `class` | Receipt chain | - |

##### YAWL-Hooks Integration

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `createYAWLPolicyPack` | `(workflow) => PolicyPack` | Create policy pack | `@unrdf/hooks` |
| `createTaskEnablementHook` | `(task) => Hook` | Create enablement hook | `@unrdf/hooks` |
| `createTaskCompletionHook` | `(task) => Hook` | Create completion hook | `@unrdf/hooks` |
| `createResourceAllocationHook` | `(resource) => Hook` | Create allocation hook | `@unrdf/hooks` |

##### RDF Store Operations

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `createYawlStore` | `() => Store` | Create YAWL RDF store | `@unrdf/oxigraph` |
| `addCase` | `(store, case) => void` | Add case to store | - |
| `getCase` | `(store, caseId) => Case?` | Get case from store | - |
| `addWorkItem` | `(store, workItem) => void` | Add work item | - |
| `queryWorkItems` | `(store, caseId) => WorkItem[]` | Query work items | - |

##### GraphQL API

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `graphqlApi.createWorkflow` | `(spec) => Workflow` | Create via GraphQL | `graphql` |
| `graphqlApi.createCase` | `(workflowId, data) => Case` | Create case via GraphQL | `graphql` |
| `graphqlApi.queryWorkItems` | `(caseId) => WorkItem[]` | Query via GraphQL | `graphql` |

##### Visualization

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `createLiveViz` | `(engine) => Visualization` | Create live visualization | `d3`, `@observablehq/plot` |

##### YAWL Ontology

Comprehensive RDF vocabulary with:
- **Namespaces**: `YAWL`, `YAWL_CASE`, `YAWL_TASK`, `YAWL_WORK`
- **Classes**: `WorkflowCase`, `WorkflowSpec`, `Task`, `WorkItem`, `Condition`, `Flow`
- **Properties**: `status`, `createdAt`, `workItems`, `caseData`, `taskRef`, etc.
- **URI Factories**: `caseUri`, `taskUri`, `workItemUri`, `specUri`

---

## Knowledge & Reasoning

### @unrdf/knowledge-engine

**Version**: 5.0.1
**Description**: Rule Engine, Inference, and Pattern Matching

#### Key Capabilities

1. **SPARQL Query Execution**
2. **Reasoning & Inference (EYE)**
3. **SHACL Validation**
4. **RDF Canonicalization**
5. **AI-Enhanced Search**
6. **Knowledge Substrate Core**

#### Capability Atoms

##### Query Execution

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `query` | `(store, sparql) => Promise<Results>` | Execute SPARQL query | `@unrdf/core` |
| `select` | `(store, sparql) => Promise<Bindings[]>` | SELECT query | `@unrdf/core` |
| `ask` | `(store, sparql) => Promise<boolean>` | ASK query | `@unrdf/core` |
| `construct` | `(store, sparql) => Promise<Quad[]>` | CONSTRUCT query | `@unrdf/core` |
| `describe` | `(store, uri) => Promise<Quad[]>` | DESCRIBE query | `@unrdf/core` |
| `update` | `(store, sparql) => Promise<void>` | UPDATE query | `@unrdf/core` |

##### Query Optimization

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `QueryOptimizer` | `class` | Optimize SPARQL queries | `@unrdf/core` |
| `QueryOptimizer.optimize` | `(query) => OptimizedQuery` | Optimize query | - |
| `QueryOptimizer.explain` | `(query) => QueryPlan` | Explain query plan | - |

##### Reasoning & Inference

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `reason` | `(data, rules) => Promise<Quad[]>` | Apply reasoning rules | `eyereasoner` |
| `reasonMultiple` | `(datasets[], rules) => Promise<Quad[]>` | Batch reasoning | `eyereasoner` |
| `extractInferred` | `(original, inferred) => Quad[]` | Extract new triples | - |
| `validateRules` | `(rules) => ValidationResult` | Validate rule syntax | `eyereasoner` |
| `createReasoningSession` | `() => Session` | Create reasoning session | `eyereasoner` |

##### SHACL Validation

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `validateShacl` | `(data, shapes) => Promise<Report>` | Validate with SHACL | `@unrdf/core` |
| `validateShaclMultiple` | `(datasets[], shapes) => Promise<Report[]>` | Batch validation | `@unrdf/core` |
| `formatValidationReport` | `(report) => string` | Format report | - |
| `hasValidationErrors` | `(report) => boolean` | Check errors | - |
| `getValidationErrors` | `(report) => Error[]` | Extract errors | - |

##### Canonicalization

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `canonicalize` | `(quads[]) => string` | Canonicalize RDF | `@noble/hashes` |
| `isIsomorphic` | `(ds1, ds2) => boolean` | Check isomorphism | `@noble/hashes` |
| `getCanonicalHash` | `(quads[]) => string` | Get canonical hash | `@noble/hashes` |
| `groupByIsomorphism` | `(datasets[]) => Group[]` | Group isomorphic datasets | - |
| `findDuplicates` | `(datasets[]) => Duplicate[]` | Find duplicate graphs | - |
| `createCanonicalizationSession` | `() => Session` | Create session | - |

##### Parsing & Serialization

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `parseTurtle` | `(turtle: string) => Promise<Quad[]>` | Parse Turtle | `@unrdf/core` |
| `toTurtle` | `(quads[]) => Promise<string>` | Serialize to Turtle | `@unrdf/core` |
| `toNQuads` | `(quads[]) => string` | Serialize to N-Quads | `@unrdf/core` |
| `parseJsonLd` | `(jsonld) => Promise<Quad[]>` | Parse JSON-LD | `@unrdf/core` |
| `toJsonLd` | `(quads[]) => Promise<object>` | Serialize to JSON-LD | `@unrdf/core` |

##### AI-Enhanced Search

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `aiSearch` | `(store, query, embeddings?) => Promise<Results>` | AI-powered search | `@xenova/transformers` |

##### Knowledge Substrate Core

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `createKnowledgeSubstrateCore` | `(config) => Substrate` | Create knowledge substrate | `@unrdf/core` |
| `KnowledgeSubstrateCore.query` | `(sparql) => Promise<Results>` | Query substrate | - |
| `KnowledgeSubstrateCore.reason` | `(rules) => Promise<Quad[]>` | Apply reasoning | `eyereasoner` |

##### Hook Management

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `defineHook` | `(config) => Hook` | Define hook | `zod` |
| `registerHook` | `(hook) => void` | Register hook | - |
| `deregisterHook` | `(name) => void` | Deregister hook | - |
| `evaluateHook` | `(hook, context) => Result` | Evaluate hook | - |

##### Policy Management

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `PolicyPackManager` | `class` | Manage policy packs | `zod` |
| `PolicyPack` | `class` | Policy pack | - |

##### Transaction Management

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `TransactionManager` | `class` | Manage transactions | `@unrdf/core` |
| `TransactionManager.begin` | `() => Transaction` | Begin transaction | - |
| `TransactionManager.commit` | `(tx) => Promise<void>` | Commit transaction | - |
| `TransactionManager.rollback` | `(tx) => Promise<void>` | Rollback transaction | - |

##### File Resolution

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `resolveFileUri` | `(uri) => Promise<string>` | Resolve file URI | - |
| `calculateFileHash` | `(content) => string` | Calculate file hash | - |
| `loadFileWithHash` | `(uri) => Promise<{content, hash}>` | Load with verification | - |
| `loadSparqlFile` | `(uri) => Promise<string>` | Load SPARQL file | - |

##### Observability

| Capability | Signature | Description | Dependencies |
|------------|-----------|-------------|--------------|
| `createObservabilityManager` | `(config) => Manager` | Create observability manager | `@opentelemetry/api` |
| `defaultObservabilityManager` | `ObservabilityManager` | Default manager instance | - |

---

## Capability Index

### By Category

**RDF Operations**:
- Store creation and management
- Quad manipulation (add, remove, query)
- Pattern matching
- RDF term creation (IRI, Literal, BlankNode)

**SPARQL**:
- Query execution (SELECT, ASK, CONSTRUCT, DESCRIBE, UPDATE)
- Query optimization and planning
- Prepared queries
- Federated queries

**Governance**:
- Hook definition and execution
- Policy packs
- Admission control
- Work item execution
- Quality metrics (Lean Six Sigma)

**Time & Event Sourcing**:
- Nanosecond timestamping
- Event logging
- Universe snapshots
- Receipt chains
- Cryptographic verification

**Distribution**:
- Federation coordination
- RAFT consensus
- Multi-master replication
- Query routing
- Peer management

**Streaming**:
- Change feeds
- Real-time validation
- Subscription management
- Sync protocol
- Stream processing

**Workflow**:
- YAWL workflow patterns
- Petri net execution
- Resource allocation
- Case management
- Task lifecycle

**Reasoning**:
- Inference rules (EYE)
- SHACL validation
- Canonicalization
- Isomorphism checking

**Performance**:
- Object pooling
- JIT compilation
- Query optimization
- Caching strategies
- Performance monitoring

**Observability**:
- OpenTelemetry integration
- Metrics collection
- Distributed tracing
- Performance tracking

### By Dependency

**Zero Dependencies**:
- `@unrdf/core/rdf/store` (core store operations)
- `@unrdf/kgc-4d/time` (time utilities)
- `@unrdf/hooks/define-hook` (hook definition)

**Depends on @unrdf/oxigraph**:
- `@unrdf/core`
- `@unrdf/hooks`
- `@unrdf/kgc-runtime`
- `@unrdf/kgc-4d`
- `@unrdf/yawl`

**Depends on @unrdf/core**:
- `@unrdf/hooks`
- `@unrdf/streaming`
- `@unrdf/federation`
- `@unrdf/knowledge-engine`

**External Heavy Dependencies**:
- `oxigraph` - High-performance SPARQL engine
- `eyereasoner` - Reasoning engine
- `@comunica/query-sparql` - Advanced federation
- `@xenova/transformers` - AI embeddings
- `isomorphic-git` - Git operations

---

## Usage Patterns

### Basic RDF Operations

```javascript
import { createStore, namedNode, literal, quad } from '@unrdf/core';

const store = createStore();
const alice = namedNode('http://example.org/alice');
const name = namedNode('http://xmlns.com/foaf/0.1/name');
const aliceLit = literal('Alice');

store.add(quad(alice, name, aliceLit));
const quads = store.match(alice);
```

### Hook-Based Governance

```javascript
import { defineHook, executeHook } from '@unrdf/hooks';

const hook = defineHook({
  name: 'validate-subject-iri',
  trigger: 'before-add',
  validate: (quad) => quad.subject.termType === 'NamedNode'
});

const result = executeHook(hook, myQuad);
if (!result.passed) {
  console.error('Validation failed');
}
```

### Time-Travel with KGC-4D

```javascript
import { freezeUniverse, reconstructState, now } from '@unrdf/kgc-4d';

const snapshot = freezeUniverse(store, now());
// ...later...
const restored = reconstructState(snapshot);
```

### Federated Query

```javascript
import { federatedQuery } from '@unrdf/federation';

const sources = [
  'http://dbpedia.org/sparql',
  'http://wikidata.org/sparql'
];

const results = await federatedQuery(`
  SELECT ?person ?name WHERE {
    ?person foaf:name ?name .
  }
`, sources);
```

### YAWL Workflow

```javascript
import { createWorkflowEngine, createWorkflow, sequence } from '@unrdf/yawl';

const engine = createWorkflowEngine({ kgc4d: true });
const workflow = createWorkflow({
  name: 'Order Processing',
  tasks: sequence(['validate', 'process', 'ship'])
});

const caseInstance = await engine.createCase(workflow, { orderId: '123' });
```

---

## Appendix: Package Summary Table

| Package | Version | Key Exports | Main Dependency |
|---------|---------|-------------|-----------------|
| `@unrdf/core` | 6.0.0-alpha.1 | Store, SPARQL, Validation | `@unrdf/oxigraph` |
| `@unrdf/oxigraph` | 5.0.1 | OxigraphStore, dataFactory | `oxigraph` |
| `@unrdf/hooks` | 5.0.1 | defineHook, executeHook | `@unrdf/core` |
| `@unrdf/kgc-runtime` | 1.0.0 | WorkItemExecutor, Receipts | `@unrdf/oxigraph` |
| `@unrdf/kgc-4d` | 5.0.1 | freezeUniverse, HDIT | `@unrdf/core` |
| `@unrdf/federation` | 6.0.0 | Coordinator, RAFT | `@unrdf/core` |
| `@unrdf/streaming` | 5.0.1 | ChangeFeed, Sync | `@unrdf/core` |
| `@unrdf/yawl` | 6.0.0 | WorkflowEngine, Patterns | `@unrdf/kgc-4d` |
| `@unrdf/knowledge-engine` | 5.0.1 | Reasoning, SHACL | `@unrdf/core` |

---

**End of Document**
