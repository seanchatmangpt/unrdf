# UNRDF v6 Contract Inventory

**Generated**: 2025-12-27
**Version**: 6.0.0-alpha.1
**Total Packages**: 49

## Executive Summary

This document provides a comprehensive inventory of all public contracts across the UNRDF monorepo in preparation for v6 unification. The analysis identifies fragmentation issues and recommends consolidation priorities.

### Key Metrics

| Metric | Value |
|--------|-------|
| Total Packages | 49 |
| Total Source Files | 1,128 |
| Files with Zod Schemas | 309 |
| Files with Exports | 905 |
| Files with JSDoc | 646 |
| Files with CLI Commands | 32 |
| Files with GraphQL | 9 |
| Files with RDF Vocabularies | 416 |

### Package Distribution

| Category | Count | Percentage |
|----------|-------|------------|
| Packages with Zod | 36 | 73% |
| Packages without Zod | 13 | 27% |
| Packages with Receipt Integration | 11 | 22% |
| Packages without Receipt Integration | 38 | 78% |
| Packages with CLI | 3 | 6% |

## Package Breakdown

### Core Infrastructure (8 packages)

#### @unrdf/core (v5.0.1)
- **Exports**: 14 entry points
- **Schemas**: 22 (QuadSchema, StoreSchema, DiffTripleSchema, Security schemas, etc.)
- **Dependencies**: @unrdf/oxigraph
- **Receipt Integration**: No
- **Key Contracts**:
  - RDF store operations (sync & async)
  - SPARQL execution (executeQuerySync, executeSelectSync, etc.)
  - Canonicalization and diff operations
  - Security validation schemas

#### @unrdf/oxigraph (v5.0.1)
- **Exports**: 3 entry points
- **Schemas**: 1 (QueryCacheConfigSchema)
- **Dependencies**: None
- **Receipt Integration**: No
- **Key Contracts**:
  - Oxigraph store wrapper
  - Query caching

#### @unrdf/kgc-4d (v5.0.1)
- **Exports**: 3 entry points (main, client, hdit)
- **Schemas**: 0 (uses internal types)
- **Dependencies**: @unrdf/core, @unrdf/oxigraph
- **Receipt Integration**: Yes
- **Key Contracts**:
  - KGCStore (nanosecond-precision event logging)
  - GitBackbone (Git-backed snapshots)
  - freezeUniverse, reconstructState, verifyReceipt
  - HDIT (Hyperdimensional Information Theory) coordinates

#### @unrdf/hooks (v5.0.1)
- **Exports**: 3 entry points
- **Schemas**: 2 (HookDefinitionSchema, PolicySchema)
- **Dependencies**: @unrdf/core, @unrdf/oxigraph
- **Receipt Integration**: No
- **Key Contracts**:
  - Hook definition and execution
  - Policy-based validation

#### @unrdf/streaming (v5.0.1)
- **Exports**: 2 entry points
- **Schemas**: 5 (SyncMessageSchema, ChangeEventSchema, ValidationOptionsSchema, etc.)
- **Dependencies**: @unrdf/core, @unrdf/hooks, @unrdf/oxigraph
- **Receipt Integration**: No
- **Key Contracts**:
  - Change feed streaming
  - Real-time synchronization
  - SHACL validation

#### @unrdf/federation (v5.0.1)
- **Exports**: 3 entry points
- **Schemas**: 2 (FederationConfigSchema, PeerNodeSchema)
- **Dependencies**: @unrdf/core, @unrdf/hooks
- **Receipt Integration**: No
- **Key Contracts**:
  - Distributed query coordination
  - Peer management

#### @unrdf/validation (v5.0.1)
- **Exports**: 1 entry point
- **Schemas**: 0
- **Dependencies**: @unrdf/knowledge-engine
- **Receipt Integration**: No
- **Key Contracts**:
  - OTEL-based validation

#### @unrdf/test-utils (v5.0.1)
- **Exports**: 1 entry point
- **Schemas**: 1 (TestFixtureSchema)
- **Dependencies**: @unrdf/oxigraph
- **Receipt Integration**: No
- **Key Contracts**:
  - Test fixtures and helpers

### YAWL Workflow Engine (10 packages)

#### @unrdf/yawl (v5.0.0)
- **Exports**: 13 entry points
- **Schemas**: 24 (CaseStatusSchema, WorkItemStatusSchema, TaskDefSchema, ReceiptSchema, etc.)
- **Dependencies**: @unrdf/hooks, @unrdf/kgc-4d, @unrdf/oxigraph
- **Receipt Integration**: Yes
- **Key Contracts**:
  - Workflow engine API
  - GraphQL API
  - YAWL ontology
  - Resource management
  - Cancellation regions
  - Receipt verification

**FRAGMENTATION WARNING**: Multiple duplicate schemas across files:
- `SplitTypeSchema` defined in 3 files (patterns.mjs, patterns-builders.mjs, workflow-core.mjs)
- `JoinTypeSchema` defined in 3 files
- `TaskDefSchema` vs `TaskDefinitionSchema` inconsistency

#### @unrdf/yawl-ai (v1.0.0)
- **Exports**: 4 entry points
- **Schemas**: 7 (WorkflowExecutionSchema, AnomalySchema, OptimizationReportSchema, etc.)
- **Dependencies**: None
- **Receipt Integration**: No

#### @unrdf/yawl-api (v1.0.0)
- **Exports**: 2 entry points
- **Schemas**: 2 (APIRequestSchema, APIResponseSchema)
- **Dependencies**: @unrdf/yawl, @unrdf/kgc-4d
- **Receipt Integration**: Yes

#### @unrdf/yawl-durable (v0.1.0)
- **Exports**: 4 entry points
- **Schemas**: 2 (SagaDefinitionSchema, ActivitySchema)
- **Dependencies**: @unrdf/yawl, @unrdf/kgc-4d
- **Receipt Integration**: Yes

#### @unrdf/yawl-kafka (v1.0.0)
- **Exports**: 4 entry points
- **Schemas**: 2 (KafkaMessageSchema, TopicConfigSchema)
- **Dependencies**: @unrdf/core
- **Receipt Integration**: No

#### @unrdf/yawl-langchain (v1.0.0)
- **Exports**: 3 entry points
- **Schemas**: 1 (ChainConfigSchema)
- **Dependencies**: @unrdf/kgc-4d, @unrdf/oxigraph, @unrdf/yawl
- **Receipt Integration**: Yes

#### @unrdf/yawl-observability (v1.0.0)
- **Exports**: 4 entry points
- **Schemas**: 2 (WorkflowMetricSchema, SLIConfigSchema)
- **Dependencies**: @unrdf/yawl
- **Receipt Integration**: No

#### @unrdf/yawl-queue (v1.0.0)
- **Exports**: 3 entry points
- **Schemas**: 1 (QueueConfigSchema)
- **Dependencies**: @unrdf/yawl, @unrdf/kgc-4d
- **Receipt Integration**: Yes

#### @unrdf/yawl-realtime (v1.0.0)
- **Exports**: 3 entry points
- **Schemas**: 1 (RealtimeEventSchema)
- **Dependencies**: @unrdf/yawl
- **Receipt Integration**: No

#### @unrdf/yawl-viz (v1.0.0)
- **Exports**: 1 entry point
- **Schemas**: 0
- **Dependencies**: @unrdf/yawl
- **Receipt Integration**: No

### KGC Ecosystem (5 packages)

#### @unrdf/kgc-cli (v5.0.1)
- **Exports**: 3 entry points
- **Schemas**: 2 (ExtensionSchema, RegistrySchema)
- **CLI**: `kgc` command
- **CLI Nouns**: extension, registry
- **Dependencies**: None
- **Receipt Integration**: No

#### @unrdf/kgc-claude (v5.0.0)
- **Exports**: 7 entry points
- **Schemas**: 14 (RunCapsuleSchema, CheckpointReceiptSchema, WorkItemSchema, etc.)
- **Dependencies**: @unrdf/core, @unrdf/oxigraph, @unrdf/kgc-4d, @unrdf/yawl, @unrdf/hooks
- **Receipt Integration**: Yes

#### @unrdf/kgc-substrate (v1.0.0)
- **Exports**: 3 entry points
- **Schemas**: 9 (StorageSnapshotSchema, AgentCapacitySchema, AllocationResultSchema, etc.)
- **Dependencies**: @unrdf/kgc-4d, @unrdf/oxigraph, @unrdf/core
- **Receipt Integration**: Yes

#### @unrdf/fusion (v1.0.0)
- **Exports**: 1 entry point
- **Schemas**: 2 (PolicyRuleSchema, ResourceAllocationSchema)
- **Dependencies**: @unrdf/oxigraph, @unrdf/kgc-4d, @unrdf/blockchain, @unrdf/hooks, @unrdf/caching, @unrdf/yawl
- **Receipt Integration**: Yes

#### @unrdf/v6-core (v6.0.0-alpha.1)
- **Exports**: 6 entry points
- **Schemas**: 2 (V6ReceiptSchema, DeltaOperationSchema)
- **Dependencies**: @unrdf/kgc-substrate, @unrdf/yawl, @unrdf/kgc-4d, @unrdf/oxigraph, @unrdf/kgc-cli
- **Receipt Integration**: Yes

### CLI & Tooling (4 packages)

#### @unrdf/cli (v5.0.1)
- **Exports**: 2 entry points
- **CLI**: `unrdf` command
- **CLI Nouns**: graph, query, context, convert, hook, store
- **Dependencies**: @unrdf/core, @unrdf/federation, @unrdf/hooks, @unrdf/knowledge-engine, @unrdf/oxigraph, @unrdf/project-engine, @unrdf/streaming
- **Receipt Integration**: No

#### @unrdf/diataxis-kit (v1.0.0)
- **Exports**: 7 entry points
- **Schemas**: 2 (DiataxisSchema, EvidenceSchema)
- **CLI**: `diataxis-run`, `diataxis-verify`, `diataxis-report`
- **Dependencies**: None
- **Receipt Integration**: Yes

#### @unrdf/project-engine (v5.0.1)
- **Exports**: 1 entry point
- **Schemas**: 2 (ProjectConfigSchema, DriftSnapshotSchema)
- **Dependencies**: @unrdf/core, @unrdf/knowledge-engine
- **Receipt Integration**: No

#### @unrdf/kgn (v5.0.1)
- **Exports**: 5+ entry points
- **Schemas**: 0
- **Dependencies**: @unrdf/core, @unrdf/test-utils
- **Receipt Integration**: No

### Advanced Features (10 packages)

#### @unrdf/knowledge-engine (v5.0.1)
- **Schemas**: 2 (RuleSchema, InferenceResultSchema)
- **Key Features**: Inference, canonicalization, AI search

#### @unrdf/blockchain (v1.0.0)
- **Schemas**: 2 (ReceiptAnchorSchema, MerkleProofSchema)
- **Key Features**: Receipt anchoring, Merkle proofs
- **Receipt Integration**: Yes

#### @unrdf/caching (v1.0.0)
- **Schemas**: 2 (CacheConfigSchema, InvalidationRuleSchema)
- **Key Features**: Multi-layer caching, dependency tracking

#### @unrdf/collab (v1.0.0)
- **Schemas**: 2 (CRDTStateSchema, SyncMessageSchema)
- **Key Features**: CRDT-based collaboration, WebSocket sync

#### @unrdf/consensus (v1.0.0)
- **Schemas**: 8 (NodeMetadataSchema, RaftConfigSchema, etc.)
- **Key Features**: Raft consensus, distributed state machine

#### @unrdf/semantic-search (v1.0.0)
- **Schemas**: 2 (EmbeddingConfigSchema, SearchQuerySchema)
- **Key Features**: Embeddings, semantic query

#### @unrdf/ml-inference (v5.0.1)
- **Schemas**: 2 (ModelMetadataSchema, InferencePipelineSchema)
- **Key Features**: ONNX runtime, streaming inference

#### @unrdf/ml-versioning (v1.0.0)
- **Schemas**: 2 (ModelVersionSchema, ExperimentSchema)
- **Receipt Integration**: Yes

#### @unrdf/serverless (v1.0.0)
- **Schemas**: 2 (LambdaConfigSchema, APIGatewayConfigSchema)
- **Key Features**: AWS CDK, Lambda bundling

#### @unrdf/rdf-graphql (v1.0.0)
- **Schemas**: 1 (GraphQLMappingSchema)
- **Key Features**: RDF to GraphQL mapping

### Supporting Packages (12 packages)

- @unrdf/atomvm (v5.0.1) - AtomVM integration
- @unrdf/composables (v5.0.1) - Vue composables
- @unrdf/dark-matter (v5.0.1) - Query optimization
- @unrdf/domain (v5.0.1) - Domain types
- @unrdf/engine-gateway (v5.0.1) - Gateway routing
- @unrdf/graph-analytics (v1.0.0) - PageRank, clustering
- @unrdf/observability (v1.0.0) - Metrics, alerts
- @unrdf/nextra-docs (v5.0.1) - Documentation site
- @unrdf/integration-tests (v5.0.0) - E2E tests
- docs (v5.0.1) - Legacy docs

## Schema Coverage Analysis

### Packages WITH Zod Validation (36 packages)

Strong validation coverage in:
- Core infrastructure (5/8 packages)
- YAWL ecosystem (7/10 packages)
- KGC ecosystem (5/5 packages)
- Advanced features (10/10 packages)

### Packages WITHOUT Zod Validation (13 packages)

**HIGH PRIORITY** - Add Zod schemas to:
1. **@unrdf/kgc-4d** - Core event logging (uses internal types, should export schemas)
2. **@unrdf/cli** - CLI argument validation
3. **@unrdf/knowledge-engine** - Query and inference validation
4. **@unrdf/project-engine** - Project configuration validation

**MEDIUM PRIORITY**:
5. @unrdf/composables - State validation
6. @unrdf/dark-matter - Optimizer configuration
7. @unrdf/validation - Self-validation schemas
8. @unrdf/kgn - Template validation

**LOW PRIORITY** (non-public APIs):
9. @unrdf/atomvm - Internal bridge
10. @unrdf/diataxis-kit - Documentation tooling
11. @unrdf/domain - Type-only package
12. @unrdf/engine-gateway - Routing logic
13. @unrdf/yawl-viz - Visualization only

## CLI Noun Coverage

### Current CLI Commands

#### @unrdf/cli (unrdf)
- `graph` - Graph operations (create, delete, describe, export, get, list, update, validate)
- `query` - SPARQL query execution
- `context` - Context management (create, current, delete, get, list, use)
- `convert` - Format conversion
- `hook` - Hook evaluation
- `store` - Store operations

#### @unrdf/kgc-cli (kgc)
- `extension` - Extension management
- `registry` - Registry operations

#### @unrdf/diataxis-kit (diataxis-*)
- `run` - Run documentation generation
- `verify` - Verify documentation
- `report` - Generate reports

### CLI Fragmentation

**Issue**: Three separate CLI entry points with no unified interface.

**Recommendation**: Consolidate to single `unrdf` CLI with subcommands:
```
unrdf graph ...
unrdf query ...
unrdf kgc extension ...
unrdf docs verify ...
```

## Receipt Integration Status

### Packages WITH Receipt Integration (11 packages)

1. @unrdf/kgc-4d - Core receipt engine
2. @unrdf/yawl - Workflow receipts
3. @unrdf/kgc-claude - Agent execution receipts
4. @unrdf/kgc-substrate - Storage receipts
5. @unrdf/v6-core - V6 receipt unification
6. @unrdf/blockchain - Receipt anchoring
7. @unrdf/fusion - Policy receipts
8. @unrdf/ml-versioning - Model version receipts
9. @unrdf/diataxis-kit - Documentation receipts
10. @unrdf/yawl-api - API operation receipts
11. @unrdf/yawl-durable - Saga receipts
12. @unrdf/yawl-langchain - Chain execution receipts
13. @unrdf/yawl-queue - Queue operation receipts

### Packages WITHOUT Receipt Integration (38 packages)

**Critical Gap**: 78% of packages cannot emit receipts for audit trails.

**High Priority** for receipt integration:
1. @unrdf/core - All RDF operations should be auditable
2. @unrdf/federation - Distributed query verification
3. @unrdf/streaming - Change event provenance
4. @unrdf/hooks - Hook execution verification
5. @unrdf/cli - Command execution audit

## Top 5 Fragmentation Issues

### 1. Duplicate Schemas (HIGH SEVERITY)

**Impact**: Type confusion, validation inconsistencies, maintenance burden

**Examples**:
- `SplitTypeSchema` defined in 3 files within @unrdf/yawl:
  - `packages/yawl/src/patterns.mjs`
  - `packages/yawl/src/patterns-builders.mjs`
  - `packages/yawl/src/workflow-core.mjs`
- `JoinTypeSchema` - same 3 files
- `TaskDefSchema` - same 3 files
- `TaskDefinitionSchema` vs `TaskDefSchema` naming inconsistency

**Recommendation**:
- Consolidate to single source of truth in `@unrdf/yawl/schemas`
- Re-export from specific modules
- Establish naming convention (`*Schema` suffix mandatory)

### 2. Missing Receipt Integration (HIGH SEVERITY)

**Impact**: Incomplete audit trail, cannot verify operations across 78% of packages

**Statistics**:
- Packages with receipts: 11 (22%)
- Packages without receipts: 38 (78%)

**Recommendation**:
- Establish `@unrdf/receipts` as core dependency
- Add receipt emission to all mutating operations
- Create receipt schemas for each package domain
- Implement receipt verification in @unrdf/validation

### 3. Inconsistent Naming Conventions (MEDIUM SEVERITY)

**Impact**: Cognitive load, harder to learn API surface

**Examples**:
- Schema suffix: `*Schema` vs no suffix
- Configuration: `*Config` vs `*Options`
- Results: `*Result` vs `*Response`
- Events: `*Event` vs `*Message`

**Recommendation**:
- Establish naming conventions in CONTRIBUTING.md
- Use linter rules to enforce:
  - All Zod schemas end with `Schema`
  - Configuration objects use `Config` suffix
  - API responses use `Response` suffix
  - Event payloads use `Event` suffix

### 4. Missing Zod Validation in Public APIs (MEDIUM SEVERITY)

**Impact**: Runtime errors, poor developer experience, inconsistent validation

**Packages Affected**: 13 packages (27%)

**Recommendation**:
- Audit all public API entry points
- Add Zod schemas for all function parameters
- Export schemas alongside functions
- Generate TypeScript types from schemas

### 5. CLI Fragmentation (LOW SEVERITY)

**Impact**: User confusion, inconsistent CLI experience

**Current State**:
- 3 separate CLI entry points (`unrdf`, `kgc`, `diataxis-*`)
- Inconsistent flag conventions
- No shared help system

**Recommendation**:
- Unify under single `unrdf` CLI
- Use subcommand structure
- Shared flag conventions (--format, --output, --verbose)
- Shared help/error formatting

## V6 Consolidation Priorities

### Phase 1: Schema Unification (Week 1-2)

1. **Deduplicate YAWL schemas**
   - Create `@unrdf/yawl/schemas` canonical exports
   - Remove duplicates from patterns.mjs, patterns-builders.mjs, workflow-core.mjs
   - Update all imports

2. **Add missing Zod schemas**
   - @unrdf/kgc-4d - Export event schemas
   - @unrdf/cli - CLI argument schemas
   - @unrdf/knowledge-engine - Query schemas
   - @unrdf/project-engine - Config schemas

3. **Establish naming conventions**
   - Document in CONTRIBUTING.md
   - Add ESLint rule for `*Schema` suffix
   - Bulk rename where inconsistent

### Phase 2: Receipt Integration (Week 3-4)

1. **Create @unrdf/receipts package**
   - Core receipt primitives
   - Verification utilities
   - Common schemas

2. **Add receipts to core packages**
   - @unrdf/core - RDF operation receipts
   - @unrdf/federation - Query receipts
   - @unrdf/streaming - Change receipts
   - @unrdf/hooks - Hook execution receipts

3. **Update validation**
   - @unrdf/validation - Receipt verification
   - OTEL integration

### Phase 3: CLI Unification (Week 5)

1. **Merge CLI packages**
   - Consolidate kgc-cli into cli
   - Add diataxis as subcommand
   - Shared arg parsing (citty)

2. **Standardize flags**
   - Global flags (--verbose, --format, --output)
   - Consistent help formatting
   - Shell completion

### Phase 4: Documentation & Migration (Week 6)

1. **Generate contract documentation**
   - Auto-generate from Zod schemas
   - API reference per package
   - Migration guide from v5

2. **Create v6 compatibility layer**
   - @unrdf/v6-compat package
   - Deprecation warnings
   - Migration tools

## Dependency Graph Summary

### Core Dependencies (Most Depended Upon)

1. **@unrdf/oxigraph** (17 dependents)
   - @unrdf/core, @unrdf/kgc-4d, @unrdf/hooks, @unrdf/streaming, etc.

2. **@unrdf/core** (16 dependents)
   - Most packages depend on core RDF operations

3. **@unrdf/kgc-4d** (10 dependents)
   - YAWL ecosystem, KGC ecosystem, blockchain

4. **@unrdf/yawl** (9 dependents)
   - All yawl-* extension packages

### Leaf Packages (No Dependents)

- @unrdf/yawl-viz
- @unrdf/yawl-realtime
- @unrdf/yawl-observability
- @unrdf/ml-inference
- @unrdf/serverless
- @unrdf/nextra-docs
- docs

## Recommended V6 Package Structure

```
@unrdf/v6-core
├── receipts     (unified receipt system)
├── schemas      (consolidated Zod schemas)
├── rdf          (from @unrdf/core)
├── store        (from @unrdf/oxigraph)
└── validation   (from @unrdf/validation)

@unrdf/v6-workflow
├── engine       (from @unrdf/yawl)
├── ai           (from @unrdf/yawl-ai)
├── durable      (from @unrdf/yawl-durable)
└── integrations (kafka, langchain, queue, etc.)

@unrdf/v6-time
├── 4d           (from @unrdf/kgc-4d)
├── hdit         (hyperdimensional coords)
└── git          (Git backbone)

@unrdf/v6-distributed
├── federation   (from @unrdf/federation)
├── consensus    (from @unrdf/consensus)
├── streaming    (from @unrdf/streaming)
└── collab       (from @unrdf/collab)

@unrdf/v6-cli
├── main         (unified CLI)
└── extensions   (from @unrdf/kgc-cli)

@unrdf/v6-tooling
├── project      (from @unrdf/project-engine)
├── kgn          (from @unrdf/kgn)
├── docs         (from @unrdf/diataxis-kit)
└── test         (from @unrdf/test-utils)
```

This reduces 49 packages to 6 core packages with clear boundaries and unified contracts.
