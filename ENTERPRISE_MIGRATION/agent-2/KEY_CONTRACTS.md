# Key Package Contracts - Detailed View

Generated from CONTRACTS.lock.json
Date: 2025-12-26
Hash: 2a3c1f197191d56c

---

## @unrdf/oxigraph

**Purpose**: Graph database using Oxigraph SPARQL engine
**Version**: 5.0.1
**Hash**: df5027117f89b55d

### Public API

#### Main Export (.)
- `createStore(quads)` - Create new Oxigraph-backed RDF store
  - Parameters: quads (required)
  - Returns: OxigraphStore
  - Async: No

- `dataFactory` - RDF term creation functions
  - namedNode, blankNode, literal, defaultGraph, quad, triple

- `OxigraphStore` - Store class

#### Store Export (./store)
- `OxigraphStore` - Direct access to store class

#### Types Export (./types)
- Type definitions (no exports)

### Error Patterns
- "Quad is required" (3 instances)
- "Query must be a non-empty string" (2 instances)
- "Data must be a non-empty string"
- "Format option is required" (2 instances)

### Logging Fields
- duration
- error
- operation

---

## @unrdf/core

**Purpose**: RDF Graph Operations, SPARQL Execution, Foundational Substrate
**Version**: 5.0.1
**Hash**: 48d3590cb8a230ea

### Public API

#### Exports (14 total)
1. `.` - Main entry point
2. `./rdf` - RDF operations
3. `./rdf/minimal-n3-integration` - N3 integration
4. `./rdf/n3-justified-only` - Justified N3 usage
5. `./sparql` - SPARQL execution
6. `./types` - Type definitions
7. `./constants` - Constants
8. `./validation` - Validation utilities
9. `./health` - Health checks
10. `./logger` - Logging utilities
11. `./metrics` - Metrics collection
12. `./security` - Security utilities
13. `./security-schemas` - Security schemas
14. `./utils/sparql-utils` - SPARQL utilities

### Key Functions

**RDF Operations**:
- Data model creation
- Quad manipulation
- N3 streaming (justified modules only)

**SPARQL**:
- Query execution
- Result processing
- SPARQL utilities

**Validation**:
- Input validation
- Schema validation
- SHACL support

### Architecture Notes
- N3 library centralized in justified modules
- Pure functions for RDF operations
- Zod schemas for validation

---

## @unrdf/hooks

**Purpose**: Policy Definition and Execution Framework
**Version**: 5.0.1
**Hash**: 3b23ca267b112696

### Public API

#### Hook Definition (./define)
- `defineHook` - Define new knowledge hook
- `isValidHook` - Validate hook structure
- `getHookMetadata` - Extract metadata
- `hasValidation` - Check if hook validates
- `hasTransformation` - Check if hook transforms
- Schemas: `HookTriggerSchema`, `HookConfigSchema`, `HookSchema`

#### Hook Execution (./executor)
- `executeHook` - Run single hook
- `executeHookChain` - Run hook chain
- `executeHooksByTrigger` - Run hooks by trigger
- `wouldPassHooks` - Dry-run validation
- `validateOnly` - Validation-only execution
- `clearHookCaches` - Cache management
- `prewarmHookCache` - Cache warming
- Batch API:
  - `executeBatch` - Batch execution
  - `validateBatch` - Batch validation
  - `transformBatch` - Batch transformation

#### Advanced Features
- `compileHookChain` - JIT compilation
- `compileValidationOnlyChain` - Validation JIT
- `clearCompiledChainCache` - Clear compiled cache
- `getCompilerStats` - Compiler statistics
- `isJitAvailable` - Check JIT availability
- `QuadPool` - Object pooling for performance
- `KnowledgeHookManager` - Class-based interface
- `HookScheduler` - Cron/interval triggers
- `QualityMetricsCollector` - Lean Six Sigma hooks

### Error Patterns
- "Hook must define either validate, transform, or run function"
- "SHACL condition requires ref with uri and sha256"
- "Policy pack not loaded. Call load() first." (6 instances)
- "VM2 not available. Install with: pnpm add vm2"

### Built-in Hooks
- `validateSubjectIRI`, `validatePredicateIRI`, `validateObjectLiteral`
- `validateIRIFormat`, `validateLanguageTag`, `rejectBlankNodes`
- `normalizeNamespace`, `normalizeLanguageTag`, `trimLiterals`
- Pooled variants: `normalizeLanguageTagPooled`, `trimLiteralsPooled`
- `standardValidation` - Standard validation suite

---

## @unrdf/streaming

**Purpose**: Change Feeds and Real-time Synchronization
**Version**: 5.0.1
**Hash**: 7545bae95a43c930

### Public API

#### Main Export (.)
- Streaming infrastructure
- Change feed processing
- Real-time sync

#### Processor Export (./processor)
- Stream processors
- Change handlers
- Sync coordinators

### Dependencies
- @opentelemetry/api
- @unrdf/core
- @unrdf/hooks
- @unrdf/oxigraph
- ws (WebSocket)
- lru-cache

### Logging Fields
- duration
- error
- operation
- requestId
- statusCode

---

## @unrdf/federation

**Purpose**: Peer Discovery and Distributed Query Execution
**Version**: 5.0.1
**Hash**: 33e3ba54db7ffd96

### Public API

#### Main Export (.)
- Federation infrastructure
- Peer discovery
- Query distribution

#### Coordinator Export (./coordinator)
- Query coordination
- Result aggregation
- Peer management

#### Advanced SPARQL (./advanced-sparql)
- Distributed SPARQL execution
- Federated query optimization
- Result merging

### Dependencies
- @comunica/query-sparql
- @opentelemetry/api
- @unrdf/core
- @unrdf/hooks
- prom-client (Prometheus metrics)

### Logging Fields
- duration
- error
- operation

---

## @unrdf/kgc-4d

**Purpose**: 4D Datum & Universe Freeze Engine - Nanosecond-precision event logging
**Version**: 5.0.1
**Hash**: 3d5df499b3d77e49

### Public API

#### Main Export (.)
- Event logging
- Time-travel queries
- Universe snapshots

#### Client Export (./client)
- Client interface
- Query API
- Snapshot management

#### HDIT Export (./hdit)
- Hyperdimensional indexing
- Time-series operations
- Git-backed snapshots

### Dependencies
- @unrdf/core
- @unrdf/oxigraph
- isomorphic-git
- hash-wasm

### Features
- Nanosecond precision timestamps
- Git-backed event storage
- 4D query capabilities
- Universe freeze/thaw

---

## @unrdf/yawl

**Purpose**: Workflow Automation and Orchestration
**Version**: 5.0.0
**Hash**: 75f972c62eb021a5

### Public API

**13 Exports** - Largest package by export count

#### Core Workflow
- Workflow definition
- Step execution
- State management
- Event handling

#### Advanced Features
- Conditional branching
- Parallel execution
- Error recovery
- Retry mechanisms
- Timeout handling

### Extension Packages
- **yawl-ai**: AI integration
- **yawl-api**: REST API
- **yawl-durable**: Durable execution
- **yawl-kafka**: Kafka integration
- **yawl-langchain**: LangChain integration
- **yawl-observability**: OTEL integration
- **yawl-queue**: Queue-based workflows
- **yawl-realtime**: Real-time workflows
- **yawl-viz**: Workflow visualization

---

## Package Interdependencies

### Dependency Graph

```
@unrdf/oxigraph (base layer)
  └─> @unrdf/core
      ├─> @unrdf/hooks
      │   ├─> @unrdf/streaming
      │   └─> @unrdf/federation
      ├─> @unrdf/kgc-4d
      └─> [other packages]
```

### Core Stack
1. **oxigraph**: Storage engine
2. **core**: RDF operations + SPARQL
3. **hooks**: Policy enforcement
4. **streaming**: Real-time sync
5. **federation**: Distribution

### Critical Contracts

**MUST NOT BREAK** (used by all packages):
- `@unrdf/oxigraph.createStore(quads)`
- `@unrdf/oxigraph.dataFactory.*`
- `@unrdf/core` RDF operations
- `@unrdf/core` SPARQL execution

**Version Constraints**:
- Breaking changes in oxigraph → major version bump for ALL packages
- Breaking changes in core → major version bump for dependent packages
- Breaking changes in hooks → minor impact (opt-in)

---

## Contract Hashes by Package

| Package | Version | Hash | Stability |
|---------|---------|------|-----------|
| @unrdf/oxigraph | 5.0.1 | df5027117f89b55d | Critical |
| @unrdf/core | 5.0.1 | 48d3590cb8a230ea | Critical |
| @unrdf/hooks | 5.0.1 | 3b23ca267b112696 | Stable |
| @unrdf/streaming | 5.0.1 | 7545bae95a43c930 | Stable |
| @unrdf/federation | 5.0.1 | 33e3ba54db7ffd96 | Stable |
| @unrdf/kgc-4d | 5.0.1 | 3d5df499b3d77e49 | Stable |
| @unrdf/yawl | 5.0.0 | 75f972c62eb021a5 | Active |

**Stability Levels**:
- **Critical**: Breaking changes affect all packages
- **Stable**: Mature API, rare breaking changes
- **Active**: Frequent additions, occasional breaking changes

---

## DTO Schemas Detected

### Validation Schemas (Zod)
- HookTriggerSchema
- HookConfigSchema
- HookSchema
- HookResultSchema
- ChainResultSchema
- HookRegistrySchema
- ScheduleConfigSchema
- QualityGateSchema
- SPCDataPointSchema

### Naming Conventions

**Observed Patterns**:
- Schemas: `*Schema` suffix
- DTOs: `*DTO` suffix (less common)
- Request/Response: Inline types (not exported)

---

## Logging Contract

### Standard Fields (Observed)
- `packageName` - Package identifier
- `functionName` - Function being executed
- `operation` - Operation type
- `duration` - Execution time (ms)
- `error` - Error object or message
- `statusCode` - HTTP status or result code
- `requestId` - Request correlation ID
- `userId` - User identifier
- `traceId` - Distributed tracing ID

### Log Levels (Standard)
- debug
- info
- warn
- error
- fatal

**Note**: Most packages don't use structured logging yet (opportunity for standardization).

---

## Error Code Patterns

### Observed Conventions

**Validation Errors**:
- "X is required"
- "X must be a non-empty string"
- "X must be a Y"

**State Errors**:
- "X not loaded. Call Y first."
- "X not available"
- "Cannot X: Y"

**Recommendation**: Standardize error codes with prefixes:
- `ERR_VALIDATION_*` - Input validation
- `ERR_STATE_*` - Invalid state
- `ERR_RUNTIME_*` - Runtime errors
- `ERR_EXTERNAL_*` - External system errors

---

## ID and Naming Rules

### Package Naming
- Scope: `@unrdf/`
- Format: `kebab-case`
- Examples: `oxigraph`, `kgc-4d`, `yawl-kafka`

### Code Conventions
- **Functions**: `camelCase` (createStore, executeHook)
- **Classes**: `PascalCase` (OxigraphStore, KnowledgeHookManager)
- **Constants**: `UPPER_SNAKE_CASE` (COI_WAIT_DEFAULT_TIMEOUT_MS)
- **Files**: `kebab-case.mjs` (contract-scanner.mjs)

### Export Paths
- Main: `"."` → `./src/index.mjs`
- Sub-exports: `"./feature"` → `./src/feature.mjs`
- Utilities: `"./utils/x"` → `./src/utils/x.mjs`

---

## Contract Evolution Workflow

### 1. Make Changes
Edit source code in any package.

### 2. Verify
```bash
node agent-2/verify-contracts.mjs --verify
```

### 3. Review Differences
If verification fails:
- Review breaking vs non-breaking changes
- Determine version bump required
- Plan migration path if breaking

### 4. Update Version
Update package.json version according to semver:
- Breaking → major (5.0.1 → 6.0.0)
- Addition → minor (5.0.1 → 5.1.0)
- Fix only → patch (5.0.1 → 5.0.2)

### 5. Accept Contracts
```bash
node agent-2/verify-contracts.mjs --accept "v6.0.0 - Add federated SPARQL"
```

### 6. Commit
Commit both package.json and CONTRACTS.lock.json together.

---

**Last Updated**: 2025-12-26
**Contract Version**: 1.0.0
**Overall Hash**: 2a3c1f197191d56c
