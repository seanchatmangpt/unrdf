# UNRDF Runtime Analysis - Detailed Notes

## File Paths of Runtime-Specific Code

### Node-only Packages and Their Node-specific Code

### @unrdf/atomvm

**Path**: `packages/atomvm`

**Node Modules Used**:

- `fs`
- `path`
- `module`
- `child_process`

**Node-specific Patterns**:

- `__dirname` (2 occurrences)
- `__filename` (2 occurrences)

**Files with Node-specific Code** (2):

- `src/cli.mjs`
- `src/node-runtime.mjs`

### @unrdf/caching

**Path**: `packages/caching`

**Node-specific Dependencies**: ioredis

### @unrdf/cli

**Path**: `packages/cli`

**Node Modules Used**:

- `fs`
- `path`
- `process`

**Node-specific Patterns**:

- `process.cwd()` (7 occurrences)
- `Buffer` (2 occurrences)

**Files with Node-specific Code** (10):

- `src/commands/graph/create.mjs`
- `src/commands/graph/delete.mjs`
- `src/commands/graph/list.mjs`
- `src/commands/graph/update.mjs`
- `src/commands/hook/eval.mjs`
- `src/commands/init.mjs`
- `src/lib/store-backup.mjs`
- `src/lib/store-restore.mjs`
- `src/store-backup.mjs`
- `src/store-restore.mjs`

### @unrdf/collab

**Path**: `packages/collab`

**Node-specific Dependencies**: ws

### @unrdf/consensus

**Path**: `packages/consensus`

**Node Modules Used**:

- `events`
- `crypto`

**Files with Node-specific Code** (4):

- `src/membership/cluster-manager.mjs`
- `src/raft/raft-coordinator.mjs`
- `src/state/distributed-state-machine.mjs`
- `src/transport/websocket-transport.mjs`

**Node-specific Dependencies**: ws

### @unrdf/core

**Path**: `packages/core`

**Node Modules Used**:

- `child_process`
- `fs`
- `path`
- `crypto`

**Node-specific Patterns**:

- `process.env` (5 occurrences)
- `global` (6 occurrences)
- `Buffer` (3 occurrences)
- `process.cwd()` (2 occurrences)
- `__dirname` (2 occurrences)

**Files with Node-specific Code** (16):

- `src/config.mjs`
- `src/debug.mjs`
- `src/diff.mjs`
- `src/health.mjs`
- `src/logger.mjs`
- `src/profiling/memory-profiler.mjs`
- `src/security.mjs`
- `src/utils/circuit-breaker.mjs`
- `src/utils/enhanced-errors.mjs`
- `src/utils/io-utils.mjs`
- ... and 6 more files

### @unrdf/federation

**Path**: `packages/federation`

**Node Modules Used**:

- `events`
- `crypto`

**Files with Node-specific Code** (3):

- `src/federation/consensus-manager.mjs`
- `src/federation/data-replication.mjs`
- `src/federation/federation-coordinator.mjs`

### @unrdf/hooks

**Path**: `packages/hooks`

**Node Modules Used**:

- `worker_threads`
- `path`
- `crypto`
- `fs`

**Node-specific Patterns**:

- `process.cwd()` (4 occurrences)
- `__dirname` (4 occurrences)
- `__filename` (3 occurrences)
- `process.env` (4 occurrences)
- `global` (6 occurrences)
- `Buffer` (1 occurrence)

**Files with Node-specific Code** (17):

- `src/hooks/condition-evaluator.mjs`
- `src/hooks/effect-sandbox-worker.mjs`
- `src/hooks/effect-sandbox.mjs`
- `src/hooks/file-resolver.mjs`
- `src/hooks/knowledge-hook-engine.mjs`
- `src/hooks/observability.mjs`
- `src/hooks/policy-pack.mjs`
- `src/hooks/quad-pool.mjs`
- `src/hooks/security/path-validator.mjs`
- `src/hooks/security/sandbox-restrictions.mjs`
- ... and 7 more files

### @unrdf/kgc-4d

**Path**: `packages/kgc-4d`

**Node Modules Used**:

- `fs`
- `path`
- `crypto`

**Node-specific Patterns**:

- `Buffer` (2 occurrences)
- `process.env` (1 occurrence)

**Files with Node-specific Code** (6):

- `src/doctest/extractor.mjs`
- `src/doctest/runner.mjs`
- `src/git.mjs`
- `src/guards.mjs`
- `src/store.mjs`
- `src/time.mjs`

### @unrdf/kgc-claude

**Path**: `packages/kgc-claude`

**Node Modules Used**:

- `fs`
- `path`

**Node-specific Patterns**:

- `process.cwd()` (1 occurrence)

**Files with Node-specific Code** (1):

- `src/PolicyBridge.mjs`

### @unrdf/kgc-substrate

**Path**: `packages/kgc-substrate`

**Node Modules Used**:

- `crypto`
- `fs`
- `path`

**Node-specific Patterns**:

- `Buffer` (1 occurrence)

**Files with Node-specific Code** (3):

- `src/KnowledgeStore.mjs`
- `src/ReceiptChain.mjs`
- `src/Workspace.mjs`

### @unrdf/kgn

**Path**: `packages/kgn`

**Node Modules Used**:

- `crypto`
- `fs`
- `path`
- `worker_threads`
- `stream`
- `perf_hooks`
- `os`
- `child_process`
- `util`
- `events`

**Node-specific Patterns**:

- `Buffer` (2 occurrences)
- `__dirname` (3 occurrences)
- `__filename` (4 occurrences)
- `process.cwd()` (2 occurrences)
- `process.env` (3 occurrences)
- `global` (1 occurrence)

**Files with Node-specific Code** (27):

- `src/base/filter-templates.js`
- `src/base/template-base.js`
- `src/core/attestor.js`
- `src/core/filters.js`
- `src/core/kgen-engine.js`
- `src/core/post-processor.js`
- `src/doc-generator/cli.mjs`
- `src/doc-generator/mdx-generator.mjs`
- `src/doc-generator/parser.mjs`
- `src/doc-generator/rdf-builder.mjs`
- ... and 17 more files

**Node-specific Dependencies**: fs-extra

### @unrdf/knowledge-engine

**Path**: `packages/knowledge-engine`

**Node Modules Used**:

- `worker_threads`
- `path`
- `crypto`
- `fs`
- `assert`
- `events`

**Node-specific Patterns**:

- `process.env` (4 occurrences)
- `Buffer` (3 occurrences)
- `process.cwd()` (8 occurrences)
- `global` (7 occurrences)
- `__dirname` (3 occurrences)
- `__filename` (2 occurrences)

**Files with Node-specific Code** (27):

- `src/browser-shims.mjs`
- `src/condition-evaluator.mjs`
- `src/define-hook.mjs`
- `src/effect-sandbox-browser.mjs`
- `src/effect-sandbox-worker.mjs`
- `src/effect-sandbox.mjs`
- `src/file-resolver.mjs`
- `src/hook-executor.mjs`
- `src/hook-management.mjs`
- `src/ken.mjs`
- ... and 17 more files

### @unrdf/ml-inference

**Path**: `packages/ml-inference`

**Node Modules Used**:

- `fs`

**Files with Node-specific Code** (1):

- `src/utils/model-generator.mjs`

### @unrdf/ml-versioning

**Path**: `packages/ml-versioning`

**Node Modules Used**:

- `crypto`

**Files with Node-specific Code** (1):

- `src/version-store.mjs`

### @unrdf/observability

**Path**: `packages/observability`

**Node-specific Dependencies**: express

### @unrdf/project-engine

**Path**: `packages/project-engine`

**Node Modules Used**:

- `fs`
- `path`
- `crypto`

**Files with Node-specific Code** (11):

- `src/code-complexity-js.mjs`
- `src/domain-infer.mjs`
- `src/drift-snapshot.mjs`
- `src/fs-scan.mjs`
- `src/golden-structure.mjs`
- `src/initialize.mjs`
- `src/mapek-orchestration.mjs`
- `src/materialize-apply.mjs`
- `src/materialize-plan.mjs`
- `src/materialize.mjs`
- ... and 1 more files

### @unrdf/serverless

**Path**: `packages/serverless`

**Node-specific Patterns**:

- `global` (2 occurrences)
- `process.env` (2 occurrences)

**Files with Node-specific Code** (3):

- `src/cdk/unrdf-stack.mjs`
- `src/deploy/lambda-bundler.mjs`
- `src/storage/dynamodb-adapter.mjs`

**Node-specific Dependencies**: aws-cdk-lib

### @unrdf/streaming

**Path**: `packages/streaming`

**Node Modules Used**:

- `events`
- `crypto`

**Files with Node-specific Code** (2):

- `src/streaming/real-time-validator.mjs`
- `src/streaming/sync-protocol.mjs`

**Node-specific Dependencies**: ws

### @unrdf/yawl

**Path**: `packages/yawl`

**Node Modules Used**:

- `crypto`

**Files with Node-specific Code** (8):

- `src/api/workflow-api-validation.mjs`
- `src/api/workflow-creation.mjs`
- `src/cancellation/yawl-cancellation-core.mjs`
- `src/cancellation/yawl-cancellation-manager.mjs`
- `src/cancellation/yawl-cancellation-regions.mjs`
- `src/cancellation/yawl-cancellation.mjs`
- `src/engine.mjs`
- `src/hooks/yawl-hooks.mjs`

### @unrdf/yawl-api

**Path**: `packages/yawl-api`

### @unrdf/yawl-kafka

**Path**: `packages/yawl-kafka`

**Node-specific Patterns**:

- `process.env` (1 occurrence)
- `Buffer` (1 occurrence)

**Files with Node-specific Code** (2):

- `src/examples/analytics-pipeline.mjs`
- `src/schemas.mjs`

**Node-specific Dependencies**: kafkajs

### @unrdf/yawl-queue

**Path**: `packages/yawl-queue`

**Node-specific Dependencies**: ioredis

## Dual-runtime Packages (Browser-safe)

These packages can safely be used in browsers:

- **@unrdf/blockchain** (`packages/blockchain`)
  - Blockchain integration for UNRDF - Cryptographic receipt anchoring and audit trails
- **@unrdf/diataxis-kit** (`packages/diataxis-kit`)
  - Diátaxis documentation kit for monorepo package inventory and deterministic doc scaffold generation
- **@unrdf/engine-gateway** (`packages/engine-gateway`)
  - μ(O) Engine Gateway - Enforcement layer for Oxigraph-first, N3-minimal RDF processing
- **@unrdf/graph-analytics** (`packages/graph-analytics`)
  - Advanced graph analytics for RDF knowledge graphs using graphlib
- **@unrdf/oxigraph** (`packages/oxigraph`)
  - UNRDF Oxigraph - Graph database benchmarking implementation using Oxigraph SPARQL engine
- **@unrdf/rdf-graphql** (`packages/rdf-graphql`)
  - Type-safe GraphQL interface for RDF knowledge graphs with automatic schema generation
- **@unrdf/semantic-search** (`packages/semantic-search`)
  - AI-powered semantic search over RDF knowledge graphs using vector embeddings
- **@unrdf/yawl-ai** (`packages/yawl-ai`)
  - AI-powered workflow optimization using TensorFlow.js and YAWL patterns
- **@unrdf/yawl-durable** (`packages/yawl-durable`)
  - Durable execution framework inspired by Temporal.io using YAWL and KGC-4D
- **@unrdf/yawl-langchain** (`packages/yawl-langchain`)
  - LangChain integration for YAWL workflow engine - AI-powered workflow orchestration with RDF context
- **@unrdf/yawl-realtime** (`packages/yawl-realtime`)
  - Real-time collaboration framework for YAWL workflows using Socket.io

## Gaps and Recommendations

### Critical Findings

1. **Streaming package** - Requires Node `stream` module. For browser use:
   - Replace with fetch-based streaming
   - Use async iterables instead of Node streams

2. **Hooks package** - Has file resolver and sandbox. For browser:
   - Abstract file operations
   - Use IndexedDB for storage

3. **Knowledge engine** - File I/O operations. For browser:
   - Move file operations to separate adapters
   - Use URL-based configuration

### Workarounds for Browser Deployment

1. **File System Abstraction**

   ```javascript
   interface StorageProvider {
     read(path: string): Promise<string>;
     write(path: string, data: string): Promise<void>;
   }
   ```

2. **Environment Variables**

   ```javascript
   // Instead of: process.env.DEBUG
   const config = { debug: true }; // Pass as config object
   ```

3. **Polyfills for Common Modules**
   - `buffer` → Use Uint8Array
   - `crypto` → Use Web Crypto API
   - `path` → Use custom path utilities

## Implementation Strategy

For maximum browser compatibility:

1. **Phase 1**: Use dual-runtime packages directly
2. **Phase 2**: Wrap Node-only packages with adapters
3. **Phase 3**: Tree-shake unused Node code at build time

## Evidence

- Total packages analyzed: 40
- Node-only: 23
- Dual-runtime: 11
- Browser-only: 6

Generated: 2025-12-27T03:06:44.718Z
