# API Reference

Complete API documentation for @unrdf/kgc-claude.

## Table of Contents

- [Constants](#constants)
- [Run Capsule](#run-capsule)
- [Checkpoint](#checkpoint)
- [Autonomy Guard](#autonomy-guard)
- [Shard Merge](#shard-merge)
- [Async Workflow](#async-workflow)
- [Projection](#projection)
- [Substrate Factory](#substrate-factory)

---

## Constants

### GRAPHS

Named graphs for KGC-Claude substrate.

```typescript
export const GRAPHS: {
  UNIVERSE: 'http://kgc.io/Universe';
  EVENT_LOG: 'http://kgc.io/EventLog';
  SYSTEM: 'http://kgc.io/System';
  RUN_CAPSULES: 'http://kgc.io/RunCapsules';
  WORK_ITEMS: 'http://kgc.io/WorkItems';
  AGENT_SHARDS: 'http://kgc.io/AgentShards';
};
```

### PREDICATES

RDF predicates for Claude substrate operations.

```typescript
export const PREDICATES: {
  // Run capsule predicates
  RUN_ID: string;
  RUN_STATUS: string;
  TOOL_TRACE: string;
  ARTIFACTS: string;
  DELTA_O: string;
  DELTA_PI: string;
  DELTA_LAMBDA: string;
  DELTA_Q: string;
  RUN_HASH: string;
  PARENT_RUN: string;

  // Checkpoint predicates
  CHECKPOINT_ID: string;
  SNAPSHOT_HASH: string;
  // ... etc
};
```

### RUN_STATUS

```typescript
export const RUN_STATUS: {
  PENDING: 'pending';
  RUNNING: 'running';
  COMPLETED: 'completed';
  FAILED: 'failed';
  DENIED: 'denied';
  ROLLED_BACK: 'rolled_back';
};
```

### WORK_ITEM_STATUS

```typescript
export const WORK_ITEM_STATUS: {
  QUEUED: 'queued';
  ASSIGNED: 'assigned';
  EXECUTING: 'executing';
  COMPLETED: 'completed';
  FAILED: 'failed';
  CANCELLED: 'cancelled';
};
```

### DENIAL_REASONS

```typescript
export const DENIAL_REASONS: {
  BUDGET_EXCEEDED: 'budget_exceeded';
  DELTA_TOO_LARGE: 'delta_too_large';
  TOOLS_EXCEEDED: 'tools_exceeded';
  FILES_EXCEEDED: 'files_exceeded';
  INVARIANT_VIOLATED: 'invariant_violated';
  SHARD_CONFLICT: 'shard_conflict';
};
```

---

## Run Capsule

Deterministic Δ_run objects for Claude executions.

### Types

#### RunCapsule

```typescript
interface RunCapsule {
  id: string; // UUID
  parentRunId?: string; // UUID of parent run
  t_ns: bigint; // Timestamp in nanoseconds
  timestamp_iso: string; // ISO 8601 timestamp
  status: 'pending' | 'running' | 'completed' | 'failed' | 'denied' | 'rolled_back';

  // The four deltas
  deltaO: Delta[]; // Ontology/universe changes
  deltaPi: Delta[]; // Projection changes
  deltaLambda: Delta[]; // Law/constraint changes
  deltaQ: Delta[]; // Invariant changes

  // Tool trace and artifacts
  toolTrace: ToolCall[];
  artifacts: Artifact[];

  // Cryptographic proofs
  runHash: string; // BLAKE3 hash (64 hex chars)
  previousRunHash: string | null;
  vectorClock?: any;

  // Admission result
  admitted: boolean;
  denialReason?: string;
}
```

#### ToolCall

```typescript
interface ToolCall {
  id: string;
  name: string;
  input?: Record<string, any>;
  output?: any;
  startTime: bigint | number;
  endTime?: bigint | number;
  status: 'pending' | 'success' | 'error';
  error?: string;
}
```

#### Artifact

```typescript
interface Artifact {
  id: string;
  type: 'file' | 'edit' | 'create' | 'delete' | 'command';
  path?: string;
  contentHash?: string;
  metadata?: Record<string, any>;
}
```

#### Delta

```typescript
interface Delta {
  type: 'add' | 'delete' | 'modify';
  target: string;
  before?: any;
  after?: any;
  hash?: string;
}
```

### Functions

#### createRunCapsule

Create a new run capsule builder.

```typescript
function createRunCapsule(options?: {
  parentRunId?: string;
  previousRunHash?: string;
  vectorClock?: any;
}): RunCapsuleBuilder;
```

**Parameters:**

- `options.parentRunId` - UUID of parent run for nested runs
- `options.previousRunHash` - Previous run hash for chaining
- `options.vectorClock` - Vector clock for causality tracking

**Returns:** RunCapsuleBuilder instance

**Example:**

```javascript
const run = createRunCapsule({ previousRunHash: lastRunHash });
run.addToolCall({ name: 'Read', input: { file: 'foo.txt' } });
const capsule = await run.seal();
```

#### RunCapsuleBuilder

Builder instance returned by `createRunCapsule`.

**Methods:**

##### addToolCall

```typescript
addToolCall(call: {
  name: string;
  input?: Record<string, any>;
}): string  // Returns tool call ID
```

##### completeToolCall

```typescript
completeToolCall(callId: string, result: {
  output?: any;
  error?: string;
}): void
```

##### addArtifact

```typescript
addArtifact(artifact: {
  type: 'file' | 'edit' | 'create' | 'delete' | 'command';
  path?: string;
  contentHash?: string;
  metadata?: Record<string, any>;
}): string  // Returns artifact ID
```

##### addDeltaO, addDeltaPi, addDeltaLambda, addDeltaQ

```typescript
addDeltaO(delta: Delta): void
addDeltaPi(delta: Delta): void
addDeltaLambda(delta: Delta): void
addDeltaQ(delta: Delta): void
```

##### getMetrics

```typescript
getMetrics(): {
  deltaSize: number;
  toolOps: number;
  filesTouched: number;
}
```

##### deny

```typescript
deny(reason: string): void
```

##### seal

```typescript
seal(): Promise<RunCapsule>
```

#### checkAdmission

Check if a run capsule should be admitted.

```typescript
function checkAdmission(
  capsule: RunCapsule,
  context?: {
    history?: Set<string>;
    preserveQ?: (capsule: RunCapsule) => boolean;
  }
): { admitted: boolean; reason?: string };
```

**Parameters:**

- `capsule` - Run capsule to check
- `context.history` - Set of previously admitted run hashes
- `context.preserveQ` - Function to check invariant preservation

**Returns:** Admission decision with reason if denied

#### persistRunCapsule

Persist a run capsule to the KGC store.

```typescript
function persistRunCapsule(store: KGCStore, capsule: RunCapsule): Promise<{ receipt: Object }>;
```

#### replayRunCapsule

Replay a run capsule from persisted state.

```typescript
function replayRunCapsule(store: KGCStore, runId: string): Promise<RunCapsule | null>;
```

---

## Checkpoint

Universal freeze/thaw with cryptographic receipts.

### Types

#### CheckpointReceipt

```typescript
interface CheckpointReceipt {
  id: string;
  t_ns: bigint;
  timestamp_iso: string;
  snapshotHash: string;
  gitRef?: string;
  universeSize: number;
  runCapsuleIds: string[];
  vectorClock?: any;
  previousCheckpointHash: string | null;
  checkpointHash: string;
}
```

### Functions

#### freeze

Create a universal checkpoint.

```typescript
function freeze(
  store: KGCStore,
  gitBackbone: GitBackbone,
  options?: {
    runCapsuleIds?: string[];
    vectorClock?: any;
  }
): Promise<CheckpointReceipt>;
```

**Parameters:**

- `store` - KGCStore instance
- `gitBackbone` - GitBackbone instance
- `options.runCapsuleIds` - Run capsule IDs to include
- `options.vectorClock` - Vector clock for causality

**Returns:** Checkpoint receipt with hash

#### thaw

Restore state from a checkpoint.

```typescript
function thaw(store: KGCStore, gitBackbone: GitBackbone, checkpointId: string): Promise<KGCStore>;
```

#### verifyCheckpoint

Verify a checkpoint receipt.

```typescript
function verifyCheckpoint(
  receipt: CheckpointReceipt,
  gitBackbone: GitBackbone
): Promise<{ valid: boolean; reason?: string }>;
```

#### withCheckpoint

Execute with checkpoint protection.

```typescript
function withCheckpoint(
  store: KGCStore,
  gitBackbone: GitBackbone,
  operation: (ctx: { checkpoint: CheckpointReceipt }) => Promise<any>
): Promise<{ result: any; checkpoint: CheckpointReceipt }>;
```

#### getCheckpointHistory

Get all checkpoints sorted by time.

```typescript
function getCheckpointHistory(): CheckpointReceipt[];
```

#### clearCheckpointHistory

Clear checkpoint history (for testing).

```typescript
function clearCheckpointHistory(): void;
```

#### reconstructSession

Reconstruct state at specific time.

```typescript
function reconstructSession(
  store: KGCStore,
  gitBackbone: GitBackbone,
  targetTime: bigint
): Promise<KGCStore>;
```

#### calculateDrift

Calculate drift between actual and expected state.

```typescript
function calculateDrift(actualStore: KGCStore, expectedStore: KGCStore): number; // 0 = perfect, higher = more drift
```

---

## Autonomy Guard

Bounded autonomy with explicit budgets.

### Types

#### Budget

```typescript
interface Budget {
  maxDeltaSize: number; // Max delta operations
  maxToolOps: number; // Max tool calls
  maxFilesTouched: number; // Max unique files
  maxRewriteCost: number; // Max rewrite cost units
  epochDuration: bigint; // Epoch duration in nanoseconds
}
```

#### Usage

```typescript
interface Usage {
  deltaSize: number;
  toolOps: number;
  filesTouched: number;
  rewriteCost: number;
  epochStart: bigint;
}
```

#### DenialReceipt

```typescript
interface DenialReceipt {
  id: string;
  t_ns: bigint;
  timestamp_iso: string;
  reason:
    | 'budget_exceeded'
    | 'delta_too_large'
    | 'tools_exceeded'
    | 'files_exceeded'
    | 'invariant_violated'
    | 'shard_conflict';
  requested: {
    deltaSize?: number;
    toolOps?: number;
    filesTouched?: number;
    rewriteCost?: number;
  };
  budget: Budget;
  usage: Usage;
  receiptHash: string;
}
```

### Functions

#### createAutonomyGuard

Create an autonomy guard with specified budget.

```typescript
function createAutonomyGuard(budgetConfig?: Partial<Budget>): AutonomyGuard;
```

**Returns:** AutonomyGuard instance

#### AutonomyGuard

Instance returned by `createAutonomyGuard`.

**Methods:**

##### getBudget

```typescript
getBudget(): Budget
```

##### getUsage

```typescript
getUsage(): Usage
```

##### getRemaining

```typescript
getRemaining(): {
  deltaSize: number;
  toolOps: number;
  filesTouched: number;
  rewriteCost: number;
}
```

##### check

```typescript
check(request: {
  deltaSize?: number;
  toolOps?: number;
  files?: string[];
  rewriteCost?: number;
}): Promise<{ allowed: boolean; denialReceipt?: DenialReceipt }>
```

##### consume

```typescript
consume(consumption: {
  deltaSize?: number;
  toolOps?: number;
  files?: string[];
  rewriteCost?: number;
}): void
```

##### getDenialHistory

```typescript
getDenialHistory(): DenialReceipt[]
```

##### resetEpoch

```typescript
resetEpoch(): void
```

##### createScope

```typescript
createScope(scopedBudget: Partial<Budget>): AutonomyGuard
```

#### withGuard

Execute with guard protection.

```typescript
function withGuard(
  guard: AutonomyGuard,
  estimate: {
    deltaSize?: number;
    toolOps?: number;
    files?: string[];
    rewriteCost?: number;
  },
  operation: () => Promise<any>
): Promise<{ success: boolean; result?: any; denialReceipt?: DenialReceipt }>;
```

#### createProductionGuard

Create a default production guard.

```typescript
function createProductionGuard(): AutonomyGuard;
// maxDeltaSize: 500, maxToolOps: 100, maxFilesTouched: 50
```

#### createStrictGuard

Create a strict guard for high-risk operations.

```typescript
function createStrictGuard(): AutonomyGuard;
// maxDeltaSize: 50, maxToolOps: 10, maxFilesTouched: 5
```

---

## Shard Merge

Multi-agent concurrency without conflicts.

### Types

#### ShardScope

```typescript
interface ShardScope {
  files: string[]; // File path patterns (globs)
  graphs: string[]; // Graph URIs
  subjects: string[]; // Subject URI patterns
  predicates: string[]; // Predicate URI patterns
}
```

#### AgentShard

```typescript
interface AgentShard {
  id: string;
  agentId: string;
  scope: ShardScope;
  priority: number;
  createdAt: bigint;
  vectorClock: any;
}
```

#### MergeResult

```typescript
interface MergeResult {
  merged: AttributedDelta[];
  conflicts: Array<{
    delta1: AttributedDelta;
    delta2: AttributedDelta;
    resolution: 'delta1_wins' | 'delta2_wins' | 'both_rejected';
    reason: string;
  }>;
  receiptHash: string;
}
```

### Functions

#### createShard

Create a new agent shard.

```typescript
function createShard(
  agentId: string,
  scope?: Partial<ShardScope>,
  options?: { priority?: number }
): AgentShard;
```

#### addDelta

Add a delta to a shard.

```typescript
function addDelta(shardId: string, delta: Delta): void;
```

#### mergeDeltas

Merge deltas from multiple shards.

```typescript
function mergeDeltas(shardIds: string[]): Promise<MergeResult>;
```

#### applyMergedDeltas

Apply merged deltas to store.

```typescript
function applyMergedDeltas(
  store: KGCStore,
  deltas: AttributedDelta[]
): Promise<{ applied: number; failed: number; receiptHash: string }>;
```

#### checkShardOverlap

Check if two shards have overlapping scopes.

```typescript
function checkShardOverlap(
  shardId1: string,
  shardId2: string
): {
  hasOverlap: boolean;
  files: string[];
  graphs: string[];
  subjects: string[];
};
```

#### createMultiAgentSession

Create a multi-agent session.

```typescript
function createMultiAgentSession(agents: Array<{ agentId: string; shard: AgentShard }>): {
  shards: AgentShard[];
  merge: () => Promise<MergeResult>;
};
```

#### getPendingDeltas

Get pending deltas for a shard.

```typescript
function getPendingDeltas(shardId: string): AttributedDelta[];
```

#### clearPendingDeltas

Clear pending deltas (for testing).

```typescript
function clearPendingDeltas(shardId: string): void;
```

---

## Async Workflow

Long-running work as workflow graph inside O.

### Types

#### WorkItem

```typescript
interface WorkItem {
  id: string;
  type: string;
  payload: any;
  status: 'queued' | 'assigned' | 'executing' | 'completed' | 'failed' | 'cancelled';
  constraints: WorkItemConstraint;
  budget: WorkItemBudget;
  executorId?: string;
  createdAt: bigint;
  assignedAt?: bigint;
  completedAt?: bigint;
  retryCount: number;
  receipts: string[];
  result?: any;
  error?: string;
}
```

#### ExecutionReceipt

```typescript
interface ExecutionReceipt {
  id: string;
  workItemId: string;
  executorId: string;
  t_ns: bigint;
  timestamp_iso: string;
  status: 'started' | 'progress' | 'completed' | 'failed' | 'cancelled';
  progress?: number; // 0-100
  output?: any;
  error?: string;
  receiptHash: string;
  previousReceiptHash: string | null;
}
```

### Functions

#### enqueueWorkItem

Create and enqueue a new work item.

```typescript
function enqueueWorkItem(config: {
  type: string;
  payload: any;
  constraints?: Partial<WorkItemConstraint>;
  budget?: Partial<WorkItemBudget>;
}): WorkItem;
```

#### registerExecutor

Register an executor with capabilities.

```typescript
function registerExecutor(
  executorId: string,
  config: {
    capabilities: string[];
    busy: boolean;
  }
): void;
```

#### assignWorkItem

Assign work to executor.

```typescript
function assignWorkItem(workItemId: string, executorId: string): WorkItem;
```

#### startExecution

Start executing a work item.

```typescript
function startExecution(workItemId: string, executorId: string): Promise<ExecutionReceipt>;
```

#### reportProgress

Report progress on a work item.

```typescript
function reportProgress(
  workItemId: string,
  executorId: string,
  progress: number // 0-100
): Promise<ExecutionReceipt>;
```

#### completeWorkItem

Complete a work item.

```typescript
function completeWorkItem(
  workItemId: string,
  executorId: string,
  result: { success: boolean; output?: any }
): Promise<ExecutionReceipt>;
```

#### failWorkItem

Mark a work item as failed.

```typescript
function failWorkItem(
  workItemId: string,
  executorId: string,
  error: { error: string; retryable?: boolean }
): Promise<ExecutionReceipt>;
```

#### cancelWorkItem

Cancel a work item.

```typescript
function cancelWorkItem(
  workItemId: string,
  executorId: string,
  reason: { reason: string }
): Promise<ExecutionReceipt>;
```

#### getWorkItem

Get a work item by ID.

```typescript
function getWorkItem(workItemId: string): WorkItem | null;
```

#### getWorkItemReceipts

Get all receipts for a work item.

```typescript
function getWorkItemReceipts(workItemId: string): ExecutionReceipt[];
```

#### getQueuedItems

Get all queued work items.

```typescript
function getQueuedItems(): WorkItem[];
```

#### getExecutingItems

Get all executing work items.

```typescript
function getExecutingItems(): WorkItem[];
```

---

## Projection

Surface parity via projections.

### Types

#### ProjectionConfig

```typescript
interface ProjectionConfig {
  name: string;
  surface: 'cli' | 'ide' | 'ui' | 'api' | 'doc' | 'custom';
  query?: string; // SPARQL query
  sourceGraphs: string[];
  transform?: string; // Transform function name
  format: 'json' | 'markdown' | 'html' | 'text' | 'yaml' | 'custom';
}
```

#### ProjectionResult

```typescript
interface ProjectionResult {
  id: string;
  config: ProjectionConfig;
  content: any;
  contentHash: string;
  sourceHash: string;
  t_ns: bigint;
  timestamp_iso: string;
}
```

### Functions

#### registerProjection

Register a projection configuration.

```typescript
function registerProjection(config: ProjectionConfig): void;
```

#### registerTransform

Register a transform function.

```typescript
function registerTransform(name: string, fn: (data: any) => any): void;
```

#### project

Project universe state to a surface.

```typescript
function project(store: KGCStore, projection: string | ProjectionConfig): Promise<ProjectionResult>;
```

#### calculateProjectionDrift

Calculate drift between two projection results.

```typescript
function calculateProjectionDrift(result1: ProjectionResult, result2: ProjectionResult): number;
```

#### getProjections

Get all registered projections.

```typescript
function getProjections(): ProjectionConfig[];
```

#### getProjectionsForSurface

Get projections for a specific surface.

```typescript
function getProjectionsForSurface(
  surface: 'cli' | 'ide' | 'ui' | 'api' | 'doc' | 'custom'
): ProjectionConfig[];
```

#### clearProjections

Clear all registered projections (for testing).

```typescript
function clearProjections(): void;
```

---

## Substrate Factory

### createSubstrate

Create a complete KGC-Claude substrate instance.

```typescript
function createSubstrate(
  store: KGCStore,
  gitBackbone: GitBackbone,
  options?: {
    budget?: Partial<Budget>;
  }
): KGCClaudeSubstrate;
```

**Returns:** Substrate instance with integrated modules

#### KGCClaudeSubstrate

```typescript
interface KGCClaudeSubstrate {
  store: KGCStore;
  gitBackbone: GitBackbone;
  guard: AutonomyGuard;

  createRun(options?: RunOptions): RunCapsuleBuilder;
  persist(capsule: RunCapsule): Promise<{ receipt: Object }>;
  checkpoint(options?: CheckpointOptions): Promise<CheckpointReceipt>;
  restore(checkpointId: string): Promise<KGCStore>;
  withCheckpoint(operation: Function): Promise<{ result: any; checkpoint: CheckpointReceipt }>;
  createMultiAgentSession(agents: any[]): MultiAgentSession;
}
```

**Example:**

```javascript
import { createSubstrate } from '@unrdf/kgc-claude';
import { KGCStore, GitBackbone } from '@unrdf/kgc-4d';

const store = new KGCStore();
const git = new GitBackbone('/repo');
const substrate = createSubstrate(store, git);

const run = substrate.createRun();
run.addToolCall({ name: 'Read', input: {} });
const capsule = await run.seal();
await substrate.persist(capsule);
```

---

## See Also

- [Tutorial](./tutorial.md) - Step-by-step getting started guide
- [How-To Guides](./how-to/) - Task-oriented guides
- [Explanation](./explanation.md) - Conceptual documentation
