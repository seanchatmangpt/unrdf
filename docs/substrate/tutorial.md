# Tutorial: Getting Started with KGC-Claude Substrate

This tutorial will guide you through creating your first run capsule, checkpoint, and multi-agent workflow using the KGC-Claude substrate.

## Prerequisites

- Node.js >= 18.0.0
- pnpm >= 7.0.0
- Basic understanding of JavaScript/TypeScript
- Familiarity with async/await patterns

## Installation

```bash
# Install the package
pnpm add @unrdf/kgc-claude

# Peer dependencies
pnpm add @unrdf/core @unrdf/oxigraph @unrdf/kgc-4d @unrdf/yawl
```

## Step 1: Initialize the Substrate

First, let's set up the KGC store and Git backbone, then create a substrate instance:

```javascript
import { createSubstrate } from '@unrdf/kgc-claude';
import { KGCStore } from '@unrdf/kgc-4d';
import { GitBackbone } from '@unrdf/kgc-4d';

// Initialize storage
const store = new KGCStore();
const git = new GitBackbone('/path/to/your/repo');

// Create substrate with default production guard
const substrate = createSubstrate(store, git);

console.log('Substrate initialized!');
```

**What's happening?**

- `KGCStore`: In-memory RDF store for universe state
- `GitBackbone`: Git-based persistence for snapshots
- `createSubstrate`: Factory that wires up all substrate modules

## Step 2: Create Your First Run Capsule

A run capsule captures a single Claude execution with all tool calls and artifacts:

```javascript
// Create a new run
const run = substrate.createRun();

// Simulate a tool call
const readCallId = run.addToolCall({
  name: 'Read',
  input: { file: 'src/index.mjs' },
});

// Complete the tool call
run.completeToolCall(readCallId, {
  output: 'export function hello() { return "world"; }',
});

// Add an artifact (simulating file creation)
run.addArtifact({
  type: 'edit',
  path: 'src/index.mjs',
  contentHash: 'abc123def456',
});

// Add state changes
run.addDeltaO({
  type: 'modify',
  target: 'src/index.mjs',
  before: 'old content',
  after: 'new content',
});

console.log('Run capsule populated!');
```

**What's happening?**

- `createRun()`: Creates a builder for accumulating operations
- `addToolCall()`: Records a tool invocation with input
- `completeToolCall()`: Records tool output or error
- `addArtifact()`: Tracks files produced or modified
- `addDeltaO()`: Records changes to universe state (Δ_O)

## Step 3: Check Autonomy and Seal the Run

Before persisting, check if the run is within budget:

```javascript
// Get metrics from the run
const metrics = run.getMetrics();
console.log('Metrics:', metrics);
// { deltaSize: 1, toolOps: 1, filesTouched: 1 }

// Check against guard budget
const check = await substrate.guard.check({
  deltaSize: metrics.deltaSize,
  toolOps: metrics.toolOps,
  files: ['src/index.mjs'],
});

if (!check.allowed) {
  console.error('Run denied:', check.denialReceipt.reason);
  run.deny(check.denialReceipt.reason);
} else {
  console.log('Run allowed, sealing...');
}

// Seal the run (compute hash and finalize)
const capsule = await run.seal();

console.log('Run capsule sealed!');
console.log('Run ID:', capsule.id);
console.log('Run hash:', capsule.runHash);
console.log('Status:', capsule.status);
console.log('Admitted:', capsule.admitted);
```

**What's happening?**

- `getMetrics()`: Calculates resource usage
- `guard.check()`: Validates against budget limits
- `seal()`: Finalizes and computes BLAKE3 hash
- Sealed capsule is immutable and cryptographically signed

## Step 4: Persist the Run Capsule

Save the run to the KGC store:

```javascript
// Persist to store
const { receipt } = await substrate.persist(capsule);

console.log('Run persisted!');
console.log('Receipt:', receipt);
```

**What's happening?**

- `persist()`: Appends run to event log and universe
- Returns a receipt with event metadata

## Step 5: Create a Checkpoint

Checkpoints create snapshots for rollback:

```javascript
// Create a checkpoint after the run
const checkpoint = await substrate.checkpoint({
  runCapsuleIds: [capsule.id],
});

console.log('Checkpoint created!');
console.log('Checkpoint ID:', checkpoint.id);
console.log('Snapshot hash:', checkpoint.snapshotHash);
console.log('Git ref:', checkpoint.gitRef);
console.log('Universe size:', checkpoint.universeSize);
```

**What's happening?**

- `checkpoint()`: Freezes current universe state
- Creates Git commit with snapshot
- Returns cryptographic receipt with hash chain

## Step 6: Verify the Snapshot

Verify checkpoint integrity:

```javascript
import { verifyCheckpoint } from '@unrdf/kgc-claude';

const verification = await verifyCheckpoint(checkpoint, git);

if (verification.valid) {
  console.log('Checkpoint verified!');
} else {
  console.error('Verification failed:', verification.reason);
}
```

**What's happening?**

- `verifyCheckpoint()`: Recomputes snapshot hash
- Compares against stored receipt
- Validates hash chain to previous checkpoint

## Step 7: Execute with Checkpoint Protection

Use `withCheckpoint` for automatic rollback on error:

```javascript
try {
  const { result, checkpoint } = await substrate.withCheckpoint(async ctx => {
    // Do risky operations
    const run2 = substrate.createRun();
    run2.addToolCall({ name: 'Edit', input: { file: 'risky.mjs' } });

    const capsule2 = await run2.seal();
    await substrate.persist(capsule2);

    return { runCapsuleIds: [capsule2.id] };
  });

  console.log('Operation succeeded with checkpoint:', checkpoint.id);
} catch (error) {
  console.error('Operation failed, rolled back:', error.message);
}
```

**What's happening?**

- Creates checkpoint before operation
- Executes async function
- On success: creates second checkpoint
- On failure: restores to first checkpoint

## Step 8: Configure Custom Autonomy Guard

Create a guard with custom budget:

```javascript
import { createAutonomyGuard } from '@unrdf/kgc-claude';

const strictGuard = createAutonomyGuard({
  maxDeltaSize: 10,
  maxToolOps: 5,
  maxFilesTouched: 3,
  maxRewriteCost: 100,
  epochDuration: 1800000000000n, // 30 minutes
});

// Check remaining capacity
const remaining = strictGuard.getRemaining();
console.log('Remaining capacity:', remaining);

// Use the guard
const check = await strictGuard.check({
  deltaSize: 5,
  toolOps: 2,
  files: ['file1.mjs', 'file2.mjs'],
});

if (check.allowed) {
  // Consume budget
  strictGuard.consume({
    deltaSize: 5,
    toolOps: 2,
    files: ['file1.mjs', 'file2.mjs'],
  });

  console.log('Budget consumed');
  console.log('New remaining:', strictGuard.getRemaining());
}
```

**What's happening?**

- `createAutonomyGuard()`: Creates guard with limits
- `getRemaining()`: Shows available capacity
- `check()`: Validates proposed operation
- `consume()`: Deducts from budget

## Step 9: Multi-Agent Concurrency

Create shards for concurrent agents:

```javascript
import { createMultiAgentSession, createShard } from '@unrdf/kgc-claude';

// Create agent shards with scopes
const agent1Shard = createShard(
  'agent-1',
  {
    files: ['src/components/**'],
    graphs: ['http://kgc.io/Universe'],
  },
  { priority: 1 }
);

const agent2Shard = createShard(
  'agent-2',
  {
    files: ['src/services/**'],
    graphs: ['http://kgc.io/Universe'],
  },
  { priority: 1 }
);

// Create multi-agent session
const session = createMultiAgentSession([
  { agentId: 'agent-1', shard: agent1Shard },
  { agentId: 'agent-2', shard: agent2Shard },
]);

console.log('Multi-agent session created');
console.log('Shards:', session.shards.length);
```

**What's happening?**

- `createShard()`: Defines agent's operational scope
- `createMultiAgentSession()`: Coordinates multiple agents
- Shards prevent conflicts through scoping

## Step 10: Async Workflow

Enqueue and execute async work items:

```javascript
import {
  enqueueWorkItem,
  registerExecutor,
  assignWorkItem,
  startExecution,
  completeWorkItem,
} from '@unrdf/kgc-claude';

// Enqueue a work item
const workItem = enqueueWorkItem({
  type: 'file_edit',
  payload: { path: 'src/new-feature.mjs', content: '// TODO' },
  constraints: {
    maxDuration: 60000000000n, // 60 seconds
    requiredCapabilities: ['file_system'],
  },
  budget: {
    maxDeltaSize: 20,
    maxToolOps: 10,
    maxFilesTouched: 5,
  },
});

console.log('Work item enqueued:', workItem.id);

// Register an executor
registerExecutor('executor-1', {
  capabilities: ['file_system'],
  busy: false,
});

// Assign work to executor
const assigned = assignWorkItem(workItem.id, 'executor-1');
console.log('Assigned:', assigned);

// Start execution
const startReceipt = await startExecution(workItem.id, 'executor-1');
console.log('Execution started:', startReceipt.id);

// Report progress
await reportProgress(workItem.id, 'executor-1', 50);

// Complete work
const completeReceipt = await completeWorkItem(workItem.id, 'executor-1', {
  success: true,
  output: { path: 'src/new-feature.mjs', linesAdded: 42 },
});

console.log('Work completed:', completeReceipt.id);

// Get work item with receipts
const completedItem = getWorkItem(workItem.id);
console.log('Final status:', completedItem.status);
console.log('Receipts:', completedItem.receipts.length);
```

**What's happening?**

- `enqueueWorkItem()`: Creates async task in queue
- `registerExecutor()`: Declares executor capabilities
- `assignWorkItem()`: Allocates work to executor
- `startExecution()`: Begins work with receipt
- `completeWorkItem()`: Finalizes with result receipt

## Next Steps

Congratulations! You've learned the basics of the KGC-Claude substrate. Next:

1. **Explore [How-To Guides](./how-to/)** for production patterns
2. **Read [API Reference](./reference.md)** for detailed function documentation
3. **Study [Explanations](./explanation.md)** to understand design rationale
4. **Check out [Examples](../../examples/)** for real-world usage

## Troubleshooting

### Common Issues

**Issue**: `TypeError: Cannot read property 'check' of undefined`

- **Solution**: Ensure substrate is created with both store and git backbone

**Issue**: `DenialReceipt: budget_exceeded`

- **Solution**: Increase guard limits or scope work into smaller epochs

**Issue**: `Checkpoint verification failed: hash mismatch`

- **Solution**: Check Git repo integrity, ensure no manual edits to snapshots

**Issue**: `Shard conflict: overlapping scopes`

- **Solution**: Ensure agent shards have non-overlapping file/graph patterns

## Summary

You've learned how to:

- ✅ Initialize a KGC-Claude substrate
- ✅ Create and seal run capsules
- ✅ Check autonomy guards
- ✅ Persist runs to the store
- ✅ Create checkpoints for rollback
- ✅ Verify checkpoint integrity
- ✅ Configure custom guards
- ✅ Use multi-agent concurrency
- ✅ Build async workflows

The substrate provides deterministic, auditable, and scalable Claude integration!
