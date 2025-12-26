# YAWL Quick Start Guide

Get started with YAWL workflows in 10 minutes.

## What is YAWL?

YAWL (Yet Another Workflow Language) is an enterprise workflow engine implementing Van der Aalst's workflow patterns. It provides:

- 20 control flow patterns (sequential, parallel, conditional, loops)
- Event sourcing with cryptographic receipts
- Time-travel queries and audit trails
- RDF-native storage with SPARQL queries
- Policy-driven execution with hooks

## Installation

```bash
pnpm add @unrdf/yawl @unrdf/oxigraph
```

## Your First Workflow (2 minutes)

Create a simple 3-task sequential workflow:

```javascript
import { createWorkflow, createCase, enableTask, startTask, completeTask } from '@unrdf/yawl';
import { createStore } from '@unrdf/oxigraph';

// 1. Create RDF store
const store = createStore();

// 2. Define workflow
const { workflow_id } = await createWorkflow(store, {
  id: 'my-first-workflow',
  name: 'Document Approval',
  tasks: [
    { id: 'draft', name: 'Create Draft', kind: 'atomic' },
    { id: 'review', name: 'Review', kind: 'atomic' },
    { id: 'publish', name: 'Publish', kind: 'atomic' },
  ],
  flow: [
    { from: 'draft', to: 'review' },
    { from: 'review', to: 'publish' },
  ],
});

// 3. Start workflow instance
const { case_id, enabled_tasks } = await createCase(store, {
  workflowId: workflow_id,
  caseId: 'case-001',
});

console.log('Enabled tasks:', enabled_tasks); // ['draft']

// 4. Execute first task
const { work_item_id } = await enableTask(store, {
  caseId: case_id,
  taskId: 'draft',
});

await startTask(store, {
  caseId: case_id,
  workItemId: work_item_id,
  actor: 'alice@example.com',
});

const receipt = await completeTask(store, {
  caseId: case_id,
  workItemId: work_item_id,
  outputData: { content: 'My draft...' },
});

console.log('Next enabled tasks:', receipt.enabled_tasks); // ['review']
```

**Run it:**

```bash
node my-first-workflow.mjs
```

## Core Concepts

### 1. Workflow Definition

A workflow is a template defining tasks and their flow:

```javascript
{
  id: 'workflow-id',           // Unique identifier
  name: 'Workflow Name',       // Human-readable name
  tasks: [                     // Task definitions
    { id: 'task1', name: 'Task 1', kind: 'atomic' },
    { id: 'task2', name: 'Task 2', kind: 'atomic' }
  ],
  flow: [                      // Control flow
    { from: 'task1', to: 'task2' }
  ]
}
```

### 2. Case (Workflow Instance)

A case is a running instance of a workflow:

```javascript
const { case_id } = await createCase(store, {
  workflowId: 'workflow-id',
  caseId: 'case-123',
  initialData: { key: 'value' }, // Optional data
});
```

### 3. Task Lifecycle

```
enabled → started → completed
```

**Enable task:**

```javascript
const { work_item_id } = await enableTask(store, {
  caseId: 'case-123',
  taskId: 'task1',
});
```

**Start task (claim it):**

```javascript
await startTask(store, {
  caseId: 'case-123',
  workItemId: work_item_id,
  actor: 'user@example.com',
});
```

**Complete task:**

```javascript
const receipt = await completeTask(store, {
  caseId: 'case-123',
  workItemId: work_item_id,
  outputData: { result: 'success' },
});
```

### 4. Receipts

Every operation returns a cryptographic receipt:

```javascript
{
  receipt_id: 'receipt-abc123',
  event_id: 'event-xyz789',
  hash: 'sha256:...',
  previous_hash: 'sha256:...',
  timestamp: '2024-12-25T10:30:00Z',
  decision: 'ACCEPT',
  justification: 'Task completed successfully',
  enabled_tasks: ['task2']
}
```

## Common Patterns

### Pattern 1: Sequential Workflow

Tasks execute one after another:

```javascript
{
  tasks: [
    { id: 'step1', name: 'Step 1', kind: 'atomic' },
    { id: 'step2', name: 'Step 2', kind: 'atomic' },
    { id: 'step3', name: 'Step 3', kind: 'atomic' }
  ],
  flow: [
    { from: 'step1', to: 'step2' },
    { from: 'step2', to: 'step3' }
  ]
}
```

### Pattern 2: Parallel Execution

Multiple tasks execute simultaneously:

```javascript
{
  tasks: [
    { id: 'start', name: 'Start', kind: 'atomic' },
    { id: 'parallel1', name: 'Parallel 1', kind: 'atomic' },
    { id: 'parallel2', name: 'Parallel 2', kind: 'atomic' },
    { id: 'end', name: 'End', kind: 'atomic' }
  ],
  flow: [
    // AND-split: enable both parallel tasks
    { from: 'start', to: 'parallel1', splitType: 'AND' },
    { from: 'start', to: 'parallel2', splitType: 'AND' },

    // AND-join: wait for both to complete
    { from: 'parallel1', to: 'end', joinType: 'AND' },
    { from: 'parallel2', to: 'end', joinType: 'AND' }
  ]
}
```

### Pattern 3: Conditional Routing

Choose path based on data:

```javascript
{
  tasks: [
    { id: 'check', name: 'Check Amount', kind: 'atomic' },
    { id: 'auto', name: 'Auto Approve', kind: 'atomic' },
    { id: 'manual', name: 'Manual Review', kind: 'atomic' },
    { id: 'done', name: 'Done', kind: 'atomic' }
  ],
  flow: [
    // XOR-split: choose one path
    {
      from: 'check',
      to: 'auto',
      splitType: 'XOR',
      condition: { amount_lt: 1000 }
    },
    {
      from: 'check',
      to: 'manual',
      splitType: 'XOR',
      condition: { amount_gte: 1000 }
    },

    // XOR-join: merge paths
    { from: 'auto', to: 'done', joinType: 'XOR' },
    { from: 'manual', to: 'done', joinType: 'XOR' }
  ]
}
```

## Error Handling

All functions return receipts with decision status:

```javascript
try {
  const receipt = await completeTask(store, {
    caseId: 'case-123',
    workItemId: 'wi-456',
    outputData: { result: 'success' },
  });

  if (receipt.decision === 'ACCEPT') {
    console.log('Success!', receipt.enabled_tasks);
  } else if (receipt.decision === 'REJECT') {
    console.error('Rejected:', receipt.justification);
  }
} catch (error) {
  console.error('Error:', error.message);
}
```

## Time Travel

Query workflow state at any point in time:

```javascript
import { replayCase } from '@unrdf/yawl';

// Replay entire case
const state = await replayCase(store, {
  caseId: 'case-123',
});

console.log('Status:', state.status);
console.log('Completed tasks:', state.completed_tasks);

// Time-travel to specific time
const historicalState = await replayCase(store, {
  caseId: 'case-123',
  asOfTime: '2024-12-25T10:00:00Z',
});

console.log('State at 10am:', historicalState);
```

## Next Steps

1. Run the [examples](../../examples/yawl/) to see patterns in action
2. Read the [YAWL Patterns Guide](./yawl-patterns.md) for all 20 patterns
3. Explore [Use Cases](./yawl-use-cases.md) for production scenarios
4. Check the [API Reference](../../packages/yawl/README.md) for full documentation

## Questions?

- See [UNRDF Documentation](https://github.com/unrdf/unrdf)
- Open an issue on GitHub
- Join the community discussion

---

**Next:** [YAWL Patterns Guide](./yawl-patterns.md) - Learn all 20 control flow patterns
