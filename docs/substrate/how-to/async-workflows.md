# How to Build Async Workflows

Async workflows model long-running work as WorkItem nodes in the knowledge graph with explicit state transitions and receipt-based progress tracking.

## Problem

You need to:

- Queue async tasks for execution
- Track work item status and progress
- Assign work to executors based on capabilities
- Generate receipts for every workflow step
- Handle retries and failures
- Manage dependencies between work items

## Solution

Use the async workflow primitives to create WorkItems with constraints, budgets, and executor assignment.

## Step-by-Step

### 1. Enqueue a Work Item

```javascript
import { enqueueWorkItem } from '@unrdf/kgc-claude';

const workItem = enqueueWorkItem({
  type: 'file_edit',
  payload: {
    path: 'src/new-feature.ts',
    content: 'export function newFeature() { ... }',
  },
  constraints: {
    maxDuration: 60000000000n, // 60 seconds in nanoseconds
    requiredCapabilities: ['file_system', 'typescript'],
    dependencies: [], // Work item IDs that must complete first
    maxRetries: 3,
  },
  budget: {
    maxDeltaSize: 20,
    maxToolOps: 10,
    maxFilesTouched: 5,
  },
});

console.log('Work item enqueued:');
console.log('ID:', workItem.id);
console.log('Status:', workItem.status); // 'queued'
console.log('Created:', workItem.createdAt);
```

### 2. Register Executors

```javascript
import { registerExecutor } from '@unrdf/kgc-claude';

// Register executor with capabilities
registerExecutor('executor-1', {
  capabilities: ['file_system', 'typescript', 'testing'],
  busy: false,
});

registerExecutor('executor-2', {
  capabilities: ['file_system', 'javascript'],
  busy: false,
});

console.log('Executors registered');
```

### 3. Assign Work to Executor

```javascript
import { assignWorkItem } from '@unrdf/kgc-claude';

const assigned = assignWorkItem(workItem.id, 'executor-1');

console.log('Work assigned:');
console.log('Executor:', assigned.executorId);
console.log('Status:', assigned.status); // 'assigned'
console.log('Assigned at:', assigned.assignedAt);
```

### 4. Start Execution

```javascript
import { startExecution } from '@unrdf/kgc-claude';

const startReceipt = await startExecution(workItem.id, 'executor-1');

console.log('Execution started:');
console.log('Receipt ID:', startReceipt.id);
console.log('Status:', startReceipt.status); // 'started'
console.log('Receipt hash:', startReceipt.receiptHash);
```

### 5. Report Progress

```javascript
import { reportProgress } from '@unrdf/kgc-claude';

// Report 25% complete
await reportProgress(workItem.id, 'executor-1', 25);

// Report 50% complete
await reportProgress(workItem.id, 'executor-1', 50);

// Report 75% complete
const progressReceipt = await reportProgress(workItem.id, 'executor-1', 75);

console.log('Progress reported:');
console.log('Progress:', progressReceipt.progress); // 75
console.log('Receipt hash:', progressReceipt.receiptHash);
```

### 6. Complete Work Item

```javascript
import { completeWorkItem } from '@unrdf/kgc-claude';

const completeReceipt = await completeWorkItem(workItem.id, 'executor-1', {
  success: true,
  output: {
    path: 'src/new-feature.ts',
    linesAdded: 42,
    testsPassing: true,
  },
});

console.log('Work completed:');
console.log('Status:', completeReceipt.status); // 'completed'
console.log('Output:', completeReceipt.output);
console.log('Receipt hash:', completeReceipt.receiptHash);
```

### 7. Handle Failures

```javascript
import { failWorkItem } from '@unrdf/kgc-claude';

// If work fails
const failReceipt = await failWorkItem(workItem.id, 'executor-1', {
  error: 'Type checking failed',
  retryable: true,
});

console.log('Work failed:');
console.log('Error:', failReceipt.error);
console.log('Retry count:', failReceipt.retryCount);

// Item will retry if retryCount < maxRetries
```

### 8. Cancel Work Item

```javascript
import { cancelWorkItem } from '@unrdf/kgc-claude';

const cancelReceipt = await cancelWorkItem(workItem.id, 'executor-1', {
  reason: 'User cancelled operation',
});

console.log('Work cancelled:');
console.log('Status:', cancelReceipt.status); // 'cancelled'
```

### 9. Query Work Items

```javascript
import { getQueuedItems, getExecutingItems, getWorkItem } from '@unrdf/kgc-claude';

// Get all queued items
const queued = getQueuedItems();
console.log('Queued items:', queued.length);

// Get all executing items
const executing = getExecutingItems();
console.log('Executing items:', executing.length);

// Get specific work item
const item = getWorkItem(workItem.id);
console.log('Work item:', item.id);
console.log('Status:', item.status);
console.log('Receipts:', item.receipts.length);
```

### 10. Get Work Item Receipts

```javascript
import { getWorkItemReceipts } from '@unrdf/kgc-claude';

const receipts = getWorkItemReceipts(workItem.id);

console.log('Receipt chain:');
receipts.forEach((receipt, index) => {
  console.log(`${index + 1}. ${receipt.status} at ${receipt.timestamp_iso}`);
  console.log(`   Hash: ${receipt.receiptHash}`);
  console.log(`   Previous: ${receipt.previousReceiptHash || 'none'}`);
});
```

## Advanced Patterns

### Work Item with Dependencies

```javascript
// Create parent work
const parentWork = enqueueWorkItem({
  type: 'setup_environment',
  payload: { env: 'production' },
});

// Complete parent
await assignWorkItem(parentWork.id, 'executor-1');
await startExecution(parentWork.id, 'executor-1');
await completeWorkItem(parentWork.id, 'executor-1', { success: true });

// Create dependent work
const dependentWork = enqueueWorkItem({
  type: 'deploy_application',
  payload: { target: 'production' },
  constraints: {
    dependencies: [parentWork.id], // Must wait for parent
  },
});

console.log('Dependent work will wait for:', parentWork.id);
```

### Auto-Assignment Based on Capabilities

```javascript
function autoAssignWorkItem(workItemId) {
  const item = getWorkItem(workItemId);
  const required = item.constraints.requiredCapabilities;

  // Find suitable executor
  const executors = getAllExecutors(); // Hypothetical function

  const suitable = executors.find(executor => {
    const hasCapabilities = required.every(cap => executor.capabilities.includes(cap));
    return hasCapabilities && !executor.busy;
  });

  if (suitable) {
    assignWorkItem(workItemId, suitable.id);
    console.log(`Auto-assigned to ${suitable.id}`);
    return suitable.id;
  } else {
    console.log('No suitable executor available');
    return null;
  }
}

const executorId = autoAssignWorkItem(workItem.id);
```

### Retry with Exponential Backoff

```javascript
async function executeWithRetry(workItemId, executorId, maxRetries = 3) {
  let retryCount = 0;

  while (retryCount < maxRetries) {
    try {
      await startExecution(workItemId, executorId);

      // Simulate work
      await performWork(workItemId);

      await completeWorkItem(workItemId, executorId, { success: true });
      return { success: true };
    } catch (error) {
      retryCount++;

      await failWorkItem(workItemId, executorId, {
        error: error.message,
        retryable: retryCount < maxRetries,
      });

      if (retryCount < maxRetries) {
        // Exponential backoff: 1s, 2s, 4s, etc.
        const backoff = Math.pow(2, retryCount) * 1000;
        console.log(`Retry ${retryCount} after ${backoff}ms`);
        await new Promise(resolve => setTimeout(resolve, backoff));
      }
    }
  }

  return { success: false, error: 'Max retries exceeded' };
}
```

### Work Queue with Priority

```javascript
class PriorityWorkQueue {
  constructor() {
    this.queue = [];
  }

  enqueue(workItem, priority = 0) {
    this.queue.push({ workItem, priority });
    this.queue.sort((a, b) => b.priority - a.priority); // Higher first
  }

  dequeue() {
    return this.queue.shift()?.workItem;
  }

  peek() {
    return this.queue[0]?.workItem;
  }

  size() {
    return this.queue.length;
  }
}

const priorityQueue = new PriorityWorkQueue();

// Add high-priority work
const criticalWork = enqueueWorkItem({
  type: 'security_patch',
  payload: { vulnerability: 'CVE-2024-1234' },
});
priorityQueue.enqueue(criticalWork, 100);

// Add normal work
const normalWork = enqueueWorkItem({
  type: 'feature_update',
  payload: { feature: 'dark_mode' },
});
priorityQueue.enqueue(normalWork, 10);

// Process in order
while (priorityQueue.size() > 0) {
  const item = priorityQueue.dequeue();
  console.log('Processing:', item.type);
}
```

### Workflow Orchestrator

```javascript
class WorkflowOrchestrator {
  constructor() {
    this.workflows = new Map();
  }

  defineWorkflow(name, steps) {
    this.workflows.set(name, steps);
  }

  async executeWorkflow(name, initialPayload) {
    const steps = this.workflows.get(name);
    if (!steps) throw new Error(`Workflow not found: ${name}`);

    const results = [];
    let previousOutput = initialPayload;

    for (const step of steps) {
      console.log(`Executing step: ${step.name}`);

      const workItem = enqueueWorkItem({
        type: step.type,
        payload: previousOutput,
        constraints: step.constraints,
        budget: step.budget,
      });

      const executorId = await this.findExecutor(step.requiredCapabilities);
      assignWorkItem(workItem.id, executorId);

      await startExecution(workItem.id, executorId);

      const result = await this.executeStep(workItem.id, executorId, step.handler);

      await completeWorkItem(workItem.id, executorId, { output: result });

      results.push(result);
      previousOutput = result; // Chain output
    }

    return results;
  }

  async findExecutor(capabilities) {
    // Find suitable executor
    return 'executor-1'; // Simplified
  }

  async executeStep(workItemId, executorId, handler) {
    const item = getWorkItem(workItemId);
    return handler(item.payload);
  }
}

// Define workflow
const orchestrator = new WorkflowOrchestrator();

orchestrator.defineWorkflow('deploy', [
  {
    name: 'build',
    type: 'build_application',
    handler: async payload => ({ built: true, artifacts: ['app.js'] }),
  },
  {
    name: 'test',
    type: 'run_tests',
    handler: async payload => ({ tested: true, passing: 42 }),
  },
  {
    name: 'deploy',
    type: 'deploy_to_production',
    handler: async payload => ({ deployed: true, url: 'https://app.example.com' }),
  },
]);

// Execute
const results = await orchestrator.executeWorkflow('deploy', { version: '1.0.0' });
console.log('Workflow results:', results);
```

### Timeout Enforcement

```javascript
async function executeWithTimeout(workItemId, executorId, timeoutNs) {
  const startTime = process.hrtime.bigint();

  const startReceipt = await startExecution(workItemId, executorId);

  const checkTimeout = setInterval(() => {
    const elapsed = process.hrtime.bigint() - startTime;

    if (elapsed > timeoutNs) {
      clearInterval(checkTimeout);

      failWorkItem(workItemId, executorId, {
        error: 'Execution timeout exceeded',
        retryable: false,
      });

      console.error('Work item timed out:', workItemId);
    }
  }, 1000); // Check every second

  try {
    // Perform work
    await performWork(workItemId);

    clearInterval(checkTimeout);

    await completeWorkItem(workItemId, executorId, { success: true });
  } catch (error) {
    clearInterval(checkTimeout);
    throw error;
  }
}
```

## Best Practices

1. **Always define constraints**: maxDuration, requiredCapabilities prevent hangs
2. **Use budgets**: Limit resource consumption per work item
3. **Report progress**: Frequent updates enable monitoring
4. **Handle retries**: Transient failures are common
5. **Track dependencies**: Ensure correct execution order
6. **Generate receipts**: Every state transition needs a receipt
7. **Persist work items**: Store to database for durability

## Common Issues

**Issue**: Work item stuck in 'assigned' state

- **Cause**: Executor never called startExecution
- **Fix**: Implement executor heartbeat, auto-reassign stale items

**Issue**: Capability mismatch

- **Cause**: Executor doesn't have required capabilities
- **Fix**: Validate capabilities before assignment

**Issue**: Dependencies never satisfied

- **Cause**: Circular dependencies or failed parent
- **Fix**: Detect cycles, handle parent failures

**Issue**: Progress receipts lost

- **Cause**: Not persisting to store
- **Fix**: Call persistWorkItem after each state change

## See Also

- [API Reference: Async Workflow](../reference.md#async-workflow)
- [Explanation: Why Async Workflows](../explanation.md#async-workflows)
- [Tutorial: Step 10](../tutorial.md#step-10-async-workflow)
