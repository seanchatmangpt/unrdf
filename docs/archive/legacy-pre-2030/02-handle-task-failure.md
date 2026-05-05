# How-To: Handle Task Failure

YAWL provides several layers of failure handling: per-task failure transitions, case-level cancellation, and the circuit-breaker pattern for recurring faults. This guide covers each.

---

## Mark a task as failed

Call `engine.failTask()` on any active work item. The work item moves to `failed` (a terminal state) and the engine emits `task:failed`:

```javascript
import { WorkflowEngine, createWorkflow, sequence, ENGINE_EVENTS } from '@unrdf/yawl';

const workflow = createWorkflow({
  id: 'fragile-workflow',
  name: 'Fragile Workflow',
  tasks: [
    { id: 'fetch', name: 'Fetch Data' },
    { id: 'process', name: 'Process Data' },
    { id: 'store', name: 'Store Results' },
  ],
  flows: [sequence('fetch', 'process'), sequence('process', 'store')],
});

const engine = new WorkflowEngine({ nodeId: 'failure-demo' });
engine.registerWorkflow(workflow);

engine.on(ENGINE_EVENTS.TASK_FAILED, e => {
  console.error('[task:failed]', e.taskId, '-', e.error);
});

const { case: yawlCase } = await engine.createCase('fragile-workflow');

const [fetchItem] = yawlCase.getEnabledWorkItems();
await engine.startTask(yawlCase.id, fetchItem.id);

// Simulate a failure
await engine.failTask(yawlCase.id, fetchItem.id, new Error('Network timeout'));

console.log('Work item status:', fetchItem.status); // failed
console.log('Case status:', yawlCase.status); // failed
```

A failed case does not enable further tasks. The case status moves to `failed`.

---

## Cancel a specific work item

Cancel a single enabled or active work item without failing the whole case. Downstream tasks of that work item are not enabled:

```javascript
const { case: yawlCase } = await engine.createCase('fragile-workflow');

const [fetchItem] = yawlCase.getEnabledWorkItems();
// Cancel before starting
await engine.cancelCase(yawlCase.id, fetchItem.id);

console.log('Work item status:', fetchItem.status); // cancelled
```

---

## Handle timeout

Set a `timeout` (milliseconds) on a task definition. The engine emits `task:timeout` and transitions the work item to `timeout`:

```javascript
const timeoutWorkflow = createWorkflow({
  id: 'timeout-demo',
  name: 'Timeout Demo',
  tasks: [
    { id: 'slow-step', name: 'Slow Step', timeout: 2000 }, // 2s limit
  ],
  flows: [],
});

engine.registerWorkflow(timeoutWorkflow);
engine.on(ENGINE_EVENTS.TIMER_EXPIRED, e => {
  console.warn('[timer:expired]', e.workItemId);
});

const { case: timeoutCase } = await engine.createCase('timeout-demo');
const [item] = timeoutCase.getEnabledWorkItems();
await engine.startTask(timeoutCase.id, item.id);

// If completeTask is not called within 2s, the engine transitions to timeout
```

Valid terminal statuses for a work item: `completed`, `cancelled`, `failed`, `timeout`.

---

## Use the circuit breaker

The `WorkflowEngine` implements a circuit-breaker pattern per `workflowId:taskId` key. After `circuitBreakerThreshold` consecutive failures (default: 5), further calls to `enableTask` for that task throw immediately:

```javascript
const engine = new WorkflowEngine({
  nodeId: 'cb-demo',
  circuitBreakerThreshold: 3, // open after 3 failures
  circuitBreakerResetTimeout: 10000, // try again after 10s
});

engine.on(ENGINE_EVENTS.CIRCUIT_BREAKER_OPEN, e => {
  console.warn('[circuit:open]', e.taskId);
});
engine.on(ENGINE_EVENTS.CIRCUIT_BREAKER_CLOSE, e => {
  console.log('[circuit:close]', e.taskId);
});
```

The breaker resets automatically after `circuitBreakerResetTimeout` ms, or immediately on a successful task completion.

---

## Recover from case failure with a snapshot

If you enabled snapshots (`enableSnapshots: true`, the default), you can restore the case to a known-good checkpoint:

```javascript
const engine = new WorkflowEngine({
  nodeId: 'recovery-demo',
  enableSnapshots: true,
});
engine.registerWorkflow(workflow);

const { case: yawlCase } = await engine.createCase('fragile-workflow');

// Create a checkpoint after the first task succeeds
const [fetchItem] = yawlCase.getEnabledWorkItems();
await engine.startTask(yawlCase.id, fetchItem.id);
await engine.completeTask(yawlCase.id, fetchItem.id, { data: 'fetched' });

await engine.createSnapshot(yawlCase.id); // manual checkpoint

// Later, a task fails
const [processItem] = yawlCase.getEnabledWorkItems();
await engine.startTask(yawlCase.id, processItem.id);
await engine.failTask(yawlCase.id, processItem.id, new Error('Parse error'));

// Restore to the last checkpoint
const restored = await engine.restoreFromSnapshot(yawlCase.id);
console.log('Restored case status:', restored.status); // active, back to checkpoint
```

---

## See also

- [Reference: Task API](../reference/task-api.md) — `TaskStatus` enum and `VALID_TRANSITIONS`
- [Reference: Workflow Engine](../reference/workflow-schema.md) — `EngineConfigSchema` options
- [Explanation: YAWL Pattern Language](../explanation/01-yawl-pattern-language.md) — cancellation region semantics
