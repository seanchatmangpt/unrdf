# Tutorial: Your First Workflow

In this tutorial you will define a three-task sequential workflow, run a case through the engine, and observe the cryptographic receipt that YAWL generates for each task transition. By the end you will understand the core lifecycle and be ready to build more complex workflows.

**Time**: 20–30 minutes
**Prerequisite**: `pnpm add @unrdf/yawl`

---

## Part 1: Define a Workflow

A workflow is a directed graph of **tasks** connected by **flows**. The simplest possible graph is a linear sequence: task A leads to task B, which leads to task C.

Create `first-workflow.mjs`:

```javascript
import { WorkflowEngine, createWorkflow, sequence } from '@unrdf/yawl';

// 1. Define the workflow structure
const workflow = createWorkflow({
  id: 'order-processing',
  name: 'Order Processing',
  version: '1.0.0',
  tasks: [
    { id: 'receive-order', name: 'Receive Order' },
    { id: 'process-payment', name: 'Process Payment' },
    { id: 'ship-order', name: 'Ship Order' },
  ],
  flows: [sequence('receive-order', 'process-payment'), sequence('process-payment', 'ship-order')],
});

console.log('Workflow created:', workflow.id);
console.log(
  'Tasks:',
  workflow.getTasks().map(t => t.id)
);
console.log('Start task:', workflow.startTaskId);
```

Run it:

```bash
node first-workflow.mjs
```

Expected output:

```
Workflow created: order-processing
Tasks: [ 'receive-order', 'process-payment', 'ship-order' ]
Start task: receive-order
```

**What happened:**

- `createWorkflow()` validates the spec and builds the internal task index and control-flow graph
- `sequence('A', 'B')` creates a `FlowDef` object `{ from: 'A', to: 'B' }`
- YAWL infers the start task as the one with no incoming flows (`receive-order`)

---

## Part 2: Register with the Engine and Create a Case

The `WorkflowEngine` manages running cases. A **case** is one execution instance of a workflow — analogous to a process instance in BPM terms.

```javascript
import { WorkflowEngine, createWorkflow, sequence } from '@unrdf/yawl';

const workflow = createWorkflow({
  id: 'order-processing',
  name: 'Order Processing',
  tasks: [
    { id: 'receive-order', name: 'Receive Order' },
    { id: 'process-payment', name: 'Process Payment' },
    { id: 'ship-order', name: 'Ship Order' },
  ],
  flows: [sequence('receive-order', 'process-payment'), sequence('process-payment', 'ship-order')],
});

// 2. Create an engine and register the workflow
const engine = new WorkflowEngine({ nodeId: 'tutorial-node' });
engine.registerWorkflow(workflow);

// 3. Create a case (an instance of the workflow)
const { case: yawlCase, receipt: startReceipt } = await engine.createCase('order-processing', {
  orderId: 'ORD-001',
  customer: 'Alice',
  amount: 250,
});

console.log('Case created:', yawlCase.id);
console.log('Case status:', yawlCase.status);
console.log('Start receipt valid:', startReceipt.valid);

// 4. Inspect the initial work items
const enabledItems = yawlCase.getEnabledWorkItems();
console.log(
  'Enabled tasks:',
  enabledItems.map(w => w.taskDefId ?? w.taskId)
);
```

Expected output:

```
Case created: <uuid>
Case status: active
Start receipt valid: true
Enabled tasks: [ 'receive-order' ]
```

**What happened:**

- `engine.createCase()` creates a `YawlCase`, emits `case:created` and `case:started` events, and automatically enables the start task
- The engine returns a `receipt` — a BLAKE3-chained proof record for the `CASE_CREATED` event
- Only `receive-order` is enabled because the other tasks have unsatisfied incoming flows

---

## Part 3: Execute the Task Lifecycle

Each task progresses through: `disabled` → `enabled` → `active` → `completed`.

```javascript
import { WorkflowEngine, createWorkflow, sequence } from '@unrdf/yawl';

const workflow = createWorkflow({
  id: 'order-processing',
  name: 'Order Processing',
  tasks: [
    { id: 'receive-order', name: 'Receive Order' },
    { id: 'process-payment', name: 'Process Payment' },
    { id: 'ship-order', name: 'Ship Order' },
  ],
  flows: [sequence('receive-order', 'process-payment'), sequence('process-payment', 'ship-order')],
});

const engine = new WorkflowEngine({ nodeId: 'tutorial-node' });
engine.registerWorkflow(workflow);

const { case: yawlCase } = await engine.createCase('order-processing');

// --- Step 1: receive-order ---
let [workItem] = yawlCase.getEnabledWorkItems();
console.log('\n-- receive-order --');
console.log('Status before start:', workItem.status); // enabled

await engine.startTask(yawlCase.id, workItem.id);
console.log('Status after start:', workItem.status); // active

const { receipt: receiveReceipt, downstreamEnabled } = await engine.completeTask(
  yawlCase.id,
  workItem.id,
  { orderReceived: true }
);

console.log('Status after complete:', workItem.status); // completed
console.log(
  'Downstream enabled:',
  downstreamEnabled.map(d => d.taskId)
);
console.log('Receipt event type:', receiveReceipt.eventType);

// --- Step 2: process-payment ---
[workItem] = yawlCase.getEnabledWorkItems();
console.log('\n-- process-payment --');
await engine.startTask(yawlCase.id, workItem.id);
await engine.completeTask(yawlCase.id, workItem.id, { charged: 250 });

// --- Step 3: ship-order ---
[workItem] = yawlCase.getEnabledWorkItems();
console.log('\n-- ship-order --');
await engine.startTask(yawlCase.id, workItem.id);
await engine.completeTask(yawlCase.id, workItem.id, { trackingId: 'TRK-9999' });

console.log('\nCase status:', yawlCase.status); // completed
```

Expected output (abbreviated):

```
-- receive-order --
Status before start: enabled
Status after start: active
Status after complete: completed
Downstream enabled: [ 'process-payment' ]
Receipt event type: TASK_COMPLETED

-- process-payment --

-- ship-order --

Case status: completed
```

**What happened:**

- `startTask()` transitions the work item from `enabled` to `active`
- `completeTask()` transitions to `completed` and evaluates outgoing flows, enabling downstream tasks
- After the last task completes YAWL detects no more enabled tasks and marks the case `completed`
- Every `completeTask()` returns a receipt with a 64-character BLAKE3 hash chained to the previous receipt

---

## Part 4: Listen to Engine Events

The engine emits events for every lifecycle transition. Subscribe before creating a case:

```javascript
import { WorkflowEngine, createWorkflow, sequence, ENGINE_EVENTS } from '@unrdf/yawl';

const workflow = createWorkflow({
  id: 'order-processing',
  name: 'Order Processing',
  tasks: [
    { id: 'receive-order', name: 'Receive Order' },
    { id: 'process-payment', name: 'Process Payment' },
    { id: 'ship-order', name: 'Ship Order' },
  ],
  flows: [sequence('receive-order', 'process-payment'), sequence('process-payment', 'ship-order')],
});

const engine = new WorkflowEngine({ nodeId: 'event-demo' });

// Subscribe to lifecycle events
engine.on(ENGINE_EVENTS.CASE_CREATED, e => console.log('[case:created]', e.caseId));
engine.on(ENGINE_EVENTS.TASK_ENABLED, e => console.log('[task:enabled]', e.taskId));
engine.on(ENGINE_EVENTS.TASK_STARTED, e => console.log('[task:started]', e.workItemId));
engine.on(ENGINE_EVENTS.TASK_COMPLETED, e => console.log('[task:completed]', e.workItemId));
engine.on(ENGINE_EVENTS.CASE_COMPLETED, e => console.log('[case:completed]', e.caseId));

engine.registerWorkflow(workflow);
const { case: yawlCase } = await engine.createCase('order-processing');

for (const item of ['receive-order', 'process-payment', 'ship-order']) {
  const [workItem] = yawlCase.getEnabledWorkItems();
  await engine.startTask(yawlCase.id, workItem.id);
  await engine.completeTask(yawlCase.id, workItem.id, {});
}
```

Sample output:

```
[case:created] 3f2a...
[task:enabled] receive-order
[task:started] <workitem-uuid>
[task:completed] <workitem-uuid>
[task:enabled] process-payment
[task:started] <workitem-uuid>
[task:completed] <workitem-uuid>
[task:enabled] ship-order
[task:started] <workitem-uuid>
[task:completed] <workitem-uuid>
[case:completed] 3f2a...
```

All `ENGINE_EVENTS` keys: `CASE_CREATED`, `CASE_STARTED`, `CASE_COMPLETED`, `CASE_FAILED`, `CASE_CANCELLED`, `TASK_ENABLED`, `TASK_STARTED`, `TASK_COMPLETED`, `TASK_CANCELLED`, `TASK_FAILED`, `RESOURCE_ALLOCATED`, `RESOURCE_RELEASED`, `CHECKPOINT_CREATED`.

---

## What You Learned

You now know the fundamental YAWL lifecycle:

1. **Define** — `createWorkflow({ id, tasks, flows })` with `sequence()` builder
2. **Register** — `engine.registerWorkflow(workflow)` before creating cases
3. **Instantiate** — `engine.createCase(workflowId, initialData)` → `{ case, receipt }`
4. **Drive** — `engine.startTask(caseId, workItemId)` then `engine.completeTask(caseId, workItemId, output)`
5. **Query** — `yawlCase.getEnabledWorkItems()` to discover the next work
6. **Observe** — `engine.on(ENGINE_EVENTS.*)` for any lifecycle event

## Next Steps

- [Tutorial 02: Conditional Branching](./02-conditional-branching.md) — add XOR routing to your workflow
- [How-To: Handle Task Failure](../how-to/02-handle-task-failure.md) — fail, retry, and circuit-break tasks
- [Reference: Workflow Schema](../reference/workflow-schema.md) — full spec for task and flow definitions
