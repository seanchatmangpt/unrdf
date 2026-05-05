# How-To: Orchestrate with Daemon

`@unrdf/daemon` and `@unrdf/yawl` are designed to work together: the daemon schedules operations, and those operations drive YAWL workflow cases. This guide shows the integration pattern.

---

## The pattern

```
Daemon operation handler
  └── engine.createCase(workflowId, data)
        └── drives tasks through their lifecycle
              └── emits ENGINE_EVENTS for monitoring
```

The daemon handles the **scheduling concern** (when to run, cron, retries). YAWL handles the **workflow concern** (what to run and in what order).

---

## Set up the engine and workflow once

Create the engine and register workflows at startup, outside any operation handler:

```javascript
import { Daemon } from '@unrdf/daemon';
import { WorkflowEngine, createWorkflow, sequence, ENGINE_EVENTS } from '@unrdf/yawl';

// ── 1. Define your workflow ──────────────────────────────────────────────────
const nightly = createWorkflow({
  id: 'nightly-report',
  name: 'Nightly Report Generation',
  tasks: [
    { id: 'extract', name: 'Extract Data' },
    { id: 'transform', name: 'Transform Data' },
    { id: 'load', name: 'Load to Warehouse' },
    { id: 'notify', name: 'Send Email Report' },
  ],
  flows: [
    sequence('extract', 'transform'),
    sequence('transform', 'load'),
    sequence('load', 'notify'),
  ],
});

// ── 2. Create the engine ─────────────────────────────────────────────────────
const engine = new WorkflowEngine({ nodeId: 'etl-node' });
engine.registerWorkflow(nightly);

// Subscribe globally for observability
engine.on(ENGINE_EVENTS.CASE_CREATED, e => console.log('[yawl]', 'case created', e.caseId));
engine.on(ENGINE_EVENTS.TASK_COMPLETED, e => console.log('[yawl]', 'task done', e.workItemId));
engine.on(ENGINE_EVENTS.CASE_COMPLETED, e => console.log('[yawl]', 'case done', e.caseId));
engine.on(ENGINE_EVENTS.TASK_FAILED, e => console.error('[yawl]', 'task failed', e.error));
```

---

## Create a daemon operation that drives the workflow

The operation handler creates a case and runs each task to completion. For automated tasks, the handler itself is the worker:

```javascript
// ── 3. Define the daemon operation ──────────────────────────────────────────
const etlOperation = {
  id: 'run-nightly-etl',
  name: 'Nightly ETL Pipeline',
  handler: async () => {
    const runDate = new Date().toISOString().slice(0, 10);

    // Create a fresh case for tonight's run
    const { case: yawlCase } = await engine.createCase('nightly-report', {
      runDate,
      environment: process.env.NODE_ENV ?? 'production',
    });

    // Drive each task to completion in sequence
    const results = {};

    // -- extract --
    let [wi] = yawlCase.getEnabledWorkItems();
    await engine.startTask(yawlCase.id, wi.id);
    const extractResult = await doExtract(runDate);
    await engine.completeTask(yawlCase.id, wi.id, extractResult);
    results.extract = extractResult;

    // -- transform --
    [wi] = yawlCase.getEnabledWorkItems();
    await engine.startTask(yawlCase.id, wi.id);
    const transformResult = await doTransform(extractResult.records);
    await engine.completeTask(yawlCase.id, wi.id, transformResult);
    results.transform = transformResult;

    // -- load --
    [wi] = yawlCase.getEnabledWorkItems();
    await engine.startTask(yawlCase.id, wi.id);
    const loadResult = await doLoad(transformResult.transformed);
    await engine.completeTask(yawlCase.id, wi.id, loadResult);
    results.load = loadResult;

    // -- notify --
    [wi] = yawlCase.getEnabledWorkItems();
    await engine.startTask(yawlCase.id, wi.id);
    await engine.completeTask(yawlCase.id, wi.id, { sent: true });

    return {
      caseId: yawlCase.id,
      runDate,
      receiptsCount: yawlCase.receipts?.length ?? 0,
      ...results,
    };
  },
};

// Placeholder implementations — replace with real logic
async function doExtract(date) {
  return { records: 1500 };
}
async function doTransform(records) {
  return { transformed: records };
}
async function doLoad(transformed) {
  return { rowsLoaded: transformed };
}
```

---

## Schedule with cron and start the daemon

```javascript
// ── 4. Wire the daemon ───────────────────────────────────────────────────────
const daemon = new Daemon({
  id: 'etl-daemon',
  logger: console,
});

daemon.on('operation:success', e => {
  console.log('[daemon] ETL complete', e.operationId, `(${e.duration}ms)`);
});
daemon.on('operation:failure', e => {
  console.error('[daemon] ETL failed', e.error);
});

await daemon.start();
daemon.schedule(etlOperation);

// Run immediately for testing; in production use a cron trigger
const result = await daemon.execute('run-nightly-etl');
console.log('Run result:', result);

await daemon.stop();
```

In production, remove the manual `daemon.execute()` call and configure a cron trigger in the daemon schedule config to run at `0 2 * * *` (daily at 2 AM).

---

## Handle errors so the daemon can retry

If any YAWL task throws, propagate the error out of the operation handler. The daemon marks the operation as failed and can apply retry logic:

```javascript
handler: async () => {
  const { case: yawlCase } = await engine.createCase('nightly-report', { runDate: today });

  try {
    // ... drive tasks ...
  } catch (err) {
    // Fail the active work item before re-throwing
    const active = yawlCase.getEnabledWorkItems().filter(w => w.status === 'active');
    for (const wi of active) {
      await engine.failTask(yawlCase.id, wi.id, err);
    }
    throw err; // daemon sees this as operation:failure
  }
};
```

---

## Monitor both layers

Subscribe to both daemon events and YAWL engine events for a complete picture:

| Layer  | Event               | Meaning                        |
| ------ | ------------------- | ------------------------------ |
| Daemon | `operation:started` | Operation handler began        |
| YAWL   | `case:created`      | New workflow instance created  |
| YAWL   | `task:enabled`      | Next task is ready             |
| YAWL   | `task:completed`    | A task finished                |
| YAWL   | `case:completed`    | All tasks done                 |
| Daemon | `operation:success` | Handler returned without error |
| Daemon | `operation:failure` | Handler threw                  |

---

## See also

- [Daemon Tutorial 01: Your First Daemon](../../daemon/tutorials/01-first-daemon.md)
- [Reference: WorkflowEngine](../reference/workflow-schema.md)
- [Reference: ENGINE_EVENTS](../reference/task-api.md#engine-events)
