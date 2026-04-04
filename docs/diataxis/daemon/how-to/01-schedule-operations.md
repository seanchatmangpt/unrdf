# How to Schedule Operations

**Goal**: Register operations with the daemon, execute them, monitor completion, and handle failures.

---

## Register and execute an operation

Every operation needs a unique `id` and an async `handler` function. Schedule it before calling `execute`.

```javascript
import { Daemon } from '@unrdf/daemon';

const daemon = new Daemon({ id: 'my-daemon' });

const sendReport = {
  id: 'send-report-op',
  name: 'Send Daily Report',
  handler: async () => {
    return {
      status: 'sent',
      recipients: ['team@example.com'],
      timestamp: new Date().toISOString(),
    };
  },
  metadata: {
    priority: 'high',
    retryable: true,
  },
};

await daemon.start();
daemon.schedule(sendReport);

const result = await daemon.execute('send-report-op');
console.log(result);
// { status: 'sent', recipients: [...], timestamp: '...' }

await daemon.stop();
```

`metadata` is optional context carried with the operation but not executed — use it for priority, tags, or business context.

---

## List scheduled operations

```javascript
const operations = daemon.listOperations();
// [
//   {
//     id: 'send-report-op',
//     name: 'Send Daily Report',
//     status: 'scheduled',
//     createdAt: Date,
//     metadata: { priority: 'high', retryable: true }
//   }
// ]
operations.forEach(op => console.log(op.name, op.status));
```

---

## Remove a scheduled operation

```javascript
const removed = daemon.unschedule('send-report-op');
console.log(removed); // true if found and removed, false otherwise
```

---

## React to completion events

Instead of awaiting `execute()` in a loop, listen to events for decoupled handling:

```javascript
daemon.on('operation:success', event => {
  console.log(`${event.name} succeeded in ${event.duration}ms`);
  // Chain next operation if needed
  if (event.operationId === 'send-report-op') {
    daemon.execute('archive-report-op').catch(console.error);
  }
});

daemon.on('operation:failure', event => {
  console.error(`${event.name} failed: ${event.error}`);
  // Alert, retry, or skip
});
```

Available events: `daemon:started`, `operation:enqueued`, `operation:started`, `operation:success`, `operation:failure`, `daemon:stopped`.

---

## Implement graceful shutdown

Handle `SIGTERM` and `SIGINT` to let in-flight operations finish:

```javascript
process.on('SIGTERM', async () => {
  daemon.shuttingDown = true;

  // Wait for active operations (up to 10 seconds)
  const deadline = Date.now() + 10_000;
  while (daemon.getHealth().activeOperations > 0 && Date.now() < deadline) {
    await new Promise(r => setTimeout(r, 100));
  }

  await daemon.stop();
  process.exit(0);
});
```

Set `daemon.shuttingDown = true` so handlers can check it and exit early.

---

## Run operations at intervals

Poll `execute()` on a timer, or combine with cron triggers via `evaluateTrigger`:

```javascript
import { evaluateTrigger } from '@unrdf/daemon/trigger-evaluator';

let lastRun = 0;

setInterval(async () => {
  const trigger = { type: 'interval', value: 5000 };
  const { shouldExecute } = evaluateTrigger(trigger, lastRun);
  if (shouldExecute) {
    lastRun = Date.now();
    await daemon.execute('health-check').catch(console.error);
  }
}, 1000);
```

Trigger types: `interval` (ms), `cron` (expression), `idle` (threshold ms), `reactive` (entity type), `event` (event name).

---

## Troubleshoot missing operations

If `execute()` throws `Operation not found`:

1. Confirm `schedule()` was called before `execute()`
2. Check the `id` strings match exactly (case-sensitive)
3. Verify `listOperations()` includes the operation

```javascript
const ops = daemon.listOperations();
const ids = ops.map(o => o.id);
console.log('Scheduled IDs:', ids);
```
