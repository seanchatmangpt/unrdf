# Tutorial: Your First Daemon

In this tutorial you will create a daemon, schedule an operation, execute it, and observe the results through events. By the end you will understand the core lifecycle and be ready to build more complex workflows.

**Time**: 15-20 minutes
**Prerequisite**: `pnpm add @unrdf/daemon`

---

## Part 1: Hello Daemon

Create `hello-daemon.mjs` with the simplest possible daemon:

```javascript
import { Daemon } from '@unrdf/daemon';

// 1. Create a daemon instance
const daemon = new Daemon({
  id: 'tutorial-daemon',
  logger: console,
});

// 2. Define an operation — a named async function
const operation = {
  id: 'greeting-op',
  name: 'Send Greeting',
  handler: async () => {
    return { message: 'Hello, daemon world!', timestamp: new Date() };
  },
};

// 3. Listen for the result before it arrives
daemon.on('operation:success', event => {
  console.log('Operation completed:', event.operationId, `(${event.duration}ms)`);
});

// 4. Start, schedule, execute, stop
await daemon.start();
daemon.schedule(operation);
const result = await daemon.execute('greeting-op');
console.log('Result:', result);
await daemon.stop();
```

Run it:

```bash
node hello-daemon.mjs
```

Expected output:

```
Operation completed: greeting-op (2ms)
Result: { message: 'Hello, daemon world!', timestamp: 2026-04-04T... }
```

**What happened:**

- `new Daemon()` creates the controller (extends EventEmitter)
- `start()` initializes the scheduler and emits `daemon:started`
- `schedule()` registers the operation and emits `operation:enqueued`
- `execute()` runs it immediately, emitting `operation:started` then `operation:success`
- `stop()` cleans up and emits `daemon:stopped`

---

## Part 2: Watch All Events

The daemon communicates entirely through events. Add handlers for the full lifecycle:

```javascript
import { Daemon } from '@unrdf/daemon';

const daemon = new Daemon({ id: 'event-demo' });

// Full event lifecycle
daemon.on('daemon:started', e => console.log('[daemon:started]', e.nodeId));
daemon.on('operation:enqueued', e => console.log('[enqueued]', e.name));
daemon.on('operation:started', e => console.log('[started]', e.name));
daemon.on('operation:success', e => console.log('[success]', e.name, e.duration + 'ms'));
daemon.on('operation:failure', e => console.log('[failure]', e.name, e.error));
daemon.on('daemon:stopped', e => console.log('[daemon:stopped]'));

const dataImport = {
  id: 'data-import',
  name: 'Import Data',
  handler: async () => {
    await new Promise(r => setTimeout(r, 100)); // simulate work
    return { imported: 1000, status: 'success' };
  },
};

await daemon.start();
daemon.schedule(dataImport);

try {
  const result = await daemon.execute('data-import');
  console.log('Imported:', result.imported, 'records');
} catch (e) {
  console.error('Failed:', e.message);
}

await daemon.stop();
```

The event sequence is always: `enqueued` → `started` → `success` or `failure`.

---

## Part 3: Check Health and Metrics

After running some operations, inspect the daemon's status:

```javascript
import { Daemon } from '@unrdf/daemon';

const daemon = new Daemon({ id: 'health-demo', logger: console });

// Three operations: one fast, one slow, one failing
const ops = [
  { id: 'fast', name: 'Fast Op', handler: async () => ({ ok: true }) },
  {
    id: 'slow',
    name: 'Slow Op',
    handler: async () => {
      await new Promise(r => setTimeout(r, 500));
      return { ok: true };
    },
  },
  {
    id: 'broken',
    name: 'Broken Op',
    handler: async () => {
      throw new Error('intentional');
    },
  },
];

await daemon.start();
ops.forEach(op => daemon.schedule(op));

for (const op of ops) {
  try {
    await daemon.execute(op.id);
  } catch {}
}

// Health: current operational state
const health = daemon.getHealth();
console.log('Running:', health.isRunning);
console.log('Uptime:', Math.round(health.uptime / 1000) + 's');
console.log('Active:', health.activeOperations);
console.log('Completed:', health.completedOperations);

// Metrics: accumulated performance data
const metrics = daemon.getMetrics();
console.log('Total ops:', metrics.totalOperations);
console.log('Success rate:', metrics.successRate.toFixed(1) + '%');
console.log('Avg duration:', metrics.averageDuration.toFixed(1) + 'ms');

await daemon.stop();
```

---

## Part 4: Schedule with Cron

Operations can be registered with cron triggers for time-based scheduling:

```javascript
import { Daemon } from '@unrdf/daemon';
import { evaluateTrigger } from '@unrdf/daemon/trigger-evaluator';

const daemon = new Daemon({ id: 'cron-demo' });

const dailyBackup = {
  id: 'daily-backup',
  name: 'Daily Database Backup',
  handler: async () => {
    return {
      status: 'completed',
      backupPath: `/backups/db-${new Date().toISOString()}.bak`,
    };
  },
};

// Check when a cron trigger would next fire
const cronTrigger = { type: 'cron', expression: '0 2 * * *', timezone: 'UTC' };
const evaluation = evaluateTrigger(cronTrigger, Date.now() - 86400000);
console.log('Should execute now:', evaluation.shouldExecute);
console.log('Next in ms:', evaluation.nextExecutionTime);

await daemon.start();
daemon.schedule(dailyBackup);

// Execute immediately for testing
const result = await daemon.execute('daily-backup');
console.log('Backup result:', result);

await daemon.stop();
```

Common cron patterns:

| Expression     | Meaning                  |
| -------------- | ------------------------ |
| `0 2 * * *`    | Daily at 2:00 AM         |
| `0 */6 * * *`  | Every 6 hours            |
| `*/15 * * * *` | Every 15 minutes         |
| `0 0 * * 0`    | Every Sunday at midnight |

---

## What You Learned

You now know the essential daemon patterns:

1. **Create and start** — `new Daemon(config)` then `await daemon.start()`
2. **Schedule operations** — `daemon.schedule({ id, handler })` with unique IDs
3. **Execute immediately** — `await daemon.execute(id)` returns the handler's result
4. **Listen to events** — `daemon.on('operation:success', handler)` for all lifecycle events
5. **Inspect state** — `daemon.getHealth()` and `daemon.getMetrics()` for observability
6. **Stop cleanly** — `await daemon.stop()` before process exit

## Next Steps

- [How to Schedule Operations](../how-to/01-schedule-operations.md) — detailed scheduling patterns
- [How to Enable OTel Tracing](../how-to/02-enable-otel-tracing.md) — distributed trace your daemon
- [Daemon API Reference](../reference/daemon-api.md) — complete method signatures
- [Daemon Architecture](../explanation/01-daemon-architecture.md) — understand the design
