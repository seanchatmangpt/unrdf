# Daemon API Reference

Complete API for `@unrdf/daemon`. All classes extend Node.js `EventEmitter`.

---

## Daemon Class

### Constructor

```typescript
new Daemon(config: Object)
```

| Parameter                  | Type   | Default            | Description                             |
| -------------------------- | ------ | ------------------ | --------------------------------------- |
| `config.id`                | string | `'default-daemon'` | Unique daemon identifier                |
| `config.nodeId`            | string | auto-generated     | Node identifier in a cluster            |
| `config.clusterId`         | string | —                  | Cluster identifier                      |
| `config.maxConcurrent`     | number | `5`                | Max concurrent operations               |
| `config.port`              | number | `8080`             | MCP server port                         |
| `config.logger`            | Object | `console`          | Logger instance                         |
| `config.globalRetryPolicy` | Object | —                  | Default retry policy for all operations |
| `config.operations`        | Array  | `[]`               | Pre-registered operation definitions    |

Throws `Error` if configuration fails Zod schema validation.

```javascript
const daemon = new Daemon({
  id: 'worker-daemon',
  nodeId: 'node-1',
  clusterId: 'production-cluster',
  maxConcurrent: 10,
  logger: customLogger,
});
```

---

## Lifecycle Methods

### start()

```typescript
async start(): Promise<void>
```

Initialize the daemon and scheduler. Emits `daemon:started`.

```javascript
await daemon.start();
// Daemon now accepts scheduled operations
```

### stop()

```typescript
async stop(): Promise<void>
```

Gracefully stop the daemon. Waits for pending operations when possible. Emits `daemon:stopped`.

```javascript
await daemon.stop();
```

---

## Operation Methods

### schedule(operation)

```typescript
schedule(operation: Object): void
```

Register an operation for execution.

| Parameter            | Type     | Required | Description                        |
| -------------------- | -------- | -------- | ---------------------------------- |
| `operation.id`       | string   | yes      | Unique identifier                  |
| `operation.name`     | string   | no       | Human-readable name                |
| `operation.handler`  | Function | yes      | Async function returning any value |
| `operation.metadata` | Object   | no       | Contextual data (not executed)     |

Emits `operation:enqueued`. Throws if `id` or `handler` is missing.

```javascript
daemon.schedule({
  id: 'backup-db',
  name: 'Database Backup',
  handler: async () => ({ status: 'completed', size: '2.5GB' }),
  metadata: { priority: 'high', retention: '30d' },
});
```

### unschedule(operationId)

```typescript
unschedule(operationId: string): boolean
```

Remove a scheduled operation. Returns `true` if found and removed, `false` otherwise.

```javascript
daemon.unschedule('backup-db'); // true
```

### listOperations()

```typescript
listOperations(): Array<Object>
```

Returns all currently scheduled operations. Each entry has: `id`, `name`, `status`, `createdAt` (Date), `metadata`.

```javascript
const ops = daemon.listOperations();
ops.forEach(op => console.log(op.name, op.status));
```

### execute(operationId)

```typescript
async execute(operationId: string): Promise<any>
```

Execute a scheduled operation immediately. Returns the handler's resolved value. Emits `operation:started` then `operation:success` or `operation:failure`.

Throws `Error` if operation is not found or handler throws.

```javascript
const result = await daemon.execute('backup-db');
console.log(result.status); // 'completed'
```

---

## Status Methods

### getHealth()

```typescript
getHealth(): Object
```

Current operational state snapshot.

| Field                 | Type    | Description                  |
| --------------------- | ------- | ---------------------------- |
| `nodeId`              | string  | This node's identifier       |
| `clusterId`           | string  | Cluster identifier           |
| `isRunning`           | boolean | Whether daemon is active     |
| `isLeader`            | boolean | Leadership status in cluster |
| `uptime`              | number  | Running duration (ms)        |
| `activeOperations`    | number  | Currently executing          |
| `queuedOperations`    | number  | Waiting in queue             |
| `completedOperations` | number  | Total completed since start  |
| `timestamp`           | Date    | Snapshot time                |

### getMetrics()

```typescript
getMetrics(): Object
```

Accumulated performance metrics.

| Field                  | Type   | Description               |
| ---------------------- | ------ | ------------------------- |
| `nodeId`               | string | Node identifier           |
| `totalOperations`      | number | All executed              |
| `successfulOperations` | number | Success count             |
| `failedOperations`     | number | Failure count             |
| `averageDuration`      | number | Mean execution time (ms)  |
| `totalDuration`        | number | Sum of all durations (ms) |
| `successRate`          | number | Percentage (0–100)        |
| `timestamp`            | Date   | Metrics snapshot time     |

---

## Events

All events are standard Node.js `EventEmitter` events.

| Event                | Emitted when                          | Event object fields                                                   |
| -------------------- | ------------------------------------- | --------------------------------------------------------------------- |
| `daemon:started`     | Daemon initialization complete        | `nodeId`, `timestamp`                                                 |
| `daemon:stopped`     | Daemon shut down                      | `nodeId`, `timestamp`                                                 |
| `operation:enqueued` | Operation registered via `schedule()` | `operationId`, `name`, `timestamp`                                    |
| `operation:started`  | Handler invocation begins             | `operationId`, `name`, `timestamp`                                    |
| `operation:success`  | Handler resolved                      | `operationId`, `name`, `duration` (ms), `timestamp`                   |
| `operation:failure`  | Handler rejected                      | `operationId`, `name`, `error` (string), `duration` (ms), `timestamp` |

---

## TriggerEvaluator

### evaluateTrigger(trigger, lastExecuted)

```typescript
evaluateTrigger(trigger: Object, lastExecuted: number): Object
```

Evaluate whether a trigger should fire now.

```javascript
import { evaluateTrigger } from '@unrdf/daemon/trigger-evaluator';

const result = evaluateTrigger(
  { type: 'interval', value: 5000 },
  Date.now() - 6000 // last ran 6 seconds ago
);
// { shouldExecute: true, nextExecutionTime: 0 }
```

**Trigger types:**

| Type       | `value` field | Meaning                                     |
| ---------- | ------------- | ------------------------------------------- |
| `interval` | number (ms)   | Fire every N milliseconds                   |
| `cron`     | string        | Standard cron expression                    |
| `idle`     | number (ms)   | Fire when daemon idle longer than threshold |
| `reactive` | string        | Fire when entity type changes               |
| `event`    | string        | Fire when named event is emitted            |

**Returns:** `{ shouldExecute: boolean, nextExecutionTime: number }` where `nextExecutionTime` is ms until next execution (0 means now).

---

## Zod Schemas

All schemas are exported from `@unrdf/daemon` for validation.

| Schema                     | Validates                                    |
| -------------------------- | -------------------------------------------- |
| `DaemonConfigSchema`       | Constructor `config` object                  |
| `ScheduledOperationSchema` | Full operation definition including triggers |
| `OperationReceiptSchema`   | Execution record with cryptographic proof    |
| `DaemonHealthSchema`       | `getHealth()` return value                   |
| `DaemonMetricsSchema`      | `getMetrics()` return value                  |
| `RetryPolicySchema`        | Exponential backoff configuration            |
| `TriggerSchema`            | Discriminated union of all trigger types     |

**RetryPolicy fields:** `maxAttempts`, `backoffMs`, `backoffMultiplier`, `maxBackoffMs`, `jitterFactor`.

---

## Error Codes

| Code                  | Meaning                            |
| --------------------- | ---------------------------------- |
| `OPERATION_NOT_FOUND` | `execute()` called with unknown ID |
| `OPERATION_FAILED`    | Handler threw an error             |
| `INVALID_CONFIG`      | Config failed schema validation    |
| `TIMEOUT`             | Operation exceeded time limit      |
