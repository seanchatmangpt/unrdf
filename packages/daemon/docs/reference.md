# API Reference: @unrdf/daemon

Complete API documentation for @unrdf/daemon.

## Daemon Class

Main daemon controller extending EventEmitter.

### Constructor

```typescript
new Daemon(config: Object)
```

**Parameters:**
- `config.id` (string, default: 'default-daemon') - Unique daemon identifier
- `config.nodeId` (string, optional) - Node identifier in cluster
- `config.clusterId` (string, optional) - Cluster identifier
- `config.maxConcurrent` (number, default: 5) - Max concurrent operations
- `config.logger` (Object, optional) - Logger instance (defaults to console)

**Throws:**
- `Error` - If configuration is invalid

**Example:**
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

### lifecycle Methods

#### start()

```typescript
async start(): Promise<void>
```

Start the daemon and initialize the scheduler. Emits `daemon:started` event.

**Returns:** Promise that resolves when daemon is started

**Example:**
```javascript
await daemon.start();
// Daemon now accepts scheduled operations
```

#### stop()

```typescript
async stop(): Promise<void>
```

Gracefully stop the daemon. Emits `daemon:stopped` event. Waits for pending operations if needed.

**Returns:** Promise that resolves when daemon is stopped

**Example:**
```javascript
await daemon.stop();
// All operations complete before returning
```

---

### Operation Methods

#### schedule(operation)

```typescript
schedule(operation: Object): void
```

Register an operation for execution.

**Parameters:**
- `operation.id` (string, required) - Unique operation identifier
- `operation.name` (string, optional) - Human-readable name
- `operation.handler` (Function, required) - Async operation function
- `operation.metadata` (Object, optional) - Operation context data

**Throws:**
- `Error` - If operation is invalid (missing id or handler)

**Emits:**
- `operation:enqueued` event

**Example:**
```javascript
daemon.schedule({
  id: 'backup-db',
  name: 'Database Backup',
  handler: async () => {
    // Your async work here
    return { status: 'completed', size: '2.5GB' };
  },
  metadata: { priority: 'high', retention: '30d' },
});
```

#### unschedule(operationId)

```typescript
unschedule(operationId: string): boolean
```

Remove a scheduled operation.

**Parameters:**
- `operationId` (string) - Operation identifier to remove

**Returns:**
- `boolean` - True if operation was found and removed, false otherwise

**Example:**
```javascript
const removed = daemon.unschedule('backup-db');
if (removed) {
  console.log('Operation removed');
}
```

#### listOperations()

```typescript
listOperations(): Array<Object>
```

Get all currently scheduled operations.

**Returns:**
- `Array` of operation objects with properties:
  - `id` (string) - Operation identifier
  - `name` (string) - Operation name
  - `status` (string) - Current status ('scheduled', 'running', etc.)
  - `createdAt` (Date) - When operation was scheduled
  - `metadata` (Object) - Operation context data

**Example:**
```javascript
const operations = daemon.listOperations();
operations.forEach(op => {
  console.log(`${op.name} (${op.status})`);
});
```

#### execute(operationId)

```typescript
async execute(operationId: string): Promise<any>
```

Execute an operation immediately.

**Parameters:**
- `operationId` (string) - Operation identifier to execute

**Returns:**
- `Promise` resolving with operation result

**Throws:**
- `Error` - If operation not found or execution fails

**Emits:**
- `operation:started` event
- `operation:success` or `operation:failure` event

**Example:**
```javascript
try {
  const result = await daemon.execute('backup-db');
  console.log('Success:', result);
} catch (e) {
  console.error('Failed:', e.message);
}
```

---

### Status Methods

#### getHealth()

```typescript
getHealth(): Object
```

Get current daemon health status.

**Returns:**
- `Object` with properties:
  - `nodeId` (string) - This node identifier
  - `clusterId` (string) - Cluster identifier
  - `isRunning` (boolean) - Daemon active status
  - `isLeader` (boolean) - Leadership status in cluster
  - `uptime` (number) - Running duration (milliseconds)
  - `activeOperations` (number) - Currently executing
  - `queuedOperations` (number) - Waiting in queue
  - `completedOperations` (number) - Total completed
  - `timestamp` (Date) - Status snapshot time

**Example:**
```javascript
const health = daemon.getHealth();
console.log(`Status: ${health.isRunning ? 'healthy' : 'stopped'}`);
console.log(`Uptime: ${health.uptime}ms`);
console.log(`Active: ${health.activeOperations}`);
```

#### getMetrics()

```typescript
getMetrics(): Object
```

Get daemon performance metrics.

**Returns:**
- `Object` with properties:
  - `nodeId` (string) - Node identifier
  - `totalOperations` (number) - All executed
  - `successfulOperations` (number) - Successful count
  - `failedOperations` (number) - Failed count
  - `averageDuration` (number) - Mean execution time (ms)
  - `totalDuration` (number) - Sum of all durations (ms)
  - `successRate` (number) - Success percentage (0-100)
  - `timestamp` (Date) - Metrics snapshot time

**Example:**
```javascript
const metrics = daemon.getMetrics();
console.log(`Success Rate: ${metrics.successRate.toFixed(1)}%`);
console.log(`Avg Duration: ${metrics.averageDuration.toFixed(2)}ms`);
```

---

## Events

All events are standard Node.js EventEmitter events.

### daemon:started

Emitted when daemon starts successfully.

**Event Object:**
- `nodeId` (string) - Node identifier
- `timestamp` (Date) - When event occurred

**Example:**
```javascript
daemon.on('daemon:started', (event) => {
  console.log(`Daemon started: ${event.nodeId}`);
});
```

### daemon:stopped

Emitted when daemon stops.

**Event Object:**
- `nodeId` (string) - Node identifier
- `timestamp` (Date) - When event occurred

**Example:**
```javascript
daemon.on('daemon:stopped', (event) => {
  console.log('Daemon stopped');
});
```

### operation:enqueued

Emitted when operation is scheduled.

**Event Object:**
- `operationId` (string) - Operation identifier
- `name` (string) - Operation name
- `timestamp` (Date) - When queued

**Example:**
```javascript
daemon.on('operation:enqueued', (event) => {
  console.log(`Queued: ${event.name}`);
});
```

### operation:started

Emitted when operation execution begins.

**Event Object:**
- `operationId` (string) - Operation identifier
- `name` (string) - Operation name
- `timestamp` (Date) - When started

**Example:**
```javascript
daemon.on('operation:started', (event) => {
  console.log(`Started: ${event.name}`);
});
```

### operation:success

Emitted when operation completes successfully.

**Event Object:**
- `operationId` (string) - Operation identifier
- `name` (string) - Operation name
- `duration` (number) - Execution time (milliseconds)
- `timestamp` (Date) - When completed

**Example:**
```javascript
daemon.on('operation:success', (event) => {
  console.log(`Success: ${event.name} (${event.duration}ms)`);
});
```

### operation:failure

Emitted when operation fails with error.

**Event Object:**
- `operationId` (string) - Operation identifier
- `name` (string) - Operation name
- `error` (string) - Error message
- `duration` (number) - Execution time (milliseconds)
- `timestamp` (Date) - When failed

**Example:**
```javascript
daemon.on('operation:failure', (event) => {
  console.error(`Failed: ${event.name} - ${event.error}`);
});
```

---

## TriggerEvaluator

Evaluate trigger execution based on schedules.

### evaluateTrigger(trigger, lastExecuted)

```typescript
evaluateTrigger(trigger: Object, lastExecuted: number): Object
```

Evaluate whether a trigger should execute now.

**Parameters:**
- `trigger.type` (string) - Trigger type: 'interval', 'cron', 'idle', 'reactive', 'event'
- `trigger.value` (string|number) - Type-specific value:
  - interval: milliseconds (number)
  - cron: expression (string)
  - idle: threshold ms (number)
  - reactive: entity type (string)
  - event: event name (string)
- `lastExecuted` (number) - Last execution timestamp (ms since epoch)

**Returns:**
- `Object` with:
  - `shouldExecute` (boolean) - Whether to execute now
  - `nextExecutionTime` (number) - Milliseconds until next execution

**Example:**
```javascript
// Interval trigger
const result = evaluateTrigger(
  { type: 'interval', value: 5000 },
  Date.now() - 6000  // Was last executed 6 seconds ago
);
// Returns: { shouldExecute: true, nextExecutionTime: 0 }

// Cron trigger
const cronResult = evaluateTrigger(
  { type: 'cron', value: '0 * * * *' }, // Every hour
  Date.now()
);
// Returns: { shouldExecute: false, nextExecutionTime: 1800000 }
```

---

## Schemas (Zod Validation)

All entities are validated using Zod schemas.

### DaemonConfigSchema

Validates daemon configuration.

```javascript
import { DaemonConfigSchema } from '@unrdf/daemon';

const config = DaemonConfigSchema.parse({
  id: 'my-daemon',
  maxConcurrent: 10,
  logger: console,
});
```

**Validated Fields:**
- `id` (string, required) - Unique identifier
- `nodeId` (string, optional)
- `clusterId` (string, optional)
- `maxConcurrent` (number, default: 5)
- `logger` (Object, optional)

### RetryPolicySchema

Configures exponential backoff retry strategy.

```javascript
const policy = {
  maxAttempts: 3,
  backoffMs: 1000,
  backoffMultiplier: 2,
  maxBackoffMs: 30000,
  jitterFactor: 0.1,
};
```

### TriggerSchema

Validates trigger configurations (discriminated union).

```javascript
// Interval trigger
{ type: 'interval', intervalMs: 5000 }

// Cron trigger
{ type: 'cron', expression: '0 * * * *', timezone: 'UTC' }

// Idle trigger
{ type: 'idle', idleThresholdMs: 60000 }

// Reactive trigger
{ type: 'reactive', entityType: 'User', operation: 'create' }

// Event trigger
{ type: 'event', eventName: 'user:signup', filter: { role: 'admin' } }
```

### ScheduledOperationSchema

Validates complete scheduled operation definition.

```javascript
const operation = {
  id: 'uuid-here',
  name: 'My Operation',
  description: 'Does important work',
  triggers: [
    { type: 'interval', intervalMs: 5000 }
  ],
  action: {
    type: 'process',
    payload: { /* custom data */ }
  },
  retryPolicy: { maxAttempts: 3 },
  enabled: true,
  metadata: { /* custom */ },
};
```

### OperationReceiptSchema

Records execution lifecycle with cryptographic proof.

```javascript
const receipt = {
  id: 'uuid',
  operationId: 'uuid',
  operationType: 'backup',
  status: 'success', // pending, running, success, failed, skipped
  startedAt: new Date(),
  completedAt: new Date(),
  duration: 1234,
  result: { /* operation result */ },
  attempts: 1,
  proof: {
    hash: 'sha256-hash',
    timestamp: new Date(),
    signature: 'optional-signature',
  },
};
```

---

## Error Handling

Operations may throw errors that are caught and reported.

**Common Error Codes:**
- `OPERATION_NOT_FOUND` - Operation with ID not scheduled
- `OPERATION_FAILED` - Handler threw error
- `INVALID_CONFIG` - Configuration failed schema validation
- `TIMEOUT` - Operation exceeded time limit

**Example:**
```javascript
daemon.on('operation:failure', (event) => {
  if (event.error.includes('OPERATION_NOT_FOUND')) {
    // Handle missing operation
  } else if (event.error.includes('timeout')) {
    // Handle timeout
  } else {
    // Generic failure
  }
});
```

---

## Type Safety

While @unrdf/daemon uses JSDoc for type hints, all runtime validation uses Zod schemas.

**Using with TypeScript:**
```typescript
import { Daemon } from '@unrdf/daemon';
import type { Daemon as DaemonType } from '@unrdf/daemon';

const daemon: DaemonType = new Daemon({ id: 'my-daemon' });
```

For full type definitions, refer to JSDoc comments in source files.
