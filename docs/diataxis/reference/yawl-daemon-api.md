# Reference: YAWL Daemon API

Complete API reference for YAWL daemon integration.

---

## Table of Contents

- [Classes](#classes)
  - [Daemon](#daemon)
  - [YawlDaemonBridge](#yawldaemonbridge)
- [Configuration Schemas](#configuration-schemas)
- [Events](#events)
- [CLI Commands](#cli-commands)
- [Error Handling](#error-handling)
- [Type Definitions](#type-definitions)

---

## Classes

### Daemon

Main daemon controller for managing scheduled operations.

#### Constructor

```typescript
new Daemon(config: DaemonConfig)
```

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `config` | `Object` | Yes | Daemon configuration |
| `config.daemonId` | `string` | No | Unique daemon identifier (default: auto-generated) |
| `config.name` | `string` | No | Human-readable name (default: 'default-daemon') |
| `config.nodeId` | `string` | No | Node identifier in cluster (default: auto-generated) |
| `config.clusterId` | `string` | No | Cluster identifier (default: 'default-cluster') |
| `config.maxConcurrent` | `number` | No | Max concurrent operations (default: 5) |
| `config.logger` | `Object` | No | Custom logger instance (default: console) |

**Returns:** `Daemon` instance

**Throws:** `Error` if configuration is invalid

**Example:**

```javascript
import { Daemon } from '@unrdf/daemon';

const daemon = new Daemon({
  daemonId: 'my-daemon',
  name: 'My Production Daemon',
  maxConcurrent: 10,
  logger: console,
});
```

---

#### daemon.start()

Start the daemon and initialize scheduler.

```typescript
async start(): Promise<void>
```

**Returns:** `Promise<void>`

**Events Emitted:** `daemon:started`

**Example:**

```javascript
await daemon.start();
// Daemon is now running
```

---

#### daemon.stop()

Gracefully stop the daemon.

```typescript
async stop(): Promise<void>
```

**Returns:** `Promise<void>`

**Events Emitted:** `daemon:stopped`

**Example:**

```javascript
await daemon.stop();
// Daemon has stopped, resources cleaned up
```

---

#### daemon.schedule()

Schedule an operation for execution.

```typescript
schedule(operation: ScheduledOperation): void
```

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operation` | `Object` | Yes | Operation to schedule |
| `operation.id` | `string` | Yes | Unique operation identifier |
| `operation.name` | `string` | No | Human-readable operation name |
| `operation.handler` | `Function` | Yes | Async function to execute |
| `operation.metadata` | `Object` | No | Operation metadata |

**Throws:** `Error` if operation is invalid

**Events Emitted:** `operation:enqueued`

**Example:**

```javascript
daemon.schedule({
  id: 'backup-db',
  name: 'Database Backup',
  handler: async () => {
    // Backup logic
    return { status: 'success', backupSize: 1024 };
  },
  metadata: { priority: 'high' },
});
```

---

#### daemon.unschedule()

Remove a scheduled operation.

```typescript
unschedule(operationId: string): boolean
```

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operationId` | `string` | Yes | Operation identifier to remove |

**Returns:** `boolean` - `true` if found and removed, `false` otherwise

**Example:**

```javascript
const removed = daemon.unschedule('backup-db');
if (removed) {
  console.log('Operation unscheduled successfully');
}
```

---

#### daemon.execute()

Execute an operation immediately.

```typescript
async execute(operationId: string): Promise<any>
```

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `operationId` | `string` | Yes | Operation identifier to execute |

**Returns:** `Promise<any>` - Operation result

**Throws:** `Error` if operation not found or execution fails

**Events Emitted:** `operation:started`, `operation:success`, or `operation:failure`

**Example:**

```javascript
try {
  const result = await daemon.execute('backup-db');
  console.log('Backup completed:', result);
} catch (error) {
  console.error('Backup failed:', error.message);
}
```

---

#### daemon.listOperations()

List all scheduled operations.

```typescript
listOperations(): Array<OperationInfo>
```

**Returns:** `Array<Object>` - Array of operation info objects

**Example:**

```javascript
const operations = daemon.listOperations();
operations.forEach(op => {
  console.log(`${op.name}: ${op.status}`);
});
```

---

#### daemon.getHealth()

Get daemon health status.

```typescript
getHealth(): DaemonHealth
```

**Returns:** `Object` - Health information

**Example:**

```javascript
const health = daemon.getHealth();
console.log('Daemon running:', health.isRunning);
console.log('Uptime:', health.uptime);
console.log('Active operations:', health.activeOperations);
```

**Response Schema:**

```typescript
{
  nodeId: string;
  clusterId: string;
  isRunning: boolean;
  isLeader: boolean;
  uptime: number; // milliseconds
  activeOperations: number;
  queuedOperations: number;
  completedOperations: number;
  timestamp: Date;
}
```

---

#### daemon.getMetrics()

Get daemon performance metrics.

```typescript
getMetrics(): DaemonMetrics
```

**Returns:** `Object` - Performance metrics

**Example:**

```javascript
const metrics = daemon.getMetrics();
console.log('Success rate:', metrics.successRate);
console.log('Avg duration:', metrics.averageDuration);
```

**Response Schema:**

```typescript
{
  nodeId: string;
  totalOperations: number;
  successfulOperations: number;
  failedOperations: number;
  averageDuration: number; // milliseconds
  totalDuration: number; // milliseconds
  successRate: number; // percentage (0-100)
  timestamp: Date;
}
```

---

### YawlDaemonBridge

Bridges YAWL workflow engine and daemon scheduler.

#### Constructor

```typescript
new YawlDaemonBridge(daemon: Daemon, yawlEngine: YawlEngine, config: BridgeConfig)
```

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `daemon` | `Daemon` | Yes | Daemon instance |
| `yawlEngine` | `YawlEngine` | Yes | YAWL engine instance |
| `config` | `Object` | Yes | Bridge configuration |
| `config.bridgeId` | `string` | No | Bridge identifier (default: auto-generated) |
| `config.daemonNodeId` | `string` | Yes | Daemon node identifier |
| `config.maxConcurrentCases` | `number` | No | Max concurrent cases (default: 100) |
| `config.retryPolicy` | `Object` | No | Retry policy configuration |
| `config.timeoutDefaults` | `Object` | No | Timeout defaults |
| `config.enableAutoRetry` | `boolean` | No | Enable automatic retry (default: true) |
| `config.enableTimeoutTracking` | `boolean` | No | Enable timeout tracking (default: true) |
| `config.enableDistribution` | `boolean` | No | Enable task distribution (default: true) |
| `config.logger` | `Object` | No | Custom logger (default: console) |

**Returns:** `YawlDaemonBridge` instance

**Throws:** `Error` if configuration is invalid or daemon/engine are invalid

**Example:**

```javascript
import { YawlDaemonBridge } from '@unrdf/daemon/integrations/yawl';

const bridge = new YawlDaemonBridge(daemon, yawlEngine, {
  daemonNodeId: 'node-1',
  maxConcurrentCases: 100,
  enableAutoRetry: true,
  retryPolicy: {
    maxAttempts: 3,
    backoffMs: 1000,
  },
});
```

---

#### bridge.start()

Start the bridge and begin event listening.

```typescript
async start(): Promise<void>
```

**Returns:** `Promise<void>`

**Events Emitted:** `bridge:started`

**Example:**

```javascript
await bridge.start();
// Bridge is now active
```

---

#### bridge.stop()

Stop the bridge and clean up resources.

```typescript
async stop(): Promise<void>
```

**Returns:** `Promise<void>`

**Events Emitted:** `bridge:stopped`

**Example:**

```javascript
await bridge.stop();
// Bridge stopped, event listeners removed
```

---

#### bridge.scheduleRecurringCase()

Schedule recurring case creation.

```typescript
async scheduleRecurringCase(
  workflowId: string,
  schedule: string,
  params?: CaseParams
): Promise<ScheduleResult>
```

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `workflowId` | `string` | Yes | Workflow identifier |
| `schedule` | `string` | Yes | Cron expression or interval in ms |
| `params` | `Object` | No | Case creation parameters |
| `params.caseIdPrefix` | `string` | No | Prefix for auto-generated case IDs |
| `params.inputData` | `Object` | No | Input data for case |
| `params.priority` | `number` | No | Priority level (1-10) |

**Returns:** `Promise<Object>` - Schedule result with `operationId`, `workflowId`, `success`

**Throws:** `Error` if workflowId is invalid or scheduling fails

**Events Emitted:** `case:created-by-schedule` (when cases are created)

**Example:**

```javascript
// Daily at 2 AM
const result = await bridge.scheduleRecurringCase(
  'approval-workflow',
  '0 2 * * *',
  {
    caseIdPrefix: 'daily',
    priority: 5,
    inputData: { department: 'engineering' },
  }
);

console.log('Scheduled operation:', result.operationId);
```

---

#### bridge.watchTaskTimeout()

Watch a task for timeout and auto-cancel if exceeded.

```typescript
async watchTaskTimeout(
  caseId: string,
  taskId: string,
  timeoutMs: number
): Promise<WatchResult>
```

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `caseId` | `string` | Yes | Case identifier |
| `taskId` | `string` | Yes | Task identifier |
| `timeoutMs` | `number` | Yes | Timeout in milliseconds (min: 1000) |

**Returns:** `Promise<Object>` - Watch result with `operationId`, `caseId`, `taskId`, `timeoutMs`, `success`

**Throws:** `Error` if parameters are invalid

**Events Emitted:** `task:timeout-enforced` (when timeout is enforced)

**Example:**

```javascript
// Set 60 second timeout
const result = await bridge.watchTaskTimeout(
  'case-001',
  'review-task',
  60000
);

console.log('Watching timeout for:', result.taskId);
```

---

#### bridge.scheduleRetry()

Schedule automatic retry for failed task.

```typescript
async scheduleRetry(
  caseId: string,
  taskId: string,
  backoffPolicy?: RetryPolicy
): Promise<RetryResult>
```

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `caseId` | `string` | Yes | Case identifier |
| `taskId` | `string` | Yes | Task identifier |
| `backoffPolicy` | `Object` | No | Custom retry policy (defaults to config) |
| `backoffPolicy.maxAttempts` | `number` | No | Max retry attempts (default: 3) |
| `backoffPolicy.backoffMs` | `number` | No | Initial backoff in ms (default: 1000) |
| `backoffPolicy.backoffMultiplier` | `number` | No | Exponential multiplier (default: 2) |
| `backoffPolicy.maxBackoffMs` | `number` | No | Max backoff cap (default: 30000) |
| `backoffPolicy.jitterFactor` | `number` | No | Jitter factor 0-1 (default: 0.1) |

**Returns:** `Promise<Object>` - Retry schedule result

**Throws:** `Error` if parameters are invalid

**Events Emitted:** `task:retry-executed`, `task:retry-exhausted`

**Example:**

```javascript
// Schedule retry with custom policy
const result = await bridge.scheduleRetry('case-001', 'failed-task', {
  maxAttempts: 5,
  backoffMs: 2000,
  backoffMultiplier: 2,
});

console.log('Retry scheduled:', result.operationId);
```

---

#### bridge.waitForChoiceTrigger()

Wait for external trigger before proceeding with deferred choice.

```typescript
async waitForChoiceTrigger(
  caseId: string,
  taskId: string,
  triggerPattern: TriggerPattern
): Promise<any>
```

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `caseId` | `string` | Yes | Case identifier |
| `taskId` | `string` | Yes | Task identifier |
| `triggerPattern` | `Object` | Yes | Trigger condition pattern |
| `triggerPattern.eventName` | `string` | Yes | Event name to listen for |
| `triggerPattern.filter` | `Object` | No | Optional event filter |
| `triggerPattern.timeoutMs` | `number` | No | Max wait time before timeout |

**Returns:** `Promise<any>` - Resolves when trigger fires, rejects on timeout

**Throws:** `Error` if parameters are invalid or timeout occurs

**Example:**

```javascript
// Wait for user approval
const result = await bridge.waitForChoiceTrigger(
  'case-001',
  'approval-choice',
  {
    eventName: 'user:approved',
    filter: { userId: 'user-123' },
    timeoutMs: 3600000, // 1 hour
  }
);

console.log('Trigger received:', result);
```

**Manual Trigger Resolution:**

```javascript
// Later, manually resolve the trigger
const trigger = bridge.choiceTriggers.get('case-001:approval-choice');
if (trigger?.resolve) {
  trigger.resolve({
    eventName: 'user:approved',
    decision: 'APPROVE',
  });
}
```

---

#### bridge.distributeAndSplitTasks()

Distribute and split parallel tasks across daemon nodes.

```typescript
async distributeAndSplitTasks(
  caseId: string,
  taskIds: Array<string>,
  options?: DistributionOptions
): Promise<DistributionResult>
```

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `caseId` | `string` | Yes | Case identifier |
| `taskIds` | `Array<string>` | Yes | Task identifiers to distribute |
| `options` | `Object` | No | Distribution options |
| `options.strategy` | `string` | No | Distribution strategy (default: 'round-robin') |
| `options.nodeIds` | `Array<string>` | No | Target node IDs (optional) |

**Returns:** `Promise<Object>` - Distribution result

**Throws:** `Error` if parameters are invalid

**Events Emitted:** `tasks:distributed`

**Strategy Options:**
- `round-robin` - Tasks distributed sequentially across nodes
- `least-loaded` - Tasks assigned to node with fewest active operations
- `random` - Tasks randomly distributed
- `affinity` - Tasks grouped by affinity key

**Example:**

```javascript
// Distribute tasks with round-robin
const result = await bridge.distributeAndSplitTasks(
  'case-001',
  ['review-a', 'review-b', 'review-c'],
  { strategy: 'round-robin' }
);

console.log('Distribution ID:', result.distributionId);
console.log('Tasks distributed:', result.taskIds);
```

---

#### bridge.getStats()

Get bridge statistics and state.

```typescript
getStats(): BridgeStats
```

**Returns:** `Object` - Statistics object

**Example:**

```javascript
const stats = bridge.getStats();
console.log('Active timeouts:', stats.activeTimeouts);
console.log('Active retries:', stats.activeRetries);
```

**Response Schema:**

```typescript
{
  bridgeId: string;
  isRunning: boolean;
  caseSchedules: number;
  activeTimeouts: number;
  activeRetries: number;
  activeTriggers: number;
  distributions: number;
  timestamp: Date;
}
```

---

## Configuration Schemas

### DaemonConfigSchema

```javascript
import { DaemonConfigSchema } from '@unrdf/daemon';

const config = DaemonConfigSchema.parse({
  daemonId: 'my-daemon',
  name: 'My Daemon',
  // ... other options
});
```

**Schema:**

```typescript
{
  daemonId?: string;           // Default: auto-generated
  name?: string;               // Default: 'default-daemon'
  port?: number;               // Default: 8080 (range: 1024-65535)
  logLevel?: 'debug' | 'info' | 'warn' | 'error';  // Default: 'info'
  concurrency?: number;        // Default: 10 (max: 100)
  healthCheckIntervalMs?: number;  // Default: 30000
  metricsRetentionMs?: number;     // Default: 3600000
  operations?: Array<ScheduledOperation>;  // Default: []
  globalRetryPolicy?: RetryPolicy;
  environment?: Record<string, string>;  // Default: {}
}
```

---

### RetryPolicySchema

```javascript
import { RetryPolicySchema } from '@unrdf/daemon/integrations/yawl';

const policy = RetryPolicySchema.parse({
  maxAttempts: 3,
  backoffMs: 1000,
  // ... other options
});
```

**Schema:**

```typescript
{
  maxAttempts?: number;        // Default: 3 (range: 1-10)
  backoffMs?: number;          // Default: 1000 (range: 100-60000)
  backoffMultiplier?: number;  // Default: 2 (range: 1.1-10)
  maxBackoffMs?: number;       // Default: 30000 (range: 1000-300000)
  jitterFactor?: number;       // Default: 0.1 (range: 0-1)
}
```

---

### YawlTimeoutConfigSchema

```javascript
import { YawlTimeoutConfigSchema } from '@unrdf/daemon/integrations/yawl';

const timeouts = YawlTimeoutConfigSchema.parse({
  taskTimeoutMs: 60000,
  // ... other options
});
```

**Schema:**

```typescript
{
  taskTimeoutMs?: number;      // Default: 30000 (range: 1000-3600000)
  caseTimeoutMs?: number;      // Default: 3600000 (range: 5000-86400000)
  checkIntervalMs?: number;    // Default: 5000 (range: 100-30000)
}
```

---

### YawlDaemonBridgeConfigSchema

```javascript
import { YawlDaemonBridgeConfigSchema } from '@unrdf/daemon/integrations/yawl';

const config = YawlDaemonBridgeConfigSchema.parse({
  daemonNodeId: 'node-1',
  // ... other options
});
```

**Schema:**

```typescript
{
  bridgeId?: string;                 // Default: auto-generated
  daemonNodeId: string;              // Required
  maxConcurrentCases?: number;       // Default: 100 (range: 1-10000)
  retryPolicy?: RetryPolicy;         // Default: see RetryPolicySchema
  timeoutDefaults?: YawlTimeoutConfig;  // Default: see YawlTimeoutConfigSchema
  enableAutoRetry?: boolean;         // Default: true
  enableTimeoutTracking?: boolean;   // Default: true
  enableDistribution?: boolean;      // Default: true
  logger?: any;                      // Default: console
}
```

---

## Events

### Daemon Events

#### daemon:started

Emitted when daemon starts.

**Payload:**

```typescript
{
  nodeId: string;
  timestamp: Date;
}
```

**Example:**

```javascript
daemon.on('daemon:started', (event) => {
  console.log(`Daemon started: ${event.nodeId}`);
});
```

---

#### daemon:stopped

Emitted when daemon stops.

**Payload:**

```typescript
{
  nodeId: string;
  timestamp: Date;
}
```

---

#### operation:enqueued

Emitted when operation is added to queue.

**Payload:**

```typescript
{
  operationId: string;
  name: string;
  timestamp: Date;
}
```

---

#### operation:started

Emitted when operation execution starts.

**Payload:**

```typescript
{
  operationId: string;
  name: string;
  timestamp: Date;
}
```

---

#### operation:success

Emitted when operation completes successfully.

**Payload:**

```typescript
{
  operationId: string;
  name: string;
  duration: number; // milliseconds
  timestamp: Date;
}
```

---

#### operation:failure

Emitted when operation fails.

**Payload:**

```typescript
{
  operationId: string;
  name: string;
  error: string;
  duration: number; // milliseconds
  timestamp: Date;
}
```

---

### Bridge Events

#### bridge:started

Emitted when bridge starts.

**Payload:**

```typescript
{
  bridgeId: string;
  timestamp: Date;
}
```

---

#### bridge:stopped

Emitted when bridge stops.

**Payload:**

```typescript
{
  bridgeId: string;
  timestamp: Date;
}
```

---

#### case:created-by-schedule

Emitted when scheduled case is created.

**Payload:**

```typescript
{
  bridgeId: string;
  workflowId: string;
  caseId: string;
  result: any;
  timestamp: Date;
}
```

---

#### task:timeout-enforced

Emitted when task timeout is enforced.

**Payload:**

```typescript
{
  bridgeId: string;
  caseId: string;
  taskId: string;
  timeoutMs: number;
  timestamp: Date;
}
```

---

#### task:retry-executed

Emitted when retry is executed.

**Payload:**

```typescript
{
  bridgeId: string;
  caseId: string;
  taskId: string;
  attempt: number;
  result: any;
  timestamp: Date;
}
```

---

#### task:retry-exhausted

Emitted when retry attempts are exhausted.

**Payload:**

```typescript
{
  bridgeId: string;
  caseId: string;
  taskId: string;
  attempts: number;
  error: string;
  timestamp: Date;
}
```

---

#### tasks:distributed

Emitted when tasks are distributed.

**Payload:**

```typescript
{
  bridgeId: string;
  caseId: string;
  taskIds: Array<string>;
  strategy: string;
  results: Array<any>;
  timestamp: Date;
}
```

---

## CLI Commands

### unrdf daemon list

List all configured operations.

**Syntax:**

```bash
unrdf daemon list [options]
```

**Options:**

| Option | Type | Description |
|--------|------|-------------|
| `--json` | boolean | Output as JSON |
| `--include-metadata` | boolean | Include metadata in output |

**Example:**

```bash
unrdf daemon list --include-metadata
```

---

### unrdf daemon run

Execute operation immediately.

**Syntax:**

```bash
unrdf daemon run <operation> [options]
```

**Arguments:**

| Argument | Type | Required | Description |
|----------|------|----------|-------------|
| `operation` | string | Yes | Operation ID to execute |

**Options:**

| Option | Type | Description |
|--------|------|-------------|
| `--payload <json>` | string | Operation payload (JSON string) |
| `--json` | boolean | Output as JSON |
| `--timeout <ms>` | number | Execution timeout in milliseconds (default: 30000) |

**Example:**

```bash
unrdf daemon run backup-graphs --timeout 60000
```

---

### unrdf daemon schedule

Add scheduled trigger to operation.

**Syntax:**

```bash
unrdf daemon schedule <operation> <trigger> [options]
```

**Arguments:**

| Argument | Type | Required | Description |
|----------|------|----------|-------------|
| `operation` | string | Yes | Operation ID to schedule |
| `trigger` | string | Yes | Trigger type (cron, interval, reactive, event) |

**Options:**

| Option | Type | Description |
|--------|------|-------------|
| `--payload <json>` | string | Trigger payload (JSON string) |
| `--json` | boolean | Output as JSON |

**Example:**

```bash
unrdf daemon schedule sync-federation cron --payload '{"schedule":"0 * * * *"}'
```

---

### unrdf daemon status

Show daemon health and metrics.

**Syntax:**

```bash
unrdf daemon status [options]
```

**Options:**

| Option | Type | Description |
|--------|------|-------------|
| `--json` | boolean | Output as JSON |
| `--include-metrics` | boolean | Include detailed metrics |

**Example:**

```bash
unrdf daemon status --include-metrics
```

---

### unrdf daemon logs

View operation logs with filtering.

**Syntax:**

```bash
unrdf daemon logs [options]
```

**Options:**

| Option | Type | Description |
|--------|------|-------------|
| `--follow` | boolean | Follow log output (stream mode) |
| `--filter <pattern>` | string | Filter logs by pattern (regex) |
| `--max-lines <n>` | number | Maximum lines to display (default: 100) |
| `--json` | boolean | Output as JSON |

**Example:**

```bash
unrdf daemon logs --follow --filter "yawl"
```

---

### unrdf daemon config

Display current daemon configuration.

**Syntax:**

```bash
unrdf daemon config [options]
```

**Options:**

| Option | Type | Description |
|--------|------|-------------|
| `--json` | boolean | Output as JSON |

**Example:**

```bash
unrdf daemon config --json
```

---

### unrdf daemon cluster

Show Raft cluster status and members.

**Syntax:**

```bash
unrdf daemon cluster [options]
```

**Options:**

| Option | Type | Description |
|--------|------|-------------|
| `--json` | boolean | Output as JSON |
| `--include-metrics` | boolean | Include detailed member metrics |

**Example:**

```bash
unrdf daemon cluster --include-metrics
```

---

## Error Handling

### Common Errors

#### InvalidConfigurationError

Thrown when daemon or bridge configuration is invalid.

```javascript
try {
  const daemon = new Daemon({ maxConcurrent: -1 }); // Invalid
} catch (error) {
  if (error instanceof z.ZodError) {
    console.error('Invalid configuration:', error.errors);
  }
}
```

---

#### OperationNotFoundError

Thrown when trying to execute or unschedule a non-existent operation.

```javascript
try {
  await daemon.execute('non-existent-operation');
} catch (error) {
  if (error.message.includes('Operation not found')) {
    console.error('Operation does not exist');
  }
}
```

---

#### TimeoutError

Thrown when an operation or trigger times out.

```javascript
try {
  const result = await bridge.waitForChoiceTrigger(
    'case-001',
    'approval',
    { eventName: 'approval', timeoutMs: 1000 }
  );
} catch (error) {
  if (error.message.includes('timeout')) {
    console.error('Trigger timed out');
  }
}
```

---

### Error Handling Best Practices

1. **Always handle async errors:**

```javascript
daemon.on('operation:failure', (event) => {
  console.error(`Operation failed: ${event.error}`);
  // Implement retry logic or alerting
});
```

2. **Validate configuration with schemas:**

```javascript
import { DaemonConfigSchema } from '@unrdf/daemon';

const config = DaemonConfigSchema.safeParse(userConfig);
if (!config.success) {
  console.error('Configuration errors:', config.error.errors);
  // Handle validation errors
}
```

3. **Set reasonable timeouts:**

```javascript
// Set timeout for long-running operations
await daemon.execute('long-operation', { timeout: 300000 }); // 5 minutes
```

4. **Implement graceful shutdown:**

```javascript
process.on('SIGINT', async () => {
  console.log('Shutting down gracefully...');
  await bridge.stop();
  await daemon.stop();
  process.exit(0);
});
```

---

## Type Definitions

### ScheduledOperation

```typescript
interface ScheduledOperation {
  id: string;
  name?: string;
  handler: () => Promise<any>;
  metadata?: Record<string, any>;
}
```

---

### DaemonHealth

```typescript
interface DaemonHealth {
  nodeId: string;
  clusterId: string;
  isRunning: boolean;
  isLeader: boolean;
  uptime: number;
  activeOperations: number;
  queuedOperations: number;
  completedOperations: number;
  timestamp: Date;
}
```

---

### DaemonMetrics

```typescript
interface DaemonMetrics {
  nodeId: string;
  totalOperations: number;
  successfulOperations: number;
  failedOperations: number;
  averageDuration: number;
  totalDuration: number;
  successRate: number;
  timestamp: Date;
}
```

---

### BridgeStats

```typescript
interface BridgeStats {
  bridgeId: string;
  isRunning: boolean;
  caseSchedules: number;
  activeTimeouts: number;
  activeRetries: number;
  activeTriggers: number;
  distributions: number;
  timestamp: Date;
}
```

---

### RetryPolicy

```typescript
interface RetryPolicy {
  maxAttempts: number;
  backoffMs: number;
  backoffMultiplier: number;
  maxBackoffMs: number;
  jitterFactor: number;
}
```

---

### YawlTimeoutConfig

```typescript
interface YawlTimeoutConfig {
  taskTimeoutMs: number;
  caseTimeoutMs: number;
  checkIntervalMs: number;
}
```

---

## See Also

- [Tutorial: YAWL Daemon Setup](../tutorials/yawl-daemon-setup.md) - Complete setup guide
- [How-to: YAWL Daemon Management](../how-to/yawl-daemon-management.md) - Common management tasks
- [Example: YAWL Daemon Workflow](../../examples/yawl-daemon-workflow.mjs) - Production example
- [Daemon Package README](../../../packages/daemon/README.md) - Core daemon documentation
- [YAWL Package README](../../../packages/yawl/README.md) - YAWL workflow engine documentation
