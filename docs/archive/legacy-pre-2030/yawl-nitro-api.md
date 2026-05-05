# YAWL-Nitro API Reference

**Version**: 1.0.0
**Package**: `@unrdf/daemon/integrations/nitro-tasks`
**Last Updated**: 2026-01-11

Complete API documentation for YAWL-Nitro integration.

---

## Table of Contents

1. [Classes](#classes)
2. [Factory Functions](#factory-functions)
3. [Events](#events)
4. [Type Definitions](#type-definitions)
5. [Configuration](#configuration)
6. [Error Handling](#error-handling)
7. [Performance Characteristics](#performance-characteristics)

---

## Classes

### NitroTaskExecutor

Main class for bridging Daemon operations with Nitro tasks.

#### Constructor

```javascript
new NitroTaskExecutor(daemon, config)
```

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `daemon` | `Daemon` | Yes | Daemon instance |
| `config` | `NitroExecutorConfig` | No | Executor configuration |

**Returns:** `NitroTaskExecutor` instance

**Throws:**
- `TypeError` - If daemon is not a valid Daemon instance
- `ValidationError` - If config is invalid

**Example:**

```javascript
import { Daemon } from '@unrdf/daemon';
import { NitroTaskExecutor } from '@unrdf/daemon/integrations/nitro-tasks';

const daemon = new Daemon({ daemonId: crypto.randomUUID(), name: 'my-daemon' });
const executor = new NitroTaskExecutor(daemon, {
  timeout: 30000,
  maxRetries: 3,
});
```

---

#### Properties

##### `id`

```javascript
executor.id: string
```

Unique executor instance ID (UUID v4).

**Example:**

```javascript
console.log(executor.id); // "550e8400-e29b-41d4-a716-446655440000"
```

---

##### `daemon`

```javascript
executor.daemon: Daemon
```

Reference to the underlying Daemon instance.

**Example:**

```javascript
const health = executor.daemon.getHealth();
```

---

##### `config`

```javascript
executor.config: NitroExecutorConfig
```

Executor configuration object (read-only).

**Example:**

```javascript
console.log(executor.config.timeout); // 30000
console.log(executor.config.maxRetries); // 3
```

---

##### `daemonToNitroMap`

```javascript
executor.daemonToNitroMap: Map<string, string>
```

Mapping from Daemon operation IDs to Nitro task IDs.

**Example:**

```javascript
const nitroTaskId = executor.daemonToNitroMap.get(operationId);
console.log(nitroTaskId); // "daemon:my-task"
```

---

##### `nitroToDaemonMap`

```javascript
executor.nitroToDaemonMap: Map<string, string>
```

Reverse mapping from Nitro task IDs to Daemon operation IDs.

**Example:**

```javascript
const operationId = executor.nitroToDaemonMap.get('daemon:my-task');
```

---

##### `taskMetadata`

```javascript
executor.taskMetadata: Map<string, TaskMetadata>
```

Metadata for each registered task (indexed by Daemon operation ID).

**Example:**

```javascript
const metadata = executor.taskMetadata.get(operationId);
console.log(metadata.description); // "My task description"
console.log(metadata.cronExpression); // "0 * * * *"
```

---

#### Methods

##### `start()`

Start the executor and underlying daemon.

```javascript
await executor.start(): Promise<void>
```

**Returns:** Promise that resolves when started

**Throws:**
- `Error` - If daemon fails to start

**Emits:** `executor:started` event

**Example:**

```javascript
await executor.start();
console.log('Executor started');
```

---

##### `stop()`

Stop the executor and underlying daemon.

```javascript
await executor.stop(): Promise<void>
```

**Returns:** Promise that resolves when stopped

**Emits:** `executor:stopped` event

**Example:**

```javascript
await executor.stop();
console.log('Executor stopped');
```

---

##### `registerOperationAsTask()`

Register a Daemon operation as a Nitro task.

```javascript
executor.registerOperationAsTask(
  operationId: string,
  taskType: string,
  metadata?: TaskMetadata
): TaskRegistrationResult
```

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `operationId` | `string` | Yes | Daemon operation ID (UUID) |
| `taskType` | `string` | Yes | Task type identifier (e.g., `'db:migrate'`) |
| `metadata` | `TaskMetadata` | No | Task metadata (description, cron, tags, etc.) |

**Returns:** `TaskRegistrationResult`

```typescript
{
  success: boolean;
  operationId: string;
  nitroTaskId: string;
  taskMetadata: TaskMetadata;
}
```

**Throws:**
- `Error` - If operation not found in daemon
- `Error` - If operation already registered

**Emits:** `task:registered` event

**Example:**

```javascript
const result = executor.registerOperationAsTask(
  '550e8400-e29b-41d4-a716-446655440000',
  'db:migrate',
  {
    description: 'Run database migrations',
    cronExpression: '0 2 * * *',
    priority: 'high',
    tags: ['critical', 'database'],
  }
);

console.log(result.nitroTaskId); // "daemon:db:migrate"
```

---

##### `unregisterTask()`

Unregister a Nitro task.

```javascript
executor.unregisterTask(operationId: string): boolean
```

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `operationId` | `string` | Yes | Daemon operation ID |

**Returns:** `boolean` - `true` if unregistered, `false` if not found

**Emits:** `task:unregistered` event (if successful)

**Example:**

```javascript
const removed = executor.unregisterTask(operationId);
if (removed) {
  console.log('Task unregistered');
}
```

---

##### `runTask()`

Execute a Nitro task.

```javascript
await executor.runTask(
  taskId: string,
  payload?: Record<string, any>
): Promise<TaskExecutionResult>
```

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `taskId` | `string` | Yes | Nitro task ID (e.g., `'daemon:my-task'`) |
| `payload` | `Record<string, any>` | No | Task execution payload |

**Returns:** `Promise<TaskExecutionResult>`

```typescript
{
  success: boolean;
  taskId: string;
  operationId: string;
  result: any;
  duration: number; // milliseconds
  timestamp: Date;
}
```

**Throws:**
- `Error` - If task not found
- `Error` - If daemon not running
- `Error` - If execution fails after retries
- `TimeoutError` - If execution exceeds timeout

**Example:**

```javascript
try {
  const result = await executor.runTask('daemon:db:migrate', {
    dryRun: false,
    verbose: true,
  });
  console.log('Task completed:', result.result);
} catch (error) {
  console.error('Task failed:', error.message);
}
```

---

##### `validateTask()`

Validate a task before execution (pre-flight check).

```javascript
executor.validateTask(taskId: string): TaskValidationResult
```

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `taskId` | `string` | Yes | Nitro task ID |

**Returns:** `TaskValidationResult`

```typescript
{
  valid: boolean;
  taskId: string;
  operationId?: string;
  reason?: string; // If invalid
}
```

**Example:**

```javascript
const validation = executor.validateTask('daemon:my-task');
if (!validation.valid) {
  console.error('Validation failed:', validation.reason);
} else {
  console.log('Task is valid and ready to execute');
}
```

---

##### `listTasks()`

List all registered Nitro tasks.

```javascript
executor.listTasks(): TaskInfo[]
```

**Returns:** `TaskInfo[]`

```typescript
{
  daemonOperationId: string;
  nitroTaskId: string;
  operationType: string;
  description?: string;
  cronExpression?: string;
  priority?: string;
  tags?: string[];
}[]
```

**Example:**

```javascript
const tasks = executor.listTasks();
tasks.forEach((task) => {
  console.log(`${task.nitroTaskId}: ${task.description}`);
});
```

---

##### `getMetrics()`

Get executor execution metrics.

```javascript
executor.getMetrics(): ExecutorMetrics
```

**Returns:** `ExecutorMetrics`

```typescript
{
  executorId: string;
  tasksExecuted: number;
  tasksSucceeded: number;
  tasksFailed: number;
  totalDuration: number; // milliseconds
  averageDuration: number; // milliseconds
  registeredTasks: number;
  executionHistory: number; // History entry count
  daemonHealth: DaemonHealth;
  daemonMetrics: DaemonMetrics;
}
```

**Example:**

```javascript
const metrics = executor.getMetrics();
console.log(`Success rate: ${(metrics.tasksSucceeded / metrics.tasksExecuted * 100).toFixed(1)}%`);
console.log(`Avg duration: ${metrics.averageDuration.toFixed(0)}ms`);
```

---

##### `resetMetrics()`

Reset executor metrics to zero.

```javascript
executor.resetMetrics(): void
```

**Example:**

```javascript
executor.resetMetrics();
console.log('Metrics reset');
```

---

##### `getStatus()`

Get executor status and health.

```javascript
executor.getStatus(): ExecutorStatus
```

**Returns:** `ExecutorStatus`

```typescript
{
  executorId: string;
  running: boolean;
  registeredTasks: number;
  metrics: ExecutorMetrics;
  tasks: TaskInfo[];
  lastActivity?: Date;
}
```

**Example:**

```javascript
const status = executor.getStatus();
if (!status.running) {
  console.error('Executor is not running');
}
```

---

##### `getExecutionHistory()`

Get task execution history.

```javascript
executor.getExecutionHistory(taskId?: string): ExecutionHistoryEntry[]
```

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `taskId` | `string` | No | Filter by task ID (all tasks if omitted) |

**Returns:** `ExecutionHistoryEntry[]`

```typescript
{
  taskId: string;
  operationId: string;
  success: boolean;
  duration: number; // milliseconds
  timestamp: Date;
  error?: string;
  result?: any;
}[]
```

**Example:**

```javascript
// All history
const allHistory = executor.getExecutionHistory();

// Specific task history
const taskHistory = executor.getExecutionHistory('daemon:my-task');
```

---

##### `discoverOperationsAsTaskCandidates()`

Discover daemon operations that can be registered as tasks.

```javascript
executor.discoverOperationsAsTaskCandidates(
  filter?: (operation: Operation) => boolean
): Operation[]
```

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `filter` | `(operation: Operation) => boolean` | No | Filter function |

**Returns:** `Operation[]` - Array of daemon operations

**Example:**

```javascript
// All operations
const candidates = executor.discoverOperationsAsTaskCandidates();

// Filtered
const dbOps = executor.discoverOperationsAsTaskCandidates(
  (op) => op.name.startsWith('db:')
);
```

---

##### `on()`

Register event listener.

```javascript
executor.on(eventName: string, handler: Function): Function
```

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `eventName` | `string` | Yes | Event name (see [Events](#events)) |
| `handler` | `Function` | Yes | Event handler function |

**Returns:** `Function` - Cleanup function to unregister listener

**Example:**

```javascript
const cleanup = executor.on('task:succeeded', (event) => {
  console.log(`Task ${event.nitroTaskId} succeeded`);
});

// Later: unregister
cleanup();
```

---

## Factory Functions

### createNitroTaskExecutor()

Factory function to create a NitroTaskExecutor instance.

```javascript
createNitroTaskExecutor(
  daemon: Daemon,
  config?: NitroExecutorConfig
): NitroTaskExecutor
```

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `daemon` | `Daemon` | Yes | Daemon instance |
| `config` | `NitroExecutorConfig` | No | Configuration |

**Returns:** `NitroTaskExecutor` instance

**Example:**

```javascript
import { createNitroTaskExecutor } from '@unrdf/daemon/integrations/nitro-tasks';

const executor = createNitroTaskExecutor(daemon, {
  autoStart: true,
  timeout: 30000,
  maxRetries: 3,
});
```

---

### integrateNitroTasks()

Integrate multiple daemon operations as Nitro tasks in one call.

```javascript
await integrateNitroTasks(
  daemon: Daemon,
  operations: Array<{ id: string; taskType: string; metadata?: TaskMetadata }>,
  config?: NitroExecutorConfig
): Promise<NitroTaskExecutor>
```

**Parameters:**

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `daemon` | `Daemon` | Yes | Daemon instance |
| `operations` | `Array<TaskRegistration>` | Yes | Operations to register |
| `config` | `NitroExecutorConfig` | No | Executor configuration |

**Returns:** `Promise<NitroTaskExecutor>`

**Example:**

```javascript
import { integrateNitroTasks } from '@unrdf/daemon/integrations/nitro-tasks';

const executor = await integrateNitroTasks(
  daemon,
  [
    { id: op1Id, taskType: 'db:migrate', metadata: { priority: 'high' } },
    { id: op2Id, taskType: 'cache:clear', metadata: { priority: 'normal' } },
    { id: op3Id, taskType: 'email:send', metadata: { cronExpression: '0 9 * * *' } },
  ],
  { autoStart: true }
);
```

---

## Events

### Executor Events

#### `executor:started`

Emitted when executor starts.

**Payload:**

```typescript
{
  executorId: string;
  timestamp: Date;
}
```

**Example:**

```javascript
executor.on('executor:started', (event) => {
  console.log(`Executor ${event.executorId} started at ${event.timestamp}`);
});
```

---

#### `executor:stopped`

Emitted when executor stops.

**Payload:**

```typescript
{
  executorId: string;
  timestamp: Date;
}
```

---

### Task Events

#### `task:registered`

Emitted when a task is registered.

**Payload:**

```typescript
{
  daemonOperationId: string;
  nitroTaskId: string;
  operationType: string;
  metadata: TaskMetadata;
  timestamp: Date;
}
```

**Example:**

```javascript
executor.on('task:registered', (event) => {
  console.log(`Task registered: ${event.nitroTaskId}`);
});
```

---

#### `task:unregistered`

Emitted when a task is unregistered.

**Payload:**

```typescript
{
  daemonOperationId: string;
  nitroTaskId: string;
  timestamp: Date;
}
```

---

#### `task:started`

Emitted when a task execution starts.

**Payload:**

```typescript
{
  daemonOperationId: string;
  nitroTaskId: string;
  timestamp: Date;
}
```

**Example:**

```javascript
executor.on('task:started', (event) => {
  console.log(`Task started: ${event.nitroTaskId}`);
});
```

---

#### `task:succeeded`

Emitted when a task execution succeeds.

**Payload:**

```typescript
{
  daemonOperationId: string;
  nitroTaskId: string;
  result: any;
  duration: number; // milliseconds
  timestamp: Date;
}
```

**Example:**

```javascript
executor.on('task:succeeded', (event) => {
  console.log(`Task ${event.nitroTaskId} succeeded in ${event.duration}ms`);
});
```

---

#### `task:failed`

Emitted when a task execution fails.

**Payload:**

```typescript
{
  daemonOperationId: string;
  nitroTaskId: string;
  error: string;
  duration: number; // milliseconds
  attempt?: number; // If retry enabled
  nextRetryIn?: number; // ms until next retry
  timestamp: Date;
}
```

**Example:**

```javascript
executor.on('task:failed', (event) => {
  console.error(`Task ${event.nitroTaskId} failed: ${event.error}`);
  if (event.nextRetryIn) {
    console.log(`Retrying in ${event.nextRetryIn}ms`);
  }
});
```

---

#### `task:retry-exhausted`

Emitted when all retry attempts are exhausted.

**Payload:**

```typescript
{
  daemonOperationId: string;
  nitroTaskId: string;
  error: string;
  attempts: number;
  timestamp: Date;
}
```

**Example:**

```javascript
executor.on('task:retry-exhausted', (event) => {
  console.error(`Task ${event.nitroTaskId} failed after ${event.attempts} attempts`);
});
```

---

## Type Definitions

### NitroExecutorConfig

Configuration for NitroTaskExecutor.

```typescript
interface NitroExecutorConfig {
  /** Executor ID (auto-generated if not provided) */
  executorId?: string;

  /** Auto-start daemon on creation */
  autoStart?: boolean; // Default: false

  /** Task execution timeout (milliseconds) */
  timeout?: number; // Default: 30000

  /** Maximum retry attempts for failed tasks */
  maxRetries?: number; // Default: 3

  /** Retry policy configuration */
  retryPolicy?: {
    /** Initial backoff delay (ms) */
    backoffMs?: number; // Default: 1000
    /** Backoff multiplier for exponential backoff */
    backoffMultiplier?: number; // Default: 2
    /** Maximum backoff delay (ms) */
    maxBackoffMs?: number; // Default: 30000
    /** Jitter factor (0-1) to randomize backoff */
    jitterFactor?: number; // Default: 0.1
  };

  /** Task ID prefix (e.g., 'daemon:') */
  taskPrefix?: string; // Default: 'daemon:'

  /** Enable event relay from daemon to executor */
  enableEventRelay?: boolean; // Default: true

  /** Enable metrics collection */
  enableMetrics?: boolean; // Default: true

  /** Timeout defaults for different contexts */
  timeoutDefaults?: {
    /** Default task timeout */
    taskTimeoutMs?: number; // Default: 30000
    /** Default case/workflow timeout */
    caseTimeoutMs?: number; // Default: 3600000
  };

  /** Maximum concurrent workflow cases */
  maxConcurrentCases?: number; // Default: 100
}
```

---

### TaskMetadata

Metadata for a Nitro task.

```typescript
interface TaskMetadata {
  /** Human-readable task description */
  description?: string;

  /** Cron expression for scheduling */
  cronExpression?: string;

  /** Task priority (high, normal, low) */
  priority?: 'high' | 'normal' | 'low';

  /** Tags for categorization */
  tags?: string[];

  /** Task operation type identifier */
  operationType?: string;

  /** Custom metadata */
  [key: string]: any;
}
```

---

### TaskRegistrationResult

Result of task registration.

```typescript
interface TaskRegistrationResult {
  /** Registration success status */
  success: boolean;

  /** Daemon operation ID */
  operationId: string;

  /** Generated Nitro task ID */
  nitroTaskId: string;

  /** Task metadata */
  taskMetadata: TaskMetadata;
}
```

---

### TaskExecutionResult

Result of task execution.

```typescript
interface TaskExecutionResult {
  /** Execution success status */
  success: boolean;

  /** Nitro task ID */
  taskId: string;

  /** Daemon operation ID */
  operationId: string;

  /** Task execution result */
  result: any;

  /** Execution duration (milliseconds) */
  duration: number;

  /** Execution timestamp */
  timestamp: Date;

  /** Error message (if failed) */
  error?: string;
}
```

---

### TaskValidationResult

Result of task validation.

```typescript
interface TaskValidationResult {
  /** Validation success status */
  valid: boolean;

  /** Nitro task ID */
  taskId: string;

  /** Daemon operation ID (if valid) */
  operationId?: string;

  /** Validation failure reason (if invalid) */
  reason?: string;
}
```

---

### ExecutorMetrics

Executor performance metrics.

```typescript
interface ExecutorMetrics {
  /** Executor instance ID */
  executorId: string;

  /** Total tasks executed */
  tasksExecuted: number;

  /** Total tasks succeeded */
  tasksSucceeded: number;

  /** Total tasks failed */
  tasksFailed: number;

  /** Total execution duration (ms) */
  totalDuration: number;

  /** Average execution duration (ms) */
  averageDuration: number;

  /** Number of registered tasks */
  registeredTasks: number;

  /** Execution history entry count */
  executionHistory: number;

  /** Daemon health status */
  daemonHealth: DaemonHealth;

  /** Daemon metrics */
  daemonMetrics: DaemonMetrics;
}
```

---

## Configuration

### Default Configuration

```javascript
const DEFAULT_CONFIG = {
  autoStart: false,
  timeout: 30000, // 30 seconds
  maxRetries: 3,
  retryPolicy: {
    backoffMs: 1000, // 1 second
    backoffMultiplier: 2, // Exponential: 1s, 2s, 4s, 8s, 16s
    maxBackoffMs: 30000, // 30 seconds
    jitterFactor: 0.1, // 10% randomness
  },
  taskPrefix: 'daemon:',
  enableEventRelay: true,
  enableMetrics: true,
  timeoutDefaults: {
    taskTimeoutMs: 30000, // 30 seconds
    caseTimeoutMs: 3600000, // 1 hour
  },
  maxConcurrentCases: 100,
};
```

### Configuration Examples

#### Production Configuration

```javascript
const prodConfig = {
  autoStart: true,
  timeout: 60000, // 1 minute
  maxRetries: 5,
  retryPolicy: {
    backoffMs: 2000,
    backoffMultiplier: 2,
    maxBackoffMs: 60000,
    jitterFactor: 0.15,
  },
  taskPrefix: 'prod:',
  enableEventRelay: true,
  enableMetrics: true,
  maxConcurrentCases: 1000,
};
```

#### Development Configuration

```javascript
const devConfig = {
  autoStart: true,
  timeout: 10000, // 10 seconds
  maxRetries: 1,
  retryPolicy: {
    backoffMs: 500,
    backoffMultiplier: 1.5,
    maxBackoffMs: 5000,
  },
  taskPrefix: 'dev:',
  enableEventRelay: true,
  enableMetrics: true,
};
```

---

## Error Handling

### Error Types

#### `TaskNotFoundError`

Thrown when attempting to execute a non-existent task.

```javascript
try {
  await executor.runTask('non-existent');
} catch (error) {
  if (error instanceof TaskNotFoundError) {
    console.error('Task not found:', error.taskId);
  }
}
```

#### `DaemonNotRunningError`

Thrown when daemon is not running.

```javascript
try {
  await executor.runTask('daemon:my-task');
} catch (error) {
  if (error instanceof DaemonNotRunningError) {
    console.error('Daemon is not running');
    await executor.start();
  }
}
```

#### `TaskTimeoutError`

Thrown when task execution exceeds timeout.

```javascript
try {
  await executor.runTask('daemon:slow-task');
} catch (error) {
  if (error instanceof TaskTimeoutError) {
    console.error('Task timed out after', error.timeout, 'ms');
  }
}
```

#### `TaskValidationError`

Thrown when task validation fails.

```javascript
try {
  executor.registerOperationAsTask('invalid-id', 'task');
} catch (error) {
  if (error instanceof TaskValidationError) {
    console.error('Validation error:', error.message);
  }
}
```

### Error Handling Best Practices

```javascript
// Comprehensive error handling
try {
  const result = await executor.runTask(taskId, payload);
  console.log('Success:', result);
} catch (error) {
  if (error instanceof TaskNotFoundError) {
    // Task doesn't exist
    console.error('Task not found');
  } else if (error instanceof DaemonNotRunningError) {
    // Daemon stopped
    console.error('Daemon not running');
    await executor.start();
  } else if (error instanceof TaskTimeoutError) {
    // Timeout exceeded
    console.error('Task timed out');
  } else {
    // Other errors
    console.error('Execution failed:', error.message);
  }
}
```

---

## Performance Characteristics

### Latency Benchmarks

| Operation | P50 | P95 | P99 |
|-----------|-----|-----|-----|
| Task registration | <1ms | <5ms | <10ms |
| Task execution (no-op) | 5ms | 15ms | 30ms |
| Metadata lookup | <1ms | <2ms | <5ms |
| Event relay | <0.5ms | <2ms | <5ms |
| Metrics update | <1ms | <3ms | <10ms |

### Memory Footprint

- **Executor instance**: ~50KB (base)
- **Per registered task**: ~2KB (metadata)
- **Execution history (LRU)**: ~100KB (1000 entries)
- **Total for 100 tasks**: ~450KB

### Throughput

- **Concurrent task executions**: Unlimited (limited by daemon concurrency)
- **Default daemon concurrency**: 10 parallel operations
- **Max throughput**: ~1000 tasks/sec (with concurrent execution)

### Optimization Tips

1. **Batch operations**: Use `Promise.all()` for concurrent execution
2. **Tune timeout**: Set appropriate timeout for operation type
3. **Monitor metrics**: Use `getMetrics()` to track performance
4. **Limit history**: Control LRU cache size in daemon config
5. **Disable unused features**: Turn off event relay/metrics if not needed

---

## See Also

- **[Tutorial: YAWL-Nitro Integration](/docs/diataxis/tutorials/yawl-nitro-integration.md)**
- **[How-To: YAWL-Nitro Tasks](/docs/diataxis/how-to/yawl-nitro-tasks.md)**
- **[Explanation: YAWL-Nitro Architecture](/docs/diataxis/explanation/yawl-nitro-architecture.md)**
- **[Example: YAWL-Nitro Workflow](/examples/yawl-nitro-workflow.mjs)**
- **[@unrdf/daemon API](/docs/diataxis/reference/yawl-daemon-api.md)**
- **[@unrdf/yawl API](/packages/yawl/README.md)**

**Version**: 1.0.0
**Last Updated**: 2026-01-11
