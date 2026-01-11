# @unrdf/daemon

Background daemon for managing scheduled tasks and event-driven operations in UNRDF.

## Features

- **Scheduled Task Execution** - Execute operations based on cron, intervals, or events
- **Clustering Support** - Coordinate task execution across multiple daemon nodes
- **Event-Driven Architecture** - React to system events with automatic task execution
- **Health Monitoring** - Real-time health checks and performance metrics
- **Retry Policies** - Configurable exponential backoff with jitter
- **Receipt Generation** - Audit trail with cryptographic proofs for operations
- **Concurrency Control** - Limit concurrent operations with queue management

## Installation

```bash
pnpm add @unrdf/daemon
```

## Quick Start

```javascript
import { Daemon } from '@unrdf/daemon';

// Create daemon
const daemon = new Daemon({
  daemonId: 'my-daemon',
  name: 'My Daemon',
  concurrency: 5,
});

// Start daemon
await daemon.start();

// Schedule operation
daemon.schedule({
  id: 'my-task',
  name: 'My Task',
  handler: async () => {
    console.log('Task executing...');
    return { status: 'completed' };
  },
});

// Execute operation
const result = await daemon.execute('my-task');

// Stop daemon
await daemon.stop();
```

## Core Components

### Daemon Class
Main controller for managing scheduled operations and clustering.

**Key Methods**:
- `start()` - Start the daemon
- `stop()` - Stop the daemon
- `schedule(operation)` - Register an operation
- `unschedule(operationId)` - Unregister an operation
- `execute(operationId)` - Execute an operation immediately
- `listOperations()` - List all scheduled operations
- `getHealth()` - Get daemon health status
- `getMetrics()` - Get performance metrics

### TriggerEvaluator
Evaluates events against trigger patterns and determines which tasks to execute.

**Key Methods**:
- `registerTrigger(eventName, pattern, actions)` - Register trigger pattern
- `evaluate(event)` - Evaluate event and get matched task IDs
- `matches(event, trigger)` - Check event-trigger match

### Schemas
Comprehensive Zod validation schemas for configuration and operations:
- `DaemonConfigSchema` - Daemon configuration
- `ScheduledOperationSchema` - Operation definition
- `OperationReceiptSchema` - Operation execution record
- `DaemonHealthSchema` - Health status
- `DaemonMetricsSchema` - Performance metrics

## Configuration

```javascript
const daemon = new Daemon({
  // Core settings
  daemonId: 'unique-id',
  name: 'Human-readable name',

  // Network/server
  port: 8080,                    // Server port (default: 8080)

  // Performance
  concurrency: 10,               // Max concurrent ops (default: 10)
  healthCheckIntervalMs: 30000,  // Health check frequency
  metricsRetentionMs: 3600000,   // Metrics window size

  // Logging
  logLevel: 'info',              // Log level
  logger: customLogger,          // Optional custom logger

  // Optional: Retry policy
  globalRetryPolicy: {
    maxAttempts: 3,
    backoffMs: 1000,
    backoffMultiplier: 2,
    maxBackoffMs: 30000,
    jitterFactor: 0.1,
  },

  // Optional: Pre-registered operations
  operations: [...],

  // Optional: Environment variables
  environment: { KEY: 'value' },
});
```

## Events

The daemon emits events for monitoring:
- `daemon:started` - Daemon has started
- `daemon:stopped` - Daemon has stopped
- `operation:enqueued` - Operation added to queue
- `operation:started` - Operation execution started
- `operation:success` - Operation completed successfully
- `operation:failure` - Operation failed

```javascript
daemon.on('operation:success', (event) => {
  console.log(`Operation ${event.operationId} took ${event.duration}ms`);
});
```

## Health & Metrics

### Health Status
```javascript
const health = daemon.getHealth();
// {
//   nodeId: 'node-xxx',
//   clusterId: 'cluster-xxx',
//   isRunning: true,
//   isLeader: false,
//   uptime: 12345,
//   activeOperations: 2,
//   queuedOperations: 5,
//   completedOperations: 42,
//   timestamp: Date
// }
```

### Metrics
```javascript
const metrics = daemon.getMetrics();
// {
//   nodeId: 'node-xxx',
//   totalOperations: 42,
//   successfulOperations: 40,
//   failedOperations: 2,
//   averageDuration: 156,
//   totalDuration: 6552,
//   successRate: 95.24,
//   timestamp: Date
// }
```

## Testing

Run tests with:
```bash
pnpm test
pnpm test:watch
pnpm test:coverage
```

## Examples

See the `examples/` directory for complete working examples:
- `01-basic.mjs` - Basic daemon setup and operations

## Documentation

See `docs/README.md` for detailed documentation.

## License

MIT
