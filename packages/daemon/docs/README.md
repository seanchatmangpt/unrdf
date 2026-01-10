# @unrdf/daemon Documentation

Background daemon for managing scheduled tasks and event-driven operations in UNRDF.

## Overview

The daemon module provides:
- **Scheduled Task Execution** - Schedule operations with cron, interval, or event-based triggers
- **Clustering Support** - Coordinate task execution across multiple daemon nodes
- **Event-Driven Architecture** - React to system events with automatic task execution
- **Health Monitoring** - Real-time health checks and performance metrics
- **Retry Policies** - Configurable exponential backoff with jitter
- **Receipt Generation** - Audit trail with cryptographic proofs

## Quick Start

```javascript
import { Daemon } from '@unrdf/daemon';

const daemon = new Daemon({ id: 'my-daemon' });

daemon.on('operation:success', (event) => {
  console.log(`✓ ${event.name} completed in ${event.duration}ms`);
});

await daemon.start();
daemon.schedule({
  id: 'my-op',
  name: 'My Operation',
  handler: async () => ({ status: 'done' })
});

const result = await daemon.execute('my-op');
await daemon.stop();
```

## Core Components

### Daemon Class
Main daemon controller for managing scheduled operations and clustering.

**Key Methods:**
- `start()` / `stop()` - Lifecycle management
- `schedule()` / `unschedule()` - Operation management
- `execute()` - Run immediately
- `getHealth()` / `getMetrics()` - Monitoring

### TriggerEvaluator
Evaluates trigger conditions for scheduled execution.

**Trigger Types:**
- `interval` - Fixed time intervals
- `cron` - POSIX cron expressions
- `idle` - When daemon is idle
- `reactive` - On entity mutations
- `event` - Custom events

### Schemas
Comprehensive Zod validation for all daemon entities.

**Key Schemas:**
- `DaemonConfigSchema` - Configuration validation
- `ScheduledOperationSchema` - Operation definitions
- `TriggerSchema` - Trigger specifications
- `OperationReceiptSchema` - Execution proof records

## Documentation Structure (Diataxis)

Choose your learning path based on your goal:

### 1. **[Tutorial](./tutorial.md)** - Learning-Oriented (15-20 min)
Start here if you're new to @unrdf/daemon.
- Hello World daemon
- Schedule interval operations
- Schedule cron operations
- React to events
- Multi-node cluster setup

### 2. **[How-To Guides](./how-to.md)** - Task-Oriented (5 min each)
Jump here when you need to accomplish a specific task.
- How to schedule an operation
- How to listen to completion events
- How to implement graceful shutdown
- How to monitor daemon health
- How to troubleshoot failures

### 3. **[API Reference](./reference.md)** - Complete API Documentation
Search here for detailed method signatures and parameters.
- Daemon class methods
- All available events
- Schema definitions
- Error codes and handling

### 4. **[Explanation](./explanation.md)** - Conceptual Understanding (Deep-Dive)
Read this to understand the "why" behind design decisions.
- Architecture overview
- Design decisions (ADRs)
- Clustering & distribution patterns
- Integration patterns
- Performance characteristics
- When to use daemon

## Examples

Three complete, runnable examples included:

### 01: Basic Daemon (100 lines)
Demonstrates fundamental daemon usage:
- Create and start daemon
- Schedule multiple operations
- Execute operations
- Listen to lifecycle events
- Check health and metrics

Run:
```bash
node examples/01-basic-daemon.mjs
```

### 02: Distributed Cluster (150 lines)
Demonstrates multi-node clustering:
- Create 3-node cluster
- Distribute operations across nodes
- Simulate leader election
- Demonstrate failover
- Monitor cluster health

Run:
```bash
node examples/02-distributed-cluster.mjs
```

### 03: Event Sourcing (120 lines)
Demonstrates KGC-4D style event sourcing:
- Generate cryptographic receipts
- Create immutable audit trail
- Verify operation integrity
- Chain operations together
- Enable operation replay

Run:
```bash
node examples/03-event-sourcing.mjs
```

## Configuration

Basic daemon configuration:

```javascript
const daemon = new Daemon({
  id: 'unique-identifier',          // Required
  nodeId: 'node-1',                 // Optional: cluster node ID
  clusterId: 'production-cluster',  // Optional: cluster name
  maxConcurrent: 5,                 // Optional: max parallel ops
  logger: customLogger,              // Optional: logger instance
});
```

All configuration validated using Zod schemas. See `src/schemas.mjs` for full schema definitions.

## Testing

Run all daemon tests:
```bash
pnpm test
```

Run daemon-specific tests:
```bash
pnpm -C packages/daemon test
```

Watch mode:
```bash
pnpm test:watch
```

Tests located in `../test/` directory.

## Performance Benchmarks

Run all benchmarks:
```bash
pnpm benchmark
```

Run daemon-specific benchmarks:
```bash
pnpm benchmark:daemon
```

Benchmarks located in `../benchmarks/` directory. See [Explanation](./explanation.md) for performance characteristics and targets.

## Key Design Decisions

1. **EventEmitter over Promises** - Enables multi-listener event handling and operation chaining
2. **LRU Cache for History** - Bounded memory with fast access to recent completions
3. **Zod Runtime Validation** - Catch errors early with schema enforcement
4. **Pure Handlers** - Operations must be stateless for clustering support

See [Explanation Guide](./explanation.md) for full ADRs (Architecture Decision Records).

## Common Use Cases

| Use Case | Example |
|----------|---------|
| Periodic ETL | Daily data processing and aggregation |
| Scheduled Maintenance | Cleanup old logs, optimize database |
| Event-Driven Workflows | Send email on user signup |
| Distributed Coordination | Leader-only tasks in cluster |
| Audit Logging | Generate receipts with proofs |

## Integration Points

**With KGC-4D:**
Generate cryptographic receipts for full audit trails and event sourcing.

**With Hooks:**
Execute policy-driven validation and cleanup workflows.

**With Streaming:**
React to change feeds and entity mutations.

See [Explanation](./explanation.md) for integration patterns.

## Next Steps

1. **New to daemons?** → Start with [Tutorial](./tutorial.md)
2. **Need to do something specific?** → Use [How-To Guides](./how-to.md)
3. **Looking up an API?** → Check [API Reference](./reference.md)
4. **Want to understand the design?** → Read [Explanation](./explanation.md)
5. **Ready to build?** → Run the [Examples](../examples/)

## Files

```
packages/daemon/
├── docs/
│   ├── README.md (this file)
│   ├── tutorial.md (learning path)
│   ├── how-to.md (task solutions)
│   ├── reference.md (API docs)
│   └── explanation.md (deep dives)
├── examples/
│   ├── 01-basic-daemon.mjs
│   ├── 02-distributed-cluster.mjs
│   └── 03-event-sourcing.mjs
├── src/
│   ├── daemon.mjs (main class)
│   ├── schemas.mjs (Zod validation)
│   ├── trigger-evaluator.mjs (trigger logic)
│   ├── integrations/ (clustering, streaming, etc)
│   └── index.mjs (exports)
└── test/
    └── *.test.mjs
```
