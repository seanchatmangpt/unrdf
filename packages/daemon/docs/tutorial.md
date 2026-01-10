# Tutorial: Getting Started with @unrdf/daemon

Learn how to build background task systems with @unrdf/daemon. This tutorial covers essential patterns through hands-on examples (15-20 minutes).

## What You'll Learn

By the end of this tutorial, you'll understand:
- How to create and start a daemon instance
- Scheduling operations with different trigger types
- Reacting to daemon events
- Building a simple clustered setup
- Monitoring daemon health

## Prerequisites

- Node.js 18+ with ESM support
- Basic understanding of async/await
- @unrdf/daemon installed

## Part 1: Hello World - Your First Daemon

Create a minimal daemon that executes a single operation:

```javascript
import { Daemon } from '@unrdf/daemon';

// Create a daemon instance
const daemon = new Daemon({
  id: 'tutorial-daemon',
  logger: console,
});

// Define an operation
const operation = {
  id: 'greeting-op',
  name: 'Send Greeting',
  handler: async () => {
    return { message: 'Hello, daemon world!', timestamp: new Date() };
  },
};

// Listen for completion
daemon.on('operation:success', (event) => {
  console.log('✓ Operation completed:', event.operationId);
});

// Run it
await daemon.start();
daemon.schedule(operation);
const result = await daemon.execute('greeting-op');
console.log(result); // { message: 'Hello, daemon world!', timestamp: ... }
await daemon.stop();
```

**Key concepts introduced:**
- `Daemon` constructor creates an instance
- `.schedule()` registers an operation
- `.execute()` runs it immediately
- Events track execution (`operation:success`)
- `.start()` / `.stop()` manage lifecycle

---

## Part 2: Schedule Operations with Fixed Intervals

Execute operations repeatedly at fixed time intervals:

```javascript
import { Daemon } from '@unrdf/daemon';

const daemon = new Daemon({
  id: 'interval-daemon',
  logger: console,
});

let execCount = 0;

// Operation that runs every 5 seconds
const healthCheck = {
  id: 'health-check',
  name: 'System Health Check',
  handler: async () => {
    execCount++;
    const health = {
      nodeId: daemon.nodeId,
      uptime: daemon.getHealth().uptime,
      executionCount: execCount,
      timestamp: new Date().toISOString(),
    };
    console.log('[HealthCheck]', health);
    return health;
  },
};

// Listen to all operation events
daemon.on('operation:enqueued', (event) => {
  console.log('📋 Queued:', event.operationId);
});

daemon.on('operation:started', (event) => {
  console.log('▶️  Started:', event.operationId);
});

daemon.on('operation:success', (event) => {
  console.log('✓ Success:', event.operationId, `(${event.duration}ms)`);
});

daemon.on('operation:failure', (event) => {
  console.log('✗ Failed:', event.operationId, '-', event.error);
});

// Start daemon
await daemon.start();

// Schedule the operation
daemon.schedule(healthCheck);

// Execute manually first time
await daemon.execute('health-check');

// Then simulate interval execution
for (let i = 0; i < 3; i++) {
  await new Promise(resolve => setTimeout(resolve, 5000));
  try {
    await daemon.execute('health-check');
  } catch (e) {
    console.error('Operation failed:', e.message);
  }
}

await daemon.stop();
```

**Key concepts introduced:**
- Repeated operation execution with timing
- Event-driven monitoring
- Error handling in operations
- Daemon metrics and health status

---

## Part 3: Schedule Operations with Cron Expressions

Execute operations on complex schedules using cron syntax:

```javascript
import { Daemon } from '@unrdf/daemon';
import { evaluateTrigger } from '@unrdf/daemon/trigger-evaluator';

const daemon = new Daemon({
  id: 'cron-daemon',
  logger: console,
});

// Operation that runs daily at 2 AM
const cronOp = {
  id: 'daily-backup',
  name: 'Daily Database Backup',
  handler: async () => {
    return {
      type: 'backup',
      status: 'completed',
      backupPath: `/backups/db-${new Date().toISOString()}.bak`,
      size: '2.5GB',
    };
  },
};

// Example cron patterns explained:
//   '0 2 * * *'     -> Every day at 2:00 AM
//   '0 */6 * * *'   -> Every 6 hours
//   '0 0 * * 0'     -> Every Sunday at midnight
//   '*/15 * * * *'  -> Every 15 minutes

// In a real system, evaluate cron triggers
const cronTrigger = {
  type: 'cron',
  expression: '0 2 * * *', // Daily at 2 AM
  timezone: 'UTC',
};

// Check when next execution should occur
const evaluation = evaluateTrigger(cronTrigger, Date.now() - 86400000);
console.log('Trigger evaluation:', evaluation);

// Schedule and execute
await daemon.start();
daemon.schedule(cronOp);

// Simulate immediate execution to test the operation
try {
  const result = await daemon.execute('daily-backup');
  console.log('Backup result:', result);
} catch (e) {
  console.error('Backup failed:', e.message);
}

await daemon.stop();
```

**Key concepts introduced:**
- Cron expression syntax and evaluation
- Timezone-aware scheduling
- Trigger evaluation function
- Testing scheduled operations

---

## Part 4: React to Events and Build Event-Driven Workflows

Listen to system events and trigger operations automatically:

```javascript
import { Daemon } from '@unrdf/daemon';

const daemon = new Daemon({
  id: 'event-daemon',
  logger: console,
});

// Track events
const eventLog = [];

// Operation: Process user signup
const processSignup = {
  id: 'process-signup',
  name: 'Process User Signup',
  handler: async () => {
    const event = eventLog[eventLog.length - 1];
    if (event && event.type === 'user:created') {
      return {
        status: 'sent_welcome_email',
        userId: event.userId,
        email: event.email,
      };
    }
    throw new Error('No signup event found');
  },
};

// Operation: Sync to external service
const syncUser = {
  id: 'sync-user',
  name: 'Sync User to CRM',
  handler: async () => {
    const event = eventLog[eventLog.length - 1];
    return {
      status: 'synced',
      service: 'external-crm',
      event,
    };
  },
};

// Listen for operation success and trigger chained operations
daemon.on('operation:success', async (event) => {
  console.log('✓ Operation completed:', event.operationId);

  // Chain operations based on result
  if (event.operationId === 'process-signup') {
    console.log('  Triggering sync operation...');
    try {
      await daemon.execute('sync-user');
    } catch (e) {
      console.error('Sync failed:', e.message);
    }
  }
});

daemon.on('operation:failure', (event) => {
  console.log('✗ Operation failed:', event.operationId, '-', event.error);
});

// Simulate custom events
daemon.on('user:created', (userData) => {
  console.log('👤 User created event received:', userData.email);
  eventLog.push({ type: 'user:created', ...userData });
});

await daemon.start();
daemon.schedule(processSignup);
daemon.schedule(syncUser);

// Emit custom event to trigger workflow
daemon.emit('user:created', {
  userId: 'user-123',
  email: 'alice@example.com',
  name: 'Alice',
});

// Execute the workflow
try {
  await daemon.execute('process-signup');
} catch (e) {
  console.error('Workflow error:', e.message);
}

await daemon.stop();
```

**Key concepts introduced:**
- Event-driven operation triggering
- Operation chaining and workflows
- Custom events in daemon
- Error handling in event flows

---

## Part 5: Build a Simple Multi-Node Cluster

Coordinate operation execution across multiple daemon instances:

```javascript
import { Daemon } from '@unrdf/daemon';

// Simulate a 3-node cluster
const cluster = [
  {
    id: 'node-1',
    clusterId: 'demo-cluster',
    nodeId: 'node-1',
  },
  {
    id: 'node-2',
    clusterId: 'demo-cluster',
    nodeId: 'node-2',
  },
  {
    id: 'node-3',
    clusterId: 'demo-cluster',
    nodeId: 'node-3',
  },
];

// Create daemon instances
const daemons = cluster.map(cfg => new Daemon(cfg));

// Shared operation - distributes work
const distributedOp = {
  id: 'process-batch',
  name: 'Process Data Batch',
  handler: async function() {
    return {
      status: 'completed',
      nodeId: this.nodeId,
      itemsProcessed: 100,
      timestamp: new Date().toISOString(),
    };
  },
};

// Start all daemons
await Promise.all(daemons.map(d => d.start()));

// Schedule operation on each node
daemons.forEach(daemon => {
  daemon.schedule(distributedOp);
});

// Listen for completion on all nodes
daemons.forEach((daemon, idx) => {
  daemon.on('operation:success', (event) => {
    console.log(`[Node ${idx + 1}] ✓ ${event.operationId}`);
  });
});

// Execute operation on all nodes (demonstrates distribution)
console.log('Executing on all 3 nodes...');
const results = await Promise.all(
  daemons.map(d => d.execute('process-batch').catch(e => ({ error: e.message })))
);

console.log('\nResults:');
results.forEach((result, idx) => {
  console.log(`  Node ${idx + 1}:`, result);
});

// Show health of each node
console.log('\nCluster Health:');
daemons.forEach((daemon, idx) => {
  const health = daemon.getHealth();
  console.log(`  Node ${idx + 1}: running=${health.isRunning}, active=${health.activeOperations}`);
});

// Cleanup
await Promise.all(daemons.map(d => d.stop()));
```

**Key concepts introduced:**
- Multiple daemon instances
- Cluster coordination
- Distributed operation execution
- Health monitoring across cluster

---

## Summary

You've learned the core patterns of @unrdf/daemon:

1. **Basic Setup** - Create, start, and execute operations
2. **Interval Scheduling** - Execute operations repeatedly
3. **Cron Scheduling** - Complex time-based schedules
4. **Event-Driven Workflows** - Chain operations via events
5. **Clustering** - Coordinate across multiple nodes

## Next Steps

- Read the [How-To Guide](./how-to.md) for specific task solutions
- Check [API Reference](./reference.md) for detailed method documentation
- Explore [Explanation Guide](./explanation.md) for architectural insights
- Review [Examples](../examples/) for more advanced patterns

## Common Questions

**Q: How do I persist scheduled operations?**
A: Implement a custom storage layer that saves/loads operation definitions on startup. Use the `schedule()` method in a loop during initialization.

**Q: Can I use database queries as handlers?**
A: Yes! Any async function works. Pass your database operations directly as the `handler`.

**Q: How does clustering handle leader election?**
A: Use the distributed integration with a Raft coordinator. See examples/02-distributed-cluster.mjs for details.
