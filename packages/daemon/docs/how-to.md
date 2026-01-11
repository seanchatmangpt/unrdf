# How-To Guide: @unrdf/daemon

Practical solutions for common tasks (5-10 minutes per task).

## 1. How to Schedule an Operation

**Problem:** I need to run a task at specific times without restarting the daemon.

**Solution:**

```javascript
import { Daemon } from '@unrdf/daemon';

const daemon = new Daemon({ id: 'my-daemon' });

// Define your operation
const sendReport = {
  id: 'send-report-op',
  name: 'Send Daily Report',
  handler: async () => {
    // Your business logic here
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

// Schedule it
daemon.schedule(sendReport);

// Execute immediately
await daemon.execute('send-report-op');

// Or list all scheduled operations
console.log(daemon.listOperations());
// Output: [
//   {
//     id: 'send-report-op',
//     name: 'Send Daily Report',
//     status: 'scheduled',
//     createdAt: 2025-01-10T10:30:00Z,
//     metadata: { priority: 'high', retryable: true }
//   }
// ]

await daemon.stop();
```

**Key Points:**
- Operations must have unique `id` and an async `handler` function
- Use `metadata` to store operational context
- Schedule operations before executing them
- `listOperations()` returns current scheduled operations

---

## 2. How to Listen to Completion Events

**Problem:** I need to react when operations complete (success or failure).

**Solution:**

```javascript
import { Daemon } from '@unrdf/daemon';

const daemon = new Daemon({ id: 'my-daemon' });

const operation = {
  id: 'data-import',
  name: 'Import Data',
  handler: async () => {
    // Simulate work
    await new Promise(r => setTimeout(r, 500));
    return { imported: 1000, status: 'success' };
  },
};

// Listen to lifecycle events
daemon.on('daemon:started', (event) => {
  console.log(`📍 Daemon started at ${event.timestamp}`);
});

daemon.on('operation:enqueued', (event) => {
  console.log(`📋 Operation enqueued: ${event.name}`);
});

daemon.on('operation:started', (event) => {
  console.log(`▶️  Operation started: ${event.name}`);
});

daemon.on('operation:success', (event) => {
  console.log(`✓ Operation succeeded: ${event.name} (${event.duration}ms)`);
  // Do something after success
  console.log(`  Processed ${event.name}`);
});

daemon.on('operation:failure', (event) => {
  console.log(`✗ Operation failed: ${event.name}`);
  console.log(`  Error: ${event.error}`);
  // Handle failure
  sendAlert(`Operation ${event.name} failed: ${event.error}`);
});

daemon.on('daemon:stopped', (event) => {
  console.log(`🛑 Daemon stopped`);
});

await daemon.start();
daemon.schedule(operation);

try {
  const result = await daemon.execute('data-import');
  console.log('Result:', result);
} catch (e) {
  console.error('Execution error:', e.message);
}

await daemon.stop();
```

**Available Events:**
- `daemon:started` - Daemon initialization complete
- `operation:enqueued` - Operation added to queue
- `operation:started` - Operation execution started
- `operation:success` - Operation completed successfully
- `operation:failure` - Operation failed with error
- `daemon:stopped` - Daemon shut down

**Event Object Properties:**
- `operationId` - Unique operation identifier
- `name` - Human-readable operation name
- `timestamp` - When event occurred
- `duration` - Execution time (success/failure events)
- `error` - Error message (failure events only)

---

## 3. How to Implement Graceful Shutdown

**Problem:** I need to cleanly shut down the daemon and finish in-flight operations.

**Solution:**

```javascript
import { Daemon } from '@unrdf/daemon';

const daemon = new Daemon({
  id: 'graceful-daemon',
  logger: console,
});

// Track active operations
let activeOps = 0;

const longRunning = {
  id: 'process-large-file',
  name: 'Process Large File',
  handler: async () => {
    activeOps++;
    console.log(`Active operations: ${activeOps}`);

    try {
      // Simulate long work with cancellation support
      await new Promise((resolve, reject) => {
        const timeout = setTimeout(() => {
          resolve({ status: 'completed', records: 50000 });
        }, 3000);

        // Check for cancellation signal
        if (daemon.shuttingDown) {
          clearTimeout(timeout);
          reject(new Error('Shutdown in progress'));
        }
      });
    } finally {
      activeOps--;
    }
  },
};

// Handle shutdown signals
process.on('SIGTERM', async () => {
  console.log('🛑 SIGTERM received, starting graceful shutdown...');
  daemon.shuttingDown = true;

  // Wait a bit for in-flight operations
  await new Promise(r => setTimeout(r, 2000));

  // Stop daemon
  await daemon.stop();
  console.log('✓ Daemon stopped gracefully');
  process.exit(0);
});

process.on('SIGINT', async () => {
  console.log('🛑 SIGINT received (Ctrl+C), starting graceful shutdown...');
  daemon.shuttingDown = true;

  // Stop accepting new operations
  const health = daemon.getHealth();
  console.log(`Waiting for ${health.activeOperations} active operations...`);

  // Wait for completion
  const maxWait = 10000;
  const startTime = Date.now();
  while (activeOps > 0 && Date.now() - startTime < maxWait) {
    await new Promise(r => setTimeout(r, 100));
  }

  await daemon.stop();
  console.log('✓ Shutdown complete');
  process.exit(0);
});

// Shutdown timeout (force exit after 30s)
setTimeout(() => {
  console.error('❌ Graceful shutdown timeout, forcing exit');
  process.exit(1);
}, 30000);

// Start daemon
await daemon.start();
daemon.schedule(longRunning);

// Simulate some operations
for (let i = 0; i < 3; i++) {
  daemon.execute('process-large-file').catch(e => {
    if (e.message !== 'Shutdown in progress') {
      console.error('Operation error:', e.message);
    }
  });
  await new Promise(r => setTimeout(r, 500));
}

// Daemon runs until SIGTERM/SIGINT received
```

**Best Practices:**
1. Set `daemon.shuttingDown` flag to signal operations
2. Wait for in-flight operations to complete
3. Set reasonable timeout (10-30 seconds)
4. Force exit if timeout exceeded
5. Log shutdown progress for debugging

---

## 4. How to Monitor Daemon Health

**Problem:** I need to track daemon performance and detect problems.

**Solution:**

```javascript
import { Daemon } from '@unrdf/daemon';

const daemon = new Daemon({
  id: 'monitored-daemon',
  logger: console,
});

// Define some test operations
const operations = [
  {
    id: 'op-success',
    name: 'Quick Success',
    handler: async () => ({ status: 'ok' }),
  },
  {
    id: 'op-slow',
    name: 'Slow Operation',
    handler: async () => {
      await new Promise(r => setTimeout(r, 500));
      return { status: 'ok', duration: 500 };
    },
  },
  {
    id: 'op-failure',
    name: 'Failing Operation',
    handler: async () => {
      throw new Error('Intentional failure');
    },
  },
];

// Health monitoring function
function printHealth(daemon) {
  const health = daemon.getHealth();
  const metrics = daemon.getMetrics();

  console.log('\n=== DAEMON HEALTH ===');
  console.log(`Status: ${health.isRunning ? '🟢 Running' : '🔴 Stopped'}`);
  console.log(`Node ID: ${health.nodeId}`);
  console.log(`Cluster ID: ${health.clusterId}`);
  console.log(`Uptime: ${Math.round(health.uptime / 1000)}s`);
  console.log(`Active Operations: ${health.activeOperations}`);
  console.log(`Queued Operations: ${health.queuedOperations}`);
  console.log(`Completed Operations: ${health.completedOperations}`);

  console.log('\n=== METRICS ===');
  if (metrics) {
    console.log(`Total Executed: ${metrics.totalOperations}`);
    console.log(`Successful: ${metrics.successfulOperations}`);
    console.log(`Failed: ${metrics.failedOperations}`);
    console.log(`Success Rate: ${metrics.successRate.toFixed(1)}%`);
    console.log(`Avg Duration: ${metrics.averageDuration.toFixed(2)}ms`);
  }

  console.log('\n=== SCHEDULED OPERATIONS ===');
  const ops = daemon.listOperations();
  if (ops.length === 0) {
    console.log('(none)');
  } else {
    ops.forEach(op => {
      console.log(`  - ${op.name} (${op.status})`);
    });
  }
}

// Optional: Periodic health checks
let healthCheckInterval;

async function startHealthMonitoring(daemon, intervalMs = 5000) {
  healthCheckInterval = setInterval(() => {
    printHealth(daemon);
  }, intervalMs);
}

function stopHealthMonitoring() {
  if (healthCheckInterval) {
    clearInterval(healthCheckInterval);
  }
}

// Demo
await daemon.start();
operations.forEach(op => daemon.schedule(op));

// Print initial health
printHealth(daemon);

// Start periodic monitoring
startHealthMonitoring(daemon, 3000);

// Execute some operations
console.log('\n📌 Executing operations...\n');
for (const op of operations) {
  try {
    await daemon.execute(op.id);
  } catch (e) {
    console.log(`(Expected error: ${e.message})`);
  }
  await new Promise(r => setTimeout(r, 1000));
}

// Final health check
console.log('\n📌 Final health check:\n');
printHealth(daemon);

stopHealthMonitoring();
await daemon.stop();
```

**Health Metrics Available:**
- `isRunning` - Daemon active status
- `activeOperations` - Currently executing
- `queuedOperations` - Waiting in queue
- `completedOperations` - Total completed
- `uptime` - Running duration (ms)
- `totalOperations` - All executed
- `successfulOperations` - Count of successes
- `failedOperations` - Count of failures
- `successRate` - Percentage success (0-100)
- `averageDuration` - Mean execution time (ms)

---

## 5. How to Troubleshoot Failures

**Problem:** Operations are failing silently or with unclear errors.

**Solution:**

```javascript
import { Daemon } from '@unrdf/daemon';

const daemon = new Daemon({
  id: 'debug-daemon',
  logger: console, // Use debug logger
});

// Operation with detailed error handling
const problematicOp = {
  id: 'risky-operation',
  name: 'Operation with Error Handling',
  handler: async () => {
    console.log('[Handler] Starting...');

    // Validate inputs
    if (!process.env.API_KEY) {
      throw new Error('Missing required env: API_KEY');
    }

    try {
      // Simulate work that might fail
      const result = await fetchDataWithTimeout(5000);
      console.log('[Handler] Result:', result);
      return result;
    } catch (e) {
      // Re-throw with context
      console.error('[Handler] Caught error:', e);
      throw new Error(`Data fetch failed: ${e.message}`);
    }
  },
};

// Helper with timeout
async function fetchDataWithTimeout(timeoutMs) {
  return Promise.race([
    fetch('https://api.example.com/data').then(r => r.json()),
    new Promise((_, reject) =>
      setTimeout(() => reject(new Error('Timeout')), timeoutMs)
    ),
  ]);
}

// Detailed event handlers for debugging
daemon.on('operation:started', (event) => {
  console.log(`[STARTED] ${event.operationId}`);
});

daemon.on('operation:failure', (event) => {
  console.error(`[FAILED] ${event.operationId}`);
  console.error(`  Error: ${event.error}`);
  console.error(`  Duration: ${event.duration}ms`);

  // Additional diagnostics
  const opDef = daemon.operations.get(event.operationId);
  if (opDef && opDef.metadata) {
    console.error(`  Metadata:`, opDef.metadata);
  }
});

daemon.on('operation:success', (event) => {
  console.log(`[SUCCESS] ${event.operationId} - ${event.duration}ms`);
});

// Helper: Get operation execution history
function getOperationHistory(daemon, operationId) {
  const completed = daemon.completedOperations.entries();
  return completed.filter(([key]) => key === operationId).map(([_, record]) => record);
}

// Helper: Diagnose operation
function diagnoseOperation(daemon, operationId) {
  const operation = daemon.operations.get(operationId);
  const history = getOperationHistory(daemon, operationId);

  console.log('\n=== OPERATION DIAGNOSIS ===');
  console.log(`Operation: ${operation?.name || 'NOT FOUND'}`);
  console.log(`Total Executions: ${history.length}`);

  if (history.length > 0) {
    const recent = history[history.length - 1];
    console.log(`Last Status: ${recent.status}`);
    console.log(`Last Duration: ${recent.duration}ms`);
    if (recent.error) {
      console.log(`Last Error: ${recent.error}`);
    }
  }
}

// Test
await daemon.start();
daemon.schedule(problematicOp);

try {
  await daemon.execute('risky-operation');
} catch (e) {
  console.error('❌ Execution failed:', e.message);
  diagnoseOperation(daemon, 'risky-operation');
}

await daemon.stop();
```

**Debugging Checklist:**
1. ✓ Check `operation:failure` events for error messages
2. ✓ Enable console logging in handlers
3. ✓ Validate required environment variables before execution
4. ✓ Wrap database/API calls with timeout handling
5. ✓ Use `.getHealth()` and `.getMetrics()` to check daemon state
6. ✓ Review execution history in `completedOperations` cache
7. ✓ Check operation metadata for business context

**Common Issues:**
- **"Operation not found"** → Verify operation was scheduled with `schedule()`
- **"Timeout"** → Increase timeout or optimize handler code
- **"Missing env variable"** → Set environment variables before starting daemon
- **"Silent failures"** → Ensure `operation:failure` handler is registered
