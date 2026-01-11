# How-to: YAWL Daemon Management

**Estimated Time:** 5-10 minutes
**Prerequisites:** Completed [YAWL Daemon Setup Tutorial](../tutorials/yawl-daemon-setup.md)

Quick reference guide for common YAWL daemon management tasks.

---

## Start and Stop Daemon

### Start Daemon Programmatically

```javascript
import { Daemon } from '@unrdf/daemon';
import { YawlDaemonBridge } from '@unrdf/daemon/integrations/yawl';

const daemon = new Daemon({ daemonId: 'my-daemon', name: 'My Daemon' });
const bridge = new YawlDaemonBridge(daemon, yawlEngine, {
  daemonNodeId: 'node-1',
});

await daemon.start();
await bridge.start();
```

### Stop Daemon Gracefully

```javascript
await bridge.stop();
await daemon.stop();
```

**Result:** Daemon shuts down, cleans up resources, and completes pending operations.

---

## Schedule Recurring Case Creation

### Daily at Specific Time

```javascript
// Every day at 2 AM
await bridge.scheduleRecurringCase(
  'approval-workflow',
  '0 2 * * *',
  { caseIdPrefix: 'daily', priority: 5 }
);
```

### Hourly

```javascript
// Every hour
await bridge.scheduleRecurringCase(
  'hourly-sync',
  '0 * * * *',
  { caseIdPrefix: 'hourly' }
);
```

### Every N Minutes

```javascript
// Every 15 minutes
await bridge.scheduleRecurringCase(
  'frequent-task',
  '*/15 * * * *',
  { caseIdPrefix: 'frequent' }
);
```

### Custom Weekday Schedule

```javascript
// Monday-Friday at 9 AM
await bridge.scheduleRecurringCase(
  'weekday-workflow',
  '0 9 * * 1-5',
  { caseIdPrefix: 'weekday' }
);
```

**Cron Format:** `minute hour day month weekday`
- `*` = any value
- `*/n` = every n units
- `1-5` = range (Monday-Friday)

---

## Configure Task Timeouts

### Set Timeout for Specific Task

```javascript
await bridge.watchTaskTimeout(
  'case-001',      // Case ID
  'review-task',   // Task ID
  60000            // Timeout: 60 seconds
);
```

### Configure Default Timeout

```javascript
const bridge = new YawlDaemonBridge(daemon, yawlEngine, {
  enableTimeoutTracking: true,
  timeoutDefaults: {
    taskTimeoutMs: 300000,    // 5 minutes
    caseTimeoutMs: 3600000,   // 1 hour
    checkIntervalMs: 10000,   // Check every 10 seconds
  },
});
```

### Disable Timeout Tracking

```javascript
const bridge = new YawlDaemonBridge(daemon, yawlEngine, {
  enableTimeoutTracking: false, // Disable automatic timeout enforcement
});
```

---

## Configure Retry Policies

### Custom Retry Policy for Bridge

```javascript
const bridge = new YawlDaemonBridge(daemon, yawlEngine, {
  enableAutoRetry: true,
  retryPolicy: {
    maxAttempts: 5,          // Retry up to 5 times
    backoffMs: 2000,         // Start with 2 second delay
    backoffMultiplier: 2,    // Double each time (2s, 4s, 8s, 16s, 32s)
    maxBackoffMs: 60000,     // Cap at 60 seconds
    jitterFactor: 0.2,       // Add 20% random jitter
  },
});
```

### Manual Retry for Specific Task

```javascript
await bridge.scheduleRetry('case-001', 'failed-task', {
  maxAttempts: 3,
  backoffMs: 1000,
});
```

### Disable Automatic Retry

```javascript
const bridge = new YawlDaemonBridge(daemon, yawlEngine, {
  enableAutoRetry: false, // Manual retry only
});
```

---

## Monitor Health and Metrics

### Get Daemon Health

```javascript
const health = daemon.getHealth();

console.log('Status:', health.isRunning);
console.log('Uptime:', health.uptime);
console.log('Active Operations:', health.activeOperations);
console.log('Completed Operations:', health.completedOperations);
```

### Get Performance Metrics

```javascript
const metrics = daemon.getMetrics();

console.log('Total Operations:', metrics.totalOperations);
console.log('Success Rate:', metrics.successRate);
console.log('Avg Duration:', metrics.averageDuration);
```

### Get Bridge Statistics

```javascript
const stats = bridge.getStats();

console.log('Bridge ID:', stats.bridgeId);
console.log('Case Schedules:', stats.caseSchedules);
console.log('Active Timeouts:', stats.activeTimeouts);
console.log('Active Retries:', stats.activeRetries);
```

### Listen to Health Events

```javascript
daemon.on('operation:success', (event) => {
  console.log(`Success: ${event.operationId} in ${event.duration}ms`);
});

daemon.on('operation:failure', (event) => {
  console.error(`Failed: ${event.operationId} - ${event.error}`);
});

bridge.on('task:timeout-enforced', (event) => {
  console.warn(`Timeout: ${event.taskId} in case ${event.caseId}`);
});

bridge.on('task:retry-exhausted', (event) => {
  console.error(`Retry exhausted: ${event.taskId} after ${event.attempts} attempts`);
});
```

---

## Use CLI Commands

### List Scheduled Operations

```bash
unrdf daemon list
```

**Output:**
```
📋 Configured Operations
══════════════════════════════════════════════════════════════════════
ID                        Name                           Status
──────────────────────────────────────────────────────────────────────
backup-graphs             Backup RDF Graphs              scheduled
cleanup-temp              Cleanup Temporary Files        scheduled
sync-federation           Synchronize Federation Nodes   scheduled
──────────────────────────────────────────────────────────────────────
Total Operations: 3
```

### Execute Operation Immediately

```bash
unrdf daemon run backup-graphs
```

**Output:**
```
✅ Operation executed successfully
══════════════════════════════════════════════
Operation ID: backup-graphs
Status: success
Duration: 234ms
Processed: 42 items
Affected: 15 items
══════════════════════════════════════════════
```

### Schedule New Trigger

```bash
unrdf daemon schedule sync-federation cron --payload '{"schedule":"0 * * * *"}'
```

### View Daemon Status

```bash
unrdf daemon status --include-metrics
```

**Output:**
```
⚡ Daemon Status
══════════════════════════════════════════════
Node ID: node-default
Cluster ID: default-cluster
Running: ✅ Yes
Leader: 👑 Yes
Uptime: 2.5h
Active Operations: 2
Queued Operations: 5
Completed Operations: 156
══════════════════════════════════════════════

📊 Metrics
──────────────────────────────────────────────
Total Operations: 156
Successful: 148
Failed: 8
Success Rate: 94.87%
Average Duration: 245ms
Total Duration: 38.22s
══════════════════════════════════════════════
```

### View Operation Logs

```bash
# View last 50 logs
unrdf daemon logs --max-lines 50

# Filter logs by pattern
unrdf daemon logs --filter "yawl"

# Follow logs in real-time
unrdf daemon logs --follow
```

### Show Configuration

```bash
unrdf daemon config
```

### Show Cluster Status

```bash
unrdf daemon cluster --include-metrics
```

---

## Distribute Parallel Tasks

### Round-Robin Distribution

```javascript
await bridge.distributeAndSplitTasks(
  'case-001',
  ['review-a', 'review-b', 'review-c'],
  { strategy: 'round-robin' }
);
```

**Result:** Tasks distributed evenly across daemon nodes.

### Least-Loaded Distribution

```javascript
await bridge.distributeAndSplitTasks(
  'case-001',
  ['task-1', 'task-2', 'task-3'],
  { strategy: 'least-loaded' }
);
```

**Result:** Tasks assigned to nodes with least active operations.

### Random Distribution

```javascript
await bridge.distributeAndSplitTasks(
  'case-001',
  ['task-a', 'task-b', 'task-c'],
  { strategy: 'random' }
);
```

**Result:** Tasks randomly distributed across nodes.

---

## Handle Deferred Choices

### Wait for External Event

```javascript
const triggerPromise = bridge.waitForChoiceTrigger(
  'case-001',
  'approval-choice',
  {
    eventName: 'user:approved',
    filter: { userId: 'user-123' },
    timeoutMs: 3600000, // 1 hour timeout
  }
);

// Later, when event occurs, resolve the trigger
const trigger = bridge.choiceTriggers.get('case-001:approval-choice');
if (trigger?.resolve) {
  trigger.resolve({
    eventName: 'user:approved',
    decision: 'APPROVE',
  });
}

await triggerPromise;
```

**Result:** Workflow waits for external event before proceeding.

---

## Clean Up Resources

### Unschedule Operation

```javascript
daemon.unschedule('operation-id');
```

### Clear Timeout Watch

```javascript
const timeout = bridge.taskTimeouts.get('case-001:task-001');
if (timeout?.operationId) {
  daemon.unschedule(timeout.operationId);
  bridge.taskTimeouts.delete('case-001:task-001');
}
```

### Remove Retry Schedule

```javascript
const retry = bridge.taskRetries.get('case-001:task-001');
if (retry?.operationId) {
  daemon.unschedule(retry.operationId);
  bridge.taskRetries.delete('case-001:task-001');
}
```

---

## Debug Common Issues

### Check Active Operations

```javascript
const operations = daemon.listOperations();
operations.forEach(op => {
  console.log(`${op.id}: ${op.status}`);
});
```

### Verify Bridge Configuration

```javascript
console.log('Bridge Config:', {
  bridgeId: bridge.id,
  enableAutoRetry: bridge.config.enableAutoRetry,
  enableTimeoutTracking: bridge.config.enableTimeoutTracking,
  maxConcurrentCases: bridge.config.maxConcurrentCases,
});
```

### Check Event Listeners

```javascript
console.log('YAWL listeners:', yawlEngine.listenerCount('task:failed'));
console.log('Daemon listeners:', daemon.listenerCount('operation:failure'));
```

---

## Best Practices

### 1. Always Use Graceful Shutdown

```javascript
process.on('SIGINT', async () => {
  console.log('Shutting down gracefully...');
  await bridge.stop();
  await daemon.stop();
  process.exit(0);
});
```

### 2. Set Reasonable Timeouts

```javascript
// Too short (not recommended)
taskTimeoutMs: 1000 // 1 second - may cause premature cancellations

// Reasonable
taskTimeoutMs: 60000 // 1 minute - good for most tasks

// Too long (not recommended)
taskTimeoutMs: 3600000 // 1 hour - may hide stuck tasks
```

### 3. Configure Retry with Backoff

```javascript
// Good: Exponential backoff with jitter
retryPolicy: {
  maxAttempts: 3,
  backoffMs: 1000,
  backoffMultiplier: 2,
  jitterFactor: 0.1,
}

// Bad: Fixed delay, no jitter
retryPolicy: {
  maxAttempts: 10,
  backoffMs: 1000,
  backoffMultiplier: 1, // No exponential backoff
  jitterFactor: 0,      // No jitter (thundering herd risk)
}
```

### 4. Monitor Health Regularly

```javascript
setInterval(() => {
  const health = daemon.getHealth();
  if (!health.isRunning) {
    console.error('Daemon is not running!');
  }
  if (health.activeOperations > 100) {
    console.warn('High operation load:', health.activeOperations);
  }
}, 30000); // Check every 30 seconds
```

### 5. Use Structured Logging

```javascript
const logger = {
  debug: (msg, meta) => console.debug(JSON.stringify({ level: 'debug', msg, ...meta })),
  info: (msg, meta) => console.log(JSON.stringify({ level: 'info', msg, ...meta })),
  warn: (msg, meta) => console.warn(JSON.stringify({ level: 'warn', msg, ...meta })),
  error: (msg, meta) => console.error(JSON.stringify({ level: 'error', msg, ...meta })),
};

const daemon = new Daemon({
  daemonId: 'my-daemon',
  logger,
});
```

---

## Quick Reference

### Cron Schedule Examples

| Schedule | Cron | Description |
|----------|------|-------------|
| Every hour | `0 * * * *` | At minute 0 |
| Every day at 2 AM | `0 2 * * *` | 2:00 AM daily |
| Every 15 minutes | `*/15 * * * *` | :00, :15, :30, :45 |
| Weekdays at 9 AM | `0 9 * * 1-5` | Mon-Fri at 9:00 AM |
| First day of month | `0 0 1 * *` | Midnight on 1st |

### Distribution Strategies

| Strategy | Use Case | Load Pattern |
|----------|----------|--------------|
| `round-robin` | Even distribution | Tasks distributed sequentially |
| `least-loaded` | Load balancing | Tasks to node with fewest active ops |
| `random` | Simple spread | Random distribution |
| `affinity` | Data locality | Tasks grouped by affinity key |

### Retry Backoff Calculator

```javascript
// Calculate retry delays
function calculateRetryDelays(policy) {
  const delays = [];
  for (let i = 0; i < policy.maxAttempts; i++) {
    const delay = Math.min(
      policy.backoffMs * Math.pow(policy.backoffMultiplier, i),
      policy.maxBackoffMs
    );
    delays.push(delay);
  }
  return delays;
}

// Example: { maxAttempts: 5, backoffMs: 1000, backoffMultiplier: 2, maxBackoffMs: 30000 }
// Result: [1000, 2000, 4000, 8000, 16000] ms
```

---

## See Also

- [Tutorial: YAWL Daemon Setup](../tutorials/yawl-daemon-setup.md) - Complete setup guide
- [Reference: YAWL Daemon API](../reference/yawl-daemon-api.md) - Full API documentation
- [Example: YAWL Daemon Workflow](../../examples/yawl-daemon-workflow.mjs) - Production example
