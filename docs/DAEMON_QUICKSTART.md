# @unrdf/daemon — Quick Start Guide

> **Status**: Beta | **Version**: 26.4.4 | **Node**: >= 18.0.0

The daemon is UNRDF's background task scheduler. It handles scheduled operations, event-driven execution, enterprise security (BLAKE3 auth), multi-node clustering, and YAWL workflow orchestration.

---

## Installation

```bash
pnpm add @unrdf/daemon
```

---

## 1. Basic Lifecycle

```javascript
import { Daemon } from '@unrdf/daemon';

const daemon = new Daemon({
  daemonId: '550e8400-e29b-41d4-a716-446655440000', // UUID required
  name: 'My Daemon',
  concurrency: 5,
  port: 8080,
  logLevel: 'info',
});

// Start (idempotent — safe to call multiple times)
await daemon.start();

// ... schedule and execute tasks ...

// Stop gracefully (idempotent)
await daemon.stop();
```

### Lifecycle Events

```javascript
daemon.on('daemon:started', e => console.log('Started:', e.nodeId));
daemon.on('daemon:stopped', e => console.log('Stopped:', e.nodeId));
```

### Health Check

```javascript
const health = daemon.getHealth();
// { isRunning, uptime, activeOperations, queuedOperations, completedOperations, ... }

const metrics = daemon.getMetrics();
// { totalOperations, successRate, averageDuration, ... }
```

---

## 2. Task Scheduling

### Register a Task

```javascript
daemon.schedule({
  id: 'daily-cleanup',
  name: 'Daily Cleanup',
  handler: async () => {
    const count = await cleanStaleRecords();
    return { cleaned: count };
  },
  metadata: { tags: ['maintenance'], priority: 'low' },
});
```

### Execute Immediately

```javascript
const result = await daemon.execute('daily-cleanup');
console.log(result); // { cleaned: 42 }
```

### List and Remove Tasks

```javascript
const ops = daemon.listOperations(); // all scheduled tasks
const removed = daemon.unschedule('daily-cleanup'); // true | false
```

### Task Events

```javascript
daemon.on('operation:enqueued', e => console.log(`Queued: ${e.operationId}`));
daemon.on('operation:started', e => console.log(`Started: ${e.operationId}`));
daemon.on('operation:success', e => console.log(`Done: ${e.operationId} (${e.duration}ms)`));
daemon.on('operation:failure', e => console.error(`Failed: ${e.operationId}`, e.error));
```

---

## 3. Configuration Reference

```javascript
new Daemon({
  // Identity (required)
  daemonId: 'uuid-v4',
  name: 'Descriptive Name',

  // Network
  port: 8080, // 1024–65535, default: 8080

  // Concurrency
  concurrency: 10, // 1–100, default: 10

  // Health
  healthCheckIntervalMs: 30000, // default: 30s
  metricsRetentionMs: 3600000, // default: 1 hour

  // Clustering (optional)
  nodeId: 'node-1',
  clusterId: 'my-cluster',

  // Logging
  logLevel: 'info', // debug | info | warn | error

  // Retry policy
  globalRetryPolicy: {
    maxAttempts: 3,
    backoffMs: 1000,
    backoffMultiplier: 2,
    maxBackoffMs: 30000,
    jitterFactor: 0.1,
  },
});
```

All configuration is validated with Zod at construction time. Invalid values throw immediately.

### Environment Variables

```bash
UNRDF_API_KEY=hex_key_32_to_128_chars   # API authentication
RATE_LIMIT_MAX_REQUESTS=100              # Per-window request limit
RATE_LIMIT_WINDOW_MS=60000               # Rate limit window
NODE_ENV=production                      # Enables strict security
```

---

## 4. Security: API Key Authentication

The daemon uses BLAKE3 hashing with constant-time comparison to prevent timing attacks.

### Generate and Verify Keys

```javascript
import { createAuthenticator, generateSecureApiKey } from '@unrdf/daemon';

// Generate a key pair
const { key, hash } = await generateSecureApiKey();
// Store `key` in secrets manager, `hash` in config

// Create authenticator
const { authenticator } = await createAuthenticator({
  environment: 'production',
  keyHash: hash,
});

// Verify incoming request
const result = await authenticator.authenticate({
  headers: { 'x-api-key': incomingKey },
});

if (result.authenticated) {
  // proceed
} else {
  console.error('Denied:', result.reason);
}
```

### Dev vs Production

| Mode          | Missing Key   | Invalid Key |
| ------------- | ------------- | ----------- |
| `development` | Warns, allows | Rejects     |
| `production`  | Rejects       | Rejects     |

### Audit Log

```javascript
const log = authenticator.getAuditLog(); // up to 1000 entries
// [{ timestamp, status, reason }, ...]
```

---

## 5. YAWL Workflow Bridge

The daemon integrates with YAWL for complex, multi-step workflow orchestration.

### Setup

```javascript
import { createYawlBridge } from '@unrdf/daemon/integrations/yawl';
import { WorkflowEngine } from '@unrdf/yawl';

const bridge = createYawlBridge(daemon, new WorkflowEngine(), {
  daemonNodeId: 'node-1',
  maxConcurrentCases: 100,
  enableAutoRetry: true,
  retryPolicy: {
    maxAttempts: 3,
    backoffMs: 1000,
    backoffMultiplier: 2,
    maxBackoffMs: 30000,
  },
  timeoutDefaults: {
    taskTimeoutMs: 30000,
    caseTimeoutMs: 3600000,
    checkIntervalMs: 5000,
  },
});

await bridge.start();
```

### Schedule Recurring Workflows

```javascript
await bridge.scheduleRecurringCase('approval-workflow', '0 * * * *', {
  caseIdPrefix: 'batch',
  priority: 5,
  inputData: { batchSize: 100 },
});
```

### Timeouts, Retries, and Distribution

```javascript
// Auto-cancel if task exceeds 60s
await bridge.watchTaskTimeout('case-001', 'review-task', 60000);

// Retry with exponential backoff
await bridge.scheduleRetry('case-001', 'process-task', {
  maxAttempts: 5,
  backoffMs: 2000,
  backoffMultiplier: 2,
});

// Distribute tasks across cluster nodes
await bridge.distributeAndSplitTasks(
  'case-001',
  ['task-a', 'task-b', 'task-c'],
  { strategy: 'least-loaded' } // or: round-robin, random, affinity
);
```

### Deferred Choice (Wait for External Event)

```javascript
await bridge.waitForChoiceTrigger('case-001', 'deferred-choice', {
  eventName: 'user:approved',
  filter: { userId: 'user-123' },
  timeoutMs: 3600000,
});
```

### Bridge Stats

```javascript
const stats = bridge.getStats();
// { isRunning, caseSchedules, activeTimeouts, activeRetries, activeTriggers, ... }

await bridge.stop();
```

---

## 6. Complete Example

```javascript
import { Daemon, createYawlBridge } from '@unrdf/daemon';
import { WorkflowEngine } from '@unrdf/yawl';

// Create and start
const daemon = new Daemon({
  daemonId: '550e8400-e29b-41d4-a716-446655440000',
  name: 'Production Daemon',
  concurrency: 10,
});

daemon.on('operation:success', e => console.log(`Done: ${e.operationId}`));
daemon.on('operation:failure', e => console.error(`Fail: ${e.operationId}`));

await daemon.start();

// Schedule tasks
daemon.schedule({
  id: 'hourly-sync',
  handler: async () => ({ synced: true }),
});

await daemon.execute('hourly-sync');

// Wire up YAWL
const bridge = createYawlBridge(daemon, new WorkflowEngine(), {
  daemonNodeId: daemon.nodeId,
  maxConcurrentCases: 50,
});
await bridge.start();

await bridge.scheduleRecurringCase('daily-report', '0 9 * * *', {
  caseIdPrefix: 'report',
});

// Health monitoring
setInterval(() => {
  const h = daemon.getHealth();
  console.log(`Active: ${h.activeOperations}, Queued: ${h.queuedOperations}`);
}, 30000);

// Graceful shutdown
process.on('SIGTERM', async () => {
  await bridge.stop();
  await daemon.stop();
});
```

---

## 7. Deployment

### Docker

```dockerfile
FROM node:18-alpine
WORKDIR /app
COPY package.json pnpm-lock.yaml ./
RUN pnpm install --prod
COPY . .
ENV NODE_ENV=production
EXPOSE 8080
CMD ["node", "app.mjs"]
```

### Kubernetes (StatefulSet)

```yaml
apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: unrdf-daemon
spec:
  serviceName: daemon
  replicas: 3
  template:
    spec:
      containers:
        - name: daemon
          image: unrdf/daemon:latest
          ports:
            - containerPort: 8080
          env:
            - name: UNRDF_API_KEY
              valueFrom:
                secretKeyRef:
                  name: daemon-secrets
                  key: api-key
            - name: NODE_ENV
              value: 'production'
```

### Production Checklist

- [ ] API key authentication enabled (`UNRDF_API_KEY`)
- [ ] Rate limiting configured
- [ ] `NODE_ENV=production`
- [ ] Health check endpoint monitored
- [ ] Audit logging enabled
- [ ] Graceful shutdown handler (`SIGTERM`)

---

## CLI Reference

```bash
unrdf daemon list              # List all scheduled operations
unrdf daemon run <task-id>     # Execute a task immediately
unrdf daemon status            # Health and metrics
unrdf daemon config            # View current configuration
unrdf daemon cluster           # Cluster node status
```

---

## Further Reading

| Document                 | Location                                         |
| ------------------------ | ------------------------------------------------ |
| Full API reference       | `packages/daemon/docs/reference.md`              |
| Architecture explanation | `packages/daemon/docs/explanation.md`            |
| YAWL integration guide   | `packages/daemon/docs/yawl-integration-guide.md` |
| Security hardening       | `packages/daemon/docs/security-hardening.md`     |
| Production deployment    | `packages/daemon/docs/production-deployment.md`  |
| Operational runbooks     | `packages/daemon/docs/operational-runbooks.md`   |
