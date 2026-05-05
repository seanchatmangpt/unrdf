# YAWL Integration Guide

**@unrdf/daemon + @unrdf/yawl Workflow Composition**

## Overview

The daemon+YAWL integration bridges scheduled operations with workflow orchestration, enabling:
- **Scheduled workflow execution**: Create cases on cron schedules or intervals
- **Timeout enforcement**: Automatically cancel tasks exceeding time limits
- **Automatic retry**: Exponential backoff for failed tasks
- **Parallel distribution**: Load-balanced task execution across nodes
- **Deferred choice**: Wait for external events to trigger workflow branches

This composition is production-ready and proven in enterprise environments.

---

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                     APPLICATION LAYER                            │
│  (Your code: schedule workflows, handle events, query state)    │
└───────────────────┬──────────────────────────────────┬──────────┘
                    │                                  │
                    ▼                                  ▼
    ┌───────────────────────────┐      ┌──────────────────────────┐
    │   YawlDaemonBridge        │◄────►│   YAWL Workflow Engine   │
    │  - Schedule cases         │      │  - Case management       │
    │  - Watch timeouts         │      │  - Task execution        │
    │  - Retry failures         │      │  - Event sourcing        │
    │  - Distribute tasks       │      │  - Cryptographic receipts│
    └──────────┬────────────────┘      └──────────────────────────┘
               │
               ▼
    ┌───────────────────────────┐
    │      Daemon Scheduler      │
    │  - Cron scheduling         │
    │  - Concurrency control     │
    │  - Health monitoring       │
    │  - Metrics collection      │
    └────────────────────────────┘
```

**Data flow:**
1. App schedules recurring workflow case creation via bridge
2. Daemon triggers case creation on schedule
3. YAWL engine creates case and enables initial tasks
4. Bridge watches task execution for timeouts
5. On failure, bridge schedules automatic retry
6. On success, YAWL emits receipts and enables next tasks

---

## When to Use Each Pattern

### Use Daemon Alone When:
- ✅ Simple scheduled operations (backups, cleanup, reports)
- ✅ Fire-and-forget tasks
- ✅ No complex control flow (no parallelism, branching, synchronization)
- ✅ Stateless operations

**Example:** Daily database backup at 2 AM.

### Use YAWL Alone When:
- ✅ Complex workflows (parallel/conditional routing)
- ✅ Human-in-the-loop approvals
- ✅ Audit requirements (cryptographic receipts)
- ✅ Manual trigger (not scheduled)

**Example:** Document approval workflow triggered by user submission.

### Use Daemon + YAWL When:
- ✅ **Scheduled workflows**: Run complex workflows on cron schedules
- ✅ **Timeout enforcement**: Cancel long-running workflow tasks
- ✅ **Automatic recovery**: Retry failed tasks with backoff
- ✅ **Distributed execution**: Parallel tasks across multiple nodes
- ✅ **Deferred choice**: Wait for external events in workflows

**Example:** Hourly batch processing with parallel validation tasks and retry on failure.

---

## API Quick Reference

### Setup

```javascript
import { Daemon } from '@unrdf/daemon';
import { createWorkflowEngine } from '@unrdf/yawl';
import { createYawlBridge } from '@unrdf/daemon/integrations/yawl';
import { createStore } from '@unrdf/oxigraph';

// 1. Create RDF store for workflow state
const store = createStore();

// 2. Create YAWL engine
const yawlEngine = createWorkflowEngine(store);

// 3. Create daemon
const daemon = new Daemon({
  daemonId: 'workflow-daemon',
  name: 'Workflow Scheduler',
  concurrency: 10,
});

// 4. Create bridge
const bridge = createYawlBridge(daemon, yawlEngine, {
  daemonNodeId: 'node-1',
  maxConcurrentCases: 100,
  enableAutoRetry: true,
  enableTimeoutTracking: true,
  retryPolicy: {
    maxAttempts: 3,
    backoffMs: 1000,
    backoffMultiplier: 2,
    maxBackoffMs: 30000,
    jitterFactor: 0.1,
  },
  timeoutDefaults: {
    taskTimeoutMs: 30000,  // 30s task timeout
    caseTimeoutMs: 3600000, // 1h case timeout
  },
});

// 5. Start daemon and bridge
await daemon.start();
await bridge.start();
```

### Core Operations

#### Schedule Recurring Case Creation

```javascript
// Create workflow case every hour
await bridge.scheduleRecurringCase(
  'batch-processing-workflow',
  '0 * * * *', // Cron: top of every hour
  {
    caseIdPrefix: 'batch',
    inputData: { priority: 'high', source: 'scheduled' },
  }
);
```

#### Watch Task Timeout

```javascript
// Auto-cancel task if exceeds 60s
await bridge.watchTaskTimeout('case-001', 'validation-task', 60000);
```

#### Schedule Automatic Retry

```javascript
// Retry failed task with exponential backoff
await bridge.scheduleRetry('case-001', 'api-call-task', {
  maxAttempts: 5,
  backoffMs: 2000,
  backoffMultiplier: 2,
  maxBackoffMs: 60000,
});
```

#### Distribute Parallel Tasks

```javascript
// Load-balance parallel tasks across nodes
await bridge.distributeAndSplitTasks(
  'case-001',
  ['task-a', 'task-b', 'task-c'],
  { strategy: 'least-loaded' }
);
```

#### Wait for External Trigger

```javascript
// Wait for user approval event before proceeding
await bridge.waitForChoiceTrigger('case-001', 'approval-task', {
  eventName: 'user:approved',
  filter: { caseId: 'case-001' },
  timeoutMs: 3600000, // 1 hour timeout
});
```

---

## Example Workflows

### 1. Scheduled Batch Processing

```javascript
import { createWorkflow, createCase, enableTask, completeTask } from '@unrdf/yawl';

// Define workflow
await createWorkflow(store, {
  id: 'batch-processing',
  name: 'Hourly Batch Processing',
  tasks: [
    { id: 'fetch-data', name: 'Fetch Data', kind: 'atomic' },
    { id: 'validate', name: 'Validate Records', kind: 'atomic' },
    { id: 'transform', name: 'Transform', kind: 'atomic' },
    { id: 'load', name: 'Load to DB', kind: 'atomic' },
  ],
  flow: [
    { from: 'fetch-data', to: 'validate' },
    { from: 'validate', to: 'transform' },
    { from: 'transform', to: 'load' },
  ],
});

// Schedule hourly execution
await bridge.scheduleRecurringCase('batch-processing', '0 * * * *', {
  caseIdPrefix: 'batch',
  inputData: { source: 'hourly-cron' },
});

// Bridge automatically creates cases every hour
// Daemon executes workflow via YAWL engine
```

### 2. Timeout-Enforced API Workflow

```javascript
// Workflow with external API calls
await createWorkflow(store, {
  id: 'api-integration',
  name: 'External API Integration',
  tasks: [
    { id: 'call-api-a', name: 'Call API A', kind: 'atomic' },
    { id: 'call-api-b', name: 'Call API B', kind: 'atomic' },
    { id: 'aggregate', name: 'Aggregate Results', kind: 'atomic' },
  ],
  flow: [
    { from: 'call-api-a', to: 'aggregate', splitType: 'AND' },
    { from: 'call-api-b', to: 'aggregate', splitType: 'AND' },
  ],
});

// Create case
const caseResult = await createCase(store, {
  workflowId: 'api-integration',
  caseId: 'api-case-001',
});

// Bridge automatically watches timeouts (if enableTimeoutTracking: true)
// Tasks exceeding 30s (timeoutDefaults.taskTimeoutMs) are auto-cancelled
```

### 3. Retry-Enabled Data Pipeline

```javascript
// Workflow with retry on failure
await createWorkflow(store, {
  id: 'data-pipeline',
  name: 'Data Pipeline with Retry',
  tasks: [
    { id: 'extract', name: 'Extract', kind: 'atomic' },
    { id: 'transform', name: 'Transform', kind: 'atomic' },
    { id: 'load', name: 'Load', kind: 'atomic' },
  ],
  flow: [
    { from: 'extract', to: 'transform' },
    { from: 'transform', to: 'load' },
  ],
});

// Bridge listens to task:failed events
// Automatically retries with exponential backoff (if enableAutoRetry: true)
```

---

## Troubleshooting

### Problem: Scheduled cases not created

**Symptoms:**
- Bridge starts successfully
- Cron schedule registered
- No case created at expected time

**Diagnosis:**
```javascript
// 1. Check bridge stats
const stats = bridge.getStats();
console.log('Case schedules:', stats.caseSchedules); // Should be > 0

// 2. Check daemon operations
const ops = daemon.listOperations();
console.log('Scheduled operations:', ops.length); // Should include case creation

// 3. Check daemon health
const health = daemon.getHealth();
console.log('Daemon running:', health.isRunning); // Should be true
```

**Solution:**
- Verify daemon is started: `await daemon.start()`
- Verify bridge is started: `await bridge.start()`
- Check cron syntax: Use `'0 * * * *'` for hourly (not `'@hourly'`)

### Problem: Task timeout not enforced

**Symptoms:**
- Task exceeds timeout threshold
- Task not cancelled
- No timeout event emitted

**Diagnosis:**
```javascript
// 1. Check timeout tracking enabled
console.log('Timeout tracking:', bridge.config.enableTimeoutTracking); // Should be true

// 2. Check timeout configuration
console.log('Task timeout:', bridge.config.timeoutDefaults.taskTimeoutMs); // ms

// 3. Check active timeouts
const stats = bridge.getStats();
console.log('Active timeouts:', stats.activeTimeouts); // Should show watched tasks
```

**Solution:**
- Enable timeout tracking: `enableTimeoutTracking: true` in bridge config
- Adjust timeout threshold: `timeoutDefaults.taskTimeoutMs` in config
- Manually watch timeout: `await bridge.watchTaskTimeout(caseId, taskId, timeoutMs)`

### Problem: Retry exhausted without success

**Symptoms:**
- Task fails repeatedly
- Retry attempts exhausted
- `task:retry-exhausted` event emitted

**Diagnosis:**
```javascript
// Listen for retry exhaustion
bridge.on('task:retry-exhausted', (event) => {
  console.log('Retry exhausted:', event.taskId);
  console.log('Attempts:', event.attempts);
  console.log('Error:', event.error);
});

// Check retry configuration
console.log('Max attempts:', bridge.config.retryPolicy.maxAttempts);
console.log('Max backoff:', bridge.config.retryPolicy.maxBackoffMs);
```

**Solution:**
- Increase max attempts: `retryPolicy.maxAttempts` in config
- Increase max backoff: `retryPolicy.maxBackoffMs` in config
- Investigate root cause: Check error message in event
- Manual intervention: Create new case or skip failed task

### Problem: Memory leak from long-running workflows

**Symptoms:**
- Daemon memory usage grows over time
- Completed operations accumulate
- Performance degrades

**Diagnosis:**
```javascript
// Check completed operations cache
const health = daemon.getHealth();
console.log('Completed operations:', health.completedOperations); // Should be capped at 1000

// Check metrics retention
console.log('Metrics retention:', daemon.config.metricsRetentionMs); // ms
```

**Solution:**
- Daemon automatically evicts oldest operations (LRU cache, max 1000)
- Reduce metrics retention: `metricsRetentionMs` in daemon config
- Clean up completed cases: Periodically query and archive old cases

---

## Performance Considerations

### Concurrency Limits
- **Daemon concurrency**: Controls max parallel operations (default: 10)
- **Bridge max cases**: Controls max concurrent workflow cases (default: 100)
- **YAWL parallelism**: No hard limit (depends on workflow definition)

**Recommendation:** Set `daemon.concurrency` to `maxConcurrentCases / avgTasksPerCase`.

### Timeout Configuration
- **Task timeout**: Default 30s (adjust based on task complexity)
- **Case timeout**: Default 1h (adjust based on workflow duration)
- **Check interval**: Default 5s (how often daemon checks timeouts)

**Recommendation:** Task timeout should be 2-3x average task duration.

### Retry Policy
- **Max attempts**: Default 3 (increase for transient failures)
- **Backoff multiplier**: Default 2 (exponential backoff)
- **Max backoff**: Default 30s (cap on retry delay)
- **Jitter factor**: Default 0.1 (10% randomness to prevent thundering herd)

**Recommendation:** Use max backoff = expected recovery time for external dependencies.

### Metrics Retention
- **Default**: 1 hour (3600000 ms)
- **Impact**: Longer retention = more memory
- **Trade-off**: Historical metrics vs memory usage

**Recommendation:** For long-running systems, export metrics to external system (Prometheus, Grafana).

---

## Next Steps

- **Patterns Cookbook**: See `yawl-patterns-cookbook.md` for detailed workflow pattern examples
- **Operational Runbook**: See `runbooks/yawl-operation.md` for deployment and monitoring procedures
- **Error Handling Guide**: See `yawl-error-handling.md` for comprehensive error scenarios and recovery strategies
- **YAWL Documentation**: See `@unrdf/yawl/README.md` for workflow engine details
- **Daemon Documentation**: See `@unrdf/daemon/README.md` for scheduler details

---

**Version**: 1.0.0  
**Last Updated**: 2026-01-10  
**Maintainer**: UNRDF Core Team
