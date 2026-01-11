# Tutorial: YAWL Daemon Setup

**Estimated Time:** 15-20 minutes
**Difficulty:** Intermediate
**Prerequisites:** Basic knowledge of Node.js, YAWL workflows, and command-line tools

## What You'll Learn

By the end of this tutorial, you will:

- Set up a daemon for managing YAWL workflow operations
- Configure scheduled case creation with cron expressions
- Implement task timeout enforcement
- Configure automatic retry policies with exponential backoff
- Monitor daemon health and metrics
- Use CLI commands for daemon management

## What You'll Build

A production-ready YAWL daemon that:
- Creates workflow cases daily at 2 AM
- Enforces 60-second timeout on review tasks
- Automatically retries failed tasks up to 3 times
- Provides real-time health monitoring and metrics

---

## Step 1: Install Dependencies

Install required packages:

```bash
cd /home/user/unrdf
pnpm add @unrdf/daemon @unrdf/yawl @unrdf/oxigraph
```

Expected output:
```
✓ Dependencies installed successfully
```

**Checkpoint:** Verify installation succeeded before continuing.

---

## Step 2: Create Your Workflow Definition

Create a file `my-workflow.mjs`:

```javascript
/**
 * @file Document Approval Workflow
 * @description Multi-stage approval workflow with parallel reviewers
 */

import { createWorkflow } from '@unrdf/yawl';
import { createStore } from '@unrdf/oxigraph';

export async function createApprovalWorkflow() {
  const store = createStore();

  // Define workflow with 5 tasks
  const workflowReceipt = await createWorkflow(store, {
    id: 'doc-approval',
    name: 'Document Approval Workflow',
    tasks: [
      { id: 'submit', name: 'Submit Document', kind: 'atomic' },
      { id: 'legal-review', name: 'Legal Review', kind: 'atomic' },
      { id: 'tech-review', name: 'Technical Review', kind: 'atomic' },
      { id: 'approve', name: 'Final Approval', kind: 'atomic' },
      { id: 'publish', name: 'Publish Document', kind: 'atomic' },
    ],
    flow: [
      // Submit triggers parallel reviews
      { from: 'submit', to: 'legal-review', splitType: 'AND' },
      { from: 'submit', to: 'tech-review', splitType: 'AND' },

      // Both reviews must complete before approval
      { from: 'legal-review', to: 'approve', joinType: 'AND' },
      { from: 'tech-review', to: 'approve', joinType: 'AND' },

      // Approval leads to publishing
      { from: 'approve', to: 'publish' },
    ],
  });

  console.log('✓ Workflow created:', workflowReceipt.workflow_id);
  return { store, workflowId: workflowReceipt.workflow_id };
}
```

**Checkpoint:** Run `node my-workflow.mjs` to verify the workflow definition is valid.

---

## Step 3: Create Mock YAWL Engine

For this tutorial, we'll use a mock engine. In production, use the real YAWL engine.

Create `yawl-engine-mock.mjs`:

```javascript
/**
 * @file Mock YAWL Engine for Testing
 * @description Simulates YAWL engine interface for daemon integration
 */

import { EventEmitter } from 'events';

export class MockYawlEngine extends EventEmitter {
  constructor() {
    super();
    this.cases = new Map();
    this.tasks = new Map();
  }

  /**
   * Override on() to return unsubscriber function
   */
  on(eventName, handler) {
    super.on(eventName, handler);
    return () => this.off(eventName, handler);
  }

  /**
   * Create a workflow case
   */
  async createCase({ workflowId, caseId, inputData = {} }) {
    console.log(`  Creating case ${caseId} for workflow ${workflowId}`);

    this.cases.set(caseId, {
      id: caseId,
      workflowId,
      inputData,
      status: 'RUNNING',
      createdAt: new Date(),
    });

    this.emit('case:created', { caseId, workflowId });

    return {
      caseId,
      workflowId,
      status: 'RUNNING',
    };
  }

  /**
   * Enable a task for execution
   */
  async enableTask({ caseId, taskId }) {
    console.log(`  Enabling task ${taskId} in case ${caseId}`);

    const key = `${caseId}:${taskId}`;
    this.tasks.set(key, {
      caseId,
      taskId,
      status: 'ENABLED',
      enabledAt: new Date(),
    });

    this.emit('task:enabled', { caseId, taskId });

    return { caseId, taskId, status: 'ENABLED' };
  }

  /**
   * Cancel a task
   */
  async cancelTask({ caseId, taskId, reason = '' }) {
    console.log(`  Cancelling task ${taskId} in case ${caseId}: ${reason}`);

    const key = `${caseId}:${taskId}`;
    this.tasks.set(key, {
      caseId,
      taskId,
      status: 'CANCELLED',
      reason,
      cancelledAt: new Date(),
    });

    this.emit('task:cancelled', { caseId, taskId, reason });

    return { caseId, taskId, status: 'CANCELLED' };
  }

  /**
   * Complete a task
   */
  async completeTask({ caseId, taskId, outputData = {} }) {
    console.log(`  Completing task ${taskId} in case ${caseId}`);

    const key = `${caseId}:${taskId}`;
    this.tasks.set(key, {
      caseId,
      taskId,
      status: 'COMPLETED',
      outputData,
      completedAt: new Date(),
    });

    this.emit('task:completed', { caseId, taskId });

    return { caseId, taskId, status: 'COMPLETED' };
  }

  /**
   * Simulate task failure
   */
  async failTask({ caseId, taskId, error = '' }) {
    console.log(`  Task ${taskId} failed in case ${caseId}: ${error}`);

    const key = `${caseId}:${taskId}`;
    this.tasks.set(key, {
      caseId,
      taskId,
      status: 'FAILED',
      error,
      failedAt: new Date(),
    });

    this.emit('task:failed', { caseId, taskId, error });

    return { caseId, taskId, status: 'FAILED' };
  }

  /**
   * Get task state
   */
  getTaskState(caseId, taskId) {
    return this.tasks.get(`${caseId}:${taskId}`);
  }

  /**
   * Get case state
   */
  getCaseState(caseId) {
    return this.cases.get(caseId);
  }
}
```

---

## Step 4: Create Daemon with YAWL Bridge

Create `yawl-daemon.mjs`:

```javascript
/**
 * @file YAWL Daemon Setup
 * @description Production daemon for managing YAWL workflow operations
 */

import { Daemon } from '@unrdf/daemon';
import { YawlDaemonBridge } from '@unrdf/daemon/integrations/yawl';
import { MockYawlEngine } from './yawl-engine-mock.mjs';

async function setupYawlDaemon() {
  console.log('=== YAWL Daemon Setup ===\n');

  // Step 1: Create daemon instance
  console.log('Step 1: Creating daemon...');
  const daemon = new Daemon({
    daemonId: 'yawl-daemon-001',
    name: 'YAWL Workflow Daemon',
    concurrency: 10,
    logger: console,
  });
  console.log('✓ Daemon created\n');

  // Step 2: Create YAWL engine
  console.log('Step 2: Creating YAWL engine...');
  const yawlEngine = new MockYawlEngine();
  console.log('✓ YAWL engine created\n');

  // Step 3: Create bridge between daemon and YAWL
  console.log('Step 3: Creating YAWL-Daemon bridge...');
  const bridge = new YawlDaemonBridge(daemon, yawlEngine, {
    daemonNodeId: 'node-primary',
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
      taskTimeoutMs: 60000, // 60 seconds
      caseTimeoutMs: 3600000, // 1 hour
      checkIntervalMs: 5000, // Check every 5 seconds
    },
  });
  console.log('✓ Bridge created\n');

  // Step 4: Set up event listeners for monitoring
  console.log('Step 4: Setting up event listeners...');

  daemon.on('daemon:started', (event) => {
    console.log(`[DAEMON] Started at ${event.timestamp.toISOString()}`);
  });

  bridge.on('bridge:started', (event) => {
    console.log(`[BRIDGE] Started at ${event.timestamp.toISOString()}`);
  });

  bridge.on('case:created-by-schedule', (event) => {
    console.log(`[BRIDGE] Case created: ${event.caseId} for workflow ${event.workflowId}`);
  });

  bridge.on('task:timeout-enforced', (event) => {
    console.log(`[BRIDGE] Task timeout: ${event.taskId} in case ${event.caseId} (${event.timeoutMs}ms)`);
  });

  bridge.on('task:retry-executed', (event) => {
    console.log(`[BRIDGE] Task retry: ${event.taskId} in case ${event.caseId} (attempt ${event.attempt})`);
  });

  bridge.on('task:retry-exhausted', (event) => {
    console.log(`[BRIDGE] Retry exhausted: ${event.taskId} in case ${event.caseId} after ${event.attempts} attempts`);
  });

  yawlEngine.on('case:created', (event) => {
    console.log(`[YAWL] Case created: ${event.caseId}`);
  });

  yawlEngine.on('task:enabled', (event) => {
    console.log(`[YAWL] Task enabled: ${event.taskId} in case ${event.caseId}`);
  });

  yawlEngine.on('task:cancelled', (event) => {
    console.log(`[YAWL] Task cancelled: ${event.taskId} in case ${event.caseId}`);
  });

  console.log('✓ Event listeners configured\n');

  return { daemon, bridge, yawlEngine };
}

// Run setup
const { daemon, bridge, yawlEngine } = await setupYawlDaemon();

// Step 5: Start daemon and bridge
console.log('Step 5: Starting daemon and bridge...');
await daemon.start();
await bridge.start();
console.log('✓ Daemon and bridge started\n');

// Step 6: Schedule recurring case creation
console.log('Step 6: Scheduling recurring case creation...');
const scheduleResult = await bridge.scheduleRecurringCase(
  'doc-approval',
  '0 2 * * *', // Every day at 2 AM
  {
    caseIdPrefix: 'daily-approval',
    priority: 5,
    inputData: {
      department: 'engineering',
      submittedBy: 'automation',
    },
  }
);
console.log(`✓ Scheduled operation: ${scheduleResult.operationId}\n`);

// Step 7: Demonstrate timeout enforcement
console.log('Step 7: Setting up timeout enforcement...');
const caseId = 'demo-case-001';
await yawlEngine.createCase({
  caseId,
  workflowId: 'doc-approval',
});

const timeoutResult = await bridge.watchTaskTimeout(
  caseId,
  'legal-review',
  5000 // 5 seconds for demo
);
console.log(`✓ Watching timeout for task: ${timeoutResult.taskId}\n`);

// Step 8: Demonstrate retry policy
console.log('Step 8: Demonstrating retry policy...');
await yawlEngine.enableTask({ caseId, taskId: 'tech-review' });
await yawlEngine.failTask({
  caseId,
  taskId: 'tech-review',
  error: 'Network timeout',
});
// Bridge automatically schedules retry due to enableAutoRetry: true
console.log('✓ Automatic retry scheduled\n');

// Step 9: Display daemon health and metrics
console.log('Step 9: Checking daemon health...');
const health = daemon.getHealth();
console.log('Health Status:');
console.log(`  Running: ${health.isRunning}`);
console.log(`  Uptime: ${Math.round(health.uptime / 1000)}s`);
console.log(`  Active Operations: ${health.activeOperations}`);
console.log(`  Queued Operations: ${health.queuedOperations}`);
console.log(`  Completed Operations: ${health.completedOperations}\n`);

const metrics = daemon.getMetrics();
console.log('Metrics:');
console.log(`  Total Operations: ${metrics.totalOperations}`);
console.log(`  Successful: ${metrics.successfulOperations}`);
console.log(`  Failed: ${metrics.failedOperations}`);
console.log(`  Success Rate: ${metrics.successRate.toFixed(1)}%`);
console.log(`  Avg Duration: ${metrics.averageDuration.toFixed(2)}ms\n`);

const bridgeStats = bridge.getStats();
console.log('Bridge Statistics:');
console.log(`  Bridge ID: ${bridgeStats.bridgeId}`);
console.log(`  Running: ${bridgeStats.isRunning}`);
console.log(`  Case Schedules: ${bridgeStats.caseSchedules}`);
console.log(`  Active Timeouts: ${bridgeStats.activeTimeouts}`);
console.log(`  Active Retries: ${bridgeStats.activeRetries}`);
console.log(`  Active Triggers: ${bridgeStats.activeTriggers}`);
console.log(`  Distributions: ${bridgeStats.distributions}\n`);

// Step 10: Simulate some time passing to see timeout enforcement
console.log('Step 10: Waiting to demonstrate timeout enforcement (6 seconds)...');
await new Promise(resolve => setTimeout(resolve, 6000));

// Check if task was cancelled due to timeout
const taskState = yawlEngine.getTaskState(caseId, 'legal-review');
if (taskState?.status === 'CANCELLED') {
  console.log('✓ Task was automatically cancelled due to timeout\n');
} else {
  console.log(`  Task status: ${taskState?.status || 'unknown'}\n`);
}

// Step 11: Clean up
console.log('Step 11: Shutting down...');
await bridge.stop();
await daemon.stop();
console.log('✓ Daemon and bridge stopped\n');

console.log('=== Tutorial Complete ===\n');
console.log('You have successfully:');
console.log('  ✓ Created a daemon with YAWL bridge');
console.log('  ✓ Scheduled recurring case creation');
console.log('  ✓ Configured timeout enforcement');
console.log('  ✓ Set up automatic retry policies');
console.log('  ✓ Monitored health and metrics');
console.log('\nNext steps:');
console.log('  - Integrate with real YAWL workflows');
console.log('  - Configure production retry policies');
console.log('  - Set up distributed task execution');
console.log('  - Implement custom event triggers');
```

---

## Step 5: Run the Daemon

Execute the daemon setup:

```bash
node yawl-daemon.mjs
```

Expected output:
```
=== YAWL Daemon Setup ===

Step 1: Creating daemon...
✓ Daemon created

Step 2: Creating YAWL engine...
✓ YAWL engine created

Step 3: Creating YAWL-Daemon bridge...
✓ Bridge created

Step 4: Setting up event listeners...
✓ Event listeners configured

Step 5: Starting daemon and bridge...
[DAEMON] Started at 2026-01-11T...
[BRIDGE] Started at 2026-01-11T...
✓ Daemon and bridge started

Step 6: Scheduling recurring case creation...
✓ Scheduled operation: yawl-case-doc-approval-...

Step 7: Setting up timeout enforcement...
  Creating case demo-case-001 for workflow doc-approval
[YAWL] Case created: demo-case-001
✓ Watching timeout for task: legal-review

Step 8: Demonstrating retry policy...
  Enabling task tech-review in case demo-case-001
[YAWL] Task enabled: tech-review in case demo-case-001
  Task tech-review failed in case demo-case-001: Network timeout
[YAWL] Task failed: tech-review in case demo-case-001
✓ Automatic retry scheduled

...

✓ Task was automatically cancelled due to timeout

=== Tutorial Complete ===
```

**Checkpoint:** Verify all steps completed successfully.

---

## Step 6: Use CLI Commands

While the daemon is running (in a separate terminal), try these CLI commands:

```bash
# List all scheduled operations
unrdf daemon list

# View daemon status
unrdf daemon status --include-metrics

# View operation logs
unrdf daemon logs --max-lines 50

# Show daemon configuration
unrdf daemon config
```

---

## Understanding What You Built

### Architecture

```
┌────────────────────────────────────────────────┐
│                  Daemon                        │
│  - Schedules operations                        │
│  - Manages concurrency                         │
│  - Tracks metrics                              │
└────────────┬───────────────────────────────────┘
             │
             │ Bridge Interface
             │
┌────────────▼───────────────────────────────────┐
│            YawlDaemonBridge                    │
│  - Scheduled case creation                     │
│  - Timeout enforcement                         │
│  - Automatic retries                           │
│  - Event mapping                               │
└────────────┬───────────────────────────────────┘
             │
             │ YAWL API
             │
┌────────────▼───────────────────────────────────┐
│            YAWL Engine                         │
│  - Workflow execution                          │
│  - Case management                             │
│  - Event emission                              │
└────────────────────────────────────────────────┘
```

### Key Concepts

1. **Daemon**: Manages scheduled operations with concurrency control
2. **Bridge**: Connects daemon scheduling to YAWL workflow operations
3. **Event-Driven**: Bridge listens to YAWL events and triggers daemon operations
4. **Automatic Retry**: Failed tasks are automatically retried with exponential backoff
5. **Timeout Enforcement**: Tasks exceeding time limits are automatically cancelled

---

## Next Steps

Now that you have a working YAWL daemon:

1. **Production Setup**: Replace mock engine with real YAWL engine
2. **Custom Workflows**: Define your own workflow definitions
3. **Advanced Scheduling**: Use cron expressions for complex schedules
4. **Monitoring**: Integrate with observability tools
5. **Distributed Execution**: Set up multiple daemon nodes with Raft consensus

---

## Troubleshooting

### Daemon won't start

**Problem:** `Error: Invalid configuration`

**Solution:** Verify daemon configuration matches DaemonConfigSchema:
```javascript
const daemon = new Daemon({
  daemonId: 'my-daemon', // Required: unique identifier
  name: 'My Daemon',     // Required: human-readable name
});
```

### Timeout not working

**Problem:** Tasks aren't being cancelled after timeout

**Solution:** Check that timeout tracking is enabled:
```javascript
const bridge = new YawlDaemonBridge(daemon, yawlEngine, {
  enableTimeoutTracking: true, // Must be true
  timeoutDefaults: {
    taskTimeoutMs: 60000, // Ensure this is set
  },
});
```

### Retries not happening

**Problem:** Failed tasks aren't being retried

**Solution:** Verify auto-retry is enabled:
```javascript
const bridge = new YawlDaemonBridge(daemon, yawlEngine, {
  enableAutoRetry: true, // Must be true
  retryPolicy: {
    maxAttempts: 3, // Ensure > 1
  },
});
```

---

## Summary

Congratulations! You have:

- ✅ Set up a daemon for YAWL workflow automation
- ✅ Configured scheduled case creation with cron
- ✅ Implemented timeout enforcement
- ✅ Configured automatic retry with exponential backoff
- ✅ Monitored daemon health and metrics
- ✅ Used CLI commands for daemon management

**Total Time:** 15-20 minutes

**Files Created:**
- `my-workflow.mjs` - Workflow definition
- `yawl-engine-mock.mjs` - Mock YAWL engine
- `yawl-daemon.mjs` - Complete daemon setup

---

## Further Reading

- [How-to: YAWL Daemon Management](../how-to/yawl-daemon-management.md) - Common management tasks
- [Reference: YAWL Daemon API](../reference/yawl-daemon-api.md) - Complete API documentation
- [Example: YAWL Daemon Workflow](../../examples/yawl-daemon-workflow.mjs) - Production example
- [YAWL Patterns](../explanation/yawl-patterns.md) - Understanding YAWL control flow patterns
