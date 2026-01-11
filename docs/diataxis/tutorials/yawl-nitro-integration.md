# YAWL-Nitro Integration Tutorial

**Time Required**: 20-25 minutes
**Difficulty**: Intermediate
**Prerequisites**: Basic understanding of JavaScript, Node.js, and workflow concepts

## What You'll Learn

In this tutorial, you'll learn how to integrate YAWL workflows with Nitro's task scheduling system using the `@unrdf/daemon` bridge. By the end, you'll have:

- Set up a complete YAWL-Nitro integration
- Created scheduled workflow executions
- Implemented timeout enforcement
- Added automatic retry capabilities
- Built monitoring and health checks

## What is YAWL-Nitro Integration?

The YAWL-Nitro integration combines two powerful systems:

- **YAWL**: Enterprise workflow engine with 20 Van der Aalst control flow patterns
- **Nitro**: Modern task scheduling and execution engine (from Nuxt ecosystem)
- **Daemon Bridge**: `@unrdf/daemon` that connects both systems

**Use Cases:**
- Scheduled batch processing workflows
- Automated report generation pipelines
- Data synchronization with retry logic
- Multi-step API integrations with timeout enforcement

---

## Step 1: Project Setup (3 minutes)

### 1.1 Create Project Directory

```bash
mkdir yawl-nitro-tutorial
cd yawl-nitro-tutorial
pnpm init
```

### 1.2 Install Dependencies

```bash
pnpm add @unrdf/yawl @unrdf/daemon @unrdf/oxigraph zod
```

**What each package does:**
- `@unrdf/yawl`: Workflow engine with cryptographic receipts
- `@unrdf/daemon`: Scheduler and task executor
- `@unrdf/oxigraph`: RDF triple store (workflow state storage)
- `zod`: Runtime validation for task payloads

### 1.3 Create Project Structure

```bash
mkdir -p src examples test
touch src/index.mjs
```

---

## Step 2: Create Your First Workflow (5 minutes)

Let's build a simple document approval workflow that runs on a schedule.

### 2.1 Define the Workflow

Create `src/approval-workflow.mjs`:

```javascript
/**
 * @file Document Approval Workflow Definition
 */

import { createWorkflow } from '@unrdf/yawl';

/**
 * Create a 3-step approval workflow
 * @param {Store} store - RDF store
 * @returns {Promise<Object>} Workflow receipt
 */
export async function createApprovalWorkflow(store) {
  const workflowReceipt = await createWorkflow(store, {
    id: 'document-approval',
    name: 'Document Approval Workflow',
    description: 'Three-step document review and approval process',
    tasks: [
      {
        id: 'submit',
        name: 'Submit Document',
        kind: 'atomic',
        description: 'Submit document for review',
      },
      {
        id: 'review',
        name: 'Review Document',
        kind: 'atomic',
        description: 'Review document content',
      },
      {
        id: 'approve',
        name: 'Approve Document',
        kind: 'atomic',
        description: 'Final approval decision',
      },
    ],
    flow: [
      { from: 'submit', to: 'review' },
      { from: 'review', to: 'approve' },
    ],
  });

  console.log('✓ Workflow created:', workflowReceipt.workflow_id);
  return workflowReceipt;
}
```

**What's happening:**
1. We define 3 atomic tasks (indivisible units of work)
2. Flow connects tasks sequentially: `submit → review → approve`
3. YAWL creates cryptographic receipt for auditability
4. Workflow definition stored as RDF triples

---

## Step 3: Set Up Daemon Integration (5 minutes)

### 3.1 Create Daemon Instance

Create `src/daemon-setup.mjs`:

```javascript
/**
 * @file Daemon Setup and Configuration
 */

import { Daemon } from '@unrdf/daemon';
import { createNitroTaskExecutor } from '@unrdf/daemon/integrations/nitro-tasks';

/**
 * Initialize daemon with Nitro integration
 * @returns {Promise<Object>} Daemon and executor instances
 */
export async function setupDaemon() {
  // Create daemon for task scheduling
  const daemon = new Daemon({
    daemonId: crypto.randomUUID(),
    name: 'approval-workflow-daemon',
    logLevel: 'info',
    concurrency: 10, // Max 10 parallel tasks
  });

  // Create Nitro task executor bridge
  const executor = createNitroTaskExecutor(daemon, {
    autoStart: true, // Auto-start daemon
    timeout: 30000, // 30s default timeout
    maxRetries: 3, // Retry failed tasks 3 times
    enableEventRelay: true, // Relay events between systems
    enableMetrics: true, // Track execution metrics
  });

  console.log('✓ Daemon started:', daemon.id);
  console.log('✓ Executor created:', executor.id);

  return { daemon, executor };
}
```

### 3.2 Understanding Configuration

**Daemon Options:**
- `concurrency`: Max parallel operations (10 recommended for workflows)
- `logLevel`: `'error' | 'warn' | 'info' | 'debug'`

**Executor Options:**
- `timeout`: Task execution timeout (30s is safe default)
- `maxRetries`: Automatic retry attempts for failures
- `enableEventRelay`: Synchronize events between Daemon and Nitro
- `enableMetrics`: Track performance metrics

---

## Step 4: Register Workflow Operations (5 minutes)

### 4.1 Create Task Handlers

Create `src/task-handlers.mjs`:

```javascript
/**
 * @file Task Handler Implementations
 */

import { createCase, enableTask, startTask, completeTask } from '@unrdf/yawl';

/**
 * Submit document task handler
 * @param {Object} context - Task execution context
 * @returns {Promise<Object>} Task result
 */
export async function submitDocumentHandler(context = {}) {
  const { documentId, author, content } = context;

  console.log(`[Submit] Processing document ${documentId} by ${author}`);

  // Simulate document validation
  await new Promise((r) => setTimeout(r, 100));

  if (!content || content.length < 10) {
    throw new Error('Document content too short');
  }

  return {
    success: true,
    documentId,
    author,
    timestamp: new Date(),
    status: 'submitted',
  };
}

/**
 * Review document task handler
 * @param {Object} context - Task execution context
 * @returns {Promise<Object>} Task result
 */
export async function reviewDocumentHandler(context = {}) {
  const { documentId, reviewer = 'auto-reviewer' } = context;

  console.log(`[Review] Reviewing document ${documentId} by ${reviewer}`);

  // Simulate review process
  await new Promise((r) => setTimeout(r, 200));

  // Simple validation: pass 90% of the time
  const passed = Math.random() > 0.1;

  return {
    success: true,
    documentId,
    reviewer,
    passed,
    timestamp: new Date(),
    comments: passed ? 'Approved for publication' : 'Needs revision',
  };
}

/**
 * Approve document task handler
 * @param {Object} context - Task execution context
 * @returns {Promise<Object>} Task result
 */
export async function approveDocumentHandler(context = {}) {
  const { documentId, approver = 'system' } = context;

  console.log(`[Approve] Finalizing document ${documentId} by ${approver}`);

  // Simulate approval process
  await new Promise((r) => setTimeout(r, 100));

  return {
    success: true,
    documentId,
    approver,
    timestamp: new Date(),
    finalStatus: 'published',
  };
}
```

### 4.2 Register Operations with Daemon

Add to `src/daemon-setup.mjs`:

```javascript
import {
  submitDocumentHandler,
  reviewDocumentHandler,
  approveDocumentHandler,
} from './task-handlers.mjs';

/**
 * Register workflow task operations with daemon
 * @param {Daemon} daemon - Daemon instance
 * @returns {Object} Operation IDs
 */
export function registerWorkflowOperations(daemon) {
  const submitOpId = crypto.randomUUID();
  const reviewOpId = crypto.randomUUID();
  const approveOpId = crypto.randomUUID();

  // Register submit operation
  daemon.schedule({
    id: submitOpId,
    name: 'workflow:submit-document',
    handler: submitDocumentHandler,
    metadata: { category: 'workflow', priority: 'high' },
  });

  // Register review operation
  daemon.schedule({
    id: reviewOpId,
    name: 'workflow:review-document',
    handler: reviewDocumentHandler,
    metadata: { category: 'workflow', priority: 'normal' },
  });

  // Register approve operation
  daemon.schedule({
    id: approveOpId,
    name: 'workflow:approve-document',
    handler: approveDocumentHandler,
    metadata: { category: 'workflow', priority: 'high' },
  });

  console.log('✓ Registered 3 workflow operations');

  return { submitOpId, reviewOpId, approveOpId };
}
```

---

## Step 5: Connect Operations to Nitro Tasks (3 minutes)

### 5.1 Register as Nitro Tasks

Add to `src/daemon-setup.mjs`:

```javascript
/**
 * Register daemon operations as Nitro tasks
 * @param {NitroTaskExecutor} executor - Executor instance
 * @param {Object} operationIds - Operation ID mappings
 */
export function registerNitroTasks(executor, operationIds) {
  const { submitOpId, reviewOpId, approveOpId } = operationIds;

  // Register submit task
  executor.registerOperationAsTask(submitOpId, 'document:submit', {
    description: 'Submit document for approval',
    cronExpression: '0 9 * * *', // Daily at 9 AM
    priority: 'high',
    tags: ['workflow', 'submit'],
  });

  // Register review task
  executor.registerOperationAsTask(reviewOpId, 'document:review', {
    description: 'Review submitted document',
    cronExpression: '0 10 * * *', // Daily at 10 AM
    priority: 'normal',
    tags: ['workflow', 'review'],
  });

  // Register approve task
  executor.registerOperationAsTask(approveOpId, 'document:approve', {
    description: 'Approve reviewed document',
    cronExpression: '0 11 * * *', // Daily at 11 AM
    priority: 'high',
    tags: ['workflow', 'approve'],
  });

  console.log('✓ Registered 3 Nitro tasks');
}
```

**Cron Expression Guide:**
- `'0 9 * * *'` - Every day at 9:00 AM
- `'0 */6 * * *'` - Every 6 hours
- `'*/5 * * * *'` - Every 5 minutes
- `'0 0 * * 0'` - Every Sunday at midnight

---

## Step 6: Add Monitoring and Events (4 minutes)

### 6.1 Set Up Event Listeners

Create `src/monitoring.mjs`:

```javascript
/**
 * @file Monitoring and Event Handling
 */

/**
 * Set up executor event monitoring
 * @param {NitroTaskExecutor} executor - Executor instance
 */
export function setupMonitoring(executor) {
  // Task execution started
  executor.on('task:started', (event) => {
    console.log(`▶ Task started: ${event.nitroTaskId}`);
  });

  // Task execution succeeded
  executor.on('task:succeeded', (event) => {
    console.log(`✓ Task succeeded: ${event.nitroTaskId} (${event.duration}ms)`);
  });

  // Task execution failed
  executor.on('task:failed', (event) => {
    console.error(`✗ Task failed: ${event.nitroTaskId}`);
    console.error(`  Error: ${event.error}`);
    console.error(`  Duration: ${event.duration}ms`);
  });

  // Task registered
  executor.on('task:registered', (event) => {
    console.log(`✓ Task registered: ${event.nitroTaskId}`);
  });

  console.log('✓ Monitoring enabled');
}

/**
 * Generate metrics report
 * @param {NitroTaskExecutor} executor - Executor instance
 */
export function generateMetricsReport(executor) {
  const metrics = executor.getMetrics();
  const status = executor.getStatus();

  console.log('\n📊 Execution Metrics:');
  console.log(`  Tasks Executed:  ${metrics.tasksExecuted}`);
  console.log(`  Tasks Succeeded: ${metrics.tasksSucceeded}`);
  console.log(`  Tasks Failed:    ${metrics.tasksFailed}`);
  console.log(`  Success Rate:    ${((metrics.tasksSucceeded / metrics.tasksExecuted) * 100 || 0).toFixed(1)}%`);
  console.log(`  Avg Duration:    ${metrics.averageDuration.toFixed(0)}ms`);
  console.log(`  Registered:      ${metrics.registeredTasks} tasks`);
  console.log(`  Daemon Running:  ${status.running}\n`);
}
```

---

## Step 7: Put It All Together (5 minutes)

### 7.1 Create Main Application

Create `src/index.mjs`:

```javascript
#!/usr/bin/env node
/**
 * @file YAWL-Nitro Integration Example
 * @description Complete working example of workflow scheduling
 */

import { createStore } from '@unrdf/oxigraph';
import { createApprovalWorkflow } from './approval-workflow.mjs';
import {
  setupDaemon,
  registerWorkflowOperations,
  registerNitroTasks,
} from './daemon-setup.mjs';
import { setupMonitoring, generateMetricsReport } from './monitoring.mjs';

/**
 * Main application entry point
 */
async function main() {
  console.log('🚀 YAWL-Nitro Integration Tutorial\n');

  // Step 1: Create RDF store for workflow state
  console.log('Step 1: Creating RDF store...');
  const store = createStore();

  // Step 2: Create workflow definition
  console.log('Step 2: Creating workflow...');
  const workflowReceipt = await createApprovalWorkflow(store);

  // Step 3: Set up daemon and executor
  console.log('Step 3: Setting up daemon...');
  const { daemon, executor } = await setupDaemon();

  // Step 4: Register workflow operations
  console.log('Step 4: Registering operations...');
  const operationIds = registerWorkflowOperations(daemon);

  // Step 5: Register Nitro tasks
  console.log('Step 5: Registering Nitro tasks...');
  registerNitroTasks(executor, operationIds);

  // Step 6: Set up monitoring
  console.log('Step 6: Setting up monitoring...');
  setupMonitoring(executor);

  // Step 7: Execute a test workflow
  console.log('\nStep 7: Executing test workflow...\n');

  const taskId = executor.daemonToNitroMap.get(operationIds.submitOpId);
  const result = await executor.runTask(taskId, {
    documentId: 'DOC-001',
    author: 'tutorial-user',
    content: 'This is a test document for the tutorial workflow.',
  });

  console.log('\n✓ Test execution result:');
  console.log(JSON.stringify(result, null, 2));

  // Step 8: Show metrics
  console.log('\nStep 8: Metrics Report');
  generateMetricsReport(executor);

  // Step 9: List all registered tasks
  console.log('Step 9: Registered Tasks:\n');
  const tasks = executor.listTasks();
  tasks.forEach((task) => {
    console.log(`  • ${task.nitroTaskId}`);
    console.log(`    Type: ${task.operationType}`);
    console.log(`    Cron: ${task.cronExpression || 'manual'}`);
    console.log(`    Priority: ${task.priority}\n`);
  });

  console.log('✅ Tutorial Complete!\n');
  console.log('Next Steps:');
  console.log('  - Add error handling with try/catch');
  console.log('  - Implement timeout tracking');
  console.log('  - Add automatic retry logic');
  console.log('  - Connect to real Nitro dev server');
  console.log('  - Deploy to production\n');

  // Graceful shutdown
  console.log('🛑 Shutting down...');
  await executor.stop();
  console.log('✓ Shutdown complete\n');
}

// Run application
main().catch((error) => {
  console.error('Fatal error:', error);
  process.exit(1);
});
```

### 7.2 Run the Tutorial

```bash
node src/index.mjs
```

**Expected Output:**

```
🚀 YAWL-Nitro Integration Tutorial

Step 1: Creating RDF store...
Step 2: Creating workflow...
✓ Workflow created: document-approval
Step 3: Setting up daemon...
✓ Daemon started: abc-123-def
✓ Executor created: executor-456
Step 4: Registering operations...
✓ Registered 3 workflow operations
Step 5: Registering Nitro tasks...
✓ Task registered: daemon:document:submit
✓ Task registered: daemon:document:review
✓ Task registered: daemon:document:approve
✓ Registered 3 Nitro tasks
Step 6: Setting up monitoring...
✓ Monitoring enabled

Step 7: Executing test workflow...

▶ Task started: daemon:document:submit
[Submit] Processing document DOC-001 by tutorial-user
✓ Task succeeded: daemon:document:submit (105ms)

✓ Test execution result:
{
  "success": true,
  "taskId": "daemon:document:submit",
  "operationId": "...",
  "result": {
    "success": true,
    "documentId": "DOC-001",
    "author": "tutorial-user",
    "timestamp": "2026-01-11T10:30:00.000Z",
    "status": "submitted"
  },
  "duration": 105
}

Step 8: Metrics Report

📊 Execution Metrics:
  Tasks Executed:  1
  Tasks Succeeded: 1
  Tasks Failed:    0
  Success Rate:    100.0%
  Avg Duration:    105ms
  Registered:      3 tasks
  Daemon Running:  true

Step 9: Registered Tasks:

  • daemon:document:submit
    Type: document:submit
    Cron: 0 9 * * *
    Priority: high

  • daemon:document:review
    Type: document:review
    Cron: 0 10 * * *
    Priority: normal

  • daemon:document:approve
    Type: document:approve
    Cron: 0 11 * * *
    Priority: high

✅ Tutorial Complete!
```

---

## What You've Learned

✅ Created a YAWL workflow definition
✅ Set up daemon for task scheduling
✅ Created Nitro task executor bridge
✅ Registered workflow operations as tasks
✅ Implemented event monitoring
✅ Generated execution metrics
✅ Executed scheduled workflow tasks

---

## Next Steps

### Beginner Track
1. **Add Error Handling**: Wrap task execution in try/catch blocks
2. **Implement Validation**: Use Zod schemas for task payloads
3. **Add Logging**: Integrate Winston or Pino for structured logs

### Intermediate Track
1. **Timeout Enforcement**: Use `executor.watchTaskTimeout()`
2. **Automatic Retry**: Configure retry policy with exponential backoff
3. **Health Checks**: Create `/health` API endpoint
4. **Metrics Export**: Send metrics to Prometheus or Grafana

### Advanced Track
1. **Multi-Node Deployment**: Distribute tasks across daemon cluster
2. **Event-Driven Workflows**: Trigger workflows from external events
3. **YAWL Patterns**: Implement parallel split, exclusive choice, OR-join
4. **Production Deployment**: K8s manifests, Docker compose, monitoring

---

## Common Issues and Solutions

### Issue 1: Daemon Won't Start

**Symptom:** `Error: Daemon already running`

**Solution:**
```javascript
if (daemon.isRunning) {
  await daemon.stop();
}
await daemon.start();
```

### Issue 2: Task Not Found

**Symptom:** `Error: Task daemon:my-task not found`

**Solution:** Verify operation registration before task registration:
```javascript
// 1. Register operation first
daemon.schedule({ id: opId, name: 'my-op', handler: ... });

// 2. Then register as Nitro task
executor.registerOperationAsTask(opId, 'my-task');
```

### Issue 3: Metrics Not Updating

**Symptom:** `metrics.tasksExecuted` always 0

**Solution:** Enable metrics in executor config:
```javascript
const executor = createNitroTaskExecutor(daemon, {
  enableMetrics: true, // Must be true
  enableEventRelay: true, // Required for metric updates
});
```

---

## Additional Resources

- **[How-To: YAWL-Nitro Tasks](/docs/diataxis/how-to/yawl-nitro-tasks.md)** - Quick task recipes
- **[API Reference: YAWL-Nitro](/docs/diataxis/reference/yawl-nitro-api.md)** - Complete API documentation
- **[Explanation: YAWL-Nitro Architecture](/docs/diataxis/explanation/yawl-nitro-architecture.md)** - Deep architectural dive
- **[Example: YAWL-Nitro Workflow](/examples/yawl-nitro-workflow.mjs)** - Production-ready example

---

**Tutorial Version**: 1.0.0
**Last Updated**: 2026-01-11
**Estimated Completion Time**: 20-25 minutes
