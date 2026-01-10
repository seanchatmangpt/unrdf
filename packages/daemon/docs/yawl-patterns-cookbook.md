# YAWL Patterns Cookbook

**6 Production-Ready Daemon+YAWL Workflow Patterns**

This cookbook provides detailed walkthroughs for the 6 most common workflow patterns using daemon+YAWL integration.

---

## Pattern 1: Scheduled Sequential Workflow

### Use Case
Run a multi-step workflow on a schedule (e.g., nightly data processing, daily reports, hourly cleanup).

### Walkthrough

#### Step 1: Define Sequential Workflow

```javascript
import { createWorkflow } from '@unrdf/yawl';
import { createStore } from '@unrdf/oxigraph';

const store = createStore();

const workflowReceipt = await createWorkflow(store, {
  id: 'nightly-etl',
  name: 'Nightly ETL Pipeline',
  tasks: [
    { id: 'extract', name: 'Extract from Source DB', kind: 'atomic' },
    { id: 'transform', name: 'Transform Data', kind: 'atomic' },
    { id: 'validate', name: 'Validate Records', kind: 'atomic' },
    { id: 'load', name: 'Load to Data Warehouse', kind: 'atomic' },
    { id: 'notify', name: 'Send Success Notification', kind: 'atomic' },
  ],
  flow: [
    { from: 'extract', to: 'transform' },
    { from: 'transform', to: 'validate' },
    { from: 'validate', to: 'load' },
    { from: 'load', to: 'notify' },
  ],
});

console.log('Workflow created:', workflowReceipt.workflow_id);
```

#### Step 2: Create Bridge and Schedule

```javascript
import { Daemon } from '@unrdf/daemon';
import { createYawlBridge } from '@unrdf/daemon/integrations/yawl';
import { createWorkflowEngine } from '@unrdf/yawl';

const yawlEngine = createWorkflowEngine(store);
const daemon = new Daemon({ daemonId: 'etl-daemon', concurrency: 5 });
const bridge = createYawlBridge(daemon, yawlEngine, {
  daemonNodeId: 'node-1',
  enableAutoRetry: true,
});

await daemon.start();
await bridge.start();

// Schedule nightly at 2 AM
await bridge.scheduleRecurringCase('nightly-etl', '0 2 * * *', {
  caseIdPrefix: 'etl',
  inputData: { date: new Date().toISOString(), environment: 'production' },
});

console.log('Scheduled nightly ETL at 2 AM');
```

#### Step 3: Monitor Execution

```javascript
// Listen for case creation
bridge.on('case:created-by-schedule', (event) => {
  console.log(`ETL case created: ${event.caseId} at ${event.timestamp}`);
});

// Listen for task completion
yawlEngine.on('task:completed', (event) => {
  console.log(`Task completed: ${event.taskId} in case ${event.caseId}`);
});

// Listen for case completion
yawlEngine.on('case:completed', (event) => {
  console.log(`ETL pipeline completed: ${event.caseId}`);
  
  // Extract metrics
  const duration = event.completedAt - event.startedAt;
  console.log(`Total duration: ${duration}ms`);
});
```

### Expected Behavior

**Timeline:**
1. 2:00 AM: Bridge creates case `etl-2026-01-10-02-00`
2. 2:00:05 AM: Task `extract` starts
3. 2:01:30 AM: Task `extract` completes, `transform` starts
4. 2:03:00 AM: Task `transform` completes, `validate` starts
5. 2:03:45 AM: Task `validate` completes, `load` starts
6. 2:05:30 AM: Task `load` completes, `notify` starts
7. 2:05:32 AM: Task `notify` completes, case marked complete

**Receipts Generated:**
- Workflow creation receipt (hash chain starts)
- Case creation receipt (links to workflow)
- 5 task completion receipts (one per task, chained)
- Case completion receipt (final state snapshot)

### Configuration Recommendations

```javascript
{
  // Daemon settings
  concurrency: 5,  // Max 5 tasks in parallel (for sequential, only 1 active)
  
  // Bridge settings
  enableAutoRetry: true,  // Retry on DB connection failures
  retryPolicy: {
    maxAttempts: 3,  // Retry extract/load up to 3 times
    backoffMs: 5000, // 5s initial backoff (DB recovery time)
    backoffMultiplier: 2,
    maxBackoffMs: 60000, // Cap at 1 min
  },
  timeoutDefaults: {
    taskTimeoutMs: 300000, // 5 min per task (adjust based on data volume)
    caseTimeoutMs: 3600000, // 1 hour max for entire pipeline
  },
}
```

---

## Pattern 2: Parallel Task Distribution

### Use Case
Execute multiple independent tasks in parallel (e.g., multi-reviewer approval, parallel data validation, fan-out processing).

### Walkthrough

#### Step 1: Define Parallel Workflow

```javascript
const workflowReceipt = await createWorkflow(store, {
  id: 'document-approval',
  name: 'Multi-Reviewer Document Approval',
  tasks: [
    { id: 'submit', name: 'Submit Document', kind: 'atomic' },
    { id: 'legal-review', name: 'Legal Review', kind: 'atomic' },
    { id: 'tech-review', name: 'Technical Review', kind: 'atomic' },
    { id: 'finance-review', name: 'Finance Review', kind: 'atomic' },
    { id: 'compliance-review', name: 'Compliance Review', kind: 'atomic' },
    { id: 'finalize', name: 'Finalize Approval', kind: 'atomic' },
  ],
  flow: [
    // AND-split: submit triggers all reviews in parallel
    { from: 'submit', to: 'legal-review', splitType: 'AND' },
    { from: 'submit', to: 'tech-review', splitType: 'AND' },
    { from: 'submit', to: 'finance-review', splitType: 'AND' },
    { from: 'submit', to: 'compliance-review', splitType: 'AND' },
    
    // AND-join: finalize waits for all reviews
    { from: 'legal-review', to: 'finalize', joinType: 'AND' },
    { from: 'tech-review', to: 'finalize', joinType: 'AND' },
    { from: 'finance-review', to: 'finalize', joinType: 'AND' },
    { from: 'compliance-review', to: 'finalize', joinType: 'AND' },
  ],
});
```

#### Step 2: Create Case and Distribute Tasks

```javascript
import { createCase, enableTask } from '@unrdf/yawl';

// Create case
const caseReceipt = await createCase(store, {
  workflowId: 'document-approval',
  caseId: 'doc-001',
  inputData: {
    documentId: 'contract-2026-001',
    submitter: 'alice@example.com',
    priority: 'high',
  },
});

// Enable submit task
const submitReceipt = await enableTask(store, {
  caseId: 'doc-001',
  taskId: 'submit',
});

// Use bridge to distribute parallel review tasks across nodes
await bridge.distributeAndSplitTasks(
  'doc-001',
  ['legal-review', 'tech-review', 'finance-review', 'compliance-review'],
  { strategy: 'least-loaded' }  // Load balance across available nodes
);
```

#### Step 3: Monitor Parallel Execution

```javascript
// Track parallel task completion
const taskCompletions = new Map();

yawlEngine.on('task:completed', (event) => {
  if (event.caseId === 'doc-001') {
    taskCompletions.set(event.taskId, {
      completedAt: event.timestamp,
      duration: event.duration,
    });
    
    console.log(`Review completed: ${event.taskId} (${event.duration}ms)`);
    console.log(`Completed: ${taskCompletions.size}/4 reviews`);
  }
});

// Detect when all parallel tasks complete
bridge.on('tasks:distributed', (event) => {
  console.log(`Distributed ${event.taskIds.length} tasks using ${event.strategy}`);
});
```

### Expected Behavior

**Timeline (all times relative to case start):**
- T+0s: Submit task completed
- T+1s: All 4 review tasks enabled (AND-split)
- T+1s: Bridge distributes tasks to 4 nodes (if available)
- T+45s: Tech review completes (fastest)
- T+67s: Legal review completes
- T+89s: Compliance review completes
- T+102s: Finance review completes (slowest, blocks finalize)
- T+103s: Finalize task enabled (all 4 reviews complete, AND-join satisfied)
- T+105s: Finalize task completed, case marked complete

**Parallelism:** All 4 reviews execute concurrently (not sequentially).

### Configuration Recommendations

```javascript
{
  // Daemon settings
  concurrency: 10,  // Allow up to 10 parallel tasks
  
  // Bridge settings
  enableDistribution: true,  // Enable task distribution
  maxConcurrentCases: 50,  // Allow 50 approval workflows simultaneously
  
  // Distribution strategy
  strategy: 'least-loaded',  // Balance load across nodes
  // Alternatives: 'round-robin', 'random', 'affinity'
}
```

---

## Pattern 3: Timeout-Enforced Workflow

### Use Case
Workflows with external API calls, user interactions, or long-running tasks that must be cancelled if they exceed time limits.

### Walkthrough

#### Step 1: Define Workflow with External Dependencies

```javascript
const workflowReceipt = await createWorkflow(store, {
  id: 'api-integration',
  name: 'Third-Party API Integration',
  tasks: [
    { id: 'call-payment-api', name: 'Call Payment Gateway', kind: 'atomic' },
    { id: 'call-shipping-api', name: 'Call Shipping Provider', kind: 'atomic' },
    { id: 'call-inventory-api', name: 'Call Inventory System', kind: 'atomic' },
    { id: 'aggregate-results', name: 'Aggregate API Results', kind: 'atomic' },
  ],
  flow: [
    { from: 'call-payment-api', to: 'aggregate-results', splitType: 'AND' },
    { from: 'call-shipping-api', to: 'aggregate-results', splitType: 'AND' },
    { from: 'call-inventory-api', to: 'aggregate-results', splitType: 'AND' },
  ],
});
```

#### Step 2: Configure Timeouts

```javascript
const bridge = createYawlBridge(daemon, yawlEngine, {
  daemonNodeId: 'node-1',
  enableTimeoutTracking: true,  // Enable automatic timeout enforcement
  timeoutDefaults: {
    taskTimeoutMs: 10000,  // 10s default timeout per task
    caseTimeoutMs: 60000,  // 1 min max for entire workflow
    checkIntervalMs: 1000,  // Check every 1s
  },
});
```

#### Step 3: Handle Timeout Events

```javascript
// Listen for timeout enforcement
bridge.on('task:timeout-enforced', (event) => {
  console.error(`Task timed out: ${event.taskId} in ${event.caseId}`);
  console.error(`Timeout threshold: ${event.timeoutMs}ms`);
  
  // Log for incident review
  logIncident({
    type: 'task_timeout',
    caseId: event.caseId,
    taskId: event.taskId,
    timeoutMs: event.timeoutMs,
    timestamp: event.timestamp,
  });
  
  // Notify on-call engineer
  sendAlert({
    message: `API call timeout: ${event.taskId}`,
    severity: 'high',
  });
});

// Handle task failures (including timeouts)
yawlEngine.on('task:failed', (event) => {
  console.error(`Task failed: ${event.taskId}`, event.error);
  
  if (event.error.includes('timeout')) {
    // Bridge will automatically retry if enableAutoRetry: true
    console.log('Automatic retry scheduled');
  }
});
```

### Expected Behavior

**Scenario 1: All APIs respond within timeout**
- T+0s: All 3 API tasks start
- T+2.5s: Payment API responds (success)
- T+4.1s: Shipping API responds (success)
- T+6.3s: Inventory API responds (success)
- T+6.4s: Aggregate task starts and completes
- **Result:** Workflow completes successfully in 6.4s

**Scenario 2: One API exceeds timeout**
- T+0s: All 3 API tasks start
- T+2.5s: Payment API responds (success)
- T+4.1s: Shipping API responds (success)
- T+10s: Inventory API timeout (10s threshold)
- T+10s: Bridge cancels inventory task
- T+10s: `task:timeout-enforced` event emitted
- T+11s: Bridge schedules retry (attempt 1 of 3)
- T+13.2s: Inventory API responds (retry success)
- T+13.3s: Aggregate task starts and completes
- **Result:** Workflow completes successfully with 1 retry

**Scenario 3: Retry exhaustion**
- After 3 failed attempts, `task:retry-exhausted` event emitted
- Case marked as failed
- Manual intervention required

### Configuration Recommendations

```javascript
{
  // Timeout configuration
  timeoutDefaults: {
    taskTimeoutMs: 10000,  // 10s for API calls (P95 latency + buffer)
    caseTimeoutMs: 60000,  // 1 min max (sum of all task timeouts + buffer)
    checkIntervalMs: 1000,  // 1s check interval (low overhead)
  },
  
  // Retry configuration (for timeout recovery)
  retryPolicy: {
    maxAttempts: 3,  // Retry up to 3 times
    backoffMs: 2000,  // 2s initial backoff (API recovery time)
    backoffMultiplier: 2,  // Exponential backoff
    maxBackoffMs: 10000,  // Cap at 10s
  },
  
  // Enable automatic timeout tracking
  enableTimeoutTracking: true,
  enableAutoRetry: true,
}
```

---

## Pattern 4: Retry-with-Backoff Pipeline

### Use Case
Data pipelines with transient failures (database connection errors, network issues, rate limits).

### Walkthrough

#### Step 1: Define Pipeline Workflow

```javascript
const workflowReceipt = await createWorkflow(store, {
  id: 'data-ingestion',
  name: 'Data Ingestion Pipeline',
  tasks: [
    { id: 'fetch-source', name: 'Fetch from External API', kind: 'atomic' },
    { id: 'parse-data', name: 'Parse JSON Response', kind: 'atomic' },
    { id: 'validate-schema', name: 'Validate Schema', kind: 'atomic' },
    { id: 'write-db', name: 'Write to Database', kind: 'atomic' },
    { id: 'update-cache', name: 'Update Redis Cache', kind: 'atomic' },
  ],
  flow: [
    { from: 'fetch-source', to: 'parse-data' },
    { from: 'parse-data', to: 'validate-schema' },
    { from: 'validate-schema', to: 'write-db' },
    { from: 'write-db', to: 'update-cache' },
  ],
});
```

#### Step 2: Configure Aggressive Retry Policy

```javascript
const bridge = createYawlBridge(daemon, yawlEngine, {
  daemonNodeId: 'node-1',
  enableAutoRetry: true,  // Auto-retry all failures
  retryPolicy: {
    maxAttempts: 5,  // Up to 5 attempts per task
    backoffMs: 1000,  // 1s initial backoff
    backoffMultiplier: 2,  // Exponential: 1s, 2s, 4s, 8s, 16s
    maxBackoffMs: 30000,  // Cap at 30s
    jitterFactor: 0.2,  // 20% jitter to prevent thundering herd
  },
});
```

#### Step 3: Track Retry Attempts

```javascript
// Track retry attempts per task
const retryAttempts = new Map();

bridge.on('task:retry-executed', (event) => {
  const key = `${event.caseId}:${event.taskId}`;
  const attempts = (retryAttempts.get(key) || 0) + 1;
  retryAttempts.set(key, attempts);
  
  console.log(`Retry attempt ${event.attempt}/${bridge.config.retryPolicy.maxAttempts} for ${event.taskId}`);
  console.log(`Next retry in ${calculateNextBackoff(event.attempt)}ms`);
});

bridge.on('task:retry-exhausted', (event) => {
  console.error(`Retry exhausted for ${event.taskId} after ${event.attempts} attempts`);
  console.error(`Last error: ${event.error}`);
  
  // Send alert for manual intervention
  sendAlert({
    message: `Pipeline task failed permanently: ${event.taskId}`,
    caseId: event.caseId,
    taskId: event.taskId,
    attempts: event.attempts,
    error: event.error,
  });
});

function calculateNextBackoff(attempt) {
  const { backoffMs, backoffMultiplier, maxBackoffMs, jitterFactor } = bridge.config.retryPolicy;
  const baseBackoff = Math.min(backoffMs * Math.pow(backoffMultiplier, attempt - 1), maxBackoffMs);
  const jitter = baseBackoff * jitterFactor * Math.random();
  return baseBackoff + jitter;
}
```

### Expected Behavior

**Scenario: Transient DB connection error**
- T+0s: Task `write-db` starts
- T+0.5s: Task fails with `ECONNREFUSED` (DB unavailable)
- T+0.5s: Bridge schedules retry (attempt 1)
- T+1.5s: Retry attempt 1 (1s backoff)
- T+2.0s: Retry fails again
- T+2.0s: Bridge schedules retry (attempt 2)
- T+5.8s: Retry attempt 2 (3.8s backoff = 4s base - 0.2s jitter)
- T+6.1s: Retry succeeds (DB recovered)
- T+6.1s: Task marked complete, next task `update-cache` starts

**Backoff schedule:**
- Attempt 1: ~1s (1000ms base)
- Attempt 2: ~2s (2000ms base ± 20% jitter)
- Attempt 3: ~4s (4000ms base ± 20% jitter)
- Attempt 4: ~8s (8000ms base ± 20% jitter)
- Attempt 5: ~16s (16000ms base ± 20% jitter)
- After attempt 5: Retry exhausted, manual intervention

### Configuration Recommendations

```javascript
{
  // Retry policy for transient failures
  retryPolicy: {
    maxAttempts: 5,  // Generous retry count for transient issues
    backoffMs: 1000,  // Start with 1s
    backoffMultiplier: 2,  // Double each time
    maxBackoffMs: 30000,  // Cap at 30s (long enough for service recovery)
    jitterFactor: 0.2,  // 20% jitter (prevent thundering herd)
  },
  
  // Enable automatic retry on all failures
  enableAutoRetry: true,
}
```

---

## Pattern 5: Deferred Choice (Event-Triggered)

### Use Case
Workflows that wait for external events (user approval, webhook callback, payment confirmation).

### Walkthrough

#### Step 1: Define Workflow with Deferred Choice

```javascript
const workflowReceipt = await createWorkflow(store, {
  id: 'order-fulfillment',
  name: 'E-commerce Order Fulfillment',
  tasks: [
    { id: 'create-order', name: 'Create Order', kind: 'atomic' },
    { id: 'wait-payment', name: 'Wait for Payment', kind: 'deferred' },
    { id: 'process-payment', name: 'Process Payment', kind: 'atomic' },
    { id: 'ship-order', name: 'Ship Order', kind: 'atomic' },
    { id: 'cancel-order', name: 'Cancel Order', kind: 'atomic' },
  ],
  flow: [
    { from: 'create-order', to: 'wait-payment' },
    
    // Deferred choice: wait for payment OR cancel
    {
      from: 'wait-payment',
      to: 'process-payment',
      condition: { eventName: 'payment:received' },
    },
    {
      from: 'wait-payment',
      to: 'cancel-order',
      condition: { eventName: 'order:cancelled' },
    },
    
    { from: 'process-payment', to: 'ship-order' },
  ],
});
```

#### Step 2: Create Case and Wait for Event

```javascript
import { createCase, enableTask } from '@unrdf/yawl';

// Create order case
const caseReceipt = await createCase(store, {
  workflowId: 'order-fulfillment',
  caseId: 'order-12345',
  inputData: {
    orderId: 'ORD-12345',
    customerId: 'CUST-9876',
    amount: 199.99,
  },
});

// Wait for payment event (timeout after 24 hours)
const paymentPromise = bridge.waitForChoiceTrigger('order-12345', 'wait-payment', {
  eventName: 'payment:received',
  filter: { orderId: 'ORD-12345' },
  timeoutMs: 86400000,  // 24 hours
});

console.log('Waiting for payment event...');

// Simulate external payment gateway webhook
setTimeout(() => {
  yawlEngine.emit('payment:received', {
    orderId: 'ORD-12345',
    amount: 199.99,
    paymentId: 'PAY-abc123',
    timestamp: new Date(),
  });
}, 5000);  // Payment received after 5s

// Wait for choice resolution
try {
  await paymentPromise;
  console.log('Payment received, proceeding to fulfillment');
} catch (error) {
  console.error('Payment timeout, cancelling order');
}
```

#### Step 3: Handle Event Triggers

```javascript
// Listen for deferred choice resolution
bridge.on('choice:resolved', (event) => {
  console.log(`Choice resolved for ${event.taskId}`);
  console.log(`Chosen path: ${event.selectedPath}`);
  console.log(`Trigger event: ${event.eventName}`);
});

// Handle timeout scenario
bridge.on('choice:timeout', (event) => {
  console.error(`Deferred choice timeout: ${event.taskId}`);
  
  // Auto-cancel order on payment timeout
  yawlEngine.emit('order:cancelled', {
    orderId: event.metadata.orderId,
    reason: 'payment_timeout',
    timestamp: new Date(),
  });
});
```

### Expected Behavior

**Scenario 1: Payment received within timeout**
- T+0s: Order created, case starts
- T+1s: Deferred choice task `wait-payment` enabled
- T+1s: Bridge starts waiting for `payment:received` event
- T+5s: Payment gateway sends webhook
- T+5s: Event emitted: `payment:received`
- T+5s: Bridge resolves deferred choice → `process-payment` path
- T+5.1s: Task `process-payment` starts
- T+7s: Payment processed, `ship-order` starts
- **Result:** Order fulfilled successfully

**Scenario 2: Timeout (no payment)**
- T+0s: Order created
- T+1s: Deferred choice `wait-payment` enabled
- T+1s: Bridge waits for event (24h timeout)
- T+86400s: Timeout reached (no payment received)
- T+86400s: Bridge emits `choice:timeout` event
- T+86400s: Auto-cancel triggered → `cancel-order` path
- **Result:** Order cancelled due to payment timeout

### Configuration Recommendations

```javascript
{
  // Timeout for external events
  timeoutDefaults: {
    caseTimeoutMs: 86400000,  // 24 hours for payment
  },
  
  // Choice trigger configuration
  choiceTriggers: [
    {
      eventName: 'payment:received',
      filter: { orderId: '<orderId>' },  // Template replaced at runtime
      timeoutMs: 86400000,  // 24 hours
    },
    {
      eventName: 'order:cancelled',
      filter: { orderId: '<orderId>' },
      timeoutMs: 86400000,
    },
  ],
}
```

---

## Pattern 6: Conditional Routing (XOR-Split)

### Use Case
Workflows with branching logic based on data (approval thresholds, risk scoring, A/B testing).

### Walkthrough

#### Step 1: Define Workflow with Conditional Routing

```javascript
const workflowReceipt = await createWorkflow(store, {
  id: 'expense-approval',
  name: 'Expense Approval Workflow',
  tasks: [
    { id: 'submit-expense', name: 'Submit Expense Report', kind: 'atomic' },
    { id: 'check-amount', name: 'Check Amount', kind: 'atomic' },
    { id: 'auto-approve', name: 'Auto Approve', kind: 'atomic' },
    { id: 'manager-review', name: 'Manager Review', kind: 'atomic' },
    { id: 'director-review', name: 'Director Review', kind: 'atomic' },
    { id: 'notify-result', name: 'Notify Result', kind: 'atomic' },
  ],
  flow: [
    { from: 'submit-expense', to: 'check-amount' },
    
    // XOR-split: choose ONE path based on amount
    {
      from: 'check-amount',
      to: 'auto-approve',
      splitType: 'XOR',
      condition: { amount_lt: 100 },  // Amount < $100
    },
    {
      from: 'check-amount',
      to: 'manager-review',
      splitType: 'XOR',
      condition: { amount_gte: 100, amount_lt: 1000 },  // $100-$999
    },
    {
      from: 'check-amount',
      to: 'director-review',
      splitType: 'XOR',
      condition: { amount_gte: 1000 },  // $1000+
    },
    
    // XOR-join: merge paths
    { from: 'auto-approve', to: 'notify-result', joinType: 'XOR' },
    { from: 'manager-review', to: 'notify-result', joinType: 'XOR' },
    { from: 'director-review', to: 'notify-result', joinType: 'XOR' },
  ],
});
```

#### Step 2: Submit Cases with Different Amounts

```javascript
import { createCase } from '@unrdf/yawl';

// Case 1: Low amount → auto-approve path
const case1 = await createCase(store, {
  workflowId: 'expense-approval',
  caseId: 'expense-001',
  inputData: {
    employeeId: 'EMP-123',
    amount: 45.50,
    category: 'meals',
    description: 'Client lunch',
  },
});

// Case 2: Medium amount → manager review path
const case2 = await createCase(store, {
  workflowId: 'expense-approval',
  caseId: 'expense-002',
  inputData: {
    employeeId: 'EMP-456',
    amount: 450.00,
    category: 'travel',
    description: 'Flight to conference',
  },
});

// Case 3: High amount → director review path
const case3 = await createCase(store, {
  workflowId: 'expense-approval',
  caseId: 'expense-003',
  inputData: {
    employeeId: 'EMP-789',
    amount: 5200.00,
    category: 'equipment',
    description: 'New laptop',
  },
});
```

#### Step 3: Monitor Routing Decisions

```javascript
// Track which path each case takes
yawlEngine.on('task:enabled', (event) => {
  if (['auto-approve', 'manager-review', 'director-review'].includes(event.taskId)) {
    console.log(`Case ${event.caseId} routed to: ${event.taskId}`);
    console.log(`Amount: $${event.caseData.amount}`);
  }
});

// Collect routing statistics
const routingStats = { autoApprove: 0, managerReview: 0, directorReview: 0 };

yawlEngine.on('case:completed', (event) => {
  if (event.workflowId === 'expense-approval') {
    const completedTasks = event.completedTasks;
    
    if (completedTasks.includes('auto-approve')) {
      routingStats.autoApprove += 1;
    } else if (completedTasks.includes('manager-review')) {
      routingStats.managerReview += 1;
    } else if (completedTasks.includes('director-review')) {
      routingStats.directorReview += 1;
    }
    
    console.log('Routing distribution:', routingStats);
  }
});
```

### Expected Behavior

**Case 1 (amount: $45.50):**
- T+0s: Submit expense
- T+1s: Check amount ($45.50 < $100)
- T+1s: XOR-split evaluates: `auto-approve` condition TRUE
- T+1s: Task `auto-approve` enabled (only this path)
- T+2s: Auto-approve completes
- T+2s: Notify result
- **Path taken:** Submit → Check → Auto-Approve → Notify

**Case 2 (amount: $450.00):**
- T+0s: Submit expense
- T+1s: Check amount ($450.00 in range $100-$999)
- T+1s: XOR-split evaluates: `manager-review` condition TRUE
- T+1s: Task `manager-review` enabled (only this path)
- T+60s: Manager approves (manual action)
- T+60s: Notify result
- **Path taken:** Submit → Check → Manager-Review → Notify

**Case 3 (amount: $5200.00):**
- T+0s: Submit expense
- T+1s: Check amount ($5200.00 >= $1000)
- T+1s: XOR-split evaluates: `director-review` condition TRUE
- T+1s: Task `director-review` enabled (only this path)
- T+300s: Director approves (manual action)
- T+300s: Notify result
- **Path taken:** Submit → Check → Director-Review → Notify

### Configuration Recommendations

```javascript
{
  // Routing condition evaluation
  conditionEvaluator: {
    // Custom condition evaluator for complex logic
    evaluate: (data, condition) => {
      if ('amount_lt' in condition) {
        return data.amount < condition.amount_lt;
      }
      if ('amount_gte' in condition && 'amount_lt' in condition) {
        return data.amount >= condition.amount_gte && data.amount < condition.amount_lt;
      }
      if ('amount_gte' in condition) {
        return data.amount >= condition.amount_gte;
      }
      return false;
    },
  },
  
  // Timeout for manual approval steps
  timeoutDefaults: {
    taskTimeoutMs: 86400000,  // 24 hours for manager/director review
  },
}
```

---

## Summary Table

| Pattern | Use Case | Key Features | Typical Duration |
|---------|----------|--------------|------------------|
| Sequential | ETL, reports, cleanup | Simple linear flow | Minutes to hours |
| Parallel | Multi-reviewer, fan-out | AND-split/join, load balancing | Seconds to minutes |
| Timeout-Enforced | API calls, external services | Auto-cancel, retry | Seconds (with retry) |
| Retry-with-Backoff | Data pipelines, DB writes | Exponential backoff, jitter | Seconds to minutes |
| Deferred Choice | Payments, webhooks, approvals | Event-driven, timeout fallback | Hours to days |
| Conditional Routing | Approval thresholds, A/B tests | XOR-split, data-driven | Varies by path |

---

## Next Steps

- **Integration Guide**: See `yawl-integration-guide.md` for setup and API reference
- **Operational Runbook**: See `runbooks/yawl-operation.md` for deployment procedures
- **Error Handling Guide**: See `yawl-error-handling.md` for failure recovery strategies

---

**Version**: 1.0.0  
**Last Updated**: 2026-01-10  
**Maintainer**: UNRDF Core Team
