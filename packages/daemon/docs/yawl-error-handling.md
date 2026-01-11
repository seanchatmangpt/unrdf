# YAWL Error Handling Guide

**Comprehensive error handling for daemon+YAWL workflows**

---

## Error Type Taxonomy

The daemon+YAWL integration handles 6 primary error types:

| Error Type | Cause | Daemon Response | Recovery Strategy |
|------------|-------|-----------------|-------------------|
| **Task Failure** | Task handler throws exception | Catch, log, emit `operation:failure` | Automatic retry (if enabled) |
| **Timeout** | Task exceeds time limit | Cancel task, emit `task:timeout-enforced` | Retry or manual intervention |
| **Retry Exhaustion** | Max retry attempts reached | Log exhaustion, emit `task:retry-exhausted` | Manual intervention required |
| **Deadlock** | Tasks waiting on each other | Detect via cycle analysis | Cancel blocked tasks |
| **Cascade Failure** | One failure triggers others | Isolate failure scope | Circuit breaker pattern |
| **Receipt Verification Failure** | Hash mismatch or missing receipt | Log verification error, continue execution | Manual audit required |

---

## Error Type 1: Task Failure

### What It Is
Task handler function throws an exception or returns a rejected promise.

### How Daemon Responds
1. Catches exception in task executor
2. Logs error with context (task ID, case ID, error message)
3. Emits `operation:failure` event
4. Increments `failedOperations` metric
5. If `enableAutoRetry: true`, schedules retry with backoff

### Configuration

```javascript
const bridge = createYawlBridge(daemon, yawlEngine, {
  enableAutoRetry: true,  // Enable automatic retry
  retryPolicy: {
    maxAttempts: 3,  // Retry up to 3 times
    backoffMs: 1000,  // 1s initial backoff
    backoffMultiplier: 2,  // Exponential: 1s, 2s, 4s
    maxBackoffMs: 30000,  // Cap at 30s
    jitterFactor: 0.1,  // 10% randomness
  },
});
```

### Example Scenario

```javascript
// Task handler that may fail
const handler = async () => {
  // Simulate API call that fails
  const response = await fetch('https://api.example.com/data');
  
  if (!response.ok) {
    throw new Error(`API error: ${response.status}`);
  }
  
  return await response.json();
};

// Schedule task
daemon.schedule({
  id: 'fetch-data',
  name: 'Fetch External Data',
  handler,
});

// Listen for failure
daemon.on('operation:failure', (event) => {
  console.error(`Task failed: ${event.name}`);
  console.error(`Error: ${event.error}`);
  console.error(`Duration: ${event.duration}ms`);
  console.error(`Attempt: ${event.attempts || 1}`);
});

// Listen for retry
bridge.on('task:retry-executed', (event) => {
  console.log(`Retry attempt ${event.attempt}/${bridge.config.retryPolicy.maxAttempts}`);
  console.log(`Next backoff: ${calculateBackoff(event.attempt)}ms`);
});
```

### Recovery Strategy

**Automatic Recovery (Transient Failure):**
```
T+0s: Task fails with ECONNREFUSED
T+0s: Bridge schedules retry (attempt 1)
T+1s: Retry attempt 1
T+1s: Task succeeds
T+1s: Next task enabled
✓ Recovered automatically
```

**Manual Intervention (Permanent Failure):**
```
T+0s: Task fails with 401 Unauthorized
T+0s: Bridge schedules retry (attempt 1)
T+1s: Retry attempt 1 fails (401)
T+3s: Retry attempt 2 fails (401)
T+7s: Retry attempt 3 fails (401)
T+7s: Retry exhausted
✗ Manual fix required: Update API credentials
```

### Expected Receipts

```javascript
{
  id: 'receipt-abc123',
  operationId: 'fetch-data',
  status: 'failure',  // Marked as failed
  error: {
    message: 'API error: 500',
    stack: 'Error: API error: 500\n    at handler (...)',
    code: 'API_ERROR',
  },
  attempts: 1,
  timestamp: '2026-01-10T10:15:30Z',
  duration: 2340,  // Time before failure
  metadata: {
    caseId: 'case-001',
    taskId: 'fetch-data',
    workflowId: 'data-pipeline',
  },
}
```

---

## Error Type 2: Timeout

### What It Is
Task execution exceeds configured timeout threshold.

### How Daemon Responds
1. Daemon schedules periodic timeout checks (every `checkIntervalMs`)
2. On timeout detection, daemon calls `yawlEngine.cancelTask()`
3. Bridge emits `task:timeout-enforced` event
4. Task marked as `cancelled` in YAWL store
5. Timeout recorded in metrics

### Configuration

```javascript
const bridge = createYawlBridge(daemon, yawlEngine, {
  enableTimeoutTracking: true,  // Enable automatic timeout enforcement
  timeoutDefaults: {
    taskTimeoutMs: 30000,  // 30s per task
    caseTimeoutMs: 3600000,  // 1h per case
    checkIntervalMs: 5000,  // Check every 5s
  },
});
```

### Example Scenario

```javascript
// Create case with timeout tracking
const caseReceipt = await createCase(store, {
  workflowId: 'api-integration',
  caseId: 'api-case-001',
});

// Bridge automatically watches timeouts for all enabled tasks
// (if enableTimeoutTracking: true)

// Listen for timeout enforcement
bridge.on('task:timeout-enforced', (event) => {
  console.error(`Task timed out: ${event.taskId}`);
  console.error(`Case: ${event.caseId}`);
  console.error(`Timeout threshold: ${event.timeoutMs}ms`);
  console.error(`Timestamp: ${event.timestamp}`);
  
  // Send alert
  sendAlert({
    severity: 'high',
    message: `Task timeout: ${event.taskId}`,
    caseId: event.caseId,
    taskId: event.taskId,
  });
});

// Handle task cancellation
yawlEngine.on('task:cancelled', (event) => {
  console.log(`Task cancelled: ${event.taskId}`);
  console.log(`Reason: ${event.reason}`);  // "Timeout after 30000ms"
});
```

### Recovery Strategy

**Automatic Retry (if enabled):**
```
T+0s: Task starts
T+30s: Timeout threshold reached
T+30s: Bridge cancels task
T+30s: Retry scheduled (attempt 1)
T+31s: Retry starts with fresh timeout
T+35s: Task completes (within timeout)
✓ Recovered via retry
```

**Manual Intervention (retry also times out):**
```
T+0s: Task starts
T+30s: Timeout (attempt 1)
T+31s: Retry starts
T+61s: Timeout (attempt 2)
T+63s: Retry starts
T+93s: Timeout (attempt 3)
T+93s: Retry exhausted
✗ Manual fix required: Investigate slow dependency
```

### Expected Receipts

```javascript
{
  id: 'receipt-timeout-456',
  operationId: 'slow-api-call',
  status: 'timeout',  // Marked as timeout
  error: {
    message: 'Timeout after 30000ms',
    code: 'TIMEOUT',
    timeoutMs: 30000,
    elapsed: 30005,
  },
  attempts: 1,
  timestamp: '2026-01-10T10:16:00Z',
  duration: 30005,
  metadata: {
    caseId: 'case-001',
    taskId: 'slow-api-call',
    cancelledAt: '2026-01-10T10:16:00Z',
  },
}
```

---

## Error Type 3: Retry Exhaustion

### What It Is
Task fails repeatedly, exceeding `maxAttempts` in retry policy.

### How Daemon Responds
1. After final retry attempt fails, bridge checks `attempts >= maxAttempts`
2. Unschedules retry operation
3. Emits `task:retry-exhausted` event
4. Task remains in `failed` state (no further automatic recovery)
5. Case may be blocked (if task is required for next steps)

### Configuration

```javascript
const bridge = createYawlBridge(daemon, yawlEngine, {
  enableAutoRetry: true,
  retryPolicy: {
    maxAttempts: 3,  // Max 3 attempts before giving up
    backoffMs: 1000,
    backoffMultiplier: 2,
    maxBackoffMs: 30000,
  },
});
```

### Example Scenario

```javascript
// Task that fails permanently
const handler = async () => {
  // Invalid API key (not transient)
  const response = await fetch('https://api.example.com/data', {
    headers: { 'Authorization': 'Bearer invalid-token' },
  });
  
  if (response.status === 401) {
    throw new Error('Unauthorized: Invalid API key');
  }
  
  return await response.json();
};

// Listen for retry exhaustion
bridge.on('task:retry-exhausted', (event) => {
  console.error(`Retry exhausted: ${event.taskId}`);
  console.error(`Attempts: ${event.attempts}`);
  console.error(`Last error: ${event.error}`);
  console.error(`Case ID: ${event.caseId}`);
  
  // Manual intervention required
  createIncident({
    title: `Task failed permanently: ${event.taskId}`,
    severity: 'critical',
    description: `Case ${event.caseId} blocked after ${event.attempts} retry attempts.`,
    error: event.error,
    runbook: 'yawl-error-handling.md#error-type-3-retry-exhaustion',
  });
  
  // Notify operations team
  sendPageDutyAlert({
    service: 'unrdf-workflows',
    incident_key: `retry-exhausted-${event.caseId}-${event.taskId}`,
    description: `Manual intervention required for ${event.taskId}`,
  });
});
```

### Recovery Strategy

**Manual Investigation:**
```bash
# 1. Retrieve error details
curl "http://localhost:8080/yawl/cases/${caseId}/tasks/${taskId}" | jq

# 2. Review error logs
journalctl -u unrdf-daemon --since "1 hour ago" | grep "${taskId}"

# 3. Identify root cause
# - Invalid credentials? Update secrets
# - API endpoint changed? Update handler
# - Rate limit exceeded? Increase backoff
# - Permanent service outage? Skip task or cancel case
```

**Manual Resolution Options:**

**Option 1: Fix root cause and recreate case**
```bash
# Fix issue (e.g., update API key)
export API_KEY="new-valid-key"

# Recreate case from scratch
node recreate-case.mjs --workflow-id data-pipeline --case-id case-001-retry
```

**Option 2: Skip failed task (if safe)**
```bash
# Manually mark task as completed (use with caution!)
node complete-task.mjs --case-id case-001 --task-id fetch-data --force
```

**Option 3: Cancel case (if unrecoverable)**
```bash
# Cancel entire workflow case
node cancel-case.mjs --case-id case-001 --reason "Permanent API failure"
```

### Expected Receipts

```javascript
{
  id: 'receipt-exhausted-789',
  operationId: 'fetch-data',
  status: 'retry_exhausted',
  error: {
    message: 'Unauthorized: Invalid API key',
    code: 'RETRY_EXHAUSTED',
    attempts: 3,
    lastAttemptError: 'Unauthorized: Invalid API key',
  },
  attempts: 3,
  timestamp: '2026-01-10T10:16:45Z',
  duration: 7234,  // Total time across all attempts
  metadata: {
    caseId: 'case-001',
    taskId: 'fetch-data',
    retrySchedule: [1000, 2000, 4000],  // Backoff schedule used
  },
}
```

---

## Error Type 4: Deadlock

### What It Is
Circular dependency in workflow: Task A waits for Task B, Task B waits for Task A.

### How Daemon Responds
1. YAWL engine detects circular dependency during case creation or task enablement
2. Throws `WorkflowDeadlockError` with cycle details
3. Daemon logs error and rejects case creation
4. No case is created (fail-fast validation)

### Configuration

```javascript
// No specific configuration needed
// Deadlock detection is built into YAWL engine
```

### Example Scenario (Invalid Workflow)

```javascript
// INVALID: Circular dependency
const workflowWithDeadlock = {
  id: 'circular-workflow',
  tasks: [
    { id: 'task-a', name: 'Task A', kind: 'atomic' },
    { id: 'task-b', name: 'Task B', kind: 'atomic' },
  ],
  flow: [
    { from: 'task-a', to: 'task-b' },
    { from: 'task-b', to: 'task-a' },  // Circular!
  ],
};

try {
  await createWorkflow(store, workflowWithDeadlock);
} catch (error) {
  console.error('Workflow creation failed:', error.message);
  // Expected: "Circular dependency detected: task-a → task-b → task-a"
  
  if (error.code === 'WORKFLOW_DEADLOCK') {
    console.error('Cycle:', error.details.cycle);  // ['task-a', 'task-b', 'task-a']
    console.error('Blocked tasks:', error.details.blockedTasks);
  }
}
```

### Recovery Strategy

**Prevention (Design Time):**
- Validate workflow before deployment using `WorkflowSpecSchema.parse()`
- Use workflow visualization tools to detect cycles
- Test workflows in staging with dry-run mode

**Detection (Runtime):**
- If deadlock somehow occurs during execution, YAWL engine will detect it when evaluating join conditions
- Tasks waiting indefinitely will trigger case timeout (if configured)

**Resolution:**
```bash
# 1. Identify circular dependency in workflow definition
# 2. Fix workflow: Remove circular flow edge
# 3. Redeploy workflow
node deploy-workflow.mjs --workflow-id circular-workflow --version v2
```

### Expected Receipts

No receipts generated (workflow creation rejected before case creation).

---

## Error Type 5: Cascade Failure

### What It Is
One task failure triggers failures in dependent tasks, causing widespread workflow disruption.

### How Daemon Responds
1. Daemon isolates failures to individual operations (no shared state)
2. Each task failure is logged independently
3. Failures do NOT automatically propagate to dependent tasks
4. Dependent tasks are blocked (not failed) until prerequisite completes

### Configuration

```javascript
const daemon = new Daemon({
  daemonId: 'cascade-safe-daemon',
  concurrency: 10,  // Limit concurrent operations to prevent overload
  isolateFailures: true,  // Default: failures don't cascade
});
```

### Example Scenario

```javascript
// Workflow with sequential dependencies
await createWorkflow(store, {
  id: 'etl-pipeline',
  tasks: [
    { id: 'extract', name: 'Extract', kind: 'atomic' },
    { id: 'transform', name: 'Transform', kind: 'atomic' },
    { id: 'load', name: 'Load', kind: 'atomic' },
    { id: 'notify', name: 'Notify', kind: 'atomic' },
  ],
  flow: [
    { from: 'extract', to: 'transform' },
    { from: 'transform', to: 'load' },
    { from: 'load', to: 'notify' },
  ],
});

// Extract task fails
// Expected behavior:
// - Extract: FAILED (after retries)
// - Transform: BLOCKED (waiting for extract)
// - Load: BLOCKED (waiting for transform)
// - Notify: BLOCKED (waiting for load)
// 
// No cascade: Transform/Load/Notify are NOT marked as failed

// Listen for blocked tasks
yawlEngine.on('task:blocked', (event) => {
  console.log(`Task blocked: ${event.taskId}`);
  console.log(`Waiting for: ${event.prerequisiteTasks}`);
  console.log(`Reason: Prerequisite failed or incomplete`);
});
```

### Recovery Strategy

**Automatic Isolation:**
- Failures isolated to individual tasks
- Dependent tasks remain in `enabled` or `waiting` state (not `failed`)
- Case remains open (not cancelled)

**Manual Resolution:**
```bash
# Option 1: Fix root cause and recreate case
node recreate-case.mjs --case-id case-001

# Option 2: Skip failed task (if data loss acceptable)
node skip-task.mjs --case-id case-001 --task-id extract --allow-data-loss

# Option 3: Partial completion (complete successful prefix)
node complete-case-partial.mjs --case-id case-001 --completed-up-to transform
```

### Expected Receipts

```javascript
// Receipt for failed task
{
  id: 'receipt-cascade-extract',
  operationId: 'extract',
  status: 'failed',
  error: { message: 'Database connection failed' },
  timestamp: '2026-01-10T10:17:00Z',
}

// Receipts for blocked tasks (NOT failed)
{
  id: 'receipt-cascade-transform',
  operationId: 'transform',
  status: 'blocked',  // Not failed!
  blockedBy: ['extract'],
  timestamp: '2026-01-10T10:17:00Z',
}
```

---

## Error Type 6: Receipt Verification Failure

### What It Is
Cryptographic hash mismatch when verifying receipt integrity.

### How Daemon Responds
1. Receipt generation always succeeds (best-effort)
2. Verification happens post-execution (async)
3. Verification failure logged as warning (not error)
4. Operation still marked as completed (execution succeeded)
5. Audit flag raised for manual review

### Configuration

```javascript
const daemon = new Daemon({
  daemonId: 'receipt-safe-daemon',
  receipts: {
    enableVerification: true,  // Enable post-execution verification
    verificationMode: 'async',  // Don't block on verification
    onVerificationFailure: 'log',  // Log warning (don't fail operation)
  },
});
```

### Example Scenario

```javascript
// Execute operation with receipt generation
daemon.schedule({
  id: 'data-mutation',
  name: 'Update Database',
  handler: async () => {
    const result = await db.update({ id: 123, value: 'new-value' });
    return result;
  },
});

const result = await daemon.execute('data-mutation');
console.log('Operation completed:', result);

// Listen for verification failures
daemon.on('receipt:verification-failed', (event) => {
  console.warn(`Receipt verification failed: ${event.receiptId}`);
  console.warn(`Operation: ${event.operationId}`);
  console.warn(`Expected hash: ${event.expectedHash}`);
  console.warn(`Actual hash: ${event.actualHash}`);
  
  // Raise audit flag
  raiseAuditFlag({
    receiptId: event.receiptId,
    operationId: event.operationId,
    mismatch: {
      expected: event.expectedHash,
      actual: event.actualHash,
    },
    timestamp: event.timestamp,
  });
  
  // Manual review required
  createAuditTicket({
    title: `Receipt verification failure: ${event.operationId}`,
    severity: 'medium',
    description: 'Hash mismatch detected, manual audit required',
  });
});
```

### Recovery Strategy

**Investigation:**
```bash
# 1. Retrieve receipt
curl "http://localhost:8080/receipts/${receiptId}" | jq

# 2. Recalculate hash from operation data
node verify-receipt.mjs --receipt-id ${receiptId}

# 3. Compare expected vs actual
# If mismatch confirmed:
# - Check for data corruption
# - Check for tampering
# - Review access logs
```

**Resolution:**
```bash
# If false positive (e.g., timestamp skew):
node regenerate-receipt.mjs --operation-id ${operationId}

# If true positive (tampering detected):
# 1. Investigate security incident
# 2. Revert tampered data
# 3. Recreate receipt from audit log
```

### Expected Receipts

```javascript
// Original receipt (possibly corrupted)
{
  id: 'receipt-verification-001',
  operationId: 'data-mutation',
  status: 'completed',  // Operation succeeded
  hash: 'abc123...',  // Hash mismatch detected later
  timestamp: '2026-01-10T10:18:00Z',
  verification: {
    status: 'failed',  // Verification failed
    expectedHash: 'def456...',
    actualHash: 'abc123...',
    verifiedAt: '2026-01-10T10:18:05Z',
  },
}
```

---

## Error Handling Best Practices

### 1. Always Log with Context

```javascript
// GOOD: Rich context
daemon.on('operation:failure', (event) => {
  console.error('Operation failed', {
    operationId: event.operationId,
    name: event.name,
    error: event.error,
    duration: event.duration,
    caseId: event.metadata?.caseId,
    taskId: event.metadata?.taskId,
    attempt: event.attempts || 1,
    timestamp: event.timestamp,
  });
});

// BAD: No context
daemon.on('operation:failure', (event) => {
  console.error('Failed:', event.error);
});
```

### 2. Set Appropriate Timeouts

```javascript
// Calculate timeout based on P95 latency
const p95Latency = 2000;  // 2s (from metrics)
const buffer = 1.5;  // 50% buffer
const timeout = p95Latency * buffer;  // 3s

const bridge = createYawlBridge(daemon, yawlEngine, {
  timeoutDefaults: {
    taskTimeoutMs: timeout,
  },
});
```

### 3. Configure Retry for Transient Failures Only

```javascript
// GOOD: Detect transient vs permanent errors
const handler = async () => {
  try {
    return await apiCall();
  } catch (error) {
    // Mark error as transient or permanent
    error.transient = error.code === 'ECONNREFUSED' || error.status >= 500;
    throw error;
  }
};

bridge.on('task:retry-executed', (event) => {
  if (!event.error?.transient) {
    // Don't retry permanent errors
    bridge.unschedule(event.retryOperationId);
  }
});
```

### 4. Monitor Error Rates

```javascript
// Track error types and rates
const errorStats = {
  taskFailure: 0,
  timeout: 0,
  retryExhaustion: 0,
  total: 0,
};

daemon.on('operation:failure', () => {
  errorStats.taskFailure += 1;
  errorStats.total += 1;
});

bridge.on('task:timeout-enforced', () => {
  errorStats.timeout += 1;
  errorStats.total += 1;
});

bridge.on('task:retry-exhausted', () => {
  errorStats.retryExhaustion += 1;
  errorStats.total += 1;
});

// Export metrics every 1 min
setInterval(() => {
  console.log('Error rates:', {
    taskFailureRate: errorStats.taskFailure / errorStats.total,
    timeoutRate: errorStats.timeout / errorStats.total,
    retryExhaustionRate: errorStats.retryExhaustion / errorStats.total,
  });
}, 60000);
```

### 5. Fail Fast, Recover Gracefully

```javascript
// Validate inputs at workflow creation (fail fast)
try {
  WorkflowSpecSchema.parse(workflowDef);
} catch (error) {
  console.error('Invalid workflow definition:', error.message);
  process.exit(1);  // Fail fast
}

// Recover gracefully from runtime errors
daemon.on('operation:failure', async (event) => {
  // Don't crash, just log and continue
  console.error('Operation failed, will retry:', event.operationId);
});
```

---

## Summary

| Error Type | Prevention | Detection | Recovery |
|------------|-----------|-----------|----------|
| Task Failure | Input validation, error handling | Daemon events, logs | Automatic retry |
| Timeout | Set appropriate thresholds | Timeout events, metrics | Retry or manual fix |
| Retry Exhaustion | Fix permanent errors early | Exhaustion events, alerts | Manual intervention |
| Deadlock | Workflow validation | Cycle detection | Redesign workflow |
| Cascade Failure | Isolate task failures | Health checks, metrics | Partial completion |
| Receipt Verification | Enable verification | Verification events | Manual audit |

---

**Version**: 1.0.0  
**Last Updated**: 2026-01-10  
**Maintainer**: UNRDF Core Team
