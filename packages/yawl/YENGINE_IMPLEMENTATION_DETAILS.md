# YAWL Engine Implementation Details

**CRITICAL NOTE**: This codebase is JavaScript/ESM (.mjs), not Java. The YEngine.java reference does not exist here. This document analyzes the **actual JavaScript implementation** of the YAWL engine in @unrdf/yawl.

---

## 1. Class Structure and Fields

### 1.1 WorkflowEngine (Main Engine Class)

**File**: `/home/user/unrdf/packages/yawl/src/engine.mjs`

**Inheritance Chain** (Mixin Composition):
```
EngineCore
  ← withEvents (engine-events.mjs)
  ← withHooks (engine-hooks.mjs)
  ← withHealth (engine-health.mjs)
  ← withSnapshots (engine-snapshots.mjs)
  ← withQueries (engine-queries.mjs)
  ← WorkflowEngine
```

### 1.2 Core Engine Fields (from EngineCore)

**File**: `/home/user/unrdf/packages/yawl/src/engine-core.mjs:111-199`

```javascript
class EngineCore {
  constructor(config = {}) {
    // Configuration
    this.nodeId = validated.nodeId ?? `yawl-${Date.now()}`;
    this.gitPath = validated.gitPath;
    this.enableEventLog = validated.enableEventLog;  // boolean
    this.enableSnapshots = validated.enableSnapshots;  // boolean
    this.snapshotInterval = validated.snapshotInterval;  // ms
    this.circuitBreakerThreshold = validated.circuitBreakerThreshold;  // int
    this.circuitBreakerResetTimeout = validated.circuitBreakerResetTimeout;  // ms
    this.maxConcurrentCases = validated.maxConcurrentCases;  // default: 1000
    this.defaultTaskTimeout = validated.defaultTaskTimeout;  // default: 30000ms

    // External adapters (optional)
    this._hookAdapter = validated.hookAdapter;
    this._kgcAdapter = validated.kgcAdapter;
    this._supervisorAdapter = validated.supervisorAdapter;

    // Core stores
    this.store = validated.store ?? new KGCStore({ nodeId: this.nodeId });  // KGC-4D store
    this.git = this.gitPath ? new GitBackbone(this.gitPath) : null;  // Git backbone

    // Workflow and case management
    this.workflows = new Map();  // Map<string, YawlWorkflow>
    this.cases = new Map();  // Map<string, YawlCase>

    // Resource management
    this.resourcePool = new YawlResourcePool();

    // Event sourcing
    this.events = [];  // Array<Object> - in-memory event log
    this.checkpoints = new Map();  // Map<bigint, Object> - time-travel checkpoints

    // Event subscription
    this._eventHandlers = new Map();  // Map<string, Set<Function>>

    // Hook registry
    this._policyPacks = new Map();  // Map<string, Object>

    // Circuit breaker state
    this._circuitBreakers = new Map();  // Map<string, Object>

    // Statistics
    this._stats = {
      casesCreated: 0,
      casesCompleted: 0,
      casesFailed: 0,
      casesCancelled: 0,
      tasksEnabled: 0,
      tasksStarted: 0,
      tasksCompleted: 0,
      tasksFailed: 0,
      tasksCancelled: 0,
      tasksTimedOut: 0,
      circuitBreakerTrips: 0,
      checkpointsCreated: 0,
      eventsLogged: 0,
      startedAt: now(),  // bigint nanoseconds
    };

    // Health check state
    this._health = {
      status: HealthStatus.HEALTHY,
      lastCheck: now(),
      components: {
        store: true,
        git: this.git ? true : null,
        workflows: true,
        cases: true,
      },
      errors: [],
    };

    // Snapshot timer
    this._snapshotTimer = null;
  }
}
```

---

## 2. Case Lifecycle Methods - Exact Sequences

### 2.1 createCase() - Complete Sequence

**File**: `/home/user/unrdf/packages/yawl/src/engine.mjs:73-141`

```javascript
async createCase(workflowId, initialData = {}, options = {}) {
  // STEP 1: Lookup workflow
  const workflow = this.workflows.get(workflowId);
  if (!workflow) {
    throw new Error(`Workflow ${workflowId} not found`);
  }

  // STEP 2: Check capacity (NO LOCKING - race condition possible)
  if (this.cases.size >= this.maxConcurrentCases) {
    throw new Error(`Maximum concurrent cases (${this.maxConcurrentCases}) exceeded`);
  }

  // STEP 3: Generate case ID
  const caseId = options.caseId || randomUUID();  // UUID v4

  // STEP 4: Create YawlCase object
  const yawlCase = new YawlCase(
    { id: caseId, workflowId, data: initialData },
    workflow
  );

  // STEP 5: Store in engine (NO LOCKING - race condition possible)
  this.cases.set(caseId, yawlCase);
  this._stats.casesCreated++;

  // STEP 6: Log to KGC-4D (if enabled)
  if (this.enableEventLog) {
    await this._logCaseEvent(YAWL_EVENT_TYPES.CASE_CREATED, {
      caseId: yawlCase.id,
      specId: workflowId,
      timestamp: toISO(now()),
    });
  }

  // STEP 7: Append in-memory event
  this._appendEvent({
    type: 'CASE_CREATED',
    caseId,
    workflowId,
    data: initialData,
  });

  // STEP 8: Emit event to subscribers
  this.emit(ENGINE_EVENTS.CASE_CREATED, { caseId, workflowId, data: initialData });

  // STEP 9: Start the case (enable start task)
  const startResult = await yawlCase.start();

  // STEP 10: Log start events
  this._appendEvent({
    type: 'CASE_STARTED',
    caseId,
    taskId: workflow.startTaskId,
    workItemId: startResult.task.id,
  });

  this.emit(ENGINE_EVENTS.CASE_STARTED, { caseId, workflowId, taskId: workflow.startTaskId, workItemId: startResult.task.id });
  this.emit(ENGINE_EVENTS.TASK_ENABLED, { caseId, taskId: workflow.startTaskId, workItemId: startResult.task.id });

  this._stats.tasksEnabled++;

  return { case: yawlCase, receipt: startResult.receipt };
}
```

**Key Observations**:
- **NO SYNCHRONIZED BLOCKS**: JavaScript is single-threaded (event loop model)
- **Race Condition Risk**: `cases.size` check and `cases.set()` are NOT atomic
- **Case ID**: UUID v4 via Node.js `crypto.randomUUID()`
- **Receipt Chain**: Started immediately with `yawlCase.start()`

### 2.2 YawlCase.start() - Internal Sequence

**File**: `/home/user/unrdf/packages/yawl/src/case-lifecycle.mjs:25-42`

```javascript
async start() {
  // STEP 1: Status check
  if (this._status !== CaseStatus.CREATED) {
    throw new Error(`Cannot start case: already ${this._status}`);
  }

  // STEP 2: Get start task from workflow
  const startTaskId = this.workflow.getStartTaskId();
  if (!startTaskId) {
    throw new Error('Workflow has no start task');
  }

  // STEP 3: Transition status (NO LOCKING)
  this._status = CaseStatus.RUNNING;
  this.startedAt = now();

  // STEP 4: Enable start task (creates work item)
  const task = await this.enableTask(startTaskId);

  return { task: task.task, receipt: task.receipt };
}
```

### 2.3 Case.enableTask() - Task Enabling Sequence

**File**: `/home/user/unrdf/packages/yawl/src/case-lifecycle.mjs:50-112`

```javascript
async enableTask(taskId, actor) {
  // STEP 1: Lookup task definition
  const taskDef = this.workflow.getTask(taskId);
  if (!taskDef) {
    throw new Error(`Task ${taskId} not found in workflow`);
  }

  // STEP 2: Circuit breaker check
  if (this.circuitBreakers.get(taskId) === false) {
    throw new Error(`Task ${taskId} is disabled by circuit breaker`);
  }

  // STEP 3: Capture before state (for receipt)
  const beforeState = this.getState();

  // STEP 4: Create work item with UUID
  const workItemId = randomUUID();
  const task = new YawlTask({
    id: workItemId,
    name: taskDef.name ?? taskId,
    caseId: this.id,
    role: taskDef.role,
    timeout: taskDef.timeout,
    cancellationRegion: taskDef.cancellationRegion,
  });

  // STEP 5: Transition task to ENABLED
  task.enable();

  // STEP 6: Store work item (NO LOCKING)
  this.workItems.set(workItemId, task);
  this.activatedTasks.add(taskId);

  // STEP 7: Track work item by task definition
  if (!this.workItemsByTask.has(taskId)) {
    this.workItemsByTask.set(taskId, new Set());
  }
  this.workItemsByTask.get(taskId).add(workItemId);

  // STEP 8: Update Petri net marking
  this.enable(task);  // Consumes input token, produces enabled token

  // STEP 9: Capture after state
  const afterState = this.getState();
  const previousReceipt = this.receipts[this.receipts.length - 1];

  // STEP 10: Build cryptographic receipt
  const receipt = await buildReceipt({
    caseId: this.id,
    taskId,
    action: 'enable',
    actor,
    beforeState,
    afterState,
    previousReceipt,
  });

  // STEP 11: Append receipt to chain
  this.receipts.push(receipt);

  // STEP 12: Log event
  this._appendEvent({
    type: 'TASK_ENABLED',
    taskId,
    workItemId,
    actor,
    receiptId: receipt.id,
  });

  return { task, receipt };
}
```

---

## 3. State Management - Storage Mechanisms

### 3.1 Case State Storage

**File**: `/home/user/unrdf/packages/yawl/src/case-core.mjs:58-127`

```javascript
class CaseCore {
  constructor(data, workflow) {
    // Metadata
    this.id = validated.id;
    this.workflowId = validated.workflowId;
    this.workflow = workflow;  // Reference to workflow definition
    this._status = validated.status;  // 'created' | 'running' | 'completed' | ...
    this.createdAt = validated.createdAt ?? now();  // bigint nanoseconds
    this.startedAt = validated.startedAt;
    this.completedAt = validated.completedAt;
    this.data = validated.data ?? {};  // Case variables

    // Work item storage
    this.workItems = new Map();  // Map<workItemId, YawlTask>
    this.workItemsByTask = new Map();  // Map<taskDefId, Set<workItemId>>

    // Audit trail
    this.receipts = [];  // Array<YawlReceipt> - cryptographic chain

    // Task tracking
    this.completedTasks = new Set();  // Set<taskDefId>
    this.activatedTasks = new Set();  // Set<taskDefId> - for OR-join

    // Circuit breakers
    this.circuitBreakers = new Map();  // Map<taskDefId, boolean>

    // Petri net state
    this._marking = new Map();  // Map<conditionId, tokenCount>

    // Event log
    this.eventLog = [];  // Array<Object>
  }
}
```

**Storage Location**: In-memory (`Map` data structures)

### 3.2 Petri Net Marking (Token Management)

**File**: `/home/user/unrdf/packages/yawl/src/case-state.mjs:22-157`

```javascript
// Initialize marking with start token
_initializeMarking() {
  const startTaskId = this.workflow.getStartTaskId();
  if (startTaskId) {
    const inputConditionId = `input:${startTaskId}`;
    this._marking.set(inputConditionId, 1);
  }
}

// Token operations (NOT ATOMIC - no locks)
_addTokens(conditionId, count = 1) {
  const current = this._marking.get(conditionId) ?? 0;
  this._marking.set(conditionId, current + count);
}

_removeTokens(conditionId, count = 1) {
  const current = this._marking.get(conditionId) ?? 0;
  if (current < count) return false;
  const remaining = current - count;
  if (remaining === 0) {
    this._marking.delete(conditionId);
  } else {
    this._marking.set(conditionId, remaining);
  }
  return true;
}

// Enable: input → enabled
enable(workItem) {
  const taskDefId = this.getTaskDefIdForWorkItem(workItem.id);
  const inputConditionId = `input:${taskDefId}`;
  const enabledConditionId = `enabled:${taskDefId}`;

  this._removeTokens(inputConditionId, 1);
  this._addTokens(enabledConditionId, 1);

  // Event logging
  this._appendEvent({ type: 'TOKENS_CONSUMED', conditionId: inputConditionId, count: 1, workItemId: workItem.id });
  this._appendEvent({ type: 'TOKENS_PRODUCED', conditionId: enabledConditionId, count: 1, workItemId: workItem.id });
}

// Fire transition: started → output
_fireTransition(workItem) {
  const taskDefId = this.getTaskDefIdForWorkItem(workItem.id);
  const startedConditionId = `started:${taskDefId}`;
  const outputConditionId = `output:${taskDefId}`;

  this._removeTokens(startedConditionId, 1);
  this._addTokens(outputConditionId, 1);

  this._appendEvent({ type: 'TRANSITION_FIRED', taskId: taskDefId, workItemId: workItem.id });
}
```

**Key Insight**: Token operations are **NOT transactional**. No ACID guarantees.

### 3.3 State Persistence Options

**Three-tier storage**:

1. **In-Memory** (Default):
   - `engine.cases` Map
   - `case.workItems` Map
   - `case._marking` Map
   - Lost on engine restart

2. **KGC-4D Event Log** (Optional, `enableEventLog: true`):
   - Events stored in `KGCStore` (RDF triple store backed by Oxigraph)
   - Immutable event stream
   - Queryable via SPARQL
   - Persisted to disk via Oxigraph

3. **Git Snapshots** (Optional, `gitPath` configured):
   - Periodic checkpoints via `freezeUniverse()`
   - Entire store state serialized to Git commits
   - Merkle tree hashing for integrity
   - Time-travel via Git history

---

## 4. Concurrency Model

### 4.1 Thread Model: Event Loop (JavaScript)

**NOT Java threads**. JavaScript uses:

- **Single-threaded event loop** (Node.js)
- **Asynchronous I/O** (non-blocking)
- **No preemptive multithreading**
- **No synchronized blocks**

### 4.2 Locking Mechanisms

**Answer: NONE**

```javascript
// NO equivalent to Java's:
// synchronized (this._pmgr) { ... }

// Example of race condition:
async createCase(workflowId, initialData = {}, options = {}) {
  // RACE: Two calls can both pass this check
  if (this.cases.size >= this.maxConcurrentCases) {
    throw new Error(`Maximum concurrent cases exceeded`);
  }

  const caseId = options.caseId || randomUUID();

  // RACE: Both can create cases with same ID if options.caseId provided
  const yawlCase = new YawlCase({ id: caseId, workflowId, data: initialData }, workflow);

  // RACE: Both can write to Map
  this.cases.set(caseId, yawlCase);
  this._stats.casesCreated++;  // RACE: Not atomic increment

  return { case: yawlCase, receipt: startResult.receipt };
}
```

### 4.3 Concurrency Safety

**How is concurrency handled?**

1. **Event Loop Serialization**:
   - All code runs on single thread
   - `await` yields control but doesn't allow preemption mid-function
   - Two `createCase()` calls CANNOT execute simultaneously
   - Sequential execution even with concurrent HTTP requests

2. **Async I/O**:
   - File I/O (Git snapshots) is non-blocking
   - RDF store queries (Oxigraph) run asynchronously
   - But JavaScript code is serialized

3. **Risk Areas**:
   - **Resource pool**: No transactional allocation
   - **Case count check**: Time-of-check to time-of-use gap
   - **Circuit breaker state**: No atomic compare-and-swap

**Code Evidence**:

```javascript
// File: engine.mjs:80-82
if (this.cases.size >= this.maxConcurrentCases) {
  throw new Error(`Maximum concurrent cases (${this.maxConcurrentCases}) exceeded`);
}
// BUG: In high-concurrency server with async interleaving,
// this check can pass for multiple calls before any sets the case
```

### 4.4 Deadlock Prevention

**Not Applicable**: No locks means no deadlocks.

---

## 5. Case ID Generation Algorithm

**File**: `/home/user/unrdf/packages/yawl/src/engine.mjs:84`

```javascript
const caseId = options.caseId || randomUUID();
```

**Algorithm**: Node.js `crypto.randomUUID()`

**Implementation** (Node.js internal):
- UUID v4 (random)
- 122 random bits
- Uses cryptographically secure random number generator (CSRNG)
- Format: `xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx`
- Example: `9b1deb4d-3b7d-4bad-9bdd-2b0d7b3dcb6d`

**Collision Probability**:
- ~1 in 2^122 (5.3 × 10^36)
- Negligible for practical use

---

## 6. Task Execution Mechanics

### 6.1 Engine.completeTask() - Full Sequence

**File**: `/home/user/unrdf/packages/yawl/src/engine.mjs:293-400`

```javascript
async completeTask(caseId, workItemId, output = {}, actor) {
  // STEP 1: Lookup case
  const yawlCase = this.cases.get(caseId);
  if (!yawlCase) {
    throw new Error(`Case ${caseId} not found`);
  }

  // STEP 2: Lookup work item
  const task = yawlCase.workItems.get(workItemId);
  if (!task) {
    throw new Error(`Work item ${workItemId} not found`);
  }

  // STEP 3: Verify task is running
  if (task.status !== TaskStatus.ACTIVE) {
    throw new Error(`Task ${workItemId} is not active (status: ${task.status})`);
  }

  // STEP 4: Release resource if allocated
  if (task.assignedResource) {
    const nextFromQueue = this.resourcePool.release(task.assignedResource);
    this.emit(ENGINE_EVENTS.RESOURCE_RELEASED, { caseId, workItemId, resourceId: task.assignedResource });

    if (nextFromQueue) {
      this._appendEvent({
        type: 'RESOURCE_REALLOCATED',
        resourceId: nextFromQueue.resource.id,
        fromWorkItemId: workItemId,
        toTaskId: nextFromQueue.taskId,
      });
    }
  }

  // STEP 5: Get task definition ID
  const taskDefId = yawlCase.getTaskDefIdForWorkItem(workItemId);

  // STEP 6: Run post-completion hook (if policy pack exists)
  const hookRouting = await this._executeRoutingHook(
    yawlCase.workflowId,
    taskDefId,
    { caseId, actor, output, env: output }
  );

  // STEP 7: Complete task in case (Petri net transition)
  const result = await yawlCase.completeTask(workItemId, output, actor);

  // STEP 8: Reset circuit breaker on success
  const breakerKey = `${yawlCase.workflowId}:${taskDefId}`;
  this._resetCircuitBreaker(breakerKey);

  // STEP 9: Log event
  this._appendEvent({
    type: 'TASK_COMPLETED',
    caseId,
    workItemId,
    output,
    actor,
    downstreamEnabled: result.downstreamEnabled.map(d => d.taskId),
  });

  // STEP 10: Log to KGC-4D
  if (this.enableEventLog) {
    await this._logTaskEvent(YAWL_EVENT_TYPES.TASK_COMPLETED, {
      workItemId,
      completedAt: toISO(result.task.completedAt),
      result: output,
    }, caseId);
  }

  // STEP 11: Emit events
  this.emit(ENGINE_EVENTS.TASK_COMPLETED, {
    caseId,
    workItemId,
    taskId: taskDefId,
    output,
    actor,
    downstreamEnabled: result.downstreamEnabled,
    hookReceipt: hookRouting?.receipt,
  });

  this._stats.tasksCompleted++;

  // STEP 12: Emit events for downstream tasks
  for (const downstream of result.downstreamEnabled) {
    this.emit(ENGINE_EVENTS.TASK_ENABLED, {
      caseId,
      taskId: downstream.taskId,
      workItemId: downstream.workItemId,
    });
    this._stats.tasksEnabled++;
  }

  // STEP 13: Check if case completed
  if (yawlCase.status === CaseStatus.COMPLETED) {
    this._appendEvent({ type: 'CASE_COMPLETED', caseId });
    this.emit(ENGINE_EVENTS.CASE_COMPLETED, { caseId, workflowId: yawlCase.workflowId });
    this._stats.casesCompleted++;
  }

  return result;
}
```

### 6.2 Case.completeTask() - Internal Sequence

**File**: `/home/user/unrdf/packages/yawl/src/case-lifecycle.mjs:174-256`

```javascript
async completeTask(workItemId, output = {}, actor) {
  // STEP 1: Lookup work item
  const task = this.workItems.get(workItemId);
  if (!task) {
    throw new Error(`Work item ${workItemId} not found`);
  }

  // STEP 2: Capture before state
  const beforeState = this.getState();
  const taskDefId = this.getTaskDefIdForWorkItem(workItemId);

  // STEP 3: Transition task to COMPLETED
  task.complete(output);
  this.completedTasks.add(taskDefId);

  // STEP 4: Fire Petri net transition (consume started token, produce output token)
  this._fireTransition(task);

  // STEP 5: Update case data with task output
  this.data = { ...this.data, ...output };

  // STEP 6: Evaluate downstream tasks (split semantics)
  const toEnable = this.workflow.evaluateDownstream(taskDefId, {
    data: this.data,
    case: this,
    output,
  });

  // STEP 7: Produce tokens in downstream input conditions
  for (const nextTaskId of toEnable) {
    const inputConditionId = `input:${nextTaskId}`;
    this._addTokens(inputConditionId, 1);
  }

  // STEP 8: Enable downstream tasks (check join semantics)
  const downstreamEnabled = [];
  for (const nextTaskId of toEnable) {
    if (this.workflow.canEnable(nextTaskId, this.completedTasks, this.activatedTasks)) {
      const enabled = await this.enableTask(nextTaskId, actor);
      downstreamEnabled.push({
        taskId: nextTaskId,
        enabledAt: enabled.task.enabledAt,
      });
    }
  }

  // STEP 9: Check if case is complete
  const endTaskIds = this.workflow.getEndTaskIds();
  const allEndTasksComplete = endTaskIds.every(id => this.completedTasks.has(id));

  if (allEndTasksComplete) {
    this._status = CaseStatus.COMPLETED;
    this.completedAt = now();
  }

  // STEP 10: Capture after state
  const afterState = this.getState();
  const previousReceipt = this.receipts[this.receipts.length - 1];

  // STEP 11: Build receipt
  const receipt = await buildReceipt({
    caseId: this.id,
    taskId: taskDefId,
    action: 'complete',
    actor,
    beforeState,
    afterState,
    previousReceipt,
    output,
    downstreamEnabled,
  });

  // STEP 12: Append receipt to chain
  this.receipts.push(receipt);

  // STEP 13: Log event
  this._appendEvent({
    type: 'TASK_COMPLETED',
    taskId: taskDefId,
    workItemId,
    output,
    downstreamEnabled: downstreamEnabled.map(d => d.taskId),
    actor,
    receiptId: receipt.id,
  });

  return { task, receipt, downstreamEnabled };
}
```

---

## 7. Receipt Chain Mechanism

### 7.1 Receipt Structure

**File**: `/home/user/unrdf/packages/yawl/src/receipt-core.mjs`

```javascript
{
  id: string,  // UUID v4
  caseId: string,
  taskId: string,
  action: string,  // 'enable' | 'start' | 'complete' | 'cancel' | 'timeout'
  actor: string,
  timestamp: bigint,  // nanoseconds
  beforeHash: string,  // SHA-256 of before state
  afterHash: string,  // SHA-256 of after state
  previousHash: string,  // Hash of previous receipt (blockchain-style)
  chainHash: string,  // Hash of this receipt
  output: Object,  // Task output data
  downstreamEnabled: Array,  // List of enabled tasks
}
```

### 7.2 Receipt Building Sequence

**File**: `/home/user/unrdf/packages/yawl/src/receipt-core.mjs`

```javascript
async function buildReceipt({
  caseId,
  taskId,
  action,
  actor,
  beforeState,
  afterState,
  previousReceipt,
  output,
  downstreamEnabled,
}) {
  const receiptId = randomUUID();
  const timestamp = now();

  // Hash states
  const beforeHash = hashState(beforeState);
  const afterHash = hashState(afterState);

  // Previous receipt hash (blockchain chaining)
  const previousHash = previousReceipt ? previousReceipt.chainHash : '0'.repeat(64);

  // Build receipt object
  const receipt = {
    id: receiptId,
    caseId,
    taskId,
    action,
    actor,
    timestamp,
    beforeHash,
    afterHash,
    previousHash,
    output,
    downstreamEnabled,
  };

  // Compute chain hash (includes previousHash)
  const chainHash = hashReceipt(receipt);
  receipt.chainHash = chainHash;

  return receipt;
}

function hashState(state) {
  const canonical = JSON.stringify(state, Object.keys(state).sort());
  return crypto.createHash('sha256').update(canonical).digest('hex');
}

function hashReceipt(receipt) {
  const canonical = JSON.stringify({
    id: receipt.id,
    caseId: receipt.caseId,
    taskId: receipt.taskId,
    action: receipt.action,
    timestamp: receipt.timestamp.toString(),
    beforeHash: receipt.beforeHash,
    afterHash: receipt.afterHash,
    previousHash: receipt.previousHash,
  });
  return crypto.createHash('sha256').update(canonical).digest('hex');
}
```

---

## 8. Recovery and Persistence

### 8.1 Engine Restart Behavior

**Default (In-Memory Only)**:
```javascript
const engine = createWorkflowEngine();
// All state in RAM
// engine.cases = new Map()
// engine.workflows = new Map()
```

**On restart**: ALL cases lost. No recovery.

### 8.2 With KGC-4D Event Log

**Configuration**:
```javascript
const engine = createWorkflowEngine({
  enableEventLog: true,
  store: new KGCStore({ nodeId: 'production-1' }),
});
```

**Persistence**:
- Events logged to RDF store (Oxigraph)
- Oxigraph persists to disk automatically
- On restart: Must manually reconstruct cases by replaying events

**Manual Reconstruction**:
```javascript
async function recoverCases(engine) {
  const caseIds = await getCaseIdsFromStore(engine.store);

  for (const caseId of caseIds) {
    const history = await getWorkflowAuditTrail(engine.store, caseId);

    // Replay events to rebuild case
    const workflow = await engine.loadWorkflow(history.workflowId);
    const yawlCase = new YawlCase({ id: caseId, workflowId: history.workflowId }, workflow);

    for (const event of history.events) {
      await replayEvent(yawlCase, event);
    }

    engine.cases.set(caseId, yawlCase);
  }
}
```

**NOT automatic**. Engine does NOT auto-recover.

### 8.3 With Git Snapshots

**Configuration**:
```javascript
const engine = createWorkflowEngine({
  enableSnapshots: true,
  gitPath: '/var/lib/yawl/snapshots',
  snapshotInterval: 60000,  // 60 seconds
});
```

**Mechanism**:
```javascript
// Automatic timer (if started)
setInterval(async () => {
  await freezeUniverse(engine.store, engine.git);
}, engine.snapshotInterval);

// Manual checkpoint
await engine.checkpoint('before-release');
```

**Recovery**:
```javascript
// Restore to specific Git commit
const commitHash = 'abc123...';
await engine.git.checkout(commitHash);
await engine.store.loadFromGit(engine.git);

// Replay events since checkpoint
const checkpoint = engine.checkpoints.get(checkpointTime);
const caseStates = checkpoint.caseStates;

for (const [caseId, caseState] of Object.entries(caseStates)) {
  const workflow = await engine.loadWorkflow(caseState.workflowId);
  const yawlCase = YawlCase.fromJSON(caseState, workflow);
  engine.cases.set(caseId, yawlCase);
}
```

---

## 9. Critical Differences from Java YEngine

| Aspect | Java YEngine (Expected) | JavaScript @unrdf/yawl (Actual) |
|--------|------------------------|----------------------------------|
| **Language** | Java | JavaScript/ESM |
| **Concurrency** | Threads + synchronized blocks | Event loop (single-threaded) |
| **Locking** | `synchronized (_pmgr)` | None |
| **Case Storage** | Persistent (DB or file) | In-memory Map (default) |
| **Persistence** | Built-in | Optional (KGC-4D + Git) |
| **Recovery** | Automatic on restart | Manual reconstruction |
| **Thread Safety** | Explicit locks | Event loop serialization |
| **Case ID** | Sequential or UUID | UUID v4 (crypto.randomUUID) |
| **State Model** | Java objects | JavaScript objects + Petri net |
| **Receipts** | Likely absent | Cryptographic chain (SHA-256) |

---

## 10. Performance Characteristics

### 10.1 Bottlenecks

1. **In-Memory Case Storage**:
   - `cases.size >= maxConcurrentCases` → 1000 cases default
   - Each case holds full state + all work items + receipts
   - Memory grows unbounded without cleanup

2. **Receipt Chain**:
   - Every task transition hashes entire case state
   - SHA-256 computation on every action
   - Receipt array grows linearly with case lifetime

3. **Event Log**:
   - SPARQL inserts to Oxigraph (disk I/O)
   - Slows down high-throughput scenarios

4. **No Batching**:
   - Each `completeTask()` is independent
   - No bulk operations

### 10.2 Scalability Limits

```javascript
// Default configuration
{
  maxConcurrentCases: 1000,
  defaultTaskTimeout: 30000,  // 30s
  snapshotInterval: 60000,  // 60s
}
```

**Estimated Limits** (single Node.js process):
- **Cases**: ~1,000 concurrent (configurable)
- **Work Items per Case**: ~10,000 (memory limited)
- **Throughput**: ~100 task completions/sec (with event log)
- **Memory**: ~500 MB per 1,000 cases (estimate)

---

## 11. Code Snippets - Key Operations

### 11.1 Workflow Registration

**File**: `/home/user/unrdf/packages/yawl/src/engine-core.mjs:211-229`

```javascript
registerWorkflow(workflowOrSpec) {
  const workflow =
    workflowOrSpec instanceof YawlWorkflow
      ? workflowOrSpec
      : new YawlWorkflow(workflowOrSpec);

  const validation = workflow.validate();
  if (!validation.valid) {
    throw new Error(`Invalid workflow: ${validation.errors.join(', ')}`);
  }

  workflow.lock();  // Freezes workflow (immutable)
  this.workflows.set(workflow.id, workflow);

  this._onWorkflowRegistered(workflow);  // Mixin hook

  return workflow;
}
```

### 11.2 Resource Allocation

**File**: `/home/user/unrdf/packages/yawl/src/engine.mjs:218-257`

```javascript
async startTask(caseId, workItemId, options = {}) {
  const yawlCase = this.cases.get(caseId);
  const task = yawlCase.workItems.get(workItemId);

  // Try to allocate resource
  let allocatedResource;
  if (task.role || options.resourceId) {
    const allocation = this.resourcePool.allocate({
      taskId: workItemId,
      role: task.role,
      preferredResourceId: options.resourceId,
    });

    if (allocation.queued) {
      throw new Error(`No available resources for role ${task.role}`);
    }

    allocatedResource = allocation.resource;

    this.emit(ENGINE_EVENTS.RESOURCE_ALLOCATED, {
      caseId,
      workItemId,
      resourceId: allocatedResource.id,
      role: task.role,
    });
  }

  const result = await yawlCase.startTask(
    workItemId,
    allocatedResource?.id ?? options.resourceId,
    options.actor
  );

  // ... event logging ...

  return { ...result, resource: allocatedResource };
}
```

### 11.3 Circuit Breaker Logic

**File**: `/home/user/unrdf/packages/yawl/src/engine-execution.mjs:42-46`

```javascript
// Check circuit breaker before enabling task
const breakerKey = `${yawlCase.workflowId}:${taskId}`;
if (isCircuitOpen(engine, breakerKey)) {
  throw new Error(`Circuit breaker open for task ${taskId}`);
}
```

**Breaker State Management**:
```javascript
// Record failure (on timeout or error)
function recordCircuitFailure(engine, breakerKey) {
  if (!engine._circuitBreakers.has(breakerKey)) {
    engine._circuitBreakers.set(breakerKey, { failures: 0, lastFailure: now(), isOpen: false });
  }

  const breaker = engine._circuitBreakers.get(breakerKey);
  breaker.failures++;
  breaker.lastFailure = now();

  if (breaker.failures >= engine.circuitBreakerThreshold) {
    breaker.isOpen = true;
    engine._stats.circuitBreakerTrips++;
  }
}

// Reset on success
function resetCircuitBreaker(engine, breakerKey) {
  if (engine._circuitBreakers.has(breakerKey)) {
    engine._circuitBreakers.set(breakerKey, { failures: 0, lastFailure: 0n, isOpen: false });
  }
}
```

---

## 12. Summary: Implementation Mechanics

### When `engine.createCase(workflowId, data)` is called:

1. **No lock acquired** (JavaScript is single-threaded)
2. **Workflow lookup**: `this.workflows.get(workflowId)`
3. **Capacity check**: `this.cases.size >= maxConcurrentCases` (race-prone but mitigated by event loop)
4. **Case ID**: `randomUUID()` (128-bit UUID v4)
5. **Case creation**: `new YawlCase({ id, workflowId, data }, workflow)`
6. **Storage**: `this.cases.set(caseId, yawlCase)` (in-memory Map)
7. **Event log**: Optional SPARQL insert to Oxigraph
8. **Start case**: `yawlCase.start()` → enables start task
9. **Receipt chain**: First receipt created with `previousHash = 0x00...`
10. **Petri net**: Initial token placed in start task's input condition
11. **Return**: `{ case, receipt }`

### State Storage:

- **Running cases**: `engine.cases` Map (RAM)
- **Work items**: `case.workItems` Map (RAM)
- **Petri net marking**: `case._marking` Map (RAM)
- **Receipts**: `case.receipts` Array (RAM)
- **Events**: `engine.events` Array (RAM) + optional RDF store (disk)
- **Checkpoints**: `engine.checkpoints` Map (RAM) + optional Git (disk)

### Concurrency:

- **No synchronized blocks** (JavaScript doesn't have them)
- **Event loop serialization** prevents simultaneous execution
- **Async I/O** allows concurrent network/disk operations
- **Race conditions possible** (e.g., capacity check) but mitigated by single-threaded execution

### Recovery:

- **Default**: None (all state lost on restart)
- **With event log**: Manual reconstruction by replaying events
- **With Git snapshots**: Manual checkout + case restoration

---

## 13. Code Evidence: Key Files

| File | Purpose | Lines |
|------|---------|-------|
| `/packages/yawl/src/engine.mjs` | Main engine class | 701 |
| `/packages/yawl/src/engine-core.mjs` | Core state + workflow registration | 336 |
| `/packages/yawl/src/engine-execution.mjs` | Task execution methods | 487 |
| `/packages/yawl/src/case-core.mjs` | Case data structures | 257 |
| `/packages/yawl/src/case-lifecycle.mjs` | Case lifecycle methods | 373 |
| `/packages/yawl/src/case-state.mjs` | Petri net marking | 244 |
| `/packages/yawl/src/receipt-core.mjs` | Receipt generation | ~300 |
| `/packages/yawl/src/engine-coordination.mjs` | Events + time-travel | 385 |

**Total Engine Implementation**: ~3,000 lines of JavaScript

---

## Final Note

This is a **JavaScript implementation** of YAWL concepts, NOT the Java reference implementation. The absence of `YEngine.java` is expected. The design favors:

1. **Event sourcing** (receipts + KGC-4D)
2. **Time-travel** (Git snapshots)
3. **Functional programming** (pure functions + immutable workflows)
4. **Petri net semantics** (explicit token management)

The concurrency model is fundamentally different from Java's thread-based approach.
