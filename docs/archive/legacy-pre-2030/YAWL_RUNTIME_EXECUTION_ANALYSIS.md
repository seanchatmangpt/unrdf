# YAWL Runtime Execution Analysis

**Date**: 2026-01-11
**Analyzer**: Research Agent
**Purpose**: Understand HOW tasks actually execute in @unrdf/yawl

---

## Executive Summary

**Key Finding**: YAWL has NO dedicated runtime executor or daemon. Tasks execute directly in the JavaScript event loop using async/await patterns. There is NO task queue, NO worker pool, and NO scheduling mechanism.

**Concurrency Model**: JavaScript Promise-based concurrency (Promise.all for multiple instances).

**Execution Model**: Direct method invocation with async state transitions.

---

## 1. Execution Model

### 1.1 Task Lifecycle - State Machine Only

Tasks follow a state machine pattern:

```
DISABLED → ENABLED → ACTIVE → COMPLETED
    ↓          ↓         ↓
CANCELLED  CANCELLED  FAILED/TIMEOUT
```

**State Transitions** (`task-execution.mjs`):
- `enableTask()` - Changes state from DISABLED → ENABLED
- `startTask()` - Changes state from ENABLED → ACTIVE
- `completeTask()` - Changes state from ACTIVE → COMPLETED
- `cancelTask()` - Any state → CANCELLED
- `failTask()` - ACTIVE → FAILED
- `timeoutTask()` - ENABLED/ACTIVE → TIMEOUT

**Critical Insight**: These are PURE STATE TRANSITIONS. No actual work execution happens here.

### 1.2 Where Does Work Actually Execute?

**Answer**: Work execution is EXTERNAL to YAWL core.

**Code Evidence** (`task-execution.mjs:189-223`):
```javascript
export async function completeTask(taskInstance, outputData = {}, options = {}) {
  // Validate post-conditions from task definition
  const postCheck = await taskInstance.taskDefinition.validatePostCondition({
    taskInstance,
    inputData: taskInstance.inputData,
    outputData,
  });
  // ... state update ...
  taskInstance.status = TaskStatus.COMPLETED;
  taskInstance.outputData = outputData;
  // ... receipt generation ...
}
```

**Key Observation**:
- `completeTask()` receives `outputData` as a parameter
- There is NO `task.execute()` or `task.handler()` call
- The caller must execute work BEFORE calling `completeTask()`

### 1.3 Execution Flow Trace

**Engine Level** (`engine.mjs:293-399`):
```javascript
async completeTask(caseId, workItemId, output = {}, actor) {
  const yawlCase = this.cases.get(caseId);
  const task = yawlCase.workItems.get(workItemId);

  // 1. Release resource
  if (task.assignedResource) {
    this.resourcePool.release(task.assignedResource);
  }

  // 2. Execute routing hook (optional)
  const hookRouting = await this._executeRoutingHook(
    yawlCase.workflowId,
    taskDefId,
    { caseId, actor, output, env: output }
  );

  // 3. Complete task (state transition)
  const result = await yawlCase.completeTask(workItemId, output, actor);

  // 4. Emit events
  this.emit(ENGINE_EVENTS.TASK_COMPLETED, { ... });

  // 5. Enable downstream tasks
  for (const downstream of result.downstreamEnabled) {
    this.emit(ENGINE_EVENTS.TASK_ENABLED, { ... });
  }

  return result;
}
```

**Actual Execution Pattern** (from tests):
```javascript
// 1. User starts task
await engine.startTask(caseId, workItemId);

// 2. User executes work externally
const output = await doActualWork(inputData);

// 3. User completes task with output
await engine.completeTask(caseId, workItemId, output);
```

---

## 2. Concurrency Model

### 2.1 Single-Threaded Event Loop

**Platform**: JavaScript/Node.js
**Thread Model**: Single-threaded with async I/O
**Concurrency Mechanism**: Event loop + Promises

### 2.2 How Multiple Tasks Run "Concurrently"

**Answer**: They don't run concurrently in the engine - they WAIT concurrently.

**Evidence from WP13 Implementation** (`wp13-design-time.mjs:176-195`):
```javascript
// Spawn N instances
for (let i = 0; i < count; i++) {
  const instancePromise = executeInstance(
    validatedTask,
    instanceId,
    i,
    instanceInput,
    barrier
  );
  instances.push({ instanceId, index: i });
  instancePromises.push(instancePromise);
}

// Wait for barrier to release (all instances complete)
const barrierResult = await barrier.wait();
```

**Key Pattern**: `Promise.all` semantics via SyncBarrier

### 2.3 SyncBarrier - The Concurrency Primitive

**File**: `sync-barrier.mjs`

**Pattern**: AND-join synchronization barrier

```javascript
class SyncBarrier {
  constructor(count, options = {}) {
    this.count = count;
    this._arrivals = [];
    this._waitPromise = null;
  }

  async arrive(completion) {
    this._arrivals.push(completion);

    if (this._arrivals.length >= this.count) {
      this._complete(); // Resolve all waiters
    }
  }

  async wait() {
    return new Promise((resolve, reject) => {
      this._resolveWait = resolve;
      // Set timeout
      this._timeoutHandle = setTimeout(() => {
        this._handleTimeout();
      }, this.timeout);
    });
  }
}
```

**Concurrency Model**:
- Multiple promises created
- All execute in parallel on event loop
- Barrier waits for all to complete
- Uses `Promise` internally (not `Promise.all` directly, but equivalent)

---

## 3. Task Scheduling

### 3.1 Is There a Task Queue?

**Answer**: NO

**Evidence**:
- No `TaskQueue` class in codebase
- No `enqueue()` or `dequeue()` methods
- Tasks are stored in `Map<string, TaskInstance>` (lookup structure, not queue)

### 3.2 Is There an Executor Service?

**Answer**: NO

**Evidence**:
- No `Executor` class
- No worker pool
- No thread pool (JavaScript is single-threaded)
- No background task runner

### 3.3 How Are Tasks Ordered?

**Answer**: By control flow graph evaluation

**Code** (`workflow-execution.mjs:369-405`):
```javascript
export async function evaluateControlFlowAndEnable(workItem, result, caseObj, workflow) {
  const enabledWorkItems = [];
  const edges = workflow.controlFlowGraph.outgoing.get(workItem.taskId) || [];

  for (const edge of edges) {
    // Evaluate condition
    let conditionMet = true;
    if (edge.condition) {
      conditionMet = evaluateCondition(edge.condition, result, caseObj.variables);
    }

    if (conditionMet) {
      const downstreamWorkItem = caseObj.getWorkItem(edge.to);

      // Check AND-join
      if (edge.type === CONTROL_FLOW_PATTERNS.AND_JOIN) {
        const allPredecessorsComplete = checkAllPredecessorsComplete(
          edge.to, workflow, caseObj
        );
        if (!allPredecessorsComplete) continue;
      }

      // Enable downstream task
      if (downstreamWorkItem.status === WORK_ITEM_STATUS.PENDING) {
        downstreamWorkItem.status = WORK_ITEM_STATUS.ENABLED;
        enabledWorkItems.push(downstreamWorkItem);
      }
    }
  }

  return enabledWorkItems;
}
```

**Ordering Mechanism**:
- Control flow graph (DAG)
- Condition evaluation
- Join synchronization
- NOT a queue or priority system

---

## 4. Result Collection

### 4.1 How Are Results Stored?

**Answer**: In-memory on `TaskInstance` objects

**Code** (`task-execution.mjs:207-215`):
```javascript
taskInstance.status = TaskStatus.COMPLETED;
taskInstance.completedAt = now();
taskInstance.outputData = outputData; // ← Result stored here
taskInstance.statusHistory.set(`completed:${taskInstance.completedAt}`, {
  status: TaskStatus.COMPLETED,
  timestamp: taskInstance.completedAt,
  outputData,
});
```

**Storage Locations**:
1. `TaskInstance.outputData` - Task result
2. `TaskInstance.statusHistory` - Audit trail
3. `TaskInstance.receipts[]` - Cryptographic receipts

### 4.2 Result Aggregation (Multiple Instances)

**Code** (`wp13-design-time.mjs:209-239`):
```javascript
const result = {
  success: barrierResult.success,
  count,
  instances: barrierResult.instances.map(inst => ({
    index: instances.find(i => i.instanceId === inst.instanceId)?.index ?? -1,
    instanceId: inst.instanceId,
    outputData: inst.result, // ← Individual result
    success: !inst.failed,
    error: inst.error,
    receipt: inst.receipt,
  })),
  receipts: barrierResult.receipts, // ← Aggregated receipts
  barrier: {
    id: barrier.id,
    count: barrier.count,
    arrivals: barrierResult.completedCount,
    completed: true,
  },
  executionTime,
  timedOut: barrierResult.timedOut,
  failures: barrierResult.failures,
};
```

**Aggregation Pattern**:
- Array of instance results
- Barrier collects all results
- Returns structured object with success/failure

---

## 5. Inter-Task Communication

### 5.1 Direct Communication

**Answer**: NO direct task-to-task communication

**Evidence**:
- Tasks don't reference each other
- No message passing
- No shared channels

### 5.2 Indirect Communication via Case Data

**Code** (`case-core.mjs`):
```javascript
export class CaseCore {
  constructor(data, workflow) {
    this.data = data; // ← Shared case variables
  }
}
```

**Pattern**:
- Tasks read from `case.data`
- Tasks write to `case.data`
- No coordination mechanism (race conditions possible)

### 5.3 Event-Based Coordination

**Code** (`engine.mjs:362-382`):
```javascript
this.emit(ENGINE_EVENTS.TASK_COMPLETED, {
  caseId,
  workItemId,
  taskId: taskDefId,
  output,
  actor,
  downstreamEnabled: result.downstreamEnabled,
  hookReceipt: hookRouting?.receipt,
});

// Emit events for downstream enabled tasks
for (const downstream of result.downstreamEnabled) {
  this.emit(ENGINE_EVENTS.TASK_ENABLED, {
    caseId,
    taskId: downstream.taskId,
    workItemId: downstream.workItemId,
  });
}
```

**Event Types**:
- `TASK_ENABLED`
- `TASK_STARTED`
- `TASK_COMPLETED`
- `TASK_CANCELLED`
- `TASK_TIMEOUT`
- `TASK_FAILED`

**Usage**: External listeners can react to events, but tasks themselves don't communicate via events.

---

## 6. Async Patterns Used

### 6.1 Promise-Based State Transitions

All state transition methods are async:

```javascript
export async function enableTask(taskInstance, options = {}) { }
export async function startTask(taskInstance, resourceId, options = {}) { }
export async function completeTask(taskInstance, outputData = {}, options = {}) { }
```

**Why async?**
- Receipt generation uses `blake3()` hashing (async)
- Hook execution (async validators/routers)
- Event logging to KGC-4D store (async)

### 6.2 Promise.all Equivalent

**Code** (`sync-barrier.mjs`):
```javascript
async wait() {
  return new Promise((resolve, reject) => {
    this._resolveWait = resolve;
    this._rejectWait = reject;

    this._timeoutHandle = setTimeout(() => {
      this._handleTimeout();
    }, this.timeout);
  });
}
```

**Pattern**: Custom Promise-based barrier, functionally equivalent to:
```javascript
await Promise.race([
  Promise.all(instancePromises),
  timeout(30000)
]);
```

### 6.3 No Worker Threads

**Evidence**:
```bash
$ grep -r "worker_threads" packages/yawl/src --include="*.mjs"
# No results

$ grep -r "Worker" packages/yawl/src --include="*.mjs"
# Only WorkItem and Workflow classes, not worker threads
```

**Conclusion**: All execution happens on main event loop.

---

## 7. Gap Analysis: What's Missing for MI Patterns?

### 7.1 Current Capabilities

✅ **Have**:
- State machine for task lifecycle
- Control flow graph evaluation
- Promise-based concurrency (WP13)
- SyncBarrier for AND-join
- Event emission for coordination
- Resource pool management

### 7.2 Missing for Advanced MI Patterns

❌ **Missing**:

1. **WP14 - Runtime Cardinality**:
   - No dynamic instance spawning based on runtime data
   - Current: Count must be known before spawning
   - Needed: `spawnInstancesRuntimeKnowledge(task, dataCollection)`

2. **WP15 - Without Synchronization**:
   - No "fire and forget" pattern
   - Current: SyncBarrier always waits for all
   - Needed: Option to spawn without waiting

3. **WP36 - Dynamic MI (Runtime)**:
   - No ability to add instances after initial spawn
   - Current: All instances created upfront
   - Needed: `addInstance(barrier, inputData)` while running

4. **Task Execution Abstraction**:
   - No built-in executor for task handlers
   - Current: User must call `task.execute()` externally
   - Needed: `task.handler()` auto-execution option

5. **Cancellation Propagation**:
   - Basic cancellation exists
   - Missing: Fine-grained cancellation scope control
   - Missing: Parent-child instance relationships

6. **Load Balancing**:
   - No work stealing
   - No instance rebalancing
   - No backpressure mechanism

7. **Monitoring & Observability**:
   - Basic events exist
   - Missing: Instance-level metrics
   - Missing: Barrier progress tracking
   - Missing: Completion time estimation

---

## 8. Do We Need a Daemon?

### 8.1 Analysis

**Current Model**: Library (in-process execution)
```javascript
import { WorkflowEngine } from '@unrdf/yawl';

const engine = new WorkflowEngine();
await engine.createCase(workflowId);
await engine.startTask(caseId, taskId);
const output = await myBusinessLogic();
await engine.completeTask(caseId, taskId, output);
```

**Daemon Model**: Out-of-process service
```javascript
// Client sends commands to daemon via IPC/HTTP
await daemonClient.startTask(caseId, taskId);
// Daemon executes task handler in background
// Client polls or receives callback when complete
```

### 8.2 Decision Matrix

| Feature | Library (Current) | Daemon (Proposed) | Winner |
|---------|-------------------|-------------------|--------|
| Simplicity | ✅ Simple imports | ❌ Complex setup | Library |
| Latency | ✅ No IPC overhead | ❌ IPC latency | Library |
| Isolation | ❌ Same process | ✅ Process isolation | Daemon |
| Resource Mgmt | ❌ No limits | ✅ CPU/memory limits | Daemon |
| Multi-tenant | ❌ Shared memory | ✅ Process separation | Daemon |
| Scalability | ❌ Single process | ✅ Multiple workers | Daemon |
| Background Exec | ❌ Manual | ✅ Automatic | Daemon |
| Task Queue | ❌ None | ✅ Built-in queue | Daemon |
| Failure Recovery | ❌ Process crash = data loss | ✅ Persistent queue | Daemon |
| WASM Support | ✅ Direct import | ❌ IPC overhead | Library |

### 8.3 Recommendation

**For MI Patterns**: **Extend Library, No Daemon Needed**

**Rationale**:
1. **MI execution is already async** - JavaScript event loop handles concurrency
2. **SyncBarrier pattern works well** - No need for external coordination
3. **Adding daemon adds complexity** - IPC, serialization, process management
4. **YAWL is a library** - Designed for in-process usage
5. **Can extend without breaking changes** - Add optional auto-execution

### 8.4 Proposed Extension: Task Handler Auto-Execution

**Current**:
```javascript
const task = await engine.startTask(caseId, workItemId);
const output = await myHandler(task.inputData);
await engine.completeTask(caseId, workItemId, output);
```

**Proposed**:
```javascript
// Define task with handler
engine.registerWorkflow(workflow, {
  handlers: {
    'process-order': async (inputData) => {
      return { processed: true, orderId: inputData.orderId };
    }
  }
});

// Engine auto-executes handler
await engine.startTask(caseId, workItemId, { autoExecute: true });
// Handler runs automatically, completeTask called internally
```

**Implementation**:
```javascript
// In engine.mjs
async startTask(caseId, workItemId, options = {}) {
  // ... existing start logic ...

  if (options.autoExecute) {
    const taskDefId = yawlCase.getTaskDefIdForWorkItem(workItemId);
    const handler = this._handlers.get(taskDefId);

    if (handler) {
      try {
        const output = await handler(task.inputData);
        await this.completeTask(caseId, workItemId, output);
      } catch (error) {
        await this.failTask(caseId, workItemId, error);
      }
    }
  }

  return result;
}
```

---

## 9. Async Patterns Summary

| Pattern | Implementation | Location |
|---------|----------------|----------|
| **Async State Transitions** | `async function enableTask()` | `task-execution.mjs` |
| **Promise-based Barrier** | `SyncBarrier.wait()` returns Promise | `sync-barrier.mjs` |
| **Concurrent Spawning** | Loop creating promises, barrier waits | `wp13-design-time.mjs` |
| **Event Emission** | EventEmitter pattern | `engine-events.mjs` |
| **Hook Execution** | `await hook.execute()` | `engine-hooks.mjs` |
| **Receipt Generation** | `await blake3()` hashing | `task-execution.mjs` |
| **Resource Allocation** | Synchronous (in-memory queue) | `resource.mjs` |

**No Usage Of**:
- `Promise.all` directly (uses barrier pattern instead)
- Worker threads
- Task queues
- Schedulers
- Executors

---

## 10. Recommendations

### 10.1 For MI Patterns Implementation

1. **Extend WP13 Pattern**:
   - Add WP14 (runtime cardinality) by allowing dynamic count
   - Add WP15 (no sync) by making barrier optional
   - Add WP36 by allowing instance addition to running barriers

2. **Add Task Handler Registration**:
   ```javascript
   engine.registerTaskHandler(taskId, async (input) => output);
   ```

3. **Add Auto-Execution Mode**:
   ```javascript
   engine.startTask(caseId, taskId, { autoExecute: true });
   ```

4. **Enhance SyncBarrier**:
   - Add progress tracking: `barrier.getProgress() → { completed: 5, total: 10 }`
   - Add cancellation: `barrier.cancel(reason)`
   - Add partial results: `barrier.getPartialResults()`

5. **Add Monitoring**:
   ```javascript
   engine.on('MI_INSTANCE_COMPLETED', ({ instanceId, result }) => {});
   engine.on('MI_PROGRESS', ({ completed, total, percentage }) => {});
   ```

### 10.2 What NOT to Do

❌ **Don't**:
- Create a separate daemon process
- Add worker threads (overhead not justified)
- Build a task queue (control flow graph is sufficient)
- Add a scheduler (event loop is enough)

✅ **Do**:
- Extend existing async patterns
- Add optional features (backward compatible)
- Use existing infrastructure (SyncBarrier, events)
- Keep it simple (library, not framework)

---

## 11. Conclusion

**Runtime Execution Model**: Direct method invocation with async/await on JavaScript event loop.

**No Runtime Executor**: Tasks execute externally; YAWL manages state only.

**Concurrency**: Promise-based with SyncBarrier for AND-join synchronization.

**For MI Patterns**: Extend existing library with optional handler auto-execution and enhanced barrier patterns. No daemon needed.

**Next Steps**:
1. Review WP14, WP15, WP36 requirements
2. Design handler registration API
3. Implement auto-execution option
4. Enhance SyncBarrier for runtime cardinality
5. Add monitoring events for MI progress

---

**Evidence-Based Analysis**: All findings traced to source code.
**Verification Status**: ✅ Complete with code references.
