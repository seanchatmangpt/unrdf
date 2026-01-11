# WP1-11 Infrastructure Patterns - Proven Implementation Guide

**Document Purpose**: Reverse-engineered patterns from WP1-11 for reuse in WP12-15 (Multiple Instance patterns)

**Last Updated**: 2026-01-11
**Source**: UNRDF YAWL v6.0.0 production codebase
**Target**: WP12-15 Multiple Instance pattern implementations

---

## Table of Contents

1. [State Machine Pattern](#1-state-machine-pattern)
2. [Receipt Chain Pattern](#2-receipt-chain-pattern)
3. [Async Execution Pattern](#3-async-execution-pattern)
4. [Event Bus Pattern](#4-event-bus-pattern)
5. [Validation Pattern](#5-validation-pattern)
6. [Control Flow Evaluation Pattern](#6-control-flow-evaluation-pattern)
7. [Error Handling with Circuit Breaker](#7-error-handling-with-circuit-breaker)
8. [Prototype Extension Pattern](#8-prototype-extension-pattern)
9. [Reusable Abstractions for MI Patterns](#9-reusable-abstractions-for-mi-patterns)

---

## 1. State Machine Pattern

### Pattern Overview

**Location**: `task-core.mjs`, `task-execution.mjs`
**Purpose**: Enforce valid state transitions with immutable transition rules

### Core Implementation

```javascript
// 1. Define state enumeration (frozen)
export const TaskStatus = Object.freeze({
  DISABLED: 'disabled',
  ENABLED: 'enabled',
  ACTIVE: 'active',
  COMPLETED: 'completed',
  CANCELLED: 'cancelled',
  FAILED: 'failed',
  TIMEOUT: 'timeout',
});

// 2. Define valid transition graph
export const VALID_TRANSITIONS = Object.freeze({
  [TaskStatus.DISABLED]: [TaskStatus.ENABLED, TaskStatus.CANCELLED],
  [TaskStatus.ENABLED]: [TaskStatus.DISABLED, TaskStatus.ACTIVE, TaskStatus.CANCELLED, TaskStatus.TIMEOUT],
  [TaskStatus.ACTIVE]: [TaskStatus.COMPLETED, TaskStatus.CANCELLED, TaskStatus.FAILED, TaskStatus.TIMEOUT],
  [TaskStatus.COMPLETED]: [], // Terminal state
  [TaskStatus.CANCELLED]: [], // Terminal state
  [TaskStatus.FAILED]: [], // Terminal state
  [TaskStatus.TIMEOUT]: [], // Terminal state
});

// 3. Validate transitions before execution
export function validateTransition(fromStatus, toStatus) {
  const validTargets = VALID_TRANSITIONS[fromStatus] ?? [];
  if (!validTargets.includes(toStatus)) {
    return {
      valid: false,
      reason: `Invalid transition from ${fromStatus} to ${toStatus}. Valid targets: ${validTargets.join(', ') || 'none'}`,
    };
  }
  return { valid: true };
}

// 4. State transition with validation + receipt
export async function enableTask(taskInstance, options = {}) {
  // STEP 1: Validate transition is legal
  const validation = validateTransition(taskInstance.status, TaskStatus.ENABLED);
  if (!validation.valid) {
    throw new Error(`Cannot enable task ${taskInstance.id}: ${validation.reason}`);
  }

  // STEP 2: Validate pre-conditions (business rules)
  const preCheck = await taskInstance.taskDefinition.validatePreCondition({
    taskInstance,
    inputData: taskInstance.inputData,
  });
  if (!preCheck.valid) {
    throw new Error(`Cannot enable task ${taskInstance.id}: ${preCheck.reason}`);
  }

  // STEP 3: Capture before-state for receipt
  const beforeStatus = taskInstance.status;
  const beforeHash = await computeStateHash(taskInstance);

  // STEP 4: Execute state transition
  taskInstance.status = TaskStatus.ENABLED;
  taskInstance.enabledAt = now();
  taskInstance.statusHistory.set(`enabled:${taskInstance.enabledAt}`, {
    status: TaskStatus.ENABLED,
    timestamp: taskInstance.enabledAt,
  });

  // STEP 5: Generate cryptographic receipt
  const receipt = await generateReceipt(taskInstance, 'enable', beforeStatus, beforeHash, {
    ...options.justification,
    reason: options.justification?.reason ?? 'Task enabled - pre-conditions satisfied',
  });

  return { task: taskInstance, receipt };
}
```

### Key Characteristics

1. **Immutable transition rules** - Use `Object.freeze()` to prevent runtime modification
2. **Validation before mutation** - Always validate BEFORE changing state
3. **Cryptographic audit trail** - Every transition generates a hash-chained receipt
4. **Status history** - Maintain timestamped history of all state changes
5. **Pre/post-condition hooks** - Business rule validation at transition points

### Reusability for WP12-15

```javascript
// For MI patterns, extend with instance-level states:
export const InstanceStatus = Object.freeze({
  SPAWNED: 'spawned',        // Instance created
  ENABLED: 'enabled',         // Ready to run
  ACTIVE: 'active',           // Running
  COMPLETED: 'completed',     // Done
  FAILED: 'failed',           // Error
  CANCELLED: 'cancelled',     // Cancelled
  BARRIER_WAITING: 'barrier_waiting', // WP13/14: Waiting at barrier
});

export const INSTANCE_VALID_TRANSITIONS = Object.freeze({
  [InstanceStatus.SPAWNED]: [InstanceStatus.ENABLED, InstanceStatus.CANCELLED],
  [InstanceStatus.ENABLED]: [InstanceStatus.ACTIVE, InstanceStatus.CANCELLED],
  [InstanceStatus.ACTIVE]: [InstanceStatus.COMPLETED, InstanceStatus.FAILED, InstanceStatus.CANCELLED],
  [InstanceStatus.COMPLETED]: [InstanceStatus.BARRIER_WAITING], // WP13/14 only
  [InstanceStatus.BARRIER_WAITING]: [InstanceStatus.ENABLED, InstanceStatus.CANCELLED], // Barrier release
  [InstanceStatus.FAILED]: [],
  [InstanceStatus.CANCELLED]: [],
});
```

---

## 2. Receipt Chain Pattern

### Pattern Overview

**Location**: `task-execution.mjs`, `receipt-core.mjs`, `receipt-chain.mjs`
**Purpose**: Cryptographic hash chain for auditability and proof of execution

### Core Implementation

```javascript
// 1. Receipt generation with hash chaining
export async function generateReceipt(taskInstance, action, beforeStatus, beforeHash, justification = {}) {
  const afterHash = await computeStateHash(taskInstance);

  const receipt = {
    id: `receipt-${taskInstance.id}-${action}-${Date.now()}`,
    taskInstanceId: taskInstance.id,
    caseId: taskInstance.caseId,
    action,
    timestamp: now(),
    beforeStatus,
    afterStatus: taskInstance.status,
    beforeHash,
    afterHash,
    previousReceiptHash: taskInstance._lastReceiptHash, // CRITICAL: Chain link
    justification: {
      hookId: justification.hookId,
      reason: justification.reason ?? `Transition ${action} executed`,
      validated: justification.validated ?? true,
    },
    actor: justification.actor,
    inputData: action === 'start' ? taskInstance.inputData : undefined,
    outputData: action === 'complete' ? taskInstance.outputData : undefined,
  };

  // 2. Compute receipt hash for chaining (deterministic fields only)
  const receiptHash = await blake3(JSON.stringify({
    id: receipt.id,
    taskInstanceId: receipt.taskInstanceId,
    caseId: receipt.caseId,
    action: receipt.action,
    timestamp: receipt.timestamp.toString(),
    beforeHash: receipt.beforeHash,
    afterHash: receipt.afterHash,
    previousReceiptHash: receipt.previousReceiptHash,
  }));

  // 3. Update chain state
  receipt.hash = receiptHash;
  taskInstance._lastReceiptHash = receiptHash; // Next receipt will reference this
  taskInstance.receipts.push(receipt);

  return receipt;
}

// 4. State hashing (deterministic serialization)
export async function computeStateHash(taskInstance) {
  const state = {
    id: taskInstance.id,
    taskDefId: taskInstance.taskDefId,
    caseId: taskInstance.caseId,
    status: taskInstance.status,
    inputData: taskInstance.inputData,
    outputData: taskInstance.outputData,
    enabledAt: taskInstance.enabledAt?.toString(),
    startedAt: taskInstance.startedAt?.toString(),
    completedAt: taskInstance.completedAt?.toString(),
  };
  return blake3(JSON.stringify(state));
}

// 5. Receipt chain verification
export async function verifyReceiptChain(receipts) {
  const errors = [];
  let previousHash = null;

  for (let i = 0; i < receipts.length; i++) {
    const receipt = receipts[i];

    // Verify chain link
    if (receipt.previousReceiptHash !== previousHash) {
      errors.push(
        `Receipt ${i} chain broken: expected previousHash ${previousHash}, got ${receipt.previousReceiptHash}`
      );
    }

    // Verify receipt hash
    const computedHash = await blake3(JSON.stringify({
      id: receipt.id,
      taskInstanceId: receipt.taskInstanceId,
      caseId: receipt.caseId,
      action: receipt.action,
      timestamp: receipt.timestamp.toString(),
      beforeHash: receipt.beforeHash,
      afterHash: receipt.afterHash,
      previousReceiptHash: receipt.previousReceiptHash,
    }));

    if (computedHash !== receipt.hash) {
      errors.push(`Receipt ${i} hash mismatch: computed ${computedHash}, stored ${receipt.hash}`);
    }

    previousHash = receipt.hash;
  }

  return {
    valid: errors.length === 0,
    errors,
  };
}
```

### Key Characteristics

1. **Hash chaining** - Each receipt references previous receipt's hash
2. **Deterministic serialization** - Only stable fields in hash computation
3. **Before/after state capture** - State hash at transition boundaries
4. **Justification metadata** - Why the transition occurred
5. **Verification** - Can verify chain integrity cryptographically

### Reusability for WP12-15

```javascript
// For MI patterns, track aggregate receipts:
export async function generateAggregateReceipt(parentTask, instances, action) {
  const instanceHashes = await Promise.all(
    instances.map(inst => computeStateHash(inst))
  );

  const aggregateReceipt = {
    id: `aggregate-${parentTask.id}-${action}-${Date.now()}`,
    parentTaskId: parentTask.id,
    action,
    timestamp: now(),
    instanceCount: instances.length,
    instanceHashes,
    // Merkle root of instance hashes for efficient verification
    merkleRoot: await computeMerkleRoot(instanceHashes),
    previousReceiptHash: parentTask._lastReceiptHash,
  };

  const receiptHash = await blake3(JSON.stringify({
    id: aggregateReceipt.id,
    parentTaskId: aggregateReceipt.parentTaskId,
    action: aggregateReceipt.action,
    timestamp: aggregateReceipt.timestamp.toString(),
    merkleRoot: aggregateReceipt.merkleRoot,
    previousReceiptHash: aggregateReceipt.previousReceiptHash,
  }));

  aggregateReceipt.hash = receiptHash;
  parentTask._lastReceiptHash = receiptHash;
  parentTask.receipts.push(aggregateReceipt);

  return aggregateReceipt;
}
```

---

## 3. Async Execution Pattern

### Pattern Overview

**Location**: `engine-execution.mjs`, `case-lifecycle.mjs`
**Purpose**: Async task execution with resource allocation, error handling, and downstream propagation

### Core Implementation

```javascript
// 1. Enable task (async with validation)
export async function enableTask(engine, caseId, taskId, actor) {
  const { YAWL_EVENT_TYPES } = await import('./events/yawl-events.mjs');

  const yawlCase = engine.cases.get(caseId);
  if (!yawlCase) {
    throw new Error(`Case ${caseId} not found`);
  }

  // Check circuit breaker
  const breakerKey = `${yawlCase.workflowId}:${taskId}`;
  if (isCircuitOpen(engine, breakerKey)) {
    throw new Error(`Circuit breaker open for task ${taskId}`);
  }

  // Run pre-enablement hook if policy pack exists
  const policyPack = engine._policyPacks.get(yawlCase.workflowId);
  if (policyPack && policyPack.getValidator) {
    const validator = policyPack.getValidator(taskId);
    if (validator) {
      const validation = await validator(engine.store, { caseId, actor });
      if (!validation.valid) {
        throw new Error(`Task enablement denied: ${validation.receipt?.justification?.reason || 'Unknown'}`);
      }
    }
  }

  // Execute enable transition
  const result = await yawlCase.enableTask(taskId, actor);

  // Append event to in-memory log
  appendEvent(engine, {
    type: 'TASK_ENABLED',
    caseId,
    taskId,
    workItemId: result.task.id,
    actor,
  });

  // Log to KGC-4D event store (optional)
  if (engine.enableEventLog) {
    await logTaskEvent(engine, YAWL_EVENT_TYPES.TASK_ENABLED, {
      caseId,
      taskId,
      workItemId: result.task.id,
      enabledAt: toISO(result.task.enabledAt),
    });
  }

  // Emit to event subscribers
  engine.emit(ENGINE_EVENTS.TASK_ENABLED, {
    caseId,
    taskId,
    workItemId: result.task.id,
    actor,
  });

  engine._stats.tasksEnabled++;

  return result;
}

// 2. Complete task with downstream propagation
export async function completeTask(engine, caseId, workItemId, output = {}, actor) {
  const { YAWL_EVENT_TYPES } = await import('./events/yawl-events.mjs');

  const yawlCase = engine.cases.get(caseId);
  if (!yawlCase) {
    throw new Error(`Case ${caseId} not found`);
  }

  const task = yawlCase.workItems.get(workItemId);
  if (!task) {
    throw new Error(`Work item ${workItemId} not found`);
  }

  // Verify task is active
  if (task.status !== TaskStatus_RUNNING) {
    throw new Error(`Task ${workItemId} is not running (status: ${task.status})`);
  }

  // Release resource if allocated
  if (task.assignedResource) {
    const nextFromQueue = engine.resourcePool.release(task.assignedResource);

    engine.emit(ENGINE_EVENTS.RESOURCE_RELEASED, {
      caseId,
      workItemId,
      resourceId: task.assignedResource,
    });

    if (nextFromQueue) {
      appendEvent(engine, {
        type: 'RESOURCE_REALLOCATED',
        resourceId: nextFromQueue.resource.id,
        fromWorkItemId: workItemId,
        toTaskId: nextFromQueue.taskId,
      });
    }
  }

  // Run post-completion hook if policy pack exists
  const policyPack = engine._policyPacks.get(yawlCase.workflowId);
  const taskDefId = yawlCase.getTaskDefIdForWorkItem(workItemId);

  let hookRouting = null;
  if (policyPack && policyPack.getRouter) {
    const router = policyPack.getRouter(taskDefId);
    if (router) {
      hookRouting = await router(engine.store, {
        caseId,
        actor,
        output,
        env: output,
      });
    }
  }

  // Execute complete transition + evaluate downstream
  const result = await yawlCase.completeTask(workItemId, output, actor);

  // Reset circuit breaker on success
  const breakerKey = `${yawlCase.workflowId}:${taskDefId}`;
  resetCircuitBreaker(engine, breakerKey);

  appendEvent(engine, {
    type: 'TASK_COMPLETED',
    caseId,
    workItemId,
    output,
    actor,
    downstreamEnabled: result.downstreamEnabled.map(d => d.taskId),
  });

  // Emit events
  engine.emit(ENGINE_EVENTS.TASK_COMPLETED, {
    caseId,
    workItemId,
    taskId: taskDefId,
    output,
    actor,
    downstreamEnabled: result.downstreamEnabled,
    hookReceipt: hookRouting?.receipt,
  });

  engine._stats.tasksCompleted++;

  // CRITICAL: Emit events for downstream enabled tasks
  for (const downstream of result.downstreamEnabled) {
    engine.emit(ENGINE_EVENTS.TASK_ENABLED, {
      caseId,
      taskId: downstream.taskId,
      workItemId: downstream.workItemId,
    });
    engine._stats.tasksEnabled++;
  }

  // Check if case completed
  if (yawlCase.status === CaseStatus.COMPLETED) {
    appendEvent(engine, {
      type: 'CASE_COMPLETED',
      caseId,
    });

    engine.emit(ENGINE_EVENTS.CASE_COMPLETED, {
      caseId,
      workflowId: yawlCase.workflowId,
    });

    engine._stats.casesCompleted++;
  }

  return result;
}
```

### Key Characteristics

1. **Pre-validation** - Check circuit breaker, policy hooks before execution
2. **Resource management** - Allocate/release resources automatically
3. **Hook integration** - Pre/post hooks for business logic
4. **Event emission** - Multiple layers (in-memory, KGC-4D, subscribers)
5. **Downstream propagation** - Automatically enable next tasks in workflow
6. **Error recovery** - Circuit breaker integration for fault tolerance

### Reusability for WP12-15

```javascript
// For MI patterns, batch spawn with async instance creation:
export async function spawnInstancesAsync(taskDef, caseId, count, options = {}) {
  const spawnStartTime = now();

  // Pre-spawn validation
  if (count <= 0) {
    throw new Error(`Instance count must be positive, got ${count}`);
  }

  // Create instances concurrently
  const instances = await Promise.all(
    Array.from({ length: count }, async (_, i) => {
      const instanceId = `${taskDef.id}-instance-${i}-${Date.now()}`;
      const inputData = {
        ...options.baseInputData,
        ...(options.instanceInputs?.[i] || {}),
        _instanceIndex: i,
        _totalInstances: count,
      };

      const instance = new TaskInstance(taskDef, caseId, {
        id: instanceId,
        inputData,
      });

      // Track instance
      if (options.tracker) {
        options.tracker.addInstance(taskDef.id, instanceId, {
          index: i,
          status: InstanceStatus.SPAWNED,
        });
      }

      return instance;
    })
  );

  // Generate aggregate receipt for spawn operation
  const aggregateReceipt = await generateAggregateReceipt(
    { id: taskDef.id, caseId },
    instances,
    'spawn'
  );

  const spawnEndTime = now();

  return {
    parentTaskId: taskDef.id,
    instanceIds: instances.map(i => i.id),
    instances,
    aggregateReceipt,
    metadata: {
      totalInstances: count,
      spawnedAt: spawnStartTime,
      completedAt: spawnEndTime,
      duration: Number(spawnEndTime - spawnStartTime) / 1_000_000, // Convert ns to ms
    },
  };
}
```

---

## 4. Event Bus Pattern

### Pattern Overview

**Location**: `engine-events.mjs`
**Purpose**: Publish-subscribe event system for decoupled coordination

### Core Implementation

```javascript
// 1. Event subscription with unsubscribe function
on(eventType, handler) {
  if (typeof handler !== 'function') {
    throw new TypeError('Handler must be a function');
  }

  if (!this._eventHandlers.has(eventType)) {
    this._eventHandlers.set(eventType, new Set());
  }

  this._eventHandlers.get(eventType).add(handler);

  // Return unsubscribe function
  return () => {
    const handlers = this._eventHandlers.get(eventType);
    if (handlers) {
      handlers.delete(handler);
    }
  };
}

// 2. Event emission with error isolation
emit(eventType, data) {
  const handlers = this._eventHandlers.get(eventType);
  if (!handlers) return;

  const event = {
    type: eventType,
    timestamp: now().toString(),
    timestampISO: toISO(now()),
    ...data,
  };

  for (const handler of handlers) {
    try {
      handler(event);
    } catch (error) {
      // Log error but don't throw to avoid disrupting other handlers
      console.error(`Error in event handler for ${eventType}:`, error);
    }
  }
}

// 3. Remove all handlers for an event type
off(eventType) {
  this._eventHandlers.delete(eventType);
}

// 4. In-memory event log
_appendEvent(eventData) {
  const timestamp = now();
  this.events.push({
    ...eventData,
    timestamp: timestamp.toString(),
    timestampISO: toISO(timestamp),
  });
  this._stats.eventsLogged++;
}

// 5. Query events
getEventsForCase(caseId) {
  return this.events.filter(e => e.caseId === caseId);
}

getAllEvents() {
  return [...this.events];
}
```

### Key Characteristics

1. **Decoupled communication** - Publishers don't know subscribers
2. **Error isolation** - Handler errors don't disrupt other handlers
3. **Unsubscribe pattern** - Clean removal of handlers
4. **Timestamped events** - All events get KGC-4D timestamps
5. **Multiple subscribers** - Many handlers can listen to same event

### Reusability for WP12-15

```javascript
// For MI patterns, emit instance-level events:
export const MI_EVENTS = Object.freeze({
  INSTANCE_SPAWNED: 'mi:instance:spawned',
  INSTANCE_ENABLED: 'mi:instance:enabled',
  INSTANCE_STARTED: 'mi:instance:started',
  INSTANCE_COMPLETED: 'mi:instance:completed',
  INSTANCE_FAILED: 'mi:instance:failed',
  BARRIER_REACHED: 'mi:barrier:reached',        // WP13/14
  BARRIER_RELEASED: 'mi:barrier:released',      // WP13/14
  THRESHOLD_MET: 'mi:threshold:met',            // WP14
  ALL_INSTANCES_COMPLETED: 'mi:all:completed',  // WP13
});

// Example: Track barrier progress with events
tracker.on(MI_EVENTS.INSTANCE_COMPLETED, (event) => {
  const { instanceId, parentTaskId } = event;

  const parent = tracker.getParentTask(parentTaskId);
  const completedCount = tracker.getCompletedCount(parentTaskId);

  if (parent.barrierThreshold && completedCount >= parent.barrierThreshold) {
    tracker.emit(MI_EVENTS.THRESHOLD_MET, {
      parentTaskId,
      completedCount,
      threshold: parent.barrierThreshold,
      timestamp: now(),
    });
  }
});
```

---

## 5. Validation Pattern

### Pattern Overview

**Location**: `task-validation.mjs`, `workflow-validation.mjs`
**Purpose**: Multi-layer validation (structural, business rules, receipts)

### Core Implementation

```javascript
// 1. Pre-condition validation (business rules)
export async function validatePreCondition(taskDef, context) {
  if (!taskDef.preCondition) {
    return { valid: true };
  }
  try {
    const result = await taskDef.preCondition(context);
    return typeof result === 'boolean'
      ? { valid: result, reason: result ? undefined : 'Pre-condition failed' }
      : result;
  } catch (error) {
    return { valid: false, reason: `Pre-condition error: ${error.message}` };
  }
}

// 2. Post-condition validation (output verification)
export async function validatePostCondition(taskDef, context) {
  if (!taskDef.postCondition) {
    return { valid: true };
  }
  try {
    const result = await taskDef.postCondition(context);
    return typeof result === 'boolean'
      ? { valid: result, reason: result ? undefined : 'Post-condition failed' }
      : result;
  } catch (error) {
    return { valid: false, reason: `Post-condition error: ${error.message}` };
  }
}

// 3. Workflow structure validation
Workflow.prototype.validate = function() {
  const errors = [];
  const warnings = [];

  this._validateBasicStructure(errors, warnings);
  this._validateControlFlowIntegrity(errors, warnings);
  this._validateSplitJoinConsistency(errors, warnings);
  this._validateReachability(errors, warnings);
  this._validateCancellationRegions(errors, warnings);
  this._validateNoCycles(errors, warnings);

  return { valid: errors.length === 0, errors, warnings };
};

// 4. Reachability validation (all tasks reachable from start)
Workflow.prototype._validateReachability = function(errors, warnings) {
  if (!this._startTaskId) return;

  const visited = new Set();
  const queue = [this._startTaskId];

  while (queue.length > 0) {
    const taskId = queue.shift();
    if (visited.has(taskId)) continue;
    visited.add(taskId);

    const outgoing = this._outgoingFlows.get(taskId) ?? [];
    for (const flow of outgoing) {
      if (!visited.has(flow.to)) queue.push(flow.to);
    }
  }

  for (const [taskId] of this._tasks) {
    if (!visited.has(taskId)) {
      errors.push(`Task '${taskId}' is not reachable from start task`);
    }
  }
};

// 5. Split/join consistency validation
Workflow.prototype._validateSplitJoinConsistency = function(errors, warnings) {
  for (const [taskId, task] of this._tasks) {
    const outgoing = this._outgoingFlows.get(taskId) ?? [];
    const incoming = this._incomingFlows.get(taskId) ?? [];

    const splitType = task.splitType ?? SPLIT_TYPE.SEQUENCE;
    const joinType = task.joinType ?? JOIN_TYPE.SEQUENCE;

    // Validate split cardinality
    if (splitType === SPLIT_TYPE.SEQUENCE && outgoing.length > 1) {
      errors.push(`Task '${taskId}' has sequence split but ${outgoing.length} outgoing flows`);
    }

    if ((splitType === SPLIT_TYPE.AND || splitType === SPLIT_TYPE.XOR || splitType === SPLIT_TYPE.OR)
        && outgoing.length < 2) {
      warnings.push(`Task '${taskId}' has ${splitType} split but only ${outgoing.length} outgoing flow(s)`);
    }

    // Validate join cardinality
    if (joinType === JOIN_TYPE.SEQUENCE && incoming.length > 1) {
      errors.push(`Task '${taskId}' has sequence join but ${incoming.length} incoming flows`);
    }

    if ((joinType === JOIN_TYPE.AND || joinType === JOIN_TYPE.XOR || joinType === JOIN_TYPE.OR)
        && incoming.length < 2) {
      warnings.push(`Task '${taskId}' has ${joinType} join but only ${incoming.length} incoming flow(s)`);
    }
  }
};
```

### Key Characteristics

1. **Layered validation** - Structural, state machine, business rules, cryptographic
2. **Try-catch safety** - Validation errors don't crash the engine
3. **Boolean or detailed result** - Flexible return types
4. **Graph analysis** - Reachability, cycles, split/join matching
5. **Warnings vs errors** - Non-blocking warnings for best practices

### Reusability for WP12-15

```javascript
// For MI patterns, add instance-level validation:
export async function validateInstanceSpawn(taskDef, count, options) {
  const errors = [];
  const warnings = [];

  // Validate count
  if (count <= 0) {
    errors.push(`Instance count must be positive, got ${count}`);
  }
  if (count > 1000) {
    warnings.push(`Instance count ${count} is very high, may impact performance`);
  }

  // Validate input data cardinality
  if (options.instanceInputs && options.instanceInputs.length !== count) {
    errors.push(
      `instanceInputs length (${options.instanceInputs.length}) must match count (${count})`
    );
  }

  // Validate task definition supports MI
  if (!taskDef.kind || taskDef.kind !== 'MultipleInstanceTask') {
    warnings.push(
      `Task '${taskDef.id}' kind is '${taskDef.kind}', expected 'MultipleInstanceTask'`
    );
  }

  // Validate barrier configuration (WP13/14)
  if (options.barrierType && !['static', 'dynamic', 'threshold'].includes(options.barrierType)) {
    errors.push(`Invalid barrierType '${options.barrierType}', expected static/dynamic/threshold`);
  }

  if (options.barrierType === 'threshold' && !options.threshold) {
    errors.push(`barrierType 'threshold' requires threshold option`);
  }

  return {
    valid: errors.length === 0,
    errors,
    warnings,
  };
}
```

---

## 6. Control Flow Evaluation Pattern

### Pattern Overview

**Location**: `workflow-execution.mjs`
**Purpose**: Evaluate which downstream tasks to enable based on split semantics

### Core Implementation

```javascript
// 1. Evaluate downstream tasks after completion
Workflow.prototype.evaluateDownstream = function(completedTaskId, context = {}) {
  const taskDef = this._tasks.get(completedTaskId);
  if (!taskDef) return [];

  const outFlows = this._outgoingFlows.get(completedTaskId) ?? [];
  if (outFlows.length === 0) return [];

  const splitType = taskDef.splitType ?? SPLIT_TYPE.SEQUENCE;
  const toEnable = [];

  // Sort flows by priority (higher first)
  const sortedFlows = [...outFlows].sort(
    (a, b) => (b.priority ?? 0) - (a.priority ?? 0)
  );

  switch (splitType) {
    case SPLIT_TYPE.SEQUENCE:
      // Enable the single outgoing task
      if (sortedFlows.length > 0) {
        toEnable.push(sortedFlows[0].to);
      }
      break;

    case SPLIT_TYPE.AND:
      // Enable all outgoing tasks (parallel split)
      for (const flow of sortedFlows) {
        toEnable.push(flow.to);
      }
      break;

    case SPLIT_TYPE.XOR:
      // Enable first matching condition (exclusive choice)
      for (const flow of sortedFlows) {
        if (!flow.condition) {
          if (flow.isDefault) continue; // Save default for fallback
          toEnable.push(flow.to);
          break;
        }
        try {
          if (flow.condition(context)) {
            toEnable.push(flow.to);
            break;
          }
        } catch {
          // Skip flow on condition error
        }
      }
      // Use default flow if no match
      if (toEnable.length === 0) {
        const defaultFlow = sortedFlows.find(f => f.isDefault);
        if (defaultFlow) {
          toEnable.push(defaultFlow.to);
        } else if (sortedFlows.length > 0) {
          toEnable.push(sortedFlows[sortedFlows.length - 1].to);
        }
      }
      break;

    case SPLIT_TYPE.OR:
      // Enable all matching conditions (multi-choice)
      for (const flow of sortedFlows) {
        if (!flow.condition) {
          toEnable.push(flow.to);
        } else {
          try {
            if (flow.condition(context)) {
              toEnable.push(flow.to);
            }
          } catch {
            // Skip flow on condition error
          }
        }
      }
      // Must enable at least one
      if (toEnable.length === 0 && sortedFlows.length > 0) {
        toEnable.push(sortedFlows[0].to);
      }
      break;
  }

  return toEnable;
};

// 2. Check if task can be enabled based on join semantics
Workflow.prototype.canEnable = function(taskId, completedTasks, activatedTasks = new Set()) {
  const taskDef = this._tasks.get(taskId);
  if (!taskDef) return false;

  const inFlows = this._incomingFlows.get(taskId) ?? [];
  if (inFlows.length === 0) return true; // Start task

  const joinType = taskDef.joinType ?? JOIN_TYPE.SEQUENCE;
  const incomingTaskIds = inFlows.map(f => f.from);

  switch (joinType) {
    case JOIN_TYPE.SEQUENCE:
      // Single incoming must be complete
      return incomingTaskIds.some(id => completedTasks.has(id));

    case JOIN_TYPE.AND:
      // All incoming must be complete (synchronization)
      return incomingTaskIds.every(id => completedTasks.has(id));

    case JOIN_TYPE.XOR:
      // Any one incoming complete (simple merge)
      return incomingTaskIds.some(id => completedTasks.has(id));

    case JOIN_TYPE.OR:
      // All ACTIVATED incoming must be complete (structured sync merge)
      const activated = incomingTaskIds.filter(id => activatedTasks.has(id));
      if (activated.length === 0) {
        return incomingTaskIds.some(id => completedTasks.has(id));
      }
      return activated.every(id => completedTasks.has(id));

    default:
      return incomingTaskIds.some(id => completedTasks.has(id));
  }
};
```

### Key Characteristics

1. **Split semantics** - Sequence, AND, XOR, OR control flow
2. **Priority ordering** - Flows evaluated in priority order
3. **Condition evaluation** - Safe execution with try-catch
4. **Default flows** - Fallback for XOR when no condition matches
5. **Join semantics** - Sequence, AND-join, XOR-merge, OR-merge

### Reusability for WP12-15

```javascript
// For MI patterns, evaluate when to release barrier:
export function canReleaseBarrier(tracker, parentTaskId, barrierConfig) {
  const instances = tracker.getInstances(parentTaskId);
  const completed = instances.filter(i => i.status === InstanceStatus.COMPLETED);
  const waiting = instances.filter(i => i.status === InstanceStatus.BARRIER_WAITING);

  switch (barrierConfig.type) {
    case 'static':
      // WP13: Wait for ALL instances to complete
      return completed.length + waiting.length === instances.length;

    case 'threshold':
      // WP14: Wait for threshold number to complete
      return completed.length >= barrierConfig.threshold;

    case 'dynamic':
      // WP15: Evaluate dynamic condition
      try {
        return barrierConfig.condition({
          completed: completed.length,
          total: instances.length,
          waiting: waiting.length,
          instances,
        });
      } catch {
        return false;
      }

    default:
      return false;
  }
}

// Determine which instances to enable after barrier release
export function selectInstancesToRelease(tracker, parentTaskId, releaseStrategy) {
  const waiting = tracker.getInstances(parentTaskId)
    .filter(i => i.status === InstanceStatus.BARRIER_WAITING);

  switch (releaseStrategy) {
    case 'all':
      return waiting;

    case 'first-n':
      return waiting.slice(0, releaseStrategy.n);

    case 'predicate':
      return waiting.filter(releaseStrategy.predicate);

    default:
      return waiting;
  }
}
```

---

## 7. Error Handling with Circuit Breaker

### Pattern Overview

**Location**: `engine-execution.mjs`, `engine-hooks.mjs`
**Purpose**: Fail-fast error recovery with circuit breaker pattern

### Core Implementation

```javascript
// 1. Check circuit breaker before task execution
export async function enableTask(engine, caseId, taskId, actor) {
  const yawlCase = engine.cases.get(caseId);
  if (!yawlCase) {
    throw new Error(`Case ${caseId} not found`);
  }

  // Check circuit breaker
  const breakerKey = `${yawlCase.workflowId}:${taskId}`;
  if (isCircuitOpen(engine, breakerKey)) {
    throw new Error(`Circuit breaker open for task ${taskId}`);
  }

  // ... rest of enable logic
}

// 2. Reset circuit breaker on success
export async function completeTask(engine, caseId, workItemId, output = {}, actor) {
  // ... task completion logic

  // Reset circuit breaker on success
  const breakerKey = `${yawlCase.workflowId}:${taskDefId}`;
  resetCircuitBreaker(engine, breakerKey);

  // ... rest of completion logic
}

// 3. Increment failure count on error
export async function timeoutTask(engine, caseId, workItemId) {
  // ... timeout handling logic

  // Increment circuit breaker failure count
  const breakerKey = `${yawlCase.workflowId}:${taskDefId}`;
  recordCircuitFailure(engine, breakerKey);

  // ... rest of timeout logic
}

// 4. Circuit breaker state management (engine-hooks.mjs)
export function isCircuitOpen(engine, breakerKey) {
  const state = engine._circuitBreakers.get(breakerKey);
  if (!state) return false;

  // Circuit is open if failure count exceeds threshold
  if (state.failures >= (state.threshold || 5)) {
    // Check if cooldown period has passed
    const cooldown = state.cooldown || 60000; // 1 minute default
    if (now() - state.lastFailure < BigInt(cooldown * 1_000_000)) {
      return true; // Still in cooldown
    }
    // Cooldown passed, reset
    state.failures = 0;
    return false;
  }

  return false;
}

export function recordCircuitFailure(engine, breakerKey) {
  if (!engine._circuitBreakers.has(breakerKey)) {
    engine._circuitBreakers.set(breakerKey, {
      failures: 0,
      threshold: 5,
      cooldown: 60000,
      lastFailure: now(),
    });
  }

  const state = engine._circuitBreakers.get(breakerKey);
  state.failures++;
  state.lastFailure = now();

  if (state.failures >= state.threshold) {
    engine.emit(ENGINE_EVENTS.CIRCUIT_BREAKER_OPEN, {
      breakerKey,
      failures: state.failures,
    });
  }
}

export function resetCircuitBreaker(engine, breakerKey) {
  const state = engine._circuitBreakers.get(breakerKey);
  if (state) {
    state.failures = 0;
    engine.emit(ENGINE_EVENTS.CIRCUIT_BREAKER_CLOSE, {
      breakerKey,
    });
  }
}
```

### Key Characteristics

1. **Fail-fast** - Prevent cascading failures by rejecting early
2. **Automatic recovery** - Cooldown period allows system to recover
3. **Configurable thresholds** - Failure count and cooldown duration
4. **Event emission** - Notify when circuit opens/closes
5. **Per-task granularity** - Circuit breaker per workflow:task combination

### Reusability for WP12-15

```javascript
// For MI patterns, apply circuit breaker per instance:
export async function spawnInstanceWithCircuitBreaker(taskDef, caseId, index, options) {
  const breakerKey = `mi:${taskDef.id}:instance:${index}`;

  // Check if too many instances have failed recently
  if (isCircuitOpen(options.engine, breakerKey)) {
    throw new Error(`Circuit breaker open for instance ${index} of task ${taskDef.id}`);
  }

  try {
    const instance = await createInstance(taskDef, caseId, index, options);

    // Reset on successful spawn
    resetCircuitBreaker(options.engine, breakerKey);

    return instance;
  } catch (error) {
    // Record failure
    recordCircuitFailure(options.engine, breakerKey);
    throw error;
  }
}

// Aggregate circuit breaker for MI spawn operations
export function checkAggregateCircuitBreaker(engine, taskDefId, failureRate = 0.5) {
  const breakerKey = `mi:aggregate:${taskDefId}`;
  const state = engine._circuitBreakers.get(breakerKey);

  if (!state) return false;

  // Open circuit if failure rate exceeds threshold
  const totalAttempts = state.successes + state.failures;
  if (totalAttempts >= 10 && state.failures / totalAttempts > failureRate) {
    return true;
  }

  return false;
}
```

---

## 8. Prototype Extension Pattern

### Pattern Overview

**Location**: Used throughout (`task-execution.mjs`, `task-validation.mjs`, `workflow-execution.mjs`)
**Purpose**: Extend class prototypes with methods that access private properties

### Core Implementation

```javascript
// 1. Define class with private properties (closure)
export class TaskInstance {
  constructor(taskDef, caseId, options = {}) {
    // Private properties prefixed with _
    this._lastReceiptHash = null;
    this._stateHash = null;

    // Public properties
    this.id = options.id ?? `${caseId}-${taskDef.id}-${Date.now()}`;
    this.status = TaskStatus.DISABLED;
    // ...
  }
}

// 2. Extend prototype with methods (separate file)
export function extendTaskInstance(TaskInstanceClass) {
  /**
   * Enable this task (disabled -> enabled)
   */
  TaskInstanceClass.prototype.enable = async function(options = {}) {
    return enableTask(this, options);
  };

  /**
   * Start this task (enabled -> active)
   */
  TaskInstanceClass.prototype.start = async function(resourceId, options = {}) {
    return startTask(this, resourceId, options);
  };

  /**
   * Complete this task (active -> completed)
   */
  TaskInstanceClass.prototype.complete = async function(outputData = {}, options = {}) {
    return completeTask(this, outputData, options);
  };

  // ... more methods
}

// 3. Apply extension in index.mjs
import { TaskInstance } from './task-core.mjs';
import { extendTaskInstance as extendExecution } from './task-execution.mjs';
import { extendTaskInstance as extendValidation } from './task-validation.mjs';

extendExecution(TaskInstance);
extendValidation(TaskInstance);

export { TaskInstance };

// 4. Alternative: Direct prototype assignment (workflow-execution.mjs)
Workflow.prototype.validate = function() {
  const errors = [];
  const warnings = [];
  this._validateBasicStructure(errors, warnings);
  // Can access private _tasks, _flows, etc.
  return { valid: errors.length === 0, errors, warnings };
};

Workflow.prototype.evaluateDownstream = function(completedTaskId, context = {}) {
  const taskDef = this._tasks.get(completedTaskId);
  // Can access private _outgoingFlows
  const outFlows = this._outgoingFlows.get(completedTaskId) ?? [];
  // ...
};
```

### Key Characteristics

1. **Separation of concerns** - Core structure vs execution vs validation in separate files
2. **Access to private properties** - Methods can access underscore-prefixed properties
3. **Incremental extension** - Add methods from multiple modules
4. **Type safety** - JSDoc annotations for IDE support
5. **Module organization** - Keep files under 500 lines by splitting concerns

### Reusability for WP12-15

```javascript
// For MI patterns, extend with MI-specific methods:
export function extendWithMultiInstance(TaskDefinitionClass) {
  /**
   * Spawn multiple instances (WP12-15)
   */
  TaskDefinitionClass.prototype.spawnInstances = async function(caseId, count, options = {}) {
    const pattern = this.multiInstancePattern; // 'no-sync', 'design-time', 'runtime', 'dynamic'

    switch (pattern) {
      case 'no-sync':
        return spawnInstancesNoSync(this, caseId, count, options);
      case 'design-time':
        return spawnInstancesDesignTime(this, caseId, count, options);
      case 'runtime':
        return spawnInstancesRuntime(this, caseId, count, options);
      case 'dynamic':
        return spawnInstancesDynamic(this, caseId, count, options);
      default:
        throw new Error(`Unknown MI pattern: ${pattern}`);
    }
  };

  /**
   * Check if all instances have reached barrier
   */
  TaskDefinitionClass.prototype.checkBarrier = function(tracker) {
    const instances = tracker.getInstances(this.id);
    const barrier = this.barrierConfig;

    return canReleaseBarrier(tracker, this.id, barrier);
  };

  /**
   * Release barrier and enable next iteration
   */
  TaskDefinitionClass.prototype.releaseBarrier = async function(tracker, releaseStrategy) {
    const toRelease = selectInstancesToRelease(tracker, this.id, releaseStrategy);

    for (const instance of toRelease) {
      await instance.enable();
    }

    return { releasedCount: toRelease.length, instances: toRelease };
  };
}
```

---

## 9. Reusable Abstractions for MI Patterns

### Pattern Overview

**Purpose**: Common infrastructure for WP12-15 implementation based on WP1-11 patterns

### Instance Tracker

```javascript
/**
 * Multi-instance tracker (extracted from instance-tracker.mjs)
 *
 * Tracks parent-child relationships, instance status, and barrier state
 */
export class MultiInstanceTracker extends EventEmitter {
  constructor() {
    super();
    this._parentToInstances = new Map(); // parentId -> Set<instanceId>
    this._instanceToParent = new Map();  // instanceId -> parentId
    this._instanceState = new Map();     // instanceId -> { status, data, ... }
    this._barrierState = new Map();      // parentId -> { waiting: Set, released: Set }
  }

  /**
   * Register instance spawn
   */
  addInstance(parentId, instanceId, state = {}) {
    if (!this._parentToInstances.has(parentId)) {
      this._parentToInstances.set(parentId, new Set());
    }

    this._parentToInstances.get(parentId).add(instanceId);
    this._instanceToParent.set(instanceId, parentId);
    this._instanceState.set(instanceId, {
      status: InstanceStatus.SPAWNED,
      createdAt: now(),
      ...state,
    });

    this.emit(MI_EVENTS.INSTANCE_SPAWNED, {
      parentId,
      instanceId,
      timestamp: now(),
    });
  }

  /**
   * Update instance status
   */
  updateStatus(instanceId, status, metadata = {}) {
    const state = this._instanceState.get(instanceId);
    if (!state) {
      throw new Error(`Instance ${instanceId} not found in tracker`);
    }

    const previousStatus = state.status;
    state.status = status;
    state.lastUpdated = now();
    Object.assign(state, metadata);

    this.emit(MI_EVENTS.INSTANCE_STATUS_CHANGED, {
      instanceId,
      previousStatus,
      newStatus: status,
      timestamp: state.lastUpdated,
    });

    // Check barrier conditions
    const parentId = this._instanceToParent.get(instanceId);
    if (status === InstanceStatus.COMPLETED || status === InstanceStatus.BARRIER_WAITING) {
      this._checkBarrierConditions(parentId);
    }
  }

  /**
   * Get all instances for parent
   */
  getInstances(parentId) {
    const instanceIds = this._parentToInstances.get(parentId) ?? new Set();
    return Array.from(instanceIds).map(id => ({
      id,
      ...this._instanceState.get(id),
    }));
  }

  /**
   * Get instance count by status
   */
  getCountByStatus(parentId, status) {
    const instances = this.getInstances(parentId);
    return instances.filter(i => i.status === status).length;
  }

  /**
   * Check if barrier conditions are met
   * @private
   */
  _checkBarrierConditions(parentId) {
    const instances = this.getInstances(parentId);
    const waiting = instances.filter(i => i.status === InstanceStatus.BARRIER_WAITING);
    const completed = instances.filter(i => i.status === InstanceStatus.COMPLETED);

    // Emit barrier reached event when all instances are at barrier
    if (waiting.length + completed.length === instances.length && waiting.length > 0) {
      this.emit(MI_EVENTS.BARRIER_REACHED, {
        parentId,
        waitingCount: waiting.length,
        completedCount: completed.length,
        totalCount: instances.length,
        timestamp: now(),
      });
    }
  }

  /**
   * Release barrier for instances
   */
  releaseBarrier(parentId, instanceIds = null) {
    if (!this._barrierState.has(parentId)) {
      this._barrierState.set(parentId, { waiting: new Set(), released: new Set() });
    }

    const state = this._barrierState.get(parentId);
    const toRelease = instanceIds
      ? instanceIds
      : Array.from(this._parentToInstances.get(parentId) ?? []);

    for (const instanceId of toRelease) {
      state.waiting.delete(instanceId);
      state.released.add(instanceId);

      // Update instance status to enabled
      this.updateStatus(instanceId, InstanceStatus.ENABLED, {
        releasedAt: now(),
      });
    }

    this.emit(MI_EVENTS.BARRIER_RELEASED, {
      parentId,
      releasedCount: toRelease.length,
      instanceIds: toRelease,
      timestamp: now(),
    });

    return { releasedCount: toRelease.length, instanceIds: toRelease };
  }
}

// Global singleton tracker
export const globalInstanceTracker = new MultiInstanceTracker();
```

### Barrier Abstraction

```javascript
/**
 * Synchronization barrier for MI patterns WP13/14/15
 */
export class SyncBarrier {
  constructor(parentTaskId, config = {}) {
    this.parentTaskId = parentTaskId;
    this.type = config.type; // 'static', 'threshold', 'dynamic'
    this.threshold = config.threshold;
    this.condition = config.condition; // For dynamic barriers
    this.waiting = new Set();
    this.released = new Set();
    this.createdAt = now();
  }

  /**
   * Register instance arrival at barrier
   */
  async arrive(instanceId, tracker) {
    this.waiting.add(instanceId);

    // Update instance status
    tracker.updateStatus(instanceId, InstanceStatus.BARRIER_WAITING, {
      arrivedAt: now(),
    });

    // Check if barrier can be released
    if (this.canRelease(tracker)) {
      await this.release(tracker);
    }
  }

  /**
   * Check if barrier can be released
   */
  canRelease(tracker) {
    const instances = tracker.getInstances(this.parentTaskId);
    const completed = instances.filter(i =>
      i.status === InstanceStatus.COMPLETED ||
      i.status === InstanceStatus.BARRIER_WAITING
    );

    switch (this.type) {
      case 'static':
        // WP13: Wait for ALL instances
        return completed.length === instances.length;

      case 'threshold':
        // WP14: Wait for threshold
        return completed.length >= this.threshold;

      case 'dynamic':
        // WP15: Evaluate condition
        try {
          return this.condition({
            waiting: this.waiting.size,
            completed: completed.length,
            total: instances.length,
            instances,
          });
        } catch {
          return false;
        }

      default:
        return false;
    }
  }

  /**
   * Release barrier
   */
  async release(tracker) {
    const toRelease = Array.from(this.waiting);

    for (const instanceId of toRelease) {
      this.waiting.delete(instanceId);
      this.released.add(instanceId);
    }

    tracker.releaseBarrier(this.parentTaskId, toRelease);

    return {
      releasedCount: toRelease.length,
      instanceIds: toRelease,
      releasedAt: now(),
    };
  }

  /**
   * Get barrier statistics
   */
  getStats() {
    return {
      type: this.type,
      waiting: this.waiting.size,
      released: this.released.size,
      createdAt: this.createdAt,
      duration: now() - this.createdAt,
    };
  }
}
```

### Receipt Batching for MI

```javascript
/**
 * Batch receipt generation for MI instances
 * Based on receipt-batch.mjs pattern
 */
export async function generateInstanceBatchReceipts(instances, action) {
  const receipts = [];

  // Generate individual receipts in parallel
  const individualReceipts = await Promise.all(
    instances.map(async (instance, index) => {
      const beforeHash = await computeStateHash(instance);

      // Simulate state change
      instance.status = getNextStatus(action);

      const afterHash = await computeStateHash(instance);

      return {
        id: `receipt-${instance.id}-${action}-${Date.now()}-${index}`,
        instanceId: instance.id,
        action,
        timestamp: now(),
        beforeHash,
        afterHash,
        index,
      };
    })
  );

  // Compute Merkle tree for efficient verification
  const hashes = individualReceipts.map(r => r.beforeHash);
  const merkleRoot = await computeMerkleRoot(hashes);

  // Create aggregate receipt
  const aggregateReceipt = {
    id: `aggregate-${instances[0]?.taskDefId}-${action}-${Date.now()}`,
    action,
    timestamp: now(),
    instanceCount: instances.length,
    instanceReceipts: individualReceipts,
    merkleRoot,
  };

  const aggregateHash = await blake3(JSON.stringify({
    id: aggregateReceipt.id,
    action: aggregateReceipt.action,
    timestamp: aggregateReceipt.timestamp.toString(),
    merkleRoot: aggregateReceipt.merkleRoot,
    instanceCount: aggregateReceipt.instanceCount,
  }));

  aggregateReceipt.hash = aggregateHash;

  return {
    individualReceipts,
    aggregateReceipt,
  };
}

/**
 * Compute Merkle root from hashes
 */
async function computeMerkleRoot(hashes) {
  if (hashes.length === 0) return '';
  if (hashes.length === 1) return hashes[0];

  const tree = [...hashes];

  while (tree.length > 1) {
    const nextLevel = [];
    for (let i = 0; i < tree.length; i += 2) {
      if (i + 1 < tree.length) {
        const combined = await blake3(tree[i] + tree[i + 1]);
        nextLevel.push(combined);
      } else {
        nextLevel.push(tree[i]);
      }
    }
    tree.length = 0;
    tree.push(...nextLevel);
  }

  return tree[0];
}
```

---

## Usage Examples for WP12-15

### Example 1: WP12 - No Synchronization

```javascript
import { TaskDefinition } from '@unrdf/yawl/task-core';
import { globalInstanceTracker } from '@unrdf/yawl/multiple-instance/instance-tracker';
import { generateInstanceBatchReceipts } from '@unrdf/yawl/multiple-instance/batch-receipts';

// Define MI task without synchronization
const notifyCustomersTask = new TaskDefinition({
  id: 'notify-customers',
  name: 'Notify All Customers',
  kind: 'MultipleInstanceTask',
  multiInstancePattern: 'no-sync',
});

// Spawn 100 instances (one per customer)
const customers = await getCustomers(); // Array of 100 customers

const instances = await Promise.all(
  customers.map(async (customer, i) => {
    const instance = new TaskInstance(notifyCustomersTask, caseId, {
      id: `notify-${customer.id}`,
      inputData: { customer },
    });

    globalInstanceTracker.addInstance(notifyCustomersTask.id, instance.id, {
      index: i,
      customerId: customer.id,
    });

    return instance;
  })
);

// Generate batch receipts for spawn
const { aggregateReceipt } = await generateInstanceBatchReceipts(instances, 'spawn');

// Enable and start instances independently (no synchronization)
for (const instance of instances) {
  await instance.enable();
  await instance.start();
  // Each completes independently
}
```

### Example 2: WP13 - Design-Time Synchronization

```javascript
import { SyncBarrier } from '@unrdf/yawl/multiple-instance/sync-barrier';

// Define MI task with static barrier
const processPaymentsTask = new TaskDefinition({
  id: 'process-payments',
  name: 'Process Payments',
  kind: 'MultipleInstanceTask',
  multiInstancePattern: 'design-time',
  barrierConfig: {
    type: 'static', // Wait for ALL instances
  },
});

// Spawn N instances (known at design time)
const paymentCount = 50;
const barrier = new SyncBarrier(processPaymentsTask.id, {
  type: 'static',
});

const instances = await spawnInstances(processPaymentsTask, caseId, paymentCount);

// Execute instances
for (const instance of instances) {
  await instance.enable();
  await instance.start();

  // Simulate async payment processing
  setTimeout(async () => {
    await instance.complete({ status: 'paid' });

    // Arrive at barrier
    await barrier.arrive(instance.id, globalInstanceTracker);
    // Barrier automatically releases when ALL instances arrive
  }, Math.random() * 5000);
}

// Listen for barrier release
globalInstanceTracker.on(MI_EVENTS.BARRIER_RELEASED, (event) => {
  console.log(`All ${event.releasedCount} payments processed!`);
  // Continue to next workflow task
});
```

### Example 3: WP14 - Runtime A-priori (Threshold)

```javascript
// Define MI task with threshold barrier
const approveDocumentsTask = new TaskDefinition({
  id: 'approve-documents',
  name: 'Approve Documents',
  kind: 'MultipleInstanceTask',
  multiInstancePattern: 'runtime',
  barrierConfig: {
    type: 'threshold',
    threshold: 3, // Need 3 approvals out of N
  },
});

// Spawn instances at runtime (count determined by case data)
const approvers = await getApprovers(caseId); // Returns 5 approvers
const barrier = new SyncBarrier(approveDocumentsTask.id, {
  type: 'threshold',
  threshold: 3,
});

const instances = await spawnInstances(approveDocumentsTask, caseId, approvers.length, {
  instanceInputs: approvers.map(a => ({ approver: a })),
});

// Execute instances
for (const instance of instances) {
  await instance.enable();
  await instance.start();

  // Simulate async approval
  setTimeout(async () => {
    await instance.complete({ approved: true });
    await barrier.arrive(instance.id, globalInstanceTracker);
    // Barrier releases when 3 instances complete
  }, Math.random() * 10000);
}

// Listen for threshold met
globalInstanceTracker.on(MI_EVENTS.BARRIER_RELEASED, (event) => {
  console.log(`Threshold met! ${event.releasedCount} approvals received.`);
  // Can proceed even though 2 approvers haven't responded yet
});
```

### Example 4: WP15 - Dynamic (Expression-Based)

```javascript
import { ExpressionEvaluator } from '@unrdf/yawl/multiple-instance/expression-evaluator';

// Define MI task with dynamic barrier
const auditTask = new TaskDefinition({
  id: 'audit-transactions',
  name: 'Audit Transactions',
  kind: 'MultipleInstanceTask',
  multiInstancePattern: 'dynamic',
  barrierConfig: {
    type: 'dynamic',
    condition: (ctx) => {
      // Release when 80% complete OR any instance finds fraud
      const percentComplete = ctx.completed / ctx.total;
      const hasFraud = ctx.instances.some(i => i.data?.fraudDetected);
      return percentComplete >= 0.8 || hasFraud;
    },
  },
});

const transactions = await getTransactions(caseId); // 1000 transactions
const barrier = new SyncBarrier(auditTask.id, {
  type: 'dynamic',
  condition: auditTask.barrierConfig.condition,
});

const instances = await spawnInstances(auditTask, caseId, transactions.length, {
  instanceInputs: transactions.map(t => ({ transaction: t })),
});

// Execute instances
for (const instance of instances) {
  await instance.enable();
  await instance.start();

  setTimeout(async () => {
    const result = await auditTransaction(instance.inputData.transaction);
    await instance.complete({ ...result });

    // Store result in tracker for condition evaluation
    globalInstanceTracker.updateStatus(instance.id, InstanceStatus.COMPLETED, {
      data: result,
    });

    await barrier.arrive(instance.id, globalInstanceTracker);
    // Barrier releases based on dynamic condition
  }, Math.random() * 3000);
}

globalInstanceTracker.on(MI_EVENTS.BARRIER_RELEASED, (event) => {
  const stats = globalInstanceTracker.getStats(auditTask.id);
  console.log(`Audit complete! Checked ${stats.completed}/${stats.total} transactions.`);
  if (stats.fraudDetected > 0) {
    console.log(`ALERT: ${stats.fraudDetected} fraudulent transactions found!`);
  }
});
```

---

## Summary: Pattern Reuse Map

| Pattern | WP1-11 Location | WP12-15 Usage |
|---------|-----------------|---------------|
| **State Machine** | `task-core.mjs` | Instance lifecycle (SPAWNED → ENABLED → ACTIVE → COMPLETED → BARRIER_WAITING) |
| **Receipt Chain** | `task-execution.mjs` | Aggregate receipts with Merkle trees for instance batches |
| **Async Execution** | `engine-execution.mjs` | Parallel instance spawn, concurrent execution with Promise.all |
| **Event Bus** | `engine-events.mjs` | MI-specific events (INSTANCE_SPAWNED, BARRIER_REACHED, etc.) |
| **Validation** | `task-validation.mjs` | Validate spawn count, barrier config, instance inputs |
| **Control Flow** | `workflow-execution.mjs` | Barrier release logic (static/threshold/dynamic conditions) |
| **Circuit Breaker** | `engine-execution.mjs` | Per-instance and aggregate failure tracking |
| **Prototype Extension** | All modules | Extend TaskDefinition with `.spawnInstances()`, `.checkBarrier()` |

---

## Key Takeaways

1. **Copy exactly, don't improve** - Reuse proven patterns from WP1-11
2. **State machine is foundational** - All MI patterns need instance state tracking
3. **Receipts are critical** - Use batch receipts with Merkle trees for MI
4. **Events enable coordination** - Event bus coordinates barrier synchronization
5. **Validation prevents bugs** - Validate spawn parameters, barrier config upfront
6. **Circuit breaker for reliability** - Fail-fast when instance spawn rate is too high
7. **Async by default** - All MI operations are Promise-based
8. **Tracker is central** - MultiInstanceTracker maintains parent-child relationships

---

**Next Steps for WP12-15 Implementation:**

1. ✅ Read this document thoroughly
2. ✅ Copy `MultiInstanceTracker`, `SyncBarrier` abstractions
3. ✅ Implement WP12 using "no sync" pattern (simplest)
4. ✅ Implement WP13 using `SyncBarrier` with type='static'
5. ✅ Implement WP14 using `SyncBarrier` with type='threshold'
6. ✅ Implement WP15 using `SyncBarrier` with type='dynamic'
7. ✅ Add batch receipt generation for all spawn operations
8. ✅ Extend TaskDefinition with `.spawnInstances()` method
9. ✅ Write tests using proven test patterns from WP1-11
10. ✅ Run OTEL validation (score ≥80/100 required)

**End of Document**
