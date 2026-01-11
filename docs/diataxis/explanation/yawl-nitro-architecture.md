# YAWL-Nitro Architecture Explanation

**Version**: 1.0.0
**Last Updated**: 2026-01-11

A conceptual deep-dive into the YAWL-Nitro integration architecture, design decisions, and patterns.

---

## Table of Contents

1. [Overview](#overview)
2. [Architectural Principles](#architectural-principles)
3. [System Components](#system-components)
4. [Integration Patterns](#integration-patterns)
5. [Data Flow](#data-flow)
6. [Event System](#event-system)
7. [Concurrency and Parallelism](#concurrency-and-parallelism)
8. [Error Handling Strategy](#error-handling-strategy)
9. [Performance Design](#performance-design)
10. [Security Considerations](#security-considerations)
11. [Design Decisions](#design-decisions)
12. [Comparison with Alternatives](#comparison-with-alternatives)

---

## Overview

The YAWL-Nitro integration bridges two distinct systems with complementary strengths:

- **YAWL**: Enterprise workflow engine providing complex control flow patterns, cryptographic receipts, and time-travel capabilities
- **Nitro**: Modern task scheduling engine with cron support, API integration, and developer experience optimizations
- **Daemon**: Orchestration layer connecting both systems

### Why Integration is Needed

**Problem**: YAWL excels at workflow orchestration but lacks built-in scheduling. Nitro excels at task scheduling but lacks workflow patterns.

**Solution**: The `NitroTaskExecutor` bridge allows YAWL workflows to benefit from Nitro's scheduling capabilities while maintaining YAWL's workflow semantics.

**Value Proposition**:
- Schedule complex workflows (not just simple tasks)
- Enforce timeouts on long-running workflow tasks
- Automatic retry with exponential backoff
- Unified monitoring across workflow and task execution
- Production-grade deployment patterns

---

## Architectural Principles

### 1. Separation of Concerns

**Principle**: Each system focuses on its core responsibility.

```
YAWL:    Workflow orchestration, control flow, state management
Daemon:  Operation scheduling, concurrency control, health monitoring
Nitro:   Task registration, cron scheduling, API integration
```

**Benefit**: Systems can evolve independently without breaking integration.

### 2. Loose Coupling

**Principle**: Systems communicate through well-defined interfaces.

```javascript
// YAWL doesn't know about Nitro
const yawlReceipt = await createCase(store, { workflowId });

// Nitro doesn't know about YAWL
const nitroResult = await executor.runTask(taskId, payload);

// Bridge connects both
executor.registerOperationAsTask(operationId, 'workflow:execute');
```

**Benefit**: Can replace Nitro with another scheduler without changing YAWL code.

### 3. Event-Driven Communication

**Principle**: Systems communicate via events, not direct calls.

```javascript
// Daemon emits event
daemon.emit('operation:success', { operationId, duration });

// Executor relays to Nitro event system
executor.on('operation:success', (event) => {
  executor.emit('task:succeeded', {
    nitroTaskId: daemonToNitroMap.get(event.operationId),
    duration: event.duration,
  });
});
```

**Benefit**: Decoupled components, easy to add observers (logging, metrics, alerts).

### 4. Idempotency

**Principle**: Operations can be safely retried.

```javascript
// Task handlers should be idempotent
daemon.schedule({
  id: opId,
  name: 'upsert-record',
  handler: async ({ recordId, data }) => {
    // Upsert is idempotent - safe to retry
    return await database.upsert(recordId, data);
  },
});
```

**Benefit**: Automatic retry without side effects.

### 5. Fail-Fast Validation

**Principle**: Validate early, fail fast.

```javascript
// Validate before execution (not during)
const validation = executor.validateTask(taskId);
if (!validation.valid) {
  throw new Error(`Pre-flight check failed: ${validation.reason}`);
}

// Now safe to execute
await executor.runTask(taskId, payload);
```

**Benefit**: Errors detected before expensive operations.

---

## System Components

### Component Diagram

```
┌─────────────────────────────────────────────────────────────────────────┐
│                          APPLICATION LAYER                               │
│  (User code: workflow definitions, task handlers, API routes)           │
└───────────────────────┬────────────────────────┬────────────────────────┘
                        │                        │
                        ▼                        ▼
        ┌───────────────────────────┐  ┌──────────────────────────┐
        │   NitroTaskExecutor       │  │   YAWL Workflow Engine   │
        │   (Bridge/Adapter)        │◄─┤   - Case management     │
        │   - Task registration     │  │   - Task execution       │
        │   - Payload transform     │  │   - Event sourcing       │
        │   - Event relay           │  │   - Cryptographic        │
        │   - Metrics aggregation   │  │     receipts             │
        └──────────┬────────────────┘  └──────────────────────────┘
                   │
                   ▼
        ┌───────────────────────────┐
        │      Daemon Scheduler      │
        │   - Cron scheduling        │
        │   - Concurrency control    │
        │   - Health monitoring      │
        │   - Metrics collection     │
        └───────────┬────────────────┘
                    │
                    ▼
        ┌───────────────────────────┐
        │   Oxigraph RDF Store       │
        │   - Workflow state         │
        │   - SPARQL queries         │
        │   - Time-travel support    │
        └────────────────────────────┘
```

### NitroTaskExecutor (Bridge)

**Responsibilities:**
1. Register Daemon operations as Nitro tasks
2. Map between Daemon operation IDs and Nitro task IDs
3. Transform payloads between systems
4. Relay events between Daemon and Nitro
5. Aggregate metrics from both systems
6. Provide unified API for task execution

**Key Design:**
- Bidirectional mapping: `daemonToNitroMap` and `nitroToDaemonMap`
- Event relay with transformation
- Metrics aggregation from multiple sources
- Task metadata storage for scheduling info

### Daemon

**Responsibilities:**
1. Schedule operations with cron expressions
2. Control concurrency (max parallel operations)
3. Track operation health and metrics
4. Emit lifecycle events
5. Manage operation queue

**Key Design:**
- Operation registry (Map of operation IDs to handlers)
- Concurrency semaphore (limit parallel execution)
- Event emitter for lifecycle hooks
- LRU cache for completed operations (max 1000)

### YAWL Engine

**Responsibilities:**
1. Workflow state management
2. Control flow pattern execution
3. Event sourcing with cryptographic receipts
4. Time-travel and replay
5. SPARQL query interface

**Key Design:**
- RDF-native state (all workflow state as triples)
- Immutable event log (append-only)
- BLAKE3 hash chains for receipts
- SPARQL queries for state inspection

---

## Integration Patterns

### Pattern 1: Operation-Task Registration

**Concept**: Daemon operations registered as Nitro tasks.

```javascript
// 1. Define operation in Daemon
daemon.schedule({
  id: operationId,
  name: 'my-operation',
  handler: async (context) => { /* ... */ },
});

// 2. Register as Nitro task
executor.registerOperationAsTask(operationId, 'my-task', {
  description: 'My task',
  cronExpression: '0 * * * *',
});

// 3. Execute via Nitro
const nitroTaskId = executor.daemonToNitroMap.get(operationId);
await executor.runTask(nitroTaskId, payload);
```

**Rationale**: Separates operation logic (Daemon) from scheduling metadata (Nitro).

### Pattern 2: Bidirectional Mapping

**Concept**: Maintain two-way mapping between IDs.

```javascript
// Forward map: Daemon → Nitro
daemonToNitroMap.set(operationId, nitroTaskId);

// Reverse map: Nitro → Daemon
nitroToDaemonMap.set(nitroTaskId, operationId);

// Usage
const nitroTaskId = daemonToNitroMap.get(operationId);
const operationId = nitroToDaemonMap.get(nitroTaskId);
```

**Rationale**: Enables efficient lookups from either direction without iteration.

### Pattern 3: Event Relay

**Concept**: Translate Daemon events to Nitro events.

```javascript
// Daemon event
daemon.on('operation:success', (event) => {
  const nitroTaskId = daemonToNitroMap.get(event.operationId);

  // Relay as Nitro event
  executor.emit('task:succeeded', {
    daemonOperationId: event.operationId,
    nitroTaskId,
    duration: event.duration,
    result: event.result,
  });
});
```

**Rationale**: Unified event interface for external observers.

### Pattern 4: Payload Transformation

**Concept**: Transform data between system formats.

```javascript
function transformNitroToYawl(nitroPayload) {
  return {
    caseId: nitroPayload.task_id,
    userId: nitroPayload.user_id,
    data: nitroPayload.parameters,
    timestamp: new Date(nitroPayload.scheduled_at),
  };
}

function transformYawlToNitro(yawlResult) {
  return {
    success: yawlResult.status === 'completed',
    result: yawlResult.outputData,
    completed_at: yawlResult.completedAt?.toISOString(),
  };
}
```

**Rationale**: Each system uses its native format; transformations isolated.

### Pattern 5: Timeout Enforcement

**Concept**: Enforce execution time limits.

```javascript
async function executeWithTimeout(handler, timeoutMs) {
  return await Promise.race([
    handler(),
    new Promise((_, reject) =>
      setTimeout(() => reject(new Error('Timeout')), timeoutMs)
    ),
  ]);
}

// In executor
const result = await executeWithTimeout(
  () => daemon.execute(operationId, context),
  executor.config.timeout
);
```

**Rationale**: Prevents runaway operations from blocking system.

---

## Data Flow

### Sequential Data Flow

```
1. User/Scheduler triggers task
     ↓
2. Executor validates task (pre-flight check)
     ↓
3. Executor looks up Daemon operation ID
     ↓
4. Executor transforms Nitro payload → Daemon context
     ↓
5. Executor calls daemon.execute(operationId, context)
     ↓
6. Daemon executes operation handler
     ↓
7. Daemon emits 'operation:success' event
     ↓
8. Executor relays as 'task:succeeded' event
     ↓
9. Executor transforms result: Daemon → Nitro
     ↓
10. Executor returns TaskExecutionResult
```

### Parallel Data Flow (Metrics)

```
                    ┌───────────────────┐
                    │  Task Execution   │
                    └─────────┬─────────┘
                              │
                 ┌────────────┼────────────┐
                 ▼            ▼            ▼
        ┌────────────┐  ┌──────────┐  ┌──────────┐
        │  Executor  │  │  Daemon  │  │   YAWL   │
        │  Metrics   │  │  Metrics │  │  Metrics │
        └────────────┘  └──────────┘  └──────────┘
                 │            │            │
                 └────────────┼────────────┘
                              ▼
                    ┌───────────────────┐
                    │ Aggregated Metrics│
                    └───────────────────┘
```

### Event Flow (Success Case)

```
Operation Handler
     │
     ├─ operation:started (Daemon event)
     │      │
     │      └─→ task:started (Executor relays)
     │
     ├─ [Execute handler logic]
     │
     └─ operation:success (Daemon event)
            │
            └─→ task:succeeded (Executor relays)
                    │
                    ├─→ Metrics updated
                    ├─→ Execution history stored
                    └─→ External observers notified
```

### Event Flow (Failure with Retry)

```
Operation Handler
     │
     ├─ operation:started
     │
     ├─ [Execute handler logic]
     │
     ├─ operation:failure (Attempt 1)
     │      │
     │      └─→ task:failed (Executor relays)
     │              │
     │              └─→ Schedule retry (backoff: 1s)
     │
     ├─ [Wait 1 second]
     │
     ├─ operation:started (Attempt 2)
     │
     ├─ [Execute handler logic]
     │
     ├─ operation:failure (Attempt 2)
     │      │
     │      └─→ task:failed
     │              │
     │              └─→ Schedule retry (backoff: 2s)
     │
     ├─ [Wait 2 seconds]
     │
     ├─ operation:started (Attempt 3)
     │
     ├─ [Execute handler logic]
     │
     └─ operation:success
            │
            └─→ task:succeeded
```

---

## Event System

### Event Architecture

**Design**: Event emitter pattern with type-safe events.

```javascript
class NitroTaskExecutor extends EventEmitter {
  emit(event, payload) {
    // Validate payload against event schema
    const validated = EventPayloadSchema[event].parse(payload);
    super.emit(event, validated);
  }

  on(event, handler) {
    super.on(event, handler);
    return () => this.off(event, handler); // Cleanup function
  }
}
```

### Event Ordering Guarantees

**Guarantee**: Events for a single task are ordered.

```
Task A: started → succeeded
Task B: started → failed → retry-exhausted
```

**No Guarantee**: Events across different tasks.

```
Task A: started
Task B: started
Task A: succeeded  ← May interleave
Task B: failed
```

**Rationale**: Tasks execute concurrently; strict ordering would require serialization.

### Event Propagation

**Synchronous Events**: Executor internal events

```javascript
// Synchronous - all listeners execute immediately
executor.emit('task:registered', metadata);
// ← All listeners complete before next line
console.log('Registration complete');
```

**Asynchronous Events**: Daemon operation events

```javascript
// Asynchronous - listeners execute in background
daemon.emit('operation:success', result);
// → Returns immediately, listeners run async
```

**Rationale**: Daemon events may trigger expensive operations (logging, alerts); don't block execution.

---

## Concurrency and Parallelism

### Daemon Concurrency Model

**Semaphore Pattern**: Limit concurrent operations.

```javascript
class Daemon {
  constructor({ concurrency = 10 }) {
    this.concurrency = concurrency;
    this.activeCount = 0;
    this.queue = [];
  }

  async execute(operationId, context) {
    // Wait for semaphore
    while (this.activeCount >= this.concurrency) {
      await new Promise((resolve) => this.queue.push(resolve));
    }

    this.activeCount++;

    try {
      const result = await this.executeOperation(operationId, context);
      return result;
    } finally {
      this.activeCount--;
      const next = this.queue.shift();
      if (next) next(); // Release next waiter
    }
  }
}
```

**Benefit**: Prevents resource exhaustion from unlimited parallelism.

### Executor Concurrency Model

**No Limit**: Executor doesn't impose concurrency limit.

```javascript
// All execute concurrently (limited only by Daemon)
await Promise.all([
  executor.runTask('task1'),
  executor.runTask('task2'),
  executor.runTask('task3'),
]);
```

**Rationale**: Executor is stateless; concurrency control at Daemon layer.

### YAWL Concurrency Model

**Case-Level Isolation**: Each workflow case is independent.

```javascript
// Concurrent cases don't interfere
await Promise.all([
  createCase(store, { workflowId, caseId: 'case-1' }),
  createCase(store, { workflowId, caseId: 'case-2' }),
]);
```

**Task-Level Parallelism**: AND-split tasks execute concurrently.

```javascript
// Workflow with parallel tasks
const workflow = {
  tasks: [
    { id: 'task-a', ... },
    { id: 'task-b', ... },
    { id: 'task-c', ... },
  ],
  flow: [
    // AND-split: task-a enables task-b AND task-c
    { from: 'task-a', to: 'task-b', splitType: 'AND' },
    { from: 'task-a', to: 'task-c', splitType: 'AND' },
  ],
};

// task-b and task-c execute in parallel
```

---

## Error Handling Strategy

### Error Classification

**Categories**:
1. **Transient Errors**: Network timeouts, rate limits → Retry
2. **Permanent Errors**: Invalid input, auth failure → Don't retry
3. **Unknown Errors**: Unexpected exceptions → Retry with caution

**Implementation**:

```javascript
class RetryableError extends Error {
  retryable = true;
}

class NonRetryableError extends Error {
  retryable = false;
}

// In handler
if (error.code === 'ECONNREFUSED') {
  throw new RetryableError('Network unavailable');
} else if (error.code === 'INVALID_INPUT') {
  throw new NonRetryableError('Bad request');
}
```

### Retry Strategy

**Exponential Backoff with Jitter**:

```javascript
function calculateBackoff(attempt, config) {
  const {
    backoffMs = 1000,
    backoffMultiplier = 2,
    maxBackoffMs = 30000,
    jitterFactor = 0.1,
  } = config;

  // Exponential: 1s, 2s, 4s, 8s, 16s, ...
  const exponential = backoffMs * Math.pow(backoffMultiplier, attempt - 1);

  // Cap at max
  const capped = Math.min(exponential, maxBackoffMs);

  // Add jitter (±10%)
  const jitter = capped * jitterFactor * (Math.random() * 2 - 1);

  return Math.max(0, capped + jitter);
}

// Example: Attempt 3
calculateBackoff(3, defaultConfig);
// → 4000ms ± 400ms = 3600ms-4400ms
```

**Rationale**: Jitter prevents "thundering herd" when multiple tasks retry simultaneously.

### Circuit Breaker

**Concept**: Stop retrying after repeated failures.

```javascript
class CircuitBreaker {
  constructor({ threshold = 5, timeout = 60000 }) {
    this.threshold = threshold;
    this.timeout = timeout;
    this.failures = 0;
    this.state = 'CLOSED'; // CLOSED, OPEN, HALF_OPEN
  }

  async execute(fn) {
    if (this.state === 'OPEN') {
      throw new Error('Circuit breaker OPEN');
    }

    try {
      const result = await fn();
      this.onSuccess();
      return result;
    } catch (error) {
      this.onFailure();
      throw error;
    }
  }

  onSuccess() {
    this.failures = 0;
    this.state = 'CLOSED';
  }

  onFailure() {
    this.failures++;
    if (this.failures >= this.threshold) {
      this.state = 'OPEN';
      setTimeout(() => {
        this.state = 'HALF_OPEN';
        this.failures = 0;
      }, this.timeout);
    }
  }
}
```

**Rationale**: Prevents cascading failures when downstream service is down.

---

## Performance Design

### Memory Optimization

**LRU Cache for Execution History**:

```javascript
class LRUCache {
  constructor(maxSize = 1000) {
    this.maxSize = maxSize;
    this.cache = new Map();
  }

  set(key, value) {
    // Remove oldest entry if at capacity
    if (this.cache.size >= this.maxSize) {
      const oldest = this.cache.keys().next().value;
      this.cache.delete(oldest);
    }
    this.cache.set(key, value);
  }
}
```

**Rationale**: Unbounded history would grow indefinitely; LRU caps memory.

### Lazy Metric Calculation

**Concept**: Calculate aggregates on-demand, not per-event.

```javascript
// ❌ Bad: Calculate on every event
executor.on('task:succeeded', () => {
  executor.metrics.averageDuration = calculateAverage(); // Expensive
});

// ✅ Good: Calculate when requested
getMetrics() {
  return {
    averageDuration: this.calculateAverageDuration(), // Lazy
  };
}
```

**Rationale**: Metrics queried infrequently; no need to compute continuously.

### Event Batching

**Concept**: Batch multiple events into single notification.

```javascript
class BatchedEventEmitter {
  constructor(batchIntervalMs = 100) {
    this.batch = [];
    this.interval = setInterval(() => this.flush(), batchIntervalMs);
  }

  emit(event, payload) {
    this.batch.push({ event, payload });
  }

  flush() {
    if (this.batch.length === 0) return;
    this.emitBatch(this.batch);
    this.batch = [];
  }
}
```

**Rationale**: Reduces overhead when many tasks complete simultaneously.

---

## Security Considerations

### Input Validation

**Principle**: Validate all external inputs.

```javascript
import { z } from 'zod';

const PayloadSchema = z.object({
  documentId: z.string().uuid(),
  userId: z.string().email(),
  action: z.enum(['submit', 'review', 'approve']),
});

daemon.schedule({
  handler: async (context) => {
    // Validate before processing
    const validated = PayloadSchema.parse(context);
    // Now safe to use validated data
  },
});
```

**Rationale**: Prevents injection attacks and unexpected behavior.

### Timeout Protection

**Principle**: All operations have time limits.

```javascript
// Default timeout prevents DoS
const executor = createNitroTaskExecutor(daemon, {
  timeout: 30000, // 30s max
});
```

**Rationale**: Prevents malicious/buggy handlers from blocking system.

### Error Information Leakage

**Principle**: Don't expose internal details in errors.

```javascript
// ❌ Bad: Exposes internal paths
throw new Error(`Failed to connect to ${dbConnection.internalHost}`);

// ✅ Good: Generic message
throw new Error('Database connection failed');
```

**Rationale**: Prevents information disclosure to attackers.

---

## Design Decisions

### Why Bidirectional Mapping?

**Alternative**: Single map with lookups via iteration.

```javascript
// Alternative: Only daemonToNitroMap
const operationId = Array.from(daemonToNitroMap.entries())
  .find(([_, nitroId]) => nitroId === taskId)?.[0];
```

**Decision**: Use two maps for O(1) lookups in both directions.

**Rationale**: Lookups are frequent; iteration is O(n) and inefficient for large task counts.

### Why Event Relay Instead of Direct Integration?

**Alternative**: Nitro listens directly to Daemon events.

**Decision**: Executor relays events with transformation.

**Rationale**:
1. Decouples Nitro from Daemon (Nitro doesn't need Daemon knowledge)
2. Allows transformation (Daemon operation IDs → Nitro task IDs)
3. Enables filtering (relay only relevant events)

### Why Separate Executor from Daemon?

**Alternative**: Add Nitro integration directly to Daemon.

**Decision**: Separate NitroTaskExecutor class.

**Rationale**:
1. **Separation of concerns**: Daemon is scheduler, Executor is adapter
2. **Modularity**: Can use Daemon without Nitro
3. **Testability**: Test Daemon and Executor independently

### Why Timeout at Executor Level?

**Alternative**: Timeout at Daemon level or task handler level.

**Decision**: Configurable timeout at Executor level.

**Rationale**:
1. Executor manages task execution lifecycle
2. Consistent timeout enforcement across all tasks
3. Can override per-task in handler if needed

---

## Comparison with Alternatives

### vs. Bull/BullMQ

**Bull**:
- Redis-based queue
- Job scheduling with cron
- Retry and backoff
- No workflow patterns

**YAWL-Nitro**:
- RDF-based state
- Workflow orchestration (20 patterns)
- Cryptographic receipts
- Time-travel

**When to use Bull**: Simple job queues, Redis infrastructure
**When to use YAWL-Nitro**: Complex workflows, audit requirements

### vs. Temporal

**Temporal**:
- Durable execution (survives restarts)
- Saga pattern
- Language-agnostic
- Requires Temporal server

**YAWL-Nitro**:
- Lightweight (no external server)
- RDF-native
- JavaScript/Node.js only
- KGC integration

**When to use Temporal**: Multi-language workflows, durable execution critical
**When to use YAWL-Nitro**: RDF/SPARQL integration, cryptographic audit trails

### vs. Apache Airflow

**Airflow**:
- DAG-based workflows
- Python-centric
- Strong UI
- Scheduler + executor separate

**YAWL-Nitro**:
- YAWL pattern-based
- JavaScript/Node.js
- Programmatic (no UI)
- Integrated scheduler

**When to use Airflow**: Data pipelines, Python ecosystem
**When to use YAWL-Nitro**: JavaScript workflows, RDF integration

---

## Summary

### Key Architectural Insights

1. **Bridge Pattern**: NitroTaskExecutor bridges Daemon and Nitro without coupling them
2. **Event-Driven**: Loose coupling via events enables independent evolution
3. **Bidirectional Mapping**: O(1) lookups in both directions for efficiency
4. **Layered Concerns**: Executor (scheduling) + YAWL (orchestration) + RDF (state)
5. **Idempotent Handlers**: Enable safe retries without side effects

### Design Philosophy

**Simplicity**: Prefer simple solutions over clever ones
**Composability**: Small, focused components that compose well
**Observability**: Events and metrics for monitoring
**Resilience**: Timeouts, retries, circuit breakers
**Performance**: Lazy evaluation, LRU caching, event batching

---

## See Also

- **[Tutorial: YAWL-Nitro Integration](/docs/diataxis/tutorials/yawl-nitro-integration.md)**
- **[How-To: YAWL-Nitro Tasks](/docs/diataxis/how-to/yawl-nitro-tasks.md)**
- **[API Reference: YAWL-Nitro](/docs/diataxis/reference/yawl-nitro-api.md)**
- **[Example: YAWL-Nitro Workflow](/examples/yawl-nitro-workflow.mjs)**

**Version**: 1.0.0
**Last Updated**: 2026-01-11
