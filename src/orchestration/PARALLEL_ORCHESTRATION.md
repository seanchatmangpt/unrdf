# Parallel Orchestration System

Advanced parallel agent execution with worker pools, priority queues, and streaming observables.

## Features

### 1. Worker Pool Management
- **Fixed or dynamic sizing**: 2-10+ concurrent workers
- **Load balancing strategies**: Round-robin, least-busy, random
- **Health monitoring**: Auto-recovery from stuck workers
- **Graceful lifecycle**: Idle worker cleanup

### 2. Priority-Based Task Queue
- **5 priority levels**: Critical, High, Normal, Low, Background
- **FIFO within priority**: Fair scheduling
- **Bounded queue size**: Backpressure handling
- **Task retry logic**: Automatic retry with exponential backoff
- **Timeout support**: Configurable per-task timeouts

### 3. Agent Router
- **Capability-based routing**: Match tasks to agent capabilities
- **Role specialization**: 10 agent roles (α₁-α₁₀)
  - `α₁-observer`: Observe and collect
  - `α₂-compressor`: Compress and optimize
  - `α₃-validator`: Validate and verify
  - `α₄-orchestrator`: Orchestrate and coordinate
  - `α₅-analyzer`: Analyze and evaluate
  - `α₆-optimizer`: Optimize and improve
  - `α₇-monitor`: Monitor and track
  - `α₈-aggregator`: Aggregate and merge
  - `α₉-executor`: Execute and process
  - `α₁₀-coordinator`: Coordinate and align
- **Load-aware routing**: Distribute load across agents
- **Max concurrent limits**: Prevent agent overload

### 4. Circuit Breaker
- **Failure detection**: Track consecutive failures
- **State management**: Closed → Open → Half-Open
- **Automatic recovery**: Exponential backoff
- **Per-agent isolation**: Independent circuit breakers

### 5. Streaming Observables
- **Real-time updates**: Incremental progress streaming
- **Delta compression**: O_τ → O_τ+1 deltas
- **Snapshot support**: Periodic full snapshots
- **Backpressure handling**: Prevent memory overflow
- **Event-driven**: Subscribe to updates, completion, errors

### 6. Checkpointing & Resume
- **Automatic checkpointing**: Periodic state snapshots
- **Resume capability**: Recover from failures
- **Audit trail**: Full execution history

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                  ParallelOrchestrator                        │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐      │
│  │  WorkerPool  │  │  TaskQueue   │  │ AgentRouter  │      │
│  │              │  │              │  │              │      │
│  │ Min: 2       │  │ Priority:    │  │ Capability   │      │
│  │ Max: 10      │  │ 5 levels     │  │ Matching     │      │
│  │ Strategy:    │  │ FIFO         │  │ Load         │      │
│  │ Least-busy   │  │ Bounded      │  │ Balancing    │      │
│  └──────────────┘  └──────────────┘  └──────────────┘      │
│                                                              │
│  ┌──────────────────────────────────────────────────────┐   │
│  │              Circuit Breakers                         │   │
│  │  [α₁] [α₂] [α₃] [α₄] [α₅] [α₆] [α₇] [α₈] [α₉] [α₁₀]  │   │
│  └──────────────────────────────────────────────────────┘   │
│                                                              │
│  ┌──────────────────────────────────────────────────────┐   │
│  │           Streaming Observable System                 │   │
│  │  Delta Updates | Snapshots | Backpressure            │   │
│  └──────────────────────────────────────────────────────┘   │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

## Usage

### Basic Example

```javascript
import {
  createParallelOrchestrator,
  AgentRole,
  TaskPriority
} from '@unrdf/orchestration';

// Create orchestrator
const orchestrator = createParallelOrchestrator({
  workerPool: {
    minWorkers: 2,
    maxWorkers: 10,
    assignmentStrategy: 'least-busy'
  },
  taskQueue: {
    maxSize: 1000,
    defaultPriority: TaskPriority.NORMAL
  },
  streaming: {
    enableDeltaCompression: true,
    snapshotInterval: 10
  }
});

await orchestrator.start();

// Register agents
orchestrator.registerAgent({
  agentId: 'observer-1',
  role: AgentRole.OBSERVER,
  capabilities: ['observe', 'compress'],
  maxConcurrent: 3
});

// Submit task
const observable = orchestrator.submit({
  type: 'observe-compress',
  priority: TaskPriority.HIGH,
  capabilities: ['observe', 'compress'],
  payload: { data: [...] }
});

// Subscribe to updates
observable.on('delta', (update) => {
  console.log('Progress:', update.data.progress);
});

observable.on('complete', (result) => {
  console.log('Complete:', result);
});

observable.on('error', ({ error }) => {
  console.error('Failed:', error);
});

// Shutdown
await orchestrator.shutdown({ graceful: true });
```

### Advanced Features

#### Priority Scheduling

```javascript
// Critical system tasks
orchestrator.submit({
  type: 'system-monitor',
  priority: TaskPriority.CRITICAL,
  capabilities: ['monitor']
});

// Normal data processing
orchestrator.submit({
  type: 'data-process',
  priority: TaskPriority.NORMAL,
  capabilities: ['process']
});

// Background optimization
orchestrator.submit({
  type: 'optimize',
  priority: TaskPriority.BACKGROUND,
  capabilities: ['optimize']
});
```

#### Checkpointing

```javascript
// Automatic checkpointing (every 5s)
const orchestrator = createParallelOrchestrator({
  checkpointing: {
    enabled: true,
    interval: 5000
  }
});

// Manual checkpoint
orchestrator.checkpoint(taskId, {
  progress: 50,
  data: 'checkpoint-state'
});

// Resume from checkpoint
const state = orchestrator.resumeFromCheckpoint(taskId);
```

#### Circuit Breaker

```javascript
// Configure per-agent failure handling
const orchestrator = createParallelOrchestrator({
  circuitBreaker: {
    failureThreshold: 5,     // Open after 5 failures
    successThreshold: 2,     // Close after 2 successes
    resetTimeout: 30000      // Try again after 30s
  }
});

// Monitor circuit state
orchestrator.on('agent:circuit-state', ({ agentId, from, to }) => {
  console.log(`Agent ${agentId}: ${from} → ${to}`);
});
```

## Performance Characteristics

### Throughput
- **Worker Pool**: 10+ concurrent agents
- **Task Queue**: 1000+ queued tasks
- **Routing**: <1ms capability matching
- **Streaming**: <100ms delta updates

### Scalability
- **Horizontal**: Add workers dynamically (2→10)
- **Vertical**: Priority-based preemption
- **Backpressure**: Automatic queue throttling

### Reliability
- **Circuit Breakers**: Isolated failure domains
- **Checkpointing**: Resume from failure
- **Retry Logic**: Exponential backoff
- **Health Monitoring**: Auto-recovery

## Implementation Details

### Files

| File | Lines | Purpose |
|------|-------|---------|
| `worker-pool.mjs` | 540 | Worker lifecycle management |
| `task-queue.mjs` | 548 | Priority-based task queue |
| `agent-router.mjs` | 375 | Capability-based routing |
| `circuit-breaker.mjs` | 409 | Failure handling |
| `parallel-orchestrator.mjs` | 709 | Main orchestration engine |
| `parallel-orchestration.test.mjs` | 669 | Comprehensive tests |
| `example-workflows/parallel-agent-execution.mjs` | - | Usage example |

**Total**: ~3,250 lines of production code + tests

### Test Coverage

- **Worker Pool**: 15+ test cases
- **Task Queue**: 18+ test cases
- **Agent Router**: 12+ test cases
- **Circuit Breaker**: 10+ test cases
- **Parallel Orchestrator**: 10+ test cases
- **Integration**: 5+ end-to-end scenarios

### Patterns Used

1. **Factory Pattern**: `createParallelOrchestrator()`
2. **Observer Pattern**: EventEmitter-based observables
3. **Circuit Breaker Pattern**: Failure isolation
4. **Strategy Pattern**: Routing strategies
5. **Object Pool Pattern**: Worker pool
6. **Priority Queue Pattern**: Task scheduling

## Integration

### With Existing Orchestration

The parallel orchestration system extends the existing `WorkflowOrchestrator`:

```javascript
// Sequential workflow (existing)
import { executeWorkflow } from '@unrdf/orchestration';

await executeWorkflow({
  changedPackages: ['@unrdf/core'],
  packages: { ... }
});

// Parallel agent execution (new)
import { createParallelOrchestrator } from '@unrdf/orchestration';

const orchestrator = createParallelOrchestrator({...});
await orchestrator.start();
// ... register agents and submit tasks
```

### Backward Compatibility

All existing orchestration features remain available:
- `WorkflowOrchestrator` - Multi-package workflows
- `StageExecutor` - Stage-based execution
- `DependencyResolver` - Topological sorting
- `RollbackManager` - Automatic rollback

## Future Enhancements

1. **Distributed Execution**: Cross-machine agent pools
2. **Persistent Checkpoints**: Database-backed state
3. **Advanced Routing**: ML-based agent selection
4. **Metrics & Monitoring**: OTEL integration
5. **Rate Limiting**: Per-agent throttling
6. **Dead Letter Queue**: Failed task recovery

## References

- [KGC-4D Methodology](../../docs/bb80-20-methodology.md)
- [Orchestration Index](./index.mjs)
- [Example Usage](./example-workflows/parallel-agent-execution.mjs)
- [Tests](./parallel-orchestration.test.mjs)
