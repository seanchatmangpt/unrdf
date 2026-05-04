# @unrdf/yawl-queue

Distributed YAWL workflow execution using BullMQ and Redis.

## Overview

`@unrdf/yawl-queue` bridges UNRDF's YAWL workflow engine with BullMQ's robust distributed queue system, enabling:

- **Distributed Task Execution**: Execute YAWL workflows across multiple worker processes
- **Redis-Backed State**: Reliable distributed coordination and state management
- **Intelligent Retry Policies**: Leverage YAWL cancellation regions for retry logic
- **Cryptographic Receipts**: Maintain YAWL's BLAKE3-based proof chains
- **Priority & Delay**: Fine-grained control over task scheduling
- **Worker Pools**: Scale horizontally with multiple workers

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    YAWLQueueAdapter                         │
│  ┌──────────────┐      ┌──────────────┐                    │
│  │ YAWL Engine  │◄────►│  BullMQ      │                    │
│  │              │      │  Queue       │                    │
│  │ - Workflows  │      │              │                    │
│  │ - Cases      │      │ - Job Queue  │                    │
│  │ - Receipts   │      │ - Priority   │                    │
│  └──────────────┘      └──────┬───────┘                    │
│                               │                             │
└───────────────────────────────┼─────────────────────────────┘
                                │
                        ┌───────▼────────┐
                        │  Redis         │
                        │  - State       │
                        │  - Job Data    │
                        │  - Metadata    │
                        └───────┬────────┘
                                │
                 ┌──────────────┼──────────────┐
                 │              │              │
            ┌────▼────┐    ┌────▼────┐   ┌────▼────┐
            │ Worker 1│    │ Worker 2│   │ Worker 3│
            │         │    │         │   │         │
            │ Process │    │ Process │   │ Process │
            │ YAWL    │    │ YAWL    │   │ YAWL    │
            │ Tasks   │    │ Tasks   │   │ Tasks   │
            └─────────┘    └─────────┘   └─────────┘
```

## Installation

```bash
pnpm add @unrdf/yawl-queue
```

### Prerequisites

- Node.js >= 18.0.0
- Redis >= 6.0.0
- pnpm >= 7.0.0

## Quick Start

### 1. Create a Workflow

```javascript
import { createWorkflow } from '@unrdf/yawl';

const workflow = createWorkflow({
  id: 'data-processing',
  name: 'Data Processing Pipeline',
  tasks: [
    { id: 'fetch', name: 'Fetch Data', priority: 10 },
    { id: 'validate', name: 'Validate', priority: 8 },
    { id: 'transform', name: 'Transform', priority: 5 },
    { id: 'store', name: 'Store Results', priority: 9 },
  ],
  flows: [
    { from: 'fetch', to: 'validate' },
    { from: 'validate', to: 'transform' },
    { from: 'transform', to: 'store' },
  ],
});
```

### 2. Setup the Adapter

```javascript
import { YAWLQueueAdapter } from '@unrdf/yawl-queue';

const adapter = new YAWLQueueAdapter({
  redis: {
    host: 'localhost',
    port: 6379,
  },
  queueName: 'data-processing',
  defaultJobOptions: {
    attempts: 3,
    backoff: {
      type: 'exponential',
      delay: 1000,
    },
  },
});

await adapter.registerWorkflow(workflow);
```

### 3. Create Workers

```javascript
// Define task handlers
const taskHandler = async (job, task) => {
  const taskId = task.taskDefId || task.id;

  switch (taskId) {
    case 'fetch':
      return { data: await fetchData() };
    case 'validate':
      return { valid: validateData(job.data.input) };
    case 'transform':
      return { transformed: transformData(job.data.input) };
    case 'store':
      await storeData(job.data.input);
      return { stored: true };
  }
};

// Create worker pool (3 workers)
for (let i = 0; i < 3; i++) {
  adapter.createWorker({
    concurrency: 2,
    taskHandler,
  });
}
```

### 4. Execute Workflow

```javascript
const { caseId, jobId } = await adapter.executeCase('data-processing', {
  source: 'database',
  batchSize: 100,
});

console.log(`Case ${caseId} started with job ${jobId}`);

// Monitor status
const status = await adapter.getCaseStatus(caseId);
console.log(status);
// {
//   caseId: '...',
//   workflowId: 'data-processing',
//   status: 'running',
//   enabledTasks: 1,
//   activeTasks: 2,
//   completedTasks: 3,
//   receipts: 7
// }
```

## Task-to-Job Mapping

The adapter automatically maps YAWL task lifecycle to BullMQ jobs:

| YAWL Action | BullMQ Job | Priority | Delay |
|-------------|------------|----------|-------|
| Enable Task | `enable-{taskId}` | From task.priority | 0 |
| Start Task | `start-{taskId}` | From task.priority | From task.delay |
| Complete Task | `complete-{taskId}` | From task.priority | 0 |
| Cancel Task | `cancel-{taskId}` | High (99) | 0 |

### Job Data Schema

Each BullMQ job contains:

```javascript
{
  caseId: string,
  workflowId: string,
  taskId: string,         // Task definition ID
  workItemId: string,     // Task instance ID
  action: 'enable' | 'start' | 'complete' | 'cancel',
  input: object,          // Task input data
  output: object,         // Task output data (for complete)
  actor: string,          // Optional actor identifier
  reason: string          // Optional reason (for cancel)
}
```

## Distributed Execution Flow

### Sequential Execution

```javascript
const workflow = createWorkflow({
  id: 'sequential',
  tasks: [
    { id: 'task1', splitType: 'sequence' },
    { id: 'task2', splitType: 'sequence' },
    { id: 'task3', splitType: 'sequence' },
  ],
  flows: [
    { from: 'task1', to: 'task2' },
    { from: 'task2', to: 'task3' },
  ],
});

// Execution: task1 → task2 → task3 (one after another)
```

### Parallel Execution

```javascript
const workflow = createWorkflow({
  id: 'parallel',
  tasks: [
    { id: 'start', splitType: 'and' },        // Split
    { id: 'task-a', joinType: 'and' },
    { id: 'task-b', joinType: 'and' },
    { id: 'join', joinType: 'and' },          // Join
  ],
  flows: [
    { from: 'start', to: 'task-a' },
    { from: 'start', to: 'task-b' },
    { from: 'task-a', to: 'join' },
    { from: 'task-b', to: 'join' },
  ],
});

// Execution: start → [task-a, task-b] (parallel) → join
// Multiple workers can process task-a and task-b concurrently
```

## Retry Policies

The adapter uses YAWL's cancellation regions to implement intelligent retry policies:

```javascript
const workflow = createWorkflow({
  id: 'with-retries',
  tasks: [
    {
      id: 'fetch',
      cancellationRegion: 'fetch-region',  // Retry on failure
      timeout: 5000,
    },
    {
      id: 'process',
      cancellationRegion: 'process-region',
      timeout: 10000,
    },
  ],
  flows: [{ from: 'fetch', to: 'process' }],
});
```

Retry behavior:
- **With cancellation region**: Uses `defaultJobOptions.attempts` for retry count
- **Without cancellation region**: Retries based on BullMQ job options
- **After max retries**: Task is cancelled in YAWL engine

## YAWL Receipts

The adapter maintains YAWL's cryptographic receipt chain:

```javascript
const { caseId } = await adapter.executeCase('workflow-id');

// Receipts are automatically generated for each transition
const receipts = adapter.receipts.get(caseId);

receipts.forEach((receipt, index) => {
  console.log(`Receipt ${index}:`, {
    id: receipt.id,                    // BLAKE3 hash
    timestamp: receipt.timestamp,      // Nanosecond precision
    eventType: receipt.eventType,      // 'task:enabled', 'task:completed', etc.
    previousHash: receipt.previousHash // Chain to previous receipt
  });
});
```

Receipt chain guarantees:
- **Immutability**: Each receipt is cryptographically signed
- **Ordering**: Receipts form a hash chain
- **Verification**: Can verify entire execution history
- **Audit Trail**: Complete provenance of all state transitions

## Complete Example: ETL Pipeline

See [`src/examples/data-pipeline.mjs`](src/examples/data-pipeline.mjs) for a full ETL pipeline with:

- **7 tasks**: ingest, validate, transform (3 parallel), aggregate, store
- **5 workers**: Distributed across multiple processes
- **Priority scheduling**: High-priority tasks processed first
- **Error handling**: Automatic retries with exponential backoff
- **Status monitoring**: Real-time pipeline progress tracking

Run the example:

```bash
# Start Redis
docker run -d -p 6379:6379 redis:7-alpine

# Run pipeline
pnpm example
```

Expected output:

```
================================================================================
YAWL-Queue: Distributed ETL Data Pipeline
================================================================================

Registered workflow: etl-pipeline (7 tasks)

Creating 5 workers...
Created 5 workers

================================================================================
Executing ETL Pipeline...
================================================================================

Case created: case-etl-pipeline-1703...
Initial job queued: case-etl-pipeline-1703-start-...

[Worker 12345] INGEST: Loading raw data...
[Worker 12345] INGEST: Loaded 6 records

[Worker 12346] VALIDATE: Checking data quality...
[Worker 12346] VALIDATE: 6/6 valid

[Worker 12347] TRANSFORM-1: Processing batch 1...
[Worker 12348] TRANSFORM-2: Processing batch 2...
[Worker 12349] TRANSFORM-3: Processing batch 3...

[Worker 12347] TRANSFORM-1: Processed 2 records
[Worker 12348] TRANSFORM-2: Processed 2 records
[Worker 12349] TRANSFORM-3: Processed 2 records

[Worker 12345] AGGREGATE: Combining transformed batches...
[Worker 12345] AGGREGATE: Combined 6 records

[Worker 12346] STORE: Persisting final data...
[Worker 12346] STORE: Saved aggregated data

================================================================================
Pipeline Completed Successfully!
================================================================================
```

## API Reference

### YAWLQueueAdapter

#### Constructor

```javascript
new YAWLQueueAdapter(config)
```

**Config Options:**

```typescript
{
  redis?: {
    host?: string;          // Default: 'localhost'
    port?: number;          // Default: 6379
    password?: string;
    db?: number;
  };
  queueName?: string;       // Default: 'yawl-workflows'
  defaultJobOptions?: {
    attempts?: number;      // Default: 3
    backoff?: {
      type: 'exponential' | 'fixed';
      delay: number;        // Milliseconds
    };
    removeOnComplete?: boolean;
    removeOnFail?: boolean;
  };
  engineConfig?: object;    // YAWL engine config
}
```

#### Methods

##### registerWorkflow(workflow)

Register a YAWL workflow definition.

```javascript
await adapter.registerWorkflow(workflow);
```

##### executeCase(workflowId, initialData, options)

Execute a new workflow case.

```javascript
const { caseId, jobId } = await adapter.executeCase(
  'workflow-id',
  { input: 'data' },
  { caseId: 'custom-id' }
);
```

##### getCaseStatus(caseId)

Get current case status.

```javascript
const status = await adapter.getCaseStatus(caseId);
```

##### createWorker(options)

Create a worker to process jobs.

```javascript
const worker = adapter.createWorker({
  concurrency: 2,
  taskHandler: async (job, task) => {
    // Process task
    return { result: 'data' };
  },
});
```

##### getStats()

Get adapter statistics.

```javascript
const stats = await adapter.getStats();
// {
//   queue: { name, waiting, active, completed, failed },
//   workers: { count, ids },
//   engine: { casesCreated, tasksCompleted, ... },
//   receipts: { totalCases, totalReceipts }
// }
```

##### close()

Close all workers and connections.

```javascript
await adapter.close();
```

## Testing

Run tests:

```bash
# Unit tests
pnpm test

# Watch mode
pnpm test:watch

# Coverage
pnpm test:coverage
```

Test requirements:
- Redis server running on `localhost:6379`
- Database 15 is used for tests (isolated from production)

## Performance Considerations

### Worker Scaling

```javascript
// CPU-bound tasks: workers ≈ CPU cores
for (let i = 0; i < os.cpus().length; i++) {
  adapter.createWorker({ concurrency: 1 });
}

// I/O-bound tasks: higher concurrency per worker
for (let i = 0; i < 4; i++) {
  adapter.createWorker({ concurrency: 10 });
}
```

### Job Options

```javascript
{
  // Fast retry for transient failures
  attempts: 3,
  backoff: {
    type: 'exponential',
    delay: 1000  // 1s, 2s, 4s
  },

  // Cleanup completed jobs
  removeOnComplete: {
    age: 3600,  // Keep 1 hour
    count: 1000 // Keep last 1000
  },

  // Retain failed jobs for debugging
  removeOnFail: false
}
```

### Priority Queue

Higher priority = processed first (1-100 scale):

```javascript
{
  id: 'critical-task',
  priority: 99  // Highest priority
}
```

## Troubleshooting

### Jobs Not Processing

1. Check Redis connection:
   ```bash
   redis-cli ping  # Should return PONG
   ```

2. Verify workers are running:
   ```javascript
   console.log(adapter.workers.size);  // Should be > 0
   ```

3. Check queue status:
   ```javascript
   const stats = await adapter.getStats();
   console.log(stats.queue);
   ```

### Task Failures

Enable verbose logging:

```javascript
adapter.engine.on('TASK_FAILED', (event) => {
  console.error('Task failed:', event);
});

worker.on('failed', (job, err) => {
  console.error('Job failed:', job?.id, err.message);
});
```

### Receipt Verification

```javascript
import { verifyReceipt, verifyChainLink } from '@unrdf/yawl/receipt';

const receipts = adapter.receipts.get(caseId);

for (let i = 1; i < receipts.length; i++) {
  const valid = await verifyChainLink(receipts[i - 1], receipts[i]);
  console.log(`Receipt ${i} chain valid:`, valid);
}
```

## License

MIT

## Contributing

Contributions welcome! Please read CONTRIBUTING.md first.

## Related Packages

- [@unrdf/yawl](../yawl) - Core YAWL workflow engine
- [@unrdf/kgc-4d](../kgc-4d) - 4D knowledge graph with time-travel
- [@unrdf/oxigraph](../oxigraph) - RDF triple store
- [BullMQ](https://docs.bullmq.io/) - Distributed queue system

## Acknowledgments

Built on top of:
- **YAWL** (van der Aalst et al.) - Workflow patterns and semantics
- **BullMQ** - Robust Redis-based queue
- **UNRDF** - Unified RDF framework
