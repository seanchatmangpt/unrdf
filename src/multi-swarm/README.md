# Multi-Swarm Coordination System

Hierarchical agent coordination for large-scale processing with fault isolation and receipt-based audit trails.

## Architecture

```
Queen Swarm (Meta-orchestrator)
    ├── Worker Swarm 1 (Domain: Compression)
    │   ├── Agent 1
    │   ├── Agent 2
    │   └── Agent N
    ├── Worker Swarm 2 (Domain: Validation)
    │   ├── Agent 1
    │   └── Agent N
    └── Worker Swarm N (Domain: Custom)
        └── Agents...
```

## Features

- **Hierarchical Coordination**: Queen orchestrates multiple worker swarms
- **Inter-Swarm Messaging**: Message queue with priority support
- **Work Distribution**: Round-robin, least-loaded, domain-based strategies
- **Work Stealing**: Automatic load balancing between swarms
- **Nested Receipt Chains**: Queen-level and worker-level audit trails
- **Fault Isolation**: Failures don't cascade across swarms
- **Domain Specialization**: Swarms can handle specific task domains

## Components

### CoordinationHub

Central coordination point for all swarms.

```javascript
import { CoordinationHub } from './multi-swarm/index.mjs';

const hub = new CoordinationHub();

// Register swarms
hub.registerSwarm('compression-swarm', { domain: 'compression', capacity: 10 });
hub.registerSwarm('validation-swarm', { domain: 'validation', capacity: 5 });

// Distribute work
const swarmId = hub.distributeWork({
  type: 'compress',
  domain: 'compression',
  payload: { data: [...] }
});

// Get statistics
const stats = hub.getStats();
```

### WorkerSwarm

Domain-specific agent manager.

```javascript
import { WorkerSwarm } from './multi-swarm/index.mjs';

const swarm = new WorkerSwarm('compression-swarm', {
  domain: 'compression',
  capacity: 10
});

// Add agents
swarm.addAgent('agent-1', async (work) => {
  return compressData(work);
});

// Connect to hub
swarm.connectToHub(hub);

// Start swarm
await swarm.start();

// Submit work
const result = await swarm.submitWork({
  type: 'compress',
  payload: { data: [...] }
});

// Verify receipts
const verification = await swarm.verifyReceipts();
```

### QueenSwarm

Meta-orchestrator for worker swarms.

```javascript
import { QueenSwarm, WorkerSwarm } from './multi-swarm/index.mjs';

const queen = new QueenSwarm();

// Create worker swarms
const compressionSwarm = new WorkerSwarm('compression-swarm', {
  domain: 'compression',
  capacity: 10
});

compressionSwarm.addAgent('agent-1', compressProcessor);
compressionSwarm.addAgent('agent-2', compressProcessor);

const validationSwarm = new WorkerSwarm('validation-swarm', {
  domain: 'validation',
  capacity: 5
});

validationSwarm.addAgent('agent-1', validateProcessor);

// Add to queen
queen.addWorkerSwarm(compressionSwarm);
queen.addWorkerSwarm(validationSwarm);

// Start system
await queen.start();

// Submit job (automatically partitioned across swarms)
const result = await queen.submitJob({
  type: 'process-batch',
  payload: [...data],
  partitionStrategy: 'domain',
  aggregationStrategy: 'concat'
});

// Verify all receipt chains
const verification = await queen.verifyAllReceipts();
```

## Quick Start

```javascript
import { createMultiSwarmSystem } from './multi-swarm/index.mjs';

const system = await createMultiSwarmSystem({
  swarms: [
    {
      id: 'compression-swarm',
      domain: 'compression',
      capacity: 10,
      agents: [
        { id: 'agent-1', processor: compressData },
        { id: 'agent-2', processor: compressData }
      ]
    },
    {
      id: 'validation-swarm',
      domain: 'validation',
      capacity: 5,
      agents: [
        { id: 'agent-1', processor: validateData }
      ]
    }
  ]
});

await system.start();

const result = await system.submitJob({
  type: 'process',
  payload: [...data],
  partitionStrategy: 'domain'
});

await system.stop();
```

## Partition Strategies

### Domain-Based
Partitions work by swarm domain. One partition per unique domain.

```javascript
await queen.submitJob({
  type: 'process',
  payload: [...],
  partitionStrategy: 'domain'
});
```

### Round-Robin
Distributes work evenly across all swarms.

```javascript
await queen.submitJob({
  type: 'process',
  payload: [...],
  partitionStrategy: 'round-robin'
});
```

### Least-Loaded
Sends work to the swarm with lowest utilization.

```javascript
await queen.submitJob({
  type: 'process',
  payload: [...],
  partitionStrategy: 'least-loaded'
});
```

## Aggregation Strategies

### Concat
Concatenates array results from all swarms.

```javascript
await queen.submitJob({
  type: 'process',
  payload: [[1, 2], [3, 4]],
  aggregationStrategy: 'concat'
  // Result: [1, 2, 3, 4]
});
```

### Merge
Merges object results from all swarms.

```javascript
await queen.submitJob({
  type: 'process',
  payload: [{ a: 1 }, { b: 2 }],
  aggregationStrategy: 'merge'
  // Result: { a: 1, b: 2 }
});
```

### Reduce
Custom reduce function (specify in payload).

```javascript
await queen.submitJob({
  type: 'process',
  payload: {
    data: [1, 2, 3, 4],
    reducer: (acc, val) => acc + val,
    initialValue: 0
  },
  aggregationStrategy: 'reduce'
  // Result: 10
});
```

## Work Stealing

Overloaded swarms automatically offload work to idle swarms.

```javascript
// Swarm A is overloaded (80%+ utilization)
// Swarm B is idle
// Swarm B automatically steals work from Swarm A

const stolen = hub.requestWorkSteal('swarm-b');
// Returns work item from overloaded swarm
```

## Receipt Chains

### Worker-Level Receipts
Each worker swarm maintains its own receipt chain.

```javascript
const verification = await workerSwarm.verifyReceipts();
console.log(verification.valid); // true/false
console.log(verification.errors); // Array of errors if invalid
```

### Queen-Level Receipts
Queen maintains a receipt chain that includes hashes of all worker receipts.

```javascript
const verification = await queen.verifyAllReceipts();
console.log(verification.queen.valid); // Queen chain valid
console.log(verification.workers); // Worker chain validations
```

### Nested Verification
Verify entire hierarchy at once.

```javascript
const verification = await queen.verifyAllReceipts();

// Queen chain
if (verification.queen.valid) {
  console.log('Queen receipts valid');
}

// Worker chains
for (const worker of verification.workers) {
  console.log(`${worker.swarmId}: ${worker.verification.valid}`);
}

// Overall validity
if (verification.valid) {
  console.log('All receipt chains valid');
}
```

## Statistics

### Hub Statistics
```javascript
const stats = hub.getStats();
// {
//   totalSwarms: 3,
//   activeSwarms: 2,
//   idleSwarms: 1,
//   overloadedSwarms: 0,
//   queueSize: 5,
//   activeWork: 10,
//   completedWork: 100,
//   failedWork: 2
// }
```

### Worker Swarm Statistics
```javascript
const stats = swarm.getStats();
// {
//   id: 'compression-swarm',
//   domain: 'compression',
//   status: 'working',
//   agents: { total: 10, idle: 3, working: 7 },
//   work: { queued: 5, active: 7, completed: 100 },
//   utilization: '70.00%',
//   receipts: 100,
//   performance: { successRate: '98.00%' }
// }
```

### Queen Statistics
```javascript
const stats = queen.getStats();
// {
//   queen: { swarms: 3, activeJobs: 2, completedJobs: 50 },
//   coordination: { ... },
//   swarms: [ ... ],
//   jobs: { pending: 0, executing: 2, completed: 50 },
//   performance: { averageJobDuration: 1234, successRate: '98.00%' }
// }
```

## Use Cases

### 1. Large-Scale Observable Processing
Partition 1000+ observables across multiple swarms.

```javascript
// See examples/large-scale-partitioning.mjs
const result = await queen.submitJob({
  type: 'transform',
  payload: observables, // Array of 1000+ items
  partitionStrategy: 'domain' // Automatically partitioned
});
```

### 2. Specialized Processing Pipeline
Use different swarms for different stages.

```javascript
// Compression swarm
const compressed = await queen.submitJob({
  type: 'compress',
  domain: 'compression',
  payload: data
});

// Validation swarm
const validated = await queen.submitJob({
  type: 'validate',
  domain: 'validation',
  payload: compressed
});
```

### 3. Fault Isolation
Failures in one swarm don't affect others.

```javascript
// See examples/fault-isolation.mjs
// Unreliable swarm fails 50% of the time
// Reliable swarm continues working
// Queen remains operational
```

## Events

### Queen Events
```javascript
queen.on('job:queued', ({ jobId }) => {});
queen.on('job:completed', ({ jobId, result }) => {});
queen.on('job:failed', ({ jobId, error }) => {});
queen.on('swarm:added', ({ swarmId, domain }) => {});
queen.on('queen:heartbeat', ({ swarms, activeJobs }) => {});
```

### Worker Swarm Events
```javascript
swarm.on('work:queued', ({ workId }) => {});
swarm.on('work:started', ({ workId, agentId }) => {});
swarm.on('work:completed', ({ workId, result }) => {});
swarm.on('work:failed', ({ workId, error }) => {});
swarm.on('work:retry', ({ workId, retry }) => {});
swarm.on('agent:added', ({ agentId }) => {});
```

### Hub Events
```javascript
hub.on('swarm:registered', ({ swarmId, metadata }) => {});
hub.on('work:distributed', ({ workId, swarmId }) => {});
hub.on('work:completed', ({ workId, result }) => {});
hub.on('work:stolen', ({ workId, from, to }) => {});
```

## Examples

All examples are in the `examples/` directory:

- **compression-validation.mjs**: Two-stage pipeline with compression + validation
- **fault-isolation.mjs**: Demonstrates fault isolation with unreliable swarms
- **large-scale-partitioning.mjs**: Process 1000+ observables across multiple swarms

Run examples:
```bash
node src/multi-swarm/examples/compression-validation.mjs
node src/multi-swarm/examples/fault-isolation.mjs
node src/multi-swarm/examples/large-scale-partitioning.mjs
```

## Testing

```bash
# Run all multi-swarm tests
timeout 5s npm test src/multi-swarm/__tests__

# Run specific test suite
timeout 5s npm test src/multi-swarm/__tests__/coordination.test.mjs
timeout 5s npm test src/multi-swarm/__tests__/worker-swarm.test.mjs
timeout 5s npm test src/multi-swarm/__tests__/queen.test.mjs
```

## Performance Considerations

- **Swarm Capacity**: Set appropriate capacity based on workload (10-50 agents per swarm)
- **Heartbeat Interval**: Default 5s, adjust based on coordination needs
- **Work Timeout**: Default 30s, increase for long-running tasks
- **Partition Size**: Larger partitions = less overhead, smaller = better parallelism
- **Message Queue**: Default 1000 messages, increase for high-throughput scenarios

## Receipt Chain Guarantees

- **Integrity**: BLAKE3 hashing ensures tamper detection
- **Linearity**: Before/after hash linkage enforces ordering
- **Completeness**: All operations have receipts
- **Nested Validation**: Queen receipts include worker receipt hashes
- **Temporal Ordering**: Monotonically increasing epochs

## API Reference

See inline JSDoc comments in source files for complete API documentation.

## License

Part of the UNRDF project.
