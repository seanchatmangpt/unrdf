# @unrdf/yawl-durable

Durable execution framework inspired by Temporal.io, built on YAWL's event sourcing and KGC-4D temporal capabilities.

## Core Innovation

**YAWL's cryptographic receipt chain IS the event history for deterministic replay.**

This framework maps Temporal.io patterns to YAWL primitives:

| Temporal.io Concept | YAWL Implementation |
|---------------------|---------------------|
| Event History | Receipt chain (BLAKE3 hashes) |
| Deterministic Replay | Replay receipts to rebuild state |
| Activities | YAWL Tasks + execution hooks |
| Sagas | Cancellation regions + compensating tasks |
| Timeouts | Task timeout + TASK_TIMEOUT event |
| Retries | Pre-condition hook with retry counter |
| Versioning | KGC-4D temporal queries + gitRef |
| Side Effects | Idempotent activity execution |

## Features

- **Deterministic Replay**: Reconstruct exact workflow state from receipt chain
- **Saga Pattern**: Long-running transactions with automatic compensation
- **Activity Retries**: Configurable retry policies with exponential backoff
- **Timeout Enforcement**: Activity-level timeouts
- **Time Travel**: Replay to any point in execution history
- **Workflow Versioning**: Version workflows using KGC-4D temporal queries
- **Exactly-Once Semantics**: Cryptographic proof of execution

## Installation

```bash
pnpm add @unrdf/yawl-durable
```

## Quick Start

### Define a Workflow

```javascript
import { DurableWorkflowEngine } from '@unrdf/yawl-durable';

const engine = new DurableWorkflowEngine();

await engine.defineWorkflow({
  id: 'order-processing',
  name: 'Order Processing Workflow',
  activities: [
    {
      id: 'validateOrder',
      handler: async (input) => {
        // Validate order
        return { valid: true, orderId: input.orderId };
      },
      timeout: 5000,
      retryPolicy: {
        maxAttempts: 3,
        initialInterval: 1000,
        backoffCoefficient: 2,
      },
    },
    {
      id: 'processPayment',
      handler: async (input) => {
        // Process payment
        return { ...input, paymentId: 'PMT-123' };
      },
      timeout: 30000,
    },
  ],
  flow: [
    { from: 'validateOrder', to: 'processPayment' },
  ],
});
```

### Execute a Workflow

```javascript
const execution = await engine.startWorkflow('order-processing', {
  orderId: 'ORD-123',
  amount: 99.99,
});

// Activities execute automatically based on flow
console.log(`Execution started: ${execution.executionId}`);
```

### Deterministic Replay

```javascript
// Get receipt history
const receipts = engine.getReceiptHistory(execution.executionId);

// Replay to reconstruct state
const state = await replayFromReceipts(receipts);

console.log(`Completed tasks: ${state.completedTasks}`);
console.log(`Workflow data: ${JSON.stringify(state.data)}`);
```

## Saga Pattern

Sagas implement distributed transactions with automatic compensation.

### Define a Saga

```javascript
import { createSagaWorkflow, executeSaga } from '@unrdf/yawl-durable/saga';

const sagaConfig = createSagaWorkflow({
  id: 'booking-saga',
  name: 'Travel Booking Saga',
  steps: [
    {
      id: 'bookFlight',
      handler: async (input) => {
        const booking = await flightAPI.book(input.flightId);
        return { ...input, flightBooking: booking };
      },
      compensate: async (output) => {
        // Rollback flight booking
        await flightAPI.cancel(output.flightBooking.id);
      },
    },
    {
      id: 'bookHotel',
      handler: async (input) => {
        const booking = await hotelAPI.book(input.hotelId);
        return { ...input, hotelBooking: booking };
      },
      compensate: async (output) => {
        // Rollback hotel booking
        await hotelAPI.cancel(output.hotelBooking.id);
      },
    },
  ],
});

await engine.defineWorkflow(sagaConfig);
```

### Execute Saga

```javascript
const result = await executeSaga(engine, 'booking-saga', {
  flightId: 'FL-123',
  hotelId: 'HTL-456',
});

if (result.success) {
  console.log('Saga completed successfully!');
  console.log(`Flight: ${result.output.flightBooking.id}`);
  console.log(`Hotel: ${result.output.hotelBooking.id}`);
} else {
  console.log('Saga failed and was compensated');
  console.log(`Compensated: ${result.compensated.join(', ')}`);
}
```

## Advanced Features

### Time Travel Debugging

Replay workflow to any point in execution history:

```javascript
import { replayToTimestamp } from '@unrdf/yawl-durable/replay';

const receipts = engine.getReceiptHistory(executionId);

// Replay to specific timestamp
const state = await replayToTimestamp(receipts, targetTimestamp);

console.log(`State at ${targetTimestamp}:`);
console.log(`  Completed: ${state.completedTasks}`);
console.log(`  Active: ${state.activeTasks}`);
```

### Receipt Chain Verification

Verify cryptographic integrity of execution history:

```javascript
const verification = await engine.verifyReceiptChain(executionId);

if (verification.valid) {
  console.log('Receipt chain is valid!');
  console.log(`Genesis hash: ${verification.genesisHash}`);
  console.log(`Latest hash: ${verification.latestHash}`);
} else {
  console.error(`Chain verification failed: ${verification.error}`);
}
```

### Activity Retries

Configure retry behavior per activity:

```javascript
{
  id: 'flaky-api-call',
  handler: async (input) => {
    // May fail occasionally
    return await externalAPI.call(input);
  },
  timeout: 10000,
  retryPolicy: {
    maxAttempts: 5,
    initialInterval: 1000,  // Start with 1s
    backoffCoefficient: 2,  // Double each time: 1s, 2s, 4s, 8s, 16s
    maximumInterval: 30000, // Cap at 30s
  },
}
```

### Workflow Versioning

Support multiple workflow versions:

```javascript
// Define v1
await engine.defineWorkflow({
  id: 'my-workflow',
  version: '1.0.0',
  activities: [/* v1 activities */],
});

// Define v2 (enhanced)
await engine.defineWorkflow({
  id: 'my-workflow-v2',
  version: '2.0.0',
  activities: [/* v2 activities with improvements */],
});

// Old executions continue on v1, new ones use v2
```

## Architecture

### Deterministic Replay

The core of durable execution is deterministic replay. YAWL's receipt chain provides:

1. **Immutable Event History**: Each state transition generates a BLAKE3 receipt
2. **Cryptographic Chain**: Receipts chain together (previousHash → payloadHash → receiptHash)
3. **State Reconstruction**: Replay receipts to rebuild exact workflow state
4. **Time Travel**: Query state at any point in history

```
Receipt Chain:
┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│ CASE_CREATED│───▶│TASK_ENABLED │───▶│TASK_COMPLETED│
│ hash: ABC...│    │ hash: DEF...│    │ hash: GHI... │
│ prev: null  │    │ prev: ABC...│    │ prev: DEF... │
└─────────────┘    └─────────────┘    └─────────────┘
```

### Saga Compensation

Sagas use YAWL's cancellation regions:

1. Each activity gets its own cancellation region
2. On failure, previous activities are compensated in reverse order
3. Compensation handlers rollback side effects
4. All compensation is tracked in receipts

```
Saga Execution:
┌──────────┐  Success  ┌──────────┐  Success  ┌──────────┐
│ Activity1│──────────▶│ Activity2│──────────▶│ Activity3│
└──────────┘           └──────────┘           └──────────┘
     │                      │                      │
     │                      │                      ✗ Fails!
     │                      │
     ▼                      ▼
┌──────────┐           ┌──────────┐
│Compensate│◀──────────│Compensate│
│Activity1 │           │Activity2 │
└──────────┘           └──────────┘
```

## Examples

See [`src/examples/booking-saga.mjs`](./src/examples/booking-saga.mjs) for a complete example of:

- Distributed transaction saga (flight + hotel + car rental)
- Compensation on failure
- Retry logic
- Deterministic replay

Run the example:

```bash
node src/examples/booking-saga.mjs
```

## Testing

```bash
# Run tests
pnpm test

# Run with coverage
pnpm test --coverage
```

## API Reference

### DurableWorkflowEngine

Main orchestration engine.

#### Methods

- `defineWorkflow(config)` - Define a workflow
- `startWorkflow(workflowId, input, options)` - Start execution
- `executeActivity(executionId, activityId, input)` - Execute activity
- `replay(executionId)` - Replay from receipts
- `getExecutionStatus(executionId)` - Get execution status
- `getReceiptHistory(executionId)` - Get receipt chain
- `verifyReceiptChain(executionId)` - Verify integrity

### Saga Functions

- `createSagaWorkflow(config)` - Create saga workflow
- `executeSaga(engine, workflowId, input)` - Execute saga with compensation
- `compensateSaga(engine, executionId, completedActivities)` - Manual compensation

### Replay Functions

- `replayFromReceipts(receipts)` - Rebuild state from receipts
- `replayToTimestamp(receipts, targetTimestamp)` - Time travel replay
- `verifyReceiptChain(receipts)` - Verify receipt integrity
- `computeStateHash(receipts)` - Compute state hash

## Comparison to Temporal.io

| Feature | Temporal.io | YAWL Durable |
|---------|------------|--------------|
| Event History | MySQL/PostgreSQL | BLAKE3 receipt chain |
| Deterministic Replay | Event replay | Receipt replay |
| Storage | External DB | In-memory + KGC-4D |
| Saga Pattern | Code-based | YAWL cancellation regions |
| Time Travel | Query events | Query receipts |
| Versioning | Worker versioning | KGC-4D temporal queries |
| Proof of Execution | Logs | Cryptographic receipts |

## Contributing

See [CONTRIBUTING.md](../../CONTRIBUTING.md) for development guidelines.

## License

MIT
