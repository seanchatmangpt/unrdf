# @unrdf/event-automation

Event-driven automation for UNRDF v6.1.0 with automatic delta processing, receipt generation, and policy enforcement.

## Features

- **Auto-Process Deltas**: Automatically process deltas from V6-Core ΔGate
- **Receipt Generation**: Automatic receipt creation for all operations with cryptographic hashing
- **Policy Enforcement**: Hook-based policy enforcement with before/after triggers
- **Event Replay**: Built-in audit trail with event replay capabilities
- **OTEL Instrumentation**: Full OpenTelemetry integration for observability
- **High Performance**: <5ms P95 delta processing, <1ms P95 receipt creation

## Installation

```bash
pnpm add @unrdf/event-automation
```

## Quick Start

```javascript
import { createEventAutomationEngine } from '@unrdf/event-automation';

// Create and start the engine
const engine = createEventAutomationEngine({
  id: 'my-automation',
  maxConcurrent: 10,
  enableReceipts: true,
  enablePolicies: true,
});

// Process a delta
const delta = {
  id: 'delta-1',
  operations: [
    {
      operation: 'insert',
      subject: 'http://example.org/subject',
      predicate: 'http://example.org/predicate',
      object: 'http://example.org/object',
    },
  ],
  timestamp: Date.now(),
};

const result = await engine.processDelta(delta);

console.log('Processing result:', result);
// {
//   success: true,
//   deltaId: 'delta-1',
//   receipts: [...],
//   policyResults: [...],
//   duration: 2.5
// }
```

## Core Components

### Event Automation Engine

Main orchestrator for delta processing with receipts and policies.

```javascript
import { EventAutomationEngine } from '@unrdf/event-automation';

const engine = new EventAutomationEngine({
  id: 'default-automation',
  autoStart: true,
  maxConcurrent: 10,
  enableReceipts: true,
  enablePolicies: true,
  enableReplay: true,
  replayBufferSize: 1000,
});

// Start engine
engine.start();

// Process delta
const result = await engine.processDelta(delta);

// Batch process
const results = await engine.batchProcess(deltas, { parallel: true });

// Replay events
const replayResults = await engine.replay({
  fromTimestamp: Date.now() - 3600000,
  batchSize: 100,
});

// Get statistics
const stats = engine.getStatistics();

// Stop engine
await engine.stop();
```

### Delta Processor

Processes deltas with validation and transformation.

```javascript
import { createDeltaProcessor } from '@unrdf/event-automation';

const processor = createDeltaProcessor({
  validateOperations: true,
});

const result = await processor.processDelta(delta);

// Batch processing
const results = await processor.batchProcess(deltas, {
  parallel: true,
});

// Get metrics
const metrics = processor.getMetrics();
// {
//   totalProcessed: 100,
//   totalSucceeded: 98,
//   totalFailed: 2,
//   averageDuration: 1.2,
//   p95Duration: 2.5,
//   p99Duration: 4.8
// }
```

### Receipt Tracker

Manages receipt generation and verification.

```javascript
import { createReceiptTracker } from '@unrdf/event-automation';

const tracker = createReceiptTracker({
  maxReceipts: 10000,
});

// Create receipt
const receipt = await tracker.createReceipt(delta, {
  operation: 'process',
  entityType: 'Delta',
});

// Verify receipt
const isValid = await tracker.verifyReceipt(receipt);

// Get receipts for delta
const receipts = tracker.getReceiptsForDelta(deltaId);

// Get metrics
const metrics = tracker.getMetrics();
```

### Policy Enforcer

Enforces policies with hook integration.

```javascript
import { createPolicyEnforcer } from '@unrdf/event-automation';

const enforcer = createPolicyEnforcer({
  failOnPolicyViolation: true,
});

// Register policy
const policyId = enforcer.registerPolicy({
  id: 'validate-subject',
  name: 'Validate Subject IRI',
  trigger: 'before:delta',
  priority: 90,
  condition: async (context) => {
    return context.delta.operations.length > 0;
  },
  action: async (context) => {
    for (const op of context.delta.operations) {
      if (!op.subject.startsWith('http://')) {
        throw new Error(`Invalid subject IRI: ${op.subject}`);
      }
    }
  },
});

// Evaluate policies
const results = await enforcer.evaluatePolicies('before:delta', {
  delta,
  engine,
});

// Enable/disable policies
enforcer.disablePolicy(policyId);
enforcer.enablePolicy(policyId);
```

## Policy Triggers

Policies can be registered for the following triggers:

- `before:delta` - Before delta processing
- `after:delta` - After delta processing
- `before:receipt` - Before receipt creation
- `after:receipt` - After receipt creation

## Event Replay

The engine maintains a replay buffer for audit trails and recovery.

```javascript
// Replay all events
const results = await engine.replay();

// Replay with filters
const results = await engine.replay({
  fromTimestamp: Date.now() - 3600000,  // Last hour
  toTimestamp: Date.now(),
  deltaIds: ['delta-1', 'delta-2'],
  batchSize: 100,
  parallel: true,
});
```

## Performance Targets

| Operation | P95 Target | P99 Target |
|-----------|------------|------------|
| Delta Processing | <5ms | <10ms |
| Receipt Creation | <1ms | <2ms |
| Policy Evaluation | <2ms | <5ms |
| Event Replay (1000 events) | <100ms | <200ms |

## Statistics & Monitoring

```javascript
const stats = engine.getStatistics();

console.log(stats);
// {
//   totalProcessed: 1000,
//   totalSucceeded: 998,
//   totalFailed: 2,
//   totalReceipts: 998,
//   averageDuration: 1.8,
//   p95Duration: 3.2,
//   p99Duration: 5.1,
//   uptimeMs: 3600000,
//   policies: {
//     total: 5,
//     evaluations: 5000,
//     passed: 4990,
//     failed: 10
//   },
//   replayBufferSize: 1000,
//   activeProcessing: 2
// }
```

## Error Handling

The engine handles errors gracefully and continues processing:

```javascript
const result = await engine.processDelta(invalidDelta);

if (!result.success) {
  console.error('Processing failed:', result.error);
  // Take corrective action
}
```

## Integration with V6-Core

```javascript
import { createEventAutomationEngine } from '@unrdf/event-automation';
import { DeltaGate } from '@unrdf/v6-core/delta/gate';

const engine = createEventAutomationEngine();
const deltaGate = new DeltaGate();

// Process deltas from ΔGate
deltaGate.on('delta', async (delta) => {
  const result = await engine.processDelta(delta);
  console.log('Delta processed:', result);
});
```

## Testing

```bash
# Run tests
pnpm test

# Run tests with coverage
pnpm test:coverage

# Watch mode
pnpm test:watch
```

## License

MIT

## Contributing

See [CONTRIBUTING.md](../../CONTRIBUTING.md) for contribution guidelines.
