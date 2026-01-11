# Daemon ΔGate Integration Guide

> **ΔGate (Delta Gate)** - UNRDF v6 unified receipt coordination and delta validation system
>
> Integrates background daemon scheduling with v6-core delta operations for deterministic, auditable state management.

## Overview

The `DaemonDeltaGate` class provides a unified receipt coordination system that bridges the daemon's background task execution with UNRDF v6's delta-driven architecture. It ensures that all state mutations flow through a single, validated, receipt-producing mechanism.

### Key Features

- **Delta Contract Validation**: Enforces admissibility policies before operations execute
- **Deterministic Receipt Generation**: Creates immutable proof chains with SHA256 hashing
- **Differential State Tracking**: Captures old→new state transitions for full auditability
- **Atomic Operations**: All-or-none execution guarantees via operation bundling
- **Rollback Support**: Reverses deltas via inverse operation generation
- **Cross-Package Coordination**: Integrates YAWL, streaming, hooks, and other packages
- **Health Tracking**: Monitors operational health via delta acceptance/rejection rates

## Architecture

### ΔGate Flow

```
┌─────────────────────────────────────────────────────────────┐
│ 1. DAEMON proposes Delta                                    │
│    (background task trigger)                                │
└───────────────────┬─────────────────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────────────────────────┐
│ 2. VALIDATE Contract                                        │
│    - Schema validation (Zod)                                │
│    - Capture old state (snapshot)                           │
│    - Check admissibility policies                           │
└───────────────────┬─────────────────────────────────────────┘
                    │
         ┌──────────┴──────────┐
         │                     │
         ▼                     ▼
    LAWFUL                UNLAWFUL
         │                     │
         ▼                     ▼
┌────────────────────┐ ┌─────────────────────┐
│ 3a. APPLY Δ       │ │ 3b. REJECT & Return │
│ - Execute ops     │ │     Denial Receipt  │
│ - Atomic commit   │ │                     │
│ - Capture new     │ └─────────────────────┘
│   state           │
│                   │
│ 4a. RECEIPT GEN   │
│ - Proof chain     │
│ - SHA256 hash     │
│ - Audit trail     │
└────────────────────┘
         │
         ▼
┌──────────────────────────────────────────────────────────┐
│ 5. EMIT Events for coordination                         │
│    - delta:applied → YAWL, streaming, hooks             │
│    - Health status → monitoring                          │
│    - Cross-package callbacks                            │
└──────────────────────────────────────────────────────────┘
```

## Delta Contract Schema

### Delta Structure

```javascript
{
  // Identity
  "id": "123e4567-e89b-12d3-a456-426614174000",  // UUID v4

  // Timestamps
  "timestamp_ns": 1704067200000000000n,          // Nanoseconds (BigInt)
  "timestamp_iso": "2024-01-01T00:00:00.000Z",  // ISO 8601

  // Operations (atomic bundle)
  "operations": [
    {
      "op": "set",                    // add | set | delete | insert
      "path": "daemon.status",        // State path (dot notation)
      "oldValue": "idle",             // Previous value (for audit)
      "newValue": "running",          // New value
      "timestamp_ns": 1704067200000000000n
    },
    // ... more operations
  ],

  // Source metadata
  "source": {
    "package": "@unrdf/yawl",         // Origin package
    "actor": "workflow-task-1",       // Optional: Actor/user ID
    "nodeId": "daemon-node-1",        // Optional: Node identifier
    "context": { /* ... */ }          // Optional: Custom context
  },

  // Admissibility policies (optional)
  "admissibility": {
    "policyId": "policy-1",           // Optional: Policy identifier
    "constraints": ["no-concurrent"], // Optional: Constraint IDs
    "preConditions": ["always"]       // Optional: Pre-conditions to check
  },

  // Proof chain
  "previousDeltaId": null             // UUID of previous delta (null = genesis)
}
```

### Operation Types

#### 1. **set** - Update or create value
```javascript
{
  "op": "set",
  "path": "config.maxWorkers",
  "oldValue": 5,
  "newValue": 10,
  "timestamp_ns": 1704067200000000000n
}
```

#### 2. **delete** - Remove value
```javascript
{
  "op": "delete",
  "path": "temp.cache",
  "oldValue": { /* old data */ },
  "timestamp_ns": 1704067200000000000n
}
```

#### 3. **insert** - Insert into array-like path
```javascript
{
  "op": "insert",
  "path": "queue",
  "index": 0,
  "value": { "task": "urgent" },
  "timestamp_ns": 1704067200000000000n
}
```

## Receipt Schema

Every delta produces a receipt (success or denial):

```javascript
{
  // Identity
  "id": "receipt-uuid",                    // Receipt UUID v4
  "deltaId": "delta-uuid",                // Linked delta

  // Result
  "applied": true,                         // true | false
  "reason": undefined,                     // Rejection reason (if applied=false)

  // State
  "stateHash": "abc123...",               // SHA256 of new state
  "operationsApplied": 3,                 // Count of successful ops
  "operationsFailed": 0,                  // Count of failed ops

  // Timestamps
  "timestamp_ns": 1704067200000000000n,
  "timestamp_iso": "2024-01-01T00:00:00.000Z",

  // Proof chain
  "previousReceiptHash": "def456...",     // SHA256 of previous receipt
  "receiptHash": "ghi789..."              // SHA256 of this receipt
}
```

## Usage Examples

### Basic Delta Proposal

```javascript
import { DaemonDeltaGate } from '@unrdf/daemon/integrations/v6-deltagate';
import { crypto } from 'node:crypto';

const gate = new DaemonDeltaGate({
  daemonId: 'my-daemon-1',
  logger: console,
});

const delta = {
  id: crypto.randomUUID(),
  timestamp_ns: BigInt(Date.now()) * 1_000_000n,
  timestamp_iso: new Date().toISOString(),
  operations: [
    {
      op: 'set',
      path: 'status',
      newValue: 'running',
      timestamp_ns: BigInt(Date.now()) * 1_000_000n,
    },
  ],
  source: {
    package: '@unrdf/daemon',
    actor: 'background-task',
  },
  previousDeltaId: null,
};

// Propose delta
const receipt = await gate.proposeDelta(delta);

if (receipt.applied) {
  console.log('State updated:', receipt.stateHash);
  console.log('Proof:', receipt.receiptHash);
} else {
  console.error('Delta rejected:', receipt.reason);
}
```

### With Admissibility Policies

```javascript
const delta = {
  id: crypto.randomUUID(),
  timestamp_ns: BigInt(Date.now()) * 1_000_000n,
  timestamp_iso: new Date().toISOString(),
  operations: [
    {
      op: 'set',
      path: 'critical.enabled',
      newValue: false,
      timestamp_ns: BigInt(Date.now()) * 1_000_000n,
    },
  ],
  source: {
    package: '@unrdf/hooks',
    actor: 'policy-engine',
  },
  admissibility: {
    policyId: 'critical-shutdown',
    constraints: ['no-active-tasks'],
    preConditions: ['always'],
  },
  previousDeltaId: null,
};

const receipt = await gate.proposeDelta(delta);
// Receipt will include policy validation results
```

### Rollback via Reversal

```javascript
// Original delta creates state
const originalReceipt = await gate.proposeDelta(delta);

// Later: Rollback by reversing operations
const rollbackReceipt = await gate.rollback(originalReceipt.id);

if (rollbackReceipt.applied) {
  console.log('Rolled back successfully');
  console.log('New state hash:', rollbackReceipt.stateHash);
}
```

### Cross-Package Coordination

```javascript
// Register YAWL as a coordinator
gate.registerCoordinator('@unrdf/yawl', {
  onDelta: async (delta, receipt) => {
    if (receipt.applied) {
      // Trigger YAWL workflow on successful delta
      console.log('Triggering workflow task');
    }
  },
});

// When delta is applied, emit event for coordinators
gate.on('delta:applied', (event) => {
  const coordinator = gate.coordinators.get('@unrdf/yawl');
  if (coordinator) {
    coordinator.onDelta(event.delta, event.receipt);
  }
});
```

### Health Monitoring

```javascript
// Check operational health
const health = gate.getHealthStatus();

console.log('Status:', health.status);           // healthy|degraded|unhealthy
console.log('Processed:', health.deltasProcessed);
console.log('Rejected:', health.deltasRejected);
console.log('Rejection rate:',
  (health.deltasRejected / (health.deltasProcessed + health.deltasRejected) * 100).toFixed(1) + '%'
);
```

### Receipt Chain Validation

```javascript
// Get complete proof chain
const chain = gate.getReceiptChain();

for (const receipt of chain) {
  console.log(`Receipt ${receipt.id}`);
  console.log(`  Valid chain: ${receipt.chainValid}`);
  console.log(`  State hash: ${receipt.stateHash}`);
  console.log(`  Proof: ${receipt.receiptHash}`);
}
```

## Validation Rules

### Delta Validation

1. **Schema**: Must match `DeltaContractSchema` (Zod validated)
2. **Operations**: Minimum 1 operation required
3. **Timestamps**: Must be valid nanosecond precision
4. **Source**: Must include package name
5. **Admissibility**: Optional, but checked if present

### Receipt Validation

1. **Hash Length**: All hashes must be exactly 64 characters (SHA256)
2. **Chain**: Previous receipt hash must match previous receipt's hash
3. **Consistency**: Applied receipts must have valid stateHash

### Policy Validation

1. **Pre-conditions**: All must evaluate to true
2. **Constraints**: All must be satisfied
3. **Failure Mode**: Single violation → delta rejected

## Error Handling

### Rejection Reasons

```javascript
// Pre-condition failed
"Pre-condition failed: never"

// Constraint failed
"Constraint failed: no-concurrent"

// Schema validation
"Validation failed: operations must be an array"

// Application error
"Application failed: Cannot set path with null value"
```

### Recovery Strategies

1. **Validation Errors**: Fix delta contract and resubmit
2. **Policy Violations**: Modify delta to satisfy constraints
3. **Application Errors**: Verify state consistency before resubmitting
4. **Rollback Failures**: Check that receipt was actually applied before rolling back

## Performance Characteristics

### Latency Targets (P95)

| Operation | Target | Notes |
|-----------|--------|-------|
| Delta validation | <1ms | Zod schema check |
| State capture | <1ms | Map iteration |
| Operation apply | <5ms | Per operation |
| Receipt generation | <1ms | Hash computation |
| Rollback | <10ms | Reverse + reapply |
| 100 concurrent | <5s | Parallel processing |

### Memory

- Each receipt: ~500 bytes
- Each delta: ~1KB
- History limit: 1000 deltas/receipts = ~1.5MB

## Integration Points

### With @unrdf/daemon

```javascript
class DaemonTask {
  async execute() {
    // Create delta from task execution
    const delta = {
      id: this.taskId,
      operations: this.getStateChanges(),
      source: { package: '@unrdf/daemon' },
      previousDeltaId: null,
    };

    // Propose through gate
    const receipt = await this.gate.proposeDelta(delta);
    return receipt;
  }
}
```

### With @unrdf/yawl

```javascript
// YAWL task completion triggers delta
yawl.on('task:completed', async (task) => {
  const delta = {
    id: task.id,
    operations: [
      { op: 'set', path: `tasks.${task.id}.status`, newValue: 'completed' },
    ],
    source: { package: '@unrdf/yawl', actor: task.actorId },
    previousDeltaId: task.previousDeltaId,
  };

  await gate.proposeDelta(delta);
});
```

### With @unrdf/hooks

```javascript
// Hook execution records delta
hooks.on('hook:executed', async (hook) => {
  const delta = {
    id: hook.id,
    operations: hook.getStateChanges(),
    source: { package: '@unrdf/hooks', actor: 'policy-engine' },
    admissibility: {
      policyId: hook.policyId,
      constraints: hook.constraints,
    },
    previousDeltaId: hook.previousDeltaId,
  };

  await gate.proposeDelta(delta);
});
```

### With @unrdf/streaming

```javascript
// Stream changes as deltas
stream.on('change', async (change) => {
  const delta = {
    id: change.id,
    operations: [change.toOperation()],
    source: { package: '@unrdf/streaming', context: change.stream },
    previousDeltaId: null,
  };

  await gate.proposeDelta(delta);
});
```

## Best Practices

1. **Always Provide Source**: Include package name and optional actor ID
2. **Chain Deltas**: Link via `previousDeltaId` for proof chain
3. **Capture Old Values**: Include `oldValue` in operations for audit trail
4. **Use Policies**: Define admissibility for critical operations
5. **Monitor Health**: Track rejection rates for operational insight
6. **Implement Rollback**: Store receipt IDs to enable recovery
7. **Batch Operations**: Group related changes in single delta
8. **Validate Receipts**: Check `chainValid` for proof integrity

## Troubleshooting

### Delta Rejected with "Pre-condition failed"
- Check that admissibility.preConditions are satisfied
- Verify operation dependencies are met
- Consider adding more pre-conditions for safety

### Receipt Chain Invalid
- Ensure deltas are proposed sequentially
- Check that previousReceiptHash is accurate
- Verify no concurrent modifications to same path

### High Rejection Rate
- Monitor health status regularly
- Review policy constraints
- Check admission logs for patterns

### Performance Degradation
- Check state size (large values slow capture)
- Reduce operation count per delta
- Enable batching for high-frequency operations

## References

- **v6 Core**: `@unrdf/v6-core`
- **Daemon**: `@unrdf/daemon`
- **YAWL Integration**: `@unrdf/daemon/integrations/yawl`
- **Hooks Integration**: `@unrdf/daemon/integrations/hooks`
- **Streaming Integration**: `@unrdf/daemon/integrations/streaming`
