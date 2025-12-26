# YAWL Realtime - Real-time Collaboration Framework

Real-time collaboration framework for YAWL workflows using Socket.io. Enables multiple users to collaboratively execute workflows with optimistic locking, CRDT-inspired state synchronization, and cryptographic receipt verification.

## Features

- **Real-time Event Broadcasting**: All YAWL engine events are broadcast to connected clients
- **Optimistic Locking**: Concurrent task claims with Lamport timestamp-based conflict resolution
- **CRDT-Inspired Merging**: Last-Write-Wins for data, Add-Wins for work items
- **Receipt Verification**: Uses YAWL's cryptographic receipts for conflict detection
- **Automatic State Sync**: Clients stay synchronized with latest workflow state

## Installation

```bash
pnpm add @unrdf/yawl-realtime
```

## Architecture

### Collaboration Protocol

```
┌─────────────┐         ┌──────────────────┐         ┌─────────────┐
│  Client A   │◄───────►│  YAWL Realtime   │◄───────►│  Client B   │
│  (Alice)    │  Socket │     Server       │  Socket │   (Bob)     │
└─────────────┘   .io   └──────────────────┘   .io   └─────────────┘
      │                          │                           │
      │  1. task:claim          │                           │
      ├─────────────────────────►│                           │
      │  (timestamp: 100)        │                           │
      │                          │  2. Optimistic Lock       │
      │                          │     Check                 │
      │                          │  - Compare timestamps     │
      │                          │  - Verify receipt hash    │
      │                          │                           │
      │  3. task:claimed         │                           │
      │◄─────────────────────────┤  4. task:locked (broadcast)
      │  (success: true)         ├──────────────────────────►│
      │                          │                           │
      │  5. task:complete        │                           │
      ├─────────────────────────►│                           │
      │  (output: {...})         │                           │
      │                          │  6. Release Lock          │
      │                          │     Update State          │
      │                          │                           │
      │  7. task:completed       │  8. yawl:event (broadcast)│
      │◄─────────────────────────┼──────────────────────────►│
      │                          │                           │
```

### Optimistic Locking

Uses **Lamport timestamps** for distributed conflict resolution:

1. Each client maintains a logical clock
2. Clock increments on every action
3. On task claim, client sends timestamp
4. Server compares timestamps:
   - **Higher timestamp wins** (newer claim)
   - Equal timestamps: first arrival wins
5. Losing client receives conflict notification

### CRDT-Inspired State Merging

State updates use Conflict-free Replicated Data Type semantics:

- **Last-Write-Wins (LWW)**: Scalar values in case data
- **Add-Wins**: Work items (set union)
- **Receipt Chain**: Tracks causality via BLAKE3 hashes

```javascript
// State merge example
{
  data: {
    key1: "value2",  // LWW: overwrites with newer value
    key2: "value2"   // Add-Wins: adds new key
  },
  workItems: {
    wi1: {...},      // Add-Wins: union of all work items
    wi2: {...}
  }
}
```

## Usage

### Server Setup

```javascript
import { createWorkflowEngine } from '@unrdf/yawl';
import { YAWLRealtimeServer } from '@unrdf/yawl-realtime/server';

const engine = createWorkflowEngine();
const server = new YAWLRealtimeServer(engine, {
  port: 3000,
  corsOptions: {
    origin: '*',
    methods: ['GET', 'POST'],
  },
});

await server.start();
console.log('Server listening on port 3000');
```

### Client Connection

```javascript
import { YAWLRealtimeClient } from '@unrdf/yawl-realtime/client';

const client = new YAWLRealtimeClient({
  serverUrl: 'http://localhost:3000',
  userId: 'alice@example.com',
});

await client.connect();
```

### Claiming Tasks

```javascript
// Listen for available tasks
client.on('task:enabled', async (event) => {
  console.log(`Task available: ${event.taskId}`);

  // Attempt to claim
  const result = await client.claimTask(event.caseId, event.workItemId);

  if (result.success) {
    console.log('Task claimed!');

    if (result.conflict) {
      console.log(`Resolved conflict: ${result.conflict.resolution}`);
    }
  } else {
    console.log(`Claim denied: ${result.conflict.resolution}`);
  }
});
```

### Completing Tasks

```javascript
// Complete a claimed task
const result = await client.completeTask(caseId, workItemId, {
  decision: 'approved',
  comments: 'Looks good!',
  approvedBy: client.userId,
});

if (result.success) {
  console.log('Task completed!');
  console.log(`Downstream tasks enabled: ${result.downstreamEnabled.length}`);
}
```

### State Synchronization

```javascript
// Request current state
const { state, locks } = await client.syncState(caseId);

console.log('Case data:', state.data);
console.log('Work items:', Object.keys(state.workItems));
console.log('Active locks:', locks.map(l => l.lock.userId));
```

### Releasing Tasks

```javascript
// Release without completing
const result = await client.releaseTask(caseId, workItemId);

if (result.success) {
  console.log('Task released, available for others');
}
```

## Events

### Client Events

- `yawl:event` - Any YAWL engine event
- `task:enabled` - Task becomes available
- `task:started` - Task execution begins
- `task:completed` - Task finished
- `task:locked` - Another user claimed a task
- `task:unlocked` - Task lock released
- `error` - Error occurred

### Server-Side Engine Events

All YAWL `ENGINE_EVENTS` are broadcast:

- `case:created`, `case:started`, `case:completed`
- `task:enabled`, `task:started`, `task:completed`, `task:cancelled`
- `resource:allocated`, `resource:released`
- `circuit:open`, `circuit:close`

## Conflict Resolution

### Scenario 1: Concurrent Claims

```javascript
// Alice claims at t=100
await aliceClient.claimTask(caseId, taskId);

// Bob claims at t=105 (concurrent)
await bobClient.claimTask(caseId, taskId);

// Result: Bob wins (higher timestamp)
// Alice receives conflict notification
```

### Scenario 2: Receipt Hash Mismatch

```javascript
const result = await client.claimTask(caseId, taskId, {
  expectedReceiptHash: 'abc123',
});

// If server's receipt hash differs:
// result.success = false
// result.conflict = { type: 'receipt_mismatch', ... }
```

### Scenario 3: State Merge Conflicts

```javascript
// Client A updates: { status: 'pending' }
// Client B updates: { status: 'approved' }

// Merged result:
// { status: 'approved' } // Last-Write-Wins
// + conflict notification with old value
```

## Example: Multi-User Approval Workflow

See [`src/examples/approval-collab.mjs`](src/examples/approval-collab.mjs) for a complete example with 3 users:

```bash
pnpm run dev
```

Output:

```
Alice submits request
  ↓
Bob (L1 Approver) claims and approves
  ↓
Carol (L2 Approver) claims and approves
  ↓
Workflow completes
```

## API Reference

### YAWLRealtimeServer

```typescript
class YAWLRealtimeServer {
  constructor(engine: WorkflowEngine, options: ServerOptions);
  start(): Promise<void>;
  stop(): Promise<void>;
  getStats(): ServerStats;
}
```

### YAWLRealtimeClient

```typescript
class YAWLRealtimeClient {
  constructor(options: ClientOptions);
  connect(): Promise<void>;
  disconnect(): Promise<void>;

  claimTask(caseId: string, workItemId: string, options?: ClaimOptions): Promise<ClaimResult>;
  completeTask(caseId: string, workItemId: string, output: object): Promise<CompleteResult>;
  releaseTask(caseId: string, workItemId: string): Promise<ReleaseResult>;
  syncState(caseId: string): Promise<StateResult>;

  on(eventType: string, handler: Function): UnsubscribeFn;
  getStatus(): ClientStatus;
}
```

### OptimisticLockManager

```typescript
class OptimisticLockManager {
  acquire(workItemId: string, caseId: string, userId: string, timestamp: number): LockResult;
  release(workItemId: string, userId: string): ReleaseResult;
  getLock(workItemId: string): Lock | null;
  getLocksForCase(caseId: string): Lock[];
}
```

### StateSyncManager

```typescript
class StateSyncManager {
  mergeState(caseId: string, update: StateUpdate, receiptHash: string): MergeResult;
  getState(caseId: string): CaseState | null;
  verifyReceiptChain(caseId: string, expectedHash: string): VerificationResult;
}
```

## Testing

```bash
pnpm test                 # Run all tests
pnpm test:watch          # Watch mode
pnpm test realtime       # Run specific test file
```

Tests cover:
- Optimistic locking with concurrent claims
- Lamport timestamp conflict resolution
- CRDT state merging
- Receipt chain verification
- Multi-client integration scenarios

## Performance Characteristics

- **Lock Acquisition**: O(1) with Map-based storage
- **State Merge**: O(n) where n = number of updated keys
- **Event Broadcasting**: O(m) where m = number of connected clients
- **Receipt Verification**: O(1) hash comparison

## Production Considerations

1. **Lock Timeouts**: Implement automatic lock release after inactivity
2. **Persistence**: Add state persistence for server restarts
3. **Scaling**: Use Redis adapter for multi-server Socket.io
4. **Authentication**: Add JWT or session-based auth
5. **Rate Limiting**: Prevent claim spam
6. **Monitoring**: Track lock contention metrics

## License

MIT

## Related Packages

- `@unrdf/yawl` - Core YAWL workflow engine
- `@unrdf/kgc-4d` - 4D knowledge graph with time-travel
- `@unrdf/hooks` - Policy-as-code validation hooks
