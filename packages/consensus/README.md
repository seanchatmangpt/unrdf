# @unrdf/consensus

Production-grade Raft consensus implementation for distributed workflow coordination.

## Features

- **Raft Consensus Algorithm**: Leader election, log replication, strong consistency
- **WebSocket Transport**: Real-time bidirectional communication with msgpackr serialization
- **Dynamic Cluster Membership**: Add/remove nodes without downtime
- **Distributed State Machine**: Replicated key-value store for workflow state
- **Automatic Failover**: Handles node failures with leader re-election
- **Health Monitoring**: Continuous health checks with automatic recovery
- **OpenTelemetry Integration**: Full observability with metrics and tracing

## Installation

```bash
pnpm add @unrdf/consensus
```

## Quick Start

### Basic 2-Node Cluster

```javascript
import { createRaftCoordinator, createDistributedStateMachine } from '@unrdf/consensus';

// Node 1
const node1 = createRaftCoordinator({
  nodeId: 'node-1',
  port: 8080,
  host: 'localhost'
});
await node1.initialize();

const state1 = createDistributedStateMachine({ nodeId: 'node-1' });
await state1.initialize(node1);

// Node 2
const node2 = createRaftCoordinator({
  nodeId: 'node-2',
  port: 8081,
  host: 'localhost'
});
await node2.initialize();

const state2 = createDistributedStateMachine({ nodeId: 'node-2' });
await state2.initialize(node2);

// Connect peers
node1.addPeer('node-2', 'localhost', 8081);
node2.addPeer('node-1', 'localhost', 8080);

// Use state machine on leader
const leader = node1.isLeader ? state1 : state2;
await leader.set('workflow:123', { status: 'running', progress: 0.5 });
```

### Full Cluster with Membership Management

```javascript
import {
  createRaftCoordinator,
  createClusterManager,
  createDistributedStateMachine
} from '@unrdf/consensus';

// Create coordinator
const coordinator = createRaftCoordinator({
  nodeId: 'node-1',
  port: 8080,
  host: 'localhost',
  electionTimeoutMin: 150,
  electionTimeoutMax: 300,
  heartbeatInterval: 50
});

// Create cluster manager
const cluster = createClusterManager({
  nodeId: 'node-1',
  healthCheckInterval: 5000,
  autoDiscovery: false
});

// Create state machine
const state = createDistributedStateMachine({
  nodeId: 'node-1',
  enableSnapshots: true,
  snapshotInterval: 1000
});

// Initialize
await coordinator.initialize();
await cluster.initialize(coordinator);
await state.initialize(coordinator);

// Add cluster nodes
await cluster.addNode({
  nodeId: 'node-2',
  host: 'localhost',
  port: 8081
});

await cluster.addNode({
  nodeId: 'node-3',
  host: 'localhost',
  port: 8082
});

// Listen for events
coordinator.on('leader_elected', ({ leaderId }) => {
  console.log(`New leader: ${leaderId}`);
});

cluster.on('node_health_changed', ({ nodeId, health }) => {
  console.log(`Node ${nodeId} health: ${health}`);
});

state.on('state_changed', ({ key, value, operation }) => {
  console.log(`State ${operation}: ${key}`);
});
```

## API Reference

### RaftCoordinator

Manages Raft consensus for the cluster.

```javascript
const coordinator = createRaftCoordinator({
  nodeId: string,           // Unique node identifier
  port: number,             // WebSocket port
  host: string,             // Host address
  electionTimeoutMin: 150,  // Min election timeout (ms)
  electionTimeoutMax: 300,  // Max election timeout (ms)
  heartbeatInterval: 50,    // Heartbeat interval (ms)
  snapshotThreshold: 1000   // Snapshot threshold
});

// Methods
await coordinator.initialize();
coordinator.addPeer(nodeId, host, port);
coordinator.removePeer(nodeId);
await coordinator.replicateCommand(command);
coordinator.getState();
coordinator.getWorkflowState(workflowId);
await coordinator.shutdown();

// Events
coordinator.on('leader_elected', ({ leaderId }) => {});
coordinator.on('became_follower', () => {});
coordinator.on('command_applied', ({ command, entry }) => {});
coordinator.on('peer_connected', ({ nodeId }) => {});
coordinator.on('peer_disconnected', ({ nodeId }) => {});
```

### ClusterManager

Manages dynamic cluster membership and health monitoring.

```javascript
const cluster = createClusterManager({
  nodeId: string,
  healthCheckInterval: 5000,      // Health check interval (ms)
  healthCheckTimeout: 2000,       // Health check timeout (ms)
  maxFailedHealthChecks: 3,       // Max failures before unhealthy
  autoDiscovery: false,           // Enable auto-discovery
  discoveryInterval: 10000        // Discovery interval (ms)
});

// Methods
await cluster.initialize(raftCoordinator);
await cluster.addNode(nodeMetadata);
await cluster.removeNode(nodeId);
cluster.getNodes();
cluster.getHealthyNodes();
cluster.getNodeHealth(nodeId);
cluster.getStats();
await cluster.shutdown();

// Events
cluster.on('node_joined', ({ nodeId, metadata }) => {});
cluster.on('node_left', ({ nodeId }) => {});
cluster.on('node_health_changed', ({ nodeId, health, previousHealth }) => {});
cluster.on('node_unhealthy', ({ nodeId }) => {});
```

### DistributedStateMachine

Replicated key-value store synchronized via Raft.

```javascript
const state = createDistributedStateMachine({
  nodeId: string,
  snapshotInterval: 1000,    // Entries between snapshots
  maxStateSize: 100000,      // Max state entries
  enableSnapshots: true,     // Enable snapshotting
  persistSnapshots: false    // Persist to disk
});

// Methods
await state.initialize(raftCoordinator);
await state.set(key, value);
state.get(key);
state.has(key);
await state.delete(key);
await state.update(key, updateFn);
await state.batchUpdate(changes);
state.keys();
state.values();
state.entries();
state.getStats();
await state.shutdown();

// Events
state.on('state_changed', ({ key, value, operation }) => {});
state.on('snapshot_created', ({ snapshotId, index, stateSize }) => {});
state.on('snapshot_restored', ({ snapshotId, index, stateSize }) => {});
```

### WebSocketTransport

Low-level WebSocket transport with msgpackr serialization.

```javascript
const transport = createWebSocketTransport({
  nodeId: string,
  port: number,
  host: '0.0.0.0',
  reconnectInterval: 1000,
  reconnectMaxRetries: 10,
  messageTimeout: 5000
});

// Methods
await transport.start();
transport.addPeer(nodeId, host, port);
transport.removePeer(nodeId);
await transport.send(to, message);
transport.sendResponse(originalMessage, response);
transport.getConnectedPeers();
transport.isConnected(nodeId);
await transport.shutdown();

// Events
transport.on('message', (message) => {});
transport.on('peer_connected', ({ nodeId }) => {});
transport.on('peer_disconnected', ({ nodeId }) => {});
transport.on('reconnect_failed', ({ nodeId, attempts }) => {});
```

## Examples

### Run Demos

```bash
# Simple 2-node demo
pnpm demo

# 3-node cluster with operations
pnpm demo:3node

# Failover test
pnpm demo:failover
```

### Workflow Coordination

```javascript
// Start workflow (on leader only)
await state.set('workflow:data-pipeline', {
  name: 'Data Processing',
  status: 'running',
  progress: 0,
  startedAt: Date.now()
});

// Update progress
await state.update('workflow:data-pipeline', prev => ({
  ...prev,
  progress: 0.5
}));

// Complete workflow
await state.update('workflow:data-pipeline', prev => ({
  ...prev,
  status: 'completed',
  progress: 1,
  completedAt: Date.now()
}));

// Read from any node (local read, no consensus needed)
const workflow = state.get('workflow:data-pipeline');
console.log(workflow.status); // 'completed'
```

### Batch Operations

```javascript
// Update multiple workflows atomically
await state.batchUpdate([
  { key: 'workflow:1', value: { status: 'completed' } },
  { key: 'workflow:2', value: { status: 'failed' } },
  { key: 'workflow:3', value: { status: 'running', progress: 0.75 } }
]);
```

## Architecture

### Raft Consensus

- **Leader Election**: Randomized timeouts (150-300ms default) prevent split votes
- **Log Replication**: Leader replicates commands to all followers
- **Commit Rules**: Majority acknowledgment required for commits
- **Safety**: Strong consistency guarantees via Raft algorithm

### Network Architecture

```
┌──────────────┐         ┌──────────────┐         ┌──────────────┐
│   Node 1     │◄───────►│   Node 2     │◄───────►│   Node 3     │
│  (Leader)    │         │  (Follower)  │         │  (Follower)  │
└──────────────┘         └──────────────┘         └──────────────┘
       │                         │                         │
       │    WebSocket + msgpackr │                         │
       └─────────────────────────┴─────────────────────────┘
```

### State Machine

1. **Command Submission**: Client submits to leader
2. **Log Replication**: Leader replicates to followers
3. **Majority Acknowledgment**: Wait for majority
4. **Commit**: Leader commits entry
5. **Apply**: All nodes apply to state machine
6. **Response**: Client receives acknowledgment

## Testing

```bash
# Run all tests
pnpm test

# Watch mode
pnpm test:watch
```

## Performance

- **Throughput**: 1000+ commands/sec (3-node cluster)
- **Latency**: 10-50ms (local network)
- **Failover**: 500-1000ms (leader re-election)
- **Message Size**: Reduced 40% with msgpackr vs JSON

## Integration with Federation

```javascript
import { createFederationCoordinator } from '@unrdf/federation';
import { createRaftCoordinator } from '@unrdf/consensus';

// Create Raft coordinator
const raft = createRaftCoordinator({
  nodeId: 'node-1',
  port: 8080
});
await raft.initialize();

// Create federation coordinator (uses Raft internally)
const federation = createFederationCoordinator({
  federationId: 'fed-1',
  enableConsensus: true
});
await federation.initialize();

// Federation automatically uses Raft for store registration
await federation.registerStore({
  storeId: 'store-1',
  endpoint: 'http://store1:3000'
});
```

## License

MIT
