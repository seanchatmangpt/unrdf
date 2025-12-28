# @unrdf/consensus Capability Map

**Version**: 1.0.0
**Status**: Production Ready
**Runtime**: Node.js ≥18.0.0
**Last Updated**: 2025-12-28

---

## Overview

Production-grade Raft consensus for distributed workflow coordination. Provides leader election, log replication, strong consistency guarantees, and fault tolerance for distributed RDF systems.

**Key Capabilities**:
- **RAFT Consensus**: Leader election, log replication, and safety guarantees
- **Cluster Management**: Dynamic membership changes with health monitoring
- **Distributed State Machine**: Replicated state with command execution
- **WebSocket Transport**: Low-latency RPC for cluster communication

**Package Exports**:
```javascript
import {
  createRaftCoordinator,
  createClusterManager,
  createDistributedStateMachine,
  createWebSocketTransport
} from '@unrdf/consensus';
```

**Dependencies**:
- Required: `@unrdf/federation` (workspace), `ws` (^8.18.3), `msgpackr` (^1.11.8), `zod` (^4.1.13)
- Optional: `@opentelemetry/api` (^1.9.0) for observability

**Evidence**:
- Test Coverage: Not specified
- Test Files: vitest-based test suite
- OTEL Validation: Not specified
- Example Files: 3-node cluster, failover demos

---

## Capability Atoms

### Core Capabilities (Tier 1)

| Atom | Type | Runtime | Evidence | Compositions |
|------|------|---------|----------|--------------|
| `RaftCoordinator` | Class | Node | [src/raft/raft-coordinator.mjs](file:///home/user/unrdf/packages/consensus/src/raft/raft-coordinator.mjs) | C1 |
| `createRaftCoordinator()` | Function | Node | [src/raft/raft-coordinator.mjs](file:///home/user/unrdf/packages/consensus/src/raft/raft-coordinator.mjs) | C1 |
| `ClusterManager` | Class | Node | [src/membership/cluster-manager.mjs](file:///home/user/unrdf/packages/consensus/src/membership/cluster-manager.mjs) | C2 |
| `createClusterManager()` | Function | Node | [src/membership/cluster-manager.mjs](file:///home/user/unrdf/packages/consensus/src/membership/cluster-manager.mjs) | C2 |
| `DistributedStateMachine` | Class | Node | [src/state/distributed-state-machine.mjs](file:///home/user/unrdf/packages/consensus/src/state/distributed-state-machine.mjs) | C3 |
| `WebSocketTransport` | Class | Node | [src/transport/websocket-transport.mjs](file:///home/user/unrdf/packages/consensus/src/transport/websocket-transport.mjs) | C4 |

**Verification**:
```bash
timeout 5s pnpm --filter @unrdf/consensus test
```

---

## Composition Patterns

**C1**: **RAFT Coordinator** - Leader election → Log replication → Commit
```javascript
import { createRaftCoordinator } from '@unrdf/consensus';

const coordinator = createRaftCoordinator({
  nodeId: 'node-1',
  port: 8080,
  peers: ['node-2:8081', 'node-3:8082'],
  electionTimeout: 300,
  heartbeatInterval: 100
});

await coordinator.initialize();
await coordinator.start();
```

**C2**: **Cluster Management** - Add peers → Health monitoring → Failover
```javascript
import { createClusterManager } from '@unrdf/consensus';

const cluster = createClusterManager({ nodeId: 'node-1' });
await cluster.initialize(coordinator);

await cluster.addPeer('node-4', { endpoint: 'http://node4:8080' });
const health = cluster.getHealth('node-4');
```

**C3**: **Distributed State Machine** - Apply commands → Replicate → Consensus
```javascript
import { createDistributedStateMachine } from '@unrdf/consensus';

const stateMachine = createDistributedStateMachine({ nodeId: 'node-1' });
await stateMachine.initialize(coordinator);

const result = await stateMachine.applyCommand({
  type: 'insert-quad',
  payload: quad
});
// Waits for quorum consensus
```

**C4**: **WebSocket Transport** - Low-latency RPC for RAFT
```javascript
import { createWebSocketTransport } from '@unrdf/consensus';

const transport = createWebSocketTransport({
  port: 8080,
  codec: 'msgpack' // Binary serialization
});

transport.on('message', (msg) => {
  // Handle RAFT message
});
```

---

## Performance Model

**Theoretical Performance**:

Based on RAFT algorithm:
- Time Complexity: O(n) for leader election rounds (n=cluster size)
- Space Complexity: O(l) for log entries (l=log size)
- Scalability: Limited by leader capacity

**Empirical Benchmarks** (from performance-analysis.md):

| Operation | Dataset Size | Execution Time | Notes |
|-----------|--------------|----------------|-------|
| Leader election | 3-node cluster | 100-500ms | Depends on RTT |
| Log replication | Per entry | 50-200ms | Quorum-dependent |
| Consensus throughput | Leader capacity | 100-500 commands/sec | Leader-limited |

**Performance Characteristics**:
- Requires stable network (partitions trigger re-election)
- Quorum requirement: >50% nodes must be available
- Leader bottleneck for all writes
- Byzantine tolerance requires 3f+1 nodes for f failures

**Optimization Strategies**:
1. **Batching**: Batch multiple commands per log entry
2. **Pipelining**: Pipeline log replication
3. **Leader Lease**: Reduce read latency with leader lease

**Verification**:
```bash
timeout 30s pnpm --filter @unrdf/consensus run demo:3node
```

---

## Runtime Compatibility Matrix

| Capability | Node.js | Browser | BEAM/WASM | Notes |
|------------|---------|---------|-----------|-------|
| RAFT Coordinator | ✅ ≥18.0 | ❌ Not supported | ❌ Not supported | Server-only |
| Cluster Manager | ✅ ≥18.0 | ❌ Not supported | ❌ Not supported | Server-only |
| State Machine | ✅ ≥18.0 | ❌ Not supported | ❌ Not supported | Server-only |
| WebSocket Transport | ✅ ≥18.0 | ❌ Not supported | ❌ Not supported | Server-only |

**Legend**:
- ✅ Fully supported
- ❌ Not supported

---

## Evidence & Verification

### Source Code References

All capability atoms are traceable to source:
- RAFT: [src/raft/raft-coordinator.mjs](file:///home/user/unrdf/packages/consensus/src/raft/raft-coordinator.mjs)
- Cluster: [src/membership/cluster-manager.mjs](file:///home/user/unrdf/packages/consensus/src/membership/cluster-manager.mjs)
- State Machine: [src/state/distributed-state-machine.mjs](file:///home/user/unrdf/packages/consensus/src/state/distributed-state-machine.mjs)

### Verification Commands

**Quick Verification** (< 5 seconds):
```bash
timeout 5s pnpm --filter @unrdf/consensus test
```

---

## Cross-References

### Related Packages
- **@unrdf/federation**: Federated query coordination

### External Resources
- [RAFT Paper](https://raft.github.io/raft.pdf)
- [RAFT Consensus](https://raft.github.io/)

---

**Document Metadata**:
- **Template Version**: 1.0.0
- **Generated**: 2025-12-28
- **Maintainer**: @unrdf/core-team
- **Last Review**: 2025-12-28
- **Next Review**: 2026-03-28
