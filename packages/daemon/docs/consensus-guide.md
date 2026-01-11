# Raft Consensus Integration Guide for UNRDF Daemon

## Overview

The Raft consensus integration enables UNRDF Daemon to coordinate distributed operation execution across a cluster of nodes. Using the Raft consensus algorithm, the daemon ensures strong consistency, automatic leader election, and resilience to network partitions and node failures.

**Key Capabilities**:
- **Leader Election**: Automatic election of cluster leader within 3-node minimum cluster
- **Operation Replication**: Atomic replication of operations across healthy cluster nodes
- **Network Partition Recovery**: Automatic detection and recovery from split-brain scenarios
- **Graceful Node Removal**: Coordinated node removal with log replication verification
- **Log Compaction**: Automatic log cleanup with safety guarantees

---

## Architecture

### Component Integration

```
┌─────────────────────────────────────────────────────────────┐
│ Daemon (Operation Executor)                                 │
├─────────────────────────────────────────────────────────────┤
│ ConsensusManager                                            │
│ ├─ RaftCoordinator (Leader Election + Log Replication)   │
│ ├─ ClusterManager (Membership + Health Monitoring)        │
│ └─ Operation Log (State Machine)                          │
├─────────────────────────────────────────────────────────────┤
│ Network Transport (WebSocket)                               │
├─────────────────────────────────────────────────────────────┤
│ Cluster Peers (node-2, node-3, ...)                        │
└─────────────────────────────────────────────────────────────┘
```

### State Machine

Operations flow through distinct states in the consensus layer:

```
PENDING ──→ REPLICATED ──→ COMMITTED
   │                            ▲
   └──────→ FAILED ─────────────┘
```

- **PENDING**: Operation queued but not yet replicated
- **REPLICATED**: Operation replicated to majority of nodes
- **COMMITTED**: Operation durably committed to cluster state machine
- **FAILED**: Operation failed to replicate or commit

---

## Cluster Setup Procedures

### Minimum Cluster Requirements

- **Minimum Nodes**: 3 (for split-brain prevention and fault tolerance)
- **Replication Factor**: 3 (all nodes must have copy)
- **Network**: Low-latency, reliable connection (same datacenter recommended)
- **Storage**: Persistent disk for operation log

### Step 1: Initialize Cluster

```javascript
import { createRaftCoordinator, createClusterManager } from '@unrdf/consensus';
import { Daemon } from '@unrdf/daemon';
import { ConsensusManager } from '@unrdf/daemon/integrations/consensus';

// Node 1 (Bootstrap)
const daemon1 = new Daemon({
  daemonId: 'daemon-1',
  name: 'cluster-node-1',
  nodeId: 'node-1',
  clusterId: 'prod-cluster',
});

const raftNode1 = createRaftCoordinator({
  nodeId: 'node-1',
  port: 8080,
  host: '10.0.1.100', // Internal IP
});

const clusterMgr1 = createClusterManager({
  nodeId: 'node-1',
  healthCheckInterval: 5000,
  autoDiscovery: true,
});

const consensus1 = new ConsensusManager(daemon1, raftNode1, clusterMgr1, {
  replicationFactor: 3,
  partitionDetectionMs: 3000,
});

await daemon1.start();
await raftNode1.initialize();
await clusterMgr1.initialize(raftNode1);
await consensus1.initialize();
```

### Step 2: Add Cluster Peers

After initializing leader node, add followers:

```javascript
// On leader (node-1)
raftNode1.addPeer('node-2', '10.0.1.101', 8081);
raftNode1.addPeer('node-3', '10.0.1.102', 8082);

// Add to cluster manager
clusterMgr1.addNode({
  nodeId: 'node-2',
  host: '10.0.1.101',
  port: 8081,
  capabilities: ['task-execution', 'event-processing'],
});

clusterMgr1.addNode({
  nodeId: 'node-3',
  host: '10.0.1.102',
  port: 8082,
  capabilities: ['task-execution', 'event-processing'],
});
```

### Step 3: Verify Cluster Health

```javascript
// Check leader election
const state = consensus1.getConsensusState();
console.log('Leader:', state.leaderId);
console.log('Is Leader:', state.isLeader);
console.log('Current Term:', state.currentTerm);
console.log('Partition State:', state.partitionState);

// Expected output:
// Leader: node-1
// Is Leader: true
// Current Term: 1
// Partition State: healthy
```

### Step 4: Monitor Ongoing Health

```javascript
setInterval(() => {
  const state = consensus1.getConsensusState();
  const daemon1Health = daemon1.getHealth();

  console.log(`[${new Date().toISOString()}] Consensus State`, {
    leader: state.leaderId,
    term: state.currentTerm,
    logSize: state.logSize,
    partitions: state.partitionState,
    activeOps: daemon1Health.activeOperations,
  });
}, 10000);
```

---

## Operation Replication

### Replicating Operations

```javascript
// Only leader can replicate operations
if (state.isLeader) {
  const result = await consensus1.replicateOperation({
    id: 'task-123',
    daemonId: 'daemon-1',
    type: 'data_import',
    payload: {
      sourceUrl: 'https://data.example.com/dataset',
      batchSize: 1000,
    },
    timestamp: Date.now(),
  });

  console.log({
    operationId: result.operationId,
    replicated: result.replicated,      // true = replicated to majority
    committed: result.committed,         // true = durably stored
    logIndex: result.logIndex,          // Position in log
    term: result.term,                  // Current Raft term
  });
}
```

### Replication Safety Guarantees

1. **Majority Replication**: Operation replicates to at least (N/2 + 1) nodes
2. **Durability**: Once committed, operation survives node failures
3. **Ordering**: Operations applied in same order on all nodes
4. **Idempotence**: Safe to retry failed operations (use operation ID)

### Example: Idempotent Operation Handler

```javascript
class OperationHandler {
  constructor(daemon, consensus) {
    this.daemon = daemon;
    this.consensus = consensus;
    this.executedOps = new Set(); // Persistent store in production
  }

  async executeOperation(opId, handler) {
    // Prevent duplicate execution
    if (this.executedOps.has(opId)) {
      return { skipped: true, opId };
    }

    try {
      const result = await handler();
      this.executedOps.add(opId);
      return { success: true, opId, result };
    } catch (error) {
      return { error: error.message, opId };
    }
  }
}
```

---

## Failure Scenarios and Recovery

### Scenario 1: Leader Failure

**Situation**: Current leader crashes or becomes unreachable

**Detection**: Followers detect missing heartbeats (3-5 second timeout)

**Recovery**: Automatic new leader election
```javascript
consensus1.on('consensus:leader_lost', () => {
  console.log('Leader lost, triggering election...');
  // Automatically handled by Raft
});

consensus1.on('consensus:leader_elected', ({ leaderId, term }) => {
  console.log(`New leader elected: ${leaderId} at term ${term}`);
  // Resume operation replication
});
```

**Time to Recovery**: 3-5 seconds
**Data Loss**: None (majority has current state)

### Scenario 2: Network Partition (Split-Brain)

**Situation**: Cluster splits into two partitions

**Detection**: Nodes in minority partition lose heartbeats
```javascript
consensus1.on('partition:detected', ({ nodeId, health }) => {
  console.log(`Partition detected from node ${nodeId}`);
  console.log(`Partition state: ${consensus1.partitionState}`);
  // Minority partition stops accepting new operations
});
```

**Safety**: Only majority partition can elect leader
- Minority partition: Returns errors to operation requests
- Majority partition: Continues normal operation

**Recovery**: When partition heals, minority rejoins
```javascript
consensus1.on('partition:recovered', ({ newLeader }) => {
  console.log(`Partition recovered, leader: ${newLeader}`);
  // Minority replays log from majority leader
});
```

### Scenario 3: Single Node Failure

**Situation**: One node crashes, cluster has 3 nodes

**Impact**:
- Quorum still available (2/3 nodes)
- Operations continue normally
- Node automatically rejoins when back online

**Recovery**: Automated catch-up
```javascript
clusterManager.on('node_joined', ({ nodeId }) => {
  console.log(`Node rejoined: ${nodeId}`);
  // Consensus automatically replicates missing operations
});
```

### Scenario 4: Cascading Failures

**Situation**: Multiple nodes fail rapidly

**Safety**:
- After 2nd node failure: Cluster becomes read-only (cannot commit new operations)
- After 3rd node failure: Cluster halts entirely

**Prevention**:
```javascript
// Monitor available nodes
const healthyNodes = clusterManager.getHealthyNodes();
if (healthyNodes.length < 2) {
  console.warn('CRITICAL: Cluster lost quorum, no new operations accepted');
  // Page on-call engineer
}
```

**Recovery**: Wait for majority of nodes to return
```javascript
// Monitor recovery
const availableNodes = clusterManager.getHealthyNodes();
if (availableNodes.length >= 2) {
  console.log('Quorum restored, cluster operational again');
}
```

---

## Graceful Node Removal

### Removing a Node from Cluster

```javascript
// Only leader can remove nodes
if (consensus1.isLeader) {
  try {
    const result = await consensus1.removeNodeGracefully('node-2');
    console.log(`Node removed: ${result.nodeId}`);
  } catch (error) {
    console.error(`Failed to remove node: ${error.message}`);
  }
}
```

### Graceful Shutdown Procedure

```javascript
async function shutdownNode() {
  const nodeId = daemon1.nodeId;

  // Step 1: Stop accepting new operations
  daemon1.isRunning = false;

  // Step 2: Wait for current operations to complete (max 30s)
  const startTime = Date.now();
  while (daemon1.activeCount > 0 && Date.now() - startTime < 30000) {
    await new Promise(resolve => setTimeout(resolve, 100));
  }

  // Step 3: Remove from cluster
  if (consensus1.isLeader) {
    await consensus1.removeNodeGracefully(nodeId);
  } else {
    // Follower: Just shutdown
    await consensus1.shutdown();
  }

  // Step 4: Stop all services
  await raftNode1.stop();
  await daemon1.stop();

  console.log(`Node ${nodeId} shutdown gracefully`);
}
```

### Readmitting Rejoined Node

```javascript
// When node comes back online
const daemon2 = new Daemon({
  daemonId: 'daemon-2',
  name: 'cluster-node-2',
  nodeId: 'node-2',
  clusterId: 'prod-cluster',
});

const consensus2 = new ConsensusManager(daemon2, raftNode2, clusterMgr2);
await consensus2.initialize();

// Consensus automatically replicates missing log entries
consensus2.on('log:compacted', ({ entriesRemoved }) => {
  console.log(`Caught up on ${entriesRemoved} operations`);
});
```

---

## Performance Characteristics

### Latency (P95)

| Operation | Latency | Notes |
|-----------|---------|-------|
| Local operation | <1ms | Execute without replication |
| Replicate to majority | 5-50ms | Network RTT dependent |
| Wait for commit | 10-100ms | Includes leader + 2 followers |
| Log compaction | <10ms | Async, non-blocking |
| Leader election | 3000-5000ms | Only on failure, rare |

### Throughput

```
Baseline (3-node cluster):
  - 1000+ operations/second sustained
  - P99 latency: <100ms
  - Network bandwidth: ~500KB/s per node

Under stress (100 concurrent):
  - 5000+ operations/second peak
  - P99 latency: <500ms
  - Network bandwidth: ~2MB/s per node
```

### Memory Usage

```
Per node (idle):
  - Daemon: 20-50MB
  - Consensus manager: 10-20MB
  - Raft coordinator: 5-10MB
  - Total: ~50MB baseline

Per 10,000 operations in log:
  - Additional: ~5MB
  - (Compact to keep <100MB per node)
```

### Scaling

**Optimal cluster size**: 3-5 nodes
- 3 nodes: Best latency, min redundancy
- 5 nodes: Balance latency vs fault tolerance
- 7+ nodes: Higher latency, diminishing redundancy benefit

---

## Troubleshooting Checklist

### Cluster Not Starting

**Symptoms**:
- No leader elected
- All nodes showing as followers
- Timeout errors on replication

**Diagnosis**:
```bash
# Check network connectivity between nodes
ping 10.0.1.101
ping 10.0.1.102

# Verify port accessibility
netstat -tuln | grep 808

# Check logs for connection errors
journalctl -u daemon -n 50 | grep "peer"
```

**Solutions**:
1. Verify all nodes can reach each other on configured ports
2. Check firewall rules (allow TCP 8080-8082)
3. Ensure clock sync across nodes (`ntpd` running)
4. Review consensus config (replicationFactor matches node count)

### Partition Detected But Cluster Operational

**Symptoms**:
- Partition events emitted frequently
- Operations still succeed
- Performance degraded

**Diagnosis**:
```javascript
const state = consensus1.getConsensusState();
console.log('Partition state:', state.partitionState);
console.log('Time since last heartbeat:',
  Date.now() - consensus1.lastHeartbeat);
```

**Solutions**:
1. Check network latency: `ping -c 10 peer-node` (should be <5ms)
2. Increase `partitionDetectionMs` if network is flaky
3. Check for network packet loss: `mtr -c 10 peer-node`
4. Move cluster to lower-latency network

### Memory Growing Unbounded

**Symptoms**:
- Memory usage steadily increasing
- Process gets OOM killed after hours
- Log compaction events not emitted

**Diagnosis**:
```javascript
const state = consensus1.getConsensusState();
console.log('Log size:', state.logSize);
// If > 10,000 entries without compaction, issue exists
```

**Solutions**:
1. Lower `logCompactionThreshold` (default 5000)
2. Lower `maxLogSize` (default 10000)
3. Check for stuck operations that never complete
4. Implement operation TTL to remove old entries

### Operations Not Committing

**Symptoms**:
- `replicateOperation` returns `committed: false`
- Operations eventually timeout
- High latency on new operations

**Diagnosis**:
```javascript
const state = consensus1.getConsensusState();
const clusterState = clusterManager.getHealthyNodes();
console.log(`Healthy nodes: ${clusterState.length}`);
console.log(`Quorum available: ${clusterState.length >= 2}`);
```

**Solutions**:
1. Check if quorum available (at least 2/3 nodes healthy)
2. Verify leader is healthy: `state.isLeader && state.leaderId`
3. Check for slow operations blocking replication queue
4. Restart leader to force new election

### High Replication Latency

**Symptoms**:
- `replicateOperation` latency > 500ms
- P99 commit time > 1000ms
- Cluster otherwise healthy

**Diagnosis**:
```bash
# Check network latency
iperf3 -c peer-node -t 10  # Target: >100Mbps

# Monitor system resources
top -b -n 1 | head -20     # Check CPU, memory
iotop -b -n 1              # Check disk I/O
```

**Solutions**:
1. Move cluster to lower-latency network
2. Add more CPU cores (Raft CPU intensive)
3. Use SSD for operation log (faster I/O)
4. Reduce operation payload size

### Leader Not Being Elected

**Symptoms**:
- No `consensus:leader_elected` event
- Multiple nodes think they're followers
- Cluster unable to process operations

**Diagnosis**:
```javascript
// On each node
setInterval(() => {
  console.log({
    nodeId: consensus.nodeId,
    isLeader: consensus.isLeader,
    leaderId: consensus.leaderId,
    currentTerm: consensus.currentTerm,
  });
}, 1000);
```

**Solutions**:
1. Check network connectivity (leader election needs heartbeats)
2. Verify system time is synchronized (NTP)
3. Check for split-brain condition (two nodes think they're leader)
4. Restart Raft coordinator on all nodes

---

## Monitoring and Alerting

### Key Metrics to Monitor

```javascript
// Dashboard metrics
const metrics = {
  'consensus.leader_id': state.leaderId,
  'consensus.is_leader': state.isLeader ? 1 : 0,
  'consensus.current_term': state.currentTerm,
  'consensus.partition_state': partitionStateValue(state.partitionState),
  'consensus.log_size': state.logSize,
  'consensus.operations.replicated': replicatedCount,
  'consensus.operations.committed': committedCount,
  'cluster.healthy_nodes': clusterManager.getHealthyNodes().length,
  'cluster.total_nodes': clusterManager.nodes.size,
};
```

### Alert Thresholds

```
- No leader elected: CRITICAL (alert immediately)
- Partition detected: WARNING (if > 1 minute, escalate)
- Quorum lost: CRITICAL (no new operations possible)
- Memory > 500MB: WARNING (compaction may be failing)
- Replication latency P99 > 1s: WARNING (network issue)
- Operation success rate < 99%: WARNING (check node health)
```

### Prometheus Integration

```
# HELP consensus_operations_replicated Total operations replicated
# TYPE consensus_operations_replicated counter
consensus_operations_replicated{node="node-1"} 10000

# HELP consensus_log_size Current operation log size
# TYPE consensus_log_size gauge
consensus_log_size{node="node-1"} 500

# HELP consensus_partitions_detected Total network partitions detected
# TYPE consensus_partitions_detected counter
consensus_partitions_detected{node="node-1"} 2
```

---

## Advanced Configuration

### Tuning for Latency-Sensitive Workloads

```javascript
const config = {
  replicationFactor: 3,
  partitionDetectionMs: 1000,      // Fast partition detection
  recoveryRetryMs: 100,             // Quick retry
  snapshotInterval: 500,            // Frequent snapshots
  logCompactionThreshold: 1000,     // Compact often
  maxLogSize: 2000,                 // Keep small log
};
```

### Tuning for High-Throughput Workloads

```javascript
const config = {
  replicationFactor: 3,
  partitionDetectionMs: 5000,       // Tolerate brief hiccups
  recoveryRetryMs: 1000,            // Slower retry (save network)
  snapshotInterval: 2000,           // Less frequent snapshots
  logCompactionThreshold: 10000,    // Batch compaction
  maxLogSize: 20000,                // Larger buffer
};
```

---

## Best Practices

1. **Always Use Odd Number of Nodes**: 3, 5, or 7 nodes (not 2, 4, 6)
2. **Monitor Partition State**: Alert on any partition detection
3. **Implement Operation Idempotence**: Handle duplicate execution
4. **Version Your Operation Schema**: Allow evolution over time
5. **Regular Backups**: Export operation log periodically
6. **Test Failure Scenarios**: Kill nodes in production-like tests
7. **Use Configuration Management**: Version Raft/Cluster config
8. **Log Rotation**: Don't let log consume unlimited disk
9. **Health Checks**: Custom health checks for operation handlers
10. **Gradual Rollout**: Add/remove nodes one at a time

---

## References

- [Raft Consensus Paper](https://raft.github.io/raft.pdf)
- [@unrdf/consensus API Docs](../../../consensus/README.md)
- [@unrdf/daemon API Docs](../README.md)
- [Distributed Systems Testing Guide](../../../docs/testing-guide.md)
