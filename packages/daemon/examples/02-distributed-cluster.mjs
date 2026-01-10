/**
 * @file Distributed Cluster Example
 * @module examples/02-distributed-cluster
 * @description Demonstrates multi-node daemon cluster setup:
 * - Creating multiple daemon nodes
 * - Coordinating operations across cluster
 * - Simulating leader election
 * - Distributed work distribution
 * - Failover handling
 *
 * This example simulates a 3-node cluster without requiring
 * external Raft implementation (uses simple leader tracking).
 */

import { Daemon } from '../src/daemon.mjs';

/**
 * Simulate a simple cluster manager (normally Raft-based)
 */
class ClusterManager {
  constructor(nodeIds) {
    this.nodeIds = nodeIds;
    this.leaderId = nodeIds[0]; // Start with first node as leader
    this.nodeStates = new Map();
    this.nodeIds.forEach(id => {
      this.nodeStates.set(id, { healthy: true, lastHeartbeat: Date.now() });
    });
  }

  electNewLeader() {
    const healthyNodes = Array.from(this.nodeStates.entries())
      .filter(([_, state]) => state.healthy)
      .map(([id]) => id);

    if (healthyNodes.length > 0) {
      this.leaderId = healthyNodes[0];
      return this.leaderId;
    }
    return null;
  }

  setNodeHealth(nodeId, healthy) {
    const state = this.nodeStates.get(nodeId);
    if (state) {
      state.healthy = healthy;
      state.lastHeartbeat = Date.now();
      if (!healthy && this.leaderId === nodeId) {
        this.electNewLeader();
      }
    }
  }

  getHealthyNodes() {
    return Array.from(this.nodeStates.entries())
      .filter(([_, state]) => state.healthy)
      .map(([id]) => id);
  }

  isLeader(nodeId) {
    return this.leaderId === nodeId;
  }
}

/**
 * Example: 3-node cluster coordinating batch processing
 */
async function distributedClusterExample() {
  console.log('=== Distributed Daemon Cluster Example ===\n');

  // Setup cluster
  const clusterManager = new ClusterManager(['node-1', 'node-2', 'node-3']);
  const daemons = new Map();

  // Create daemon for each node
  for (const nodeId of clusterManager.nodeIds) {
    const daemon = new Daemon({
      id: `daemon-${nodeId}`,
      nodeId,
      clusterId: 'production-cluster',
      logger: console,
    });
    daemons.set(nodeId, daemon);
  }

  // Define distributed operation (runs on all nodes)
  const processBatch = {
    id: 'process-batch',
    name: 'Process Data Batch',
    handler: async function() {
      // Handler has access to 'this' context
      const nodeId = this.nodeId || 'unknown';
      const itemsPerNode = 100;

      // Simulate batch processing
      await new Promise(resolve => setTimeout(resolve, Math.random() * 1000));

      return {
        status: 'completed',
        nodeId,
        itemsProcessed: itemsPerNode,
        timestamp: new Date().toISOString(),
      };
    },
    metadata: { distributed: true, scope: 'global' },
  };

  // Define leader-only operation
  const aggregateResults = {
    id: 'aggregate-results',
    name: 'Aggregate Results',
    handler: async function() {
      const nodeId = this.nodeId || 'unknown';
      const isLeader = clusterManager.isLeader(nodeId);

      if (!isLeader) {
        return {
          status: 'skipped',
          reason: 'not_leader',
          nodeId,
        };
      }

      // Simulate aggregation
      await new Promise(resolve => setTimeout(resolve, 500));

      return {
        status: 'aggregated',
        nodeId,
        totalItems: 300, // 100 * 3 nodes
        aggregationType: 'sum',
        timestamp: new Date().toISOString(),
      };
    },
    metadata: { distributed: true, scope: 'leader' },
  };

  // Setup event monitoring for all nodes
  daemons.forEach((daemon, nodeId) => {
    daemon.on('operation:success', (event) => {
      console.log(`[${nodeId}] ✓ ${event.name} (${event.duration}ms)`);
    });

    daemon.on('operation:failure', (event) => {
      console.error(`[${nodeId}] ✗ ${event.name} - ${event.error}`);
    });
  });

  // Start all daemons
  console.log('🚀 Starting cluster nodes...\n');
  for (const daemon of daemons.values()) {
    await daemon.start();
  }

  // Schedule operations on all nodes
  daemons.forEach(daemon => {
    daemon.schedule(processBatch);
    daemon.schedule(aggregateResults);
  });

  // Display initial cluster state
  console.log('📊 Initial Cluster State:');
  console.log(`  Leader: ${clusterManager.leaderId}`);
  console.log(`  Healthy Nodes: ${clusterManager.getHealthyNodes().join(', ')}`);
  console.log('');

  // Phase 1: All nodes process batch
  console.log('📌 Phase 1: Distributed Batch Processing');
  console.log('Executing on all 3 nodes...\n');

  const batchResults = await Promise.all(
    Array.from(daemons.values()).map(d => {
      // Bind 'this' context to handler
      const daemon = d;
      const boundHandler = async () => {
        return processBatch.handler.call(daemon);
      };
      const op = { ...processBatch, handler: boundHandler };
      return d.execute(processBatch.id).catch(e => ({ error: e.message }));
    })
  );

  console.log('\nResults:');
  batchResults.forEach((result, idx) => {
    console.log(`  Node ${idx + 1}: ${JSON.stringify(result)}`);
  });

  // Wait between phases
  await new Promise(resolve => setTimeout(resolve, 1000));

  // Phase 2: Leader aggregates results
  console.log('\n📌 Phase 2: Leader Aggregation');
  console.log(`Leader node: ${clusterManager.leaderId}\n`);

  const leaderDaemon = daemons.get(clusterManager.leaderId);
  const boundAggHandler = async () => {
    return aggregateResults.handler.call(leaderDaemon);
  };

  const aggregateOp = { ...aggregateResults, handler: boundAggHandler };

  const aggregateResult = await leaderDaemon.execute(aggregateResults.id)
    .catch(e => ({ error: e.message }));

  console.log(`Result: ${JSON.stringify(aggregateResult)}`);

  // Wait before failover simulation
  await new Promise(resolve => setTimeout(resolve, 1000));

  // Phase 3: Simulate leader failure and failover
  console.log('\n📌 Phase 3: Simulating Leader Failure & Failover');
  const deadLeader = clusterManager.leaderId;
  console.log(`Marking ${deadLeader} as unhealthy...\n`);

  clusterManager.setNodeHealth(deadLeader, false);
  const newLeader = clusterManager.leaderId;
  console.log(`✓ New leader elected: ${newLeader}`);
  console.log(`✓ Healthy nodes: ${clusterManager.getHealthyNodes().join(', ')}`);

  // Show health of each node
  console.log('\n📋 Cluster Health Status:');
  daemons.forEach((daemon, nodeId) => {
    const health = daemon.getHealth();
    const isLeader = clusterManager.isLeader(nodeId);
    const status = clusterManager.nodeStates.get(nodeId).healthy ? '🟢' : '🔴';

    console.log(`  ${status} ${nodeId}:`);
    console.log(`    Running: ${health.isRunning}`);
    console.log(`    Role: ${isLeader ? 'LEADER' : 'FOLLOWER'}`);
    console.log(`    Uptime: ${Math.round(health.uptime / 1000)}s`);
    console.log(`    Completed Ops: ${health.completedOperations}`);
  });

  // Show aggregate metrics
  console.log('\n📈 Cluster Metrics:');
  let totalOps = 0;
  let totalSuccess = 0;

  daemons.forEach((daemon, nodeId) => {
    const metrics = daemon.getMetrics();
    totalOps += metrics.totalOperations;
    totalSuccess += metrics.successfulOperations;
    console.log(`  ${nodeId}:`);
    console.log(`    Executed: ${metrics.totalOperations}`);
    console.log(`    Success Rate: ${metrics.successRate.toFixed(1)}%`);
  });

  console.log(`  \n  Cluster Total:`);
  console.log(`    Total Operations: ${totalOps}`);
  console.log(`    Total Successful: ${totalSuccess}`);
  console.log(`    Success Rate: ${totalOps > 0 ? (totalSuccess / totalOps * 100).toFixed(1) : 0}%`);

  // Cleanup
  console.log('\n⏹️  Stopping cluster...');
  const stopPromises = Array.from(daemons.values()).map(d => d.stop());
  await Promise.all(stopPromises);
  console.log('✓ All nodes stopped gracefully\n');
}

// Run example
await distributedClusterExample();
