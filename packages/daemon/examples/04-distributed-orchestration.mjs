/**
 * @file Distributed Orchestration Example
 * @module examples/04-distributed-orchestration
 * @description Demonstrates 3-node Raft cluster setup with distributed operation coordination and failure recovery.
 *
 * Features:
 * - Multi-node Raft cluster (3 nodes)
 * - Leader election and failover
 * - Distributed operation scheduling and execution
 * - Node health monitoring
 * - Graceful failure recovery
 * - Cluster-wide metrics aggregation
 *
 * Use Case: High-availability workflow orchestration across multiple nodes
 */

import { Daemon } from '../src/daemon.mjs';
import { EventEmitter } from 'events';

/**
 * Simple Raft cluster simulator
 * Emulates Raft consensus for node coordination
 */
class RaftCluster extends EventEmitter {
  constructor(nodes) {
    super();
    this.nodes = new Map(nodes.map(id => [id, {
      id,
      status: 'healthy',
      term: 0,
      votedFor: null,
      log: [],
      commitIndex: 0,
    }]));

    this.currentLeader = null;
    this.currentTerm = 0;
  }

  /**
   * Run leader election
   * @returns {string} Elected leader node ID
   */
  electLeader() {
    const healthyNodes = Array.from(this.nodes.entries())
      .filter(([, node]) => node.status === 'healthy')
      .map(([id]) => id);

    if (healthyNodes.length === 0) {
      throw new Error('No healthy nodes available');
    }

    this.currentTerm += 1;
    this.currentLeader = healthyNodes[0];
    this.nodes.get(this.currentLeader).term = this.currentTerm;

    this.emit('leader:elected', {
      leader: this.currentLeader,
      term: this.currentTerm,
      timestamp: new Date(),
    });

    return this.currentLeader;
  }

  /**
   * Append entry to cluster log (replicated across nodes)
   * @param {Object} entry - Log entry
   * @returns {boolean} Whether entry was replicated
   */
  appendEntry(entry) {
    if (!this.currentLeader) {
      this.electLeader();
    }

    const leaderNode = this.nodes.get(this.currentLeader);
    const logEntry = {
      term: this.currentTerm,
      timestamp: Date.now(),
      ...entry,
    };

    leaderNode.log.push(logEntry);

    // Simulate replication to followers
    let replicatedCount = 1; // Leader itself
    for (const [nodeId, node] of this.nodes) {
      if (nodeId !== this.currentLeader && node.status === 'healthy') {
        node.log.push(logEntry);
        replicatedCount += 1;
      }
    }

    const majority = Math.ceil(this.nodes.size / 2);
    const replicated = replicatedCount >= majority;

    if (replicated) {
      leaderNode.commitIndex = leaderNode.log.length;
      this.emit('entry:replicated', {
        entryId: entry.id,
        replicatedTo: replicatedCount,
        timestamp: new Date(),
      });
    }

    return replicated;
  }

  /**
   * Mark node as failed
   * Triggers leader election if leader fails
   * @param {string} nodeId - Node to fail
   */
  failNode(nodeId) {
    const node = this.nodes.get(nodeId);
    if (!node) {
      throw new Error(`Node not found: ${nodeId}`);
    }

    node.status = 'failed';

    this.emit('node:failed', {
      nodeId,
      timestamp: new Date(),
    });

    // Trigger new election if leader failed
    if (nodeId === this.currentLeader) {
      const healthyNodes = Array.from(this.nodes.entries())
        .filter(([, n]) => n.status === 'healthy')
        .length;

      if (healthyNodes > this.nodes.size / 2) {
        this.electLeader();
      }
    }
  }

  /**
   * Recover failed node
   * @param {string} nodeId - Node to recover
   */
  recoverNode(nodeId) {
    const node = this.nodes.get(nodeId);
    if (!node) {
      throw new Error(`Node not found: ${nodeId}`);
    }

    node.status = 'healthy';

    this.emit('node:recovered', {
      nodeId,
      timestamp: new Date(),
    });
  }

  /**
   * Get cluster status
   * @returns {Object} Cluster status
   */
  getStatus() {
    const nodes = Array.from(this.nodes.entries()).map(([id, node]) => ({
      id,
      status: node.status,
      logSize: node.log.length,
      term: node.term,
    }));

    const healthyCount = nodes.filter(n => n.status === 'healthy').length;

    return {
      leader: this.currentLeader,
      term: this.currentTerm,
      healthyNodes: healthyCount,
      totalNodes: this.nodes.size,
      nodes,
    };
  }
}

/**
 * Distributed daemon coordinator
 * Coordinates operations across cluster nodes
 */
class DistributedDaemonCoordinator extends EventEmitter {
  constructor(cluster) {
    super();
    this.cluster = cluster;
    this.operations = new Map();
    this.nodeOperations = new Map();
    this.results = new Map();
  }

  /**
   * Schedule operation for distributed execution
   * @param {Object} params - Operation parameters
   * @returns {Promise<Object>} Scheduling result
   */
  async scheduleDistributedOperation(params) {
    const { operationId, operationName, targetNodes, handler } = params;

    const operation = {
      operationId,
      operationName,
      targetNodes,
      handler,
      status: 'scheduled',
      scheduledAt: Date.now(),
      executions: new Map(),
    };

    this.operations.set(operationId, operation);

    // Replicate in cluster log
    const replicated = this.cluster.appendEntry({
      id: operationId,
      type: 'operation_schedule',
      operation: { operationId, operationName, targetNodes },
    });

    if (!replicated) {
      throw new Error('Failed to replicate operation in cluster');
    }

    this.emit('operation:scheduled', {
      operationId,
      operationName,
      targetNodes,
      timestamp: new Date(),
    });

    return {
      operationId,
      scheduled: true,
      replicated,
    };
  }

  /**
   * Execute operation on target nodes
   * @param {string} operationId - Operation identifier
   * @returns {Promise<Object>} Execution results
   */
  async executeDistributed(operationId) {
    const operation = this.operations.get(operationId);
    if (!operation) {
      throw new Error(`Operation not found: ${operationId}`);
    }

    operation.status = 'executing';
    const results = [];

    console.log(`    ▶️  Executing on ${operation.targetNodes.length} nodes`);

    for (const nodeId of operation.targetNodes) {
      const node = this.cluster.nodes.get(nodeId);

      if (!node || node.status !== 'healthy') {
        console.log(`    ⚠️  Node ${nodeId} is not healthy, skipping`);
        continue;
      }

      try {
        const result = await operation.handler(nodeId);

        const execution = {
          nodeId,
          status: 'success',
          result,
          startTime: Date.now(),
          duration: Date.now() - Date.now(),
        };

        operation.executions.set(nodeId, execution);
        results.push(execution);

        console.log(`    ✓ Completed on ${nodeId}`);

        this.emit('operation:executed', {
          operationId,
          nodeId,
          status: 'success',
        });
      } catch (error) {
        console.error(`    ✗ Failed on ${nodeId}: ${error.message}`);

        const execution = {
          nodeId,
          status: 'failed',
          error: error.message,
        };

        operation.executions.set(nodeId, execution);
        results.push(execution);

        this.emit('operation:executed', {
          operationId,
          nodeId,
          status: 'failed',
          error: error.message,
        });
      }
    }

    operation.status = 'completed';

    // Replicate completion
    this.cluster.appendEntry({
      id: `${operationId}-complete`,
      type: 'operation_complete',
      operationId,
      resultCount: results.length,
    });

    return {
      operationId,
      results,
      completedCount: results.filter(r => r.status === 'success').length,
      failedCount: results.filter(r => r.status === 'failed').length,
    };
  }

  /**
   * Get cluster-wide operation results
   * @returns {Object} Aggregated results
   */
  getClusterMetrics() {
    const totalOperations = this.operations.size;
    const completedOperations = Array.from(this.operations.values())
      .filter(op => op.status === 'completed').length;

    let totalExecutions = 0;
    let successfulExecutions = 0;

    for (const operation of this.operations.values()) {
      for (const execution of operation.executions.values()) {
        totalExecutions += 1;
        if (execution.status === 'success') {
          successfulExecutions += 1;
        }
      }
    }

    return {
      totalOperations,
      completedOperations,
      totalExecutions,
      successfulExecutions,
      successRate: totalExecutions > 0 ? (successfulExecutions / totalExecutions * 100) : 0,
    };
  }
}

/**
 * Main distributed orchestration example
 */
async function distributedOrchestrationExample() {
  console.log('=== Distributed Orchestration Example ===\n');
  console.log('This example demonstrates a 3-node Raft cluster with distributed operations\n');

  // =========================================================================
  // Initialize cluster and coordinator
  // =========================================================================

  const nodeIds = ['node-1', 'node-2', 'node-3'];
  const cluster = new RaftCluster(nodeIds);
  const coordinator = new DistributedDaemonCoordinator(cluster);

  // Setup event listeners
  cluster.on('leader:elected', (event) => {
    console.log(`  🎯 Leader elected: ${event.leader} (term ${event.term})`);
  });

  cluster.on('entry:replicated', (event) => {
    console.log(`  ✓ Entry replicated: ${event.entryId} (to ${event.replicatedTo} nodes)`);
  });

  cluster.on('node:failed', (event) => {
    console.log(`  ⚠️  Node failed: ${event.nodeId}`);
  });

  cluster.on('node:recovered', (event) => {
    console.log(`  ✓ Node recovered: ${event.nodeId}`);
  });

  coordinator.on('operation:scheduled', (event) => {
    console.log(`  📋 Operation scheduled: ${event.operationId}`);
  });

  // =========================================================================
  // Phase 1: Cluster initialization
  // =========================================================================

  console.log('📡 Cluster Initialization:\n');

  // Note: In this example, we're demonstrating the pattern without creating Daemon instances
  // since the focus is on the distributed cluster coordination mechanism.

  cluster.electLeader();
  const status1 = cluster.getStatus();
  console.log(`  Cluster Status:`);
  console.log(`    Leader: ${status1.leader}`);
  console.log(`    Healthy Nodes: ${status1.healthyNodes}/${status1.totalNodes}`);
  console.log(`    Term: ${status1.term}\n`);

  // =========================================================================
  // Phase 2: Schedule distributed operations
  // =========================================================================

  console.log('▶️  Scheduling Distributed Operations:\n');

  // Operation 1: Data processing job
  await coordinator.scheduleDistributedOperation({
    operationId: 'job-data-process',
    operationName: 'Distributed Data Processing',
    targetNodes: ['node-1', 'node-2', 'node-3'],
    handler: async (nodeId) => {
      // Simulate data processing on node
      await new Promise(resolve => setTimeout(resolve, 100));
      return {
        nodeId,
        processedRecords: Math.floor(Math.random() * 10000) + 1000,
        duration: 100,
      };
    },
  });

  // Operation 2: Backup job
  await coordinator.scheduleDistributedOperation({
    operationId: 'job-backup',
    operationName: 'Distributed Backup',
    targetNodes: ['node-2', 'node-3'],
    handler: async (nodeId) => {
      // Simulate backup on node
      await new Promise(resolve => setTimeout(resolve, 150));
      return {
        nodeId,
        backupSize: '2.5GB',
        timestamp: new Date().toISOString(),
      };
    },
  });

  // Operation 3: Cache warm-up
  await coordinator.scheduleDistributedOperation({
    operationId: 'job-cache-warmup',
    operationName: 'Cache Warm-up',
    targetNodes: nodeIds,
    handler: async (nodeId) => {
      // Simulate cache warm-up
      await new Promise(resolve => setTimeout(resolve, 80));
      return {
        nodeId,
        itemsCached: Math.floor(Math.random() * 50000) + 10000,
        cacheSize: '512MB',
      };
    },
  });

  // =========================================================================
  // Phase 3: Execute operations
  // =========================================================================

  console.log('\n▶️  Executing Distributed Operations:\n');

  console.log('[Op 1] Data Processing:');
  const result1 = await coordinator.executeDistributed('job-data-process');
  console.log(`    Completed: ${result1.completedCount}/${result1.completedCount + result1.failedCount}`);

  console.log('\n[Op 2] Backup:');
  const result2 = await coordinator.executeDistributed('job-backup');
  console.log(`    Completed: ${result2.completedCount}/${result2.completedCount + result2.failedCount}`);

  console.log('\n[Op 3] Cache Warm-up:');
  const result3 = await coordinator.executeDistributed('job-cache-warmup');
  console.log(`    Completed: ${result3.completedCount}/${result3.completedCount + result3.failedCount}`);

  // =========================================================================
  // Phase 4: Simulate failure and recovery
  // =========================================================================

  console.log('\n\n⚠️  Simulating Node Failure:\n');

  // Fail node-2
  cluster.failNode('node-2');
  console.log('');

  // Show cluster status
  const status2 = cluster.getStatus();
  console.log(`  Updated Cluster Status:`);
  console.log(`    Leader: ${status2.leader}`);
  console.log(`    Healthy Nodes: ${status2.healthyNodes}/${status2.totalNodes}`);
  console.log(`    Status: ${status2.healthyNodes > status2.totalNodes / 2 ? 'OPERATIONAL' : 'DEGRADED'}\n`);

  // Try to execute operation on degraded cluster
  console.log('  Attempting operation on degraded cluster:');
  await coordinator.scheduleDistributedOperation({
    operationId: 'job-recovery-test',
    operationName: 'Recovery Test Operation',
    targetNodes: nodeIds,
    handler: async (nodeId) => {
      await new Promise(resolve => setTimeout(resolve, 50));
      return { nodeId, status: 'ok' };
    },
  });

  const result4 = await coordinator.executeDistributed('job-recovery-test');
  console.log(`    Completed: ${result4.completedCount} (node-2 was skipped)\n`);

  // =========================================================================
  // Phase 5: Recovery
  // =========================================================================

  console.log('🔄 Node Recovery:\n');

  cluster.recoverNode('node-2');
  console.log('');

  const status3 = cluster.getStatus();
  console.log(`  Cluster Status After Recovery:`);
  console.log(`    Leader: ${status3.leader}`);
  console.log(`    Healthy Nodes: ${status3.healthyNodes}/${status3.totalNodes}`);
  console.log(`    Status: OPERATIONAL\n`);

  // Execute operation on recovered cluster
  console.log('  Operating on fully recovered cluster:');
  await coordinator.scheduleDistributedOperation({
    operationId: 'job-final-sync',
    operationName: 'Final Synchronization',
    targetNodes: nodeIds,
    handler: async (nodeId) => {
      await new Promise(resolve => setTimeout(resolve, 75));
      return { nodeId, syncStatus: 'complete' };
    },
  });

  const result5 = await coordinator.executeDistributed('job-final-sync');
  console.log(`    Completed: ${result5.completedCount}/${result5.completedCount + result5.failedCount}\n`);

  // =========================================================================
  // Phase 6: Display metrics
  // =========================================================================

  console.log('\n📊 Cluster-Wide Metrics:\n');

  const metrics = coordinator.getClusterMetrics();
  console.log(`  Total Operations Scheduled: ${metrics.totalOperations}`);
  console.log(`  Operations Completed: ${metrics.completedOperations}`);
  console.log(`  Total Executions: ${metrics.totalExecutions}`);
  console.log(`  Successful Executions: ${metrics.successfulExecutions}`);
  console.log(`  Success Rate: ${metrics.successRate.toFixed(1)}%`);

  // =========================================================================
  // Phase 7: Display cluster log
  // =========================================================================

  console.log('\n\n📝 Cluster Log (Leader):\n');

  const leaderNode = cluster.nodes.get(cluster.currentLeader);
  const logEntries = leaderNode.log.slice(-5); // Last 5 entries

  logEntries.forEach((entry, idx) => {
    console.log(`  [${idx + 1}] ${entry.type} (term ${entry.term})`);
    if (entry.operation) {
      console.log(`      Operation: ${entry.operation.operationName}`);
    }
  });

  console.log('\n✅ Distributed orchestration example completed!');
}

// Run the example
await distributedOrchestrationExample();
