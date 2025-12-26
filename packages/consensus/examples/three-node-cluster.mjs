/**
 * @fileoverview Three-Node Cluster Demo
 * @module consensus/examples/three-node-cluster
 *
 * @description
 * Demonstrates a 3-node Raft cluster with leader election and state replication.
 * Shows how nodes coordinate and maintain consensus.
 *
 * Usage:
 *   node examples/three-node-cluster.mjs
 */

import { createRaftCoordinator } from '../src/raft/raft-coordinator.mjs';
import { createClusterManager } from '../src/membership/cluster-manager.mjs';
import { createDistributedStateMachine } from '../src/state/distributed-state-machine.mjs';

/**
 * Node configuration
 */
const NODES = [
  { nodeId: 'node-1', port: 8080, host: 'localhost' },
  { nodeId: 'node-2', port: 8081, host: 'localhost' },
  { nodeId: 'node-3', port: 8082, host: 'localhost' },
];

/**
 * Create and initialize a cluster node
 * @param {Object} nodeConfig - Node configuration
 * @param {Array<Object>} peers - Peer configurations
 * @returns {Promise<Object>} Node components
 */
async function createNode(nodeConfig, peers) {
  console.log(`\n[${nodeConfig.nodeId}] Initializing...`);

  // Create Raft coordinator
  const coordinator = createRaftCoordinator(nodeConfig);

  // Create cluster manager
  const cluster = createClusterManager({
    nodeId: nodeConfig.nodeId,
    healthCheckInterval: 3000,
  });

  // Create state machine
  const state = createDistributedStateMachine({
    nodeId: nodeConfig.nodeId,
    enableSnapshots: true,
    snapshotInterval: 10,
  });

  // Initialize components
  await coordinator.initialize();
  await cluster.initialize(coordinator);
  await state.initialize(coordinator);

  // Set up event listeners
  coordinator.on('leader_elected', ({ leaderId }) => {
    console.log(`[${nodeConfig.nodeId}] 🎖️  Leader elected: ${leaderId}`);
  });

  coordinator.on('became_follower', () => {
    console.log(`[${nodeConfig.nodeId}] 👥 Became follower`);
  });

  coordinator.on('command_applied', ({ command }) => {
    console.log(`[${nodeConfig.nodeId}] ✅ Command applied: ${command.type}`);
  });

  cluster.on('node_joined', ({ nodeId }) => {
    console.log(`[${nodeConfig.nodeId}] ➕ Node joined: ${nodeId}`);
  });

  cluster.on('node_health_changed', ({ nodeId, health }) => {
    console.log(`[${nodeConfig.nodeId}] 🏥 Health changed: ${nodeId} -> ${health}`);
  });

  state.on('state_changed', ({ key, operation }) => {
    console.log(`[${nodeConfig.nodeId}] 💾 State ${operation}: ${key}`);
  });

  state.on('snapshot_created', ({ snapshotId, stateSize }) => {
    console.log(`[${nodeConfig.nodeId}] 📸 Snapshot created: ${snapshotId} (size: ${stateSize})`);
  });

  // Add peers
  for (const peer of peers) {
    coordinator.addPeer(peer.nodeId, peer.host, peer.port);
    await cluster.addNode(peer);
  }

  console.log(`[${nodeConfig.nodeId}] ✓ Initialized with ${peers.length} peers`);

  return { coordinator, cluster, state };
}

/**
 * Run workflow operations on the leader
 * @param {Array<Object>} nodes - Array of node components
 */
async function runWorkflowOperations(nodes) {
  console.log('\n\n=== Starting Workflow Operations ===\n');

  // Wait for leader election
  await new Promise(resolve => setTimeout(resolve, 1000));

  // Find the leader
  const leaderNode = nodes.find(n => n.coordinator.isLeader);

  if (!leaderNode) {
    console.log('⚠️  No leader elected yet, waiting...');
    await new Promise(resolve => setTimeout(resolve, 2000));
    return runWorkflowOperations(nodes);
  }

  const leaderNodeId = leaderNode.coordinator.config.nodeId;
  console.log(`\n[Leader: ${leaderNodeId}] Starting workflow operations...\n`);

  // Operation 1: Start workflow
  await leaderNode.state.set('workflow:data-pipeline', {
    name: 'Data Processing Pipeline',
    status: 'running',
    progress: 0,
  });
  console.log(`[${leaderNodeId}] Started workflow: data-pipeline`);
  await new Promise(resolve => setTimeout(resolve, 200));

  // Operation 2: Update workflow progress
  await leaderNode.state.update('workflow:data-pipeline', prev => ({
    ...prev,
    progress: 0.5,
  }));
  console.log(`[${leaderNodeId}] Updated workflow progress: 50%`);
  await new Promise(resolve => setTimeout(resolve, 200));

  // Operation 3: Start another workflow
  await leaderNode.state.set('workflow:ml-training', {
    name: 'ML Model Training',
    status: 'running',
    progress: 0.25,
  });
  console.log(`[${leaderNodeId}] Started workflow: ml-training`);
  await new Promise(resolve => setTimeout(resolve, 200));

  // Operation 4: Batch update
  await leaderNode.state.batchUpdate([
    { key: 'workflow:data-pipeline', value: { status: 'completed', progress: 1 } },
    { key: 'workflow:ml-training', value: { status: 'running', progress: 0.75 } },
  ]);
  console.log(`[${leaderNodeId}] Batch updated workflows`);
  await new Promise(resolve => setTimeout(resolve, 200));

  // Verify state replication across all nodes
  console.log('\n\n=== Verifying State Replication ===\n');

  for (const node of nodes) {
    const nodeId = node.coordinator.config.nodeId;
    const pipeline = node.state.get('workflow:data-pipeline');
    const mlTraining = node.state.get('workflow:ml-training');

    console.log(`[${nodeId}] State verification:`);
    console.log(`  - data-pipeline: ${pipeline ? pipeline.status : 'NOT FOUND'}`);
    console.log(`  - ml-training: ${mlTraining ? mlTraining.status : 'NOT FOUND'}`);
  }

  // Print statistics
  console.log('\n\n=== Cluster Statistics ===\n');

  for (const node of nodes) {
    const nodeId = node.coordinator.config.nodeId;
    const raftState = node.coordinator.getState();
    const clusterStats = node.cluster.getStats();
    const stateStats = node.state.getStats();

    console.log(`[${nodeId}]:`);
    console.log(`  Raft: ${raftState.isLeader ? 'LEADER' : 'FOLLOWER'}, term: ${raftState.term}, log: ${raftState.logLength}`);
    console.log(`  Cluster: ${clusterStats.totalNodes} nodes, ${clusterStats.healthStats.healthy || 0} healthy`);
    console.log(`  State: ${stateStats.stateSize} entries, ${stateStats.snapshotCount} snapshots`);
  }
}

/**
 * Main demo function
 */
async function main() {
  console.log('========================================');
  console.log('  Three-Node Raft Cluster Demo');
  console.log('========================================');

  const nodes = [];

  // Create all nodes
  for (const nodeConfig of NODES) {
    const peers = NODES.filter(n => n.nodeId !== nodeConfig.nodeId);
    const node = await createNode(nodeConfig, peers);
    nodes.push(node);

    // Small delay between node startups
    await new Promise(resolve => setTimeout(resolve, 500));
  }

  console.log('\n✓ All nodes initialized\n');

  // Run workflow operations
  await runWorkflowOperations(nodes);

  // Keep running for observation
  console.log('\n\n=== Cluster Running (press Ctrl+C to stop) ===\n');

  // Graceful shutdown on SIGINT
  process.on('SIGINT', async () => {
    console.log('\n\nShutting down cluster...\n');

    for (const node of nodes) {
      await node.state.shutdown();
      await node.cluster.shutdown();
      await node.coordinator.shutdown();
    }

    console.log('✓ Cluster shutdown complete\n');
    process.exit(0);
  });
}

// Run the demo
main().catch(error => {
  console.error('Demo failed:', error);
  process.exit(1);
});
