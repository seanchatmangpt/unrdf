/**
 * @fileoverview Failover Test Demo
 * @module consensus/examples/failover-test
 *
 * @description
 * Demonstrates automatic failover when leader node fails.
 * Shows how the cluster elects a new leader and continues operation.
 *
 * Usage:
 *   node examples/failover-test.mjs
 */

import { createRaftCoordinator } from '../src/raft/raft-coordinator.mjs';
import { createClusterManager } from '../src/membership/cluster-manager.mjs';
import { createDistributedStateMachine } from '../src/state/distributed-state-machine.mjs';

/**
 * Node configuration
 */
const NODES = [
  { nodeId: 'node-1', port: 9080, host: 'localhost' },
  { nodeId: 'node-2', port: 9081, host: 'localhost' },
  { nodeId: 'node-3', port: 9082, host: 'localhost' },
];

/**
 * Create a cluster node
 * @param {Object} nodeConfig - Node configuration
 * @param {Array<Object>} peers - Peer configurations
 * @returns {Promise<Object>} Node components
 */
async function createNode(nodeConfig, peers) {
  const coordinator = createRaftCoordinator(nodeConfig);
  const cluster = createClusterManager({ nodeId: nodeConfig.nodeId });
  const state = createDistributedStateMachine({ nodeId: nodeConfig.nodeId });

  await coordinator.initialize();
  await cluster.initialize(coordinator);
  await state.initialize(coordinator);

  // Event logging
  coordinator.on('leader_elected', ({ leaderId }) => {
    console.log(`[${nodeConfig.nodeId}] 🎖️  NEW LEADER: ${leaderId}`);
  });

  coordinator.on('became_follower', () => {
    console.log(`[${nodeConfig.nodeId}] Became follower`);
  });

  cluster.on('node_health_changed', ({ nodeId, health, previousHealth }) => {
    console.log(`[${nodeConfig.nodeId}] Health: ${nodeId} changed ${previousHealth} -> ${health}`);
  });

  // Add peers
  for (const peer of peers) {
    coordinator.addPeer(peer.nodeId, peer.host, peer.port);
    await cluster.addNode(peer);
  }

  return { coordinator, cluster, state, config: nodeConfig };
}

/**
 * Find current leader
 * @param {Array<Object>} nodes - Array of nodes
 * @returns {Object|null} Leader node or null
 */
function findLeader(nodes) {
  return nodes.find(n => n.coordinator && n.coordinator.isLeader) || null;
}

/**
 * Simulate node failure
 * @param {Object} node - Node to fail
 */
async function simulateFailure(node) {
  console.log(`\n💥 SIMULATING FAILURE of ${node.config.nodeId}\n`);

  await node.state.shutdown();
  await node.cluster.shutdown();
  await node.coordinator.shutdown();

  // Mark as failed
  node.coordinator = null;
  node.cluster = null;
  node.state = null;
}

/**
 * Main failover test
 */
async function main() {
  console.log('========================================');
  console.log('  Raft Failover Test');
  console.log('========================================\n');

  // Step 1: Create cluster
  console.log('Step 1: Creating 3-node cluster...\n');

  const nodes = [];
  for (const nodeConfig of NODES) {
    const peers = NODES.filter(n => n.nodeId !== nodeConfig.nodeId);
    const node = await createNode(nodeConfig, peers);
    nodes.push(node);
    await new Promise(resolve => setTimeout(resolve, 300));
  }

  console.log('✓ Cluster created\n');

  // Step 2: Wait for initial leader election
  console.log('Step 2: Waiting for leader election...\n');
  await new Promise(resolve => setTimeout(resolve, 2000));

  let leader = findLeader(nodes);
  if (!leader) {
    console.log('⚠️  No leader elected, waiting longer...');
    await new Promise(resolve => setTimeout(resolve, 2000));
    leader = findLeader(nodes);
  }

  if (!leader) {
    throw new Error('Failed to elect leader');
  }

  console.log(`✓ Initial leader: ${leader.config.nodeId}\n`);

  // Step 3: Perform operations on leader
  console.log('Step 3: Performing operations on leader...\n');

  await leader.state.set('test:key1', { value: 'data1', timestamp: Date.now() });
  console.log(`[${leader.config.nodeId}] Set test:key1`);

  await leader.state.set('test:key2', { value: 'data2', timestamp: Date.now() });
  console.log(`[${leader.config.nodeId}] Set test:key2\n`);

  await new Promise(resolve => setTimeout(resolve, 500));

  // Verify replication
  console.log('Verifying state replication before failure:\n');
  for (const node of nodes) {
    if (!node.coordinator) continue;
    const key1 = node.state.get('test:key1');
    const key2 = node.state.get('test:key2');
    console.log(`[${node.config.nodeId}] key1: ${key1 ? '✓' : '✗'}, key2: ${key2 ? '✓' : '✗'}`);
  }

  // Step 4: Simulate leader failure
  console.log('\n----------------------------------------');
  console.log('Step 4: Simulating leader failure...');
  console.log('----------------------------------------\n');

  const failedLeaderId = leader.config.nodeId;
  await simulateFailure(leader);

  console.log(`✓ ${failedLeaderId} shut down\n`);

  // Step 5: Wait for new leader election
  console.log('Step 5: Waiting for new leader election...\n');
  await new Promise(resolve => setTimeout(resolve, 1500));

  const activeNodes = nodes.filter(n => n.coordinator !== null);
  const newLeader = findLeader(activeNodes);

  if (!newLeader) {
    console.log('⚠️  No new leader yet, waiting longer...');
    await new Promise(resolve => setTimeout(resolve, 2000));
    const retryLeader = findLeader(activeNodes);
    if (retryLeader) {
      console.log(`✓ New leader elected: ${retryLeader.config.nodeId}\n`);
    } else {
      console.log('⚠️  Leader election still in progress\n');
    }
  } else {
    console.log(`✓ New leader elected: ${newLeader.config.nodeId}\n`);
  }

  // Step 6: Verify data persistence
  console.log('Step 6: Verifying data persistence after failover...\n');

  for (const node of activeNodes) {
    const key1 = node.state.get('test:key1');
    const key2 = node.state.get('test:key2');
    console.log(`[${node.config.nodeId}] key1: ${key1 ? '✓ PRESERVED' : '✗ LOST'}, key2: ${key2 ? '✓ PRESERVED' : '✗ LOST'}`);
  }

  // Step 7: Perform new operations on new leader
  if (newLeader) {
    console.log(`\nStep 7: Performing operations on new leader (${newLeader.config.nodeId})...\n`);

    await newLeader.state.set('test:key3', { value: 'data3', timestamp: Date.now() });
    console.log(`[${newLeader.config.nodeId}] Set test:key3\n`);

    await new Promise(resolve => setTimeout(resolve, 500));

    // Verify new operation replicated
    console.log('Verifying new operations replicated:\n');
    for (const node of activeNodes) {
      const key3 = node.state.get('test:key3');
      console.log(`[${node.config.nodeId}] key3: ${key3 ? '✓ REPLICATED' : '✗ NOT REPLICATED'}`);
    }
  }

  // Step 8: Print final statistics
  console.log('\n========================================');
  console.log('  Failover Test Results');
  console.log('========================================\n');

  console.log(`Original Leader: ${failedLeaderId} (FAILED)`);
  console.log(`New Leader: ${newLeader ? newLeader.config.nodeId : 'NONE'}`);
  console.log(`Active Nodes: ${activeNodes.length}/${NODES.length}`);

  console.log('\nNode Statistics:\n');
  for (const node of activeNodes) {
    const raftState = node.coordinator.getState();
    const stateStats = node.state.getStats();
    console.log(`[${node.config.nodeId}]:`);
    console.log(`  Role: ${raftState.isLeader ? 'LEADER' : 'FOLLOWER'}`);
    console.log(`  Term: ${raftState.term}`);
    console.log(`  Log Length: ${raftState.logLength}`);
    console.log(`  State Size: ${stateStats.stateSize} entries`);
  }

  console.log('\n✓ Failover test complete\n');

  // Cleanup
  console.log('Cleaning up...\n');
  for (const node of activeNodes) {
    await node.state.shutdown();
    await node.cluster.shutdown();
    await node.coordinator.shutdown();
  }

  console.log('✓ Cleanup complete\n');
  process.exit(0);
}

// Run the test
main().catch(error => {
  console.error('Failover test failed:', error);
  process.exit(1);
});
