/**
 * @fileoverview Simple Consensus Demo
 * @module consensus/examples/consensus-demo
 *
 * @description
 * Basic demonstration of Raft consensus with 2 nodes.
 * Shows initialization, leader election, and state replication.
 *
 * Usage:
 *   node examples/consensus-demo.mjs
 */

import { createRaftCoordinator, createDistributedStateMachine } from '../src/index.mjs';

async function main() {
  console.log('========================================');
  console.log('  Simple Consensus Demo (2 nodes)');
  console.log('========================================\n');

  // Create Node 1
  console.log('Creating Node 1...');
  const node1 = createRaftCoordinator({
    nodeId: 'node-1',
    port: 7080,
    host: 'localhost',
  });

  const state1 = createDistributedStateMachine({ nodeId: 'node-1' });

  await node1.initialize();
  await state1.initialize(node1);

  // Event handlers for Node 1
  node1.on('leader_elected', ({ leaderId }) => {
    console.log(`[node-1] Leader elected: ${leaderId}`);
  });

  state1.on('state_changed', ({ key, value, operation }) => {
    console.log(`[node-1] State ${operation}: ${key} = ${JSON.stringify(value)}`);
  });

  console.log('✓ Node 1 initialized\n');

  // Create Node 2
  console.log('Creating Node 2...');
  const node2 = createRaftCoordinator({
    nodeId: 'node-2',
    port: 7081,
    host: 'localhost',
  });

  const state2 = createDistributedStateMachine({ nodeId: 'node-2' });

  await node2.initialize();
  await state2.initialize(node2);

  // Event handlers for Node 2
  node2.on('leader_elected', ({ leaderId }) => {
    console.log(`[node-2] Leader elected: ${leaderId}`);
  });

  state2.on('state_changed', ({ key, value, operation }) => {
    console.log(`[node-2] State ${operation}: ${key} = ${JSON.stringify(value)}`);
  });

  console.log('✓ Node 2 initialized\n');

  // Connect peers
  console.log('Connecting peers...');
  node1.addPeer('node-2', 'localhost', 7081);
  node2.addPeer('node-1', 'localhost', 7080);
  console.log('✓ Peers connected\n');

  // Wait for leader election
  console.log('Waiting for leader election...\n');
  await new Promise(resolve => setTimeout(resolve, 2000));

  // Find leader
  const leader = node1.isLeader ? { node: node1, state: state1, id: 'node-1' } : { node: node2, state: state2, id: 'node-2' };

  console.log(`\n✓ Leader: ${leader.id}\n`);

  // Perform operations
  console.log('Performing operations on leader...\n');

  await leader.state.set('counter', 0);
  await new Promise(resolve => setTimeout(resolve, 200));

  await leader.state.update('counter', prev => prev + 1);
  await new Promise(resolve => setTimeout(resolve, 200));

  await leader.state.update('counter', prev => prev + 1);
  await new Promise(resolve => setTimeout(resolve, 200));

  // Verify replication
  console.log('\nVerifying replication:\n');
  console.log(`[node-1] counter = ${state1.get('counter')}`);
  console.log(`[node-2] counter = ${state2.get('counter')}`);

  console.log('\n✓ Demo complete\n');

  // Cleanup
  await state1.shutdown();
  await state2.shutdown();
  await node1.shutdown();
  await node2.shutdown();

  process.exit(0);
}

main().catch(error => {
  console.error('Demo failed:', error);
  process.exit(1);
});
