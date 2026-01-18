/**
 * @fileoverview Consensus System Tests
 * @module consensus/test/consensus
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { createRaftCoordinator } from '../src/raft/raft-coordinator.mjs';
import { createClusterManager } from '../src/membership/cluster-manager.mjs';
import { createDistributedStateMachine } from '../src/state/distributed-state-machine.mjs';
import { createWebSocketTransport } from '../src/transport/websocket-transport.mjs';

describe('WebSocket Transport', () => {
  let transport;

  beforeEach(async () => {
    transport = createWebSocketTransport({
      nodeId: 'test-node',
      port: 10080,
    });
    try {
      await transport.start();
    } catch (error) {
      // Port might be in use, skip WebSocket tests
      console.warn('WebSocket transport start failed:', error.message);
    }
  });

  afterEach(async () => {
    if (transport) {
      try {
        await transport.shutdown();
      } catch (error) {
        console.warn('WebSocket transport shutdown error:', error.message);
      }
    }
  }, { timeout: 5000 });

  it('should start transport successfully', () => {
    expect(transport.server).toBeDefined();
    expect(transport.config.nodeId).toBe('test-node');
    expect(transport.config.port).toBe(10080);
  });

  it('should add and track peers', () => {
    transport.addPeer('peer-1', 'localhost', 10081);
    expect(transport.peers.has('peer-1')).toBe(true);

    const peer = transport.peers.get('peer-1');
    expect(peer.host).toBe('localhost');
    expect(peer.port).toBe(10081);
  });

  it('should remove peers', () => {
    transport.addPeer('peer-1', 'localhost', 10081);
    transport.removePeer('peer-1');
    expect(transport.peers.has('peer-1')).toBe(false);
  });
});

describe('Raft Coordinator', () => {
  let coordinator;

  beforeEach(async () => {
    coordinator = createRaftCoordinator({
      nodeId: 'raft-test',
      port: 10090,
    });
    await coordinator.initialize();
  });

  afterEach(async () => {
    if (coordinator) {
      await coordinator.shutdown();
    }
  });

  it('should initialize coordinator', () => {
    expect(coordinator.config.nodeId).toBe('raft-test');
    expect(coordinator.transport).toBeDefined();
    expect(coordinator.raft).toBeDefined();
  });

  it('should add peers', () => {
    coordinator.addPeer('peer-1', 'localhost', 10091);
    const state = coordinator.getState();
    expect(state.peers).toContain('peer-1');
  });

  it('should get coordinator state', () => {
    const state = coordinator.getState();
    expect(state.nodeId).toBe('raft-test');
    expect(state.isLeader).toBeDefined();
    expect(state.term).toBeGreaterThanOrEqual(0);
    expect(state.logLength).toBeGreaterThanOrEqual(0);
  });

  it('should track workflows', () => {
    const workflows = coordinator.getAllWorkflows();
    expect(Array.isArray(workflows)).toBe(true);
  });
});

describe('Cluster Manager', () => {
  let coordinator;
  let cluster;

  beforeEach(async () => {
    coordinator = createRaftCoordinator({
      nodeId: 'cluster-test',
      port: 10100,
    });
    await coordinator.initialize();

    cluster = createClusterManager({
      nodeId: 'cluster-test',
      healthCheckInterval: 10000, // Long interval for tests
    });
    await cluster.initialize(coordinator);
  });

  afterEach(async () => {
    if (cluster) {
      await cluster.shutdown();
    }
    if (coordinator) {
      await coordinator.shutdown();
    }
  });

  it('should initialize cluster manager', () => {
    expect(cluster.config.nodeId).toBe('cluster-test');
    expect(cluster.raftCoordinator).toBe(coordinator);
  });

  it('should get cluster stats', () => {
    const stats = cluster.getStats();
    expect(stats.totalNodes).toBeGreaterThanOrEqual(0);
    expect(stats.healthStats).toBeDefined();
  });

  it('should track nodes', () => {
    const nodes = cluster.getNodes();
    expect(Array.isArray(nodes)).toBe(true);
  });
});

describe('Distributed State Machine', () => {
  let coordinator;
  let state;

  beforeEach(async () => {
    coordinator = createRaftCoordinator({
      nodeId: 'state-test',
      port: 10110,
    });
    await coordinator.initialize();

    // Make this node the leader for testing
    coordinator.isLeader = true;

    state = createDistributedStateMachine({
      nodeId: 'state-test',
      enableSnapshots: false, // Disable for testing
    });
    await state.initialize(coordinator);
  });

  afterEach(async () => {
    if (state) {
      await state.shutdown();
    }
    if (coordinator) {
      await coordinator.shutdown();
    }
  });

  it('should initialize state machine', () => {
    expect(state.config.nodeId).toBe('state-test');
    expect(state.raftCoordinator).toBe(coordinator);
  });

  it('should get state stats', () => {
    const stats = state.getStats();
    expect(stats.stateSize).toBeGreaterThanOrEqual(0);
    expect(stats.changeLogSize).toBeGreaterThanOrEqual(0);
    expect(stats.snapshotCount).toBeGreaterThanOrEqual(0);
  });

  it('should list keys, values, entries', () => {
    const keys = state.keys();
    const values = state.values();
    const entries = state.entries();

    expect(Array.isArray(keys)).toBe(true);
    expect(Array.isArray(values)).toBe(true);
    expect(Array.isArray(entries)).toBe(true);
  });
});

describe('Integration Tests', () => {
  let nodes = [];

  afterEach(async () => {
    // Cleanup all nodes
    for (const node of nodes) {
      if (node.state) await node.state.shutdown();
      if (node.cluster) await node.cluster.shutdown();
      if (node.coordinator) await node.coordinator.shutdown();
    }
    nodes = [];
  });

  it('should create a 2-node cluster', async () => {
    const node1 = await createTestNode('int-node-1', 10120);
    const node2 = await createTestNode('int-node-2', 10121);

    nodes.push(node1, node2);

    node1.coordinator.addPeer('int-node-2', 'localhost', 10121);
    node2.coordinator.addPeer('int-node-1', 'localhost', 10120);

    // Wait a bit for connections
    await new Promise(resolve => setTimeout(resolve, 500));

    expect(node1.coordinator.getState().peers.length).toBeGreaterThan(0);
    expect(node2.coordinator.getState().peers.length).toBeGreaterThan(0);
  });
});

/**
 * Helper to create a test node
 */
async function createTestNode(nodeId, port) {
  const coordinator = createRaftCoordinator({ nodeId, port });
  const cluster = createClusterManager({ nodeId, healthCheckInterval: 60000 });
  const state = createDistributedStateMachine({ nodeId, enableSnapshots: false });

  await coordinator.initialize();
  await cluster.initialize(coordinator);
  await state.initialize(coordinator);

  return { coordinator, cluster, state };
}
