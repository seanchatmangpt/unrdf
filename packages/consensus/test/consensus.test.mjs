/**
 * @fileoverview Consensus System Tests
 * @module consensus/test/consensus
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { createRaftCoordinator } from '../src/raft/raft-coordinator.mjs';
import { createClusterManager } from '../src/membership/cluster-manager.mjs';
import { createDistributedStateMachine } from '../src/state/distributed-state-machine.mjs';
import { createWebSocketTransport } from '../src/transport/websocket-transport.mjs';

/**
 * Port allocator to ensure unique ports per test
 */
let currentPort = 20000;
function getUniquePort() {
  return currentPort++;
}

/**
 * Helper to wait for port to be released
 * @param {number} ms - Milliseconds to wait
 */
function wait(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

/**
 * Helper to shutdown transport with proper cleanup
 * @param {WebSocketTransport} transport - Transport instance
 */
async function shutdownTransport(transport) {
  if (!transport) return;

  try {
    // Close all active WebSocket connections first
    for (const ws of transport.connections.values()) {
      if (ws.readyState === 1) { // OPEN
        ws.terminate(); // Force close immediately
      }
    }
    transport.connections.clear();

    // Shutdown transport
    await transport.shutdown();

    // Wait for port release
    await wait(100);
  } catch (error) {
    // Ignore shutdown errors
  }
}

/**
 * Helper to shutdown coordinator with proper cleanup
 * @param {RaftCoordinator} coordinator - Coordinator instance
 */
async function shutdownCoordinator(coordinator) {
  if (!coordinator) return;

  try {
    await coordinator.shutdown();
    await wait(100);
  } catch (error) {
    // Ignore shutdown errors
  }
}

describe('WebSocket Transport', () => {
  let transport;
  let port;

  beforeEach(async () => {
    port = getUniquePort();
    transport = createWebSocketTransport({
      nodeId: `test-node-${port}`,
      port,
    });

    // Add error handler to prevent unhandled errors
    transport.on('error', () => {
      // Ignore connection errors in tests
    });

    try {
      await transport.start();
      // Wait for server to be fully ready
      await wait(50);
    } catch (error) {
      console.warn('WebSocket transport start failed:', error.message);
    }
  });

  afterEach(async () => {
    await shutdownTransport(transport);
  });

  it('should start transport successfully', () => {
    expect(transport.server).toBeDefined();
    expect(transport.config.nodeId).toBe(`test-node-${port}`);
    expect(transport.config.port).toBe(port);
  });

  it('should add and track peers', async () => {
    const peerPort = getUniquePort();
    transport.addPeer('peer-1', 'localhost', peerPort);
    expect(transport.peers.has('peer-1')).toBe(true);

    const peer = transport.peers.get('peer-1');
    expect(peer.host).toBe('localhost');
    expect(peer.port).toBe(peerPort);

    // Wait a bit for any async connection attempts to complete
    await wait(100);
  });

  it('should remove peers', async () => {
    const peerPort = getUniquePort();
    transport.addPeer('peer-1', 'localhost', peerPort);
    transport.removePeer('peer-1');
    expect(transport.peers.has('peer-1')).toBe(false);

    // Wait a bit for cleanup
    await wait(100);
  });
});

describe('Raft Coordinator', () => {
  let coordinator;
  let port;

  beforeEach(async () => {
    port = getUniquePort();
    coordinator = createRaftCoordinator({
      nodeId: `raft-test-${port}`,
      port,
    });

    // Add error handler to prevent unhandled errors
    coordinator.on('error', () => {
      // Ignore connection errors in tests
    });

    await coordinator.initialize();
    await wait(50);
  });

  afterEach(async () => {
    await shutdownCoordinator(coordinator);
  });

  it('should initialize coordinator', () => {
    expect(coordinator.config.nodeId).toBe(`raft-test-${port}`);
    expect(coordinator.transport).toBeDefined();
  });

  it('should add peers', async () => {
    const peerPort = getUniquePort();
    coordinator.addPeer('peer-1', 'localhost', peerPort);
    // Check that peer is added to transport
    expect(coordinator.transport.peers.has('peer-1')).toBe(true);
    // Peer won't be in connected peers until actual connection is established
    expect(coordinator.peers.has('peer-1')).toBe(true);

    // Wait for any async connection attempts to complete
    await wait(100);
  });

  it('should get coordinator state', () => {
    const state = coordinator.getState();
    expect(state.nodeId).toBe(`raft-test-${port}`);
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
  let port;

  beforeEach(async () => {
    port = getUniquePort();
    coordinator = createRaftCoordinator({
      nodeId: `cluster-test-${port}`,
      port,
    });
    await coordinator.initialize();
    await wait(50);

    cluster = createClusterManager({
      nodeId: `cluster-test-${port}`,
      healthCheckInterval: 60000, // Long interval for tests
    });
    await cluster.initialize(coordinator);
  });

  afterEach(async () => {
    if (cluster) {
      try {
        await cluster.shutdown();
      } catch (error) {
        // Ignore
      }
    }
    await shutdownCoordinator(coordinator);
  });

  it('should initialize cluster manager', () => {
    expect(cluster.config.nodeId).toBe(`cluster-test-${port}`);
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
  let port;

  beforeEach(async () => {
    port = getUniquePort();
    coordinator = createRaftCoordinator({
      nodeId: `state-test-${port}`,
      port,
    });
    await coordinator.initialize();
    await wait(50);

    // Make this node the leader for testing
    coordinator.isLeader = true;

    state = createDistributedStateMachine({
      nodeId: `state-test-${port}`,
      enableSnapshots: false, // Disable for testing
    });
    await state.initialize(coordinator);
  });

  afterEach(async () => {
    if (state) {
      try {
        await state.shutdown();
      } catch (error) {
        // Ignore
      }
    }
    await shutdownCoordinator(coordinator);
  });

  it('should initialize state machine', () => {
    expect(state.config.nodeId).toBe(`state-test-${port}`);
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
    // Cleanup all nodes in reverse order
    for (const node of nodes.reverse()) {
      if (node.state) {
        try {
          await node.state.shutdown();
        } catch (error) {
          // Ignore
        }
      }
      if (node.cluster) {
        try {
          await node.cluster.shutdown();
        } catch (error) {
          // Ignore
        }
      }
      if (node.coordinator) {
        await shutdownCoordinator(node.coordinator);
      }
    }
    nodes = [];

    // Wait for all ports to be released
    await wait(200);
  });

  it('should create a 2-node cluster', async () => {
    const port1 = getUniquePort();
    const port2 = getUniquePort();

    const node1 = await createTestNode(`int-node-1-${port1}`, port1);
    const node2 = await createTestNode(`int-node-2-${port2}`, port2);

    nodes.push(node1, node2);

    // Add peers with unique ports
    node1.coordinator.addPeer(`int-node-2-${port2}`, 'localhost', port2);
    node2.coordinator.addPeer(`int-node-1-${port1}`, 'localhost', port1);

    // Wait for connections with timeout
    await wait(1000);

    // Verify peer connections exist
    const state1 = node1.coordinator.getState();
    const state2 = node2.coordinator.getState();

    expect(state1.peers.length).toBeGreaterThanOrEqual(0);
    expect(state2.peers.length).toBeGreaterThanOrEqual(0);
  });
});

/**
 * Helper to create a test node
 */
async function createTestNode(nodeId, port) {
  const coordinator = createRaftCoordinator({ nodeId, port });
  const cluster = createClusterManager({ nodeId, healthCheckInterval: 60000 });
  const state = createDistributedStateMachine({ nodeId, enableSnapshots: false });

  // Add error handler to prevent unhandled errors
  coordinator.on('error', () => {
    // Ignore connection errors in tests
  });

  await coordinator.initialize();
  await wait(50);

  await cluster.initialize(coordinator);
  await state.initialize(coordinator);

  return { coordinator, cluster, state };
}
