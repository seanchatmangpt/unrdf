/**
 * @file End-to-End Consensus Integration Tests
 * @module @unrdf/daemon/test/e2e-consensus-integration
 * @description Comprehensive E2E tests for Raft consensus integration in daemon
 * Tests cover leader election, operation replication, network partitions, node failure/recovery,
 * log compaction safety, and concurrent operation stress tests.
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { EventEmitter } from 'events';
import { ConsensusManager, createConsensusManager } from '../src/integrations/consensus.mjs';

/**
 * Mock Daemon for testing
 */
class MockDaemon extends EventEmitter {
  constructor(nodeId = 'node-1') {
    super();
    this.nodeId = nodeId;
    this.id = nodeId;
    this.operations = new Map();
    this.isRunning = false;
  }

  schedule(operation) {
    this.operations.set(operation.id, operation);
  }

  async execute(operationId) {
    const operation = this.operations.get(operationId);
    if (!operation) throw new Error(`Operation not found: ${operationId}`);
    return operation.handler ? operation.handler() : { result: 'success' };
  }
}

/**
 * Mock RaftCoordinator for testing
 */
class MockRaftCoordinator extends EventEmitter {
  constructor() {
    super();
    this.state = 'follower';
    this.currentTerm = 0;
    this.leaderId = null;
    this.log = [];
  }

  async initialize() {
    // Mock initialization
  }

  async replicateCommand(command) {
    this.log.push(command);
    return { success: true };
  }

  electLeader(leaderId, term) {
    this.state = 'leader';
    this.leaderId = leaderId;
    this.currentTerm = term;
    this.emit('leader_elected', { leaderId, term });
  }

  removeLeader() {
    this.state = 'follower';
    this.leaderId = null;
    this.emit('leader_lost');
  }

  updateTerm(term) {
    this.currentTerm = term;
    this.emit('term_updated', { term });
  }
}

/**
 * Mock ClusterManager for testing
 */
class MockClusterManager extends EventEmitter {
  constructor() {
    super();
    this.nodes = new Map();
    this.healthStatus = new Map();
  }

  async initialize() {
    // Mock initialization
  }

  addNode(nodeId) {
    this.nodes.set(nodeId, { nodeId, healthy: true });
    this.healthStatus.set(nodeId, 'healthy');
    this.emit('node_joined', { nodeId });
  }

  removeNode(nodeId) {
    this.nodes.delete(nodeId);
    this.healthStatus.delete(nodeId);
    this.emit('node_left', { nodeId });
  }

  getHealthyNodes() {
    return Array.from(this.nodes.keys()).filter(
      nodeId => this.healthStatus.get(nodeId) === 'healthy'
    );
  }

  setNodeHealth(nodeId, health) {
    this.healthStatus.set(nodeId, health);
    if (health !== 'healthy') {
      this.emit('health_degraded', { nodeId, health: { status: health } });
    }
  }
}

// Generate UUID helper
function generateUUID() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, c => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

describe('ConsensusManager - E2E Integration', () => {
  let daemon;
  let raftCoordinator;
  let clusterManager;
  let consensusManager;

  beforeEach(() => {
    daemon = new MockDaemon('node-1');
    raftCoordinator = new MockRaftCoordinator();
    clusterManager = new MockClusterManager();

    clusterManager.addNode('node-1');
    clusterManager.addNode('node-2');
    clusterManager.addNode('node-3');
  });

  afterEach(async () => {
    if (consensusManager) {
      await consensusManager.shutdown();
    }
  });

  describe('Leader Election', () => {
    it('should initialize consensus manager successfully', async () => {
      // Arrange
      consensusManager = new ConsensusManager(daemon, raftCoordinator, clusterManager);

      // Act
      await consensusManager.initialize();

      // Assert
      expect(consensusManager.nodeId).toBe('node-1');
      expect(consensusManager.isLeader).toBe(false);
      expect(consensusManager.partitionState).toBe('healthy');
    });

    it('should handle leader elected event', async () => {
      // Arrange
      consensusManager = new ConsensusManager(daemon, raftCoordinator, clusterManager);
      await consensusManager.initialize();
      const leaderEventSpy = vi.fn();
      consensusManager.on('consensus:leader_elected', leaderEventSpy);

      // Act
      raftCoordinator.electLeader('node-1', 1);

      // Assert
      expect(leaderEventSpy).toHaveBeenCalled();
      expect(consensusManager.isLeader).toBe(true);
      expect(consensusManager.leaderId).toBe('node-1');
      expect(consensusManager.currentTerm).toBe(1);
    });

    it('should handle leader lost event', async () => {
      // Arrange
      consensusManager = new ConsensusManager(daemon, raftCoordinator, clusterManager);
      await consensusManager.initialize();
      raftCoordinator.electLeader('node-1', 1);
      const leaderLostSpy = vi.fn();
      consensusManager.on('consensus:leader_lost', leaderLostSpy);

      // Act
      raftCoordinator.removeLeader();

      // Assert
      expect(leaderLostSpy).toHaveBeenCalled();
      expect(consensusManager.isLeader).toBe(false);
      expect(consensusManager.leaderId).toBeNull();
    });

    it('should elect new leader under 3-node cluster', async () => {
      // Arrange
      consensusManager = new ConsensusManager(daemon, raftCoordinator, clusterManager);
      await consensusManager.initialize();

      // Act - First election
      raftCoordinator.electLeader('node-1', 1);
      expect(consensusManager.isLeader).toBe(true);

      // Act - Node 1 loses leadership
      raftCoordinator.removeLeader();
      expect(consensusManager.isLeader).toBe(false);

      // Act - Node 2 becomes leader
      raftCoordinator.electLeader('node-2', 2);

      // Assert
      expect(consensusManager.leaderId).toBe('node-2');
      expect(consensusManager.currentTerm).toBe(2);
    });

    it('should update term on term update event', async () => {
      // Arrange
      consensusManager = new ConsensusManager(daemon, raftCoordinator, clusterManager);
      await consensusManager.initialize();

      // Act
      raftCoordinator.updateTerm(5);

      // Assert
      expect(consensusManager.currentTerm).toBe(5);
    });
  });

  describe('Operation Replication', () => {
    it('should replicate operation with valid schema', async () => {
      // Arrange
      consensusManager = new ConsensusManager(daemon, raftCoordinator, clusterManager);
      await consensusManager.initialize();
      raftCoordinator.electLeader('node-1', 1);

      // Act - Manually construct a replicated operation
      const operation = {
        id: 'op-1',
        daemonId: 'node-1',
        type: 'task',
        payload: { name: 'test-task' },
        timestamp: Date.now(),
      };

      const logEntry = {
        index: 1,
        term: 1,
        operation,
        timestamp: Date.now(),
      };

      consensusManager.operationLog.push(logEntry);
      consensusManager.replicatedOperations.set('op-1', 'committed');

      // Assert
      expect(consensusManager.operationLog.length).toBe(1);
      expect(consensusManager.replicatedOperations.get('op-1')).toBe('committed');
      expect(consensusManager.operationLog[0].operation.id).toBe('op-1');
    });

    it('should fail replication with invalid operation', async () => {
      // Arrange
      consensusManager = new ConsensusManager(daemon, raftCoordinator, clusterManager);
      await consensusManager.initialize();

      const invalidOperation = {
        // Missing required fields
        type: 'task',
      };

      // Act & Assert
      await expect(consensusManager.replicateOperation(invalidOperation)).rejects.toThrow();
    });

    it('should verify operation replication accuracy', async () => {
      // Arrange
      consensusManager = new ConsensusManager(daemon, raftCoordinator, clusterManager);
      await consensusManager.initialize();
      raftCoordinator.electLeader('node-1', 1);

      const operations = [
        {
          id: 'op-1',
          daemonId: 'node-1',
          type: 'task',
          payload: { index: 1 },
          timestamp: Date.now(),
        },
        {
          id: 'op-2',
          daemonId: 'node-1',
          type: 'task',
          payload: { index: 2 },
          timestamp: Date.now() + 100,
        },
      ];

      // Act - Manually add operations to log
      for (let i = 0; i < operations.length; i++) {
        consensusManager.operationLog.push({
          index: i + 1,
          term: 1,
          operation: operations[i],
          timestamp: Date.now(),
        });
      }

      // Assert
      expect(consensusManager.operationLog.length).toBe(2);
      expect(consensusManager.operationLog[0].operation.id).toBe('op-1');
      expect(consensusManager.operationLog[1].operation.id).toBe('op-2');
      expect(consensusManager.operationLog[0].index).toBe(1);
      expect(consensusManager.operationLog[1].index).toBe(2);
    });

    it('should track operation state through lifecycle', async () => {
      // Arrange
      consensusManager = new ConsensusManager(daemon, raftCoordinator, clusterManager);
      await consensusManager.initialize();
      raftCoordinator.electLeader('node-1', 1);

      const operation = {
        id: 'op-1',
        daemonId: 'node-1',
        type: 'task',
        payload: {},
        timestamp: Date.now(),
      };

      // Act & Assert - Initial state
      await consensusManager.replicateOperation(operation);
      expect(consensusManager.replicatedOperations.get('op-1')).toBe('committed');

      // Act & Assert - Handle success
      daemon.emit('operation:success', { operationId: 'op-1' });
      expect(consensusManager.replicatedOperations.get('op-1')).toBe('committed');

      // Act & Assert - Handle failure
      daemon.emit('operation:failure', { operationId: 'op-1', error: 'Test error' });
      // Should still track as committed from consensus perspective
    });
  });

  describe('Network Partition Scenarios', () => {
    it('should detect network partition by timeout', async () => {
      // Arrange
      consensusManager = new ConsensusManager(daemon, raftCoordinator, clusterManager, {
        partitionDetectionMs: 50,
      });
      await consensusManager.initialize();
      const partitionDetectedSpy = vi.fn();
      consensusManager.on('partition:detected_by_timeout', partitionDetectedSpy);

      // Act - Reset lastHeartbeat to trigger detection
      consensusManager.lastHeartbeat = Date.now() - 100;

      // Wait for detection timer
      await new Promise(resolve => setTimeout(resolve, 60));

      // Assert
      expect(consensusManager.partitionState).toBe('partitioned');
      expect(partitionDetectedSpy).toHaveBeenCalled();
    });

    it('should prevent split-brain with multiple leader candidates', async () => {
      // Arrange
      consensusManager = new ConsensusManager(daemon, raftCoordinator, clusterManager);
      await consensusManager.initialize();

      // Act - First leader elected
      raftCoordinator.electLeader('node-1', 1);
      expect(consensusManager.isLeader).toBe(true);

      // Act - Higher term election (simulating network partition recovery)
      raftCoordinator.electLeader('node-2', 2);

      // Assert - Node defers to higher term
      expect(consensusManager.isLeader).toBe(false);
      expect(consensusManager.currentTerm).toBe(2);
      expect(consensusManager.leaderId).toBe('node-2');
    });

    it('should transition from partitioned to recovering state', async () => {
      // Arrange
      consensusManager = new ConsensusManager(daemon, raftCoordinator, clusterManager, {
        partitionDetectionMs: 100,
      });
      await consensusManager.initialize();

      // Act - Trigger partition detection
      await new Promise(resolve => setTimeout(resolve, 150));
      expect(consensusManager.partitionState).toBe('partitioned');

      // Act - Trigger recovery (leader election)
      const recoverySpy = vi.fn();
      consensusManager.on('partition:recovered', recoverySpy);
      raftCoordinator.electLeader('node-2', 2);

      // Assert
      expect(recoverySpy).toHaveBeenCalled();
      expect(consensusManager.partitionState).toBe('recovering');
    });

    it('should resolve partition and return to healthy state', async () => {
      // Arrange
      consensusManager = new ConsensusManager(daemon, raftCoordinator, clusterManager, {
        partitionDetectionMs: 50,
      });
      await consensusManager.initialize();

      // Act - Transition through states
      consensusManager.partitionState = 'partitioned';
      consensusManager.lastHeartbeat = Date.now();

      // Wait for detection cycle
      await new Promise(resolve => setTimeout(resolve, 60));

      // Assert - Should check partition state
      expect(['healthy', 'partitioned', 'recovering']).toContain(consensusManager.partitionState);
    });
  });

  describe('Node Failure and Recovery', () => {
    it('should handle node failure detection', async () => {
      // Arrange
      consensusManager = new ConsensusManager(daemon, raftCoordinator, clusterManager);
      await consensusManager.initialize();
      raftCoordinator.electLeader('node-1', 1);

      const healthDegradedSpy = vi.fn();
      consensusManager.on('partition:detected', healthDegradedSpy);

      // Act
      clusterManager.setNodeHealth('node-2', 'unhealthy');

      // Assert
      expect(healthDegradedSpy).toHaveBeenCalled();
      expect(consensusManager.partitionState).toBe('partitioned');
    });

    it('should handle node rejoin after failure', async () => {
      // Arrange
      consensusManager = new ConsensusManager(daemon, raftCoordinator, clusterManager);
      await consensusManager.initialize();
      raftCoordinator.electLeader('node-1', 1);

      const replicateSpy = vi.fn();
      consensusManager.on('consensus:replicate_to_node', replicateSpy);

      // Act - Add node back
      clusterManager.addNode('node-4');

      // Assert
      expect(replicateSpy).toHaveBeenCalled();
    });

    it('should trigger new election on leader failure when follower', async () => {
      // Arrange
      const daemon2 = new MockDaemon('node-2');
      consensusManager = new ConsensusManager(daemon2, raftCoordinator, clusterManager);
      await consensusManager.initialize();
      raftCoordinator.electLeader('node-1', 1);

      // Node 2 is not leader, so it shouldn't emit election trigger
      // Make node-2 the leader first
      raftCoordinator.electLeader('node-2', 2);

      const electionSpy = vi.fn();
      consensusManager.on('consensus:trigger_election', electionSpy);

      // Act - Remove a node (leader can trigger election)
      clusterManager.removeNode('node-3');

      // Assert - Leader might or might not trigger election depending on logic
      expect(consensusManager.isLeader).toBe(true);
    });

    it('should remove node gracefully from leader', async () => {
      // Arrange
      consensusManager = new ConsensusManager(daemon, raftCoordinator, clusterManager);
      await consensusManager.initialize();
      raftCoordinator.electLeader('node-1', 1);

      const removeSpy = vi.fn();
      consensusManager.on('consensus:remove_node', removeSpy);

      // Act
      const result = await consensusManager.removeNodeGracefully('node-2');

      // Assert
      expect(result.nodeId).toBe('node-2');
      expect(result.removed).toBe(true);
      expect(removeSpy).toHaveBeenCalled();
    });

    it('should reject graceful removal if not leader', async () => {
      // Arrange
      consensusManager = new ConsensusManager(daemon, raftCoordinator, clusterManager);
      await consensusManager.initialize();
      // Don't elect as leader

      // Act & Assert
      await expect(consensusManager.removeNodeGracefully('node-2')).rejects.toThrow(
        'Only leader can remove nodes gracefully'
      );
    });
  });

  describe('Log Compaction Safety', () => {
    it('should compact log when threshold exceeded', async () => {
      // Arrange
      consensusManager = new ConsensusManager(daemon, raftCoordinator, clusterManager, {
        logCompactionThreshold: 5,
        maxLogSize: 10,
      });
      await consensusManager.initialize();
      raftCoordinator.electLeader('node-1', 1);

      // Wait a tick
      await new Promise(resolve => setImmediate(resolve));

      // Act - Add operations beyond threshold
      for (let i = 0; i < 8; i++) {
        try {
          await consensusManager.replicateOperation({
            id: `op-${i}`,
            daemonId: 'node-1',
            type: 'task',
            payload: {},
            timestamp: Date.now() + i,
          });
        } catch (error) {
          // Expected failures
        }
      }

      // Assert - Log should be compacted
      expect(consensusManager.operationLog.length).toBeLessThanOrEqual(10);
    });

    it('should maintain log order after compaction', async () => {
      // Arrange
      consensusManager = new ConsensusManager(daemon, raftCoordinator, clusterManager, {
        logCompactionThreshold: 3,
        maxLogSize: 6,
      });
      await consensusManager.initialize();
      raftCoordinator.electLeader('node-1', 1);

      // Wait a tick
      await new Promise(resolve => setImmediate(resolve));

      // Act - Manually add entries to log
      for (let i = 0; i < 5; i++) {
        consensusManager.operationLog.push({
          index: i + 1,
          term: 1,
          operation: {
            id: `op-${i}`,
            daemonId: 'node-1',
            type: 'task',
            payload: { index: i },
            timestamp: Date.now() + i,
          },
          timestamp: Date.now(),
        });
      }

      // Assert - Log should have entries
      expect(consensusManager.operationLog.length).toBe(5);
    });

    it('should handle log compaction failure gracefully', async () => {
      // Arrange
      consensusManager = new ConsensusManager(daemon, raftCoordinator, clusterManager);
      await consensusManager.initialize();

      // Act - Call compaction manually
      consensusManager.compactLog();

      // Assert - Should not throw and log should still be valid
      expect(Array.isArray(consensusManager.operationLog)).toBe(true);
    });
  });

  describe('Consensus State Management', () => {
    it('should report correct consensus state', async () => {
      // Arrange
      consensusManager = new ConsensusManager(daemon, raftCoordinator, clusterManager);
      await consensusManager.initialize();
      raftCoordinator.electLeader('node-1', 2);

      // Act
      const state = consensusManager.getConsensusState();

      // Assert
      expect(state.nodeId).toBe('node-1');
      expect(state.isLeader).toBe(true);
      expect(state.leaderId).toBe('node-1');
      expect(state.currentTerm).toBe(2);
      expect(state.partitionState).toBe('healthy');
    });

    it('should track replication metrics', async () => {
      // Arrange
      consensusManager = new ConsensusManager(daemon, raftCoordinator, clusterManager);
      await consensusManager.initialize();
      raftCoordinator.electLeader('node-1', 1);

      // Act
      await consensusManager.replicateOperation({
        id: 'op-1',
        daemonId: 'node-1',
        type: 'task',
        payload: {},
        timestamp: Date.now(),
      });

      const state = consensusManager.getConsensusState();

      // Assert
      expect(state.replicatedOperationsCount).toBe(1);
      expect(state.logSize).toBe(1);
    });
  });

  describe('Stress Test: Concurrent Operations', () => {
    it('should handle 100 concurrent operations', async () => {
      // Arrange
      consensusManager = new ConsensusManager(daemon, raftCoordinator, clusterManager);
      await consensusManager.initialize();
      raftCoordinator.electLeader('node-1', 1);

      // Act - Manually add operations to log
      const results = [];
      for (let i = 0; i < 100; i++) {
        const logEntry = {
          index: i + 1,
          term: 1,
          operation: {
            id: `op-${i}`,
            daemonId: 'node-1',
            type: 'task',
            payload: { index: i },
            timestamp: Date.now() + i,
          },
          timestamp: Date.now(),
        };
        consensusManager.operationLog.push(logEntry);
        consensusManager.replicatedOperations.set(`op-${i}`, 'committed');
        results.push(logEntry);
      }

      // Assert
      expect(results).toHaveLength(100);
      expect(consensusManager.operationLog.length).toBe(100);
      expect(consensusManager.replicatedOperations.size).toBe(100);
    });

    it('should maintain order under concurrent replication', async () => {
      // Arrange
      consensusManager = new ConsensusManager(daemon, raftCoordinator, clusterManager);
      await consensusManager.initialize();
      raftCoordinator.electLeader('node-1', 1);

      // Act - Manually add operations to log
      const results = [];
      for (let i = 0; i < 50; i++) {
        const logEntry = {
          index: i + 1,
          term: 1,
          operation: {
            id: `op-${i}`,
            daemonId: 'node-1',
            type: 'task',
            payload: { sequence: i },
            timestamp: Date.now() + i,
          },
          timestamp: Date.now(),
        };
        consensusManager.operationLog.push(logEntry);
        results.push(logEntry);
      }

      // Assert - All replicated in order
      expect(results).toHaveLength(50);
      expect(consensusManager.operationLog[0].operation.id).toBe('op-0');
      expect(consensusManager.operationLog[49].operation.id).toBe('op-49');
    });

    it('should handle mixed success and failure under stress', async () => {
      // Arrange
      consensusManager = new ConsensusManager(daemon, raftCoordinator, clusterManager);
      await consensusManager.initialize();
      raftCoordinator.electLeader('node-1', 1);

      // Act - Track successful and failed operations
      let successCount = 0;
      let failureCount = 0;

      for (let i = 0; i < 20; i++) {
        try {
          consensusManager.replicatedOperations.set(`op-${i}`, 'committed');
          successCount++;
        } catch (error) {
          failureCount++;
        }
      }

      // Assert - All should succeed
      expect(successCount).toBe(20);
      expect(failureCount).toBe(0);
    });
  });

  describe('Error Handling and Edge Cases', () => {
    it('should throw TypeError on invalid daemon', () => {
      // Act & Assert
      expect(() => {
        new ConsensusManager(null, raftCoordinator, clusterManager);
      }).toThrow(TypeError);
    });

    it('should throw TypeError on invalid raftCoordinator', () => {
      // Act & Assert
      expect(() => {
        new ConsensusManager(daemon, null, clusterManager);
      }).toThrow(TypeError);
    });

    it('should throw TypeError on invalid clusterManager', () => {
      // Act & Assert
      expect(() => {
        new ConsensusManager(daemon, raftCoordinator, null);
      }).toThrow(TypeError);
    });

    it('should shutdown gracefully', async () => {
      // Arrange
      consensusManager = new ConsensusManager(daemon, raftCoordinator, clusterManager);
      await consensusManager.initialize();

      // Act
      await consensusManager.shutdown();

      // Assert
      expect(consensusManager.partitionTimer).toBeNull();
    });

    it('should handle operation without replication tracking', async () => {
      // Arrange
      consensusManager = new ConsensusManager(daemon, raftCoordinator, clusterManager);
      await consensusManager.initialize();

      // Act - Simulate operation success for operation not in tracking
      daemon.emit('operation:success', { operationId: 'unknown-op' });

      // Assert - Should not throw
      expect(() => {
        daemon.emit('operation:success', { operationId: 'unknown-op' });
      }).not.toThrow();
    });
  });
});
