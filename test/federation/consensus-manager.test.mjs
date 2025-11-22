/**
 * @fileoverview Tests for RAFT Consensus Manager
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  _ConsensusManager,
  createConsensusManager,
  NodeState,
} from '../../src/knowledge-engine/federation/consensus-manager.mjs';

describe('ConsensusManager', () => {
  let consensus;

  beforeEach(async () => {
    consensus = createConsensusManager({
      nodeId: 'test-node-1',
      electionTimeoutMin: 50,
      electionTimeoutMax: 100,
      heartbeatInterval: 20,
    });
    await consensus.initialize();
  });

  afterEach(async () => {
    await consensus.shutdown();
  });

  describe('initialization', () => {
    it('should initialize with follower state', () => {
      expect(consensus.state).toBe(NodeState.FOLLOWER);
      expect(consensus.currentTerm).toBe(0);
      expect(consensus.votedFor).toBe(null);
    });

    it('should emit initialized event', async () => {
      const manager = createConsensusManager({ nodeId: 'test-node-2' });
      const eventPromise = new Promise(resolve => {
        manager.on('initialized', data => {
          expect(data.nodeId).toBe('test-node-2');
          resolve();
        });
      });
      await manager.initialize();
      await eventPromise;
    });
  });

  describe('peer management', () => {
    it('should add peers', () => {
      consensus.addPeer('node-2', 'http://node2:8080');
      consensus.addPeer('node-3', 'http://node3:8080');

      expect(consensus.peers.size).toBe(2);
      expect(consensus.peers.has('node-2')).toBe(true);
      expect(consensus.peers.has('node-3')).toBe(true);
    });

    it('should remove peers', () => {
      consensus.addPeer('node-2', 'http://node2:8080');
      consensus.removePeer('node-2');

      expect(consensus.peers.size).toBe(0);
      expect(consensus.peers.has('node-2')).toBe(false);
    });
  });

  describe('leader election', () => {
    it('should transition to candidate state during election', async () => {
      consensus.addPeer('node-2', 'http://node2:8080');
      consensus.addPeer('node-3', 'http://node3:8080');

      await consensus.startElection();

      expect(consensus.currentTerm).toBeGreaterThan(0);
      expect(consensus.stats.electionsStarted).toBeGreaterThan(0);
    });

    it('should emit state change events', async () => {
      const eventPromise = new Promise(resolve => {
        consensus.on('stateChange', state => {
          expect([NodeState.LEADER, NodeState.FOLLOWER]).toContain(state);
          resolve();
        });
      });

      consensus.addPeer('node-2', 'http://node2:8080');
      await consensus.startElection();
      await eventPromise;
    });
  });

  describe('log replication', () => {
    beforeEach(async () => {
      // Make this node the leader
      consensus.state = NodeState.LEADER;
      consensus.leaderId = consensus.config.nodeId;
      consensus.addPeer('node-2', 'http://node2:8080');
      consensus.addPeer('node-3', 'http://node3:8080');
    });

    it('should replicate commands as leader', async () => {
      const command = {
        type: 'ADD_STORE',
        storeId: 'store-1',
        data: { endpoint: 'http://store1:3000' },
      };

      const success = await consensus.replicate(command);

      expect(success).toBe(true);
      expect(consensus.log.length).toBe(1);
      expect(consensus.log[0].command).toEqual(command);
    });

    it('should reject replication when not leader', async () => {
      consensus.state = NodeState.FOLLOWER;

      const command = { type: 'TEST' };

      await expect(consensus.replicate(command)).rejects.toThrow('Only leader can replicate');
    });

    it('should increment log entries replicated stat', async () => {
      const initialCount = consensus.stats.logEntriesReplicated;

      await consensus.replicate({
        type: 'TEST',
        data: {},
      });

      expect(consensus.stats.logEntriesReplicated).toBe(initialCount + 1);
    });

    it('should emit commandApplied event', async () => {
      const _eventPromise = new Promise(resolve => {
        consensus.on('commandApplied', command => {
          expect(command.type).toBe('TEST_COMMAND');
          resolve();
        });
      });

      await consensus.replicate({
        type: 'TEST_COMMAND',
        data: {},
      });

      // Give time for async event
      await new Promise(resolve => setTimeout(resolve, 50));
    });
  });

  describe('heartbeat mechanism', () => {
    it('should send heartbeats as leader', async () => {
      consensus.state = NodeState.LEADER;
      consensus.leaderId = consensus.config.nodeId;
      consensus.addPeer('node-2', 'http://node2:8080');

      // Start heartbeat explicitly
      consensus.startHeartbeat();

      const initialHeartbeats = consensus.stats.heartbeatsSent;

      await new Promise(resolve => setTimeout(resolve, 100));

      expect(consensus.stats.heartbeatsSent).toBeGreaterThan(initialHeartbeats);
    });
  });

  describe('state information', () => {
    it('should return current state', () => {
      const state = consensus.getState();

      expect(state).toMatchObject({
        nodeId: 'test-node-1',
        state: expect.any(String),
        currentTerm: expect.any(Number),
        logLength: expect.any(Number),
        commitIndex: expect.any(Number),
        lastApplied: expect.any(Number),
        peerCount: expect.any(Number),
      });
    });

    it('should include statistics', () => {
      const state = consensus.getState();

      expect(state.stats).toMatchObject({
        electionsStarted: expect.any(Number),
        electionsWon: expect.any(Number),
        heartbeatsSent: expect.any(Number),
        heartbeatsReceived: expect.any(Number),
        logEntriesReplicated: expect.any(Number),
      });
    });
  });

  describe('append entries handling', () => {
    it('should handle append entries request', () => {
      const request = {
        term: 1,
        leaderId: 'node-2',
        prevLogIndex: 0,
        prevLogTerm: 0,
        entries: [],
        leaderCommit: 0,
      };

      const response = consensus.handleAppendEntries(request);

      expect(response.success).toBe(true);
      expect(consensus.stats.heartbeatsReceived).toBeGreaterThan(0);
    });

    it('should update term on higher term', () => {
      const request = {
        term: 5,
        leaderId: 'node-2',
        prevLogIndex: 0,
        prevLogTerm: 0,
        entries: [],
        leaderCommit: 0,
      };

      consensus.handleAppendEntries(request);

      expect(consensus.currentTerm).toBe(5);
      expect(consensus.state).toBe(NodeState.FOLLOWER);
    });
  });

  describe('shutdown', () => {
    it('should clean up timers on shutdown', async () => {
      await consensus.shutdown();

      expect(consensus.electionTimer).toBe(null);
      expect(consensus.heartbeatTimer).toBe(null);
    });

    it('should emit shutdown event', async () => {
      const eventPromise = new Promise(resolve => {
        consensus.on('shutdown', () => {
          resolve();
        });
      });

      await consensus.shutdown();
      await eventPromise;
    });
  });
});
