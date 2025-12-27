/**
 * @file raft.test.mjs
 * @description Tests for Raft consensus implementation
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { RaftNode, NodeState, createRaftNode } from '../../src/consensus/raft.mjs';

describe('Raft Consensus', () => {
  describe('RaftNode', () => {
    /** @type {RaftNode} */
    let node;

    beforeEach(() => {
      node = new RaftNode({
        nodeId: 'node-1',
        peers: ['node-2', 'node-3', 'node-4', 'node-5'],
      });
    });

    it('should initialize in follower state', () => {
      expect(node.state).toBe(NodeState.FOLLOWER);
      expect(node.currentTerm).toBe(0);
      expect(node.votedFor).toBeNull();
      expect(node.log.length).toBe(1); // Sentinel entry
    });

    it('should start and stop correctly', async () => {
      const startedSpy = vi.fn();
      const stoppedSpy = vi.fn();

      node.on('started', startedSpy);
      node.on('stopped', stoppedSpy);

      await node.start();
      expect(node.running).toBe(true);
      expect(startedSpy).toHaveBeenCalledWith('node-1');

      await node.stop();
      expect(node.running).toBe(false);
      expect(stoppedSpy).toHaveBeenCalledWith('node-1');
    });

    it('should transition to candidate on election timeout', async () => {
      const stateChangeSpy = vi.fn();
      node.on('stateChange', stateChangeSpy);

      node.config.electionTimeoutMin = 10;
      node.config.electionTimeoutMax = 20;

      await node.start();

      // Wait for election timeout
      await new Promise((resolve) => setTimeout(resolve, 50));

      expect(stateChangeSpy).toHaveBeenCalledWith(NodeState.CANDIDATE, 1);
      expect(node.currentTerm).toBe(1);
      expect(node.votedFor).toBe('node-1');

      await node.stop();
    });

    it('should become leader with majority votes', async () => {
      const stateChangeSpy = vi.fn();
      node.on('stateChange', stateChangeSpy);

      // Mock RPC handler to grant votes
      node.setRPCHandler(async (peer, rpc, args) => {
        if (rpc === 'RequestVote') {
          return { term: args.term, voteGranted: true };
        }
        return { term: args.term, success: true };
      });

      await node.start();
      node.becomeCandidate();

      // Wait for election to complete
      await new Promise((resolve) => setTimeout(resolve, 50));

      expect(node.state).toBe(NodeState.LEADER);
      expect(stateChangeSpy).toHaveBeenCalledWith(NodeState.LEADER, 1);

      await node.stop();
    });

    it('should handle RequestVote RPC correctly', () => {
      node.currentTerm = 5;

      // Request from older term - deny
      const reply1 = node.handleRequestVote({
        term: 3,
        candidateId: 'node-2',
        lastLogIndex: 0,
        lastLogTerm: 0,
      });

      expect(reply1.term).toBe(5);
      expect(reply1.voteGranted).toBe(false);

      // Request from newer term - grant if log up-to-date
      const reply2 = node.handleRequestVote({
        term: 6,
        candidateId: 'node-2',
        lastLogIndex: 0,
        lastLogTerm: 0,
      });

      expect(reply2.term).toBe(6);
      expect(reply2.voteGranted).toBe(true);
      expect(node.currentTerm).toBe(6);
      expect(node.votedFor).toBe('node-2');
    });

    it('should handle AppendEntries RPC correctly', () => {
      node.currentTerm = 5;
      node.log = [
        { term: 0, index: 0, command: null, timestamp: Date.now() },
        { term: 1, index: 1, command: 'cmd1', timestamp: Date.now() },
        { term: 2, index: 2, command: 'cmd2', timestamp: Date.now() },
      ];

      // Heartbeat from older term - reject
      const reply1 = node.handleAppendEntries({
        term: 3,
        leaderId: 'node-2',
        prevLogIndex: 0,
        prevLogTerm: 0,
        entries: [],
        leaderCommit: 0,
      });

      expect(reply1.term).toBe(5);
      expect(reply1.success).toBe(false);

      // Valid heartbeat from leader
      const reply2 = node.handleAppendEntries({
        term: 5,
        leaderId: 'node-2',
        prevLogIndex: 2,
        prevLogTerm: 2,
        entries: [],
        leaderCommit: 0,
      });

      expect(reply2.term).toBe(5);
      expect(reply2.success).toBe(true);
      expect(node.leaderId).toBe('node-2');
    });

    it('should replicate log entries', () => {
      node.currentTerm = 5;
      node.log = [{ term: 0, index: 0, command: null, timestamp: Date.now() }];

      const newEntry = {
        term: 5,
        index: 1,
        command: 'new-cmd',
        timestamp: Date.now(),
      };

      const reply = node.handleAppendEntries({
        term: 5,
        leaderId: 'node-2',
        prevLogIndex: 0,
        prevLogTerm: 0,
        entries: [newEntry],
        leaderCommit: 0,
      });

      expect(reply.success).toBe(true);
      expect(node.log.length).toBe(2);
      expect(node.log[1].command).toBe('new-cmd');
    });

    it('should commit entries when leaderCommit advances', async () => {
      const committedSpy = vi.fn();
      node.on('committed', committedSpy);

      node.currentTerm = 5;
      node.log = [
        { term: 0, index: 0, command: null, timestamp: Date.now() },
        { term: 5, index: 1, command: 'cmd1', timestamp: Date.now() },
        { term: 5, index: 2, command: 'cmd2', timestamp: Date.now() },
      ];
      node.commitIndex = 0;
      node.lastApplied = 0;

      node.handleAppendEntries({
        term: 5,
        leaderId: 'node-2',
        prevLogIndex: 2,
        prevLogTerm: 5,
        entries: [],
        leaderCommit: 2,
      });

      expect(node.commitIndex).toBe(2);
      expect(committedSpy).toHaveBeenCalledTimes(2);
      expect(committedSpy).toHaveBeenCalledWith(node.log[1]);
      expect(committedSpy).toHaveBeenCalledWith(node.log[2]);
    });

    it('should propose command as leader', async () => {
      const proposedSpy = vi.fn();
      node.on('proposed', proposedSpy);

      node.becomeLeader();

      // Mock RPC handler
      node.setRPCHandler(async () => ({ term: 1, success: true }));

      const entry = await node.propose({ type: 'set', key: 'x', value: 42 });

      expect(entry.term).toBe(1);
      expect(entry.index).toBe(1);
      expect(entry.command).toEqual({ type: 'set', key: 'x', value: 42 });
      expect(proposedSpy).toHaveBeenCalledWith(entry);
    });

    it('should reject propose when not leader', async () => {
      node.state = NodeState.FOLLOWER;

      await expect(node.propose({ type: 'set', key: 'x', value: 42 })).rejects.toThrow(
        'Not leader'
      );
    });

    it('should handle leader election under failures', async () => {
      // Simulate 5-node cluster with 2 failures
      const nodes = [];
      for (let i = 1; i <= 5; i++) {
        const n = new RaftNode({
          nodeId: `node-${i}`,
          peers: ['node-1', 'node-2', 'node-3', 'node-4', 'node-5'].filter(
            (id) => id !== `node-${i}`
          ),
          electionTimeoutMin: 150,
          electionTimeoutMax: 300,
        });
        nodes.push(n);
      }

      // Setup network between nodes (3 alive, 2 failed)
      const aliveNodes = nodes.slice(0, 3);
      const failedNodes = nodes.slice(3, 5);

      for (const node of aliveNodes) {
        node.setRPCHandler(async (peer, rpc, args) => {
          const peerNode = aliveNodes.find((n) => n.nodeId === peer);
          if (!peerNode) {
            throw new Error('Peer not available');
          }

          if (rpc === 'RequestVote') {
            return peerNode.handleRequestVote(args);
          } else if (rpc === 'AppendEntries') {
            return peerNode.handleAppendEntries(args);
          }
        });

        await node.start();
      }

      // Wait for leader election
      await new Promise((resolve) => setTimeout(resolve, 1000));

      // Check that exactly one node became leader
      const leaders = aliveNodes.filter((n) => n.state === NodeState.LEADER);
      expect(leaders.length).toBe(1);

      // Check that majority (2/3) of alive nodes agree on leader
      const leaderId = leaders[0].nodeId;
      const followers = aliveNodes.filter(
        (n) => n.state === NodeState.FOLLOWER && n.leaderId === leaderId
      );
      expect(followers.length).toBeGreaterThanOrEqual(1);

      // Cleanup
      for (const node of aliveNodes) {
        await node.stop();
      }
    });

    it('should maintain safety property - at most one leader per term', async () => {
      // Simulate split vote scenario
      const nodes = [];
      for (let i = 1; i <= 3; i++) {
        const n = new RaftNode({
          nodeId: `node-${i}`,
          peers: ['node-1', 'node-2', 'node-3'].filter((id) => id !== `node-${i}`),
          electionTimeoutMin: 100,
          electionTimeoutMax: 200,
        });
        nodes.push(n);
      }

      // Setup network
      for (const node of nodes) {
        node.setRPCHandler(async (peer, rpc, args) => {
          const peerNode = nodes.find((n) => n.nodeId === peer);
          if (!peerNode) throw new Error('Peer not found');

          if (rpc === 'RequestVote') {
            return peerNode.handleRequestVote(args);
          } else if (rpc === 'AppendEntries') {
            return peerNode.handleAppendEntries(args);
          }
        });

        await node.start();
      }

      // Wait for elections
      await new Promise((resolve) => setTimeout(resolve, 1000));

      // Verify: at most one leader per term
      const termLeaders = new Map();
      for (const node of nodes) {
        if (node.state === NodeState.LEADER) {
          const term = node.currentTerm;
          if (termLeaders.has(term)) {
            throw new Error(`Multiple leaders in term ${term}`);
          }
          termLeaders.set(term, node.nodeId);
        }
      }

      expect(termLeaders.size).toBeLessThanOrEqual(1);

      // Cleanup
      for (const node of nodes) {
        await node.stop();
      }
    });
  });

  describe('createRaftNode', () => {
    it('should create a new Raft node', () => {
      const node = createRaftNode({
        nodeId: 'test-node',
        peers: ['peer-1', 'peer-2'],
      });

      expect(node).toBeInstanceOf(RaftNode);
      expect(node.nodeId).toBe('test-node');
      expect(node.peers).toEqual(['peer-1', 'peer-2']);
    });
  });
});
