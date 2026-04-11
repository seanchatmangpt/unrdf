/**
 * @vitest-environment node
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  proposeResult,
  votePictlResult,
  getQuorumStatus,
  validateVoteTally,
  generateResultId,
} from '../src/quorum.mjs';

describe('Federation Quorum Voting', () => {
  let quorumState;

  beforeEach(() => {
    quorumState = {
      nodeId: 'pictl-node-1',
      quorumThreshold: 2,
      results: new Map(),
      votes: new Map(),
      receipts: [],
      peers: new Map(),
      ontology: null,
    };
  });

  describe('Result Proposal', () => {
    it('should propose a result with unique ID', () => {
      const result = {
        fitness: 0.92,
        precision: 0.88,
        model: { type: 'petri-net' },
      };

      const proposed = proposeResult(result, 'pictl-node-1', quorumState);

      expect(proposed).toBeDefined();
      expect(proposed.resultId).toMatch(/^[a-f0-9]{16}$/);
      expect(proposed.proposerId).toBe('pictl-node-1');
      expect(proposed.status).toBe('pending');
      expect(proposed.votes).toHaveLength(0);
    });

    it('should generate same ID for identical results', () => {
      const result = { fitness: 0.92, precision: 0.88 };

      const id1 = generateResultId(result);
      const id2 = generateResultId(result);

      expect(id1).toBe(id2);
      expect(id1).toMatch(/^[a-f0-9]{16}$/);
    });

    it('should generate different IDs for different results', () => {
      const id1 = generateResultId({ fitness: 0.92 });
      const id2 = generateResultId({ fitness: 0.88 });

      expect(id1).not.toBe(id2);
    });

    it('should store proposed result in quorum state', () => {
      const result = { fitness: 0.92 };
      const proposed = proposeResult(result, 'pictl-node-1', quorumState);

      const stored = quorumState.results.get(proposed.resultId);
      expect(stored).toBeDefined();
      expect(stored.result).toEqual(result);
    });
  });

  describe('Vote Recording', () => {
    it('should record a single vote', () => {
      const result = { fitness: 0.92 };
      const proposed = proposeResult(result, 'pictl-node-1', quorumState);

      const vote = votePictlResult(proposed.resultId, 'approve', 'Valid', quorumState);

      expect(vote).toBeDefined();
      expect(vote.vote).toBe('approve');
      expect(vote.nodeId).toBe('pictl-node-1');
      expect(vote.totalVotes).toBe(1);
    });

    it('should reject votes for non-existent results', () => {
      expect(() => {
        votePictlResult('nonexistent', 'approve', '', quorumState);
      }).toThrow('Result not found');
    });

    it('should accumulate multiple votes', () => {
      const result = { fitness: 0.92 };
      const proposed = proposeResult(result, 'pictl-node-1', quorumState);

      // Simulate votes from different nodes
      votePictlResult(proposed.resultId, 'approve', 'Valid on node 1', quorumState);

      // Change node ID for next vote
      quorumState.nodeId = 'pictl-node-2';
      votePictlResult(proposed.resultId, 'approve', 'Valid on node 2', quorumState);

      const stored = quorumState.results.get(proposed.resultId);
      expect(stored.votes).toHaveLength(2);
      expect(stored.votes[0].vote).toBe('approve');
      expect(stored.votes[1].vote).toBe('approve');
    });

    it('should track approval and rejection counts', () => {
      const result = { fitness: 0.92 };
      const proposed = proposeResult(result, 'pictl-node-1', quorumState);

      quorumState.nodeId = 'node-1';
      votePictlResult(proposed.resultId, 'approve', '', quorumState);

      quorumState.nodeId = 'node-2';
      votePictlResult(proposed.resultId, 'reject', '', quorumState);

      quorumState.nodeId = 'node-3';
      const vote3 = votePictlResult(proposed.resultId, 'approve', '', quorumState);

      expect(vote3.totalVotes).toBe(3);
      expect(vote3.approvalCount).toBe(2); // 2 approvals
    });
  });

  describe('Quorum Consensus', () => {
    it('should reach consensus with 2/3 approval (3 nodes)', () => {
      const result = { fitness: 0.92 };
      const proposed = proposeResult(result, 'pictl-node-1', quorumState);

      // 3 nodes vote
      quorumState.nodeId = 'node-1';
      votePictlResult(proposed.resultId, 'approve', '', quorumState);

      quorumState.nodeId = 'node-2';
      votePictlResult(proposed.resultId, 'approve', '', quorumState);

      quorumState.nodeId = 'node-3';
      const vote3 = votePictlResult(proposed.resultId, 'reject', '', quorumState);

      // 2 approvals out of 3 = 66.7% → approved
      expect(vote3.consensusStatus).toBe('approved');
    });

    it('should reach consensus with 1/3 rejection (3 nodes)', () => {
      const result = { fitness: 0.92 };
      const proposed = proposeResult(result, 'pictl-node-1', quorumState);

      quorumState.nodeId = 'node-1';
      votePictlResult(proposed.resultId, 'approve', '', quorumState);

      quorumState.nodeId = 'node-2';
      votePictlResult(proposed.resultId, 'approve', '', quorumState);

      quorumState.nodeId = 'node-3';
      const vote3 = votePictlResult(proposed.resultId, 'reject', '', quorumState);

      const stored = quorumState.results.get(proposed.resultId);
      expect(stored.status).toBe('approved');
    });

    it('should reach consensus with 2 approvals out of 3 votes', () => {
      const result = { fitness: 0.92 };
      const proposed = proposeResult(result, 'pictl-node-1', quorumState);

      quorumState.nodeId = 'node-1';
      votePictlResult(proposed.resultId, 'approve', '', quorumState);

      quorumState.nodeId = 'node-2';
      votePictlResult(proposed.resultId, 'approve', '', quorumState);

      quorumState.nodeId = 'node-3';
      const vote3 = votePictlResult(proposed.resultId, 'reject', '', quorumState);

      // 2 approvals out of 3 = 66.7% > 66.7% threshold → approved
      expect(vote3.consensusStatus).toBe('approved');
    });

    it('should create receipt when consensus reached', () => {
      const result = { fitness: 0.92 };
      const proposed = proposeResult(result, 'pictl-node-1', quorumState);

      quorumState.nodeId = 'node-1';
      votePictlResult(proposed.resultId, 'approve', '', quorumState);

      quorumState.nodeId = 'node-2';
      const vote2 = votePictlResult(proposed.resultId, 'approve', '', quorumState);

      // With 2 votes both approving, threshold is 2 (100%), so receipt is created
      expect(vote2.receipt).toBeDefined();
      expect(vote2.receipt.hash).toMatch(/^[a-f0-9]{64}$/);
    });
  });

  describe('Receipt Chain', () => {
    it('should create receipt chain with BLAKE3 hashes', () => {
      const result1 = { fitness: 0.92 };
      const proposed1 = proposeResult(result1, 'pictl-node-1', quorumState);

      quorumState.nodeId = 'node-1';
      votePictlResult(proposed1.resultId, 'approve', '', quorumState);

      quorumState.nodeId = 'node-2';
      votePictlResult(proposed1.resultId, 'approve', '', quorumState);

      expect(quorumState.receipts).toHaveLength(1);
      const receipt1 = quorumState.receipts[0];
      expect(receipt1.hash).toMatch(/^[a-f0-9]{64}$/);
      expect(receipt1.previousHash).toBe('0'.repeat(64)); // First receipt
    });

    it('should chain receipts with previousHash', () => {
      // First result
      const result1 = { fitness: 0.92 };
      const proposed1 = proposeResult(result1, 'pictl-node-1', quorumState);
      quorumState.nodeId = 'node-1';
      votePictlResult(proposed1.resultId, 'approve', '', quorumState);
      quorumState.nodeId = 'node-2';
      votePictlResult(proposed1.resultId, 'approve', '', quorumState);

      expect(quorumState.receipts).toHaveLength(1);
      const receipt1 = quorumState.receipts[0];

      // Second result
      const result2 = { fitness: 0.88 };
      const proposed2 = proposeResult(result2, 'pictl-node-1', quorumState);
      quorumState.nodeId = 'node-1';
      votePictlResult(proposed2.resultId, 'approve', '', quorumState);
      quorumState.nodeId = 'node-2';
      votePictlResult(proposed2.resultId, 'approve', '', quorumState);

      expect(quorumState.receipts).toHaveLength(2);
      const receipt2 = quorumState.receipts[1];
      expect(receipt2.previousHash).toBe(receipt1.hash);
    });

    it('should validate receipt chain integrity', () => {
      const result1 = { fitness: 0.92 };
      const proposed1 = proposeResult(result1, 'pictl-node-1', quorumState);
      quorumState.nodeId = 'node-1';
      votePictlResult(proposed1.resultId, 'approve', '', quorumState);
      quorumState.nodeId = 'node-2';
      votePictlResult(proposed1.resultId, 'approve', '', quorumState);

      const validation = validateVoteTally(quorumState);
      expect(validation.valid).toBe(true);
      expect(validation.receiptCount).toBe(1);
    });

    it('should detect chain breaks', () => {
      const result1 = { fitness: 0.92 };
      const proposed1 = proposeResult(result1, 'pictl-node-1', quorumState);
      quorumState.nodeId = 'node-1';
      votePictlResult(proposed1.resultId, 'approve', '', quorumState);
      quorumState.nodeId = 'node-2';
      votePictlResult(proposed1.resultId, 'approve', '', quorumState);

      // Create a second result to form a chain
      const result2 = { fitness: 0.88 };
      const proposed2 = proposeResult(result2, 'pictl-node-1', quorumState);
      quorumState.nodeId = 'node-1';
      votePictlResult(proposed2.resultId, 'approve', '', quorumState);
      quorumState.nodeId = 'node-2';
      votePictlResult(proposed2.resultId, 'approve', '', quorumState);

      // Now tamper with first receipt hash (which breaks the chain to second)
      quorumState.receipts[0].hash = '0'.repeat(64);

      const validation = validateVoteTally(quorumState);
      expect(validation.valid).toBe(false);
      expect(validation.error).toContain('chain integrity violation');
    });
  });

  describe('Quorum Status', () => {
    it('should report empty quorum status', () => {
      const status = getQuorumStatus(quorumState);

      expect(status.nodeId).toBe('pictl-node-1');
      expect(status.results.total).toBe(0);
      expect(status.results.approved).toBe(0);
      expect(status.results.pending).toBe(0);
      expect(status.receipts.count).toBe(0);
    });

    it('should report pending results', () => {
      const result = { fitness: 0.92 };
      proposeResult(result, 'pictl-node-1', quorumState);

      const status = getQuorumStatus(quorumState);
      expect(status.results.total).toBe(1);
      expect(status.results.pending).toBe(1);
      expect(status.results.approved).toBe(0);
    });

    it('should report approved results in status', () => {
      const result = { fitness: 0.92, precision: 0.88 };
      const proposed = proposeResult(result, 'pictl-node-1', quorumState);

      quorumState.nodeId = 'node-1';
      votePictlResult(proposed.resultId, 'approve', '', quorumState);

      quorumState.nodeId = 'node-2';
      votePictlResult(proposed.resultId, 'approve', '', quorumState);

      const status = getQuorumStatus(quorumState);
      expect(status.results.approved).toBe(1);
      expect(status.approvedResults).toHaveLength(1);
      expect(status.approvedResults[0].result).toEqual(result);
    });

    it('should include receipt chain status', () => {
      const result = { fitness: 0.92 };
      const proposed = proposeResult(result, 'pictl-node-1', quorumState);

      quorumState.nodeId = 'node-1';
      votePictlResult(proposed.resultId, 'approve', '', quorumState);

      quorumState.nodeId = 'node-2';
      votePictlResult(proposed.resultId, 'approve', '', quorumState);

      const status = getQuorumStatus(quorumState);
      expect(status.receipts.count).toBe(1);
      expect(status.receipts.chainValid).toBe(true);
      expect(status.receipts.latestHash).toMatch(/^[a-f0-9]{64}$/);
    });
  });
});
