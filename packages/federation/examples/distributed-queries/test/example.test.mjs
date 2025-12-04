/**
 * @file Distributed Queries Example Tests
 * @vitest-environment node
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { createCoordinator, aggregateResults } from '@unrdf/federation';

describe('Distributed Queries Example', () => {
  let coordinator;

  beforeEach(() => {
    coordinator = createCoordinator({
      peers: [
        { id: 'peer-1', endpoint: 'http://localhost:8001/sparql', metadata: { dataset: 'test1' } },
        { id: 'peer-2', endpoint: 'http://localhost:8002/sparql', metadata: { dataset: 'test2' } },
      ],
      strategy: 'broadcast',
      timeout: 5000,
    });
  });

  describe('Coordinator Creation', () => {
    it('should create coordinator with initial peers', () => {
      const peers = coordinator.listPeers();
      expect(peers).toHaveLength(2);
      expect(peers[0].id).toBe('peer-1');
      expect(peers[1].id).toBe('peer-2');
    });

    it('should create coordinator with configuration', () => {
      const stats = coordinator.getStats();
      expect(stats.totalPeers).toBe(2);
    });
  });

  describe('Peer Management', () => {
    it('should add peer to federation', async () => {
      await coordinator.addPeer('peer-3', 'http://localhost:8003/sparql', {
        dataset: 'test3',
      });

      const peers = coordinator.listPeers();
      expect(peers).toHaveLength(3);
    });

    it('should remove peer from federation', () => {
      const removed = coordinator.removePeer('peer-1');
      expect(removed).toBe(true);

      const peers = coordinator.listPeers();
      expect(peers).toHaveLength(1);
    });

    it('should get specific peer', () => {
      const peer = coordinator.getPeer('peer-1');
      expect(peer).toBeDefined();
      expect(peer?.id).toBe('peer-1');
    });
  });

  describe('Distributed Queries', () => {
    it('should handle query with no healthy peers', async () => {
      const emptyCoordinator = createCoordinator({ peers: [] });
      const result = await emptyCoordinator.query('SELECT * WHERE { ?s ?p ?o }');

      expect(result.success).toBe(false);
      expect(result.error).toBe('No healthy peers available');
    });

    it('should query specific peer', async () => {
      const result = await coordinator.queryPeer(
        'peer-1',
        'SELECT * WHERE { ?s ?p ?o } LIMIT 1'
      );

      expect(result).toBeDefined();
      expect(result.peerId).toBe('peer-1');
      expect(result.success).toBe(false); // Will fail since peer is not actually running
    });

    it('should handle query to non-existent peer', async () => {
      const result = await coordinator.queryPeer('non-existent', 'SELECT * WHERE { ?s ?p ?o }');

      expect(result.success).toBe(false);
      expect(result.error).toContain('not found');
    });
  });

  describe('Result Aggregation', () => {
    it('should aggregate successful results', () => {
      const peerResults = [
        {
          success: true,
          data: {
            results: {
              bindings: [{ s: { value: 'http://example.org/a' } }],
            },
          },
          duration: 100,
          peerId: 'peer-1',
        },
        {
          success: true,
          data: {
            results: {
              bindings: [{ s: { value: 'http://example.org/b' } }],
            },
          },
          duration: 150,
          peerId: 'peer-2',
        },
      ];

      const aggregated = aggregateResults(peerResults);
      expect(aggregated).toHaveLength(2);
    });

    it('should deduplicate results', () => {
      const peerResults = [
        {
          success: true,
          data: {
            results: {
              bindings: [{ s: { value: 'http://example.org/a' } }],
            },
          },
          duration: 100,
          peerId: 'peer-1',
        },
        {
          success: true,
          data: {
            results: {
              bindings: [{ s: { value: 'http://example.org/a' } }], // Duplicate
            },
          },
          duration: 150,
          peerId: 'peer-2',
        },
      ];

      const aggregated = aggregateResults(peerResults);
      expect(aggregated).toHaveLength(1); // Duplicates removed
    });

    it('should handle failed results', () => {
      const peerResults = [
        {
          success: false,
          data: null,
          error: 'Connection failed',
          duration: 100,
          peerId: 'peer-1',
        },
        {
          success: true,
          data: {
            results: {
              bindings: [{ s: { value: 'http://example.org/a' } }],
            },
          },
          duration: 150,
          peerId: 'peer-2',
        },
      ];

      const aggregated = aggregateResults(peerResults);
      expect(aggregated).toHaveLength(1); // Only successful results
    });

    it('should handle array results', () => {
      const peerResults = [
        {
          success: true,
          data: [{ id: 1 }, { id: 2 }],
          duration: 100,
          peerId: 'peer-1',
        },
      ];

      const aggregated = aggregateResults(peerResults);
      expect(aggregated).toHaveLength(2);
    });

    it('should return empty array for no successful results', () => {
      const peerResults = [
        {
          success: false,
          data: null,
          error: 'Failed',
          duration: 100,
          peerId: 'peer-1',
        },
      ];

      const aggregated = aggregateResults(peerResults);
      expect(aggregated).toHaveLength(0);
    });
  });

  describe('Health Checks', () => {
    it('should run health check on all peers', async () => {
      const health = await coordinator.healthCheck();

      expect(health.totalPeers).toBe(2);
      expect(health.results).toHaveLength(2);
    });

    it('should track peer health status', async () => {
      await coordinator.healthCheck();
      const stats = coordinator.getStats();

      expect(stats.totalPeers).toBe(2);
      // Peers will be unreachable since they're not actually running
      expect(stats.unreachablePeers).toBeGreaterThan(0);
    });
  });

  describe('Statistics', () => {
    it('should track query statistics', async () => {
      await coordinator.query('SELECT * WHERE { ?s ?p ?o }');
      await coordinator.queryPeer('peer-1', 'SELECT * WHERE { ?s ?p ?o }');

      const stats = coordinator.getStats();
      expect(stats.totalQueries).toBe(2);
      expect(stats.totalErrors).toBeGreaterThan(0); // Queries will fail
    });

    it('should calculate error rate', async () => {
      await coordinator.query('SELECT * WHERE { ?s ?p ?o }');

      const stats = coordinator.getStats();
      expect(stats.errorRate).toBeGreaterThanOrEqual(0);
      expect(stats.errorRate).toBeLessThanOrEqual(1);
    });
  });

  describe('Periodic Health Checks', () => {
    it('should start and stop health checks', () => {
      coordinator.startHealthChecks();
      // Health checks started successfully

      coordinator.stopHealthChecks();
      // Health checks stopped successfully

      // No error thrown
      expect(true).toBe(true);
    });
  });
});
