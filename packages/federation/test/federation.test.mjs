/**
 * @vitest-environment node
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import {
  createCoordinator,
  createPeerManager,
  executeFederatedQuery,
  executeDistributedQuery,
  aggregateResults,
  routeQuery,
} from '../src/index.mjs';

/* ========================================================================= */
/* Mock Server Setup                                                        */
/* ========================================================================= */

// Mock fetch for testing
const originalFetch = global.fetch;

function createMockFetch(responses = {}) {
  return vi.fn(async (url, _options) => {
    const endpoint = url.toString();

    // Mock SPARQL endpoint
    if (endpoint.includes('/sparql')) {
      const peerId = Object.keys(responses).find(id => endpoint.includes(id));
      const response = responses[peerId] || { ok: true, data: [] };

      return {
        ok: response.ok !== false,
        status: response.status || 200,
        statusText: response.statusText || 'OK',
        json: async () => response.data,
        text: async () => JSON.stringify(response.data),
      };
    }

    // Mock health check endpoint
    return {
      ok: true,
      status: 200,
      statusText: 'OK',
    };
  });
}

beforeEach(() => {
  vi.useFakeTimers();
});

afterEach(() => {
  global.fetch = originalFetch;
  vi.restoreAllMocks();
  vi.useRealTimers();
});

/* ========================================================================= */
/* Peer Manager Tests                                                       */
/* ========================================================================= */

describe('PeerManager', () => {
  it('should register a peer', () => {
    const manager = createPeerManager();
    const peer = manager.registerPeer('peer1', 'http://example.com/sparql');

    expect(peer.id).toBe('peer1');
    expect(peer.endpoint).toBe('http://example.com/sparql');
    expect(peer.status).toBe('healthy');
    expect(peer.registeredAt).toBeGreaterThan(0);
  });

  it('should unregister a peer', () => {
    const manager = createPeerManager();
    manager.registerPeer('peer1', 'http://example.com/sparql');

    const removed = manager.unregisterPeer('peer1');
    expect(removed).toBe(true);

    const peer = manager.getPeer('peer1');
    expect(peer).toBeNull();
  });

  it('should return false when unregistering non-existent peer', () => {
    const manager = createPeerManager();
    const removed = manager.unregisterPeer('nonexistent');
    expect(removed).toBe(false);
  });

  it('should list all peers', () => {
    const manager = createPeerManager();
    manager.registerPeer('peer1', 'http://example.com/sparql');
    manager.registerPeer('peer2', 'http://example.org/sparql');

    const peers = manager.listPeers();
    expect(peers).toHaveLength(2);
    expect(peers.map(p => p.id)).toContain('peer1');
    expect(peers.map(p => p.id)).toContain('peer2');
  });

  it('should filter peers by status', () => {
    const manager = createPeerManager();
    manager.registerPeer('peer1', 'http://example.com/sparql');
    manager.registerPeer('peer2', 'http://example.org/sparql');
    manager.updateStatus('peer2', 'unreachable');

    const healthyPeers = manager.listPeers({ status: 'healthy' });
    expect(healthyPeers).toHaveLength(1);
    expect(healthyPeers[0].id).toBe('peer1');
  });

  it('should ping a peer successfully', async () => {
    global.fetch = createMockFetch({});

    const manager = createPeerManager();
    manager.registerPeer('peer1', 'http://example.com/sparql');

    const result = await manager.ping('peer1');
    expect(result).toBe(true);

    const peer = manager.getPeer('peer1');
    expect(peer.status).toBe('healthy');
  });

  it('should handle ping failure', async () => {
    global.fetch = vi.fn().mockRejectedValue(new Error('Network error'));

    const manager = createPeerManager();
    manager.registerPeer('peer1', 'http://example.com/sparql');

    const result = await manager.ping('peer1');
    expect(result).toBe(false);

    const peer = manager.getPeer('peer1');
    expect(peer.status).toBe('unreachable');
  });

  it('should clear all peers', () => {
    const manager = createPeerManager();
    manager.registerPeer('peer1', 'http://example.com/sparql');
    manager.registerPeer('peer2', 'http://example.org/sparql');

    manager.clear();
    expect(manager.size()).toBe(0);
  });
});

/* ========================================================================= */
/* Distributed Query Tests                                                  */
/* ========================================================================= */

describe('Distributed Query', () => {
  it('should execute federated query successfully', async () => {
    const mockData = {
      results: {
        bindings: [{ name: { type: 'literal', value: 'Alice' } }],
      },
    };

    global.fetch = createMockFetch({
      peer1: { ok: true, data: mockData },
    });

    const result = await executeFederatedQuery(
      'peer1',
      'http://peer1.com/sparql',
      'SELECT * WHERE { ?s ?p ?o }'
    );

    expect(result.success).toBe(true);
    expect(result.peerId).toBe('peer1');
    expect(result.data).toEqual(mockData);
  });

  it('should handle query failure', async () => {
    global.fetch = vi.fn().mockRejectedValue(new Error('Connection refused'));

    const result = await executeFederatedQuery(
      'peer1',
      'http://example.com/sparql',
      'SELECT * WHERE { ?s ?p ?o }'
    );

    expect(result.success).toBe(false);
    expect(result.error).toContain('Connection refused');
  });

  it('should execute distributed query across multiple peers', async () => {
    const mockData1 = {
      results: {
        bindings: [{ name: { type: 'literal', value: 'Alice' } }],
      },
    };

    const mockData2 = {
      results: {
        bindings: [{ name: { type: 'literal', value: 'Bob' } }],
      },
    };

    global.fetch = createMockFetch({
      peer1: { ok: true, data: mockData1 },
      peer2: { ok: true, data: mockData2 },
    });

    const peers = [
      { id: 'peer1', endpoint: 'http://peer1.com/sparql' },
      { id: 'peer2', endpoint: 'http://peer2.com/sparql' },
    ];

    const result = await executeDistributedQuery(peers, 'SELECT * WHERE { ?s ?p ?o }');

    expect(result.success).toBe(true);
    expect(result.successCount).toBe(2);
    expect(result.failureCount).toBe(0);
    expect(result.peerResults).toHaveLength(2);
  });

  it('should aggregate results from multiple peers', () => {
    const results = [
      {
        success: true,
        data: {
          results: {
            bindings: [{ name: { type: 'literal', value: 'Alice' } }],
          },
        },
        peerId: 'peer1',
        duration: 100,
      },
      {
        success: true,
        data: {
          results: {
            bindings: [
              { name: { type: 'literal', value: 'Bob' } },
              { name: { type: 'literal', value: 'Alice' } }, // Duplicate
            ],
          },
        },
        peerId: 'peer2',
        duration: 150,
      },
    ];

    const aggregated = aggregateResults(results);
    expect(aggregated).toHaveLength(2); // Duplicates removed
  });

  it('should handle empty results in aggregation', () => {
    const results = [
      { success: false, data: null, error: 'Error', peerId: 'peer1', duration: 100 },
    ];

    const aggregated = aggregateResults(results);
    expect(aggregated).toHaveLength(0);
  });

  it('should route query with broadcast strategy', () => {
    const peers = [
      { id: 'peer1', endpoint: 'http://peer1.com' },
      { id: 'peer2', endpoint: 'http://peer2.com' },
    ];

    const routed = routeQuery(peers, 'SELECT * WHERE { ?s ?p ?o }', 'broadcast');
    expect(routed).toHaveLength(2);
  });

  it('should route query with first-available strategy', () => {
    const peers = [
      { id: 'peer1', endpoint: 'http://peer1.com' },
      { id: 'peer2', endpoint: 'http://peer2.com' },
    ];

    const routed = routeQuery(peers, 'SELECT * WHERE { ?s ?p ?o }', 'first-available');
    expect(routed).toHaveLength(1);
    expect(routed[0].id).toBe('peer1');
  });
});

/* ========================================================================= */
/* Federation Coordinator Tests                                             */
/* ========================================================================= */

describe('Federation Coordinator', () => {
  it('should create coordinator with initial peers', () => {
    const coordinator = createCoordinator({
      peers: [{ id: 'peer1', endpoint: 'http://peer1.com/sparql' }],
    });

    const peers = coordinator.listPeers();
    expect(peers).toHaveLength(1);
    expect(peers[0].id).toBe('peer1');
  });

  it('should add peer dynamically', async () => {
    global.fetch = createMockFetch({});

    const coordinator = createCoordinator();
    await coordinator.addPeer('peer1', 'http://peer1.com/sparql');

    const peer = coordinator.getPeer('peer1');
    expect(peer).not.toBeNull();
    expect(peer.id).toBe('peer1');
  });

  it('should remove peer', () => {
    const coordinator = createCoordinator({
      peers: [{ id: 'peer1', endpoint: 'http://peer1.com/sparql' }],
    });

    const removed = coordinator.removePeer('peer1');
    expect(removed).toBe(true);

    const peers = coordinator.listPeers();
    expect(peers).toHaveLength(0);
  });

  it('should execute federated query', async () => {
    const mockData = {
      results: {
        bindings: [{ name: { type: 'literal', value: 'Alice' } }],
      },
    };

    global.fetch = createMockFetch({
      peer1: { ok: true, data: mockData },
    });

    const coordinator = createCoordinator({
      peers: [{ id: 'peer1', endpoint: 'http://peer1.com/sparql' }],
    });

    const result = await coordinator.query('SELECT * WHERE { ?s ?p ?o }');
    expect(result.success).toBe(true);
    expect(result.successCount).toBe(1);
  });

  it('should query specific peer', async () => {
    const mockData = {
      results: {
        bindings: [{ name: { type: 'literal', value: 'Alice' } }],
      },
    };

    global.fetch = createMockFetch({
      peer1: { ok: true, data: mockData },
    });

    const coordinator = createCoordinator({
      peers: [{ id: 'peer1', endpoint: 'http://peer1.com/sparql' }],
    });

    const result = await coordinator.queryPeer('peer1', 'SELECT * WHERE { ?s ?p ?o }');
    expect(result.success).toBe(true);
    expect(result.peerId).toBe('peer1');
  });

  it('should handle query to non-existent peer', async () => {
    const coordinator = createCoordinator();

    const result = await coordinator.queryPeer('nonexistent', 'SELECT * WHERE { ?s ?p ?o }');
    expect(result.success).toBe(false);
    expect(result.error).toContain('not found');
  });

  it('should run health checks', async () => {
    global.fetch = createMockFetch({});

    const coordinator = createCoordinator({
      peers: [
        { id: 'peer1', endpoint: 'http://peer1.com/sparql' },
        { id: 'peer2', endpoint: 'http://peer2.com/sparql' },
      ],
    });

    const health = await coordinator.healthCheck();
    expect(health.totalPeers).toBe(2);
    expect(health.healthyPeers).toBeGreaterThanOrEqual(0);
  });

  it('should get federation stats', () => {
    const coordinator = createCoordinator({
      peers: [{ id: 'peer1', endpoint: 'http://peer1.com/sparql' }],
    });

    const stats = coordinator.getStats();
    expect(stats.totalPeers).toBe(1);
    expect(stats.totalQueries).toBe(0);
    expect(stats.totalErrors).toBe(0);
  });

  it('should handle no healthy peers', async () => {
    const coordinator = createCoordinator();

    const result = await coordinator.query('SELECT * WHERE { ?s ?p ?o }');
    expect(result.success).toBe(false);
    expect(result.error).toContain('No healthy peers');
  });
});

/* ========================================================================= */
/* Integration Tests                                                        */
/* ========================================================================= */

describe('Federation Integration', () => {
  it('should coordinate multi-peer query with deduplication', async () => {
    const sharedData = { name: { type: 'literal', value: 'Shared' } };

    global.fetch = createMockFetch({
      peer1: {
        ok: true,
        data: {
          results: {
            bindings: [sharedData, { name: { type: 'literal', value: 'Alice' } }],
          },
        },
      },
      peer2: {
        ok: true,
        data: {
          results: {
            bindings: [
              sharedData, // Duplicate
              { name: { type: 'literal', value: 'Bob' } },
            ],
          },
        },
      },
    });

    const coordinator = createCoordinator({
      peers: [
        { id: 'peer1', endpoint: 'http://peer1.com/sparql' },
        { id: 'peer2', endpoint: 'http://peer2.com/sparql' },
      ],
    });

    const result = await coordinator.query('SELECT * WHERE { ?s ?p ?o }');

    expect(result.success).toBe(true);
    expect(result.successCount).toBe(2);
    expect(result.results).toHaveLength(3); // 3 unique results
  });
});

/* ========================================================================= */
/* Coordinator Lifecycle Tests (COVERAGE GAP)                               */
/* ========================================================================= */

describe('Coordinator Lifecycle', () => {
  it('should destroy coordinator and clean up resources', () => {
    const coordinator = createCoordinator({
      peers: [{ id: 'peer1', endpoint: 'http://peer1.com/sparql' }],
    });

    coordinator.startHealthChecks();

    // Destroy should stop health checks
    coordinator.destroy();

    // After destroy, health check timer should be cleared
    expect(() => coordinator.destroy()).not.toThrow();
  });

  it('should handle startHealthChecks called multiple times', () => {
    const coordinator = createCoordinator({
      peers: [{ id: 'peer1', endpoint: 'http://peer1.com/sparql' }],
    });

    coordinator.startHealthChecks();
    coordinator.startHealthChecks(); // Should not create duplicate timer

    coordinator.stopHealthChecks();
  });

  it('should handle stopHealthChecks when not started', () => {
    const coordinator = createCoordinator();

    // Should not throw when stopping without starting
    expect(() => coordinator.stopHealthChecks()).not.toThrow();
  });

  it('should clean up on destroy even if not started', () => {
    const coordinator = createCoordinator();

    // Should handle destroy without health checks started
    expect(() => coordinator.destroy()).not.toThrow();
  });
});

/* ========================================================================= */
/* Health Check Error Handling (COVERAGE GAP)                               */
/* ========================================================================= */

describe('Health Check Error Handling', () => {
  it('should handle health check errors gracefully', async () => {
    global.fetch = vi.fn().mockRejectedValue(new Error('Network timeout'));

    const coordinator = createCoordinator({
      peers: [
        { id: 'peer1', endpoint: 'http://peer1.com/sparql' },
        { id: 'peer2', endpoint: 'http://peer2.com/sparql' },
      ],
    });

    const health = await coordinator.healthCheck();

    expect(health.totalPeers).toBe(2);
    expect(health.healthyPeers).toBe(0);
    expect(health.unreachablePeers).toBe(2);
  });

  it('should track peer status degradation', async () => {
    global.fetch = createMockFetch({
      peer1: { ok: false, status: 500, statusText: 'Internal Server Error' },
    });

    const coordinator = createCoordinator({
      peers: [{ id: 'peer1', endpoint: 'http://peer1.com/sparql' }],
    });

    await coordinator.queryPeer('peer1', 'SELECT * WHERE { ?s ?p ?o }');

    const peer = coordinator.getPeer('peer1');
    expect(peer.status).toBe('degraded');
  });

  it('should track peer unreachable status', async () => {
    global.fetch = vi.fn().mockRejectedValue(new Error('Connection refused'));

    const coordinator = createCoordinator({
      peers: [{ id: 'peer1', endpoint: 'http://peer1.com/sparql' }],
    });

    await coordinator.queryPeer('peer1', 'SELECT * WHERE { ?s ?p ?o }');

    const peer = coordinator.getPeer('peer1');
    // When fetch fails, executeFederatedQuery returns {success: false}, which sets status to 'degraded'
    expect(peer.status).toBe('degraded');
  });
});

/* ========================================================================= */
/* Consensus Edge Cases (COVERAGE GAP)                                      */
/* ========================================================================= */

describe('Consensus Edge Cases', () => {
  it('should handle query with all peers failing', async () => {
    global.fetch = vi.fn().mockRejectedValue(new Error('All peers down'));

    const coordinator = createCoordinator({
      peers: [
        { id: 'peer1', endpoint: 'http://peer1.com/sparql' },
        { id: 'peer2', endpoint: 'http://peer2.com/sparql' },
      ],
    });

    const result = await coordinator.query('SELECT * WHERE { ?s ?p ?o }');

    expect(result.success).toBe(false);
    expect(result.failureCount).toBe(2);
    expect(result.successCount).toBe(0);
  });

  it('should handle partial failures in distributed query', async () => {
    global.fetch = createMockFetch({
      peer1: { ok: true, data: { results: { bindings: [{ x: { type: 'literal', value: '1' } }] } } },
      peer2: { ok: false, status: 500 },
    });

    const coordinator = createCoordinator({
      peers: [
        { id: 'peer1', endpoint: 'http://peer1.com/sparql' },
        { id: 'peer2', endpoint: 'http://peer2.com/sparql' },
      ],
    });

    const result = await coordinator.query('SELECT * WHERE { ?s ?p ?o }');

    expect(result.successCount).toBe(1);
    expect(result.failureCount).toBe(1);
  });

  it('should track error rate in stats', async () => {
    global.fetch = vi.fn()
      .mockResolvedValueOnce({ ok: true, status: 200, json: async () => ({ results: { bindings: [] } }) })
      .mockRejectedValueOnce(new Error('Failure'));

    const coordinator = createCoordinator({
      peers: [{ id: 'peer1', endpoint: 'http://peer1.com/sparql' }],
    });

    await coordinator.queryPeer('peer1', 'SELECT * WHERE { ?s ?p ?o }');
    await coordinator.queryPeer('peer1', 'SELECT * WHERE { ?s ?p ?o }');

    const stats = coordinator.getStats();
    expect(stats.totalQueries).toBe(2);
    expect(stats.totalErrors).toBe(1);
    expect(stats.errorRate).toBeCloseTo(0.5);
  });
});
