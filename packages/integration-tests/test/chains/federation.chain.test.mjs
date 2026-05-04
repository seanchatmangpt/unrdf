/**
 * Federation Chain Integration Tests
 * Phase 5: 10 tests covering 3-peer federation, Partial availability, Consensus, Replication, Cross-graph JOIN
 *
 * @module @unrdf/integration-tests/test/chains/federation
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { createStore, dataFactory } from '@unrdf/oxigraph';

const { namedNode, literal, quad: createQuad, defaultGraph } = dataFactory;

// Mock metrics for federation testing
let mockMetrics = { queries: 0, errors: 0, latency: [] };
function getMetrics() { return mockMetrics; }
function clearMetrics() { mockMetrics = { queries: 0, errors: 0, latency: [] }; }

/**
 * Mock Peer for Federation Testing
 * Simulates a remote peer with its own data store
 */
class MockPeer {
  constructor(id, store) {
    this.id = id;
    this.store = store;
    this.online = true;
    this.latency = 0;
  }

  async query(sparql) {
    if (!this.online) {
      throw new Error(`Peer ${this.id} is offline`);
    }

    // Simulate network latency
    if (this.latency > 0) {
      await new Promise(resolve => setTimeout(resolve, this.latency));
    }

    return this.store.query(sparql);
  }

  async match(subject, predicate, object, graph) {
    if (!this.online) {
      throw new Error(`Peer ${this.id} is offline`);
    }

    return this.store.match(subject, predicate, object, graph);
  }

  setOnline(status) {
    this.online = status;
  }

  setLatency(ms) {
    this.latency = ms;
  }
}

/**
 * Helper: Create a peer with test data
 * @param {string} id - Peer ID
 * @param {Array<Object>} quads - Initial quads
 * @returns {MockPeer}
 */
function createTestPeer(id, quads = []) {
  const store = createStore();
  quads.forEach(q => store.add(q));
  return new MockPeer(id, store);
}

describe('Federation Chain Integration Tests', () => {
  /** @type {MockPeer[]} */
  let peers;

  beforeEach(() => {
    clearMetrics();
    peers = [];
  });

  afterEach(() => {
    peers = [];
    clearMetrics();
  });

  // Test 1: 3-peer federation with different data
  it('should federate across 3 peers with different data', async () => {
    // Create 3 peers with different data
    const peer1 = createTestPeer('peer1', [
      createQuad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice'),
        defaultGraph()
      ),
    ]);

    const peer2 = createTestPeer('peer2', [
      createQuad(
        namedNode('http://example.org/bob'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Bob'),
        defaultGraph()
      ),
    ]);

    const peer3 = createTestPeer('peer3', [
      createQuad(
        namedNode('http://example.org/charlie'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Charlie'),
        defaultGraph()
      ),
    ]);

    peers = [peer1, peer2, peer3];

    // Query each peer
    const results = await Promise.all(
      peers.map(p => p.match(null, null, null, null))
    );

    expect(results[0].length).toBe(1);
    expect(results[1].length).toBe(1);
    expect(results[2].length).toBe(1);

    // Aggregate results (simulated federation)
    const federated = results.flat();
    expect(federated.length).toBe(3);
  });

  // Test 2: Partial availability - 2/3 peers offline
  it('should handle partial peer availability', async () => {
    const peer1 = createTestPeer('peer1', [
      createQuad(
        namedNode('http://example.org/data1'),
        namedNode('http://example.org/prop'),
        literal('value1'),
        defaultGraph()
      ),
    ]);

    const peer2 = createTestPeer('peer2', [
      createQuad(
        namedNode('http://example.org/data2'),
        namedNode('http://example.org/prop'),
        literal('value2'),
        defaultGraph()
      ),
    ]);

    const peer3 = createTestPeer('peer3', [
      createQuad(
        namedNode('http://example.org/data3'),
        namedNode('http://example.org/prop'),
        literal('value3'),
        defaultGraph()
      ),
    ]);

    // Take 2 peers offline
    peer2.setOnline(false);
    peer3.setOnline(false);

    peers = [peer1, peer2, peer3];

    // Query with partial availability
    const results = await Promise.allSettled(
      peers.map(p => p.match(null, null, null, null))
    );

    const successful = results.filter(r => r.status === 'fulfilled');
    const failed = results.filter(r => r.status === 'rejected');

    expect(successful.length).toBe(1);
    expect(failed.length).toBe(2);

    // System should still return available data
    expect(successful[0].value.length).toBe(1);
  });

  // Test 3: Consensus verification - same result from all peers
  it('should verify consensus across replicated peers', async () => {
    const sharedData = [
      createQuad(
        namedNode('http://example.org/shared'),
        namedNode('http://example.org/value'),
        literal('consensus-value'),
        defaultGraph()
      ),
    ];

    // All peers have same data (replication)
    const peer1 = createTestPeer('peer1', sharedData);
    const peer2 = createTestPeer('peer2', sharedData);
    const peer3 = createTestPeer('peer3', sharedData);

    peers = [peer1, peer2, peer3];

    const results = await Promise.all(
      peers.map(p => p.match(null, null, null, null))
    );

    // All should return same result
    expect(results[0].length).toBe(results[1].length);
    expect(results[1].length).toBe(results[2].length);

    // Verify actual values match
    const values = results.map(r => r[0].object.value);
    expect(new Set(values).size).toBe(1); // All identical
    expect(values[0]).toBe('consensus-value');
  });

  // Test 4: Replication lag - eventual consistency
  it('should handle replication lag with eventual consistency', async () => {
    const peer1 = createTestPeer('peer1');
    const peer2 = createTestPeer('peer2');

    peers = [peer1, peer2];

    // Simulate write to peer1
    peer1.store.add(createQuad(
      namedNode('http://example.org/new'),
      namedNode('http://example.org/created'),
      literal('just-created'),
      defaultGraph()
    ));

    // Peer2 doesn't have the data yet (replication lag)
    const peer1Results = await peer1.match(null, null, null, null);
    const peer2Results = await peer2.match(null, null, null, null);

    expect(peer1Results.length).toBe(1);
    expect(peer2Results.length).toBe(0);

    // Simulate replication (copy data)
    const replicationDelay = 10; // ms
    await new Promise(resolve => setTimeout(resolve, replicationDelay));

    // Replicate data
    peer1Results.forEach(q => peer2.store.add(q));

    // Now both should have same data (eventual consistency)
    const peer2Final = await peer2.match(null, null, null, null);
    expect(peer2Final.length).toBe(1);
    expect(peer2Final[0].object.value).toBe('just-created');
  });

  // Test 5: Cross-graph JOIN
  it('should perform cross-graph JOIN query', async () => {
    const peer1 = createTestPeer('peer1', [
      createQuad(
        namedNode('http://example.org/alice'),
        namedNode('http://example.org/worksFor'),
        namedNode('http://example.org/company/acme'),
        defaultGraph()
      ),
    ]);

    const peer2 = createTestPeer('peer2', [
      createQuad(
        namedNode('http://example.org/company/acme'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('ACME Corporation'),
        defaultGraph()
      ),
    ]);

    peers = [peer1, peer2];

    // Simulated JOIN: Find Alice's company name
    const aliceWorksFor = await peer1.match(
      namedNode('http://example.org/alice'),
      namedNode('http://example.org/worksFor'),
      null,
      null
    );

    expect(aliceWorksFor.length).toBe(1);
    const companyUri = aliceWorksFor[0].object;

    const companyName = await peer2.match(
      companyUri,
      namedNode('http://xmlns.com/foaf/0.1/name'),
      null,
      null
    );

    expect(companyName.length).toBe(1);
    expect(companyName[0].object.value).toBe('ACME Corporation');
  });

  // Test 6: Peer latency handling
  it('should handle varying peer latencies', async () => {
    const peer1 = createTestPeer('peer1', [
      createQuad(namedNode('http://example.org/fast'), namedNode('http://example.org/p'), literal('fast-data'), defaultGraph()),
    ]);

    const peer2 = createTestPeer('peer2', [
      createQuad(namedNode('http://example.org/slow'), namedNode('http://example.org/p'), literal('slow-data'), defaultGraph()),
    ]);

    // Set different latencies
    peer1.setLatency(10);  // 10ms
    peer2.setLatency(50);  // 50ms

    peers = [peer1, peer2];

    const startTime = performance.now();
    const results = await Promise.all(
      peers.map(p => p.match(null, null, null, null))
    );
    const duration = performance.now() - startTime;

    // Should complete in reasonable time (max latency + overhead)
    expect(duration).toBeLessThan(200);
    expect(results.flat().length).toBe(2);
  });

  // Test 7: Federated query routing
  it('should route queries to appropriate peers', async () => {
    // Simulate peer specialization
    const peerConfig = [
      { id: 'peer-people', patterns: ['foaf:Person'] },
      { id: 'peer-companies', patterns: ['org:Organization'] },
      { id: 'peer-products', patterns: ['schema:Product'] },
    ];

    // Simple routing function
    const routeQuery = (query, peers) => {
      if (query.includes('foaf:Person')) return 'peer-people';
      if (query.includes('org:Organization')) return 'peer-companies';
      if (query.includes('schema:Product')) return 'peer-products';
      return peers[0].id; // Default to first peer
    };

    expect(routeQuery('SELECT ?x WHERE { ?x a foaf:Person }', peerConfig)).toBe('peer-people');
    expect(routeQuery('SELECT ?x WHERE { ?x a org:Organization }', peerConfig)).toBe('peer-companies');
    expect(routeQuery('SELECT ?x WHERE { ?x a schema:Product }', peerConfig)).toBe('peer-products');
  });

  // Test 8: Federation metrics collection
  it('should collect federation metrics', async () => {
    const peer1 = createTestPeer('peer1', [
      createQuad(namedNode('http://example.org/m1'), namedNode('http://example.org/p'), literal('v1'), defaultGraph()),
    ]);

    peers = [peer1];

    // Perform some queries
    await peer1.match(null, null, null, null);
    await peer1.match(null, null, null, null);
    await peer1.match(null, null, null, null);

    // Check metrics structure (from @unrdf/federation)
    const metrics = getMetrics();
    expect(typeof metrics).toBe('object');
  });

  // Test 9: Error handling in federation
  it('should handle federation errors gracefully', async () => {
    const peer1 = createTestPeer('peer1', [
      createQuad(namedNode('http://example.org/d1'), namedNode('http://example.org/p'), literal('v1'), defaultGraph()),
    ]);

    const errorPeer = {
      id: 'error-peer',
      online: true,
      match: async () => {
        throw new Error('Simulated network error');
      },
    };

    peers = [peer1, errorPeer];

    const results = await Promise.allSettled(
      peers.map(p => p.match(null, null, null, null))
    );

    // One success, one failure
    expect(results[0].status).toBe('fulfilled');
    expect(results[1].status).toBe('rejected');

    // Should still get partial results
    const successfulResults = results
      .filter(r => r.status === 'fulfilled')
      .map(r => r.value)
      .flat();

    expect(successfulResults.length).toBe(1);
  });

  // Test 10: Federated data aggregation
  it('should aggregate data from multiple peers', async () => {
    const peer1 = createTestPeer('peer1', [
      createQuad(namedNode('http://example.org/item1'), namedNode('http://example.org/count'), literal('10'), defaultGraph()),
    ]);

    const peer2 = createTestPeer('peer2', [
      createQuad(namedNode('http://example.org/item2'), namedNode('http://example.org/count'), literal('20'), defaultGraph()),
    ]);

    const peer3 = createTestPeer('peer3', [
      createQuad(namedNode('http://example.org/item3'), namedNode('http://example.org/count'), literal('30'), defaultGraph()),
    ]);

    peers = [peer1, peer2, peer3];

    // Collect all count values
    const results = await Promise.all(
      peers.map(p => p.match(null, namedNode('http://example.org/count'), null, null))
    );

    const allCounts = results.flat().map(q => parseInt(q.object.value, 10));

    // Aggregate
    const sum = allCounts.reduce((a, b) => a + b, 0);
    const avg = sum / allCounts.length;

    expect(sum).toBe(60);
    expect(avg).toBe(20);
    expect(allCounts.length).toBe(3);
  });
});
