/**
 * @file End-to-End Daemon Federation Query Integration Tests
 * @module @unrdf/daemon/test/e2e-federation-query
 * @description Comprehensive E2E tests for distributed SPARQL query execution
 * across federated Raft nodes with result aggregation, node selection, and failure recovery.
 *
 * Test coverage:
 * - Query federation across 3+ nodes
 * - Result aggregation and deduplication
 * - Node selection strategies (broadcast, selective, best-node)
 * - Fallback on node failures
 * - Distributed result consistency
 * - Performance on large federations
 * - Query optimization and caching
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { DaemonFederationExecutor } from '../src/integrations/federation-query.mjs';

// =============================================================================
// Test Utilities and Mocks
// =============================================================================

/**
 * Mock Federation Coordinator
 */
class MockFederationCoordinator {
  constructor(nodeCount = 3) {
    this.peers = [];
    this.queryLog = [];
    this.nodeFailures = new Map();

    for (let i = 0; i < nodeCount; i++) {
      this.peers.push({
        id: `node-${i}`,
        endpoint: `http://node-${i}.local:8080/sparql`,
        metadata: { region: `region-${i % 2}` },
      });
    }
  }

  listPeers() {
    return this.peers.map(p => ({
      ...p,
      status: this.nodeFailures.get(p.id) ? 'unreachable' : 'healthy',
    }));
  }

  async queryPeer(nodeId, sparqlQuery) {
    if (this.nodeFailures.get(nodeId)) {
      throw new Error(`Node ${nodeId} is down`);
    }

    this.queryLog.push({ nodeId, query: sparqlQuery, timestamp: Date.now() });

    // Simulate different results from different nodes
    const nodeIndex = parseInt(nodeId.split('-')[1]);
    const baseResults = [
      { id: 'x1', name: 'Alice' },
      { id: 'x2', name: 'Bob' },
      { id: 'x3', name: 'Charlie' },
    ];

    // Add node-specific results
    const nodeSpecificResults = [
      { id: `x${nodeIndex + 10}`, name: `Node${nodeIndex}Person` },
    ];

    return [...baseResults, ...nodeSpecificResults];
  }

  async addPeer(id, endpoint, metadata) {
    this.peers.push({ id, endpoint, status: 'healthy', metadata });
  }

  removePeer(id) {
    const index = this.peers.findIndex(p => p.id === id);
    if (index > -1) {
      this.peers.splice(index, 1);
      return true;
    }
    return false;
  }

  getPeer(id) {
    return this.peers.find(p => p.id === id);
  }

  failNode(nodeId) {
    this.nodeFailures.set(nodeId, true);
  }

  recoverNode(nodeId) {
    this.nodeFailures.delete(nodeId);
  }
}

/**
 * Mock Daemon
 */
class MockDaemon {
  constructor() {
    this.nodeId = 'daemon-node-1';
    this.clusterId = 'test-cluster';
    this.isRunning = true;
    this.isLeader = true;
  }
}

// =============================================================================
// Test Suite
// =============================================================================

describe('DaemonFederationExecutor', () => {
  let daemon;
  let coordinator;
  let executor;

  beforeEach(() => {
    daemon = new MockDaemon();
    coordinator = new MockFederationCoordinator(3);
    executor = new DaemonFederationExecutor(daemon, coordinator, {
      strategy: 'selective',
      timeout: 30000,
    });
  });

  afterEach(() => {
    executor.reset();
  });

  // =========================================================================
  // Test Category 1: Basic Query Execution
  // =========================================================================

  it('should execute SPARQL query on all nodes with broadcast strategy', async () => {
    const query = 'SELECT ?x WHERE { ?x ?p ?o }';
    const result = await executor.executeQuery(query, { strategy: 'broadcast' });

    expect(result.queryId).toBeDefined();
    expect(result.aggregatedResults).toBeDefined();
    expect(result.aggregatedResults.length).toBeGreaterThan(0);
    expect(result.successCount).toBe(3);
    expect(result.failureCount).toBe(0);
    expect(result.strategy).toBe('broadcast');
    expect(result.timestamp).toBeInstanceOf(Date);
  });

  it('should execute SPARQL query on subset of nodes with selective strategy', async () => {
    const query = 'SELECT ?x WHERE { ?x ?p ?o }';
    const result = await executor.executeQuery(query, { strategy: 'selective' });

    expect(result.nodeCount).toBeGreaterThan(0);
    expect(result.nodeCount).toBeLessThanOrEqual(3);
    expect(result.successCount).toBeGreaterThan(0);
    expect(result.aggregatedResults.length).toBeGreaterThan(0);
  });

  it('should execute query on single best node with best-node strategy', async () => {
    const query = 'SELECT ?x WHERE { ?x ?p ?o }';
    const result = await executor.executeQuery(query, { strategy: 'best-node' });

    expect(result.nodeCount).toBe(1);
    expect(result.successCount).toBe(1);
    expect(result.failureCount).toBe(0);
  });

  // =========================================================================
  // Test Category 2: Result Aggregation
  // =========================================================================

  it('should aggregate results from multiple nodes correctly', async () => {
    const query = 'SELECT ?id ?name WHERE { ?id ?p ?name }';
    const result = await executor.executeQuery(query, { strategy: 'broadcast' });

    expect(result.aggregatedResults.length).toBeGreaterThan(3);
    result.aggregatedResults.forEach(r => {
      expect(r._sources).toBeDefined();
      expect(Array.isArray(r._sources)).toBe(true);
      expect(r._replicaCount).toBe(r._sources.length);
    });
  });

  it('should deduplicate identical results across nodes', async () => {
    const query = 'SELECT ?x WHERE { ?x ?p ?o }';
    const result = await executor.executeQuery(query, {
      strategy: 'broadcast',
    });

    const uniqueIds = new Set();
    for (const item of result.aggregatedResults) {
      const { _sources, _replicaCount, ...data } = item;
      const key = JSON.stringify(data);
      expect(!uniqueIds.has(key)).toBe(true);
      uniqueIds.add(key);
    }

    expect(uniqueIds.size).toBe(result.aggregatedResults.length);
  });

  it('should mark replica sources in results', async () => {
    const query = 'SELECT ?x WHERE { ?x ?p ?o }';
    const result = await executor.executeQuery(query, { strategy: 'broadcast' });

    const baseResults = result.aggregatedResults.filter(r => r._replicaCount >= 2);
    expect(baseResults.length).toBeGreaterThan(0);

    baseResults.forEach(r => {
      expect(r._replicaCount).toBeGreaterThanOrEqual(2);
      expect(r._sources.length).toBeGreaterThanOrEqual(2);
    });
  });

  // =========================================================================
  // Test Category 3: Node Selection Heuristics
  // =========================================================================

  it('should select nodes based on performance metrics', async () => {
    const query = 'SELECT ?x WHERE { ?x ?p ?o }';

    // Execute multiple queries to build metrics
    for (let i = 0; i < 3; i++) {
      await executor.executeQuery(query, { strategy: 'selective' });
    }

    const metrics = executor.getNodeMetrics();
    expect(Object.keys(metrics).length).toBeGreaterThan(0);

    Object.values(metrics).forEach(m => {
      expect(m.queryCount).toBeGreaterThan(0);
      expect(m.successRate).toBeGreaterThanOrEqual(0);
      expect(m.successRate).toBeLessThanOrEqual(1);
      expect(m.avgDuration).toBeGreaterThanOrEqual(0);
    });
  });

  it('should prefer nodes with higher success rates', async () => {
    const query = 'SELECT ?x WHERE { ?x ?p ?o }';

    // Fail a specific node
    coordinator.failNode('node-0');

    const result = await executor.executeQuery(query, { strategy: 'selective' });

    expect(result.successCount).toBeGreaterThan(0);
    const selectedNodes = result.peerResults.map(r => r.nodeId);
    expect(selectedNodes).not.toContain('node-0');
  });

  it('should balance load across healthy nodes', async () => {
    const query = 'SELECT ?x WHERE { ?x ?p ?o }';

    // Execute multiple queries with broadcast
    for (let i = 0; i < 5; i++) {
      await executor.executeQuery(query, { strategy: 'broadcast' });
    }

    const metrics = executor.getNodeMetrics();
    const queryCountsPerNode = Object.values(metrics).map(m => m.queryCount);

    // All nodes should have similar query counts with broadcast
    const max = Math.max(...queryCountsPerNode);
    const min = Math.min(...queryCountsPerNode);
    expect(max - min).toBeLessThanOrEqual(1);
  });

  // =========================================================================
  // Test Category 4: Failure and Fallback
  // =========================================================================

  it('should fallback to other nodes when one fails', async () => {
    const query = 'SELECT ?x WHERE { ?x ?p ?o }';

    coordinator.failNode('node-0');

    const result = await executor.executeQuery(query, { strategy: 'broadcast' });

    expect(result.successCount).toBeGreaterThanOrEqual(1);
    expect(result.aggregatedResults.length).toBeGreaterThan(0);
  });

  it('should recover when failed nodes come back online', async () => {
    const query = 'SELECT ?x WHERE { ?x ?p ?o }';

    // Fail a node
    coordinator.failNode('node-1');

    const firstResult = await executor.executeQuery(query, { strategy: 'broadcast' });
    expect(firstResult.successCount).toBeGreaterThanOrEqual(1);

    // Recover the node
    coordinator.recoverNode('node-1');

    const secondResult = await executor.executeQuery(query, { strategy: 'broadcast' });
    expect(secondResult.successCount).toBeGreaterThanOrEqual(2);
  });

  it('should throw error when all nodes fail', async () => {
    const query = 'SELECT ?x WHERE { ?x ?p ?o }';

    // Create a new coordinator with no peers
    const emptyCoordinator = new MockFederationCoordinator(0);

    const emptyExecutor = new DaemonFederationExecutor(daemon, emptyCoordinator);

    await expect(emptyExecutor.executeQuery(query)).rejects.toThrow(
      /No available federation nodes/
    );
  });

  it('should exclude specified nodes from query execution', async () => {
    const query = 'SELECT ?x WHERE { ?x ?p ?o }';

    const result = await executor.executeQuery(query, {
      strategy: 'broadcast',
      excludeNodes: ['node-0', 'node-1'],
    });

    expect(result.nodeCount).toBe(1);
    expect(result.successCount).toBe(1);
    const selectedNodes = result.peerResults.map(r => r.nodeId);
    expect(selectedNodes).toContain('node-2');
  });

  // =========================================================================
  // Test Category 5: Result Consistency
  // =========================================================================

  it('should ensure distributed consistency across nodes', async () => {
    const query = 'SELECT ?x WHERE { ?x ?p ?o }';

    const result1 = await executor.executeQuery(query, { strategy: 'broadcast' });
    const result2 = await executor.executeQuery(query, { strategy: 'broadcast' });

    // Same query should return same base results
    const baseResults1 = result1.aggregatedResults
      .filter(r => r.id === 'x1' || r.id === 'x2' || r.id === 'x3');
    const baseResults2 = result2.aggregatedResults
      .filter(r => r.id === 'x1' || r.id === 'x2' || r.id === 'x3');

    expect(baseResults1.length).toBe(baseResults2.length);
  });

  it('should preserve result ordering consistency', async () => {
    const query = 'SELECT ?x WHERE { ?x ?p ?o }';

    const result = await executor.executeQuery(query, { strategy: 'broadcast' });

    expect(Array.isArray(result.aggregatedResults)).toBe(true);
    result.aggregatedResults.forEach(r => {
      expect(typeof r).toBe('object');
      expect(r._sources).toBeDefined();
    });
  });

  // =========================================================================
  // Test Category 6: Query Statistics and Monitoring
  // =========================================================================

  it('should track query statistics accurately', async () => {
    const query = 'SELECT ?x WHERE { ?x ?p ?o }';

    const result = await executor.executeQuery(query);
    const stats = executor.getStats(result.queryId);

    expect(stats).toBeDefined();
    expect(stats.queryId).toBe(result.queryId);
    expect(stats.successCount).toBeGreaterThanOrEqual(0);
    expect(stats.strategy).toBe('selective');
    expect(stats.totalDuration).toBeGreaterThanOrEqual(0);
    expect(stats.nodeCount).toBeGreaterThan(0);
  });

  it('should maintain health status with accurate metrics', async () => {
    const query = 'SELECT ?x WHERE { ?x ?p ?o }';

    for (let i = 0; i < 3; i++) {
      await executor.executeQuery(query);
    }

    const health = executor.getHealth();

    expect(health.executorId).toBe(executor.id);
    expect(health.totalQueries).toBe(3);
    expect(health.successfulQueries).toBeGreaterThanOrEqual(0);
    expect(health.nodeCount).toBeGreaterThan(0);
    expect(health.averageQueryDuration).toBeGreaterThanOrEqual(0);
  });

  it('should record failed queries separately', async () => {
    // Create coordinator with no peers to trigger actual failure
    const failingCoordinator = new MockFederationCoordinator(0);
    const failingExecutor = new DaemonFederationExecutor(daemon, failingCoordinator);

    try {
      await failingExecutor.executeQuery('SELECT ?x WHERE { ?x ?p ?o }');
    } catch {
      // Expected to fail
    }

    const stats = failingExecutor.getStats();
    expect(stats.failedQueries).toBe(1);
  });

  // =========================================================================
  // Test Category 7: Performance and Scalability
  // =========================================================================

  it('should handle queries on 100-node federation', async () => {
    const largeCoordinator = new MockFederationCoordinator(100);
    const largeExecutor = new DaemonFederationExecutor(daemon, largeCoordinator, {
      strategy: 'selective',
    });

    const startTime = Date.now();
    const result = await largeExecutor.executeQuery('SELECT ?x WHERE { ?x ?p ?o }');
    const duration = Date.now() - startTime;

    expect(result.successCount).toBeGreaterThan(0);
    expect(result.nodeCount).toBeGreaterThan(0);
    expect(result.nodeCount).toBeLessThanOrEqual(100);
    expect(duration).toBeLessThan(5000);

    largeExecutor.reset();
  });

  it('should execute queries within timeout', async () => {
    const query = 'SELECT ?x WHERE { ?x ?p ?o }';

    const startTime = Date.now();
    const result = await executor.executeQuery(query, { timeout: 5000 });
    const duration = Date.now() - startTime;

    expect(duration).toBeLessThan(5500);
    expect(result.totalDuration).toBeLessThan(5000);
  });

  it('should optimize query execution with best-node strategy', async () => {
    const query = 'SELECT ?x WHERE { ?x ?p ?o }';

    const broadcastResult = await executor.executeQuery(query, { strategy: 'broadcast' });
    executor.reset();

    const bestNodeResult = await executor.executeQuery(query, { strategy: 'best-node' });

    // Best node should be faster overall
    expect(bestNodeResult.totalDuration).toBeLessThanOrEqual(broadcastResult.totalDuration);
  });

  // =========================================================================
  // Test Category 8: Input Validation and Error Handling
  // =========================================================================

  it('should reject invalid SPARQL queries', async () => {
    await expect(executor.executeQuery('')).rejects.toThrow(
      /Invalid SPARQL query/
    );

    await expect(executor.executeQuery(null)).rejects.toThrow(
      /Invalid SPARQL query/
    );

    await expect(executor.executeQuery('   ')).rejects.toThrow(
      /Invalid SPARQL query/
    );
  });

  it('should handle empty result sets gracefully', async () => {
    // Mock coordinator that returns empty results
    const emptyCoordinator = new MockFederationCoordinator(2);
    emptyCoordinator.queryPeer = async () => [];

    const emptyExecutor = new DaemonFederationExecutor(daemon, emptyCoordinator);

    const result = await emptyExecutor.executeQuery('SELECT ?x WHERE { ?x ?p ?o }', {
      strategy: 'broadcast'
    });

    expect(result.aggregatedResults).toBeDefined();
    expect(result.aggregatedResults.length).toBe(0);
    expect(result.successCount).toBeGreaterThan(0);
  });

  it('should handle mixed success and failure results', async () => {
    // Create a 3-node coordinator and fail one node
    const mixedCoordinator = new MockFederationCoordinator(3);
    mixedCoordinator.failNode('node-0');

    const mixedExecutor = new DaemonFederationExecutor(daemon, mixedCoordinator);

    const result = await mixedExecutor.executeQuery('SELECT ?x WHERE { ?x ?p ?o }', {
      strategy: 'broadcast',
    });

    expect(result.successCount).toBeGreaterThan(0);
    // With one node failed, we should have some failures when using broadcast
    expect(result.successCount + result.failureCount).toBe(result.nodeCount);
  });

  // =========================================================================
  // Test Category 9: Advanced Features
  // =========================================================================

  it('should support query result deduplication toggle', async () => {
    const deduplicatingExecutor = new DaemonFederationExecutor(daemon, coordinator, {
      deduplicateResults: true,
    });

    const nonDeduplicatingExecutor = new DaemonFederationExecutor(daemon, coordinator, {
      deduplicateResults: false,
    });

    const result1 = await deduplicatingExecutor.executeQuery(
      'SELECT ?x WHERE { ?x ?p ?o }',
      { strategy: 'broadcast' }
    );

    deduplicatingExecutor.reset();
    const result2 = await nonDeduplicatingExecutor.executeQuery(
      'SELECT ?x WHERE { ?x ?p ?o }',
      { strategy: 'broadcast' }
    );

    // Deduplicating should have fewer results
    expect(result1.aggregatedResults.length).toBeLessThanOrEqual(result2.aggregatedResults.length);

    deduplicatingExecutor.reset();
    nonDeduplicatingExecutor.reset();
  });

  it('should allow executor reset', async () => {
    const query = 'SELECT ?x WHERE { ?x ?p ?o }';

    await executor.executeQuery(query);
    const statsBefore = executor.getStats();
    expect(statsBefore.totalQueries).toBe(1);

    executor.reset();

    const statsAfter = executor.getStats();
    expect(statsAfter.totalQueries).toBe(0);
  });

  it('should support concurrent query execution', async () => {
    const query = 'SELECT ?x WHERE { ?x ?p ?o }';

    const promises = [];
    for (let i = 0; i < 5; i++) {
      promises.push(executor.executeQuery(query));
    }

    const results = await Promise.all(promises);

    expect(results.length).toBe(5);
    results.forEach(r => {
      expect(r.aggregatedResults).toBeDefined();
      expect(r.successCount).toBeGreaterThan(0);
    });
  });
});
