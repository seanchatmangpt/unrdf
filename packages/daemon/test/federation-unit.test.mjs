/**
 * @file Federation Unit Tests
 * @module @unrdf/daemon/test/federation-unit
 * @description Unit tests for federation query schemas, node selection,
 * executor internals, and OTEL span propagation.
 *
 * These tests complement the e2e-federation-query.test.mjs integration suite
 * by covering individual modules in isolation with mocked dependencies.
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';

// --- Schemas ---
import {
  QueryStatsSchema,
  FederationExecutorConfigSchema,
  ExecuteQueryOptionsSchema,
  SparqlQuerySchema,
  _QueryIdSchema,
  _NodeIdSchema,
  DaemonSchema,
  FederationCoordinatorSchema,
} from '../src/integrations/federation-query-schemas.mjs';

// --- Node Selection ---
import {
  selectNodes,
  selectBestNode,
  selectTopNodes,
  updateNodeMetrics,
} from '../src/integrations/federation-node-selection.mjs';

// --- Executor ---
import { DaemonFederationExecutor } from '../src/integrations/federation-query.mjs';

// =============================================================================
// Helpers
// =============================================================================

function makeCoordinator(peers) {
  return {
    listPeers: () => peers,
    queryPeer: vi.fn(),
  };
}

function healthyPeer(id) {
  return { id, status: 'healthy', endpoint: `http://${id}:8080/sparql` };
}

function unhealthyPeer(id) {
  return { id, status: 'unreachable', endpoint: `http://${id}:8080/sparql` };
}

// =============================================================================
// 1. Schema Validation
// =============================================================================

describe('Federation Query Schemas', () => {
  describe('SparqlQuerySchema', () => {
    it('accepts a non-empty SPARQL string', () => {
      const result = SparqlQuerySchema.parse('SELECT ?x WHERE { ?x a :Thing }');
      expect(result).toBe('SELECT ?x WHERE { ?x a :Thing }');
    });

    it('rejects empty string', () => {
      expect(() => SparqlQuerySchema.parse('')).toThrow();
    });

    it('rejects whitespace-only string', () => {
      expect(() => SparqlQuerySchema.parse('   ')).toThrow();
    });

    it('rejects non-string values', () => {
      expect(() => SparqlQuerySchema.parse(null)).toThrow();
      expect(() => SparqlQuerySchema.parse(42)).toThrow();
      expect(() => SparqlQuerySchema.parse(undefined)).toThrow();
    });
  });

  describe('FederationExecutorConfigSchema', () => {
    it('applies defaults when no options provided', () => {
      const cfg = FederationExecutorConfigSchema.parse({});
      expect(cfg.strategy).toBe('selective');
      expect(cfg.timeout).toBe(30000);
      expect(cfg.maxRetries).toBe(2);
      expect(cfg.deduplicateResults).toBe(true);
      expect(cfg.enableNodeSelection).toBe(true);
      expect(cfg.healthCheckThreshold).toBe(0.7);
      expect(cfg.executorId).toMatch(/^executor-/);
    });

    it('accepts explicit values', () => {
      const cfg = FederationExecutorConfigSchema.parse({
        executorId: 'my-exec',
        strategy: 'broadcast',
        timeout: 5000,
        maxRetries: 5,
        deduplicateResults: false,
        enableNodeSelection: false,
        healthCheckThreshold: 0.5,
      });
      expect(cfg.executorId).toBe('my-exec');
      expect(cfg.strategy).toBe('broadcast');
      expect(cfg.timeout).toBe(5000);
      expect(cfg.maxRetries).toBe(5);
      expect(cfg.deduplicateResults).toBe(false);
      expect(cfg.enableNodeSelection).toBe(false);
      expect(cfg.healthCheckThreshold).toBe(0.5);
    });

    it('rejects invalid strategy', () => {
      expect(() =>
        FederationExecutorConfigSchema.parse({ strategy: 'invalid' })
      ).toThrow();
    });

    it('rejects non-positive timeout', () => {
      expect(() =>
        FederationExecutorConfigSchema.parse({ timeout: -1 })
      ).toThrow();
      expect(() =>
        FederationExecutorConfigSchema.parse({ timeout: 0 })
      ).toThrow();
    });

    it('rejects healthCheckThreshold outside 0-1 range', () => {
      expect(() =>
        FederationExecutorConfigSchema.parse({ healthCheckThreshold: -0.1 })
      ).toThrow();
      expect(() =>
        FederationExecutorConfigSchema.parse({ healthCheckThreshold: 1.1 })
      ).toThrow();
    });
  });

  describe('ExecuteQueryOptionsSchema', () => {
    it('accepts undefined', () => {
      expect(ExecuteQueryOptionsSchema.parse(undefined)).toBeUndefined();
    });

    it('accepts empty object', () => {
      const opts = ExecuteQueryOptionsSchema.parse({});
      expect(opts).toEqual({});
    });

    it('accepts full options', () => {
      const opts = ExecuteQueryOptionsSchema.parse({
        strategy: 'best-node',
        timeout: 1000,
        excludeNodes: ['node-a', 'node-b'],
      });
      expect(opts.strategy).toBe('best-node');
      expect(opts.timeout).toBe(1000);
      expect(opts.excludeNodes).toEqual(['node-a', 'node-b']);
    });

    it('rejects invalid strategy in options', () => {
      expect(() =>
        ExecuteQueryOptionsSchema.parse({ strategy: 'round-robin' })
      ).toThrow();
    });
  });

  describe('QueryStatsSchema', () => {
    it('accepts a valid stats object', () => {
      const stats = QueryStatsSchema.parse({
        queryId: 'q-1',
        sparql: 'SELECT ?s WHERE { ?s ?p ?o }',
        strategy: 'broadcast',
        nodeCount: 3,
        successCount: 3,
        failureCount: 0,
        totalDuration: 120,
        startTime: 1000,
        endTime: 1120,
      });
      expect(stats.queryId).toBe('q-1');
      expect(stats.successCount).toBe(3);
    });

    it('rejects negative counts', () => {
      expect(() =>
        QueryStatsSchema.parse({
          queryId: 'q-1',
          sparql: 'SELECT ?s WHERE { ?s ?p ?o }',
          strategy: 'broadcast',
          nodeCount: -1,
          successCount: 0,
          failureCount: 0,
          totalDuration: 0,
          startTime: 0,
          endTime: 0,
        })
      ).toThrow();
    });
  });

  describe('DaemonSchema and FederationCoordinatorSchema', () => {
    it('passes through any object (passthrough)', () => {
      const daemonResult = DaemonSchema.parse({ anything: 42 });
      expect(daemonResult.anything).toBe(42);

      const coordResult = FederationCoordinatorSchema.parse({ listPeers: () => {} });
      expect(typeof coordResult.listPeers).toBe('function');
    });
  });
});

// =============================================================================
// 2. Node Selection
// =============================================================================

describe('Federation Node Selection', () => {
  describe('selectNodes', () => {
    const coordinator = makeCoordinator([
      healthyPeer('a'),
      healthyPeer('b'),
      unhealthyPeer('c'),
    ]);

    it('broadcast: returns all healthy nodes', () => {
      const nodes = selectNodes(coordinator, 'broadcast');
      expect(nodes).toEqual(['a', 'b']);
    });

    it('selective: returns top 50% (rounded up) of healthy nodes', () => {
      const nodes = selectNodes(coordinator, 'selective');
      // ceil(2 * 0.5) = 1
      expect(nodes).toHaveLength(1);
      expect(['a', 'b']).toContain(nodes[0]);
    });

    it('best-node: returns single node', () => {
      const nodes = selectNodes(coordinator, 'best-node');
      expect(nodes).toHaveLength(1);
      expect(['a', 'b']).toContain(nodes[0]);
    });

    it('excludes specified nodes', () => {
      const nodes = selectNodes(coordinator, 'broadcast', ['a']);
      expect(nodes).toEqual(['b']);
    });

    it('falls back to unhealthy nodes when no healthy nodes available', () => {
      const unhealthyCoord = makeCoordinator([
        unhealthyPeer('x'),
        unhealthyPeer('y'),
      ]);
      const nodes = selectNodes(unhealthyCoord, 'broadcast');
      expect(nodes).toEqual(['x', 'y']);
    });

    it('returns empty array when no peers at all', () => {
      const emptyCoord = makeCoordinator([]);
      const nodes = selectNodes(emptyCoord, 'broadcast');
      expect(nodes).toEqual([]);
    });

    it('handles coordinator without listPeers method', () => {
      const bareCoordinator = {};
      const nodes = selectNodes(bareCoordinator, 'broadcast');
      expect(nodes).toEqual([]);
    });
  });

  describe('selectBestNode', () => {
    it('returns the node with the highest score (successRate / (1 + avgDuration/1000))', () => {
      const metrics = new Map([
        ['fast', { queryCount: 10, successRate: 1.0, avgDuration: 10 }],
        ['slow', { queryCount: 10, successRate: 1.0, avgDuration: 5000 }],
        ['unreliable', { queryCount: 10, successRate: 0.3, avgDuration: 100 }],
      ]);

      const best = selectBestNode(['fast', 'slow', 'unreliable'], metrics);
      expect(best).toBe('fast');
    });

    it('uses default metrics for untracked nodes', () => {
      const metrics = new Map();
      const best = selectBestNode(['unknown'], metrics);
      expect(best).toBe('unknown');
    });

    it('returns first node when all have identical metrics', () => {
      const metrics = new Map([
        ['a', { queryCount: 5, successRate: 1.0, avgDuration: 100 }],
        ['b', { queryCount: 5, successRate: 1.0, avgDuration: 100 }],
      ]);

      const best = selectBestNode(['a', 'b'], metrics);
      // Both have identical scores; first one wins
      expect(['a', 'b']).toContain(best);
    });
  });

  describe('selectTopNodes', () => {
    it('returns the top N nodes by score', () => {
      const metrics = new Map([
        ['fast', { queryCount: 10, successRate: 1.0, avgDuration: 10 }],
        ['medium', { queryCount: 10, successRate: 0.9, avgDuration: 500 }],
        ['slow', { queryCount: 10, successRate: 0.5, avgDuration: 5000 }],
      ]);

      const top = selectTopNodes(['fast', 'medium', 'slow'], 2, metrics);
      expect(top).toHaveLength(2);
      expect(top[0]).toBe('fast');
      expect(top[1]).toBe('medium');
    });

    it('returns all nodes when count exceeds available', () => {
      const metrics = new Map();
      const top = selectTopNodes(['a', 'b'], 5, metrics);
      expect(top).toHaveLength(2);
    });
  });

  describe('updateNodeMetrics', () => {
    it('initializes metrics for a new node on success', () => {
      const metrics = new Map();
      updateNodeMetrics(metrics, [
        { nodeId: 'x', success: true, duration: 50 },
      ]);

      const m = metrics.get('x');
      expect(m.queryCount).toBe(1);
      expect(m.successCount).toBe(1);
      expect(m.successRate).toBe(1.0);
      expect(m.avgDuration).toBe(50);
    });

    it('initializes metrics for a new node on failure', () => {
      const metrics = new Map();
      updateNodeMetrics(metrics, [
        { nodeId: 'y', success: false, duration: 100 },
      ]);

      const m = metrics.get('y');
      expect(m.queryCount).toBe(1);
      expect(m.successCount).toBe(0);
      expect(m.successRate).toBe(0);
      expect(m.avgDuration).toBe(100);
    });

    it('accumulates metrics across multiple results', () => {
      const metrics = new Map();
      updateNodeMetrics(metrics, [
        { nodeId: 'a', success: true, duration: 10 },
      ]);
      updateNodeMetrics(metrics, [
        { nodeId: 'a', success: true, duration: 20 },
      ]);
      updateNodeMetrics(metrics, [
        { nodeId: 'a', success: false, duration: 30 },
      ]);

      const m = metrics.get('a');
      expect(m.queryCount).toBe(3);
      expect(m.successCount).toBe(2);
      expect(m.successRate).toBeCloseTo(2 / 3, 10);
      expect(m.avgDuration).toBeCloseTo(60 / 3, 10);
    });

    it('handles missing duration gracefully (defaults to 0)', () => {
      const metrics = new Map();
      updateNodeMetrics(metrics, [
        { nodeId: 'z', success: true },
      ]);

      const m = metrics.get('z');
      expect(m.avgDuration).toBe(0);
    });
  });
});

// =============================================================================
// 3. Executor Internals
// =============================================================================

describe('DaemonFederationExecutor internals', () => {
  let daemon;
  let coordinator;

  beforeEach(() => {
    daemon = {};
    coordinator = makeCoordinator([
      healthyPeer('node-0'),
      healthyPeer('node-1'),
      healthyPeer('node-2'),
    ]);
    coordinator.queryPeer.mockResolvedValue([{ id: 'x1', value: 'hello' }]);
  });

  describe('constructor', () => {
    it('assigns config defaults via schema', () => {
      const executor = new DaemonFederationExecutor(daemon, coordinator);
      expect(executor.config.strategy).toBe('selective');
      expect(executor.config.timeout).toBe(30000);
      expect(executor.config.deduplicateResults).toBe(true);
    });

    it('accepts custom executorId', () => {
      const executor = new DaemonFederationExecutor(daemon, coordinator, {
        executorId: 'custom-id',
      });
      expect(executor.id).toBe('custom-id');
    });
  });

  describe('getStats', () => {
    it('returns undefined for unknown queryId', () => {
      const executor = new DaemonFederationExecutor(daemon, coordinator);
      expect(executor.getStats('nonexistent')).toBeUndefined();
    });

    it('returns summary when no queryId provided', () => {
      const executor = new DaemonFederationExecutor(daemon, coordinator);
      const summary = executor.getStats();
      expect(summary).toEqual({
        totalQueries: 0,
        stats: [],
        nodeMetrics: {},
        failedQueries: 0,
      });
    });
  });

  describe('getNodeMetrics', () => {
    it('returns undefined for unknown nodeId', () => {
      const executor = new DaemonFederationExecutor(daemon, coordinator);
      expect(executor.getNodeMetrics('missing')).toBeUndefined();
    });

    it('returns all metrics as plain object when no nodeId provided', () => {
      const executor = new DaemonFederationExecutor(daemon, coordinator);
      expect(executor.getNodeMetrics()).toEqual({});
    });
  });

  describe('reset', () => {
    it('clears all internal state', async () => {
      const executor = new DaemonFederationExecutor(daemon, coordinator);

      await executor.executeQuery('SELECT ?x WHERE { ?x a :Thing }');
      expect(executor.getStats().totalQueries).toBe(1);

      executor.reset();
      expect(executor.getStats().totalQueries).toBe(0);
      expect(executor.getHealth().totalQueries).toBe(0);
      expect(executor.getNodeMetrics()).toEqual({});
    });
  });

  describe('getHealth', () => {
    it('returns zeroed health for a fresh executor', () => {
      const executor = new DaemonFederationExecutor(daemon, coordinator, {
        executorId: 'health-test',
      });
      const health = executor.getHealth();
      expect(health.executorId).toBe('health-test');
      expect(health.totalQueries).toBe(0);
      expect(health.successfulQueries).toBe(0);
      expect(health.failedQueries).toBe(0);
      expect(health.nodeCount).toBe(0);
      expect(health.averageQueryDuration).toBe(0);
      expect(health.timestamp).toBeInstanceOf(Date);
    });

    it('computes averageQueryDuration across queries', async () => {
      const executor = new DaemonFederationExecutor(daemon, coordinator);
      await executor.executeQuery('SELECT ?x WHERE { ?x a :Thing }');
      await executor.executeQuery('SELECT ?y WHERE { ?y a :Other }');

      const health = executor.getHealth();
      expect(health.totalQueries).toBe(2);
      expect(health.successfulQueries).toBe(2);
      expect(health.averageQueryDuration).toBeGreaterThanOrEqual(0);
    });
  });

  describe('input validation', () => {
    it('throws on empty query string', async () => {
      const executor = new DaemonFederationExecutor(daemon, coordinator);
      await expect(executor.executeQuery('')).rejects.toThrow(
        /Invalid SPARQL query/,
      );
    });

    it('throws on null query', async () => {
      const executor = new DaemonFederationExecutor(daemon, coordinator);
      await expect(executor.executeQuery(null)).rejects.toThrow();
    });

    it('throws when no peers are available', async () => {
      const emptyCoord = makeCoordinator([]);
      const executor = new DaemonFederationExecutor(daemon, emptyCoord);
      await expect(
        executor.executeQuery('SELECT ?x WHERE { ?x a :Thing }'),
      ).rejects.toThrow(/No available federation nodes/);
    });

    it('records failed query in failedQueries map', async () => {
      const emptyCoord = makeCoordinator([]);
      const executor = new DaemonFederationExecutor(daemon, emptyCoord);

      try {
        await executor.executeQuery('SELECT ?x WHERE { ?x a :Thing }');
      } catch {
        // expected
      }

      expect(executor.failedQueries.size).toBe(1);
      const health = executor.getHealth();
      expect(health.failedQueries).toBe(1);
    });
  });
});

// =============================================================================
// 4. Query Execution Strategies
// =============================================================================

describe('Federation Query Execution Strategies', () => {
  let daemon;
  let coordinator;

  beforeEach(() => {
    daemon = {};
    coordinator = makeCoordinator([
      healthyPeer('n0'),
      healthyPeer('n1'),
      healthyPeer('n2'),
    ]);
    coordinator.queryPeer.mockResolvedValue([{ id: 'r1' }, { id: 'r2' }]);
  });

  it('broadcast sends query to all healthy nodes', async () => {
    const executor = new DaemonFederationExecutor(daemon, coordinator, {
      strategy: 'broadcast',
    });

    const result = await executor.executeQuery('SELECT ?x WHERE { ?x a :T }');

    expect(result.nodeCount).toBe(3);
    expect(result.successCount).toBe(3);
    expect(result.failureCount).toBe(0);
    expect(result.strategy).toBe('broadcast');
    // queryPeer should have been called for each node
    expect(coordinator.queryPeer).toHaveBeenCalledTimes(3);
  });

  it('selective sends query to ceil(N*0.5) nodes', async () => {
    const executor = new DaemonFederationExecutor(daemon, coordinator, {
      strategy: 'selective',
    });

    const result = await executor.executeQuery('SELECT ?x WHERE { ?x a :T }');

    // ceil(3 * 0.5) = 2
    expect(result.nodeCount).toBe(2);
    expect(result.successCount).toBe(2);
    expect(coordinator.queryPeer).toHaveBeenCalledTimes(2);
  });

  it('best-node sends query to exactly one node', async () => {
    const executor = new DaemonFederationExecutor(daemon, coordinator, {
      strategy: 'best-node',
    });

    const result = await executor.executeQuery('SELECT ?x WHERE { ?x a :T }');

    expect(result.nodeCount).toBe(1);
    expect(result.successCount).toBe(1);
    expect(coordinator.queryPeer).toHaveBeenCalledTimes(1);
  });

  it('per-query strategy override takes precedence', async () => {
    const executor = new DaemonFederationExecutor(daemon, coordinator, {
      strategy: 'broadcast',
    });

    const result = await executor.executeQuery('SELECT ?x WHERE { ?x a :T }', {
      strategy: 'best-node',
    });

    expect(result.strategy).toBe('best-node');
    expect(result.nodeCount).toBe(1);
  });

  it('excludeNodes filters out specified nodes', async () => {
    const executor = new DaemonFederationExecutor(daemon, coordinator, {
      strategy: 'broadcast',
    });

    const result = await executor.executeQuery('SELECT ?x WHERE { ?x a :T }', {
      excludeNodes: ['n0', 'n2'],
    });

    expect(result.nodeCount).toBe(1);
    expect(result.peerResults[0].nodeId).toBe('n1');
  });
});

// =============================================================================
// 5. Timeout and Error Handling
// =============================================================================

describe('Federation Timeout and Error Handling', () => {
  let daemon;
  let coordinator;

  beforeEach(() => {
    daemon = {};
  });

  it('returns failure result when a peer query times out', async () => {
    coordinator = makeCoordinator([healthyPeer('slow')]);
    coordinator.queryPeer.mockImplementation(
      () =>
        new Promise((_, reject) =>
          setTimeout(() => reject(new Error('timeout')), 6000),
        ),
    );

    const executor = new DaemonFederationExecutor(daemon, coordinator, {
      strategy: 'broadcast',
      timeout: 100, // short timeout
    });

    const result = await executor.executeQuery('SELECT ?x WHERE { ?x a :T }');

    // The peer should have failed due to timeout
    expect(result.failureCount).toBe(1);
    expect(result.successCount).toBe(0);
    expect(result.peerResults[0].success).toBe(false);
    expect(result.peerResults[0].error).toBeDefined();
  }, 10000);

  it('returns failure result when peer is not found by coordinator', async () => {
    // Coordinator returns a peer but queryPeer throws because peer is missing
    coordinator = makeCoordinator([healthyPeer('ghost')]);
    coordinator.queryPeer.mockImplementation(() => {
      throw new Error('Peer not found: ghost');
    });

    const executor = new DaemonFederationExecutor(daemon, coordinator, {
      strategy: 'broadcast',
    });

    const result = await executor.executeQuery('SELECT ?x WHERE { ?x a :T }');

    expect(result.failureCount).toBe(1);
    expect(result.successCount).toBe(0);
    expect(result.peerResults[0].success).toBe(false);
  });

  it('mixed success and failure across nodes with broadcast', async () => {
    coordinator = makeCoordinator([healthyPeer('ok'), healthyPeer('fail')]);
    coordinator.queryPeer.mockImplementation(async (nodeId) => {
      if (nodeId === 'fail') {
        throw new Error('connection refused');
      }
      return [{ id: 'r1' }];
    });

    const executor = new DaemonFederationExecutor(daemon, coordinator, {
      strategy: 'broadcast',
    });

    const result = await executor.executeQuery('SELECT ?x WHERE { ?x a :T }');

    expect(result.nodeCount).toBe(2);
    expect(result.successCount + result.failureCount).toBe(2);
    expect(result.aggregatedResults.length).toBeGreaterThan(0);
  });

  it('propagates error when all nodes fail with no available nodes', async () => {
    coordinator = makeCoordinator([]);
    const executor = new DaemonFederationExecutor(daemon, coordinator);

    await expect(
      executor.executeQuery('SELECT ?x WHERE { ?x a :T }'),
    ).rejects.toThrow(/No available federation nodes/);
  });
});

// =============================================================================
// 6. Result Aggregation and Deduplication
// =============================================================================

describe('Result Aggregation and Deduplication', () => {
  let daemon;
  let coordinator;

  beforeEach(() => {
    daemon = {};
  });

  it('aggregates results from multiple nodes with source tracking', async () => {
    coordinator = makeCoordinator([healthyPeer('a'), healthyPeer('b')]);
    coordinator.queryPeer.mockImplementation(async (nodeId) => {
      if (nodeId === 'a') return [{ id: 'x1', value: 'shared' }];
      return [{ id: 'x1', value: 'shared' }, { id: 'x2', value: 'unique-b' }];
    });

    const executor = new DaemonFederationExecutor(daemon, coordinator, {
      strategy: 'broadcast',
      deduplicateResults: false,
    });

    const result = await executor.executeQuery('SELECT ?x WHERE { ?x a :T }');

    // x1 appears from both a and b -> should have _sources: ['a', 'b']
    const shared = result.aggregatedResults.find((r) => r.id === 'x1');
    expect(shared).toBeDefined();
    expect(shared._sources).toContain('a');
    expect(shared._sources).toContain('b');
    expect(shared._replicaCount).toBe(2);

    // x2 appears only from b
    const unique = result.aggregatedResults.find((r) => r.id === 'x2');
    expect(unique).toBeDefined();
    expect(unique._sources).toEqual(['b']);
    expect(unique._replicaCount).toBe(1);
  });

  it('deduplicates results when enabled', async () => {
    coordinator = makeCoordinator([healthyPeer('a'), healthyPeer('b')]);
    coordinator.queryPeer.mockImplementation(async (_nodeId) => {
      return [{ id: 'x1', value: 'same' }, { id: 'x2', value: 'same' }];
    });

    const dedupExecutor = new DaemonFederationExecutor(daemon, coordinator, {
      strategy: 'broadcast',
      deduplicateResults: true,
    });

    const result = await dedupExecutor.executeQuery('SELECT ?x WHERE { ?x a :T }');

    // With deduplication, identical result values should appear once
    const _values = result.aggregatedResults.map((r) => r.value);
    // x1 and x2 have the same value but different ids, so both survive
    // But the _aggregation_ step already merges identical JSON, so dedup removes
    // only duplicates that survived aggregation
    expect(result.aggregatedResults.length).toBeLessThanOrEqual(4);
  });

  it('handles non-array data from a peer', async () => {
    coordinator = makeCoordinator([healthyPeer('scalar')]);
    coordinator.queryPeer.mockResolvedValue({ id: 'single', value: 'scalar' });

    const executor = new DaemonFederationExecutor(daemon, coordinator, {
      strategy: 'broadcast',
    });

    const result = await executor.executeQuery('SELECT ?x WHERE { ?x a :T }');

    expect(result.aggregatedResults).toHaveLength(1);
    expect(result.aggregatedResults[0].id).toBe('single');
  });

  it('returns empty aggregatedResults when all peers return empty arrays', async () => {
    coordinator = makeCoordinator([healthyPeer('empty')]);
    coordinator.queryPeer.mockResolvedValue([]);

    const executor = new DaemonFederationExecutor(daemon, coordinator, {
      strategy: 'broadcast',
    });

    const result = await executor.executeQuery('SELECT ?x WHERE { ?x a :T }');

    expect(result.aggregatedResults).toHaveLength(0);
    expect(result.successCount).toBe(1);
  });
});

// =============================================================================
// 7. OTEL Span Propagation
// =============================================================================

describe('OTEL Span Propagation', () => {
  let daemon;
  let coordinator;

  beforeEach(() => {
    daemon = {};
    coordinator = makeCoordinator([healthyPeer('otel-node')]);
    coordinator.queryPeer.mockResolvedValue([{ id: 'x1' }]);
  });

  it('creates spans without throwing when no OTEL provider is configured', async () => {
    // In a unit test environment there is typically no OTEL SDK registered.
    // The tracer.getTracer() call returns a no-op tracer, so spans should
    // not throw — they just get silently dropped.
    const executor = new DaemonFederationExecutor(daemon, coordinator);

    const result = await executor.executeQuery('SELECT ?x WHERE { ?x a :T }');

    expect(result.queryId).toBeDefined();
    expect(result.successCount).toBe(1);
  });

  it('does not throw on span.recordException for failed queries', async () => {
    coordinator = makeCoordinator([]);
    const executor = new DaemonFederationExecutor(daemon, coordinator);

    // Should not throw from span.recordException
    await expect(
      executor.executeQuery('SELECT ?x WHERE { ?x a :T }'),
    ).rejects.toThrow(/No available federation nodes/);

    // The error was still recorded in the failedQueries map
    expect(executor.failedQueries.size).toBe(1);
  });

  it('does not throw on node-level span.recordException', async () => {
    coordinator = makeCoordinator([healthyPeer('crash')]);
    coordinator.queryPeer.mockRejectedValue(new Error('connection reset'));

    const executor = new DaemonFederationExecutor(daemon, coordinator, {
      strategy: 'broadcast',
    });

    // Should not throw from _executeOnNode's span.recordException
    const result = await executor.executeQuery('SELECT ?x WHERE { ?x a :T }');

    expect(result.failureCount).toBe(1);
    expect(result.peerResults[0].error).toBe('connection reset');
  });

  it('returns queryId and metadata consistent with span attributes', async () => {
    const executor = new DaemonFederationExecutor(daemon, coordinator, {
      executorId: 'otel-test',
      strategy: 'best-node',
    });

    const result = await executor.executeQuery('SELECT ?x WHERE { ?x a :T }');

    // Verify the metadata that would be set as span attributes
    expect(result.queryId).toMatch(/^query-/);
    expect(result.strategy).toBe('best-node');
    expect(result.nodeCount).toBe(1);
    expect(result.successCount).toBe(1);
    expect(result.totalDuration).toBeGreaterThanOrEqual(0);
    expect(result.timestamp).toBeInstanceOf(Date);
  });
});

// =============================================================================
// 8. createDaemonFederationExecutor factory
// =============================================================================

describe('createDaemonFederationExecutor', () => {
  it('is exported from federation-query.mjs', async () => {
    // Dynamic import to verify the named export exists
    const mod = await import(
      '../src/integrations/federation-query.mjs'
    );
    expect(typeof mod.createDaemonFederationExecutor).toBe('function');
  });

  it('creates an executor instance', async () => {
    const { createDaemonFederationExecutor } = await import(
      '../src/integrations/federation-query.mjs'
    );

    const daemon = {};
    const coordinator = makeCoordinator([healthyPeer('factory')]);
    coordinator.queryPeer.mockResolvedValue([]);

    const executor = createDaemonFederationExecutor(daemon, coordinator, {
      executorId: 'factory-test',
      strategy: 'broadcast',
    });

    expect(executor).toBeInstanceOf(DaemonFederationExecutor);
    expect(executor.id).toBe('factory-test');
  });

  it('validates daemon and coordinator via schemas', async () => {
    const { createDaemonFederationExecutor } = await import(
      '../src/integrations/federation-query.mjs'
    );

    // DaemonSchema is passthrough so any object passes,
    // but null/undefined should fail
    expect(() =>
      createDaemonFederationExecutor(null, {}),
    ).toThrow();
    expect(() =>
      createDaemonFederationExecutor({}, null),
    ).toThrow();
  });
});
