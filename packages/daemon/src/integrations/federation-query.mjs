/**
 * @file Daemon Federation Query Integration
 * @module @unrdf/daemon/integrations/federation-query
 * @description Distributed SPARQL query execution across federated Raft nodes
 * with intelligent scheduling, result aggregation, and failure recovery
 */

import { z } from 'zod';
import { trace, SpanStatusCode } from '@opentelemetry/api';

const tracer = trace.getTracer('@unrdf/daemon-federation');

/**
 * Query execution statistics
 */
const QueryStatsSchema = z.object({
  queryId: z.string(),
  sparql: z.string(),
  strategy: z.enum(['broadcast', 'selective', 'best-node']),
  nodeCount: z.number().int().nonnegative(),
  successCount: z.number().int().nonnegative(),
  failureCount: z.number().int().nonnegative(),
  totalDuration: z.number().nonnegative(),
  startTime: z.number(),
  endTime: z.number(),
  timestamp: z.date().default(() => new Date()),
});

/**
 * Federation executor configuration
 */
const FederationExecutorConfigSchema = z.object({
  executorId: z.string().min(1).default(() => `executor-${Date.now()}`),
  strategy: z.enum(['broadcast', 'selective', 'best-node']).default('selective'),
  timeout: z.number().positive().default(30000),
  maxRetries: z.number().int().min(1).default(2),
  deduplicateResults: z.boolean().default(true),
  enableNodeSelection: z.boolean().default(true),
  healthCheckThreshold: z.number().min(0).max(1).default(0.7),
});

/**
 * Distributed Federated Query Executor for Daemon
 *
 * Coordinates distributed SPARQL query execution across federated Raft nodes
 * with topology awareness, intelligent scheduling, and result consistency.
 *
 * @class DaemonFederationExecutor
 *
 * @example
 * const executor = new DaemonFederationExecutor(daemon, federationCoordinator, {
 *   strategy: 'selective',
 *   timeout: 30000,
 * });
 *
 * const results = await executor.executeQuery('SELECT ?x WHERE { ?x ?p ?o }');
 * console.log(`Got ${results.aggregatedResults.length} results from ${results.nodeCount} nodes`);
 */
export class DaemonFederationExecutor {
  /**
   * Create a federation query executor
   * @param {Object} daemon - Daemon instance
   * @param {Object} federationCoordinator - Federation coordinator
   * @param {Object} [options] - Executor options
   */
  constructor(daemon, federationCoordinator, options = {}) {
    const cfg = FederationExecutorConfigSchema.parse(options);

    this.id = cfg.executorId;
    this.daemon = daemon;
    this.coordinator = federationCoordinator;
    this.config = cfg;

    this.queryStats = new Map();
    this.nodeMetrics = new Map();
    this.resultCache = new Map();
    this.failedQueries = new Map();
  }

  /**
   * Execute a SPARQL query across federated nodes
   *
   * @param {string} sparqlQuery - SPARQL query string
   * @param {Object} [options] - Query execution options
   * @param {string} [options.strategy] - Override execution strategy
   * @param {number} [options.timeout] - Override timeout in milliseconds
   * @param {Array<string>} [options.excludeNodes] - Nodes to exclude from query
   * @returns {Promise<Object>} Aggregated query results with metadata
   * @throws {Error} If query execution fails on all nodes
   *
   * @example
   * const results = await executor.executeQuery(
   *   'SELECT ?person ?name WHERE { ?person foaf:name ?name }',
   *   { strategy: 'best-node', timeout: 10000 }
   * );
   */
  async executeQuery(sparqlQuery, options = {}) {
    const queryId = `query-${Date.now()}-${Math.random().toString(36).slice(2, 9)}`;
    const span = tracer.startSpan('federation.executeQuery', {
      attributes: {
        'query.id': queryId,
        'query.type': 'sparql',
        'executor.id': this.id,
      },
    });

    const startTime = Date.now();

    try {
      // Validate query
      if (!sparqlQuery || typeof sparqlQuery !== 'string' || sparqlQuery.trim().length === 0) {
        throw new Error('Invalid SPARQL query: must be non-empty string');
      }

      // Determine execution strategy
      const strategy = options.strategy || this.config.strategy;
      const timeout = options.timeout || this.config.timeout;
      const excludeNodes = options.excludeNodes || [];

      span.setAttributes({
        'query.strategy': strategy,
        'query.timeout': timeout,
        'query.excludeNodes': excludeNodes.length,
      });

      // Get available nodes for federation
      const availableNodes = this._selectNodes(strategy, excludeNodes);

      if (availableNodes.length === 0) {
        throw new Error('No available federation nodes for query execution');
      }

      span.setAttributes({
        'query.nodeCount': availableNodes.length,
      });

      // Execute query across selected nodes
      const executionPromises = availableNodes.map(nodeId =>
        this._executeOnNode(queryId, nodeId, sparqlQuery, timeout).catch(error => ({
          nodeId,
          success: false,
          error: error.message,
          duration: Date.now() - startTime,
        }))
      );

      // Race or parallel based on strategy
      let peerResults;

      if (strategy === 'best-node') {
        peerResults = await Promise.race(executionPromises).then(result => [result]);
      } else {
        peerResults = await Promise.allSettled(executionPromises).then(results =>
          results.map(r => (r.status === 'fulfilled' ? r.value : r.reason))
        );
      }

      // Aggregate results
      const aggregatedResults = this._aggregateResults(peerResults);

      // Deduplicate if configured
      const finalResults = this.config.deduplicateResults
        ? this._deduplicateResults(aggregatedResults)
        : aggregatedResults;

      // Record statistics
      const successCount = peerResults.filter(r => r.success).length;
      const failureCount = peerResults.length - successCount;
      const totalDuration = Date.now() - startTime;

      const stats = {
        queryId,
        sparql: sparqlQuery,
        strategy,
        nodeCount: availableNodes.length,
        successCount,
        failureCount,
        totalDuration,
        startTime,
        endTime: Date.now(),
      };

      this.queryStats.set(queryId, stats);
      this._updateNodeMetrics(peerResults);

      span.setAttributes({
        'query.successCount': successCount,
        'query.failureCount': failureCount,
        'query.resultCount': finalResults.length,
        'query.totalDuration': totalDuration,
      });

      return {
        queryId,
        aggregatedResults: finalResults,
        peerResults,
        nodeCount: availableNodes.length,
        successCount,
        failureCount,
        totalDuration,
        strategy,
        timestamp: new Date(),
      };
    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
      this.failedQueries.set(queryId, error.message);
      throw error;
    } finally {
      span.end();
    }
  }

  /**
   * Select nodes for query execution based on strategy
   *
   * @private
   * @param {string} strategy - Selection strategy
   * @param {Array<string>} excludeNodes - Nodes to exclude
   * @returns {Array<string>} Selected node IDs
   */
  _selectNodes(strategy, excludeNodes = []) {
    const peers = this.coordinator.listPeers?.() || [];
    const availableNodes = peers
      .filter(p => !excludeNodes.includes(p.id) && p.status === 'healthy')
      .map(p => p.id);

    if (availableNodes.length === 0) {
      return peers.filter(p => !excludeNodes.includes(p.id)).map(p => p.id);
    }

    switch (strategy) {
      case 'best-node': {
        // Select single best node based on metrics
        return [this._selectBestNode(availableNodes)];
      }

      case 'selective': {
        // Select top performing nodes (50%)
        const count = Math.max(1, Math.ceil(availableNodes.length * 0.5));
        return this._selectTopNodes(availableNodes, count);
      }

      case 'broadcast':
      default: {
        // Use all available nodes
        return availableNodes;
      }
    }
  }

  /**
   * Select single best node based on performance metrics
   *
   * @private
   * @param {Array<string>} nodeIds - Node IDs to choose from
   * @returns {string} Best node ID
   */
  _selectBestNode(nodeIds) {
    let bestNode = nodeIds[0];
    let bestScore = -Infinity;

    for (const nodeId of nodeIds) {
      const metrics = this.nodeMetrics.get(nodeId) || {
        queryCount: 0,
        successRate: 1.0,
        avgDuration: 0,
      };

      // Score = success rate / (1 + avgDuration)
      const score = metrics.successRate / (1 + metrics.avgDuration / 1000);

      if (score > bestScore) {
        bestScore = score;
        bestNode = nodeId;
      }
    }

    return bestNode;
  }

  /**
   * Select top performing nodes
   *
   * @private
   * @param {Array<string>} nodeIds - Node IDs to choose from
   * @param {number} count - Number of nodes to select
   * @returns {Array<string>} Selected node IDs
   */
  _selectTopNodes(nodeIds, count) {
    const scored = nodeIds.map(nodeId => {
      const metrics = this.nodeMetrics.get(nodeId) || {
        queryCount: 0,
        successRate: 1.0,
        avgDuration: 0,
      };

      const score = metrics.successRate / (1 + metrics.avgDuration / 1000);
      return { nodeId, score };
    });

    return scored.sort((a, b) => b.score - a.score).slice(0, count).map(s => s.nodeId);
  }

  /**
   * Execute query on a specific node
   *
   * @private
   * @param {string} queryId - Query identifier
   * @param {string} nodeId - Node ID to query
   * @param {string} sparqlQuery - SPARQL query
   * @param {number} timeout - Query timeout in milliseconds
   * @returns {Promise<Object>} Query result from node
   */
  async _executeOnNode(queryId, nodeId, sparqlQuery, timeout) {
    const span = tracer.startSpan('federation.executeOnNode', {
      attributes: {
        'query.id': queryId,
        'node.id': nodeId,
        'query.timeout': timeout,
      },
    });

    const startTime = Date.now();

    try {
      // Find peer endpoint
      const peers = this.coordinator.listPeers?.() || [];
      const peer = peers.find(p => p.id === nodeId);

      if (!peer) {
        throw new Error(`Peer not found: ${nodeId}`);
      }

      // Execute query via coordinator
      const result = await Promise.race([
        this.coordinator.queryPeer?.(nodeId, sparqlQuery),
        new Promise((_, reject) =>
          setTimeout(() => reject(new Error('Query timeout')), timeout)
        ),
      ]);

      const duration = Date.now() - startTime;

      span.setAttributes({
        'node.duration': duration,
        'result.success': true,
      });

      return {
        nodeId,
        success: true,
        data: result,
        duration,
        timestamp: Date.now(),
      };
    } catch (error) {
      const duration = Date.now() - startTime;

      span.recordException(error);
      span.setAttributes({
        'node.duration': duration,
        'result.success': false,
        'result.error': error.message,
      });

      return {
        nodeId,
        success: false,
        error: error.message,
        duration,
        timestamp: Date.now(),
      };
    } finally {
      span.end();
    }
  }

  /**
   * Aggregate results from multiple peers
   *
   * @private
   * @param {Array<Object>} peerResults - Results from each peer
   * @returns {Array<Object>} Aggregated results
   */
  _aggregateResults(peerResults) {
    const resultMap = new Map();

    for (const result of peerResults) {
      if (!result.success || !result.data) {
        continue;
      }

      // Handle different result formats
      const resultData = Array.isArray(result.data) ? result.data : [result.data];

      for (const item of resultData) {
        const key = JSON.stringify(item);
        if (!resultMap.has(key)) {
          resultMap.set(key, {
            value: item,
            sources: [result.nodeId],
          });
        } else {
          const existing = resultMap.get(key);
          if (!existing.sources.includes(result.nodeId)) {
            existing.sources.push(result.nodeId);
          }
        }
      }
    }

    return Array.from(resultMap.values()).map(entry => ({
      ...entry.value,
      _sources: entry.sources,
      _replicaCount: entry.sources.length,
    }));
  }

  /**
   * Deduplicate results based on content
   *
   * @private
   * @param {Array<Object>} results - Results to deduplicate
   * @returns {Array<Object>} Deduplicated results
   */
  _deduplicateResults(results) {
    const seen = new Set();
    const unique = [];

    for (const result of results) {
      // Remove internal metadata fields before hashing
      const { _sources, _replicaCount, ...cleanResult } = result;
      const key = JSON.stringify(cleanResult);

      if (!seen.has(key)) {
        seen.add(key);
        unique.push(result);
      }
    }

    return unique;
  }

  /**
   * Update node performance metrics
   *
   * @private
   * @param {Array<Object>} peerResults - Results from peer execution
   */
  _updateNodeMetrics(peerResults) {
    for (const result of peerResults) {
      const nodeId = result.nodeId;
      const existing = this.nodeMetrics.get(nodeId) || {
        queryCount: 0,
        successCount: 0,
        totalDuration: 0,
        successRate: 1.0,
        avgDuration: 0,
      };

      const isSuccess = result.success === true;

      existing.queryCount += 1;
      if (isSuccess) {
        existing.successCount += 1;
      }
      existing.totalDuration += result.duration || 0;
      existing.successRate = existing.successCount / existing.queryCount;
      existing.avgDuration = existing.totalDuration / existing.queryCount;

      this.nodeMetrics.set(nodeId, existing);
    }
  }

  /**
   * Get query statistics
   *
   * @param {string} [queryId] - Optional query ID to get specific stats
   * @returns {Object|Map} Statistics for query or all queries
   */
  getStats(queryId) {
    if (queryId) {
      return this.queryStats.get(queryId);
    }

    return {
      totalQueries: this.queryStats.size,
      stats: Array.from(this.queryStats.values()),
      nodeMetrics: Object.fromEntries(this.nodeMetrics),
      failedQueries: this.failedQueries.size,
    };
  }

  /**
   * Get node performance metrics
   *
   * @param {string} [nodeId] - Optional node ID
   * @returns {Object} Node metrics
   */
  getNodeMetrics(nodeId) {
    if (nodeId) {
      return this.nodeMetrics.get(nodeId);
    }

    return Object.fromEntries(this.nodeMetrics);
  }

  /**
   * Reset executor statistics
   */
  reset() {
    this.queryStats.clear();
    this.nodeMetrics.clear();
    this.resultCache.clear();
    this.failedQueries.clear();
  }

  /**
   * Get executor health status
   *
   * @returns {Object} Health information
   */
  getHealth() {
    const totalQueries = this.queryStats.size;
    const successfulQueries = Array.from(this.queryStats.values()).filter(
      s => s.successCount > 0
    ).length;

    const stats = Array.from(this.queryStats.values());
    const avgDuration = stats.length > 0 ? stats.reduce((sum, s) => sum + s.totalDuration, 0) / stats.length : 0;

    return {
      executorId: this.id,
      totalQueries,
      successfulQueries,
      failedQueries: this.failedQueries.size,
      nodeCount: this.nodeMetrics.size,
      averageQueryDuration: avgDuration,
      timestamp: new Date(),
    };
  }
}

/**
 * Create a daemon federation executor
 *
 * @param {Object} daemon - Daemon instance
 * @param {Object} federationCoordinator - Federation coordinator
 * @param {Object} [options] - Executor options
 * @returns {DaemonFederationExecutor} Executor instance
 *
 * @example
 * const executor = createDaemonFederationExecutor(daemon, coordinator, {
 *   strategy: 'selective',
 *   timeout: 30000,
 * });
 */
export function createDaemonFederationExecutor(daemon, federationCoordinator, options = {}) {
  return new DaemonFederationExecutor(daemon, federationCoordinator, options);
}

export { FederationExecutorConfigSchema, QueryStatsSchema };
