/**
 * @file Daemon Federation Query Integration
 * @module @unrdf/daemon/integrations/federation-query
 * @description Distributed SPARQL query execution across federated Raft nodes
 * with intelligent scheduling, result aggregation, and failure recovery
 */

import { trace, SpanStatusCode } from '@opentelemetry/api';
import {
  QueryStatsSchema,
  FederationExecutorConfigSchema,
  ExecuteQueryOptionsSchema,
  SparqlQuerySchema,
  QueryIdSchema,
  NodeIdSchema,
  DaemonSchema,
  FederationCoordinatorSchema,
} from './federation-query-schemas.mjs';
import {
  selectNodes,
  updateNodeMetrics,
} from './federation-node-selection.mjs';

const tracer = trace.getTracer('@unrdf/daemon-federation');

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
   */
  async executeQuery(sparqlQuery, options = {}) {
    // Validate inputs
    const validatedQuery = SparqlQuerySchema.parse(sparqlQuery);
    const validatedOptions = ExecuteQueryOptionsSchema.parse(options) || {};

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
      // Determine execution strategy
      const strategy = validatedOptions.strategy || this.config.strategy;
      const timeout = validatedOptions.timeout || this.config.timeout;
      const excludeNodes = validatedOptions.excludeNodes || [];

      span.setAttributes({
        'query.strategy': strategy,
        'query.timeout': timeout,
        'query.excludeNodes': excludeNodes.length,
      });

      // Get available nodes for federation
      const availableNodes = selectNodes(this.coordinator, strategy, excludeNodes, this.nodeMetrics);

      if (availableNodes.length === 0) {
        throw new Error('No available federation nodes for query execution');
      }

      span.setAttributes({
        'query.nodeCount': availableNodes.length,
      });

      // Execute query across selected nodes
      const executionPromises = availableNodes.map(nodeId =>
        this._executeOnNode(queryId, nodeId, validatedQuery, timeout).catch(error => ({
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
        sparql: validatedQuery,
        strategy,
        nodeCount: availableNodes.length,
        successCount,
        failureCount,
        totalDuration,
        startTime,
        endTime: Date.now(),
      };

      this.queryStats.set(queryId, stats);
      updateNodeMetrics(this.nodeMetrics, peerResults);

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
   * Get query statistics
   *
   * @param {string} [queryId] - Optional query ID to get specific stats
   * @returns {Object|Map} Statistics for query or all queries
   */
  getStats(queryId) {
    if (queryId !== undefined) {
      const validated = QueryIdSchema.parse(queryId);
      return this.queryStats.get(validated);
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
    if (nodeId !== undefined) {
      const validated = NodeIdSchema.parse(nodeId);
      return this.nodeMetrics.get(validated);
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
  DaemonSchema.parse(daemon);
  FederationCoordinatorSchema.parse(federationCoordinator);
  const validatedOptions = FederationExecutorConfigSchema.parse(options);

  return new DaemonFederationExecutor(daemon, federationCoordinator, validatedOptions);
}

export {
  FederationExecutorConfigSchema,
  QueryStatsSchema,
  ExecuteQueryOptionsSchema,
  SparqlQuerySchema,
  QueryIdSchema,
  NodeIdSchema,
  DaemonSchema,
  FederationCoordinatorSchema,
};
