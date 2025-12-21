/**
 * @file Federation Coordinator - Orchestrate distributed queries across peers
 * @module federation/coordinator
 */

import { trace } from '@opentelemetry/api';
import { z } from 'zod';
import { createPeerManager } from './peer-manager.mjs';
import {
  executeDistributedQuery,
  executeFederatedQuery,
  routeQuery,
} from './distributed-query.mjs';
import { recordQuery, recordError, updatePeerMetrics, trackConcurrentQuery } from './metrics.mjs';

const tracer = trace.getTracer('@unrdf/federation');

/**
 * @typedef {Object} CoordinatorConfig
 * @property {Array<{id: string, endpoint: string, metadata?: Object}>} [peers] - Initial peers
 * @property {'broadcast' | 'selective' | 'first-available'} [strategy='broadcast'] - Query routing strategy
 * @property {number} [timeout=30000] - Default query timeout in milliseconds
 * @property {number} [healthCheckInterval=60000] - Health check interval in milliseconds
 */

/**
 * @typedef {Object} FederationCoordinator
 * @property {function(string, string, Object=): Promise} addPeer - Add a peer to the federation
 * @property {function(string): boolean} removePeer - Remove a peer from the federation
 * @property {function(string): Object | null} getPeer - Get peer information
 * @property {function(Object=): Array} listPeers - List all peers
 * @property {function(string, Object=): Promise} query - Execute a federated query
 * @property {function(string, string, Object=): Promise} queryPeer - Query a specific peer
 * @property {function(): Promise} healthCheck - Run health checks on all peers
 * @property {function(): void} startHealthChecks - Start periodic health checks
 * @property {function(): void} stopHealthChecks - Stop periodic health checks
 * @property {function(): void} destroy - Destroy coordinator and clean up resources
 * @property {function(): Object} getStats - Get federation statistics
 */

/* ========================================================================= */
/* Zod Schemas                                                              */
/* ========================================================================= */

export const CoordinatorConfigSchema = z.object({
  peers: z
    .array(
      z.object({
        id: z.string(),
        endpoint: z.string().url(),
        metadata: z.record(z.string(), z.unknown()).optional(),
      })
    )
    .optional()
    .default([]),
  strategy: z.enum(['broadcast', 'selective', 'first-available']).optional().default('broadcast'),
  timeout: z.number().positive().optional().default(30000),
  healthCheckInterval: z.number().positive().optional().default(60000),
});

/* ========================================================================= */
/* Coordinator Factory                                                      */
/* ========================================================================= */

/**
 * Create a federation coordinator.
 *
 * @param {CoordinatorConfig} [config] - Coordinator configuration
 * @returns {FederationCoordinator} Federation coordinator instance
 */
export function createCoordinator(config = {}) {
  const validatedConfig = CoordinatorConfigSchema.parse(config);
  const peerManager = createPeerManager();

  let healthCheckTimer = null;
  let queryCount = 0;
  let errorCount = 0;

  // Register initial peers
  for (const peer of validatedConfig.peers) {
    peerManager.registerPeer(peer.id, peer.endpoint, peer.metadata);
  }

  return {
    /**
     * Add a peer to the federation.
     *
     * @param {string} id - Peer identifier
     * @param {string} endpoint - Peer endpoint URL
     * @param {Object} [metadata] - Optional peer metadata
     * @returns {Promise<Object>} Registered peer information
     */
    async addPeer(id, endpoint, metadata = {}) {
      const span = tracer.startSpan('coordinator.addPeer');
      try {
        span.setAttributes({
          'peer.id': id,
          'peer.endpoint': endpoint,
        });

        const peer = peerManager.registerPeer(id, endpoint, metadata);

        // Verify peer is reachable
        const isHealthy = await peerManager.ping(id);
        if (!isHealthy) {
          console.warn(`Peer ${id} registered but is not currently reachable`);
        }

        span.setAttributes({
          'peer.healthy': isHealthy,
          'peer.status': peer.status,
        });

        return peer;
      } catch (error) {
        span.recordException(error);
        throw error;
      } finally {
        span.end();
      }
    },

    /**
     * Remove a peer from the federation.
     *
     * @param {string} id - Peer identifier
     * @returns {boolean} True if peer was removed
     */
    removePeer(id) {
      const span = tracer.startSpan('coordinator.removePeer');
      try {
        span.setAttributes({
          'peer.id': id,
        });

        const removed = peerManager.unregisterPeer(id);
        span.setAttributes({
          'peer.removed': removed,
        });

        return removed;
      } catch (error) {
        span.recordException(error);
        throw error;
      } finally {
        span.end();
      }
    },

    /**
     * Get information about a specific peer.
     *
     * @param {string} id - Peer identifier
     * @returns {Object | null} Peer information
     */
    getPeer(id) {
      return peerManager.getPeer(id);
    },

    /**
     * List all registered peers.
     *
     * @param {Object} [options] - Filtering options
     * @returns {Array} Array of peer information
     */
    listPeers(options = {}) {
      return peerManager.listPeers(options);
    },

    /**
     * Execute a federated query across the federation.
     *
     * @param {string} sparqlQuery - SPARQL query string
     * @param {Object} [options] - Query options
     * @param {string} [options.strategy] - Override default routing strategy
     * @param {number} [options.timeout] - Override default timeout
     * @param {string} [options.format='json'] - Response format
     * @returns {Promise<Object>} Aggregated query results
     */
    async query(sparqlQuery, options = {}) {
      const span = tracer.startSpan('coordinator.query');
      const endConcurrent = trackConcurrentQuery();

      try {
        queryCount++;

        const strategy = options.strategy || validatedConfig.strategy;
        const timeout = options.timeout || validatedConfig.timeout;

        const allPeers = peerManager.listPeers({ status: 'healthy' });

        span.setAttributes({
          'query.strategy': strategy,
          'query.timeout': timeout,
          'peers.healthy': allPeers.length,
          'query.length': sparqlQuery.length,
        });

        if (allPeers.length === 0) {
          errorCount++;
          span.setAttributes({
            'query.error': 'no_healthy_peers',
          });
          return {
            success: false,
            results: [],
            peerResults: [],
            totalDuration: 0,
            successCount: 0,
            failureCount: 0,
            error: 'No healthy peers available',
          };
        }

        const targetPeers = routeQuery(allPeers, sparqlQuery, strategy);
        span.setAttributes({
          'peers.target': targetPeers.length,
        });

        const result = await executeDistributedQuery(targetPeers, sparqlQuery, {
          timeout,
          format: options.format,
          strategy: options.executionStrategy || 'parallel',
        });

        if (!result.success) {
          errorCount++;
          // Record failed queries
          for (const peerResult of result.peerResults || []) {
            if (!peerResult.success) {
              recordError(peerResult.peerId, 'query_failed');
            }
          }
        } else {
          // Record successful queries
          for (const peerResult of result.peerResults || []) {
            if (peerResult.success) {
              recordQuery(peerResult.peerId, peerResult.duration || 0, strategy);
            } else {
              recordError(peerResult.peerId, 'query_failed');
            }
          }
        }

        // Update peer metrics
        updatePeerMetrics(this.getStats());

        span.setAttributes({
          'query.success': result.success,
          'query.results': result.results?.length || 0,
          'query.duration': result.totalDuration || 0,
          'query.successCount': result.successCount || 0,
          'query.failureCount': result.failureCount || 0,
        });

        return result;
      } catch (error) {
        errorCount++;
        span.recordException(error);
        span.setAttributes({
          'query.error': error.message,
        });
        const targetPeers = peerManager.listPeers({ status: 'healthy' });
        // Record errors for all target peers
        for (const peer of targetPeers) {
          recordError(peer.id, 'execution_error');
        }
        return {
          success: false,
          results: [],
          peerResults: [],
          totalDuration: 0,
          successCount: 0,
          failureCount: targetPeers.length,
          error: error.message,
        };
      } finally {
        endConcurrent();
        span.end();
      }
    },

    /**
     * Execute a query against a specific peer.
     *
     * @param {string} peerId - Peer identifier
     * @param {string} sparqlQuery - SPARQL query string
     * @param {Object} [options] - Query options
     * @returns {Promise<Object>} Query result
     */
    async queryPeer(peerId, sparqlQuery, options = {}) {
      const endConcurrent = trackConcurrentQuery();

      try {
        queryCount++;

        const peer = peerManager.getPeer(peerId);
        if (!peer) {
          errorCount++;
          recordError(peerId, 'peer_not_found');
          return {
            success: false,
            data: null,
            error: `Peer ${peerId} not found`,
            duration: 0,
            peerId,
          };
        }

        try {
          const result = await executeFederatedQuery(peer.id, peer.endpoint, sparqlQuery, {
            timeout: options.timeout || validatedConfig.timeout,
            format: options.format,
          });

          if (!result.success) {
            errorCount++;
            // Check if error indicates connection failure (unreachable) vs query error (degraded)
            const isConnectionError = result.error &&
              (result.error.includes('Connection refused') ||
               result.error.includes('timeout') ||
               result.error.includes('ECONNREFUSED') ||
               result.error.includes('AbortError'));

            peerManager.updateStatus(peerId, isConnectionError ? 'unreachable' : 'degraded');
            recordError(peerId, 'query_failed');
          } else {
            peerManager.updateStatus(peerId, 'healthy');
            recordQuery(peerId, result.duration || 0, 'direct');
          }

          // Update peer metrics
          updatePeerMetrics(this.getStats());

          return result;
        } catch (error) {
          errorCount++;
          peerManager.updateStatus(peerId, 'unreachable');
          recordError(peerId, 'execution_error');
          return {
            success: false,
            data: null,
            error: error.message,
            duration: 0,
            peerId,
          };
        }
      } finally {
        endConcurrent();
      }
    },

    /**
     * Run health checks on all peers.
     *
     * @returns {Promise<Object>} Health check results
     */
    async healthCheck() {
      const span = tracer.startSpan('coordinator.healthCheck');
      try {
        const peers = peerManager.listPeers();
        span.setAttributes({
          'peer.count': peers.length,
        });

        const results = await Promise.all(
          peers.map(async peer => {
            const isHealthy = await peerManager.ping(peer.id);
            return {
              id: peer.id,
              healthy: isHealthy,
              status: peer.status,
            };
          })
        );

        const healthyCount = results.filter(r => r.healthy).length;
        const degradedCount = results.filter(r => r.status === 'degraded').length;
        const unreachableCount = results.filter(r => r.status === 'unreachable').length;

        span.setAttributes({
          'peer.healthy': healthyCount,
          'peer.degraded': degradedCount,
          'peer.unreachable': unreachableCount,
          'error.count': errorCount,
        });

        return {
          totalPeers: peers.length,
          healthyPeers: healthyCount,
          degradedPeers: degradedCount,
          unreachablePeers: unreachableCount,
          results,
        };
      } catch (error) {
        span.recordException(error);
        throw error;
      } finally {
        span.end();
      }
    },

    /**
     * Start periodic health checks.
     *
     * @returns {void}
     */
    startHealthChecks() {
      if (healthCheckTimer) {
        return;
      }

      healthCheckTimer = setInterval(async () => {
        try {
          await this.healthCheck();
        } catch (error) {
          // Don't crash timer loop on health check errors
          console.error('Health check error:', error.message);
        }
      }, validatedConfig.healthCheckInterval);
    },

    /**
     * Stop periodic health checks.
     *
     * @returns {void}
     */
    stopHealthChecks() {
      if (healthCheckTimer) {
        clearInterval(healthCheckTimer);
        healthCheckTimer = null;
      }
    },

    /**
     * Destroy the coordinator and clean up resources.
     * Call this when shutting down to prevent memory leaks.
     *
     * @returns {void}
     */
    destroy() {
      if (healthCheckTimer) {
        clearInterval(healthCheckTimer);
        healthCheckTimer = null;
      }
    },

    /**
     * Get federation statistics.
     *
     * @returns {Object} Statistics
     */
    getStats() {
      const peers = peerManager.listPeers();
      return {
        totalPeers: peers.length,
        healthyPeers: peers.filter(p => p.status === 'healthy').length,
        degradedPeers: peers.filter(p => p.status === 'degraded').length,
        unreachablePeers: peers.filter(p => p.status === 'unreachable').length,
        totalQueries: queryCount,
        totalErrors: errorCount,
        errorRate: queryCount > 0 ? errorCount / queryCount : 0,
      };
    },
  };
}
