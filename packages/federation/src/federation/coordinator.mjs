/**
 * @file Federation Coordinator - Orchestrate distributed queries across peers
 * @module federation/coordinator
 */

import { z } from 'zod';
import { createPeerManager } from './peer-manager.mjs';
import {
  executeDistributedQuery,
  executeFederatedQuery,
  routeQuery,
} from './distributed-query.mjs';

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
        metadata: z.record(z.any()).optional(),
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
      const peer = peerManager.registerPeer(id, endpoint, metadata);

      // Verify peer is reachable
      const isHealthy = await peerManager.ping(id);
      if (!isHealthy) {
        console.warn(`Peer ${id} registered but is not currently reachable`);
      }

      return peer;
    },

    /**
     * Remove a peer from the federation.
     *
     * @param {string} id - Peer identifier
     * @returns {boolean} True if peer was removed
     */
    removePeer(id) {
      return peerManager.unregisterPeer(id);
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
      queryCount++;

      const strategy = options.strategy || validatedConfig.strategy;
      const timeout = options.timeout || validatedConfig.timeout;

      const allPeers = peerManager.listPeers({ status: 'healthy' });

      if (allPeers.length === 0) {
        errorCount++;
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

      try {
        const result = await executeDistributedQuery(targetPeers, sparqlQuery, {
          timeout,
          format: options.format,
          strategy: options.executionStrategy || 'parallel',
        });

        if (!result.success) {
          errorCount++;
        }

        return result;
      } catch (error) {
        errorCount++;
        return {
          success: false,
          results: [],
          peerResults: [],
          totalDuration: 0,
          successCount: 0,
          failureCount: targetPeers.length,
          error: error.message,
        };
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
      queryCount++;

      const peer = peerManager.getPeer(peerId);
      if (!peer) {
        errorCount++;
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
          peerManager.updateStatus(peerId, 'degraded');
        } else {
          peerManager.updateStatus(peerId, 'healthy');
        }

        return result;
      } catch (error) {
        errorCount++;
        peerManager.updateStatus(peerId, 'unreachable');
        return {
          success: false,
          data: null,
          error: error.message,
          duration: 0,
          peerId,
        };
      }
    },

    /**
     * Run health checks on all peers.
     *
     * @returns {Promise<Object>} Health check results
     */
    async healthCheck() {
      const peers = peerManager.listPeers();
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

      return {
        totalPeers: peers.length,
        healthyPeers: results.filter(r => r.healthy).length,
        degradedPeers: results.filter(r => r.status === 'degraded').length,
        unreachablePeers: results.filter(r => r.status === 'unreachable').length,
        results,
      };
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
        await this.healthCheck();
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
