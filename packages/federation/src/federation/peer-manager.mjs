/**
 * @file Peer Manager - Registration and health tracking for federation peers
 * @module federation/peer-manager
 */

import { z } from 'zod';

/**
 * @typedef {Object} PeerInfo
 * @property {string} id - Unique peer identifier
 * @property {string} endpoint - HTTP endpoint URL for the peer
 * @property {number} registeredAt - Timestamp when peer was registered
 * @property {number} lastSeen - Timestamp of last successful health check
 * @property {'healthy' | 'degraded' | 'unreachable'} status - Current health status
 * @property {Object} [metadata] - Optional peer metadata
 */

/* ========================================================================= */
/* Zod Schemas                                                              */
/* ========================================================================= */

export const PeerConfigSchema = z.object({
  id: z.string().min(1, 'Peer ID must not be empty'),
  endpoint: z.string().url('Peer endpoint must be a valid URL'),
  metadata: z.record(z.any()).optional(),
});

export const PeerInfoSchema = PeerConfigSchema.extend({
  registeredAt: z.number().positive(),
  lastSeen: z.number().positive(),
  status: z.enum(['healthy', 'degraded', 'unreachable']),
});

/* ========================================================================= */
/* Peer Manager                                                             */
/* ========================================================================= */

/**
 * Create a peer manager for tracking federation peers.
 *
 * @returns {Object} Peer manager instance
 */
export function createPeerManager() {
  /** @type {Map<string, PeerInfo>} */
  const peers = new Map();

  return {
    /**
     * Register a new peer or update existing peer.
     *
     * @param {string} id - Unique peer identifier
     * @param {string} endpoint - HTTP endpoint URL
     * @param {Object} [metadata] - Optional metadata
     * @returns {PeerInfo} Registered peer info
     */
    registerPeer(id, endpoint, metadata = {}) {
      const config = PeerConfigSchema.parse({ id, endpoint, metadata });

      const now = Date.now();
      const peerInfo = {
        id: config.id,
        endpoint: config.endpoint,
        metadata: config.metadata || {},
        registeredAt: peers.has(id) ? peers.get(id).registeredAt : now,
        lastSeen: now,
        status: 'healthy',
      };

      peers.set(id, peerInfo);
      return peerInfo;
    },

    /**
     * Unregister a peer.
     *
     * @param {string} id - Peer identifier to remove
     * @returns {boolean} True if peer was removed, false if not found
     */
    unregisterPeer(id) {
      return peers.delete(id);
    },

    /**
     * Get information about a specific peer.
     *
     * @param {string} id - Peer identifier
     * @returns {PeerInfo | null} Peer information or null if not found
     */
    getPeer(id) {
      return peers.get(id) || null;
    },

    /**
     * List all registered peers.
     *
     * @param {Object} [options] - Filtering options
     * @param {'healthy' | 'degraded' | 'unreachable'} [options.status] - Filter by status
     * @returns {PeerInfo[]} Array of peer information
     */
    listPeers(options = {}) {
      const allPeers = Array.from(peers.values());

      if (options.status) {
        return allPeers.filter(peer => peer.status === options.status);
      }

      return allPeers;
    },

    /**
     * Check peer health with a simple HTTP request.
     *
     * @param {string} id - Peer identifier
     * @param {number} [timeout=5000] - Request timeout in milliseconds
     * @returns {Promise<boolean>} True if peer is reachable
     */
    async ping(id, timeout = 5000) {
      const peer = peers.get(id);
      if (!peer) {
        return false;
      }

      try {
        const controller = new AbortController();
        const timeoutId = setTimeout(() => controller.abort(), timeout);

        const response = await fetch(peer.endpoint, {
          method: 'HEAD',
          signal: controller.signal,
        });

        clearTimeout(timeoutId);

        const isHealthy = response.ok;
        const now = Date.now();

        peer.lastSeen = now;
        peer.status = isHealthy ? 'healthy' : 'degraded';

        return isHealthy;
      } catch (error) {
        peer.status = 'unreachable';
        return false;
      }
    },

    /**
     * Update peer status manually.
     *
     * @param {string} id - Peer identifier
     * @param {'healthy' | 'degraded' | 'unreachable'} status - New status
     * @returns {boolean} True if status was updated
     */
    updateStatus(id, status) {
      const peer = peers.get(id);
      if (!peer) {
        return false;
      }

      peer.status = status;
      peer.lastSeen = Date.now();
      return true;
    },

    /**
     * Clear all registered peers.
     *
     * @returns {void}
     */
    clear() {
      peers.clear();
    },

    /**
     * Get count of registered peers.
     *
     * @returns {number} Number of registered peers
     */
    size() {
      return peers.size;
    },
  };
}
