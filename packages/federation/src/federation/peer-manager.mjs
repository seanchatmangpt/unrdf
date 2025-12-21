/**
 * @file Peer Manager - Registration and health tracking for federation peers
 * @module federation/peer-manager
 */

import { trace } from '@opentelemetry/api';
import { z } from 'zod';

const tracer = trace.getTracer('@unrdf/federation');

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
  metadata: z.record(z.string(), z.unknown()).optional(),
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
      const span = tracer.startSpan('peer-manager.registerPeer');
      try {
        span.setAttributes({
          'peer.id': id,
          'peer.endpoint': endpoint,
          'peer.isUpdate': peers.has(id),
        });

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

        span.setAttributes({
          'peer.total': peers.size,
        });

        return peerInfo;
      } catch (error) {
        span.recordException(error);
        throw error;
      } finally {
        span.end();
      }
    },

    /**
     * Unregister a peer.
     *
     * @param {string} id - Peer identifier to remove
     * @returns {boolean} True if peer was removed, false if not found
     */
    unregisterPeer(id) {
      const span = tracer.startSpan('peer-manager.unregisterPeer');
      try {
        span.setAttributes({
          'peer.id': id,
        });

        const removed = peers.delete(id);

        span.setAttributes({
          'peer.removed': removed,
          'peer.total': peers.size,
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
      const span = tracer.startSpan('peer-manager.ping');
      try {
        span.setAttributes({
          'peer.id': id,
          'ping.timeout': timeout,
        });

        const peer = peers.get(id);
        if (!peer) {
          span.setAttributes({
            'peer.found': false,
          });
          return false;
        }

        const controller = new AbortController();
        const timeoutId = setTimeout(() => controller.abort(), timeout);
        const startTime = Date.now();

        try {
          const response = await fetch(peer.endpoint, {
            method: 'HEAD',
            signal: controller.signal,
          });

          clearTimeout(timeoutId);

          const isHealthy = response.ok;
          const now = Date.now();
          const duration = now - startTime;

          peer.lastSeen = now;
          peer.status = isHealthy ? 'healthy' : 'degraded';

          span.setAttributes({
            'peer.found': true,
            'peer.healthy': isHealthy,
            'peer.status': peer.status,
            'ping.duration': duration,
            'http.status': response.status,
          });

          return isHealthy;
        } catch (error) {
          peer.status = 'unreachable';
          const duration = Date.now() - startTime;

          span.setAttributes({
            'peer.found': true,
            'peer.healthy': false,
            'peer.status': 'unreachable',
            'ping.duration': duration,
            'ping.error': error.message,
          });

          return false;
        }
      } catch (error) {
        span.recordException(error);
        throw error;
      } finally {
        span.end();
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
      const span = tracer.startSpan('peer-manager.updateStatus');
      try {
        span.setAttributes({
          'peer.id': id,
          'peer.status': status,
        });

        const peer = peers.get(id);
        if (!peer) {
          span.setAttributes({
            'peer.found': false,
          });
          return false;
        }

        const oldStatus = peer.status;
        peer.status = status;
        peer.lastSeen = Date.now();

        span.setAttributes({
          'peer.found': true,
          'peer.oldStatus': oldStatus,
        });

        return true;
      } catch (error) {
        span.recordException(error);
        throw error;
      } finally {
        span.end();
      }
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
