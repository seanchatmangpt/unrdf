/**
 * @file YAWL Realtime Client - Socket.io client for collaborative workflows
 * @module @unrdf/yawl-realtime/client
 *
 * @description
 * Real-time client that:
 * - Connects to YAWL Realtime Server
 * - Claims tasks optimistically with Lamport timestamps
 * - Receives real-time workflow updates
 * - Handles conflict resolution
 */

import { io } from 'socket.io-client';
import { z } from 'zod';

// =============================================================================
// Schemas
// =============================================================================

const ClientOptionsSchema = z.object({
  serverUrl: z.string().url().default('http://localhost:3000'),
  userId: z.string(),
  autoReconnect: z.boolean().default(true),
  reconnectionDelay: z.number().default(1000),
});

// =============================================================================
// YAWL Realtime Client
// =============================================================================

/**
 * Real-time client for collaborative YAWL workflows
 *
 * @example
 * ```javascript
 * import { YAWLRealtimeClient } from '@unrdf/yawl-realtime/client';
 *
 * const client = new YAWLRealtimeClient({
 *   serverUrl: 'http://localhost:3000',
 *   userId: 'alice@example.com',
 * });
 *
 * await client.connect();
 *
 * client.on('yawl:event', (event) => {
 *   console.log('Workflow event:', event);
 * });
 *
 * const result = await client.claimTask(caseId, workItemId);
 * ```
 */
export class YAWLRealtimeClient {
  /**
   * @param {Object} options - Client options
   */
  constructor(options) {
    const config = ClientOptionsSchema.parse(options);

    this.serverUrl = config.serverUrl;
    this.userId = config.userId;
    this.autoReconnect = config.autoReconnect;

    /** @type {import('socket.io-client').Socket|null} */
    this.socket = null;

    /** @type {Map<string, Function>} Event handlers */
    this.eventHandlers = new Map();

    /** @type {number} Lamport clock for this client */
    this.lamportClock = 0;

    /** @type {Set<string>} Currently held locks */
    this.heldLocks = new Set();

    /** @type {boolean} Connection state */
    this.connected = false;
  }

  /**
   * Get next Lamport timestamp
   * @returns {number}
   */
  getNextTimestamp() {
    this.lamportClock++;
    return this.lamportClock;
  }

  /**
   * Update Lamport clock based on received timestamp
   * @param {number} receivedTimestamp
   */
  updateClock(receivedTimestamp) {
    this.lamportClock = Math.max(this.lamportClock, receivedTimestamp) + 1;
  }

  /**
   * Connect to the server
   * @returns {Promise<void>}
   */
  async connect() {
    return new Promise((resolve, reject) => {
      this.socket = io(this.serverUrl, {
        reconnection: this.autoReconnect,
        reconnectionDelay: this.reconnectionDelay,
      });

      this.socket.on('connect', () => {
        console.log(`[YAWLRealtimeClient] Connected to ${this.serverUrl}`);
        this.connected = true;

        // Identify with server
        this.socket.emit('identify', { userId: this.userId });
      });

      this.socket.on('identified', (data) => {
        console.log(`[YAWLRealtimeClient] Identified as ${data.userId} (${data.socketId})`);
        resolve();
      });

      this.socket.on('disconnect', () => {
        console.log('[YAWLRealtimeClient] Disconnected from server');
        this.connected = false;
        this.heldLocks.clear();
      });

      this.socket.on('error', (error) => {
        console.error('[YAWLRealtimeClient] Socket error:', error);
        reject(error);
      });

      // Setup event forwarding
      this._setupEventForwarding();

      // Connection timeout
      setTimeout(() => {
        if (!this.connected) {
          reject(new Error('Connection timeout'));
        }
      }, 5000);
    });
  }

  /**
   * Setup event forwarding from socket to client handlers
   * @private
   */
  _setupEventForwarding() {
    // Forward all YAWL events
    this.socket.on('yawl:event', (event) => {
      // Update Lamport clock if event has timestamp
      if (event.timestamp) {
        this.updateClock(parseInt(event.timestamp) || 0);
      }

      this._emit('yawl:event', event);
      this._emit(event.type, event);
    });

    // Forward task lock/unlock events
    this.socket.on('task:locked', (data) => {
      this._emit('task:locked', data);
    });

    this.socket.on('task:unlocked', (data) => {
      this.heldLocks.delete(data.workItemId);
      this._emit('task:unlocked', data);
    });

    // Forward errors
    this.socket.on('error', (error) => {
      this._emit('error', error);
    });
  }

  /**
   * Claim a task optimistically
   * @param {string} caseId - Case ID
   * @param {string} workItemId - Work item ID
   * @param {Object} [options] - Claim options
   * @param {string} [options.expectedReceiptHash] - Expected receipt hash
   * @returns {Promise<{success: boolean, task?: Object, conflict?: Object}>}
   */
  async claimTask(caseId, workItemId, options = {}) {
    if (!this.connected) {
      throw new Error('Not connected to server');
    }

    return new Promise((resolve, reject) => {
      const timestamp = this.getNextTimestamp();

      this.socket.emit('task:claim', {
        caseId,
        workItemId,
        userId: this.userId,
        timestamp,
        expectedReceiptHash: options.expectedReceiptHash,
      });

      // Listen for response
      const timeout = setTimeout(() => {
        this.socket.off('task:claimed', handler);
        reject(new Error('Task claim timeout'));
      }, 5000);

      const handler = (response) => {
        if (response.workItemId === workItemId) {
          clearTimeout(timeout);
          this.socket.off('task:claimed', handler);

          if (response.success) {
            this.heldLocks.add(workItemId);
            console.log(`[YAWLRealtimeClient] Task claimed: ${workItemId}`);

            if (response.conflict) {
              console.log(`[YAWLRealtimeClient] Conflict resolved: ${response.conflict.resolution}`);
            }
          } else {
            console.log(`[YAWLRealtimeClient] Task claim failed: ${workItemId}`);
          }

          resolve(response);
        }
      };

      this.socket.on('task:claimed', handler);
    });
  }

  /**
   * Complete a claimed task
   * @param {string} caseId - Case ID
   * @param {string} workItemId - Work item ID
   * @param {Object} output - Task output
   * @param {Object} [options] - Completion options
   * @returns {Promise<{success: boolean, task?: Object, receipt?: Object}>}
   */
  async completeTask(caseId, workItemId, output, options = {}) {
    if (!this.connected) {
      throw new Error('Not connected to server');
    }

    if (!this.heldLocks.has(workItemId)) {
      throw new Error(`Task ${workItemId} not claimed by this client`);
    }

    return new Promise((resolve, reject) => {
      this.socket.emit('task:complete', {
        caseId,
        workItemId,
        userId: this.userId,
        output,
        receiptHash: options.receiptHash,
      });

      const timeout = setTimeout(() => {
        this.socket.off('task:completed', handler);
        reject(new Error('Task completion timeout'));
      }, 5000);

      const handler = (response) => {
        if (response.workItemId === workItemId) {
          clearTimeout(timeout);
          this.socket.off('task:completed', handler);

          if (response.success) {
            this.heldLocks.delete(workItemId);
            console.log(`[YAWLRealtimeClient] Task completed: ${workItemId}`);
          } else {
            console.log(`[YAWLRealtimeClient] Task completion failed: ${response.error}`);
          }

          resolve(response);
        }
      };

      this.socket.on('task:completed', handler);
    });
  }

  /**
   * Release a claimed task without completing
   * @param {string} caseId - Case ID
   * @param {string} workItemId - Work item ID
   * @returns {Promise<{success: boolean}>}
   */
  async releaseTask(caseId, workItemId) {
    if (!this.connected) {
      throw new Error('Not connected to server');
    }

    return new Promise((resolve, reject) => {
      this.socket.emit('task:release', {
        caseId,
        workItemId,
        userId: this.userId,
      });

      const timeout = setTimeout(() => {
        this.socket.off('task:released', handler);
        reject(new Error('Task release timeout'));
      }, 5000);

      const handler = (response) => {
        if (response.workItemId === workItemId) {
          clearTimeout(timeout);
          this.socket.off('task:released', handler);

          if (response.success) {
            this.heldLocks.delete(workItemId);
            console.log(`[YAWLRealtimeClient] Task released: ${workItemId}`);
          }

          resolve(response);
        }
      };

      this.socket.on('task:released', handler);
    });
  }

  /**
   * Request state synchronization
   * @param {string} caseId - Case ID
   * @returns {Promise<{state: Object, locks: Array}>}
   */
  async syncState(caseId) {
    if (!this.connected) {
      throw new Error('Not connected to server');
    }

    return new Promise((resolve, reject) => {
      this.socket.emit('state:sync', { caseId });

      const timeout = setTimeout(() => {
        this.socket.off('state:synced', handler);
        reject(new Error('State sync timeout'));
      }, 5000);

      const handler = (response) => {
        if (response.caseId === caseId) {
          clearTimeout(timeout);
          this.socket.off('state:synced', handler);
          resolve(response);
        }
      };

      this.socket.on('state:synced', handler);
    });
  }

  /**
   * Subscribe to events
   * @param {string} eventType - Event type
   * @param {Function} handler - Event handler
   * @returns {Function} Unsubscribe function
   */
  on(eventType, handler) {
    if (!this.eventHandlers.has(eventType)) {
      this.eventHandlers.set(eventType, new Set());
    }

    this.eventHandlers.get(eventType).add(handler);

    // Return unsubscribe function
    return () => {
      const handlers = this.eventHandlers.get(eventType);
      if (handlers) {
        handlers.delete(handler);
      }
    };
  }

  /**
   * Emit event to local handlers
   * @param {string} eventType - Event type
   * @param {Object} data - Event data
   * @private
   */
  _emit(eventType, data) {
    const handlers = this.eventHandlers.get(eventType);
    if (!handlers) return;

    for (const handler of handlers) {
      try {
        handler(data);
      } catch (error) {
        console.error(`[YAWLRealtimeClient] Error in event handler for ${eventType}:`, error);
      }
    }
  }

  /**
   * Disconnect from server
   * @returns {Promise<void>}
   */
  async disconnect() {
    if (this.socket) {
      this.socket.disconnect();
      this.connected = false;
      this.heldLocks.clear();
      console.log('[YAWLRealtimeClient] Disconnected');
    }
  }

  /**
   * Get client status
   * @returns {Object}
   */
  getStatus() {
    return {
      connected: this.connected,
      userId: this.userId,
      lamportClock: this.lamportClock,
      heldLocks: Array.from(this.heldLocks),
    };
  }
}

// =============================================================================
// Exports
// =============================================================================

export default YAWLRealtimeClient;
