/**
 * @file Multi-User Collaboration
 * @module @unrdf/spatial-kg/collaboration
 * @description Synchronize spatial graph state across multiple users
 */

import { CollaborationStateSchema } from './schemas.mjs';
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('@unrdf/spatial-kg/collaboration');

/**
 * Collaboration Manager
 */
export class CollaborationManager {
  /**
   * @param {Object} options - Collaboration options
   * @param {string} options.userId - Current user ID
   * @param {string} [options.serverUrl] - Server URL (if using server)
   */
  constructor(options) {
    this.userId = options.userId;
    this.serverUrl = options.serverUrl;
    this.users = new Map();
    this.localState = null;
    this.listeners = new Map();
    this.syncInterval = null;
    this.connected = false;
  }

  /**
   * Connect to collaboration server
   * @returns {Promise<void>}
   */
  async connect() {
    return tracer.startActiveSpan('collaboration.connect', async (span) => {
      try {
        // In real implementation, establish WebSocket/WebRTC connection
        // For now, simulate connection
        this.connected = true;

        // Start sync loop
        this.syncInterval = setInterval(() => {
          this._syncState();
        }, 100); // 10 Hz sync rate

        span.setAttributes({
          'collaboration.userId': this.userId,
          'collaboration.connected': true,
        });
      } finally {
        span.end();
      }
    });
  }

  /**
   * Disconnect from collaboration server
   * @returns {void}
   */
  disconnect() {
    if (this.syncInterval) {
      clearInterval(this.syncInterval);
      this.syncInterval = null;
    }
    this.connected = false;
    this.users.clear();
  }

  /**
   * Update local user state
   * @param {Object} state - User state
   * @param {Object} state.position - User position
   * @param {Object} state.rotation - User rotation (quaternion)
   * @param {string} [state.selectedNode] - Selected node ID
   * @returns {void}
   */
  updateLocalState(state) {
    const timestamp = Date.now();

    this.localState = CollaborationStateSchema.parse({
      userId: this.userId,
      position: state.position,
      rotation: state.rotation,
      selectedNode: state.selectedNode,
      timestamp,
    });

    this._emit('local-state-updated', this.localState);
  }

  /**
   * Get state of remote user
   * @param {string} userId - User ID
   * @returns {Object|null} User state
   */
  getUserState(userId) {
    return this.users.get(userId) || null;
  }

  /**
   * Get all remote users
   * @returns {Array<Object>} All user states
   */
  getAllUsers() {
    return Array.from(this.users.values());
  }

  /**
   * Sync state with server/peers
   * @private
   */
  _syncState() {
    if (!this.connected || !this.localState) return;

    tracer.startActiveSpan('collaboration.sync', (span) => {
      try {
        // Broadcast local state
        this._broadcast(this.localState);

        // In real implementation, receive updates from server/peers
        // For testing, we'll simulate receiving updates

        span.setAttributes({
          'collaboration.users': this.users.size,
          'collaboration.timestamp': this.localState.timestamp,
        });
      } finally {
        span.end();
      }
    });
  }

  /**
   * Broadcast state to peers
   * @private
   */
  _broadcast(_state) {
    // In real implementation, send via WebSocket/WebRTC
    // For now, this is a no-op (simulated in tests)
  }

  /**
   * Receive remote user state
   * @param {Object} state - Remote user state
   * @returns {void}
   */
  receiveState(state) {
    const validated = CollaborationStateSchema.parse(state);

    if (validated.userId === this.userId) return; // Ignore own state

    const existing = this.users.get(validated.userId);

    // Only update if newer
    if (!existing || validated.timestamp > existing.timestamp) {
      this.users.set(validated.userId, validated);
      this._emit('user-state-updated', validated);
    }
  }

  /**
   * Remove user from collaboration
   * @param {string} userId - User ID
   * @returns {void}
   */
  removeUser(userId) {
    if (this.users.delete(userId)) {
      this._emit('user-left', { userId });
    }
  }

  /**
   * Register event listener
   * @param {string} event - Event type
   * @param {Function} callback - Event handler
   * @returns {Function} Unsubscribe function
   */
  on(event, callback) {
    if (!this.listeners.has(event)) {
      this.listeners.set(event, new Set());
    }

    this.listeners.get(event).add(callback);

    return () => {
      const listeners = this.listeners.get(event);
      if (listeners) {
        listeners.delete(callback);
      }
    };
  }

  /**
   * Emit event to listeners
   * @private
   */
  _emit(event, data) {
    const listeners = this.listeners.get(event);
    if (listeners) {
      for (const callback of listeners) {
        try {
          callback(data);
        } catch (error) {
          console.error(`Collaboration listener error:`, error);
        }
      }
    }
  }

  /**
   * Get connection status
   * @returns {boolean} Connected status
   */
  isConnected() {
    return this.connected;
  }

  /**
   * Get collaboration statistics
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      connected: this.connected,
      userId: this.userId,
      userCount: this.users.size,
      lastSync: this.localState?.timestamp || null,
    };
  }
}
