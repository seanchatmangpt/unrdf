/**
 * @fileoverview WebSocket synchronization for collaborative RDF graphs
 *
 * Provides real-time synchronization between multiple clients using y-websocket.
 * Handles connection management, reconnection, and awareness (presence).
 *
 * @module @unrdf/collab/sync
 */

import * as _Y from 'yjs';
import { WebsocketProvider } from 'y-websocket';
import { z } from 'zod';

/**
 * @typedef {Object} SyncOptions
 * @property {string} url - WebSocket server URL
 * @property {string} roomName - Room name for collaboration
 * @property {Object} [awareness] - Awareness configuration
 * @property {boolean} [connect=true] - Auto-connect
 * @property {number} [maxBackoffTime=5000] - Max reconnection backoff
 */

/** Validation schema for sync options */
const SyncOptionsSchema = z.object({
  url: z.string().url(),
  roomName: z.string().min(1),
  awareness: z.any().optional(),
  connect: z.boolean().default(true),
  maxBackoffTime: z.number().positive().default(5000),
});

/**
 * WebSocketSync - Real-time synchronization for collaborative RDF graphs
 *
 * Wraps y-websocket provider with RDF-specific features:
 * - Automatic reconnection with exponential backoff
 * - Presence awareness (who's editing, cursors, etc.)
 * - Connection state management
 * - Sync state tracking
 *
 * @example
 * import { CollaborativeRDFGraph } from '@unrdf/collab/crdt';
 * import { WebSocketSync } from '@unrdf/collab/sync';
 *
 * const graph = new CollaborativeRDFGraph();
 * const sync = new WebSocketSync(graph, {
 *   url: 'ws://localhost:1234',
 *   roomName: 'my-rdf-graph',
 *   awareness: {
 *     user: { name: 'Alice', color: '#ff0000' }
 *   }
 * });
 *
 * sync.on('synced', () => {
 *   console.log('Graph synced with server');
 * });
 */
export class WebSocketSync {
  /**
   * Create a new WebSocket sync provider
   * @param {import('../crdt/rdf-crdt.mjs').CollaborativeRDFGraph} graph - Collaborative graph
   * @param {SyncOptions} options - Sync options
   */
  constructor(graph, options) {
    // Validate options
    const result = SyncOptionsSchema.safeParse(options);
    if (!result.success) {
      throw new Error(`Invalid sync options: ${result.error.message}`);
    }

    this.graph = graph;
    this.options = result.data;

    /** @type {WebsocketProvider} */
    this.provider = new WebsocketProvider(
      this.options.url,
      this.options.roomName,
      graph.getYDoc(),
      {
        connect: this.options.connect,
        maxBackoffTime: this.options.maxBackoffTime,
      }
    );

    // Set awareness state if provided
    if (this.options.awareness) {
      this.provider.awareness.setLocalState(this.options.awareness);
    }

    /** @type {Map<string, Array<Function>>} - Event listeners */
    this.listeners = new Map();

    // Forward provider events
    this._setupEventForwarding();
  }

  /**
   * Setup event forwarding from y-websocket to our event system
   * @private
   */
  _setupEventForwarding() {
    // Connection status
    this.provider.on('status', (event) => {
      this._emit('status', event);
    });

    // Sync state (when fully synced)
    this.provider.on('sync', (isSynced) => {
      this._emit('synced', { isSynced });
    });

    // Connection events
    this.provider.on('connection-close', (event) => {
      this._emit('disconnected', event);
    });

    this.provider.on('connection-error', (event) => {
      this._emit('error', event);
    });

    // Awareness changes (presence)
    this.provider.awareness.on('change', (changes) => {
      this._emit('awareness', this._getAwarenessState(changes));
    });
  }

  /**
   * Get awareness state for all clients
   * @param {Object} [changes] - Change event
   * @returns {Object} Awareness state
   * @private
   */
  _getAwarenessState(changes) {
    const states = new Map();
    this.provider.awareness.getStates().forEach((state, clientID) => {
      states.set(clientID, state);
    });

    return {
      states: Array.from(states.entries()).map(([clientID, state]) => ({
        clientID,
        ...state,
      })),
      added: changes?.added || [],
      updated: changes?.updated || [],
      removed: changes?.removed || [],
    };
  }

  /**
   * Connect to WebSocket server
   */
  connect() {
    this.provider.connect();
  }

  /**
   * Disconnect from WebSocket server
   */
  disconnect() {
    this.provider.disconnect();
  }

  /**
   * Update local awareness state (presence)
   * @param {Object} state - Awareness state (user info, cursor, selection, etc.)
   *
   * @example
   * sync.setAwareness({
   *   user: { name: 'Alice', color: '#ff0000' },
   *   cursor: { subject: 'http://example.org/alice' }
   * });
   */
  setAwareness(state) {
    this.provider.awareness.setLocalState(state);
  }

  /**
   * Get awareness state for a specific client
   * @param {number} clientID - Client ID
   * @returns {Object|null} Client's awareness state
   */
  getClientAwareness(clientID) {
    return this.provider.awareness.getStates().get(clientID) || null;
  }

  /**
   * Get all clients' awareness states
   * @returns {Array<Object>} Array of {clientID, ...state}
   */
  getAllAwareness() {
    const states = [];
    this.provider.awareness.getStates().forEach((state, clientID) => {
      states.push({ clientID, ...state });
    });
    return states;
  }

  /**
   * Subscribe to sync events
   * @param {string} event - Event name: 'status', 'synced', 'disconnected', 'error', 'awareness'
   * @param {Function} listener - Callback function
   * @returns {Function} Unsubscribe function
   *
   * @example
   * const unsubscribe = sync.on('awareness', (state) => {
   *   console.log('Users online:', state.states.length);
   * });
   */
  on(event, listener) {
    if (!this.listeners.has(event)) {
      this.listeners.set(event, []);
    }
    this.listeners.get(event).push(listener);

    return () => {
      const listeners = this.listeners.get(event);
      const idx = listeners.indexOf(listener);
      if (idx !== -1) listeners.splice(idx, 1);
    };
  }

  /**
   * Emit an event to all listeners
   * @param {string} event - Event name
   * @param {any} data - Event data
   * @private
   */
  _emit(event, data) {
    const listeners = this.listeners.get(event);
    if (listeners) {
      listeners.forEach((listener) => listener(data));
    }
  }

  /**
   * Get connection status
   * @returns {string} Status: 'connected', 'disconnected', 'connecting'
   */
  getStatus() {
    return this.provider.wsconnected
      ? 'connected'
      : this.provider.wsconnecting
        ? 'connecting'
        : 'disconnected';
  }

  /**
   * Check if fully synced with server
   * @returns {boolean} True if synced
   */
  isSynced() {
    return this.provider.synced;
  }

  /**
   * Destroy the sync provider
   */
  destroy() {
    this.provider.destroy();
    this.listeners.clear();
  }
}
