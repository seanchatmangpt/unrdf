/**
 * @fileoverview IndexedDB persistence for offline-first RDF collaboration
 *
 * Provides automatic persistence to IndexedDB for offline-first architecture.
 * Changes sync automatically when connection is restored.
 *
 * @module @unrdf/collab/sync
 */

import * as _Y from 'yjs';
import { IndexeddbPersistence } from 'y-indexeddb';
import { z } from 'zod';

/**
 * @typedef {Object} PersistOptions
 * @property {string} dbName - IndexedDB database name
 * @property {boolean} [autoLoad=true] - Auto-load from IndexedDB on startup
 */

/** Validation schema for persist options */
const PersistOptionsSchema = z.object({
  dbName: z.string().min(1),
  autoLoad: z.boolean().default(true),
});

/**
 * IndexedDBPersist - Offline-first persistence for collaborative RDF graphs
 *
 * Automatically saves all changes to IndexedDB and loads on startup.
 * Works seamlessly with WebSocketSync for offline-first architecture.
 *
 * @example
 * import { CollaborativeRDFGraph } from '@unrdf/collab/crdt';
 * import { IndexedDBPersist } from '@unrdf/collab/sync';
 *
 * const graph = new CollaborativeRDFGraph();
 * const persist = new IndexedDBPersist(graph, {
 *   dbName: 'my-rdf-graph',
 *   autoLoad: true
 * });
 *
 * // Wait for initial load
 * await persist.whenSynced();
 *
 * // Now safe to use graph - data restored from IndexedDB
 * graph.addTriple({ ... });
 */
export class IndexedDBPersist {
  /**
   * Create a new IndexedDB persistence layer
   * @param {import('../crdt/rdf-crdt.mjs').CollaborativeRDFGraph} graph - Collaborative graph
   * @param {PersistOptions} options - Persistence options
   */
  constructor(graph, options) {
    // Validate options
    const result = PersistOptionsSchema.safeParse(options);
    if (!result.success) {
      throw new Error(`Invalid persist options: ${result.error.message}`);
    }

    this.graph = graph;
    this.options = result.data;

    /** @type {IndexeddbPersistence} */
    this.provider = new IndexeddbPersistence(
      this.options.dbName,
      graph.getYDoc()
    );

    /** @type {Promise<void>} - Resolves when initially synced */
    this.syncedPromise = new Promise((resolve) => {
      this.provider.once('synced', () => {
        resolve();
      });
    });

    /** @type {Map<string, Array<Function>>} - Event listeners */
    this.listeners = new Map();

    // Forward events
    this._setupEventForwarding();
  }

  /**
   * Setup event forwarding from y-indexeddb
   * @private
   */
  _setupEventForwarding() {
    this.provider.on('synced', () => {
      this._emit('synced', { timestamp: Date.now() });
    });
  }

  /**
   * Wait for initial sync from IndexedDB
   * @returns {Promise<void>}
   *
   * @example
   * await persist.whenSynced();
   * console.log('Data loaded from IndexedDB');
   */
  whenSynced() {
    return this.syncedPromise;
  }

  /**
   * Manually trigger save to IndexedDB
   * (Usually automatic, but useful for force-save)
   * @returns {Promise<void>}
   */
  async forceSave() {
    // y-indexeddb saves automatically on every change
    // This is a no-op but kept for API compatibility
    return Promise.resolve();
  }

  /**
   * Clear all data from IndexedDB
   * WARNING: This deletes all local data
   * @returns {Promise<void>}
   */
  async clearDatabase() {
    return this.provider.clearData();
  }

  /**
   * Subscribe to persistence events
   * @param {string} event - Event name: 'synced'
   * @param {Function} listener - Callback function
   * @returns {Function} Unsubscribe function
   *
   * @example
   * persist.on('synced', () => {
   *   console.log('Saved to IndexedDB');
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
   * Destroy the persistence provider
   */
  destroy() {
    this.provider.destroy();
    this.listeners.clear();
  }
}
