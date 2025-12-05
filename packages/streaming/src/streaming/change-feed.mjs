/**
 * @file Change Feed - Real-time change event emitter
 * @module streaming/change-feed
 *
 * @description
 * Provides a simple EventTarget-based change feed for broadcasting
 * quad changes (add, remove, update) to subscribers.
 */

import { z } from 'zod';

/**
 * Schema for change events
 */
const ChangeEventSchema = z.object({
  type: z.enum(['add', 'remove', 'update']),
  quad: z.object({
    subject: z.any(),
    predicate: z.any(),
    object: z.any(),
    graph: z.any().optional(),
  }),
  timestamp: z.number(),
  metadata: z.record(z.any()).optional(),
});

/**
 * Create a change feed emitter
 *
 * @param {Object} [store] - Optional N3 Store to monitor
 * @returns {Object} Change feed with emit, subscribe, getHistory, and replay
 *
 * @example
 * const feed = createChangeFeed();
 * feed.addEventListener('change', (event) => {
 *   console.log('Change:', event.detail);
 * });
 * feed.emitChange({
 *   type: 'add',
 *   quad: { subject: s, predicate: p, object: o },
 *   timestamp: Date.now()
 * });
 */
export function createChangeFeed(store) {
  const target = new EventTarget();
  const changes = [];
  const subscribers = new Set();

  // Hook into store if provided
  if (store) {
    const originalAddQuad = store.addQuad.bind(store);
    const originalRemoveQuad = store.removeQuad.bind(store);

    store.addQuad = function (quad) {
      const result = originalAddQuad(quad);
      feed.emitChange({
        type: 'add',
        quad: {
          subject: quad.subject,
          predicate: quad.predicate,
          object: quad.object,
          graph: quad.graph,
        },
      });
      return result;
    };

    store.removeQuad = function (quad) {
      const result = originalRemoveQuad(quad);
      feed.emitChange({
        type: 'remove',
        quad: {
          subject: quad.subject,
          predicate: quad.predicate,
          object: quad.object,
          graph: quad.graph,
        },
      });
      return result;
    };
  }

  const feed = {
    /**
     * Emit a change event
     *
     * @param {Object} change - Change event
     * @param {string} change.type - Change type: 'add', 'remove', or 'update'
     * @param {Object} change.quad - The quad being changed
     * @param {number} [change.timestamp] - When change occurred (defaults to Date.now())
     * @param {Object} [change.metadata] - Optional metadata
     */
    emitChange(change) {
      const validated = ChangeEventSchema.parse({
        ...change,
        timestamp: change.timestamp ?? Date.now(),
      });

      changes.push(validated);

      const event = new CustomEvent('change', {
        detail: validated,
      });

      target.dispatchEvent(event);

      // Notify subscribers
      for (const callback of subscribers) {
        callback(validated);
      }
    },

    /**
     * Subscribe to change events
     *
     * @param {Function} callback - Callback function to receive changes
     * @returns {Function} Unsubscribe function
     */
    subscribe(callback) {
      subscribers.add(callback);
      return () => {
        subscribers.delete(callback);
      };
    },

    /**
     * Get change history
     *
     * @param {Object} [options] - Query options
     * @param {number} [options.since] - Return only changes after this timestamp
     * @param {number} [options.limit] - Maximum number of changes to return
     * @returns {Array} Array of change events
     */
    getHistory(options = {}) {
      let result = [...changes];

      if (options.since !== undefined) {
        result = result.filter(change => change.timestamp >= options.since);
      }

      if (options.limit !== undefined) {
        result = result.slice(0, options.limit);
      }

      return result;
    },

    /**
     * Add event listener for changes
     *
     * @param {string} type - Event type ('change')
     * @param {Function} callback - Callback function
     * @param {Object} [options] - Event listener options
     */
    addEventListener(type, callback, options) {
      target.addEventListener(type, callback, options);
    },

    /**
     * Remove event listener
     *
     * @param {string} type - Event type ('change')
     * @param {Function} callback - Callback function
     * @param {Object} [options] - Event listener options
     */
    removeEventListener(type, callback, options) {
      target.removeEventListener(type, callback, options);
    },

    /**
     * Get all changes
     *
     * @returns {Array} All changes
     */
    getChanges() {
      return [...changes];
    },

    /**
     * Clear all changes
     */
    clearChanges() {
      changes.length = 0;
    },

    /**
     * Replay changes to a callback
     *
     * @param {Function} callback - Callback to receive each change
     */
    replay(callback) {
      for (const change of changes) {
        callback(change);
      }
    },
  };

  return feed;
}
