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
 * @returns {Object} Change feed with emit and addEventListener
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
export function createChangeFeed() {
  const target = new EventTarget();
  const changes = [];

  return {
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
}
