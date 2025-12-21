/**
 * @file Subscription Manager - Manage change feed subscriptions
 * @module streaming/subscription-manager
 *
 * @description
 * Manages subscriptions to change feeds with optional filtering by
 * subject, predicate, or object patterns.
 */

import { z } from 'zod';
import { createChangeFeed } from './change-feed.mjs';

/**
 * Schema for subscription filters
 */
const FilterSchema = z
  .object({
    subject: z.any().optional(),
    predicate: z.any().optional(),
    object: z.any().optional(),
    graph: z.any().optional(),
  })
  .optional();

/**
 * Create a subscription manager
 *
 * @param {Object} storeOrFeed - N3 Store or ChangeFeed to manage subscriptions for
 * @returns {Object} Subscription manager
 *
 * @example
 * const store = new Store();
 * const manager = createSubscriptionManager(store);
 * const subId = manager.subscribe({ subject: 'http://example.org/person/1' }, (change) => {
 *   console.log('Change:', change);
 * });
 */
export function createSubscriptionManager(storeOrFeed) {
  // If given a Store, wrap it in a ChangeFeed
  const feed = storeOrFeed.addEventListener ? storeOrFeed : createChangeFeed(storeOrFeed);

  const subscriptions = new Map();
  let nextId = 1;

  /**
   * Check if a quad matches a filter
   *
   * @param {Object} quad - Quad to check
   * @param {Object} filter - Filter to match against
   * @returns {boolean} True if quad matches filter
   */
  function matchesFilter(quad, filter) {
    if (!filter) return true;

    if (filter.subject !== undefined) {
      if (quad.subject?.value !== filter.subject?.value) {
        return false;
      }
    }

    if (filter.predicate !== undefined) {
      if (quad.predicate?.value !== filter.predicate?.value) {
        return false;
      }
    }

    if (filter.object !== undefined) {
      if (quad.object?.value !== filter.object?.value) {
        return false;
      }
    }

    if (filter.graph !== undefined) {
      if (quad.graph?.value !== filter.graph?.value) {
        return false;
      }
    }

    return true;
  }

  return {
    /**
     * Subscribe to changes
     *
     * @param {Function | Object} callbackOrFilter - Callback function or filter object (for backward compatibility)
     * @param {Object} [filterOrUndefined] - Filter object (if callback is first arg) or undefined
     * @param {*} [filterOrUndefined.subject] - Filter by subject
     * @param {*} [filterOrUndefined.predicate] - Filter by predicate
     * @param {*} [filterOrUndefined.object] - Filter by object
     * @param {*} [filterOrUndefined.graph] - Filter by graph
     * @returns {string} Subscription ID
     */
    subscribe(callbackOrFilter, filterOrUndefined) {
      // Backward compatibility: Old API is subscribe(callback, filter)
      let filter = null;
      let cb = null;

      if (typeof callbackOrFilter === 'function') {
        // Old API: subscribe(callback) or subscribe(callback, filter)
        cb = callbackOrFilter;
        filter = filterOrUndefined || null;
      } else if (typeof filterOrUndefined === 'function') {
        // New API: subscribe(filter, callback)
        filter = callbackOrFilter;
        cb = filterOrUndefined;
      } else {
        throw new TypeError('subscribe requires a callback function');
      }

      const validated = FilterSchema.parse(filter);
      const id = `sub_${nextId++}`;

      const listener = event => {
        try {
          const change = event.detail;
          if (matchesFilter(change.quad, validated)) {
            // Pass the change object itself, not wrapped in array
            cb(change);
          }
        } catch (error) {
          console.error('[subscription-manager] Listener error:', error);
          // Don't crash the event loop
        }
      };

      subscriptions.set(id, {
        callback: cb,
        filter: validated,
        listener,
      });

      feed.addEventListener('change', listener);

      return id;
    },

    /**
     * Unsubscribe from changes
     *
     * @param {string} subscriptionId - Subscription ID to remove
     * @returns {boolean} True if subscription was removed
     */
    unsubscribe(subscriptionId) {
      const sub = subscriptions.get(subscriptionId);
      if (!sub) return false;

      feed.removeEventListener('change', sub.listener);
      subscriptions.delete(subscriptionId);
      return true;
    },

    /**
     * List all active subscriptions
     *
     * @returns {Array} Array of subscription info
     */
    listSubscriptions() {
      return Array.from(subscriptions.entries()).map(([id, sub]) => ({
        id,
        filter: sub.filter,
      }));
    },

    /**
     * Clear all subscriptions
     */
    clearSubscriptions() {
      for (const [_id, sub] of subscriptions) {
        feed.removeEventListener('change', sub.listener);
      }
      subscriptions.clear();
    },

    /**
     * Destroy the subscription manager and cleanup all resources
     */
    destroy() {
      this.clearSubscriptions();
    },
  };
}
