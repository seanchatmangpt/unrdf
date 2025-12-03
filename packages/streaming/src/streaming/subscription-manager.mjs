/**
 * @file Subscription Manager - Manage change feed subscriptions
 * @module streaming/subscription-manager
 *
 * @description
 * Manages subscriptions to change feeds with optional filtering by
 * subject, predicate, or object patterns.
 */

import { z } from 'zod';

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
 * @param {Object} feed - Change feed to manage subscriptions for
 * @returns {Object} Subscription manager
 *
 * @example
 * const feed = createChangeFeed();
 * const manager = createSubscriptionManager(feed);
 * const subId = manager.subscribe((change) => {
 *   console.log('Change:', change);
 * }, { subject: 'http://example.org/person/1' });
 */
export function createSubscriptionManager(feed) {
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
     * @param {Function} callback - Callback function to receive changes
     * @param {Object} [filter] - Optional filter
     * @param {*} [filter.subject] - Filter by subject
     * @param {*} [filter.predicate] - Filter by predicate
     * @param {*} [filter.object] - Filter by object
     * @param {*} [filter.graph] - Filter by graph
     * @returns {number} Subscription ID
     */
    subscribe(callback, filter) {
      const validated = FilterSchema.parse(filter);
      const id = nextId++;

      const listener = event => {
        const change = event.detail;
        if (matchesFilter(change.quad, validated)) {
          callback(change);
        }
      };

      subscriptions.set(id, {
        callback,
        filter: validated,
        listener,
      });

      feed.addEventListener('change', listener);

      return id;
    },

    /**
     * Unsubscribe from changes
     *
     * @param {number} subscriptionId - Subscription ID to remove
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
      for (const [id, sub] of subscriptions) {
        feed.removeEventListener('change', sub.listener);
      }
      subscriptions.clear();
    },
  };
}
