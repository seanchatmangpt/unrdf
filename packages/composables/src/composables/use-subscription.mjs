/**
 * useSubscription Composable - Change Feed Subscriptions
 *
 * Subscribe to real-time RDF change feeds with reactive updates.
 * Supports filtering and automatic cleanup on unmount.
 *
 * @module composables/use-subscription
 */

import { ref, computed, onUnmounted } from 'vue';
import { createSubscriptionManager } from '@unrdf/streaming';
import { z } from 'zod';

/**
 * Options schema for useSubscription
 */
const UseSubscriptionOptionsSchema = z
  .object({
    filter: z.function().optional(),
    maxChanges: z.number().optional().default(100),
    autoUnsubscribe: z.boolean().optional().default(true),
  })
  .strict();

/**
 * Subscribe to RDF change feeds
 *
 * @param {object} feed - Change feed object
 * @param {Function} [filter] - Optional filter function for changes
 * @param {object} [options={}] - Configuration options
 * @param {Function} [options.filter] - Filter function (delta => boolean)
 * @param {number} [options.maxChanges=100] - Maximum changes to keep in memory
 * @param {boolean} [options.autoUnsubscribe=true] - Auto-unsubscribe on unmount
 * @returns {{
 *   changes: import('vue').Ref<Array>,
 *   unsubscribe: () => void,
 *   count: import('vue').ComputedRef<number>,
 *   lastChange: import('vue').ComputedRef<object | null>
 * }} Subscription state and methods
 * @example
 * const feed = createChangeFeed(store)
 * const { changes, count, unsubscribe } = useSubscription(feed, (delta) => delta.additions.length > 0)
 * watch(changes, (newChanges) => console.log('New changes:', newChanges))
 */
export function useSubscription(feed, filter, options = {}) {
  const opts = UseSubscriptionOptionsSchema.parse({
    ...options,
    filter: filter || options.filter,
  });

  // Reactive state
  const changes = ref([]);
  const subscriptionManager = createSubscriptionManager();
  let isSubscribed = false;

  // Computed properties
  const count = computed(() => changes.value.length);
  const lastChange = computed(() => {
    return changes.value.length > 0 ? changes.value[changes.value.length - 1] : null;
  });

  /**
   * Handle incoming changes
   *
   * @param {object} delta - Change delta
   */
  function handleChange(delta) {
    // Apply filter if provided
    if (opts.filter && !opts.filter(delta)) {
      return;
    }

    // Add change to list
    changes.value.push(delta);

    // Enforce max changes limit
    if (changes.value.length > opts.maxChanges) {
      changes.value.shift();
    }
  }

  /**
   * Unsubscribe from feed
   */
  function unsubscribe() {
    if (isSubscribed) {
      subscriptionManager.unsubscribe(feed, handleChange);
      isSubscribed = false;
    }
  }

  // Subscribe to feed
  try {
    subscriptionManager.subscribe(feed, handleChange);
    isSubscribed = true;
  } catch (err) {
    console.error('Failed to subscribe to feed:', err);
  }

  // Auto-cleanup on unmount
  if (opts.autoUnsubscribe) {
    onUnmounted(() => {
      unsubscribe();
    });
  }

  return {
    changes,
    unsubscribe,
    count,
    lastChange,
  };
}
