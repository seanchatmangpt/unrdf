/**
 * @file Stream Processor - Transform and process change streams
 * @module streaming/stream-processor
 *
 * @description
 * Provides chainable stream operations for processing change feeds:
 * batching, debouncing, filtering, and transforming.
 */

/**
 * Create a stream processor for a change feed
 *
 * @param {Object} feed - Change feed to process
 * @returns {Object} Stream processor
 *
 * @example
 * const processor = createStreamProcessor(feed);
 * processor.batch(10).filter(c => c.type === 'add').subscribe((changes) => {
 *   console.log('Batch:', changes);
 * });
 */
export function createStreamProcessor(feed) {
  const operations = [];

  /**
   * Apply all operations to a change
   *
   * @param {Object} change - Change to process
   * @returns {Object|null} Processed change or null if filtered out
   */
  function applyOperations(change) {
    let result = change;
    for (const op of operations) {
      result = op(result);
      if (result === null) return null;
    }
    return result;
  }

  return {
    /**
     * Batch changes into groups
     *
     * @param {number} batchSize - Number of changes per batch
     * @returns {Object} Processor for chaining
     *
     * @example
     * processor.batch(10).subscribe((changes) => {
     *   console.log('Batch of', changes.length);
     * });
     */
    batch(batchSize) {
      const buffer = [];
      const subscribers = [];

      feed.addEventListener('change', event => {
        const change = applyOperations(event.detail);
        if (change === null) return;

        buffer.push(change);

        if (buffer.length >= batchSize) {
          const batch = buffer.splice(0, batchSize);
          for (const subscriber of subscribers) {
            subscriber(batch);
          }
        }
      });

      return {
        subscribe(callback) {
          subscribers.push(callback);
        },
        operations,
      };
    },

    /**
     * Debounce rapid changes
     *
     * @param {number} delayMs - Delay in milliseconds
     * @returns {Object} Processor for chaining
     *
     * @example
     * processor.debounce(500).subscribe((change) => {
     *   console.log('Debounced:', change);
     * });
     */
    debounce(delayMs) {
      const subscribers = [];
      let timeout = null;
      let latestChange = null;

      feed.addEventListener('change', event => {
        const change = applyOperations(event.detail);
        if (change === null) return;

        latestChange = change;

        if (timeout !== null) {
          clearTimeout(timeout);
        }

        timeout = setTimeout(() => {
          for (const subscriber of subscribers) {
            subscriber(latestChange);
          }
          timeout = null;
          latestChange = null;
        }, delayMs);
      });

      return {
        subscribe(callback) {
          subscribers.push(callback);
        },
        operations,
      };
    },

    /**
     * Filter changes by predicate
     *
     * @param {Function} predicate - Filter function
     * @returns {Object} Processor for chaining
     *
     * @example
     * processor.filter(c => c.type === 'add').subscribe((change) => {
     *   console.log('Add:', change);
     * });
     */
    filter(predicate) {
      operations.push(change => (predicate(change) ? change : null));
      return this;
    },

    /**
     * Map changes to new values
     *
     * @param {Function} mapper - Mapping function
     * @returns {Object} Processor for chaining
     *
     * @example
     * processor.map(c => ({ ...c, processed: true })).subscribe((change) => {
     *   console.log('Mapped:', change);
     * });
     */
    map(mapper) {
      operations.push(change => mapper(change));
      return this;
    },

    /**
     * Subscribe to processed changes
     *
     * @param {Function} callback - Callback to receive changes
     */
    subscribe(callback) {
      feed.addEventListener('change', event => {
        const change = applyOperations(event.detail);
        if (change !== null) {
          callback(change);
        }
      });
    },
  };
}
