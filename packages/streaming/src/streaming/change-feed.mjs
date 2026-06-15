/**
 * @file Change Feed - Real-time change event emitter
 * @module streaming/change-feed
 *
 * @description
 * Provides a simple EventTarget-based change feed for broadcasting
 * quad changes (add, remove, update) to subscribers.
 */

import { trace } from '@opentelemetry/api';
import { RingBuffer } from '@unrdf/core';
import { z } from 'zod';

const tracer = trace.getTracer('@unrdf/streaming');

/**
 * Schema for change events
 */
/**
 * Maximum number of changes to keep in history (ring buffer)
 */
const DEFAULT_MAX_HISTORY_SIZE = 10000;
const HARD_MAX_HISTORY_SIZE = 1000000; // 1M events limit to prevent OOM

const ChangeEventSchema = z.object({
  type: z.enum(['add', 'remove', 'update']),
  quad: z.object({
    subject: z.any(),
    predicate: z.any(),
    object: z.any(),
    graph: z.any().optional(),
  }),
  timestamp: z.number(),
  metadata: z.any().optional(),
});

/**
 * Configuration schema for change feed
 * Note: Infinity is validated separately as Zod considers it "not a number"
 */
const ChangeFeedConfigSchema = z
  .object({
    maxHistorySize: z
      .union([z.number().nonnegative(), z.literal(Infinity)])
      .optional()
      .default(DEFAULT_MAX_HISTORY_SIZE),
  })
  .optional()
  .default({});

/**
 * Create a change feed emitter
 *
 * @param {Object} [store] - Optional N3 Store to monitor
 * @param {Object} [config] - Configuration options
 * @param {number} [config.maxHistorySize=10000] - Maximum number of changes to keep in history
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
export function createChangeFeed(store, config = {}) {
  const validatedConfig = ChangeFeedConfigSchema.parse(config);
  let target = new EventTarget();
  const subscribers = new Set();
  
  // Enforce hard limit on history size
  let maxSize = validatedConfig.maxHistorySize;
  if (maxSize === Infinity || maxSize > HARD_MAX_HISTORY_SIZE) {
    maxSize = HARD_MAX_HISTORY_SIZE;
  }

  // Ring buffer state using @unrdf/core RingBuffer
  const history = new RingBuffer(maxSize > 0 ? maxSize : 1); // Minimum size of 1 if history enabled
  const hasHistory = maxSize > 0;

  // Original store methods for restoration on destroy
  let originalAddQuad, originalRemoveQuad, originalRemoveQuads;

  // Hook into store if provided
  if (store && typeof store.addQuad === 'function') {
    originalAddQuad = store.addQuad;
    originalRemoveQuad = store.removeQuad;
    originalRemoveQuads = store.removeQuads;

    store.addQuad = function (quad) {
      const result = originalAddQuad.call(store, quad);
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

    // Support both N3 Store (removeQuad) and Oxigraph (removeQuads)
    if (typeof originalRemoveQuad === 'function') {
      store.removeQuad = function (quad) {
        const result = originalRemoveQuad.call(store, quad);
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

    if (typeof originalRemoveQuads === 'function') {
      store.removeQuads = function (quads) {
        const result = originalRemoveQuads.call(store, quads);
        for (const quad of quads) {
          feed.emitChange({
            type: 'remove',
            quad: {
              subject: quad.subject,
              predicate: quad.predicate,
              object: quad.object,
              graph: quad.graph,
            },
          });
        }
        return result;
      };
    }
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
      const span = tracer.startSpan('change-feed.emitChange');
      try {
        const validated = ChangeEventSchema.parse({
          ...change,
          timestamp: change.timestamp ?? Date.now(),
        });

        span.setAttributes({
          'change.type': validated.type,
          'change.timestamp': validated.timestamp,
          'subscribers.count': subscribers.size,
        });

        // Implement ring buffer using @unrdf/core RingBuffer
        if (hasHistory) {
          const wasFull = history.isFull();
          history.push(validated);
          span.setAttributes({
            'history.size': history.length,
            'history.trimmed': wasFull,
          });
        } else {
          span.setAttributes({
            'history.size': 0,
            'history.trimmed': false,
          });
        }

        const event = new CustomEvent('change', {
          detail: validated,
        });

        target.dispatchEvent(event);

        // Notify subscribers
        for (const callback of subscribers) {
          try {
            callback(validated);
          } catch (error) {
            console.error('[change-feed] Subscriber callback error:', error);
            // Don't crash on subscriber errors
          }
        }
      } catch (error) {
        span.recordException(error);
        throw error;
      } finally {
        span.end();
      }
    },

    /**
     * Subscribe to change events
     *
     * @param {Function} callback - Callback function to receive changes
     * @returns {Function} Unsubscribe function
     */
    subscribe(callback) {
      const span = tracer.startSpan('change-feed.subscribe');
      try {
        subscribers.add(callback);

        span.setAttributes({
          'subscribers.count': subscribers.size,
        });

        return () => {
          subscribers.delete(callback);
        };
      } catch (error) {
        span.recordException(error);
        throw error;
      } finally {
        span.end();
      }
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
      const span = tracer.startSpan('change-feed.getHistory');
      try {
        const allChanges = this.getChanges();
        span.setAttributes({
          'history.totalSize': allChanges.length,
          'history.hasSinceFilter': options.since !== undefined,
          'history.hasLimitFilter': options.limit !== undefined,
        });

        let result = allChanges;

        if (options.since !== undefined) {
          result = result.filter(change => change.timestamp >= options.since);
          span.setAttributes({
            'history.sinceValue': options.since,
          });
        }

        if (options.limit !== undefined) {
          result = result.slice(0, options.limit);
          span.setAttributes({
            'history.limitValue': options.limit,
          });
        }

        span.setAttributes({
          'history.resultSize': result.length,
        });

        return result;
      } catch (error) {
        span.recordException(error);
        throw error;
      } finally {
        span.end();
      }
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
     * Get all changes in order
     *
     * @returns {Array} All changes
     */
    getChanges() {
      if (!hasHistory) {
        return [];
      }
      return history.toArray();
    },

    /**
     * Clear all changes
     */
    clearChanges() {
      if (hasHistory) {
        history.clear();
      }
    },

    /**
     * Replay changes to a callback
     *
     * @param {Function} callback - Callback to receive each change
     */
    replay(callback) {
      if (hasHistory) {
        history.forEach(callback);
      }
    },

    /**
     * Destroy the change feed and cleanup all resources
     * Removes all event listeners, clears subscribers, and resets state
     */
    destroy() {
      // Clear change history
      this.clearChanges();

      // Clear all subscribers
      subscribers.clear();

      // Remove all event listeners by creating a new EventTarget
      // This is the most reliable way to remove all listeners
      target = new EventTarget();

      // Restore store methods if they were patched
      if (store) {
        if (originalAddQuad) {
          store.addQuad = originalAddQuad;
        }
        if (originalRemoveQuad) {
          store.removeQuad = originalRemoveQuad;
        }
        if (originalRemoveQuads) {
          store.removeQuads = originalRemoveQuads;
        }
      }
    },
  };

  return feed;
}
