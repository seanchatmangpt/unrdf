/**
 * @file use-subscription-manager.mjs
 * @description React hook for managing pattern-based subscriptions to graph changes
 * @since 3.2.0
 */

import { useState, useCallback, useEffect, useRef } from 'react';
import { useKnowledgeEngineContext } from '../core/use-knowledge-engine-context.mjs';
import { RingBuffer } from '../../knowledge-engine/utils/ring-buffer.mjs';

/**
 * Hook for managing pattern-based subscriptions to RDF graph changes
 * Uses RingBuffer (TRIZ #10 Prior Action) for O(1) event buffering
 *
 * @since 3.2.0
 * @param {Object} config - Subscription configuration
 * @param {string} [config.pattern] - SPARQL pattern to match
 * @param {Function} [config.filter] - Custom filter function
 * @param {boolean} [config.autoStart=true] - Auto-start subscription
 * @param {number} [config.bufferSize=100] - Max events to buffer
 * @returns {Object} Subscription state and operations
 * @throws {Error} When subscription manager not initialized
 * @throws {Error} When subscription ID not found for unsubscribe/pause/resume
 * @throws {Error} When SPARQL pattern is invalid
 * @performance Uses RingBuffer for O(1) push/clear operations. Events beyond bufferSize
 *   are automatically discarded (oldest first). Multiple subscriptions increase memory linearly.
 *
 * @example
 * // Pattern-based subscription with filtering
 * const {
 *   subscribe,
 *   unsubscribe,
 *   events,
 *   isActive,
 *   clear
 * } = useSubscriptionManager({
 *   pattern: `
 *     PREFIX schema: <http://schema.org/>
 *     ?s schema:price ?price
 *   `,
 *   filter: (delta) => {
 *     return delta.added.some(q => parseFloat(q.object.value) > 100);
 *   }
 * });
 *
 * @example
 * // Multiple subscriptions with manual control
 * const { subscribe, unsubscribeAll, pause, resume } = useSubscriptionManager();
 * const { subscriptionId } = await subscribe('?s rdf:type ?type');
 * await pause(subscriptionId);
 * await resume(subscriptionId);
 */
export function useSubscriptionManager(config = {}) {
  const { engine } = useKnowledgeEngineContext();
  const [subscriptions, setSubscriptions] = useState(new Map());
  const [events, setEvents] = useState([]);
  const [isActive, setIsActive] = useState(false);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);
  const subscriptionRef = useRef(null);
  // TRIZ #10 Prior Action: Use RingBuffer for O(1) event operations
  const eventsRef = useRef(/** @type {RingBuffer<Object>|null} */ (null));

  // Initialize subscription manager
  useEffect(() => {
    if (!engine) return;

    let mounted = true;

    // Initialize RingBuffer with configured size (TRIZ #10)
    const bufferSize = config.bufferSize || 100;
    if (!eventsRef.current || eventsRef.current.capacity !== bufferSize) {
      eventsRef.current = new RingBuffer(bufferSize);
    }

    async function initializeManager() {
      try {
        setLoading(true);

        // Import subscription module
        const { SubscriptionManager } = await import(
          '../../knowledge-engine/streaming/subscription-manager.mjs'
        );

        // Create subscription manager
        const manager = new SubscriptionManager({ engine });

        if (!mounted) return;

        subscriptionRef.current = manager;

        // Auto-start if configured
        if (config.autoStart && config.pattern) {
          await subscribe(config.pattern, {
            filter: config.filter,
            bufferSize: bufferSize
          });
        }

        setLoading(false);
      } catch (err) {
        if (!mounted) return;
        setError(err);
        setLoading(false);
      }
    }

    initializeManager();

    return () => {
      mounted = false;
      if (subscriptionRef.current) {
        subscriptionRef.current.close?.();
      }
    };
  }, [engine]);

  // Subscribe to pattern
  const subscribe = useCallback(async (pattern, options = {}) => {
    if (!subscriptionRef.current) {
      throw new Error('Subscription manager not initialized');
    }

    try {
      const subscriptionId = options.id || `sub-${Date.now()}`;

      // Ensure RingBuffer is initialized with correct size
      const maxBuffer = options.bufferSize || config.bufferSize || 100;
      if (!eventsRef.current || eventsRef.current.capacity !== maxBuffer) {
        eventsRef.current = new RingBuffer(maxBuffer);
      }

      const subscription = await subscriptionRef.current.subscribe({
        pattern,
        filter: options.filter,
        bufferSize: maxBuffer,
        onEvent: (event) => {
          // TRIZ #10 Prior Action: O(1) push with automatic overflow handling
          // RingBuffer automatically discards oldest items when full
          eventsRef.current.push({
            ...event,
            subscriptionId,
            timestamp: new Date().toISOString()
          });

          // Convert to array for React state (RingBuffer handles size limiting)
          setEvents(eventsRef.current.toArray());

          // Custom callback
          options.onEvent?.(event);
        },
        onError: (err) => {
          setError(err);
          options.onError?.(err);
        }
      });

      setSubscriptions(prev => new Map(prev).set(subscriptionId, {
        pattern,
        subscription,
        options
      }));

      setIsActive(true);

      return { subscriptionId, subscription };
    } catch (err) {
      setError(err);
      throw err;
    }
  }, [config]);

  // Unsubscribe from pattern
  const unsubscribe = useCallback(async (subscriptionId) => {
    if (!subscriptionRef.current) {
      throw new Error('Subscription manager not initialized');
    }

    try {
      const sub = subscriptions.get(subscriptionId);
      if (!sub) {
        throw new Error(`Subscription ${subscriptionId} not found`);
      }

      await sub.subscription.unsubscribe();

      const newSubs = new Map(subscriptions);
      newSubs.delete(subscriptionId);
      setSubscriptions(newSubs);

      if (newSubs.size === 0) {
        setIsActive(false);
      }

      return { success: true };
    } catch (err) {
      setError(err);
      throw err;
    }
  }, [subscriptions]);

  // Unsubscribe all
  const unsubscribeAll = useCallback(async () => {
    if (!subscriptionRef.current) {
      throw new Error('Subscription manager not initialized');
    }

    try {
      for (const [id, sub] of subscriptions) {
        await sub.subscription.unsubscribe();
      }

      setSubscriptions(new Map());
      setIsActive(false);

      return { success: true };
    } catch (err) {
      setError(err);
      throw err;
    }
  }, [subscriptions]);

  // Clear events buffer
  const clear = useCallback(() => {
    // TRIZ #10: Use RingBuffer clear for O(1) reset
    if (eventsRef.current) {
      eventsRef.current.clear();
    }
    setEvents([]);
  }, []);

  // Get subscription by ID
  const getSubscription = useCallback((subscriptionId) => {
    return subscriptions.get(subscriptionId);
  }, [subscriptions]);

  // Pause subscription
  const pause = useCallback(async (subscriptionId) => {
    const sub = subscriptions.get(subscriptionId);
    if (!sub) {
      throw new Error(`Subscription ${subscriptionId} not found`);
    }

    try {
      await sub.subscription.pause();
      return { success: true };
    } catch (err) {
      setError(err);
      throw err;
    }
  }, [subscriptions]);

  // Resume subscription
  const resume = useCallback(async (subscriptionId) => {
    const sub = subscriptions.get(subscriptionId);
    if (!sub) {
      throw new Error(`Subscription ${subscriptionId} not found`);
    }

    try {
      await sub.subscription.resume();
      return { success: true };
    } catch (err) {
      setError(err);
      throw err;
    }
  }, [subscriptions]);

  return {
    subscribe,
    unsubscribe,
    unsubscribeAll,
    events,
    subscriptions: Array.from(subscriptions.entries()).map(([id, sub]) => ({
      id,
      pattern: sub.pattern,
      options: sub.options
    })),
    isActive,
    loading,
    error,
    clear,
    getSubscription,
    pause,
    resume
  };
}
