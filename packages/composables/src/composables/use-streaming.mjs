/**
 * useStreaming Composable - Real-time Stream Processing
 *
 * Process real-time RDF change streams with batching and debouncing.
 * Optimized for high-frequency updates.
 *
 * @module composables/use-streaming
 */

import { ref, computed, onUnmounted } from 'vue';
import { createStreamProcessor } from '@unrdf/streaming';
import { z } from 'zod';

/**
 * Options schema for useStreaming
 */
const UseStreamingOptionsSchema = z
  .object({
    batchSize: z.number().optional().default(10),
    debounceMs: z.number().optional().default(100),
    maxEvents: z.number().optional().default(1000),
    autoStart: z.boolean().optional().default(true),
  })
  .strict();

/**
 * Process real-time RDF change streams
 *
 * @param {object} feed - Change feed or stream source
 * @param {object} [options={}] - Configuration options
 * @param {number} [options.batchSize=10] - Events per batch
 * @param {number} [options.debounceMs=100] - Debounce delay in ms
 * @param {number} [options.maxEvents=1000] - Maximum events to keep
 * @param {boolean} [options.autoStart=true] - Start processing automatically
 * @returns {{
 *   events: import('vue').Ref<Array>,
 *   processing: import('vue').Ref<boolean>,
 *   error: import('vue').Ref<Error | null>,
 *   start: () => void,
 *   stop: () => void,
 *   clear: () => void,
 *   eventCount: import('vue').ComputedRef<number>,
 *   latestEvent: import('vue').ComputedRef<object | null>
 * }} Stream processing state and methods
 * @example
 * const feed = createChangeFeed(store)
 * const { events, eventCount, start, stop } = useStreaming(feed, { batchSize: 20 })
 * watch(events, (stream) => console.log('Stream updated:', stream.length))
 */
export function useStreaming(feed, options = {}) {
  const opts = UseStreamingOptionsSchema.parse(options);

  // Reactive state
  const events = ref([]);
  const processing = ref(false);
  const error = ref(null);

  // Stream processor
  const processor = createStreamProcessor({
    batchSize: opts.batchSize,
    debounceMs: opts.debounceMs,
  });

  // Computed properties
  const eventCount = computed(() => events.value.length);
  const latestEvent = computed(() => {
    return events.value.length > 0 ? events.value[events.value.length - 1] : null;
  });

  // Batch buffer and timer
  let batchBuffer = [];
  let debounceTimer = null;

  /**
   * Process a batch of events
   */
  function processBatch() {
    if (batchBuffer.length === 0) return;

    try {
      // Add batch to events
      events.value.push(...batchBuffer);

      // Enforce max events limit
      if (events.value.length > opts.maxEvents) {
        const overflow = events.value.length - opts.maxEvents;
        events.value.splice(0, overflow);
      }

      batchBuffer = [];
    } catch (err) {
      error.value = err;
    }
  }

  /**
   * Handle incoming event
   *
   * @param {object} event - Stream event
   */
  function handleEvent(event) {
    batchBuffer.push(event);

    // Flush if batch size reached
    if (batchBuffer.length >= opts.batchSize) {
      if (debounceTimer) {
        clearTimeout(debounceTimer);
      }
      processBatch();
      return;
    }

    // Debounce batch processing
    if (debounceTimer) {
      clearTimeout(debounceTimer);
    }
    debounceTimer = setTimeout(() => {
      processBatch();
      debounceTimer = null;
    }, opts.debounceMs);
  }

  /**
   * Start stream processing
   */
  function start() {
    if (processing.value) return;

    try {
      processor.process(feed, handleEvent);
      processing.value = true;
      error.value = null;
    } catch (err) {
      error.value = err;
    }
  }

  /**
   * Stop stream processing
   */
  function stop() {
    if (!processing.value) return;

    processing.value = false;

    // Flush remaining buffer
    if (debounceTimer) {
      clearTimeout(debounceTimer);
      debounceTimer = null;
    }
    processBatch();
  }

  /**
   * Clear all events
   */
  function clear() {
    events.value = [];
    batchBuffer = [];
    error.value = null;
  }

  // Auto-start if enabled
  if (opts.autoStart) {
    start();
  }

  // Cleanup on unmount
  onUnmounted(() => {
    stop();
    clear();
  });

  return {
    events,
    processing,
    error,
    start,
    stop,
    clear,
    eventCount,
    latestEvent,
  };
}
