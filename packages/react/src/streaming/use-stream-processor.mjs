/**
 * @file use-stream-processor.mjs
 * @description React hook for window/aggregation operations on streams
 * @since 3.2.0
 */

import { useState, useCallback, useEffect, useRef } from 'react';
import { useChangeFeed } from './use-change-feed.mjs';

/**
 * Hook for processing change streams with windowing and aggregation
 *
 * @since 3.2.0
 * @param {Object} config - Stream processor configuration
 * @param {string} [config.windowType='tumbling'] - Window type: 'tumbling', 'sliding', 'session'
 * @param {number} [config.windowSize=5000] - Window size (ms)
 * @param {number} [config.windowSlide] - Slide interval for sliding windows (ms)
 * @param {number} [config.sessionGap=30000] - Gap for session windows (ms)
 * @param {Function} [config.aggregator] - Custom aggregation function
 * @returns {Object} Stream processor state and operations
 * @throws {Error} When underlying change feed fails to initialize
 * @throws {Error} When aggregator function throws during window processing
 * @performance Sliding windows maintain overlap buffer - use tumbling for lower memory.
 *   Session windows use timers per-event - high event rates increase timer overhead.
 *   Windows array grows unbounded - call clear() periodically for long-running processors.
 *
 * @example
 * // Tumbling window for real-time analytics
 * const {
 *   windows,
 *   currentWindow,
 *   start,
 *   stop
 * } = useStreamProcessor({
 *   windowType: 'tumbling',
 *   windowSize: 5000,
 *   aggregator: (events) => ({
 *     count: events.length,
 *     priceChanges: events.filter(e =>
 *       e.quads.some(q => q.predicate.value.includes('price'))
 *     ).length,
 *     avgPrice: events.reduce((sum, e) => {
 *       const price = e.quads.find(q => q.predicate.value.includes('price'));
 *       return sum + (price ? parseFloat(price.object.value) : 0);
 *     }, 0) / events.length
 *   })
 * });
 *
 * @example
 * // Sliding window for moving averages
 * const { windows, stats } = useStreamProcessor({
 *   windowType: 'sliding',
 *   windowSize: 10000,
 *   windowSlide: 2000
 * });
 */
export function useStreamProcessor(config = {}) {
  const {
    changes,
    start: startFeed,
    stop: stopFeed,
    isRunning: feedRunning,
  } = useChangeFeed(config.changeFeed || {});
  const [windows, setWindows] = useState([]);
  const [currentWindow, setCurrentWindow] = useState(null);
  const [isProcessing, setIsProcessing] = useState(false);
  const [stats, setStats] = useState({
    windowsProcessed: 0,
    eventsProcessed: 0,
    avgWindowSize: 0,
  });
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);
  const processorRef = useRef(null);
  const windowBufferRef = useRef([]);
  const windowTimerRef = useRef(null);
  const lastEventTimeRef = useRef(null);

  // Initialize stream processor
  useEffect(() => {
    if (!changes.length) return;

    let mounted = true;

    async function initializeProcessor() {
      try {
        setLoading(true);

        // Import stream processor module
        const { StreamProcessor } = await import(
          '../../knowledge-engine/streaming/stream-processor.mjs'
        );

        // Create stream processor
        const processor = new StreamProcessor({
          windowType: config.windowType || 'tumbling',
          windowSize: config.windowSize || 5000,
          windowSlide: config.windowSlide,
          sessionGap: config.sessionGap || 30000,
          aggregator: config.aggregator || defaultAggregator,
          onWindowComplete: window => {
            if (!mounted) return;

            setWindows(prev => [...prev, window]);
            setStats(prev => ({
              windowsProcessed: prev.windowsProcessed + 1,
              eventsProcessed: prev.eventsProcessed + window.events.length,
              avgWindowSize: Math.round(
                (prev.eventsProcessed + window.events.length) / (prev.windowsProcessed + 1)
              ),
            }));
          },
        });

        if (!mounted) return;

        processorRef.current = processor;
        setLoading(false);
      } catch (err) {
        if (!mounted) return;
        setError(err);
        setLoading(false);
      }
    }

    initializeProcessor();

    return () => {
      mounted = false;
      if (windowTimerRef.current) {
        clearTimeout(windowTimerRef.current);
      }
    };
  }, [config.windowType, config.windowSize, config.windowSlide, config.sessionGap]);

  // Default aggregator
  function defaultAggregator(events) {
    return {
      count: events.length,
      operations: {
        inserts: events.filter(e => e.operation === 'insert').length,
        deletes: events.filter(e => e.operation === 'delete').length,
      },
      startTime: events[0]?.timestamp,
      endTime: events[events.length - 1]?.timestamp,
    };
  }

  // Process changes into windows
  useEffect(() => {
    if (!isProcessing || !processorRef.current || !changes.length) return;

    const latestChange = changes[changes.length - 1];
    processEvent(latestChange);
  }, [changes, isProcessing]);

  // Process individual event
  const processEvent = useCallback(
    async event => {
      if (!processorRef.current) return;

      try {
        const windowType = config.windowType || 'tumbling';
        const windowSize = config.windowSize || 5000;
        const sessionGap = config.sessionGap || 30000;

        windowBufferRef.current.push(event);

        if (windowType === 'tumbling') {
          // Tumbling window: fixed-size, non-overlapping
          if (!windowTimerRef.current) {
            windowTimerRef.current = setTimeout(() => {
              completeWindow();
              windowTimerRef.current = null;
            }, windowSize);
          }
        } else if (windowType === 'sliding') {
          // Sliding window: overlapping windows
          const slideInterval = config.windowSlide || Math.floor(windowSize / 2);
          if (!windowTimerRef.current) {
            windowTimerRef.current = setInterval(() => {
              completeWindow(true); // Keep some events for next window
            }, slideInterval);
          }
        } else if (windowType === 'session') {
          // Session window: gap-based
          const now = Date.now();
          if (lastEventTimeRef.current && now - lastEventTimeRef.current > sessionGap) {
            completeWindow();
          }
          lastEventTimeRef.current = now;

          if (windowTimerRef.current) {
            clearTimeout(windowTimerRef.current);
          }
          windowTimerRef.current = setTimeout(() => {
            completeWindow();
            windowTimerRef.current = null;
          }, sessionGap);
        }

        // Update current window preview
        setCurrentWindow({
          events: [...windowBufferRef.current],
          size: windowBufferRef.current.length,
          startTime: windowBufferRef.current[0]?.timestamp,
          endTime: event.timestamp,
        });
      } catch (err) {
        setError(err);
      }
    },
    [config]
  );

  // Complete window and apply aggregation
  const completeWindow = useCallback(
    (keepOverlap = false) => {
      if (windowBufferRef.current.length === 0) return;

      const events = [...windowBufferRef.current];
      const aggregated = (config.aggregator || defaultAggregator)(events);

      const window = {
        id: `window-${Date.now()}`,
        events,
        aggregated,
        startTime: events[0]?.timestamp,
        endTime: events[events.length - 1]?.timestamp,
        size: events.length,
      };

      setWindows(prev => [...prev, window]);
      setStats(prev => ({
        windowsProcessed: prev.windowsProcessed + 1,
        eventsProcessed: prev.eventsProcessed + events.length,
        avgWindowSize: Math.round(
          (prev.eventsProcessed + events.length) / (prev.windowsProcessed + 1)
        ),
      }));

      // For sliding windows, keep overlapping events
      if (keepOverlap) {
        const slideInterval = config.windowSlide || Math.floor((config.windowSize || 5000) / 2);
        const cutoffTime = Date.now() - slideInterval;
        windowBufferRef.current = windowBufferRef.current.filter(
          e => new Date(e.timestamp).getTime() > cutoffTime
        );
      } else {
        windowBufferRef.current = [];
      }

      setCurrentWindow(null);
    },
    [config]
  );

  // Start stream processing
  const start = useCallback(async () => {
    try {
      await startFeed();
      setIsProcessing(true);
      return { success: true };
    } catch (err) {
      setError(err);
      throw err;
    }
  }, [startFeed]);

  // Stop stream processing
  const stop = useCallback(async () => {
    try {
      await stopFeed();
      setIsProcessing(false);

      // Complete any pending window
      if (windowBufferRef.current.length > 0) {
        completeWindow();
      }

      if (windowTimerRef.current) {
        clearTimeout(windowTimerRef.current);
        windowTimerRef.current = null;
      }

      return { success: true };
    } catch (err) {
      setError(err);
      throw err;
    }
  }, [stopFeed, completeWindow]);

  // Clear windows
  const clear = useCallback(() => {
    setWindows([]);
    setCurrentWindow(null);
    windowBufferRef.current = [];
    setStats({
      windowsProcessed: 0,
      eventsProcessed: 0,
      avgWindowSize: 0,
    });
  }, []);

  // Get windows by time range
  const getWindowsByTimeRange = useCallback(
    (startTime, endTime) => {
      return windows.filter(
        w =>
          new Date(w.startTime) >= new Date(startTime) && new Date(w.endTime) <= new Date(endTime)
      );
    },
    [windows]
  );

  return {
    windows,
    currentWindow,
    start,
    stop,
    clear,
    isProcessing: isProcessing && feedRunning,
    stats,
    loading,
    error,
    getWindowsByTimeRange,
  };
}
