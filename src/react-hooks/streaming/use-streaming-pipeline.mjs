/**
 * @file use-streaming-pipeline.mjs
 * @description React hook for complete streaming orchestration pipeline
 * @since 3.2.0
 */

import { useState, useCallback, useEffect, useRef } from 'react';
import { useSubscriptionManager } from './use-subscription-manager.mjs';
import { useChangeFeed } from './use-change-feed.mjs';
import { useStreamProcessor } from './use-stream-processor.mjs';
import { useRealTimeValidator } from './use-real-time-validator.mjs';

/**
 * Hook for orchestrating complete streaming pipeline with subscriptions,
 * change feeds, processing, and validation
 *
 * @since 3.2.0
 * @param {Object} config - Pipeline configuration
 * @param {Object} [config.subscription] - Subscription config
 * @param {Object} [config.changeFeed] - Change feed config
 * @param {Object} [config.processor] - Stream processor config
 * @param {Object} [config.validator] - Validator config
 * @param {boolean} [config.enableValidation=true] - Enable validation
 * @param {boolean} [config.enableProcessing=true] - Enable stream processing
 * @param {Function} [config.onPipelineEvent] - Pipeline event callback
 * @returns {Object} Pipeline state and operations
 * @throws {Error} When any pipeline component fails to start
 * @throws {Error} When stop is called while pipeline is not running
 * @performance Combines 4 hooks with cumulative memory/CPU overhead. Disable unused
 *   features via enableValidation/enableProcessing. Monitor health.status for degradation.
 *   Use pause/resume for temporary stops without full teardown.
 *
 * @example
 * // Full pipeline with all features
 * const {
 *   start,
 *   stop,
 *   isPipelineRunning,
 *   subscriptions,
 *   changes,
 *   windows,
 *   violations,
 *   stats
 * } = useStreamingPipeline({
 *   subscription: {
 *     pattern: `?s schema:price ?price`
 *   },
 *   processor: {
 *     windowType: 'tumbling',
 *     windowSize: 5000,
 *     aggregator: (events) => ({
 *       avgPrice: events.reduce((sum, e) => sum + e.price, 0) / events.length
 *     })
 *   },
 *   validator: {
 *     shapeGraph: 'http://example.org/shapes',
 *     onViolation: (v) => console.warn('Violation:', v)
 *   },
 *   enableValidation: true,
 *   enableProcessing: true
 * });
 *
 * @example
 * // Minimal pipeline without validation
 * const { start, changes, health } = useStreamingPipeline({
 *   enableValidation: false,
 *   enableProcessing: false
 * });
 */
export function useStreamingPipeline(config = {}) {
  // Initialize all pipeline components
  const subscription = useSubscriptionManager(config.subscription || {});
  const changeFeed = useChangeFeed(config.changeFeed || {});
  const processor = useStreamProcessor(config.processor || { changeFeed: config.changeFeed });
  const validator = useRealTimeValidator(config.validator || { changeFeed: config.changeFeed });

  const [isPipelineRunning, setIsPipelineRunning] = useState(false);
  const [pipelineStats, setPipelineStats] = useState({
    uptime: 0,
    eventsProcessed: 0,
    validationsPassed: 0,
    validationsFailed: 0,
    windowsCompleted: 0
  });
  const [error, setError] = useState(null);
  const uptimeIntervalRef = useRef(null);
  const startTimeRef = useRef(null);

  // Update pipeline stats
  useEffect(() => {
    if (!isPipelineRunning) return;

    const updateStats = () => {
      const uptime = startTimeRef.current
        ? Math.floor((Date.now() - startTimeRef.current) / 1000)
        : 0;

      setPipelineStats({
        uptime,
        eventsProcessed: changeFeed.stats.totalChanges,
        validationsPassed: validator.stats.passed,
        validationsFailed: validator.stats.failed,
        windowsCompleted: processor.stats.windowsProcessed
      });
    };

    uptimeIntervalRef.current = setInterval(updateStats, 1000);

    return () => {
      if (uptimeIntervalRef.current) {
        clearInterval(uptimeIntervalRef.current);
      }
    };
  }, [isPipelineRunning, changeFeed.stats, validator.stats, processor.stats]);

  // Start entire pipeline
  const start = useCallback(async () => {
    try {
      setError(null);
      startTimeRef.current = Date.now();

      // Start change feed first
      await changeFeed.start();

      // Start subscriptions
      if (config.subscription?.pattern) {
        await subscription.subscribe(config.subscription.pattern, {
          filter: config.subscription.filter
        });
      }

      // Start stream processor
      if (config.enableProcessing !== false) {
        await processor.start();
      }

      // Start validator
      if (config.enableValidation !== false) {
        await validator.start();
      }

      setIsPipelineRunning(true);

      config.onPipelineEvent?.({
        type: 'pipeline:started',
        timestamp: new Date().toISOString()
      });

      return { success: true };
    } catch (err) {
      setError(err);
      config.onPipelineEvent?.({
        type: 'pipeline:error',
        error: err,
        timestamp: new Date().toISOString()
      });
      throw err;
    }
  }, [subscription, changeFeed, processor, validator, config]);

  // Stop entire pipeline
  const stop = useCallback(async () => {
    try {
      setError(null);

      // Stop in reverse order
      if (config.enableValidation !== false) {
        await validator.stop();
      }

      if (config.enableProcessing !== false) {
        await processor.stop();
      }

      await subscription.unsubscribeAll();
      await changeFeed.stop();

      setIsPipelineRunning(false);

      config.onPipelineEvent?.({
        type: 'pipeline:stopped',
        timestamp: new Date().toISOString(),
        stats: pipelineStats
      });

      return { success: true };
    } catch (err) {
      setError(err);
      config.onPipelineEvent?.({
        type: 'pipeline:error',
        error: err,
        timestamp: new Date().toISOString()
      });
      throw err;
    }
  }, [subscription, changeFeed, processor, validator, config, pipelineStats]);

  // Pause pipeline
  const pause = useCallback(async () => {
    try {
      await changeFeed.stop();
      setIsPipelineRunning(false);

      config.onPipelineEvent?.({
        type: 'pipeline:paused',
        timestamp: new Date().toISOString()
      });

      return { success: true };
    } catch (err) {
      setError(err);
      throw err;
    }
  }, [changeFeed, config]);

  // Resume pipeline
  const resume = useCallback(async () => {
    try {
      await changeFeed.start();
      setIsPipelineRunning(true);

      config.onPipelineEvent?.({
        type: 'pipeline:resumed',
        timestamp: new Date().toISOString()
      });

      return { success: true };
    } catch (err) {
      setError(err);
      throw err;
    }
  }, [changeFeed, config]);

  // Clear all pipeline data
  const clear = useCallback(() => {
    subscription.clear();
    changeFeed.clear();
    processor.clear();
    validator.clear();

    setPipelineStats({
      uptime: 0,
      eventsProcessed: 0,
      validationsPassed: 0,
      validationsFailed: 0,
      windowsCompleted: 0
    });

    config.onPipelineEvent?.({
      type: 'pipeline:cleared',
      timestamp: new Date().toISOString()
    });
  }, [subscription, changeFeed, processor, validator, config]);

  // Get pipeline health
  const getHealth = useCallback(() => {
    const isHealthy = isPipelineRunning &&
      !error &&
      changeFeed.isRunning &&
      (config.enableProcessing === false || processor.isProcessing) &&
      (config.enableValidation === false || validator.isRunning);

    return {
      status: isHealthy ? 'healthy' : 'unhealthy',
      components: {
        changeFeed: changeFeed.isRunning,
        subscriptions: subscription.isActive,
        processor: config.enableProcessing === false || processor.isProcessing,
        validator: config.enableValidation === false || validator.isRunning
      },
      error
    };
  }, [isPipelineRunning, error, changeFeed, subscription, processor, validator, config]);

  // Get comprehensive stats
  const getStats = useCallback(() => {
    return {
      pipeline: pipelineStats,
      changeFeed: changeFeed.stats,
      processor: processor.stats,
      validator: validator.stats,
      subscriptions: {
        active: subscription.subscriptions.length,
        events: subscription.events.length
      }
    };
  }, [pipelineStats, changeFeed, processor, validator, subscription]);

  return {
    // Pipeline control
    start,
    stop,
    pause,
    resume,
    clear,

    // Pipeline state
    isPipelineRunning,
    error,
    health: getHealth(),
    stats: getStats(),

    // Component access
    subscription,
    changeFeed,
    processor,
    validator,

    // Convenience accessors
    subscriptions: subscription.subscriptions,
    changes: changeFeed.changes,
    windows: processor.windows,
    violations: validator.violations,
    validChanges: validator.validChanges,
    invalidChanges: validator.invalidChanges
  };
}
