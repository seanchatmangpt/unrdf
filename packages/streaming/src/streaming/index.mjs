/**
 * @file Streaming module entry point
 * @module streaming
 *
 * @description
 * Real-time streaming and WebSocket subscriptions for UNRDF knowledge graphs.
 * Provides subscription management, change feeds, stream processing, and
 * real-time SHACL validation.
 */

import {
  SubscriptionManager,
  createSubscriptionManager,
  SubscriptionPatternType,
} from './subscription-manager.mjs';

import { ChangeFeed, createChangeFeed, createChangeFeedHook, ChangeType } from './change-feed.mjs';

import {
  StreamProcessor,
  createStreamProcessor,
  WindowType,
  Aggregators,
} from './stream-processor.mjs';

import {
  RealTimeValidator,
  createRealTimeValidator,
  ValidationMode,
} from './real-time-validator.mjs';

// Re-export all imported items
export {
  SubscriptionManager,
  createSubscriptionManager,
  SubscriptionPatternType,
  ChangeFeed,
  createChangeFeed,
  createChangeFeedHook,
  ChangeType,
  StreamProcessor,
  createStreamProcessor,
  WindowType,
  Aggregators,
  RealTimeValidator,
  createRealTimeValidator,
  ValidationMode,
};

/**
 * Create a complete streaming pipeline
 * @param {Object} config - Pipeline configuration
 * @returns {Object} Streaming pipeline
 */
export function createStreamingPipeline(config = {}) {
  const {
    subscriptionManager: subConfig,
    changeFeed: feedConfig,
    streamProcessor: processorConfig,
    validator: validatorConfig,
  } = config;

  const subscriptionManager = createSubscriptionManager(subConfig);
  const changeFeed = createChangeFeed(feedConfig);
  const streamProcessor = createStreamProcessor(processorConfig);
  const validator = validatorConfig ? createRealTimeValidator(validatorConfig) : null;

  // Wire up the pipeline
  changeFeed.on('change', async change => {
    try {
      // Process through stream processor
      const result = await streamProcessor.process(change);

      // Validate if validator is configured
      if (validator && change.delta) {
        const validationResult = await validator.validateDelta(change.delta);
        result.validation = validationResult;
      }

      // Emit processed result
      subscriptionManager.emit('pipeline-result', result);
    } catch (error) {
      console.error('[StreamingPipeline] Processing error:', error.message);
      subscriptionManager.emit('pipeline-error', error);
    }
  });

  return {
    subscriptionManager,
    changeFeed,
    streamProcessor,
    validator,

    /**
     * Start the pipeline
     */
    start() {
      changeFeed.start();
      streamProcessor.start();
    },

    /**
     * Stop the pipeline
     */
    stop() {
      changeFeed.stop();
      streamProcessor.stop();
    },

    /**
     * Get pipeline metrics
     */
    getMetrics() {
      return {
        subscriptionManager: subscriptionManager.getMetrics(),
        changeFeed: changeFeed.getMetrics(),
        streamProcessor: streamProcessor.getMetrics(),
        validator: validator ? validator.getMetrics() : null,
      };
    },

    /**
     * Cleanup pipeline resources
     */
    async cleanup() {
      await subscriptionManager.cleanup();
      await changeFeed.cleanup();
      await streamProcessor.cleanup();
      if (validator) {
        await validator.cleanup();
      }
    },
  };
}
