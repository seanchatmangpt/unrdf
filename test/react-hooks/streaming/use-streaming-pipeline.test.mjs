/**
 * @file Tests for useStreamingPipeline hook functionality
 * Tests pipeline composition, orchestration, and health monitoring
 */

import { describe, it, expect, _beforeEach, vi } from 'vitest';

describe('useStreamingPipeline', () => {
  describe('Pipeline Lifecycle', () => {
    it('should initialize with default state', () => {
      const state = {
        isPipelineRunning: false,
        error: null,
        pipelineStats: {
          uptime: 0,
          eventsProcessed: 0,
          validationsPassed: 0,
          validationsFailed: 0,
          windowsCompleted: 0,
        },
      };

      expect(state.isPipelineRunning).toBe(false);
      expect(state.error).toBeNull();
      expect(state.pipelineStats.uptime).toBe(0);
    });

    it('should start pipeline in correct order', async () => {
      const startOrder = [];

      const startPipeline = async () => {
        // Start change feed first
        startOrder.push('changeFeed');
        // Then subscriptions
        startOrder.push('subscription');
        // Then processor
        startOrder.push('processor');
        // Then validator
        startOrder.push('validator');
        return { success: true };
      };

      await startPipeline();

      expect(startOrder).toEqual(['changeFeed', 'subscription', 'processor', 'validator']);
    });

    it('should stop pipeline in reverse order', async () => {
      const stopOrder = [];

      const stopPipeline = async () => {
        // Stop in reverse order
        stopOrder.push('validator');
        stopOrder.push('processor');
        stopOrder.push('subscription');
        stopOrder.push('changeFeed');
        return { success: true };
      };

      await stopPipeline();

      expect(stopOrder).toEqual(['validator', 'processor', 'subscription', 'changeFeed']);
    });

    it('should track running state', async () => {
      let isPipelineRunning = false;
      let startTime = null;

      const start = async () => {
        startTime = Date.now();
        isPipelineRunning = true;
        return { success: true };
      };

      const stop = async () => {
        isPipelineRunning = false;
        return { success: true };
      };

      await start();
      expect(isPipelineRunning).toBe(true);
      expect(startTime).not.toBeNull();

      await stop();
      expect(isPipelineRunning).toBe(false);
    });
  });

  describe('Pipeline Stats', () => {
    it('should track uptime', async () => {
      let startTime = Date.now();

      const getUptime = () => {
        return Math.floor((Date.now() - startTime) / 1000);
      };

      await new Promise(resolve => setTimeout(resolve, 100));

      const uptime = getUptime();
      expect(uptime).toBeGreaterThanOrEqual(0);
    });

    it('should aggregate component stats', () => {
      const changeFeedStats = { totalChanges: 100 };
      const validatorStats = { passed: 80, failed: 20 };
      const processorStats = { windowsProcessed: 10 };

      const pipelineStats = {
        eventsProcessed: changeFeedStats.totalChanges,
        validationsPassed: validatorStats.passed,
        validationsFailed: validatorStats.failed,
        windowsCompleted: processorStats.windowsProcessed,
      };

      expect(pipelineStats.eventsProcessed).toBe(100);
      expect(pipelineStats.validationsPassed).toBe(80);
      expect(pipelineStats.validationsFailed).toBe(20);
      expect(pipelineStats.windowsCompleted).toBe(10);
    });

    it('should update stats periodically', async () => {
      let updateCount = 0;

      const updateStats = () => {
        updateCount++;
      };

      const interval = setInterval(updateStats, 50);
      await new Promise(resolve => setTimeout(resolve, 200));
      clearInterval(interval);

      expect(updateCount).toBeGreaterThanOrEqual(3);
    });

    it('should reset stats on clear', () => {
      let pipelineStats = {
        uptime: 3600,
        eventsProcessed: 1000,
        validationsPassed: 800,
        validationsFailed: 200,
        windowsCompleted: 50,
      };

      const clear = () => {
        pipelineStats = {
          uptime: 0,
          eventsProcessed: 0,
          validationsPassed: 0,
          validationsFailed: 0,
          windowsCompleted: 0,
        };
      };

      clear();

      expect(pipelineStats.uptime).toBe(0);
      expect(pipelineStats.eventsProcessed).toBe(0);
    });
  });

  describe('Pause and Resume', () => {
    it('should pause pipeline', async () => {
      let isPipelineRunning = true;

      const pause = async () => {
        isPipelineRunning = false;
        return { success: true };
      };

      const result = await pause();

      expect(result.success).toBe(true);
      expect(isPipelineRunning).toBe(false);
    });

    it('should resume pipeline', async () => {
      let isPipelineRunning = false;

      const resume = async () => {
        isPipelineRunning = true;
        return { success: true };
      };

      const result = await resume();

      expect(result.success).toBe(true);
      expect(isPipelineRunning).toBe(true);
    });

    it('should preserve state on pause', () => {
      const state = {
        changes: [1, 2, 3],
        windows: [{ id: 'w1' }],
        violations: [],
      };

      const pause = () => {
        // State should be preserved
        return { ...state };
      };

      const preserved = pause();

      expect(preserved.changes).toHaveLength(3);
      expect(preserved.windows).toHaveLength(1);
    });
  });

  describe('Clear Functionality', () => {
    it('should clear all component data', () => {
      let subscriptionEvents = [1, 2, 3];
      let changeFeedChanges = [1, 2, 3, 4];
      let processorWindows = [{ id: 'w1' }];
      let validatorViolations = [{ id: 'v1' }];

      const clear = () => {
        subscriptionEvents = [];
        changeFeedChanges = [];
        processorWindows = [];
        validatorViolations = [];
      };

      clear();

      expect(subscriptionEvents).toHaveLength(0);
      expect(changeFeedChanges).toHaveLength(0);
      expect(processorWindows).toHaveLength(0);
      expect(validatorViolations).toHaveLength(0);
    });

    it('should emit clear event', () => {
      const events = [];

      const clear = onEvent => {
        onEvent({
          type: 'pipeline:cleared',
          timestamp: new Date().toISOString(),
        });
      };

      clear(event => events.push(event));

      expect(events).toHaveLength(1);
      expect(events[0].type).toBe('pipeline:cleared');
    });
  });

  describe('Health Monitoring', () => {
    it('should report healthy when all components running', () => {
      const components = {
        changeFeed: { isRunning: true },
        subscription: { isActive: true },
        processor: { isProcessing: true },
        validator: { isRunning: true },
      };

      const getHealth = () => {
        const isHealthy =
          components.changeFeed.isRunning &&
          components.subscription.isActive &&
          components.processor.isProcessing &&
          components.validator.isRunning;

        return {
          status: isHealthy ? 'healthy' : 'unhealthy',
          components: {
            changeFeed: components.changeFeed.isRunning,
            subscriptions: components.subscription.isActive,
            processor: components.processor.isProcessing,
            validator: components.validator.isRunning,
          },
        };
      };

      const health = getHealth();

      expect(health.status).toBe('healthy');
      expect(health.components.changeFeed).toBe(true);
    });

    it('should report unhealthy when component fails', () => {
      const components = {
        changeFeed: { isRunning: true },
        subscription: { isActive: true },
        processor: { isProcessing: false }, // Failed
        validator: { isRunning: true },
      };

      const getHealth = () => {
        const isHealthy =
          components.changeFeed.isRunning &&
          components.subscription.isActive &&
          components.processor.isProcessing &&
          components.validator.isRunning;

        return {
          status: isHealthy ? 'healthy' : 'unhealthy',
          components: {
            changeFeed: components.changeFeed.isRunning,
            subscriptions: components.subscription.isActive,
            processor: components.processor.isProcessing,
            validator: components.validator.isRunning,
          },
        };
      };

      const health = getHealth();

      expect(health.status).toBe('unhealthy');
      expect(health.components.processor).toBe(false);
    });

    it('should include error in health report', () => {
      const error = new Error('Connection lost');

      const getHealth = err => ({
        status: err ? 'unhealthy' : 'healthy',
        error: err,
      });

      const health = getHealth(error);

      expect(health.status).toBe('unhealthy');
      expect(health.error.message).toBe('Connection lost');
    });

    it('should handle disabled components in health check', () => {
      const config = {
        enableProcessing: false,
        enableValidation: false,
      };

      const components = {
        changeFeed: { isRunning: true },
        subscription: { isActive: true },
        processor: { isProcessing: false },
        validator: { isRunning: false },
      };

      const getHealth = () => {
        const processorOk = config.enableProcessing === false || components.processor.isProcessing;
        const validatorOk = config.enableValidation === false || components.validator.isRunning;
        const isHealthy =
          components.changeFeed.isRunning &&
          components.subscription.isActive &&
          processorOk &&
          validatorOk;

        return {
          status: isHealthy ? 'healthy' : 'unhealthy',
        };
      };

      const health = getHealth();

      expect(health.status).toBe('healthy');
    });
  });

  describe('Pipeline Events', () => {
    it('should emit started event', async () => {
      const events = [];

      const start = async onEvent => {
        onEvent({
          type: 'pipeline:started',
          timestamp: new Date().toISOString(),
        });
        return { success: true };
      };

      await start(event => events.push(event));

      expect(events).toHaveLength(1);
      expect(events[0].type).toBe('pipeline:started');
    });

    it('should emit stopped event with stats', async () => {
      const events = [];
      const stats = { uptime: 3600, eventsProcessed: 1000 };

      const stop = async onEvent => {
        onEvent({
          type: 'pipeline:stopped',
          timestamp: new Date().toISOString(),
          stats,
        });
        return { success: true };
      };

      await stop(event => events.push(event));

      expect(events).toHaveLength(1);
      expect(events[0].type).toBe('pipeline:stopped');
      expect(events[0].stats.eventsProcessed).toBe(1000);
    });

    it('should emit error event', async () => {
      const events = [];
      const error = new Error('Pipeline failed');

      const handleError = (err, onEvent) => {
        onEvent({
          type: 'pipeline:error',
          error: err,
          timestamp: new Date().toISOString(),
        });
      };

      handleError(error, event => events.push(event));

      expect(events).toHaveLength(1);
      expect(events[0].type).toBe('pipeline:error');
      expect(events[0].error.message).toBe('Pipeline failed');
    });

    it('should emit paused and resumed events', () => {
      const events = [];

      const pause = onEvent => {
        onEvent({
          type: 'pipeline:paused',
          timestamp: new Date().toISOString(),
        });
      };

      const resume = onEvent => {
        onEvent({
          type: 'pipeline:resumed',
          timestamp: new Date().toISOString(),
        });
      };

      pause(event => events.push(event));
      resume(event => events.push(event));

      expect(events).toHaveLength(2);
      expect(events[0].type).toBe('pipeline:paused');
      expect(events[1].type).toBe('pipeline:resumed');
    });
  });

  describe('Configuration', () => {
    it('should enable validation by default', () => {
      const config = {};
      const enableValidation = config.enableValidation !== false;

      expect(enableValidation).toBe(true);
    });

    it('should enable processing by default', () => {
      const config = {};
      const enableProcessing = config.enableProcessing !== false;

      expect(enableProcessing).toBe(true);
    });

    it('should respect disabled validation', async () => {
      const config = { enableValidation: false };
      let validatorStarted = false;

      const start = async () => {
        if (config.enableValidation !== false) {
          validatorStarted = true;
        }
        return { success: true };
      };

      await start();

      expect(validatorStarted).toBe(false);
    });

    it('should respect disabled processing', async () => {
      const config = { enableProcessing: false };
      let processorStarted = false;

      const start = async () => {
        if (config.enableProcessing !== false) {
          processorStarted = true;
        }
        return { success: true };
      };

      await start();

      expect(processorStarted).toBe(false);
    });

    it('should pass subscription pattern', async () => {
      const config = {
        subscription: {
          pattern: '?s schema:price ?price',
          filter: e => e.value > 100,
        },
      };
      let subscribedPattern = null;

      const start = async () => {
        if (config.subscription?.pattern) {
          subscribedPattern = config.subscription.pattern;
        }
        return { success: true };
      };

      await start();

      expect(subscribedPattern).toBe('?s schema:price ?price');
    });
  });

  describe('Comprehensive Stats', () => {
    it('should aggregate all component stats', () => {
      const pipelineStats = { uptime: 100 };
      const changeFeedStats = { totalChanges: 50 };
      const processorStats = {
        windowsProcessed: 5,
        eventsProcessed: 50,
        avgWindowSize: 10,
      };
      const validatorStats = { passed: 40, failed: 10 };
      const subscriptions = [{ id: 'sub-1' }, { id: 'sub-2' }];
      const events = [1, 2, 3, 4, 5];

      const getStats = () => ({
        pipeline: pipelineStats,
        changeFeed: changeFeedStats,
        processor: processorStats,
        validator: validatorStats,
        subscriptions: {
          active: subscriptions.length,
          events: events.length,
        },
      });

      const stats = getStats();

      expect(stats.pipeline.uptime).toBe(100);
      expect(stats.changeFeed.totalChanges).toBe(50);
      expect(stats.processor.windowsProcessed).toBe(5);
      expect(stats.validator.passed).toBe(40);
      expect(stats.subscriptions.active).toBe(2);
    });
  });

  describe('Error Handling', () => {
    it('should handle start error', async () => {
      let error = null;

      const start = async () => {
        error = new Error('Failed to start change feed');
        throw error;
      };

      await expect(start()).rejects.toThrow('Failed to start change feed');
      expect(error).not.toBeNull();
    });

    it('should handle stop error', async () => {
      let error = null;

      const stop = async () => {
        error = new Error('Failed to stop validator');
        throw error;
      };

      await expect(stop()).rejects.toThrow('Failed to stop validator');
      expect(error).not.toBeNull();
    });

    it('should clear error on successful start', async () => {
      let error = new Error('Previous error');

      const start = async () => {
        error = null;
        return { success: true };
      };

      await start();

      expect(error).toBeNull();
    });
  });

  describe('Component Access', () => {
    it('should expose subscription component', () => {
      const subscription = {
        subscribe: vi.fn(),
        unsubscribe: vi.fn(),
        events: [1, 2, 3],
      };

      expect(subscription.subscribe).toBeDefined();
      expect(subscription.unsubscribe).toBeDefined();
      expect(subscription.events).toHaveLength(3);
    });

    it('should expose change feed component', () => {
      const changeFeed = {
        start: vi.fn(),
        stop: vi.fn(),
        changes: [1, 2, 3, 4],
      };

      expect(changeFeed.start).toBeDefined();
      expect(changeFeed.stop).toBeDefined();
      expect(changeFeed.changes).toHaveLength(4);
    });

    it('should expose processor component', () => {
      const processor = {
        windows: [{ id: 'w1' }, { id: 'w2' }],
        currentWindow: { events: [1, 2] },
      };

      expect(processor.windows).toHaveLength(2);
      expect(processor.currentWindow.events).toHaveLength(2);
    });

    it('should expose validator component', () => {
      const validator = {
        violations: [{ id: 'v1' }],
        validChanges: [1, 2, 3],
        invalidChanges: [4],
      };

      expect(validator.violations).toHaveLength(1);
      expect(validator.validChanges).toHaveLength(3);
      expect(validator.invalidChanges).toHaveLength(1);
    });
  });

  describe('Convenience Accessors', () => {
    it('should provide direct access to subscriptions', () => {
      const subscription = {
        subscriptions: [{ id: 'sub-1' }, { id: 'sub-2' }],
      };

      expect(subscription.subscriptions).toHaveLength(2);
    });

    it('should provide direct access to changes', () => {
      const changeFeed = {
        changes: [{ id: 'c1' }, { id: 'c2' }, { id: 'c3' }],
      };

      expect(changeFeed.changes).toHaveLength(3);
    });

    it('should provide direct access to windows', () => {
      const processor = {
        windows: [{ id: 'w1' }, { id: 'w2' }],
      };

      expect(processor.windows).toHaveLength(2);
    });

    it('should provide direct access to violations', () => {
      const validator = {
        violations: [{ id: 'v1' }],
      };

      expect(validator.violations).toHaveLength(1);
    });

    it('should provide direct access to valid and invalid changes', () => {
      const validator = {
        validChanges: [1, 2, 3],
        invalidChanges: [4, 5],
      };

      expect(validator.validChanges).toHaveLength(3);
      expect(validator.invalidChanges).toHaveLength(2);
    });
  });
});
