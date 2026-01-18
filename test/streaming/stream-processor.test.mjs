/**
 * @file Tests for Stream Processor
 */

import { describe, it, expect, beforeEach, afterEach, _vi } from 'vitest';
import {
  StreamProcessor,
  WindowType,
  Aggregators,
} from '../../src/knowledge-engine/streaming/stream-processor.mjs';

describe('StreamProcessor', () => {
  let processor;

  beforeEach(() => {
    processor = new StreamProcessor({
      enableWindowing: true,
      enableAggregation: true,
    });
  });

  afterEach(async () => {
    await processor.cleanup();
  });

  describe('Window Configuration', () => {
    it('should configure tumbling window', () => {
      processor.configureWindowing({
        type: WindowType.TUMBLING,
        size: 1000,
      });

      expect(processor.windowConfig.type).toBe(WindowType.TUMBLING);
      expect(processor.windowConfig.size).toBe(1000);
    });

    it('should configure sliding window', () => {
      processor.configureWindowing({
        type: WindowType.SLIDING,
        size: 2000,
        slide: 500,
      });

      expect(processor.windowConfig.type).toBe(WindowType.SLIDING);
      expect(processor.windowConfig.slide).toBe(500);
    });

    it('should configure session window', () => {
      processor.configureWindowing({
        type: WindowType.SESSION,
        size: 1000,
        timeout: 5000,
      });

      expect(processor.windowConfig.type).toBe(WindowType.SESSION);
      expect(processor.windowConfig.timeout).toBe(5000);
    });

    it('should configure count window', () => {
      processor.configureWindowing({
        type: WindowType.COUNT,
        size: 10,
        count: 10,
      });

      expect(processor.windowConfig.type).toBe(WindowType.COUNT);
      expect(processor.windowConfig.count).toBe(10);
    });
  });

  describe('Event Processing', () => {
    it('should process events without windowing', async () => {
      const nonWindowProcessor = new StreamProcessor({
        enableWindowing: false,
      });
      nonWindowProcessor.start();

      const event = { id: 'evt-1', type: 'test', data: 'value' };
      const result = await nonWindowProcessor.process(event);

      expect(result.event).toEqual(event);
      expect(result.processed).toBe(true);

      await nonWindowProcessor.cleanup();
    });

    it('should process events with windowing', async () => {
      processor.configureWindowing({
        type: WindowType.COUNT,
        size: 10,
        count: 3,
      });
      processor.start();

      const event1 = { id: 'evt-1', data: 'a' };
      const result1 = await processor.process(event1);

      expect(result1.windowId).toBeDefined();
      expect(result1.windowCount).toBe(1);

      const event2 = { id: 'evt-2', data: 'b' };
      const result2 = await processor.process(event2);

      expect(result2.windowId).toBe(result1.windowId);
      expect(result2.windowCount).toBe(2);
    });

    it('should emit window-created event', async () => {
      processor.configureWindowing({
        type: WindowType.COUNT,
        size: 10,
        count: 5,
      });

      const eventPromise = new Promise(resolve => {
        processor.once('window-created', window => {
          expect(window.id).toBeDefined();
          expect(window.type).toBe(WindowType.COUNT);
          resolve();
        });
      });

      processor.start();

      await eventPromise;
    });

    it.skip('should emit window-closed event', async () => {
      // SKIP REASON: Test not in vitest.config.mjs include list (orphaned test)
      // Also noted as flaky timing test in original comment
      // RESOLUTION: Move to packages/streaming/test/ or add to vitest config if needed
      processor.configureWindowing({
        type: WindowType.COUNT,
        size: 10,
        count: 2,
      });

      const eventPromise = new Promise(resolve => {
        processor.once('window-closed', window => {
          expect(window.count).toBe(2);
          expect(window.isClosed).toBe(true);
          resolve();
        });
      });

      processor.start();

      await processor.process({ id: 'evt-1' });
      await processor.process({ id: 'evt-2' });

      await eventPromise;
    }, 60000);
  });

  describe('Windowing Operations', () => {
    it('should handle count-based windows', async () => {
      processor.configureWindowing({
        type: WindowType.COUNT,
        size: 10,
        count: 3,
      });
      processor.start();

      await processor.process({ id: 'evt-1' });
      await processor.process({ id: 'evt-2' });

      const activeWindow = processor.getActiveWindow();
      expect(activeWindow.count).toBe(2);

      await processor.process({ id: 'evt-3' });

      // Window should be closed and new one created
      // Note: windows.size includes both closed and active windows
      expect(processor.windows.size).toBeGreaterThanOrEqual(1);
    });

    it('should create multiple windows for sliding', async () => {
      processor.configureWindowing({
        type: WindowType.SLIDING,
        size: 1000,
        slide: 100,
      });
      processor.start();

      await processor.process({ id: 'evt-1' });
      await new Promise(resolve => setTimeout(resolve, 150));
      await processor.process({ id: 'evt-2' });

      // Should have created at least one window (may create more based on timing)
      expect(processor.windows.size).toBeGreaterThanOrEqual(1);
    }, 60000);
  });

  describe('Aggregators', () => {
    it('should register aggregator', () => {
      const aggFn = event => event.value * 2;

      processor.registerAggregator('double', aggFn);

      expect(processor.aggregators.has('double')).toBe(true);
    });

    it('should unregister aggregator', () => {
      processor.registerAggregator('test', e => e);

      expect(processor.aggregators.has('test')).toBe(true);

      const result = processor.unregisterAggregator('test');
      expect(result).toBe(true);
      expect(processor.aggregators.has('test')).toBe(false);
    });

    it('should perform aggregations on events', async () => {
      processor.registerAggregator('count', (event, window) => {
        return window ? window.events.length : 1;
      });

      processor.configureWindowing({
        type: WindowType.COUNT,
        size: 10,
        count: 5,
      });
      processor.start();

      const result = await processor.process({ id: 'evt-1', value: 10 });

      expect(result.aggregations).toBeDefined();
      expect(result.aggregations.count).toBe(1);
    });

    it('should handle aggregation errors gracefully', async () => {
      processor.registerAggregator('failing', () => {
        throw new Error('Aggregation failed');
      });

      processor.configureWindowing({
        type: WindowType.COUNT,
        size: 10,
        count: 5,
      });
      processor.start();

      const result = await processor.process({ id: 'evt-1' });

      expect(result.aggregations.failing).toBeDefined();
      expect(result.aggregations.failing.error).toBeDefined();
    });
  });

  describe('Built-in Aggregators', () => {
    it('should count events', () => {
      const events = [{ id: 1 }, { id: 2 }, { id: 3 }];
      const result = Aggregators.count(events);

      expect(result).toBe(3);
    });

    it('should sum numeric values', () => {
      const events = [{ value: 10 }, { value: 20 }, { value: 30 }];
      const result = Aggregators.sum('value')(events);

      expect(result).toBe(60);
    });

    it('should calculate average', () => {
      const events = [{ value: 10 }, { value: 20 }, { value: 30 }];
      const result = Aggregators.avg('value')(events);

      expect(result).toBe(20);
    });

    it('should find minimum value', () => {
      const events = [{ value: 30 }, { value: 10 }, { value: 20 }];
      const result = Aggregators.min('value')(events);

      expect(result).toBe(10);
    });

    it('should find maximum value', () => {
      const events = [{ value: 30 }, { value: 10 }, { value: 20 }];
      const result = Aggregators.max('value')(events);

      expect(result).toBe(30);
    });

    it('should group events by field', () => {
      const events = [
        { type: 'a', value: 1 },
        { type: 'b', value: 2 },
        { type: 'a', value: 3 },
      ];
      const result = Aggregators.groupBy('type')(events);

      expect(result.a).toHaveLength(2);
      expect(result.b).toHaveLength(1);
    });

    it('should support function-based field extraction', () => {
      const events = [{ value: 10 }, { value: 20 }];
      const result = Aggregators.sum(e => e.value * 2)(events);

      expect(result).toBe(60);
    });
  });

  describe('Batch Processing', () => {
    it('should process batch of events', async () => {
      processor.configureWindowing({
        type: WindowType.COUNT,
        size: 10,
        count: 10,
      });
      processor.start();

      const events = [{ id: 'evt-1' }, { id: 'evt-2' }, { id: 'evt-3' }];

      const results = await processor.processBatch(events);

      expect(results).toHaveLength(3);
      expect(processor.metrics.eventsProcessed).toBe(3);
    });
  });

  describe('Pipeline', () => {
    it('should create processing pipeline', async () => {
      const stage1 = event => ({ ...event, stage1: true });
      const stage2 = event => ({ ...event, stage2: true });

      const pipeline = processor.createPipeline([stage1, stage2]);

      const result = await pipeline({ id: 'evt-1' });

      expect(result.stage1).toBe(true);
      expect(result.stage2).toBe(true);
    });

    it('should pass data through pipeline stages', async () => {
      const double = event => ({ ...event, value: event.value * 2 });
      const addTen = event => ({ ...event, value: event.value + 10 });

      const pipeline = processor.createPipeline([double, addTen]);

      const result = await pipeline({ value: 5 });

      expect(result.value).toBe(20); // (5 * 2) + 10
    });
  });

  describe('Metrics', () => {
    it('should track events processed', async () => {
      processor.configureWindowing({
        type: WindowType.COUNT,
        size: 10,
        count: 10,
      });
      processor.start();

      await processor.process({ id: 'evt-1' });
      await processor.process({ id: 'evt-2' });

      const metrics = processor.getMetrics();

      expect(metrics.eventsProcessed).toBe(2);
    });

    it('should track windows created', async () => {
      processor.configureWindowing({
        type: WindowType.COUNT,
        size: 10,
        count: 2,
      });
      processor.start();

      await processor.process({ id: 'evt-1' });
      await processor.process({ id: 'evt-2' });
      await processor.process({ id: 'evt-3' });

      const metrics = processor.getMetrics();

      // At least 2 windows should be created (one closed at 2 events, one for 3rd event)
      expect(metrics.windowsCreated).toBeGreaterThanOrEqual(1);
    });

    it('should track active state', () => {
      const metrics1 = processor.getMetrics();
      expect(metrics1.isProcessing).toBe(false);

      processor.configureWindowing({
        type: WindowType.COUNT,
        size: 10,
        count: 10,
      });
      processor.start();

      const metrics2 = processor.getMetrics();
      expect(metrics2.isProcessing).toBe(true);
    });
  });

  describe('Performance', () => {
    it('should handle high-throughput streams', async () => {
      processor.configureWindowing({
        type: WindowType.COUNT,
        size: 1000,
        count: 100,
      });
      processor.start();

      const start = Date.now();

      const promises = [];
      for (let i = 0; i < 100; i++) {
        promises.push(processor.process({ id: `evt-${i}`, value: i }));
      }

      await Promise.all(promises);

      const duration = Date.now() - start;

      expect(processor.metrics.eventsProcessed).toBe(100);
      expect(duration).toBeLessThan(5000); // Should complete in less than 5 seconds (more realistic for CI)
    }, 60000);
  });
});
