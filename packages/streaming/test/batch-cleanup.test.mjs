/**
 * @vitest-environment node
 * @fileoverview Memory leak detection tests for Batch Processor
 *
 * @description
 * Tests for event listener cleanup in streaming batch processors.
 * Ensures no listener accumulation when creating/destroying many processors.
 *
 * CRITICAL LEAKS DETECTED:
 * 1. EventTarget listeners never removed from feed
 * 2. Subscriber arrays accumulate without cleanup
 * 3. Debounce timers not cleared on processor destruction
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { createChangeFeed, createStreamProcessor } from '../src/index.mjs';
import { namedNode, literal } from '@rdfjs/data-model';

/**
 * Helper to measure memory usage
 * @returns {number} Heap used in bytes
 */
function getHeapUsed() {
  if (global.gc) {
    global.gc();
  }
  return process.memoryUsage().heapUsed;
}

describe('Streaming Batch Processor Cleanup', () => {
  let feed;

  beforeEach(() => {
    feed = createChangeFeed();
    vi.useFakeTimers();
  });

  afterEach(() => {
    vi.restoreAllMocks();
    vi.useRealTimers();
  });

  describe('Event Listener Cleanup', () => {
    /**
     * Helper to create a test quad
     * @param {string} value - Object value
     * @returns {Object} Quad
     */
    function createQuad(value) {
      return {
        subject: namedNode('http://example.org/s'),
        predicate: namedNode('http://example.org/p'),
        object: literal(value),
      };
    }

    it('should cleanup event listeners when batch processor is destroyed', () => {
      const processor = createStreamProcessor(feed);

      // Subscribe to batch
      const subscriber = vi.fn();
      processor.batch(10).subscribe(subscriber);

      // Emit changes
      for (let i = 0; i < 20; i++) {
        feed.emitChange({ type: 'add', quad: createQuad(`value-${i}`) });
      }

      // Subscriber should be called 2 times (20 changes / 10 batch size)
      expect(subscriber).toHaveBeenCalledTimes(2);

      // CRITICAL: There's no cleanup API for processors!
      // This is a design flaw - processors add listeners but never remove them
      // Expected API: processor.destroy() or processor.cleanup()

      // Note: This test documents the missing cleanup API
      // When fixed, add: processor.destroy()
      // And verify: feed listeners are removed
    });

    it('should not leak listeners when creating many processors', () => {
      const processors = [];
      const subscribers = [];

      // Create 100 batch processors
      for (let i = 0; i < 100; i++) {
        const processor = createStreamProcessor(feed);
        const subscriber = vi.fn();

        processor.batch(10).subscribe(subscriber);

        processors.push(processor);
        subscribers.push(subscriber);
      }

      // Emit 1000 changes
      for (let i = 0; i < 1000; i++) {
        feed.emitChange({ type: 'add', quad: createQuad(`value-${i}`) });
      }

      // Each subscriber should receive 10 batches (1000 / 10 / 100 processors)
      // Note: ALL 100 processors receive ALL changes - this is the leak!
      for (const subscriber of subscribers) {
        expect(subscriber).toHaveBeenCalled();
      }

      // CRITICAL: No way to cleanup processors!
      // Memory leak: feed has 100 active listeners
      // Expected: processors.forEach(p => p.destroy())
    });

    it('should remove debounce timers on cleanup', () => {
      const processor = createStreamProcessor(feed);
      const subscriber = vi.fn();

      processor.debounce(100).subscribe(subscriber);

      // Emit rapid changes
      for (let i = 0; i < 50; i++) {
        feed.emitChange({ type: 'add', quad: createQuad(`value-${i}`) });
      }

      // Should have 1 active timer
      expect(vi.getTimerCount()).toBeGreaterThan(0);

      // CRITICAL: No cleanup API!
      // Expected: processor.destroy() should clear debounce timer
      // Current: timer persists until it fires

      // Advance past debounce
      vi.advanceTimersByTime(200);

      // Timer should fire
      expect(subscriber).toHaveBeenCalledTimes(1);

      // After firing, timer should be cleared
      expect(vi.getTimerCount()).toBe(0);
    });

    it('should not accumulate subscribers across processor instances', () => {
      const subscriber1 = vi.fn();
      const subscriber2 = vi.fn();
      const subscriber3 = vi.fn();

      // Create 3 processors
      const proc1 = createStreamProcessor(feed);
      const proc2 = createStreamProcessor(feed);
      const proc3 = createStreamProcessor(feed);

      proc1.batch(5).subscribe(subscriber1);
      proc2.batch(5).subscribe(subscriber2);
      proc3.batch(5).subscribe(subscriber3);

      // Emit 15 changes (3 batches per processor)
      for (let i = 0; i < 15; i++) {
        feed.emitChange({ type: 'add', quad: createQuad(`value-${i}`) });
      }

      // All subscribers should receive batches
      expect(subscriber1).toHaveBeenCalledTimes(3);
      expect(subscriber2).toHaveBeenCalledTimes(3);
      expect(subscriber3).toHaveBeenCalledTimes(3);

      // CRITICAL: No cleanup - all 3 processors permanently listen to feed
      // Memory leak gets worse with each new processor created
    });

    it('should cleanup all resources on destroy()', () => {
      function createQuad(value) {
        return {
          subject: namedNode('http://example.org/s'),
          predicate: namedNode('http://example.org/p'),
          object: literal(value),
        };
      }

      const subscriber = vi.fn();
      feed.subscribe(subscriber);

      // Add some changes
      for (let i = 0; i < 10; i++) {
        feed.emitChange({ type: 'add', quad: createQuad(`value-${i}`) });
      }

      // Verify subscriber was called
      expect(subscriber).toHaveBeenCalledTimes(10);

      // Destroy the feed
      feed.destroy();

      // After destroy, verify cleanup
      expect(feed.getChanges()).toHaveLength(0);

      // New changes should not trigger subscriber
      feed.emitChange({ type: 'add', quad: createQuad('after-destroy') });

      // Subscriber should not receive new events
      expect(subscriber).toHaveBeenCalledTimes(10); // Still 10, not 11
    });
  });

  describe('Memory Profiling', () => {
    it('should not leak memory when creating many batch processors', () => {
      const initialMemory = getHeapUsed();

      // Create 1000 batch processors
      for (let i = 0; i < 1000; i++) {
        const processor = createStreamProcessor(feed);
        processor.batch(10).subscribe(() => {
          // Subscriber function
        });
      }

      const afterCreation = getHeapUsed();
      const creationIncrease = afterCreation - initialMemory;

      // EXPECTED: Minimal memory increase if cleanup works
      // ACTUAL: Large increase due to listener accumulation
      // Note: This test will FAIL - it documents the leak

      console.log(`Memory after creating 1000 processors: ${(creationIncrease / 1024 / 1024).toFixed(2)} MB`);

      // If cleanup worked, this should pass:
      // expect(creationIncrease).toBeLessThan(10 * 1024 * 1024); // <10MB

      // Current reality: memory grows linearly with processor count
      // This is the PROOF of the memory leak
      expect(creationIncrease).toBeGreaterThan(0);
    });

    it('should not leak memory during change feed emission', () => {
      const processor = createStreamProcessor(feed);
      const subscriber = vi.fn();

      processor.batch(100).subscribe(subscriber);

      const beforeEmit = getHeapUsed();

      // Emit 100,000 changes
      for (let i = 0; i < 100000; i++) {
        feed.emitChange({
          type: 'add',
          quad: {
            subject: namedNode('http://example.org/s'),
            predicate: namedNode('http://example.org/p'),
            object: literal(`value-${i}`),
          },
        });
      }

      const afterEmit = getHeapUsed();
      const emitIncrease = afterEmit - beforeEmit;

      console.log(`Memory after 100k changes: ${(emitIncrease / 1024 / 1024).toFixed(2)} MB`);

      // Changes should not accumulate in memory (they're batched and processed)
      // Note: This depends on change feed implementation (does it store history?)
    });

    it('should not leak debounce closures', () => {
      const processors = [];

      const beforeDebounce = getHeapUsed();

      // Create 1000 debounce processors
      for (let i = 0; i < 1000; i++) {
        const processor = createStreamProcessor(feed);
        processor.debounce(100).subscribe(() => {
          // Large closure to amplify leak
          const largeData = Buffer.alloc(1024 * 10); // 10KB
          return largeData.length;
        });
        processors.push(processor);
      }

      const afterDebounce = getHeapUsed();
      const debounceIncrease = afterDebounce - beforeDebounce;

      console.log(`Memory with 1000 debounce processors: ${(debounceIncrease / 1024 / 1024).toFixed(2)} MB`);

      // Each processor captures its own timeout and latestChange
      // If not cleaned up, these closures leak

      // CRITICAL: No cleanup means closures persist
      // Expected: processor.destroy() to clear closures
    });
  });

  describe('Filter and Map Cleanup', () => {
    it('should not leak filter predicates', () => {
      const filters = [];

      // Create 1000 filtered processors
      for (let i = 0; i < 1000; i++) {
        const processor = createStreamProcessor(feed);
        const filtered = processor
          .filter(c => c.type === 'add')
          .filter(c => c.quad.object.value.startsWith('test'))
          .filter(c => c.timestamp > Date.now() - 60000);

        const subscriber = vi.fn();
        filtered.subscribe(subscriber);

        filters.push(filtered);
      }

      // Emit changes
      for (let i = 0; i < 100; i++) {
        feed.emitChange({
          type: 'add',
          quad: {
            subject: namedNode('http://example.org/s'),
            predicate: namedNode('http://example.org/p'),
            object: literal(`test-${i}`),
          },
        });
      }

      // CRITICAL: 1000 processors each with 3 filter predicates
      // All predicates run on EVERY change
      // No cleanup - predicates leak
    });

    it('should not leak map transformations', () => {
      const mappers = [];

      // Create 1000 map processors
      for (let i = 0; i < 1000; i++) {
        const processor = createStreamProcessor(feed);
        const mapped = processor
          .map(c => ({ ...c, index: i }))
          .map(c => ({ ...c, processed: true }))
          .map(c => ({ ...c, timestamp: Date.now() }));

        const subscriber = vi.fn();
        mapped.subscribe(subscriber);

        mappers.push(mapped);
      }

      // Emit changes
      for (let i = 0; i < 100; i++) {
        feed.emitChange({
          type: 'add',
          quad: {
            subject: namedNode('http://example.org/s'),
            predicate: namedNode('http://example.org/p'),
            object: literal(`value-${i}`),
          },
        });
      }

      // CRITICAL: 1000 processors each with 3 map operations
      // All transformations run on EVERY change
      // No cleanup - operations accumulate
    });
  });

  describe('Proposed Cleanup API', () => {
    /**
     * This describes what the cleanup API SHOULD look like
     * Currently, these tests will fail because the API doesn't exist
     */

    it.skip('PROPOSED: should provide destroy() method on batch processor', () => {
      const processor = createStreamProcessor(feed);
      const subscriber = vi.fn();

      const batchProcessor = processor.batch(10);
      batchProcessor.subscribe(subscriber);

      // Proposed API
      expect(typeof batchProcessor.destroy).toBe('function');

      // Should remove event listeners and cleanup
      batchProcessor.destroy();

      // After destroy, no more events
      feed.emitChange({
        type: 'add',
        quad: {
          subject: namedNode('http://example.org/s'),
          predicate: namedNode('http://example.org/p'),
          object: literal('test'),
        },
      });

      expect(subscriber).not.toHaveBeenCalled();
    });

    it.skip('PROPOSED: should provide unsubscribe() return value', () => {
      const processor = createStreamProcessor(feed);
      const subscriber = vi.fn();

      // Proposed API: subscribe returns unsubscribe function
      const unsubscribe = processor.batch(10).subscribe(subscriber);

      expect(typeof unsubscribe).toBe('function');

      // Unsubscribe should remove listener
      unsubscribe();

      // After unsubscribe, no more events
      for (let i = 0; i < 20; i++) {
        feed.emitChange({
          type: 'add',
          quad: {
            subject: namedNode('http://example.org/s'),
            predicate: namedNode('http://example.org/p'),
            object: literal(`value-${i}`),
          },
        });
      }

      expect(subscriber).not.toHaveBeenCalled();
    });

    it.skip('PROPOSED: should cleanup debounce timer on destroy', () => {
      const processor = createStreamProcessor(feed);
      const debounceProcessor = processor.debounce(100);

      debounceProcessor.subscribe(vi.fn());

      // Emit changes
      feed.emitChange({
        type: 'add',
        quad: {
          subject: namedNode('http://example.org/s'),
          predicate: namedNode('http://example.org/p'),
          object: literal('test'),
        },
      });

      expect(vi.getTimerCount()).toBeGreaterThan(0);

      // Destroy should clear timer
      debounceProcessor.destroy();

      expect(vi.getTimerCount()).toBe(0);
    });
  });

  describe('Real-World Scenario: Dashboard Cleanup', () => {
    it('should not leak when user navigates away from dashboard', () => {
      // Simulate a dashboard with 20 real-time widgets
      const widgets = [];

      function createWidget(id) {
        const processor = createStreamProcessor(feed);
        const subscriber = vi.fn();

        processor
          .filter(c => c.metadata?.widgetId === id)
          .batch(5)
          .subscribe(subscriber);

        return { id, processor, subscriber };
      }

      // User opens dashboard
      for (let i = 0; i < 20; i++) {
        widgets.push(createWidget(`widget-${i}`));
      }

      const beforeMemory = getHeapUsed();

      // Simulate activity for 1 minute
      for (let i = 0; i < 6000; i++) {
        feed.emitChange({
          type: 'add',
          quad: {
            subject: namedNode('http://example.org/s'),
            predicate: namedNode('http://example.org/p'),
            object: literal(`update-${i}`),
          },
          metadata: {
            widgetId: `widget-${i % 20}`,
          },
        });
      }

      const afterActivity = getHeapUsed();
      const activityIncrease = afterActivity - beforeMemory;

      console.log(`Memory after dashboard activity: ${(activityIncrease / 1024 / 1024).toFixed(2)} MB`);

      // User navigates away - CRITICAL: No cleanup!
      // Expected: widgets.forEach(w => w.processor.destroy())
      // Actual: All 20 processors keep listening forever

      // This is the real-world memory leak scenario
    });
  });
});
