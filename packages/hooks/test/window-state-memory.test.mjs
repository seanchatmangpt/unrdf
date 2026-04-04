/**
 * Window State Memory Protection Test Suite
 *
 * Tests SlidingWindow memory protection:
 * - LRU eviction correctness
 * - Memory limit enforcement
 * - High-frequency event handling (10k+ events)
 * - Memory recovery after eviction
 * - Telemetry/metrics tracking
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { SlidingWindow } from '../src/hooks/condition-evaluator.mjs';

describe('SlidingWindow Memory Protection', () => {
  let window;

  beforeEach(() => {
    window = new SlidingWindow(60000, 60000, true, 100);
  });

  describe('LRU eviction correctness', () => {
    it('should evict oldest events when maxHistorySize is reached', () => {
      for (let i = 0; i < 150; i++) {
        window.add(i);
      }
      // Should have at most maxHistorySize events
      expect(window.events.length).toBeLessThanOrEqual(100);
      // Oldest events (0-49) should be evicted; newest should remain
      const values = window.events.map(e => e.value);
      expect(values).toContain(149);
      expect(values).toContain(100);
      expect(values).not.toContain(0);
      expect(values).not.toContain(49);
    });

    it('should preserve event order after eviction', () => {
      for (let i = 0; i < 200; i++) {
        window.add(i);
      }
      const values = window.events.map(e => e.value);
      for (let i = 1; i < values.length; i++) {
        expect(values[i]).toBeGreaterThan(values[i - 1]);
      }
    });

    it('should not evict when under maxHistorySize', () => {
      for (let i = 0; i < 50; i++) {
        window.add(i);
      }
      expect(window.events.length).toBe(50);
      const metrics = window.getMetrics();
      expect(metrics.totalEvictions).toBe(0);
    });

    it('should evict exactly the right number of events', () => {
      for (let i = 0; i < 100; i++) {
        window.add(i);
      }
      expect(window.events.length).toBe(100);
      expect(window.getMetrics().totalEvictions).toBe(0);

      // Adding one more should evict one
      window.add(100);
      expect(window.events.length).toBe(100);
      expect(window.getMetrics().totalEvictions).toBe(1);
      expect(window.events[0].value).toBe(1);
    });

    it('should work with event-based (non-time) windows', () => {
      const eventWindow = new SlidingWindow(10, 10, false, 50);
      for (let i = 0; i < 75; i++) {
        eventWindow.add(i);
      }
      expect(eventWindow.events.length).toBeLessThanOrEqual(50);
      expect(eventWindow.getMetrics().totalEvictions).toBe(25);
    });
  });

  describe('Memory limit enforcement', () => {
    it('should estimate memory usage proportional to event count', () => {
      for (let i = 0; i < 50; i++) {
        window.add(i);
      }
      const estimate = window.estimateMemoryUsage();
      expect(estimate).toBe(50 * 200); // 200 bytes per event
    });

    it('should throw when memory exceeds 500MB hard limit', () => {
      // Create a window with a very large maxHistorySize
      const bigWindow = new SlidingWindow(60000, 60000, true, 5_000_000);

      // Override the static limit for testing to something achievable
      const origLimit = SlidingWindow.MEMORY_LIMIT_BYTES;
      SlidingWindow.MEMORY_LIMIT_BYTES = 1000; // 1KB limit for testing

      try {
        expect(() => {
          // Add enough events to trigger the memory check at 1000th event
          for (let i = 0; i < 1001; i++) {
            bigWindow.add(i);
          }
        }).toThrow(/memory limit exceeded/i);
      } finally {
        SlidingWindow.MEMORY_LIMIT_BYTES = origLimit;
      }
    });

    it('should warn when memory exceeds 100MB threshold', () => {
      const origWarn = SlidingWindow.MEMORY_WARN_BYTES;
      SlidingWindow.MEMORY_WARN_BYTES = 500; // 500 bytes for testing

      const warnWindow = new SlidingWindow(60000, 60000, true, 50000);

      // Suppress console.warn during test
      const warnings = [];
      const origConsoleWarn = console.warn;
      console.warn = (...args) => warnings.push(args.join(' '));

      try {
        // Add 1000 events to trigger the check
        for (let i = 0; i < 1000; i++) {
          warnWindow.add(i);
        }
        expect(warnWindow.getMetrics().memoryWarnings).toBeGreaterThan(0);
        expect(warnings.some(w => w.includes('[SlidingWindow] Memory warning'))).toBe(true);
      } finally {
        console.warn = origConsoleWarn;
        SlidingWindow.MEMORY_WARN_BYTES = origWarn;
      }
    });

    it('should report memory estimate via getMetrics', () => {
      for (let i = 0; i < 10; i++) {
        window.add(i);
      }
      const metrics = window.getMetrics();
      expect(metrics.estimatedMemoryBytes).toBe(10 * 200);
      expect(metrics.maxHistorySize).toBe(100);
    });
  });

  describe('High-frequency event handling (10k+ events)', () => {
    it('should handle 10,000 rapid events without exceeding maxHistorySize', () => {
      const hw = new SlidingWindow(60000, 60000, true, 1000);
      for (let i = 0; i < 10000; i++) {
        hw.add(i);
      }
      expect(hw.events.length).toBeLessThanOrEqual(1000);
      const metrics = hw.getMetrics();
      expect(metrics.totalEventsAdded).toBe(10000);
      expect(metrics.totalEvictions).toBe(9000);
    });

    it('should handle 50,000 events with small maxHistorySize', () => {
      const hw = new SlidingWindow(60000, 60000, true, 100);
      for (let i = 0; i < 50000; i++) {
        hw.add(i);
      }
      expect(hw.events.length).toBeLessThanOrEqual(100);
      expect(hw.getMetrics().totalEvictions).toBe(49900);
    });

    it('should maintain correct newest events after bulk insertion', () => {
      const hw = new SlidingWindow(60000, 60000, true, 500);
      for (let i = 0; i < 15000; i++) {
        hw.add(i);
      }
      const values = hw.events.map(e => e.value);
      // The newest 500 events should be 14500-14999
      expect(values[0]).toBe(14500);
      expect(values[values.length - 1]).toBe(14999);
    });

    it('should track peak event count accurately', () => {
      const hw = new SlidingWindow(60000, 60000, true, 200);
      for (let i = 0; i < 300; i++) {
        hw.add(i);
      }
      expect(hw.getMetrics().peakEventCount).toBe(200);
    });
  });

  describe('Memory recovery after eviction', () => {
    it('should free memory when events are evicted', () => {
      for (let i = 0; i < 100; i++) {
        window.add({ largePayload: 'x'.repeat(100) });
      }
      const memBefore = window.estimateMemoryUsage();

      // Clear the window
      window.clear();
      const memAfter = window.estimateMemoryUsage();

      expect(memAfter).toBe(0);
      expect(memAfter).toBeLessThan(memBefore);
    });

    it('should recover event capacity after clear()', () => {
      for (let i = 0; i < 100; i++) {
        window.add(i);
      }
      expect(window.events.length).toBe(100);

      window.clear();
      expect(window.events.length).toBe(0);

      // Can add events again up to max
      for (let i = 0; i < 50; i++) {
        window.add(i);
      }
      expect(window.events.length).toBe(50);
    });

    it('should reduce memory as time-based pruning removes old events', () => {
      // Use a very short time window
      const shortWindow = new SlidingWindow(10, 10, true, 10000);
      for (let i = 0; i < 50; i++) {
        shortWindow.add(i);
      }

      // Wait for window to expire, then prune
      const start = Date.now();
      while (Date.now() - start < 20) {
        // busy wait
      }
      shortWindow.prune();
      expect(shortWindow.events.length).toBe(0);
      expect(shortWindow.estimateMemoryUsage()).toBe(0);
    });
  });

  describe('Telemetry and metrics tracking', () => {
    it('should track totalEventsAdded correctly', () => {
      for (let i = 0; i < 25; i++) {
        window.add(i);
      }
      expect(window.getMetrics().totalEventsAdded).toBe(25);
    });

    it('should track totalEvictions correctly', () => {
      for (let i = 0; i < 150; i++) {
        window.add(i);
      }
      expect(window.getMetrics().totalEvictions).toBe(50);
    });

    it('should track currentEventCount correctly', () => {
      for (let i = 0; i < 80; i++) {
        window.add(i);
      }
      expect(window.getMetrics().currentEventCount).toBe(80);
    });

    it('should track peakEventCount correctly', () => {
      for (let i = 0; i < 200; i++) {
        window.add(i);
      }
      const metrics = window.getMetrics();
      // Peak should be maxHistorySize since we exceeded it
      expect(metrics.peakEventCount).toBe(100);
    });

    it('should expose maxHistorySize in metrics', () => {
      expect(window.getMetrics().maxHistorySize).toBe(100);
    });

    it('should provide complete metrics snapshot', () => {
      for (let i = 0; i < 10; i++) {
        window.add(i);
      }
      const metrics = window.getMetrics();
      expect(metrics).toHaveProperty('totalEvictions');
      expect(metrics).toHaveProperty('totalEventsAdded');
      expect(metrics).toHaveProperty('currentEventCount');
      expect(metrics).toHaveProperty('peakEventCount');
      expect(metrics).toHaveProperty('estimatedMemoryBytes');
      expect(metrics).toHaveProperty('maxHistorySize');
      expect(metrics).toHaveProperty('memoryWarnings');
    });

    it('should default maxHistorySize to 10000', () => {
      const defaultWindow = new SlidingWindow(60000);
      expect(defaultWindow.maxHistorySize).toBe(10000);
      expect(defaultWindow.getMetrics().maxHistorySize).toBe(10000);
    });

    it('should track memoryWarnings count', () => {
      const origWarn = SlidingWindow.MEMORY_WARN_BYTES;
      SlidingWindow.MEMORY_WARN_BYTES = 100; // Very low threshold

      const ww = new SlidingWindow(60000, 60000, true, 50000);
      const origConsoleWarn = console.warn;
      console.warn = () => {};

      try {
        for (let i = 0; i < 2000; i++) {
          ww.add(i);
        }
        expect(ww.getMetrics().memoryWarnings).toBeGreaterThan(0);
      } finally {
        console.warn = origConsoleWarn;
        SlidingWindow.MEMORY_WARN_BYTES = origWarn;
      }
    });
  });

  describe('Constructor parameter validation', () => {
    it('should accept custom maxHistorySize', () => {
      const w = new SlidingWindow(5000, 5000, true, 500);
      expect(w.maxHistorySize).toBe(500);
    });

    it('should default maxHistorySize when not provided', () => {
      const w = new SlidingWindow(5000);
      expect(w.maxHistorySize).toBe(10000);
    });

    it('should initialize metrics to zero', () => {
      const metrics = window.getMetrics();
      expect(metrics.totalEvictions).toBe(0);
      expect(metrics.totalEventsAdded).toBe(0);
      expect(metrics.peakEventCount).toBe(0);
      expect(metrics.memoryWarnings).toBe(0);
    });
  });
});
