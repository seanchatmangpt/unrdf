/**
 * @file Debug utilities test suite (80/20 fast suite)
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import {
  DebugLogger,
  createDebugger,
  PerformanceTracker,
  perfTracker,
  trace,
  formatBytes,
} from '../src/debug.mjs';

describe('Debug Utilities', () => {
  let originalDebug;

  beforeEach(() => {
    originalDebug = process.env.DEBUG;
  });

  afterEach(() => {
    process.env.DEBUG = originalDebug;
  });

  describe('DebugLogger Basics', () => {
    it('should create logger with namespace', () => {
      const logger = new DebugLogger('store');
      expect(logger.namespace).toBe('unrdf:store');
      expect(logger.timers).toBeInstanceOf(Map);
    });

    it('should log when debug enabled', () => {
      process.env.DEBUG = 'unrdf:store';
      const logger = new DebugLogger('store');
      const spy = vi.spyOn(console, 'debug').mockImplementation(() => {});

      logger.log('Test message');
      expect(spy).toHaveBeenCalled();

      spy.mockRestore();
    });
  });

  describe('PerformanceTracker', () => {
    it('should track operation metrics', () => {
      const tracker = new PerformanceTracker();
      const id = tracker.start('testOp');
      const metrics = tracker.end(id);

      expect(metrics).toBeDefined();
      expect(metrics.operation).toBe('testOp');
      expect(parseFloat(metrics.durationMs)).toBeGreaterThanOrEqual(0);
    });
  });

  describe('Utility Functions', () => {
    it('should wrap function with trace', () => {
      const add = (a, b) => a + b;
      const tracedAdd = trace(add, 'add');
      expect(tracedAdd(2, 3)).toBe(5);
    });

    it('should format bytes correctly', () => {
      expect(formatBytes(0)).toBe('0 Bytes');
      expect(formatBytes(1024)).toBe('1.00 KB');
      expect(formatBytes(1024 * 1024)).toBe('1.00 MB');
    });
  });
});
