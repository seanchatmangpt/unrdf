/**
 * @file Debug utilities test suite
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import {
  DebugLogger,
  createDebugger,
  PerformanceTracker,
  perfTracker,
  trace,
  formatBytes,
  getSystemInfo,
} from '../src/debug.mjs';

describe('Debug Utilities', () => {
  let originalDebug;

  beforeEach(() => {
    originalDebug = process.env.DEBUG;
  });

  afterEach(() => {
    process.env.DEBUG = originalDebug;
  });

  describe('DebugLogger', () => {
    it('should create logger with namespace', () => {
      const logger = new DebugLogger('store');

      expect(logger.namespace).toBe('unrdf:store');
      expect(logger.timers).toBeInstanceOf(Map);
    });

    it('should detect enabled debug mode', () => {
      process.env.DEBUG = 'unrdf:*';
      const logger = new DebugLogger('store');

      expect(logger.enabled).toBe(true);
    });

    it('should detect disabled debug mode', () => {
      process.env.DEBUG = '';
      const logger = new DebugLogger('store');

      expect(logger.enabled).toBe(false);
    });

    it('should detect specific namespace', () => {
      process.env.DEBUG = 'unrdf:store,unrdf:query';
      const storeLogger = new DebugLogger('store');
      const hookLogger = new DebugLogger('hooks');

      expect(storeLogger.enabled).toBe(true);
      expect(hookLogger.enabled).toBe(false);
    });

    it('should log messages when enabled', () => {
      process.env.DEBUG = 'unrdf:store';
      const logger = new DebugLogger('store');

      const spy = vi.spyOn(console, 'debug').mockImplementation(() => {});

      logger.log('Test message', { foo: 'bar' });

      expect(spy).toHaveBeenCalled();
      const call = spy.mock.calls[0][0];
      expect(call).toContain('[unrdf:store]');
      expect(call).toContain('Test message');

      spy.mockRestore();
    });

    it('should not log when disabled', () => {
      process.env.DEBUG = '';
      const logger = new DebugLogger('store');

      const spy = vi.spyOn(console, 'debug').mockImplementation(() => {});

      logger.log('Test message');

      expect(spy).not.toHaveBeenCalled();

      spy.mockRestore();
    });

    it('should log errors', () => {
      process.env.DEBUG = 'unrdf:store';
      const logger = new DebugLogger('store');

      const spy = vi.spyOn(console, 'error').mockImplementation(() => {});
      const error = new Error('Test error');

      logger.error('Operation failed', error);

      expect(spy).toHaveBeenCalled();
      const call = spy.mock.calls[0][0];
      expect(call).toContain('ERROR: Operation failed');

      spy.mockRestore();
    });

    it('should log warnings', () => {
      process.env.DEBUG = 'unrdf:store';
      const logger = new DebugLogger('store');

      const spy = vi.spyOn(console, 'warn').mockImplementation(() => {});

      logger.warn('Warning message', { foo: 'bar' });

      expect(spy).toHaveBeenCalled();
      const call = spy.mock.calls[0][0];
      expect(call).toContain('WARN: Warning message');

      spy.mockRestore();
    });

    it('should track timing', () => {
      process.env.DEBUG = 'unrdf:store';
      const logger = new DebugLogger('store');

      logger.time('operation');
      expect(logger.timers.has('operation')).toBe(true);

      const duration = logger.timeEnd('operation');
      expect(duration).toBeGreaterThanOrEqual(0);
      expect(logger.timers.has('operation')).toBe(false);
    });

    it('should handle unknown timer', () => {
      process.env.DEBUG = 'unrdf:store';
      const logger = new DebugLogger('store');

      const spy = vi.spyOn(console, 'warn').mockImplementation(() => {});

      const duration = logger.timeEnd('unknown');

      expect(duration).toBeNull();
      expect(spy).toHaveBeenCalled();

      spy.mockRestore();
    });

    it('should log memory usage', () => {
      process.env.DEBUG = 'unrdf:store';
      const logger = new DebugLogger('store');

      const spy = vi.spyOn(console, 'debug').mockImplementation(() => {});

      logger.memory('Test');

      expect(spy).toHaveBeenCalled();
      const call = spy.mock.calls[0][0];
      expect(call).toContain('Test');
      expect(call).toContain('MB');

      spy.mockRestore();
    });

    it('should create child logger', () => {
      const parent = new DebugLogger('store');
      const child = parent.child('query');

      expect(child.namespace).toBe('unrdf:store:query');
    });
  });

  describe('createDebugger', () => {
    it('should create DebugLogger instance', () => {
      const logger = createDebugger('store');

      expect(logger).toBeInstanceOf(DebugLogger);
      expect(logger.namespace).toBe('unrdf:store');
    });
  });

  describe('PerformanceTracker', () => {
    let tracker;

    beforeEach(() => {
      tracker = new PerformanceTracker();
    });

    it('should start tracking operation', () => {
      const id = tracker.start('testOp');

      expect(id).toBeTruthy();
      expect(id).toContain('testOp_');
      expect(tracker.metrics.has(id)).toBe(true);
    });

    it('should end tracking and return metrics', () => {
      const id = tracker.start('testOp');

      // Simulate some work
      const _sum = Array.from({ length: 1000 }, (_, i) => i).reduce((a, b) => a + b, 0);

      const metrics = tracker.end(id);

      expect(metrics).toBeDefined();
      expect(metrics.operation).toBe('testOp');
      expect(parseFloat(metrics.durationMs)).toBeGreaterThan(0);
      expect(metrics.timestamp).toBeDefined();
      expect(tracker.metrics.has(id)).toBe(false);
    });

    it('should handle unknown metric ID', () => {
      const metrics = tracker.end('unknown');

      expect(metrics).toBeNull();
    });

    it('should get active metrics', () => {
      const _id1 = tracker.start('op1');
      const _id2 = tracker.start('op2');

      const active = tracker.getActive();

      expect(active).toHaveLength(2);
      expect(active[0].operation).toBe('op1');
      expect(active[1].operation).toBe('op2');
      expect(parseFloat(active[0].elapsedMs)).toBeGreaterThanOrEqual(0);
    });

    it('should clear all metrics', () => {
      tracker.start('op1');
      tracker.start('op2');

      expect(tracker.metrics.size).toBe(2);

      tracker.clear();

      expect(tracker.metrics.size).toBe(0);
    });
  });

  describe('Global perfTracker', () => {
    it('should be a PerformanceTracker instance', () => {
      expect(perfTracker).toBeInstanceOf(PerformanceTracker);
    });

    it('should track operations', () => {
      const id = perfTracker.start('globalOp');
      const metrics = perfTracker.end(id);

      expect(metrics).toBeDefined();
      expect(metrics.operation).toBe('globalOp');
    });
  });

  describe('trace', () => {
    it('should wrap synchronous function', () => {
      const add = (a, b) => a + b;
      const tracedAdd = trace(add, 'add');

      const result = tracedAdd(2, 3);

      expect(result).toBe(5);
    });

    it('should wrap async function', async () => {
      const asyncAdd = async (a, b) => {
        await new Promise((resolve) => setTimeout(resolve, 10));
        return a + b;
      };

      const tracedAdd = trace(asyncAdd, 'asyncAdd');

      const result = await tracedAdd(2, 3);

      expect(result).toBe(5);
    });

    it('should handle errors in sync function', () => {
      const throwError = () => {
        throw new Error('Test error');
      };

      const traced = trace(throwError, 'throwError');

      expect(() => traced()).toThrow('Test error');
    });

    it('should handle errors in async function', async () => {
      const asyncThrow = async () => {
        throw new Error('Async error');
      };

      const traced = trace(asyncThrow, 'asyncThrow');

      await expect(traced()).rejects.toThrow('Async error');
    });

    it('should use function name if not provided', () => {
      function namedFunction() {
        return 42;
      }

      const traced = trace(namedFunction);

      expect(traced()).toBe(42);
    });
  });

  describe('formatBytes', () => {
    it('should format bytes', () => {
      expect(formatBytes(0)).toBe('0 Bytes');
      expect(formatBytes(1024)).toBe('1.00 KB');
      expect(formatBytes(1024 * 1024)).toBe('1.00 MB');
      expect(formatBytes(1024 * 1024 * 1024)).toBe('1.00 GB');
    });

    it('should handle decimal values', () => {
      expect(formatBytes(1536)).toBe('1.50 KB');
      expect(formatBytes(1024 * 1024 * 2.5)).toBe('2.50 MB');
    });
  });

  describe('getSystemInfo', () => {
    it('should return system information', () => {
      const info = getSystemInfo();

      expect(info.node).toBeTruthy();
      expect(info.platform).toBeTruthy();
      expect(info.arch).toBeTruthy();
      expect(info.memory).toBeDefined();
      expect(info.memory.rss).toContain('MB');
      expect(info.memory.heapTotal).toContain('MB');
      expect(info.memory.heapUsed).toContain('MB');
      expect(info.uptime).toContain('minutes');
      expect(info.env).toBeDefined();
    });

    it('should include environment variables', () => {
      process.env.DEBUG = 'unrdf:*';
      process.env.NODE_ENV = 'test';

      const info = getSystemInfo();

      expect(info.env.DEBUG).toBe('unrdf:*');
      expect(info.env.NODE_ENV).toBe('test');
    });
  });

  describe('Integration', () => {
    it('should work together for debugging session', () => {
      process.env.DEBUG = 'unrdf:test';

      const logger = createDebugger('test');
      const id = perfTracker.start('integration-test');

      logger.log('Starting test');
      logger.time('operation');

      // Simulate work
      const result = Array.from({ length: 100 }, (_, i) => i).reduce((a, b) => a + b, 0);

      logger.timeEnd('operation');
      logger.memory('After operation');

      const metrics = perfTracker.end(id);

      expect(result).toBe(4950);
      expect(metrics).toBeDefined();
      expect(metrics.operation).toBe('integration-test');
    });

    it('should handle nested tracing', () => {
      const inner = (x) => x * 2;
      const outer = (x) => inner(x) + 1;

      const _tracedInner = trace(inner, 'inner');
      const tracedOuter = trace(outer, 'outer');

      const result = tracedOuter(5);

      expect(result).toBe(11);
    });
  });
});
