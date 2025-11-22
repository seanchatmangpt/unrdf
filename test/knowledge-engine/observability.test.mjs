/**
 * @fileoverview Comprehensive Observability Test Suite
 * Tests OTEL spans, metrics, performance tracking, and error handling
 *
 * This suite validates that all knowledge-engine operations create proper
 * OTEL spans with correct attributes, status, and performance metrics.
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import {
  ObservabilityManager,
  createObservabilityManager,
} from '../../src/knowledge-engine/observability.mjs';
import { _Store, DataFactory } from 'n3';

const { _namedNode, _literal } = DataFactory;

describe('ObservabilityManager', () => {
  let observability;
  let mockTracer;
  let mockMeter;

  beforeEach(() => {
    // Create fresh observability manager for each test
    observability = createObservabilityManager({
      serviceName: 'test-service',
      serviceVersion: '1.0.0',
      enableTracing: false, // Disable actual OTEL for unit tests
      enableMetrics: false,
    });

    // Mock tracer and meter
    mockTracer = {
      startSpan: vi.fn((name, options) => ({
        name,
        attributes: options?.attributes || {},
        status: null,
        setAttributes: vi.fn(function (attrs) {
          Object.assign(this.attributes, attrs);
        }),
        recordException: vi.fn(),
        setStatus: vi.fn(function (status) {
          this.status = status;
        }),
        end: vi.fn(),
      })),
    };

    mockMeter = {
      createCounter: vi.fn(() => ({ add: vi.fn() })),
      createHistogram: vi.fn(() => ({ record: vi.fn() })),
      createUpDownCounter: vi.fn(() => ({ add: vi.fn() })),
    };
  });

  afterEach(async () => {
    await observability.shutdown();
  });

  describe('Initialization', () => {
    it('should initialize with default configuration', () => {
      expect(observability).toBeDefined();
      expect(observability.config).toBeDefined();
      expect(observability.config.serviceName).toBe('test-service');
      expect(observability.config.serviceVersion).toBe('1.0.0');
    });

    it('should initialize metrics structure', () => {
      expect(observability.metrics).toBeDefined();
      expect(observability.metrics.transactionLatency).toEqual([]);
      expect(observability.metrics.hookExecutionRate).toBe(0);
      expect(observability.metrics.errorCount).toBe(0);
      expect(observability.metrics.totalTransactions).toBe(0);
    });

    it('should handle initialization errors gracefully', async () => {
      const badConfig = createObservabilityManager({
        endpoint: 'invalid://endpoint',
        enableTracing: true,
      });

      await badConfig.initialize();
      expect(badConfig.initialized).toBe(true); // Should fallback
    });
  });

  describe('Transaction Span Lifecycle', () => {
    beforeEach(() => {
      observability.tracer = mockTracer;
    });

    it('should create transaction span with correct attributes', () => {
      const transactionId = 'tx-123';
      const attributes = {
        'transaction.type': 'update',
        'transaction.priority': 'high',
      };

      const spanContext = observability.startTransactionSpan(transactionId, attributes);

      expect(spanContext).toBeDefined();
      expect(spanContext.transactionId).toBe(transactionId);
      expect(spanContext.startTime).toBeDefined();
      expect(mockTracer.startSpan).toHaveBeenCalledWith('kgc.transaction', {
        attributes: expect.objectContaining({
          'kgc.transaction.id': transactionId,
          'kgc.service.name': 'test-service',
          ...attributes,
        }),
      });
    });

    it('should end transaction span successfully', () => {
      const transactionId = 'tx-456';

      // Start span
      const spanContext = observability.startTransactionSpan(transactionId);
      const span = spanContext.span;

      // End span
      observability.endTransactionSpan(transactionId, { 'result.size': 100 });

      expect(span.setAttributes).toHaveBeenCalledWith(
        expect.objectContaining({
          'kgc.transaction.success': true,
          'result.size': 100,
        })
      );
      expect(span.setStatus).toHaveBeenCalledWith({ code: 1 }); // OK status
      expect(span.end).toHaveBeenCalled();
      expect(observability.activeSpans.has(transactionId)).toBe(false);
    });

    it('should end transaction span with error', () => {
      const transactionId = 'tx-789';
      const error = new Error('Transaction failed');

      // Start span
      const spanContext = observability.startTransactionSpan(transactionId);
      const span = spanContext.span;

      // End span with error
      observability.endTransactionSpan(transactionId, {}, error);

      expect(span.recordException).toHaveBeenCalledWith(error);
      expect(span.setStatus).toHaveBeenCalledWith({
        code: 2,
        message: error.message,
      }); // ERROR status
      expect(span.end).toHaveBeenCalled();
    });

    it('should track transaction duration', () => {
      const transactionId = 'tx-duration';

      observability.startTransactionSpan(transactionId);

      // Simulate some work
      const delay = 50;
      const before = Date.now();
      while (Date.now() - before < delay) {}

      observability.endTransactionSpan(transactionId);

      const latency = observability.metrics.transactionLatency[0];
      expect(latency).toBeDefined();
      expect(latency.duration).toBeGreaterThanOrEqual(delay);
      expect(latency.success).toBe(true);
    });

    it('should handle missing transaction span gracefully', () => {
      expect(() => {
        observability.endTransactionSpan('nonexistent-tx');
      }).not.toThrow();
    });

    it('should work without tracer (fallback mode)', () => {
      observability.tracer = null;

      const spanContext = observability.startTransactionSpan('tx-no-tracer');

      expect(spanContext).toBeDefined();
      expect(spanContext.transactionId).toBe('tx-no-tracer');
      expect(spanContext.startTime).toBeDefined();
      expect(spanContext.span).toBeUndefined();
    });
  });

  describe('Hook Span Lifecycle', () => {
    beforeEach(() => {
      observability.tracer = mockTracer;
    });

    it('should create hook span with parent transaction', () => {
      const transactionId = 'tx-parent';
      const hookId = 'hook-123';

      // Start transaction first
      observability.startTransactionSpan(transactionId);
      const parentSpan = observability.activeSpans.get(transactionId).span;

      // Start hook span
      const hookSpan = observability.startHookSpan(hookId, transactionId, {
        'hook.type': 'before',
        'hook.name': 'validation-hook',
      });

      expect(hookSpan).toBeDefined();
      expect(hookSpan.hookId).toBe(hookId);
      expect(mockTracer.startSpan).toHaveBeenCalledWith('kgc.hook', {
        parent: parentSpan,
        attributes: expect.objectContaining({
          'kgc.hook.id': hookId,
          'kgc.transaction.id': transactionId,
          'hook.type': 'before',
          'hook.name': 'validation-hook',
        }),
      });
    });

    it('should end hook span successfully', () => {
      const transactionId = 'tx-hook';
      const hookId = 'hook-456';

      // Start transaction and hook
      observability.startTransactionSpan(transactionId);
      const hookSpanContext = observability.startHookSpan(hookId, transactionId);
      const span = hookSpanContext.span;

      const spanKey = `${transactionId}:${hookId}`;

      // End hook span
      observability.endHookSpan(hookId, transactionId, {
        'hook.result': 'success',
      });

      expect(span.setAttributes).toHaveBeenCalledWith(
        expect.objectContaining({
          'kgc.hook.success': true,
          'hook.result': 'success',
        })
      );
      expect(span.setStatus).toHaveBeenCalledWith({ code: 1 }); // OK
      expect(span.end).toHaveBeenCalled();
      expect(observability.activeSpans.has(spanKey)).toBe(false);
    });

    it('should end hook span with error', () => {
      const transactionId = 'tx-hook-error';
      const hookId = 'hook-error';
      const error = new Error('Hook execution failed');

      // Start transaction and hook
      observability.startTransactionSpan(transactionId);
      const hookSpanContext = observability.startHookSpan(hookId, transactionId);
      const span = hookSpanContext.span;

      const _spanKey = `${transactionId}:${hookId}`;

      // End hook span with error
      observability.endHookSpan(hookId, transactionId, {}, error);

      expect(span.recordException).toHaveBeenCalledWith(error);
      expect(span.setStatus).toHaveBeenCalledWith({
        code: 2,
        message: error.message,
      }); // ERROR
      expect(span.end).toHaveBeenCalled();
    });

    it('should track hook execution duration', () => {
      const transactionId = 'tx-hook-duration';
      const hookId = 'hook-duration';

      observability.startTransactionSpan(transactionId);
      observability.startHookSpan(hookId, transactionId);

      // Simulate work
      const delay = 30;
      const before = Date.now();
      while (Date.now() - before < delay) {}

      observability.endHookSpan(hookId, transactionId);

      expect(observability.metrics.hookExecutionRate).toBe(1);
    });

    it('should handle hook without parent transaction', () => {
      const hookId = 'orphan-hook';

      const hookSpan = observability.startHookSpan(hookId, 'nonexistent-tx');

      expect(hookSpan).toBeDefined();
      expect(mockTracer.startSpan).toHaveBeenCalledWith('kgc.hook', {
        parent: undefined,
        attributes: expect.objectContaining({
          'kgc.hook.id': hookId,
        }),
      });
    });
  });

  describe('Error Recording', () => {
    beforeEach(() => {
      observability.meter = mockMeter;
      observability.errorCounter = mockMeter.createCounter('test_errors');
    });

    it('should record error with attributes', () => {
      const error = new TypeError('Invalid input');
      const attributes = {
        'error.context': 'validation',
        'error.severity': 'high',
      };

      observability.recordError(error, attributes);

      expect(observability.metrics.errorCount).toBe(1);
      expect(observability.errorCounter.add).toHaveBeenCalledWith(1, {
        'error.type': 'TypeError',
        'error.message': 'Invalid input',
        ...attributes,
      });
    });

    it('should increment error count', () => {
      const error1 = new Error('Error 1');
      const error2 = new Error('Error 2');

      observability.recordError(error1);
      observability.recordError(error2);

      expect(observability.metrics.errorCount).toBe(2);
    });

    it('should handle error recording without counter', () => {
      observability.errorCounter = null;

      expect(() => {
        observability.recordError(new Error('Test error'));
      }).not.toThrow();

      expect(observability.metrics.errorCount).toBe(1);
    });

    it('should record different error types', () => {
      const errors = [
        new Error('Generic error'),
        new TypeError('Type error'),
        new ReferenceError('Reference error'),
        new SyntaxError('Syntax error'),
      ];

      errors.forEach(error => observability.recordError(error));

      expect(observability.metrics.errorCount).toBe(4);
    });
  });

  describe('Cache Statistics', () => {
    beforeEach(() => {
      observability.meter = mockMeter;
      observability.cacheHitCounter = mockMeter.createCounter('cache_hits');
      observability.cacheMissCounter = mockMeter.createCounter('cache_misses');
    });

    it('should track cache hits', () => {
      observability.updateCacheStats(true);
      observability.updateCacheStats(true);
      observability.updateCacheStats(true);

      expect(observability.metrics.cacheStats.hits).toBe(3);
      expect(observability.metrics.cacheStats.misses).toBe(0);
      expect(observability.cacheHitCounter.add).toHaveBeenCalledTimes(3);
    });

    it('should track cache misses', () => {
      observability.updateCacheStats(false);
      observability.updateCacheStats(false);

      expect(observability.metrics.cacheStats.hits).toBe(0);
      expect(observability.metrics.cacheStats.misses).toBe(2);
      expect(observability.cacheMissCounter.add).toHaveBeenCalledTimes(2);
    });

    it('should track mixed cache operations', () => {
      observability.updateCacheStats(true); // hit
      observability.updateCacheStats(false); // miss
      observability.updateCacheStats(true); // hit
      observability.updateCacheStats(true); // hit
      observability.updateCacheStats(false); // miss

      expect(observability.metrics.cacheStats.hits).toBe(3);
      expect(observability.metrics.cacheStats.misses).toBe(2);
    });

    it('should work without metrics enabled', () => {
      observability.cacheHitCounter = null;
      observability.cacheMissCounter = null;

      expect(() => {
        observability.updateCacheStats(true);
        observability.updateCacheStats(false);
      }).not.toThrow();

      expect(observability.metrics.cacheStats.hits).toBe(1);
      expect(observability.metrics.cacheStats.misses).toBe(1);
    });
  });

  describe('Queue Depth and Backpressure', () => {
    beforeEach(() => {
      observability.meter = mockMeter;
      observability.queueDepthGauge = mockMeter.createUpDownCounter('queue_depth');
    });

    it('should track queue depth changes', () => {
      observability.updateQueueDepth(50);
      expect(observability.metrics.backpressure.queueDepth).toBe(50);

      observability.updateQueueDepth(100);
      expect(observability.metrics.backpressure.queueDepth).toBe(100);
    });

    it('should warn on high watermark breach', () => {
      const consoleWarnSpy = vi.spyOn(console, 'warn').mockImplementation(() => {});

      const highWatermark = observability.metrics.backpressure.watermarks.high;
      observability.updateQueueDepth(highWatermark + 1);

      expect(consoleWarnSpy).toHaveBeenCalledWith(expect.stringContaining('High queue depth'));

      consoleWarnSpy.mockRestore();
    });

    it('should not warn below high watermark', () => {
      const consoleWarnSpy = vi.spyOn(console, 'warn').mockImplementation(() => {});

      const highWatermark = observability.metrics.backpressure.watermarks.high;
      observability.updateQueueDepth(highWatermark - 1);

      expect(consoleWarnSpy).not.toHaveBeenCalled();

      consoleWarnSpy.mockRestore();
    });

    it('should handle queue depth decrease', () => {
      observability.updateQueueDepth(100);
      observability.updateQueueDepth(50);
      observability.updateQueueDepth(25);

      expect(observability.metrics.backpressure.queueDepth).toBe(25);
    });
  });

  describe('Memory Usage Tracking', () => {
    it('should track memory usage snapshots', () => {
      observability.updateMemoryUsage();

      expect(observability.metrics.memoryUsage).toHaveLength(1);

      const snapshot = observability.metrics.memoryUsage[0];
      expect(snapshot.timestamp).toBeDefined();
      expect(snapshot.rss).toBeGreaterThan(0);
      expect(snapshot.heapUsed).toBeGreaterThan(0);
      expect(snapshot.heapTotal).toBeGreaterThan(0);
      expect(snapshot.external).toBeGreaterThanOrEqual(0);
    });

    it('should limit memory snapshots to 100 entries', () => {
      // Add 150 snapshots
      for (let i = 0; i < 150; i++) {
        observability.updateMemoryUsage();
      }

      expect(observability.metrics.memoryUsage).toHaveLength(100);
    });

    it('should track memory over time', () => {
      observability.updateMemoryUsage();
      const firstSnapshot = observability.metrics.memoryUsage[0];

      // Allocate some memory
      const _array = new Array(1000000).fill('test');

      observability.updateMemoryUsage();
      const secondSnapshot = observability.metrics.memoryUsage[1];

      expect(secondSnapshot.timestamp).toBeGreaterThan(firstSnapshot.timestamp);
      expect(secondSnapshot.heapUsed).toBeGreaterThan(0);
    });
  });

  describe('Performance Metrics', () => {
    beforeEach(() => {
      // Add some test data
      const now = Date.now();
      observability.metrics.transactionLatency = [
        { timestamp: now, duration: 100, success: true },
        { timestamp: now, duration: 200, success: true },
        { timestamp: now, duration: 150, success: true },
        { timestamp: now, duration: 300, success: false },
        { timestamp: now, duration: 250, success: true },
      ];
      observability.metrics.hookExecutionRate = 10;
      observability.metrics.totalTransactions = 5;
      observability.metrics.errorCount = 1;
      observability.metrics.cacheStats = { hits: 80, misses: 20, size: 100 };
    });

    it('should calculate latency percentiles', () => {
      const metrics = observability.getPerformanceMetrics();

      expect(metrics.transactionLatency.p50).toBeGreaterThan(0);
      expect(metrics.transactionLatency.p95).toBeGreaterThan(0);
      expect(metrics.transactionLatency.p99).toBeGreaterThan(0);
      expect(metrics.transactionLatency.max).toBe(300);
    });

    it('should calculate p95 latency correctly', () => {
      const metrics = observability.getPerformanceMetrics();

      // p95 should be the 95th percentile value (300 in sorted [100, 150, 200, 250, 300])
      expect(metrics.transactionLatency.p95).toBe(300);
    });

    it('should report hook execution rate', () => {
      const metrics = observability.getPerformanceMetrics();

      expect(metrics.hookExecutionRate).toBe(10);
    });

    it('should calculate error rate', () => {
      const metrics = observability.getPerformanceMetrics();

      expect(metrics.errorRate).toBe(0.2); // 1 error / 5 transactions
    });

    it('should report cache hit rate', () => {
      const metrics = observability.getPerformanceMetrics();

      expect(metrics.cacheStats.hitRate).toBe(0.8); // 80 hits / 100 total
    });

    it('should handle empty metrics', () => {
      observability.metrics.transactionLatency = [];
      observability.metrics.totalTransactions = 0;
      observability.metrics.cacheStats = { hits: 0, misses: 0, size: 0 };

      const metrics = observability.getPerformanceMetrics();

      expect(metrics.transactionLatency.p50).toBe(0);
      expect(metrics.errorRate).toBe(0);
      expect(metrics.cacheStats.hitRate).toBe(0);
    });

    it('should include backpressure metrics', () => {
      observability.metrics.backpressure.queueDepth = 75;

      const metrics = observability.getPerformanceMetrics();

      expect(metrics.backpressure.queueDepth).toBe(75);
      expect(metrics.backpressure.watermarks.high).toBe(1000);
      expect(metrics.backpressure.watermarks.low).toBe(100);
    });

    it('should include current memory usage', () => {
      const metrics = observability.getPerformanceMetrics();

      expect(metrics.memoryUsage).toBeDefined();
      expect(metrics.memoryUsage.rss).toBeGreaterThan(0);
      expect(metrics.memoryUsage.heapUsed).toBeGreaterThan(0);
    });
  });

  describe('Shutdown and Cleanup', () => {
    beforeEach(() => {
      observability.tracer = mockTracer;
    });

    it('should end all active spans on shutdown', async () => {
      // Start multiple spans
      observability.startTransactionSpan('tx-1');
      observability.startTransactionSpan('tx-2');
      observability.startTransactionSpan('tx-3');

      const spans = [
        observability.activeSpans.get('tx-1').span,
        observability.activeSpans.get('tx-2').span,
        observability.activeSpans.get('tx-3').span,
      ];

      await observability.shutdown();

      spans.forEach(span => {
        expect(span.end).toHaveBeenCalled();
      });

      expect(observability.activeSpans.size).toBe(0);
    });

    it('should clear active spans map', async () => {
      observability.startTransactionSpan('tx-cleanup');
      observability.startTransactionSpan('tx-cleanup-2');

      expect(observability.activeSpans.size).toBe(2);

      await observability.shutdown();

      expect(observability.activeSpans.size).toBe(0);
    });

    it('should handle shutdown with no active spans', async () => {
      expect(observability.activeSpans.size).toBe(0);

      await expect(observability.shutdown()).resolves.not.toThrow();
    });
  });

  describe('Integration with Knowledge Engine Operations', () => {
    beforeEach(() => {
      observability.tracer = mockTracer;
      observability.meter = mockMeter;
      observability.transactionCounter = mockMeter.createCounter('transactions');
      observability.transactionDuration = mockMeter.createHistogram('transaction_duration');
      observability.hookExecutionCounter = mockMeter.createCounter('hooks');
      observability.hookDuration = mockMeter.createHistogram('hook_duration');
    });

    it('should track parse operation with spans', () => {
      const transactionId = 'parse-tx';

      observability.startTransactionSpan(transactionId, {
        'operation.type': 'parse',
        'input.format': 'turtle',
      });

      // Simulate parse work
      const parseStart = Date.now();
      while (Date.now() - parseStart < 10) {}

      observability.endTransactionSpan(transactionId, {
        'output.size': 100,
        'output.format': 'store',
      });

      expect(observability.metrics.totalTransactions).toBe(1);
      expect(observability.metrics.transactionLatency.length).toBe(1);
    });

    it('should track query operation with spans', () => {
      const transactionId = 'query-tx';

      observability.startTransactionSpan(transactionId, {
        'operation.type': 'query',
        'query.type': 'select',
      });

      observability.endTransactionSpan(transactionId, {
        'results.count': 50,
      });

      const span = mockTracer.startSpan.mock.results[0].value;
      expect(span.attributes['operation.type']).toBe('query');
    });

    it('should track validation operation with error', () => {
      const transactionId = 'validate-tx';
      const error = new Error('SHACL validation failed');

      observability.startTransactionSpan(transactionId, {
        'operation.type': 'validate',
      });

      observability.recordError(error, {
        'validation.type': 'shacl',
        'transaction.id': transactionId,
      });

      observability.endTransactionSpan(
        transactionId,
        {
          'validation.conforms': false,
        },
        error
      );

      expect(observability.metrics.errorCount).toBe(1);
      expect(observability.metrics.totalTransactions).toBe(1);
    });

    it('should track hook lifecycle within transaction', () => {
      const transactionId = 'hook-lifecycle-tx';
      const hookId = 'before-hook';

      // Start transaction
      observability.startTransactionSpan(transactionId);

      // Execute hook
      observability.startHookSpan(hookId, transactionId, {
        'hook.phase': 'before',
        'hook.type': 'validation',
      });

      // Simulate hook work
      const hookStart = Date.now();
      while (Date.now() - hookStart < 5) {}

      observability.endHookSpan(hookId, transactionId, {
        'hook.success': true,
      });

      // End transaction
      observability.endTransactionSpan(transactionId);

      expect(observability.metrics.hookExecutionRate).toBe(1);
      expect(observability.metrics.totalTransactions).toBe(1);
    });

    it('should handle complex operation with multiple hooks', () => {
      const transactionId = 'complex-tx';
      const hooks = ['before-hook-1', 'before-hook-2', 'after-hook-1'];

      observability.startTransactionSpan(transactionId);

      // Execute multiple hooks
      hooks.forEach((hookId, _index) => {
        observability.startHookSpan(hookId, transactionId);
        observability.endHookSpan(hookId, transactionId);
      });

      observability.endTransactionSpan(transactionId);

      expect(observability.metrics.hookExecutionRate).toBe(3);
      expect(observability.metrics.totalTransactions).toBe(1);
    });
  });

  describe('Performance Validation', () => {
    beforeEach(() => {
      observability.tracer = mockTracer;
      observability.meter = mockMeter;
      observability.transactionCounter = mockMeter.createCounter('transactions');
      observability.transactionDuration = mockMeter.createHistogram('transaction_duration');
    });

    it('should meet latency requirements (<100ms for hook execution)', () => {
      const transactionId = 'perf-tx';
      const hookId = 'perf-hook';

      observability.startTransactionSpan(transactionId);
      observability.startHookSpan(hookId, transactionId);

      // Simulate fast hook execution
      const hookStart = Date.now();
      while (Date.now() - hookStart < 50) {} // 50ms execution

      observability.endHookSpan(hookId, transactionId);
      observability.endTransactionSpan(transactionId);

      const metrics = observability.getPerformanceMetrics();
      expect(metrics.transactionLatency.p95).toBeLessThan(100);
    });

    it('should track throughput', () => {
      // Execute multiple transactions
      for (let i = 0; i < 10; i++) {
        const txId = `tx-${i}`;
        observability.startTransactionSpan(txId);
        observability.endTransactionSpan(txId);
      }

      expect(observability.metrics.totalTransactions).toBe(10);
    });

    it('should maintain low error rate', () => {
      // Execute transactions with few errors
      for (let i = 0; i < 100; i++) {
        const txId = `tx-${i}`;
        observability.startTransactionSpan(txId);

        if (i < 2) {
          // Only 2 errors
          observability.recordError(new Error('Test error'));
          observability.endTransactionSpan(txId, {}, new Error('Test error'));
        } else {
          observability.endTransactionSpan(txId);
        }
      }

      const metrics = observability.getPerformanceMetrics();
      expect(metrics.errorRate).toBeLessThan(0.05); // Less than 5%
    });
  });

  describe('Edge Cases and Error Handling', () => {
    it('should handle null transaction ID', () => {
      expect(() => {
        observability.startTransactionSpan(null);
      }).not.toThrow();
    });

    it('should handle undefined attributes', () => {
      expect(() => {
        observability.startTransactionSpan('tx-undefined', undefined);
      }).not.toThrow();
    });

    it('should handle very large transaction counts', () => {
      // Add 2000 transactions (beyond the 1000 limit)
      for (let i = 0; i < 2000; i++) {
        observability.metrics.transactionLatency.push({
          timestamp: Date.now(),
          duration: 100,
          success: true,
        });
      }

      // Should trim to 1000
      observability._updateTransactionMetrics(100, true);

      expect(observability.metrics.transactionLatency.length).toBeLessThanOrEqual(1000);
    });

    it('should calculate percentile for single value', () => {
      const values = [100];
      const p95 = observability._calculatePercentile(values, 0.95);

      expect(p95).toBe(100);
    });

    it('should calculate percentile for empty array', () => {
      const values = [];
      const p95 = observability._calculatePercentile(values, 0.95);

      expect(p95).toBe(0);
    });

    it('should handle concurrent span operations', () => {
      // Need to have tracer for this test
      observability.tracer = mockTracer;

      const transactions = ['tx-1', 'tx-2', 'tx-3', 'tx-4', 'tx-5'];

      // Start all transactions
      transactions.forEach(txId => {
        observability.startTransactionSpan(txId);
      });

      expect(observability.activeSpans.size).toBe(5);

      // End all transactions
      transactions.forEach(txId => {
        observability.endTransactionSpan(txId);
      });

      expect(observability.activeSpans.size).toBe(0);
    });
  });

  describe('createObservabilityManager Factory', () => {
    it('should create new instance with custom config', () => {
      const manager = createObservabilityManager({
        serviceName: 'custom-service',
        serviceVersion: '2.0.0',
        enableTracing: true,
        enableMetrics: true,
      });

      expect(manager).toBeInstanceOf(ObservabilityManager);
      expect(manager.config.serviceName).toBe('custom-service');
      expect(manager.config.serviceVersion).toBe('2.0.0');
    });

    it('should create instance with default config', () => {
      const manager = createObservabilityManager();

      expect(manager).toBeInstanceOf(ObservabilityManager);
      expect(manager.config).toBeDefined();
    });
  });

  describe('OTEL Span Attribute Validation', () => {
    beforeEach(() => {
      observability.tracer = mockTracer;
    });

    it('should include required span attributes for transaction', () => {
      const transactionId = 'attr-tx';

      observability.startTransactionSpan(transactionId, {
        'operation.type': 'parse',
        'input.size': 1024,
        'input.format': 'turtle',
      });

      const span = mockTracer.startSpan.mock.results[0].value;

      expect(span.attributes).toMatchObject({
        'kgc.transaction.id': transactionId,
        'kgc.service.name': 'test-service',
        'operation.type': 'parse',
        'input.size': 1024,
        'input.format': 'turtle',
      });
    });

    it('should include duration in span attributes on end', () => {
      const transactionId = 'duration-tx';

      const spanContext = observability.startTransactionSpan(transactionId);
      const span = spanContext.span;

      observability.endTransactionSpan(transactionId);

      expect(span.setAttributes).toHaveBeenCalledWith(
        expect.objectContaining({
          'kgc.transaction.duration_ms': expect.any(Number),
          'kgc.transaction.success': true,
        })
      );
    });

    it('should set proper span status codes', () => {
      const txSuccess = 'tx-success';
      const txFail = 'tx-fail';

      // Success case
      const successContext = observability.startTransactionSpan(txSuccess);
      const successSpan = successContext.span;
      observability.endTransactionSpan(txSuccess);
      expect(successSpan.setStatus).toHaveBeenCalledWith({ code: 1 }); // OK

      // Failure case
      const failContext = observability.startTransactionSpan(txFail);
      const failSpan = failContext.span;
      observability.endTransactionSpan(txFail, {}, new Error('Failed'));
      expect(failSpan.setStatus).toHaveBeenCalledWith({
        code: 2,
        message: 'Failed',
      }); // ERROR
    });
  });
});

describe('Performance Metrics Schema Validation', () => {
  let observability;

  beforeEach(() => {
    observability = createObservabilityManager({
      enableTracing: false,
      enableMetrics: false,
    });
  });

  it('should return valid performance metrics structure', () => {
    const metrics = observability.getPerformanceMetrics();

    expect(metrics).toHaveProperty('transactionLatency');
    expect(metrics.transactionLatency).toHaveProperty('p50');
    expect(metrics.transactionLatency).toHaveProperty('p95');
    expect(metrics.transactionLatency).toHaveProperty('p99');
    expect(metrics.transactionLatency).toHaveProperty('max');

    expect(metrics).toHaveProperty('hookExecutionRate');
    expect(metrics).toHaveProperty('errorRate');
    expect(metrics).toHaveProperty('memoryUsage');
    expect(metrics).toHaveProperty('cacheStats');
    expect(metrics).toHaveProperty('backpressure');
  });

  it('should have valid cache stats structure', () => {
    const metrics = observability.getPerformanceMetrics();

    expect(metrics.cacheStats).toHaveProperty('hitRate');
    expect(metrics.cacheStats).toHaveProperty('size');
    expect(metrics.cacheStats).toHaveProperty('maxSize');

    expect(typeof metrics.cacheStats.hitRate).toBe('number');
    expect(typeof metrics.cacheStats.size).toBe('number');
    expect(typeof metrics.cacheStats.maxSize).toBe('number');
  });

  it('should have valid backpressure structure', () => {
    const metrics = observability.getPerformanceMetrics();

    expect(metrics.backpressure).toHaveProperty('queueDepth');
    expect(metrics.backpressure).toHaveProperty('watermarks');
    expect(metrics.backpressure.watermarks).toHaveProperty('high');
    expect(metrics.backpressure.watermarks).toHaveProperty('low');
  });
});
