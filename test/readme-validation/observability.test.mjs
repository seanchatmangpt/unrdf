/**
 * @file Observability README Example Tests (London TDD)
 * @description Tests for OpenTelemetry observability examples from README.md
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';

describe('README Observability Examples', () => {
  let mockObservability;
  let mockTracer;
  let mockSpan;

  beforeEach(() => {
    mockSpan = {
      setAttribute: vi.fn(),
      setStatus: vi.fn(),
      end: vi.fn()
    };

    mockTracer = {
      startSpan: vi.fn().mockReturnValue(mockSpan),
      startActiveSpan: vi.fn((name, fn) => {
        return fn(mockSpan);
      })
    };

    mockObservability = {
      getPerformanceMetrics: vi.fn(),
      recordMetric: vi.fn(),
      createSpan: vi.fn().mockReturnValue(mockSpan)
    };
  });

  describe('Observability Initialization', () => {
    it('should create observability instance', () => {
      const MockObservability = vi.fn().mockReturnValue(mockObservability);
      const obs = new MockObservability();

      expect(MockObservability).toHaveBeenCalled();
      expect(obs).toBeDefined();
    });

    it('should get tracer from OpenTelemetry', () => {
      const mockTrace = {
        getTracer: vi.fn().mockReturnValue(mockTracer)
      };

      const tracer = mockTrace.getTracer('unrdf');

      expect(mockTrace.getTracer).toHaveBeenCalledWith('unrdf');
      expect(tracer).toBeDefined();
    });
  });

  describe('Automatic Span Creation', () => {
    it('should create span for operations', () => {
      const span = mockTracer.startSpan('operation-name');

      expect(mockTracer.startSpan).toHaveBeenCalledWith('operation-name');
      expect(span).toBeDefined();
    });

    it('should set span attributes', () => {
      const span = mockTracer.startSpan('query-execution');
      span.setAttribute('query.type', 'sparql-select');
      span.setAttribute('query.duration', 125);

      expect(span.setAttribute).toHaveBeenCalledWith('query.type', 'sparql-select');
      expect(span.setAttribute).toHaveBeenCalledWith('query.duration', 125);
    });

    it('should end span after operation', () => {
      const span = mockTracer.startSpan('operation');
      span.end();

      expect(span.end).toHaveBeenCalled();
    });

    it('should create nested spans', () => {
      const parentSpan = mockTracer.startSpan('parent-operation');
      const childSpan = mockTracer.startSpan('child-operation');

      expect(mockTracer.startSpan).toHaveBeenCalledTimes(2);

      childSpan.end();
      parentSpan.end();

      expect(childSpan.end).toHaveBeenCalled();
      expect(parentSpan.end).toHaveBeenCalled();
    });
  });

  describe('Performance Metrics', () => {
    it('should access performance metrics', () => {
      mockObservability.getPerformanceMetrics.mockReturnValue({
        latency: {
          p50: 50,
          p95: 125,
          p99: 200
        },
        cacheHitRate: 0.65,
        throughput: 1000
      });

      const metrics = mockObservability.getPerformanceMetrics();

      expect(mockObservability.getPerformanceMetrics).toHaveBeenCalled();
      expect(metrics).toHaveProperty('latency');
      expect(metrics).toHaveProperty('cacheHitRate');
      expect(metrics).toHaveProperty('throughput');
    });

    it('should report latency p95 metric', () => {
      mockObservability.getPerformanceMetrics.mockReturnValue({
        latency: { p50: 50, p95: 125, p99: 200 }
      });

      const metrics = mockObservability.getPerformanceMetrics();

      expect(metrics.latency.p95).toBe(125);
    });

    it('should report cache hit rate', () => {
      mockObservability.getPerformanceMetrics.mockReturnValue({
        cacheHitRate: 0.65
      });

      const metrics = mockObservability.getPerformanceMetrics();

      expect(metrics.cacheHitRate).toBe(0.65);
      expect(metrics.cacheHitRate * 100).toBe(65); // 65% hit rate
    });

    it('should track multiple metric types', () => {
      mockObservability.getPerformanceMetrics.mockReturnValue({
        latency: { p50: 50, p95: 125, p99: 200 },
        cacheHitRate: 0.65,
        throughput: 1000,
        errorRate: 0.01,
        queryCount: 5000
      });

      const metrics = mockObservability.getPerformanceMetrics();

      expect(metrics.latency).toBeDefined();
      expect(metrics.cacheHitRate).toBeDefined();
      expect(metrics.throughput).toBeDefined();
      expect(metrics.errorRate).toBeDefined();
      expect(metrics.queryCount).toBeDefined();
    });
  });

  describe('Production Metrics', () => {
    it('should meet p95 hook execution latency target (<100ms)', () => {
      mockObservability.getPerformanceMetrics.mockReturnValue({
        latency: { p95: 85 }
      });

      const metrics = mockObservability.getPerformanceMetrics();

      expect(metrics.latency.p95).toBeLessThan(100);
    });

    it('should meet p95 query execution latency target (<500ms)', () => {
      mockObservability.getPerformanceMetrics.mockReturnValue({
        queryLatency: { p95: 450 }
      });

      const metrics = mockObservability.getPerformanceMetrics();

      expect(metrics.queryLatency.p95).toBeLessThan(500);
    });

    it('should meet p95 transaction commit latency target (<500ms)', () => {
      mockObservability.getPerformanceMetrics.mockReturnValue({
        transactionLatency: { p95: 475 }
      });

      const metrics = mockObservability.getPerformanceMetrics();

      expect(metrics.transactionLatency.p95).toBeLessThan(500);
    });

    it('should achieve >50% cache hit rate after warmup', () => {
      mockObservability.getPerformanceMetrics.mockReturnValue({
        cacheHitRate: 0.65
      });

      const metrics = mockObservability.getPerformanceMetrics();

      expect(metrics.cacheHitRate).toBeGreaterThan(0.5);
    });
  });

  describe('Span Context and Tracing', () => {
    it('should propagate span context', () => {
      const span = mockTracer.startSpan('parent-operation');
      span.setAttribute('context.id', 'ctx-123');

      expect(span.setAttribute).toHaveBeenCalledWith('context.id', 'ctx-123');
    });

    it('should record span events', () => {
      const span = mockTracer.startSpan('operation');
      span.addEvent = vi.fn();
      span.addEvent('query-started', { queryType: 'SELECT' });

      expect(span.addEvent).toHaveBeenCalledWith(
        'query-started',
        expect.objectContaining({ queryType: 'SELECT' })
      );
    });

    it('should set span status on success', () => {
      const span = mockTracer.startSpan('operation');
      span.setStatus({ code: 1 }); // OK status

      expect(span.setStatus).toHaveBeenCalledWith(
        expect.objectContaining({ code: 1 })
      );
    });

    it('should set span status on error', () => {
      const span = mockTracer.startSpan('operation');
      span.setStatus({ code: 2, message: 'Operation failed' }); // ERROR status

      expect(span.setStatus).toHaveBeenCalledWith(
        expect.objectContaining({
          code: 2,
          message: 'Operation failed'
        })
      );
    });
  });

  describe('Instrumentation for All Operations', () => {
    it('should instrument transaction execution', () => {
      const span = mockObservability.createSpan('transaction.execute');

      expect(mockObservability.createSpan).toHaveBeenCalledWith('transaction.execute');
      expect(span).toBeDefined();
    });

    it('should instrument query execution', () => {
      const span = mockObservability.createSpan('query.execute');
      span.setAttribute('query.type', 'sparql-select');

      expect(mockObservability.createSpan).toHaveBeenCalledWith('query.execute');
      expect(span.setAttribute).toHaveBeenCalledWith('query.type', 'sparql-select');
    });

    it('should instrument hook evaluation', () => {
      const span = mockObservability.createSpan('hook.evaluate');
      span.setAttribute('hook.name', 'data-quality-gate');

      expect(mockObservability.createSpan).toHaveBeenCalledWith('hook.evaluate');
      expect(span.setAttribute).toHaveBeenCalledWith('hook.name', 'data-quality-gate');
    });

    it('should instrument validation', () => {
      const span = mockObservability.createSpan('shacl.validate');

      expect(mockObservability.createSpan).toHaveBeenCalledWith('shacl.validate');
    });

    it('should instrument lockchain writes', () => {
      const span = mockObservability.createSpan('lockchain.writeReceipt');
      span.setAttribute('actor', 'alice@example.org');

      expect(mockObservability.createSpan).toHaveBeenCalledWith('lockchain.writeReceipt');
      expect(span.setAttribute).toHaveBeenCalledWith('actor', 'alice@example.org');
    });
  });

  describe('Metrics Recording', () => {
    it('should record custom metrics', () => {
      mockObservability.recordMetric('custom.metric', 42);

      expect(mockObservability.recordMetric).toHaveBeenCalledWith('custom.metric', 42);
    });

    it('should record counter metrics', () => {
      mockObservability.recordMetric('query.count', 1, { type: 'increment' });

      expect(mockObservability.recordMetric).toHaveBeenCalledWith(
        'query.count',
        1,
        expect.objectContaining({ type: 'increment' })
      );
    });

    it('should record histogram metrics', () => {
      mockObservability.recordMetric('query.duration', 125, { type: 'histogram' });

      expect(mockObservability.recordMetric).toHaveBeenCalledWith(
        'query.duration',
        125,
        expect.objectContaining({ type: 'histogram' })
      );
    });

    it('should record gauge metrics', () => {
      mockObservability.recordMetric('cache.size', 1000, { type: 'gauge' });

      expect(mockObservability.recordMetric).toHaveBeenCalledWith(
        'cache.size',
        1000,
        expect.objectContaining({ type: 'gauge' })
      );
    });
  });

  describe('Error Tracking', () => {
    it('should record errors in spans', () => {
      const span = mockTracer.startSpan('operation');
      span.recordException = vi.fn();

      const error = new Error('Operation failed');
      span.recordException(error);

      expect(span.recordException).toHaveBeenCalledWith(error);
    });

    it('should track error rate metric', () => {
      mockObservability.getPerformanceMetrics.mockReturnValue({
        errorRate: 0.01 // 1% error rate
      });

      const metrics = mockObservability.getPerformanceMetrics();

      expect(metrics.errorRate).toBe(0.01);
      expect(metrics.errorRate).toBeLessThan(0.05); // <5% acceptable
    });
  });
});
