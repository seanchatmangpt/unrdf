/**
 * @file Advanced Metrics Tests
 * @module observability/test/advanced-metrics
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { createAdvancedMetrics } from '../src/advanced-metrics.mjs';

describe('AdvancedMetrics', () => {
  let metrics;

  beforeEach(() => {
    metrics = createAdvancedMetrics({
      serviceName: 'test-service',
      enabled: true,
      samplingRate: 1.0, // 100% for testing
    });
  });

  describe('Configuration', () => {
    it('should create metrics with default config', () => {
      const defaultMetrics = createAdvancedMetrics();
      expect(defaultMetrics.config.serviceName).toBe('unrdf');
      expect(defaultMetrics.config.enabled).toBe(true);
    });

    it('should accept custom configuration', () => {
      const customMetrics = createAdvancedMetrics({
        serviceName: 'custom-service',
        samplingRate: 0.5,
      });

      expect(customMetrics.config.serviceName).toBe('custom-service');
      expect(customMetrics.config.samplingRate).toBe(0.5);
    });

    it('should validate configuration with Zod schema', () => {
      expect(() => {
        createAdvancedMetrics({
          samplingRate: 1.5, // Invalid: > 1
        });
      }).toThrow();
    });
  });

  describe('Business Metrics', () => {
    it('should record successful operation', () => {
      const spy = vi.spyOn(metrics.businessMetrics.operations, 'add');

      metrics.recordOperation({
        operation: 'sparql-query',
        success: true,
        duration: 50,
      });

      expect(spy).toHaveBeenCalledWith(1, {
        operation: 'sparql-query',
        result: 'success',
      });
    });

    it('should record failed operation with error type', () => {
      const spy = vi.spyOn(metrics.businessMetrics.failuresByType, 'add');

      metrics.recordOperation({
        operation: 'triple-insert',
        success: false,
        duration: 100,
        errorType: 'ValidationError',
      });

      expect(spy).toHaveBeenCalledWith(1, {
        operation: 'triple-insert',
        error_type: 'ValidationError',
      });
    });

    it('should record SLA violations', () => {
      const spy = vi.spyOn(metrics.businessMetrics.slaViolations, 'add');

      metrics.recordOperation({
        operation: 'query-execution',
        success: true,
        duration: 150,
        slaThreshold: 100,
      });

      expect(spy).toHaveBeenCalledWith(1, {
        operation: 'query-execution',
        threshold: '100',
      });
    });

    it('should not record SLA violation when under threshold', () => {
      const spy = vi.spyOn(metrics.businessMetrics.slaViolations, 'add');

      metrics.recordOperation({
        operation: 'query-execution',
        success: true,
        duration: 50,
        slaThreshold: 100,
      });

      expect(spy).not.toHaveBeenCalled();
    });

    it('should record success rate', () => {
      const spy = vi.spyOn(metrics.businessMetrics.successRate, 'add');

      metrics.recordSuccessRate('query-execution', 0.95);

      expect(spy).toHaveBeenCalledWith(0.95, { operation: 'query-execution' });
    });
  });

  describe('Latency Metrics', () => {
    it('should record operation latency in histogram', () => {
      const spy = vi.spyOn(metrics.latencyMetrics.histogram, 'record');

      metrics.recordOperation({
        operation: 'parse-turtle',
        success: true,
        duration: 75,
      });

      expect(spy).toHaveBeenCalledWith(75, { operation: 'parse-turtle' });
    });

    it('should record latency percentiles', () => {
      const p50Spy = vi.spyOn(metrics.latencyMetrics.p50, 'add');
      const p95Spy = vi.spyOn(metrics.latencyMetrics.p95, 'add');
      const p99Spy = vi.spyOn(metrics.latencyMetrics.p99, 'add');

      metrics.recordLatencyPercentiles('query', {
        p50: 25,
        p90: 50,
        p95: 100,
        p99: 500,
        max: 1000,
      });

      expect(p50Spy).toHaveBeenCalledWith(25, { operation: 'query' });
      expect(p95Spy).toHaveBeenCalledWith(100, { operation: 'query' });
      expect(p99Spy).toHaveBeenCalledWith(500, { operation: 'query' });
    });
  });

  describe('Throughput Metrics', () => {
    it('should record throughput', () => {
      const spy = vi.spyOn(metrics.throughputMetrics.opsPerSecond, 'add');

      metrics.recordThroughput('insert', 125);

      expect(spy).toHaveBeenCalledWith(125, { operation: 'insert' });
    });

    it('should update throughput automatically', async () => {
      // Record operations
      for (let i = 0; i < 10; i++) {
        metrics.recordOperation({
          operation: 'test-op',
          success: true,
          duration: 10,
        });
      }

      // Wait for throughput calculation (>1 second)
      await new Promise(resolve => setTimeout(resolve, 1100));

      // Throughput should be calculated
      expect(metrics.operationCounts.size).toBe(0); // Cleared after calculation
    });
  });

  describe('Resource Metrics', () => {
    it('should record memory utilization', () => {
      const spy = vi.spyOn(metrics.resourceMetrics.heapUsed, 'add');

      metrics.recordResourceUtilization();

      expect(spy).toHaveBeenCalled();
      const callArgs = spy.mock.calls[0][0];
      expect(callArgs).toBeGreaterThan(0);
    });

    it('should record event loop lag', () => {
      const lagSpy = vi.spyOn(metrics.resourceMetrics.eventLoopLag, 'record');
      const cpuSpy = vi.spyOn(metrics.resourceMetrics.cpuLoad, 'add');

      metrics.recordEventLoopLag(15);

      expect(lagSpy).toHaveBeenCalledWith(15);
      expect(cpuSpy).toHaveBeenCalled();
    });

    it('should estimate CPU load from event loop lag', () => {
      const spy = vi.spyOn(metrics.resourceMetrics.cpuLoad, 'add');

      // High lag = high CPU estimate
      metrics.recordEventLoopLag(150);

      const cpuLoad = spy.mock.calls[0][0];
      expect(cpuLoad).toBeGreaterThan(0);
      expect(cpuLoad).toBeLessThanOrEqual(1);
    });
  });

  describe('Sampling', () => {
    it('should sample at configured rate', () => {
      const sampledMetrics = createAdvancedMetrics({
        enabled: true,
        samplingRate: 0.5, // 50%
      });

      const spy = vi.spyOn(sampledMetrics.businessMetrics.operations, 'add');

      // Record 100 operations
      for (let i = 0; i < 100; i++) {
        sampledMetrics.recordOperation({
          operation: 'test',
          success: true,
          duration: 10,
        });
      }

      // Should sample ~50 (with some variance)
      const callCount = spy.mock.calls.length;
      expect(callCount).toBeGreaterThan(30);
      expect(callCount).toBeLessThan(70);
    });

    it('should not record when disabled', () => {
      const disabledMetrics = createAdvancedMetrics({
        enabled: false,
      });

      const spy = vi.spyOn(disabledMetrics.businessMetrics?.operations || {}, 'add', () => {});

      disabledMetrics.recordOperation({
        operation: 'test',
        success: true,
        duration: 10,
      });

      expect(spy).not.toHaveBeenCalled();
    });
  });

  describe('Performance', () => {
    it('should record operation in <0.1ms', () => {
      const start = performance.now();

      for (let i = 0; i < 1000; i++) {
        metrics.recordOperation({
          operation: 'perf-test',
          success: true,
          duration: 10,
        });
      }

      const elapsed = performance.now() - start;
      const avgTime = elapsed / 1000;

      expect(avgTime).toBeLessThan(0.1); // <0.1ms per operation
    });

    it('should have minimal memory overhead', () => {
      const before = process.memoryUsage().heapUsed;

      // Record 10,000 operations
      for (let i = 0; i < 10000; i++) {
        metrics.recordOperation({
          operation: 'memory-test',
          success: true,
          duration: 10,
        });
      }

      const after = process.memoryUsage().heapUsed;
      const overhead = (after - before) / 1024 / 1024; // MB

      expect(overhead).toBeLessThan(10); // <10MB overhead
    });
  });

  describe('Summary', () => {
    it('should provide metrics summary', () => {
      metrics.recordOperation({
        operation: 'test-op',
        success: true,
        duration: 10,
      });

      const summary = metrics.getSummary();

      expect(summary).toHaveProperty('enabled');
      expect(summary).toHaveProperty('samplingRate');
      expect(summary).toHaveProperty('operationTypes');
      expect(summary.enabled).toBe(true);
    });
  });
});
