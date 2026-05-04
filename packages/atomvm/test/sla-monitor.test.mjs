/**
 * @fileoverview SLA Monitor Tests
 * @module test/sla-monitor
 *
 * Tests for SLA monitoring, latency tracking, error rate detection,
 * percentile calculations, and report generation.
 */

import { describe, it, beforeEach, expect } from 'vitest';
import {
  SLAMonitor,
  createSLAMonitor,
  defaultSLAMonitor,
  OPERATION_TYPES,
} from '../src/sla-monitor.mjs';

describe('SLAMonitor', () => {
  let monitor;

  beforeEach(() => {
    monitor = new SLAMonitor({
      latencyThreshold: 10,
      errorRate: 0.001,
    });
  });

  describe('Latency Recording', () => {
    it('should record latency and calculate average', () => {
      monitor.recordLatency('sparql_query', 5);
      monitor.recordLatency('sparql_query', 7);
      monitor.recordLatency('sparql_query', 8);

      const metrics = monitor.getMetrics('sparql_query');

      expect(metrics.sampleCount).toBe(3);
      expect(metrics.successCount).toBe(3);
      expect(Math.abs(metrics.latency.avg - (5 + 7 + 8) / 3)).toBeLessThan(0.001);
    });

    it('should calculate min and max latency', () => {
      monitor.recordLatency('test_op', 2);
      monitor.recordLatency('test_op', 15);
      monitor.recordLatency('test_op', 8);

      const metrics = monitor.getMetrics('test_op');

      expect(metrics.latency.min).toBe(2);
      expect(metrics.latency.max).toBe(15);
    });

    it('should calculate percentiles correctly', () => {
      // Add 100 samples with values 1-100
      for (let i = 1; i <= 100; i++) {
        monitor.recordLatency('percentile_test', i);
      }

      const metrics = monitor.getMetrics('percentile_test');

      // P50 should be around 50
      expect(metrics.latency.p50).toBeGreaterThanOrEqual(49);
      expect(metrics.latency.p50).toBeLessThanOrEqual(51);

      // P95 should be around 95
      expect(metrics.latency.p95).toBeGreaterThanOrEqual(94);
      expect(metrics.latency.p95).toBeLessThanOrEqual(96);

      // P99 should be around 99
      expect(metrics.latency.p99).toBeGreaterThanOrEqual(98);
      expect(metrics.latency.p99).toBeLessThanOrEqual(100);
    });

    it('should maintain sliding window of samples', () => {
      const smallWindowMonitor = new SLAMonitor({ windowSize: 5 });

      for (let i = 1; i <= 10; i++) {
        smallWindowMonitor.recordLatency('window_test', i);
      }

      const metrics = smallWindowMonitor.getMetrics('window_test');

      // Should only have last 5 samples (6, 7, 8, 9, 10)
      expect(metrics.sampleCount).toBe(5);
      expect(metrics.latency.min).toBe(6);
      expect(metrics.latency.max).toBe(10);
    });

    it('should reject invalid operation name', () => {
      expect(() => monitor.recordLatency('', 5)).toThrow(/non-empty string/);
      expect(() => monitor.recordLatency(null, 5)).toThrow(/non-empty string/);
      expect(() => monitor.recordLatency(123, 5)).toThrow(/non-empty string/);
    });

    it('should reject invalid latency value', () => {
      expect(() => monitor.recordLatency('test', -1)).toThrow(/non-negative number/);
      expect(() => monitor.recordLatency('test', 'fast')).toThrow(/non-negative number/);
      expect(() => monitor.recordLatency('test', NaN)).toThrow(/non-negative number/);
    });
  });

  describe('Error Recording', () => {
    it('should record errors and compute error rate', () => {
      monitor.recordLatency('error_test', 5);
      monitor.recordLatency('error_test', 5);
      monitor.recordLatency('error_test', 5);
      monitor.recordError('error_test', new Error('Test error'));

      const metrics = monitor.getMetrics('error_test');

      expect(metrics.errorCount).toBe(1);
      expect(metrics.successCount).toBe(3);
      // Error rate = 1 / 4 = 0.25
      expect(Math.abs(metrics.errorRate - 0.25)).toBeLessThan(0.001);
    });

    it('should handle string error messages', () => {
      monitor.recordError('string_error', 'Connection timeout');

      const metrics = monitor.getMetrics('string_error');
      expect(metrics.errorCount).toBe(1);
    });

    it('should reject invalid operation name for errors', () => {
      expect(() => monitor.recordError('', new Error('test'))).toThrow(/non-empty string/);
    });
  });

  describe('SLA Violation Detection', () => {
    it('should detect latency violation (>10ms)', () => {
      monitor.recordLatency('slow_op', 15); // Exceeds 10ms threshold

      const violations = monitor.getViolations();

      expect(violations.length).toBe(1);
      expect(violations[0].operation).toBe('slow_op');
      expect(violations[0].type).toBe('latency');
      expect(violations[0].message).toContain('15.00ms');
    });

    it('should detect error rate violation (>0.1%)', () => {
      // Need at least 10 operations for error rate check
      for (let i = 0; i < 10; i++) {
        monitor.recordLatency('error_rate_test', 5);
      }

      // Now add errors to exceed 0.1% threshold
      // With 10 successes, we need enough errors to exceed 0.1%
      // Adding 2 errors makes error rate = 2/12 = 16.7%
      monitor.recordError('error_rate_test', new Error('Error 1'));
      monitor.recordError('error_rate_test', new Error('Error 2'));

      const violations = monitor.getViolations();
      const errorViolations = violations.filter(v => v.type === 'error_rate');

      expect(errorViolations.length).toBeGreaterThan(0);
    });

    it('should mark operation as not within SLA on violation', () => {
      // P95 above threshold
      for (let i = 0; i < 20; i++) {
        monitor.recordLatency('sla_check', 15); // All above 10ms
      }

      expect(monitor.isWithinSLA('sla_check')).toBe(false);
    });

    it('should mark operation as within SLA when compliant', () => {
      for (let i = 0; i < 20; i++) {
        monitor.recordLatency('compliant_op', 5); // All below 10ms
      }

      expect(monitor.isWithinSLA('compliant_op')).toBe(true);
    });

    it('should return true for unknown operations', () => {
      expect(monitor.isWithinSLA('unknown_op')).toBe(true);
    });
  });

  describe('Report Generation', () => {
    it('should generate text format report', () => {
      monitor.recordLatency('query', 5);
      monitor.recordLatency('query', 8);
      monitor.recordLatency('insert', 2);
      monitor.recordError('insert', new Error('Failed'));

      const report = monitor.generateReport();

      expect(report).toContain('SLA COMPLIANCE REPORT');
      expect(report).toContain('query');
      expect(report).toContain('insert');
      expect(report).toContain('Latency');
      expect(report).toContain('P95');
    });

    it('should show thresholds in report', () => {
      const report = monitor.generateReport();

      expect(report).toContain('10ms');
      expect(report).toContain('0.1%');
    });

    it('should show violations in report', () => {
      monitor.recordLatency('slow_query', 25);

      const report = monitor.generateReport();

      expect(report).toContain('VIOLATIONS');
      expect(report).toContain('slow_query');
    });

    it('should handle empty metrics', () => {
      const report = monitor.generateReport();

      expect(report).toContain('No operations recorded');
    });
  });

  describe('Metrics Export', () => {
    it('should export metrics as JSON', () => {
      monitor.recordLatency('export_test', 5);
      monitor.recordLatency('export_test', 8);

      const exported = monitor.exportMetrics();

      expect(exported.generatedAt).toBeDefined();
      expect(exported.thresholds).toBeDefined();
      expect(exported.thresholds.latencyMs).toBe(10);
      expect(exported.operations.export_test).toBeDefined();
      expect(exported.summary).toBeDefined();
      expect(exported.summary.operationCount).toBe(1);
    });

    it('should export OTEL-compatible metrics', () => {
      monitor.recordLatency('otel_test', 5);

      const otelMetrics = monitor.exportOTELMetrics();

      expect(otelMetrics.resource).toBeDefined();
      expect(Array.isArray(otelMetrics.metrics)).toBe(true);
      expect(otelMetrics.metrics.length).toBeGreaterThan(0);

      // Check for expected metric types
      const metricNames = otelMetrics.metrics.map(m => m.name);
      expect(metricNames).toContain('sla_operation_latency_ms');
      expect(metricNames).toContain('sla_operation_error_rate');
      expect(metricNames).toContain('sla_compliance');
    });

    it('should include compliance status in export', () => {
      // Compliant operation
      monitor.recordLatency('good_op', 5);

      // Non-compliant operation
      for (let i = 0; i < 10; i++) {
        monitor.recordLatency('bad_op', 15);
      }

      const exported = monitor.exportMetrics();

      expect(exported.operations.good_op.withinSLA).toBe(true);
      expect(exported.operations.bad_op.withinSLA).toBe(false);
    });
  });

  describe('Reset Functionality', () => {
    it('should reset all metrics', () => {
      monitor.recordLatency('op1', 5);
      monitor.recordLatency('op2', 8);
      monitor.recordError('op1', new Error('test'));

      monitor.reset();

      const allMetrics = monitor.getAllMetrics();
      expect(allMetrics.size).toBe(0);
      expect(monitor.getViolations().length).toBe(0);
    });

    it('should reset specific operation', () => {
      monitor.recordLatency('keep', 5);
      monitor.recordLatency('remove', 8);

      monitor.resetOperation('remove');

      expect(monitor.getMetrics('keep')).toBeDefined();
      expect(monitor.getMetrics('remove')).toBeNull();
    });
  });

  describe('Threshold Management', () => {
    it('should get current thresholds', () => {
      const thresholds = monitor.getThresholds();

      expect(thresholds.latencyMs).toBe(10);
      expect(thresholds.errorRate).toBe(0.001);
    });

    it('should update thresholds dynamically', () => {
      monitor.setThresholds({
        latencyMs: 5,
        errorRate: 0.0001,
      });

      const thresholds = monitor.getThresholds();
      expect(thresholds.latencyMs).toBe(5);
      expect(thresholds.errorRate).toBe(0.0001);
    });

    it('should reject invalid threshold values', () => {
      expect(() => monitor.setThresholds({ latencyMs: -1 })).toThrow(/positive number/);
      expect(() => monitor.setThresholds({ errorRate: 2 })).toThrow(/between 0 and 1/);
    });
  });

  describe('Throughput Tracking', () => {
    it('should track operations per second', async () => {
      // Record several operations
      for (let i = 0; i < 10; i++) {
        monitor.recordLatency('throughput_test', 1);
      }

      const metrics = monitor.getMetrics('throughput_test');

      expect(metrics.throughput.operationsPerSecond).toBeGreaterThan(0);
      expect(metrics.throughput.totalOperations).toBe(10);
    });
  });

  describe('Standard Operation Types', () => {
    it('should export operation type constants', () => {
      expect(OPERATION_TYPES.TRIPLE_INSERT).toBe('triple_insert');
      expect(OPERATION_TYPES.SPARQL_QUERY).toBe('sparql_query');
      expect(OPERATION_TYPES.RPC_CALL).toBe('rpc_call');
      expect(OPERATION_TYPES.VALIDATION).toBe('validation');
      expect(OPERATION_TYPES.CACHE_HIT).toBe('cache_hit');
    });

    it('should work with operation type constants', () => {
      monitor.recordLatency(OPERATION_TYPES.SPARQL_QUERY, 5);
      monitor.recordLatency(OPERATION_TYPES.CACHE_HIT, 0.5);

      const queryMetrics = monitor.getMetrics(OPERATION_TYPES.SPARQL_QUERY);
      const cacheMetrics = monitor.getMetrics(OPERATION_TYPES.CACHE_HIT);

      expect(queryMetrics).toBeDefined();
      expect(cacheMetrics).toBeDefined();
      expect(cacheMetrics.latency.avg).toBeLessThan(1); // Cache hit should be fast
    });
  });

  describe('Factory Functions', () => {
    it('should create monitor with factory function', () => {
      const customMonitor = createSLAMonitor({
        latencyThreshold: 5,
        errorRate: 0.01,
      });

      const thresholds = customMonitor.getThresholds();
      expect(thresholds.latencyMs).toBe(5);
      expect(thresholds.errorRate).toBe(0.01);
    });

    it('should have default monitor instance', () => {
      expect(defaultSLAMonitor).toBeInstanceOf(SLAMonitor);

      // Default thresholds
      const thresholds = defaultSLAMonitor.getThresholds();
      expect(thresholds.latencyMs).toBe(10);
      expect(thresholds.errorRate).toBe(0.001);
    });
  });

  describe('getAllMetrics', () => {
    it('should return metrics for all operations', () => {
      monitor.recordLatency('op1', 5);
      monitor.recordLatency('op2', 8);
      monitor.recordLatency('op3', 3);

      const allMetrics = monitor.getAllMetrics();

      expect(allMetrics.size).toBe(3);
      expect(allMetrics.has('op1')).toBe(true);
      expect(allMetrics.has('op2')).toBe(true);
      expect(allMetrics.has('op3')).toBe(true);
    });
  });

  describe('Edge Cases', () => {
    it('should handle single sample', () => {
      monitor.recordLatency('single', 7);

      const metrics = monitor.getMetrics('single');

      expect(metrics.sampleCount).toBe(1);
      expect(metrics.latency.avg).toBe(7);
      expect(metrics.latency.p50).toBe(7);
      expect(metrics.latency.p95).toBe(7);
      expect(metrics.latency.p99).toBe(7);
    });

    it('should handle zero latency', () => {
      monitor.recordLatency('instant', 0);

      const metrics = monitor.getMetrics('instant');
      expect(metrics.latency.avg).toBe(0);
    });

    it('should handle very high latency', () => {
      monitor.recordLatency('slow', 10000);

      const metrics = monitor.getMetrics('slow');
      expect(metrics.latency.max).toBe(10000);
      expect(metrics.withinSLA).toBe(false);
    });
  });
});
