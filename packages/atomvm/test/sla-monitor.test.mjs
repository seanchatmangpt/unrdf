/**
 * @fileoverview SLA Monitor Tests
 * @module test/sla-monitor
 *
 * Tests for SLA monitoring, latency tracking, error rate detection,
 * percentile calculations, and report generation.
 */

import { describe, it, beforeEach } from 'node:test';
import assert from 'node:assert';
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

      assert.strictEqual(metrics.sampleCount, 3);
      assert.strictEqual(metrics.successCount, 3);
      assert.ok(Math.abs(metrics.latency.avg - (5 + 7 + 8) / 3) < 0.001);
    });

    it('should calculate min and max latency', () => {
      monitor.recordLatency('test_op', 2);
      monitor.recordLatency('test_op', 15);
      monitor.recordLatency('test_op', 8);

      const metrics = monitor.getMetrics('test_op');

      assert.strictEqual(metrics.latency.min, 2);
      assert.strictEqual(metrics.latency.max, 15);
    });

    it('should calculate percentiles correctly', () => {
      // Add 100 samples with values 1-100
      for (let i = 1; i <= 100; i++) {
        monitor.recordLatency('percentile_test', i);
      }

      const metrics = monitor.getMetrics('percentile_test');

      // P50 should be around 50
      assert.ok(metrics.latency.p50 >= 49 && metrics.latency.p50 <= 51);

      // P95 should be around 95
      assert.ok(metrics.latency.p95 >= 94 && metrics.latency.p95 <= 96);

      // P99 should be around 99
      assert.ok(metrics.latency.p99 >= 98 && metrics.latency.p99 <= 100);
    });

    it('should maintain sliding window of samples', () => {
      const smallWindowMonitor = new SLAMonitor({ windowSize: 5 });

      for (let i = 1; i <= 10; i++) {
        smallWindowMonitor.recordLatency('window_test', i);
      }

      const metrics = smallWindowMonitor.getMetrics('window_test');

      // Should only have last 5 samples (6, 7, 8, 9, 10)
      assert.strictEqual(metrics.sampleCount, 5);
      assert.strictEqual(metrics.latency.min, 6);
      assert.strictEqual(metrics.latency.max, 10);
    });

    it('should reject invalid operation name', () => {
      assert.throws(() => monitor.recordLatency('', 5), /non-empty string/);
      assert.throws(() => monitor.recordLatency(null, 5), /non-empty string/);
      assert.throws(() => monitor.recordLatency(123, 5), /non-empty string/);
    });

    it('should reject invalid latency value', () => {
      assert.throws(() => monitor.recordLatency('test', -1), /non-negative number/);
      assert.throws(() => monitor.recordLatency('test', 'fast'), /non-negative number/);
      assert.throws(() => monitor.recordLatency('test', NaN), /non-negative number/);
    });
  });

  describe('Error Recording', () => {
    it('should record errors and compute error rate', () => {
      monitor.recordLatency('error_test', 5);
      monitor.recordLatency('error_test', 5);
      monitor.recordLatency('error_test', 5);
      monitor.recordError('error_test', new Error('Test error'));

      const metrics = monitor.getMetrics('error_test');

      assert.strictEqual(metrics.errorCount, 1);
      assert.strictEqual(metrics.successCount, 3);
      // Error rate = 1 / 4 = 0.25
      assert.ok(Math.abs(metrics.errorRate - 0.25) < 0.001);
    });

    it('should handle string error messages', () => {
      monitor.recordError('string_error', 'Connection timeout');

      const metrics = monitor.getMetrics('string_error');
      assert.strictEqual(metrics.errorCount, 1);
    });

    it('should reject invalid operation name for errors', () => {
      assert.throws(() => monitor.recordError('', new Error('test')), /non-empty string/);
    });
  });

  describe('SLA Violation Detection', () => {
    it('should detect latency violation (>10ms)', () => {
      monitor.recordLatency('slow_op', 15); // Exceeds 10ms threshold

      const violations = monitor.getViolations();

      assert.strictEqual(violations.length, 1);
      assert.strictEqual(violations[0].operation, 'slow_op');
      assert.strictEqual(violations[0].type, 'latency');
      assert.ok(violations[0].message.includes('15.00ms'));
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

      assert.ok(errorViolations.length > 0);
    });

    it('should mark operation as not within SLA on violation', () => {
      // P95 above threshold
      for (let i = 0; i < 20; i++) {
        monitor.recordLatency('sla_check', 15); // All above 10ms
      }

      assert.strictEqual(monitor.isWithinSLA('sla_check'), false);
    });

    it('should mark operation as within SLA when compliant', () => {
      for (let i = 0; i < 20; i++) {
        monitor.recordLatency('compliant_op', 5); // All below 10ms
      }

      assert.strictEqual(monitor.isWithinSLA('compliant_op'), true);
    });

    it('should return true for unknown operations', () => {
      assert.strictEqual(monitor.isWithinSLA('unknown_op'), true);
    });
  });

  describe('Report Generation', () => {
    it('should generate text format report', () => {
      monitor.recordLatency('query', 5);
      monitor.recordLatency('query', 8);
      monitor.recordLatency('insert', 2);
      monitor.recordError('insert', new Error('Failed'));

      const report = monitor.generateReport();

      assert.ok(report.includes('SLA COMPLIANCE REPORT'));
      assert.ok(report.includes('query'));
      assert.ok(report.includes('insert'));
      assert.ok(report.includes('Latency'));
      assert.ok(report.includes('P95'));
    });

    it('should show thresholds in report', () => {
      const report = monitor.generateReport();

      assert.ok(report.includes('10ms'));
      assert.ok(report.includes('0.1%'));
    });

    it('should show violations in report', () => {
      monitor.recordLatency('slow_query', 25);

      const report = monitor.generateReport();

      assert.ok(report.includes('VIOLATIONS'));
      assert.ok(report.includes('slow_query'));
    });

    it('should handle empty metrics', () => {
      const report = monitor.generateReport();

      assert.ok(report.includes('No operations recorded'));
    });
  });

  describe('Metrics Export', () => {
    it('should export metrics as JSON', () => {
      monitor.recordLatency('export_test', 5);
      monitor.recordLatency('export_test', 8);

      const exported = monitor.exportMetrics();

      assert.ok(exported.generatedAt);
      assert.ok(exported.thresholds);
      assert.strictEqual(exported.thresholds.latencyMs, 10);
      assert.ok(exported.operations.export_test);
      assert.ok(exported.summary);
      assert.strictEqual(exported.summary.operationCount, 1);
    });

    it('should export OTEL-compatible metrics', () => {
      monitor.recordLatency('otel_test', 5);

      const otelMetrics = monitor.exportOTELMetrics();

      assert.ok(otelMetrics.resource);
      assert.ok(Array.isArray(otelMetrics.metrics));
      assert.ok(otelMetrics.metrics.length > 0);

      // Check for expected metric types
      const metricNames = otelMetrics.metrics.map(m => m.name);
      assert.ok(metricNames.includes('sla_operation_latency_ms'));
      assert.ok(metricNames.includes('sla_operation_error_rate'));
      assert.ok(metricNames.includes('sla_compliance'));
    });

    it('should include compliance status in export', () => {
      // Compliant operation
      monitor.recordLatency('good_op', 5);

      // Non-compliant operation
      for (let i = 0; i < 10; i++) {
        monitor.recordLatency('bad_op', 15);
      }

      const exported = monitor.exportMetrics();

      assert.strictEqual(exported.operations.good_op.withinSLA, true);
      assert.strictEqual(exported.operations.bad_op.withinSLA, false);
    });
  });

  describe('Reset Functionality', () => {
    it('should reset all metrics', () => {
      monitor.recordLatency('op1', 5);
      monitor.recordLatency('op2', 8);
      monitor.recordError('op1', new Error('test'));

      monitor.reset();

      const allMetrics = monitor.getAllMetrics();
      assert.strictEqual(allMetrics.size, 0);
      assert.strictEqual(monitor.getViolations().length, 0);
    });

    it('should reset specific operation', () => {
      monitor.recordLatency('keep', 5);
      monitor.recordLatency('remove', 8);

      monitor.resetOperation('remove');

      assert.ok(monitor.getMetrics('keep'));
      assert.strictEqual(monitor.getMetrics('remove'), null);
    });
  });

  describe('Threshold Management', () => {
    it('should get current thresholds', () => {
      const thresholds = monitor.getThresholds();

      assert.strictEqual(thresholds.latencyMs, 10);
      assert.strictEqual(thresholds.errorRate, 0.001);
    });

    it('should update thresholds dynamically', () => {
      monitor.setThresholds({
        latencyMs: 5,
        errorRate: 0.0001,
      });

      const thresholds = monitor.getThresholds();
      assert.strictEqual(thresholds.latencyMs, 5);
      assert.strictEqual(thresholds.errorRate, 0.0001);
    });

    it('should reject invalid threshold values', () => {
      assert.throws(() => monitor.setThresholds({ latencyMs: -1 }), /positive number/);
      assert.throws(() => monitor.setThresholds({ errorRate: 2 }), /between 0 and 1/);
    });
  });

  describe('Throughput Tracking', () => {
    it('should track operations per second', async () => {
      // Record several operations
      for (let i = 0; i < 10; i++) {
        monitor.recordLatency('throughput_test', 1);
      }

      const metrics = monitor.getMetrics('throughput_test');

      assert.ok(metrics.throughput.operationsPerSecond > 0);
      assert.strictEqual(metrics.throughput.totalOperations, 10);
    });
  });

  describe('Standard Operation Types', () => {
    it('should export operation type constants', () => {
      assert.strictEqual(OPERATION_TYPES.TRIPLE_INSERT, 'triple_insert');
      assert.strictEqual(OPERATION_TYPES.SPARQL_QUERY, 'sparql_query');
      assert.strictEqual(OPERATION_TYPES.RPC_CALL, 'rpc_call');
      assert.strictEqual(OPERATION_TYPES.VALIDATION, 'validation');
      assert.strictEqual(OPERATION_TYPES.CACHE_HIT, 'cache_hit');
    });

    it('should work with operation type constants', () => {
      monitor.recordLatency(OPERATION_TYPES.SPARQL_QUERY, 5);
      monitor.recordLatency(OPERATION_TYPES.CACHE_HIT, 0.5);

      const queryMetrics = monitor.getMetrics(OPERATION_TYPES.SPARQL_QUERY);
      const cacheMetrics = monitor.getMetrics(OPERATION_TYPES.CACHE_HIT);

      assert.ok(queryMetrics);
      assert.ok(cacheMetrics);
      assert.ok(cacheMetrics.latency.avg < 1); // Cache hit should be fast
    });
  });

  describe('Factory Functions', () => {
    it('should create monitor with factory function', () => {
      const customMonitor = createSLAMonitor({
        latencyThreshold: 5,
        errorRate: 0.01,
      });

      const thresholds = customMonitor.getThresholds();
      assert.strictEqual(thresholds.latencyMs, 5);
      assert.strictEqual(thresholds.errorRate, 0.01);
    });

    it('should have default monitor instance', () => {
      assert.ok(defaultSLAMonitor instanceof SLAMonitor);

      // Default thresholds
      const thresholds = defaultSLAMonitor.getThresholds();
      assert.strictEqual(thresholds.latencyMs, 10);
      assert.strictEqual(thresholds.errorRate, 0.001);
    });
  });

  describe('getAllMetrics', () => {
    it('should return metrics for all operations', () => {
      monitor.recordLatency('op1', 5);
      monitor.recordLatency('op2', 8);
      monitor.recordLatency('op3', 3);

      const allMetrics = monitor.getAllMetrics();

      assert.strictEqual(allMetrics.size, 3);
      assert.ok(allMetrics.has('op1'));
      assert.ok(allMetrics.has('op2'));
      assert.ok(allMetrics.has('op3'));
    });
  });

  describe('Edge Cases', () => {
    it('should handle single sample', () => {
      monitor.recordLatency('single', 7);

      const metrics = monitor.getMetrics('single');

      assert.strictEqual(metrics.sampleCount, 1);
      assert.strictEqual(metrics.latency.avg, 7);
      assert.strictEqual(metrics.latency.p50, 7);
      assert.strictEqual(metrics.latency.p95, 7);
      assert.strictEqual(metrics.latency.p99, 7);
    });

    it('should handle zero latency', () => {
      monitor.recordLatency('instant', 0);

      const metrics = monitor.getMetrics('instant');
      assert.strictEqual(metrics.latency.avg, 0);
    });

    it('should handle very high latency', () => {
      monitor.recordLatency('slow', 10000);

      const metrics = monitor.getMetrics('slow');
      assert.strictEqual(metrics.latency.max, 10000);
      assert.strictEqual(metrics.withinSLA, false);
    });
  });
});
