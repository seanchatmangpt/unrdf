/**
 * @file Metrics Tests
 * @description Tests for metrics collection system
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { createMetrics } from '../src/metrics.mjs';

describe('Metrics System', () => {
  let metrics;

  beforeEach(() => {
    metrics = createMetrics({ prefix: 'test' });
  });

  describe('createMetrics', () => {
    it('should create metrics collector', () => {
      expect(metrics).toHaveProperty('incrementCounter');
      expect(metrics).toHaveProperty('recordGauge');
      expect(metrics).toHaveProperty('recordHistogram');
      expect(metrics).toHaveProperty('recordDuration');
      expect(metrics).toHaveProperty('recordSummary');
      expect(metrics).toHaveProperty('startTimer');
      expect(metrics).toHaveProperty('toPrometheus');
      expect(metrics).toHaveProperty('toJSON');
      expect(metrics).toHaveProperty('reset');
    });
  });

  describe('Counters', () => {
    it('should increment counter', () => {
      metrics.incrementCounter('requests_total');
      metrics.incrementCounter('requests_total');
      metrics.incrementCounter('requests_total');

      const data = metrics.toJSON();
      expect(data.counters['requests_total']).toBe(3);
    });

    it('should increment counter with labels', () => {
      metrics.incrementCounter('requests_total', { method: 'GET', status: '200' });
      metrics.incrementCounter('requests_total', { method: 'GET', status: '200' });
      metrics.incrementCounter('requests_total', { method: 'POST', status: '201' });

      const data = metrics.toJSON();
      expect(data.counters['requests_total{method="GET",status="200"}']).toBe(2);
      expect(data.counters['requests_total{method="POST",status="201"}']).toBe(1);
    });

    it('should increment counter by custom value', () => {
      metrics.incrementCounter('bytes_sent', {}, 1024);
      metrics.incrementCounter('bytes_sent', {}, 2048);

      const data = metrics.toJSON();
      expect(data.counters['bytes_sent']).toBe(3072);
    });
  });

  describe('Gauges', () => {
    it('should set gauge value', () => {
      metrics.recordGauge('active_connections', 42);
      metrics.recordGauge('active_connections', 38);

      const data = metrics.toJSON();
      expect(data.gauges['active_connections']).toBe(38);
    });

    it('should set gauge with labels', () => {
      metrics.recordGauge('queue_size', 10, { queue: 'default' });
      metrics.recordGauge('queue_size', 5, { queue: 'priority' });

      const data = metrics.toJSON();
      expect(data.gauges['queue_size{queue="default"}']).toBe(10);
      expect(data.gauges['queue_size{queue="priority"}']).toBe(5);
    });
  });

  describe('Histograms', () => {
    it('should record histogram observations', () => {
      metrics.recordHistogram('request_duration', 0.042);
      metrics.recordHistogram('request_duration', 0.098);
      metrics.recordHistogram('request_duration', 0.156);

      const data = metrics.toJSON();
      const histogram = data.histograms['request_duration'];

      expect(histogram.count).toBe(3);
      expect(histogram.sum).toBeCloseTo(0.296, 3);
      expect(histogram.avg).toBeCloseTo(0.0987, 3);
    });

    it('should track histogram buckets', () => {
      metrics.recordHistogram('request_duration', 0.001); // <0.005
      metrics.recordHistogram('request_duration', 0.008); // <0.01
      metrics.recordHistogram('request_duration', 0.042); // <0.05

      const data = metrics.toJSON();
      const histogram = data.histograms['request_duration'];

      expect(histogram.buckets['0.005']).toBe(1);
      expect(histogram.buckets['0.01']).toBe(2);
      expect(histogram.buckets['0.05']).toBe(3);
    });
  });

  describe('Timer', () => {
    it('should measure duration', async () => {
      const timer = metrics.startTimer();
      await new Promise(resolve => setTimeout(resolve, 100));
      const duration = timer.end();

      expect(duration).toBeGreaterThanOrEqual(0.09); // Allow 10ms tolerance
      expect(duration).toBeLessThan(0.2);
    });

    it('should record duration to histogram', async () => {
      const timer = metrics.startTimer();
      await new Promise(resolve => setTimeout(resolve, 50));
      metrics.recordDuration('operation_duration', timer);

      const data = metrics.toJSON();
      const histogram = data.histograms['operation_duration'];

      expect(histogram.count).toBe(1);
      expect(histogram.sum).toBeGreaterThanOrEqual(0.04); // Allow 10ms tolerance
    });
  });

  describe('Summary', () => {
    it('should record summary observations', () => {
      metrics.recordSummary('query_duration', 0.042);
      metrics.recordSummary('query_duration', 0.098);
      metrics.recordSummary('query_duration', 0.156);

      const data = metrics.toJSON();
      const summary = data.summaries['query_duration'];

      expect(summary.count).toBe(3);
      expect(summary.sum).toBeCloseTo(0.296, 3);
      expect(summary.avg).toBeCloseTo(0.0987, 3);
    });

    it('should calculate percentiles', () => {
      // Add values: 0.01, 0.02, ..., 0.10
      for (let i = 1; i <= 100; i++) {
        metrics.recordSummary('query_duration', i / 1000);
      }

      const data = metrics.toJSON();
      const summary = data.summaries['query_duration'];

      expect(summary.p50).toBeCloseTo(0.050, 2);
      expect(summary.p95).toBeCloseTo(0.095, 2);
      expect(summary.p99).toBeCloseTo(0.099, 2);
    });
  });

  describe('Prometheus Export', () => {
    it('should export metrics in Prometheus format', () => {
      metrics.incrementCounter('requests_total', { status: '200' });
      metrics.recordGauge('active_connections', 42);
      metrics.recordHistogram('request_duration', 0.042);

      const output = metrics.toPrometheus();

      expect(output).toContain('# TYPE test_requests_total counter');
      expect(output).toContain('test_requests_total{status="200"} 1');
      expect(output).toContain('# TYPE test_active_connections gauge');
      expect(output).toContain('test_active_connections 42');
      expect(output).toContain('# TYPE test_request_duration histogram');
      expect(output).toContain('test_request_duration_count');
      expect(output).toContain('test_request_duration_sum');
    });

    it('should export histogram buckets', () => {
      metrics.recordHistogram('request_duration', 0.042);

      const output = metrics.toPrometheus();

      expect(output).toContain('le="0.005"');
      expect(output).toContain('le="0.01"');
      expect(output).toContain('le="0.05"');
      expect(output).toContain('le="0.1"');
      expect(output).toContain('le="+Inf"');
    });
  });

  describe('JSON Export', () => {
    it('should export metrics as JSON', () => {
      metrics.incrementCounter('requests_total');
      metrics.recordGauge('active_connections', 42);
      metrics.recordHistogram('request_duration', 0.042);

      const data = metrics.toJSON();

      expect(data).toHaveProperty('counters');
      expect(data).toHaveProperty('gauges');
      expect(data).toHaveProperty('histograms');
      expect(data).toHaveProperty('summaries');
    });
  });

  describe('Reset', () => {
    it('should reset all metrics', () => {
      metrics.incrementCounter('requests_total');
      metrics.recordGauge('active_connections', 42);
      metrics.recordHistogram('request_duration', 0.042);

      metrics.reset();

      const data = metrics.toJSON();
      expect(Object.keys(data.counters).length).toBe(0);
      expect(Object.keys(data.gauges).length).toBe(0);
      expect(Object.keys(data.histograms).length).toBe(0);
    });
  });
});
