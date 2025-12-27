/**
 * @fileoverview Monitoring Tests
 * @module monitoring/monitoring.test
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { MetricsCollector } from './metrics-collector.mjs';
import { AdaptiveOptimizer } from './adaptive-optimizer.mjs';
import { PrometheusExporter } from './prometheus-exporter.mjs';

describe('MetricsCollector', () => {
  let collector;

  beforeEach(() => {
    collector = new MetricsCollector({
      sampleInterval: 100,
      bufferSize: 100,
    });
  });

  afterEach(() => {
    collector.stop();
  });

  it('should create a metrics collector', () => {
    expect(collector).toBeDefined();
    expect(collector.sampleInterval).toBe(100);
    expect(collector.bufferSize).toBe(100);
  });

  it('should start and stop collection', () => {
    collector.start();
    expect(collector.intervalId).not.toBeNull();

    collector.stop();
    expect(collector.intervalId).toBeNull();
  });

  it('should record operations', () => {
    collector.recordOperation(10.5);
    collector.recordOperation(15.2);
    collector.recordOperation(8.3);

    expect(collector.totalOperations).toBe(3);
    expect(collector.latencyBuffer).toHaveLength(3);
  });

  it('should record compression ratios', () => {
    collector.recordCompression(2.5);
    collector.recordCompression(3.0);

    expect(collector.compressionSamples).toBe(2);
    expect(collector.compressionRatioSum).toBe(5.5);
  });

  it('should update receipt count', () => {
    collector.updateReceiptCount(100);
    expect(collector.lastReceiptCount).toBe(100);

    collector.updateReceiptCount(150);
    expect(collector.lastReceiptCount).toBe(150);
  });

  it('should update agent metrics', () => {
    collector.updateAgentMetrics('α₁', {
      tasksCompleted: 10,
      tasksQueued: 5,
      cpuTime: 1000,
      memoryUsed: 1024 * 1024,
      avgLatency: 12.5,
    });

    const metrics = collector.getAgentMetrics('α₁');
    expect(metrics).toBeDefined();
    expect(metrics.tasksCompleted).toBe(10);
    expect(metrics.tasksQueued).toBe(5);
  });

  it('should calculate latency percentiles', () => {
    // Add sample latencies
    for (let i = 0; i < 100; i++) {
      collector.recordOperation(i);
    }

    const stats = collector.calculateLatencyPercentiles();

    expect(stats.p50).toBeGreaterThan(0);
    expect(stats.p95).toBeGreaterThan(stats.p50);
    expect(stats.p99).toBeGreaterThan(stats.p95);
    expect(stats.min).toBe(0);
    expect(stats.max).toBe(99);
    expect(stats.count).toBe(100);
  });

  it('should collect snapshots', () => {
    collector.start();

    return new Promise(resolve => {
      setTimeout(() => {
        const snapshots = collector.getAllSnapshots();
        expect(snapshots.length).toBeGreaterThan(0);

        const current = collector.getCurrentSnapshot();
        expect(current).toBeDefined();
        expect(current.timestamp).toBeGreaterThan(0);
        expect(current.cpu).toBeDefined();
        expect(current.memory).toBeDefined();

        collector.stop();
        resolve();
      }, 200);
    });
  });

  it('should calculate average throughput', async () => {
    collector.start();

    // Record operations
    for (let i = 0; i < 10; i++) {
      collector.recordOperation(5);
    }

    await new Promise(resolve => setTimeout(resolve, 200));

    const avgThroughput = collector.getAverageThroughput(1000);
    expect(avgThroughput).toBeGreaterThanOrEqual(0);

    collector.stop();
  });

  it('should export to JSON', () => {
    collector.recordOperation(10);
    collector.recordCompression(2.5);
    collector.updateReceiptCount(50);

    const json = collector.toJSON();

    expect(json).toBeDefined();
    expect(json.startTime).toBeGreaterThan(0);
    expect(json.summary).toBeDefined();
    expect(json.summary.totalOperations).toBe(1);
  });
});

describe('AdaptiveOptimizer', () => {
  let collector;
  let optimizer;

  beforeEach(() => {
    collector = new MetricsCollector({ sampleInterval: 100 });
    optimizer = new AdaptiveOptimizer(collector, {
      evaluationInterval: 100,
      minCooldown: 100,
    });
  });

  afterEach(() => {
    optimizer.stop();
    collector.stop();
  });

  it('should create an adaptive optimizer', () => {
    expect(optimizer).toBeDefined();
    expect(optimizer.parameters).toBeDefined();
    expect(optimizer.strategies.size).toBeGreaterThan(0);
  });

  it('should register default strategies', () => {
    expect(optimizer.strategies.has('tune-epsilon')).toBe(true);
    expect(optimizer.strategies.has('allocate-budget')).toBe(true);
    expect(optimizer.strategies.has('scale-agents')).toBe(true);
    expect(optimizer.strategies.has('optimize-batch-size')).toBe(true);
  });

  it('should register custom strategy', () => {
    optimizer.registerStrategy({
      name: 'custom-strategy',
      evaluate: () => false,
      apply: () => ({ success: false }),
      cooldown: 1000,
    });

    expect(optimizer.strategies.has('custom-strategy')).toBe(true);
  });

  it('should get and set parameters', () => {
    const params = optimizer.getParameters();
    expect(params.epsilon).toBeDefined();
    expect(params.budget).toBeDefined();

    optimizer.setParameter('epsilon', 0.05);
    expect(optimizer.getParameters().epsilon).toBe(0.05);
  });

  it('should start and stop', () => {
    optimizer.start();
    expect(optimizer.intervalId).not.toBeNull();

    optimizer.stop();
    expect(optimizer.intervalId).toBeNull();
  });

  it('should export to JSON', () => {
    const json = optimizer.toJSON();

    expect(json).toBeDefined();
    expect(json.parameters).toBeDefined();
    expect(json.history).toBeDefined();
    expect(json.strategies).toBeDefined();
    expect(json.strategies.length).toBeGreaterThan(0);
  });
});

describe('PrometheusExporter', () => {
  let collector;
  let exporter;

  beforeEach(() => {
    collector = new MetricsCollector({ sampleInterval: 100 });
    exporter = new PrometheusExporter(collector, {
      port: 9191, // Use different port for tests
      host: '127.0.0.1',
    });
  });

  afterEach(() => {
    exporter.stop();
    collector.stop();
  });

  it('should create a Prometheus exporter', () => {
    expect(exporter).toBeDefined();
    expect(exporter.port).toBe(9191);
    expect(exporter.namespace).toBe('unrdf');
  });

  it('should generate Prometheus output', async () => {
    collector.start();

    // Record some metrics
    collector.recordOperation(10);
    collector.recordOperation(15);
    collector.recordCompression(2.5);

    await new Promise(resolve => setTimeout(resolve, 150));

    const output = exporter.generatePrometheusOutput();

    expect(output).toBeDefined();
    expect(output).toContain('unrdf_cpu_percentage');
    expect(output).toContain('unrdf_memory_heap_used_bytes');
    expect(output).toContain('unrdf_throughput_ops_per_sec');
    expect(output).toContain('unrdf_latency_milliseconds');

    collector.stop();
  });

  it('should register custom metrics', () => {
    exporter.registerMetric('custom_counter', 'counter', 'Custom counter metric', 0);

    expect(exporter.customMetrics.has('custom_counter')).toBe(true);
  });

  it('should set metric values', () => {
    exporter.registerMetric('test_gauge', 'gauge', 'Test gauge', 0);
    exporter.setMetric('test_gauge', 42);

    const metric = exporter.customMetrics.get('test_gauge');
    expect(metric.value).toBe(42);
  });

  it('should increment counter metrics', () => {
    exporter.registerMetric('test_counter', 'counter', 'Test counter', 0);
    exporter.incrementMetric('test_counter', 5);
    exporter.incrementMetric('test_counter', 3);

    const metric = exporter.customMetrics.get('test_counter');
    expect(metric.value).toBe(8);
  });

  it('should start and stop HTTP server', async () => {
    exporter.start();
    expect(exporter.server).not.toBeNull();

    // Give server time to start
    await new Promise(resolve => setTimeout(resolve, 100));

    exporter.stop();
    expect(exporter.server).toBeNull();
  });
});
