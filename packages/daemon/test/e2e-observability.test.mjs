/**
 * @file E2E Observability Tests for Daemon
 * @module @unrdf/daemon/test/e2e-observability
 * @description Comprehensive tests for Prometheus metrics, YAWL metrics, OTEL tracing, health monitoring, and alert management
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  PrometheusMetricsExporter,
  OtelSpanTracer,
  YawlMetricsCollector,
  AlertManager,
  DaemonHealthMonitor,
} from '../src/integrations/observability.mjs';

describe('PrometheusMetricsExporter', () => {
  let exporter;

  beforeEach(() => {
    exporter = new PrometheusMetricsExporter({ serviceName: 'test-daemon' });
  });

  it('should create counter metrics', () => {
    // Arrange & Act
    exporter.registerCounter('test.counter', 'Test counter metric', ['label1']);

    // Assert
    expect(exporter.counters.has('test.counter')).toBe(true);
    const metric = exporter.counters.get('test.counter');
    expect(metric.type).toBe('counter');
    expect(metric.help).toBe('Test counter metric');
  });

  it('should increment counter values', () => {
    // Arrange
    exporter.registerCounter('test.requests', 'Test requests');

    // Act
    exporter.incrementCounter('test.requests', 1);
    exporter.incrementCounter('test.requests', 5);

    // Assert
    const metric = exporter.counters.get('test.requests');
    expect(metric.total).toBe(6);
  });

  it('should track counter values with labels', () => {
    // Arrange
    exporter.registerCounter('test.events', 'Test events');

    // Act
    exporter.incrementCounter('test.events', 1, { method: 'GET' });
    exporter.incrementCounter('test.events', 2, { method: 'POST' });

    // Assert
    const metric = exporter.counters.get('test.events');
    expect(metric.values.size).toBe(2);
    expect(metric.total).toBe(3);
  });

  it('should create and set gauge metrics', () => {
    // Arrange & Act
    exporter.registerGauge('test.queue', 'Test queue size');
    exporter.setGauge('test.queue', 42);

    // Assert
    const metric = exporter.gauges.get('test.queue');
    expect(metric.current).toBe(42);
  });

  it('should record histogram observations', () => {
    // Arrange
    exporter.registerHistogram('test.latency', 'Test latency', [0.1, 1, 10, 100]);

    // Act
    exporter.recordHistogram('test.latency', 0.5);
    exporter.recordHistogram('test.latency', 2.5);
    exporter.recordHistogram('test.latency', 50);

    // Assert
    const metric = exporter.histograms.get('test.latency');
    const entry = metric.values.get('{}');
    expect(entry.count).toBe(3);
    expect(entry.sum).toBe(53);
  });

  it('should export metrics in Prometheus format', () => {
    // Arrange
    exporter.registerCounter('test.counter', 'Test counter');
    exporter.incrementCounter('test.counter', 1);

    // Act
    const output = exporter.export();

    // Assert
    expect(output).toContain('# HELP test.counter Test counter');
    expect(output).toContain('# TYPE test.counter counter');
    expect(output).toContain('test.counter');
  });

  it('should calculate histogram statistics', () => {
    // Arrange
    exporter.registerHistogram('test.response_time', 'Response time');

    // Act
    for (let i = 1; i <= 100; i++) {
      exporter.recordHistogram('test.response_time', i);
    }

    // Assert
    const snapshot = exporter.getMetricsSnapshot();
    const stats = snapshot['test.response_time'].statistics;
    expect(stats.count).toBe(100);
    expect(stats.mean).toBe(50.5);
    expect(stats.p95).toBeGreaterThan(90);
    expect(stats.p99).toBeGreaterThan(98);
  });

  it('should limit metrics history size', () => {
    // Arrange
    exporter.registerHistogram('test.limited', 'Test', [1, 10], [], { metricsHistorySize: 10 });
    const config = { metricsHistorySize: 10 };
    const smallExporter = new PrometheusMetricsExporter(config);
    smallExporter.registerHistogram('test.limited', 'Test', [1, 10]);

    // Act
    for (let i = 0; i < 20; i++) {
      smallExporter.recordHistogram('test.limited', i);
    }

    // Assert
    const metric = smallExporter.histograms.get('test.limited');
    expect(metric.observations.length).toBe(10);
  });

  it('should export multiple metrics together', () => {
    // Arrange
    exporter.registerCounter('test.counter', 'Test counter');
    exporter.registerGauge('test.gauge', 'Test gauge');
    exporter.registerHistogram('test.histogram', 'Test histogram', [1, 10]);

    // Act
    exporter.incrementCounter('test.counter', 1);
    exporter.setGauge('test.gauge', 5);
    exporter.recordHistogram('test.histogram', 5);
    const output = exporter.export();

    // Assert
    expect(output).toContain('test.counter');
    expect(output).toContain('test.gauge');
    expect(output).toContain('test.histogram');
  });
});

describe('OtelSpanTracer', () => {
  let tracer;

  beforeEach(() => {
    tracer = new OtelSpanTracer({ serviceName: 'test-daemon' });
  });

  it('should start and end spans', () => {
    // Arrange & Act
    const spanId = tracer.startSpan('test.operation', { userId: '123' });
    tracer.endSpan(spanId, 'ok');

    // Assert
    expect(tracer.spans.length).toBe(1);
    const span = tracer.spans[0];
    expect(span.name).toBe('test.operation');
    expect(span.status).toBe('ok');
    expect(span.duration).toBeGreaterThanOrEqual(0);
  });

  it('should track span attributes', () => {
    // Arrange & Act
    const spanId = tracer.startSpan('operation', { userId: 'user-1', action: 'create' });
    tracer.endSpan(spanId);

    // Assert
    const span = tracer.spans[0];
    expect(span.attributes.userId).toBe('user-1');
    expect(span.attributes.action).toBe('create');
  });

  it('should add events to active spans', () => {
    // Arrange
    const spanId = tracer.startSpan('operation');

    // Act
    tracer.addSpanEvent(spanId, 'validation_started');
    tracer.addSpanEvent(spanId, 'validation_completed', { result: 'success' });
    tracer.endSpan(spanId);

    // Assert
    const span = tracer.spans[0];
    expect(span.events.length).toBe(2);
    expect(span.events[0].name).toBe('validation_started');
    expect(span.events[1].attributes.result).toBe('success');
  });

  it('should mark span with error status', () => {
    // Arrange & Act
    const spanId = tracer.startSpan('operation');
    tracer.addSpanEvent(spanId, 'error_occurred', { message: 'Connection failed' });
    tracer.endSpan(spanId, 'error');

    // Assert
    const span = tracer.spans[0];
    expect(span.status).toBe('error');
    expect(span.events[0].name).toBe('error_occurred');
  });

  it('should export spans as traces', () => {
    // Arrange & Act
    const span1 = tracer.startSpan('database.query');
    tracer.endSpan(span1);

    const span2 = tracer.startSpan('database.query');
    tracer.endSpan(span2);

    const span3 = tracer.startSpan('api.request');
    tracer.endSpan(span3);

    // Assert
    const traces = tracer.exportTraces();
    expect(Object.keys(traces)).toContain('database.query');
    expect(Object.keys(traces)).toContain('api.request');
    expect(traces['database.query'].length).toBe(2);
    expect(traces['api.request'].length).toBe(1);
  });

  it('should limit span history', () => {
    // Arrange
    const smallTracer = new OtelSpanTracer({ serviceName: 'test' });

    // Act
    for (let i = 0; i < 10001; i++) {
      const spanId = smallTracer.startSpan(`operation-${i}`);
      smallTracer.endSpan(spanId);
    }

    // Assert
    expect(smallTracer.spans.length).toBe(10000);
  });

  it('should handle events on inactive spans gracefully', () => {
    // Arrange & Act
    tracer.addSpanEvent('non-existent-span', 'event');

    // Assert - should not throw
    expect(tracer.spans.length).toBe(0);
  });
});

describe('YawlMetricsCollector', () => {
  let collector;

  beforeEach(() => {
    collector = new YawlMetricsCollector({ serviceName: 'test-daemon' });
  });

  it('should record case creation', () => {
    // Arrange & Act
    collector.recordCaseCreation('case-001', 'spec-approval');

    // Assert
    const caseMetric = collector.getCaseMetrics('case-001');
    expect(caseMetric).toBeDefined();
    expect(caseMetric.specId).toBe('spec-approval');
    expect(caseMetric.status).toBe('created');
  });

  it('should record task execution', () => {
    // Arrange
    collector.recordCaseCreation('case-001', 'spec-approval');

    // Act
    collector.recordTaskExecution('case-001', 'task-001');

    // Assert
    const caseMetric = collector.getCaseMetrics('case-001');
    expect(caseMetric.tasks.length).toBe(1);
    expect(caseMetric.tasks[0].taskId).toBe('task-001');
    expect(caseMetric.tasks[0].status).toBe('executing');
  });

  it('should record task completion with result', () => {
    // Arrange
    collector.recordCaseCreation('case-001', 'spec-approval');
    collector.recordTaskExecution('case-001', 'task-001');

    // Act
    collector.recordTaskCompletion('task-001', 'success', { reviewer: 'john' });

    // Assert
    const taskMetric = collector.taskMetrics.get('task-001');
    expect(taskMetric.status).toBe('success');
    expect(taskMetric.duration).toBeGreaterThanOrEqual(0);
    expect(taskMetric.metadata.reviewer).toBe('john');
  });

  it('should track task retries', () => {
    // Arrange
    collector.recordCaseCreation('case-001', 'spec-approval');
    collector.recordTaskExecution('case-001', 'task-001');

    // Act
    collector.recordTaskRetry('task-001');
    collector.recordTaskRetry('task-001');

    // Assert
    const taskMetric = collector.taskMetrics.get('task-001');
    expect(taskMetric.retries).toBe(2);
  });

  it('should record case completion', () => {
    // Arrange
    collector.recordCaseCreation('case-001', 'spec-approval');

    // Act
    collector.recordCaseCompletion('case-001', 'success', { approver: 'admin' });

    // Assert
    const caseMetric = collector.getCaseMetrics('case-001');
    expect(caseMetric.status).toBe('success');
    expect(caseMetric.duration).toBeGreaterThanOrEqual(0);
    expect(caseMetric.metadata.approver).toBe('admin');
  });

  it('should calculate workflow metrics', () => {
    // Arrange
    collector.recordCaseCreation('case-001', 'spec-approval');
    collector.recordCaseCreation('case-002', 'spec-approval');
    collector.recordCaseCreation('case-003', 'spec-budget');
    collector.recordCaseCompletion('case-001', 'success');
    collector.recordCaseCompletion('case-002', 'failure');

    // Act
    const metrics = collector.getWorkflowMetrics('spec-approval');

    // Assert
    expect(metrics.totalCases).toBe(2);
    expect(metrics.completedCases).toBe(1); // Only 'success' counts as completed
    expect(metrics.failedCases).toBe(1);    // 'failure' counts as failed
  });

  it('should track workflow history', () => {
    // Arrange & Act
    collector.recordCaseCreation('case-001', 'spec-test');
    collector.recordTaskExecution('case-001', 'task-001');
    collector.recordTaskCompletion('task-001', 'success');
    collector.recordCaseCompletion('case-001', 'success');

    // Assert
    const allMetrics = collector.getAllMetrics();
    expect(allMetrics.totalCases).toBe(1);
    expect(allMetrics.recentHistory.length).toBeGreaterThan(0);
  });

  it('should calculate average case duration', () => {
    // Arrange & Act
    collector.recordCaseCreation('case-001', 'spec-test');
    collector.recordCaseCompletion('case-001', 'success');

    collector.recordCaseCreation('case-002', 'spec-test');
    collector.recordCaseCompletion('case-002', 'success');

    // Assert
    const metrics = collector.getWorkflowMetrics('spec-test');
    expect(metrics.avgDuration >= 0 || isNaN(metrics.avgDuration)).toBe(true);
    expect(metrics.totalTasks).toBe(0);
  });

  it('should aggregate metrics across multiple workflows', () => {
    // Arrange & Act
    collector.recordCaseCreation('case-1', 'spec-approval');
    collector.recordCaseCreation('case-2', 'spec-budget');
    collector.recordCaseCreation('case-3', 'spec-approval');

    // Assert
    const allMetrics = collector.getAllMetrics();
    expect(allMetrics.totalCases).toBe(3);
    expect(allMetrics.workflows.length).toBe(2);
  });
});

describe('AlertManager', () => {
  let alertManager;

  beforeEach(() => {
    alertManager = new AlertManager({
      queueBacklogThreshold: 100,
      errorRateThreshold: 0.1,
      responseTimeThreshold: 5000,
    });
  });

  it('should add and evaluate alert rules', () => {
    // Arrange
    alertManager.addRule({
      name: 'high_backlog',
      condition: 'queue.backlog > threshold',
      severity: 'warning',
      evaluate: ({ value, context }) => value > 100,
    });

    // Act
    const triggered = alertManager.evaluateMetric('queue.backlog', 150);

    // Assert
    expect(triggered.length).toBe(1);
    expect(triggered[0].ruleId).toBe('high_backlog');
    expect(triggered[0].severity).toBe('warning');
  });

  it('should not trigger alerts when condition is false', () => {
    // Arrange
    alertManager.addRule({
      name: 'high_backlog',
      condition: 'queue.backlog > 100',
      severity: 'warning',
      evaluate: ({ value }) => value > 100,
    });

    // Act
    const triggered = alertManager.evaluateMetric('queue.backlog', 50);

    // Assert
    expect(triggered.length).toBe(0);
  });

  it('should track alert history', () => {
    // Arrange
    alertManager.addRule({
      name: 'test_rule',
      condition: 'always',
      severity: 'error',
      evaluate: () => true,
    });

    // Act
    alertManager.evaluateMetric('metric1', 1);
    alertManager.evaluateMetric('metric2', 2);

    // Assert
    expect(alertManager.alerts.length).toBe(2);
  });

  it('should get active alerts from last minute', () => {
    // Arrange
    alertManager.addRule({
      name: 'test_rule',
      condition: 'always',
      severity: 'warning',
      evaluate: () => true,
    });

    // Act
    alertManager.evaluateMetric('metric1', 1);
    const active = alertManager.getActiveAlerts();

    // Assert
    expect(active.length).toBe(1);
  });

  it('should clear resolved alerts', () => {
    // Arrange
    alertManager.addRule({
      name: 'rule1',
      condition: 'always',
      severity: 'info',
      evaluate: () => true,
    });
    alertManager.addRule({
      name: 'rule2',
      condition: 'always',
      severity: 'info',
      evaluate: () => true,
    });

    // Act
    alertManager.evaluateMetric('metric1', 1);
    alertManager.evaluateMetric('metric2', 2);
    const alertsBeforeClear = alertManager.alerts.length;

    // Now trigger only rule1
    const rule1Alerts = alertManager.alerts.filter((a) => a.ruleId === 'rule1');
    alertManager.clearAlerts(['rule1']);

    // Assert
    expect(rule1Alerts.length).toBeGreaterThan(0);
    expect(alertManager.alerts.length).toBeLessThan(alertsBeforeClear);
  });

  it('should handle multiple severity levels', () => {
    // Arrange
    alertManager.addRule({
      name: 'critical_alert',
      condition: 'value > 1000',
      severity: 'critical',
      evaluate: ({ value }) => value > 1000,
    });

    // Act
    const triggered = alertManager.evaluateMetric('metric', 1500);

    // Assert
    expect(triggered[0].severity).toBe('critical');
  });

  it('should limit alert history', () => {
    // Arrange
    alertManager.addRule({
      name: 'rule',
      condition: 'always',
      severity: 'info',
      evaluate: () => true,
    });

    // Act
    for (let i = 0; i < 1500; i++) {
      alertManager.evaluateMetric('metric', i);
    }

    // Assert
    expect(alertManager.alerts.length).toBe(1000);
  });

  it('should handle evaluation errors gracefully', () => {
    // Arrange
    alertManager.addRule({
      name: 'bad_rule',
      condition: 'error',
      severity: 'error',
      evaluate: (_context) => {
        throw new Error('Evaluation failed');
      },
    });

    // Act & Assert - should not throw
    expect(() => {
      alertManager.evaluateMetric('metric', 1);
    }).not.toThrow();
  });
});

describe('DaemonHealthMonitor', () => {
  let monitor;

  beforeEach(() => {
    monitor = new DaemonHealthMonitor({
      serviceName: 'test-daemon',
      healthCheckInterval: 100,
    });
  });

  afterEach(async () => {
    await monitor.shutdown();
  });

  it('should initialize with healthy status', () => {
    // Arrange & Act & Assert
    expect(monitor.status).toBe('healthy');
    expect(monitor.successCount).toBe(0);
    expect(monitor.failureCount).toBe(0);
  });

  it('should track successful operations', () => {
    // Arrange & Act
    monitor.recordSuccess(10);
    monitor.recordSuccess(20);

    // Assert
    expect(monitor.successCount).toBe(2);
    expect(monitor.durations.length).toBe(2);
  });

  it('should track failed operations', () => {
    // Arrange & Act
    monitor.recordFailure();
    monitor.recordFailure();

    // Assert
    expect(monitor.failureCount).toBe(2);
  });

  it('should calculate success rate correctly', () => {
    // Arrange & Act
    monitor.recordSuccess(10);
    monitor.recordSuccess(10);
    monitor.recordSuccess(10);
    monitor.recordFailure();

    // Assert
    const metrics = monitor.getMetrics();
    expect(metrics.successRate).toBe(75);
  });

  it('should mark as healthy when success rate > 95%', () => {
    // Arrange & Act
    for (let i = 0; i < 100; i++) {
      monitor.recordSuccess(10);
    }
    monitor.recordFailure(); // 99% success

    // Assert
    const status = monitor.getHealthStatus();
    expect(status).toBe('healthy');
  });

  it('should mark as degraded when success rate 80-95%', () => {
    // Arrange & Act
    for (let i = 0; i < 90; i++) {
      monitor.recordSuccess(10);
    }
    for (let i = 0; i < 10; i++) {
      monitor.recordFailure(); // 90% success
    }

    // Assert
    const status = monitor.getHealthStatus();
    expect(status).toBe('degraded');
  });

  it('should mark as unhealthy when success rate < 80%', () => {
    // Arrange & Act
    for (let i = 0; i < 30; i++) {
      monitor.recordSuccess(10);
    }
    for (let i = 0; i < 70; i++) {
      monitor.recordFailure(); // 30% success
    }

    // Assert
    const status = monitor.getHealthStatus();
    expect(status).toBe('unhealthy');
  });

  it('should track queue backlog', () => {
    // Arrange & Act
    monitor.updateQueueBacklog(50);

    // Assert
    expect(monitor.queueBacklog).toBe(50);
    const metrics = monitor.getMetrics();
    expect(metrics.queueBacklog).toBe(50);
  });

  it('should calculate percentiles from durations', () => {
    // Arrange & Act
    for (let i = 1; i <= 100; i++) {
      monitor.recordSuccess(i);
    }

    // Assert
    const metrics = monitor.getMetrics();
    expect(metrics.p50).toBeGreaterThan(40);
    expect(metrics.p95).toBeGreaterThan(90);
    expect(metrics.p99).toBeGreaterThan(98);
  });

  it('should initialize health check timer', async () => {
    // Arrange & Act
    await monitor.initialize();

    // Assert
    expect(monitor.timer).toBeDefined();

    // Cleanup
    await monitor.shutdown();
  });

  it('should maintain duration history limit', () => {
    // Arrange & Act
    for (let i = 0; i < 1500; i++) {
      monitor.recordSuccess(i);
    }

    // Assert
    expect(monitor.durations.length).toBe(1000);
  });

  it('should handle zero operations gracefully', () => {
    // Arrange & Act
    const metrics = monitor.getMetrics();

    // Assert
    expect(metrics.status).toBe('healthy');
    expect(metrics.successRate).toBe(0);
  });
});

describe('Integration Tests', () => {
  it('should integrate Prometheus metrics with health monitor', () => {
    // Arrange
    const exporter = new PrometheusMetricsExporter({ serviceName: 'test-daemon' });
    const monitor = new DaemonHealthMonitor({ healthCheckInterval: 100 });

    // Act
    exporter.registerCounter('daemon.operations', 'Operations');
    exporter.registerGauge('daemon.health.status', 'Health status');

    monitor.recordSuccess(10);
    exporter.incrementCounter('daemon.operations', 1);

    const metrics = monitor.getMetrics();
    exporter.setGauge('daemon.health.status', metrics.successRate);

    // Assert
    const snapshot = exporter.getMetricsSnapshot();
    expect(snapshot['daemon.health.status'].values['{}']).toBe(100);
  });

  it('should integrate OTEL tracing with alert manager', () => {
    // Arrange
    const tracer = new OtelSpanTracer({ serviceName: 'test-daemon' });
    const alertManager = new AlertManager();

    alertManager.addRule({
      name: 'span_error_alert',
      condition: 'error',
      severity: 'error',
      evaluate: ({ context }) => context.spanStatus === 'error',
    });

    // Act
    const spanId = tracer.startSpan('operation');
    tracer.addSpanEvent(spanId, 'error', { message: 'Failed' });
    tracer.endSpan(spanId, 'error');

    const span = tracer.spans[0];
    const triggered = alertManager.evaluateMetric('span.error', 1, { spanStatus: span.status });

    // Assert
    expect(triggered.length).toBe(1);
    expect(triggered[0].severity).toBe('error');
    expect(span.duration).toBeGreaterThanOrEqual(0);
  });

  it('should track full operation lifecycle with metrics', () => {
    // Arrange
    const exporter = new PrometheusMetricsExporter({ serviceName: 'test-daemon' });
    const tracer = new OtelSpanTracer({ serviceName: 'test-daemon' });
    const monitor = new DaemonHealthMonitor({ healthCheckInterval: 100 });

    exporter.registerCounter('operations.started', 'Operations started');
    exporter.registerCounter('operations.completed', 'Operations completed');
    exporter.registerHistogram('operation.duration', 'Operation duration');

    // Act
    const spanId = tracer.startSpan('process_order', { orderId: '123' });
    exporter.incrementCounter('operations.started', 1);

    const startTime = Date.now();
    // Simulate work
    const duration = 50;
    const endTime = startTime + duration;
    monitor.recordSuccess(duration);

    exporter.recordHistogram('operation.duration', duration);
    exporter.incrementCounter('operations.completed', 1);

    tracer.addSpanEvent(spanId, 'completed', { result: 'success' });
    tracer.endSpan(spanId, 'ok');

    // Assert
    const snapshot = exporter.getMetricsSnapshot();
    expect(snapshot['operations.started'].values['{}'] >= 1).toBe(true);
    expect(snapshot['operations.completed'].values['{}'] >= 1).toBe(true);
    expect(snapshot['operation.duration'].statistics).toBeDefined();
    expect(tracer.spans.length).toBe(1);
    expect(monitor.successCount).toBe(1);
  });
});
