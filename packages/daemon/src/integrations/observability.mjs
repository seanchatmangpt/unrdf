/**
 * @file Daemon Observability Integration - OTEL metrics and health monitoring
 * @module @unrdf/daemon/integrations/observability
 */

import { z } from 'zod';

const ConfigSchema = z.object({
  serviceName: z.string().default('unrdf-daemon'),
  queueBacklogThreshold: z.number().default(100),
  healthCheckInterval: z.number().default(10000),
});

/**
 * Integrate OpenTelemetry metrics with daemon
 * @param {Object} daemon - Daemon instance
 * @param {Object} meter - OpenTelemetry meter
 * @returns {Object} Metrics accessor
 */
export function integrateOTelMetrics(daemon, meter) {
  if (!meter) {
    return {
      recordOperationScheduled: () => {},
      recordOperationExecuted: () => {},
      recordOperationFailed: () => {},
      recordQueueBacklog: () => {},
      recordHealthyNodes: () => {},
    };
  }

  const counters = {
    scheduled: meter.createCounter('daemon.operations.scheduled', { description: 'Operations scheduled' }),
    executed: meter.createCounter('daemon.operations.executed', { description: 'Operations executed' }),
    failed: meter.createCounter('daemon.operations.failed', { description: 'Operations failed' }),
  };

  const duration = meter.createHistogram('daemon.operation.duration', {
    description: 'Operation duration (ms)',
    unit: 'ms',
  });

  const gauges = {
    backlog: meter.createUpDownCounter('daemon.queue.backlog', { description: 'Queue backlog size' }),
    pending: meter.createUpDownCounter('daemon.operations.pending', { description: 'Pending operations' }),
    healthyNodes: meter.createUpDownCounter('daemon.cluster.nodes_healthy', { description: 'Healthy nodes' }),
  };

  return {
    recordOperationScheduled(attrs = {}) { counters.scheduled.add(1, attrs); },
    recordOperationExecuted(dur, attrs = {}) { counters.executed.add(1, attrs); duration.record(dur, attrs); },
    recordOperationFailed(reason, attrs = {}) { counters.failed.add(1, { reason, ...attrs }); },
    recordQueueBacklog(size) { gauges.backlog.add(size); },
    recordPending(count) { gauges.pending.add(count); },
    recordHealthyNodes(count) { gauges.healthyNodes.add(count); },
  };
}

/**
 * Integrate alert manager with daemon
 * @param {Object} daemon - Daemon instance
 * @param {Object} alertManager - Alert manager with evaluateMetric method
 * @returns {Object} Alert handler
 */
export function integrateAlertManager(daemon, alertManager) {
  const noop = { evaluate: () => {}, onOperationFailure: () => {}, onQueueBacklogHigh: () => {}, onHealthDegraded: () => {} };
  if (!alertManager) return noop;

  const evaluate = (metric, value, context = {}) => {
    if (alertManager?.evaluateMetric) alertManager.evaluateMetric(metric, value, context);
  };

  return {
    evaluate,
    onOperationFailure(reason, ctx = {}) {
      evaluate('daemon.operation.failed', 1, { reason, severity: 'error', ...ctx });
    },
    onQueueBacklogHigh(backlog, threshold) {
      if (backlog > threshold) evaluate('daemon.queue.backlog.high', backlog, { threshold, severity: 'warning' });
    },
    onHealthDegraded(status) {
      evaluate('daemon.health.degraded', 1, { status, severity: status === 'unhealthy' ? 'critical' : 'warning' });
    },
  };
}

/**
 * Health monitor for daemon operations
 * Tracks success rates and health status (healthy >95%, degraded 80-95%, unhealthy <80%)
 */
export class DaemonHealthMonitor {
  /**
   *
   */
  constructor(config = {}) {
    this.config = ConfigSchema.parse(config);
    this.successCount = 0;
    this.failureCount = 0;
    this.queueBacklog = 0;
    this.durations = [];
    this.status = 'healthy';
    this.lastCheck = Date.now();
  }

  /**
   *
   */
  async initialize() {
    this.timer = setInterval(() => this._check(), this.config.healthCheckInterval);
  }

  /**
   *
   */
  recordSuccess(duration) {
    this.successCount++;
    this.durations.push(duration);
    if (this.durations.length > 1000) this.durations.shift();
  }

  /**
   *
   */
  recordFailure() {
    this.failureCount++;
  }

  /**
   *
   */
  updateQueueBacklog(backlog) {
    this.queueBacklog = backlog;
  }

  /**
   *
   */
  getHealthStatus() {
    const total = this.successCount + this.failureCount;
    if (total === 0) return 'healthy';
    const rate = this.successCount / total;
    return rate > 0.95 ? 'healthy' : rate >= 0.8 ? 'degraded' : 'unhealthy';
  }

  /**
   *
   */
  getMetrics() {
    const total = this.successCount + this.failureCount;
    const rate = total > 0 ? (this.successCount / total) * 100 : 0;
    const avg = this.durations.length > 0
      ? this.durations.reduce((a, b) => a + b, 0) / this.durations.length : 0;

    const sorted = [...this.durations].sort((a, b) => a - b);
    const pct = (p) => sorted[Math.floor(sorted.length * p)] || 0;

    return {
      status: this.getHealthStatus(),
      successCount: this.successCount,
      failureCount: this.failureCount,
      successRate: Math.round(rate * 100) / 100,
      avgDuration: Math.round(avg * 100) / 100,
      p50: pct(0.5),
      p95: pct(0.95),
      p99: pct(0.99),
      queueBacklog: this.queueBacklog,
      lastCheck: this.lastCheck,
    };
  }

  /**
   *
   */
  _check() {
    const newStatus = this.getHealthStatus();
    if (newStatus !== this.status) {
      this.status = newStatus;
      console.log(`[DaemonHealthMonitor] Health: ${newStatus}`);
    }
    this.lastCheck = Date.now();
  }

  /**
   *
   */
  async shutdown() {
    if (this.timer) clearInterval(this.timer);
  }
}
