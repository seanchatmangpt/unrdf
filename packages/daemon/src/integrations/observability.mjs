/**
 * @file Daemon Observability Integration - OTEL metrics and health monitoring
 * @module @unrdf/daemon/integrations/observability
 * @description Comprehensive observability metrics including Prometheus export, YAWL metrics, OTEL spans, and alert management
 */

import { z } from 'zod';

const ConfigSchema = z.object({
  serviceName: z.string().default('unrdf-daemon'),
  queueBacklogThreshold: z.number().default(100),
  healthCheckInterval: z.number().default(10000),
  prometheusPort: z.number().optional(),
  alertManagerUrl: z.string().optional(),
  enableYawlMetrics: z.boolean().default(true),
  enableSpanTracing: z.boolean().default(true),
  metricsHistorySize: z.number().default(10000),
});

/**
 * Prometheus metrics exporter for daemon operations
 * Tracks all daemon operations and exports in Prometheus format
 */
export class PrometheusMetricsExporter {
  /**
   * Create Prometheus metrics exporter
   * @param {Object} config - Exporter configuration
   * @param {string} config.serviceName - Service name for metrics
   * @param {number} [config.metricsHistorySize=10000] - Max history size
   */
  constructor(config = {}) {
    this.config = ConfigSchema.parse(config);
    this.metrics = new Map();
    this.counters = new Map();
    this.gauges = new Map();
    this.histograms = new Map();
  }

  /**
   * Register a counter metric
   * @param {string} name - Metric name
   * @param {string} help - Help text
   * @param {Object} labels - Label names
   */
  registerCounter(name, help, labels = []) {
    const metric = {
      name,
      help,
      type: 'counter',
      labels,
      values: new Map(),
      total: 0,
    };
    this.counters.set(name, metric);
    this.metrics.set(name, metric);
    return metric;
  }

  /**
   * Register a gauge metric
   * @param {string} name - Metric name
   * @param {string} help - Help text
   * @param {Object} labels - Label names
   */
  registerGauge(name, help, labels = []) {
    const metric = {
      name,
      help,
      type: 'gauge',
      labels,
      values: new Map(),
      current: 0,
    };
    this.gauges.set(name, metric);
    this.metrics.set(name, metric);
    return metric;
  }

  /**
   * Register a histogram metric
   * @param {string} name - Metric name
   * @param {string} help - Help text
   * @param {Object} buckets - Bucket boundaries
   * @param {Object} labels - Label names
   */
  registerHistogram(name, help, buckets = [0.001, 0.01, 0.1, 1, 10], labels = []) {
    const metric = {
      name,
      help,
      type: 'histogram',
      labels,
      buckets,
      values: new Map(),
      observations: [],
    };
    this.histograms.set(name, metric);
    this.metrics.set(name, metric);
    return metric;
  }

  /**
   * Increment counter value
   * @param {string} name - Counter name
   * @param {number} value - Increment value
   * @param {Object} labelValues - Label values
   */
  incrementCounter(name, value = 1, labelValues = {}) {
    const metric = this.counters.get(name);
    if (!metric) return;

    const key = JSON.stringify(labelValues);
    metric.values.set(key, (metric.values.get(key) || 0) + value);
    metric.total += value;
  }

  /**
   * Set gauge value
   * @param {string} name - Gauge name
   * @param {number} value - Gauge value
   * @param {Object} labelValues - Label values
   */
  setGauge(name, value, labelValues = {}) {
    const metric = this.gauges.get(name);
    if (!metric) return;

    const key = JSON.stringify(labelValues);
    metric.values.set(key, value);
    metric.current = value;
  }

  /**
   * Record histogram observation
   * @param {string} name - Histogram name
   * @param {number} value - Observation value
   * @param {Object} labelValues - Label values
   */
  recordHistogram(name, value, labelValues = {}) {
    const metric = this.histograms.get(name);
    if (!metric) return;

    const key = JSON.stringify(labelValues);
    if (!metric.values.has(key)) {
      metric.values.set(key, { count: 0, sum: 0, buckets: new Map() });
    }

    const entry = metric.values.get(key);
    entry.count += 1;
    entry.sum += value;

    metric.buckets.forEach((bucket) => {
      if (value <= bucket) {
        entry.buckets.set(bucket, (entry.buckets.get(bucket) || 0) + 1);
      }
    });

    metric.observations.push(value);
    if (metric.observations.length > this.config.metricsHistorySize) {
      metric.observations.shift();
    }
  }

  /**
   * Export metrics in Prometheus text format
   * @returns {string} Prometheus text format
   */
  export() {
    let output = '';

    for (const [name, metric] of this.metrics) {
      output += `# HELP ${name} ${metric.help}\n`;
      output += `# TYPE ${name} ${metric.type}\n`;

      if (metric.type === 'counter') {
        for (const [labelKey, value] of metric.values) {
          const labels = labelKey !== '{}' ? labelKey : '';
          output += `${name}${labels} ${value}\n`;
        }
      } else if (metric.type === 'gauge') {
        for (const [labelKey, value] of metric.values) {
          const labels = labelKey !== '{}' ? labelKey : '';
          output += `${name}${labels} ${value}\n`;
        }
      } else if (metric.type === 'histogram') {
        for (const [labelKey, entry] of metric.values) {
          const labels = labelKey !== '{}' ? labelKey : '';
          for (const [bucket, count] of entry.buckets) {
            output += `${name}_bucket{le="${bucket}"${labels}} ${count}\n`;
          }
          output += `${name}_bucket{le="+Inf"${labels}} ${entry.count}\n`;
          output += `${name}_sum${labels} ${entry.sum}\n`;
          output += `${name}_count${labels} ${entry.count}\n`;
        }
      }
    }

    return output;
  }

  /**
   * Get current metrics as JSON object
   * @returns {Object} Metrics snapshot
   */
  getMetricsSnapshot() {
    const snapshot = {};

    for (const [name, metric] of this.metrics) {
      snapshot[name] = {
        type: metric.type,
        help: metric.help,
        values: Object.fromEntries(metric.values),
      };

      if (metric.type === 'histogram' && metric.observations.length > 0) {
        const sorted = [...metric.observations].sort((a, b) => a - b);
        snapshot[name].statistics = {
          count: sorted.length,
          sum: sorted.reduce((a, b) => a + b, 0),
          mean: sorted.reduce((a, b) => a + b, 0) / sorted.length,
          min: sorted[0],
          max: sorted[sorted.length - 1],
          p50: sorted[Math.floor(sorted.length * 0.5)],
          p95: sorted[Math.floor(sorted.length * 0.95)],
          p99: sorted[Math.floor(sorted.length * 0.99)],
        };
      }
    }

    return snapshot;
  }
}

/**
 * OpenTelemetry span tracer for daemon operations
 * Tracks operation spans for distributed tracing
 */
export class OtelSpanTracer {
  /**
   * Create OTEL span tracer
   * @param {Object} config - Tracer configuration
   * @param {string} config.serviceName - Service name
   */
  constructor(config = {}) {
    this.config = ConfigSchema.parse(config);
    this.spans = [];
    this.activeSpans = new Map();
  }

  /**
   * Start a new span
   * @param {string} name - Span name
   * @param {Object} attributes - Span attributes
   * @returns {string} Span ID
   */
  startSpan(name, attributes = {}) {
    const spanId = `span-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
    const span = {
      id: spanId,
      name,
      startTime: Date.now(),
      attributes,
      events: [],
      status: 'unset',
    };

    this.activeSpans.set(spanId, span);
    return spanId;
  }

  /**
   * Add event to span
   * @param {string} spanId - Span ID
   * @param {string} eventName - Event name
   * @param {Object} attributes - Event attributes
   */
  addSpanEvent(spanId, eventName, attributes = {}) {
    const span = this.activeSpans.get(spanId);
    if (span) {
      span.events.push({
        name: eventName,
        timestamp: Date.now(),
        attributes,
      });
    }
  }

  /**
   * End span
   * @param {string} spanId - Span ID
   * @param {string} status - Span status (ok, error)
   */
  endSpan(spanId, status = 'ok') {
    const span = this.activeSpans.get(spanId);
    if (span) {
      span.endTime = Date.now();
      span.duration = span.endTime - span.startTime;
      span.status = status;
      this.spans.push(span);
      this.activeSpans.delete(spanId);

      if (this.spans.length > 10000) {
        this.spans.shift();
      }
    }
  }

  /**
   * Get all completed spans
   * @returns {Array} Array of spans
   */
  getSpans() {
    return this.spans;
  }

  /**
   * Export spans as traces
   * @returns {Object} Trace data
   */
  exportTraces() {
    const traces = {};

    for (const span of this.spans) {
      if (!traces[span.name]) {
        traces[span.name] = [];
      }

      traces[span.name].push({
        spanId: span.id,
        duration: span.duration,
        status: span.status,
        attributes: span.attributes,
        events: span.events,
      });
    }

    return traces;
  }
}

/**
 * YAWL workflow metrics collector
 * Tracks workflow execution and case management metrics
 */
export class YawlMetricsCollector {
  /**
   * Create YAWL metrics collector
   * @param {Object} config - Collector configuration
   */
  constructor(config = {}) {
    this.config = ConfigSchema.parse(config);
    this.caseMetrics = new Map();
    this.taskMetrics = new Map();
    this.workflowMetrics = new Map();
    this.history = [];
  }

  /**
   * Record case creation
   * @param {string} caseId - Case ID
   * @param {string} specId - Specification ID
   * @param {Object} metadata - Case metadata
   */
  recordCaseCreation(caseId, specId, metadata = {}) {
    const caseMetric = {
      caseId,
      specId,
      createdAt: Date.now(),
      metadata,
      tasks: [],
      status: 'created',
      duration: null,
    };

    this.caseMetrics.set(caseId, caseMetric);
    this.history.push({ type: 'case_created', caseId, timestamp: Date.now() });
  }

  /**
   * Record task execution
   * @param {string} caseId - Case ID
   * @param {string} taskId - Task ID
   * @param {Object} metadata - Task metadata
   */
  recordTaskExecution(caseId, taskId, metadata = {}) {
    const caseMetric = this.caseMetrics.get(caseId);
    if (!caseMetric) return;

    const taskMetric = {
      taskId,
      startTime: Date.now(),
      metadata,
      status: 'executing',
      duration: null,
      retries: 0,
    };

    caseMetric.tasks.push(taskMetric);
    this.taskMetrics.set(taskId, taskMetric);
    this.history.push({ type: 'task_started', caseId, taskId, timestamp: Date.now() });
  }

  /**
   * Record task completion
   * @param {string} taskId - Task ID
   * @param {string} result - Task result (success, failure, timeout)
   * @param {Object} metadata - Completion metadata
   */
  recordTaskCompletion(taskId, result, metadata = {}) {
    const taskMetric = this.taskMetrics.get(taskId);
    if (!taskMetric) return;

    taskMetric.endTime = Date.now();
    taskMetric.duration = taskMetric.endTime - taskMetric.startTime;
    taskMetric.status = result;
    taskMetric.metadata = { ...taskMetric.metadata, ...metadata };

    this.history.push({ type: 'task_completed', taskId, result, timestamp: Date.now() });
  }

  /**
   * Record task retry
   * @param {string} taskId - Task ID
   * @param {Object} _metadata - Retry metadata (reserved for future use)
   */
  recordTaskRetry(taskId, _metadata = {}) {
    const taskMetric = this.taskMetrics.get(taskId);
    if (!taskMetric) return;

    taskMetric.retries += 1;
    this.history.push({ type: 'task_retried', taskId, retries: taskMetric.retries, timestamp: Date.now() });
  }

  /**
   * Record case completion
   * @param {string} caseId - Case ID
   * @param {string} result - Case result (success, failure)
   * @param {Object} metadata - Completion metadata
   */
  recordCaseCompletion(caseId, result, metadata = {}) {
    const caseMetric = this.caseMetrics.get(caseId);
    if (!caseMetric) return;

    caseMetric.endTime = Date.now();
    caseMetric.duration = caseMetric.endTime - caseMetric.createdAt;
    caseMetric.status = result;
    caseMetric.metadata = { ...caseMetric.metadata, ...metadata };

    this.history.push({ type: 'case_completed', caseId, result, timestamp: Date.now() });
  }

  /**
   * Get workflow metrics
   * @param {string} specId - Specification ID
   * @returns {Object} Workflow metrics
   */
  getWorkflowMetrics(specId) {
    const casesForSpec = Array.from(this.caseMetrics.values()).filter((c) => c.specId === specId);

    const completedCases = casesForSpec.filter(
      (c) => c.status && (c.status === 'success' || c.status === 'completed')
    ).length;
    const failedCases = casesForSpec.filter(
      (c) => c.status && (c.status === 'failure' || c.status === 'failed' || c.status === 'error')
    ).length;
    const casesWithDuration = casesForSpec.filter((c) => c.duration !== null && c.duration !== undefined);
    const avgDuration =
      casesWithDuration.length > 0
        ? casesWithDuration.reduce((sum, c) => sum + c.duration, 0) / casesWithDuration.length
        : 0;

    return {
      specId,
      totalCases: casesForSpec.length,
      completedCases,
      failedCases,
      avgDuration,
      totalTasks: casesForSpec.reduce((sum, c) => sum + c.tasks.length, 0),
    };
  }

  /**
   * Get case metrics
   * @param {string} caseId - Case ID
   * @returns {Object} Case metrics
   */
  getCaseMetrics(caseId) {
    return this.caseMetrics.get(caseId);
  }

  /**
   * Get all metrics
   * @returns {Object} All YAWL metrics
   */
  getAllMetrics() {
    const specIds = new Set(Array.from(this.caseMetrics.values()).map((c) => c.specId));

    return {
      totalCases: this.caseMetrics.size,
      workflows: Array.from(specIds).map((specId) => this.getWorkflowMetrics(specId)),
      recentHistory: this.history.slice(-100),
    };
  }
}

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
 * Alert manager for daemon with threshold-based triggering
 */
export class AlertManager {
  /**
   * Create alert manager
   * @param {Object} config - Alert configuration
   * @param {number} [config.queueBacklogThreshold=100] - Queue backlog alert threshold
   * @param {number} [config.errorRateThreshold=0.1] - Error rate alert threshold (0-1)
   * @param {number} [config.responseTimeThreshold=5000] - Response time threshold (ms)
   */
  constructor(config = {}) {
    this.config = {
      queueBacklogThreshold: config.queueBacklogThreshold || 100,
      errorRateThreshold: config.errorRateThreshold || 0.1,
      responseTimeThreshold: config.responseTimeThreshold || 5000,
    };

    this.alerts = [];
    this.rules = [];
    this.metrics = {};
  }

  /**
   * Add alert rule
   * @param {Object} rule - Alert rule
   * @param {string} rule.name - Rule name
   * @param {string} rule.condition - Condition to evaluate
   * @param {string} rule.severity - Alert severity (info, warning, error, critical)
   * @param {Function} rule.evaluate - Evaluation function
   */
  addRule(rule) {
    const validated = z
      .object({
        name: z.string(),
        condition: z.string(),
        severity: z.enum(['info', 'warning', 'error', 'critical']),
        evaluate: z.function(),
      })
      .parse(rule);

    this.rules.push(validated);
  }

  /**
   * Evaluate metric against rules
   * @param {string} metric - Metric name
   * @param {number} value - Metric value
   * @param {Object} context - Evaluation context
   * @returns {Array} Triggered alerts
   */
  evaluateMetric(metric, value, context = {}) {
    const triggered = [];

    for (const rule of this.rules) {
      try {
        if (rule.evaluate({ metric, value, context })) {
          const alert = {
            ruleId: rule.name,
            severity: rule.severity,
            metric,
            value,
            context,
            timestamp: Date.now(),
            message: `${rule.name}: ${metric}=${value}`,
          };

          this.alerts.push(alert);
          triggered.push(alert);

          if (this.alerts.length > 1000) {
            this.alerts.shift();
          }
        }
      } catch (error) {
        // Silently ignore evaluation errors
      }
    }

    return triggered;
  }

  /**
   * Get active alerts
   * @returns {Array} Active alerts from last minute
   */
  getActiveAlerts() {
    const oneMinuteAgo = Date.now() - 60000;
    return this.alerts.filter((a) => a.timestamp > oneMinuteAgo);
  }

  /**
   * Clear resolved alerts
   * @param {Array} resolvedAlertIds - Alert IDs to resolve
   */
  clearAlerts(resolvedAlertIds = []) {
    this.alerts = this.alerts.filter((a) => !resolvedAlertIds.includes(a.ruleId));
  }
}

/**
 * Integrate alert manager with daemon
 * @param {Object} daemon - Daemon instance
 * @param {Object} alertManager - Alert manager instance
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
   * Create health monitor
   * @param {Object} config - Health monitor configuration
   * @param {string} [config.serviceName='unrdf-daemon'] - Service name
   * @param {number} [config.queueBacklogThreshold=100] - Queue backlog threshold
   * @param {number} [config.healthCheckInterval=10000] - Health check interval (ms)
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
   * Initialize periodic health checks
   * @returns {Promise<void>}
   */
  async initialize() {
    this.timer = setInterval(() => this._check(), this.config.healthCheckInterval);
  }

  /**
   * Record successful operation
   * @param {number} duration - Operation duration (ms)
   */
  recordSuccess(duration) {
    this.successCount++;
    this.durations.push(duration);
    if (this.durations.length > 1000) this.durations.shift();
  }

  /**
   * Record failed operation
   */
  recordFailure() {
    this.failureCount++;
  }

  /**
   * Update queue backlog size
   * @param {number} backlog - Queue backlog size
   */
  updateQueueBacklog(backlog) {
    this.queueBacklog = backlog;
  }

  /**
   * Get current health status
   * @returns {string} Health status (healthy, degraded, unhealthy)
   */
  getHealthStatus() {
    const total = this.successCount + this.failureCount;
    if (total === 0) return 'healthy';
    const rate = this.successCount / total;
    return rate > 0.95 ? 'healthy' : rate >= 0.8 ? 'degraded' : 'unhealthy';
  }

  /**
   * Get current metrics snapshot
   * @returns {Object} Health metrics including status, rates, and percentiles
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
   * Perform health check and update status
   * @private
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
   * Shutdown health monitor and clear interval
   * @returns {Promise<void>}
   */
  async shutdown() {
    if (this.timer) clearInterval(this.timer);
  }
}
