/**
 * @file Daemon Observability Integration - OTEL metrics and health monitoring
 * @module @unrdf/daemon/integrations/observability
 * @description Comprehensive observability metrics including Prometheus export, YAWL metrics, OTEL spans, and alert management
 */

import { ConfigSchema } from './observability.schema.mjs';


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

  /** Get all completed spans */
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

// Re-export from monitors
export { YawlMetricsCollector, AlertManager, integrateAlertManager, DaemonHealthMonitor } from './observability.monitors.mjs';
export { ConfigSchema } from './observability.schema.mjs';
