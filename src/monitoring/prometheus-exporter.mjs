/**
 * @fileoverview Prometheus Exporter - Export metrics in Prometheus format
 * @module monitoring/prometheus-exporter
 *
 * @description
 * Export metrics in Prometheus text format:
 * - Counter, Gauge, Histogram, Summary metrics
 * - HTTP endpoint for scraping
 * - Automatic metric registration
 * - Compatible with Prometheus 2.0+
 */

import { createServer } from 'node:http';

/**
 * Prometheus metric types
 */
const MetricType = {
  COUNTER: 'counter',
  GAUGE: 'gauge',
  HISTOGRAM: 'histogram',
  SUMMARY: 'summary',
};

/**
 * Prometheus exporter
 */
export class PrometheusExporter {
  /**
   * Create a new Prometheus exporter
   * @param {object} metricsCollector - MetricsCollector instance
   * @param {object} options - Configuration options
   * @param {number} options.port - HTTP port (default: 9090)
   * @param {string} options.host - HTTP host (default: '0.0.0.0')
   * @param {string} options.path - Metrics path (default: '/metrics')
   * @param {string} options.namespace - Metric namespace (default: 'unrdf')
   */
  constructor(metricsCollector, options = {}) {
    this.metrics = metricsCollector;
    this.port = options.port || 9090;
    this.host = options.host || '0.0.0.0';
    this.path = options.path || '/metrics';
    this.namespace = options.namespace || 'unrdf';

    /** @type {import('node:http').Server|null} */
    this.server = null;

    /** @type {Map<string, object>} */
    this.customMetrics = new Map();
  }

  /**
   * Start the exporter
   */
  start() {
    if (this.server !== null) return;

    this.server = createServer((req, res) => {
      if (req.url === this.path && req.method === 'GET') {
        const output = this.generatePrometheusOutput();

        res.writeHead(200, {
          'Content-Type': 'text/plain; version=0.0.4',
        });
        res.end(output);
      } else {
        res.writeHead(404);
        res.end('Not Found');
      }
    });

    this.server.listen(this.port, this.host, () => {
      console.log(`[PrometheusExporter] Listening on http://${this.host}:${this.port}${this.path}`);
    });
  }

  /**
   * Stop the exporter
   */
  stop() {
    if (this.server !== null) {
      this.server.close();
      this.server = null;
      console.log('[PrometheusExporter] Stopped');
    }
  }

  /**
   * Generate Prometheus output
   * @returns {string}
   */
  generatePrometheusOutput() {
    const snapshot = this.metrics.getCurrentSnapshot();
    if (!snapshot) {
      return '';
    }

    const lines = [];

    // Add header
    lines.push('# HELP System and performance metrics for UNRDF');
    lines.push('');

    // CPU metrics
    lines.push(`# HELP ${this.namespace}_cpu_user_microseconds CPU user time in microseconds`);
    lines.push(`# TYPE ${this.namespace}_cpu_user_microseconds counter`);
    lines.push(`${this.namespace}_cpu_user_microseconds ${snapshot.cpu.user}`);
    lines.push('');

    lines.push(`# HELP ${this.namespace}_cpu_system_microseconds CPU system time in microseconds`);
    lines.push(`# TYPE ${this.namespace}_cpu_system_microseconds counter`);
    lines.push(`${this.namespace}_cpu_system_microseconds ${snapshot.cpu.system}`);
    lines.push('');

    lines.push(`# HELP ${this.namespace}_cpu_percentage CPU usage percentage`);
    lines.push(`# TYPE ${this.namespace}_cpu_percentage gauge`);
    lines.push(`${this.namespace}_cpu_percentage ${(snapshot.cpu.percentageUser + snapshot.cpu.percentageSystem).toFixed(2)}`);
    lines.push('');

    // Memory metrics
    lines.push(`# HELP ${this.namespace}_memory_rss_bytes Resident set size in bytes`);
    lines.push(`# TYPE ${this.namespace}_memory_rss_bytes gauge`);
    lines.push(`${this.namespace}_memory_rss_bytes ${snapshot.memory.rss}`);
    lines.push('');

    lines.push(`# HELP ${this.namespace}_memory_heap_total_bytes Heap total in bytes`);
    lines.push(`# TYPE ${this.namespace}_memory_heap_total_bytes gauge`);
    lines.push(`${this.namespace}_memory_heap_total_bytes ${snapshot.memory.heapTotal}`);
    lines.push('');

    lines.push(`# HELP ${this.namespace}_memory_heap_used_bytes Heap used in bytes`);
    lines.push(`# TYPE ${this.namespace}_memory_heap_used_bytes gauge`);
    lines.push(`${this.namespace}_memory_heap_used_bytes ${snapshot.memory.heapUsed}`);
    lines.push('');

    // Throughput metrics
    lines.push(`# HELP ${this.namespace}_operations_total Total operations completed`);
    lines.push(`# TYPE ${this.namespace}_operations_total counter`);
    lines.push(`${this.namespace}_operations_total ${snapshot.throughput.total}`);
    lines.push('');

    lines.push(`# HELP ${this.namespace}_throughput_ops_per_sec Operations per second`);
    lines.push(`# TYPE ${this.namespace}_throughput_ops_per_sec gauge`);
    lines.push(`${this.namespace}_throughput_ops_per_sec ${snapshot.throughput.opsPerSec.toFixed(2)}`);
    lines.push('');

    // Latency metrics (as histogram)
    lines.push(`# HELP ${this.namespace}_latency_milliseconds Operation latency distribution`);
    lines.push(`# TYPE ${this.namespace}_latency_milliseconds histogram`);
    lines.push(`${this.namespace}_latency_milliseconds{quantile="0.5"} ${snapshot.latency.p50.toFixed(3)}`);
    lines.push(`${this.namespace}_latency_milliseconds{quantile="0.75"} ${snapshot.latency.p75.toFixed(3)}`);
    lines.push(`${this.namespace}_latency_milliseconds{quantile="0.9"} ${snapshot.latency.p90.toFixed(3)}`);
    lines.push(`${this.namespace}_latency_milliseconds{quantile="0.95"} ${snapshot.latency.p95.toFixed(3)}`);
    lines.push(`${this.namespace}_latency_milliseconds{quantile="0.99"} ${snapshot.latency.p99.toFixed(3)}`);
    lines.push(`${this.namespace}_latency_milliseconds{quantile="0.999"} ${snapshot.latency.p999.toFixed(3)}`);
    lines.push(`${this.namespace}_latency_milliseconds_sum ${snapshot.latency.mean * snapshot.latency.count}`);
    lines.push(`${this.namespace}_latency_milliseconds_count ${snapshot.latency.count}`);
    lines.push('');

    // Compression metrics
    lines.push(`# HELP ${this.namespace}_compression_ratio Compression ratio`);
    lines.push(`# TYPE ${this.namespace}_compression_ratio gauge`);
    lines.push(`${this.namespace}_compression_ratio ${snapshot.compression.ratio.toFixed(3)}`);
    lines.push('');

    // Receipt chain metrics
    lines.push(`# HELP ${this.namespace}_receipt_chain_count Total receipts in chain`);
    lines.push(`# TYPE ${this.namespace}_receipt_chain_count counter`);
    lines.push(`${this.namespace}_receipt_chain_count ${snapshot.receiptChain.count}`);
    lines.push('');

    lines.push(`# HELP ${this.namespace}_receipt_growth_rate Receipts per second`);
    lines.push(`# TYPE ${this.namespace}_receipt_growth_rate gauge`);
    lines.push(`${this.namespace}_receipt_growth_rate ${snapshot.receiptChain.growthRate.toFixed(3)}`);
    lines.push('');

    // Agent metrics
    const agentMetrics = this.metrics.getAllAgentMetrics();
    if (agentMetrics.size > 0) {
      lines.push(`# HELP ${this.namespace}_agent_tasks_completed Tasks completed by agent`);
      lines.push(`# TYPE ${this.namespace}_agent_tasks_completed counter`);

      for (const [agentId, metrics] of agentMetrics) {
        lines.push(`${this.namespace}_agent_tasks_completed{agent="${agentId}"} ${metrics.tasksCompleted}`);
      }
      lines.push('');

      lines.push(`# HELP ${this.namespace}_agent_tasks_queued Tasks queued by agent`);
      lines.push(`# TYPE ${this.namespace}_agent_tasks_queued gauge`);

      for (const [agentId, metrics] of agentMetrics) {
        lines.push(`${this.namespace}_agent_tasks_queued{agent="${agentId}"} ${metrics.tasksQueued}`);
      }
      lines.push('');

      lines.push(`# HELP ${this.namespace}_agent_cpu_microseconds CPU time used by agent`);
      lines.push(`# TYPE ${this.namespace}_agent_cpu_microseconds counter`);

      for (const [agentId, metrics] of agentMetrics) {
        lines.push(`${this.namespace}_agent_cpu_microseconds{agent="${agentId}"} ${metrics.cpuTime}`);
      }
      lines.push('');

      lines.push(`# HELP ${this.namespace}_agent_memory_bytes Memory used by agent`);
      lines.push(`# TYPE ${this.namespace}_agent_memory_bytes gauge`);

      for (const [agentId, metrics] of agentMetrics) {
        lines.push(`${this.namespace}_agent_memory_bytes{agent="${agentId}"} ${metrics.memoryUsed}`);
      }
      lines.push('');
    }

    // Custom metrics
    for (const [name, metric] of this.customMetrics) {
      lines.push(`# HELP ${this.namespace}_${name} ${metric.help || name}`);
      lines.push(`# TYPE ${this.namespace}_${name} ${metric.type}`);

      if (metric.labels && metric.labels.length > 0) {
        for (const labelSet of metric.values) {
          const labelStr = Object.entries(labelSet.labels)
            .map(([k, v]) => `${k}="${v}"`)
            .join(',');
          lines.push(`${this.namespace}_${name}{${labelStr}} ${labelSet.value}`);
        }
      } else {
        lines.push(`${this.namespace}_${name} ${metric.value}`);
      }
      lines.push('');
    }

    return lines.join('\n');
  }

  /**
   * Register a custom metric
   * @param {string} name - Metric name
   * @param {string} type - Metric type (counter, gauge, histogram, summary)
   * @param {string} help - Help text
   * @param {*} value - Initial value
   * @param {string[]} labels - Label names
   */
  registerMetric(name, type, help, value = 0, labels = []) {
    this.customMetrics.set(name, {
      type,
      help,
      value,
      labels,
      values: [],
    });
  }

  /**
   * Set metric value
   * @param {string} name - Metric name
   * @param {*} value - Value
   * @param {object} labels - Label values
   */
  setMetric(name, value, labels = {}) {
    const metric = this.customMetrics.get(name);
    if (!metric) return;

    if (Object.keys(labels).length > 0) {
      const existing = metric.values.find(v =>
        JSON.stringify(v.labels) === JSON.stringify(labels)
      );

      if (existing) {
        existing.value = value;
      } else {
        metric.values.push({ labels, value });
      }
    } else {
      metric.value = value;
    }
  }

  /**
   * Increment counter metric
   * @param {string} name - Metric name
   * @param {number} delta - Increment value
   * @param {object} labels - Label values
   */
  incrementMetric(name, delta = 1, labels = {}) {
    const metric = this.customMetrics.get(name);
    if (!metric || metric.type !== MetricType.COUNTER) return;

    if (Object.keys(labels).length > 0) {
      const existing = metric.values.find(v =>
        JSON.stringify(v.labels) === JSON.stringify(labels)
      );

      if (existing) {
        existing.value += delta;
      } else {
        metric.values.push({ labels, value: delta });
      }
    } else {
      metric.value += delta;
    }
  }
}
