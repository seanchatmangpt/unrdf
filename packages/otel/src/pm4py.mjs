/**
 * @file PM4Py Integration - JavaScript bindings for pm4py-rust telemetry
 * @module @unrdf/otel/pm4py
 *
 * This module provides JavaScript bindings to pm4py's Python monitoring infrastructure,
 * enabling UNRDF to leverage pm4py's comprehensive observability stack.
 *
 * PM4Py Location: ~/chatmangpt/pm4py
 * - Monitoring: pm4py.monitoring.MetricCollector
 * - Alerts: pm4py.monitoring.alerts.AlertManager
 * - Dashboard: pm4py.monitoring.dashboard.MonitoringDashboard
 * - OTEL Config: deploy/otel-collector-config.yaml
 */

import { exec } from 'child_process';
import { promisify } from 'util';

const execAsync = promisify(exec);

/**
 * PM4Py metrics enum matching Python MetricType
 */
export const MetricType = {
  COUNTER: 'counter',
  GAUGE: 'gauge',
  HISTOGRAM: 'histogram',
  SUMMARY: 'summary'
};

/**
 * PM4Py Python integration helper
 */
class PM4PyIntegration {
  constructor(pythonPath = 'python3') {
    this.pythonPath = pythonPath;
    this.pm4pyPath = process.env.PM4PY_PATH || '~/chatmangpt/pm4py';
  }

  /**
   * Execute Python code in pm4py context
   */
  async execute(code) {
    const script = `
import sys
sys.path.insert(0, '${this.pm4pyPath}')
${code}
`;
    try {
      const { stdout } = await execAsync(`${this.pythonPath} -c "${script}"`);
      return { success: true, output: stdout };
    } catch (error) {
      return { success: false, error: error.message };
    }
  }

  /**
   * Get current metrics from pm4py MetricCollector
   */
  async getMetrics() {
    const code = `
from pm4py.monitoring import MetricCollector
import json

collector = MetricCollector()
metrics = collector.get_all_metrics()
result = [
  {
    'name': m.name,
    'value': m.value,
    'timestamp': m.timestamp.isoformat(),
    'labels': m.labels,
    'type': m.metric_type.value
  }
  for m in metrics
]
print(json.dumps(result))
`;
    const result = await this.execute(code);
    if (!result.success) {
      throw new Error(`Failed to get metrics: ${result.error}`);
    }
    return JSON.parse(result.output);
  }

  /**
   * Record a metric in pm4py
   */
  async recordMetric(name, value, type = MetricType.GAUGE, labels = {}) {
    const code = `
from pm4py.monitoring import MetricCollector

collector = MetricCollector()
metric = collector.${type.toLowerCase()}('${name}', '${name}')
${type === MetricType.COUNTER ? 'metric.inc()' : type === MetricType.GAUGE ? `metric.set(${value})` : `metric.observe(${value})`}
print('OK')
`;
    const result = await this.execute(code);
    return result.success;
  }

  /**
   * Get Prometheus-formatted metrics
   */
  async getPrometheusMetrics() {
    const code = `
from pm4py.monitoring import MetricCollector

collector = MetricCollector()
print(collector.to_prometheus_format())
`;
    const result = await this.execute(code);
    if (!result.success) {
      throw new Error(`Failed to get Prometheus metrics: ${result.error}`);
    }
    return result.output;
  }

  /**
   * Collect system metrics from pm4py
   */
  async getSystemMetrics() {
    const code = `
from pm4py.monitoring import SystemMetric
import json

metric = SystemMetric.collect()
print(json.dumps(metric.to_dict()))
`;
    const result = await this.execute(code);
    if (!result.success) {
      throw new Error(`Failed to get system metrics: ${result.error}`);
    }
    return JSON.parse(result.output);
  }
}

/**
 * Singleton instance
 */
let pm4pyInstance = null;

/**
 * Get PM4Py integration instance
 */
export function getPM4Py(options = {}) {
  if (!pm4pyInstance) {
    pm4pyInstance = new PM4PyIntegration(options.pythonPath);
  }
  return pm4pyInstance;
}

/**
 * Record UNRDF-specific metrics in pm4py
 */
export async function recordUNRDFMetrics(operation, metadata = {}) {
  const pm4py = getPM4Py();

  const baseLabels = {
    service: 'unrdf',
    operation,
    ...metadata
  };

  // Record operation count
  await pm4py.recordMetric(
    'unrdf_operations_total',
    1,
    MetricType.COUNTER,
    baseLabels
  );

  // Record active operations
  await pm4py.recordMetric(
    'unrdf_active_operations',
    metadata.active || 0,
    MetricType.GAUGE,
    baseLabels
  );
}

/**
 * Get pm4py system path
 */
export function getPM4PyPath() {
  return process.env.PM4PY_PATH || '~/chatmangpt/pm4py';
}

/**
 * Check if pm4py is available
 */
export async function checkPM4PyAvailable() {
  const pm4py = getPM4Py();
  try {
    const result = await pm4py.execute('from pm4py.monitoring import MetricCollector; print("OK")');
    return result.success && result.output.includes('OK');
  } catch {
    return false;
  }
}

/**
 * Initialize pm4py metrics for UNRDF
 */
export async function initializeUNRDFMetrics() {
  const pm4py = getPM4Py();

  const code = `
from pm4py.monitoring import MetricCollector

collector = MetricCollector()

# Register UNRDF-specific metrics
collector.register(collector.Counter(
    name='unrdf_operations_total',
    description='Total number of UNRDF operations'
))

collector.register(collector.Gauge(
    name='unrdf_active_operations',
    description='Currently active UNRDF operations'
))

collector.register(collector.Histogram(
    name='unrdf_operation_duration_seconds',
    description='UNRDF operation duration in seconds'
))

collector.register(collector.Gauge(
    name='unrdf_graph_quad_count',
    description='Current number of quads in graph'
))

collector.register(collector.Gauge(
    name='unrdf_sparql_query_count',
    description='Current number of SPARQL queries'
))

print('OK')
`;
  const result = await pm4py.execute(code);
  return result.success;
}

export { PM4PyIntegration };
