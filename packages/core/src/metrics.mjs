/**
 * @file Performance Metrics Collection
 * @module @unrdf/core/metrics
 *
 * @description
 * Production-grade metrics collection system with histograms, counters, and gauges.
 * Integrates with OTEL and Prometheus for comprehensive observability.
 *
 * @example
 * ```javascript
 * import { createMetrics } from '@unrdf/core/metrics';
 *
 * const metrics = createMetrics({ prefix: 'unrdf' });
 *
 * // Track request
 * metrics.incrementCounter('requests_total', { method: 'GET', status: 200 });
 *
 * // Track duration
 * const timer = metrics.startTimer();
 * await doWork();
 * metrics.recordDuration('operation_duration', timer, { operation: 'query' });
 *
 * // Record value
 * metrics.recordGauge('active_connections', 42);
 * ```
 */

import { z } from 'zod';

/**
 * Metrics configuration schema
 */
const MetricsConfigSchema = z.object({
  prefix: z.string().default('unrdf'),
  labels: z.record(z.string(), z.string()).default({}),
  buckets: z.array(z.number()).default([0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10])
});

/**
 * Create metrics collector
 *
 * @param {Object} config - Metrics configuration
 * @param {string} [config.prefix='unrdf'] - Metric name prefix
 * @param {Object} [config.labels={}] - Default labels for all metrics
 * @param {number[]} [config.buckets] - Histogram buckets (seconds)
 * @returns {Object} Metrics collector
 */
export function createMetrics(config = {}) {
  const validated = MetricsConfigSchema.parse(config);

  // Storage for metrics
  const counters = new Map();
  const gauges = new Map();
  const histograms = new Map();
  const summaries = new Map();

  /**
   * Generate metric key from name and labels
   * @param {string} name - Metric name
   * @param {Object} labels - Metric labels
   * @returns {string} Metric key
   */
  function getMetricKey(name, labels = {}) {
    const allLabels = { ...validated.labels, ...labels };
    const labelStr = Object.entries(allLabels)
      .sort(([a], [b]) => a.localeCompare(b))
      .map(([k, v]) => `${k}="${v}"`)
      .join(',');
    return labelStr ? `${name}{${labelStr}}` : name;
  }

  /**
   * Increment counter metric
   * @param {string} name - Metric name
   * @param {Object} labels - Metric labels
   * @param {number} value - Increment value (default: 1)
   */
  function incrementCounter(name, labels = {}, value = 1) {
    const key = getMetricKey(name, labels);
    const current = counters.get(key) || 0;
    counters.set(key, current + value);
  }

  /**
   * Set gauge metric
   * @param {string} name - Metric name
   * @param {number} value - Gauge value
   * @param {Object} labels - Metric labels
   */
  function recordGauge(name, value, labels = {}) {
    const key = getMetricKey(name, labels);
    gauges.set(key, value);
  }

  /**
   * Record histogram observation
   * @param {string} name - Metric name
   * @param {number} value - Observed value (in seconds)
   * @param {Object} labels - Metric labels
   */
  function recordHistogram(name, value, labels = {}) {
    const key = getMetricKey(name, labels);
    const histogram = histograms.get(key) || {
      count: 0,
      sum: 0,
      buckets: new Map(validated.buckets.map(b => [b, 0]))
    };

    histogram.count++;
    histogram.sum += value;

    // Increment bucket counts
    for (const [bucket, count] of histogram.buckets) {
      if (value <= bucket) {
        histogram.buckets.set(bucket, count + 1);
      }
    }

    histograms.set(key, histogram);
  }

  /**
   * Start timer for duration tracking
   * @returns {Object} Timer object with end() method
   */
  function startTimer() {
    const start = process.hrtime.bigint();

    return {
      end: () => {
        const end = process.hrtime.bigint();
        return Number(end - start) / 1e9; // Convert to seconds
      }
    };
  }

  /**
   * Record duration from timer
   * @param {string} name - Metric name
   * @param {Object} timer - Timer object from startTimer()
   * @param {Object} labels - Metric labels
   */
  function recordDuration(name, timer, labels = {}) {
    const duration = timer.end();
    recordHistogram(name, duration, labels);
  }

  /**
   * Record summary statistics (P50, P95, P99)
   * @param {string} name - Metric name
   * @param {number} value - Observed value
   * @param {Object} labels - Metric labels
   */
  function recordSummary(name, value, labels = {}) {
    const key = getMetricKey(name, labels);
    const summary = summaries.get(key) || {
      values: [],
      count: 0,
      sum: 0
    };

    summary.values.push(value);
    summary.count++;
    summary.sum += value;

    // Keep only last 1000 values for percentile calculation
    if (summary.values.length > 1000) {
      summary.values.shift();
    }

    summaries.set(key, summary);
  }

  /**
   * Calculate percentile from summary
   * @param {number[]} values - Sorted values array
   * @param {number} percentile - Percentile (0-1)
   * @returns {number} Percentile value
   */
  function calculatePercentile(values, percentile) {
    if (values.length === 0) return 0;
    const sorted = [...values].sort((a, b) => a - b);
    const index = Math.ceil(sorted.length * percentile) - 1;
    return sorted[Math.max(0, index)];
  }

  /**
   * Get all metrics in Prometheus exposition format
   * @returns {string} Metrics in Prometheus format
   */
  function toPrometheus() {
    const lines = [];

    // Counters
    for (const [key, value] of counters) {
      const name = key.split('{')[0];
      const fullName = `${validated.prefix}_${name}`;
      lines.push(`# TYPE ${fullName} counter`);
      lines.push(`${validated.prefix}_${key} ${value}`);
    }

    // Gauges
    for (const [key, value] of gauges) {
      const name = key.split('{')[0];
      const fullName = `${validated.prefix}_${name}`;
      lines.push(`# TYPE ${fullName} gauge`);
      lines.push(`${validated.prefix}_${key} ${value}`);
    }

    // Histograms
    for (const [key, histogram] of histograms) {
      const name = key.split('{')[0];
      const fullName = `${validated.prefix}_${name}`;
      const baseLabels = key.match(/\{(.+)\}/)?.[1] || '';

      lines.push(`# TYPE ${fullName} histogram`);

      // Bucket counts
      for (const [bucket, count] of histogram.buckets) {
        const labels = baseLabels
          ? `${baseLabels},le="${bucket}"`
          : `le="${bucket}"`;
        lines.push(`${validated.prefix}_${name}_bucket{${labels}} ${count}`);
      }

      // +Inf bucket
      const labels = baseLabels ? `${baseLabels},le="+Inf"` : `le="+Inf"`;
      lines.push(`${validated.prefix}_${name}_bucket{${labels}} ${histogram.count}`);

      // Sum and count
      lines.push(`${validated.prefix}_${name}_sum${baseLabels ? `{${baseLabels}}` : ''} ${histogram.sum}`);
      lines.push(`${validated.prefix}_${name}_count${baseLabels ? `{${baseLabels}}` : ''} ${histogram.count}`);
    }

    // Summaries
    for (const [key, summary] of summaries) {
      const name = key.split('{')[0];
      const fullName = `${validated.prefix}_${name}`;
      const baseLabels = key.match(/\{(.+)\}/)?.[1] || '';

      lines.push(`# TYPE ${fullName} summary`);

      // Percentiles
      const p50 = calculatePercentile(summary.values, 0.5);
      const p95 = calculatePercentile(summary.values, 0.95);
      const p99 = calculatePercentile(summary.values, 0.99);

      const p50Labels = baseLabels ? `${baseLabels},quantile="0.5"` : `quantile="0.5"`;
      const p95Labels = baseLabels ? `${baseLabels},quantile="0.95"` : `quantile="0.95"`;
      const p99Labels = baseLabels ? `${baseLabels},quantile="0.99"` : `quantile="0.99"`;

      lines.push(`${validated.prefix}_${name}{${p50Labels}} ${p50}`);
      lines.push(`${validated.prefix}_${name}{${p95Labels}} ${p95}`);
      lines.push(`${validated.prefix}_${name}{${p99Labels}} ${p99}`);

      // Sum and count
      lines.push(`${validated.prefix}_${name}_sum${baseLabels ? `{${baseLabels}}` : ''} ${summary.sum}`);
      lines.push(`${validated.prefix}_${name}_count${baseLabels ? `{${baseLabels}}` : ''} ${summary.count}`);
    }

    return lines.join('\n') + '\n';
  }

  /**
   * Get metrics as JSON
   * @returns {Object} Metrics data
   */
  function toJSON() {
    const result = {
      counters: Object.fromEntries(counters),
      gauges: Object.fromEntries(gauges),
      histograms: {},
      summaries: {}
    };

    // Convert histograms
    for (const [key, histogram] of histograms) {
      result.histograms[key] = {
        count: histogram.count,
        sum: histogram.sum,
        avg: histogram.count > 0 ? histogram.sum / histogram.count : 0,
        buckets: Object.fromEntries(histogram.buckets)
      };
    }

    // Convert summaries with percentiles
    for (const [key, summary] of summaries) {
      result.summaries[key] = {
        count: summary.count,
        sum: summary.sum,
        avg: summary.count > 0 ? summary.sum / summary.count : 0,
        p50: calculatePercentile(summary.values, 0.5),
        p95: calculatePercentile(summary.values, 0.95),
        p99: calculatePercentile(summary.values, 0.99)
      };
    }

    return result;
  }

  /**
   * Reset all metrics
   */
  function reset() {
    counters.clear();
    gauges.clear();
    histograms.clear();
    summaries.clear();
  }

  return {
    incrementCounter,
    recordGauge,
    recordHistogram,
    recordDuration,
    recordSummary,
    startTimer,
    toPrometheus,
    toJSON,
    reset
  };
}

/**
 * Global metrics instance for UNRDF
 */
export const metrics = createMetrics({
  prefix: 'unrdf',
  labels: {
    service: 'unrdf',
    version: '5.0.1'
  }
});
