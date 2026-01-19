/**
 * @file Prometheus-style Metrics Collection System
 * @module @unrdf/daemon/monitoring/metrics-collector
 * @description Counter, Gauge, Histogram metric types with labels support,
 * Prometheus text format export, and metric aggregation over time windows.
 */

import { z } from 'zod';

/**
 * Metric type enum values
 */
export const MetricType = {
  COUNTER: 'counter',
  GAUGE: 'gauge',
  HISTOGRAM: 'histogram',
};

/**
 * Label schema - key-value pairs for metric dimensions
 */
export const LabelsSchema = z.record(z.string(), z.string());

/**
 * Counter metric configuration schema
 */
export const CounterConfigSchema = z.object({
  name: z.string().regex(/^[a-zA-Z_:][a-zA-Z0-9_:]*$/),
  help: z.string().min(1),
  labels: z.array(z.string()).default([]),
});

/**
 * Gauge metric configuration schema
 */
export const GaugeConfigSchema = z.object({
  name: z.string().regex(/^[a-zA-Z_:][a-zA-Z0-9_:]*$/),
  help: z.string().min(1),
  labels: z.array(z.string()).default([]),
});

/**
 * Histogram metric configuration schema
 */
export const HistogramConfigSchema = z.object({
  name: z.string().regex(/^[a-zA-Z_:][a-zA-Z0-9_:]*$/),
  help: z.string().min(1),
  labels: z.array(z.string()).default([]),
  buckets: z.array(z.number()).default([0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10]),
});

/**
 * Metrics collector configuration schema
 */
export const MetricsCollectorConfigSchema = z.object({
  prefix: z.string().default('unrdf'),
  defaultLabels: LabelsSchema.default({}),
  aggregationWindow: z.number().int().min(1000).default(60000),
  maxTimeSeries: z.number().int().min(100).default(10000),
  enableAggregation: z.boolean().default(true),
});

/**
 * Counter metric for monotonically increasing values
 */
class Counter {
  /** @type {import('zod').infer<typeof CounterConfigSchema>} */
  #config;

  /** @type {Map<string, number>} */
  #values = new Map();

  /**
   * Create a counter metric
   * @param {import('zod').infer<typeof CounterConfigSchema>} config
   */
  constructor(config) {
    this.#config = CounterConfigSchema.parse(config);
    this.#values.set('', 0);
  }

  get name() {
    return this.#config.name;
  }

  get help() {
    return this.#config.help;
  }

  get type() {
    return MetricType.COUNTER;
  }

  get labelNames() {
    return this.#config.labels;
  }

  /**
   * Increment counter by value
   * @param {number} [value=1] - Value to add (must be positive)
   * @param {Record<string, string>} [labels={}] - Label values
   * @throws {Error} If value is negative
   */
  inc(value = 1, labels = {}) {
    if (value < 0) {
      throw new Error('Counter can only be incremented by positive values');
    }
    const key = this.#labelsToKey(labels);
    const current = this.#values.get(key) || 0;
    this.#values.set(key, current + value);
  }

  /**
   * Get current counter value
   * @param {Record<string, string>} [labels={}] - Label values
   * @returns {number} Current value
   */
  get(labels = {}) {
    const key = this.#labelsToKey(labels);
    return this.#values.get(key) || 0;
  }

  /**
   * Reset counter to zero
   * @param {Record<string, string>} [labels={}] - Label values
   */
  reset(labels = {}) {
    const key = this.#labelsToKey(labels);
    this.#values.set(key, 0);
  }

  /**
   * Get all label combinations and their values
   * @returns {Array<{ labels: Record<string, string>, value: number }>}
   */
  getAll() {
    const results = [];
    for (const [key, value] of this.#values) {
      results.push({
        labels: this.#keyToLabels(key),
        value,
      });
    }
    return results;
  }

  #labelsToKey(labels) {
    if (this.#config.labels.length === 0) return '';
    return this.#config.labels.map((name) => `${name}=${labels[name] || ''}`).join(',');
  }

  #keyToLabels(key) {
    if (key === '') return {};
    const labels = {};
    for (const pair of key.split(',')) {
      const [name, value] = pair.split('=');
      labels[name] = value;
    }
    return labels;
  }
}

/**
 * Gauge metric for values that can go up or down
 */
class Gauge {
  /** @type {import('zod').infer<typeof GaugeConfigSchema>} */
  #config;

  /** @type {Map<string, number>} */
  #values = new Map();

  /**
   * Create a gauge metric
   * @param {import('zod').infer<typeof GaugeConfigSchema>} config
   */
  constructor(config) {
    this.#config = GaugeConfigSchema.parse(config);
    this.#values.set('', 0);
  }

  get name() {
    return this.#config.name;
  }

  get help() {
    return this.#config.help;
  }

  get type() {
    return MetricType.GAUGE;
  }

  get labelNames() {
    return this.#config.labels;
  }

  /**
   * Set gauge to specific value
   * @param {number} value - Value to set
   * @param {Record<string, string>} [labels={}] - Label values
   */
  set(value, labels = {}) {
    const key = this.#labelsToKey(labels);
    this.#values.set(key, value);
  }

  /**
   * Increment gauge by value
   * @param {number} [value=1] - Value to add
   * @param {Record<string, string>} [labels={}] - Label values
   */
  inc(value = 1, labels = {}) {
    const key = this.#labelsToKey(labels);
    const current = this.#values.get(key) || 0;
    this.#values.set(key, current + value);
  }

  /**
   * Decrement gauge by value
   * @param {number} [value=1] - Value to subtract
   * @param {Record<string, string>} [labels={}] - Label values
   */
  dec(value = 1, labels = {}) {
    const key = this.#labelsToKey(labels);
    const current = this.#values.get(key) || 0;
    this.#values.set(key, current - value);
  }

  /**
   * Get current gauge value
   * @param {Record<string, string>} [labels={}] - Label values
   * @returns {number} Current value
   */
  get(labels = {}) {
    const key = this.#labelsToKey(labels);
    return this.#values.get(key) || 0;
  }

  /**
   * Set gauge to current Unix timestamp in seconds
   * @param {Record<string, string>} [labels={}] - Label values
   */
  setToCurrentTime(labels = {}) {
    this.set(Date.now() / 1000, labels);
  }

  /**
   * Get all label combinations and their values
   * @returns {Array<{ labels: Record<string, string>, value: number }>}
   */
  getAll() {
    const results = [];
    for (const [key, value] of this.#values) {
      results.push({
        labels: this.#keyToLabels(key),
        value,
      });
    }
    return results;
  }

  #labelsToKey(labels) {
    if (this.#config.labels.length === 0) return '';
    return this.#config.labels.map((name) => `${name}=${labels[name] || ''}`).join(',');
  }

  #keyToLabels(key) {
    if (key === '') return {};
    const labels = {};
    for (const pair of key.split(',')) {
      const [name, value] = pair.split('=');
      labels[name] = value;
    }
    return labels;
  }
}

/**
 * Histogram metric for measuring distributions of values
 */
class Histogram {
  /** @type {import('zod').infer<typeof HistogramConfigSchema>} */
  #config;

  /** @type {Map<string, { buckets: Map<number, number>, sum: number, count: number }>} */
  #data = new Map();

  /**
   * Create a histogram metric
   * @param {import('zod').infer<typeof HistogramConfigSchema>} config
   */
  constructor(config) {
    this.#config = HistogramConfigSchema.parse(config);
    this.#initializeBuckets('');
  }

  get name() {
    return this.#config.name;
  }

  get help() {
    return this.#config.help;
  }

  get type() {
    return MetricType.HISTOGRAM;
  }

  get labelNames() {
    return this.#config.labels;
  }

  get buckets() {
    return [...this.#config.buckets];
  }

  /**
   * Observe a value
   * @param {number} value - Value to observe
   * @param {Record<string, string>} [labels={}] - Label values
   */
  observe(value, labels = {}) {
    const key = this.#labelsToKey(labels);
    if (!this.#data.has(key)) {
      this.#initializeBuckets(key);
    }

    const data = this.#data.get(key);
    data.sum += value;
    data.count += 1;

    for (const bucket of this.#config.buckets) {
      if (value <= bucket) {
        data.buckets.set(bucket, data.buckets.get(bucket) + 1);
      }
    }
    data.buckets.set(Infinity, data.buckets.get(Infinity) + 1);
  }

  /**
   * Start a timer and return a function to stop it
   * @param {Record<string, string>} [labels={}] - Label values
   * @returns {() => number} Function that stops timer and returns duration
   */
  startTimer(labels = {}) {
    const start = performance.now();
    return () => {
      const duration = (performance.now() - start) / 1000;
      this.observe(duration, labels);
      return duration;
    };
  }

  /**
   * Get histogram data
   * @param {Record<string, string>} [labels={}] - Label values
   * @returns {{ buckets: Map<number, number>, sum: number, count: number } | null}
   */
  get(labels = {}) {
    const key = this.#labelsToKey(labels);
    return this.#data.get(key) || null;
  }

  /**
   * Get all label combinations and their data
   * @returns {Array<{ labels: Record<string, string>, buckets: Array<{ le: number, count: number }>, sum: number, count: number }>}
   */
  getAll() {
    const results = [];
    for (const [key, data] of this.#data) {
      const buckets = [];
      for (const [le, count] of data.buckets) {
        buckets.push({ le, count });
      }
      buckets.sort((a, b) => a.le - b.le);
      results.push({
        labels: this.#keyToLabels(key),
        buckets,
        sum: data.sum,
        count: data.count,
      });
    }
    return results;
  }

  /**
   * Reset histogram data
   * @param {Record<string, string>} [labels={}] - Label values
   */
  reset(labels = {}) {
    const key = this.#labelsToKey(labels);
    this.#initializeBuckets(key);
  }

  #initializeBuckets(key) {
    const buckets = new Map();
    for (const bucket of this.#config.buckets) {
      buckets.set(bucket, 0);
    }
    buckets.set(Infinity, 0);
    this.#data.set(key, { buckets, sum: 0, count: 0 });
  }

  #labelsToKey(labels) {
    if (this.#config.labels.length === 0) return '';
    return this.#config.labels.map((name) => `${name}=${labels[name] || ''}`).join(',');
  }

  #keyToLabels(key) {
    if (key === '') return {};
    const labels = {};
    for (const pair of key.split(',')) {
      const [name, value] = pair.split('=');
      labels[name] = value;
    }
    return labels;
  }
}

/**
 * Metrics collector with Prometheus export support
 */
export class MetricsCollector {
  /** @type {import('zod').infer<typeof MetricsCollectorConfigSchema>} */
  #config;

  /** @type {Map<string, Counter>} */
  #counters = new Map();

  /** @type {Map<string, Gauge>} */
  #gauges = new Map();

  /** @type {Map<string, Histogram>} */
  #histograms = new Map();

  /** @type {Array<{ timestamp: number, metrics: Map<string, number> }>} */
  #aggregatedHistory = [];

  /** @type {NodeJS.Timeout | null} */
  #aggregationTimer = null;

  /**
   * Create a metrics collector
   * @param {Partial<import('zod').infer<typeof MetricsCollectorConfigSchema>>} [config]
   */
  constructor(config = {}) {
    this.#config = MetricsCollectorConfigSchema.parse(config);
  }

  /**
   * Start metrics aggregation
   */
  startAggregation() {
    if (!this.#config.enableAggregation) return;

    this.#aggregationTimer = setInterval(() => {
      this.#aggregateMetrics();
    }, this.#config.aggregationWindow);
  }

  /**
   * Stop metrics aggregation
   */
  stopAggregation() {
    if (this.#aggregationTimer) {
      clearInterval(this.#aggregationTimer);
      this.#aggregationTimer = null;
    }
  }

  /**
   * Create or get a counter metric
   * @param {import('zod').infer<typeof CounterConfigSchema>} config
   * @returns {Counter}
   */
  counter(config) {
    const name = this.#prefixedName(config.name);
    if (!this.#counters.has(name)) {
      this.#counters.set(name, new Counter({ ...config, name }));
    }
    return this.#counters.get(name);
  }

  /**
   * Create or get a gauge metric
   * @param {import('zod').infer<typeof GaugeConfigSchema>} config
   * @returns {Gauge}
   */
  gauge(config) {
    const name = this.#prefixedName(config.name);
    if (!this.#gauges.has(name)) {
      this.#gauges.set(name, new Gauge({ ...config, name }));
    }
    return this.#gauges.get(name);
  }

  /**
   * Create or get a histogram metric
   * @param {import('zod').infer<typeof HistogramConfigSchema>} config
   * @returns {Histogram}
   */
  histogram(config) {
    const name = this.#prefixedName(config.name);
    if (!this.#histograms.has(name)) {
      this.#histograms.set(name, new Histogram({ ...config, name }));
    }
    return this.#histograms.get(name);
  }

  /**
   * Export all metrics in Prometheus text format
   * @returns {string} Prometheus exposition format
   */
  exportPrometheus() {
    const lines = [];

    for (const counter of this.#counters.values()) {
      lines.push(`# HELP ${counter.name} ${counter.help}`);
      lines.push(`# TYPE ${counter.name} counter`);
      for (const { labels, value } of counter.getAll()) {
        lines.push(`${counter.name}${this.#formatLabels(labels)} ${value}`);
      }
    }

    for (const gauge of this.#gauges.values()) {
      lines.push(`# HELP ${gauge.name} ${gauge.help}`);
      lines.push(`# TYPE ${gauge.name} gauge`);
      for (const { labels, value } of gauge.getAll()) {
        lines.push(`${gauge.name}${this.#formatLabels(labels)} ${value}`);
      }
    }

    for (const histogram of this.#histograms.values()) {
      lines.push(`# HELP ${histogram.name} ${histogram.help}`);
      lines.push(`# TYPE ${histogram.name} histogram`);
      for (const { labels, buckets, sum, count } of histogram.getAll()) {
        for (const { le, count: bucketCount } of buckets) {
          const leStr = le === Infinity ? '+Inf' : le.toString();
          lines.push(`${histogram.name}_bucket${this.#formatLabels({ ...labels, le: leStr })} ${bucketCount}`);
        }
        lines.push(`${histogram.name}_sum${this.#formatLabels(labels)} ${sum}`);
        lines.push(`${histogram.name}_count${this.#formatLabels(labels)} ${count}`);
      }
    }

    return lines.join('\n');
  }

  /**
   * Get aggregated metrics over time windows
   * @param {number} [windowCount=10] - Number of windows to return
   * @returns {Array<{ timestamp: number, metrics: Record<string, number> }>}
   */
  getAggregatedMetrics(windowCount = 10) {
    return this.#aggregatedHistory.slice(-windowCount).map((entry) => ({
      timestamp: entry.timestamp,
      metrics: Object.fromEntries(entry.metrics),
    }));
  }

  /**
   * Get all registered metric names
   * @returns {{ counters: string[], gauges: string[], histograms: string[] }}
   */
  getRegisteredMetrics() {
    return {
      counters: [...this.#counters.keys()],
      gauges: [...this.#gauges.keys()],
      histograms: [...this.#histograms.keys()],
    };
  }

  /**
   * Reset all metrics
   */
  resetAll() {
    this.#counters.clear();
    this.#gauges.clear();
    this.#histograms.clear();
    this.#aggregatedHistory = [];
  }

  #prefixedName(name) {
    if (this.#config.prefix && !name.startsWith(this.#config.prefix)) {
      return `${this.#config.prefix}_${name}`;
    }
    return name;
  }

  #formatLabels(labels) {
    const merged = { ...this.#config.defaultLabels, ...labels };
    const entries = Object.entries(merged).filter(([_, v]) => v !== '');
    if (entries.length === 0) return '';
    return `{${entries.map(([k, v]) => `${k}="${v}"`).join(',')}}`;
  }

  #aggregateMetrics() {
    const metrics = new Map();

    for (const [name, counter] of this.#counters) {
      for (const { labels, value } of counter.getAll()) {
        const key = `${name}${JSON.stringify(labels)}`;
        metrics.set(key, value);
      }
    }

    for (const [name, gauge] of this.#gauges) {
      for (const { labels, value } of gauge.getAll()) {
        const key = `${name}${JSON.stringify(labels)}`;
        metrics.set(key, value);
      }
    }

    this.#aggregatedHistory.push({
      timestamp: Date.now(),
      metrics,
    });

    // Keep only last hour of aggregations
    const cutoff = Date.now() - 3600000;
    this.#aggregatedHistory = this.#aggregatedHistory.filter((e) => e.timestamp > cutoff);
  }
}

/**
 * Create a preconfigured metrics collector
 * @param {Partial<import('zod').infer<typeof MetricsCollectorConfigSchema>>} [config]
 * @returns {MetricsCollector}
 */
export function createMetricsCollector(config = {}) {
  return new MetricsCollector(config);
}

export { Counter, Gauge, Histogram };
export default MetricsCollector;
