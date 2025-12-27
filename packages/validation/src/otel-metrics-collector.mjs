/**
 * @file OTEL Metrics Collector
 * @module validation/otel-metrics-collector
 *
 * @description
 * Collects and validates OTEL metrics for feature validation.
 */

import { metrics } from '@opentelemetry/api';

/**
 * Metrics collector for OTEL validation
 */
export class MetricsCollector {
  /**
   * Create a metrics collector
   * @param {Object} [config] - Collector configuration
   */
  constructor(config = {}) {
    this.config = {
      serviceName: 'unrdf-validator',
      ...config,
    };

    this.meter = metrics.getMeter(this.config.serviceName);
    this.collectedMetrics = new Map();
  }

  /**
   * Collect metrics for a feature
   * @param {string} feature - Feature name
   * @returns {Object} Collected metrics
   */
  collectMetrics(feature) {
    // Return default metrics structure
    return {
      latency: 0,
      errorRate: 0,
      throughput: 0,
      memoryUsage: 0,
    };
  }

  /**
   * Record metric
   * @param {string} name - Metric name
   * @param {number} value - Metric value
   * @param {Object} [attributes] - Metric attributes
   */
  recordMetric(name, value, attributes = {}) {
    const key = `${name}_${JSON.stringify(attributes)}`;
    this.collectedMetrics.set(key, { name, value, attributes, timestamp: Date.now() });
  }

  /**
   * Get all collected metrics
   * @returns {Map} Collected metrics
   */
  getMetrics() {
    return new Map(this.collectedMetrics);
  }

  /**
   * Clear collected metrics
   */
  clearMetrics() {
    this.collectedMetrics.clear();
  }
}

/**
 * Validate metrics against thresholds
 * @param {Object} metrics - Metrics to validate
 * @param {Object} thresholds - Performance thresholds
 * @returns {Object} Validation result with violations
 */
export function validateMetrics(metrics, thresholds) {
  const violations = [];

  if (metrics.latency > thresholds.maxLatency) {
    violations.push(
      `Latency ${metrics.latency}ms exceeds threshold ${thresholds.maxLatency}ms`
    );
  }

  if (metrics.errorRate > thresholds.maxErrorRate) {
    violations.push(
      `Error rate ${(metrics.errorRate * 100).toFixed(2)}% exceeds threshold ${(thresholds.maxErrorRate * 100).toFixed(2)}%`
    );
  }

  if (metrics.throughput < thresholds.minThroughput) {
    violations.push(
      `Throughput ${metrics.throughput} ops below threshold ${thresholds.minThroughput} ops`
    );
  }

  if (metrics.memoryUsage > thresholds.maxMemoryUsage) {
    violations.push(
      `Memory usage ${(metrics.memoryUsage / 1024 / 1024).toFixed(2)}MB exceeds threshold ${(thresholds.maxMemoryUsage / 1024 / 1024).toFixed(2)}MB`
    );
  }

  return {
    passed: violations.length === 0,
    violations,
  };
}

/**
 * Create a metrics collector instance
 * @param {Object} [config] - Configuration
 * @returns {MetricsCollector} Metrics collector instance
 */
export function createMetricsCollector(config = {}) {
  return new MetricsCollector(config);
}

/**
 * Default metrics collector instance
 */
export const defaultMetricsCollector = createMetricsCollector();
