/**
 * @file OpenTelemetry Metrics Collector
 * @module validation/otel-metrics-collector
 *
 * @description
 * Metrics collection and validation utilities for OTEL-based validation.
 */

import { metrics } from "@opentelemetry/api";

/**
 * Metrics collector for validation
 */
export class MetricsCollector {
  /**
   * Create a new metrics collector
   * @param {string} [serviceName='unrdf-validator'] - Service name for meter
   */
  constructor(serviceName = 'unrdf-validator') {
    this.meter = metrics.getMeter(serviceName);
    this.collections = new Map();

    // Create default metrics
    this.counters = {
      validations: this.meter.createCounter('validation_total', {
        description: 'Total number of validations',
      }),
      errors: this.meter.createCounter('validation_errors_total', {
        description: 'Total number of validation errors',
      }),
      spans: this.meter.createCounter('spans_collected_total', {
        description: 'Total number of spans collected',
      }),
    };

    this.histograms = {
      duration: this.meter.createHistogram('validation_duration_ms', {
        description: 'Validation duration in milliseconds',
        unit: 'ms',
      }),
      score: this.meter.createHistogram('validation_score', {
        description: 'Validation score (0-100)',
      }),
      latency: this.meter.createHistogram('operation_latency_ms', {
        description: 'Operation latency in milliseconds',
        unit: 'ms',
      }),
    };

    this.gauges = {
      activeValidations: this.meter.createUpDownCounter('active_validations', {
        description: 'Number of active validations',
      }),
      healthScore: this.meter.createUpDownCounter('feature_health_score', {
        description: 'Current health score of features',
      }),
    };
  }

  /**
   * Start collecting metrics for a validation
   * @param {string} validationId - Unique validation ID
   * @returns {Object} Collection context
   */
  startCollection(validationId) {
    const collection = {
      id: validationId,
      startTime: Date.now(),
      latencies: [],
      errors: 0,
      operations: 0,
      memorySnapshots: [],
    };

    this.collections.set(validationId, collection);
    this.gauges.activeValidations.add(1, { validation_id: validationId });

    return collection;
  }

  /**
   * Record a latency measurement
   * @param {string} validationId - Validation ID
   * @param {number} latency - Latency in ms
   * @param {Object} [attributes={}] - Additional attributes
   */
  recordLatency(validationId, latency, attributes = {}) {
    const collection = this.collections.get(validationId);
    if (collection) {
      collection.latencies.push(latency);
      collection.operations++;
    }

    this.histograms.latency.record(latency, {
      validation_id: validationId,
      ...attributes,
    });
  }

  /**
   * Record an error
   * @param {string} validationId - Validation ID
   * @param {Error} error - Error object
   * @param {Object} [attributes={}] - Additional attributes
   */
  recordError(validationId, error, attributes = {}) {
    const collection = this.collections.get(validationId);
    if (collection) {
      collection.errors++;
    }

    this.counters.errors.add(1, {
      validation_id: validationId,
      error_type: error.name,
      ...attributes,
    });
  }

  /**
   * Record an operation
   * @param {string} validationId - Validation ID
   * @param {Object} [attributes={}] - Additional attributes
   */
  recordOperation(validationId, attributes = {}) {
    const collection = this.collections.get(validationId);
    if (collection) {
      collection.operations++;
    }
  }

  /**
   * Take a memory snapshot
   * @param {string} validationId - Validation ID
   */
  snapshotMemory(validationId) {
    const collection = this.collections.get(validationId);
    if (collection) {
      collection.memorySnapshots.push({
        timestamp: Date.now(),
        heapUsed: process.memoryUsage().heapUsed,
        heapTotal: process.memoryUsage().heapTotal,
      });
    }
  }

  /**
   * End collection and compute aggregates
   * @param {string} validationId - Validation ID
   * @returns {Object} Aggregated metrics
   */
  endCollection(validationId) {
    const collection = this.collections.get(validationId);
    if (!collection) {
      return {
        latency: 0,
        errorRate: 0,
        throughput: 0,
        memoryUsage: process.memoryUsage().heapUsed,
      };
    }

    const duration = Date.now() - collection.startTime;

    // Compute aggregates
    const avgLatency = collection.latencies.length > 0
      ? collection.latencies.reduce((a, b) => a + b, 0) / collection.latencies.length
      : 0;

    const errorRate = collection.operations > 0
      ? collection.errors / collection.operations
      : 0;

    const throughput = collection.operations;

    const memoryUsage = collection.memorySnapshots.length > 0
      ? collection.memorySnapshots[collection.memorySnapshots.length - 1].heapUsed
      : process.memoryUsage().heapUsed;

    // Record final metrics
    this.histograms.duration.record(duration, { validation_id: validationId });
    this.gauges.activeValidations.add(-1, { validation_id: validationId });

    // Clean up
    this.collections.delete(validationId);

    return {
      latency: avgLatency,
      errorRate,
      throughput,
      memoryUsage,
      duration,
      totalOperations: collection.operations,
      totalErrors: collection.errors,
    };
  }

  /**
   * Record validation completion
   * @param {string} feature - Feature name
   * @param {boolean} passed - Whether validation passed
   * @param {number} score - Validation score
   * @param {number} duration - Duration in ms
   */
  recordValidationComplete(feature, passed, score, duration) {
    this.counters.validations.add(1, {
      feature,
      passed: passed.toString(),
    });

    this.histograms.score.record(score, { feature });
    this.histograms.duration.record(duration, { feature });
    this.gauges.healthScore.add(score, { feature });
  }

  /**
   * Get current metrics summary
   * @returns {Object} Metrics summary
   */
  getSummary() {
    return {
      activeCollections: this.collections.size,
      collections: Array.from(this.collections.keys()),
    };
  }
}

/**
 * Validate metrics against thresholds
 * @param {Object} metricsData - Collected metrics
 * @param {Object} thresholds - Performance thresholds
 * @returns {Object} Validation result with violations
 */
export function validateMetrics(metricsData, thresholds) {
  const violations = [];

  if (metricsData.latency > thresholds.maxLatency) {
    violations.push(
      `Latency ${metricsData.latency.toFixed(2)}ms exceeds threshold ${thresholds.maxLatency}ms`
    );
  }

  if (metricsData.errorRate > thresholds.maxErrorRate) {
    violations.push(
      `Error rate ${(metricsData.errorRate * 100).toFixed(1)}% exceeds threshold ${(thresholds.maxErrorRate * 100).toFixed(1)}%`
    );
  }

  if (metricsData.throughput < thresholds.minThroughput) {
    violations.push(
      `Throughput ${metricsData.throughput} below threshold ${thresholds.minThroughput}`
    );
  }

  if (metricsData.memoryUsage > thresholds.maxMemoryUsage) {
    violations.push(
      `Memory usage ${formatBytes(metricsData.memoryUsage)} exceeds threshold ${formatBytes(thresholds.maxMemoryUsage)}`
    );
  }

  const score = Math.max(0, 100 - violations.length * 15);

  return { violations, score };
}

/**
 * Format bytes to human readable string
 * @param {number} bytes - Bytes to format
 * @returns {string} Formatted string
 */
function formatBytes(bytes) {
  const units = ['B', 'KB', 'MB', 'GB'];
  let index = 0;
  let value = bytes;

  while (value >= 1024 && index < units.length - 1) {
    value /= 1024;
    index++;
  }

  return `${value.toFixed(2)} ${units[index]}`;
}

/**
 * Default metrics collector instance
 */
export const defaultMetricsCollector = new MetricsCollector();

/**
 * Create a new metrics collector
 * @param {string} [serviceName] - Service name
 * @returns {MetricsCollector} New collector instance
 */
export function createMetricsCollector(serviceName) {
  return new MetricsCollector(serviceName);
}
