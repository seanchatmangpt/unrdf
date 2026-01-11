/**
 * @file Advanced OpenTelemetry Metrics
 * @module observability/advanced-metrics
 *
 * @description
 * Advanced business metrics, latency histograms, throughput tracking,
 * and resource utilization monitoring with zero performance impact.
 */

import { metrics } from '@opentelemetry/api';
import { z } from 'zod';

/**
 * Advanced metrics configuration schema
 */
export const AdvancedMetricsConfigSchema = z.object({
  serviceName: z.string().default('unrdf'),
  serviceVersion: z.string().default('6.0.0'),
  enabled: z.boolean().default(true),
  samplingRate: z.number().min(0).max(1).default(0.01),
  buckets: z.object({
    latency: z.array(z.number()).default([1, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000]),
    throughput: z.array(z.number()).default([1, 10, 50, 100, 500, 1000, 5000]),
    memory: z.array(z.number()).default([1e6, 10e6, 50e6, 100e6, 500e6, 1e9, 5e9]),
  }).default({
    latency: [1, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000],
    throughput: [1, 10, 50, 100, 500, 1000, 5000],
    memory: [1e6, 10e6, 50e6, 100e6, 500e6, 1e9, 5e9],
  }),
});

/**
 * Advanced metrics manager
 *
 * Provides production-grade observability with:
 * - Business metrics (success/failure rates)
 * - Latency histograms (P50, P90, P95, P99)
 * - Throughput metrics (ops/sec)
 * - Resource utilization (memory, CPU)
 * - Zero performance impact (sampling, async recording)
 */
export class AdvancedMetrics {
  /**
   * Create advanced metrics manager
   * @param {Object} [config] - Configuration options
   */
  constructor(config = {}) {
    this.config = AdvancedMetricsConfigSchema.parse(config);
    this.meter = metrics.getMeter(this.config.serviceName, this.config.serviceVersion);

    // Metrics instances
    this.businessMetrics = null;
    this.latencyMetrics = null;
    this.throughputMetrics = null;
    this.resourceMetrics = null;

    // Internal state for throughput calculation
    this.operationCounts = new Map();
    this.lastThroughputCalculation = Date.now();

    if (this.config.enabled) {
      this._initializeMetrics();
    }
  }

  /**
   * Initialize all metrics
   * @private
   */
  _initializeMetrics() {
    this._initializeBusinessMetrics();
    this._initializeLatencyMetrics();
    this._initializeThroughputMetrics();
    this._initializeResourceMetrics();
  }

  /**
   * Initialize business metrics
   * @private
   */
  _initializeBusinessMetrics() {
    this.businessMetrics = {
      // Operation counters by type and result
      operations: this.meter.createCounter('business.operations.total', {
        description: 'Total business operations by type and result',
      }),

      // Success rate gauge
      successRate: this.meter.createUpDownCounter('business.success_rate', {
        description: 'Success rate for operations (0-1)',
      }),

      // Failure rate by error type
      failuresByType: this.meter.createCounter('business.failures.by_type', {
        description: 'Failures categorized by error type',
      }),

      // SLA violations
      slaViolations: this.meter.createCounter('business.sla_violations', {
        description: 'SLA violations by operation and threshold',
      }),
    };
  }

  /**
   * Initialize latency metrics with percentiles
   * @private
   */
  _initializeLatencyMetrics() {
    this.latencyMetrics = {
      // Histogram with explicit buckets for percentiles
      histogram: this.meter.createHistogram('latency.operation_duration_ms', {
        description: 'Operation latency in milliseconds',
        unit: 'ms',
        advice: {
          explicitBucketBoundaries: this.config.buckets.latency,
        },
      }),

      // P50 latency gauge
      p50: this.meter.createUpDownCounter('latency.p50_ms', {
        description: 'P50 (median) latency in milliseconds',
        unit: 'ms',
      }),

      // P90 latency gauge
      p90: this.meter.createUpDownCounter('latency.p90_ms', {
        description: 'P90 latency in milliseconds',
        unit: 'ms',
      }),

      // P95 latency gauge
      p95: this.meter.createUpDownCounter('latency.p95_ms', {
        description: 'P95 latency in milliseconds',
        unit: 'ms',
      }),

      // P99 latency gauge
      p99: this.meter.createUpDownCounter('latency.p99_ms', {
        description: 'P99 latency in milliseconds',
        unit: 'ms',
      }),

      // Max latency
      max: this.meter.createUpDownCounter('latency.max_ms', {
        description: 'Maximum latency in milliseconds',
        unit: 'ms',
      }),
    };
  }

  /**
   * Initialize throughput metrics
   * @private
   */
  _initializeThroughputMetrics() {
    this.throughputMetrics = {
      // Operations per second
      opsPerSecond: this.meter.createUpDownCounter('throughput.ops_per_second', {
        description: 'Operations per second',
        unit: '1/s',
      }),

      // Throughput histogram
      histogram: this.meter.createHistogram('throughput.rate', {
        description: 'Throughput rate histogram',
        unit: '1/s',
        advice: {
          explicitBucketBoundaries: this.config.buckets.throughput,
        },
      }),

      // Peak throughput
      peak: this.meter.createUpDownCounter('throughput.peak_ops_per_second', {
        description: 'Peak operations per second',
        unit: '1/s',
      }),
    };
  }

  /**
   * Initialize resource utilization metrics
   * @private
   */
  _initializeResourceMetrics() {
    this.resourceMetrics = {
      // Memory usage histogram
      memoryHistogram: this.meter.createHistogram('resource.memory_bytes', {
        description: 'Memory usage in bytes',
        unit: 'By',
        advice: {
          explicitBucketBoundaries: this.config.buckets.memory,
        },
      }),

      // Heap used
      heapUsed: this.meter.createUpDownCounter('resource.heap_used_bytes', {
        description: 'Heap memory used in bytes',
        unit: 'By',
      }),

      // Heap total
      heapTotal: this.meter.createUpDownCounter('resource.heap_total_bytes', {
        description: 'Total heap memory in bytes',
        unit: 'By',
      }),

      // External memory
      external: this.meter.createUpDownCounter('resource.external_bytes', {
        description: 'External memory used in bytes',
        unit: 'By',
      }),

      // CPU usage (approximated from event loop lag)
      cpuLoad: this.meter.createUpDownCounter('resource.cpu_load', {
        description: 'CPU load estimate (0-1)',
      }),

      // Event loop lag
      eventLoopLag: this.meter.createHistogram('resource.event_loop_lag_ms', {
        description: 'Event loop lag in milliseconds',
        unit: 'ms',
      }),
    };
  }

  /**
   * Record a business operation
   *
   * @param {Object} options - Operation options
   * @param {string} options.operation - Operation type
   * @param {boolean} options.success - Whether operation succeeded
   * @param {number} options.duration - Operation duration in ms
   * @param {string} [options.errorType] - Error type if failed
   * @param {number} [options.slaThreshold] - SLA threshold in ms
   */
  recordOperation({ operation, success, duration, errorType, slaThreshold }) {
    if (!this.config.enabled || !this._shouldSample()) return;

    // Record operation count
    this.businessMetrics.operations.add(1, {
      operation,
      result: success ? 'success' : 'failure',
    });

    // Record failure by type
    if (!success && errorType) {
      this.businessMetrics.failuresByType.add(1, {
        operation,
        error_type: errorType,
      });
    }

    // Record SLA violation
    if (slaThreshold && duration > slaThreshold) {
      this.businessMetrics.slaViolations.add(1, {
        operation,
        threshold: slaThreshold.toString(),
      });
    }

    // Record latency
    this.latencyMetrics.histogram.record(duration, { operation });

    // Update throughput tracking
    this._updateThroughput(operation);
  }

  /**
   * Record success rate
   *
   * @param {string} operation - Operation type
   * @param {number} rate - Success rate (0-1)
   */
  recordSuccessRate(operation, rate) {
    if (!this.config.enabled) return;

    this.businessMetrics.successRate.add(rate, { operation });
  }

  /**
   * Record latency percentiles
   *
   * @param {string} operation - Operation type
   * @param {Object} percentiles - Percentile values
   * @param {number} percentiles.p50 - P50 latency
   * @param {number} percentiles.p90 - P90 latency
   * @param {number} percentiles.p95 - P95 latency
   * @param {number} percentiles.p99 - P99 latency
   * @param {number} percentiles.max - Max latency
   */
  recordLatencyPercentiles(operation, { p50, p90, p95, p99, max }) {
    if (!this.config.enabled) return;

    const attrs = { operation };

    this.latencyMetrics.p50.add(p50, attrs);
    this.latencyMetrics.p90.add(p90, attrs);
    this.latencyMetrics.p95.add(p95, attrs);
    this.latencyMetrics.p99.add(p99, attrs);
    this.latencyMetrics.max.add(max, attrs);
  }

  /**
   * Record throughput
   *
   * @param {string} operation - Operation type
   * @param {number} opsPerSecond - Operations per second
   */
  recordThroughput(operation, opsPerSecond) {
    if (!this.config.enabled) return;

    const attrs = { operation };

    this.throughputMetrics.opsPerSecond.add(opsPerSecond, attrs);
    this.throughputMetrics.histogram.record(opsPerSecond, attrs);
  }

  /**
   * Record resource utilization
   */
  recordResourceUtilization() {
    if (!this.config.enabled) return;

    const memUsage = process.memoryUsage();

    this.resourceMetrics.memoryHistogram.record(memUsage.heapUsed);
    this.resourceMetrics.heapUsed.add(memUsage.heapUsed);
    this.resourceMetrics.heapTotal.add(memUsage.heapTotal);
    this.resourceMetrics.external.add(memUsage.external);
  }

  /**
   * Record event loop lag
   *
   * @param {number} lag - Lag in milliseconds
   */
  recordEventLoopLag(lag) {
    if (!this.config.enabled) return;

    this.resourceMetrics.eventLoopLag.record(lag);

    // Estimate CPU load from lag (simplified model)
    const cpuLoad = Math.min(1, lag / 100);
    this.resourceMetrics.cpuLoad.add(cpuLoad);
  }

  /**
   * Update throughput tracking
   *
   * @param {string} operation - Operation type
   * @private
   */
  _updateThroughput(operation) {
    const count = (this.operationCounts.get(operation) || 0) + 1;
    this.operationCounts.set(operation, count);

    // Calculate throughput every second
    const now = Date.now();
    const elapsed = now - this.lastThroughputCalculation;

    if (elapsed >= 1000) {
      for (const [op, opCount] of this.operationCounts.entries()) {
        const opsPerSecond = (opCount / elapsed) * 1000;
        this.recordThroughput(op, opsPerSecond);
      }

      this.operationCounts.clear();
      this.lastThroughputCalculation = now;
    }
  }

  /**
   * Determine if this measurement should be sampled
   *
   * @returns {boolean} True if should sample
   * @private
   */
  _shouldSample() {
    return Math.random() < this.config.samplingRate;
  }

  /**
   * Get current metrics summary
   *
   * @returns {Object} Metrics summary
   */
  getSummary() {
    return {
      enabled: this.config.enabled,
      samplingRate: this.config.samplingRate,
      operationTypes: Array.from(this.operationCounts.keys()),
      lastThroughputCalculation: this.lastThroughputCalculation,
    };
  }
}

/**
 * Create advanced metrics instance
 *
 * @param {Object} [config] - Configuration
 * @returns {AdvancedMetrics} Metrics instance
 */
export function createAdvancedMetrics(config = {}) {
  return new AdvancedMetrics(config);
}

/**
 * Default advanced metrics instance
 */
export const defaultAdvancedMetrics = createAdvancedMetrics();
