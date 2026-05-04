/**
 * @fileoverview SLA Monitor for Performance Tracking and Enforcement
 * @module sla-monitor
 *
 * @description
 * Tracks and enforces SLA thresholds for distributed operations.
 * Provides latency percentile calculations, error rate tracking,
 * throughput measurement, and OTEL metrics export.
 *
 * **SLA Requirements**:
 * - Latency: <10ms per operation (default)
 * - Error Rate: <0.1% (1 error per 1000 operations)
 *
 * @example
 * ```javascript
 * import { SLAMonitor } from './sla-monitor.mjs';
 *
 * const monitor = new SLAMonitor({ latencyThreshold: 10, errorRate: 0.001 });
 * monitor.recordLatency('sparql_query', 5.2);
 * monitor.recordLatency('sparql_query', 8.1);
 *
 * const metrics = monitor.getMetrics('sparql_query');
 * console.log(metrics.p95); // 95th percentile latency
 *
 * if (!monitor.isWithinSLA('sparql_query')) {
 *   console.warn('SLA violation detected!');
 * }
 * ```
 */

/**
 * Default SLA thresholds
 * @constant {number}
 */
const DEFAULT_LATENCY_THRESHOLD_MS = 10;

/**
 * Default error rate threshold (0.1%)
 * @constant {number}
 */
const DEFAULT_ERROR_RATE = 0.001;

/**
 * Default sliding window size for latency samples
 * @constant {number}
 */
const DEFAULT_WINDOW_SIZE = 1000;

/**
 * @typedef {Object} SLAMonitorOptions
 * @property {number} [latencyThreshold=10] - Maximum acceptable latency in ms
 * @property {number} [errorRate=0.001] - Maximum acceptable error rate (0-1)
 * @property {number} [windowSize=1000] - Sliding window size for samples
 */

/**
 * @typedef {Object} OperationMetrics
 * @property {string} operation - Operation name
 * @property {number} sampleCount - Total number of latency samples
 * @property {number} errorCount - Total number of errors
 * @property {number} successCount - Total successful operations
 * @property {number} errorRate - Current error rate (0-1)
 * @property {LatencyStats} latency - Latency statistics
 * @property {ThroughputStats} throughput - Throughput statistics
 * @property {boolean} withinSLA - Whether operation meets SLA
 */

/**
 * @typedef {Object} LatencyStats
 * @property {number} avg - Average latency in ms
 * @property {number} min - Minimum latency in ms
 * @property {number} max - Maximum latency in ms
 * @property {number} p50 - 50th percentile (median)
 * @property {number} p95 - 95th percentile
 * @property {number} p99 - 99th percentile
 * @property {number} stddev - Standard deviation
 */

/**
 * @typedef {Object} ThroughputStats
 * @property {number} operationsPerSecond - Current ops/sec
 * @property {number} windowStart - Window start timestamp
 * @property {number} totalOperations - Total operations in window
 */

/**
 * @typedef {Object} SLAViolation
 * @property {string} operation - Operation name
 * @property {string} type - Violation type ('latency' | 'error_rate')
 * @property {string} message - Human-readable violation message
 * @property {number} actual - Actual value
 * @property {number} threshold - Threshold value
 * @property {number} timestamp - Violation timestamp
 */

/**
 * @typedef {Object} SLAReport
 * @property {string} generatedAt - ISO timestamp
 * @property {Object} thresholds - Current thresholds
 * @property {Object.<string, OperationMetrics>} operations - Per-operation metrics
 * @property {Object} summary - Overall summary
 * @property {SLAViolation[]} violations - List of violations
 * @property {boolean} compliant - Overall SLA compliance
 */

/**
 * Internal structure for tracking operation metrics
 * @typedef {Object} OperationData
 * @property {number[]} latencies - Sliding window of latency samples
 * @property {number} errorCount - Total errors
 * @property {number} successCount - Total successes
 * @property {number} windowStart - Throughput window start
 * @property {number} windowOperations - Operations in current window
 * @property {number} lastRecordTime - Last record timestamp
 */

/**
 * SLA Monitor for tracking and enforcing performance thresholds
 *
 * Tracks latency, error rates, and throughput for operations.
 * Provides percentile calculations and SLA violation detection.
 *
 * @class
 */
export class SLAMonitor {
  /**
   * Create a new SLA Monitor
   * @param {SLAMonitorOptions} [options={}] - Configuration options
   */
  constructor(options = {}) {
    /**
     * Maximum acceptable latency in milliseconds
     * @type {number}
     * @private
     */
    this._latencyThreshold = options.latencyThreshold ?? DEFAULT_LATENCY_THRESHOLD_MS;

    /**
     * Maximum acceptable error rate (0-1)
     * @type {number}
     * @private
     */
    this._errorRateThreshold = options.errorRate ?? DEFAULT_ERROR_RATE;

    /**
     * Sliding window size for latency samples
     * @type {number}
     * @private
     */
    this._windowSize = options.windowSize ?? DEFAULT_WINDOW_SIZE;

    /**
     * Per-operation metrics storage
     * @type {Map<string, OperationData>}
     * @private
     */
    this._operations = new Map();

    /**
     * Recorded violations
     * @type {SLAViolation[]}
     * @private
     */
    this._violations = [];

    /**
     * Throughput window duration in ms (1 second)
     * @type {number}
     * @private
     */
    this._throughputWindow = 1000;
  }

  /**
   * Get or create operation data
   * @param {string} operation - Operation name
   * @returns {OperationData} Operation data
   * @private
   */
  _getOperationData(operation) {
    if (!this._operations.has(operation)) {
      this._operations.set(operation, {
        latencies: [],
        errorCount: 0,
        successCount: 0,
        windowStart: Date.now(),
        windowOperations: 0,
        lastRecordTime: 0,
      });
    }
    return this._operations.get(operation);
  }

  /**
   * Record a latency measurement for an operation
   *
   * @param {string} operation - Operation name (e.g., 'sparql_query', 'triple_insert')
   * @param {number} milliseconds - Latency in milliseconds
   * @throws {Error} If operation is not a string or milliseconds is not a number
   *
   * @example
   * ```javascript
   * monitor.recordLatency('sparql_query', 5.2);
   * monitor.recordLatency('triple_insert', 0.8);
   * ```
   */
  recordLatency(operation, milliseconds) {
    if (typeof operation !== 'string' || operation.length === 0) {
      throw new Error(`Operation must be a non-empty string, got: ${typeof operation}`);
    }
    if (typeof milliseconds !== 'number' || milliseconds < 0 || Number.isNaN(milliseconds)) {
      throw new Error(`Milliseconds must be a non-negative number, got: ${milliseconds}`);
    }

    const data = this._getOperationData(operation);
    const now = Date.now();

    // Add to sliding window
    data.latencies.push(milliseconds);

    // Trim to window size
    if (data.latencies.length > this._windowSize) {
      data.latencies.shift();
    }

    // Update success count
    data.successCount++;

    // Update throughput window
    this._updateThroughputWindow(data, now);
    data.windowOperations++;
    data.lastRecordTime = now;

    // Check for latency violation
    if (milliseconds > this._latencyThreshold) {
      this._recordViolation(operation, 'latency', milliseconds, this._latencyThreshold);
    }
  }

  /**
   * Record an error for an operation
   *
   * @param {string} operation - Operation name
   * @param {Error|string} error - Error object or message
   *
   * @example
   * ```javascript
   * monitor.recordError('sparql_query', new Error('Query timeout'));
   * monitor.recordError('rpc_call', 'Connection refused');
   * ```
   */
  recordError(operation, error) {
    if (typeof operation !== 'string' || operation.length === 0) {
      throw new Error(`Operation must be a non-empty string, got: ${typeof operation}`);
    }

    const data = this._getOperationData(operation);
    const now = Date.now();

    data.errorCount++;

    // Update throughput window
    this._updateThroughputWindow(data, now);
    data.windowOperations++;
    data.lastRecordTime = now;

    // Check for error rate violation
    const totalOps = data.successCount + data.errorCount;
    const errorRate = totalOps > 0 ? data.errorCount / totalOps : 0;

    if (errorRate > this._errorRateThreshold && totalOps >= 10) {
      this._recordViolation(operation, 'error_rate', errorRate, this._errorRateThreshold);
    }
  }

  /**
   * Update throughput window
   * @param {OperationData} data - Operation data
   * @param {number} now - Current timestamp
   * @private
   */
  _updateThroughputWindow(data, now) {
    if (now - data.windowStart > this._throughputWindow) {
      data.windowStart = now;
      data.windowOperations = 0;
    }
  }

  /**
   * Record an SLA violation
   * @param {string} operation - Operation name
   * @param {string} type - Violation type
   * @param {number} actual - Actual value
   * @param {number} threshold - Threshold value
   * @private
   */
  _recordViolation(operation, type, actual, threshold) {
    const violation = {
      operation,
      type,
      actual,
      threshold,
      timestamp: Date.now(),
      message:
        type === 'latency'
          ? `Latency ${actual.toFixed(2)}ms exceeds ${threshold}ms threshold`
          : `Error rate ${(actual * 100).toFixed(2)}% exceeds ${(threshold * 100).toFixed(1)}% threshold`,
    };

    this._violations.push(violation);
  }

  /**
   * Calculate percentile from sorted values
   * @param {number[]} sortedValues - Sorted array of values
   * @param {number} percentile - Percentile (0-100)
   * @returns {number} Percentile value
   * @private
   */
  _calculatePercentile(sortedValues, percentile) {
    if (sortedValues.length === 0) return 0;
    if (sortedValues.length === 1) return sortedValues[0];

    const index = (percentile / 100) * (sortedValues.length - 1);
    const lower = Math.floor(index);
    const upper = Math.ceil(index);
    const weight = index - lower;

    if (lower === upper) {
      return sortedValues[lower];
    }

    return sortedValues[lower] * (1 - weight) + sortedValues[upper] * weight;
  }

  /**
   * Calculate standard deviation
   * @param {number[]} values - Array of values
   * @param {number} mean - Mean value
   * @returns {number} Standard deviation
   * @private
   */
  _calculateStdDev(values, mean) {
    if (values.length < 2) return 0;

    const variance = values.reduce((acc, val) => acc + Math.pow(val - mean, 2), 0) / values.length;
    return Math.sqrt(variance);
  }

  /**
   * Get metrics for a specific operation
   *
   * @param {string} operation - Operation name
   * @returns {OperationMetrics|null} Metrics or null if operation not found
   *
   * @example
   * ```javascript
   * const metrics = monitor.getMetrics('sparql_query');
   * console.log(`P95 latency: ${metrics.latency.p95}ms`);
   * console.log(`Error rate: ${(metrics.errorRate * 100).toFixed(2)}%`);
   * ```
   */
  getMetrics(operation) {
    const data = this._operations.get(operation);
    if (!data) {
      return null;
    }

    const totalOps = data.successCount + data.errorCount;
    const errorRate = totalOps > 0 ? data.errorCount / totalOps : 0;

    // Calculate latency stats
    let latency = {
      avg: 0,
      min: 0,
      max: 0,
      p50: 0,
      p95: 0,
      p99: 0,
      stddev: 0,
    };

    if (data.latencies.length > 0) {
      const sorted = [...data.latencies].sort((a, b) => a - b);
      const sum = sorted.reduce((a, b) => a + b, 0);
      const avg = sum / sorted.length;

      latency = {
        avg,
        min: sorted[0],
        max: sorted[sorted.length - 1],
        p50: this._calculatePercentile(sorted, 50),
        p95: this._calculatePercentile(sorted, 95),
        p99: this._calculatePercentile(sorted, 99),
        stddev: this._calculateStdDev(sorted, avg),
      };
    }

    // Calculate throughput
    const now = Date.now();
    const windowDuration = now - data.windowStart;
    const opsPerSecond =
      windowDuration > 0 ? (data.windowOperations / windowDuration) * 1000 : data.windowOperations;

    const throughput = {
      operationsPerSecond: opsPerSecond,
      windowStart: data.windowStart,
      totalOperations: totalOps,
    };

    // Check SLA compliance
    const withinSLA =
      latency.p95 <= this._latencyThreshold && errorRate <= this._errorRateThreshold;

    return {
      operation,
      sampleCount: data.latencies.length,
      errorCount: data.errorCount,
      successCount: data.successCount,
      errorRate,
      latency,
      throughput,
      withinSLA,
    };
  }

  /**
   * Get metrics for all tracked operations
   *
   * @returns {Map<string, OperationMetrics>} Map of operation name to metrics
   *
   * @example
   * ```javascript
   * const allMetrics = monitor.getAllMetrics();
   * for (const [op, metrics] of allMetrics) {
   *   console.log(`${op}: ${metrics.latency.p95}ms`);
   * }
   * ```
   */
  getAllMetrics() {
    const results = new Map();
    for (const operation of this._operations.keys()) {
      results.set(operation, this.getMetrics(operation));
    }
    return results;
  }

  /**
   * Check if operation is within SLA thresholds
   *
   * @param {string} operation - Operation name
   * @returns {boolean} True if within SLA, false otherwise
   *
   * @example
   * ```javascript
   * if (!monitor.isWithinSLA('sparql_query')) {
   *   console.warn('SPARQL queries are violating SLA!');
   * }
   * ```
   */
  isWithinSLA(operation) {
    const metrics = this.getMetrics(operation);
    if (!metrics) {
      return true; // No data means no violations
    }
    return metrics.withinSLA;
  }

  /**
   * Get list of SLA violations
   *
   * @returns {SLAViolation[]} Array of recorded violations
   *
   * @example
   * ```javascript
   * const violations = monitor.getViolations();
   * for (const v of violations) {
   *   console.log(`${v.operation}: ${v.message}`);
   * }
   * ```
   */
  getViolations() {
    return [...this._violations];
  }

  /**
   * Generate a text-format SLA report
   *
   * @returns {string} Human-readable SLA report
   *
   * @example
   * ```javascript
   * const report = monitor.generateReport();
   * console.log(report);
   * ```
   */
  generateReport() {
    const lines = [];
    const timestamp = new Date().toISOString();

    lines.push('='.repeat(70));
    lines.push('SLA COMPLIANCE REPORT');
    lines.push(`Generated: ${timestamp}`);
    lines.push('='.repeat(70));
    lines.push('');

    // Thresholds
    lines.push('THRESHOLDS:');
    lines.push(`  Latency (P95): <= ${this._latencyThreshold}ms`);
    lines.push(`  Error Rate:    <= ${(this._errorRateThreshold * 100).toFixed(1)}%`);
    lines.push('');

    // Per-operation metrics
    lines.push('-'.repeat(70));
    lines.push('OPERATION METRICS:');
    lines.push('-'.repeat(70));
    lines.push('');

    let overallCompliant = true;
    const allMetrics = this.getAllMetrics();

    if (allMetrics.size === 0) {
      lines.push('  No operations recorded.');
    } else {
      for (const [operation, metrics] of allMetrics) {
        const status = metrics.withinSLA ? '[PASS]' : '[FAIL]';
        overallCompliant = overallCompliant && metrics.withinSLA;

        lines.push(`${status} ${operation}`);
        lines.push(`  Samples:      ${metrics.sampleCount}`);
        lines.push(`  Successes:    ${metrics.successCount}`);
        lines.push(`  Errors:       ${metrics.errorCount}`);
        lines.push(`  Error Rate:   ${(metrics.errorRate * 100).toFixed(2)}%`);
        lines.push(`  Latency:`);
        lines.push(`    Avg:  ${metrics.latency.avg.toFixed(2)}ms`);
        lines.push(`    P50:  ${metrics.latency.p50.toFixed(2)}ms`);
        lines.push(`    P95:  ${metrics.latency.p95.toFixed(2)}ms`);
        lines.push(`    P99:  ${metrics.latency.p99.toFixed(2)}ms`);
        lines.push(`    Max:  ${metrics.latency.max.toFixed(2)}ms`);
        lines.push(`  Throughput:   ${metrics.throughput.operationsPerSecond.toFixed(1)} ops/sec`);
        lines.push('');
      }
    }

    // Violations summary
    lines.push('-'.repeat(70));
    lines.push('VIOLATIONS:');
    lines.push('-'.repeat(70));

    if (this._violations.length === 0) {
      lines.push('  No violations recorded.');
    } else {
      for (const v of this._violations.slice(-20)) {
        // Last 20 violations
        const time = new Date(v.timestamp).toISOString();
        lines.push(`  [${time}] ${v.operation}: ${v.message}`);
      }
      if (this._violations.length > 20) {
        lines.push(`  ... and ${this._violations.length - 20} more violations`);
      }
    }

    lines.push('');
    lines.push('='.repeat(70));
    lines.push(`OVERALL STATUS: ${overallCompliant ? 'COMPLIANT' : 'NON-COMPLIANT'}`);
    lines.push('='.repeat(70));

    return lines.join('\n');
  }

  /**
   * Export metrics in JSON format for external monitoring systems
   *
   * @returns {SLAReport} JSON-serializable report object
   *
   * @example
   * ```javascript
   * const json = monitor.exportMetrics();
   * // Send to OTEL collector or monitoring system
   * await fetch('/metrics', {
   *   method: 'POST',
   *   body: JSON.stringify(json)
   * });
   * ```
   */
  exportMetrics() {
    const allMetrics = this.getAllMetrics();
    const operations = {};

    let totalSamples = 0;
    let totalErrors = 0;
    let totalSuccesses = 0;
    let overallCompliant = true;

    for (const [operation, metrics] of allMetrics) {
      operations[operation] = metrics;
      totalSamples += metrics.sampleCount;
      totalErrors += metrics.errorCount;
      totalSuccesses += metrics.successCount;
      overallCompliant = overallCompliant && metrics.withinSLA;
    }

    const totalOps = totalSuccesses + totalErrors;
    const overallErrorRate = totalOps > 0 ? totalErrors / totalOps : 0;

    return {
      generatedAt: new Date().toISOString(),
      thresholds: {
        latencyMs: this._latencyThreshold,
        errorRate: this._errorRateThreshold,
      },
      operations,
      summary: {
        operationCount: allMetrics.size,
        totalSamples,
        totalSuccesses,
        totalErrors,
        overallErrorRate,
        violationCount: this._violations.length,
      },
      violations: this._violations,
      compliant: overallCompliant,
    };
  }

  /**
   * Export metrics in OTEL-compatible format
   *
   * @returns {Object} OTEL metrics format
   *
   * @example
   * ```javascript
   * const otelMetrics = monitor.exportOTELMetrics();
   * // Can be used with OTEL collector
   * ```
   */
  exportOTELMetrics() {
    const allMetrics = this.getAllMetrics();
    const metrics = [];

    for (const [operation, data] of allMetrics) {
      // Latency histogram
      metrics.push({
        name: 'sla_operation_latency_ms',
        type: 'histogram',
        attributes: { operation },
        value: {
          sum: data.latency.avg * data.sampleCount,
          count: data.sampleCount,
          min: data.latency.min,
          max: data.latency.max,
        },
        timestamp: Date.now(),
      });

      // Error rate gauge
      metrics.push({
        name: 'sla_operation_error_rate',
        type: 'gauge',
        attributes: { operation },
        value: data.errorRate,
        timestamp: Date.now(),
      });

      // Throughput counter
      metrics.push({
        name: 'sla_operation_throughput',
        type: 'counter',
        attributes: { operation },
        value: data.throughput.totalOperations,
        timestamp: Date.now(),
      });

      // SLA compliance gauge (1 = compliant, 0 = violation)
      metrics.push({
        name: 'sla_compliance',
        type: 'gauge',
        attributes: { operation },
        value: data.withinSLA ? 1 : 0,
        timestamp: Date.now(),
      });

      // Percentiles as gauges
      metrics.push({
        name: 'sla_latency_p50_ms',
        type: 'gauge',
        attributes: { operation },
        value: data.latency.p50,
        timestamp: Date.now(),
      });

      metrics.push({
        name: 'sla_latency_p95_ms',
        type: 'gauge',
        attributes: { operation },
        value: data.latency.p95,
        timestamp: Date.now(),
      });

      metrics.push({
        name: 'sla_latency_p99_ms',
        type: 'gauge',
        attributes: { operation },
        value: data.latency.p99,
        timestamp: Date.now(),
      });
    }

    return {
      resource: {
        service: { name: 'unrdf-sla-monitor' },
      },
      metrics,
    };
  }

  /**
   * Reset all metrics (useful for testing)
   *
   * @example
   * ```javascript
   * monitor.reset();
   * // All metrics cleared
   * ```
   */
  reset() {
    this._operations.clear();
    this._violations = [];
  }

  /**
   * Reset metrics for a specific operation
   *
   * @param {string} operation - Operation name to reset
   *
   * @example
   * ```javascript
   * monitor.resetOperation('sparql_query');
   * ```
   */
  resetOperation(operation) {
    this._operations.delete(operation);
    this._violations = this._violations.filter(v => v.operation !== operation);
  }

  /**
   * Get current thresholds
   *
   * @returns {Object} Current threshold values
   */
  getThresholds() {
    return {
      latencyMs: this._latencyThreshold,
      errorRate: this._errorRateThreshold,
      windowSize: this._windowSize,
    };
  }

  /**
   * Update thresholds dynamically
   *
   * @param {Object} thresholds - New threshold values
   * @param {number} [thresholds.latencyMs] - New latency threshold in ms
   * @param {number} [thresholds.errorRate] - New error rate threshold (0-1)
   *
   * @example
   * ```javascript
   * monitor.setThresholds({ latencyMs: 5, errorRate: 0.0005 });
   * ```
   */
  setThresholds(thresholds) {
    if (thresholds.latencyMs !== undefined) {
      if (typeof thresholds.latencyMs !== 'number' || thresholds.latencyMs <= 0) {
        throw new Error('latencyMs must be a positive number');
      }
      this._latencyThreshold = thresholds.latencyMs;
    }

    if (thresholds.errorRate !== undefined) {
      if (
        typeof thresholds.errorRate !== 'number' ||
        thresholds.errorRate < 0 ||
        thresholds.errorRate > 1
      ) {
        throw new Error('errorRate must be a number between 0 and 1');
      }
      this._errorRateThreshold = thresholds.errorRate;
    }
  }
}

/**
 * Default SLA monitor instance with standard thresholds
 * @type {SLAMonitor}
 */
export const defaultSLAMonitor = new SLAMonitor();

/**
 * Create a new SLA monitor with custom options
 *
 * @param {SLAMonitorOptions} [options={}] - Configuration options
 * @returns {SLAMonitor} New monitor instance
 *
 * @example
 * ```javascript
 * import { createSLAMonitor } from './sla-monitor.mjs';
 *
 * const strictMonitor = createSLAMonitor({
 *   latencyThreshold: 5,
 *   errorRate: 0.0001
 * });
 * ```
 */
export function createSLAMonitor(options = {}) {
  return new SLAMonitor(options);
}

/**
 * Operation types for standard tracking
 * @constant {Object}
 */
export const OPERATION_TYPES = {
  TRIPLE_INSERT: 'triple_insert',
  SPARQL_QUERY: 'sparql_query',
  RPC_CALL: 'rpc_call',
  VALIDATION: 'validation',
  CACHE_HIT: 'cache_hit',
  CACHE_MISS: 'cache_miss',
  STREAMING: 'streaming',
};
