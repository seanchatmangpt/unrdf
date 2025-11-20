/**
 * @fileoverview Latency Profiler with Percentile Calculations
 * @module profiling/latency-profiler
 */

import { performance } from 'node:perf_hooks';

/**
 * @typedef {Object} LatencySession
 * @property {string} id - Session ID
 * @property {string} operationName - Operation name
 * @property {number} startTime - Start time in ms
 * @property {number} startMark - Performance mark
 * @property {number[]} measurements - Individual measurements
 */

/**
 * @typedef {Object} LatencyMetrics
 * @property {number} duration - Total duration in ms
 * @property {number} p50 - 50th percentile
 * @property {number} p75 - 75th percentile
 * @property {number} p90 - 90th percentile
 * @property {number} p95 - 95th percentile
 * @property {number} p99 - 99th percentile
 * @property {number} p999 - 99.9th percentile
 * @property {number} min - Minimum latency
 * @property {number} max - Maximum latency
 * @property {number} mean - Mean latency
 * @property {number} stddev - Standard deviation
 * @property {Object} histogram - Histogram buckets
 */

/**
 * Latency Profiler with high-resolution timing
 */
export class LatencyProfiler {
  /**
   *
   */
  constructor() {
    this.activeSessions = new Map();
    this.histogramBuckets = [1, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000];
  }

  /**
   * Start a latency profiling session
   * @param {string} operationName - Operation name
   * @returns {string} Session ID
   */
  start(operationName) {
    const sessionId = `latency-${Date.now()}-${Math.random().toString(36).slice(2, 9)}`;
    const startTime = Date.now();
    const startMark = performance.now();

    const session = {
      id: sessionId,
      operationName,
      startTime,
      startMark,
      measurements: [],
      checkpoints: []
    };

    this.activeSessions.set(sessionId, session);
    return sessionId;
  }

  /**
   * Add a checkpoint measurement
   * @param {string} sessionId - Session ID
   * @param {string} checkpointName - Checkpoint name
   * @returns {number} Time since start in ms
   */
  checkpoint(sessionId, checkpointName) {
    const session = this.activeSessions.get(sessionId);
    if (!session) {
      throw new Error(`Session not found: ${sessionId}`);
    }

    const elapsed = performance.now() - session.startMark;
    session.checkpoints.push({
      name: checkpointName,
      elapsed,
      timestamp: Date.now()
    });

    return elapsed;
  }

  /**
   * Stop profiling and return metrics
   * @param {string} sessionId - Session ID
   * @returns {LatencyMetrics}
   */
  stop(sessionId) {
    const session = this.activeSessions.get(sessionId);
    if (!session) {
      throw new Error(`Session not found: ${sessionId}`);
    }

    const endMark = performance.now();
    const duration = endMark - session.startMark;

    // Add final duration to measurements
    session.measurements.push(duration);

    // Calculate metrics
    const metrics = this.calculateMetrics(session.measurements, duration);

    // Add checkpoint data
    metrics.checkpoints = session.checkpoints;
    metrics.operationName = session.operationName;
    metrics.timestamp = session.startTime;

    // Cleanup
    this.activeSessions.delete(sessionId);

    return metrics;
  }

  /**
   * Calculate latency metrics from measurements
   * @private
   * @param {number[]} measurements - Raw measurements
   * @param {number} finalDuration - Final total duration
   * @returns {LatencyMetrics}
   */
  calculateMetrics(measurements, finalDuration) {
    if (measurements.length === 0) {
      measurements = [finalDuration];
    }

    const sorted = [...measurements].sort((a, b) => a - b);
    const sum = sorted.reduce((a, b) => a + b, 0);
    const mean = sum / sorted.length;

    // Calculate standard deviation
    const variance = sorted.reduce((acc, val) => acc + Math.pow(val - mean, 2), 0) / sorted.length;
    const stddev = Math.sqrt(variance);

    // Calculate percentiles
    const percentiles = {
      p50: this.calculatePercentile(sorted, 50),
      p75: this.calculatePercentile(sorted, 75),
      p90: this.calculatePercentile(sorted, 90),
      p95: this.calculatePercentile(sorted, 95),
      p99: this.calculatePercentile(sorted, 99),
      p999: this.calculatePercentile(sorted, 99.9)
    };

    // Build histogram
    const histogram = this.buildHistogram(sorted);

    return {
      duration: finalDuration,
      ...percentiles,
      min: sorted[0],
      max: sorted[sorted.length - 1],
      mean,
      stddev,
      histogram,
      sampleCount: sorted.length
    };
  }

  /**
   * Calculate percentile from sorted values
   * @private
   * @param {number[]} sortedValues - Sorted array of values
   * @param {number} percentile - Percentile (0-100)
   * @returns {number}
   */
  calculatePercentile(sortedValues, percentile) {
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
   * Build histogram from measurements
   * @private
   * @param {number[]} sortedValues - Sorted measurements
   * @returns {Object} Histogram buckets
   */
  buildHistogram(sortedValues) {
    const histogram = {};

    // Initialize buckets
    this.histogramBuckets.forEach(bucket => {
      histogram[bucket] = 0;
    });
    histogram['inf'] = 0;

    // Fill buckets
    sortedValues.forEach(value => {
      let placed = false;
      for (const bucket of this.histogramBuckets) {
        if (value <= bucket) {
          histogram[bucket]++;
          placed = true;
          break;
        }
      }
      if (!placed) {
        histogram['inf']++;
      }
    });

    return histogram;
  }

  /**
   * Profile a synchronous function
   * @param {string} operationName - Operation name
   * @param {Function} fn - Function to profile
   * @returns {Object} Result and metrics
   */
  profileSync(operationName, fn) {
    const sessionId = this.start(operationName);

    let result;
    let error;

    try {
      result = fn();
    } catch (err) {
      error = err;
    }

    const metrics = this.stop(sessionId);

    if (error) {
      throw error;
    }

    return { result, metrics };
  }

  /**
   * Profile an async function
   * @param {string} operationName - Operation name
   * @param {Function} fn - Async function to profile
   * @returns {Promise<Object>} Result and metrics
   */
  async profileAsync(operationName, fn) {
    const sessionId = this.start(operationName);

    let result;
    let error;

    try {
      result = await fn();
    } catch (err) {
      error = err;
    }

    const metrics = this.stop(sessionId);

    if (error) {
      throw error;
    }

    return { result, metrics };
  }

  /**
   * Check if latency meets performance budget
   * @param {LatencyMetrics} metrics - Latency metrics
   * @param {Object} budget - Performance budget
   * @param {number} [budget.p50] - p50 budget in ms
   * @param {number} [budget.p95] - p95 budget in ms
   * @param {number} [budget.p99] - p99 budget in ms
   * @param {number} [budget.max] - max budget in ms
   * @returns {Object} Budget check results
   */
  checkBudget(metrics, budget) {
    const violations = [];

    if (budget.p50 !== undefined && metrics.p50 > budget.p50) {
      violations.push({
        metric: 'p50',
        actual: metrics.p50,
        budget: budget.p50,
        exceeded: metrics.p50 - budget.p50
      });
    }

    if (budget.p95 !== undefined && metrics.p95 > budget.p95) {
      violations.push({
        metric: 'p95',
        actual: metrics.p95,
        budget: budget.p95,
        exceeded: metrics.p95 - budget.p95
      });
    }

    if (budget.p99 !== undefined && metrics.p99 > budget.p99) {
      violations.push({
        metric: 'p99',
        actual: metrics.p99,
        budget: budget.p99,
        exceeded: metrics.p99 - budget.p99
      });
    }

    if (budget.max !== undefined && metrics.max > budget.max) {
      violations.push({
        metric: 'max',
        actual: metrics.max,
        budget: budget.max,
        exceeded: metrics.max - budget.max
      });
    }

    return {
      passed: violations.length === 0,
      violations
    };
  }
}

/**
 * Quick latency measurement
 * @param {string} operationName - Operation name
 * @param {Function} fn - Function to measure
 * @returns {Promise<Object>} Result and latency
 */
export async function measureLatency(operationName, fn) {
  const profiler = new LatencyProfiler();
  const isAsync = fn.constructor.name === 'AsyncFunction';

  if (isAsync) {
    return await profiler.profileAsync(operationName, fn);
  } else {
    return profiler.profileSync(operationName, fn);
  }
}
