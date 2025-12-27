/**
 * Percentile Calculator for Benchmark Latency Measurements
 *
 * Calculates p50, p95, p99 and other statistical metrics from latency data.
 * Uses simple sorting algorithm - fast enough for typical benchmark runs.
 */

export class PercentileCalculator {
  constructor() {
    this.measurements = [];
  }

  /**
   * Add a latency measurement
   * @param {number} latencyMs - Latency in milliseconds
   */
  addMeasurement(latencyMs) {
    this.measurements.push(latencyMs);
  }

  /**
   * Get the 50th percentile (median)
   * @returns {number} p50 value in milliseconds
   */
  getP50() {
    return this.getPercentile(50);
  }

  /**
   * Get the 95th percentile
   * @returns {number} p95 value in milliseconds
   */
  getP95() {
    return this.getPercentile(95);
  }

  /**
   * Get the 99th percentile
   * @returns {number} p99 value in milliseconds
   */
  getP99() {
    return this.getPercentile(99);
  }

  /**
   * Get arbitrary percentile
   * @param {number} percentile - Percentile value (0-100)
   * @returns {number} Percentile value in milliseconds
   */
  getPercentile(percentile) {
    if (this.measurements.length === 0) {
      return 0;
    }

    const sorted = [...this.measurements].sort((a, b) => a - b);
    const index = Math.ceil((percentile / 100) * sorted.length) - 1;
    return sorted[Math.max(0, index)];
  }

  /**
   * Get mean (average) latency
   * @returns {number} Average latency in milliseconds
   */
  getMean() {
    if (this.measurements.length === 0) {
      return 0;
    }
    return this.measurements.reduce((a, b) => a + b, 0) / this.measurements.length;
  }

  /**
   * Get minimum latency
   * @returns {number} Minimum latency in milliseconds
   */
  getMin() {
    if (this.measurements.length === 0) {
      return 0;
    }
    return Math.min(...this.measurements);
  }

  /**
   * Get maximum latency
   * @returns {number} Maximum latency in milliseconds
   */
  getMax() {
    if (this.measurements.length === 0) {
      return 0;
    }
    return Math.max(...this.measurements);
  }

  /**
   * Get standard deviation
   * @returns {number} Standard deviation in milliseconds
   */
  getStdDev() {
    if (this.measurements.length === 0) {
      return 0;
    }

    const mean = this.getMean();
    const squaredDiffs = this.measurements.map(x => Math.pow(x - mean, 2));
    const variance = squaredDiffs.reduce((a, b) => a + b, 0) / this.measurements.length;
    return Math.sqrt(variance);
  }

  /**
   * Get all statistics as an object
   * @returns {Object} Complete statistics
   */
  getStats() {
    return {
      count: this.measurements.length,
      min: this.getMin(),
      max: this.getMax(),
      mean: this.getMean(),
      stdDev: this.getStdDev(),
      p50: this.getP50(),
      p95: this.getP95(),
      p99: this.getP99()
    };
  }

  /**
   * Reset all measurements
   */
  reset() {
    this.measurements = [];
  }

  /**
   * Get raw measurements
   * @returns {Array<number>} Array of measurements
   */
  getRawMeasurements() {
    return [...this.measurements];
  }
}
