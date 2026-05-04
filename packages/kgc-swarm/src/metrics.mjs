/**
 * @file metrics.mjs
 * @description Performance metrics collection for KGC-SWARM
 * Tracks: drift history, budget consumption, epoch count, throughput, latency, memory
 * Observable expansion rate: |∂ℒ_τ|
 */

import { z } from 'zod';

/**
 * Performance sample schema
 * @type {z.ZodObject}
 */
export const PerformanceSampleSchema = z.object({
  timestamp: z.number(),
  throughput: z.number().nonnegative().optional(), // ops/sec
  latency: z.number().nonnegative().optional(), // ms
  memoryUsed: z.number().nonnegative().optional(), // bytes
  cpuUsage: z.number().min(0).max(100).optional(), // percentage
});

/**
 * Expansion rate measurement
 * @typedef {Object} ExpansionRate
 * @property {number} timestamp - Measurement timestamp
 * @property {number} rate - |∂ℒ_τ| - artifacts/epoch
 * @property {number} totalArtifacts - |ℒ_τ|
 * @property {number} deltaArtifacts - ΔA_τ
 */

/**
 * Metrics summary statistics
 * @typedef {Object} MetricsSummary
 * @property {number} mean - Average value
 * @property {number} min - Minimum value
 * @property {number} max - Maximum value
 * @property {number} stdDev - Standard deviation
 * @property {number} p50 - 50th percentile (median)
 * @property {number} p95 - 95th percentile
 * @property {number} p99 - 99th percentile
 */

/**
 * MetricsCollector: Comprehensive metrics collection and analysis for KGC-SWARM
 *
 * @class
 * @example
 * const collector = new MetricsCollector();
 * collector.recordDrift({ drift: 5, normalized: 0.05, timestamp: Date.now() });
 * collector.recordPerformance({ throughput: 100, latency: 50 });
 * const summary = collector.getSummary();
 */
export class MetricsCollector {
  constructor() {
    /** @type {Array<{drift: number, normalized: number, timestamp: number}>} */
    this.driftHistory = [];

    /** @type {Array<{step: number, time: number, bytes: number, networkOps: number}>} */
    this.budgetConsumption = [];

    /** @type {Array<PerformanceSample>} */
    this.performanceSamples = [];

    /** @type {ExpansionRate[]} */
    this.expansionRates = [];

    this.epochCount = 0;
    this.startTime = Date.now();
    this.totalOperations = 0;
    this.totalLatency = 0;

    /** @type {Map<string, number>} */
    this.customMetrics = new Map();
  }

  /**
   * Record drift measurement
   *
   * @param {Object} driftResult - Drift calculation result
   * @param {number} driftResult.drift - Absolute drift
   * @param {number} driftResult.normalized - Normalized drift
   * @param {number} [driftResult.timestamp] - Optional timestamp
   */
  recordDrift(driftResult) {
    this.driftHistory.push({
      drift: driftResult.drift,
      normalized: driftResult.normalized,
      timestamp: driftResult.timestamp || Date.now(),
      added: driftResult.added,
      removed: driftResult.removed,
      modified: driftResult.modified,
    });
  }

  /**
   * Record budget consumption snapshot
   *
   * @param {Object} budget - Current budget usage
   * @param {number} budget.step - Step count
   * @param {number} budget.time - Time elapsed (ms)
   * @param {number} budget.bytes - Bytes used
   * @param {number} budget.networkOps - Network operations
   */
  recordBudget(budget) {
    this.budgetConsumption.push({
      step: budget.step,
      time: budget.time,
      bytes: budget.bytes,
      networkOps: budget.networkOps,
      timestamp: Date.now(),
    });
  }

  /**
   * Record performance sample
   *
   * @param {Object} sample - Performance measurement
   * @param {number} [sample.throughput] - Operations per second
   * @param {number} [sample.latency] - Latency in ms
   * @param {number} [sample.memoryUsed] - Memory usage in bytes
   * @param {number} [sample.cpuUsage] - CPU usage percentage
   */
  recordPerformance(sample) {
    const perfSample = {
      timestamp: Date.now(),
      ...sample,
    };

    PerformanceSampleSchema.parse(perfSample);
    this.performanceSamples.push(perfSample);

    if (sample.latency !== undefined) {
      this.totalOperations++;
      this.totalLatency += sample.latency;
    }
  }

  /**
   * Record expansion rate: |∂ℒ_τ|
   *
   * @param {number} totalArtifacts - |ℒ_τ|
   * @param {number} deltaArtifacts - ΔA_τ (change from previous epoch)
   */
  recordExpansionRate(totalArtifacts, deltaArtifacts) {
    const timestamp = Date.now();
    const elapsed = timestamp - this.startTime;
    const rate = elapsed > 0 ? (deltaArtifacts / elapsed) * 1000 : 0; // artifacts/sec

    this.expansionRates.push({
      timestamp,
      rate,
      totalArtifacts,
      deltaArtifacts,
    });
  }

  /**
   * Increment epoch counter
   */
  incrementEpoch() {
    this.epochCount++;
  }

  /**
   * Record custom metric
   *
   * @param {string} name - Metric name
   * @param {number} value - Metric value
   */
  recordCustom(name, value) {
    this.customMetrics.set(name, value);
  }

  /**
   * Calculate summary statistics for a numeric array
   *
   * @param {number[]} values - Array of numbers
   * @returns {MetricsSummary} Summary statistics
   */
  calculateStats(values) {
    if (values.length === 0) {
      return { mean: 0, min: 0, max: 0, stdDev: 0, p50: 0, p95: 0, p99: 0 };
    }

    const sorted = [...values].sort((a, b) => a - b);
    const mean = values.reduce((sum, v) => sum + v, 0) / values.length;

    const variance = values.reduce((sum, v) => sum + Math.pow(v - mean, 2), 0) / values.length;
    const stdDev = Math.sqrt(variance);

    const percentile = (p) => {
      const index = Math.ceil((p / 100) * sorted.length) - 1;
      return sorted[Math.max(0, index)];
    };

    return {
      mean,
      min: sorted[0],
      max: sorted[sorted.length - 1],
      stdDev,
      p50: percentile(50),
      p95: percentile(95),
      p99: percentile(99),
    };
  }

  /**
   * Get drift statistics
   *
   * @returns {Object} Drift summary
   */
  getDriftStats() {
    if (this.driftHistory.length === 0) {
      return null;
    }

    const normalizedDrifts = this.driftHistory.map(d => d.normalized);
    const absoluteDrifts = this.driftHistory.map(d => d.drift);

    return {
      normalized: this.calculateStats(normalizedDrifts),
      absolute: this.calculateStats(absoluteDrifts),
      trend: this.calculateDriftTrend(),
      history: this.driftHistory,
    };
  }

  /**
   * Calculate drift trend (increasing/decreasing/stable)
   *
   * @returns {string} Trend indicator
   */
  calculateDriftTrend() {
    if (this.driftHistory.length < 3) {
      return 'insufficient-data';
    }

    const recent = this.driftHistory.slice(-3).map(d => d.normalized);
    const slope = (recent[2] - recent[0]) / 2;

    if (Math.abs(slope) < 0.001) return 'stable';
    return slope > 0 ? 'increasing' : 'decreasing';
  }

  /**
   * Get performance statistics
   *
   * @returns {Object} Performance summary
   */
  getPerformanceStats() {
    if (this.performanceSamples.length === 0) {
      return null;
    }

    const throughputs = this.performanceSamples
      .filter(s => s.throughput !== undefined)
      .map(s => s.throughput);

    const latencies = this.performanceSamples
      .filter(s => s.latency !== undefined)
      .map(s => s.latency);

    const memoryUsages = this.performanceSamples
      .filter(s => s.memoryUsed !== undefined)
      .map(s => s.memoryUsed);

    const cpuUsages = this.performanceSamples
      .filter(s => s.cpuUsage !== undefined)
      .map(s => s.cpuUsage);

    return {
      throughput: throughputs.length > 0 ? this.calculateStats(throughputs) : null,
      latency: latencies.length > 0 ? this.calculateStats(latencies) : null,
      memory: memoryUsages.length > 0 ? this.calculateStats(memoryUsages) : null,
      cpu: cpuUsages.length > 0 ? this.calculateStats(cpuUsages) : null,
      avgLatency: this.totalOperations > 0 ? this.totalLatency / this.totalOperations : 0,
    };
  }

  /**
   * Get budget consumption statistics
   *
   * @returns {Object} Budget summary
   */
  getBudgetStats() {
    if (this.budgetConsumption.length === 0) {
      return null;
    }

    const latest = this.budgetConsumption[this.budgetConsumption.length - 1];
    const timeUsages = this.budgetConsumption.map(b => b.time);
    const bytesUsages = this.budgetConsumption.map(b => b.bytes);
    const networkOpsUsages = this.budgetConsumption.map(b => b.networkOps);

    return {
      current: latest,
      time: this.calculateStats(timeUsages),
      bytes: this.calculateStats(bytesUsages),
      networkOps: this.calculateStats(networkOpsUsages),
      history: this.budgetConsumption,
    };
  }

  /**
   * Get expansion rate statistics
   *
   * @returns {Object} Expansion rate summary
   */
  getExpansionStats() {
    if (this.expansionRates.length === 0) {
      return null;
    }

    const rates = this.expansionRates.map(e => e.rate);
    const latest = this.expansionRates[this.expansionRates.length - 1];

    return {
      current: latest,
      stats: this.calculateStats(rates),
      history: this.expansionRates,
    };
  }

  /**
   * Get comprehensive metrics summary
   *
   * @returns {Object} Complete metrics summary
   */
  getSummary() {
    const elapsed = Date.now() - this.startTime;

    return {
      epochCount: this.epochCount,
      elapsed,
      drift: this.getDriftStats(),
      performance: this.getPerformanceStats(),
      budget: this.getBudgetStats(),
      expansion: this.getExpansionStats(),
      custom: Object.fromEntries(this.customMetrics),
    };
  }

  /**
   * Export metrics as JSON
   *
   * @returns {string} JSON representation
   */
  toJSON() {
    return JSON.stringify(this.getSummary(), null, 2);
  }

  /**
   * Reset all metrics
   */
  reset() {
    this.driftHistory = [];
    this.budgetConsumption = [];
    this.performanceSamples = [];
    this.expansionRates = [];
    this.epochCount = 0;
    this.startTime = Date.now();
    this.totalOperations = 0;
    this.totalLatency = 0;
    this.customMetrics.clear();
  }

  /**
   * Get metrics snapshot for specific time range
   *
   * @param {number} startTime - Start timestamp
   * @param {number} endTime - End timestamp
   * @returns {Object} Filtered metrics
   */
  getSnapshot(startTime, endTime) {
    const filterByTime = (arr) => arr.filter(
      item => item.timestamp >= startTime && item.timestamp <= endTime
    );

    return {
      driftHistory: filterByTime(this.driftHistory),
      performanceSamples: filterByTime(this.performanceSamples),
      expansionRates: filterByTime(this.expansionRates),
      budgetConsumption: filterByTime(this.budgetConsumption),
    };
  }
}
