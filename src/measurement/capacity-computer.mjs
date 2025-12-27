/**
 * @fileoverview Capacity Computer - Compute C_t from admission rates
 *
 * C_t represents the admissible channel capacity - how much change
 * the system can accept per epoch while maintaining invariants.
 *
 * **Core Formula**:
 * C_t = H(admitted_deltas) = -sum(p(delta) * log2(p(delta)))
 *
 * This measures the entropy of admitted changes per partition,
 * indicating how much "information" (change) flows through.
 *
 * **Interpretation**:
 * - High C_t = High change rate, system is active
 * - Low C_t = Low change rate, system is stable/stagnant
 * - C_t spike = Burst of changes
 * - C_t drop = System becoming more restrictive
 *
 * @module measurement/capacity-computer
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';

/**
 * Admission delta schema
 */
const AdmissionDeltaSchema = z.object({
  epoch: z.string(),
  partition: z.string(),
  decision: z.enum(['allow', 'deny', 'ALLOW', 'DENY']),
  quadCount: z.number().optional(),
  byteSize: z.number().optional(),
  deltaType: z.enum(['additive', 'subtractive', 'mixed']).optional(),
  timestamp: z.string().datetime()
});

/**
 * Capacity result schema
 */
const CapacityResultSchema = z.object({
  epoch: z.string(),
  systemCapacity: z.number(),
  normalizedCapacity: z.number(),
  partitionCapacities: z.record(z.number()),
  utilizationRatio: z.number(),
  admissionRate: z.number(),
  throughput: z.object({
    quadsPerSecond: z.number(),
    deltasPerSecond: z.number()
  }),
  computedAt: z.string().datetime()
});

/**
 * Capacity Computer - Measures change capacity of the system
 *
 * @class CapacityComputer
 * @example
 * const computer = new CapacityComputer();
 * computer.recordDelta({ partition: 'A', decision: 'allow', quadCount: 10, ... });
 * const capacity = computer.computeSystemCapacity();
 */
export class CapacityComputer {
  /**
   * Create a new Capacity Computer
   * @param {Object} [config] - Configuration options
   * @param {number} [config.windowSize=1000] - Sliding window for capacity computation
   * @param {number} [config.epochDuration=60000] - Epoch duration in ms (default 1 minute)
   * @param {number} [config.maxHistorySize=10000] - Maximum delta records to keep
   * @param {number} [config.binCount=20] - Bins for entropy computation
   */
  constructor(config = {}) {
    this.config = {
      windowSize: config.windowSize || 1000,
      epochDuration: config.epochDuration || 60000,
      maxHistorySize: config.maxHistorySize || 10000,
      binCount: config.binCount || 20,
      ...config
    };

    /** @type {Array<Object>} Delta history */
    this.deltaHistory = [];

    /** @type {Map<string, Array<Object>>} Per-partition deltas */
    this.partitionDeltas = new Map();

    /** @type {Array<Object>} Capacity computation history */
    this.capacityHistory = [];

    /** @type {Map<string, number>} Theoretical max capacity per partition */
    this._theoreticalMax = new Map();

    /** @type {number} Start time for throughput calculation */
    this._startTime = Date.now();
  }

  /**
   * Record an admission delta
   *
   * @param {Object} delta - Admission delta event
   * @param {string} delta.epoch - Epoch identifier
   * @param {string} delta.partition - Partition name
   * @param {'allow'|'deny'} delta.decision - Admission decision
   * @param {number} [delta.quadCount] - Number of quads in delta
   * @param {number} [delta.byteSize] - Size in bytes
   * @param {'additive'|'subtractive'|'mixed'} [delta.deltaType] - Type of change
   * @param {string} delta.timestamp - ISO timestamp
   */
  recordDelta(delta) {
    const validated = AdmissionDeltaSchema.parse(delta);
    const enriched = {
      ...validated,
      recordedAt: Date.now(),
      normalizedDecision: validated.decision.toLowerCase()
    };

    // Add to global history
    this.deltaHistory.push(enriched);

    // Add to partition history
    if (!this.partitionDeltas.has(validated.partition)) {
      this.partitionDeltas.set(validated.partition, []);
    }
    this.partitionDeltas.get(validated.partition).push(enriched);

    // Trim histories
    this._trimHistories();
  }

  /**
   * Record multiple deltas
   *
   * @param {Array<Object>} deltas - Array of delta events
   */
  recordDeltas(deltas) {
    for (const delta of deltas) {
      this.recordDelta(delta);
    }
  }

  /**
   * Compute system-wide channel capacity
   *
   * C_system = sum(C_partition) weighted by admission rate
   *
   * @param {Object} [options] - Computation options
   * @returns {Object} System capacity result
   */
  computeSystemCapacity(options = {}) {
    const windowSize = options.windowSize || this.config.windowSize;
    const epoch = this._generateEpoch();

    // Get recent deltas
    const recentDeltas = this.deltaHistory.slice(-windowSize);

    if (recentDeltas.length < 10) {
      return {
        epoch,
        systemCapacity: 0,
        normalizedCapacity: 0,
        partitionCapacities: {},
        utilizationRatio: 0,
        admissionRate: 0,
        throughput: { quadsPerSecond: 0, deltasPerSecond: 0 },
        message: 'Insufficient data for capacity computation',
        computedAt: new Date().toISOString()
      };
    }

    // Compute per-partition capacity
    const partitionCapacities = {};
    const partitions = new Set(recentDeltas.map(d => d.partition));

    for (const partition of partitions) {
      const partitionDeltas = recentDeltas.filter(d => d.partition === partition);
      partitionCapacities[partition] = this._computePartitionCapacity(partitionDeltas);
    }

    // System capacity is entropy of all delta sizes
    const systemCapacity = this._computeEntropyOfDeltas(recentDeltas);

    // Normalized capacity (0-1 based on theoretical max)
    const theoreticalMax = this._computeTheoreticalMax(recentDeltas);
    const normalizedCapacity = theoreticalMax > 0 ? systemCapacity / theoreticalMax : 0;

    // Admission rate
    const admittedCount = recentDeltas.filter(d => d.normalizedDecision === 'allow').length;
    const admissionRate = recentDeltas.length > 0 ? admittedCount / recentDeltas.length : 0;

    // Throughput
    const throughput = this._computeThroughput(recentDeltas);

    // Utilization ratio
    const utilizationRatio = this._computeUtilizationRatio(partitionCapacities);

    const result = {
      epoch,
      systemCapacity,
      normalizedCapacity,
      partitionCapacities,
      utilizationRatio,
      admissionRate,
      throughput,
      windowSize: recentDeltas.length,
      computedAt: new Date().toISOString()
    };

    // Record in history
    this._recordCapacityHistory(result);

    return result;
  }

  /**
   * Compute partition-specific capacity
   *
   * @param {string} partition - Partition name
   * @param {Object} [options] - Computation options
   * @returns {Object} Partition capacity result
   */
  computePartitionCapacity(partition, options = {}) {
    const windowSize = options.windowSize || this.config.windowSize;
    const deltas = this.partitionDeltas.get(partition) || [];
    const recentDeltas = deltas.slice(-windowSize);

    if (recentDeltas.length < 5) {
      return {
        partition,
        capacity: 0,
        normalizedCapacity: 0,
        admissionRate: 0,
        throughput: { quadsPerSecond: 0, deltasPerSecond: 0 },
        message: 'Insufficient data',
        computedAt: new Date().toISOString()
      };
    }

    const capacity = this._computePartitionCapacity(recentDeltas);
    const admittedDeltas = recentDeltas.filter(d => d.normalizedDecision === 'allow');
    const admissionRate = recentDeltas.length > 0
      ? admittedDeltas.length / recentDeltas.length
      : 0;

    // Theoretical max for this partition
    const theoreticalMax = this._computeTheoreticalMax(recentDeltas);
    const normalizedCapacity = theoreticalMax > 0 ? capacity / theoreticalMax : 0;

    const throughput = this._computeThroughput(recentDeltas);

    // Delta type distribution
    const deltaTypes = this._computeDeltaTypeDistribution(recentDeltas);

    return {
      partition,
      capacity,
      normalizedCapacity,
      admissionRate,
      throughput,
      deltaTypes,
      dataPoints: recentDeltas.length,
      computedAt: new Date().toISOString()
    };
  }

  /**
   * Compute capacity trend over time
   *
   * @param {number} [windowSize=10] - Number of measurements
   * @returns {Object} Trend analysis
   */
  getCapacityTrend(windowSize = 10) {
    if (this.capacityHistory.length < 2) {
      return {
        trend: 'insufficient_data',
        dataPoints: this.capacityHistory.length
      };
    }

    const recent = this.capacityHistory.slice(-windowSize);
    const capacities = recent.map(h => h.systemCapacity);

    // Statistics
    const mean = capacities.reduce((a, b) => a + b, 0) / capacities.length;
    const variance = capacities.reduce((sum, c) => sum + Math.pow(c - mean, 2), 0) / capacities.length;
    const stdDev = Math.sqrt(variance);

    // Trend via regression
    const n = capacities.length;
    const xMean = (n - 1) / 2;
    let numerator = 0;
    let denominator = 0;

    for (let i = 0; i < n; i++) {
      numerator += (i - xMean) * (capacities[i] - mean);
      denominator += (i - xMean) * (i - xMean);
    }

    const slope = denominator !== 0 ? numerator / denominator : 0;

    // Classify trend
    let trend;
    const cv = mean > 0 ? (stdDev / mean) * 100 : 0;

    if (cv < 10 && Math.abs(slope) < 0.05) {
      trend = 'stable';
    } else if (slope > 0.1) {
      trend = 'increasing';
    } else if (slope < -0.1) {
      trend = 'decreasing';
    } else if (cv > 30) {
      trend = 'volatile';
    } else {
      trend = 'stable';
    }

    return {
      trend,
      windowSize: recent.length,
      mean,
      stdDev,
      coefficientOfVariation: cv,
      slope,
      min: Math.min(...capacities),
      max: Math.max(...capacities),
      latest: capacities[capacities.length - 1],
      first: capacities[0],
      interpretation: this._interpretCapacityTrend(trend, mean)
    };
  }

  /**
   * Detect capacity anomalies
   *
   * @param {Object} [options] - Detection options
   * @returns {Object} Anomaly detection result
   */
  detectCapacityAnomalies(options = {}) {
    const threshold = options.threshold || 2.0;

    if (this.capacityHistory.length < 5) {
      return {
        anomalyDetected: false,
        message: 'Insufficient history for anomaly detection',
        historySize: this.capacityHistory.length
      };
    }

    const recent = this.capacityHistory.slice(-20);
    const capacities = recent.map(h => h.systemCapacity);

    // Statistics
    const mean = capacities.reduce((a, b) => a + b, 0) / capacities.length;
    const variance = capacities.reduce((sum, c) => sum + Math.pow(c - mean, 2), 0) / capacities.length;
    const stdDev = Math.sqrt(variance);

    const latest = capacities[capacities.length - 1];
    const zScore = stdDev > 0 ? (latest - mean) / stdDev : 0;

    const anomalyDetected = Math.abs(zScore) > threshold;

    // Check for burst patterns
    const burstDetected = this._detectBurst(capacities);

    // Check for capacity exhaustion
    const exhaustionDetected = this._detectExhaustion(recent);

    const anomalies = [];
    if (anomalyDetected) {
      anomalies.push({
        type: zScore > 0 ? 'capacity_spike' : 'capacity_drop',
        severity: Math.abs(zScore) > 3 ? 'critical' : 'warning',
        zScore,
        latestCapacity: latest,
        mean
      });
    }

    if (burstDetected.detected) {
      anomalies.push({
        type: 'burst_pattern',
        severity: 'warning',
        ...burstDetected
      });
    }

    if (exhaustionDetected.detected) {
      anomalies.push({
        type: 'capacity_exhaustion',
        severity: 'critical',
        ...exhaustionDetected
      });
    }

    return {
      anomalyDetected: anomalies.length > 0,
      anomalies,
      latestCapacity: latest,
      mean,
      stdDev,
      zScore,
      recommendation: this._generateAnomalyRecommendation(anomalies),
      checkedAt: new Date().toISOString()
    };
  }

  /**
   * Compute admission rate over time windows
   *
   * @param {Object} [options] - Rate computation options
   * @returns {Object} Admission rate analysis
   */
  computeAdmissionRates(options = {}) {
    const windows = options.windows || [60000, 300000, 900000]; // 1m, 5m, 15m
    const now = Date.now();
    const rates = {};

    for (const window of windows) {
      const windowStart = now - window;
      const windowDeltas = this.deltaHistory.filter(
        d => d.recordedAt >= windowStart
      );

      const admitted = windowDeltas.filter(d => d.normalizedDecision === 'allow');
      const denied = windowDeltas.filter(d => d.normalizedDecision === 'deny');

      const windowSec = window / 1000;
      const windowLabel = windowSec < 120
        ? `${windowSec}s`
        : `${Math.round(windowSec / 60)}m`;

      rates[windowLabel] = {
        total: windowDeltas.length,
        admitted: admitted.length,
        denied: denied.length,
        admissionRate: windowDeltas.length > 0
          ? admitted.length / windowDeltas.length
          : 0,
        deltasPerSecond: windowDeltas.length / windowSec,
        quadsPerSecond: admitted.reduce((sum, d) => sum + (d.quadCount || 0), 0) / windowSec
      };
    }

    // Compare rates across windows
    const windowLabels = Object.keys(rates);
    const admissionRates = windowLabels.map(w => rates[w].admissionRate);

    let rateStability = 'stable';
    if (admissionRates.length >= 2) {
      const variance = admissionRates.reduce((sum, r, i, arr) => {
        const mean = arr.reduce((a, b) => a + b, 0) / arr.length;
        return sum + Math.pow(r - mean, 2);
      }, 0) / admissionRates.length;

      if (variance > 0.1) {
        const trend = admissionRates[0] > admissionRates[admissionRates.length - 1]
          ? 'improving'
          : 'degrading';
        rateStability = `${trend} (variance: ${variance.toFixed(3)})`;
      }
    }

    return {
      rates,
      windows: windowLabels,
      rateStability,
      computedAt: new Date().toISOString()
    };
  }

  /**
   * Compute capacity forecast
   *
   * @param {number} [horizonEpochs=10] - Number of future epochs to forecast
   * @returns {Object} Capacity forecast
   */
  forecastCapacity(horizonEpochs = 10) {
    if (this.capacityHistory.length < 5) {
      return {
        forecast: null,
        message: 'Insufficient history for forecasting',
        historySize: this.capacityHistory.length
      };
    }

    const recent = this.capacityHistory.slice(-20);
    const capacities = recent.map(h => h.systemCapacity);

    // Simple linear regression
    const n = capacities.length;
    const xMean = (n - 1) / 2;
    const yMean = capacities.reduce((a, b) => a + b, 0) / n;

    let numerator = 0;
    let denominator = 0;
    for (let i = 0; i < n; i++) {
      numerator += (i - xMean) * (capacities[i] - yMean);
      denominator += (i - xMean) * (i - xMean);
    }

    const slope = denominator !== 0 ? numerator / denominator : 0;
    const intercept = yMean - slope * xMean;

    // Forecast future epochs
    const forecast = [];
    const lastIdx = n - 1;

    for (let i = 1; i <= horizonEpochs; i++) {
      const futureIdx = lastIdx + i;
      const predictedCapacity = Math.max(0, intercept + slope * futureIdx);

      forecast.push({
        epochOffset: i,
        predictedCapacity,
        confidenceInterval: {
          low: Math.max(0, predictedCapacity * 0.8),
          high: predictedCapacity * 1.2
        }
      });
    }

    // Trend analysis
    const endCapacity = forecast[forecast.length - 1].predictedCapacity;
    const startCapacity = capacities[capacities.length - 1];
    const changePercent = startCapacity > 0
      ? ((endCapacity - startCapacity) / startCapacity) * 100
      : 0;

    return {
      forecast,
      currentCapacity: startCapacity,
      forecastedCapacity: endCapacity,
      changePercent,
      trend: slope > 0 ? 'increasing' : (slope < 0 ? 'decreasing' : 'stable'),
      slope,
      confidence: this._computeForecastConfidence(capacities),
      computedAt: new Date().toISOString()
    };
  }

  /**
   * Get capacity by delta type
   *
   * @returns {Object} Capacity breakdown by delta type
   */
  getCapacityByDeltaType() {
    const byType = {
      additive: [],
      subtractive: [],
      mixed: [],
      unknown: []
    };

    for (const delta of this.deltaHistory) {
      const type = delta.deltaType || 'unknown';
      if (byType[type]) {
        byType[type].push(delta);
      }
    }

    const capacities = {};
    for (const [type, deltas] of Object.entries(byType)) {
      if (deltas.length >= 5) {
        capacities[type] = {
          count: deltas.length,
          capacity: this._computeEntropyOfDeltas(deltas),
          admissionRate: deltas.filter(d => d.normalizedDecision === 'allow').length / deltas.length,
          avgQuadCount: deltas.reduce((sum, d) => sum + (d.quadCount || 0), 0) / deltas.length
        };
      } else {
        capacities[type] = {
          count: deltas.length,
          capacity: 0,
          admissionRate: 0,
          message: 'Insufficient data'
        };
      }
    }

    return {
      capacities,
      totalDeltas: this.deltaHistory.length,
      computedAt: new Date().toISOString()
    };
  }

  /**
   * Compute partition capacity
   * @private
   */
  _computePartitionCapacity(deltas) {
    if (deltas.length === 0) return 0;

    // Use quad counts as the measure
    const quadCounts = deltas
      .filter(d => d.normalizedDecision === 'allow' && d.quadCount > 0)
      .map(d => d.quadCount);

    if (quadCounts.length === 0) return 0;

    return this._computeEntropy(quadCounts);
  }

  /**
   * Compute entropy of delta distribution
   * @private
   */
  _computeEntropyOfDeltas(deltas) {
    // Extract features for entropy computation
    const features = deltas
      .filter(d => d.normalizedDecision === 'allow')
      .map(d => {
        // Combine multiple features into single value
        const quadScore = d.quadCount ? Math.log2(d.quadCount + 1) : 0;
        const byteScore = d.byteSize ? Math.log2(d.byteSize + 1) : 0;
        return quadScore + byteScore * 0.1;
      });

    if (features.length === 0) return 0;

    return this._computeEntropy(features);
  }

  /**
   * Compute entropy of a numeric array
   * @private
   */
  _computeEntropy(values) {
    if (values.length === 0) return 0;

    const min = Math.min(...values);
    const max = Math.max(...values);
    const range = max - min;

    if (range === 0) return 0;

    // Bin values
    const binSize = range / this.config.binCount;
    const bins = new Map();

    for (const value of values) {
      const binIdx = Math.min(
        Math.floor((value - min) / binSize),
        this.config.binCount - 1
      );
      bins.set(binIdx, (bins.get(binIdx) || 0) + 1);
    }

    // Compute entropy
    const n = values.length;
    let entropy = 0;

    for (const count of bins.values()) {
      const p = count / n;
      if (p > 0) {
        entropy -= p * Math.log2(p);
      }
    }

    return entropy;
  }

  /**
   * Compute theoretical maximum entropy
   * @private
   */
  _computeTheoreticalMax(deltas) {
    // Maximum entropy = log2(number of distinct values)
    const quadCounts = new Set(deltas.map(d => d.quadCount || 0));
    return Math.log2(Math.max(quadCounts.size, this.config.binCount));
  }

  /**
   * Compute throughput metrics
   * @private
   */
  _computeThroughput(deltas) {
    if (deltas.length === 0) {
      return { quadsPerSecond: 0, deltasPerSecond: 0 };
    }

    const timestamps = deltas.map(d => new Date(d.timestamp).getTime());
    const duration = (Math.max(...timestamps) - Math.min(...timestamps)) / 1000;

    if (duration <= 0) {
      return { quadsPerSecond: 0, deltasPerSecond: 0 };
    }

    const admittedDeltas = deltas.filter(d => d.normalizedDecision === 'allow');
    const totalQuads = admittedDeltas.reduce((sum, d) => sum + (d.quadCount || 0), 0);

    return {
      quadsPerSecond: totalQuads / duration,
      deltasPerSecond: admittedDeltas.length / duration
    };
  }

  /**
   * Compute utilization ratio across partitions
   * @private
   */
  _computeUtilizationRatio(partitionCapacities) {
    const capacities = Object.values(partitionCapacities);
    if (capacities.length === 0) return 0;

    const maxCapacity = Math.max(...capacities);
    const avgCapacity = capacities.reduce((a, b) => a + b, 0) / capacities.length;

    return maxCapacity > 0 ? avgCapacity / maxCapacity : 0;
  }

  /**
   * Compute delta type distribution
   * @private
   */
  _computeDeltaTypeDistribution(deltas) {
    const types = { additive: 0, subtractive: 0, mixed: 0, unknown: 0 };

    for (const delta of deltas) {
      const type = delta.deltaType || 'unknown';
      if (types[type] !== undefined) {
        types[type]++;
      }
    }

    const total = deltas.length;
    return {
      counts: types,
      ratios: {
        additive: total > 0 ? types.additive / total : 0,
        subtractive: total > 0 ? types.subtractive / total : 0,
        mixed: total > 0 ? types.mixed / total : 0,
        unknown: total > 0 ? types.unknown / total : 0
      }
    };
  }

  /**
   * Detect burst patterns in capacity
   * @private
   */
  _detectBurst(capacities) {
    if (capacities.length < 5) {
      return { detected: false };
    }

    // Look for sudden spikes
    const diffs = [];
    for (let i = 1; i < capacities.length; i++) {
      diffs.push(Math.abs(capacities[i] - capacities[i - 1]));
    }

    const avgDiff = diffs.reduce((a, b) => a + b, 0) / diffs.length;
    const maxDiff = Math.max(...diffs);

    const burstDetected = maxDiff > avgDiff * 3;

    return {
      detected: burstDetected,
      maxChange: maxDiff,
      avgChange: avgDiff,
      burstRatio: avgDiff > 0 ? maxDiff / avgDiff : 0
    };
  }

  /**
   * Detect capacity exhaustion
   * @private
   */
  _detectExhaustion(historyRecords) {
    if (historyRecords.length < 3) {
      return { detected: false };
    }

    // Check if admission rate is declining
    const admissionRates = historyRecords.map(h => h.admissionRate);
    const recent = admissionRates.slice(-5);

    // Check for consistently low admission rate
    const avgRate = recent.reduce((a, b) => a + b, 0) / recent.length;
    const declining = recent.every((r, i) => i === 0 || r <= recent[i - 1]);

    const exhaustionDetected = avgRate < 0.3 && declining;

    return {
      detected: exhaustionDetected,
      avgAdmissionRate: avgRate,
      declining,
      message: exhaustionDetected
        ? 'System may be approaching capacity limits'
        : null
    };
  }

  /**
   * Generate epoch string
   * @private
   */
  _generateEpoch() {
    const now = new Date();
    return `tau_${now.toISOString().replace(/[-:T.Z]/g, '_').slice(0, 19)}`;
  }

  /**
   * Trim histories to configured limits
   * @private
   */
  _trimHistories() {
    while (this.deltaHistory.length > this.config.maxHistorySize) {
      const removed = this.deltaHistory.shift();
      // Also remove from partition history
      const seq = this.partitionDeltas.get(removed.partition);
      if (seq && seq.length > 0 && seq[0].epoch === removed.epoch) {
        seq.shift();
      }
    }

    while (this.capacityHistory.length > 100) {
      this.capacityHistory.shift();
    }
  }

  /**
   * Record capacity in history
   * @private
   */
  _recordCapacityHistory(result) {
    this.capacityHistory.push({
      epoch: result.epoch,
      systemCapacity: result.systemCapacity,
      normalizedCapacity: result.normalizedCapacity,
      admissionRate: result.admissionRate,
      timestamp: result.computedAt
    });
  }

  /**
   * Interpret capacity trend
   * @private
   */
  _interpretCapacityTrend(trend, mean) {
    const interpretations = {
      stable: mean > 2
        ? 'Steady change flow through the system'
        : 'Low but stable change activity',
      increasing: 'System is processing more diverse changes',
      decreasing: 'Change variety is decreasing - possible convergence or restriction',
      volatile: 'Irregular change patterns - investigate burst activity'
    };
    return interpretations[trend] || 'Unknown trend';
  }

  /**
   * Generate recommendation for anomalies
   * @private
   */
  _generateAnomalyRecommendation(anomalies) {
    if (anomalies.length === 0) {
      return 'Capacity metrics are within normal bounds';
    }

    const critical = anomalies.filter(a => a.severity === 'critical');
    if (critical.length > 0) {
      return 'Critical capacity anomaly detected - investigate immediately';
    }

    return 'Capacity anomalies detected - monitor closely';
  }

  /**
   * Compute forecast confidence
   * @private
   */
  _computeForecastConfidence(values) {
    if (values.length < 5) return 'low';

    const mean = values.reduce((a, b) => a + b, 0) / values.length;
    const variance = values.reduce((sum, v) => sum + Math.pow(v - mean, 2), 0) / values.length;
    const cv = mean > 0 ? Math.sqrt(variance) / mean : 1;

    if (cv < 0.1) return 'high';
    if (cv < 0.3) return 'medium';
    return 'low';
  }

  /**
   * Clear all data
   */
  clear() {
    this.deltaHistory = [];
    this.partitionDeltas.clear();
    this.capacityHistory = [];
    this._theoreticalMax.clear();
    this._startTime = Date.now();
  }

  /**
   * Export data for persistence
   * @returns {Object} Exportable data
   */
  exportData() {
    return {
      deltaHistory: [...this.deltaHistory],
      capacityHistory: [...this.capacityHistory],
      config: { ...this.config },
      exportedAt: new Date().toISOString()
    };
  }

  /**
   * Import historical data
   * @param {Object} data - Previously exported data
   */
  importData(data) {
    if (data.deltaHistory) {
      for (const delta of data.deltaHistory) {
        this.recordDelta(delta);
      }
    }
    if (data.capacityHistory) {
      this.capacityHistory = [...data.capacityHistory];
    }
  }
}

/**
 * Create a capacity computer with default configuration
 * @param {Object} [config] - Configuration options
 * @returns {CapacityComputer} New capacity computer
 */
export function createCapacityComputer(config = {}) {
  return new CapacityComputer(config);
}

/**
 * Quick capacity computation from deltas
 * @param {Array<Object>} deltas - Admission deltas
 * @param {Object} [options] - Computation options
 * @returns {Object} Capacity result
 */
export function computeCapacity(deltas, options = {}) {
  const computer = new CapacityComputer(options);
  computer.recordDeltas(deltas);
  return computer.computeSystemCapacity(options);
}

/**
 * Check if capacity is healthy
 * @param {Object} capacityResult - Capacity measurement
 * @param {Object} [thresholds] - Custom thresholds
 * @returns {Object} Health check result
 */
export function checkCapacityHealth(capacityResult, thresholds = {}) {
  const defaults = {
    minAdmissionRate: 0.5,
    maxCapacity: 10,
    minThroughput: 0.1
  };

  const t = { ...defaults, ...thresholds };
  const issues = [];

  if (capacityResult.admissionRate < t.minAdmissionRate) {
    issues.push({
      type: 'low_admission_rate',
      message: `Admission rate ${(capacityResult.admissionRate * 100).toFixed(1)}% below threshold`,
      severity: 'warning'
    });
  }

  if (capacityResult.throughput.deltasPerSecond < t.minThroughput) {
    issues.push({
      type: 'low_throughput',
      message: `Throughput ${capacityResult.throughput.deltasPerSecond.toFixed(3)} deltas/s is low`,
      severity: 'info'
    });
  }

  return {
    healthy: issues.filter(i => i.severity === 'critical').length === 0,
    issues,
    capacity: capacityResult.systemCapacity,
    admissionRate: capacityResult.admissionRate,
    checkedAt: new Date().toISOString()
  };
}

export default CapacityComputer;
