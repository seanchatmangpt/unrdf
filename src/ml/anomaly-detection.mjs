/**
 * @file anomaly-detection.mjs
 * @description Anomaly detection for KGC-SWARM drift patterns
 * Native JavaScript implementation using statistical methods
 *
 * Features:
 * - Z-score outlier detection
 * - IQR (Interquartile Range) outlier detection
 * - Drift pattern anomalies
 * - Guard violation prediction
 * - No external ML libraries - pure JavaScript with typed arrays
 */

import { z } from 'zod';

/**
 * Drift observation schema
 * @type {z.ZodObject}
 */
export const DriftObservationSchema = z.object({
  epoch: z.number(),
  drift: z.number(),
  normalized: z.number(),
  added: z.number().optional(),
  removed: z.number().optional(),
  modified: z.number().optional(),
  timestamp: z.number(),
});

/**
 * Anomaly result schema
 * @type {z.ZodObject}
 */
export const AnomalySchema = z.object({
  type: z.enum(['drift-spike', 'drift-plateau', 'guard-risk', 'statistical-outlier']),
  severity: z.number().min(0).max(1),
  description: z.string(),
  epoch: z.number(),
  value: z.number(),
  threshold: z.number(),
  suggestions: z.array(z.string()),
});

/**
 * AnomalyDetector: Statistical anomaly detection for drift patterns
 *
 * @class
 * @description
 * Detects unusual patterns in drift observations using statistical methods:
 * - Z-score for outlier detection
 * - IQR for robust outlier detection
 * - Temporal pattern analysis
 * - Guard violation prediction based on trends
 *
 * @example
 * const detector = new AnomalyDetector({ zScoreThreshold: 2.5 });
 * detector.observe({ epoch: 1, drift: 10, normalized: 0.1, timestamp: Date.now() });
 * const anomalies = detector.detect();
 */
export class AnomalyDetector {
  /**
   * @param {Object} config - Configuration
   * @param {number} [config.zScoreThreshold=2.5] - Z-score threshold for anomalies
   * @param {number} [config.iqrMultiplier=1.5] - IQR multiplier for outliers
   * @param {number} [config.windowSize=10] - Moving window size
   * @param {number} [config.guardRiskThreshold=0.7] - Guard risk prediction threshold
   */
  constructor(config = {}) {
    this.zScoreThreshold = config.zScoreThreshold || 2.5;
    this.iqrMultiplier = config.iqrMultiplier || 1.5;
    this.windowSize = config.windowSize || 10;
    this.guardRiskThreshold = config.guardRiskThreshold || 0.7;

    /** @type {Array<DriftObservationSchema>} */
    this.observations = [];

    /** @type {Float64Array} Drift time series for fast statistics */
    this.driftValues = new Float64Array(1000);
    this.driftIndex = 0;

    /** @type {Float64Array} Normalized drift time series */
    this.normalizedValues = new Float64Array(1000);
    this.normalizedIndex = 0;

    /** @type {Map<number, AnomalySchema>} Detected anomalies by epoch */
    this.detectedAnomalies = new Map();

    /** Statistics cache */
    this.statsCache = {
      mean: 0,
      std: 0,
      q1: 0,
      q3: 0,
      iqr: 0,
      lastUpdate: 0,
    };
  }

  /**
   * Observe a drift measurement
   *
   * @param {DriftObservationSchema} observation - Drift observation
   * @returns {void}
   */
  observe(observation) {
    const validated = DriftObservationSchema.parse(observation);
    this.observations.push(validated);

    // Update drift arrays
    if (this.driftIndex < this.driftValues.length) {
      this.driftValues[this.driftIndex++] = validated.drift;
      this.normalizedValues[this.normalizedIndex++] = validated.normalized;
    } else {
      // Expand arrays
      const newDrift = new Float64Array(this.driftValues.length * 2);
      const newNormalized = new Float64Array(this.normalizedValues.length * 2);
      newDrift.set(this.driftValues);
      newNormalized.set(this.normalizedValues);
      newDrift[this.driftIndex++] = validated.drift;
      newNormalized[this.normalizedIndex++] = validated.normalized;
      this.driftValues = newDrift;
      this.normalizedValues = newNormalized;
    }

    // Invalidate statistics cache
    this.statsCache.lastUpdate = 0;
  }

  /**
   * Compute statistics (with caching)
   *
   * @returns {{mean: number, std: number, q1: number, q3: number, iqr: number}} Statistics
   * @private
   */
  _computeStatistics() {
    // Return cached if recent
    if (this.statsCache.lastUpdate === this.driftIndex && this.driftIndex > 0) {
      return this.statsCache;
    }

    if (this.driftIndex === 0) {
      return { mean: 0, std: 0, q1: 0, q3: 0, iqr: 0 };
    }

    const values = Array.from(this.normalizedValues.slice(0, this.normalizedIndex));

    // Mean and standard deviation
    const mean = values.reduce((a, b) => a + b, 0) / values.length;
    const variance = values.reduce((sum, v) => sum + (v - mean) ** 2, 0) / values.length;
    const std = Math.sqrt(variance);

    // Quartiles (for IQR method)
    const sorted = values.slice().sort((a, b) => a - b);
    const q1Index = Math.floor(sorted.length * 0.25);
    const q3Index = Math.floor(sorted.length * 0.75);
    const q1 = sorted[q1Index];
    const q3 = sorted[q3Index];
    const iqr = q3 - q1;

    this.statsCache = { mean, std, q1, q3, iqr, lastUpdate: this.driftIndex };
    return this.statsCache;
  }

  /**
   * Detect anomalies in recent observations
   *
   * @param {Object} options - Detection options
   * @param {boolean} [options.useZScore=true] - Use Z-score method
   * @param {boolean} [options.useIQR=true] - Use IQR method
   * @param {boolean} [options.detectPatterns=true] - Detect pattern anomalies
   * @returns {Array<AnomalySchema>} Detected anomalies
   */
  detect(options = {}) {
    const useZScore = options.useZScore !== false;
    const useIQR = options.useIQR !== false;
    const detectPatterns = options.detectPatterns !== false;

    const anomalies = [];

    if (this.observations.length < 3) {
      return anomalies; // Need minimum data
    }

    const stats = this._computeStatistics();

    // Check recent observations
    const recentStart = Math.max(0, this.observations.length - this.windowSize);
    const recentObs = this.observations.slice(recentStart);

    for (const obs of recentObs) {
      // Skip if already detected
      if (this.detectedAnomalies.has(obs.epoch)) {
        continue;
      }

      // Z-score detection
      if (useZScore && stats.std > 0) {
        const zScore = Math.abs((obs.normalized - stats.mean) / stats.std);

        if (zScore > this.zScoreThreshold) {
          const anomaly = AnomalySchema.parse({
            type: 'statistical-outlier',
            severity: Math.min(1, zScore / (this.zScoreThreshold * 2)),
            description: `Statistical outlier detected: drift ${obs.normalized.toFixed(4)} (z-score: ${zScore.toFixed(2)})`,
            epoch: obs.epoch,
            value: obs.normalized,
            threshold: stats.mean + this.zScoreThreshold * stats.std,
            suggestions: [
              'Investigate cause of unusual drift pattern',
              'Check for data quality issues',
              'Review recent configuration changes',
            ],
          });

          anomalies.push(anomaly);
          this.detectedAnomalies.set(obs.epoch, anomaly);
        }
      }

      // IQR detection (more robust to outliers)
      if (useIQR && stats.iqr > 0) {
        const lowerBound = stats.q1 - this.iqrMultiplier * stats.iqr;
        const upperBound = stats.q3 + this.iqrMultiplier * stats.iqr;

        if (obs.normalized < lowerBound || obs.normalized > upperBound) {
          const isLow = obs.normalized < lowerBound;
          const anomaly = AnomalySchema.parse({
            type: 'statistical-outlier',
            severity: 0.6,
            description: `IQR outlier: drift ${obs.normalized.toFixed(4)} is ${isLow ? 'below' : 'above'} expected range`,
            epoch: obs.epoch,
            value: obs.normalized,
            threshold: isLow ? lowerBound : upperBound,
            suggestions: [
              isLow
                ? 'Unusually low drift - verify convergence is genuine'
                : 'Unusually high drift - check for system instability',
              'Monitor next few epochs for pattern',
            ],
          });

          // Only add if not already detected by Z-score
          if (!this.detectedAnomalies.has(obs.epoch)) {
            anomalies.push(anomaly);
            this.detectedAnomalies.set(obs.epoch, anomaly);
          }
        }
      }
    }

    // Pattern-based detection
    if (detectPatterns && recentObs.length >= 3) {
      const patternAnomalies = this._detectPatternAnomalies(recentObs);
      anomalies.push(...patternAnomalies);
    }

    return anomalies;
  }

  /**
   * Detect pattern-based anomalies
   *
   * @param {Array<DriftObservationSchema>} observations - Recent observations
   * @returns {Array<AnomalySchema>} Pattern anomalies
   * @private
   */
  _detectPatternAnomalies(observations) {
    const anomalies = [];

    // Drift spike: sudden increase in drift
    for (let i = 1; i < observations.length; i++) {
      const prev = observations[i - 1];
      const curr = observations[i];

      const increase = curr.normalized - prev.normalized;
      const relativeIncrease = prev.normalized > 0
        ? increase / prev.normalized
        : (increase > 0.1 ? 1 : 0);

      if (relativeIncrease > 0.5 && increase > 0.05) {
        // 50% relative increase or >0.05 absolute increase
        if (!this.detectedAnomalies.has(curr.epoch)) {
          const anomaly = AnomalySchema.parse({
            type: 'drift-spike',
            severity: Math.min(1, relativeIncrease),
            description: `Drift spike detected: ${prev.normalized.toFixed(4)} → ${curr.normalized.toFixed(4)} (${(relativeIncrease * 100).toFixed(0)}% increase)`,
            epoch: curr.epoch,
            value: curr.normalized,
            threshold: prev.normalized * 1.5,
            suggestions: [
              'Sudden drift increase may indicate instability',
              'Check for recent system changes',
              'Monitor for continued divergence',
            ],
          });

          anomalies.push(anomaly);
          this.detectedAnomalies.set(curr.epoch, anomaly);
        }
      }
    }

    // Drift plateau: drift not decreasing over window
    if (observations.length >= 5) {
      const last5 = observations.slice(-5);
      const drifts = last5.map(o => o.normalized);
      const isDecreasing = drifts.every((d, i) => i === 0 || d <= drifts[i - 1] * 1.1);

      if (!isDecreasing && drifts[0] > 0.05) {
        const latestEpoch = observations[observations.length - 1].epoch;

        if (!this.detectedAnomalies.has(latestEpoch)) {
          const anomaly = AnomalySchema.parse({
            type: 'drift-plateau',
            severity: 0.5,
            description: `Drift plateau: no decrease over last 5 epochs (${drifts[0].toFixed(4)} → ${drifts[4].toFixed(4)})`,
            epoch: latestEpoch,
            value: drifts[4],
            threshold: 0.01,
            suggestions: [
              'Drift not converging as expected',
              'May need different compression strategy',
              'Consider adjusting convergence parameters',
            ],
          });

          anomalies.push(anomaly);
          this.detectedAnomalies.set(latestEpoch, anomaly);
        }
      }
    }

    return anomalies;
  }

  /**
   * Predict guard violation risk based on trends
   *
   * @param {Object} guardCondition - Guard condition to check
   * @param {string} guardCondition.name - Guard name
   * @param {number} guardCondition.driftThreshold - Drift threshold for violation
   * @returns {{risk: number, willViolate: boolean, estimatedEpochs: number|null, confidence: number}} Prediction
   */
  predictGuardViolation(guardCondition) {
    const { name, driftThreshold } = guardCondition;

    if (this.observations.length < 5) {
      return {
        risk: 0,
        willViolate: false,
        estimatedEpochs: null,
        confidence: 0.1,
      };
    }

    // Analyze recent trend
    const recent = this.observations.slice(-10);
    const drifts = recent.map(o => o.normalized);

    // Linear regression to predict trend
    const n = drifts.length;
    const x = Array.from({ length: n }, (_, i) => i);
    const sumX = x.reduce((a, b) => a + b, 0);
    const sumY = drifts.reduce((a, b) => a + b, 0);
    const sumXY = x.reduce((sum, xi, i) => sum + xi * drifts[i], 0);
    const sumX2 = x.reduce((sum, xi) => sum + xi * xi, 0);

    const slope = (n * sumXY - sumX * sumY) / (n * sumX2 - sumX * sumX);
    const intercept = (sumY - slope * sumX) / n;

    // Predict when threshold might be crossed
    const currentDrift = drifts[drifts.length - 1];
    let estimatedEpochs = null;
    let risk = 0;

    if (slope > 0 && currentDrift < driftThreshold) {
      // Drift increasing toward threshold
      estimatedEpochs = Math.ceil((driftThreshold - currentDrift) / slope);
      risk = Math.min(1, 1 - estimatedEpochs / 20); // Higher risk for sooner violations
    } else if (currentDrift >= driftThreshold) {
      // Already at or above threshold
      estimatedEpochs = 0;
      risk = 1.0;
    }

    // Confidence based on trend consistency
    const residuals = drifts.map((y, i) => y - (intercept + slope * i));
    const mse = residuals.reduce((sum, r) => sum + r * r, 0) / n;
    const confidence = Math.max(0.1, 1 - mse / Math.max(...drifts));

    return {
      risk,
      willViolate: risk >= this.guardRiskThreshold,
      estimatedEpochs,
      confidence,
    };
  }

  /**
   * Get anomaly summary
   *
   * @returns {Object} Summary
   */
  getSummary() {
    const stats = this._computeStatistics();

    return {
      totalObservations: this.observations.length,
      detectedAnomalies: this.detectedAnomalies.size,
      statistics: {
        mean: stats.mean,
        std: stats.std,
        q1: stats.q1,
        q3: stats.q3,
        iqr: stats.iqr,
      },
      recentDrift: this.normalizedIndex > 0
        ? this.normalizedValues[this.normalizedIndex - 1]
        : 0,
      anomalyTypes: this._countAnomalyTypes(),
    };
  }

  /**
   * Count anomalies by type
   *
   * @returns {Object} Type counts
   * @private
   */
  _countAnomalyTypes() {
    const counts = {
      'drift-spike': 0,
      'drift-plateau': 0,
      'guard-risk': 0,
      'statistical-outlier': 0,
    };

    for (const anomaly of this.detectedAnomalies.values()) {
      counts[anomaly.type]++;
    }

    return counts;
  }

  /**
   * Clear detected anomalies (for fresh detection)
   *
   * @returns {void}
   */
  clearAnomalies() {
    this.detectedAnomalies.clear();
  }

  /**
   * Reset detector
   *
   * @returns {void}
   */
  reset() {
    this.observations = [];
    this.driftValues.fill(0);
    this.driftIndex = 0;
    this.normalizedValues.fill(0);
    this.normalizedIndex = 0;
    this.detectedAnomalies.clear();
    this.statsCache = { mean: 0, std: 0, q1: 0, q3: 0, iqr: 0, lastUpdate: 0 };
  }
}

/**
 * Create a new anomaly detector
 *
 * @param {Object} config - Configuration
 * @returns {AnomalyDetector} New detector instance
 */
export function createAnomalyDetector(config = {}) {
  return new AnomalyDetector(config);
}

export default { AnomalyDetector, createAnomalyDetector };
