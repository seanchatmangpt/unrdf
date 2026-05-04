/**
 * @file Z-Score Anomaly Detection for UNRDF Observability
 * @module anomaly-detector
 *
 * @description
 * Implements statistical anomaly detection using z-score analysis
 * for latency spikes and throughput drops in OTEL metrics.
 */

import { z } from 'zod';

/**
 * Configuration schema for anomaly detector
 */
export const AnomalyDetectorConfigSchema = z.object({
  zScoreThreshold: z.number().positive().default(3.0), // 3 sigma = 99.7% confidence
  windowSize: z.number().int().positive().default(100), // Rolling window size
  minSamples: z.number().int().positive().default(10), // Minimum samples before detection
  cooldownMs: z.number().int().positive().default(60000), // 1 minute cooldown between alerts
});

/**
 * Anomaly event schema
 */
export const AnomalyEventSchema = z.object({
  timestamp: z.number(),
  metric: z.string(),
  value: z.number(),
  mean: z.number(),
  stdDev: z.number(),
  zScore: z.number(),
  severity: z.enum(['low', 'medium', 'high', 'critical']),
  message: z.string(),
});

/**
 * Z-Score based anomaly detector
 */
export class AnomalyDetector {
  /**
   * Create anomaly detector
   * @param {Object} [config] - Configuration
   */
  constructor(config = {}) {
    this.config = AnomalyDetectorConfigSchema.parse(config);
    this.windows = new Map(); // metric -> rolling window of values
    this.lastAlerts = new Map(); // metric -> timestamp of last alert
    this.stats = new Map(); // metric -> {mean, stdDev}
    this.anomalyListeners = [];
  }

  /**
   * Add a metric sample
   * @param {string} metric - Metric name (e.g., 'transaction_latency', 'throughput')
   * @param {number} value - Metric value
   * @returns {Object|null} Anomaly event if detected, null otherwise
   */
  addSample(metric, value) {
    // Initialize window if needed
    if (!this.windows.has(metric)) {
      this.windows.set(metric, []);
    }

    const window = this.windows.get(metric);
    window.push(value);

    // Keep window size bounded
    if (window.length > this.config.windowSize) {
      window.shift();
    }

    // Need minimum samples for statistical significance
    if (window.length < this.config.minSamples) {
      return null;
    }

    // Calculate statistics
    const stats = this._calculateStats(window);
    this.stats.set(metric, stats);

    // Check for anomaly
    const zScore = (value - stats.mean) / stats.stdDev;
    const absZScore = Math.abs(zScore);

    if (absZScore > this.config.zScoreThreshold) {
      // Check cooldown to prevent alert spam
      const lastAlert = this.lastAlerts.get(metric);
      const now = Date.now();
      if (lastAlert && now - lastAlert < this.config.cooldownMs) {
        return null; // Still in cooldown
      }

      // Create anomaly event
      const severity = this._calculateSeverity(absZScore);
      const anomaly = AnomalyEventSchema.parse({
        timestamp: now,
        metric,
        value,
        mean: stats.mean,
        stdDev: stats.stdDev,
        zScore,
        severity,
        message: this._generateMessage(metric, value, stats, zScore, severity),
      });

      // Update last alert time
      this.lastAlerts.set(metric, now);

      // Notify listeners
      this._notifyListeners(anomaly);

      return anomaly;
    }

    return null;
  }

  /**
   * Calculate mean and standard deviation
   * @param {Array<number>} values - Values
   * @returns {{mean: number, stdDev: number}}
   * @private
   */
  _calculateStats(values) {
    const n = values.length;
    const mean = values.reduce((sum, v) => sum + v, 0) / n;
    const variance = values.reduce((sum, v) => sum + Math.pow(v - mean, 2), 0) / n;
    const stdDev = Math.sqrt(variance);
    return { mean, stdDev };
  }

  /**
   * Calculate severity based on z-score
   * @param {number} absZScore - Absolute z-score
   * @returns {string} Severity level
   * @private
   */
  _calculateSeverity(absZScore) {
    if (absZScore >= 5.0) return 'critical'; // >5σ = extremely rare
    if (absZScore >= 4.0) return 'high'; // 4-5σ
    if (absZScore >= 3.5) return 'medium'; // 3.5-4σ
    return 'low'; // 3-3.5σ
  }

  /**
   * Generate human-readable message
   * @param {string} metric - Metric name
   * @param {number} value - Current value
   * @param {Object} stats - Statistics
   * @param {number} zScore - Z-score
   * @param {string} severity - Severity
   * @returns {string} Message
   * @private
   */
  _generateMessage(metric, value, stats, zScore, severity) {
    const direction = zScore > 0 ? 'spike' : 'drop';
    const deviation = Math.abs(zScore).toFixed(2);
    return `[${severity.toUpperCase()}] ${metric} ${direction}: ${value.toFixed(2)} (${deviation}σ from mean ${stats.mean.toFixed(2)})`;
  }

  /**
   * Register anomaly listener
   * @param {Function} listener - Callback(anomaly)
   */
  onAnomaly(listener) {
    this.anomalyListeners.push(listener);
  }

  /**
   * Notify all listeners
   * @param {Object} anomaly - Anomaly event
   * @private
   */
  _notifyListeners(anomaly) {
    for (const listener of this.anomalyListeners) {
      try {
        listener(anomaly);
      } catch (error) {
        console.error('[AnomalyDetector] Listener error:', error);
      }
    }
  }

  /**
   * Get statistics for a metric
   * @param {string} metric - Metric name
   * @returns {{mean: number, stdDev: number, samples: number}|null}
   */
  getStats(metric) {
    const stats = this.stats.get(metric);
    const window = this.windows.get(metric);
    if (!stats || !window) return null;
    return {
      ...stats,
      samples: window.length,
    };
  }

  /**
   * Reset detector state
   */
  reset() {
    this.windows.clear();
    this.lastAlerts.clear();
    this.stats.clear();
  }

  /**
   * Clear metric history
   * @param {string} metric - Metric name
   */
  clearMetric(metric) {
    this.windows.delete(metric);
    this.lastAlerts.delete(metric);
    this.stats.delete(metric);
  }
}

/**
 * Create anomaly detector instance
 * @param {Object} [config] - Configuration
 * @returns {AnomalyDetector}
 */
export function createAnomalyDetector(config = {}) {
  return new AnomalyDetector(config);
}
