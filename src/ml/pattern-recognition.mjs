/**
 * @file pattern-recognition.mjs
 * @description Pattern recognition for KGC-SWARM observables O_τ
 * Native JavaScript implementation using frequency analysis and statistical learning
 *
 * Features:
 * - Detect recurring patterns in observable sequences
 * - Learn compression strategies from history
 * - Predict convergence time based on drift patterns
 * - No external ML libraries - pure JavaScript with typed arrays
 */

import { z } from 'zod';

/**
 * Observable pattern schema
 * @type {z.ZodObject}
 */
export const ObservablePatternSchema = z.object({
  artifacts: z.array(z.string()),
  driftValue: z.number(),
  normalized: z.number(),
  epoch: z.number(),
  timestamp: z.number(),
});

/**
 * Pattern recognition result schema
 * @type {z.ZodObject}
 */
export const PatternResultSchema = z.object({
  patternId: z.string(),
  frequency: z.number(),
  avgDrift: z.number(),
  avgConvergenceTime: z.number().optional(),
  confidence: z.number().min(0).max(1),
  examples: z.array(z.number()),
});

/**
 * PatternRecognizer: Learn patterns from observable sequences O_τ
 *
 * @class
 * @description
 * Analyzes observable sequences to detect recurring patterns, learn compression
 * strategies, and predict convergence times. Uses native JavaScript with statistical
 * methods for interpretability.
 *
 * @example
 * const recognizer = new PatternRecognizer({ windowSize: 5 });
 * recognizer.observe({ artifacts: ['a1'], driftValue: 0.5, epoch: 1 });
 * const patterns = recognizer.getPatterns();
 * const prediction = recognizer.predictConvergence();
 */
export class PatternRecognizer {
  /**
   * @param {Object} config - Configuration
   * @param {number} [config.windowSize=5] - Pattern window size
   * @param {number} [config.minSupport=3] - Minimum pattern occurrences
   * @param {number} [config.similarityThreshold=0.8] - Pattern similarity threshold
   */
  constructor(config = {}) {
    this.windowSize = config.windowSize || 5;
    this.minSupport = config.minSupport || 3;
    this.similarityThreshold = config.similarityThreshold || 0.8;

    /** @type {Array<{epoch: number, artifacts: Set<string>, drift: number, normalized: number, timestamp: number}>} */
    this.history = [];

    /** @type {Map<string, {count: number, driftSum: number, convergenceTimes: number[], examples: number[]}>} */
    this.patterns = new Map();

    /** @type {Float64Array} Drift time series */
    this.driftSeries = new Float64Array(1000); // Pre-allocate for performance
    this.driftSeriesIndex = 0;

    /** @type {Map<string, number>} Artifact frequency */
    this.artifactFrequency = new Map();
  }

  /**
   * Observe a new epoch state for pattern learning
   *
   * @param {Object} observation - Observable state O_τ
   * @param {string[]} observation.artifacts - Artifact identifiers
   * @param {number} observation.driftValue - Drift value
   * @param {number} observation.normalized - Normalized drift
   * @param {number} observation.epoch - Epoch number τ
   * @param {number} [observation.timestamp] - Timestamp
   * @returns {void}
   */
  observe(observation) {
    const validated = ObservablePatternSchema.parse(observation);

    const state = {
      epoch: validated.epoch,
      artifacts: new Set(validated.artifacts),
      drift: validated.driftValue,
      normalized: validated.normalized,
      timestamp: validated.timestamp || Date.now(),
    };

    this.history.push(state);

    // Update drift time series
    if (this.driftSeriesIndex < this.driftSeries.length) {
      this.driftSeries[this.driftSeriesIndex++] = validated.normalized;
    } else {
      // Expand array if needed
      const newArray = new Float64Array(this.driftSeries.length * 2);
      newArray.set(this.driftSeries);
      newArray[this.driftSeriesIndex++] = validated.normalized;
      this.driftSeries = newArray;
    }

    // Update artifact frequency
    for (const artifact of validated.artifacts) {
      this.artifactFrequency.set(
        artifact,
        (this.artifactFrequency.get(artifact) || 0) + 1
      );
    }

    // Extract patterns if we have enough history
    if (this.history.length >= this.windowSize) {
      this._extractPattern(this.history.length - 1);
    }
  }

  /**
   * Extract pattern from recent window
   *
   * @param {number} endIndex - End index in history
   * @returns {void}
   * @private
   */
  _extractPattern(endIndex) {
    const startIndex = Math.max(0, endIndex - this.windowSize + 1);
    const window = this.history.slice(startIndex, endIndex + 1);

    // Create pattern signature: sorted artifact set + drift trend
    const artifactSets = window.map(s => Array.from(s.artifacts).sort());
    const driftTrend = this._computeTrend(window.map(s => s.normalized));

    // Pattern ID: hash of artifact sequence + trend direction
    const patternId = this._hashPattern(artifactSets, driftTrend);

    // Update pattern statistics
    if (!this.patterns.has(patternId)) {
      this.patterns.set(patternId, {
        count: 0,
        driftSum: 0,
        convergenceTimes: [],
        examples: [],
      });
    }

    const pattern = this.patterns.get(patternId);
    pattern.count++;
    pattern.driftSum += window[window.length - 1].normalized;
    pattern.examples.push(endIndex);

    // Track convergence time if this pattern leads to low drift
    if (window[window.length - 1].normalized < 0.01) {
      const convergenceTime = endIndex - startIndex;
      pattern.convergenceTimes.push(convergenceTime);
    }
  }

  /**
   * Compute trend direction from time series
   *
   * @param {number[]} values - Time series values
   * @returns {string} 'increasing' | 'decreasing' | 'stable'
   * @private
   */
  _computeTrend(values) {
    if (values.length < 2) return 'stable';

    // Simple linear regression slope
    const n = values.length;
    const sumX = (n * (n - 1)) / 2; // 0 + 1 + ... + (n-1)
    const sumY = values.reduce((a, b) => a + b, 0);
    const sumXY = values.reduce((sum, y, x) => sum + x * y, 0);
    const sumX2 = (n * (n - 1) * (2 * n - 1)) / 6;

    const slope = (n * sumXY - sumX * sumY) / (n * sumX2 - sumX * sumX);

    if (slope < -0.01) return 'decreasing';
    if (slope > 0.01) return 'increasing';
    return 'stable';
  }

  /**
   * Hash pattern for identification
   *
   * @param {string[][]} artifactSets - Artifact sets in window
   * @param {string} trend - Drift trend
   * @returns {string} Pattern hash
   * @private
   */
  _hashPattern(artifactSets, trend) {
    // Simple deterministic hash: concatenate and use string hash
    const signature = artifactSets.map(set => set.join(',')).join('|') + '#' + trend;

    // FNV-1a hash for consistency
    let hash = 2166136261;
    for (let i = 0; i < signature.length; i++) {
      hash ^= signature.charCodeAt(i);
      hash = Math.imul(hash, 16777619);
    }

    return (hash >>> 0).toString(36); // Convert to base36 string
  }

  /**
   * Get recognized patterns with statistics
   *
   * @param {Object} options - Options
   * @param {boolean} [options.onlyFrequent=true] - Only return patterns above minSupport
   * @returns {Array<PatternResultSchema>} Recognized patterns
   */
  getPatterns(options = {}) {
    const onlyFrequent = options.onlyFrequent !== false;
    const results = [];

    for (const [patternId, data] of this.patterns.entries()) {
      if (onlyFrequent && data.count < this.minSupport) {
        continue;
      }

      const avgDrift = data.driftSum / data.count;
      const avgConvergenceTime = data.convergenceTimes.length > 0
        ? data.convergenceTimes.reduce((a, b) => a + b, 0) / data.convergenceTimes.length
        : undefined;

      // Confidence based on frequency and consistency
      const frequency = data.count / this.history.length;
      const consistency = data.convergenceTimes.length > 0
        ? 1 - this._computeVariance(data.convergenceTimes) / Math.max(...data.convergenceTimes, 1)
        : 0.5;
      const confidence = Math.min(1, frequency * consistency * 2);

      results.push(
        PatternResultSchema.parse({
          patternId,
          frequency: data.count,
          avgDrift,
          avgConvergenceTime,
          confidence,
          examples: data.examples.slice(0, 5), // First 5 examples
        })
      );
    }

    return results.sort((a, b) => b.confidence - a.confidence);
  }

  /**
   * Compute variance of array
   *
   * @param {number[]} values - Values
   * @returns {number} Variance
   * @private
   */
  _computeVariance(values) {
    if (values.length === 0) return 0;
    const mean = values.reduce((a, b) => a + b, 0) / values.length;
    return values.reduce((sum, v) => sum + (v - mean) ** 2, 0) / values.length;
  }

  /**
   * Predict convergence time based on learned patterns
   *
   * @param {Object} options - Prediction options
   * @param {number} [options.currentEpoch] - Current epoch (defaults to history length)
   * @returns {{estimatedEpochs: number, confidence: number, basedOnPattern: string|null}} Prediction
   */
  predictConvergence(options = {}) {
    const currentEpoch = options.currentEpoch || this.history.length;

    if (this.history.length < this.windowSize) {
      return {
        estimatedEpochs: 10, // Default estimate
        confidence: 0.1,
        basedOnPattern: null,
      };
    }

    // Find matching pattern in recent window
    const recentWindow = this.history.slice(-this.windowSize);
    const recentArtifacts = recentWindow.map(s => Array.from(s.artifacts).sort());
    const recentTrend = this._computeTrend(recentWindow.map(s => s.normalized));
    const recentPatternId = this._hashPattern(recentArtifacts, recentTrend);

    // Check if this pattern exists and has convergence data
    const matchingPattern = this.patterns.get(recentPatternId);

    if (matchingPattern && matchingPattern.convergenceTimes.length > 0) {
      const avgTime = matchingPattern.convergenceTimes.reduce((a, b) => a + b, 0)
        / matchingPattern.convergenceTimes.length;
      const confidence = Math.min(1, matchingPattern.count / this.minSupport);

      return {
        estimatedEpochs: Math.ceil(avgTime),
        confidence,
        basedOnPattern: recentPatternId,
      };
    }

    // Fallback: Use drift trend for prediction
    const driftTrend = this._analyzeDriftTrend();
    if (driftTrend.isDecreasing && driftTrend.currentDrift > 0) {
      const epochsToZero = Math.ceil(driftTrend.currentDrift / Math.abs(driftTrend.rate));
      return {
        estimatedEpochs: Math.max(1, epochsToZero),
        confidence: 0.5,
        basedOnPattern: null,
      };
    }

    return {
      estimatedEpochs: 10,
      confidence: 0.2,
      basedOnPattern: null,
    };
  }

  /**
   * Analyze drift trend from time series
   *
   * @returns {{isDecreasing: boolean, rate: number, currentDrift: number}} Trend analysis
   * @private
   */
  _analyzeDriftTrend() {
    if (this.driftSeriesIndex < 3) {
      return { isDecreasing: false, rate: 0, currentDrift: 0 };
    }

    const recentDrifts = Array.from(
      this.driftSeries.slice(Math.max(0, this.driftSeriesIndex - 10), this.driftSeriesIndex)
    );

    // Linear regression for rate
    const n = recentDrifts.length;
    const sumX = (n * (n - 1)) / 2;
    const sumY = recentDrifts.reduce((a, b) => a + b, 0);
    const sumXY = recentDrifts.reduce((sum, y, x) => sum + x * y, 0);
    const sumX2 = (n * (n - 1) * (2 * n - 1)) / 6;

    const rate = (n * sumXY - sumX * sumY) / (n * sumX2 - sumX * sumX);
    const currentDrift = recentDrifts[recentDrifts.length - 1];

    return {
      isDecreasing: rate < 0,
      rate,
      currentDrift,
    };
  }

  /**
   * Suggest compression strategy based on learned patterns
   *
   * @returns {{strategy: string, reason: string, expectedRatio: number}} Strategy suggestion
   */
  suggestCompressionStrategy() {
    // Analyze artifact frequency
    const totalArtifacts = Array.from(this.artifactFrequency.values())
      .reduce((a, b) => a + b, 0);

    const uniqueArtifacts = this.artifactFrequency.size;
    const avgFrequency = totalArtifacts / uniqueArtifacts;

    // Calculate entropy
    const entropy = Array.from(this.artifactFrequency.values())
      .reduce((h, freq) => {
        const p = freq / totalArtifacts;
        return h - p * Math.log2(p);
      }, 0);

    // Strategy selection based on distribution
    if (entropy < 2) {
      // Low entropy - high redundancy
      return {
        strategy: 'aggressive-deduplication',
        reason: `Low entropy (${entropy.toFixed(2)} bits) indicates high redundancy`,
        expectedRatio: 0.3,
      };
    } else if (avgFrequency > 5) {
      // High frequency - good compression potential
      return {
        strategy: 'frequency-based',
        reason: `High average frequency (${avgFrequency.toFixed(1)}) enables efficient encoding`,
        expectedRatio: 0.5,
      };
    } else {
      // Mixed distribution
      return {
        strategy: 'hybrid',
        reason: `Mixed distribution (entropy: ${entropy.toFixed(2)} bits, avg freq: ${avgFrequency.toFixed(1)})`,
        expectedRatio: 0.7,
      };
    }
  }

  /**
   * Get summary statistics
   *
   * @returns {Object} Summary
   */
  getSummary() {
    return {
      totalObservations: this.history.length,
      uniquePatterns: this.patterns.size,
      uniqueArtifacts: this.artifactFrequency.size,
      driftSeriesLength: this.driftSeriesIndex,
      averageDrift: this.driftSeriesIndex > 0
        ? Array.from(this.driftSeries.slice(0, this.driftSeriesIndex))
            .reduce((a, b) => a + b, 0) / this.driftSeriesIndex
        : 0,
    };
  }

  /**
   * Reset recognizer state
   */
  reset() {
    this.history = [];
    this.patterns.clear();
    this.driftSeries.fill(0);
    this.driftSeriesIndex = 0;
    this.artifactFrequency.clear();
  }
}

/**
 * Create a new pattern recognizer
 *
 * @param {Object} config - Configuration
 * @returns {PatternRecognizer} New recognizer instance
 */
export function createPatternRecognizer(config = {}) {
  return new PatternRecognizer(config);
}

export default { PatternRecognizer, createPatternRecognizer };
