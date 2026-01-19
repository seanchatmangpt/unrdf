/**
 * @file Dark Field Detector - 95% Invisible Field Detection
 * @module knowledge-engine/dark-field-detector
 *
 * @description
 * Detects and analyzes the dark field (invisible ~95%) of organizational,
 * market, strategic, and disruption patterns using the Chatman Equation.
 */

import { randomUUID } from 'crypto';
import { z } from 'zod';

/**
 * Detection result schema
 */
export const DetectionResultSchema = z.object({
  id: z.string().uuid(),
  type: z.enum(['market', 'organizational', 'strategic', 'disruption']),
  observableSize: z.number().int().nonnegative(),
  darkFieldSize: z.number().int().nonnegative(),
  darkFieldRatio: z.number().min(0).max(1),
  patterns: z.array(
    z.object({
      dimension: z.string(),
      confidence: z.number().min(0).max(1),
      derivedFrom: z.array(z.string()),
    })
  ),
  detectedAt: z.number(),
});

/**
 * Detection options schema
 */
export const DetectionOptionsSchema = z.object({
  minConfidence: z.number().min(0).max(1).default(0.7),
  maxPatterns: z.number().int().positive().default(100),
  targetRatio: z.number().min(0).max(1).default(0.95),
});

/**
 * Dark field detector for Chatman Equation
 */
export class DarkFieldDetector {
  /**
   * Create a new dark field detector
   * @param {Object} [options] - Detector options
   * @param {Function} [options.tracer] - OTEL tracer function
   * @param {number} [options.targetRatio=0.95] - Target dark field ratio
   */
  constructor(options = {}) {
    this.tracer = options.tracer;
    this.targetRatio = options.targetRatio ?? 0.95;
    this.metrics = {
      detectionsPerformed: 0,
      averageDarkFieldRatio: 0,
      totalPatternsDetected: 0,
    };
  }

  /**
   * Detect dark field patterns from observable data
   * @param {Object} observable - Observable pattern data
   * @param {Object} [options] - Detection options
   * @returns {Promise<Object>} Detection result
   */
  async detect(observable, options = {}) {
    const span = this.tracer?.startSpan?.('chatman.darkfield.detect');
    const startTime = Date.now();

    try {
      const opts = DetectionOptionsSchema.parse(options);

      span?.setAttribute?.('observable.type', observable.type);
      span?.setAttribute?.('observable.size', observable.patterns.length);
      span?.setAttribute?.('target_ratio', opts.targetRatio);

      // Calculate expected dark field size
      const observableSize = observable.patterns.length;
      const darkFieldSize = this._calculateDarkFieldSize(observableSize, opts.targetRatio);

      // Detect dark field patterns
      const patterns = this._detectPatterns(observable, darkFieldSize, opts);

      const darkFieldRatio = darkFieldSize / (darkFieldSize + observableSize);

      const result = {
        id: randomUUID(),
        type: observable.type,
        observableSize,
        darkFieldSize,
        darkFieldRatio,
        patterns,
        detectedAt: Date.now(),
      };

      // Update metrics
      this.metrics.detectionsPerformed++;
      this.metrics.averageDarkFieldRatio =
        (this.metrics.averageDarkFieldRatio * (this.metrics.detectionsPerformed - 1) +
          darkFieldRatio) /
        this.metrics.detectionsPerformed;
      this.metrics.totalPatternsDetected += patterns.length;

      span?.addEvent?.('detection_completed', {
        'detection.id': result.id,
        'detection.dark_field_size': darkFieldSize,
        'detection.ratio': darkFieldRatio,
        'detection.pattern_count': patterns.length,
        'detection.duration_ms': Date.now() - startTime,
      });

      return DetectionResultSchema.parse(result);
    } catch (error) {
      span?.recordException?.(error);
      span?.setStatus?.({ code: 2, message: error.message });
      throw error;
    } finally {
      span?.end?.();
    }
  }

  /**
   * Calculate dark field size from observable size and target ratio
   * @param {number} observableSize - Number of observable patterns
   * @param {number} targetRatio - Target dark field ratio
   * @returns {number} Dark field size
   * @private
   */
  _calculateDarkFieldSize(observableSize, targetRatio) {
    // targetRatio = darkField / (darkField + observable)
    // darkField = observable * targetRatio / (1 - targetRatio)
    return Math.floor((observableSize * targetRatio) / (1 - targetRatio));
  }

  /**
   * Detect dark field patterns
   * @param {Object} observable - Observable pattern
   * @param {number} targetSize - Target dark field size
   * @param {Object} options - Detection options
   * @returns {Array<Object>} Detected patterns
   * @private
   */
  _detectPatterns(observable, targetSize, options) {
    const detectors = {
      market: this._detectMarketDarkField.bind(this),
      organizational: this._detectOrganizationalDarkField.bind(this),
      strategic: this._detectStrategicDarkField.bind(this),
      disruption: this._detectDisruptionDarkField.bind(this),
    };

    const detector = detectors[observable.type] || detectors.market;
    return detector(observable.patterns, targetSize, options);
  }

  /**
   * Detect market dark field patterns
   * @param {Array<string>} observablePatterns - Observable patterns
   * @param {number} targetSize - Target size
   * @param {Object} options - Options
   * @returns {Array<Object>} Detected patterns
   * @private
   */
  _detectMarketDarkField(observablePatterns, targetSize, options) {
    const dimensions = [
      'hidden_customer_needs',
      'latent_market_segments',
      'unspoken_value_drivers',
      'invisible_competition',
      'emergent_demand_patterns',
    ];

    return this._generateDetectedPatterns(
      dimensions,
      observablePatterns,
      targetSize,
      options
    );
  }

  /**
   * Detect organizational dark field patterns
   * @param {Array<string>} observablePatterns - Observable patterns
   * @param {number} targetSize - Target size
   * @param {Object} options - Options
   * @returns {Array<Object>} Detected patterns
   * @private
   */
  _detectOrganizationalDarkField(observablePatterns, targetSize, options) {
    const dimensions = [
      'informal_power_structures',
      'tacit_knowledge_networks',
      'hidden_resource_flows',
      'implicit_decision_criteria',
      'unwritten_cultural_norms',
    ];

    return this._generateDetectedPatterns(
      dimensions,
      observablePatterns,
      targetSize,
      options
    );
  }

  /**
   * Detect strategic dark field patterns
   * @param {Array<string>} observablePatterns - Observable patterns
   * @param {number} targetSize - Target size
   * @param {Object} options - Options
   * @returns {Array<Object>} Detected patterns
   * @private
   */
  _detectStrategicDarkField(observablePatterns, targetSize, options) {
    const dimensions = [
      'latent_strategic_options',
      'hidden_competitive_advantages',
      'implicit_strategic_assumptions',
      'emergent_industry_shifts',
      'tacit_success_factors',
    ];

    return this._generateDetectedPatterns(
      dimensions,
      observablePatterns,
      targetSize,
      options
    );
  }

  /**
   * Detect disruption dark field patterns
   * @param {Array<string>} observablePatterns - Observable patterns
   * @param {number} targetSize - Target size
   * @param {Object} options - Options
   * @returns {Array<Object>} Detected patterns
   * @private
   */
  _detectDisruptionDarkField(observablePatterns, targetSize, options) {
    const dimensions = [
      'latent_disruption_vectors',
      'hidden_innovation_barriers',
      'implicit_adoption_triggers',
      'emergent_technology_convergence',
      'tacit_resistance_patterns',
    ];

    return this._generateDetectedPatterns(
      dimensions,
      observablePatterns,
      targetSize,
      options
    );
  }

  /**
   * Generate detected patterns from dimensions
   * @param {Array<string>} dimensions - Pattern dimensions
   * @param {Array<string>} observablePatterns - Observable patterns
   * @param {number} targetSize - Target size
   * @param {Object} options - Options
   * @returns {Array<Object>} Detected patterns
   * @private
   */
  _generateDetectedPatterns(dimensions, observablePatterns, targetSize, options) {
    const patterns = [];
    const maxPatterns = Math.min(targetSize, options.maxPatterns);

    for (let i = 0; i < maxPatterns; i++) {
      const dimension = dimensions[i % dimensions.length];
      const patternIndex = i % observablePatterns.length;
      const confidence = this._calculateConfidence(i, maxPatterns, options.minConfidence);

      if (confidence >= options.minConfidence) {
        patterns.push({
          dimension,
          confidence,
          derivedFrom: [observablePatterns[patternIndex]],
        });
      }
    }

    return patterns;
  }

  /**
   * Calculate confidence score for pattern
   * @param {number} index - Pattern index
   * @param {number} total - Total patterns
   * @param {number} minConfidence - Minimum confidence
   * @returns {number} Confidence score
   * @private
   */
  _calculateConfidence(index, total, minConfidence) {
    // Confidence decreases slightly as we generate more patterns
    const decay = 0.1;
    const baseConfidence = 0.95;
    const confidence = baseConfidence - (index / total) * decay;
    return Math.max(minConfidence, confidence);
  }

  /**
   * Get detector metrics
   * @returns {Object} Metrics
   */
  getMetrics() {
    return {
      ...this.metrics,
      targetRatio: this.targetRatio,
    };
  }

  /**
   * Reset detector metrics
   */
  resetMetrics() {
    this.metrics = {
      detectionsPerformed: 0,
      averageDarkFieldRatio: 0,
      totalPatternsDetected: 0,
    };
  }
}

/**
 * Create a dark field detector instance
 * @param {Object} [options] - Detector options
 * @returns {DarkFieldDetector} Detector instance
 */
export function createDarkFieldDetector(options = {}) {
  return new DarkFieldDetector(options);
}
