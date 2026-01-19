/**
 * @file Chatman Operator - μ Closure Operator Implementation
 * @module knowledge-engine/chatman-operator
 *
 * @description
 * Implements the μ (mu) closure operator from the Chatman Equation.
 * The μ operator calculates the closure of observable patterns to reveal
 * the complete artifact space including invisible dark field components.
 *
 * Core Formula: A = μ(O)
 * Where:
 * - A = Complete artifact space (100%)
 * - μ = Closure operator
 * - O = Observable patterns (~5%)
 */

import { randomUUID } from 'crypto';
import { z } from 'zod';

/**
 * Observable pattern schema
 */
export const ObservablePatternSchema = z.object({
  id: z.string().uuid().optional(),
  type: z.enum(['market', 'organizational', 'strategic', 'disruption']),
  patterns: z.array(z.string()).min(1),
  visibility: z.number().min(0).max(1).default(0.05),
  metadata: z.record(z.unknown()).optional(),
});

/**
 * Closure result schema
 */
export const ClosureResultSchema = z.object({
  observable: ObservablePatternSchema,
  darkField: z.object({
    patterns: z.array(z.string()),
    visibility: z.number(),
    coverage: z.number(),
  }),
  completeness: z.number().min(0).max(1),
  timestamp: z.number(),
  operatorId: z.string().uuid(),
});

/**
 * Chatman μ operator - Calculates closure of observable patterns
 */
export class ChatmanOperator {
  /**
   * Create a new Chatman operator
   * @param {Object} [options] - Operator options
   * @param {number} [options.observableRatio=0.05] - Observable to dark field ratio (5% default)
   * @param {number} [options.closureThreshold=0.95] - Minimum completeness threshold
   * @param {Function} [options.tracer] - OTEL tracer function
   */
  constructor(options = {}) {
    this.observableRatio = options.observableRatio ?? 0.05;
    this.closureThreshold = options.closureThreshold ?? 0.95;
    this.tracer = options.tracer;
    this.operatorId = randomUUID();
    this.metrics = {
      closureOperations: 0,
      averageCompleteness: 0,
      totalDarkFieldRevealed: 0,
    };
  }

  /**
   * Apply μ operator to observable patterns to compute closure
   * @param {Object} observable - Observable pattern
   * @returns {Promise<Object>} Closure result
   */
  async apply(observable) {
    const span = this.tracer?.startSpan?.('chatman.operator.apply');
    const startTime = Date.now();

    try {
      // Validate input
      const validated = ObservablePatternSchema.parse(observable);
      if (!validated.id) {
        validated.id = randomUUID();
      }

      span?.setAttribute?.('pattern.type', validated.type);
      span?.setAttribute?.('pattern.count', validated.patterns.length);

      // Calculate dark field using closure operator
      const darkField = this._calculateDarkField(validated);

      // Compute completeness metric
      const completeness = this._computeCompleteness(validated, darkField);

      span?.setAttribute?.('completeness', completeness);
      span?.setAttribute?.('dark_field_size', darkField.patterns.length);

      const result = {
        observable: validated,
        darkField,
        completeness,
        timestamp: Date.now(),
        operatorId: this.operatorId,
      };

      // Update metrics
      this.metrics.closureOperations++;
      this.metrics.averageCompleteness =
        (this.metrics.averageCompleteness * (this.metrics.closureOperations - 1) +
          completeness) /
        this.metrics.closureOperations;
      this.metrics.totalDarkFieldRevealed += darkField.patterns.length;

      span?.addEvent?.('closure_computed', {
        'closure.completeness': completeness,
        'closure.duration_ms': Date.now() - startTime,
      });

      return ClosureResultSchema.parse(result);
    } catch (error) {
      span?.recordException?.(error);
      span?.setStatus?.({ code: 2, message: error.message }); // ERROR
      throw error;
    } finally {
      span?.end?.();
    }
  }

  /**
   * Calculate dark field patterns from observable patterns
   * @param {Object} observable - Observable pattern
   * @returns {Object} Dark field
   * @private
   */
  _calculateDarkField(observable) {
    const observableCount = observable.patterns.length;

    // μ(O) = O ∪ DarkField
    // where |DarkField| ≈ |O| * (1/observableRatio - 1)
    const darkFieldMultiplier = 1 / this.observableRatio - 1;
    const darkFieldSize = Math.floor(observableCount * darkFieldMultiplier);

    // Generate dark field patterns based on observable type
    const darkFieldPatterns = this._generateDarkFieldPatterns(
      observable.type,
      observable.patterns,
      darkFieldSize
    );

    return {
      patterns: darkFieldPatterns,
      visibility: 1 - this.observableRatio,
      coverage: darkFieldSize / (darkFieldSize + observableCount),
    };
  }

  /**
   * Generate dark field patterns based on type and observables
   * @param {string} type - Pattern type
   * @param {Array<string>} observablePatterns - Observable patterns
   * @param {number} targetSize - Target dark field size
   * @returns {Array<string>} Dark field patterns
   * @private
   */
  _generateDarkFieldPatterns(type, observablePatterns, targetSize) {
    const darkField = [];
    const generators = {
      market: this._generateMarketDarkField.bind(this),
      organizational: this._generateOrganizationalDarkField.bind(this),
      strategic: this._generateStrategicDarkField.bind(this),
      disruption: this._generateDisruptionDarkField.bind(this),
    };

    const generator = generators[type] || generators.market;
    return generator(observablePatterns, targetSize);
  }

  /**
   * Generate market dynamics dark field
   * @param {Array<string>} patterns - Observable patterns
   * @param {number} size - Target size
   * @returns {Array<string>} Dark field patterns
   * @private
   */
  _generateMarketDarkField(patterns, size) {
    const darkField = [];
    const dimensions = [
      'hidden_customer_needs',
      'latent_market_segments',
      'unspoken_value_drivers',
      'invisible_competition',
      'emergent_demand_patterns',
      'tacit_buying_criteria',
      'underground_distribution_channels',
      'implicit_price_expectations',
    ];

    for (let i = 0; i < size; i++) {
      const dimension = dimensions[i % dimensions.length];
      const patternIndex = i % patterns.length;
      darkField.push(`${dimension}:derived_from:${patterns[patternIndex]}`);
    }

    return darkField;
  }

  /**
   * Generate organizational dynamics dark field
   * @param {Array<string>} patterns - Observable patterns
   * @param {number} size - Target size
   * @returns {Array<string>} Dark field patterns
   * @private
   */
  _generateOrganizationalDarkField(patterns, size) {
    const darkField = [];
    const dimensions = [
      'informal_power_structures',
      'tacit_knowledge_networks',
      'hidden_resource_flows',
      'implicit_decision_criteria',
      'unwritten_cultural_norms',
      'latent_capability_reserves',
      'invisible_coordination_mechanisms',
      'emergent_collaboration_patterns',
    ];

    for (let i = 0; i < size; i++) {
      const dimension = dimensions[i % dimensions.length];
      const patternIndex = i % patterns.length;
      darkField.push(`${dimension}:derived_from:${patterns[patternIndex]}`);
    }

    return darkField;
  }

  /**
   * Generate strategic dynamics dark field
   * @param {Array<string>} patterns - Observable patterns
   * @param {number} size - Target size
   * @returns {Array<string>} Dark field patterns
   * @private
   */
  _generateStrategicDarkField(patterns, size) {
    const darkField = [];
    const dimensions = [
      'latent_strategic_options',
      'hidden_competitive_advantages',
      'implicit_strategic_assumptions',
      'emergent_industry_shifts',
      'tacit_success_factors',
      'invisible_threat_vectors',
      'unrecognized_opportunities',
      'hidden_ecosystem_dependencies',
    ];

    for (let i = 0; i < size; i++) {
      const dimension = dimensions[i % dimensions.length];
      const patternIndex = i % patterns.length;
      darkField.push(`${dimension}:derived_from:${patterns[patternIndex]}`);
    }

    return darkField;
  }

  /**
   * Generate disruption arithmetic dark field
   * @param {Array<string>} patterns - Observable patterns
   * @param {number} size - Target size
   * @returns {Array<string>} Dark field patterns
   * @private
   */
  _generateDisruptionDarkField(patterns, size) {
    const darkField = [];
    const dimensions = [
      'latent_disruption_vectors',
      'hidden_innovation_barriers',
      'implicit_adoption_triggers',
      'emergent_technology_convergence',
      'tacit_resistance_patterns',
      'invisible_tipping_points',
      'unrecognized_substitutes',
      'hidden_value_chain_vulnerabilities',
    ];

    for (let i = 0; i < size; i++) {
      const dimension = dimensions[i % dimensions.length];
      const patternIndex = i % patterns.length;
      darkField.push(`${dimension}:derived_from:${patterns[patternIndex]}`);
    }

    return darkField;
  }

  /**
   * Compute completeness of closure
   * @param {Object} observable - Observable pattern
   * @param {Object} darkField - Dark field
   * @returns {number} Completeness ratio [0, 1]
   * @private
   */
  _computeCompleteness(observable, darkField) {
    const totalPatterns = observable.patterns.length + darkField.patterns.length;
    const coverageScore = darkField.patterns.length / totalPatterns;

    // Completeness is high when dark field coverage approaches (1 - observableRatio)
    const targetCoverage = 1 - this.observableRatio;
    const completeness = Math.min(1, coverageScore / targetCoverage);

    return completeness;
  }

  /**
   * Get operator metrics
   * @returns {Object} Metrics
   */
  getMetrics() {
    return {
      ...this.metrics,
      operatorId: this.operatorId,
      observableRatio: this.observableRatio,
      closureThreshold: this.closureThreshold,
    };
  }

  /**
   * Reset operator metrics
   */
  resetMetrics() {
    this.metrics = {
      closureOperations: 0,
      averageCompleteness: 0,
      totalDarkFieldRevealed: 0,
    };
  }
}

/**
 * Create a Chatman operator instance
 * @param {Object} [options] - Operator options
 * @returns {ChatmanOperator} Operator instance
 */
export function createChatmanOperator(options = {}) {
  return new ChatmanOperator(options);
}
