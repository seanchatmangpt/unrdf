/**
 * @file Formation Theorems - Blue Ocean Calculus
 * @module knowledge-engine/formation-theorems
 *
 * @description
 * Implements the formation theorems from blue ocean calculus, deriving
 * strategic formations from dark field patterns in the Chatman Equation.
 *
 * Key Theorems:
 * 1. Formation Emergence: Uncontested spaces emerge from dark field
 * 2. Value Innovation: New value curves from pattern recombination
 * 3. Strategic Canvas: Competitive factors mapped to dark field dimensions
 */

import { randomUUID } from 'crypto';
import { z } from 'zod';

/**
 * Formation schema
 */
export const FormationSchema = z.object({
  id: z.string().uuid(),
  theorem: z.enum(['emergence', 'value_innovation', 'strategic_canvas', 'four_actions']),
  type: z.enum(['market', 'organizational', 'strategic', 'disruption']),
  input: z.object({
    darkFieldPatterns: z.array(z.string()),
    observablePatterns: z.array(z.string()),
  }),
  output: z.object({
    formation: z.array(z.string()),
    valueProposition: z.string(),
    strategicMoves: z.array(z.string()),
  }),
  confidence: z.number().min(0).max(1),
  derivedAt: z.number(),
});

/**
 * Formation options schema
 */
export const FormationOptionsSchema = z.object({
  theorem: z
    .enum(['emergence', 'value_innovation', 'strategic_canvas', 'four_actions'])
    .default('emergence'),
  minConfidence: z.number().min(0).max(1).default(0.7),
  maxFormations: z.number().int().positive().default(10),
});

/**
 * Formation theorems calculator
 */
export class FormationTheorems {
  /**
   * Create a new formation theorems calculator
   * @param {Object} [options] - Calculator options
   * @param {Function} [options.tracer] - OTEL tracer function
   */
  constructor(options = {}) {
    this.tracer = options.tracer;
    this.metrics = {
      theoremsApplied: 0,
      formationsGenerated: 0,
      averageConfidence: 0,
    };
  }

  /**
   * Apply formation theorem to derive strategic formations
   * @param {Object} artifact - Artifact with observable and dark field
   * @param {Object} [options] - Formation options
   * @returns {Promise<Object>} Formation result
   */
  async derive(artifact, options = {}) {
    const span = this.tracer?.startSpan?.('chatman.formation.derive');
    const startTime = Date.now();

    try {
      const opts = FormationOptionsSchema.parse(options);

      span?.setAttribute?.('artifact.type', artifact.type);
      span?.setAttribute?.('theorem', opts.theorem);
      span?.setAttribute?.('dark_field_size', artifact.darkField.patterns.length);

      const theoremFn = this._getTheoremFunction(opts.theorem);
      const formation = theoremFn(artifact, opts);

      // Update metrics
      this.metrics.theoremsApplied++;
      this.metrics.formationsGenerated += formation.output.formation.length;
      this.metrics.averageConfidence =
        (this.metrics.averageConfidence * (this.metrics.theoremsApplied - 1) +
          formation.confidence) /
        this.metrics.theoremsApplied;

      span?.addEvent?.('formation_derived', {
        'formation.id': formation.id,
        'formation.theorem': formation.theorem,
        'formation.confidence': formation.confidence,
        'formation.size': formation.output.formation.length,
        'formation.duration_ms': Date.now() - startTime,
      });

      return FormationSchema.parse(formation);
    } catch (error) {
      span?.recordException?.(error);
      span?.setStatus?.({ code: 2, message: error.message });
      throw error;
    } finally {
      span?.end?.();
    }
  }

  /**
   * Get theorem function by name
   * @param {string} theorem - Theorem name
   * @returns {Function} Theorem function
   * @private
   */
  _getTheoremFunction(theorem) {
    const theorems = {
      emergence: this._applyEmergenceTheorem.bind(this),
      value_innovation: this._applyValueInnovationTheorem.bind(this),
      strategic_canvas: this._applyStrategicCanvasTheorem.bind(this),
      four_actions: this._applyFourActionsTheorem.bind(this),
    };
    return theorems[theorem];
  }

  /**
   * Apply emergence theorem - uncontested spaces from dark field
   * @param {Object} artifact - Artifact
   * @param {Object} options - Options
   * @returns {Object} Formation
   * @private
   */
  _applyEmergenceTheorem(artifact, options) {
    const formation = [];
    const strategicMoves = [];

    // Identify uncontested spaces in dark field
    const uncontestedPatterns = artifact.darkField.patterns
      .filter((p, i) => i < options.maxFormations)
      .filter(p => p.includes('latent') || p.includes('emergent') || p.includes('hidden'));

    for (const pattern of uncontestedPatterns) {
      formation.push(`blue_ocean_space:${pattern}`);
      strategicMoves.push(`explore:${pattern}`);
    }

    const valueProposition = `Create uncontested market space in: ${uncontestedPatterns
      .map(p => p.split(':')[0])
      .join(', ')}`;

    return {
      id: randomUUID(),
      theorem: 'emergence',
      type: artifact.type,
      input: {
        darkFieldPatterns: artifact.darkField.patterns,
        observablePatterns: artifact.observable.patterns,
      },
      output: {
        formation,
        valueProposition,
        strategicMoves,
      },
      confidence: 0.85,
      derivedAt: Date.now(),
    };
  }

  /**
   * Apply value innovation theorem - new value curves from recombination
   * @param {Object} artifact - Artifact
   * @param {Object} options - Options
   * @returns {Object} Formation
   * @private
   */
  _applyValueInnovationTheorem(artifact, options) {
    const formation = [];
    const strategicMoves = [];

    // Recombine dark field and observable to create value innovations
    const innovations = artifact.darkField.patterns
      .slice(0, Math.min(5, options.maxFormations))
      .map((darkPattern, i) => {
        const observablePattern =
          artifact.observable.patterns[i % artifact.observable.patterns.length];
        return `${darkPattern}+${observablePattern}`;
      });

    for (const innovation of innovations) {
      formation.push(`value_innovation:${innovation}`);
      strategicMoves.push(`innovate:${innovation}`);
    }

    const valueProposition = `Simultaneous differentiation and low cost through: ${innovations.length} value innovations`;

    return {
      id: randomUUID(),
      theorem: 'value_innovation',
      type: artifact.type,
      input: {
        darkFieldPatterns: artifact.darkField.patterns,
        observablePatterns: artifact.observable.patterns,
      },
      output: {
        formation,
        valueProposition,
        strategicMoves,
      },
      confidence: 0.8,
      derivedAt: Date.now(),
    };
  }

  /**
   * Apply strategic canvas theorem - map competitive factors
   * @param {Object} artifact - Artifact
   * @param {Object} options - Options
   * @returns {Object} Formation
   * @private
   */
  _applyStrategicCanvasTheorem(artifact, options) {
    const formation = [];
    const strategicMoves = [];

    // Map observable (current factors) vs dark field (new factors)
    const currentFactors = artifact.observable.patterns.slice(0, 5);
    const newFactors = artifact.darkField.patterns
      .filter(p => p.includes('hidden') || p.includes('latent'))
      .slice(0, 5);

    // Eliminate-Reduce-Raise-Create framework
    strategicMoves.push(`eliminate:${currentFactors[0]}`);
    strategicMoves.push(`reduce:${currentFactors[1]}`);
    strategicMoves.push(`raise:${newFactors[0]}`);
    strategicMoves.push(`create:${newFactors[1]}`);

    formation.push(`strategic_canvas:eliminate-reduce-raise-create`);
    formation.push(...newFactors.map(f => `new_factor:${f}`));

    const valueProposition = `Reconstruct market boundaries with ${newFactors.length} new competitive factors`;

    return {
      id: randomUUID(),
      theorem: 'strategic_canvas',
      type: artifact.type,
      input: {
        darkFieldPatterns: artifact.darkField.patterns,
        observablePatterns: artifact.observable.patterns,
      },
      output: {
        formation,
        valueProposition,
        strategicMoves,
      },
      confidence: 0.78,
      derivedAt: Date.now(),
    };
  }

  /**
   * Apply four actions theorem - ERRC framework
   * @param {Object} artifact - Artifact
   * @param {Object} options - Options
   * @returns {Object} Formation
   * @private
   */
  _applyFourActionsTheorem(artifact, options) {
    const formation = [];
    const strategicMoves = [];

    const observable = artifact.observable.patterns;
    const darkField = artifact.darkField.patterns;

    // Eliminate: factors to remove
    const eliminate = observable.slice(0, 2).map(p => `eliminate:${p}`);

    // Reduce: factors to reduce below industry standard
    const reduce = observable.slice(2, 4).map(p => `reduce:${p}`);

    // Raise: factors to raise above industry standard
    const raise = darkField
      .filter(p => p.includes('emergent'))
      .slice(0, 2)
      .map(p => `raise:${p}`);

    // Create: factors never offered by industry
    const create = darkField
      .filter(p => p.includes('hidden') || p.includes('latent'))
      .slice(0, 3)
      .map(p => `create:${p}`);

    formation.push(...eliminate, ...reduce, ...raise, ...create);
    strategicMoves.push(...eliminate, ...reduce, ...raise, ...create);

    const valueProposition = `Four Actions Framework: Eliminate ${eliminate.length}, Reduce ${reduce.length}, Raise ${raise.length}, Create ${create.length}`;

    return {
      id: randomUUID(),
      theorem: 'four_actions',
      type: artifact.type,
      input: {
        darkFieldPatterns: artifact.darkField.patterns,
        observablePatterns: artifact.observable.patterns,
      },
      output: {
        formation,
        valueProposition,
        strategicMoves,
      },
      confidence: 0.82,
      derivedAt: Date.now(),
    };
  }

  /**
   * Get calculator metrics
   * @returns {Object} Metrics
   */
  getMetrics() {
    return { ...this.metrics };
  }

  /**
   * Reset calculator metrics
   */
  resetMetrics() {
    this.metrics = {
      theoremsApplied: 0,
      formationsGenerated: 0,
      averageConfidence: 0,
    };
  }
}

/**
 * Create a formation theorems calculator instance
 * @param {Object} [options] - Calculator options
 * @returns {FormationTheorems} Calculator instance
 */
export function createFormationTheorems(options = {}) {
  return new FormationTheorems(options);
}
