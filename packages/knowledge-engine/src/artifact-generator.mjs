/**
 * @file Artifact Generator - A = μ(O) Calculator
 * @module knowledge-engine/artifact-generator
 *
 * @description
 * Implements the artifact generation function A = μ(O) from the Chatman Equation.
 * Transforms observable patterns into complete artifact spaces including dark field.
 */

import { randomUUID } from 'crypto';
import { z } from 'zod';
import { ChatmanOperator } from './chatman-operator.mjs';

/**
 * Artifact schema
 */
export const ArtifactSchema = z.object({
  id: z.string().uuid(),
  type: z.enum(['market', 'organizational', 'strategic', 'disruption']),
  observable: z.object({
    patterns: z.array(z.string()),
    visibility: z.number(),
  }),
  darkField: z.object({
    patterns: z.array(z.string()),
    visibility: z.number(),
    coverage: z.number(),
  }),
  totalPatterns: z.number().int().positive(),
  completeness: z.number().min(0).max(1),
  generatedAt: z.number(),
  receipt: z
    .object({
      id: z.string().uuid(),
      operation: z.literal('artifact_generation'),
      timestamp: z.number(),
    })
    .optional(),
});

/**
 * Generation options schema
 */
export const GenerationOptionsSchema = z.object({
  includeReceipt: z.boolean().default(true),
  observableRatio: z.number().min(0).max(1).default(0.05),
  validate: z.boolean().default(true),
  metadata: z.record(z.unknown()).optional(),
});

/**
 * Artifact generator for Chatman Equation
 */
export class ArtifactGenerator {
  /**
   * Create a new artifact generator
   * @param {Object} [options] - Generator options
   * @param {Function} [options.tracer] - OTEL tracer function
   * @param {Function} [options.receiptGenerator] - Receipt generation function
   */
  constructor(options = {}) {
    this.tracer = options.tracer;
    this.receiptGenerator = options.receiptGenerator;
    this.operator = new ChatmanOperator({
      tracer: this.tracer,
      observableRatio: options.observableRatio,
    });
    this.metrics = {
      artifactsGenerated: 0,
      averageCompleteness: 0,
      averageDarkFieldSize: 0,
    };
  }

  /**
   * Generate artifact from observable patterns: A = μ(O)
   * @param {Object} observable - Observable pattern
   * @param {Object} [options] - Generation options
   * @returns {Promise<Object>} Generated artifact
   */
  async generate(observable, options = {}) {
    const span = this.tracer?.startSpan?.('chatman.artifact.generate');
    const startTime = Date.now();

    try {
      const opts = GenerationOptionsSchema.parse(options);

      span?.setAttribute?.('observable.type', observable.type);
      span?.setAttribute?.('observable.pattern_count', observable.patterns.length);
      span?.setAttribute?.('options.include_receipt', opts.includeReceipt);

      // Apply μ operator to compute closure
      const closure = await this.operator.apply(observable);

      // Generate artifact from closure
      const artifact = {
        id: randomUUID(),
        type: observable.type,
        observable: {
          patterns: closure.observable.patterns,
          visibility: closure.observable.visibility,
        },
        darkField: closure.darkField,
        totalPatterns:
          closure.observable.patterns.length + closure.darkField.patterns.length,
        completeness: closure.completeness,
        generatedAt: Date.now(),
      };

      // Generate receipt if requested
      if (opts.includeReceipt) {
        artifact.receipt = this._generateReceipt(artifact, closure);
      }

      // Validate artifact
      if (opts.validate) {
        ArtifactSchema.parse(artifact);
      }

      // Update metrics
      this.metrics.artifactsGenerated++;
      this.metrics.averageCompleteness =
        (this.metrics.averageCompleteness * (this.metrics.artifactsGenerated - 1) +
          artifact.completeness) /
        this.metrics.artifactsGenerated;
      this.metrics.averageDarkFieldSize =
        (this.metrics.averageDarkFieldSize * (this.metrics.artifactsGenerated - 1) +
          artifact.darkField.patterns.length) /
        this.metrics.artifactsGenerated;

      span?.addEvent?.('artifact_generated', {
        'artifact.id': artifact.id,
        'artifact.completeness': artifact.completeness,
        'artifact.total_patterns': artifact.totalPatterns,
        'artifact.duration_ms': Date.now() - startTime,
      });

      return artifact;
    } catch (error) {
      span?.recordException?.(error);
      span?.setStatus?.({ code: 2, message: error.message });
      throw error;
    } finally {
      span?.end?.();
    }
  }

  /**
   * Generate multiple artifacts in batch
   * @param {Array<Object>} observables - Array of observable patterns
   * @param {Object} [options] - Generation options
   * @returns {Promise<Array<Object>>} Generated artifacts
   */
  async generateBatch(observables, options = {}) {
    const span = this.tracer?.startSpan?.('chatman.artifact.generate_batch');

    try {
      span?.setAttribute?.('batch_size', observables.length);

      const artifacts = await Promise.all(
        observables.map(observable => this.generate(observable, options))
      );

      span?.addEvent?.('batch_generated', {
        'batch.count': artifacts.length,
        'batch.avg_completeness': this._calculateAverageCompleteness(artifacts),
      });

      return artifacts;
    } catch (error) {
      span?.recordException?.(error);
      span?.setStatus?.({ code: 2, message: error.message });
      throw error;
    } finally {
      span?.end?.();
    }
  }

  /**
   * Generate receipt for artifact
   * @param {Object} artifact - Generated artifact
   * @param {Object} closure - Closure result
   * @returns {Object} Receipt
   * @private
   */
  _generateReceipt(artifact, closure) {
    if (this.receiptGenerator) {
      return this.receiptGenerator({
        operation: 'artifact_generation',
        entityType: 'ChatmanArtifact',
        entityId: artifact.id,
        data: {
          observableCount: artifact.observable.patterns.length,
          darkFieldCount: artifact.darkField.patterns.length,
          completeness: artifact.completeness,
          operatorId: closure.operatorId,
        },
      });
    }

    // Default receipt
    return {
      id: randomUUID(),
      operation: 'artifact_generation',
      timestamp: Date.now(),
    };
  }

  /**
   * Calculate average completeness from artifacts
   * @param {Array<Object>} artifacts - Artifacts
   * @returns {number} Average completeness
   * @private
   */
  _calculateAverageCompleteness(artifacts) {
    if (artifacts.length === 0) return 0;
    const sum = artifacts.reduce((acc, a) => acc + a.completeness, 0);
    return sum / artifacts.length;
  }

  /**
   * Get generator metrics
   * @returns {Object} Metrics
   */
  getMetrics() {
    return {
      ...this.metrics,
      operator: this.operator.getMetrics(),
    };
  }

  /**
   * Reset generator metrics
   */
  resetMetrics() {
    this.metrics = {
      artifactsGenerated: 0,
      averageCompleteness: 0,
      averageDarkFieldSize: 0,
    };
    this.operator.resetMetrics();
  }
}

/**
 * Create an artifact generator instance
 * @param {Object} [options] - Generator options
 * @returns {ArtifactGenerator} Generator instance
 */
export function createArtifactGenerator(options = {}) {
  return new ArtifactGenerator(options);
}
