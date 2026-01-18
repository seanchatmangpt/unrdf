/**
 * @file Chatman Engine - Unified Chatman Equation Integration
 * @module knowledge-engine/chatman-engine
 *
 * @description
 * Unified engine for executing Chatman Equation operations on the knowledge graph.
 * Integrates operator, artifact generator, dark field detector, formation theorems,
 * and all dynamics rules with OTEL observability and receipt generation.
 */

import { randomUUID } from 'crypto';
import { z } from 'zod';
import { ChatmanOperator } from './chatman-operator.mjs';
import { ArtifactGenerator } from './artifact-generator.mjs';
import { DarkFieldDetector } from './dark-field-detector.mjs';
import { FormationTheorems } from './formation-theorems.mjs';
import { ChatmanConfigLoader } from './chatman-config-loader.mjs';

/**
 * Engine options schema
 */
export const EngineOptionsSchema = z.object({
  observableRatio: z.number().min(0).max(1).default(0.05),
  closureThreshold: z.number().min(0).max(1).default(0.95),
  enableReceipts: z.boolean().default(true),
  tracer: z.function().optional(),
  receiptGenerator: z.function().optional(),
});

/**
 * Execution result schema
 */
export const ExecutionResultSchema = z.object({
  id: z.string().uuid(),
  operation: z.enum(['closure', 'artifact', 'detection', 'formation', 'full_pipeline']),
  input: z.record(z.unknown()),
  output: z.record(z.unknown()),
  receipt: z
    .object({
      id: z.string().uuid(),
      operation: z.string(),
      timestamp: z.number(),
    })
    .optional(),
  metrics: z.record(z.unknown()),
  executedAt: z.number(),
});

/**
 * Chatman Engine - Executes Chatman Equation operations
 */
export class ChatmanEngine {
  /**
   * Create a new Chatman engine
   * @param {Object} [options] - Engine options
   */
  constructor(options = {}) {
    const opts = EngineOptionsSchema.parse(options);

    this.tracer = opts.tracer;
    this.receiptGenerator = opts.receiptGenerator;
    this.enableReceipts = opts.enableReceipts;

    // Initialize components
    this.operator = new ChatmanOperator({
      tracer: this.tracer,
      observableRatio: opts.observableRatio,
      closureThreshold: opts.closureThreshold,
    });

    this.artifactGenerator = new ArtifactGenerator({
      tracer: this.tracer,
      receiptGenerator: this.receiptGenerator,
      observableRatio: opts.observableRatio,
    });

    this.darkFieldDetector = new DarkFieldDetector({
      tracer: this.tracer,
      targetRatio: 1 - opts.observableRatio,
    });

    this.formationTheorems = new FormationTheorems({
      tracer: this.tracer,
    });

    this.configLoader = new ChatmanConfigLoader({
      tracer: this.tracer,
    });

    this.metrics = {
      operationsExecuted: 0,
      receiptsGenerated: 0,
      averageExecutionTime: 0,
    };
  }

  /**
   * Execute closure operation: A = μ(O)
   * @param {Object} observable - Observable pattern
   * @param {Object} [options] - Execution options
   * @returns {Promise<Object>} Execution result
   */
  async executeClosure(observable, options = {}) {
    const span = this.tracer?.startSpan?.('chatman.engine.execute_closure');
    const startTime = Date.now();

    try {
      span?.setAttribute?.('observable.type', observable.type);

      const closure = await this.operator.apply(observable);
      const receipt = this._createReceipt('closure', { observable, closure });

      const result = {
        id: randomUUID(),
        operation: 'closure',
        input: { observable },
        output: { closure },
        receipt: this.enableReceipts ? receipt : undefined,
        metrics: this.operator.getMetrics(),
        executedAt: Date.now(),
      };

      this._updateMetrics(startTime);

      span?.addEvent?.('closure_executed', {
        'result.id': result.id,
        'result.completeness': closure.completeness,
      });

      return result;
    } catch (error) {
      span?.recordException?.(error);
      span?.setStatus?.({ code: 2, message: error.message });
      throw error;
    } finally {
      span?.end?.();
    }
  }

  /**
   * Execute artifact generation: A = μ(O)
   * @param {Object} observable - Observable pattern
   * @param {Object} [options] - Execution options
   * @returns {Promise<Object>} Execution result
   */
  async executeArtifact(observable, options = {}) {
    const span = this.tracer?.startSpan?.('chatman.engine.execute_artifact');
    const startTime = Date.now();

    try {
      span?.setAttribute?.('observable.type', observable.type);

      const artifact = await this.artifactGenerator.generate(observable, {
        includeReceipt: this.enableReceipts,
        ...options,
      });

      const receipt = this._createReceipt('artifact', { observable, artifact });

      const result = {
        id: randomUUID(),
        operation: 'artifact',
        input: { observable },
        output: { artifact },
        receipt: this.enableReceipts ? receipt : undefined,
        metrics: this.artifactGenerator.getMetrics(),
        executedAt: Date.now(),
      };

      this._updateMetrics(startTime);

      span?.addEvent?.('artifact_executed', {
        'result.id': result.id,
        'artifact.id': artifact.id,
      });

      return result;
    } catch (error) {
      span?.recordException?.(error);
      span?.setStatus?.({ code: 2, message: error.message });
      throw error;
    } finally {
      span?.end?.();
    }
  }

  /**
   * Execute dark field detection
   * @param {Object} observable - Observable pattern
   * @param {Object} [options] - Execution options
   * @returns {Promise<Object>} Execution result
   */
  async executeDetection(observable, options = {}) {
    const span = this.tracer?.startSpan?.('chatman.engine.execute_detection');
    const startTime = Date.now();

    try {
      span?.setAttribute?.('observable.type', observable.type);

      const detection = await this.darkFieldDetector.detect(observable, options);
      const receipt = this._createReceipt('detection', { observable, detection });

      const result = {
        id: randomUUID(),
        operation: 'detection',
        input: { observable },
        output: { detection },
        receipt: this.enableReceipts ? receipt : undefined,
        metrics: this.darkFieldDetector.getMetrics(),
        executedAt: Date.now(),
      };

      this._updateMetrics(startTime);

      span?.addEvent?.('detection_executed', {
        'result.id': result.id,
        'detection.id': detection.id,
      });

      return result;
    } catch (error) {
      span?.recordException?.(error);
      span?.setStatus?.({ code: 2, message: error.message });
      throw error;
    } finally {
      span?.end?.();
    }
  }

  /**
   * Execute formation theorem derivation
   * @param {Object} artifact - Artifact with observable and dark field
   * @param {Object} [options] - Execution options
   * @returns {Promise<Object>} Execution result
   */
  async executeFormation(artifact, options = {}) {
    const span = this.tracer?.startSpan?.('chatman.engine.execute_formation');
    const startTime = Date.now();

    try {
      span?.setAttribute?.('artifact.type', artifact.type);
      span?.setAttribute?.('theorem', options.theorem || 'emergence');

      const formation = await this.formationTheorems.derive(artifact, options);
      const receipt = this._createReceipt('formation', { artifact, formation });

      const result = {
        id: randomUUID(),
        operation: 'formation',
        input: { artifact },
        output: { formation },
        receipt: this.enableReceipts ? receipt : undefined,
        metrics: this.formationTheorems.getMetrics(),
        executedAt: Date.now(),
      };

      this._updateMetrics(startTime);

      span?.addEvent?.('formation_executed', {
        'result.id': result.id,
        'formation.id': formation.id,
      });

      return result;
    } catch (error) {
      span?.recordException?.(error);
      span?.setStatus?.({ code: 2, message: error.message });
      throw error;
    } finally {
      span?.end?.();
    }
  }

  /**
   * Execute full Chatman pipeline: Observable → Artifact → Formation
   * @param {Object} observable - Observable pattern
   * @param {Object} [options] - Execution options
   * @returns {Promise<Object>} Execution result
   */
  async executePipeline(observable, options = {}) {
    const span = this.tracer?.startSpan?.('chatman.engine.execute_pipeline');
    const startTime = Date.now();

    try {
      span?.setAttribute?.('observable.type', observable.type);

      // Step 1: Generate artifact
      const artifactResult = await this.executeArtifact(observable, options);

      // Step 2: Detect dark field
      const detectionResult = await this.executeDetection(observable, options);

      // Step 3: Derive formation
      const formationResult = await this.executeFormation(
        artifactResult.output.artifact,
        options
      );

      const receipt = this._createReceipt('full_pipeline', {
        observable,
        artifact: artifactResult.output.artifact,
        detection: detectionResult.output.detection,
        formation: formationResult.output.formation,
      });

      const result = {
        id: randomUUID(),
        operation: 'full_pipeline',
        input: { observable },
        output: {
          artifact: artifactResult.output.artifact,
          detection: detectionResult.output.detection,
          formation: formationResult.output.formation,
        },
        receipt: this.enableReceipts ? receipt : undefined,
        metrics: {
          operator: this.operator.getMetrics(),
          artifactGenerator: this.artifactGenerator.getMetrics(),
          darkFieldDetector: this.darkFieldDetector.getMetrics(),
          formationTheorems: this.formationTheorems.getMetrics(),
        },
        executedAt: Date.now(),
      };

      this._updateMetrics(startTime);

      span?.addEvent?.('pipeline_executed', {
        'result.id': result.id,
        'pipeline.duration_ms': Date.now() - startTime,
      });

      return result;
    } catch (error) {
      span?.recordException?.(error);
      span?.setStatus?.({ code: 2, message: error.message });
      throw error;
    } finally {
      span?.end?.();
    }
  }

  /**
   * Load configuration from TOML file
   * @param {string} configPath - Path to config file
   * @param {Object} [options] - Load options
   * @returns {Promise<Object>} Loaded configuration
   */
  async loadConfig(configPath, options = {}) {
    return this.configLoader.load(configPath, options);
  }

  /**
   * Create receipt for operation
   * @param {string} operation - Operation name
   * @param {Object} data - Operation data
   * @returns {Object} Receipt
   * @private
   */
  _createReceipt(operation, data) {
    if (this.receiptGenerator) {
      return this.receiptGenerator({
        operation: `chatman_${operation}`,
        entityType: 'ChatmanOperation',
        entityId: randomUUID(),
        data,
      });
    }

    const receipt = {
      id: randomUUID(),
      operation: `chatman_${operation}`,
      timestamp: Date.now(),
    };

    if (this.enableReceipts) {
      this.metrics.receiptsGenerated++;
    }

    return receipt;
  }

  /**
   * Update engine metrics
   * @param {number} startTime - Operation start time
   * @private
   */
  _updateMetrics(startTime) {
    this.metrics.operationsExecuted++;
    const duration = Date.now() - startTime;
    this.metrics.averageExecutionTime =
      (this.metrics.averageExecutionTime * (this.metrics.operationsExecuted - 1) +
        duration) /
      this.metrics.operationsExecuted;
  }

  /**
   * Get engine metrics
   * @returns {Object} Metrics
   */
  getMetrics() {
    return {
      ...this.metrics,
      components: {
        operator: this.operator.getMetrics(),
        artifactGenerator: this.artifactGenerator.getMetrics(),
        darkFieldDetector: this.darkFieldDetector.getMetrics(),
        formationTheorems: this.formationTheorems.getMetrics(),
      },
    };
  }

  /**
   * Reset all metrics
   */
  resetMetrics() {
    this.metrics = {
      operationsExecuted: 0,
      receiptsGenerated: 0,
      averageExecutionTime: 0,
    };
    this.operator.resetMetrics();
    this.artifactGenerator.resetMetrics();
    this.darkFieldDetector.resetMetrics();
    this.formationTheorems.resetMetrics();
  }
}

/**
 * Create a Chatman engine instance
 * @param {Object} [options] - Engine options
 * @returns {ChatmanEngine} Engine instance
 */
export function createChatmanEngine(options = {}) {
  return new ChatmanEngine(options);
}
