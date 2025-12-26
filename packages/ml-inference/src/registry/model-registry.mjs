/**
 * @file Model Registry - Version management and hot-swapping for ML models
 * @module ml-inference/registry/model-registry
 *
 * @description
 * Manages multiple model versions with A/B testing and zero-downtime hot-swapping.
 * Provides model versioning, metadata tracking, and deployment strategies.
 */

import { trace } from '@opentelemetry/api';
import { z } from 'zod';
import { createONNXRunner } from '../runtime/onnx-runner.mjs';

const tracer = trace.getTracer('@unrdf/ml-inference');

/**
 * Model metadata schema
 */
const ModelMetadataSchema = z.object({
  name: z.string().min(1),
  version: z.string().min(1),
  description: z.string().optional(),
  tags: z.array(z.string()).default([]),
  framework: z.string().default('onnx'),
  inputShape: z.array(z.number()).optional(),
  outputShape: z.array(z.number()).optional(),
  accuracy: z.number().min(0).max(1).optional(),
  createdAt: z.number().default(() => Date.now()),
});

/**
 * Deployment strategy schema
 */
const DeploymentStrategySchema = z.enum(['blue-green', 'canary', 'immediate']).default('immediate');

/**
 * Model Registry
 * Manages model versions, deployment, and A/B testing
 */
export class ModelRegistry {
  /**
   *
   */
  constructor() {
    this.models = new Map(); // version -> { runner, metadata, stats }
    this.activeVersion = null;
    this.canaryVersion = null;
    this.canaryTrafficPercent = 0;
  }

  /**
   * Register new model version
   *
   * @param {string} version - Model version identifier
   * @param {string|Uint8Array} modelPathOrBuffer - Path to model or buffer
   * @param {Object} metadata - Model metadata
   * @param {Object} [runnerOptions] - ONNX runner options
   * @returns {Promise<void>}
   *
   * @example
   * await registry.register('v1.0', './models/model-v1.onnx', {
   *   name: 'classifier',
   *   description: 'Text classifier model',
   *   accuracy: 0.95
   * });
   */
  async register(version, modelPathOrBuffer, metadata, runnerOptions = {}) {
    return tracer.startActiveSpan('registry.register', async span => {
      try {
        const validatedMetadata = ModelMetadataSchema.parse({
          ...metadata,
          version,
        });

        // Create runner and load model
        const runner = createONNXRunner(runnerOptions);
        const loadTime = await runner.loadModel(modelPathOrBuffer);

        // Store model
        this.models.set(version, {
          runner,
          metadata: validatedMetadata,
          stats: {
            loadTimeMs: loadTime,
            deployedAt: Date.now(),
            inferences: 0,
            errors: 0,
          },
        });

        // Set as active if first model
        if (!this.activeVersion) {
          this.activeVersion = version;
        }

        span.setAttributes({
          'model.version': version,
          'model.name': validatedMetadata.name,
          'model.loadTimeMs': loadTime,
        });

        span.setStatus({ code: 1 }); // OK
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: 2, message: error.message }); // ERROR
        throw new Error(`Failed to register model ${version}: ${error.message}`);
      } finally {
        span.end();
      }
    });
  }

  /**
   * Deploy model version with strategy
   *
   * @param {string} version - Version to deploy
   * @param {string} [strategy='immediate'] - Deployment strategy
   * @param {number} [canaryPercent=10] - Canary traffic percentage
   * @returns {Promise<void>}
   *
   * @example
   * // Immediate deployment
   * await registry.deploy('v2.0');
   *
   * // Canary deployment (10% traffic)
   * await registry.deploy('v2.0', 'canary', 10);
   */
  async deploy(version, strategy = 'immediate', canaryPercent = 10) {
    return tracer.startActiveSpan('registry.deploy', async span => {
      try {
        if (!this.models.has(version)) {
          throw new Error(`Model version ${version} not found`);
        }

        const validatedStrategy = DeploymentStrategySchema.parse(strategy);

        span.setAttributes({
          'deployment.version': version,
          'deployment.strategy': validatedStrategy,
        });

        switch (validatedStrategy) {
          case 'immediate':
            this.activeVersion = version;
            this.canaryVersion = null;
            this.canaryTrafficPercent = 0;
            break;

          case 'blue-green':
            // Instant switch
            this.activeVersion = version;
            this.canaryVersion = null;
            this.canaryTrafficPercent = 0;
            break;

          case 'canary':
            this.canaryVersion = version;
            this.canaryTrafficPercent = canaryPercent;
            span.setAttribute('deployment.canaryPercent', canaryPercent);
            break;
        }

        span.setStatus({ code: 1 }); // OK
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: 2, message: error.message }); // ERROR
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Promote canary to active version
   *
   * @returns {void}
   */
  promoteCanary() {
    if (!this.canaryVersion) {
      throw new Error('No canary version to promote');
    }

    this.activeVersion = this.canaryVersion;
    this.canaryVersion = null;
    this.canaryTrafficPercent = 0;
  }

  /**
   * Rollback canary deployment
   *
   * @returns {void}
   */
  rollbackCanary() {
    if (!this.canaryVersion) {
      throw new Error('No canary version to rollback');
    }

    this.canaryVersion = null;
    this.canaryTrafficPercent = 0;
  }

  /**
   * Get runner for inference (handles A/B routing)
   *
   * @returns {Object} ONNX runner instance
   */
  getRunner() {
    // Canary routing
    if (this.canaryVersion && Math.random() * 100 < this.canaryTrafficPercent) {
      const model = this.models.get(this.canaryVersion);
      model.stats.inferences++;
      return model.runner;
    }

    // Active version
    if (!this.activeVersion) {
      throw new Error('No active model version');
    }

    const model = this.models.get(this.activeVersion);
    model.stats.inferences++;
    return model.runner;
  }

  /**
   * Get model metadata
   *
   * @param {string} version - Model version
   * @returns {Object|null} Model metadata and stats
   */
  getModel(version) {
    const model = this.models.get(version);
    if (!model) {
      return null;
    }

    return {
      metadata: model.metadata,
      stats: model.stats,
      isActive: version === this.activeVersion,
      isCanary: version === this.canaryVersion,
    };
  }

  /**
   * List all registered models
   *
   * @returns {Array<Object>} List of models
   */
  listModels() {
    return Array.from(this.models.entries()).map(([version, model]) => ({
      version,
      metadata: model.metadata,
      stats: model.stats,
      isActive: version === this.activeVersion,
      isCanary: version === this.canaryVersion,
    }));
  }

  /**
   * Get active deployment info
   *
   * @returns {Object} Deployment status
   */
  getDeploymentInfo() {
    return {
      activeVersion: this.activeVersion,
      canaryVersion: this.canaryVersion,
      canaryTrafficPercent: this.canaryTrafficPercent,
      totalModels: this.models.size,
    };
  }

  /**
   * Unregister model version
   *
   * @param {string} version - Version to remove
   * @returns {Promise<void>}
   */
  async unregister(version) {
    if (version === this.activeVersion) {
      throw new Error('Cannot unregister active version');
    }

    if (version === this.canaryVersion) {
      this.rollbackCanary();
    }

    const model = this.models.get(version);
    if (model) {
      await model.runner.dispose();
      this.models.delete(version);
    }
  }

  /**
   * Cleanup all models
   *
   * @returns {Promise<void>}
   */
  async destroy() {
    for (const [version, model] of this.models.entries()) {
      await model.runner.dispose();
    }
    this.models.clear();
    this.activeVersion = null;
    this.canaryVersion = null;
    this.canaryTrafficPercent = 0;
  }
}

/**
 * Create model registry instance
 *
 * @returns {ModelRegistry} Registry instance
 */
export function createModelRegistry() {
  return new ModelRegistry();
}
