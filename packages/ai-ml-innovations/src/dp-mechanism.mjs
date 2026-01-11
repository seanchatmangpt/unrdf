/**
 * @file Differential Privacy Mechanisms
 * @module ai-ml-innovations/dp-mechanism
 *
 * @description
 * Differential privacy mechanisms for federated learning:
 * - Gaussian mechanism (for (ε, δ)-DP)
 * - Laplace mechanism (for ε-DP)
 * - Gradient clipping
 * - Noise calibration
 */

import { z } from 'zod';
import { randomBytes } from 'crypto';
import { trace, SpanStatusCode } from '@opentelemetry/api';
import { DPMechanismSchema } from './schemas.mjs';

const tracer = trace.getTracer('@unrdf/ai-ml-innovations');

/**
 * Differential Privacy Mechanism
 *
 * Provides differentially private noise addition to gradients
 * with proper sensitivity calibration.
 *
 * @class
 */
export class DPMechanism {
  /**
   * Create DP mechanism
   *
   * @param {Object} config - Configuration
   * @param {string} [config.mechanism='gaussian'] - Noise mechanism
   * @param {number} config.sensitivity - L2 sensitivity (clipping norm)
   * @param {number} config.epsilon - Privacy parameter (ε)
   * @param {number} [config.delta=1e-5] - Failure probability (δ)
   */
  constructor(config) {
    const validated = DPMechanismSchema.parse({
      mechanism: config.mechanism || 'gaussian',
      sensitivity: config.sensitivity || 1.0,
      epsilon: config.epsilon,
      delta: config.delta || 1e-5,
      clippingNorm: config.clippingNorm || config.sensitivity,
    });

    this.mechanism = validated.mechanism;
    this.sensitivity = validated.sensitivity;
    this.epsilon = validated.epsilon;
    this.delta = validated.delta;
    this.clippingNorm = validated.clippingNorm;

    // Calibrate noise scale
    this.noiseScale = this._calibrateNoise();
  }

  /**
   * Clip gradients to bound sensitivity
   *
   * @param {Object} gradients - Gradients to clip
   * @param {number} [maxNorm] - Maximum L2 norm (defaults to clippingNorm)
   * @returns {Object} Clipped gradients
   *
   * @example
   * const clipped = mechanism.clipGradients(gradients, 1.0);
   */
  clipGradients(gradients, maxNorm = this.clippingNorm) {
    return tracer.startActiveSpan('dp.clip_gradients', (span) => {
      try {
        const clipped = {};
        const norm = this._computeL2Norm(gradients);

        span.setAttributes({
          'dp.gradient_norm': norm,
          'dp.max_norm': maxNorm,
          'dp.clipped': norm > maxNorm,
        });

        if (norm <= maxNorm) {
          span.setStatus({ code: SpanStatusCode.OK });
          span.end();
          return gradients;
        }

        const scale = maxNorm / norm;

        for (const [key, gradient] of Object.entries(gradients)) {
          clipped[key] = gradient.map((val) => val * scale);
        }

        span.setStatus({ code: SpanStatusCode.OK });
        span.end();
        return clipped;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        span.end();
        throw error;
      }
    });
  }

  /**
   * Add differential privacy noise
   *
   * @param {Object} gradients - Gradients to noise
   * @returns {Object} Noised gradients
   *
   * @example
   * const private = mechanism.addNoise(gradients);
   */
  addNoise(gradients) {
    return tracer.startActiveSpan('dp.add_noise', (span) => {
      try {
        span.setAttributes({
          'dp.mechanism': this.mechanism,
          'dp.epsilon': this.epsilon,
          'dp.delta': this.delta,
          'dp.noise_scale': this.noiseScale,
        });

        const noised = {};

        for (const [key, gradient] of Object.entries(gradients)) {
          noised[key] = gradient.map((val) => {
            const noise =
              this.mechanism === 'gaussian'
                ? this._gaussianNoise(0, this.noiseScale)
                : this._laplaceNoise(0, this.noiseScale);

            return val + noise;
          });
        }

        span.setStatus({ code: SpanStatusCode.OK });
        span.end();
        return noised;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        span.end();
        throw error;
      }
    });
  }

  /**
   * Clip and add noise (combined operation)
   *
   * @param {Object} gradients - Gradients to privatize
   * @returns {Object} Privatized gradients
   *
   * @example
   * const private = mechanism.privatize(gradients);
   */
  privatize(gradients) {
    const clipped = this.clipGradients(gradients);
    return this.addNoise(clipped);
  }

  /**
   * Calibrate noise scale based on mechanism
   * @private
   */
  _calibrateNoise() {
    if (this.mechanism === 'gaussian') {
      // Gaussian mechanism: σ = sensitivity * sqrt(2 * ln(1.25/δ)) / ε
      return (
        this.sensitivity *
        Math.sqrt(2 * Math.log(1.25 / this.delta)) /
        this.epsilon
      );
    } else if (this.mechanism === 'laplace') {
      // Laplace mechanism: b = sensitivity / ε
      return this.sensitivity / this.epsilon;
    }

    throw new Error(`Unknown mechanism: ${this.mechanism}`);
  }

  /**
   * Compute L2 norm of gradients
   * @private
   */
  _computeL2Norm(gradients) {
    let sumSquares = 0;

    for (const gradient of Object.values(gradients)) {
      for (const val of gradient) {
        sumSquares += val * val;
      }
    }

    return Math.sqrt(sumSquares);
  }

  /**
   * Generate Gaussian noise
   * @private
   */
  _gaussianNoise(mean, std) {
    // Box-Muller transform
    const u1 = Math.random();
    const u2 = Math.random();
    const z0 = Math.sqrt(-2 * Math.log(u1)) * Math.cos(2 * Math.PI * u2);
    return mean + std * z0;
  }

  /**
   * Generate Laplace noise
   * @private
   */
  _laplaceNoise(mean, scale) {
    // Inverse CDF method
    const u = Math.random() - 0.5;
    const sign = u < 0 ? -1 : 1;
    return mean + sign * scale * Math.log(1 - 2 * Math.abs(u));
  }
}

/**
 * Create DP mechanism
 *
 * @param {Object} config - Configuration
 * @returns {DPMechanism} Mechanism instance
 */
export function createDPMechanism(config) {
  return new DPMechanism(config);
}

export default DPMechanism;
