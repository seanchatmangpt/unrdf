/**
 * @file Anomaly Detector - ML-powered workflow pattern anomaly detection
 * @module @unrdf/yawl-ai/anomaly
 *
 * @description
 * Detects unusual patterns in workflow execution using autoencoders and
 * statistical methods. Identifies deviations from normal behavior that may
 * indicate errors, security issues, or process inefficiencies.
 *
 * Techniques:
 * - Autoencoder neural network for pattern learning
 * - Statistical outlier detection (z-score, IQR)
 * - Sequence similarity analysis
 * - Temporal pattern analysis
 *
 * Integration:
 * - Monitors YAWL event streams for anomalies
 * - Flags unusual task sequences
 * - Detects timing anomalies
 * - Suggests remediation actions
 */

import * as tf from '@tensorflow/tfjs';
import { z } from 'zod';

// =============================================================================
// Schemas
// =============================================================================

/**
 * @typedef {Object} WorkflowExecution
 * @property {string} caseId - Case identifier
 * @property {string} workflowId - Workflow identifier
 * @property {Object[]} events - Execution events
 * @property {number} totalDuration - Total execution time in ms
 */
const WorkflowExecutionSchema = z.object({
  caseId: z.string(),
  workflowId: z.string(),
  events: z.array(z.object({ taskId: z.string(), duration: z.number() })),
  totalDuration: z.number().positive(),
});

/**
 * @typedef {Object} Anomaly
 * @property {string} type - Anomaly type (timing, sequence, pattern)
 * @property {number} severity - Severity score 0-1
 * @property {string} description - Human-readable description
 * @property {Object} context - Anomaly context data
 * @property {string[]} suggestions - Remediation suggestions
 */
const AnomalySchema = z.object({
  type: z.enum(['timing', 'sequence', 'pattern', 'resource']),
  severity: z.number().min(0).max(1),
  description: z.string(),
  context: z.any(),
  suggestions: z.array(z.string()),
});

// =============================================================================
// Anomaly Detector
// =============================================================================

/**
 * ML-based workflow anomaly detector
 *
 * @class AnomalyDetector
 * @description
 * Uses autoencoder and statistical methods to detect anomalous workflow patterns.
 * Learns normal execution patterns and flags deviations.
 */
export class AnomalyDetector {
  /**
   * @param {Object} config - Detector configuration
   * @param {number} [config.encodingDim=16] - Autoencoder bottleneck size
   * @param {number} [config.sequenceLength=10] - Sequence length for encoding
   * @param {number} [config.anomalyThreshold=2.5] - Z-score threshold for anomaly
   */
  constructor(config = {}) {
    this.encodingDim = config.encodingDim || 16;
    this.sequenceLength = config.sequenceLength || 10;
    this.anomalyThreshold = config.anomalyThreshold || 2.5;

    this.autoencoder = null;
    this.encoder = null;
    this.taskVocabulary = new Map();
    this.reconstructionErrors = [];
    this.errorThreshold = null;
    this.normalPatterns = new Map(); // Task sequences and statistics
    this.trained = false;
  }

  /**
   * Build autoencoder model for pattern learning
   *
   * @param {number} inputDim - Input dimension size
   * @returns {Object} Encoder and autoencoder models
   * @private
   */
  _buildAutoencoder(inputDim) {
    // Encoder
    const encoderInput = tf.input({ shape: [inputDim] });
    const encoded1 = tf.layers
      .dense({
        units: 32,
        activation: 'relu',
        kernelInitializer: 'heNormal',
      })
      .apply(encoderInput);
    const encoded2 = tf.layers
      .dense({
        units: this.encodingDim,
        activation: 'relu',
        kernelInitializer: 'heNormal',
      })
      .apply(encoded1);

    const encoder = tf.model({
      inputs: encoderInput,
      outputs: encoded2,
    });

    // Decoder
    const decoded1 = tf.layers
      .dense({
        units: 32,
        activation: 'relu',
        kernelInitializer: 'heNormal',
      })
      .apply(encoded2);
    const decoded2 = tf.layers
      .dense({
        units: inputDim,
        activation: 'sigmoid',
        kernelInitializer: 'glorotUniform',
      })
      .apply(decoded1);

    const autoencoder = tf.model({
      inputs: encoderInput,
      outputs: decoded2,
    });

    // Compile autoencoder
    autoencoder.compile({
      optimizer: tf.train.adam(0.001),
      loss: 'meanSquaredError',
      metrics: ['mae'],
    });

    return { encoder, autoencoder };
  }

  /**
   * Encode workflow execution as feature vector
   *
   * @param {WorkflowExecution} execution - Workflow execution data
   * @returns {number[]} Feature vector
   * @private
   */
  _encodeExecution(execution) {
    const features = [];

    // Task sequence encoding (one-hot)
    const taskEncoding = new Array(this.taskVocabulary.size).fill(0);
    for (const event of execution.events) {
      const taskIndex = this.taskVocabulary.get(event.taskId);
      if (taskIndex !== undefined) {
        taskEncoding[taskIndex] = 1;
      }
    }
    features.push(...taskEncoding);

    // Duration features (normalized)
    const durations = execution.events.map((e) => e.duration);
    const totalDuration = execution.totalDuration || 1;
    const normalizedDurations = durations.map((d) => d / totalDuration);

    // Pad or truncate to fixed length
    while (normalizedDurations.length < this.sequenceLength) {
      normalizedDurations.push(0);
    }
    features.push(...normalizedDurations.slice(0, this.sequenceLength));

    // Statistical features
    const avgDuration = durations.reduce((a, b) => a + b, 0) / durations.length || 0;
    const maxDuration = Math.max(...durations, 0);
    features.push(avgDuration / totalDuration, maxDuration / totalDuration);

    return features;
  }

  /**
   * Train the anomaly detector on normal workflow executions
   *
   * @param {WorkflowExecution[]} executions - Training data (normal executions)
   * @param {Object} [options] - Training options
   * @param {number} [options.epochs=50] - Number of training epochs
   * @param {number} [options.batchSize=16] - Batch size
   * @returns {Promise<Object>} Training history
   */
  async train(executions, options = {}) {
    const { epochs = 50, batchSize = 16 } = options;

    // Validate input
    executions.forEach((exec) => WorkflowExecutionSchema.parse(exec));

    // Build vocabulary
    const taskSet = new Set();
    for (const exec of executions) {
      for (const event of exec.events) {
        taskSet.add(event.taskId);
      }
    }

    let index = 0;
    for (const task of taskSet) {
      this.taskVocabulary.set(task, index++);
    }

    // Encode executions
    const encodedData = executions.map((exec) => this._encodeExecution(exec));
    const inputDim = encodedData[0].length;

    // Build autoencoder
    const { encoder, autoencoder } = this._buildAutoencoder(inputDim);
    this.encoder = encoder;
    this.autoencoder = autoencoder;

    // Train autoencoder
    const xTensor = tf.tensor2d(encodedData);

    try {
      const history = await autoencoder.fit(xTensor, xTensor, {
        epochs,
        batchSize,
        validationSplit: 0.2,
        verbose: 0,
        callbacks: {
          onEpochEnd: (epoch, logs) => {
            if (epoch % 10 === 0) {
              console.log(
                `Anomaly Detector Epoch ${epoch}: loss=${logs.loss.toFixed(4)}`,
              );
            }
          },
        },
      });

      // Calculate reconstruction errors for threshold
      const reconstructed = autoencoder.predict(xTensor);
      const errors = await this._calculateReconstructionErrors(xTensor, reconstructed);

      this.reconstructionErrors = errors;
      const mean = errors.reduce((a, b) => a + b, 0) / errors.length;
      const std = Math.sqrt(
        errors.reduce((sum, e) => sum + (e - mean) ** 2, 0) / errors.length,
      );
      this.errorThreshold = mean + this.anomalyThreshold * std;

      // Store normal patterns
      for (const exec of executions) {
        const sequence = exec.events.map((e) => e.taskId).join('->');
        if (!this.normalPatterns.has(sequence)) {
          this.normalPatterns.set(sequence, {
            count: 0,
            avgDuration: 0,
            executions: [],
          });
        }
        const pattern = this.normalPatterns.get(sequence);
        pattern.count++;
        pattern.executions.push(exec.totalDuration);
        pattern.avgDuration =
          pattern.executions.reduce((a, b) => a + b, 0) / pattern.count;
      }

      this.trained = true;
      return history;
    } finally {
      xTensor.dispose();
    }
  }

  /**
   * Calculate reconstruction errors
   *
   * @param {tf.Tensor} original - Original data
   * @param {tf.Tensor} reconstructed - Reconstructed data
   * @returns {Promise<number[]>} Mean squared errors per sample
   * @private
   */
  async _calculateReconstructionErrors(original, reconstructed) {
    const diff = tf.sub(original, reconstructed);
    const squared = tf.square(diff);
    const mse = tf.mean(squared, 1);
    const errors = await mse.data();

    diff.dispose();
    squared.dispose();
    mse.dispose();

    return Array.from(errors);
  }

  /**
   * Detect anomalies in a workflow execution
   *
   * @param {WorkflowExecution} execution - Execution to analyze
   * @returns {Promise<Anomaly[]>} Detected anomalies
   */
  async detect(execution) {
    if (!this.trained) {
      throw new Error('Detector not trained. Call train() first.');
    }

    const anomalies = [];

    // 1. Pattern-based detection (autoencoder)
    const encoded = this._encodeExecution(execution);
    const inputTensor = tf.tensor2d([encoded]);

    try {
      const reconstructed = this.autoencoder.predict(inputTensor);
      const errors = await this._calculateReconstructionErrors(
        inputTensor,
        reconstructed,
      );
      const reconstructionError = errors[0];

      if (reconstructionError > this.errorThreshold) {
        anomalies.push({
          type: 'pattern',
          severity: Math.min(
            1,
            (reconstructionError - this.errorThreshold) / this.errorThreshold,
          ),
          description: `Unusual workflow execution pattern detected (reconstruction error: ${reconstructionError.toFixed(4)})`,
          context: {
            reconstructionError,
            threshold: this.errorThreshold,
            caseId: execution.caseId,
          },
          suggestions: [
            'Review workflow definition for unexpected paths',
            'Check for data quality issues in inputs',
            'Verify task execution order matches specification',
          ],
        });
      }
    } finally {
      inputTensor.dispose();
    }

    // 2. Sequence-based detection
    const sequence = execution.events.map((e) => e.taskId).join('->');
    if (!this.normalPatterns.has(sequence)) {
      const similarPatterns = this._findSimilarPatterns(sequence);

      anomalies.push({
        type: 'sequence',
        severity: similarPatterns.length === 0 ? 0.9 : 0.5,
        description: `Unknown task sequence: ${sequence}`,
        context: {
          sequence,
          similarPatterns: similarPatterns.slice(0, 3),
          caseId: execution.caseId,
        },
        suggestions: [
          'Verify this is an expected workflow path',
          'Check for missing workflow definition updates',
          similarPatterns.length > 0
            ? `Similar pattern found: ${similarPatterns[0]}`
            : 'No similar patterns - may be completely new path',
        ],
      });
    }

    // 3. Timing-based detection
    const pattern = this.normalPatterns.get(sequence);
    if (pattern) {
      const durationDeviation = Math.abs(
        execution.totalDuration - pattern.avgDuration,
      );
      const threshold = pattern.avgDuration * 0.5; // 50% deviation

      if (durationDeviation > threshold) {
        anomalies.push({
          type: 'timing',
          severity: Math.min(1, durationDeviation / pattern.avgDuration),
          description: `Execution time significantly different from normal (${execution.totalDuration}ms vs ${pattern.avgDuration.toFixed(0)}ms)`,
          context: {
            actualDuration: execution.totalDuration,
            expectedDuration: pattern.avgDuration,
            deviation: durationDeviation,
            caseId: execution.caseId,
          },
          suggestions: [
            execution.totalDuration > pattern.avgDuration
              ? 'Performance degradation detected - check resource availability'
              : 'Unusually fast execution - verify all steps completed',
            'Review task execution logs for errors or warnings',
            'Check for resource contention or external dependencies',
          ],
        });
      }
    }

    return anomalies.map((a) => {
      try {
        return AnomalySchema.parse(a);
      } catch (error) {
        console.error('Anomaly schema validation error:', error.message);
        console.error('Anomaly data:', JSON.stringify(a, null, 2));
        throw error;
      }
    });
  }

  /**
   * Find similar patterns using edit distance
   *
   * @param {string} sequence - Task sequence to match
   * @returns {string[]} Similar sequences
   * @private
   */
  _findSimilarPatterns(sequence) {
    const similar = [];
    const seqParts = sequence.split('->');

    for (const [knownSeq] of this.normalPatterns) {
      const knownParts = knownSeq.split('->');

      // Simple similarity: count matching tasks
      const matching = seqParts.filter((task) => knownParts.includes(task)).length;
      const similarity = matching / Math.max(seqParts.length, knownParts.length);

      if (similarity > 0.6) {
        similar.push(knownSeq);
      }
    }

    return similar.sort((a, b) => {
      const scoreA = this.normalPatterns.get(a).count;
      const scoreB = this.normalPatterns.get(b).count;
      return scoreB - scoreA;
    });
  }

  /**
   * Monitor workflow stream for anomalies in real-time
   *
   * @param {AsyncIterable<WorkflowExecution>} executionStream - Stream of executions
   * @param {Function} onAnomaly - Callback when anomaly detected
   * @returns {Promise<void>}
   */
  async monitor(executionStream, onAnomaly) {
    if (!this.trained) {
      throw new Error('Detector not trained. Call train() first.');
    }

    for await (const execution of executionStream) {
      const anomalies = await this.detect(execution);

      if (anomalies.length > 0) {
        onAnomaly({
          execution,
          anomalies,
          timestamp: new Date().toISOString(),
        });
      }
    }
  }

  /**
   * Get detector summary
   *
   * @returns {Object} Summary statistics
   */
  getSummary() {
    return {
      trained: this.trained,
      vocabularySize: this.taskVocabulary.size,
      normalPatterns: this.normalPatterns.size,
      errorThreshold: this.errorThreshold,
      avgReconstructionError:
        this.reconstructionErrors.reduce((a, b) => a + b, 0) /
          this.reconstructionErrors.length || 0,
    };
  }

  /**
   * Dispose of model resources
   */
  dispose() {
    if (this.autoencoder) {
      this.autoencoder.dispose();
    }
    if (this.encoder) {
      this.encoder.dispose();
    }
  }
}

/**
 * Create a new anomaly detector
 *
 * @param {Object} config - Detector configuration
 * @returns {AnomalyDetector} New detector instance
 */
export function createDetector(config = {}) {
  return new AnomalyDetector(config);
}

export default { AnomalyDetector, createDetector };
