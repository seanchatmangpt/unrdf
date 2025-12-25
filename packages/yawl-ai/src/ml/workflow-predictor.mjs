/**
 * @file Workflow Path Predictor - ML-powered task execution path prediction
 * @module @unrdf/yawl-ai/predictor
 *
 * @description
 * Uses TensorFlow.js to predict optimal task execution sequences based on
 * historical workflow event data. Implements a sequential neural network
 * that learns patterns from YAWL event logs and suggests next-best actions.
 *
 * Architecture:
 * - Input: Task completion vectors (one-hot encoded task IDs + timing)
 * - Hidden: Dense layers with ReLU activation
 * - Output: Probability distribution over next possible tasks
 *
 * Integration:
 * - Trains on YAWL event history (TASK_COMPLETED events)
 * - Predicts next task based on current workflow state
 * - Suggests resource allocation based on predicted path
 */

import * as tf from '@tensorflow/tfjs';
import { z } from 'zod';

// =============================================================================
// Schemas
// =============================================================================

/**
 * @typedef {Object} WorkflowSequence
 * @property {string[]} taskIds - Ordered sequence of task IDs
 * @property {number[]} durations - Task completion times in ms
 * @property {string} workflowId - Workflow identifier
 */
const WorkflowSequenceSchema = z.object({
  taskIds: z.array(z.string()).min(1),
  durations: z.array(z.number()).min(1),
  workflowId: z.string(),
});

/**
 * @typedef {Object} PredictionResult
 * @property {string} nextTask - Predicted next task ID
 * @property {number} confidence - Prediction confidence (0-1)
 * @property {Object<string, number>} alternatives - Alternative tasks with probabilities
 * @property {number} estimatedDuration - Predicted task duration in ms
 */
const PredictionResultSchema = z.object({
  nextTask: z.string(),
  confidence: z.number().min(0).max(1),
  alternatives: z.record(z.string(), z.number()),
  estimatedDuration: z.number().positive(),
});

// =============================================================================
// Workflow Path Predictor
// =============================================================================

/**
 * Neural network-based workflow path predictor
 *
 * @class WorkflowPathPredictor
 * @description
 * Learns from historical workflow executions to predict optimal task sequences.
 * Uses a simple feedforward network for sequence prediction.
 */
export class WorkflowPathPredictor {
  /**
   * @param {Object} config - Predictor configuration
   * @param {number} [config.maxTasks=50] - Maximum unique tasks in vocabulary
   * @param {number} [config.sequenceLength=5] - Length of task sequences to learn
   * @param {number} [config.hiddenUnits=64] - Hidden layer size
   * @param {number} [config.learningRate=0.01] - Model learning rate
   */
  constructor(config = {}) {
    this.maxTasks = config.maxTasks || 50;
    this.sequenceLength = config.sequenceLength || 5;
    this.hiddenUnits = config.hiddenUnits || 64;
    this.learningRate = config.learningRate || 0.01;

    this.model = null;
    this.taskVocabulary = new Map(); // task ID -> index
    this.indexToTask = new Map(); // index -> task ID
    this.durationStats = new Map(); // task ID -> {mean, std}
    this.trained = false;
  }

  /**
   * Build the neural network model
   *
   * @returns {tf.Sequential} Compiled TensorFlow model
   * @private
   */
  _buildModel() {
    const model = tf.sequential();

    // Input: flattened one-hot encoded task sequence
    const inputSize = this.maxTasks * this.sequenceLength;

    // Hidden layer 1: Dense with ReLU
    model.add(
      tf.layers.dense({
        inputShape: [inputSize],
        units: this.hiddenUnits,
        activation: 'relu',
        kernelInitializer: 'heNormal',
      }),
    );

    // Hidden layer 2: Dense with ReLU
    model.add(
      tf.layers.dense({
        units: this.hiddenUnits / 2,
        activation: 'relu',
        kernelInitializer: 'heNormal',
      }),
    );

    // Dropout for regularization
    model.add(tf.layers.dropout({ rate: 0.2 }));

    // Output: Probability distribution over next task
    model.add(
      tf.layers.dense({
        units: this.maxTasks,
        activation: 'softmax',
      }),
    );

    // Compile with categorical crossentropy
    model.compile({
      optimizer: tf.train.adam(this.learningRate),
      loss: 'categoricalCrossentropy',
      metrics: ['accuracy'],
    });

    return model;
  }

  /**
   * Build vocabulary from task sequences
   *
   * @param {WorkflowSequence[]} sequences - Training sequences
   * @private
   */
  _buildVocabulary(sequences) {
    const uniqueTasks = new Set();
    const durationMap = new Map();

    for (const seq of sequences) {
      for (let i = 0; i < seq.taskIds.length; i++) {
        const taskId = seq.taskIds[i];
        uniqueTasks.add(taskId);

        // Track durations for statistics
        if (!durationMap.has(taskId)) {
          durationMap.set(taskId, []);
        }
        if (seq.durations[i]) {
          durationMap.get(taskId).push(seq.durations[i]);
        }
      }
    }

    // Build bidirectional mappings
    let index = 0;
    for (const task of uniqueTasks) {
      if (index >= this.maxTasks) break;
      this.taskVocabulary.set(task, index);
      this.indexToTask.set(index, task);
      index++;
    }

    // Calculate duration statistics
    for (const [taskId, durations] of durationMap) {
      const mean = durations.reduce((a, b) => a + b, 0) / durations.length;
      const variance =
        durations.reduce((sum, d) => sum + (d - mean) ** 2, 0) / durations.length;
      const std = Math.sqrt(variance);
      this.durationStats.set(taskId, { mean, std });
    }
  }

  /**
   * Encode task sequence as one-hot tensor
   *
   * @param {string[]} taskIds - Task ID sequence
   * @returns {number[]} Flattened one-hot encoding
   * @private
   */
  _encodeSequence(taskIds) {
    const encoded = new Array(this.maxTasks * this.sequenceLength).fill(0);

    // Take last N tasks (sequence length)
    const seq = taskIds.slice(-this.sequenceLength);

    for (let i = 0; i < seq.length; i++) {
      const taskId = seq[i];
      const taskIndex = this.taskVocabulary.get(taskId);

      if (taskIndex !== undefined) {
        const offset = i * this.maxTasks;
        encoded[offset + taskIndex] = 1;
      }
    }

    return encoded;
  }

  /**
   * Train the model on historical workflow sequences
   *
   * @param {WorkflowSequence[]} sequences - Training data from YAWL events
   * @param {Object} [options] - Training options
   * @param {number} [options.epochs=50] - Number of training epochs
   * @param {number} [options.batchSize=32] - Batch size
   * @param {number} [options.validationSplit=0.2] - Validation split ratio
   * @returns {Promise<Object>} Training history
   */
  async train(sequences, options = {}) {
    const { epochs = 50, batchSize = 32, validationSplit = 0.2 } = options;

    // Validate input
    sequences.forEach((seq) => WorkflowSequenceSchema.parse(seq));

    // Build vocabulary
    this._buildVocabulary(sequences);

    // Build model
    this.model = this._buildModel();

    // Prepare training data
    const xData = [];
    const yData = [];

    for (const seq of sequences) {
      // Create training examples: predict next task from previous N
      for (let i = this.sequenceLength; i < seq.taskIds.length; i++) {
        const inputSeq = seq.taskIds.slice(i - this.sequenceLength, i);
        const targetTask = seq.taskIds[i];

        const x = this._encodeSequence(inputSeq);
        const y = new Array(this.maxTasks).fill(0);
        const targetIndex = this.taskVocabulary.get(targetTask);

        if (targetIndex !== undefined) {
          y[targetIndex] = 1;
          xData.push(x);
          yData.push(y);
        }
      }
    }

    // Convert to tensors
    const xs = tf.tensor2d(xData);
    const ys = tf.tensor2d(yData);

    try {
      // Train the model
      const history = await this.model.fit(xs, ys, {
        epochs,
        batchSize,
        validationSplit,
        verbose: 0,
        callbacks: {
          onEpochEnd: (epoch, logs) => {
            if (epoch % 10 === 0) {
              console.log(
                `Epoch ${epoch}: loss=${logs.loss.toFixed(4)} acc=${logs.acc.toFixed(4)}`,
              );
            }
          },
        },
      });

      this.trained = true;
      return history;
    } finally {
      xs.dispose();
      ys.dispose();
    }
  }

  /**
   * Predict next task based on current workflow state
   *
   * @param {string[]} currentSequence - Recent task completion sequence
   * @returns {Promise<PredictionResult>} Prediction with confidence
   */
  async predict(currentSequence) {
    if (!this.trained) {
      throw new Error('Model not trained. Call train() first.');
    }

    // Encode input sequence
    const encoded = this._encodeSequence(currentSequence);
    const inputTensor = tf.tensor2d([encoded]);

    try {
      // Get predictions
      const prediction = this.model.predict(inputTensor);
      const probabilities = await prediction.data();

      // Find top prediction
      let maxProb = 0;
      let maxIndex = 0;
      const alternatives = {};

      for (let i = 0; i < probabilities.length; i++) {
        const prob = probabilities[i];
        const taskId = this.indexToTask.get(i);

        if (taskId && prob > 0.01) {
          alternatives[taskId] = prob;

          if (prob > maxProb) {
            maxProb = prob;
            maxIndex = i;
          }
        }
      }

      const nextTask = this.indexToTask.get(maxIndex) || 'unknown';
      const durationStat = this.durationStats.get(nextTask);
      const estimatedDuration = durationStat ? durationStat.mean : 1000;

      const result = {
        nextTask,
        confidence: maxProb,
        alternatives,
        estimatedDuration,
      };

      return PredictionResultSchema.parse(result);
    } finally {
      inputTensor.dispose();
    }
  }

  /**
   * Suggest resource allocation based on predicted path
   *
   * @param {string[]} currentSequence - Current workflow state
   * @param {number} [lookAhead=3] - Number of steps to predict ahead
   * @returns {Promise<Object>} Resource allocation suggestions
   */
  async suggestResources(currentSequence, lookAhead = 3) {
    const predictions = [];
    let sequence = [...currentSequence];

    for (let i = 0; i < lookAhead; i++) {
      const pred = await this.predict(sequence);
      predictions.push(pred);
      sequence.push(pred.nextTask);
    }

    const totalDuration = predictions.reduce((sum, p) => sum + p.estimatedDuration, 0);
    const criticalPath = predictions.map((p) => p.nextTask);

    return {
      criticalPath,
      estimatedTotalDuration: totalDuration,
      recommendedParallelization: predictions
        .filter((p) => p.confidence < 0.7)
        .map((p) => Object.keys(p.alternatives)[0]),
    };
  }

  /**
   * Get model summary
   *
   * @returns {Object} Model metadata
   */
  getSummary() {
    return {
      trained: this.trained,
      vocabularySize: this.taskVocabulary.size,
      sequenceLength: this.sequenceLength,
      hiddenUnits: this.hiddenUnits,
      modelParams: this.model ? this.model.countParams() : 0,
    };
  }

  /**
   * Dispose of model resources
   */
  dispose() {
    if (this.model) {
      this.model.dispose();
    }
  }
}

/**
 * Create a new workflow path predictor
 *
 * @param {Object} config - Predictor configuration
 * @returns {WorkflowPathPredictor} New predictor instance
 */
export function createPredictor(config = {}) {
  return new WorkflowPathPredictor(config);
}

export default { WorkflowPathPredictor, createPredictor };
