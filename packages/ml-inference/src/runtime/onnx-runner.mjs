/**
 * @file ONNX Runner - Core ONNX model inference engine
 * @module ml-inference/runtime/onnx-runner
 *
 * @description
 * High-performance ONNX model inference with batching, caching, and GPU support.
 * Provides efficient tensor operations and model session management.
 */

import * as ort from 'onnxruntime-node';
import { trace, context as _context } from '@opentelemetry/api';
import { z } from 'zod';

const tracer = trace.getTracer('@unrdf/ml-inference');

/**
 * Input tensor schema
 */
const _TensorSchema = z.object({
  data: z.instanceof(Float32Array).or(z.array(z.number())),
  dims: z.array(z.number()),
  type: z.enum(['float32', 'int32', 'int64']).default('float32'),
});

/**
 * Inference options schema
 */
const InferenceOptionsSchema = z.object({
  executionProviders: z.array(z.string()).default(['cpu']),
  graphOptimizationLevel: z.enum(['disabled', 'basic', 'extended', 'all']).default('all'),
  enableCpuMemArena: z.boolean().default(true),
  enableMemPattern: z.boolean().default(true),
  executionMode: z.enum(['sequential', 'parallel']).default('parallel'),
  logSeverityLevel: z.number().min(0).max(4).default(2),
  logVerbosityLevel: z.number().min(0).max(4).default(0),
});

/**
 * ONNX Inference Runner
 * Manages model loading, inference, and performance optimization
 */
export class ONNXRunner {
  /**
   * @param {Object} options - Runner configuration
   * @param {string[]} options.executionProviders - Execution providers (e.g., ['cuda', 'cpu'])
   * @param {string} options.graphOptimizationLevel - Graph optimization level
   */
  constructor(options = {}) {
    this.options = InferenceOptionsSchema.parse(options);
    this.session = null;
    this.inputNames = null;
    this.outputNames = null;
    this.inputMeta = null;
    this.outputMeta = null;
    this.metrics = {
      totalInferences: 0,
      totalLatencyMs: 0,
      totalSamples: 0,
      errorCount: 0,
    };
  }

  /**
   * Load ONNX model from file or buffer
   *
   * @param {string|Uint8Array} modelPathOrBuffer - Path to ONNX model or buffer
   * @returns {Promise<void>}
   *
   * @example
   * const runner = new ONNXRunner();
   * await runner.loadModel('./models/classifier.onnx');
   */
  async loadModel(modelPathOrBuffer) {
    return tracer.startActiveSpan('onnx.loadModel', async span => {
      try {
        const startTime = performance.now();

        this.session = await ort.InferenceSession.create(modelPathOrBuffer, {
          executionProviders: this.options.executionProviders,
          graphOptimizationLevel: this.options.graphOptimizationLevel,
          enableCpuMemArena: this.options.enableCpuMemArena,
          enableMemPattern: this.options.enableMemPattern,
          executionMode: this.options.executionMode,
          logSeverityLevel: this.options.logSeverityLevel,
          logVerbosityLevel: this.options.logVerbosityLevel,
        });

        // Extract model metadata
        this.inputNames = this.session.inputNames;
        this.outputNames = this.session.outputNames;
        this.inputMeta = {};
        this.outputMeta = {};

        for (const name of this.inputNames) {
          this.inputMeta[name] = this.session.inputNames.indexOf(name);
        }

        for (const name of this.outputNames) {
          this.outputMeta[name] = this.session.outputNames.indexOf(name);
        }

        const loadTime = performance.now() - startTime;

        span.setAttributes({
          'model.inputs': this.inputNames.length,
          'model.outputs': this.outputNames.length,
          'model.loadTimeMs': loadTime,
          'execution.providers': this.options.executionProviders.join(','),
        });

        span.setStatus({ code: 1 }); // OK
        return loadTime;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: 2, message: error.message }); // ERROR
        throw new Error(`Failed to load ONNX model: ${error.message}`);
      } finally {
        span.end();
      }
    });
  }

  /**
   * Run inference on input tensors
   *
   * @param {Object<string, Array|Float32Array>} inputs - Input tensors by name
   * @param {Array<number>} [batchSize] - Optional batch size for auto-batching
   * @returns {Promise<Object>} Output tensors
   *
   * @example
   * const outputs = await runner.infer({
   *   input: new Float32Array([1.0, 2.0, 3.0])
   * });
   */
  async infer(inputs, batchSize = null) {
    return tracer.startActiveSpan('onnx.infer', async span => {
      if (!this.session) {
        throw new Error('Model not loaded. Call loadModel() first.');
      }

      try {
        const startTime = performance.now();

        // Convert inputs to ONNX tensors
        const feeds = {};
        for (const [name, value] of Object.entries(inputs)) {
          if (value instanceof ort.Tensor) {
            feeds[name] = value;
          } else {
            // Auto-convert arrays to tensors
            const data = value instanceof Float32Array ? value : new Float32Array(value);
            const dims = batchSize ? [batchSize, data.length / batchSize] : [1, data.length];
            feeds[name] = new ort.Tensor('float32', data, dims);
          }
        }

        // Run inference
        const results = await this.session.run(feeds);

        const inferenceTime = performance.now() - startTime;

        // Update metrics
        this.metrics.totalInferences++;
        this.metrics.totalLatencyMs += inferenceTime;
        this.metrics.totalSamples += batchSize || 1;

        span.setAttributes({
          'inference.latencyMs': inferenceTime,
          'inference.batchSize': batchSize || 1,
          'inference.throughput': ((batchSize || 1) / inferenceTime) * 1000,
        });

        span.setStatus({ code: 1 }); // OK
        return results;
      } catch (error) {
        this.metrics.errorCount++;
        span.recordException(error);
        span.setStatus({ code: 2, message: error.message }); // ERROR
        throw new Error(`Inference failed: ${error.message}`);
      } finally {
        span.end();
      }
    });
  }

  /**
   * Run batched inference for multiple samples
   *
   * @param {Array<Object>} batchInputs - Array of input objects
   * @returns {Promise<Array>} Array of outputs
   *
   * @example
   * const outputs = await runner.inferBatch([
   *   { input: [1, 2, 3] },
   *   { input: [4, 5, 6] }
   * ]);
   */
  async inferBatch(batchInputs) {
    return tracer.startActiveSpan('onnx.inferBatch', async span => {
      const startTime = performance.now();
      const batchSize = batchInputs.length;

      try {
        // Combine inputs into single batch tensor
        const inputName = this.inputNames[0];
        const sampleSize = batchInputs[0][inputName].length;
        const batchData = new Float32Array(batchSize * sampleSize);

        for (let i = 0; i < batchSize; i++) {
          const sample = batchInputs[i][inputName];
          const data = sample instanceof Float32Array ? sample : new Float32Array(sample);
          batchData.set(data, i * sampleSize);
        }

        // Run batched inference
        const feeds = {
          [inputName]: new ort.Tensor('float32', batchData, [batchSize, sampleSize]),
        };

        const results = await this.session.run(feeds);

        // Split batch results
        const outputName = this.outputNames[0];
        const outputTensor = results[outputName];
        const outputSize = outputTensor.size / batchSize;
        const outputs = [];

        for (let i = 0; i < batchSize; i++) {
          const start = i * outputSize;
          const end = start + outputSize;
          outputs.push({
            [outputName]: Array.from(outputTensor.data.slice(start, end)),
          });
        }

        const batchTime = performance.now() - startTime;
        const throughput = (batchSize / batchTime) * 1000;

        span.setAttributes({
          'batch.size': batchSize,
          'batch.latencyMs': batchTime,
          'batch.throughput': throughput,
          'batch.avgLatencyMs': batchTime / batchSize,
        });

        span.setStatus({ code: 1 }); // OK
        return outputs;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: 2, message: error.message }); // ERROR
        throw new Error(`Batch inference failed: ${error.message}`);
      } finally {
        span.end();
      }
    });
  }

  /**
   * Get model metadata
   *
   * @returns {Object} Model information
   */
  getMetadata() {
    if (!this.session) {
      return null;
    }

    return {
      inputs: this.inputNames,
      outputs: this.outputNames,
      inputMeta: this.inputMeta,
      outputMeta: this.outputMeta,
    };
  }

  /**
   * Get performance metrics
   *
   * @returns {Object} Performance statistics
   */
  getMetrics() {
    const avgLatency =
      this.metrics.totalInferences > 0
        ? this.metrics.totalLatencyMs / this.metrics.totalInferences
        : 0;
    const throughput =
      this.metrics.totalLatencyMs > 0
        ? (this.metrics.totalSamples / this.metrics.totalLatencyMs) * 1000
        : 0;

    return {
      totalInferences: this.metrics.totalInferences,
      totalSamples: this.metrics.totalSamples,
      avgLatencyMs: avgLatency,
      throughputPerSec: throughput,
      errorCount: this.metrics.errorCount,
      errorRate:
        this.metrics.totalInferences > 0
          ? this.metrics.errorCount / this.metrics.totalInferences
          : 0,
    };
  }

  /**
   * Reset performance metrics
   */
  resetMetrics() {
    this.metrics = {
      totalInferences: 0,
      totalLatencyMs: 0,
      totalSamples: 0,
      errorCount: 0,
    };
  }

  /**
   * Dispose of model and free resources
   */
  async dispose() {
    if (this.session) {
      await this.session.release();
      this.session = null;
      this.inputNames = null;
      this.outputNames = null;
      this.inputMeta = null;
      this.outputMeta = null;
    }
  }
}

/**
 * Create ONNX runner instance
 *
 * @param {Object} options - Runner options
 * @returns {ONNXRunner} Runner instance
 */
export function createONNXRunner(options = {}) {
  return new ONNXRunner(options);
}
