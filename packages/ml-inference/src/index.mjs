/**
 * @file ML Inference - ONNX Runtime integration for RDF streaming
 * @module ml-inference
 *
 * @description
 * High-performance machine learning inference pipeline for RDF processing.
 * Provides ONNX model execution, streaming inference, and model management.
 */

export { ONNXRunner, createONNXRunner } from './runtime/onnx-runner.mjs';
export {
  StreamingInferencePipeline,
  createStreamingInferencePipeline,
} from './pipeline/streaming-inference.mjs';
export { ModelRegistry, createModelRegistry } from './registry/model-registry.mjs';

/**
 * Create complete inference stack
 *
 * @param {Object} options - Configuration options
 * @param {Object} [options.runnerOptions] - ONNX runner options
 * @param {Object} [options.pipelineOptions] - Pipeline options
 * @returns {Object} Inference stack
 *
 * @example
 * const { runner, pipeline, registry } = createInferenceStack({
 *   runnerOptions: { executionProviders: ['cpu'] },
 *   pipelineOptions: { batchSize: 32 }
 * });
 */
export function createInferenceStack(options = {}) {
  const { runnerOptions = {}, pipelineOptions = {} } = options;

  const registry = createModelRegistry();
  const runner = createONNXRunner(runnerOptions);
  const pipeline = createStreamingInferencePipeline(runner, pipelineOptions);

  return {
    runner,
    pipeline,
    registry,
    async dispose() {
      await pipeline.destroy();
      await runner.dispose();
      await registry.destroy();
    },
  };
}
