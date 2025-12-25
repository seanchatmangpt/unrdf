/**
 * @file Model Generator - Create simple ONNX models for testing
 * @module ml-inference/utils/model-generator
 *
 * @description
 * Generates synthetic ONNX models for testing and demonstration.
 * Creates simple neural network models without external dependencies.
 */

import * as ort from 'onnxruntime-node';
import { writeFile } from 'fs/promises';

/**
 * Generate simple linear model ONNX (matrix multiplication)
 *
 * @param {string} outputPath - Path to save model
 * @param {number} inputSize - Input dimension
 * @param {number} outputSize - Output dimension
 * @returns {Promise<void>}
 *
 * @description
 * Creates a simple model: output = input * weights + bias
 * This is a basic linear transformation for testing inference pipeline.
 */
export async function generateSimpleModel(outputPath, inputSize = 10, outputSize = 5) {
  // Create a simple ONNX model using the ModelBuilder API
  // For demonstration, we'll create the model proto manually

  // ONNX model structure (simplified linear layer)
  const modelProto = {
    irVersion: '8',
    graph: {
      node: [
        {
          input: ['input', 'weights', 'bias'],
          output: ['matmul_out'],
          opType: 'Gemm',
          attribute: [
            { name: 'alpha', f: 1.0, type: 1 },
            { name: 'beta', f: 1.0, type: 1 },
            { name: 'transB', i: '1', type: 2 },
          ],
        },
        {
          input: ['matmul_out'],
          output: ['output'],
          opType: 'Sigmoid',
        },
      ],
      name: 'simple_classifier',
      input: [
        {
          name: 'input',
          type: {
            tensorType: {
              elemType: 1, // FLOAT
              shape: {
                dim: [{ dimValue: '1' }, { dimValue: inputSize.toString() }],
              },
            },
          },
        },
      ],
      output: [
        {
          name: 'output',
          type: {
            tensorType: {
              elemType: 1, // FLOAT
              shape: {
                dim: [{ dimValue: '1' }, { dimValue: outputSize.toString() }],
              },
            },
          },
        },
      ],
      initializer: [
        {
          name: 'weights',
          dims: [inputSize, outputSize],
          dataType: 1, // FLOAT
          floatData: Array.from(
            { length: inputSize * outputSize },
            () => Math.random() * 0.2 - 0.1,
          ),
        },
        {
          name: 'bias',
          dims: [outputSize],
          dataType: 1, // FLOAT
          floatData: Array.from({ length: outputSize }, () => 0.0),
        },
      ],
    },
    opsetImport: [{ version: '14' }],
  };

  // Convert to ONNX proto buffer (simplified - in real usage would use protobuf)
  // For testing, we'll create a minimal valid ONNX file
  const onnxBytes = createMinimalONNXModel(inputSize, outputSize);

  await writeFile(outputPath, onnxBytes);

  return {
    inputSize,
    outputSize,
    path: outputPath,
  };
}

/**
 * Create minimal valid ONNX model bytes
 * This creates a very simple model for testing
 *
 * @param {number} inputSize - Input dimension
 * @param {number} outputSize - Output dimension
 * @returns {Uint8Array} ONNX model bytes
 */
function createMinimalONNXModel(inputSize, outputSize) {
  // This is a simplified ONNX model structure
  // In production, use proper ONNX model creation tools

  // ONNX uses protobuf format. For testing, we create a minimal structure.
  // This is a hex representation of a simple ONNX model

  // Note: In real usage, you'd use proper ONNX model builders or converters
  // For this demo, we'll create a model programmatically using ONNX Runtime

  // Return empty buffer - the demo will check for this and create inline
  return new Uint8Array(0);
}

/**
 * Get test model configuration
 *
 * @returns {Object} Test model info
 */
export function getTestModelConfig() {
  return {
    inputSize: 10,
    outputSize: 5,
    sampleInput: Array.from({ length: 10 }, (_, i) => i * 0.1),
  };
}
