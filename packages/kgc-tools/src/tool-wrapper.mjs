/**
 * @file Tool Wrapper - Contract Enforcement Layer
 * @module @unrdf/kgc-tools/tool-wrapper
 * @description Wraps tools to enforce input/output contracts and generate execution receipts
 */

import { z } from 'zod';

/**
 * Receipt schema for tool execution
 */
const ReceiptSchema = z.object({
  tool_name: z.string(),
  version: z.string(),
  inputs: z.any(),
  outputs: z.any().nullable(),
  status: z.enum(['success', 'error']),
  timestamp: z.number(),
  execution_time_ms: z.number(),
  error: z.string().optional(),
});

/**
 * Wraps a tool function to enforce contracts and generate receipts
 *
 * @param {Function} tool - The tool function to wrap
 * @param {Object} manifest - Tool manifest with schema and metadata
 * @param {string} manifest.name - Tool name
 * @param {string} manifest.version - Tool version
 * @param {z.ZodSchema} manifest.schema_in - Input validation schema
 * @param {z.ZodSchema} manifest.schema_out - Output validation schema
 * @param {string[]} manifest.capabilities - Tool capabilities
 * @returns {Function} Wrapped tool function
 *
 * @example
 * const readTool = async (inputs) => ({
 *   content: 'file content',
 *   size: 100
 * });
 *
 * const manifest = {
 *   name: 'Read',
 *   version: '1.0.0',
 *   schema_in: z.object({ path: z.string() }),
 *   schema_out: z.object({ content: z.string(), size: z.number() }),
 *   capabilities: ['file-read']
 * };
 *
 * const wrapped = Wrap(readTool, manifest);
 * const result = await wrapped({ path: '/test.txt' });
 * // Returns: { delta: {...}, receipt: {...} }
 */
export function Wrap(tool, manifest) {
  // Validate manifest structure (basic validation)
  if (!manifest || typeof manifest !== 'object') {
    throw new Error('Manifest must be an object');
  }
  if (!manifest.name || typeof manifest.name !== 'string') {
    throw new Error('Manifest must have a name string');
  }
  if (!manifest.version || typeof manifest.version !== 'string') {
    throw new Error('Manifest must have a version string');
  }
  if (!manifest.schema_in || typeof manifest.schema_in.parse !== 'function') {
    throw new Error('Manifest must have a schema_in with parse method');
  }
  if (!manifest.schema_out || typeof manifest.schema_out.parse !== 'function') {
    throw new Error('Manifest must have a schema_out with parse method');
  }
  if (!Array.isArray(manifest.capabilities)) {
    throw new Error('Manifest must have capabilities array');
  }

  /**
   * Wrapped tool function
   * @param {*} inputs - Tool inputs to validate
   * @returns {Promise<{delta: *, receipt: Object}>} Execution result with delta and receipt
   */
  return async function wrappedTool(inputs) {
    const startTime = performance.now();
    const timestamp = Date.now();

    let validatedInputs;
    let outputs = null;
    let delta = null;
    let status = 'success';
    let errorMessage;

    try {
      // Validate inputs against schema_in
      validatedInputs = manifest.schema_in.parse(inputs);

      // Execute the tool
      const rawOutputs = await tool(validatedInputs);

      // Validate outputs against schema_out
      outputs = manifest.schema_out.parse(rawOutputs);

      // Delta is the validated output
      delta = outputs;
      status = 'success';
    } catch (error) {
      // Handle validation or execution errors
      status = 'error';
      errorMessage = error.message;
      outputs = null;
      delta = null;

      // Re-validate inputs if they weren't validated yet
      if (!validatedInputs) {
        validatedInputs = inputs;
      }
    }

    const endTime = performance.now();
    const executionTimeMs = endTime - startTime;

    // Build receipt
    const receipt = {
      tool_name: manifest.name,
      version: manifest.version,
      inputs: validatedInputs,
      outputs,
      status,
      timestamp,
      execution_time_ms: executionTimeMs,
    };

    // Add error field if present
    if (errorMessage) {
      receipt.error = errorMessage;
    }

    // Validate receipt structure
    ReceiptSchema.parse(receipt);

    return {
      delta,
      receipt,
    };
  };
}

/**
 * Validates a receipt structure
 * @param {*} receipt - Receipt to validate
 * @returns {boolean} True if valid
 */
export function validateReceipt(receipt) {
  try {
    ReceiptSchema.parse(receipt);
    return true;
  } catch {
    return false;
  }
}
