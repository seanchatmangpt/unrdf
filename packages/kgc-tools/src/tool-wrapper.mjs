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
 */
export function Wrap(tool, manifest) {
  if (!manifest || typeof manifest !== 'object') throw new Error('Manifest must be an object');
  if (!manifest.name || typeof manifest.name !== 'string') throw new Error('Manifest must have a name string');
  if (!manifest.version || typeof manifest.version !== 'string') throw new Error('Manifest must have a version string');
  if (!manifest.schema_in || typeof manifest.schema_in.parse !== 'function') throw new Error('Manifest must have a schema_in with parse method');
  if (!manifest.schema_out || typeof manifest.schema_out.parse !== 'function') throw new Error('Manifest must have a schema_out with parse method');
  if (!Array.isArray(manifest.capabilities)) throw new Error('Manifest must have capabilities array');

  return async function wrappedTool(inputs) {
    const startTime = performance.now();
    const timestamp = Date.now();

    // Input admission is a precondition. Invalid inputs are refused before
    // execution and therefore reject rather than being converted into a tool
    // execution receipt.
    const validatedInputs = manifest.schema_in.parse(inputs);

    let outputs = null;
    let delta = null;
    let status = 'success';
    let errorMessage;

    try {
      const rawOutputs = await tool(validatedInputs);
      outputs = manifest.schema_out.parse(rawOutputs);
      delta = outputs;
    } catch (error) {
      status = 'error';
      errorMessage = error.message;
    }

    const receipt = {
      tool_name: manifest.name,
      version: manifest.version,
      inputs: validatedInputs,
      outputs,
      status,
      timestamp,
      execution_time_ms: performance.now() - startTime,
    };
    if (errorMessage) receipt.error = errorMessage;

    ReceiptSchema.parse(receipt);
    return { delta, receipt };
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
