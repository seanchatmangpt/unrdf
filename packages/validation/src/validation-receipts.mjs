/**
 * Validation with L5 Receipt Support
 *
 * Wraps validation operations with Zod schemas and receipts
 *
 * @module @unrdf/validation/validation-receipts
 */

import { z } from 'zod';
import {
  withReceipt,
  createContext,
} from '../../v6-core/src/receipt-pattern.mjs';

/**
 * Validation Input Schema
 */
export const ValidationInputSchema = z.object({
  data: z.any(),
  schema: z.any(), // Zod schema
  options: z
    .object({
      strict: z.boolean().default(true),
      abortEarly: z.boolean().default(false),
    })
    .default({}),
});

/**
 * Validation Result Schema
 */
export const ValidationResultSchema = z.object({
  valid: z.boolean(),
  data: z.any().optional(),
  errors: z.array(z.string()).optional(),
});

/**
 * Pure function: Validate data
 *
 * @param {Object} input - Validation input
 * @returns {Object} Validation result
 */
function validateDataImpl(input) {
  const validated = ValidationInputSchema.parse(input);

  try {
    const parsedData = validated.schema.parse(validated.data);
    return {
      valid: true,
      data: parsedData,
      errors: [],
    };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return {
        valid: false,
        errors: error.errors.map(e => `${e.path.join('.')}: ${e.message}`),
      };
    }
    throw error;
  }
}

/**
 * Wrapped: Validate data with receipt
 */
export const validateData = withReceipt(validateDataImpl, {
  operation: 'validateData',
  profile: 'verification',
  inputSchema: z.tuple([ValidationInputSchema]),
  outputSchema: ValidationResultSchema,
});

/**
 * L5 Determinism Test
 */
export async function testValidationDeterminism(context, iterations = 100) {
  const input = {
    data: { name: 'Alice', age: 30 },
    schema: z.object({
      name: z.string(),
      age: z.number().int().positive(),
    }),
    options: {},
  };

  const receipts = [];
  const hashes = new Set();

  for (let i = 0; i < iterations; i++) {
    const { receipt } = await validateData(context, input);
    receipts.push(receipt);
    hashes.add(receipt.receiptHash);
  }

  return {
    iterations,
    uniqueHashes: hashes.size,
    deterministic: hashes.size === 1,
    expectedHash: receipts[0].receiptHash,
  };
}
