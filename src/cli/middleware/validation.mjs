/**
 * @file Input Validation Middleware
 * @module cli-v2/middleware/validation
 */

import { z } from 'zod';

/**
 * Validate command arguments
 * @param {Object} args - Command arguments
 * @param {Object} schema - Zod schema
 * @returns {Object} Validated arguments
 */
export function validateArgs(args, schema) {
  try {
    return schema.parse(args);
  } catch (error) {
    if (error instanceof z.ZodError) {
      const messages = error.errors.map(e => `${e.path.join('.')}: ${e.message}`);
      throw new Error(`Validation error:\n${messages.join('\n')}`);
    }
    throw error;
  }
}

/**
 * Common argument schemas
 */
export const CommonSchemas = {
  outputFormat: z.enum(['json', 'yaml', 'table', 'tree']).default('table'),
  dryRun: z.boolean().default(false),
  verbose: z.boolean().default(false),
  watch: z.boolean().default(false)
};
