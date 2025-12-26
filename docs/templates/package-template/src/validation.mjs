/**
 * @file Validation schemas and utilities
 * @module @unrdf/package-name/validation
 */

import { z } from 'zod';

/**
 * Thing configuration schema
 */
export const ThingSchema = z.object({
  option: z.string().default('default'),
});

/**
 * Validates thing configuration
 *
 * @param {*} config - Configuration to validate
 * @returns {boolean} True if valid
 * @example
 * if (validateThing(config)) {
 *   const thing = createThing(config);
 * }
 */
export function validateThing(config) {
  try {
    ThingSchema.parse(config);
    return true;
  } catch {
    return false;
  }
}
