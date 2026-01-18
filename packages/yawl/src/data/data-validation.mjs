/**
 * @file Data Validation
 * @module @unrdf/yawl/data/data-validation
 * @description Runtime validation of YAWL data using Zod schemas and SHACL shapes.
 * Provides detailed error messages with field paths for debugging.
 */

import { z } from 'zod';

// =============================================================================
// Zod Validators
// =============================================================================

/**
 * Validate data against Zod schema
 * @param {any} data - Data to validate
 * @param {z.ZodSchema} schema - Zod schema
 * @returns {Object} Validation result
 * @returns {boolean} result.valid - Whether data is valid
 * @returns {any} result.data - Validated data (if valid)
 * @returns {Array<Object>} result.errors - Validation errors (if invalid)
 * @example
 * const schema = z.object({ name: z.string(), age: z.number() });
 * validateWithZod({ name: 'Alice', age: 30 }, schema) // => { valid: true, data: {...} }
 */
export function validateWithZod(data, schema) {
  const result = schema.safeParse(data);

  if (result.success) {
    return {
      valid: true,
      data: result.data,
      errors: [],
    };
  }

  return {
    valid: false,
    data: null,
    errors: result.error.errors.map(err => ({
      path: err.path.join('.'),
      message: err.message,
      code: err.code,
    })),
  };
}

// =============================================================================
// Custom Validators
// =============================================================================

/**
 * Validate data with regex pattern
 * @param {string} data - Data to validate
 * @param {string|RegExp} pattern - Regex pattern
 * @param {string} [message] - Custom error message
 * @returns {Object} Validation result
 * @example
 * validateRegex('test@example.com', /^[^@]+@[^@]+\.[^@]+$/) // => { valid: true }
 */
export function validateRegex(data, pattern, message = null) {
  if (typeof data !== 'string') {
    return {
      valid: false,
      errors: [{ path: '', message: 'Data must be a string' }],
    };
  }

  const regex = typeof pattern === 'string' ? new RegExp(pattern) : pattern;

  if (regex.test(data)) {
    return { valid: true, data, errors: [] };
  }

  return {
    valid: false,
    errors: [{
      path: '',
      message: message || `Value does not match pattern ${regex}`,
    }],
  };
}

/**
 * Validate data is within range
 * @param {number} data - Numeric data
 * @param {number} min - Minimum value (inclusive)
 * @param {number} max - Maximum value (inclusive)
 * @returns {Object} Validation result
 * @example
 * validateRange(50, 0, 100) // => { valid: true }
 * validateRange(150, 0, 100) // => { valid: false, errors: [...] }
 */
export function validateRange(data, min, max) {
  if (typeof data !== 'number' || isNaN(data)) {
    return {
      valid: false,
      errors: [{ path: '', message: 'Data must be a number' }],
    };
  }

  if (data < min || data > max) {
    return {
      valid: false,
      errors: [{
        path: '',
        message: `Value ${data} is outside range [${min}, ${max}]`,
      }],
    };
  }

  return { valid: true, data, errors: [] };
}

/**
 * Validate data format (email, url, uuid, etc.)
 * @param {string} data - Data to validate
 * @param {string} format - Format type (email|url|uuid|date)
 * @returns {Object} Validation result
 * @example
 * validateFormat('test@example.com', 'email') // => { valid: true }
 */
export function validateFormat(data, format) {
  const formats = {
    email: /^[^\s@]+@[^\s@]+\.[^\s@]+$/,
    url: /^https?:\/\/.+/,
    uuid: /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i,
    date: /^\d{4}-\d{2}-\d{2}$/,
  };

  const pattern = formats[format];
  if (!pattern) {
    return {
      valid: false,
      errors: [{ path: '', message: `Unknown format: ${format}` }],
    };
  }

  return validateRegex(data, pattern, `Invalid ${format} format`);
}

// =============================================================================
// SHACL-like Validation
// =============================================================================

/**
 * Validate RDF data against SHACL-like shape
 * @param {Object} data - RDF data (simplified as JavaScript object)
 * @param {Object} shape - SHACL-like shape definition
 * @returns {Object} Validation result
 * @example
 * const shape = {
 *   properties: {
 *     name: { type: 'string', required: true },
 *     age: { type: 'number', min: 0, max: 150 }
 *   }
 * };
 * validateShape({ name: 'Alice', age: 30 }, shape) // => { valid: true }
 */
export function validateShape(data, shape) {
  const errors = [];

  if (!shape.properties) {
    return { valid: true, data, errors: [] };
  }

  for (const [propName, propShape] of Object.entries(shape.properties)) {
    const value = data[propName];

    if (propShape.required && (value === undefined || value === null)) {
      errors.push({
        path: propName,
        message: `Required property missing: ${propName}`,
      });
      continue;
    }

    if (value === undefined || value === null) {
      continue;
    }

    if (propShape.type) {
      const actualType = Array.isArray(value) ? 'array' : typeof value;
      if (actualType !== propShape.type) {
        errors.push({
          path: propName,
          message: `Type mismatch: expected ${propShape.type}, got ${actualType}`,
        });
        continue;
      }
    }

    if (propShape.type === 'number') {
      if (propShape.min !== undefined && value < propShape.min) {
        errors.push({
          path: propName,
          message: `Value ${value} is below minimum ${propShape.min}`,
        });
      }
      if (propShape.max !== undefined && value > propShape.max) {
        errors.push({
          path: propName,
          message: `Value ${value} is above maximum ${propShape.max}`,
        });
      }
    }

    if (propShape.type === 'string') {
      if (propShape.minLength && value.length < propShape.minLength) {
        errors.push({
          path: propName,
          message: `String length ${value.length} is below minimum ${propShape.minLength}`,
        });
      }
      if (propShape.maxLength && value.length > propShape.maxLength) {
        errors.push({
          path: propName,
          message: `String length ${value.length} is above maximum ${propShape.maxLength}`,
        });
      }
      if (propShape.pattern) {
        const regex = new RegExp(propShape.pattern);
        if (!regex.test(value)) {
          errors.push({
            path: propName,
            message: `Value does not match pattern ${propShape.pattern}`,
          });
        }
      }
    }

    if (propShape.enum && !propShape.enum.includes(value)) {
      errors.push({
        path: propName,
        message: `Value must be one of: ${propShape.enum.join(', ')}`,
      });
    }
  }

  return {
    valid: errors.length === 0,
    data: errors.length === 0 ? data : null,
    errors,
  };
}

// =============================================================================
// Unified Validation
// =============================================================================

/**
 * Validate data using specified validator
 * @param {any} data - Data to validate
 * @param {Object} config - Validation configuration
 * @param {string} config.type - Validator type (zod|regex|range|format|shape)
 * @param {any} config.schema - Schema/pattern/shape for validation
 * @returns {Object} Validation result
 * @example
 * validate({ name: 'Alice' }, { type: 'zod', schema: z.object({ name: z.string() }) })
 */
export function validate(data, config) {
  switch (config.type) {
    case 'zod':
      return validateWithZod(data, config.schema);

    case 'regex':
      return validateRegex(data, config.pattern, config.message);

    case 'range':
      return validateRange(data, config.min, config.max);

    case 'format':
      return validateFormat(data, config.format);

    case 'shape':
      return validateShape(data, config.shape);

    default:
      return {
        valid: false,
        errors: [{ path: '', message: `Unknown validation type: ${config.type}` }],
      };
  }
}

// =============================================================================
// Exports
// =============================================================================

export default {
  validateWithZod,
  validateRegex,
  validateRange,
  validateFormat,
  validateShape,
  validate,
};
