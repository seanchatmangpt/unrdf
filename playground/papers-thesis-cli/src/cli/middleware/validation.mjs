/**
 * @fileoverview Validation Middleware
 *
 * @description
 * Validates all command arguments using Zod schemas.
 * Checks preconditions like file existence and provides
 * helpful error messages. Supports custom validators.
 *
 * @module cli/middleware/validation
 * @version 1.0.0
 * @license MIT
 */

import { z } from 'zod';
import { existsSync, statSync } from 'node:fs';
import { resolve, extname } from 'node:path';

/**
 * @typedef {Object} ValidationResult
 * @property {boolean} valid - Whether validation passed
 * @property {Array<Object>} errors - Validation errors
 * @property {Array<Object>} warnings - Validation warnings
 * @property {Object} data - Validated and transformed data
 */

/**
 * Custom validators registry
 * @type {Map<string, Function>}
 */
const customValidators = new Map();

/**
 * Common Zod schemas for CLI arguments
 */
export const CommonSchemas = {
  /**
   * File path that must exist
   */
  existingFile: z.string().refine(path => existsSync(path) && statSync(path).isFile(), {
    message: 'File does not exist or is not a file',
  }),

  /**
   * Directory path that must exist
   */
  existingDirectory: z.string().refine(path => existsSync(path) && statSync(path).isDirectory(), {
    message: 'Directory does not exist or is not a directory',
  }),

  /**
   * Valid file path (may not exist)
   */
  filePath: z
    .string()
    .refine(path => /^[^<>:"|?*]+$/.test(path), { message: 'Invalid file path characters' }),

  /**
   * Output format options
   */
  outputFormat: z.enum(['json', 'json-pretty', 'yaml', 'table', 'latex', 'csv']),

  /**
   * Paper family options
   */
  paperFamily: z
    .enum(['imrad', 'dsr', 'argument', 'contribution', 'IMRaD', 'Argument', 'Contribution', 'DSR'])
    .transform(v => v.toLowerCase()),

  /**
   * Thesis type options
   */
  thesisType: z
    .enum(['masters', 'phd', 'doctoral', 'Masters', 'PhD', 'Doctoral'])
    .transform(v => v.toLowerCase()),

  /**
   * Non-empty string
   */
  nonEmptyString: z.string().min(1, 'Value cannot be empty'),

  /**
   * Positive integer
   */
  positiveInt: z.number().int().positive(),

  /**
   * Date string
   */
  dateString: z.string().refine(d => !isNaN(Date.parse(d)), { message: 'Invalid date format' }),

  /**
   * URL string
   */
  url: z.string().url('Invalid URL format'),

  /**
   * Email string
   */
  email: z.string().email('Invalid email format'),
};

/**
 * Precondition checkers
 */
export const Preconditions = {
  /**
   * Check if file exists
   * @param {string} path - File path
   * @param {string} [label] - Error label
   * @returns {{valid: boolean, error?: string}}
   */
  fileExists(path, label = 'File') {
    const resolved = resolve(path);
    if (!existsSync(resolved)) {
      return { valid: false, error: `${label} not found: ${path}` };
    }
    if (!statSync(resolved).isFile()) {
      return { valid: false, error: `${label} is not a file: ${path}` };
    }
    return { valid: true };
  },

  /**
   * Check if directory exists
   * @param {string} path - Directory path
   * @param {string} [label] - Error label
   * @returns {{valid: boolean, error?: string}}
   */
  directoryExists(path, label = 'Directory') {
    const resolved = resolve(path);
    if (!existsSync(resolved)) {
      return { valid: false, error: `${label} not found: ${path}` };
    }
    if (!statSync(resolved).isDirectory()) {
      return { valid: false, error: `${label} is not a directory: ${path}` };
    }
    return { valid: true };
  },

  /**
   * Check file extension
   * @param {string} path - File path
   * @param {Array<string>} allowed - Allowed extensions
   * @returns {{valid: boolean, error?: string}}
   */
  fileExtension(path, allowed) {
    const ext = extname(path).toLowerCase();
    if (!allowed.includes(ext)) {
      return {
        valid: false,
        error: `Invalid file extension: ${ext}. Allowed: ${allowed.join(', ')}`,
      };
    }
    return { valid: true };
  },

  /**
   * Check environment variable is set
   * @param {string} name - Environment variable name
   * @returns {{valid: boolean, error?: string}}
   */
  envVar(name) {
    if (!process.env[name]) {
      return {
        valid: false,
        error: `Environment variable not set: ${name}`,
      };
    }
    return { valid: true };
  },

  /**
   * Check value is in allowed set
   * @param {*} value - Value to check
   * @param {Array} allowed - Allowed values
   * @param {string} [label] - Error label
   * @returns {{valid: boolean, error?: string}}
   */
  oneOf(value, allowed, label = 'Value') {
    if (!allowed.includes(value)) {
      return {
        valid: false,
        error: `${label} must be one of: ${allowed.join(', ')}. Got: ${value}`,
      };
    }
    return { valid: true };
  },
};

/**
 * Register a custom validator
 * @param {string} name - Validator name
 * @param {Function} validator - Validator function (value, context) => {valid, error?}
 */
export function registerValidator(name, validator) {
  if (typeof validator !== 'function') {
    throw new Error('Validator must be a function');
  }
  customValidators.set(name, validator);
}

/**
 * Get a registered validator
 * @param {string} name - Validator name
 * @returns {Function|undefined}
 */
export function getValidator(name) {
  return customValidators.get(name);
}

/**
 * Create a Zod schema for command arguments
 * @param {Object} argsDef - Citty-style args definition
 * @returns {z.ZodSchema}
 */
export function createArgsSchema(argsDef) {
  const shape = {};

  for (const [key, def] of Object.entries(argsDef)) {
    let schema;

    // Determine base type
    switch (def.type) {
      case 'boolean':
        schema = z.boolean();
        break;
      case 'number':
        schema = z.number();
        break;
      case 'string':
      case 'positional':
      default:
        schema = z.string();
    }

    // Apply modifiers
    if (!def.required && def.type !== 'positional') {
      schema = schema.optional();
    }

    if (def.default !== undefined) {
      schema = schema.default(def.default);
    }

    // Add description
    if (def.description) {
      schema = schema.describe(def.description);
    }

    shape[key] = schema;
  }

  return z.object(shape);
}

/**
 * Format Zod errors into user-friendly messages
 * @param {z.ZodError} zodError - Zod error object
 * @returns {Array<{path: string, message: string}>}
 */
export function formatZodErrors(zodError) {
  return zodError.errors.map(err => ({
    path: err.path.join('.'),
    message: err.message,
    code: err.code,
    received: err.received,
  }));
}

/**
 * Validation middleware handler
 * @param {Object} context - Middleware context
 * @returns {Promise<Object>} Modified context
 */
export async function validationMiddleware(context) {
  const validationResult = {
    valid: true,
    errors: [],
    warnings: [],
  };

  // Get schema from context or config
  const schema = context.config?.validation?.schema || context._argsSchema || null;

  // Validate with Zod schema if available
  if (schema) {
    try {
      const result = schema.safeParse(context.args);

      if (!result.success) {
        validationResult.valid = false;
        validationResult.errors.push(...formatZodErrors(result.error));
      } else {
        // Update args with transformed/coerced values
        context.args = result.data;
      }
    } catch (err) {
      validationResult.errors.push({
        path: '',
        message: `Schema validation error: ${err.message}`,
      });
      validationResult.valid = false;
    }
  }

  // Run precondition checks from config
  const preconditions = context.config?.validation?.preconditions || [];
  for (const precondition of preconditions) {
    const { type, args: pArgs, label } = precondition;
    const checker = Preconditions[type];

    if (checker) {
      const value = resolveValue(pArgs, context.args);
      const result = checker(value, label);

      if (!result.valid) {
        validationResult.errors.push({
          path: pArgs,
          message: result.error,
        });
        validationResult.valid = false;
      }
    }
  }

  // Run custom validators
  const customValidations = context.config?.validation?.custom || [];
  for (const validation of customValidations) {
    const { name, args: vArgs } = validation;
    const validator = customValidators.get(name);

    if (validator) {
      const value = resolveValue(vArgs, context.args);
      const result = await validator(value, context);

      if (!result.valid) {
        validationResult.errors.push({
          path: vArgs,
          message: result.error,
        });
        validationResult.valid = false;
      }
      if (result.warning) {
        validationResult.warnings.push({
          path: vArgs,
          message: result.warning,
        });
      }
    }
  }

  // Store validation result in context
  context.validation = validationResult;

  // Add validation utilities to context
  context.validate = {
    /**
     * Validate a value against a schema
     * @param {*} value - Value to validate
     * @param {z.ZodSchema} schema - Zod schema
     * @returns {ValidationResult}
     */
    value(value, schema) {
      const result = schema.safeParse(value);
      return {
        valid: result.success,
        data: result.success ? result.data : undefined,
        errors: result.success ? [] : formatZodErrors(result.error),
      };
    },

    /**
     * Check a precondition
     * @param {string} type - Precondition type
     * @param {*} value - Value to check
     * @param {string} [label] - Error label
     * @returns {{valid: boolean, error?: string}}
     */
    precondition(type, value, label) {
      const checker = Preconditions[type];
      if (!checker) {
        return { valid: false, error: `Unknown precondition: ${type}` };
      }
      return checker(value, label);
    },
  };

  // Abort if validation failed
  if (!validationResult.valid && context.config?.validation?.abortOnError !== false) {
    const errorMsg = validationResult.errors
      .map(e => `  - ${e.path ? e.path + ': ' : ''}${e.message}`)
      .join('\n');

    const error = new Error(`Validation failed:\n${errorMsg}`);
    error.validationErrors = validationResult.errors;
    throw error;
  }

  // Log warnings
  if (validationResult.warnings.length > 0 && !context.quiet) {
    console.log('\nValidation warnings:');
    validationResult.warnings.forEach(w => {
      console.log(`  - ${w.path ? w.path + ': ' : ''}${w.message}`);
    });
  }

  return context;
}

/**
 * Resolve a value from args by path
 * @param {string} path - Dot-separated path
 * @param {Object} args - Arguments object
 * @returns {*}
 */
function resolveValue(path, args) {
  if (!path || !path.includes('.')) {
    return args[path];
  }
  return path.split('.').reduce((obj, key) => obj?.[key], args);
}

/**
 * Create a validation config for a command
 * @param {Object} options - Validation options
 * @returns {Object}
 */
export function createValidationConfig(options) {
  return {
    schema: options.schema || null,
    preconditions: options.preconditions || [],
    custom: options.custom || [],
    abortOnError: options.abortOnError !== false,
  };
}

export default validationMiddleware;
