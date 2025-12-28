/**
 * @fileoverview Example of how to apply Zod schemas to functions
 *
 * This demonstrates the pattern for integrating generated schemas
 * into source functions for runtime validation.
 *
 * @module schemas/apply-schemas-example
 */

import { z } from 'zod';
import forbiddenOpsSchemas from '../admission/forbidden-operations.schema.mjs';
import invariantsSchemas from '../admission/invariants.schema.mjs';

/**
 * Pattern 1: Wrapper function with schema validation
 *
 * Wraps an existing function to add input/output validation
 */
export function withSchemaValidation(fn, schema) {
  return function(...args) {
    // Validate inputs
    const validatedArgs = schema.params.parse(args);

    // Execute function
    const result = fn(...validatedArgs);

    // Validate output
    const validatedResult = schema.returns.parse(result);

    return validatedResult;
  };
}

/**
 * Pattern 2: Direct inline validation
 *
 * Add validation directly inside function
 */
export function isProtectedNamespaceWithValidation(iri) {
  // Validate input
  const [validatedIri] = forbiddenOpsSchemas.isProtectedNamespace.params.parse([iri]);

  // Original logic
  const PROTECTED_NAMESPACES = new Set([
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
    'http://www.w3.org/2000/01/rdf-schema#',
    'http://www.w3.org/2002/07/owl#'
  ]);

  for (const protectedNS of PROTECTED_NAMESPACES) {
    if (validatedIri.startsWith(protectedNS)) {
      const result = true;
      // Validate output
      return forbiddenOpsSchemas.isProtectedNamespace.returns.parse(result);
    }
  }

  const result = false;
  return forbiddenOpsSchemas.isProtectedNamespace.returns.parse(result);
}

/**
 * Pattern 3: Higher-order function for schema application
 *
 * Creates validated versions of all functions in a module
 */
export function createValidatedModule(module, schemas) {
  const validated = {};

  for (const [fnName, fn] of Object.entries(module)) {
    if (typeof fn === 'function' && schemas[fnName]) {
      validated[fnName] = withSchemaValidation(fn, schemas[fnName]);
    } else {
      validated[fnName] = fn;
    }
  }

  return validated;
}

/**
 * Pattern 4: Decorator-style validation (for classes)
 */
export function validateMethod(schema) {
  return function(target, propertyKey, descriptor) {
    const originalMethod = descriptor.value;

    descriptor.value = function(...args) {
      // Validate inputs
      const validatedArgs = schema.params.parse(args);

      // Execute method
      const result = originalMethod.apply(this, validatedArgs);

      // Validate output
      return schema.returns.parse(result);
    };

    return descriptor;
  };
}

/**
 * Pattern 5: Async function validation
 */
export function withAsyncSchemaValidation(asyncFn, schema) {
  return async function(...args) {
    // Validate inputs
    const validatedArgs = schema.params.parse(args);

    // Execute async function
    const result = await asyncFn(...validatedArgs);

    // Validate output
    const validatedResult = schema.returns.parse(result);

    return validatedResult;
  };
}

/**
 * Example usage:
 *
 * // Import original function
 * import { isProtectedNamespace } from '../admission/forbidden-operations.mjs';
 * import forbiddenOpsSchemas from '../admission/forbidden-operations.schema.mjs';
 *
 * // Create validated version
 * const validatedIsProtectedNamespace = withSchemaValidation(
 *   isProtectedNamespace,
 *   forbiddenOpsSchemas.isProtectedNamespace
 * );
 *
 * // Use validated version
 * try {
 *   const result = validatedIsProtectedNamespace('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
 *   console.log('Valid:', result);
 * } catch (error) {
 *   console.error('Validation failed:', error.message);
 * }
 */

/**
 * Production-ready validation wrapper with error handling
 */
export function createValidatedFunction(fn, schema, options = {}) {
  const {
    validateInput = true,
    validateOutput = true,
    onValidationError = null,
    skipValidationInProduction = false
  } = options;

  return function(...args) {
    // Skip validation in production if configured
    if (skipValidationInProduction && process.env.NODE_ENV === 'production') {
      return fn(...args);
    }

    try {
      // Validate inputs
      const validatedArgs = validateInput ? schema.params.parse(args) : args;

      // Execute function
      const result = fn(...validatedArgs);

      // Validate output
      const validatedResult = validateOutput ? schema.returns.parse(result) : result;

      return validatedResult;
    } catch (error) {
      if (onValidationError) {
        return onValidationError(error, fn.name, args);
      }
      throw error;
    }
  };
}
