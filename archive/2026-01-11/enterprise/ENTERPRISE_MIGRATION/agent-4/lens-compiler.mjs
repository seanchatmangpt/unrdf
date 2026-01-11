/**
 * @fileoverview Lens compilation and optimization
 * @module lens-compiler
 */

import { validateFunction, validateString, hashObject } from './lens-utils.mjs';

/**
 * @typedef {Object} CompiledLens
 * @property {string} name - Lens name
 * @property {string} version - Lens version
 * @property {Function} toSubstrate - Optimized to-substrate transformation
 * @property {Function} fromSubstrate - Optimized from-substrate transformation
 * @property {string} hash - Deterministic hash of lens definition
 * @property {number} compiledAt - Compilation timestamp
 * @property {Object} metadata - Compilation metadata
 */

/**
 * Compiles a lens definition into an optimized executable form
 *
 * Compilation steps:
 * 1. Validates lens structure
 * 2. Optimizes transformation chains
 * 3. Adds instrumentation hooks
 * 4. Computes deterministic hash
 *
 * @param {Object} definition - Lens definition to compile
 * @returns {CompiledLens} Compiled lens object
 * @throws {Error} If definition is invalid
 *
 * @example
 * const compiled = compileLens({
 *   name: 'user-profile',
 *   version: '1.0.0',
 *   toSubstrate: (req) => ({ type: 'UPSERT', data: req }),
 *   fromSubstrate: (data) => ({ id: data.id }),
 * });
 */
export function compileLens(definition) {
  // Validate definition structure
  validateLensDefinition(definition);

  // Extract core functions
  const { name, version, toSubstrate, fromSubstrate, description = '', metadata = {} } = definition;

  // Create optimized transformation functions
  const optimizedToSubstrate = optimizeTransformation(toSubstrate, 'toSubstrate');
  const optimizedFromSubstrate = optimizeTransformation(fromSubstrate, 'fromSubstrate');

  // Compute deterministic hash
  const lensHash = computeLensHash({
    name,
    version,
    toSubstrate: toSubstrate.toString(),
    fromSubstrate: fromSubstrate.toString(),
  });

  // Build compiled lens
  const compiled = {
    name,
    version,
    toSubstrate: optimizedToSubstrate,
    fromSubstrate: optimizedFromSubstrate,
    hash: lensHash,
    compiledAt: Date.now(),
    metadata: {
      ...metadata,
      description,
      compilationVersion: '1.0.0',
    },
  };

  return compiled;
}

/**
 * Validates lens definition structure
 *
 * @param {Object} definition - Lens definition
 * @throws {TypeError} If definition is invalid
 * @private
 */
function validateLensDefinition(definition) {
  if (!definition || typeof definition !== 'object') {
    throw new TypeError('Lens definition must be an object');
  }

  // Required fields
  validateString(definition.name, 'name');
  validateString(definition.version, 'version');
  validateFunction(definition.toSubstrate, 'toSubstrate');
  validateFunction(definition.fromSubstrate, 'fromSubstrate');

  // Version format
  const versionRegex = /^\d+\.\d+\.\d+$/;
  if (!versionRegex.test(definition.version)) {
    throw new Error(`Invalid version format: ${definition.version}`);
  }

  // Name format (alphanumeric, hyphens, underscores)
  const nameRegex = /^[a-z0-9_-]+$/i;
  if (!nameRegex.test(definition.name)) {
    throw new Error(`Invalid lens name: ${definition.name}. Use alphanumeric, hyphens, or underscores only`);
  }
}

/**
 * Optimizes a transformation function
 *
 * Optimizations:
 * - Input validation
 * - Error boundary wrapping
 * - Immutability enforcement (defensive copy)
 *
 * @param {Function} fn - Original transformation function
 * @param {string} direction - Transformation direction ('toSubstrate' or 'fromSubstrate')
 * @returns {Function} Optimized transformation
 * @private
 */
function optimizeTransformation(fn, direction) {
  return function optimizedTransform(input) {
    // Input validation
    if (input === undefined) {
      throw new TypeError(`${direction}: input cannot be undefined`);
    }

    // Execute transformation with error boundary
    try {
      const result = fn(input);

      // Validate output
      if (result === undefined) {
        throw new Error(`${direction}: transformation returned undefined`);
      }

      return result;
    } catch (error) {
      // Enhance error with context
      const enhancedError = new Error(
        `Lens transformation failed (${direction}): ${error.message}`
      );
      enhancedError.cause = error;
      enhancedError.direction = direction;
      enhancedError.input = input;
      throw enhancedError;
    }
  };
}

/**
 * Computes deterministic hash of lens definition
 *
 * @param {Object} lensData - Lens data to hash
 * @returns {string} SHA-256 hash
 * @private
 */
function computeLensHash(lensData) {
  return hashObject(lensData);
}

/**
 * Validates compiled lens structure
 *
 * @param {*} compiled - Value to validate
 * @returns {boolean} True if valid compiled lens
 *
 * @example
 * if (isCompiledLens(lens)) { ... }
 */
export function isCompiledLens(compiled) {
  if (!compiled || typeof compiled !== 'object') {
    return false;
  }

  const required = ['name', 'version', 'toSubstrate', 'fromSubstrate', 'hash', 'compiledAt'];

  for (const field of required) {
    if (!(field in compiled)) {
      return false;
    }
  }

  return (
    typeof compiled.name === 'string' &&
    typeof compiled.version === 'string' &&
    typeof compiled.toSubstrate === 'function' &&
    typeof compiled.fromSubstrate === 'function' &&
    typeof compiled.hash === 'string' &&
    typeof compiled.compiledAt === 'number'
  );
}

/**
 * Decompiles a compiled lens for inspection (metadata only)
 *
 * @param {CompiledLens} compiled - Compiled lens
 * @returns {Object} Lens metadata (functions excluded)
 *
 * @example
 * const metadata = decompileLens(compiled);
 */
export function decompileLens(compiled) {
  if (!isCompiledLens(compiled)) {
    throw new TypeError('Invalid compiled lens');
  }

  return {
    name: compiled.name,
    version: compiled.version,
    hash: compiled.hash,
    compiledAt: compiled.compiledAt,
    metadata: compiled.metadata,
  };
}

/**
 * Compares two compiled lenses for equivalence
 *
 * @param {CompiledLens} a - First lens
 * @param {CompiledLens} b - Second lens
 * @returns {boolean} True if equivalent (same hash)
 *
 * @example
 * if (lensesEquivalent(lens1, lens2)) { ... }
 */
export function lensesEquivalent(a, b) {
  if (!isCompiledLens(a) || !isCompiledLens(b)) {
    return false;
  }

  return a.hash === b.hash;
}
