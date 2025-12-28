/**
 * @fileoverview V6 Grammar Closure - Main Entry Point
 *
 * Unified grammar support with AOT complexity gating and denial receipts.
 *
 * @module @unrdf/v6-core/grammar
 * @version 6.0.0-alpha.1
 */

// Parser exports
export {
  parseGrammar,
  getComplexityBounds,
  GRAMMAR_TYPES,
} from './parser.mjs';

// Compiler exports
export {
  compileGrammar,
  rejectIfTooComplex,
  emitCompileReceipt,
  emitDenialReceipt,
  COMPLEXITY_BOUNDS,
} from './compiler.mjs';

// Runtime gate exports
export {
  checkRuntimeComplexity,
  wrapWithTimeout,
  executeWithGate,
  getRuntimeBounds,
  RUNTIME_BOUNDS,
} from './runtime-gate.mjs';

/**
 * v6 Grammar version identifier
 * @constant {string}
 */
export const GRAMMAR_VERSION = '6.0.0-alpha.1';

/**
 * Full grammar closure pipeline: parse → compile → execute
 *
 * @param {string} input - Grammar input (SPARQL/SHACL/N3/OWL/ShEx)
 * @param {string} grammarType - Grammar type
 * @param {Function} executeFn - Execution function (compiled) => result
 * @param {Object} store - RDF store
 * @param {Object} [options] - Options
 * @returns {Promise<Object>} Execution result with receipts
 *
 * @example
 * const result = await grammarClosurePipeline(
 *   sparqlQuery,
 *   'sparql',
 *   (compiled) => store.query(compiled.ast.queryString),
 *   myStore
 * );
 *
 * if (result.success) {
 *   console.log('Result:', result.result);
 * } else if (result.denialReceipt) {
 *   console.error('Denied:', result.denialReceipt);
 * }
 */
export async function grammarClosurePipeline(input, grammarType, executeFn, store, options = {}) {
  const { parseGrammar } = await import('./parser.mjs');
  const { compileGrammar } = await import('./compiler.mjs');
  const { executeWithGate } = await import('./runtime-gate.mjs');

  // Parse
  const parseResult = parseGrammar(input, grammarType);
  if (!parseResult.success) {
    return {
      success: false,
      phase: 'parse',
      errors: parseResult.errors,
      parseReceipt: parseResult.parseReceipt,
    };
  }

  // Compile (AOT gating)
  const compileResult = compileGrammar(parseResult.ast, options);
  if (!compileResult.success) {
    return {
      success: false,
      phase: 'compile',
      denialReceipt: compileResult.denialReceipt,
      compileReceipt: compileResult.compileReceipt,
    };
  }

  // Execute (runtime gating)
  const execResult = await executeWithGate(
    compileResult.compiled,
    executeFn,
    store,
    options
  );

  return {
    ...execResult,
    phase: 'execute',
    parseReceipt: parseResult.parseReceipt,
    compileReceipt: compileResult.compileReceipt,
  };
}

// =============================================================================
// Legacy API Compatibility Layer (v6-smoke tests)
// =============================================================================

/**
 * Legacy V6_GRAMMAR object for backward compatibility
 * @deprecated Use grammarClosurePipeline directly
 * @constant {Object}
 * @property {string} version - Grammar version
 * @property {string[]} types - Supported grammar types
 * @property {Function} pipeline - Grammar processing pipeline
 */
export const V6_GRAMMAR = {
  version: GRAMMAR_VERSION,
  definitions: {
    receipt: { type: 'object', required: ['id', 'type', 'timestamp'] },
    delta: { type: 'object', required: ['id', 'operations'] },
    operation: { type: 'object', required: ['op', 'path'] }
  },
  types: ['SPARQL', 'SHACL', 'N3', 'OWL', 'ShEx'],
  pipeline: grammarClosurePipeline,
};

/**
 * Get grammar definition for a specific grammar type
 * @deprecated Legacy compatibility function
 * @param {string} grammarType - Grammar type to query
 * @returns {Object} JSON Schema definition with type and required fields
 * @example
 * const def = getGrammarDefinition('receipt');
 * // { type: 'object', required: ['id', 'type', 'timestamp', 'payload'] }
 */
export function getGrammarDefinition(grammarType) {
  const definitions = {
    receipt: { type: 'object', required: ['id', 'type', 'timestamp', 'payload'] },
    delta: { type: 'object', required: ['id', 'operations', 'timestamp'] },
    operation: { type: 'object', required: ['op', 'path', 'value'] }
  };
  return definitions[grammarType] || { type: 'unknown', required: [] };
}

/**
 * Validate data against specified grammar type
 * @deprecated Legacy compatibility function
 * @param {string} grammarType - Grammar type for validation
 * @param {any} data - Data to validate
 * @returns {boolean} True if valid, false otherwise
 * @example
 * const isValid = validateAgainstGrammar('receipt', myData);
 * // true or false
 */
export function validateAgainstGrammar(grammarType, data) {
  const def = getGrammarDefinition(grammarType);
  if (!def || def.type === 'unknown') return false;

  // Check required fields exist
  if (def.required) {
    for (const field of def.required) {
      if (!(field in data)) return false;
    }
  }
  return true;
}
