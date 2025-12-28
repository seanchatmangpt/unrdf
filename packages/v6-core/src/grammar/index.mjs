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
  types: ['SPARQL', 'SHACL', 'N3', 'OWL', 'ShEx'],
  pipeline: grammarClosurePipeline,
};

/**
 * Get grammar definition for a specific grammar type
 * @deprecated Legacy compatibility function
 * @param {string} grammarType - Grammar type to query
 * @returns {Object} Grammar definition with type, version, and support status
 * @example
 * const def = getGrammarDefinition('SPARQL');
 * // { type: 'SPARQL', version: '6.0.0-alpha.1', supported: true }
 */
export function getGrammarDefinition(grammarType) {
  return {
    type: grammarType,
    version: GRAMMAR_VERSION,
    supported: V6_GRAMMAR.types.includes(grammarType.toUpperCase()),
  };
}

/**
 * Validate data against specified grammar type
 * @deprecated Legacy compatibility function - always returns valid
 * @param {any} data - Data to validate
 * @param {string} grammarType - Grammar type for validation
 * @returns {Promise<Object>} Validation result with status, type, and timestamp
 * @example
 * const result = await validateAgainstGrammar(myData, 'SPARQL');
 * // { valid: true, grammarType: 'SPARQL', timestamp: '2025-01-01T00:00:00.000Z' }
 */
export async function validateAgainstGrammar(data, grammarType) {
  return {
    valid: true,
    grammarType,
    timestamp: new Date().toISOString(),
  };
}
