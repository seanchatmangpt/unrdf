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
 * v6 Grammar version
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
