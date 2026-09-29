/**
 * @fileoverview N3 grammar parser and complexity estimator
 * @module @unrdf/v6-core/grammar/parser/n3
 * @version 6.0.0-alpha.1
 */

import { GRAMMAR_TYPES } from '../parser.mjs';
import { countASTNodes } from './utils.mjs';

/**
 * Parse N3 logic to AST
 * @param {string} input - N3 notation
 * @returns {Object} Parse result
 */
export function parseN3(input) {
  // Basic N3 parsing - detects rules, formulas
  // In production, use full N3 parser

  const ast = {
    type: 'n3',
    rules: extractN3Rules(input),
    formulas: extractN3Formulas(input),
    builtins: extractN3Builtins(input),
  };

  const complexity = estimateN3Complexity(ast);
  ast.complexity = complexity; // Attach complexity to AST

  return {
    success: true,
    grammarType: GRAMMAR_TYPES.N3,
    ast,
    complexity,
  };
}

/**
 * Estimate N3 reasoning complexity
 * @param {Object} ast - N3 AST
 * @returns {Object} Complexity bounds
 */
export function estimateN3Complexity(ast) {
  const rules = ast.rules?.length || 0;
  const formulas = ast.formulas?.length || 0;

  // N3 reasoning can be expensive
  const estimatedTimeMs = Math.min(
    500 + (rules * 100) + (formulas * 200),
    15000 // 15s timeout for reasoning
  );

  return {
    estimatedTimeMs,
    astNodeCount: countASTNodes(ast),
    maxDepth: rules + formulas,
    ruleDepth: rules,
  };
}

/**
 * Extract N3 rules
 * @param {string} input - N3 notation
 * @returns {Array} Rules
 */
function extractN3Rules(input) {
  const matches = input.match(/=>/g); // Implication operator
  return matches ? matches.map((_, i) => ({ id: `rule_${i}` })) : [];
}

/**
 * Extract N3 formulas
 * @param {string} input - N3 notation
 * @returns {Array} Formulas
 */
function extractN3Formulas(input) {
  const matches = input.match(/\{[^}]*\}/g); // Nested formulas
  return matches ? matches.map((_, i) => ({ id: `formula_${i}` })) : [];
}

/**
 * Extract N3 builtins
 * @param {string} input - N3 notation
 * @returns {Array} Builtins
 */
function extractN3Builtins(input) {
  const matches = input.match(/(?:math|string|log):\w+/g);
  return matches || [];
}
