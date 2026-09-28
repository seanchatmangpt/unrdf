/**
 * @fileoverview ShEx grammar parser and complexity estimator
 * @module @unrdf/v6-core/grammar/parser/shex
 * @version 6.0.0-alpha.1
 */

import { GRAMMAR_TYPES } from '../parser.mjs';
import { countASTNodes } from './utils.mjs';

/**
 * Parse ShEx schema to AST
 * @param {string} input - ShEx schema
 * @returns {Object} Parse result
 */
export function parseShEx(input) {
  // Basic ShEx parsing - detects shapes, constraints
  // In production, use ShEx parser library

  const ast = {
    type: 'shex',
    shapes: extractShExShapes(input),
    constraints: extractShExConstraints(input),
  };

  const complexity = estimateShExComplexity(ast);
  ast.complexity = complexity; // Attach complexity to AST

  return {
    success: true,
    grammarType: GRAMMAR_TYPES.SHEX,
    ast,
    complexity,
  };
}

/**
 * Estimate ShEx validation complexity
 * @param {Object} ast - ShEx AST
 * @returns {Object} Complexity bounds
 */
export function estimateShExComplexity(ast) {
  const shapes = ast.shapes?.length || 0;
  const constraints = ast.constraints?.length || 0;

  const estimatedTimeMs = Math.min(
    300 + (shapes * 40) + (constraints * 20),
    8000 // 8s timeout for ShEx
  );

  return {
    estimatedTimeMs,
    astNodeCount: countASTNodes(ast),
    maxDepth: shapes,
  };
}

/**
 * Extract ShEx shapes
 * @param {string} input - ShEx schema
 * @returns {Array} Shapes
 */
function extractShExShapes(input) {
  const matches = input.match(/<\w+>/g); // Shape references
  return matches ? matches.map((_, i) => ({ id: `shape_${i}` })) : [];
}

/**
 * Extract ShEx constraints
 * @param {string} input - ShEx schema
 * @returns {Array} Constraints
 */
function extractShExConstraints(input) {
  const matches = input.match(/\[\s*\w+/g); // Triple constraints
  return matches ? matches.map((_, i) => ({ id: `constraint_${i}` })) : [];
}
