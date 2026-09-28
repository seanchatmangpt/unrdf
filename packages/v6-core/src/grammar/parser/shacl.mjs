/**
 * @fileoverview SHACL grammar parser and complexity estimator
 * @module @unrdf/v6-core/grammar/parser/shacl
 * @version 6.0.0-alpha.1
 */

import { GRAMMAR_TYPES } from '../parser.mjs';
import { countASTNodes } from './utils.mjs';

/**
 * Parse SHACL shapes to AST
 * @param {string} input - SHACL shapes (Turtle/JSON-LD)
 * @returns {Object} Parse result
 */
export function parseSHACL(input) {
  // Basic SHACL parsing - detects shapes, constraints
  // In production, parse Turtle/JSON-LD with SHACL semantics

  const ast = {
    type: 'shacl',
    shapes: extractSHACLShapes(input),
    targets: extractSHACLTargets(input),
    properties: extractSHACLProperties(input),
  };

  const complexity = estimateSHACLComplexity(ast);
  ast.complexity = complexity; // Attach complexity to AST

  return {
    success: true,
    grammarType: GRAMMAR_TYPES.SHACL,
    ast,
    complexity,
  };
}

/**
 * Estimate SHACL validation complexity
 * @param {Object} ast - SHACL AST
 * @returns {Object} Complexity bounds
 */
export function estimateSHACLComplexity(ast) {
  const shapes = ast.shapes?.length || 0;
  const properties = ast.properties?.length || 0;

  // SHACL is O(nodes * shapes)
  const estimatedTimeMs = Math.min(
    200 + (shapes * 20) + (properties * 30),
    10000 // 10s timeout for validation
  );

  return {
    estimatedTimeMs,
    astNodeCount: countASTNodes(ast),
    maxDepth: shapes,
    shapesDepth: shapes,
  };
}

/**
 * Extract SHACL shapes (simplified)
 * @param {string} input - SHACL Turtle
 * @returns {Array} Shapes
 */
function extractSHACLShapes(input) {
  // Simplified - count sh:NodeShape occurrences
  const matches = input.match(/sh:NodeShape/g);
  return matches ? matches.map((_, i) => ({ id: `shape_${i}` })) : [];
}

/**
 * Extract SHACL targets
 * @param {string} input - SHACL Turtle
 * @returns {Array} Targets
 */
function extractSHACLTargets(input) {
  const matches = input.match(/sh:targetClass/g);
  return matches ? matches.map((_, i) => ({ id: `target_${i}` })) : [];
}

/**
 * Extract SHACL properties
 * @param {string} input - SHACL Turtle
 * @returns {Array} Properties
 */
function extractSHACLProperties(input) {
  const matches = input.match(/sh:property/g);
  return matches ? matches.map((_, i) => ({ id: `prop_${i}` })) : [];
}
