/**
 * @fileoverview OWL grammar parser and complexity estimator
 * @module @unrdf/v6-core/grammar/parser/owl
 * @version 6.0.0-alpha.1
 */

import { GRAMMAR_TYPES } from '../parser.mjs';
import { countASTNodes } from './utils.mjs';

/**
 * Parse OWL ontology to AST
 * @param {string} input - OWL (RDF/XML, Turtle, etc.)
 * @returns {Object} Parse result
 */
export function parseOWL(input) {
  // Basic OWL parsing - detects axioms, class hierarchy
  // In production, use OWL API or rdflib.js

  const ast = {
    type: 'owl',
    axioms: extractOWLAxioms(input),
    classes: extractOWLClasses(input),
    properties: extractOWLProperties(input),
  };

  const complexity = estimateOWLComplexity(ast);
  ast.complexity = complexity; // Attach complexity to AST

  return {
    success: true,
    grammarType: GRAMMAR_TYPES.OWL,
    ast,
    complexity,
  };
}

/**
 * Estimate OWL reasoning complexity
 * @param {Object} ast - OWL AST
 * @returns {Object} Complexity bounds
 */
export function estimateOWLComplexity(ast) {
  const axioms = ast.axioms?.length || 0;
  const classes = ast.classes?.length || 0;

  const estimatedTimeMs = Math.min(
    1000 + (axioms * 50) + (classes * 30),
    20000 // 20s timeout for OWL reasoning
  );

  return {
    estimatedTimeMs,
    astNodeCount: countASTNodes(ast),
    maxDepth: axioms,
  };
}

/**
 * Extract OWL axioms
 * @param {string} input - OWL Turtle
 * @returns {Array} Axioms
 */
function extractOWLAxioms(input) {
  const matches = input.match(/owl:(Class|ObjectProperty|DatatypeProperty)/g);
  return matches ? matches.map((_, i) => ({ id: `axiom_${i}` })) : [];
}

/**
 * Extract OWL classes
 * @param {string} input - OWL Turtle
 * @returns {Array} Classes
 */
function extractOWLClasses(input) {
  const matches = input.match(/owl:Class/g);
  return matches ? matches.map((_, i) => ({ id: `class_${i}` })) : [];
}

/**
 * Extract OWL properties
 * @param {string} input - OWL Turtle
 * @returns {Array} Properties
 */
function extractOWLProperties(input) {
  const matches = input.match(/owl:(ObjectProperty|DatatypeProperty)/g);
  return matches ? matches.map((_, i) => ({ id: `prop_${i}` })) : [];
}
