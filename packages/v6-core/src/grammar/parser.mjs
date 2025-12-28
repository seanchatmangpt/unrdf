/**
 * @fileoverview Unified Grammar Parser - V6 Grammar Closure
 *
 * Parse any grammar (SPARQL/SHACL/N3/OWL/ShEx) to AST with complexity annotations.
 *
 * **Core Principle**: 100% parse acceptance - never crash on valid grammar syntax.
 *
 * @module @unrdf/v6-core/grammar/parser
 * @version 6.0.0-alpha.1
 */

import { z } from 'zod';
import { parseSPARQL, estimateSPARQLComplexity } from './parser/sparql.mjs';
import { parseSHACL, estimateSHACLComplexity } from './parser/shacl.mjs';
import { parseN3, estimateN3Complexity } from './parser/n3.mjs';
import { parseOWL, estimateOWLComplexity } from './parser/owl.mjs';
import { parseShEx, estimateShExComplexity } from './parser/shex.mjs';
import { createEmptyComplexity, createParseReceipt } from './parser/utils.mjs';

/**
 * Supported grammar types
 */
export const GRAMMAR_TYPES = {
  SPARQL: 'sparql',
  SHACL: 'shacl',
  N3: 'n3',
  OWL: 'owl',
  SHEX: 'shex',
};

/**
 * Grammar type schema
 */
const GrammarTypeSchema = z.enum([
  GRAMMAR_TYPES.SPARQL,
  GRAMMAR_TYPES.SHACL,
  GRAMMAR_TYPES.N3,
  GRAMMAR_TYPES.OWL,
  GRAMMAR_TYPES.SHEX,
]);

/**
 * Complexity bounds schema
 */
const ComplexityBoundsSchema = z.object({
  // Universal bounds
  estimatedTimeMs: z.number().positive(),
  astNodeCount: z.number().nonnegative(),
  maxDepth: z.number().positive(),

  // Grammar-specific bounds
  triplePatterns: z.number().nonnegative().optional(),
  joinDepth: z.number().nonnegative().optional(),
  filterComplexity: z.number().nonnegative().optional(),
  shapesDepth: z.number().nonnegative().optional(),
  ruleDepth: z.number().nonnegative().optional(),
});

/**
 * Parse result schema
 */
export const ParseResultSchema = z.object({
  success: z.boolean(),
  grammarType: GrammarTypeSchema,
  ast: z.any(), // Grammar-specific AST structure
  complexity: ComplexityBoundsSchema,
  parseReceipt: z.object({
    timestamp: z.string(),
    grammarVersion: z.string(),
    parser: z.string(),
  }),
  errors: z.array(z.object({
    message: z.string(),
    line: z.number().optional(),
    column: z.number().optional(),
  })).optional(),
});

/**
 * Parse any grammar to AST with complexity annotations
 *
 * **Guarantee**: Never throws on syntactically valid grammar.
 * Invalid syntax returns { success: false, errors: [...] }
 *
 * @param {string} input - Grammar input text
 * @param {string} grammarType - Grammar type (sparql|shacl|n3|owl|shex)
 * @returns {Object} Parse result with AST and complexity
 *
 * @example
 * const result = parseGrammar(sparqlQuery, 'sparql');
 * if (result.success) {
 *   console.log('Complexity:', result.complexity);
 * }
 */
export function parseGrammar(input, grammarType) {
  // Validate inputs
  if (typeof input !== 'string' || input.trim().length === 0) {
    return {
      success: false,
      grammarType: grammarType || 'unknown',
      ast: null,
      complexity: createEmptyComplexity(),
      parseReceipt: createParseReceipt(grammarType),
      errors: [{ message: 'Input must be a non-empty string' }],
    };
  }

  try {
    GrammarTypeSchema.parse(grammarType);
  } catch (error) {
    return {
      success: false,
      grammarType: grammarType || 'unknown',
      ast: null,
      complexity: createEmptyComplexity(),
      parseReceipt: createParseReceipt(grammarType),
      errors: [{ message: `Invalid grammar type: ${grammarType}` }],
    };
  }

  // Route to grammar-specific parser
  try {
    let parseResult;

    switch (grammarType) {
      case GRAMMAR_TYPES.SPARQL:
        parseResult = parseSPARQL(input);
        break;

      case GRAMMAR_TYPES.SHACL:
        parseResult = parseSHACL(input);
        break;

      case GRAMMAR_TYPES.N3:
        parseResult = parseN3(input);
        break;

      case GRAMMAR_TYPES.OWL:
        parseResult = parseOWL(input);
        break;

      case GRAMMAR_TYPES.SHEX:
        parseResult = parseShEx(input);
        break;

      default:
        throw new Error(`Unsupported grammar type: ${grammarType}`);
    }

    // Add parse receipt
    parseResult.parseReceipt = createParseReceipt(grammarType);

    return parseResult;
  } catch (error) {
    // Catch any unexpected errors (should not happen with valid grammar)
    return {
      success: false,
      grammarType,
      ast: null,
      complexity: createEmptyComplexity(),
      parseReceipt: createParseReceipt(grammarType),
      errors: [{ message: `Parse failed: ${error.message}` }],
    };
  }
}

/**
 * Get complexity bounds for parsed AST
 *
 * @param {Object} ast - Parsed AST from parseGrammar()
 * @returns {Object} Complexity bounds
 *
 * @example
 * const bounds = getComplexityBounds(ast);
 * console.log('Estimated time:', bounds.estimatedTimeMs);
 */
export function getComplexityBounds(ast) {
  if (!ast || !ast.type) {
    return createEmptyComplexity();
  }

  switch (ast.type) {
    case 'sparql':
      return estimateSPARQLComplexity(ast);
    case 'shacl':
      return estimateSHACLComplexity(ast);
    case 'n3':
      return estimateN3Complexity(ast);
    case 'owl':
      return estimateOWLComplexity(ast);
    case 'shex':
      return estimateShExComplexity(ast);
    default:
      return createEmptyComplexity();
  }
}

// Re-export grammar-specific parsers for advanced use
export {
  parseSPARQL,
  parseSHACL,
  parseN3,
  parseOWL,
  parseShEx,
  estimateSPARQLComplexity,
  estimateSHACLComplexity,
  estimateN3Complexity,
  estimateOWLComplexity,
  estimateShExComplexity,
};

// Re-export utilities
export { createEmptyComplexity, createParseReceipt } from './parser/utils.mjs';
