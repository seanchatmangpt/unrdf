/**
 * @file Daemon Knowledge Rules Condition Evaluators
 * @module @unrdf/daemon/integrations/knowledge-rules-evaluators
 * @description Condition evaluation logic for knowledge rules
 */

import {
  metadataToBindings,
  matchPatternBindings,
} from './knowledge-rules-utils.mjs';

/**
 * Evaluate a condition (SPARQL pattern or composite)
 * @param {object} condition - Condition definition
 * @param {object} operationMetadata - Operation metadata
 * @param {object} context - Evaluation context
 * @returns {Promise<object>} Condition evaluation result
 * @example
 */
export async function evaluateCondition(condition, operationMetadata, context) {
  if (condition.type === 'sparql') {
    return evaluateSparqlPattern(condition, operationMetadata, context);
  }

  if (condition.type === 'business-logic') {
    return evaluateBusinessLogic(condition, operationMetadata);
  }

  if (condition.type === 'composite') {
    return evaluateCompositeCondition(condition, operationMetadata, context);
  }

  return { matched: false, confidence: 0 };
}

/**
 * Evaluate SPARQL pattern condition
 * @param {object} pattern - SPARQL pattern definition
 * @param {object} operationMetadata - Operation metadata
 * @param {object} _context - Evaluation context
 * @returns {Promise<object>} Pattern evaluation result
 * @example
 */
export async function evaluateSparqlPattern(pattern, operationMetadata, _context) {
  try {
    // Prepare SPARQL bindings with operation metadata
    const bindings = {
      ...pattern.bindings,
      ...metadataToBindings(operationMetadata),
    };

    // In a real implementation, this would execute SPARQL against RDF store
    const matched = matchPatternBindings(pattern.query, bindings);

    return {
      matched,
      confidence: matched ? 0.9 : 0.1,
      patterns: [pattern.query],
    };
  } catch (error) {
    return {
      matched: false,
      confidence: 0,
      patterns: [],
      error: error.message,
    };
  }
}

/**
 * Evaluate business logic condition
 * @param {object} condition - Business logic condition
 * @param {object} operationMetadata - Operation metadata
 * @returns {object} Condition evaluation result
 * @example
 */
export function evaluateBusinessLogic(condition, operationMetadata) {
  try {
    const result = condition.evaluator(operationMetadata);
    return {
      matched: Boolean(result),
      confidence: result === true ? 1 : 0,
      description: condition.description,
    };
  } catch (error) {
    return {
      matched: false,
      confidence: 0,
      error: error.message,
    };
  }
}

/**
 * Evaluate composite condition (AND/OR/NOT operators)
 * @param {object} condition - Composite condition
 * @param {object} operationMetadata - Operation metadata
 * @param {object} context - Evaluation context
 * @returns {Promise<object>} Composite condition result
 * @example
 */
export async function evaluateCompositeCondition(condition, operationMetadata, context) {
  const results = await Promise.all(
    condition.conditions.map(c => evaluateCondition(c, operationMetadata, context))
  );

  switch (condition.operator) {
    case 'and':
      return {
        matched: results.every(r => r.matched),
        confidence: results.length > 0
          ? results.reduce((sum, r) => sum + r.confidence, 0) / results.length
          : 0,
      };

    case 'or':
      return {
        matched: results.some(r => r.matched),
        confidence: Math.max(...results.map(r => r.confidence), 0),
      };

    case 'not':
      return {
        matched: !results[0]?.matched,
        confidence: 1 - (results[0]?.confidence || 0),
      };

    default:
      return { matched: false, confidence: 0 };
  }
}
