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
 * @param {Object} condition - Condition definition
 * @param {Object} operationMetadata - Operation metadata
 * @param {Object} context - Evaluation context
 * @returns {Promise<Object>} Condition evaluation result
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
 * @param {Object} pattern - SPARQL pattern definition
 * @param {Object} operationMetadata - Operation metadata
 * @param {Object} _context - Evaluation context
 * @returns {Promise<Object>} Pattern evaluation result
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
 * @param {Object} condition - Business logic condition
 * @param {Object} operationMetadata - Operation metadata
 * @returns {Object} Condition evaluation result
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
 * @param {Object} condition - Composite condition
 * @param {Object} operationMetadata - Operation metadata
 * @param {Object} context - Evaluation context
 * @returns {Promise<Object>} Composite condition result
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
