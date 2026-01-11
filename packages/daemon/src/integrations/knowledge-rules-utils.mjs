/**
 * @file Daemon Knowledge Rules Utilities
 * @module @unrdf/daemon/integrations/knowledge-rules-utils
 * @description Internal utilities for the knowledge rule engine
 */

/**
 * Convert metadata to SPARQL bindings
 * @param {Object} metadata - Operation metadata
 * @returns {Object} SPARQL bindings
 */
export function metadataToBindings(metadata) {
  const bindings = {};
  for (const [key, value] of Object.entries(metadata)) {
    bindings[`?${key}`] = value;
  }
  return bindings;
}

/**
 * Match pattern bindings (simple substring matching)
 * @param {string} query - SPARQL query pattern
 * @param {Object} bindings - Bindings to match
 * @returns {boolean} Whether pattern matches
 */
export function matchPatternBindings(query, bindings) {
  for (const [var_] of Object.entries(bindings)) {
    if (query.includes(var_)) {
      return true;
    }
  }
  return false;
}

/**
 * Calculate confidence score for rule match
 * @param {Object} conditionResult - Condition evaluation result
 * @param {Object} rule - Rule definition
 * @param {Object} operationMetadata - Operation metadata
 * @returns {number} Confidence score [0-1]
 */
export function calculateConfidence(conditionResult, rule, operationMetadata) {
  const baseConfidence = conditionResult.confidence || 0;

  const depencyBoost = rule.dependencies.length > 0
    ? rule.dependencies.every(dep => dep in operationMetadata) ? 0.1 : -0.1
    : 0;

  return Math.max(0, Math.min(1, baseConfidence + depencyBoost));
}

/**
 * Generate explanation for rule evaluation
 * @param {Object} rule - Rule definition
 * @param {Object} conditionResult - Condition evaluation result
 * @param {number} confidence - Confidence score
 * @returns {Object} Explanation object
 */
export function generateExplanation(rule, conditionResult, confidence) {
  return {
    reason: confidence >= rule.minConfidence
      ? `Rule "${rule.name}" matched with ${(confidence * 100).toFixed(1)}% confidence`
      : `Rule "${rule.name}" did not meet minimum confidence threshold (${confidence.toFixed(2)} < ${rule.minConfidence.toFixed(2)})`,
    matchedPatterns: conditionResult.patterns || [],
    failedConditions: conditionResult.error ? [conditionResult.error] : [],
  };
}

/**
 * Check if two rules conflict
 * @param {Object} rule1 - First rule
 * @param {Object} rule2 - Second rule
 * @returns {Object|null} Conflict object or null if no conflict
 */
export function checkRuleConflict(rule1, rule2) {
  if (rule1.action.type !== rule2.action.type) {
    return null;
  }

  if (rule1.action.priority !== rule2.action.priority) {
    return {
      rule1Id: rule1.id,
      rule1Name: rule1.name,
      rule2Id: rule2.id,
      rule2Name: rule2.name,
      reason: `Different priority for same action type: ${rule1.action.priority} vs ${rule2.action.priority}`,
      severity: 'warning',
    };
  }

  return null;
}
