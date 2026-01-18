/**
 * @file Daemon Knowledge Rules Inference Chain Building
 * @module @unrdf/daemon/integrations/knowledge-rules-inference
 * @description Inference chain construction logic for knowledge rules
 */

/**
 * Build inference chains between matched rules
 * @param {Array} matchedRules - Array of matched rule results
 * @param {Object} engine - Rule engine instance for rule lookup
 * @param {number} maxChainDepth - Maximum chain depth
 * @returns {Array} Inference chains
 */
export function buildInferenceChains(matchedRules, engine, maxChainDepth) {
  const chains = [];
  const visited = new Set();

  for (const rule of matchedRules) {
    if (visited.has(rule.ruleId)) continue;
    const chain = buildChainForRule(rule.ruleId, matchedRules, engine, visited, 0, maxChainDepth);
    if (chain.length > 0) chains.push(chain);
  }

  return chains;
}

/**
 * Build inference chain for a specific rule
 * @param {string} ruleId - Rule identifier
 * @param {Array} matchedRules - Matched rules
 * @param {Object} engine - Rule engine instance for rule lookup
 * @param {Set} visited - Visited rule IDs
 * @param {number} depth - Current chain depth
 * @param {number} maxDepth - Maximum chain depth
 * @returns {Array} Chain of rule IDs
 */
export function buildChainForRule(ruleId, matchedRules, engine, visited, depth, maxDepth) {
  if (depth > maxDepth || visited.has(ruleId)) {
    return [];
  }

  visited.add(ruleId);
  const rule = engine.getRule(ruleId);
  if (!rule) return [];

  const chain = [ruleId];

  // Find dependent rules that match
  for (const dependent of matchedRules) {
    if (rule.dependencies.includes(dependent.ruleId)) {
      const subChain = buildChainForRule(
        dependent.ruleId,
        matchedRules,
        engine,
        visited,
        depth + 1,
        maxDepth
      );
      if (subChain.length > 0) {
        chain.push(...subChain);
      }
    }
  }

  return chain;
}

/**
 * Get inference chain for evaluation
 * @param {string} ruleId - Rule identifier
 * @param {string} evaluationId - Evaluation identifier
 * @param {Array} executionHistory - Execution history array
 * @returns {Array} Inference chain
 */
export function getInferenceChain(ruleId, evaluationId, executionHistory) {
  // Find related rules from execution history
  const related = executionHistory
    .filter(e => e.evaluationId === evaluationId && e.matched)
    .map(e => e.ruleName);
  return related;
}
