/**
 * @file Daemon Hooks Policy Helpers
 * @module @unrdf/daemon/integrations/hooks-policy-helpers
 * @description Policy evaluation helper functions
 */

/**
 * Evaluate approval policy
 *
 * @param {Object} policy - Policy configuration
 * @param {Object} context - Policy evaluation context
 * @returns {Promise<Object>} Evaluation result with decision and reason
 */
export async function evaluateApprovalPolicy(policy, _context) {
  if (!policy.config.requiresApproval) {
    return { decision: 'allow', reason: 'Approval not required' };
  }

  // In a real implementation, this would check actual approvals
  // For now, defer to external approval system
  return {
    decision: 'defer',
    reason: `Awaiting approval from: ${(policy.config.approvers || []).join(', ')}`,
  };
}

/**
 * Evaluate time-window policy
 *
 * @param {Object} policy - Policy configuration
 * @param {Object} context - Policy evaluation context
 * @returns {Promise<Object>} Evaluation result with decision and reason
 */
export async function evaluateTimeWindowPolicy(policy, _context) {
  const config = policy.config;
  if (!config.timeWindows || config.timeWindows.length === 0) {
    return { decision: 'allow', reason: 'No time windows configured' };
  }

  const now = new Date();
  const currentHour = now.getHours();
  const currentDay = now.getDay();

  for (const window of config.timeWindows) {
    const windowStart = parseInt(window.start, 10);
    const windowEnd = parseInt(window.end, 10);
    const allowedDays = window.days || [0, 1, 2, 3, 4, 5, 6];

    if (
      allowedDays.includes(currentDay) &&
      currentHour >= windowStart &&
      currentHour < windowEnd
    ) {
      return { decision: 'allow', reason: `Within allowed time window: ${window.start}-${window.end}` };
    }
  }

  return {
    decision: 'deny',
    reason: `Outside allowed time windows: ${config.timeWindows.map(w => `${w.start}-${w.end}`).join(', ')}`,
  };
}

/**
 * Evaluate resource-limit policy
 *
 * @param {Object} policy - Policy configuration
 * @param {Object} context - Policy evaluation context
 * @param {Map<string, number>} resourceUsage - Current resource usage map
 * @returns {Promise<Object>} Evaluation result with decision and reason
 */
export async function evaluateResourceLimitPolicy(policy, context, resourceUsage) {
  const config = policy.config;
  const currentUsage = resourceUsage.get(context.operationType) || 0;
  const maxAllowed = config.maxConcurrent || 10;

  if (currentUsage >= maxAllowed) {
    return {
      decision: 'defer',
      reason: `Resource limit exceeded: ${currentUsage}/${maxAllowed} concurrent operations`,
    };
  }

  return { decision: 'allow', reason: `Resource available: ${currentUsage}/${maxAllowed}` };
}

/**
 * Evaluate rate-limit policy
 *
 * @param {Object} policy - Policy configuration
 * @param {Object} context - Policy evaluation context
 * @param {Map<string, number>} operationRateLimit - Rate limit tracking map
 * @returns {Promise<Object>} Evaluation result with decision and reason
 */
export async function evaluateRateLimitPolicy(policy, context, operationRateLimit) {
  const config = policy.config;
  const maxPerMinute = config.maxPerMinute || 10;
  const key = context.operationType;

  const count = operationRateLimit.get(key) || 0;

  if (count >= maxPerMinute) {
    return {
      decision: 'defer',
      reason: `Rate limit exceeded: ${count}/${maxPerMinute} operations per minute`,
    };
  }

  operationRateLimit.set(key, count + 1);

  // Reset after a minute
  setTimeout(() => {
    operationRateLimit.set(key, 0);
  }, 60000);

  return { decision: 'allow', reason: `Rate limit OK: ${count + 1}/${maxPerMinute}` };
}

/**
 * Evaluate custom policy
 *
 * @param {Object} policy - Policy configuration
 * @param {Object} context - Policy evaluation context
 * @returns {Promise<Object>} Evaluation result with decision and reason
 */
export async function evaluateCustomPolicy(policy, context) {
  if (!policy.config.evaluatorFn || typeof policy.config.evaluatorFn !== 'function') {
    return { decision: 'allow', reason: 'No custom evaluator function provided' };
  }

  try {
    const result = await policy.config.evaluatorFn(context);
    return result;
  } catch (error) {
    return {
      decision: 'defer',
      reason: `Custom evaluator error: ${error.message}`,
    };
  }
}

/**
 * Resolve conflicts between policy decisions
 *
 * @param {string} operationId - Operation ID
 * @param {Array} evaluations - Array of policy evaluations
 * @param {string} conflictStrategy - Conflict resolution strategy
 * @param {Function} recordDecision - Function to record decision
 * @returns {Object} Resolved decision
 */
export function resolveConflicts(operationId, evaluations, conflictStrategy, recordDecision) {
  const decisions = evaluations.map(e => e.decision);
  const policiesEvaluated = evaluations.map(e => e.policyId);

  // Any deny is always respected
  if (decisions.includes('deny')) {
    const denyEval = evaluations.find(e => e.decision === 'deny');
    return recordDecision(operationId, 'deny', denyEval.reason, policiesEvaluated, true);
  }

  // Handle different conflict resolution strategies
  if (conflictStrategy === 'unanimous') {
    // All must allow, any defer/deny blocks
    if (decisions.includes('defer')) {
      const deferEval = evaluations.find(e => e.decision === 'defer');
      return recordDecision(operationId, 'defer', deferEval.reason, policiesEvaluated, true);
    }
    return recordDecision(operationId, 'allow', 'Unanimous approval', policiesEvaluated, false);
  }

  if (conflictStrategy === 'highest-priority') {
    // Use the first policy's decision (already sorted by priority)
    const firstEval = evaluations[0];
    const conflictExists = evaluations.length > 1;
    return recordDecision(
      operationId,
      firstEval.decision,
      firstEval.reason,
      policiesEvaluated,
      conflictExists
    );
  }

  if (conflictStrategy === 'majority') {
    const allows = decisions.filter(d => d === 'allow').length;
    const defers = decisions.filter(d => d === 'defer').length;
    const denies = decisions.filter(d => d === 'deny').length;

    if (allows > defers && allows > denies) {
      return recordDecision(operationId, 'allow', 'Majority approval', policiesEvaluated, true);
    }
    if (defers >= allows && defers >= denies) {
      const deferEval = evaluations.find(e => e.decision === 'defer');
      return recordDecision(operationId, 'defer', deferEval.reason, policiesEvaluated, true);
    }
    return recordDecision(operationId, 'deny', 'Majority denial', policiesEvaluated, true);
  }

  if (conflictStrategy === 'first-match') {
    const firstEval = evaluations[0];
    return recordDecision(operationId, firstEval.decision, firstEval.reason, policiesEvaluated, false);
  }

  // Default: allow
  return recordDecision(operationId, 'allow', 'Default allow decision', policiesEvaluated, false);
}
