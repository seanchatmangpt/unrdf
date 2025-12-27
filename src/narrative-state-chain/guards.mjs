/**
 * @fileoverview Guard Enforcement - Authorization and admissibility checks
 *
 * **Purpose**: Enforce guards (authorization predicates) before scenes are admitted
 * - evaluate_guard(guard, agent, action, target) → {passed, reason, proof}
 * - apply_all_guards(universe, agent, scene_delta) → {admissible, failures}
 *
 * **Design**:
 * - Guards are async predicates (can call external services)
 * - Compose guards as middleware (all must pass)
 * - Zod schemas for guard conditions
 *
 * @module narrative-state-chain/guards
 */

import { blake3 } from 'hash-wasm';

/**
 * Evaluate a single guard
 *
 * **Process**:
 * 1. Call guard.condition({ agent, action, target })
 * 2. Generate proof hash
 * 3. Return result with timestamp
 *
 * @param {import('./types.mjs').Guard} guard - Guard to evaluate
 * @param {string} agent - Agent performing action
 * @param {string} action - Action being performed
 * @param {any} target - Target of action
 * @returns {Promise<import('./types.mjs').GuardResult>} Guard evaluation result
 *
 * @example
 * const result = await evaluateGuard(
 *   myGuard,
 *   'user@example.com',
 *   'write',
 *   { resource: 'document123' }
 * );
 * if (!result.passed) {
 *   console.error('Guard failed:', result.reason);
 * }
 */
export async function evaluateGuard(guard, agent, action, target) {
  const timestamp = new Date();

  try {
    const passed = await guard.condition({ agent, action, target });

    // Generate proof hash
    const proofData = JSON.stringify({
      guardId: guard.id,
      agent,
      action,
      target,
      passed,
      timestamp: timestamp.toISOString(),
    });

    const proof = await blake3(proofData);

    return {
      guardId: guard.id,
      passed,
      reason: passed ? undefined : `Guard ${guard.name} denied access`,
      proof,
      timestamp,
    };
  } catch (error) {
    // Guard evaluation error = deny by default
    const proofData = JSON.stringify({
      guardId: guard.id,
      agent,
      action,
      error: error.message,
      timestamp: timestamp.toISOString(),
    });

    const proof = await blake3(proofData);

    return {
      guardId: guard.id,
      passed: false,
      reason: `Guard error: ${error.message}`,
      proof,
      timestamp,
    };
  }
}

/**
 * Evaluate all guards for a universe
 *
 * **Policy**: ALL guards must pass (AND composition)
 *
 * @param {import('./types.mjs').Universe} universe - Universe definition
 * @param {string} agent - Agent performing action
 * @param {Object} context - Action context
 * @param {any[]} context.observations - Observations being submitted
 * @param {Object} context.delta - State delta being applied
 * @returns {Promise<import('./types.mjs').GuardResult[]>} Array of guard results
 *
 * @example
 * const results = await evaluateAllGuards(universe, 'user@example.com', {
 *   observations: [quad1, quad2],
 *   delta: { property: 'newValue' }
 * });
 *
 * const admissible = results.every(r => r.passed);
 */
export async function evaluateAllGuards(universe, agent, context) {
  const results = [];

  for (const guard of universe.guards) {
    const result = await evaluateGuard(
      guard,
      agent,
      'scene_submission',
      context
    );
    results.push(result);
  }

  return results;
}

/**
 * Check if all guard results passed
 *
 * @param {import('./types.mjs').GuardResult[]} guardResults - Guard evaluation results
 * @returns {{admissible: boolean, failures: import('./types.mjs').GuardResult[]}} Admissibility check
 *
 * @example
 * const check = checkAdmissibility(guardResults);
 * if (!check.admissible) {
 *   console.error('Failed guards:', check.failures.map(f => f.guardId));
 * }
 */
export function checkAdmissibility(guardResults) {
  const failures = guardResults.filter(r => !r.passed);

  return {
    admissible: failures.length === 0,
    failures,
  };
}

/**
 * Create a simple allow-all guard (for testing)
 *
 * @param {string} [id] - Guard ID
 * @param {string} [name] - Guard name
 * @returns {import('./types.mjs').Guard} Allow-all guard
 *
 * @example
 * const allowGuard = createAllowAllGuard('test-guard', 'Test Allow');
 * const universe = await store.create({
 *   schema: 'http://example.org/schema#',
 *   reconcile: identityReconcile,
 *   guards: [allowGuard],
 *   metadata: { name: 'TestUniverse' }
 * });
 */
export function createAllowAllGuard(id = 'allow-all', name = 'Allow All') {
  return {
    id,
    name,
    description: 'Allows all actions (testing only)',
    condition: async () => true,
  };
}

/**
 * Create a simple deny-all guard (for testing)
 *
 * @param {string} [id] - Guard ID
 * @param {string} [name] - Guard name
 * @returns {import('./types.mjs').Guard} Deny-all guard
 *
 * @example
 * const denyGuard = createDenyAllGuard('test-deny', 'Test Deny');
 */
export function createDenyAllGuard(id = 'deny-all', name = 'Deny All') {
  return {
    id,
    name,
    description: 'Denies all actions (testing only)',
    condition: async () => false,
  };
}

/**
 * Create an agent whitelist guard
 *
 * **Use case**: Only allow specific agents to submit scenes
 *
 * @param {string[]} allowedAgents - Array of allowed agent identifiers
 * @param {string} [id] - Guard ID
 * @param {string} [name] - Guard name
 * @returns {import('./types.mjs').Guard} Whitelist guard
 *
 * @example
 * const whitelistGuard = createAgentWhitelistGuard(
 *   ['alice@example.com', 'bob@example.com'],
 *   'whitelist-guard',
 *   'Agent Whitelist'
 * );
 */
export function createAgentWhitelistGuard(
  allowedAgents,
  id = 'agent-whitelist',
  name = 'Agent Whitelist'
) {
  return {
    id,
    name,
    description: `Only allows agents: ${allowedAgents.join(', ')}`,
    condition: async ({ agent }) => allowedAgents.includes(agent),
  };
}

/**
 * Create a rate limit guard (simple in-memory)
 *
 * **Use case**: Limit actions per agent per time window
 *
 * @param {number} maxActions - Maximum actions allowed
 * @param {number} windowMs - Time window in milliseconds
 * @param {string} [id] - Guard ID
 * @param {string} [name] - Guard name
 * @returns {import('./types.mjs').Guard} Rate limit guard
 *
 * @example
 * const rateLimitGuard = createRateLimitGuard(
 *   10, // max 10 actions
 *   60000, // per minute
 *   'rate-limit',
 *   'Rate Limit'
 * );
 */
export function createRateLimitGuard(
  maxActions,
  windowMs,
  id = 'rate-limit',
  name = 'Rate Limit'
) {
  const actionLog = new Map(); // agent -> timestamp[]

  return {
    id,
    name,
    description: `Allows ${maxActions} actions per ${windowMs}ms`,
    condition: async ({ agent }) => {
      const now = Date.now();
      const agentLog = actionLog.get(agent) || [];

      // Remove old entries outside window
      const recentActions = agentLog.filter(ts => now - ts < windowMs);

      if (recentActions.length >= maxActions) {
        return false; // Rate limit exceeded
      }

      // Record this action
      recentActions.push(now);
      actionLog.set(agent, recentActions);

      return true;
    },
  };
}

/**
 * Compose multiple guards into a single guard (AND composition)
 *
 * **Use case**: Combine multiple guard conditions
 *
 * @param {import('./types.mjs').Guard[]} guards - Guards to compose
 * @param {string} [id] - Composite guard ID
 * @param {string} [name] - Composite guard name
 * @returns {import('./types.mjs').Guard} Composite guard
 *
 * @example
 * const compositeGuard = composeGuards(
 *   [whitelistGuard, rateLimitGuard],
 *   'composite',
 *   'Whitelist AND Rate Limit'
 * );
 */
export function composeGuards(guards, id = 'composite', name = 'Composite Guard') {
  return {
    id,
    name,
    description: `Composed of: ${guards.map(g => g.name).join(', ')}`,
    condition: async (context) => {
      for (const guard of guards) {
        const passed = await guard.condition(context);
        if (!passed) {
          return false; // Short-circuit on first failure
        }
      }
      return true; // All passed
    },
  };
}
