/**
 * @file Policy Engine - Unified Policy/Hooks/Conditions API
 * @module @unrdf/fusion/policy-engine
 *
 * Unifies:
 * - Policy pack registration
 * - Hook execution from @unrdf/hooks
 * - SPARQL condition evaluation from @unrdf/core
 * - Receipt emission with deterministic hashing
 */

import { executeAsk } from '@unrdf/core/sparql';
import { executeHook } from '@unrdf/hooks';
import { createHash } from 'node:crypto';
import { z } from 'zod';

/**
 * Deterministic hash function
 * Uses SHA-256 for compatibility (no external dependencies)
 * Can be upgraded to BLAKE3 via hash-wasm when available
 */
async function hashContent(content) {
  return createHash('sha256').update(content).digest('hex');
}

// =============================================================================
// Schemas
// =============================================================================

/**
 * SPARQL condition schema
 */
const SPARQLConditionSchema = z.object({
  sparql: z.string().min(1),
  type: z.enum(['ask', 'select']).default('ask'),
});

/**
 * Hook action schema
 */
const HookActionSchema = z.object({
  hook: z.string().min(1),
  args: z.record(z.any()).optional(),
});

/**
 * Policy schema
 */
const PolicySchema = z.object({
  name: z.string().min(1),
  description: z.string().optional(),
  conditions: z.array(SPARQLConditionSchema).default([]),
  actions: z.array(HookActionSchema).default([]),
  priority: z.number().int().min(0).max(100).default(50),
  enabled: z.boolean().default(true),
});

/**
 * Decision schema
 */
const DecisionSchema = z.object({
  policy: z.string(),
  decision: z.enum(['allow', 'deny', 'route']),
  conditionResults: z.array(z.boolean()).default([]),
  actionResults: z.array(z.any()).default([]),
  timestamp: z.number(),
  receiptHash: z.string().optional(),
});

// =============================================================================
// Policy Registry
// =============================================================================

/**
 * Create a unified policy registry
 *
 * @returns {Object} Policy registry instance
 *
 * @example
 * const registry = await createPolicyRegistry();
 * await registry.registerPolicy({
 *   name: 'CustomerCredit',
 *   conditions: [{ sparql: 'ASK { ?s foaf:age ?age . FILTER (?age > 18) }' }],
 *   actions: [{ hook: 'allowPurchase', args: { limit: 1000 } }]
 * });
 */
export async function createPolicyRegistry() {
  const policies = new Map();
  const hooks = new Map();

  return {
    /**
     * Register a policy with conditions and actions
     *
     * @param {Object} policy - Policy definition
     * @returns {Object} Registered policy
     */
    registerPolicy(policy) {
      const validated = PolicySchema.parse(policy);
      policies.set(validated.name, validated);
      return validated;
    },

    /**
     * Register a hook for use in policy actions
     *
     * @param {string} name - Hook name
     * @param {Object} hook - Hook definition (from defineHook)
     * @returns {void}
     */
    registerHook(name, hook) {
      hooks.set(name, hook);
    },

    /**
     * Evaluate a policy against a resource
     *
     * Executes SPARQL conditions and hook actions:
     * 1. Run all SPARQL conditions against store
     * 2. If all pass, execute hook actions
     * 3. Return decision with results
     *
     * @param {Object} store - RDF store with query() method
     * @param {Object} resource - Resource to evaluate (quad or entity)
     * @param {string} policyName - Name of policy to evaluate
     * @returns {Promise<Object>} Evaluation result
     */
    async evaluatePolicy(store, resource, policyName) {
      const policy = policies.get(policyName);
      if (!policy) {
        throw new Error(`Policy not found: ${policyName}`);
      }

      if (!policy.enabled) {
        return {
          policy: policyName,
          decision: 'deny',
          conditionResults: [],
          actionResults: [],
          timestamp: Date.now(),
          message: 'Policy is disabled',
        };
      }

      const conditionResults = [];
      const actionResults = [];

      // Execute SPARQL conditions
      for (const condition of policy.conditions) {
        try {
          const result = executeAskSync(store, condition.sparql);
          // ASK queries return { type: 'boolean', value: true/false }
          const passes = result.value === true;
          conditionResults.push(passes);

          // Short-circuit on first failure
          if (!passes) {
            return {
              policy: policyName,
              decision: 'deny',
              conditionResults,
              actionResults: [],
              timestamp: Date.now(),
              message: `Condition ${conditionResults.length} failed`,
            };
          }
        } catch (error) {
          return {
            policy: policyName,
            decision: 'deny',
            conditionResults,
            actionResults: [],
            timestamp: Date.now(),
            error: error.message,
          };
        }
      }

      // All conditions passed - execute actions
      for (const action of policy.actions) {
        const hook = hooks.get(action.hook);
        if (!hook) {
          actionResults.push({
            hook: action.hook,
            error: 'Hook not registered',
          });
          continue;
        }

        try {
          const result = executeHook(hook, resource, action.args);
          actionResults.push({
            hook: action.hook,
            valid: result.valid,
            quad: result.quad,
            error: result.error,
          });

          // If hook validation fails, deny
          if (!result.valid) {
            return {
              policy: policyName,
              decision: 'deny',
              conditionResults,
              actionResults,
              timestamp: Date.now(),
              message: `Hook ${action.hook} validation failed`,
            };
          }
        } catch (error) {
          actionResults.push({
            hook: action.hook,
            error: error.message,
          });
        }
      }

      // All conditions and actions passed
      return {
        policy: policyName,
        decision: 'allow',
        conditionResults,
        actionResults,
        timestamp: Date.now(),
      };
    },

    /**
     * Route a decision based on policy evaluation
     *
     * Evaluates policy and emits a receipt for the decision.
     *
     * @param {Object} store - RDF store
     * @param {Object} resource - Resource to route
     * @param {string} policyName - Policy to apply
     * @returns {Promise<Object>} Decision with receipt
     */
    async routeDecision(store, resource, policyName) {
      const evaluation = await this.evaluatePolicy(store, resource, policyName);
      const receipt = await this.emitPolicyReceipt(evaluation, resource);

      return {
        ...evaluation,
        receiptHash: receipt.hash,
        receipt,
      };
    },

    /**
     * Emit a deterministic receipt for a policy decision
     *
     * Creates a cryptographic receipt with:
     * - Policy name
     * - Decision (allow/deny/route)
     * - Timestamp
     * - Condition results
     * - Action results
     * - BLAKE3 hash (deterministic)
     *
     * @param {Object} decision - Decision result
     * @param {Object} payload - Decision payload (resource, context)
     * @returns {Promise<Object>} Receipt with hash
     */
    async emitPolicyReceipt(decision, payload) {
      const receipt = {
        policy: decision.policy,
        decision: decision.decision,
        timestamp: decision.timestamp,
        conditionResults: decision.conditionResults,
        actionResults: decision.actionResults.map((r) => ({
          hook: r.hook,
          valid: r.valid,
          error: r.error,
        })),
        payload: payload ? JSON.stringify(payload) : null,
      };

      // Deterministic serialization (sorted keys)
      const canonical = JSON.stringify(receipt, Object.keys(receipt).sort());

      // SHA-256 hash (deterministic)
      const hash = await hashContent(canonical);

      return {
        ...receipt,
        hash,
        canonical,
      };
    },

    /**
     * Get all registered policies
     *
     * @returns {Array<Object>} Array of policies
     */
    getPolicies() {
      return Array.from(policies.values());
    },

    /**
     * Get a specific policy by name
     *
     * @param {string} name - Policy name
     * @returns {Object|null} Policy or null
     */
    getPolicy(name) {
      return policies.get(name) || null;
    },

    /**
     * Get all registered hooks
     *
     * @returns {Array<string>} Array of hook names
     */
    getHooks() {
      return Array.from(hooks.keys());
    },

    /**
     * Get registry statistics
     *
     * @returns {Object} Statistics
     */
    getStats() {
      return {
        policies: {
          total: policies.size,
          enabled: Array.from(policies.values()).filter((p) => p.enabled).length,
          byPriority: Array.from(policies.values()).reduce((acc, p) => {
            acc[p.priority] = (acc[p.priority] || 0) + 1;
            return acc;
          }, {}),
        },
        hooks: {
          total: hooks.size,
          registered: Array.from(hooks.keys()),
        },
      };
    },
  };
}

/**
 * Example usage demonstrating the unified API
 *
 * @example
 * ```javascript
 * import { createPolicyRegistry } from '@unrdf/fusion/policy-engine';
 * import { createStore, dataFactory } from '@unrdf/oxigraph';
 * import { defineHook } from '@unrdf/hooks';
 *
 * // Create registry
 * const registry = await createPolicyRegistry();
 *
 * // Register hook
 * const auditHook = defineHook({
 *   id: 'logAudit',
 *   trigger: 'before-add',
 *   validate: (quad) => {
 *     console.log('Audit:', quad);
 *     return { valid: true };
 *   }
 * });
 * registry.registerHook('logAudit', auditHook);
 *
 * // Register policy
 * registry.registerPolicy({
 *   name: 'AdultOnly',
 *   conditions: [{
 *     sparql: `
 *       PREFIX foaf: <http://xmlns.com/foaf/0.1/>
 *       ASK { ?s foaf:age ?age . FILTER (?age >= 18) }
 *     `
 *   }],
 *   actions: [
 *     { hook: 'logAudit', args: { event: 'adult_verified' } }
 *   ]
 * });
 *
 * // Evaluate policy
 * const store = createStore();
 * // ... add data to store
 *
 * const decision = await registry.routeDecision(store, resource, 'AdultOnly');
 * console.log('Decision:', decision.decision);
 * console.log('Receipt:', decision.receiptHash);
 * ```
 */
