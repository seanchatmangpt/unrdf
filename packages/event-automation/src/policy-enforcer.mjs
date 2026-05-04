/**
 * @file Policy Enforcer
 * @module @unrdf/event-automation/policy-enforcer
 * @description Enforces policies using hooks integration for delta and receipt operations
 */

import { PolicySchema } from './schemas.mjs';

/**
 * Policy enforcer for hook-based policy execution
 */
export class PolicyEnforcer {
  /**
   * Creates a new policy enforcer
   * @param {Object} [options] - Enforcer options
   * @param {Object} [options.logger] - Logger instance
   * @param {boolean} [options.failOnPolicyViolation=true] - Fail on policy violation
   */
  constructor(options = {}) {
    this.logger = options.logger || console;
    this.failOnPolicyViolation = options.failOnPolicyViolation !== false;
    this.policies = new Map();
    this.metrics = {
      totalEvaluations: 0,
      totalPassed: 0,
      totalFailed: 0,
      durations: [],
    };
  }

  /**
   * Register a policy
   * @param {Object} policy - Policy to register
   * @returns {string} Policy ID
   * @throws {Error} If policy is invalid
   */
  registerPolicy(policy) {
    const validated = PolicySchema.parse(policy);
    this.policies.set(validated.id, validated);
    return validated.id;
  }

  /**
   * Unregister a policy
   * @param {string} policyId - Policy ID to unregister
   * @returns {boolean} Whether policy was removed
   */
  unregisterPolicy(policyId) {
    return this.policies.delete(policyId);
  }

  /**
   * Get a policy by ID
   * @param {string} policyId - Policy ID
   * @returns {Object|undefined} Policy or undefined
   */
  getPolicy(policyId) {
    return this.policies.get(policyId);
  }

  /**
   * Evaluate policies for a trigger
   * @param {string} trigger - Trigger type
   * @param {Object} context - Evaluation context
   * @returns {Promise<Array<Object>>} Policy evaluation results
   */
  async evaluatePolicies(trigger, context) {
    const relevantPolicies = this._getPoliciesForTrigger(trigger);

    if (relevantPolicies.length === 0) {
      return [];
    }

    const results = [];

    for (const policy of relevantPolicies) {
      const result = await this._evaluatePolicy(policy, context);
      results.push(result);

      // Fail fast if policy violation and failOnPolicyViolation is true
      if (!result.passed && this.failOnPolicyViolation) {
        throw new Error(
          `Policy violation: ${policy.name} - ${result.message || 'Unknown error'}`
        );
      }
    }

    return results;
  }

  /**
   * Evaluate a single policy
   * @private
   * @param {Object} policy - Policy to evaluate
   * @param {Object} context - Evaluation context
   * @returns {Promise<Object>} Evaluation result
   */
  async _evaluatePolicy(policy, context) {
    const startTime = performance.now();

    try {
      // Check if policy is enabled
      if (!policy.enabled) {
        return {
          policyId: policy.id,
          policyName: policy.name,
          passed: true,
          skipped: true,
          message: 'Policy disabled',
          duration: performance.now() - startTime,
        };
      }

      // Evaluate condition if present
      if (policy.condition) {
        const conditionResult = await policy.condition(context);
        if (!conditionResult) {
          return {
            policyId: policy.id,
            policyName: policy.name,
            passed: true,
            skipped: true,
            message: 'Condition not met',
            duration: performance.now() - startTime,
          };
        }
      }

      // Execute policy action
      await policy.action(context);

      // Update metrics
      const duration = performance.now() - startTime;
      this.metrics.totalEvaluations++;
      this.metrics.totalPassed++;
      this.metrics.durations.push(duration);

      return {
        policyId: policy.id,
        policyName: policy.name,
        passed: true,
        duration,
      };
    } catch (error) {
      const duration = performance.now() - startTime;
      this.metrics.totalEvaluations++;
      this.metrics.totalFailed++;
      this.metrics.durations.push(duration);

      this.logger.error('Policy evaluation failed:', {
        policyId: policy.id,
        policyName: policy.name,
        error: error.message,
      });

      return {
        policyId: policy.id,
        policyName: policy.name,
        passed: false,
        message: error.message,
        duration,
      };
    }
  }

  /**
   * Get policies for a specific trigger
   * @private
   * @param {string} trigger - Trigger type
   * @returns {Array<Object>} Policies for trigger
   */
  _getPoliciesForTrigger(trigger) {
    return Array.from(this.policies.values())
      .filter((policy) => policy.trigger === trigger)
      .sort((a, b) => (b.priority || 50) - (a.priority || 50));
  }

  /**
   * Get all registered policies
   * @returns {Array<Object>} All policies
   */
  getAllPolicies() {
    return Array.from(this.policies.values());
  }

  /**
   * Get policies by trigger
   * @param {string} trigger - Trigger type
   * @returns {Array<Object>} Policies for trigger
   */
  getPoliciesByTrigger(trigger) {
    return this._getPoliciesForTrigger(trigger);
  }

  /**
   * Enable a policy
   * @param {string} policyId - Policy ID
   * @returns {boolean} Whether policy was enabled
   */
  enablePolicy(policyId) {
    const policy = this.policies.get(policyId);
    if (policy) {
      policy.enabled = true;
      return true;
    }
    return false;
  }

  /**
   * Disable a policy
   * @param {string} policyId - Policy ID
   * @returns {boolean} Whether policy was disabled
   */
  disablePolicy(policyId) {
    const policy = this.policies.get(policyId);
    if (policy) {
      policy.enabled = false;
      return true;
    }
    return false;
  }

  /**
   * Get enforcer metrics
   * @returns {Object} Enforcer metrics
   */
  getMetrics() {
    const durations = this.metrics.durations;
    const avgDuration =
      durations.length > 0
        ? durations.reduce((a, b) => a + b, 0) / durations.length
        : 0;

    const sorted = [...durations].sort((a, b) => a - b);
    const p95Index = Math.floor(sorted.length * 0.95);

    return {
      totalPolicies: this.policies.size,
      totalEvaluations: this.metrics.totalEvaluations,
      totalPassed: this.metrics.totalPassed,
      totalFailed: this.metrics.totalFailed,
      averageDuration: avgDuration,
      p95Duration: sorted[p95Index] || 0,
    };
  }

  /**
   * Reset enforcer state
   */
  reset() {
    this.policies.clear();
    this.metrics = {
      totalEvaluations: 0,
      totalPassed: 0,
      totalFailed: 0,
      durations: [],
    };
  }
}

/**
 * Create a policy enforcer instance
 * @param {Object} [options] - Enforcer options
 * @returns {PolicyEnforcer} Policy enforcer instance
 */
export function createPolicyEnforcer(options = {}) {
  return new PolicyEnforcer(options);
}
