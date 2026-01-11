/**
 * @file Daemon Hooks Policy Integration
 * @module @unrdf/daemon/integrations/hooks-policy
 * @description
 * Advanced policy framework integration for daemon operations.
 * Provides policy-driven operation control with:
 * - Policy evaluation before operation execution
 * - Audit trail for all policy decisions
 * - Policy versioning and rollback support
 * - Conflict resolution for multiple policies
 * - Real-time policy updates without service restart
 */

import { EventEmitter } from 'events';
import { z } from 'zod';

/**
 * Policy definition schema
 * @type {Object}
 */
export const PolicySchema = z.object({
  id: z.string().min(1),
  name: z.string().min(1),
  version: z.number().int().min(1),
  type: z.enum(['approval', 'time-window', 'resource-limit', 'rate-limit', 'custom']),
  enabled: z.boolean().default(true),
  priority: z.number().int().min(0).max(100).default(50),
  description: z.string().optional(),
  config: z.record(z.string(), z.unknown()),
  createdAt: z.date().default(() => new Date()),
  updatedAt: z.date().default(() => new Date()),
  metadata: z.record(z.string(), z.unknown()).optional(),
});

/**
 * Policy decision schema
 * @type {Object}
 */
export const PolicyDecisionSchema = z.object({
  id: z.string(),
  operationId: z.string(),
  timestamp: z.date(),
  decision: z.enum(['allow', 'deny', 'defer']),
  reason: z.string(),
  policiesEvaluated: z.array(z.string()),
  conflictResolved: z.boolean().default(false),
  conflictStrategy: z.enum(['highest-priority', 'unanimous', 'majority', 'first-match']).optional(),
  metadata: z.record(z.string(), z.unknown()).optional(),
});

/**
 * Policy audit entry schema
 * @type {Object}
 */
export const PolicyAuditSchema = z.object({
  id: z.string(),
  timestamp: z.date(),
  policyId: z.string(),
  action: z.enum(['created', 'updated', 'deleted', 'enabled', 'disabled', 'rolled-back']),
  version: z.number().int().min(1),
  changes: z.record(z.string(), z.unknown()).optional(),
  actor: z.string().optional(),
  metadata: z.record(z.string(), z.unknown()).optional(),
});

/**
 * Policy history entry
 * @typedef {Object} PolicyHistoryEntry
 * @property {string} id - Policy ID
 * @property {number} version - Policy version
 * @property {Object} policy - Policy definition
 * @property {Date} timestamp - When this version was created
 */

/**
 * Policy evaluation context
 * @typedef {Object} PolicyEvaluationContext
 * @property {string} operationId - Operation being evaluated
 * @property {string} operationType - Type of operation
 * @property {Object} payload - Operation payload
 * @property {Object} metadata - Operation metadata
 * @property {Object} environment - Environment variables
 */

/**
 * Daemon Hook Policy Adapter - Extends HookScheduler with policy control
 *
 * Manages policy-driven operation execution with:
 * - Pre-execution policy evaluation
 * - Audit trail for all decisions
 * - Policy versioning and rollback
 * - Multi-policy conflict resolution
 */
export class DaemonHookPolicyAdapter extends EventEmitter {
  /**
   * Create daemon hook policy adapter
   *
   * @param {import('../daemon.mjs').UnrdfDaemon} daemon - Daemon instance
   * @param {import('@unrdf/hooks').HookScheduler} hookScheduler - Hook scheduler instance
   * @param {object} options - Configuration options
   * @param {string} [options.adapterId] - Unique adapter ID
   * @param {string} [options.conflictStrategy='highest-priority'] - How to resolve policy conflicts
   * @param {boolean} [options.auditEnabled=true] - Whether to enable audit trail
   * @param {number} [options.maxHistoryVersions=10] - Max policy history versions to keep
   * @throws {Error} If configuration is invalid
   */
  constructor(daemon, hookScheduler, options = {}) {
    super();

    this.id = options.adapterId || `adapter-${Date.now()}-${Math.random().toString(36).slice(2, 9)}`;
    this.daemon = daemon;
    this.hookScheduler = hookScheduler;

    /** @type {Map<string, Object>} */
    this.policies = new Map();

    /** @type {Map<string, Array<PolicyHistoryEntry>>} */
    this.policyHistory = new Map();

    /** @type {Array<Object>} */
    this.auditLog = [];

    /** @type {Array<Object>} */
    this.decisionLog = [];

    /** @type {string} */
    this.conflictStrategy = options.conflictStrategy || 'highest-priority';

    /** @type {boolean} */
    this.auditEnabled = options.auditEnabled !== false;

    /** @type {number} */
    this.maxHistoryVersions = options.maxHistoryVersions || 10;

    /** @type {Map<string, number>} */
    this.operationRateLimit = new Map();

    /** @type {Map<string, number>} */
    this.resourceUsage = new Map();

    this._setupHookInterception();
  }

  /**
   * Register a policy for operation control
   *
   * @param {Object} policyConfig - Policy configuration
   * @returns {Object} Registered policy
   * @throws {Error} If policy configuration is invalid
   * @example
   * adapter.registerPolicy({
   *   id: 'approval-policy',
   *   name: 'Require Approval',
   *   type: 'approval',
   *   priority: 90,
   *   config: { requiresApproval: true, approvers: ['admin@example.com'] }
   * });
   */
  registerPolicy(policyConfig) {
    const existing = this.policies.get(policyConfig.id);
    const newVersion = existing ? existing.version + 1 : 1;

    const validated = PolicySchema.parse({
      ...policyConfig,
      version: newVersion,
      createdAt: policyConfig.createdAt || new Date(),
      updatedAt: policyConfig.updatedAt || new Date(),
    });

    const policy = validated;

    // Store in history before updating
    if (!this.policyHistory.has(validated.id)) {
      this.policyHistory.set(validated.id, []);
    }

    this.policyHistory.get(validated.id).push({
      id: validated.id,
      version: newVersion,
      policy,
      timestamp: new Date(),
    });

    // Trim history if needed
    const history = this.policyHistory.get(validated.id);
    if (history.length > this.maxHistoryVersions) {
      history.splice(0, history.length - this.maxHistoryVersions);
    }

    this.policies.set(validated.id, policy);

    if (this.auditEnabled) {
      this._recordAudit({
        policyId: validated.id,
        action: existing ? 'updated' : 'created',
        version: newVersion,
        changes: existing ? { before: existing, after: policy } : undefined,
      });
    }

    this.emit('policy:registered', { adapterId: this.id, policy });
    return policy;
  }

  /**
   * Unregister a policy
   *
   * @param {string} policyId - Policy ID to remove
   * @returns {boolean} Whether policy was found and removed
   */
  unregisterPolicy(policyId) {
    if (!this.policies.has(policyId)) {
      return false;
    }

    const policy = this.policies.get(policyId);
    this.policies.delete(policyId);

    if (this.auditEnabled) {
      this._recordAudit({
        policyId,
        action: 'deleted',
        version: policy.version,
      });
    }

    this.emit('policy:unregistered', { adapterId: this.id, policyId });
    return true;
  }

  /**
   * Enable a policy
   *
   * @param {string} policyId - Policy ID to enable
   * @returns {boolean} Whether policy was found and enabled
   */
  enablePolicy(policyId) {
    const policy = this.policies.get(policyId);
    if (!policy) return false;

    policy.enabled = true;
    policy.updatedAt = new Date();

    if (this.auditEnabled) {
      this._recordAudit({
        policyId,
        action: 'enabled',
        version: policy.version,
      });
    }

    this.emit('policy:enabled', { adapterId: this.id, policyId });
    return true;
  }

  /**
   * Disable a policy
   *
   * @param {string} policyId - Policy ID to disable
   * @returns {boolean} Whether policy was found and disabled
   */
  disablePolicy(policyId) {
    const policy = this.policies.get(policyId);
    if (!policy) return false;

    policy.enabled = false;
    policy.updatedAt = new Date();

    if (this.auditEnabled) {
      this._recordAudit({
        policyId,
        action: 'disabled',
        version: policy.version,
      });
    }

    this.emit('policy:disabled', { adapterId: this.id, policyId });
    return true;
  }

  /**
   * Rollback a policy to a previous version
   *
   * @param {string} policyId - Policy ID to rollback
   * @param {number} version - Version to rollback to
   * @returns {Object|null} Rolled-back policy or null if not found
   */
  rollbackPolicy(policyId, version) {
    const history = this.policyHistory.get(policyId);
    if (!history) return null;

    const entry = history.find(e => e.version === version);
    if (!entry) return null;

    const currentPolicy = this.policies.get(policyId);
    const currentVersion = currentPolicy ? currentPolicy.version : 1;

    // Restore content from historical version but keep current version number
    const rolledBack = {
      ...entry.policy,
      version: currentVersion,
      updatedAt: new Date(),
    };
    this.policies.set(policyId, rolledBack);

    if (this.auditEnabled) {
      this._recordAudit({
        policyId,
        action: 'rolled-back',
        version: currentVersion,
        changes: { restoredFromVersion: version },
      });
    }

    this.emit('policy:rolled-back', { adapterId: this.id, policyId, version });
    return rolledBack;
  }

  /**
   * Evaluate policies for an operation
   *
   * @param {string} operationId - Operation ID
   * @param {PolicyEvaluationContext} context - Evaluation context
   * @returns {Promise<Object>} Decision object with allow/deny/defer
   */
  async evaluatePolicies(operationId, context) {
    const policiesList = Array.from(this.policies.values())
      .filter(p => p.enabled)
      .sort((a, b) => b.priority - a.priority);

    if (policiesList.length === 0) {
      return this._recordDecision(operationId, 'allow', 'No policies configured', []);
    }

    const evaluations = [];

    for (const policy of policiesList) {
      try {
        const result = await this._evaluatePolicy(policy, context);
        evaluations.push({
          policyId: policy.id,
          decision: result.decision,
          reason: result.reason,
        });
      } catch (error) {
        evaluations.push({
          policyId: policy.id,
          decision: 'defer',
          reason: `Policy evaluation error: ${error.message}`,
        });
      }
    }

    return this._resolveConflicts(operationId, evaluations);
  }

  /**
   * Execute operation with policy validation
   *
   * @param {string} operationId - Operation ID
   * @param {PolicyEvaluationContext} context - Evaluation context
   * @returns {Promise<Object>} Operation result
   * @throws {Error} If operation violates policies
   */
  async executeWithPolicy(operationId, context) {
    const decision = await this.evaluatePolicies(operationId, context);

    if (decision.decision === 'deny') {
      const error = new Error(`Operation denied by policy: ${decision.reason}`);
      error.code = 'POLICY_DENIED';
      throw error;
    }

    if (decision.decision === 'defer') {
      return {
        success: false,
        operationId,
        decision: 'defer',
        reason: decision.reason,
      };
    }

    // Execute the operation
    try {
      const result = await this.daemon.execute(operationId);
      return {
        success: true,
        operationId,
        result,
        decision: 'allow',
      };
    } catch (error) {
      return {
        success: false,
        operationId,
        error: error.message,
        decision: 'allow',
      };
    }
  }

  /**
   * Get policy by ID
   *
   * @param {string} policyId - Policy ID
   * @returns {Object|undefined} Policy or undefined if not found
   */
  getPolicy(policyId) {
    return this.policies.get(policyId);
  }

  /**
   * List all policies
   *
   * @returns {Array<Object>} Array of policies
   */
  listPolicies() {
    return Array.from(this.policies.values());
  }

  /**
   * Get policy history
   *
   * @param {string} policyId - Policy ID
   * @returns {Array<PolicyHistoryEntry>} Policy version history
   */
  getPolicyHistory(policyId) {
    return this.policyHistory.get(policyId) || [];
  }

  /**
   * Get audit trail for a policy
   *
   * @param {string} policyId - Policy ID (optional)
   * @returns {Array<Object>} Audit log entries
   */
  getAuditTrail(policyId) {
    if (!policyId) {
      return this.auditLog;
    }
    return this.auditLog.filter(entry => entry.policyId === policyId);
  }

  /**
   * Get decision log for an operation
   *
   * @param {string} operationId - Operation ID (optional)
   * @returns {Array<Object>} Decision log entries
   */
  getDecisionLog(operationId) {
    if (!operationId) {
      return this.decisionLog;
    }
    return this.decisionLog.filter(entry => entry.operationId === operationId);
  }

  /**
   * Get adapter statistics
   *
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      adapterId: this.id,
      totalPolicies: this.policies.size,
      enabledPolicies: Array.from(this.policies.values()).filter(p => p.enabled).length,
      auditLogSize: this.auditLog.length,
      decisionLogSize: this.decisionLog.length,
      conflictStrategy: this.conflictStrategy,
      auditEnabled: this.auditEnabled,
      policyHistorySizes: Object.fromEntries(
        Array.from(this.policyHistory.entries()).map(([id, history]) => [id, history.length])
      ),
    };
  }

  /**
   * Evaluate a single policy
   *
   * @private
   * @param {Object} policy - Policy to evaluate
   * @param {PolicyEvaluationContext} context - Evaluation context
   * @returns {Promise<Object>} Evaluation result with decision and reason
   */
  async _evaluatePolicy(policy, context) {
    switch (policy.type) {
      case 'approval':
        return this._evaluateApprovalPolicy(policy, context);
      case 'time-window':
        return this._evaluateTimeWindowPolicy(policy, context);
      case 'resource-limit':
        return this._evaluateResourceLimitPolicy(policy, context);
      case 'rate-limit':
        return this._evaluateRateLimitPolicy(policy, context);
      case 'custom':
        return this._evaluateCustomPolicy(policy, context);
      default:
        return { decision: 'allow', reason: 'Unknown policy type - allowing' };
    }
  }

  /**
   * Evaluate approval policy
   *
   * @private
   */
  async _evaluateApprovalPolicy(policy, context) {
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
   * @private
   */
  async _evaluateTimeWindowPolicy(policy, context) {
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
   * @private
   */
  async _evaluateResourceLimitPolicy(policy, context) {
    const config = policy.config;
    const currentUsage = this.resourceUsage.get(context.operationType) || 0;
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
   * @private
   */
  async _evaluateRateLimitPolicy(policy, context) {
    const config = policy.config;
    const maxPerMinute = config.maxPerMinute || 10;
    const key = context.operationType;

    const count = this.operationRateLimit.get(key) || 0;

    if (count >= maxPerMinute) {
      return {
        decision: 'defer',
        reason: `Rate limit exceeded: ${count}/${maxPerMinute} operations per minute`,
      };
    }

    this.operationRateLimit.set(key, count + 1);

    // Reset after a minute
    setTimeout(() => {
      this.operationRateLimit.set(key, 0);
    }, 60000);

    return { decision: 'allow', reason: `Rate limit OK: ${count + 1}/${maxPerMinute}` };
  }

  /**
   * Evaluate custom policy
   *
   * @private
   */
  async _evaluateCustomPolicy(policy, context) {
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
   * @private
   */
  async _resolveConflicts(operationId, evaluations) {
    const decisions = evaluations.map(e => e.decision);
    const policiesEvaluated = evaluations.map(e => e.policyId);

    // Any deny is always respected
    if (decisions.includes('deny')) {
      const denyEval = evaluations.find(e => e.decision === 'deny');
      return this._recordDecision(operationId, 'deny', denyEval.reason, policiesEvaluated, true);
    }

    // Handle different conflict resolution strategies
    if (this.conflictStrategy === 'unanimous') {
      // All must allow, any defer/deny blocks
      if (decisions.includes('defer')) {
        const deferEval = evaluations.find(e => e.decision === 'defer');
        return this._recordDecision(operationId, 'defer', deferEval.reason, policiesEvaluated, true);
      }
      return this._recordDecision(operationId, 'allow', 'Unanimous approval', policiesEvaluated, false);
    }

    if (this.conflictStrategy === 'highest-priority') {
      // Use the first policy's decision (already sorted by priority)
      const firstEval = evaluations[0];
      const conflictExists = evaluations.length > 1;
      return this._recordDecision(
        operationId,
        firstEval.decision,
        firstEval.reason,
        policiesEvaluated,
        conflictExists
      );
    }

    if (this.conflictStrategy === 'majority') {
      const allows = decisions.filter(d => d === 'allow').length;
      const defers = decisions.filter(d => d === 'defer').length;
      const denies = decisions.filter(d => d === 'deny').length;

      if (allows > defers && allows > denies) {
        return this._recordDecision(operationId, 'allow', 'Majority approval', policiesEvaluated, true);
      }
      if (defers >= allows && defers >= denies) {
        const deferEval = evaluations.find(e => e.decision === 'defer');
        return this._recordDecision(operationId, 'defer', deferEval.reason, policiesEvaluated, true);
      }
      return this._recordDecision(operationId, 'deny', 'Majority denial', policiesEvaluated, true);
    }

    if (this.conflictStrategy === 'first-match') {
      const firstEval = evaluations[0];
      return this._recordDecision(operationId, firstEval.decision, firstEval.reason, policiesEvaluated, false);
    }

    // Default: allow
    return this._recordDecision(operationId, 'allow', 'Default allow decision', policiesEvaluated, false);
  }

  /**
   * Record policy decision in log
   *
   * @private
   */
  _recordDecision(operationId, decision, reason, policiesEvaluated, conflictResolved = false) {
    // Include conflictStrategy if there are multiple policies evaluated or if conflict was resolved
    const shouldIncludeStrategy = policiesEvaluated.length > 1 || conflictResolved;

    const decisionEntry = PolicyDecisionSchema.parse({
      id: `decision-${Date.now()}-${Math.random().toString(36).slice(2, 9)}`,
      operationId,
      timestamp: new Date(),
      decision,
      reason,
      policiesEvaluated,
      conflictResolved,
      conflictStrategy: shouldIncludeStrategy ? this.conflictStrategy : undefined,
    });

    this.decisionLog.push(decisionEntry);

    // Trim decision log if too large
    if (this.decisionLog.length > 10000) {
      this.decisionLog.splice(0, this.decisionLog.length - 10000);
    }

    this.emit('policy:decision', { adapterId: this.id, decision: decisionEntry });

    return decisionEntry;
  }

  /**
   * Record audit entry
   *
   * @private
   */
  _recordAudit(entry) {
    const auditEntry = PolicyAuditSchema.parse({
      id: `audit-${Date.now()}-${Math.random().toString(36).slice(2, 9)}`,
      timestamp: new Date(),
      ...entry,
    });

    this.auditLog.push(auditEntry);

    // Trim audit log if too large
    if (this.auditLog.length > 10000) {
      this.auditLog.splice(0, this.auditLog.length - 10000);
    }

    this.emit('policy:audit', { adapterId: this.id, audit: auditEntry });
  }

  /**
   * Setup hook interception for policy enforcement
   *
   * @private
   */
  _setupHookInterception() {
    const originalExecuteHook = this.hookScheduler.executeHook;

    this.hookScheduler.executeHook = async (hook, context) => {
      // Check if hook has policy context
      if (context?.policyContext) {
        const decision = await this.evaluatePolicies(
          context.policyContext.operationId,
          context.policyContext
        );

        if (decision.decision === 'deny') {
          const error = new Error(decision.reason);
          error.code = 'POLICY_DENIED';
          throw error;
        }

        if (decision.decision === 'defer') {
          return { deferred: true, reason: decision.reason };
        }
      }

      return originalExecuteHook.call(this.hookScheduler, hook, context);
    };
  }
}

/**
 * Create daemon with hooks policy integration
 *
 * @param {import('../daemon.mjs').UnrdfDaemon} daemon - Daemon instance
 * @param {import('@unrdf/hooks').HookScheduler} hookScheduler - Hook scheduler instance
 * @param {object} options - Configuration options
 * @returns {DaemonHookPolicyAdapter} Configured adapter
 */
export function integrateHooksPolicy(daemon, hookScheduler, options = {}) {
  return new DaemonHookPolicyAdapter(daemon, hookScheduler, options);
}

export default {
  DaemonHookPolicyAdapter,
  integrateHooksPolicy,
  PolicySchema,
  PolicyDecisionSchema,
  PolicyAuditSchema,
};
