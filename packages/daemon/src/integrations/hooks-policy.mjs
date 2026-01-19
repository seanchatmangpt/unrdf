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
import { PolicySchema, PolicyDecisionSchema, PolicyAuditSchema } from './hooks-policy.schema.mjs';

import {
  evaluateApprovalPolicy,
  evaluateTimeWindowPolicy,
  evaluateResourceLimitPolicy,
  evaluateRateLimitPolicy,
  evaluateCustomPolicy,
  resolveConflicts,
} from './hooks-policy.helpers.mjs';

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
   * @param {string} policyId - Policy ID to enable
   * @returns {boolean} Whether policy was found and enabled
   */
  enablePolicy(policyId) {
    return this._togglePolicy(policyId, true);
  }

  /**
   * Disable a policy
   * @param {string} policyId - Policy ID to disable
   * @returns {boolean} Whether policy was found and disabled
   */
  disablePolicy(policyId) {
    return this._togglePolicy(policyId, false);
  }

  /**
   * Toggle policy enabled state
   * @private
   */
  _togglePolicy(policyId, enabled) {
    const policy = this.policies.get(policyId);
    if (!policy) return false;

    policy.enabled = enabled;
    policy.updatedAt = new Date();

    if (this.auditEnabled) {
      this._recordAudit({
        policyId,
        action: enabled ? 'enabled' : 'disabled',
        version: policy.version,
      });
    }

    this.emit(`policy:${enabled ? 'enabled' : 'disabled'}`, { adapterId: this.id, policyId });
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

    return resolveConflicts(
      operationId,
      evaluations,
      this.conflictStrategy,
      this._recordDecision.bind(this)
    );
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

  /** Get policy by ID */
  getPolicy(policyId) {
    return this.policies.get(policyId);
  }

  /** List all policies */
  listPolicies() {
    return Array.from(this.policies.values());
  }

  /** Get policy history */
  getPolicyHistory(policyId) {
    return this.policyHistory.get(policyId) || [];
  }

  /** Get audit trail for a policy (filtered by policyId if provided) */
  getAuditTrail(policyId) {
    return policyId ? this.auditLog.filter(e => e.policyId === policyId) : this.auditLog;
  }

  /** Get decision log for an operation (filtered by operationId if provided) */
  getDecisionLog(operationId) {
    return operationId ? this.decisionLog.filter(e => e.operationId === operationId) : this.decisionLog;
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

  /** @private Evaluate a single policy */
  async _evaluatePolicy(policy, context) {
    switch (policy.type) {
      case 'approval':
        return evaluateApprovalPolicy(policy, context);
      case 'time-window':
        return evaluateTimeWindowPolicy(policy, context);
      case 'resource-limit':
        return evaluateResourceLimitPolicy(policy, context, this.resourceUsage);
      case 'rate-limit':
        return evaluateRateLimitPolicy(policy, context, this.operationRateLimit);
      case 'custom':
        return evaluateCustomPolicy(policy, context);
      default:
        return { decision: 'allow', reason: 'Unknown policy type - allowing' };
    }
  }

  /** @private Record policy decision in log */
  _recordDecision(operationId, decision, reason, policiesEvaluated, conflictResolved = false) {
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
    if (this.decisionLog.length > 10000) {
      this.decisionLog.splice(0, this.decisionLog.length - 10000);
    }

    this.emit('policy:decision', { adapterId: this.id, decision: decisionEntry });
    return decisionEntry;
  }

  /** @private Record audit entry */
  _recordAudit(entry) {
    const auditEntry = PolicyAuditSchema.parse({
      id: `audit-${Date.now()}-${Math.random().toString(36).slice(2, 9)}`,
      timestamp: new Date(),
      ...entry,
    });

    this.auditLog.push(auditEntry);
    if (this.auditLog.length > 10000) {
      this.auditLog.splice(0, this.auditLog.length - 10000);
    }

    this.emit('policy:audit', { adapterId: this.id, audit: auditEntry });
  }

  /** @private Setup hook interception for policy enforcement */
  _setupHookInterception() {
    const originalExecuteHook = this.hookScheduler.executeHook;

    this.hookScheduler.executeHook = async (hook, context) => {
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
