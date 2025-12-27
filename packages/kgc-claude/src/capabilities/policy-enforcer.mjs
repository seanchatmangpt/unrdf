/**
 * @file Policy Enforcer - Runtime Policy Enforcement with Receipts
 * @module kgc-claude/capabilities/policy-enforcer
 *
 * @description
 * Hyper-advanced policy enforcement for Claude Code tool governance.
 * Implements deny-by-default patterns, allowlist/blocklist management,
 * and cryptographic violation receipts with evidence.
 *
 * **Capabilities:**
 * - Deny-by-default enforcement
 * - Allowlist/blocklist management
 * - Violation receipts with cryptographic proof
 * - Policy compliance reporting
 * - Real-time policy updates
 * - Evidence collection and archival
 */

import { z } from 'zod';
import { createHash } from 'crypto';

/* ========================================================================= */
/* Schemas                                                                   */
/* ========================================================================= */

/**
 * Enforcement action types
 * @enum {string}
 */
export const EnforcementAction = {
  ALLOW: 'allow',
  DENY: 'deny',
  ASK: 'ask',
  LOG: 'log',
  BLOCK: 'block',
};

/**
 * Schema for policy rule
 */
export const PolicyRuleSchema = z.object({
  id: z.string().min(1),
  pattern: z.string().min(1), // Tool pattern (e.g., "Bash(rm:*)")
  action: z.enum(['allow', 'deny', 'ask', 'log', 'block']),
  reason: z.string().optional(),
  priority: z.number().int().min(0).max(100).default(50),
  enabled: z.boolean().default(true),
  metadata: z.record(z.string(), z.any()).optional(),
});

/**
 * Schema for enforcement policy
 */
export const EnforcementPolicySchema = z.object({
  id: z.string().uuid(),
  name: z.string().min(1),
  version: z.string().regex(/^\d+\.\d+\.\d+$/),
  defaultAction: z.enum(['allow', 'deny', 'ask']).default('deny'),
  rules: z.array(PolicyRuleSchema),
  allowlist: z.array(z.string()).optional(),
  blocklist: z.array(z.string()).optional(),
  strictMode: z.boolean().default(true),
  collectEvidence: z.boolean().default(true),
});

/**
 * Schema for violation receipt
 */
export const ViolationReceiptSchema = z.object({
  id: z.string().uuid(),
  timestamp: z.date(),
  tool: z.string(),
  input: z.any(),
  action: z.enum(['deny', 'block']),
  rule: z.string().optional(),
  reason: z.string(),
  evidence: z.array(z.any()).optional(),
  hash: z.string(), // Cryptographic hash for integrity
  signature: z.string().optional(), // Optional digital signature
});

/**
 * Schema for enforcement result
 */
export const EnforcementResultSchema = z.object({
  allowed: z.boolean(),
  action: z.enum(['allow', 'deny', 'ask', 'log', 'block']),
  rule: z.string().optional(),
  reason: z.string(),
  receipt: z.any().optional(), // ViolationReceipt if denied
  timestamp: z.date(),
});

/* ========================================================================= */
/* Policy Enforcer                                                           */
/* ========================================================================= */

/**
 * Policy Enforcer for runtime policy enforcement
 */
export class PolicyEnforcer {
  /**
   * Create a new policy enforcer
   * @param {Object} policy - Enforcement policy
   */
  constructor(policy) {
    this.policy = EnforcementPolicySchema.parse(policy);
    this.violations = [];
    this.allowCache = new Set();
    this.denyCache = new Set();
    this.stats = {
      enforcements: 0,
      allows: 0,
      denies: 0,
      asks: 0,
      violations: 0,
    };
  }

  /**
   * Enforce policy for tool usage
   * @param {string} tool - Tool name
   * @param {Object} [input] - Tool input
   * @param {Object} [context] - Execution context
   * @returns {Object} Enforcement result
   */
  enforce(tool, input = {}, context = {}) {
    this.stats.enforcements++;

    // Check cache first
    if (this.allowCache.has(tool)) {
      this.stats.allows++;
      return this._createResult('allow', 'Cached allow', null);
    }

    if (this.denyCache.has(tool)) {
      this.stats.denies++;
      return this._createResult('deny', 'Cached deny', null, true);
    }

    // Check allowlist
    if (this.policy.allowlist && this._isInList(tool, this.policy.allowlist)) {
      this.allowCache.add(tool);
      this.stats.allows++;
      return this._createResult('allow', 'In allowlist', null);
    }

    // Check blocklist
    if (this.policy.blocklist && this._isInList(tool, this.policy.blocklist)) {
      this.denyCache.add(tool);
      this.stats.denies++;
      return this._createResult('deny', 'In blocklist', null, true, input);
    }

    // Evaluate policy rules
    const matchedRule = this._findMatchingRule(tool);

    if (matchedRule) {
      const action = matchedRule.action;

      // Update stats
      if (action === 'allow') {
        this.stats.allows++;
        this.allowCache.add(tool);
      } else if (action === 'deny' || action === 'block') {
        this.stats.denies++;
        this.denyCache.add(tool);
      } else if (action === 'ask') {
        this.stats.asks++;
      }

      // Create result
      const createViolation = action === 'deny' || action === 'block';
      return this._createResult(
        action,
        matchedRule.reason || `Matched rule: ${matchedRule.id}`,
        matchedRule,
        createViolation,
        input
      );
    }

    // Fall back to default action
    const defaultAction = this.policy.defaultAction;
    const reason = `Default policy: ${defaultAction}`;

    if (defaultAction === 'allow') {
      this.stats.allows++;
    } else if (defaultAction === 'deny') {
      this.stats.denies++;
    } else {
      this.stats.asks++;
    }

    const createViolation = defaultAction === 'deny';
    return this._createResult(defaultAction, reason, null, createViolation, input);
  }

  /**
   * Add a rule to the policy
   * @param {Object} rule - Policy rule
   */
  addRule(rule) {
    const validRule = PolicyRuleSchema.parse(rule);
    this.policy.rules.push(validRule);
    this._sortRules();
    this._clearCache();
  }

  /**
   * Remove a rule from the policy
   * @param {string} ruleId - Rule ID
   */
  removeRule(ruleId) {
    this.policy.rules = this.policy.rules.filter(r => r.id !== ruleId);
    this._clearCache();
  }

  /**
   * Update a rule
   * @param {string} ruleId - Rule ID
   * @param {Object} updates - Rule updates
   */
  updateRule(ruleId, updates) {
    const rule = this.policy.rules.find(r => r.id === ruleId);
    if (!rule) {
      throw new Error(`Rule ${ruleId} not found`);
    }

    Object.assign(rule, updates);
    this._sortRules();
    this._clearCache();
  }

  /**
   * Add tool to allowlist
   * @param {string} pattern - Tool pattern
   */
  addToAllowlist(pattern) {
    if (!this.policy.allowlist) {
      this.policy.allowlist = [];
    }
    if (!this.policy.allowlist.includes(pattern)) {
      this.policy.allowlist.push(pattern);
      this._clearCache();
    }
  }

  /**
   * Add tool to blocklist
   * @param {string} pattern - Tool pattern
   */
  addToBlocklist(pattern) {
    if (!this.policy.blocklist) {
      this.policy.blocklist = [];
    }
    if (!this.policy.blocklist.includes(pattern)) {
      this.policy.blocklist.push(pattern);
      this._clearCache();
    }
  }

  /**
   * Get violation receipts
   * @param {Object} [filters] - Filters
   * @returns {Array} Violation receipts
   */
  getViolations(filters = {}) {
    let violations = [...this.violations];

    if (filters.tool) {
      violations = violations.filter(v => v.tool === filters.tool);
    }

    if (filters.since) {
      violations = violations.filter(v => v.timestamp >= filters.since);
    }

    if (filters.action) {
      violations = violations.filter(v => v.action === filters.action);
    }

    return violations;
  }

  /**
   * Get compliance report
   * @returns {Object} Compliance report
   */
  getComplianceReport() {
    const total = this.stats.enforcements;
    const compliant = this.stats.allows;
    const nonCompliant = this.stats.denies;

    return {
      timestamp: new Date(),
      policy: {
        id: this.policy.id,
        name: this.policy.name,
        version: this.policy.version,
      },
      stats: {
        ...this.stats,
        complianceRate: total > 0 ? (compliant / total) * 100 : 100,
      },
      violations: {
        total: this.violations.length,
        recent: this.violations.slice(-10),
      },
      cache: {
        allowlist: this.allowCache.size,
        denylist: this.denyCache.size,
      },
    };
  }

  /**
   * Clear all caches
   */
  clearCache() {
    this._clearCache();
  }

  /**
   * Get enforcer statistics
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      ...this.stats,
      violations: this.violations.length,
      cacheSize: this.allowCache.size + this.denyCache.size,
    };
  }

  /* ======================================================================= */
  /* Private Methods                                                         */
  /* ======================================================================= */

  /**
   * Find matching rule for tool
   * @private
   */
  _findMatchingRule(tool) {
    // Rules are already sorted by priority
    for (const rule of this.policy.rules) {
      if (!rule.enabled) continue;

      if (this._matchesPattern(tool, rule.pattern)) {
        return rule;
      }
    }

    return null;
  }

  /**
   * Check if tool matches pattern
   * @private
   */
  _matchesPattern(tool, pattern) {
    // Exact match
    if (tool === pattern) return true;

    // Wildcard match
    if (pattern.includes('*')) {
      const regex = new RegExp('^' + pattern.replace(/\*/g, '.*') + '$');
      return regex.test(tool);
    }

    // Prefix match
    if (tool.startsWith(pattern + '(')) return true;

    return false;
  }

  /**
   * Check if tool is in list
   * @private
   */
  _isInList(tool, list) {
    return list.some(pattern => this._matchesPattern(tool, pattern));
  }

  /**
   * Sort rules by priority
   * @private
   */
  _sortRules() {
    this.policy.rules.sort((a, b) => (b.priority || 50) - (a.priority || 50));
  }

  /**
   * Clear decision cache
   * @private
   */
  _clearCache() {
    this.allowCache.clear();
    this.denyCache.clear();
  }

  /**
   * Create enforcement result
   * @private
   */
  _createResult(action, reason, rule, createViolation = false, input = null) {
    const result = {
      allowed: action === 'allow',
      action,
      rule: rule?.id,
      reason,
      timestamp: new Date(),
    };

    if (createViolation && this.policy.collectEvidence) {
      result.receipt = this._createViolationReceipt(
        input?.tool || 'unknown',
        input,
        action,
        rule,
        reason
      );
      this.violations.push(result.receipt);
      this.stats.violations++;
    }

    return result;
  }

  /**
   * Create violation receipt with cryptographic proof
   * @private
   */
  _createViolationReceipt(tool, input, action, rule, reason) {
    const receipt = {
      id: crypto.randomUUID(),
      timestamp: new Date(),
      tool,
      input,
      action,
      rule: rule?.id,
      reason,
    };

    // Collect evidence if enabled
    if (this.policy.collectEvidence) {
      receipt.evidence = [
        {
          type: 'policy-state',
          data: {
            policyId: this.policy.id,
            policyVersion: this.policy.version,
            defaultAction: this.policy.defaultAction,
            strictMode: this.policy.strictMode,
          },
        },
        {
          type: 'matched-rule',
          data: rule || null,
        },
        {
          type: 'tool-input',
          data: input,
        },
      ];
    }

    // Generate cryptographic hash
    receipt.hash = this._hashReceipt(receipt);

    return receipt;
  }

  /**
   * Generate cryptographic hash for receipt
   * @private
   */
  _hashReceipt(receipt) {
    const data = JSON.stringify({
      id: receipt.id,
      timestamp: receipt.timestamp.toISOString(),
      tool: receipt.tool,
      action: receipt.action,
      rule: receipt.rule,
    });
    return createHash('sha256').update(data).digest('hex');
  }
}

/* ========================================================================= */
/* Factory Functions                                                         */
/* ========================================================================= */

/**
 * Create a policy enforcer with default deny
 * @param {string} name - Policy name
 * @param {Object} [options] - Enforcer options
 * @returns {PolicyEnforcer} Policy enforcer
 */
export function createPolicyEnforcer(name, options = {}) {
  const policy = {
    id: crypto.randomUUID(),
    name,
    version: options.version || '1.0.0',
    defaultAction: options.defaultAction || 'deny',
    rules: options.rules || [],
    allowlist: options.allowlist || [],
    blocklist: options.blocklist || [],
    strictMode: options.strictMode !== false,
    collectEvidence: options.collectEvidence !== false,
  };

  return new PolicyEnforcer(policy);
}

/**
 * Create a deny-by-default enforcer
 * @param {string} name - Policy name
 * @returns {PolicyEnforcer} Policy enforcer
 */
export function createDenyByDefaultEnforcer(name) {
  return createPolicyEnforcer(name, {
    defaultAction: 'deny',
    strictMode: true,
    collectEvidence: true,
  });
}

/**
 * Create an allowlist-only enforcer
 * @param {string} name - Policy name
 * @param {Array} allowlist - Allowed tools
 * @returns {PolicyEnforcer} Policy enforcer
 */
export function createAllowlistEnforcer(name, allowlist) {
  return createPolicyEnforcer(name, {
    defaultAction: 'deny',
    allowlist,
    strictMode: true,
  });
}

/**
 * Create a blocklist enforcer
 * @param {string} name - Policy name
 * @param {Array} blocklist - Blocked tools
 * @returns {PolicyEnforcer} Policy enforcer
 */
export function createBlocklistEnforcer(name, blocklist) {
  return createPolicyEnforcer(name, {
    defaultAction: 'allow',
    blocklist,
    strictMode: true,
  });
}

/* ========================================================================= */
/* Predefined Policies                                                       */
/* ========================================================================= */

/**
 * Create a read-only policy
 * @returns {Object} Policy configuration
 */
export function createReadOnlyPolicy() {
  return {
    name: 'read-only',
    version: '1.0.0',
    defaultAction: 'deny',
    allowlist: ['Read', 'Glob', 'Grep', 'WebFetch'],
    rules: [
      {
        id: 'allow-read-tools',
        pattern: 'Read',
        action: 'allow',
        priority: 90,
        reason: 'Read-only access allowed',
      },
      {
        id: 'deny-write-tools',
        pattern: 'Write',
        action: 'deny',
        priority: 90,
        reason: 'Write operations not allowed in read-only mode',
      },
    ],
  };
}

/**
 * Create a safe development policy
 * @returns {Object} Policy configuration
 */
export function createSafeDevelopmentPolicy() {
  return {
    name: 'safe-development',
    version: '1.0.0',
    defaultAction: 'ask',
    blocklist: ['Bash(rm -rf *)', 'Bash(sudo:*)', 'Bash(chmod 777:*)'],
    rules: [
      {
        id: 'allow-git',
        pattern: 'Bash(git:*)',
        action: 'allow',
        priority: 80,
        reason: 'Git operations allowed',
      },
      {
        id: 'allow-npm',
        pattern: 'Bash(npm:*)',
        action: 'allow',
        priority: 80,
        reason: 'NPM operations allowed',
      },
      {
        id: 'deny-dangerous-rm',
        pattern: 'Bash(rm -rf:*)',
        action: 'block',
        priority: 95,
        reason: 'Dangerous rm operations blocked',
      },
    ],
  };
}

/**
 * Create a production policy (strict)
 * @returns {Object} Policy configuration
 */
export function createProductionPolicy() {
  return {
    name: 'production',
    version: '1.0.0',
    defaultAction: 'deny',
    strictMode: true,
    allowlist: ['Read', 'Grep', 'Glob'],
    rules: [
      {
        id: 'deny-all-writes',
        pattern: 'Write',
        action: 'deny',
        priority: 95,
        reason: 'No write operations in production',
      },
      {
        id: 'deny-all-bash',
        pattern: 'Bash(*)',
        action: 'deny',
        priority: 95,
        reason: 'No shell commands in production',
      },
    ],
  };
}

/* ========================================================================= */
/* Exports                                                                   */
/* ========================================================================= */

export default {
  PolicyEnforcer,
  EnforcementAction,
  createPolicyEnforcer,
  createDenyByDefaultEnforcer,
  createAllowlistEnforcer,
  createBlocklistEnforcer,
  createReadOnlyPolicy,
  createSafeDevelopmentPolicy,
  createProductionPolicy,
};
