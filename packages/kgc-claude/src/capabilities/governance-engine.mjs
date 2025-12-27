/**
 * @file Governance Engine - Advanced Tool Permission Control
 * @module kgc-claude/capabilities/governance-engine
 *
 * @description
 * Hyper-advanced governance engine for Claude Code tool access control.
 * Implements permission matrices, RBAC, and audit trail generation.
 *
 * **Capabilities:**
 * - Permission matrices for tool access
 * - Role-based access control (RBAC)
 * - Actor-based tool restrictions
 * - Audit trail with cryptographic receipts
 * - Policy decision points (PDP)
 * - Policy enforcement points (PEP)
 */

import { z } from 'zod';
import { createHash } from 'crypto';

/* ========================================================================= */
/* Schemas                                                                   */
/* ========================================================================= */

/**
 * Tool permission levels
 * @enum {string}
 */
export const PermissionLevel = {
  ALLOW: 'allow',
  DENY: 'deny',
  ASK: 'ask',
  AUDIT: 'audit',
};

/**
 * Schema for permission rule
 */
export const PermissionRuleSchema = z.object({
  tool: z.string().min(1), // Tool name or pattern (e.g., "Bash", "Write", "Bash(git:*)")
  permission: z.enum(['allow', 'deny', 'ask', 'audit']),
  roles: z.array(z.string()).optional(), // Roles that this rule applies to
  conditions: z.array(z.string()).optional(), // Condition references
  priority: z.number().int().min(0).max(100).default(50),
  reason: z.string().optional(), // Human-readable reason
});

/**
 * Schema for role definition
 */
export const RoleSchema = z.object({
  name: z.string().min(1),
  description: z.string().optional(),
  permissions: z.array(PermissionRuleSchema),
  inherits: z.array(z.string()).optional(), // Roles to inherit from
});

/**
 * Schema for actor (user/agent)
 */
export const ActorSchema = z.object({
  id: z.string().min(1),
  name: z.string().optional(),
  roles: z.array(z.string()),
  metadata: z.record(z.string(), z.any()).optional(),
});

/**
 * Schema for governance policy
 */
export const GovernancePolicySchema = z.object({
  id: z.string().uuid(),
  name: z.string().min(1),
  version: z.string().regex(/^\d+\.\d+\.\d+$/),
  defaultPermission: z.enum(['allow', 'deny', 'ask']).default('deny'),
  roles: z.array(RoleSchema),
  globalRules: z.array(PermissionRuleSchema).optional(),
  auditEnabled: z.boolean().default(true),
});

/**
 * Schema for audit entry
 */
export const AuditEntrySchema = z.object({
  timestamp: z.date(),
  actor: z.string(),
  tool: z.string(),
  input: z.any(),
  decision: z.enum(['allow', 'deny', 'ask']),
  rule: z.string().optional(),
  reason: z.string().optional(),
  hash: z.string().optional(), // Cryptographic hash for integrity
});

/* ========================================================================= */
/* Governance Engine                                                         */
/* ========================================================================= */

/**
 * Governance Engine for tool permission control
 */
export class GovernanceEngine {
  /**
   * Create a new governance engine
   * @param {Object} policy - Governance policy definition
   */
  constructor(policy) {
    this.policy = GovernancePolicySchema.parse(policy);
    this.roles = new Map();
    this.actors = new Map();
    this.auditLog = [];
    this.decisionCache = new Map();

    // Build role hierarchy
    this._buildRoleHierarchy();
  }

  /**
   * Register an actor (user/agent)
   * @param {Object} actor - Actor definition
   */
  registerActor(actor) {
    const validActor = ActorSchema.parse(actor);
    this.actors.set(validActor.id, validActor);
  }

  /**
   * Check if actor has permission to use tool
   * @param {string} actorId - Actor identifier
   * @param {string} toolName - Tool name
   * @param {Object} [toolInput] - Tool input for context
   * @returns {Object} Decision result
   */
  checkPermission(actorId, toolName, toolInput = {}) {
    const actor = this.actors.get(actorId);
    if (!actor) {
      return this._createDecision('deny', 'Actor not registered', null);
    }

    // Check decision cache
    const cacheKey = `${actorId}:${toolName}`;
    if (this.decisionCache.has(cacheKey)) {
      return this.decisionCache.get(cacheKey);
    }

    // Collect all applicable rules
    const applicableRules = this._getApplicableRules(actor, toolName);

    // Sort by priority (higher priority first)
    applicableRules.sort((a, b) => (b.priority || 50) - (a.priority || 50));

    // First matching rule wins
    let decision = null;
    let matchedRule = null;

    for (const rule of applicableRules) {
      if (this._matchesTool(toolName, rule.tool)) {
        decision = rule.permission;
        matchedRule = rule;
        break;
      }
    }

    // Fall back to default permission
    if (!decision) {
      decision = this.policy.defaultPermission;
    }

    const result = this._createDecision(
      decision,
      matchedRule?.reason || 'Default policy',
      matchedRule
    );

    // Audit if enabled
    if (this.policy.auditEnabled) {
      this._auditDecision(actorId, toolName, toolInput, result);
    }

    // Cache decision (invalidate on policy change)
    this.decisionCache.set(cacheKey, result);

    return result;
  }

  /**
   * Get permission matrix for actor
   * @param {string} actorId - Actor identifier
   * @returns {Object} Permission matrix
   */
  getPermissionMatrix(actorId) {
    const actor = this.actors.get(actorId);
    if (!actor) {
      throw new Error(`Actor ${actorId} not found`);
    }

    const matrix = {
      actorId,
      roles: actor.roles,
      permissions: {},
    };

    // Build permission map for common tools
    const commonTools = [
      'Bash',
      'Read',
      'Write',
      'Edit',
      'Glob',
      'Grep',
      'NotebookEdit',
      'WebFetch',
      'WebSearch',
    ];

    for (const tool of commonTools) {
      const decision = this.checkPermission(actorId, tool);
      matrix.permissions[tool] = {
        level: decision.decision,
        reason: decision.reason,
      };
    }

    return matrix;
  }

  /**
   * Get audit log
   * @param {Object} [filters] - Audit log filters
   * @returns {Array} Audit entries
   */
  getAuditLog(filters = {}) {
    let entries = [...this.auditLog];

    if (filters.actor) {
      entries = entries.filter(e => e.actor === filters.actor);
    }

    if (filters.tool) {
      entries = entries.filter(e => e.tool === filters.tool);
    }

    if (filters.decision) {
      entries = entries.filter(e => e.decision === filters.decision);
    }

    if (filters.since) {
      entries = entries.filter(e => e.timestamp >= filters.since);
    }

    return entries;
  }

  /**
   * Clear decision cache (call when policy changes)
   */
  clearCache() {
    this.decisionCache.clear();
  }

  /**
   * Get statistics
   * @returns {Object} Engine statistics
   */
  getStats() {
    const decisions = { allow: 0, deny: 0, ask: 0 };
    for (const entry of this.auditLog) {
      decisions[entry.decision] = (decisions[entry.decision] || 0) + 1;
    }

    return {
      actors: this.actors.size,
      roles: this.roles.size,
      auditEntries: this.auditLog.length,
      decisions,
      cacheSize: this.decisionCache.size,
    };
  }

  /* ======================================================================= */
  /* Private Methods                                                         */
  /* ======================================================================= */

  /**
   * Build role hierarchy from policy
   * @private
   */
  _buildRoleHierarchy() {
    for (const role of this.policy.roles) {
      const permissions = [...role.permissions];

      // Inherit permissions from parent roles
      if (role.inherits) {
        for (const parentRoleName of role.inherits) {
          const parentRole = this.policy.roles.find(r => r.name === parentRoleName);
          if (parentRole) {
            permissions.push(...parentRole.permissions);
          }
        }
      }

      this.roles.set(role.name, {
        ...role,
        permissions,
      });
    }
  }

  /**
   * Get applicable rules for actor and tool
   * @private
   */
  _getApplicableRules(actor, toolName) {
    const rules = [];

    // Add global rules
    if (this.policy.globalRules) {
      rules.push(...this.policy.globalRules);
    }

    // Add role-based rules
    for (const roleName of actor.roles) {
      const role = this.roles.get(roleName);
      if (role) {
        rules.push(...role.permissions);
      }
    }

    return rules;
  }

  /**
   * Check if tool name matches rule pattern
   * @private
   */
  _matchesTool(toolName, pattern) {
    // Exact match
    if (toolName === pattern) return true;

    // Wildcard match (e.g., "Bash(*)" matches "Bash(git status)")
    if (pattern.includes('*')) {
      const regex = new RegExp('^' + pattern.replace(/\*/g, '.*') + '$');
      return regex.test(toolName);
    }

    // Prefix match (e.g., "Bash" matches "Bash(git status)")
    if (toolName.startsWith(pattern + '(')) return true;

    return false;
  }

  /**
   * Create decision result
   * @private
   */
  _createDecision(decision, reason, rule) {
    return {
      decision,
      reason,
      rule: rule?.tool,
      timestamp: new Date(),
    };
  }

  /**
   * Audit a decision
   * @private
   */
  _auditDecision(actorId, toolName, toolInput, decision) {
    const entry = {
      timestamp: new Date(),
      actor: actorId,
      tool: toolName,
      input: toolInput,
      decision: decision.decision,
      rule: decision.rule,
      reason: decision.reason,
    };

    // Generate cryptographic hash for integrity
    entry.hash = this._hashAuditEntry(entry);

    this.auditLog.push(entry);
  }

  /**
   * Generate cryptographic hash for audit entry
   * @private
   */
  _hashAuditEntry(entry) {
    const data = JSON.stringify({
      timestamp: entry.timestamp.toISOString(),
      actor: entry.actor,
      tool: entry.tool,
      decision: entry.decision,
    });
    return createHash('sha256').update(data).digest('hex');
  }
}

/* ========================================================================= */
/* Factory Functions                                                         */
/* ========================================================================= */

/**
 * Create a governance engine with default policy
 * @param {Object} [options] - Engine options
 * @returns {GovernanceEngine} Governance engine
 */
export function createGovernanceEngine(options = {}) {
  const defaultPolicy = {
    id: crypto.randomUUID(),
    name: options.name || 'default-governance',
    version: '1.0.0',
    defaultPermission: options.defaultPermission || 'deny',
    roles: options.roles || [],
    globalRules: options.globalRules || [],
    auditEnabled: options.auditEnabled !== false,
  };

  return new GovernanceEngine(defaultPolicy);
}

/**
 * Create a deny-by-default engine
 * @returns {GovernanceEngine} Governance engine
 */
export function createDenyByDefaultEngine() {
  return createGovernanceEngine({
    name: 'deny-by-default',
    defaultPermission: 'deny',
    auditEnabled: true,
  });
}

/**
 * Create an allow-by-default engine (use with caution)
 * @returns {GovernanceEngine} Governance engine
 */
export function createAllowByDefaultEngine() {
  return createGovernanceEngine({
    name: 'allow-by-default',
    defaultPermission: 'allow',
    auditEnabled: true,
  });
}

/* ========================================================================= */
/* Predefined Roles                                                          */
/* ========================================================================= */

/**
 * Create a read-only role
 * @returns {Object} Role definition
 */
export function createReadOnlyRole() {
  return {
    name: 'read-only',
    description: 'Read-only access to files and code',
    permissions: [
      { tool: 'Read', permission: 'allow', priority: 90 },
      { tool: 'Glob', permission: 'allow', priority: 90 },
      { tool: 'Grep', permission: 'allow', priority: 90 },
      { tool: 'Write', permission: 'deny', priority: 90 },
      { tool: 'Edit', permission: 'deny', priority: 90 },
      { tool: 'Bash', permission: 'deny', priority: 90 },
    ],
  };
}

/**
 * Create a developer role
 * @returns {Object} Role definition
 */
export function createDeveloperRole() {
  return {
    name: 'developer',
    description: 'Full development access with safe guards',
    permissions: [
      { tool: 'Read', permission: 'allow', priority: 80 },
      { tool: 'Write', permission: 'allow', priority: 80 },
      { tool: 'Edit', permission: 'allow', priority: 80 },
      { tool: 'Glob', permission: 'allow', priority: 80 },
      { tool: 'Grep', permission: 'allow', priority: 80 },
      { tool: 'Bash(git:*)', permission: 'allow', priority: 80 },
      { tool: 'Bash(npm:*)', permission: 'allow', priority: 80 },
      { tool: 'Bash(rm:*)', permission: 'ask', priority: 90 },
      { tool: 'Bash(*)', permission: 'ask', priority: 70 },
    ],
  };
}

/**
 * Create an admin role
 * @returns {Object} Role definition
 */
export function createAdminRole() {
  return {
    name: 'admin',
    description: 'Full system access',
    permissions: [
      { tool: '*', permission: 'allow', priority: 60 },
    ],
  };
}

/* ========================================================================= */
/* Exports                                                                   */
/* ========================================================================= */

export default {
  GovernanceEngine,
  PermissionLevel,
  createGovernanceEngine,
  createDenyByDefaultEngine,
  createAllowByDefaultEngine,
  createReadOnlyRole,
  createDeveloperRole,
  createAdminRole,
};
