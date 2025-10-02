/**
 * @file RBAC Authorization Engine
 * @module sidecar/server/utils/rbac
 *
 * Role-Based Access Control with:
 * - Attribute-Based Access Control (ABAC)
 * - Cryptographic proof of authorization
 * - Policy decision caching
 * - Full OTEL tracing
 */

import { createHash, createSign } from 'crypto';
import { trace, context as otelContext } from '@opentelemetry/api';
import logger from './logger.mjs';

const tracer = trace.getTracer('rbac-engine');

/**
 * RBAC Roles Enumeration
 * @enum {string}
 */
export const Roles = {
  ADMIN: 'admin',
  AGENT: 'agent',
  WRITER: 'writer',
  READER: 'reader'
};

/**
 * Permission Actions
 * @enum {string}
 */
export const Actions = {
  READ: 'read',
  WRITE: 'write',
  DELETE: 'delete',
  EXECUTE: 'execute',
  ADMIN: 'admin'
};

/**
 * Resources
 * @enum {string}
 */
export const Resources = {
  KNOWLEDGE_HOOK: 'knowledge_hook',
  EFFECT: 'effect',
  TRANSACTION: 'transaction',
  POLICY: 'policy',
  ROLE: 'role',
  AUDIT_LOG: 'audit_log',
  SYSTEM: 'system'
};

/**
 * Policy Decision Cache
 * TTL: 5 minutes
 */
class PolicyCache {
  constructor(ttlMs = 5 * 60 * 1000) {
    this.cache = new Map();
    this.ttl = ttlMs;
  }

  /**
   * Generate cache key
   * @param {string} userId
   * @param {string} resource
   * @param {string} action
   * @param {object} attributes
   * @returns {string}
   */
  _key(userId, resource, action, attributes) {
    const attrHash = createHash('sha256')
      .update(JSON.stringify(attributes || {}))
      .digest('hex')
      .slice(0, 16);
    return `${userId}:${resource}:${action}:${attrHash}`;
  }

  /**
   * Get cached decision
   * @param {string} userId
   * @param {string} resource
   * @param {string} action
   * @param {object} attributes
   * @returns {object|null}
   */
  get(userId, resource, action, attributes) {
    const key = this._key(userId, resource, action, attributes);
    const entry = this.cache.get(key);

    if (!entry) return null;

    if (Date.now() > entry.expiresAt) {
      this.cache.delete(key);
      return null;
    }

    return entry.decision;
  }

  /**
   * Set cached decision
   * @param {string} userId
   * @param {string} resource
   * @param {string} action
   * @param {object} attributes
   * @param {object} decision
   */
  set(userId, resource, action, attributes, decision) {
    const key = this._key(userId, resource, action, attributes);
    this.cache.set(key, {
      decision,
      expiresAt: Date.now() + this.ttl
    });
  }

  /**
   * Invalidate cache for user
   * @param {string} userId
   */
  invalidateUser(userId) {
    for (const [key] of this.cache) {
      if (key.startsWith(`${userId}:`)) {
        this.cache.delete(key);
      }
    }
  }

  /**
   * Clear all cache
   */
  clear() {
    this.cache.clear();
  }
}

/**
 * RBAC Policy Engine
 */
export class RBACEngine {
  constructor(options = {}) {
    this.policies = new Map();
    this.roleAssignments = new Map(); // userId -> Set<roles>
    this.cache = new PolicyCache(options.cacheTtl);
    this.signingKey = options.signingKey || null;

    // Load default policies
    this._loadDefaultPolicies();
  }

  /**
   * Load default RBAC policies
   * @private
   */
  _loadDefaultPolicies() {
    // Admin: Full access to everything
    this.addPolicy({
      id: 'admin-full-access',
      role: Roles.ADMIN,
      resource: '*',
      action: '*',
      effect: 'allow'
    });

    // Agent: Can register effects and hooks
    this.addPolicy({
      id: 'agent-register-hook',
      role: Roles.AGENT,
      resource: Resources.KNOWLEDGE_HOOK,
      action: Actions.WRITE,
      effect: 'allow'
    });

    this.addPolicy({
      id: 'agent-register-effect',
      role: Roles.AGENT,
      resource: Resources.EFFECT,
      action: Actions.WRITE,
      effect: 'allow'
    });

    this.addPolicy({
      id: 'agent-read',
      role: Roles.AGENT,
      resource: '*',
      action: Actions.READ,
      effect: 'allow'
    });

    // Writer: Can apply transactions and read
    this.addPolicy({
      id: 'writer-transaction',
      role: Roles.WRITER,
      resource: Resources.TRANSACTION,
      action: Actions.WRITE,
      effect: 'allow'
    });

    this.addPolicy({
      id: 'writer-read',
      role: Roles.WRITER,
      resource: '*',
      action: Actions.READ,
      effect: 'allow'
    });

    // Reader: Read-only access
    this.addPolicy({
      id: 'reader-read',
      role: Roles.READER,
      resource: '*',
      action: Actions.READ,
      effect: 'allow'
    });
  }

  /**
   * Add policy to engine
   * @param {object} policy
   */
  addPolicy(policy) {
    const { id, role, resource, action, effect, condition } = policy;

    if (!id || !role || !resource || !action || !effect) {
      throw new Error('Invalid policy: missing required fields');
    }

    this.policies.set(id, {
      role,
      resource,
      action,
      effect, // 'allow' or 'deny'
      condition // optional function for ABAC
    });

    logger.debug('Policy added', { policyId: id, role, resource, action });
  }

  /**
   * Remove policy from engine
   * @param {string} policyId
   */
  removePolicy(policyId) {
    this.policies.delete(policyId);
    this.cache.clear(); // Invalidate all cache
    logger.debug('Policy removed', { policyId });
  }

  /**
   * Assign role to user
   * @param {string} userId
   * @param {string} role
   */
  assignRole(userId, role) {
    if (!Object.values(Roles).includes(role)) {
      throw new Error(`Invalid role: ${role}`);
    }

    if (!this.roleAssignments.has(userId)) {
      this.roleAssignments.set(userId, new Set());
    }

    this.roleAssignments.get(userId).add(role);
    this.cache.invalidateUser(userId);

    logger.info('Role assigned', { userId, role });
  }

  /**
   * Revoke role from user
   * @param {string} userId
   * @param {string} role
   */
  revokeRole(userId, role) {
    if (this.roleAssignments.has(userId)) {
      this.roleAssignments.get(userId).delete(role);
      this.cache.invalidateUser(userId);
      logger.info('Role revoked', { userId, role });
    }
  }

  /**
   * Get user roles
   * @param {string} userId
   * @returns {string[]}
   */
  getUserRoles(userId) {
    return Array.from(this.roleAssignments.get(userId) || []);
  }

  /**
   * Check if user has role
   * @param {string} userId
   * @param {string} role
   * @returns {boolean}
   */
  hasRole(userId, role) {
    return this.roleAssignments.get(userId)?.has(role) || false;
  }

  /**
   * Evaluate policy decision
   * @param {string} userId
   * @param {string} resource
   * @param {string} action
   * @param {object} attributes - Additional attributes for ABAC
   * @returns {Promise<object>} Decision with allow/deny and proof
   */
  async evaluate(userId, resource, action, attributes = {}) {
    return tracer.startActiveSpan('rbac.evaluate', async (span) => {
      try {
        span.setAttributes({
          'rbac.user_id': userId,
          'rbac.resource': resource,
          'rbac.action': action
        });

        // Check cache first
        const cached = this.cache.get(userId, resource, action, attributes);
        if (cached) {
          span.setAttribute('rbac.cache_hit', true);
          return cached;
        }

        span.setAttribute('rbac.cache_hit', false);

        // Get user roles
        const userRoles = this.getUserRoles(userId);

        if (userRoles.length === 0) {
          const decision = this._createDecision(false, 'No roles assigned', userId, resource, action);
          this.cache.set(userId, resource, action, attributes, decision);
          return decision;
        }

        // Evaluate policies
        let allowed = false;
        let matchedPolicies = [];

        for (const [policyId, policy] of this.policies) {
          // Check if policy applies to user's roles
          if (!userRoles.includes(policy.role)) continue;

          // Check resource match (wildcard support)
          if (policy.resource !== '*' && policy.resource !== resource) continue;

          // Check action match (wildcard support)
          if (policy.action !== '*' && policy.action !== action) continue;

          // Check ABAC condition if present
          if (policy.condition) {
            const conditionResult = await policy.condition({
              userId,
              resource,
              action,
              attributes,
              roles: userRoles
            });

            if (!conditionResult) continue;
          }

          matchedPolicies.push(policyId);

          // Deny takes precedence
          if (policy.effect === 'deny') {
            allowed = false;
            break;
          }

          if (policy.effect === 'allow') {
            allowed = true;
          }
        }

        const decision = this._createDecision(
          allowed,
          allowed ? 'Access granted' : 'Access denied',
          userId,
          resource,
          action,
          matchedPolicies
        );

        // Cache decision
        this.cache.set(userId, resource, action, attributes, decision);

        span.setAttribute('rbac.decision', allowed ? 'allow' : 'deny');
        span.setAttribute('rbac.matched_policies', matchedPolicies.length);

        return decision;
      } catch (error) {
        span.recordException(error);
        logger.error('RBAC evaluation error', { error: error.message, userId, resource, action });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Create decision object with cryptographic proof
   * @param {boolean} allowed
   * @param {string} reason
   * @param {string} userId
   * @param {string} resource
   * @param {string} action
   * @param {string[]} policies
   * @returns {object}
   * @private
   */
  _createDecision(allowed, reason, userId, resource, action, policies = []) {
    const decision = {
      allowed,
      reason,
      userId,
      resource,
      action,
      policies,
      timestamp: new Date().toISOString(),
      decisionId: createHash('sha256')
        .update(`${userId}:${resource}:${action}:${Date.now()}`)
        .digest('hex')
        .slice(0, 16)
    };

    // Add cryptographic signature if signing key available
    if (this.signingKey) {
      decision.signature = this._signDecision(decision);
    }

    return decision;
  }

  /**
   * Sign decision with private key
   * @param {object} decision
   * @returns {string}
   * @private
   */
  _signDecision(decision) {
    const sign = createSign('SHA256');
    const payload = JSON.stringify({
      allowed: decision.allowed,
      userId: decision.userId,
      resource: decision.resource,
      action: decision.action,
      timestamp: decision.timestamp,
      decisionId: decision.decisionId
    });

    sign.update(payload);
    sign.end();

    return sign.sign(this.signingKey, 'hex');
  }

  /**
   * Require permission (throws if denied)
   * @param {string} userId
   * @param {string} resource
   * @param {string} action
   * @param {object} attributes
   * @throws {Error} If access denied
   */
  async require(userId, resource, action, attributes = {}) {
    const decision = await this.evaluate(userId, resource, action, attributes);

    if (!decision.allowed) {
      const error = new Error(`Access denied: ${decision.reason}`);
      error.code = 'EACCES';
      error.decision = decision;
      throw error;
    }

    return decision;
  }

  /**
   * Get all policies
   * @returns {object[]}
   */
  getAllPolicies() {
    return Array.from(this.policies.entries()).map(([id, policy]) => ({
      id,
      ...policy
    }));
  }

  /**
   * Get role assignments
   * @returns {object}
   */
  getRoleAssignments() {
    const assignments = {};
    for (const [userId, roles] of this.roleAssignments) {
      assignments[userId] = Array.from(roles);
    }
    return assignments;
  }
}

// Singleton instance
let rbacInstance = null;

/**
 * Get RBAC engine instance
 * @param {object} options
 * @returns {RBACEngine}
 */
export function getRBACEngine(options = {}) {
  if (!rbacInstance) {
    rbacInstance = new RBACEngine(options);
  }
  return rbacInstance;
}

/**
 * Initialize RBAC engine with configuration
 * @param {object} config
 * @returns {RBACEngine}
 */
export function initializeRBAC(config = {}) {
  rbacInstance = new RBACEngine(config);
  return rbacInstance;
}

export default { getRBACEngine, initializeRBAC, Roles, Actions, Resources };
