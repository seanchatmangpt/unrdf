/**
 * @file Default RBAC Policies
 * @module sidecar/server/policies/default-rbac-policy
 *
 * Defines default Role-Based Access Control policies:
 * - Admin: Full system access
 * - Agent: Register effects/hooks, read access
 * - Writer: Apply transactions, read access
 * - Reader: Read-only access
 */

import { Roles, Actions, Resources } from '../utils/rbac.mjs';

/**
 * Default RBAC Policy Definitions
 *
 * Policy Structure:
 * - id: Unique policy identifier
 * - role: Role this policy applies to
 * - resource: Resource type (or '*' for all)
 * - action: Action type (or '*' for all)
 * - effect: 'allow' or 'deny'
 * - condition: Optional function for ABAC evaluation
 */
export const defaultPolicies = [
  // ============================================
  // ADMIN POLICIES - Full System Access
  // ============================================
  {
    id: 'admin-full-access',
    role: Roles.ADMIN,
    resource: '*',
    action: '*',
    effect: 'allow',
    description: 'Admins have unrestricted access to all resources'
  },

  // ============================================
  // AGENT POLICIES - Effect/Hook Registration
  // ============================================
  {
    id: 'agent-register-hook',
    role: Roles.AGENT,
    resource: Resources.KNOWLEDGE_HOOK,
    action: Actions.WRITE,
    effect: 'allow',
    description: 'Agents can register knowledge hooks'
  },
  {
    id: 'agent-update-hook',
    role: Roles.AGENT,
    resource: Resources.KNOWLEDGE_HOOK,
    action: Actions.WRITE,
    effect: 'allow',
    condition: async ({ userId, attributes }) => {
      // Agents can only update their own hooks
      const hookOwnerId = attributes?.body?.ownerId || attributes?.ownerId;
      return hookOwnerId === userId;
    },
    description: 'Agents can update their own knowledge hooks'
  },
  {
    id: 'agent-delete-hook',
    role: Roles.AGENT,
    resource: Resources.KNOWLEDGE_HOOK,
    action: Actions.DELETE,
    effect: 'allow',
    condition: async ({ userId, attributes }) => {
      // Agents can only delete their own hooks
      const hookOwnerId = attributes?.body?.ownerId || attributes?.ownerId;
      return hookOwnerId === userId;
    },
    description: 'Agents can delete their own knowledge hooks'
  },
  {
    id: 'agent-register-effect',
    role: Roles.AGENT,
    resource: Resources.EFFECT,
    action: Actions.WRITE,
    effect: 'allow',
    description: 'Agents can register effects'
  },
  {
    id: 'agent-execute-effect',
    role: Roles.AGENT,
    resource: Resources.EFFECT,
    action: Actions.EXECUTE,
    effect: 'allow',
    description: 'Agents can execute effects'
  },
  {
    id: 'agent-read-all',
    role: Roles.AGENT,
    resource: '*',
    action: Actions.READ,
    effect: 'allow',
    description: 'Agents have read access to all resources'
  },
  {
    id: 'agent-read-audit',
    role: Roles.AGENT,
    resource: Resources.AUDIT_LOG,
    action: Actions.READ,
    effect: 'allow',
    condition: async ({ userId, attributes }) => {
      // Agents can only read their own audit logs
      const logUserId = attributes?.query?.userId || attributes?.userId;
      return logUserId === userId || !logUserId; // Allow if querying own logs or general query
    },
    description: 'Agents can read their own audit logs'
  },

  // ============================================
  // WRITER POLICIES - Transaction Application
  // ============================================
  {
    id: 'writer-apply-transaction',
    role: Roles.WRITER,
    resource: Resources.TRANSACTION,
    action: Actions.WRITE,
    effect: 'allow',
    description: 'Writers can apply transactions'
  },
  {
    id: 'writer-read-transaction',
    role: Roles.WRITER,
    resource: Resources.TRANSACTION,
    action: Actions.READ,
    effect: 'allow',
    description: 'Writers can read transactions'
  },
  {
    id: 'writer-read-hooks',
    role: Roles.WRITER,
    resource: Resources.KNOWLEDGE_HOOK,
    action: Actions.READ,
    effect: 'allow',
    description: 'Writers can read knowledge hooks'
  },
  {
    id: 'writer-read-effects',
    role: Roles.WRITER,
    resource: Resources.EFFECT,
    action: Actions.READ,
    effect: 'allow',
    description: 'Writers can read effects'
  },
  {
    id: 'writer-read-audit',
    role: Roles.WRITER,
    resource: Resources.AUDIT_LOG,
    action: Actions.READ,
    effect: 'allow',
    condition: async ({ userId, attributes }) => {
      // Writers can only read their own audit logs
      const logUserId = attributes?.query?.userId || attributes?.userId;
      return logUserId === userId || !logUserId;
    },
    description: 'Writers can read their own audit logs'
  },

  // ============================================
  // READER POLICIES - Read-Only Access
  // ============================================
  {
    id: 'reader-read-all',
    role: Roles.READER,
    resource: '*',
    action: Actions.READ,
    effect: 'allow',
    description: 'Readers have read-only access to all resources'
  },
  {
    id: 'reader-no-write',
    role: Roles.READER,
    resource: '*',
    action: Actions.WRITE,
    effect: 'deny',
    description: 'Readers cannot write to any resource'
  },
  {
    id: 'reader-no-delete',
    role: Roles.READER,
    resource: '*',
    action: Actions.DELETE,
    effect: 'deny',
    description: 'Readers cannot delete any resource'
  },
  {
    id: 'reader-no-execute',
    role: Roles.READER,
    resource: '*',
    action: Actions.EXECUTE,
    effect: 'deny',
    description: 'Readers cannot execute any resource'
  },

  // ============================================
  // POLICY MANAGEMENT - Admin Only
  // ============================================
  {
    id: 'policy-admin-only',
    role: Roles.ADMIN,
    resource: Resources.POLICY,
    action: '*',
    effect: 'allow',
    description: 'Only admins can manage policies'
  },
  {
    id: 'policy-deny-non-admin',
    role: '*',
    resource: Resources.POLICY,
    action: '*',
    effect: 'deny',
    condition: async ({ roles }) => {
      return !roles.includes(Roles.ADMIN);
    },
    description: 'Non-admins cannot manage policies'
  },

  // ============================================
  // ROLE MANAGEMENT - Admin Only
  // ============================================
  {
    id: 'role-admin-only',
    role: Roles.ADMIN,
    resource: Resources.ROLE,
    action: '*',
    effect: 'allow',
    description: 'Only admins can manage role assignments'
  },
  {
    id: 'role-deny-non-admin',
    role: '*',
    resource: Resources.ROLE,
    action: '*',
    effect: 'deny',
    condition: async ({ roles }) => {
      return !roles.includes(Roles.ADMIN);
    },
    description: 'Non-admins cannot manage role assignments'
  },

  // ============================================
  // SYSTEM RESOURCES - Admin Only
  // ============================================
  {
    id: 'system-admin-only',
    role: Roles.ADMIN,
    resource: Resources.SYSTEM,
    action: '*',
    effect: 'allow',
    description: 'Only admins can access system resources'
  }
];

/**
 * Load default policies into RBAC engine
 * @param {RBACEngine} rbacEngine
 */
export function loadDefaultPolicies(rbacEngine) {
  for (const policy of defaultPolicies) {
    rbacEngine.addPolicy(policy);
  }
}

/**
 * Get policy by ID
 * @param {string} policyId
 * @returns {object|null}
 */
export function getPolicyById(policyId) {
  return defaultPolicies.find(p => p.id === policyId) || null;
}

/**
 * Get policies by role
 * @param {string} role
 * @returns {object[]}
 */
export function getPoliciesByRole(role) {
  return defaultPolicies.filter(p => p.role === role || p.role === '*');
}

/**
 * Get policies by resource
 * @param {string} resource
 * @returns {object[]}
 */
export function getPoliciesByResource(resource) {
  return defaultPolicies.filter(p => p.resource === resource || p.resource === '*');
}

export default {
  defaultPolicies,
  loadDefaultPolicies,
  getPolicyById,
  getPoliciesByRole,
  getPoliciesByResource
};
