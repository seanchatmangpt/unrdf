/**
 * @file RBAC Engine Tests
 * @module sidecar/test/unit/rbac
 *
 * Tests for Role-Based Access Control:
 * - Role assignment and revocation
 * - Policy evaluation
 * - Permission checks
 * - Access denial
 * - Policy caching
 * - ABAC conditions
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { RBACEngine, Roles, Actions, Resources } from '../../server/utils/rbac.mjs';

describe('RBAC Engine', () => {
  /** @type {RBACEngine} */
  let rbac;

  beforeEach(() => {
    rbac = new RBACEngine();
  });

  describe('Role Management', () => {
    it('should assign role to user', () => {
      rbac.assignRole('user1', Roles.ADMIN);

      expect(rbac.hasRole('user1', Roles.ADMIN)).toBe(true);
      expect(rbac.getUserRoles('user1')).toContain(Roles.ADMIN);
    });

    it('should assign multiple roles to user', () => {
      rbac.assignRole('user1', Roles.AGENT);
      rbac.assignRole('user1', Roles.WRITER);

      const roles = rbac.getUserRoles('user1');
      expect(roles).toContain(Roles.AGENT);
      expect(roles).toContain(Roles.WRITER);
      expect(roles.length).toBe(2);
    });

    it('should revoke role from user', () => {
      rbac.assignRole('user1', Roles.ADMIN);
      rbac.revokeRole('user1', Roles.ADMIN);

      expect(rbac.hasRole('user1', Roles.ADMIN)).toBe(false);
    });

    it('should throw error for invalid role', () => {
      expect(() => {
        rbac.assignRole('user1', 'invalid-role');
      }).toThrow('Invalid role');
    });

    it('should return empty array for user with no roles', () => {
      expect(rbac.getUserRoles('user1')).toEqual([]);
    });
  });

  describe('Policy Evaluation - Admin', () => {
    beforeEach(() => {
      rbac.assignRole('admin1', Roles.ADMIN);
    });

    it('should allow admin full access to all resources', async () => {
      const decision = await rbac.evaluate('admin1', Resources.KNOWLEDGE_HOOK, Actions.WRITE);

      expect(decision.allowed).toBe(true);
      expect(decision.userId).toBe('admin1');
    });

    it('should allow admin to delete any resource', async () => {
      const decision = await rbac.evaluate('admin1', Resources.TRANSACTION, Actions.DELETE);

      expect(decision.allowed).toBe(true);
    });

    it('should allow admin access to system resources', async () => {
      const decision = await rbac.evaluate('admin1', Resources.SYSTEM, Actions.ADMIN);

      expect(decision.allowed).toBe(true);
    });

    it('should include decision metadata', async () => {
      const decision = await rbac.evaluate('admin1', Resources.ROLE, Actions.WRITE);

      expect(decision).toHaveProperty('decisionId');
      expect(decision).toHaveProperty('timestamp');
      expect(decision).toHaveProperty('policies');
      expect(decision.policies.length).toBeGreaterThan(0);
    });
  });

  describe('Policy Evaluation - Agent', () => {
    beforeEach(() => {
      rbac.assignRole('agent1', Roles.AGENT);
    });

    it('should allow agent to register knowledge hooks', async () => {
      const decision = await rbac.evaluate('agent1', Resources.KNOWLEDGE_HOOK, Actions.WRITE);

      expect(decision.allowed).toBe(true);
    });

    it('should allow agent to register effects', async () => {
      const decision = await rbac.evaluate('agent1', Resources.EFFECT, Actions.WRITE);

      expect(decision.allowed).toBe(true);
    });

    it('should allow agent to read all resources', async () => {
      const decision = await rbac.evaluate('agent1', Resources.TRANSACTION, Actions.READ);

      expect(decision.allowed).toBe(true);
    });

    it('should deny agent from applying transactions', async () => {
      const decision = await rbac.evaluate('agent1', Resources.TRANSACTION, Actions.WRITE);

      expect(decision.allowed).toBe(false);
      expect(decision.reason).toBe('Access denied');
    });

    it('should deny agent from managing roles', async () => {
      const decision = await rbac.evaluate('agent1', Resources.ROLE, Actions.WRITE);

      expect(decision.allowed).toBe(false);
    });
  });

  describe('Policy Evaluation - Writer', () => {
    beforeEach(() => {
      rbac.assignRole('writer1', Roles.WRITER);
    });

    it('should allow writer to apply transactions', async () => {
      const decision = await rbac.evaluate('writer1', Resources.TRANSACTION, Actions.WRITE);

      expect(decision.allowed).toBe(true);
    });

    it('should allow writer to read resources', async () => {
      const decision = await rbac.evaluate('writer1', Resources.KNOWLEDGE_HOOK, Actions.READ);

      expect(decision.allowed).toBe(true);
    });

    it('should deny writer from registering hooks', async () => {
      const decision = await rbac.evaluate('writer1', Resources.KNOWLEDGE_HOOK, Actions.WRITE);

      expect(decision.allowed).toBe(false);
    });

    it('should deny writer from deleting resources', async () => {
      const decision = await rbac.evaluate('writer1', Resources.TRANSACTION, Actions.DELETE);

      expect(decision.allowed).toBe(false);
    });
  });

  describe('Policy Evaluation - Reader', () => {
    beforeEach(() => {
      rbac.assignRole('reader1', Roles.READER);
    });

    it('should allow reader to read all resources', async () => {
      const decision = await rbac.evaluate('reader1', Resources.KNOWLEDGE_HOOK, Actions.READ);

      expect(decision.allowed).toBe(true);
    });

    it('should deny reader from writing', async () => {
      const decision = await rbac.evaluate('reader1', Resources.TRANSACTION, Actions.WRITE);

      expect(decision.allowed).toBe(false);
    });

    it('should deny reader from deleting', async () => {
      const decision = await rbac.evaluate('reader1', Resources.EFFECT, Actions.DELETE);

      expect(decision.allowed).toBe(false);
    });

    it('should deny reader from executing', async () => {
      const decision = await rbac.evaluate('reader1', Resources.EFFECT, Actions.EXECUTE);

      expect(decision.allowed).toBe(false);
    });
  });

  describe('Policy Decision Caching', () => {
    beforeEach(() => {
      rbac.assignRole('user1', Roles.READER);
    });

    it('should cache policy decisions', async () => {
      const decision1 = await rbac.evaluate('user1', Resources.TRANSACTION, Actions.READ);
      const decision2 = await rbac.evaluate('user1', Resources.TRANSACTION, Actions.READ);

      // Same decision ID indicates cache hit
      expect(decision1.decisionId).toBe(decision2.decisionId);
    });

    it('should invalidate cache on role change', async () => {
      const decision1 = await rbac.evaluate('user1', Resources.TRANSACTION, Actions.WRITE);
      expect(decision1.allowed).toBe(false);

      rbac.assignRole('user1', Roles.WRITER);

      const decision2 = await rbac.evaluate('user1', Resources.TRANSACTION, Actions.WRITE);
      expect(decision2.allowed).toBe(true);
    });

    it('should invalidate cache on role revocation', async () => {
      rbac.assignRole('user1', Roles.ADMIN);

      const decision1 = await rbac.evaluate('user1', Resources.SYSTEM, Actions.ADMIN);
      expect(decision1.allowed).toBe(true);

      rbac.revokeRole('user1', Roles.ADMIN);

      const decision2 = await rbac.evaluate('user1', Resources.SYSTEM, Actions.ADMIN);
      expect(decision2.allowed).toBe(false);
    });
  });

  describe('Attribute-Based Access Control (ABAC)', () => {
    it('should evaluate conditions for policy decisions', async () => {
      // Add policy with condition
      rbac.addPolicy({
        id: 'test-abac-condition',
        role: Roles.AGENT,
        resource: 'test-resource',
        action: Actions.WRITE,
        effect: 'allow',
        condition: async ({ attributes }) => {
          return attributes?.test === true;
        }
      });

      rbac.assignRole('agent1', Roles.AGENT);

      // Without condition attribute
      const decision1 = await rbac.evaluate('agent1', 'test-resource', Actions.WRITE, {});
      expect(decision1.allowed).toBe(false);

      // With condition attribute
      const decision2 = await rbac.evaluate('agent1', 'test-resource', Actions.WRITE, { test: true });
      expect(decision2.allowed).toBe(true);
    });

    it('should support ownership-based access control', async () => {
      rbac.addPolicy({
        id: 'test-ownership',
        role: Roles.AGENT,
        resource: 'owned-resource',
        action: Actions.DELETE,
        effect: 'allow',
        condition: async ({ userId, attributes }) => {
          return attributes?.ownerId === userId;
        }
      });

      rbac.assignRole('agent1', Roles.AGENT);

      // Not owner
      const decision1 = await rbac.evaluate('agent1', 'owned-resource', Actions.DELETE, { ownerId: 'other-user' });
      expect(decision1.allowed).toBe(false);

      // Is owner
      const decision2 = await rbac.evaluate('agent1', 'owned-resource', Actions.DELETE, { ownerId: 'agent1' });
      expect(decision2.allowed).toBe(true);
    });
  });

  describe('Permission Requirements', () => {
    beforeEach(() => {
      rbac.assignRole('user1', Roles.READER);
    });

    it('should not throw when permission granted', async () => {
      await expect(
        rbac.require('user1', Resources.TRANSACTION, Actions.READ)
      ).resolves.toBeDefined();
    });

    it('should throw when permission denied', async () => {
      await expect(
        rbac.require('user1', Resources.TRANSACTION, Actions.WRITE)
      ).rejects.toThrow('Access denied');
    });

    it('should include decision in error', async () => {
      try {
        await rbac.require('user1', Resources.TRANSACTION, Actions.DELETE);
        expect.fail('Should have thrown');
      } catch (error) {
        expect(error.code).toBe('EACCES');
        expect(error.decision).toBeDefined();
        expect(error.decision.allowed).toBe(false);
      }
    });
  });

  describe('Policy Management', () => {
    it('should add custom policy', () => {
      rbac.addPolicy({
        id: 'custom-policy',
        role: Roles.AGENT,
        resource: 'custom-resource',
        action: Actions.READ,
        effect: 'allow'
      });

      const policies = rbac.getAllPolicies();
      expect(policies.some(p => p.id === 'custom-policy')).toBe(true);
    });

    it('should remove policy', () => {
      rbac.addPolicy({
        id: 'removable-policy',
        role: Roles.AGENT,
        resource: 'temp-resource',
        action: Actions.READ,
        effect: 'allow'
      });

      rbac.removePolicy('removable-policy');

      const policies = rbac.getAllPolicies();
      expect(policies.some(p => p.id === 'removable-policy')).toBe(false);
    });

    it('should throw error for invalid policy', () => {
      expect(() => {
        rbac.addPolicy({
          id: 'invalid-policy',
          // Missing required fields
        });
      }).toThrow('Invalid policy');
    });

    it('should get all policies', () => {
      const policies = rbac.getAllPolicies();

      expect(Array.isArray(policies)).toBe(true);
      expect(policies.length).toBeGreaterThan(0);
      expect(policies[0]).toHaveProperty('id');
      expect(policies[0]).toHaveProperty('role');
      expect(policies[0]).toHaveProperty('resource');
      expect(policies[0]).toHaveProperty('action');
      expect(policies[0]).toHaveProperty('effect');
    });

    it('should get role assignments', () => {
      rbac.assignRole('user1', Roles.ADMIN);
      rbac.assignRole('user2', Roles.AGENT);
      rbac.assignRole('user2', Roles.WRITER);

      const assignments = rbac.getRoleAssignments();

      expect(assignments.user1).toContain(Roles.ADMIN);
      expect(assignments.user2).toContain(Roles.AGENT);
      expect(assignments.user2).toContain(Roles.WRITER);
    });
  });

  describe('No Roles Edge Cases', () => {
    it('should deny access when user has no roles', async () => {
      const decision = await rbac.evaluate('user-no-roles', Resources.TRANSACTION, Actions.READ);

      expect(decision.allowed).toBe(false);
      expect(decision.reason).toBe('No roles assigned');
    });

    it('should return empty roles for non-existent user', () => {
      expect(rbac.getUserRoles('non-existent-user')).toEqual([]);
    });

    it('should handle role check for non-existent user', () => {
      expect(rbac.hasRole('non-existent-user', Roles.ADMIN)).toBe(false);
    });
  });

  describe('Deny Precedence', () => {
    it('should prioritize deny over allow', async () => {
      rbac.addPolicy({
        id: 'allow-policy',
        role: Roles.AGENT,
        resource: 'test-resource',
        action: Actions.WRITE,
        effect: 'allow'
      });

      rbac.addPolicy({
        id: 'deny-policy',
        role: Roles.AGENT,
        resource: 'test-resource',
        action: Actions.WRITE,
        effect: 'deny'
      });

      rbac.assignRole('agent1', Roles.AGENT);

      const decision = await rbac.evaluate('agent1', 'test-resource', Actions.WRITE);

      expect(decision.allowed).toBe(false);
    });
  });
});
