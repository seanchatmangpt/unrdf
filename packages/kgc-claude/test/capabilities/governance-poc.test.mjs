/**
 * @file Proof of Concept - Governance Capabilities
 * @description Demonstrates all three governance modules working together
 */

import { describe, it, expect } from 'vitest';
import {
  GovernanceEngine,
  createGovernanceEngine,
  createDeveloperRole,
  createReadOnlyRole,
  createAdminRole,
} from '../../src/capabilities/governance-engine.mjs';

import {
  HookComposer,
  createHookComposer,
  createBeforeHook,
  createAfterHook,
  createAroundHook,
  createErrorHook,
} from '../../src/capabilities/hook-composition.mjs';

import {
  PolicyEnforcer,
  createDenyByDefaultEnforcer,
  createAllowlistEnforcer,
  createSafeDevelopmentPolicy,
  createReadOnlyPolicy,
} from '../../src/capabilities/policy-enforcer.mjs';

describe('Agent 2: Hooks & Governance POC', () => {
  describe('1. Governance Engine - Permission Matrices', () => {
    it('should create a governance engine with RBAC', () => {
      const engine = createGovernanceEngine({
        name: 'test-governance',
        defaultPermission: 'deny',
        roles: [createDeveloperRole(), createReadOnlyRole()],
      });

      // Register actors
      engine.registerActor({
        id: 'dev-1',
        name: 'Developer 1',
        roles: ['developer'],
      });

      engine.registerActor({
        id: 'viewer-1',
        name: 'Viewer 1',
        roles: ['read-only'],
      });

      // Test developer permissions
      const devCanWrite = engine.checkPermission('dev-1', 'Write', {});
      expect(devCanWrite.decision).toBe('allow');

      const devCanGit = engine.checkPermission('dev-1', 'Bash(git status)', {});
      expect(devCanGit.decision).toBe('allow');

      // Test read-only permissions
      const viewerCanRead = engine.checkPermission('viewer-1', 'Read', {});
      expect(viewerCanRead.decision).toBe('allow');

      const viewerCanWrite = engine.checkPermission('viewer-1', 'Write', {});
      expect(viewerCanWrite.decision).toBe('deny');

      // Get permission matrix
      const devMatrix = engine.getPermissionMatrix('dev-1');
      expect(devMatrix.permissions.Write.level).toBe('allow');
      expect(devMatrix.permissions.Read.level).toBe('allow');

      const viewerMatrix = engine.getPermissionMatrix('viewer-1');
      expect(viewerMatrix.permissions.Write.level).toBe('deny');
      expect(viewerMatrix.permissions.Read.level).toBe('allow');

      // Get audit log
      const auditLog = engine.getAuditLog();
      expect(auditLog.length).toBeGreaterThan(0);

      const stats = engine.getStats();
      expect(stats.actors).toBe(2);
    });

    it('should enforce role inheritance', () => {
      const adminRole = createAdminRole();
      const customRole = {
        name: 'custom-admin',
        description: 'Custom admin with specific restrictions',
        inherits: ['admin'],
        permissions: [
          {
            tool: 'Bash(rm:*)',
            permission: 'deny',
            priority: 95,
            reason: 'No rm commands for this admin',
          },
        ],
      };

      const engine = createGovernanceEngine({
        name: 'test-inheritance',
        defaultPermission: 'deny',
        roles: [adminRole, customRole],
      });

      engine.registerActor({
        id: 'custom-admin-1',
        name: 'Custom Admin',
        roles: ['custom-admin'],
      });

      // Should inherit admin allow-all
      const canRead = engine.checkPermission('custom-admin-1', 'Read', {});
      expect(canRead.decision).toBe('allow');

      // But specific denial should override
      const canRm = engine.checkPermission('custom-admin-1', 'Bash(rm -rf /)', {});
      expect(canRm.decision).toBe('deny');
    });
  });

  describe('2. Hook Composition - Before/After/Around', () => {
    it('should execute hooks in correct order', async () => {
      const executionLog = [];

      const composer = createHookComposer('test-composition', [
        createBeforeHook('before-1', async context => {
          executionLog.push('before-1');
          context.beforeRan = true;
        }),
        createAfterHook('after-1', async context => {
          executionLog.push('after-1');
          expect(context.result).toBe('target-result');
        }),
        createAroundHook('around-1', async (context, targetFn) => {
          executionLog.push('around-before');
          const result = await targetFn(context);
          executionLog.push('around-after');
          return result;
        }),
      ]);

      const targetFn = async context => {
        executionLog.push('target');
        expect(context.beforeRan).toBe(true);
        return 'target-result';
      };

      const result = await composer.execute(targetFn, {});

      expect(result.success).toBe(true);
      expect(result.value).toBe('target-result');
      expect(executionLog).toEqual([
        'before-1',
        'around-before',
        'target',
        'around-after',
        'after-1',
      ]);
    });

    it('should handle errors with error hooks', async () => {
      let errorHandled = false;

      const composer = createHookComposer('error-handling', [
        createErrorHook('handle-error', async context => {
          errorHandled = true;
          expect(context.error).toBeDefined();
          return { recovered: true, value: 'recovered' };
        }),
      ]);

      const targetFn = async () => {
        throw new Error('Test error');
      };

      const result = await composer.execute(targetFn, {});

      expect(errorHandled).toBe(true);
      expect(result.success).toBe(true);
      expect(result.value).toBe('recovered');
      expect(result.recovered).toBe(true);
    });

    it('should respect hook priorities', async () => {
      const executionOrder = [];

      const composer = createHookComposer('priority-test', [
        createBeforeHook(
          'low-priority',
          async () => executionOrder.push('low'),
          { priority: 10 }
        ),
        createBeforeHook(
          'high-priority',
          async () => executionOrder.push('high'),
          { priority: 90 }
        ),
        createBeforeHook(
          'medium-priority',
          async () => executionOrder.push('medium'),
          { priority: 50 }
        ),
      ]);

      await composer.execute(async () => {}, {});

      expect(executionOrder).toEqual(['high', 'medium', 'low']);
    });

    it('should support conditional hooks', async () => {
      let conditionalRan = false;
      let alwaysRan = false;

      const composer = createHookComposer('conditional-test', [
        createBeforeHook(
          'conditional',
          async () => {
            conditionalRan = true;
          },
          {
            condition: context => context.runConditional === true,
          }
        ),
        createBeforeHook('always', async () => {
          alwaysRan = true;
        }),
      ]);

      // First execution - condition false
      await composer.execute(async () => {}, { runConditional: false });
      expect(conditionalRan).toBe(false);
      expect(alwaysRan).toBe(true);

      // Reset
      alwaysRan = false;

      // Second execution - condition true
      await composer.execute(async () => {}, { runConditional: true });
      expect(conditionalRan).toBe(true);
      expect(alwaysRan).toBe(true);
    });
  });

  describe('3. Policy Enforcer - Deny-by-Default', () => {
    it('should enforce deny-by-default policy', () => {
      const enforcer = createDenyByDefaultEnforcer('test-enforcer');

      // Default deny
      const result1 = enforcer.enforce('Write', { file: 'test.txt' });
      expect(result1.allowed).toBe(false);
      expect(result1.action).toBe('deny');
      expect(result1.receipt).toBeDefined();

      // Add to allowlist
      enforcer.addToAllowlist('Write');

      // Now allowed
      const result2 = enforcer.enforce('Write', { file: 'test.txt' });
      expect(result2.allowed).toBe(true);
      expect(result2.action).toBe('allow');
    });

    it('should create violation receipts with cryptographic proof', () => {
      const enforcer = createDenyByDefaultEnforcer('receipt-test');

      const result = enforcer.enforce('Bash', { command: 'rm -rf /' });

      expect(result.allowed).toBe(false);
      expect(result.receipt).toBeDefined();
      expect(result.receipt.id).toBeDefined();
      expect(result.receipt.hash).toBeDefined();
      expect(result.receipt.timestamp).toBeInstanceOf(Date);
      expect(result.receipt.evidence).toBeDefined();
      expect(result.receipt.evidence.length).toBeGreaterThan(0);

      // Verify receipt integrity
      const violations = enforcer.getViolations();
      expect(violations.length).toBe(1);
      expect(violations[0].hash).toBe(result.receipt.hash);
    });

    it('should support allowlist enforcement', () => {
      const enforcer = createAllowlistEnforcer('allowlist-test', [
        'Read',
        'Grep',
        'Glob',
      ]);

      // Allowed tools
      expect(enforcer.enforce('Read').allowed).toBe(true);
      expect(enforcer.enforce('Grep').allowed).toBe(true);
      expect(enforcer.enforce('Glob').allowed).toBe(true);

      // Not in allowlist - denied
      expect(enforcer.enforce('Write').allowed).toBe(false);
      expect(enforcer.enforce('Bash').allowed).toBe(false);
    });

    it('should support pattern matching', () => {
      const policy = createSafeDevelopmentPolicy();
      const enforcer = new PolicyEnforcer(policy);

      // Git allowed
      const gitStatus = enforcer.enforce('Bash', { command: 'git status' });
      expect(gitStatus.allowed).toBe(true);

      // NPM allowed
      const npmInstall = enforcer.enforce('Bash', { command: 'npm install' });
      expect(npmInstall.allowed).toBe(true);

      // Dangerous rm blocked
      const rmRf = enforcer.enforce('Bash', { command: 'rm -rf /' });
      expect(rmRf.allowed).toBe(false);
      expect(rmRf.action).toBe('block');
    });

    it('should generate compliance reports', () => {
      const enforcer = createDenyByDefaultEnforcer('compliance-test');

      // Simulate enforcement
      enforcer.addToAllowlist('Read');
      enforcer.enforce('Read'); // allowed
      enforcer.enforce('Write'); // denied
      enforcer.enforce('Write'); // denied
      enforcer.enforce('Bash'); // denied

      const report = enforcer.getComplianceReport();

      expect(report.stats.enforcements).toBe(4);
      expect(report.stats.allows).toBe(1);
      expect(report.stats.denies).toBe(3);
      expect(report.stats.complianceRate).toBe(25); // 1/4 = 25%
      expect(report.violations.total).toBe(3);
    });
  });

  describe('4. Integration - All Three Modules', () => {
    it('should integrate governance, hooks, and enforcement', async () => {
      // 1. Create governance engine
      const governance = createGovernanceEngine({
        name: 'integrated-governance',
        defaultPermission: 'deny',
        roles: [createDeveloperRole()],
      });

      governance.registerActor({
        id: 'dev-1',
        roles: ['developer'],
      });

      // 2. Create policy enforcer
      const enforcer = createDenyByDefaultEnforcer('integrated-enforcer');
      enforcer.addToAllowlist('Read');
      enforcer.addToAllowlist('Write');

      // 3. Create hook composer with governance and enforcement
      const composer = createHookComposer('integrated-hooks', [
        // Before hook: Check governance permission
        createBeforeHook(
          'check-governance',
          async context => {
            const decision = governance.checkPermission(
              context.actorId,
              context.tool,
              context.input
            );
            context.governanceDecision = decision;

            if (decision.decision === 'deny') {
              throw new Error(`Governance denied: ${decision.reason}`);
            }
          },
          { priority: 90 }
        ),

        // Before hook: Enforce policy
        createBeforeHook(
          'enforce-policy',
          async context => {
            const result = enforcer.enforce(context.tool, context.input);
            context.enforcementResult = result;

            if (!result.allowed) {
              throw new Error(`Policy denied: ${result.reason}`);
            }
          },
          { priority: 80 }
        ),

        // After hook: Audit
        createAfterHook('audit', async context => {
          context.audited = true;
        }),

        // Error hook: Log violations
        createErrorHook('log-violation', async context => {
          console.log('Violation:', context.error.message);
        }),
      ]);

      // Test allowed operation
      const allowedOp = await composer.execute(
        async context => {
          return `Executed ${context.tool}`;
        },
        {
          actorId: 'dev-1',
          tool: 'Write',
          input: { file: 'test.txt' },
        }
      );

      expect(allowedOp.success).toBe(true);
      expect(allowedOp.beforeResults).toHaveLength(2);
      expect(allowedOp.afterResults).toHaveLength(1);

      // Test denied operation
      const deniedOp = await composer.execute(
        async context => {
          return `Executed ${context.tool}`;
        },
        {
          actorId: 'dev-1',
          tool: 'Bash',
          input: { command: 'dangerous-command' },
        }
      );

      expect(deniedOp.success).toBe(false);
      expect(deniedOp.error).toContain('denied');
    });

    it('should produce comprehensive audit trail', async () => {
      const governance = createGovernanceEngine({
        name: 'audit-governance',
        defaultPermission: 'deny',
        roles: [createAdminRole()],
      });

      governance.registerActor({
        id: 'admin-1',
        roles: ['admin'],
      });

      const enforcer = createDenyByDefaultEnforcer('audit-enforcer');
      const auditLog = [];

      const composer = createHookComposer('audit-hooks', [
        createBeforeHook('log-before', async context => {
          auditLog.push({
            phase: 'before',
            tool: context.tool,
            timestamp: new Date(),
          });
        }),
        createAfterHook('log-after', async context => {
          auditLog.push({
            phase: 'after',
            tool: context.tool,
            result: context.result,
            timestamp: new Date(),
          });
        }),
      ]);

      await composer.execute(async context => 'success', {
        actorId: 'admin-1',
        tool: 'Read',
      });

      expect(auditLog).toHaveLength(2);
      expect(auditLog[0].phase).toBe('before');
      expect(auditLog[1].phase).toBe('after');

      const governanceAudit = governance.getAuditLog();
      expect(governanceAudit.length).toBeGreaterThan(0);

      const stats = enforcer.getStats();
      expect(stats.enforcements).toBeGreaterThan(0);
    });
  });
});
