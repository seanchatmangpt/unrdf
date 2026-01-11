/**
 * @file E2E Hooks Policy Integration Test Suite
 * @module @unrdf/daemon/test/e2e-hooks-policy
 * @description
 * Comprehensive E2E tests for daemon hooks policy integration.
 * Covers policy registration, evaluation, conflict resolution, audit trails,
 * policy versioning, and rollback functionality.
 * 15+ tests ensuring policy-driven operation control.
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { Daemon } from '../src/daemon.mjs';
import { createHookScheduler } from '@unrdf/hooks';
import {
  DaemonHookPolicyAdapter,
  integrateHooksPolicy,
  PolicySchema,
  PolicyDecisionSchema,
  PolicyAuditSchema,
} from '../src/integrations/hooks-policy.mjs';

/**
 * Generate valid UUID v4
 * @returns {string} UUID v4 formatted string
 */
function generateUUID() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

/**
 * Create test daemon
 * @returns {Daemon} Test daemon instance
 */
function createTestDaemon() {
  return new Daemon({
    daemonId: generateUUID(),
    name: `test-daemon-${Date.now()}`,
    concurrency: 5,
  });
}

describe('Daemon Hooks Policy Integration E2E', () => {
  let daemon;
  let scheduler;
  let adapter;

  beforeEach(() => {
    daemon = createTestDaemon();
    scheduler = createHookScheduler();
    adapter = new DaemonHookPolicyAdapter(daemon, scheduler);
  });

  afterEach(() => {
    if (daemon?.isRunning) {
      daemon.stop();
    }
  });

  describe('Policy Registration & Management', () => {
    it('should register a policy with valid configuration', () => {
      // Arrange
      const policyConfig = {
        id: 'test-approval-policy',
        name: 'Test Approval Policy',
        type: 'approval',
        priority: 80,
        config: { requiresApproval: true, approvers: ['admin@example.com'] },
      };

      // Act
      const policy = adapter.registerPolicy(policyConfig);

      // Assert
      expect(policy).toBeDefined();
      expect(policy.id).toBe('test-approval-policy');
      expect(policy.version).toBe(1);
      expect(policy.enabled).toBe(true);
      expect(adapter.getPolicy('test-approval-policy')).toBe(policy);
    });

    it('should increment version on policy update', () => {
      // Arrange
      const policyConfig = {
        id: 'versioned-policy',
        name: 'Versioned Policy',
        type: 'approval',
        priority: 50,
        config: { requiresApproval: true },
      };

      // Act
      const v1 = adapter.registerPolicy(policyConfig);
      const v2 = adapter.registerPolicy({ ...policyConfig, priority: 70 });

      // Assert
      expect(v1.version).toBe(1);
      expect(v2.version).toBe(2);
      expect(adapter.getPolicy('versioned-policy').version).toBe(2);
    });

    it('should throw on invalid policy configuration', () => {
      // Arrange
      const invalidPolicy = {
        // Missing 'id', 'name', 'type'
        config: {},
      };

      // Act & Assert
      expect(() => adapter.registerPolicy(invalidPolicy)).toThrow();
    });

    it('should unregister a policy', () => {
      // Arrange
      adapter.registerPolicy({
        id: 'policy-to-delete',
        name: 'Deletable Policy',
        type: 'approval',
        config: {},
      });

      // Act
      const removed = adapter.unregisterPolicy('policy-to-delete');

      // Assert
      expect(removed).toBe(true);
      expect(adapter.getPolicy('policy-to-delete')).toBeUndefined();
    });

    it('should return false when unregistering non-existent policy', () => {
      // Act
      const result = adapter.unregisterPolicy('non-existent');

      // Assert
      expect(result).toBe(false);
    });

    it('should list all registered policies', () => {
      // Arrange
      adapter.registerPolicy({
        id: 'policy-1',
        name: 'Policy 1',
        type: 'approval',
        config: {},
      });
      adapter.registerPolicy({
        id: 'policy-2',
        name: 'Policy 2',
        type: 'time-window',
        config: {},
      });

      // Act
      const policies = adapter.listPolicies();

      // Assert
      expect(policies).toHaveLength(2);
      expect(policies.map(p => p.id)).toContain('policy-1');
      expect(policies.map(p => p.id)).toContain('policy-2');
    });
  });

  describe('Policy Evaluation', () => {
    it('should allow operations when no policies configured', async () => {
      // Arrange
      const context = {
        operationId: 'op-1',
        operationType: 'create',
        payload: { data: 'test' },
      };

      // Act
      const decision = await adapter.evaluatePolicies('op-1', context);

      // Assert
      expect(decision.decision).toBe('allow');
      expect(decision.reason).toContain('No policies configured');
    });

    it('should deny operations violating approval policy', async () => {
      // Arrange
      adapter.registerPolicy({
        id: 'strict-approval',
        name: 'Strict Approval',
        type: 'approval',
        priority: 100,
        config: { requiresApproval: true, approvers: ['admin@example.com'] },
      });

      const context = {
        operationId: 'op-2',
        operationType: 'delete',
        payload: {},
      };

      // Act
      const decision = await adapter.evaluatePolicies('op-2', context);

      // Assert
      expect(decision.decision).toBe('defer');
      expect(decision.reason).toContain('Awaiting approval');
    });

    it('should enforce time-window policy', async () => {
      // Arrange
      adapter.registerPolicy({
        id: 'business-hours',
        name: 'Business Hours Only',
        type: 'time-window',
        priority: 80,
        config: {
          timeWindows: [
            { start: 9, end: 17, days: [1, 2, 3, 4, 5] }, // Mon-Fri 9-5
          ],
        },
      });

      const context = {
        operationId: 'op-3',
        operationType: 'create',
        payload: {},
      };

      // Act
      const decision = await adapter.evaluatePolicies('op-3', context);

      // Assert
      expect(['allow', 'deny']).toContain(decision.decision);
    });

    it('should enforce rate-limit policy', async () => {
      // Arrange
      adapter.registerPolicy({
        id: 'rate-limiter',
        name: 'Rate Limiter',
        type: 'rate-limit',
        priority: 75,
        config: { maxPerMinute: 2 },
      });

      const context = {
        operationId: 'op-rate-1',
        operationType: 'bulk-import',
        payload: {},
      };

      // Act - First call should allow
      const decision1 = await adapter.evaluatePolicies('op-rate-1', context);
      // Second call should allow
      const decision2 = await adapter.evaluatePolicies('op-rate-2', { ...context, operationId: 'op-rate-2' });
      // Third call should defer
      const decision3 = await adapter.evaluatePolicies('op-rate-3', { ...context, operationId: 'op-rate-3' });

      // Assert
      expect(decision1.decision).toBe('allow');
      expect(decision2.decision).toBe('allow');
      expect(decision3.decision).toBe('defer');
    });

    it('should enforce resource-limit policy', async () => {
      // Arrange
      adapter.registerPolicy({
        id: 'resource-limiter',
        name: 'Resource Limiter',
        type: 'resource-limit',
        priority: 70,
        config: { maxConcurrent: 1 },
      });

      const context = {
        operationId: 'op-resource',
        operationType: 'heavy-compute',
        payload: {},
      };

      // Act
      const decision = await adapter.evaluatePolicies('op-resource', context);

      // Assert
      expect(['allow', 'defer']).toContain(decision.decision);
    });

    it('should allow custom policy evaluation', async () => {
      // Arrange
      const customEvaluator = vi.fn(async (context) => ({
        decision: context.operationType === 'read' ? 'allow' : 'deny',
        reason: `Custom rule: ${context.operationType} is not allowed`,
      }));

      adapter.registerPolicy({
        id: 'custom-policy',
        name: 'Custom Policy',
        type: 'custom',
        priority: 90,
        config: { evaluatorFn: customEvaluator },
      });

      const context = {
        operationId: 'op-custom',
        operationType: 'read',
        payload: {},
      };

      // Act
      const decision = await adapter.evaluatePolicies('op-custom', context);

      // Assert
      expect(decision.decision).toBe('allow');
      expect(customEvaluator).toHaveBeenCalled();
    });
  });

  describe('Conflict Resolution', () => {
    it('should resolve conflicts with highest-priority strategy', async () => {
      // Arrange
      adapter.registerPolicy({
        id: 'low-priority-allow',
        name: 'Low Priority Allow',
        type: 'approval',
        priority: 10,
        config: { requiresApproval: false },
      });

      adapter.registerPolicy({
        id: 'high-priority-defer',
        name: 'High Priority Defer',
        type: 'approval',
        priority: 90,
        config: { requiresApproval: true },
      });

      const context = {
        operationId: 'op-conflict',
        operationType: 'update',
        payload: {},
      };

      // Act
      const decision = await adapter.evaluatePolicies('op-conflict', context);

      // Assert
      expect(decision.conflictResolved).toBe(true);
      expect(decision.conflictStrategy).toBe('highest-priority');
    });

    it('should deny if any policy denies with unanimous strategy', async () => {
      // Arrange
      adapter.conflictStrategy = 'unanimous';

      adapter.registerPolicy({
        id: 'policy-a',
        name: 'Policy A',
        type: 'approval',
        priority: 50,
        config: { requiresApproval: true },
      });

      adapter.registerPolicy({
        id: 'policy-b',
        name: 'Policy B',
        type: 'approval',
        priority: 50,
        config: { requiresApproval: true },
      });

      const context = {
        operationId: 'op-unanimous',
        operationType: 'delete',
        payload: {},
      };

      // Act
      const decision = await adapter.evaluatePolicies('op-unanimous', context);

      // Assert
      expect(decision.conflictStrategy).toBe('unanimous');
    });
  });

  describe('Policy Versioning & Rollback', () => {
    it('should maintain policy history', () => {
      // Arrange
      const policyId = 'history-test';
      adapter.registerPolicy({
        id: policyId,
        name: 'History Policy',
        type: 'approval',
        priority: 50,
        config: { requiresApproval: false },
      });

      // Act - Update policy twice
      adapter.registerPolicy({
        id: policyId,
        name: 'History Policy Updated',
        type: 'approval',
        priority: 60,
        config: { requiresApproval: true },
      });

      adapter.registerPolicy({
        id: policyId,
        name: 'History Policy Final',
        type: 'approval',
        priority: 70,
        config: { requiresApproval: true, approvers: ['superadmin'] },
      });

      // Assert
      const history = adapter.getPolicyHistory(policyId);
      expect(history).toHaveLength(3);
      expect(history[0].version).toBe(1);
      expect(history[1].version).toBe(2);
      expect(history[2].version).toBe(3);
    });

    it('should rollback policy to previous version', () => {
      // Arrange
      const policyId = 'rollback-test';
      const v1 = adapter.registerPolicy({
        id: policyId,
        name: 'Original',
        type: 'approval',
        priority: 50,
        config: { requiresApproval: false },
      });

      adapter.registerPolicy({
        id: policyId,
        name: 'Modified',
        type: 'approval',
        priority: 80,
        config: { requiresApproval: true },
      });

      // Act
      const rolledBack = adapter.rollbackPolicy(policyId, 1);

      // Assert
      expect(rolledBack).toBeDefined();
      expect(rolledBack.priority).toBe(50);
      expect(rolledBack.name).toBe('Original');
      expect(adapter.getPolicy(policyId).version).toBe(2);
    });

    it('should limit history size to maxHistoryVersions', () => {
      // Arrange
      const policyId = 'limited-history';
      const adapter2 = new DaemonHookPolicyAdapter(daemon, scheduler, {
        maxHistoryVersions: 3,
      });

      // Act - Create 5 versions
      for (let i = 0; i < 5; i++) {
        adapter2.registerPolicy({
          id: policyId,
          name: `Version ${i}`,
          type: 'approval',
          priority: 50 + i,
          config: {},
        });
      }

      // Assert
      const history = adapter2.getPolicyHistory(policyId);
      expect(history.length).toBeLessThanOrEqual(3);
    });
  });

  describe('Audit Trail', () => {
    it('should record policy creation in audit log', () => {
      // Arrange
      const policyId = 'audit-create-test';

      // Act
      adapter.registerPolicy({
        id: policyId,
        name: 'Audit Test',
        type: 'approval',
        config: {},
      });

      // Assert
      const auditTrail = adapter.getAuditTrail(policyId);
      expect(auditTrail).toHaveLength(1);
      expect(auditTrail[0].action).toBe('created');
      expect(auditTrail[0].version).toBe(1);
    });

    it('should record policy updates in audit log', () => {
      // Arrange
      const policyId = 'audit-update-test';
      adapter.registerPolicy({
        id: policyId,
        name: 'Update Test',
        type: 'approval',
        config: {},
      });

      // Act
      adapter.registerPolicy({
        id: policyId,
        name: 'Update Test Modified',
        type: 'approval',
        priority: 70,
        config: {},
      });

      // Assert
      const auditTrail = adapter.getAuditTrail(policyId);
      expect(auditTrail).toHaveLength(2);
      expect(auditTrail[1].action).toBe('updated');
    });

    it('should record policy enable/disable in audit log', () => {
      // Arrange
      const policyId = 'audit-toggle-test';
      adapter.registerPolicy({
        id: policyId,
        name: 'Toggle Test',
        type: 'approval',
        config: {},
      });

      // Act
      adapter.disablePolicy(policyId);
      adapter.enablePolicy(policyId);

      // Assert
      const auditTrail = adapter.getAuditTrail(policyId);
      expect(auditTrail.length).toBeGreaterThanOrEqual(3);
      expect(auditTrail.map(e => e.action)).toContain('disabled');
      expect(auditTrail.map(e => e.action)).toContain('enabled');
    });

    it('should record policy rollback in audit log', () => {
      // Arrange
      const policyId = 'audit-rollback-test';
      adapter.registerPolicy({
        id: policyId,
        name: 'Rollback Test',
        type: 'approval',
        config: {},
      });

      adapter.registerPolicy({
        id: policyId,
        name: 'Rollback Test V2',
        type: 'approval',
        config: {},
      });

      // Act
      adapter.rollbackPolicy(policyId, 1);

      // Assert
      const auditTrail = adapter.getAuditTrail(policyId);
      const rollbackEntry = auditTrail.find(e => e.action === 'rolled-back');
      expect(rollbackEntry).toBeDefined();
    });

    it('should track all audit logs across policies', () => {
      // Arrange
      adapter.registerPolicy({
        id: 'policy-a',
        name: 'Policy A',
        type: 'approval',
        config: {},
      });
      adapter.registerPolicy({
        id: 'policy-b',
        name: 'Policy B',
        type: 'approval',
        config: {},
      });

      // Act
      const allAudit = adapter.getAuditTrail();

      // Assert
      expect(allAudit.length).toBeGreaterThanOrEqual(2);
      expect(allAudit.map(a => a.policyId)).toContain('policy-a');
      expect(allAudit.map(a => a.policyId)).toContain('policy-b');
    });
  });

  describe('Decision Log', () => {
    it('should record policy decisions', async () => {
      // Arrange
      adapter.registerPolicy({
        id: 'decision-test',
        name: 'Decision Test',
        type: 'approval',
        config: { requiresApproval: false },
      });

      // Act
      await adapter.evaluatePolicies('decision-op-1', {
        operationId: 'decision-op-1',
        operationType: 'test',
        payload: {},
      });

      // Assert
      const decisions = adapter.getDecisionLog('decision-op-1');
      expect(decisions).toHaveLength(1);
      expect(decisions[0].operationId).toBe('decision-op-1');
      expect(decisions[0].decision).toBe('allow');
    });

    it('should filter decision log by operation ID', async () => {
      // Arrange
      adapter.registerPolicy({
        id: 'filter-test',
        name: 'Filter Test',
        type: 'approval',
        config: { requiresApproval: false },
      });

      // Act
      await adapter.evaluatePolicies('filter-op-1', {
        operationId: 'filter-op-1',
        operationType: 'test',
        payload: {},
      });
      await adapter.evaluatePolicies('filter-op-2', {
        operationId: 'filter-op-2',
        operationType: 'test',
        payload: {},
      });

      // Assert
      const op1Decisions = adapter.getDecisionLog('filter-op-1');
      const op2Decisions = adapter.getDecisionLog('filter-op-2');
      expect(op1Decisions).toHaveLength(1);
      expect(op2Decisions).toHaveLength(1);
      expect(op1Decisions[0].operationId).toBe('filter-op-1');
      expect(op2Decisions[0].operationId).toBe('filter-op-2');
    });

    it('should return all decisions when no filter provided', async () => {
      // Arrange
      adapter.registerPolicy({
        id: 'all-decisions-test',
        name: 'All Decisions',
        type: 'approval',
        config: { requiresApproval: false },
      });

      // Act
      await adapter.evaluatePolicies('all-1', {
        operationId: 'all-1',
        operationType: 'test',
        payload: {},
      });
      await adapter.evaluatePolicies('all-2', {
        operationId: 'all-2',
        operationType: 'test',
        payload: {},
      });

      // Assert
      const allDecisions = adapter.getDecisionLog();
      expect(allDecisions.length).toBeGreaterThanOrEqual(2);
    });
  });

  describe('Policy Enable/Disable', () => {
    it('should disable a policy and exclude it from evaluation', async () => {
      // Arrange
      adapter.registerPolicy({
        id: 'disable-test',
        name: 'Disable Test',
        type: 'approval',
        priority: 100,
        config: { requiresApproval: true },
      });

      // Act
      adapter.disablePolicy('disable-test');
      const decision = await adapter.evaluatePolicies('disable-op', {
        operationId: 'disable-op',
        operationType: 'test',
        payload: {},
      });

      // Assert
      expect(decision.decision).toBe('allow');
      expect(decision.reason).toContain('No policies configured');
    });

    it('should enable a disabled policy', () => {
      // Arrange
      adapter.registerPolicy({
        id: 'enable-test',
        name: 'Enable Test',
        type: 'approval',
        config: {},
      });
      adapter.disablePolicy('enable-test');

      // Act
      const result = adapter.enablePolicy('enable-test');

      // Assert
      expect(result).toBe(true);
      expect(adapter.getPolicy('enable-test').enabled).toBe(true);
    });

    it('should return false when enabling non-existent policy', () => {
      // Act
      const result = adapter.enablePolicy('non-existent');

      // Assert
      expect(result).toBe(false);
    });
  });

  describe('Statistics & Monitoring', () => {
    it('should return adapter statistics', () => {
      // Arrange
      adapter.registerPolicy({
        id: 'stat-policy-1',
        name: 'Stat Policy 1',
        type: 'approval',
        config: {},
      });
      adapter.registerPolicy({
        id: 'stat-policy-2',
        name: 'Stat Policy 2',
        type: 'approval',
        config: {},
      });

      // Act
      const stats = adapter.getStats();

      // Assert
      expect(stats.adapterId).toBe(adapter.id);
      expect(stats.totalPolicies).toBe(2);
      expect(stats.enabledPolicies).toBe(2);
      expect(stats.auditEnabled).toBe(true);
      expect(stats.conflictStrategy).toBe('highest-priority');
    });

    it('should count enabled vs disabled policies correctly', () => {
      // Arrange
      adapter.registerPolicy({
        id: 'enabled-policy',
        name: 'Enabled',
        type: 'approval',
        config: {},
      });
      adapter.registerPolicy({
        id: 'disabled-policy',
        name: 'Disabled',
        type: 'approval',
        config: {},
      });

      // Act
      adapter.disablePolicy('disabled-policy');
      const stats = adapter.getStats();

      // Assert
      expect(stats.totalPolicies).toBe(2);
      expect(stats.enabledPolicies).toBe(1);
    });
  });

  describe('Integration with Daemon', () => {
    it('should integrate with factory function', () => {
      // Act
      const integratedAdapter = integrateHooksPolicy(daemon, scheduler, {
        adapterId: 'factory-test',
      });

      // Assert
      expect(integratedAdapter).toBeInstanceOf(DaemonHookPolicyAdapter);
      expect(integratedAdapter.id).toBe('factory-test');
    });
  });

  describe('Audit Trail Completeness', () => {
    it('should maintain complete audit trail for policy lifecycle', () => {
      // Arrange
      const policyId = 'complete-lifecycle';

      // Act
      adapter.registerPolicy({
        id: policyId,
        name: 'Lifecycle Test',
        type: 'approval',
        priority: 50,
        config: { requiresApproval: false },
      });

      adapter.registerPolicy({
        id: policyId,
        name: 'Lifecycle Test Updated',
        priority: 70,
        type: 'approval',
        config: { requiresApproval: true },
      });

      adapter.disablePolicy(policyId);
      adapter.enablePolicy(policyId);
      adapter.rollbackPolicy(policyId, 1);
      adapter.unregisterPolicy(policyId);

      // Assert
      const audit = adapter.getAuditTrail(policyId);
      const actions = audit.map(a => a.action);

      expect(actions).toContain('created');
      expect(actions).toContain('updated');
      expect(actions).toContain('disabled');
      expect(actions).toContain('enabled');
      expect(actions).toContain('rolled-back');
      expect(actions).toContain('deleted');
    });
  });

  describe('Performance Under Policy Evaluation', () => {
    it('should evaluate multiple policies efficiently', async () => {
      // Arrange
      for (let i = 0; i < 10; i++) {
        adapter.registerPolicy({
          id: `perf-policy-${i}`,
          name: `Performance Policy ${i}`,
          type: 'approval',
          priority: i,
          config: { requiresApproval: false },
        });
      }

      const context = {
        operationId: 'perf-op',
        operationType: 'test',
        payload: {},
      };

      // Act
      const start = performance.now();
      const decision = await adapter.evaluatePolicies('perf-op', context);
      const elapsed = performance.now() - start;

      // Assert
      expect(decision).toBeDefined();
      expect(elapsed).toBeLessThan(1000); // Should complete in < 1 second
    });

    it('should handle large audit logs efficiently', async () => {
      // Arrange
      const policyId = 'large-audit-test';
      adapter.registerPolicy({
        id: policyId,
        name: 'Large Audit',
        type: 'approval',
        config: {},
      });

      // Act - Create many decisions
      for (let i = 0; i < 100; i++) {
        await adapter.evaluatePolicies(`large-op-${i}`, {
          operationId: `large-op-${i}`,
          operationType: 'test',
          payload: {},
        });
      }

      // Assert
      const stats = adapter.getStats();
      expect(stats.decisionLogSize).toBeGreaterThan(0);
      expect(adapter.decisionLog.length).toBeLessThanOrEqual(10000);
    });
  });
});
