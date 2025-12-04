/**
 * @vitest-environment node
 * FMEA Poka-Yoke Guards - Consolidated Test Suite
 * 80/20: 12 essential tests covering all critical failure modes
 */

import { describe, it, expect, vi } from 'vitest';
import { defineHook } from '../../src/hooks/define-hook.mjs';
import { executeHook } from '../../src/hooks/hook-executor.mjs';
import { KnowledgeHookManager } from '../../src/hooks/knowledge-hook-manager.mjs';
import { HookScheduler, ScheduleConfigSchema } from '../../src/hooks/hook-scheduler.mjs';
import { QualityMetricsCollector, QualityGateSchema } from '../../src/hooks/quality-metrics.mjs';

const mockQuad = {
  subject: { termType: 'NamedNode', value: 'http://example.org/s' },
  predicate: { termType: 'NamedNode', value: 'http://example.org/p' },
  object: { termType: 'Literal', value: 'test' },
  graph: { termType: 'DefaultGraph', value: '' },
};

describe('FMEA Poka-Yoke Guards', () => {
  describe('Validation Guards (RPN 280 → 28)', () => {
    it('should warn and coerce non-boolean validation return', () => {
      const consoleSpy = vi.spyOn(console, 'warn').mockImplementation(() => {});
      const hook = defineHook({
        name: 'non-boolean-hook',
        trigger: 'before-add',
        validate: () => 1, // Returns number instead of boolean
      });

      const result = executeHook(hook, mockQuad);

      expect(result.valid).toBe(true);
      expect(result.warning).toContain('Non-boolean');
      expect(consoleSpy).toHaveBeenCalledWith(expect.stringContaining('[POKA-YOKE]'));
      consoleSpy.mockRestore();
    });

    it('should reject transform returning non-Quad', () => {
      const hook = defineHook({
        name: 'bad-transform',
        trigger: 'before-add',
        transform: () => null,
      });

      const result = executeHook(hook, mockQuad);

      expect(result.valid).toBe(false);
      expect(result.error).toContain('must return a Quad');
    });

    it('should preserve stack trace on hook error', () => {
      const hook = defineHook({
        name: 'throwing-hook',
        trigger: 'before-add',
        validate: () => {
          throw new Error('Test error');
        },
      });

      const result = executeHook(hook, mockQuad);

      expect(result.valid).toBe(false);
      expect(result.errorDetails.hookName).toBe('throwing-hook');
      expect(result.errorDetails.stack).toContain('Test error');
    });
  });

  describe('Scheduler Guards (RPN 432 → 43)', () => {
    it('should reject intervalMs below 10ms', () => {
      expect(() => {
        ScheduleConfigSchema.parse({
          id: 'test',
          hookId: 'test',
          type: 'interval',
          intervalMs: 5,
        });
      }).toThrow('at least 10ms');
    });

    it('should reject intervalMs above 24 hours', () => {
      expect(() => {
        ScheduleConfigSchema.parse({
          id: 'test',
          hookId: 'test',
          type: 'interval',
          intervalMs: 86400001,
        });
      }).toThrow('cannot exceed 24 hours');
    });

    it('should throw on invalid cron expression', () => {
      const scheduler = new HookScheduler();
      expect(() => scheduler._parseCronExpression('invalid')).toThrow('Invalid cron');
    });

    it('should disable hook after 3 consecutive failures (circuit breaker)', async () => {
      const consoleSpy = vi.spyOn(console, 'error').mockImplementation(() => {});
      const warnSpy = vi.spyOn(console, 'warn').mockImplementation(() => {});

      const scheduler = new HookScheduler({
        executeHook: async () => {
          throw new Error('Simulated failure');
        },
      });

      const hook = { name: 'failing-hook', trigger: 'on-interval' };
      const scheduled = scheduler.register(hook, {
        id: 'circuit-test',
        hookId: 'failing-hook',
        type: 'interval',
        intervalMs: 1000,
      });

      await scheduler._executeScheduled(scheduled);
      await scheduler._executeScheduled(scheduled);
      await scheduler._executeScheduled(scheduled);

      expect(scheduled.errorCount).toBe(3);
      expect(scheduled.enabled).toBe(false);

      scheduler.stop();
      consoleSpy.mockRestore();
      warnSpy.mockRestore();
    });
  });

  describe('Quality Guards (RPN 448 → 45)', () => {
    it('should reject operator-threshold mismatch', () => {
      expect(() => {
        QualityGateSchema.parse({
          name: 'invalid-between',
          metric: 'defect-rate',
          operator: 'between',
          threshold: 0.5,
        });
      }).toThrow('[min, max] array');
    });

    it('should enforce audit log size limits with FIFO eviction', () => {
      const consoleSpy = vi.spyOn(console, 'warn').mockImplementation(() => {});
      const collector = new QualityMetricsCollector({ maxAuditLogSize: 5 });

      for (let i = 0; i < 10; i++) {
        collector._auditLog({ event: `test-${i}`, timestamp: new Date() });
      }

      expect(collector.auditLog.length).toBeLessThanOrEqual(5);
      consoleSpy.mockRestore();
    });
  });

  describe('Concurrency Guards (RPN 128 → 0)', () => {
    it('should reject maxExecutionDepth outside 1-10 range', () => {
      expect(() => new KnowledgeHookManager({ maxExecutionDepth: 0 })).toThrow('between 1 and 10');
      expect(() => new KnowledgeHookManager({ maxExecutionDepth: 11 })).toThrow('between 1 and 10');
    });

    it('should track and reset execution depth', async () => {
      const manager = new KnowledgeHookManager();
      let depthDuringExecution = 0;

      const hook = defineHook({
        name: 'depth-hook',
        trigger: 'before-add',
        validate: () => {
          depthDuringExecution = manager.getExecutionDepth();
          return true;
        },
      });

      manager.registerHook(hook);
      await manager.executeByTrigger('before-add', mockQuad);

      expect(depthDuringExecution).toBe(1);
      expect(manager.getExecutionDepth()).toBe(0);
    });
  });
});
