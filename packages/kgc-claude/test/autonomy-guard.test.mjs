/**
 * Autonomy Guard Tests - Bounded autonomy with capacity-limited channels
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  createAutonomyGuard,
  withGuard,
  createProductionGuard,
  createStrictGuard,
} from '../src/autonomy-guard.mjs';
import { DENIAL_REASONS } from '../src/constants.mjs';

describe('Autonomy Guard', () => {
  let guard;

  beforeEach(() => {
    guard = createAutonomyGuard({
      maxDeltaSize: 10,
      maxToolOps: 5,
      maxFilesTouched: 3,
      maxRewriteCost: 100,
    });
  });

  describe('createAutonomyGuard', () => {
    it('should create guard with specified budget', () => {
      const budget = guard.getBudget();

      expect(budget.maxDeltaSize).toBe(10);
      expect(budget.maxToolOps).toBe(5);
      expect(budget.maxFilesTouched).toBe(3);
      expect(budget.maxRewriteCost).toBe(100);
    });

    it('should track usage', () => {
      guard.consume({ deltaSize: 2, toolOps: 1 });

      const usage = guard.getUsage();

      expect(usage.deltaSize).toBe(2);
      expect(usage.toolOps).toBe(1);
    });

    it('should track remaining capacity', () => {
      guard.consume({ deltaSize: 3, toolOps: 2 });

      const remaining = guard.getRemaining();

      expect(remaining.deltaSize).toBe(7);
      expect(remaining.toolOps).toBe(3);
    });
  });

  describe('check', () => {
    it('should allow operations within budget', async () => {
      const result = await guard.check({
        deltaSize: 5,
        toolOps: 2,
      });

      expect(result.allowed).toBe(true);
      expect(result.denialReceipt).toBeUndefined();
    });

    it('should deny when delta size exceeded', async () => {
      const result = await guard.check({ deltaSize: 15 });

      expect(result.allowed).toBe(false);
      expect(result.denialReceipt).toBeDefined();
      expect(result.denialReceipt.reason).toBe(DENIAL_REASONS.DELTA_TOO_LARGE);
    });

    it('should deny when tool ops exceeded', async () => {
      const result = await guard.check({ toolOps: 10 });

      expect(result.allowed).toBe(false);
      expect(result.denialReceipt.reason).toBe(DENIAL_REASONS.TOOLS_EXCEEDED);
    });

    it('should deny when files exceeded', async () => {
      const result = await guard.check({
        files: ['a.txt', 'b.txt', 'c.txt', 'd.txt'],
      });

      expect(result.allowed).toBe(false);
      expect(result.denialReceipt.reason).toBe(DENIAL_REASONS.FILES_EXCEEDED);
    });

    it('should track cumulative usage', async () => {
      guard.consume({ deltaSize: 8 });

      const result = await guard.check({ deltaSize: 5 });

      expect(result.allowed).toBe(false);
    });
  });

  describe('consume', () => {
    it('should accumulate delta size', () => {
      guard.consume({ deltaSize: 3 });
      guard.consume({ deltaSize: 2 });

      expect(guard.getUsage().deltaSize).toBe(5);
    });

    it('should deduplicate files', () => {
      guard.consume({ files: ['a.txt', 'b.txt'] });
      guard.consume({ files: ['a.txt', 'c.txt'] });

      expect(guard.getUsage().filesTouched).toBe(3);
    });
  });

  describe('denial receipts', () => {
    it('should generate chained denial receipts', async () => {
      await guard.check({ deltaSize: 100 });
      await guard.check({ toolOps: 100 });

      const history = guard.getDenialHistory();

      expect(history.length).toBe(2);
      expect(history[0].receiptHash).toBeDefined();
      expect(history[1].receiptHash).toBeDefined();
    });

    it('should include budget and usage in denial', async () => {
      guard.consume({ deltaSize: 5 });

      const result = await guard.check({ deltaSize: 10 });

      expect(result.denialReceipt.budget.maxDeltaSize).toBe(10);
      expect(result.denialReceipt.usage.deltaSize).toBe(5);
      expect(result.denialReceipt.requested.deltaSize).toBe(10);
    });
  });

  describe('resetEpoch', () => {
    it('should reset all usage', () => {
      guard.consume({ deltaSize: 5, toolOps: 3, files: ['a.txt'] });
      guard.resetEpoch();

      const usage = guard.getUsage();

      expect(usage.deltaSize).toBe(0);
      expect(usage.toolOps).toBe(0);
      expect(usage.filesTouched).toBe(0);
    });
  });

  describe('createScope', () => {
    it('should create scoped guard with reduced budget', () => {
      guard.consume({ deltaSize: 5 });

      const scoped = guard.createScope({ maxDeltaSize: 3 });
      const scopedBudget = scoped.getBudget();

      expect(scopedBudget.maxDeltaSize).toBe(3);
    });

    it('should respect parent remaining capacity', () => {
      guard.consume({ deltaSize: 8 });

      const scoped = guard.createScope({ maxDeltaSize: 5 });
      const scopedBudget = scoped.getBudget();

      // Only 2 remaining in parent, so scope limited to 2
      expect(scopedBudget.maxDeltaSize).toBe(2);
    });
  });

  describe('withGuard', () => {
    it('should execute operation if allowed', async () => {
      const result = await withGuard(
        guard,
        { deltaSize: 2, toolOps: 1 },
        async () => 'success'
      );

      expect(result.success).toBe(true);
      expect(result.result).toBe('success');
    });

    it('should consume budget after success', async () => {
      await withGuard(guard, { deltaSize: 2 }, async () => 'ok');

      expect(guard.getUsage().deltaSize).toBe(2);
    });

    it('should return denial receipt if not allowed', async () => {
      const result = await withGuard(
        guard,
        { deltaSize: 100 },
        async () => 'never called'
      );

      expect(result.success).toBe(false);
      expect(result.denialReceipt).toBeDefined();
    });

    it('should not consume budget on error', async () => {
      try {
        await withGuard(guard, { deltaSize: 2 }, async () => {
          throw new Error('test');
        });
      } catch {
        // Expected
      }

      expect(guard.getUsage().deltaSize).toBe(0);
    });
  });

  describe('preset guards', () => {
    it('should create production guard', () => {
      const prod = createProductionGuard();
      const budget = prod.getBudget();

      expect(budget.maxDeltaSize).toBe(500);
      expect(budget.maxToolOps).toBe(100);
    });

    it('should create strict guard', () => {
      const strict = createStrictGuard();
      const budget = strict.getBudget();

      expect(budget.maxDeltaSize).toBe(50);
      expect(budget.maxToolOps).toBe(10);
    });
  });
});
