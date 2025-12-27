/**
 * @file Resource Manager Tests - Deterministic allocation and receipt verification
 * @module @unrdf/fusion/test/resource-manager
 */

import { describe, it, beforeEach } from 'node:test';
import assert from 'node:assert/strict';
import { createResourceManager } from '../src/resource-manager.mjs';

describe('ResourceManager', () => {
  describe('Pool Management', () => {
    it('should create pool with specified capacity', async () => {
      const rm = await createResourceManager({ enableReceipts: false });
      const pool = rm.createPool('test', 100, 'round-robin');

      assert.equal(pool.name, 'test');
      assert.equal(pool.capacity, 100);
      assert.equal(pool.strategy, 'round-robin');
      assert.equal(pool.allocated, 0);
    });

    it('should throw when creating duplicate pool', async () => {
      const rm = await createResourceManager({ enableReceipts: false });
      rm.createPool('test', 100);

      assert.throws(
        () => rm.createPool('test', 100),
        /Pool 'test' already exists/
      );
    });

    it('should query pool status', async () => {
      const rm = await createResourceManager({ enableReceipts: false });
      rm.createPool('compute', 100);

      const status = rm.query('compute');
      assert.equal(status.allocated, 0);
      assert.equal(status.available, 100);
      assert.equal(status.capacity, 100);
      assert.equal(status.utilization, 0);
    });
  });

  describe('Allocation and Deallocation', () => {
    let rm;

    beforeEach(async () => {
      rm = await createResourceManager({ enableReceipts: false });
      rm.createPool('test', 100);
    });

    it('should allocate resources successfully', async () => {
      const result = await rm.allocate('test', 30);

      assert.equal(result.allocated, 30);
      assert.equal(result.remaining, 70);

      const status = rm.query('test');
      assert.equal(status.allocated, 30);
      assert.equal(status.available, 70);
    });

    it('should handle multiple concurrent allocations', async () => {
      const r1 = await rm.allocate('test', 30);
      const r2 = await rm.allocate('test', 30);
      const r3 = await rm.allocate('test', 30);

      assert.equal(r1.allocated, 30);
      assert.equal(r1.remaining, 70);

      assert.equal(r2.allocated, 30);
      assert.equal(r2.remaining, 40);

      assert.equal(r3.allocated, 30);
      assert.equal(r3.remaining, 10);

      const status = rm.query('test');
      assert.equal(status.allocated, 90);
      assert.equal(status.available, 10);
    });

    it('should deallocate resources successfully', async () => {
      await rm.allocate('test', 30);
      const result = await rm.deallocate('test', 30);

      assert.equal(result.allocated, 30);
      assert.equal(result.remaining, 100);

      const status = rm.query('test');
      assert.equal(status.allocated, 0);
      assert.equal(status.available, 100);
    });

    it('should throw on insufficient capacity', async () => {
      await assert.rejects(
        async () => rm.allocate('test', 150),
        /Insufficient capacity in pool 'test'/
      );
    });

    it('should throw on over-deallocation', async () => {
      await rm.allocate('test', 30);

      await assert.rejects(
        async () => rm.deallocate('test', 50),
        /Cannot deallocate 50 from pool 'test'/
      );
    });

    it('should enforce resource limits', async () => {
      await rm.allocate('test', 60);

      assert.throws(
        () => rm.enforceLimit('test', 50),
        /Resource limit exceeded for 'test': 60 > 50/
      );

      // Should not throw when under limit
      rm.enforceLimit('test', 100);
    });
  });

  describe('Deterministic Allocation Scenario', () => {
    it('should produce deterministic results across runs', async () => {
      // Run 1
      const rm1 = await createResourceManager({ enableReceipts: true, deterministic: true });
      rm1.createPool('compute', 100, 'round-robin');

      const r1_1 = await rm1.allocate('compute', 30);
      const r1_2 = await rm1.allocate('compute', 30);
      const r1_3 = await rm1.allocate('compute', 30);
      assert.equal(r1_3.remaining, 10);

      const r1_d1 = await rm1.deallocate('compute', 30);
      assert.equal(r1_d1.remaining, 40);

      const chain1 = rm1.getReceiptChain();

      // Run 2
      const rm2 = await createResourceManager({ enableReceipts: true, deterministic: true });
      rm2.createPool('compute', 100, 'round-robin');

      const r2_1 = await rm2.allocate('compute', 30);
      const r2_2 = await rm2.allocate('compute', 30);
      const r2_3 = await rm2.allocate('compute', 30);
      assert.equal(r2_3.remaining, 10);

      const r2_d1 = await rm2.deallocate('compute', 30);
      assert.equal(r2_d1.remaining, 40);

      const chain2 = rm2.getReceiptChain();

      // Verify same number of receipts
      assert.equal(chain1.length, chain2.length);
      assert.equal(chain1.length, 4); // 3 allocations + 1 deallocation

      // Verify receipts have same structure (but different timestamps)
      for (let i = 0; i < chain1.length; i++) {
        const rec1 = chain1[i];
        const rec2 = chain2[i];

        // Same event type
        assert.equal(rec1.eventType, rec2.eventType);

        // Same case and task IDs
        assert.equal(rec1.caseId, rec2.caseId);
        assert.equal(rec1.taskId, rec2.taskId);

        // Same decision type
        assert.equal(rec1.payload.decision, rec2.payload.decision);

        // Same amount and remaining
        assert.equal(rec1.payload.context.amount, rec2.payload.context.amount);
        assert.equal(rec1.payload.context.remaining, rec2.payload.context.remaining);

        // Different timestamps (non-deterministic by nature)
        assert.notEqual(rec1.t_ns, rec2.t_ns);
        assert.notEqual(rec1.id, rec2.id);
      }

      // Verify receipt chain integrity
      for (let i = 1; i < chain1.length; i++) {
        assert.equal(chain1[i].previousReceiptHash, chain1[i - 1].receiptHash);
        assert.equal(chain2[i].previousReceiptHash, chain2[i - 1].receiptHash);
      }
    });
  });

  describe('Receipt Generation', () => {
    it('should generate receipts for allocations when enabled', async () => {
      const rm = await createResourceManager({ enableReceipts: true });
      rm.createPool('test', 100);

      const result = await rm.allocate('test', 30, {
        allocationId: 'alloc-1',
        actor: 'user-123',
        reason: 'test allocation',
      });

      assert.ok(result.receipt);
      assert.equal(result.receipt.eventType, 'RESOURCE_ALLOCATED');
      assert.equal(result.receipt.taskId, 'test');
      assert.equal(result.receipt.workItemId, 'alloc-1');
      assert.equal(result.receipt.payload.decision, 'ALLOCATE');
      assert.equal(result.receipt.payload.actor, 'user-123');
      assert.equal(result.receipt.payload.context.amount, 30);
      assert.equal(result.receipt.payload.context.remaining, 70);
    });

    it('should generate receipts for deallocations when enabled', async () => {
      const rm = await createResourceManager({ enableReceipts: true });
      rm.createPool('test', 100);

      await rm.allocate('test', 30, { allocationId: 'alloc-1' });
      const result = await rm.deallocate('test', 30, {
        allocationId: 'alloc-1',
        actor: 'user-123',
        reason: 'test deallocation',
      });

      assert.ok(result.receipt);
      assert.equal(result.receipt.eventType, 'RESOURCE_RELEASED');
      assert.equal(result.receipt.payload.decision, 'DEALLOCATE');
    });

    it('should chain receipts correctly', async () => {
      const rm = await createResourceManager({ enableReceipts: true });
      rm.createPool('test', 100);

      await rm.allocate('test', 30);
      await rm.allocate('test', 20);
      await rm.deallocate('test', 10);

      const chain = rm.getReceiptChain();
      assert.equal(chain.length, 3);

      // First receipt has no previous
      assert.equal(chain[0].previousReceiptHash, null);

      // Subsequent receipts chain to previous
      assert.equal(chain[1].previousReceiptHash, chain[0].receiptHash);
      assert.equal(chain[2].previousReceiptHash, chain[1].receiptHash);
    });

    it('should not generate receipts when disabled', async () => {
      const rm = await createResourceManager({ enableReceipts: false });
      rm.createPool('test', 100);

      const result = await rm.allocate('test', 30);
      assert.equal(result.receipt, null);
    });
  });

  describe('Statistics', () => {
    it('should track allocation and deallocation statistics', async () => {
      const rm = await createResourceManager({ enableReceipts: true });
      rm.createPool('test', 100);

      await rm.allocate('test', 30);
      await rm.allocate('test', 20);
      await rm.deallocate('test', 10);

      const stats = rm.getStats();
      assert.equal(stats.allocations, 2);
      assert.equal(stats.deallocations, 1);
      assert.equal(stats.receiptsGenerated, 3);
      assert.equal(stats.chainLength, 3);
      assert.ok(stats.pools.test);
      assert.equal(stats.pools.test.allocated, 40);
    });
  });

  describe('Reset and Clear', () => {
    it('should reset all pools', async () => {
      const rm = await createResourceManager({ enableReceipts: true });
      rm.createPool('test', 100);

      await rm.allocate('test', 30);
      rm.reset();

      const status = rm.query('test');
      assert.equal(status.allocated, 0);
      assert.equal(status.available, 100);

      const stats = rm.getStats();
      assert.equal(stats.allocations, 0);
      assert.equal(stats.chainLength, 0);
    });

    it('should clear all pools', async () => {
      const rm = await createResourceManager();
      rm.createPool('test1', 100);
      rm.createPool('test2', 200);

      rm.clear();

      assert.throws(
        () => rm.query('test1'),
        /Pool 'test1' does not exist/
      );

      assert.throws(
        () => rm.query('test2'),
        /Pool 'test2' does not exist/
      );
    });
  });
});
