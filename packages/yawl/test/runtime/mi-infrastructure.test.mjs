/**
 * @file Minimal MI Infrastructure Test
 * @description Tests for task-spawner.mjs and instance-pool.mjs
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  spawnInstances,
  enableInstance,
  startInstance,
  completeInstance,
  failInstance,
  cancelInstance,
  InstanceStatus,
} from '../../src/runtime/task-spawner.mjs';
import {
  InstancePool,
  globalInstancePool,
  executeInstances,
  executeInstancesSequential,
  waitForCompletion,
  getCompletionPercentage,
} from '../../src/runtime/instance-pool.mjs';

describe('Minimal MI Infrastructure', () => {
  describe('Task Spawner', () => {
    it('should spawn N instances with receipts', async () => {
      const result = await spawnInstances('test-task', 5, {
        baseInputData: { shared: 'value' },
        generateReceipts: true,
      });

      expect(result.instances).toHaveLength(5);
      expect(result.instanceIds).toHaveLength(5);
      expect(result.metadata.totalInstances).toBe(5);
      expect(result.aggregateReceipt).toBeDefined();
      expect(result.aggregateReceipt.merkleRoot).toBeDefined();

      // Check all instances are spawned
      for (const instance of result.instances) {
        expect(instance.status).toBe(InstanceStatus.SPAWNED);
        expect(instance.receipts).toHaveLength(1);
        expect(instance.receipts[0].action).toBe('spawn');
      }
    });

    it('should spawn instances with per-instance input', async () => {
      const result = await spawnInstances('test-task', 3, {
        baseInputData: { shared: 'base' },
        instanceInputs: [
          { orderId: 'ORD-1' },
          { orderId: 'ORD-2' },
          { orderId: 'ORD-3' },
        ],
      });

      expect(result.instances[0].inputData.orderId).toBe('ORD-1');
      expect(result.instances[1].inputData.orderId).toBe('ORD-2');
      expect(result.instances[2].inputData.orderId).toBe('ORD-3');

      // Check shared data is merged
      for (const instance of result.instances) {
        expect(instance.inputData.shared).toBe('base');
      }
    });

    it('should spawn instances without receipts', async () => {
      const result = await spawnInstances('test-task', 3, {
        generateReceipts: false,
      });

      expect(result.instances).toHaveLength(3);
      expect(result.aggregateReceipt).toBeNull();

      for (const instance of result.instances) {
        expect(instance.receipts).toHaveLength(0);
      }
    });

    it('should enable spawned instance', async () => {
      const result = await spawnInstances('test-task', 1);
      const instance = result.instances[0];

      await enableInstance(instance, { generateReceipt: true });

      expect(instance.status).toBe(InstanceStatus.ENABLED);
      expect(instance.receipts).toHaveLength(2); // spawn + enable
      expect(instance.receipts[1].action).toBe('enable');
    });

    it('should start enabled instance', async () => {
      const result = await spawnInstances('test-task', 1);
      const instance = result.instances[0];

      await enableInstance(instance);
      await startInstance(instance, { generateReceipt: true });

      expect(instance.status).toBe(InstanceStatus.ACTIVE);
      expect(instance.receipts[instance.receipts.length - 1].action).toBe('start');
    });

    it('should complete active instance', async () => {
      const result = await spawnInstances('test-task', 1);
      const instance = result.instances[0];

      await enableInstance(instance);
      await startInstance(instance);
      await completeInstance(instance, { result: 'success' }, { generateReceipt: true });

      expect(instance.status).toBe(InstanceStatus.COMPLETED);
      expect(instance.outputData.result).toBe('success');
      expect(instance.completedAt).toBeDefined();
    });

    it('should fail instance with error', async () => {
      const result = await spawnInstances('test-task', 1);
      const instance = result.instances[0];

      await enableInstance(instance);
      await startInstance(instance);
      await failInstance(instance, new Error('Test failure'));

      expect(instance.status).toBe(InstanceStatus.FAILED);
      expect(instance.error).toBe('Test failure');
      expect(instance.completedAt).toBeDefined();
    });

    it('should cancel instance', async () => {
      const result = await spawnInstances('test-task', 1);
      const instance = result.instances[0];

      await cancelInstance(instance);

      expect(instance.status).toBe(InstanceStatus.CANCELLED);
      expect(instance.completedAt).toBeDefined();
    });

    it('should reject invalid state transitions', async () => {
      const result = await spawnInstances('test-task', 1);
      const instance = result.instances[0];

      // Can't start a spawned instance
      await expect(startInstance(instance)).rejects.toThrow();

      // Can't complete a spawned instance
      await expect(completeInstance(instance)).rejects.toThrow();
    });
  });

  describe('Instance Pool', () => {
    let pool;

    beforeEach(() => {
      pool = new InstancePool();
    });

    it('should register and retrieve instances', async () => {
      const result = await spawnInstances('test-task', 3);

      for (const instance of result.instances) {
        pool.register(instance);
      }

      expect(pool.size()).toBe(3);
      expect(pool.get(result.instances[0].id)).toBeDefined();
      expect(pool.getByParent(result.parentTaskId)).toHaveLength(3);
    });

    it('should track aggregate status', async () => {
      const result = await spawnInstances('test-task', 5);

      for (const instance of result.instances) {
        pool.register(instance);
      }

      const status = pool.getAggregateStatus(result.parentTaskId);

      expect(status.totalInstances).toBe(5);
      expect(status.statusCounts.spawned).toBe(5);
      expect(status.allCompleted).toBe(false);
      expect(status.anyFailed).toBe(false);
    });

    it('should update instance status', async () => {
      const result = await spawnInstances('test-task', 1);
      const instance = result.instances[0];

      pool.register(instance);
      pool.updateStatus(instance.id, InstanceStatus.COMPLETED);

      expect(instance.status).toBe(InstanceStatus.COMPLETED);
      expect(instance.completedAt).toBeDefined();

      const status = pool.getAggregateStatus(result.parentTaskId);
      expect(status.statusCounts.completed).toBe(1);
      expect(status.allCompleted).toBe(true);
    });

    it('should remove instances', async () => {
      const result = await spawnInstances('test-task', 3);

      for (const instance of result.instances) {
        pool.register(instance);
      }

      pool.remove(result.instances[0].id);

      expect(pool.size()).toBe(2);
      expect(pool.get(result.instances[0].id)).toBeNull();
    });

    it('should clear parent instances', async () => {
      const result = await spawnInstances('test-task', 5);

      for (const instance of result.instances) {
        pool.register(instance);
      }

      pool.clearParent(result.parentTaskId);

      expect(pool.size()).toBe(0);
      expect(pool.getByParent(result.parentTaskId)).toHaveLength(0);
    });
  });

  describe('Parallel Execution', () => {
    it('should execute instances in parallel', async () => {
      const result = await spawnInstances('test-task', 5);
      const pool = new InstancePool();

      const executionTimes = [];

      const completed = await executeInstances(
        result.instances,
        async (instance) => {
          const start = Date.now();
          // Simulate async work
          await new Promise(resolve => setTimeout(resolve, 50));
          executionTimes.push(Date.now() - start);

          return { result: `output-${instance.instanceIndex}` };
        },
        { pool }
      );

      expect(completed).toHaveLength(5);

      // Check all completed
      for (const instance of completed) {
        expect(instance.status).toBe(InstanceStatus.COMPLETED);
        expect(instance.outputData).toBeDefined();
      }

      // Verify parallel execution (total time should be ~50ms, not 250ms)
      const avgTime = executionTimes.reduce((a, b) => a + b, 0) / executionTimes.length;
      expect(avgTime).toBeGreaterThanOrEqual(45);
      expect(avgTime).toBeLessThan(100);
    });

    it('should handle instance failures in parallel execution', async () => {
      const result = await spawnInstances('test-task', 5);
      const pool = new InstancePool();

      const completed = await executeInstances(
        result.instances,
        async (instance) => {
          if (instance.instanceIndex === 2) {
            throw new Error('Simulated failure');
          }
          return { result: 'success' };
        },
        { pool, failFast: false }
      );

      expect(completed).toHaveLength(5);

      const status = pool.getAggregateStatus(result.parentTaskId);
      expect(status.statusCounts.completed).toBe(4);
      expect(status.statusCounts.failed).toBe(1);
      expect(status.anyFailed).toBe(true);
    });

    it('should fail fast on error if configured', async () => {
      const result = await spawnInstances('test-task', 5);
      const pool = new InstancePool();

      await expect(
        executeInstances(
          result.instances,
          async (instance) => {
            if (instance.instanceIndex === 2) {
              throw new Error('Simulated failure');
            }
            return { result: 'success' };
          },
          { pool, failFast: true }
        )
      ).rejects.toThrow('Simulated failure');
    });
  });

  describe('Sequential Execution', () => {
    it('should execute instances sequentially', async () => {
      const result = await spawnInstances('test-task', 3);
      const pool = new InstancePool();

      const executionOrder = [];

      await executeInstancesSequential(
        result.instances,
        async (instance) => {
          executionOrder.push(instance.instanceIndex);
          return { result: `output-${instance.instanceIndex}` };
        },
        { pool }
      );

      // Verify sequential order
      expect(executionOrder).toEqual([0, 1, 2]);

      const status = pool.getAggregateStatus(result.parentTaskId);
      expect(status.allCompleted).toBe(true);
    });
  });

  describe('Completion Tracking', () => {
    it('should calculate completion percentage', async () => {
      const result = await spawnInstances('test-task', 10);
      const pool = new InstancePool();

      for (const instance of result.instances) {
        pool.register(instance);
      }

      expect(getCompletionPercentage(result.parentTaskId, { pool })).toBe(0);

      // Complete 5 instances
      for (let i = 0; i < 5; i++) {
        pool.updateStatus(result.instances[i].id, InstanceStatus.COMPLETED);
      }

      expect(getCompletionPercentage(result.parentTaskId, { pool })).toBe(50);

      // Complete all
      for (let i = 5; i < 10; i++) {
        pool.updateStatus(result.instances[i].id, InstanceStatus.COMPLETED);
      }

      expect(getCompletionPercentage(result.parentTaskId, { pool })).toBe(100);
    });

    it('should wait for all instances to complete', async () => {
      const result = await spawnInstances('test-task', 3);
      const pool = new InstancePool();

      for (const instance of result.instances) {
        pool.register(instance);
      }

      // Complete instances asynchronously
      setTimeout(() => {
        pool.updateStatus(result.instances[0].id, InstanceStatus.COMPLETED);
      }, 50);
      setTimeout(() => {
        pool.updateStatus(result.instances[1].id, InstanceStatus.COMPLETED);
      }, 100);
      setTimeout(() => {
        pool.updateStatus(result.instances[2].id, InstanceStatus.COMPLETED);
      }, 150);

      const status = await waitForCompletion(result.parentTaskId, {
        pool,
        checkInterval: 10,
        timeout: 1000,
      });

      expect(status.allCompleted).toBe(true);
      expect(status.statusCounts.completed).toBe(3);
    });

    it('should timeout if instances do not complete', async () => {
      const result = await spawnInstances('test-task', 3);
      const pool = new InstancePool();

      for (const instance of result.instances) {
        pool.register(instance);
      }

      // Only complete 2 of 3
      pool.updateStatus(result.instances[0].id, InstanceStatus.COMPLETED);
      pool.updateStatus(result.instances[1].id, InstanceStatus.COMPLETED);

      await expect(
        waitForCompletion(result.parentTaskId, {
          pool,
          checkInterval: 10,
          timeout: 100,
        })
      ).rejects.toThrow('Timeout');
    });
  });

  describe('E2E Workflow', () => {
    it('should handle complete MI workflow', async () => {
      // 1. Spawn instances
      const result = await spawnInstances('order-processing', 5, {
        baseInputData: { batchId: 'BATCH-123' },
        instanceInputs: [
          { orderId: 'ORD-1', amount: 100 },
          { orderId: 'ORD-2', amount: 200 },
          { orderId: 'ORD-3', amount: 150 },
          { orderId: 'ORD-4', amount: 300 },
          { orderId: 'ORD-5', amount: 250 },
        ],
      });

      expect(result.instances).toHaveLength(5);

      // 2. Execute instances in parallel
      const pool = new InstancePool();
      const completed = await executeInstances(
        result.instances,
        async (instance) => {
          // Simulate order processing
          const amount = instance.inputData.amount;
          return {
            orderId: instance.inputData.orderId,
            processed: true,
            fee: amount * 0.02,
          };
        },
        { pool }
      );

      // 3. Verify all completed
      const status = pool.getAggregateStatus(result.parentTaskId);
      expect(status.allCompleted).toBe(true);
      expect(status.statusCounts.completed).toBe(5);

      // 4. Verify outputs
      for (const instance of completed) {
        expect(instance.outputData.processed).toBe(true);
        expect(instance.outputData.fee).toBeDefined();
      }

      // 5. Cleanup
      pool.clearParent(result.parentTaskId);
      expect(pool.size()).toBe(0);
    });
  });
});
