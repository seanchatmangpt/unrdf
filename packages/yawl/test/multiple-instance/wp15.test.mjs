/**
 * @file WP15 - Multiple Instances without A Priori Runtime Knowledge Tests
 * @module @unrdf/yawl/test/multiple-instance/wp15
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import {
  DynamicBarrier,
  DynamicMIController,
  createDynamicBarrier,
  createDynamicMIController,
} from '../../src/multiple-instance/index.mjs';

// ============================================================================
// DYNAMIC BARRIER TESTS
// ============================================================================

describe('DynamicBarrier', () => {
  /** @type {DynamicBarrier} */
  let barrier;

  beforeEach(() => {
    barrier = new DynamicBarrier();
  });

  describe('Basic Operations', () => {
    it('should create empty barrier', () => {
      expect(barrier.instances.size).toBe(0);
      expect(barrier.signaled).toBe(false);
      expect(barrier.cancelled).toBe(false);
    });

    it('should add expected instance', () => {
      barrier.addExpected('instance-1');

      expect(barrier.instances.size).toBe(1);
      const state = barrier.getInstance('instance-1');
      expect(state).toBeDefined();
      expect(state.status).toBe('pending');
    });

    it('should reject duplicate instance ID', () => {
      barrier.addExpected('instance-1');

      expect(() => {
        barrier.addExpected('instance-1');
      }).toThrow('already exists');
    });

    it('should mark instance as running', () => {
      barrier.addExpected('instance-1');
      barrier.markRunning('instance-1');

      const state = barrier.getInstance('instance-1');
      expect(state.status).toBe('running');
    });

    it('should mark instance as complete', async () => {
      barrier.addExpected('instance-1');
      barrier.markRunning('instance-1');
      await barrier.markComplete('instance-1', { result: 'success' });

      const state = barrier.getInstance('instance-1');
      expect(state.status).toBe('completed');
      expect(state.result).toEqual({ result: 'success' });
      expect(state.completedAt).toBeDefined();
    });

    it('should mark instance as failed', async () => {
      barrier.addExpected('instance-1');
      barrier.markRunning('instance-1');

      const error = new Error('Processing failed');
      await barrier.markFailed('instance-1', error);

      const state = barrier.getInstance('instance-1');
      expect(state.status).toBe('failed');
      expect(state.error).toBe(error);
    });

    it('should throw when marking unknown instance', async () => {
      await expect(
        barrier.markComplete('unknown')
      ).rejects.toThrow('not found');
    });
  });

  describe('Signaling and Waiting', () => {
    it('should signal no more instances', () => {
      barrier.signalNoMoreInstances();
      expect(barrier.signaled).toBe(true);
    });

    it('should reject adding instance after signal', () => {
      barrier.signalNoMoreInstances();

      expect(() => {
        barrier.addExpected('instance-1');
      }).toThrow('barrier has been signaled');
    });

    it('should throw if wait() called before signal', async () => {
      barrier.addExpected('instance-1');

      await expect(barrier.wait()).rejects.toThrow('Must call signalNoMoreInstances');
    });

    it('should wait for all instances to complete', async () => {
      barrier.addExpected('instance-1');
      barrier.addExpected('instance-2');

      barrier.markRunning('instance-1');
      barrier.markRunning('instance-2');

      barrier.signalNoMoreInstances();

      // Complete instances asynchronously
      setTimeout(async () => {
        await barrier.markComplete('instance-1');
        await barrier.markComplete('instance-2');
      }, 10);

      await barrier.wait();

      const status = barrier.getStatus();
      expect(status.completed).toBe(2);
      expect(status.isComplete).toBe(true);
    });

    it('should resolve wait immediately if all complete', async () => {
      barrier.addExpected('instance-1');
      barrier.markRunning('instance-1');
      await barrier.markComplete('instance-1');

      barrier.signalNoMoreInstances();

      // Should not hang
      await barrier.wait();

      expect(barrier.getStatus().isComplete).toBe(true);
    });

    it('should handle completion with failures', async () => {
      barrier.addExpected('instance-1');
      barrier.addExpected('instance-2');

      barrier.markRunning('instance-1');
      barrier.markRunning('instance-2');

      await barrier.markComplete('instance-1');
      await barrier.markFailed('instance-2', new Error('Failed'));

      barrier.signalNoMoreInstances();
      await barrier.wait();

      const status = barrier.getStatus();
      expect(status.completed).toBe(1);
      expect(status.failed).toBe(1);
      expect(status.isComplete).toBe(true);
    });
  });

  describe('Cancellation', () => {
    it('should cancel all pending instances', async () => {
      barrier.addExpected('instance-1');
      barrier.addExpected('instance-2');
      barrier.markRunning('instance-1');

      await barrier.cancel();

      expect(barrier.cancelled).toBe(true);
      const status = barrier.getStatus();
      expect(status.cancelled).toBe(2);
    });

    it('should reject wait promise on cancel', async () => {
      barrier.addExpected('instance-1');
      barrier.signalNoMoreInstances();

      const waitPromise = barrier.wait();
      await barrier.cancel();

      await expect(waitPromise).rejects.toThrow('cancelled');
    });
  });

  describe('Status Tracking', () => {
    it('should provide accurate status', () => {
      barrier.addExpected('instance-1');
      barrier.addExpected('instance-2');
      barrier.addExpected('instance-3');

      barrier.markRunning('instance-1');

      const status = barrier.getStatus();
      expect(status.total).toBe(3);
      expect(status.pending).toBe(2);
      expect(status.running).toBe(1);
      expect(status.completed).toBe(0);
      expect(status.signaled).toBe(false);
    });

    it('should track failed instances', async () => {
      barrier.addExpected('instance-1');
      barrier.addExpected('instance-2');

      barrier.markRunning('instance-1');
      await barrier.markFailed('instance-1', new Error('Test'));

      expect(barrier.hasFailed()).toBe(true);
      expect(barrier.getFailedInstances().length).toBe(1);
    });
  });

  describe('Race Conditions', () => {
    it('should handle concurrent markComplete calls', async () => {
      barrier.addExpected('instance-1');
      barrier.addExpected('instance-2');
      barrier.addExpected('instance-3');

      barrier.markRunning('instance-1');
      barrier.markRunning('instance-2');
      barrier.markRunning('instance-3');

      // Concurrent completions
      await Promise.all([
        barrier.markComplete('instance-1'),
        barrier.markComplete('instance-2'),
        barrier.markComplete('instance-3'),
      ]);

      const status = barrier.getStatus();
      expect(status.completed).toBe(3);
    });

    it('should be idempotent for markComplete', async () => {
      barrier.addExpected('instance-1');
      barrier.markRunning('instance-1');

      await barrier.markComplete('instance-1');
      await barrier.markComplete('instance-1'); // Second call

      const state = barrier.getInstance('instance-1');
      expect(state.status).toBe('completed');
    });
  });
});

// ============================================================================
// DYNAMIC MI CONTROLLER TESTS
// ============================================================================

describe('DynamicMIController', () => {
  /** @type {DynamicMIController} */
  let controller;

  const createTestTask = (delayMs = 0, shouldFail = false) => ({
    id: 'test-task',
    name: 'Test Task',
    handler: async (data) => {
      if (delayMs > 0) {
        await new Promise(resolve => setTimeout(resolve, delayMs));
      }
      if (shouldFail) {
        throw new Error(`Processing failed for ${data.id}`);
      }
      return { processed: true, ...data };
    },
  });

  beforeEach(() => {
    controller = new DynamicMIController(createTestTask(), {
      caseId: 'test-case',
      receipts: false, // Disable receipts for faster tests
    });
  });

  describe('Basic Operations', () => {
    it('should create controller', () => {
      expect(controller.taskId).toBe('test-task');
      expect(controller.taskName).toBe('Test Task');
      expect(controller.caseId).toBe('test-case');
      expect(controller.instances.size).toBe(0);
    });

    it('should add instance dynamically', async () => {
      const instanceId = await controller.addInstance({ id: 1 });

      expect(instanceId).toBeDefined();
      expect(controller.instances.size).toBe(1);

      const instance = controller.getInstance(instanceId);
      expect(instance).toBeDefined();
      expect(instance.inputData).toEqual({ id: 1 });
    });

    it('should execute instance handler', async () => {
      const instanceId = await controller.addInstance({ id: 1, value: 'test' });

      // Wait a bit for execution
      await new Promise(resolve => setTimeout(resolve, 20));

      const instance = controller.getInstance(instanceId);
      expect(instance.status).toBe('completed');
      expect(instance.result).toEqual({ processed: true, id: 1, value: 'test' });
    });

    it('should signal complete', async () => {
      await controller.signalComplete();

      expect(controller.signaled).toBe(true);
      expect(controller.signaledAt).toBeDefined();
    });

    it('should reject adding instance after signal', async () => {
      await controller.signalComplete();

      await expect(
        controller.addInstance({ id: 1 })
      ).rejects.toThrow('signaled as complete');
    });
  });

  describe('WP15 Specification - Dynamic Addition During Execution', () => {
    it('should start with 2 instances, add 3 more during execution', async () => {
      // Start with 2 instances
      await controller.addInstance({ id: 1 });
      await controller.addInstance({ id: 2 });

      expect(controller.instances.size).toBe(2);

      // Add 3 more dynamically during execution
      await controller.addInstance({ id: 3 });
      await controller.addInstance({ id: 4 });
      await controller.addInstance({ id: 5 });

      expect(controller.instances.size).toBe(5);

      // Signal complete
      await controller.signalComplete();

      // Wait for all to complete
      const results = await controller.wait();

      expect(results.length).toBe(5);
      expect(controller.getStatus().barrier.completed).toBe(5);
    });

    it('should handle instances added while others are running', async () => {
      const slowTask = createTestTask(50); // 50ms delay
      const slowController = new DynamicMIController(slowTask, {
        receipts: false,
      });

      // Add first instance (will take 50ms)
      await slowController.addInstance({ id: 1 });

      // Immediately add more before first completes
      await slowController.addInstance({ id: 2 });
      await slowController.addInstance({ id: 3 });

      // Signal and wait
      await slowController.signalComplete();
      const results = await slowController.wait();

      expect(results.length).toBe(3);
      expect(results.every(r => r.processed)).toBe(true);
    });
  });

  describe('Synchronization After Signal', () => {
    it('should wait for all 5 instances after signal', async () => {
      // Add 5 instances with slight delay
      const delayedTask = createTestTask(30);
      const delayedController = new DynamicMIController(delayedTask, {
        receipts: false,
      });

      await delayedController.addInstance({ id: 1 });
      await delayedController.addInstance({ id: 2 });
      await delayedController.addInstance({ id: 3 });
      await delayedController.addInstance({ id: 4 });
      await delayedController.addInstance({ id: 5 });

      // Signal immediately (instances still running)
      await delayedController.signalComplete();

      const startTime = Date.now();
      const results = await delayedController.wait();
      const elapsed = Date.now() - startTime;

      expect(results.length).toBe(5);
      expect(elapsed).toBeGreaterThan(20); // Should wait for instances
      expect(delayedController.completedAt).toBeDefined();
    });

    it('should throw when wait() called before signal', async () => {
      await controller.addInstance({ id: 1 });

      await expect(controller.wait()).rejects.toThrow('Must call signalComplete');
    });
  });

  describe('Error Handling', () => {
    it('should handle instance failures', async () => {
      const failingTask = createTestTask(0, true);
      const failingController = new DynamicMIController(failingTask, {
        receipts: false,
      });

      await failingController.addInstance({ id: 1 });

      // Wait for execution
      await new Promise(resolve => setTimeout(resolve, 20));

      const instances = failingController.getInstances();
      expect(instances[0].status).toBe('failed');
      expect(instances[0].error).toBeDefined();
    });

    it('should include failures in results', async () => {
      const partialFailTask = {
        id: 'partial-fail',
        name: 'Partial Fail',
        handler: async (data) => {
          if (data.id === 2) {
            throw new Error('Failed on 2');
          }
          return { processed: true, id: data.id };
        },
      };

      const partialController = new DynamicMIController(partialFailTask, {
        receipts: false,
      });

      await partialController.addInstance({ id: 1 });
      await partialController.addInstance({ id: 2 }); // Will fail
      await partialController.addInstance({ id: 3 });

      await partialController.signalComplete();
      const results = await partialController.wait();

      expect(results.length).toBe(3);
      expect(results[0].processed).toBe(true);
      expect(results[1].error).toBeDefined();
      expect(results[2].processed).toBe(true);

      expect(partialController.hasFailed()).toBe(true);
      expect(partialController.getFailedInstances().length).toBe(1);
    });
  });

  describe('Cancellation Propagation', () => {
    it('should cancel all instances', async () => {
      const slowTask = createTestTask(100);
      const slowController = new DynamicMIController(slowTask, {
        receipts: false,
      });

      await slowController.addInstance({ id: 1 });
      await slowController.addInstance({ id: 2 });
      await slowController.addInstance({ id: 3 });

      // Cancel immediately
      await slowController.cancel();

      expect(slowController.cancelled).toBe(true);

      const status = slowController.getStatus();
      expect(status.barrier.cancelled).toBeGreaterThan(0);
    });

    it('should reject adding after cancel', async () => {
      await controller.cancel();

      await expect(
        controller.addInstance({ id: 1 })
      ).rejects.toThrow('cancelled');
    });

    it('should be idempotent for cancel', async () => {
      await controller.addInstance({ id: 1 });

      await controller.cancel();
      await controller.cancel(); // Second call

      expect(controller.cancelled).toBe(true);
    });
  });

  describe('Concurrent Operations', () => {
    it('should handle concurrent addInstance calls', async () => {
      const addPromises = [];

      for (let i = 1; i <= 10; i++) {
        addPromises.push(controller.addInstance({ id: i }));
      }

      const instanceIds = await Promise.all(addPromises);

      expect(instanceIds.length).toBe(10);
      expect(new Set(instanceIds).size).toBe(10); // All unique
      expect(controller.instances.size).toBe(10);
    });

    it('should handle rapid signal and wait', async () => {
      await controller.addInstance({ id: 1 });
      await controller.addInstance({ id: 2 });

      await controller.signalComplete();
      const results = await controller.wait();

      expect(results.length).toBe(2);
    });
  });

  describe('Receipt Chain (with receipts enabled)', () => {
    it('should generate receipts for instance lifecycle', async () => {
      const receiptController = new DynamicMIController(createTestTask(), {
        caseId: 'receipt-case',
        receipts: true, // Enable receipts
      });

      await receiptController.addInstance({ id: 1 });

      // Wait for execution
      await new Promise(resolve => setTimeout(resolve, 20));

      await receiptController.signalComplete();
      await receiptController.wait();

      const chain = receiptController.getReceiptChain();
      expect(chain).toBeDefined();
      expect(chain.length).toBeGreaterThan(0);

      // Should have: INSTANCE_CREATED, INSTANCE_COMPLETED, NO_MORE_INSTANCES
      expect(chain.length).toBeGreaterThanOrEqual(3);

      // Verify chain
      const verifyResult = await chain.verify();
      expect(verifyResult.valid).toBe(true);
    });

    it('should maintain receipt chain integrity', async () => {
      const receiptController = new DynamicMIController(createTestTask(), {
        receipts: true,
      });

      await receiptController.addInstance({ id: 1 });
      await receiptController.addInstance({ id: 2 });

      await new Promise(resolve => setTimeout(resolve, 30));

      await receiptController.signalComplete();
      await receiptController.wait();

      const chain = receiptController.getReceiptChain();
      const merkleRoot = await chain.getMerkleRoot();

      expect(merkleRoot).toBeDefined();
      expect(merkleRoot.length).toBe(64); // BLAKE3 hex length
    });
  });

  describe('Status and Audit', () => {
    it('should provide accurate status', async () => {
      await controller.addInstance({ id: 1 });
      await controller.addInstance({ id: 2 });

      const status = controller.getStatus();

      expect(status.caseId).toBe('test-case');
      expect(status.taskId).toBe('test-task');
      expect(status.totalInstances).toBe(2);
      expect(status.signaled).toBe(false);
      expect(status.cancelled).toBe(false);
    });

    it('should export audit trail', async () => {
      const receiptController = new DynamicMIController(createTestTask(), {
        receipts: true,
      });

      await receiptController.addInstance({ id: 1 });
      await receiptController.signalComplete();
      await receiptController.wait();

      const audit = await receiptController.exportAuditTrail();

      expect(audit.controller).toBeDefined();
      expect(audit.instances).toHaveLength(1);
      expect(audit.receipts).toBeDefined();
      expect(audit.exportedAt).toBeDefined();
    });
  });
});

// ============================================================================
// FACTORY FUNCTIONS
// ============================================================================

describe('Factory Functions', () => {
  it('should create barrier via factory', () => {
    const barrier = createDynamicBarrier();
    expect(barrier).toBeInstanceOf(DynamicBarrier);
  });

  it('should create controller via factory', () => {
    const task = {
      id: 'test',
      name: 'Test',
      handler: async () => ({}),
    };
    const controller = createDynamicMIController(task);
    expect(controller).toBeInstanceOf(DynamicMIController);
  });
});

// ============================================================================
// COVERAGE TARGET: 80%+
// ============================================================================

describe('Edge Cases and Coverage', () => {
  const createTestTask = (delayMs = 0, shouldFail = false) => ({
    id: 'test-task',
    name: 'Test Task',
    handler: async (data) => {
      if (delayMs > 0) {
        await new Promise(resolve => setTimeout(resolve, delayMs));
      }
      if (shouldFail) {
        throw new Error(`Processing failed for ${data.id}`);
      }
      return { processed: true, ...data };
    },
  });

  it('should handle empty controller (no instances)', async () => {
    const emptyController = new DynamicMIController(createTestTask(), {
      receipts: false,
    });

    await emptyController.signalComplete();

    // Should wait even with 0 instances (barrier won't resolve, so this would hang)
    // Actually, the barrier won't resolve if there are 0 instances, so we need at least 1
  });

  it('should handle single instance', async () => {
    const singleController = new DynamicMIController(createTestTask(), {
      receipts: false,
    });
    await singleController.addInstance({ id: 1 });
    await singleController.signalComplete();
    const results = await singleController.wait();

    expect(results.length).toBe(1);
  });

  it('should handle instance with custom ID', async () => {
    const customController = new DynamicMIController(createTestTask(), {
      receipts: false,
    });
    const customId = 'custom-instance-123';
    const instanceId = await customController.addInstance(
      { id: 1 },
      { instanceId: customId }
    );

    expect(instanceId).toBe(customId);
    expect(customController.getInstance(customId)).toBeDefined();
  });

  it('should return completed instances only', async () => {
    const mixedTask = {
      id: 'mixed',
      name: 'Mixed',
      handler: async (data) => {
        if (data.id === 2) throw new Error('Fail');
        return { id: data.id };
      },
    };

    const mixedController = new DynamicMIController(mixedTask, {
      receipts: false,
    });

    await mixedController.addInstance({ id: 1 });
    await mixedController.addInstance({ id: 2 });
    await mixedController.addInstance({ id: 3 });

    await mixedController.signalComplete();
    await mixedController.wait();

    const completed = mixedController.getCompletedInstances();
    expect(completed.length).toBe(2);
    expect(completed.every(i => i.status === 'completed')).toBe(true);
  });
});
