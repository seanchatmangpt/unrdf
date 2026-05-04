/**
 * WorkItem System Tests - TDD Approach
 * Tests async work item system with deterministic scheduling
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { WorkItemExecutor, WORK_ITEM_STATES } from '../src/work-item.mjs';

describe('WorkItemExecutor - Async Work Item System', () => {
  let executor;

  beforeEach(() => {
    executor = new WorkItemExecutor();
  });

  describe('Executor Initialization', () => {
    it('should create executor instance', () => {
      expect(executor).toBeDefined();
      expect(typeof executor).toBe('object');
    });

    it('should initialize with empty work queue', () => {
      const order = executor.getExecutionOrder();
      expect(Array.isArray(order)).toBe(true);
      expect(order.length).toBe(0);
    });
  });

  describe('enqueueWorkItem() - Add Work to Queue', () => {
    it('should enqueue work item with goal', async () => {
      const workItemId = await executor.enqueueWorkItem('Process data');

      expect(workItemId).toBeDefined();
      expect(typeof workItemId).toBe('string');
      expect(workItemId.length).toBeGreaterThan(0);
    });

    it('should enqueue work item with goal and bounds', async () => {
      const bounds = { timeout: 5000, maxRetries: 3 };
      const workItemId = await executor.enqueueWorkItem('Calculate sum', bounds);

      expect(workItemId).toBeDefined();
      expect(typeof workItemId).toBe('string');
    });

    it('should return unique IDs for each work item', async () => {
      const id1 = await executor.enqueueWorkItem('Task 1');
      const id2 = await executor.enqueueWorkItem('Task 2');
      const id3 = await executor.enqueueWorkItem('Task 3');

      expect(id1).not.toBe(id2);
      expect(id2).not.toBe(id3);
      expect(id1).not.toBe(id3);
    });

    it('should add enqueued items to execution order', async () => {
      await executor.enqueueWorkItem('Task A');
      await executor.enqueueWorkItem('Task B');

      const order = executor.getExecutionOrder();
      expect(order.length).toBe(2);
    });

    it('should initialize work item in queued state', async () => {
      const workItemId = await executor.enqueueWorkItem('Initial task');
      const workItem = await executor.pollWorkItem(workItemId);

      expect(workItem.status).toBe(WORK_ITEM_STATES.QUEUED);
    });

    it('should record creation timestamp', async () => {
      const workItemId = await executor.enqueueWorkItem('Timestamped task');
      const workItem = await executor.pollWorkItem(workItemId);

      expect(workItem.created_ns).toBeDefined();
      expect(typeof workItem.created_ns).toBe('string');
      expect(BigInt(workItem.created_ns)).toBeGreaterThan(0n);
    });
  });

  describe('pollWorkItem() - Query Work Item Status', () => {
    it('should poll work item by ID', async () => {
      const workItemId = await executor.enqueueWorkItem('Pollable task');
      const workItem = await executor.pollWorkItem(workItemId);

      expect(workItem).toBeDefined();
      expect(workItem.id).toBe(workItemId);
    });

    it('should return work item with all required fields', async () => {
      const workItemId = await executor.enqueueWorkItem('Complete task');
      const workItem = await executor.pollWorkItem(workItemId);

      expect(workItem.id).toBeDefined();
      expect(workItem.goal).toBe('Complete task');
      expect(workItem.status).toBe(WORK_ITEM_STATES.QUEUED);
      expect(workItem.receipt_log).toBeDefined();
      expect(Array.isArray(workItem.receipt_log)).toBe(true);
      expect(workItem.created_ns).toBeDefined();
      expect(workItem.started_ns).toBeNull();
      expect(workItem.finished_ns).toBeNull();
    });

    it('should return null for non-existent work item', async () => {
      const workItem = await executor.pollWorkItem('non-existent-id');
      expect(workItem).toBeNull();
    });

    it('should reflect updated status after state transition', async () => {
      const workItemId = await executor.enqueueWorkItem('Transitioning task');

      // Initial state
      let workItem = await executor.pollWorkItem(workItemId);
      expect(workItem.status).toBe(WORK_ITEM_STATES.QUEUED);

      // Transition to running
      await executor.transitionWorkItem(workItemId, WORK_ITEM_STATES.RUNNING);
      workItem = await executor.pollWorkItem(workItemId);
      expect(workItem.status).toBe(WORK_ITEM_STATES.RUNNING);
    });
  });

  describe('State Transitions', () => {
    it('should transition from queued to running', async () => {
      const workItemId = await executor.enqueueWorkItem('Task to run');

      await executor.transitionWorkItem(workItemId, WORK_ITEM_STATES.RUNNING);
      const workItem = await executor.pollWorkItem(workItemId);

      expect(workItem.status).toBe(WORK_ITEM_STATES.RUNNING);
      expect(workItem.started_ns).toBeDefined();
      expect(workItem.started_ns).not.toBeNull();
    });

    it('should transition from running to succeeded', async () => {
      const workItemId = await executor.enqueueWorkItem('Task to succeed');

      await executor.transitionWorkItem(workItemId, WORK_ITEM_STATES.RUNNING);
      await executor.transitionWorkItem(workItemId, WORK_ITEM_STATES.SUCCEEDED);

      const workItem = await executor.pollWorkItem(workItemId);
      expect(workItem.status).toBe(WORK_ITEM_STATES.SUCCEEDED);
      expect(workItem.finished_ns).toBeDefined();
      expect(workItem.finished_ns).not.toBeNull();
    });

    it('should transition from running to failed', async () => {
      const workItemId = await executor.enqueueWorkItem('Task to fail');

      await executor.transitionWorkItem(workItemId, WORK_ITEM_STATES.RUNNING);
      await executor.transitionWorkItem(workItemId, WORK_ITEM_STATES.FAILED);

      const workItem = await executor.pollWorkItem(workItemId);
      expect(workItem.status).toBe(WORK_ITEM_STATES.FAILED);
      expect(workItem.finished_ns).toBeDefined();
    });

    it('should transition to denied from queued', async () => {
      const workItemId = await executor.enqueueWorkItem('Task to deny');

      await executor.transitionWorkItem(workItemId, WORK_ITEM_STATES.DENIED);

      const workItem = await executor.pollWorkItem(workItemId);
      expect(workItem.status).toBe(WORK_ITEM_STATES.DENIED);
      expect(workItem.finished_ns).toBeDefined();
    });

    it('should count all state transitions', async () => {
      const id1 = await executor.enqueueWorkItem('Task 1');
      const id2 = await executor.enqueueWorkItem('Task 2');
      const id3 = await executor.enqueueWorkItem('Task 3');

      // 3 enqueues = 3 transitions to QUEUED
      let count = executor.getStateTransitionCount();
      expect(count).toBe(3);

      await executor.transitionWorkItem(id1, WORK_ITEM_STATES.RUNNING);
      count = executor.getStateTransitionCount();
      expect(count).toBe(4);

      await executor.transitionWorkItem(id1, WORK_ITEM_STATES.SUCCEEDED);
      await executor.transitionWorkItem(id2, WORK_ITEM_STATES.RUNNING);
      await executor.transitionWorkItem(id3, WORK_ITEM_STATES.DENIED);

      count = executor.getStateTransitionCount();
      expect(count).toBe(7);
    });

    it('should enforce valid state transitions', async () => {
      const workItemId = await executor.enqueueWorkItem('Invalid transition');

      // Cannot go from QUEUED directly to SUCCEEDED (must go through RUNNING)
      await expect(
        executor.transitionWorkItem(workItemId, WORK_ITEM_STATES.SUCCEEDED)
      ).rejects.toThrow();
    });

    it('should prevent transitions from terminal states', async () => {
      const workItemId = await executor.enqueueWorkItem('Terminal state');

      await executor.transitionWorkItem(workItemId, WORK_ITEM_STATES.RUNNING);
      await executor.transitionWorkItem(workItemId, WORK_ITEM_STATES.SUCCEEDED);

      // Cannot transition from SUCCEEDED (terminal state)
      await expect(
        executor.transitionWorkItem(workItemId, WORK_ITEM_STATES.RUNNING)
      ).rejects.toThrow();
    });
  });

  describe('finalizeWorkItem() - Complete Work', () => {
    it('should finalize work item with result and receipt', async () => {
      const workItemId = await executor.enqueueWorkItem('Task to finalize');
      await executor.transitionWorkItem(workItemId, WORK_ITEM_STATES.RUNNING);

      const result = { output: 'Success', value: 42 };
      const receipt = { id: 'receipt-1', timestamp: Date.now() };

      const finalizedItem = await executor.finalizeWorkItem(workItemId, result, receipt);

      expect(finalizedItem.status).toBe(WORK_ITEM_STATES.SUCCEEDED);
      expect(finalizedItem.finished_ns).toBeDefined();
      expect(finalizedItem.receipt_log.length).toBeGreaterThan(0);
    });

    it('should append receipt to receipt log', async () => {
      const workItemId = await executor.enqueueWorkItem('Receipts task');
      await executor.transitionWorkItem(workItemId, WORK_ITEM_STATES.RUNNING);

      const receipt1 = { step: 1, action: 'start' };
      const receipt2 = { step: 2, action: 'process' };
      const receipt3 = { step: 3, action: 'complete' };

      await executor.addReceipt(workItemId, receipt1);
      await executor.addReceipt(workItemId, receipt2);

      const finalizedItem = await executor.finalizeWorkItem(
        workItemId,
        { success: true },
        receipt3
      );

      expect(finalizedItem.receipt_log.length).toBe(3);
      expect(finalizedItem.receipt_log[0]).toEqual(receipt1);
      expect(finalizedItem.receipt_log[1]).toEqual(receipt2);
      expect(finalizedItem.receipt_log[2]).toEqual(receipt3);
    });

    it('should calculate execution duration', async () => {
      const workItemId = await executor.enqueueWorkItem('Timed task');
      await executor.transitionWorkItem(workItemId, WORK_ITEM_STATES.RUNNING);

      // Small delay to ensure measurable duration
      await new Promise(resolve => setTimeout(resolve, 10));

      const finalizedItem = await executor.finalizeWorkItem(
        workItemId,
        { done: true },
        { final: true }
      );

      const started = BigInt(finalizedItem.started_ns);
      const finished = BigInt(finalizedItem.finished_ns);
      const duration = finished - started;

      expect(duration).toBeGreaterThan(0n);
    });
  });

  describe('Receipt Logging', () => {
    it('should support append-only receipt logs', async () => {
      const workItemId = await executor.enqueueWorkItem('Logged task');

      await executor.addReceipt(workItemId, { event: 'created' });
      await executor.addReceipt(workItemId, { event: 'validated' });
      await executor.addReceipt(workItemId, { event: 'queued' });

      const workItem = await executor.pollWorkItem(workItemId);
      expect(workItem.receipt_log.length).toBe(3);
      expect(workItem.receipt_log[0]).toEqual({ event: 'created' });
      expect(workItem.receipt_log[2]).toEqual({ event: 'queued' });
    });

    it('should preserve receipt order', async () => {
      const workItemId = await executor.enqueueWorkItem('Ordered receipts');

      for (let i = 0; i < 10; i++) {
        await executor.addReceipt(workItemId, { index: i, timestamp: Date.now() });
      }

      const workItem = await executor.pollWorkItem(workItemId);
      expect(workItem.receipt_log.length).toBe(10);

      for (let i = 0; i < 10; i++) {
        expect(workItem.receipt_log[i].index).toBe(i);
      }
    });

    it('should not allow modification of existing receipts', async () => {
      const workItemId = await executor.enqueueWorkItem('Immutable receipts');
      const receipt = { data: 'original' };

      await executor.addReceipt(workItemId, receipt);

      // Modify the original object
      receipt.data = 'modified';

      const workItem = await executor.pollWorkItem(workItemId);
      // Receipt log should have original value (deep copy)
      expect(workItem.receipt_log[0].data).toBe('original');
    });
  });

  describe('getExecutionOrder() - Deterministic Scheduling', () => {
    it('should return work items in FIFO order by default', async () => {
      const id1 = await executor.enqueueWorkItem('First');
      const id2 = await executor.enqueueWorkItem('Second');
      const id3 = await executor.enqueueWorkItem('Third');

      const order = executor.getExecutionOrder();
      expect(order).toEqual([id1, id2, id3]);
    });

    it('should maintain consistent order across multiple calls', async () => {
      await executor.enqueueWorkItem('A');
      await executor.enqueueWorkItem('B');
      await executor.enqueueWorkItem('C');

      const order1 = executor.getExecutionOrder();
      const order2 = executor.getExecutionOrder();
      const order3 = executor.getExecutionOrder();

      expect(order1).toEqual(order2);
      expect(order2).toEqual(order3);
    });

    it('should only include pending and queued items in execution order', async () => {
      const id1 = await executor.enqueueWorkItem('Pending task');
      const id2 = await executor.enqueueWorkItem('Running task');
      const id3 = await executor.enqueueWorkItem('Another pending');

      await executor.transitionWorkItem(id2, WORK_ITEM_STATES.RUNNING);

      const order = executor.getExecutionOrder();
      expect(order).toContain(id1);
      expect(order).not.toContain(id2); // Running items not in execution order
      expect(order).toContain(id3);
    });

    it('should exclude completed items from execution order', async () => {
      const id1 = await executor.enqueueWorkItem('To complete');
      const id2 = await executor.enqueueWorkItem('To keep pending');

      await executor.transitionWorkItem(id1, WORK_ITEM_STATES.RUNNING);
      await executor.transitionWorkItem(id1, WORK_ITEM_STATES.SUCCEEDED);

      const order = executor.getExecutionOrder();
      expect(order).not.toContain(id1);
      expect(order).toContain(id2);
    });

    it('should support priority-based ordering', async () => {
      const id1 = await executor.enqueueWorkItem('Low priority', { priority: 1 });
      const id2 = await executor.enqueueWorkItem('High priority', { priority: 10 });
      const id3 = await executor.enqueueWorkItem('Medium priority', { priority: 5 });

      const order = executor.getExecutionOrder();
      // Higher priority first
      expect(order[0]).toBe(id2);
      expect(order[1]).toBe(id3);
      expect(order[2]).toBe(id1);
    });

    it('should be deterministic with same input', async () => {
      // Create multiple executors with same sequence
      const exec1 = new WorkItemExecutor();
      const exec2 = new WorkItemExecutor();

      const tasks = ['Task A', 'Task B', 'Task C'];

      for (const task of tasks) {
        await exec1.enqueueWorkItem(task);
        await exec2.enqueueWorkItem(task);
      }

      const order1 = exec1.getExecutionOrder();
      const order2 = exec2.getExecutionOrder();

      // Orders should be deterministic based on creation time
      expect(order1.length).toBe(order2.length);
    });
  });

  describe('Concurrent Item Handling', () => {
    it('should handle multiple concurrent enqueues', async () => {
      const promises = [];
      for (let i = 0; i < 20; i++) {
        promises.push(executor.enqueueWorkItem(`Concurrent task ${i}`));
      }

      const ids = await Promise.all(promises);

      expect(ids.length).toBe(20);
      // All IDs should be unique
      const uniqueIds = new Set(ids);
      expect(uniqueIds.size).toBe(20);
    });

    it('should handle concurrent polling', async () => {
      const workItemIds = [];
      for (let i = 0; i < 10; i++) {
        workItemIds.push(await executor.enqueueWorkItem(`Task ${i}`));
      }

      const pollPromises = workItemIds.map(id => executor.pollWorkItem(id));
      const items = await Promise.all(pollPromises);

      expect(items.length).toBe(10);
      items.forEach((item, i) => {
        expect(item.id).toBe(workItemIds[i]);
        expect(item.status).toBe(WORK_ITEM_STATES.QUEUED);
      });
    });

    it('should handle concurrent state transitions', async () => {
      const ids = [];
      for (let i = 0; i < 5; i++) {
        ids.push(await executor.enqueueWorkItem(`Task ${i}`));
      }

      const transitionPromises = ids.map(id =>
        executor.transitionWorkItem(id, WORK_ITEM_STATES.RUNNING)
      );

      await Promise.all(transitionPromises);

      const items = await Promise.all(ids.map(id => executor.pollWorkItem(id)));
      items.forEach(item => {
        expect(item.status).toBe(WORK_ITEM_STATES.RUNNING);
        expect(item.started_ns).not.toBeNull();
      });
    });

    it('should maintain data consistency under concurrent operations', async () => {
      const id = await executor.enqueueWorkItem('Concurrent ops task');

      // Concurrent operations on same work item
      const ops = [
        executor.addReceipt(id, { op: 1 }),
        executor.addReceipt(id, { op: 2 }),
        executor.addReceipt(id, { op: 3 }),
      ];

      await Promise.all(ops);

      const item = await executor.pollWorkItem(id);
      expect(item.receipt_log.length).toBe(3);
    });
  });

  describe('Storage in O (Triple Store)', () => {
    it('should store work items in RDF triple store', async () => {
      const workItemId = await executor.enqueueWorkItem('RDF storage test');

      // Query the store directly
      const items = await executor.queryWorkItems(`
        SELECT ?item ?goal WHERE {
          ?item <http://kgc.io/goal> ?goal .
        }
      `);

      expect(items.length).toBeGreaterThan(0);
    });

    it('should store work items under ./var/kgc/work-items/ namespace', async () => {
      const workItemId = await executor.enqueueWorkItem('Namespaced item');
      const workItem = await executor.pollWorkItem(workItemId);

      expect(workItem.id).toContain('work-item');
    });

    it('should persist state transitions in triple store', async () => {
      const workItemId = await executor.enqueueWorkItem('Persistent state');
      await executor.transitionWorkItem(workItemId, WORK_ITEM_STATES.RUNNING);

      // Query should reflect current state
      const items = await executor.queryWorkItems(`
        SELECT ?item ?status WHERE {
          ?item <http://kgc.io/status> ?status .
          FILTER(?status = "running")
        }
      `);

      expect(items.length).toBeGreaterThan(0);
    });
  });

  describe('Edge Cases and Error Handling', () => {
    it('should handle empty goal string', async () => {
      const workItemId = await executor.enqueueWorkItem('');
      expect(workItemId).toBeDefined();

      const workItem = await executor.pollWorkItem(workItemId);
      expect(workItem.goal).toBe('');
    });

    it('should handle very long goal strings', async () => {
      const longGoal = 'x'.repeat(10000);
      const workItemId = await executor.enqueueWorkItem(longGoal);

      const workItem = await executor.pollWorkItem(workItemId);
      expect(workItem.goal).toBe(longGoal);
    });

    it('should handle complex bounds objects', async () => {
      const bounds = {
        timeout: 5000,
        maxRetries: 3,
        priority: 5,
        metadata: {
          nested: { deeply: { value: true } }
        }
      };

      const workItemId = await executor.enqueueWorkItem('Complex bounds', bounds);
      expect(workItemId).toBeDefined();
    });

    it('should throw error on invalid state transition', async () => {
      const workItemId = await executor.enqueueWorkItem('Invalid transition');

      await expect(
        executor.transitionWorkItem(workItemId, 'INVALID_STATE')
      ).rejects.toThrow();
    });

    it('should handle polling non-existent item gracefully', async () => {
      const item = await executor.pollWorkItem('does-not-exist');
      expect(item).toBeNull();
    });

    it('should handle adding receipt to non-existent item', async () => {
      await expect(
        executor.addReceipt('does-not-exist', { data: 'test' })
      ).rejects.toThrow();
    });
  });
});
