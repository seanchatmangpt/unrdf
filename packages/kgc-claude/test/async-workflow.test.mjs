/**
 * Async Workflow Tests - Long-running work as workflow graph
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  enqueueWorkItem,
  registerExecutor,
  unregisterExecutor,
  assignWorkItem,
  startExecution,
  reportProgress,
  completeWorkItem,
  failWorkItem,
  cancelWorkItem,
  getWorkItem,
  getWorkItemReceipts,
  getQueuedItems,
  getExecutingItems,
  clearWorkItems,
  clearExecutors,
} from '../src/async-workflow.mjs';
import { WORK_ITEM_STATUS } from '../src/constants.mjs';

describe('Async Workflow', () => {
  beforeEach(() => {
    clearWorkItems();
    clearExecutors();
  });

  afterEach(() => {
    clearWorkItems();
    clearExecutors();
  });

  describe('enqueueWorkItem', () => {
    it('should create work item in queued state', () => {
      const item = enqueueWorkItem({
        type: 'file_edit',
        payload: { path: 'src/foo.mjs' },
      });

      expect(item.id).toBeDefined();
      expect(item.type).toBe('file_edit');
      expect(item.status).toBe(WORK_ITEM_STATUS.QUEUED);
    });

    it('should accept constraints', () => {
      const item = enqueueWorkItem({
        type: 'test',
        payload: {},
        constraints: {
          maxDuration: 60000000000n,
          maxRetries: 5,
        },
      });

      expect(item.constraints.maxDuration).toBe(60000000000n);
      expect(item.constraints.maxRetries).toBe(5);
    });

    it('should accept budget', () => {
      const item = enqueueWorkItem({
        type: 'test',
        payload: {},
        budget: {
          maxToolOps: 10,
        },
      });

      expect(item.budget.maxToolOps).toBe(10);
    });
  });

  describe('registerExecutor', () => {
    it('should register executor with capabilities', () => {
      registerExecutor('executor-1', ['file_edit', 'code_review']);

      // Executor should be available for matching work items
      const item = enqueueWorkItem({
        type: 'file_edit',
        payload: {},
        constraints: { requiredCapabilities: ['file_edit'] },
      });

      const result = assignWorkItem(item.id);
      expect(result.success).toBe(true);
    });
  });

  describe('assignWorkItem', () => {
    it('should assign work item to executor', () => {
      registerExecutor('exec-1', []);
      const item = enqueueWorkItem({ type: 'test', payload: {} });

      const result = assignWorkItem(item.id, 'exec-1');

      expect(result.success).toBe(true);

      const updated = getWorkItem(item.id);
      expect(updated.status).toBe(WORK_ITEM_STATUS.ASSIGNED);
      expect(updated.executorId).toBe('exec-1');
    });

    it('should fail if executor busy', () => {
      registerExecutor('exec-1', []);
      const item1 = enqueueWorkItem({ type: 'test', payload: {} });
      const item2 = enqueueWorkItem({ type: 'test', payload: {} });

      assignWorkItem(item1.id, 'exec-1');
      const result = assignWorkItem(item2.id, 'exec-1');

      expect(result.success).toBe(false);
      expect(result.reason).toContain('busy');
    });

    it('should check dependencies', () => {
      const dep = enqueueWorkItem({ type: 'dep', payload: {} });
      const item = enqueueWorkItem({
        type: 'main',
        payload: {},
        constraints: { dependencies: [dep.id] },
      });

      registerExecutor('exec-1', []);
      const result = assignWorkItem(item.id, 'exec-1');

      expect(result.success).toBe(false);
      expect(result.reason).toContain('Dependencies');
    });
  });

  describe('execution lifecycle', () => {
    it('should track full execution lifecycle', async () => {
      registerExecutor('exec-1', []);
      const item = enqueueWorkItem({ type: 'test', payload: {} });

      // Assign
      assignWorkItem(item.id, 'exec-1');

      // Start
      const startReceipt = await startExecution(item.id);
      expect(startReceipt.status).toBe('started');
      expect(getWorkItem(item.id).status).toBe(WORK_ITEM_STATUS.EXECUTING);

      // Progress
      const progressReceipt = await reportProgress(item.id, 50, { partial: true });
      expect(progressReceipt.progress).toBe(50);

      // Complete
      const completeReceipt = await completeWorkItem(item.id, { data: 'result' });
      expect(completeReceipt.status).toBe('completed');
      expect(getWorkItem(item.id).status).toBe(WORK_ITEM_STATUS.COMPLETED);
      expect(getWorkItem(item.id).result).toEqual({ data: 'result' });
    });

    it('should chain receipts', async () => {
      registerExecutor('exec-1', []);
      const item = enqueueWorkItem({ type: 'test', payload: {} });

      assignWorkItem(item.id, 'exec-1');
      await startExecution(item.id);
      await reportProgress(item.id, 50);
      await completeWorkItem(item.id, 'done');

      const receipts = getWorkItemReceipts(item.id);

      expect(receipts.length).toBe(3);
      expect(receipts[1].previousReceiptHash).toBe(receipts[0].receiptHash);
      expect(receipts[2].previousReceiptHash).toBe(receipts[1].receiptHash);
    });
  });

  describe('failWorkItem', () => {
    it('should retry up to maxRetries', async () => {
      registerExecutor('exec-1', []);
      const item = enqueueWorkItem({
        type: 'test',
        payload: {},
        constraints: { maxRetries: 2 },
      });

      // First attempt
      assignWorkItem(item.id, 'exec-1');
      await startExecution(item.id);
      await failWorkItem(item.id, 'Error 1');

      expect(getWorkItem(item.id).status).toBe(WORK_ITEM_STATUS.QUEUED);
      expect(getWorkItem(item.id).retryCount).toBe(1);

      // Second attempt
      assignWorkItem(item.id, 'exec-1');
      await startExecution(item.id);
      await failWorkItem(item.id, 'Error 2');

      expect(getWorkItem(item.id).status).toBe(WORK_ITEM_STATUS.QUEUED);
      expect(getWorkItem(item.id).retryCount).toBe(2);

      // Third attempt (exceeds retries)
      assignWorkItem(item.id, 'exec-1');
      await startExecution(item.id);
      await failWorkItem(item.id, 'Error 3');

      expect(getWorkItem(item.id).status).toBe(WORK_ITEM_STATUS.FAILED);
    });

    it('should not retry when retry=false', async () => {
      registerExecutor('exec-1', []);
      const item = enqueueWorkItem({
        type: 'test',
        payload: {},
        constraints: { maxRetries: 5 },
      });

      assignWorkItem(item.id, 'exec-1');
      await startExecution(item.id);
      await failWorkItem(item.id, 'Fatal error', false);

      expect(getWorkItem(item.id).status).toBe(WORK_ITEM_STATUS.FAILED);
    });
  });

  describe('cancelWorkItem', () => {
    it('should cancel executing work item', async () => {
      registerExecutor('exec-1', []);
      const item = enqueueWorkItem({ type: 'test', payload: {} });

      assignWorkItem(item.id, 'exec-1');
      await startExecution(item.id);

      const receipt = await cancelWorkItem(item.id);

      expect(receipt.status).toBe('cancelled');
      expect(getWorkItem(item.id).status).toBe(WORK_ITEM_STATUS.CANCELLED);
    });
  });

  describe('getQueuedItems / getExecutingItems', () => {
    it('should filter by status', async () => {
      registerExecutor('exec-1', []);

      enqueueWorkItem({ type: 'test1', payload: {} });
      enqueueWorkItem({ type: 'test2', payload: {} });
      const item3 = enqueueWorkItem({ type: 'test3', payload: {} });

      assignWorkItem(item3.id, 'exec-1');
      await startExecution(item3.id);

      expect(getQueuedItems().length).toBe(2);
      expect(getExecutingItems().length).toBe(1);
    });
  });
});
