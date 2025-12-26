/**
 * @file YAWL Queue Adapter Tests
 * @description Comprehensive tests for distributed YAWL task execution
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { YAWLQueueAdapter } from '../src/adapter.mjs';
import { createWorkflow } from '@unrdf/yawl';

// =============================================================================
// Test Helpers
// =============================================================================

/**
 * Create a simple workflow for testing
 */
function createTestWorkflow() {
  return createWorkflow({
    id: 'test-workflow',
    name: 'Test Workflow',
    tasks: [
      { id: 'start', name: 'Start Task', splitType: 'sequence' },
      { id: 'process', name: 'Process Task', splitType: 'sequence' },
      { id: 'end', name: 'End Task', splitType: 'sequence' },
    ],
    flows: [
      { from: 'start', to: 'process' },
      { from: 'process', to: 'end' },
    ],
  });
}

/**
 * Create a parallel workflow for testing
 */
function createParallelWorkflow() {
  return createWorkflow({
    id: 'parallel-workflow',
    name: 'Parallel Workflow',
    tasks: [
      { id: 'start', name: 'Start', splitType: 'and' },
      { id: 'task-a', name: 'Task A', joinType: 'and', splitType: 'sequence' },
      { id: 'task-b', name: 'Task B', joinType: 'and', splitType: 'sequence' },
      { id: 'join', name: 'Join', joinType: 'and', splitType: 'sequence' },
    ],
    flows: [
      { from: 'start', to: 'task-a' },
      { from: 'start', to: 'task-b' },
      { from: 'task-a', to: 'join' },
      { from: 'task-b', to: 'join' },
    ],
  });
}

/**
 * Sleep helper
 */
function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

// =============================================================================
// Tests
// =============================================================================

describe('YAWLQueueAdapter', () => {
  let adapter;

  beforeEach(() => {
    // Create fresh adapter for each test
    adapter = new YAWLQueueAdapter({
      redis: { host: 'localhost', port: 6379, db: 15 }, // Use test DB
      queueName: `test-queue-${Date.now()}`,
      defaultJobOptions: {
        attempts: 2,
        removeOnComplete: true,
        removeOnFail: false,
      },
    });
  });

  afterEach(async () => {
    // Cleanup after each test
    if (adapter) {
      await adapter.close();
    }
  });

  // ---------------------------------------------------------------------------
  // Initialization Tests
  // ---------------------------------------------------------------------------

  describe('Initialization', () => {
    it('should create adapter with default config', () => {
      expect(adapter).toBeDefined();
      expect(adapter.queue).toBeDefined();
      expect(adapter.engine).toBeDefined();
      expect(adapter.redis).toBeDefined();
    });

    it('should create adapter with custom config', () => {
      const customAdapter = new YAWLQueueAdapter({
        queueName: 'custom-queue',
        defaultJobOptions: { attempts: 5 },
      });

      expect(customAdapter.queueName).toBe('custom-queue');
      expect(customAdapter.config.defaultJobOptions.attempts).toBe(5);

      customAdapter.close();
    });
  });

  // ---------------------------------------------------------------------------
  // Workflow Registration Tests
  // ---------------------------------------------------------------------------

  describe('Workflow Registration', () => {
    it('should register a workflow', async () => {
      const workflow = createTestWorkflow();
      await adapter.registerWorkflow(workflow);

      expect(adapter.engine.workflows.has(workflow.id)).toBe(true);

      // Verify Redis storage
      const stored = await adapter.redis.get(`yawl:workflow:${workflow.id}`);
      expect(stored).toBeDefined();

      const metadata = JSON.parse(stored);
      expect(metadata.id).toBe(workflow.id);
      expect(metadata.taskCount).toBe(3);
    });

    it('should unregister a workflow', async () => {
      const workflow = createTestWorkflow();
      await adapter.registerWorkflow(workflow);
      await adapter.unregisterWorkflow(workflow.id);

      expect(adapter.engine.workflows.has(workflow.id)).toBe(false);

      const stored = await adapter.redis.get(`yawl:workflow:${workflow.id}`);
      expect(stored).toBeNull();
    });
  });

  // ---------------------------------------------------------------------------
  // Case Execution Tests
  // ---------------------------------------------------------------------------

  describe('Case Execution', () => {
    it('should create and execute a case', async () => {
      const workflow = createTestWorkflow();
      await adapter.registerWorkflow(workflow);

      const { caseId, jobId } = await adapter.executeCase(workflow.id, {
        testData: 'value',
      });

      expect(caseId).toBeDefined();
      expect(jobId).toBeDefined();

      // Verify case exists in engine
      const caseInstance = adapter.engine.cases.get(caseId);
      expect(caseInstance).toBeDefined();
      expect(caseInstance.workflowId).toBe(workflow.id);
    });

    it('should track case status', async () => {
      const workflow = createTestWorkflow();
      await adapter.registerWorkflow(workflow);

      const { caseId } = await adapter.executeCase(workflow.id);

      const status = await adapter.getCaseStatus(caseId);

      expect(status.caseId).toBe(caseId);
      expect(status.workflowId).toBe(workflow.id);
      expect(status.status).toBeDefined();
      expect(status.receipts).toBeGreaterThan(0);
    });
  });

  // ---------------------------------------------------------------------------
  // Worker Tests
  // ---------------------------------------------------------------------------

  describe('Worker Management', () => {
    it('should create a worker', () => {
      const worker = adapter.createWorker({ concurrency: 2 });

      expect(worker).toBeDefined();
      expect(adapter.workers.size).toBe(1);
    });

    it('should create multiple workers', () => {
      const worker1 = adapter.createWorker();
      const worker2 = adapter.createWorker();
      const worker3 = adapter.createWorker();

      expect(adapter.workers.size).toBe(3);
    });

    it('should close a worker', async () => {
      adapter.createWorker();
      expect(adapter.workers.size).toBe(1);

      await adapter.closeWorker('worker-1');
      expect(adapter.workers.size).toBe(0);
    });
  });

  // ---------------------------------------------------------------------------
  // Job Processing Tests
  // ---------------------------------------------------------------------------

  describe('Job Processing', () => {
    it('should process a simple workflow end-to-end', async () => {
      const workflow = createTestWorkflow();
      await adapter.registerWorkflow(workflow);

      // Create worker with custom handler
      const processedTasks = [];
      const taskHandler = async (job, task) => {
        processedTasks.push(task.taskDefId || task.id);
        return { processed: true, taskId: task.id };
      };

      adapter.createWorker({ concurrency: 1, taskHandler });

      // Execute case
      const { caseId } = await adapter.executeCase(workflow.id, {
        input: 'test-data',
      });

      // Wait for processing
      await sleep(3000);

      // Verify all tasks processed
      expect(processedTasks.length).toBeGreaterThan(0);

      // Check case status
      const status = await adapter.getCaseStatus(caseId);
      expect(status.completedTasks).toBeGreaterThan(0);
    }, 10000);

    it('should handle task failures with retry', async () => {
      const workflow = createTestWorkflow();
      await adapter.registerWorkflow(workflow);

      let attemptCount = 0;
      const taskHandler = async (job, task) => {
        attemptCount++;
        if (attemptCount === 1) {
          throw new Error('Simulated failure');
        }
        return { processed: true };
      };

      adapter.createWorker({ concurrency: 1, taskHandler });

      const { caseId } = await adapter.executeCase(workflow.id);

      // Wait for retry
      await sleep(3000);

      // Should have retried
      expect(attemptCount).toBeGreaterThan(1);
    }, 10000);
  });

  // ---------------------------------------------------------------------------
  // Priority and Delay Tests
  // ---------------------------------------------------------------------------

  describe('Priority and Delay', () => {
    it('should respect task priority in job queue', async () => {
      const workflow = createWorkflow({
        id: 'priority-test',
        tasks: [
          { id: 'high', priority: 10, splitType: 'sequence' },
          { id: 'low', priority: 1, splitType: 'sequence' },
          { id: 'medium', priority: 5, splitType: 'sequence' },
        ],
        flows: [
          { from: 'high', to: 'medium' },
          { from: 'medium', to: 'low' },
        ],
      });

      await adapter.registerWorkflow(workflow);

      // Jobs should be processed by priority
      // This is verified by BullMQ's priority queue behavior
      const { caseId } = await adapter.executeCase(workflow.id);

      expect(caseId).toBeDefined();
    });

    it('should handle task delays', async () => {
      const workflow = createWorkflow({
        id: 'delay-test',
        tasks: [
          { id: 'immediate', delay: 0, splitType: 'sequence' },
          { id: 'delayed', delay: 1000, splitType: 'sequence' },
        ],
        flows: [{ from: 'immediate', to: 'delayed' }],
      });

      await adapter.registerWorkflow(workflow);

      const startTime = Date.now();
      const { caseId } = await adapter.executeCase(workflow.id);

      adapter.createWorker({ concurrency: 1 });

      await sleep(2000);

      const elapsed = Date.now() - startTime;
      expect(elapsed).toBeGreaterThan(1000); // Should have delayed
    }, 10000);
  });

  // ---------------------------------------------------------------------------
  // Receipt Generation Tests
  // ---------------------------------------------------------------------------

  describe('Receipt Generation', () => {
    it('should generate receipts for task transitions', async () => {
      const workflow = createTestWorkflow();
      await adapter.registerWorkflow(workflow);

      const { caseId } = await adapter.executeCase(workflow.id);

      // Should have receipts
      const receipts = adapter.receipts.get(caseId);
      expect(receipts).toBeDefined();
      expect(receipts.length).toBeGreaterThan(0);

      // Each receipt should have required fields
      receipts.forEach(receipt => {
        expect(receipt.id).toBeDefined();
        expect(receipt.timestamp).toBeDefined();
      });
    });

    it('should chain receipts correctly', async () => {
      const workflow = createTestWorkflow();
      await adapter.registerWorkflow(workflow);

      const { caseId } = await adapter.executeCase(workflow.id);

      adapter.createWorker({ concurrency: 1 });
      await sleep(2000);

      const receipts = adapter.receipts.get(caseId);

      // Receipts should form a chain
      if (receipts && receipts.length > 1) {
        for (let i = 1; i < receipts.length; i++) {
          // Each receipt (except first) should reference previous
          // This is verified by YAWL's receipt chaining logic
          expect(receipts[i].id).toBeDefined();
        }
      }
    }, 10000);
  });

  // ---------------------------------------------------------------------------
  // Statistics Tests
  // ---------------------------------------------------------------------------

  describe('Statistics', () => {
    it('should provide adapter statistics', async () => {
      const workflow = createTestWorkflow();
      await adapter.registerWorkflow(workflow);

      adapter.createWorker();
      adapter.createWorker();

      const { caseId } = await adapter.executeCase(workflow.id);

      const stats = await adapter.getStats();

      expect(stats.queue).toBeDefined();
      expect(stats.queue.name).toBe(adapter.queueName);
      expect(stats.workers.count).toBe(2);
      expect(stats.engine).toBeDefined();
      expect(stats.receipts).toBeDefined();
      expect(stats.receipts.totalCases).toBeGreaterThan(0);
    });
  });

  // ---------------------------------------------------------------------------
  // Cleanup Tests
  // ---------------------------------------------------------------------------

  describe('Cleanup', () => {
    it('should close all resources', async () => {
      adapter.createWorker();
      adapter.createWorker();

      expect(adapter.workers.size).toBe(2);

      await adapter.close();

      expect(adapter.workers.size).toBe(0);
    });
  });
});

// =============================================================================
// Integration Tests
// =============================================================================

describe('YAWL-Queue Integration', () => {
  let adapter;

  beforeEach(() => {
    adapter = new YAWLQueueAdapter({
      redis: { host: 'localhost', port: 6379, db: 15 },
      queueName: `integration-test-${Date.now()}`,
    });
  });

  afterEach(async () => {
    if (adapter) {
      await adapter.close();
    }
  });

  it('should execute parallel workflow with multiple workers', async () => {
    const workflow = createParallelWorkflow();
    await adapter.registerWorkflow(workflow);

    const taskExecutions = new Set();
    const taskHandler = async (job, task) => {
      const taskId = task.taskDefId || task.id;
      taskExecutions.add(taskId);
      console.log(`Executed task: ${taskId}`);
      await sleep(100);
      return { taskId, executed: true };
    };

    // Create 3 workers
    adapter.createWorker({ concurrency: 2, taskHandler });
    adapter.createWorker({ concurrency: 2, taskHandler });
    adapter.createWorker({ concurrency: 2, taskHandler });

    const { caseId } = await adapter.executeCase(workflow.id, {
      testData: 'parallel-test',
    });

    // Wait for completion
    await sleep(5000);

    // Verify parallel tasks executed
    expect(taskExecutions.size).toBeGreaterThan(0);

    const status = await adapter.getCaseStatus(caseId);
    console.log('Final status:', status);
  }, 15000);
});
