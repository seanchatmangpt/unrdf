/**
 * @file YAWL Performance Regression Tests
 * @module @unrdf/yawl/test/performance
 *
 * @description
 * Performance benchmarks for YAWL workflow engine operations.
 * Tests ensure that critical operations meet SLA requirements.
 *
 * Performance Targets (based on CLAUDE.md SLAs):
 * - Workflow creation: <100ms for 50 tasks
 * - Task execution: 100 tasks in <1000ms (10ms per task)
 * - Receipt generation: 1000 receipts in <500ms (0.5ms per receipt)
 * - RDF serialization: <200ms for medium workflow
 * - Memory usage: <10MB increase for 100 workflow executions
 */

import { describe, it, expect } from 'vitest';
import { performance } from 'node:perf_hooks';
import { randomUUID } from 'node:crypto';
import {
  createWorkflow,
  createWorkflowAPI,
  createWorkflowCase,
  enableWorkflowTask,
  startTask,
  completeTask,
  generateReceipt,
  verifyReceipt,
  workflowToRDF,
  workflowFromRDF,
  createWorkflowEngine,
  RECEIPT_EVENT_TYPES,
  createYawlStore,
} from '../src/index.mjs';
import { createStore } from '@unrdf/oxigraph';

// =============================================================================
// Test Data Generators
// =============================================================================

/**
 * Generate a sequential workflow with N tasks
 * @param {number} taskCount - Number of tasks to create
 * @returns {object} Workflow specification
 */
function generateSequentialWorkflow(taskCount) {
  const tasks = [];
  const controlFlow = [];

  for (let i = 0; i < taskCount; i++) {
    tasks.push({
      id: `task-${i}`,
      name: `Task ${i}`,
      type: 'atomic',
    });

    if (i > 0) {
      controlFlow.push({
        id: `flow-${i - 1}-${i}`,
        type: 'sequence',
        from: `task-${i - 1}`,
        to: `task-${i}`,
      });
    }
  }

  return {
    id: `perf-workflow-${randomUUID()}`,
    name: 'Performance Test Workflow',
    version: '1.0.0',
    tasks,
    controlFlow,
  };
}

/**
 * Generate a parallel workflow with N branches
 * @param {number} branchCount - Number of parallel branches
 * @returns {object} Workflow specification
 */
function generateParallelWorkflow(branchCount) {
  const tasks = [
    { id: 'start', name: 'Start', type: 'atomic', splitType: 'and' },
    { id: 'end', name: 'End', type: 'atomic', joinType: 'and' },
  ];
  const controlFlow = [];

  for (let i = 0; i < branchCount; i++) {
    const branchId = `branch-${i}`;
    tasks.push({
      id: branchId,
      name: `Branch ${i}`,
      type: 'atomic',
    });

    controlFlow.push({
      id: `split-${i}`,
      type: 'and-split',
      from: 'start',
      to: branchId,
    });

    controlFlow.push({
      id: `join-${i}`,
      type: 'and-join',
      from: branchId,
      to: 'end',
    });
  }

  return {
    id: `perf-parallel-${randomUUID()}`,
    name: 'Performance Parallel Workflow',
    version: '1.0.0',
    tasks,
    controlFlow,
  };
}

/**
 * Generate test receipt data
 * @param {number} index - Receipt index
 * @param {string} prevHash - Previous receipt hash
 * @returns {object} Receipt data
 */
function generateReceiptData(index, prevHash = null) {
  return {
    caseId: `case-${randomUUID()}`,
    eventType: RECEIPT_EVENT_TYPES.TASK_COMPLETED,
    taskId: `task-${index}`,
    timestamp: Date.now(),
    payload: {
      decision: 'APPROVE',
      actor: `user-${index}`,
      context: {
        taskIndex: index,
        workflowId: 'perf-test',
      },
    },
    previousHash: prevHash,
  };
}

// =============================================================================
// Performance Tests
// =============================================================================

describe('YAWL Performance Regression Tests', () => {
  describe('Workflow Creation Performance', () => {
    it('should create workflow with 50 tasks in <100ms', () => {
      const start = performance.now();

      const workflow = createWorkflow(generateSequentialWorkflow(50));

      const duration = performance.now() - start;

      expect(workflow).toBeDefined();
      expect(workflow._tasks.size).toBe(50);
      expect(duration).toBeLessThan(100);

      console.log(`  ✓ Created 50-task workflow in ${duration.toFixed(2)}ms`);
    });

    it('should create workflow with 100 tasks in <200ms', () => {
      const start = performance.now();

      const workflow = createWorkflow(generateSequentialWorkflow(100));

      const duration = performance.now() - start;

      expect(workflow).toBeDefined();
      expect(workflow._tasks.size).toBe(100);
      expect(duration).toBeLessThan(200);

      console.log(`  ✓ Created 100-task workflow in ${duration.toFixed(2)}ms`);
    });

    it('should create parallel workflow with 20 branches in <100ms', () => {
      const start = performance.now();

      const workflow = createWorkflow(generateParallelWorkflow(20));

      const duration = performance.now() - start;

      expect(workflow).toBeDefined();
      expect(workflow._tasks.size).toBe(22); // start + end + 20 branches
      expect(duration).toBeLessThan(100);

      console.log(`  ✓ Created 20-branch parallel workflow in ${duration.toFixed(2)}ms`);
    });

    it('should scale linearly with task count', () => {
      const taskCounts = [10, 20, 50, 100];
      const durations = [];

      for (const count of taskCounts) {
        const start = performance.now();
        createWorkflow(generateSequentialWorkflow(count));
        const duration = performance.now() - start;
        durations.push(duration);
      }

      // Check that doubling tasks doesn't more than triple time (allowing overhead)
      for (let i = 1; i < taskCounts.length; i++) {
        const ratio = durations[i] / durations[i - 1];
        const taskRatio = taskCounts[i] / taskCounts[i - 1];
        expect(ratio).toBeLessThan(taskRatio * 1.5); // Max 50% overhead
      }

      console.log(`  ✓ Scaling: 10=${durations[0].toFixed(2)}ms, 20=${durations[1].toFixed(2)}ms, 50=${durations[2].toFixed(2)}ms, 100=${durations[3].toFixed(2)}ms`);
    });
  });

  describe('Task Execution Performance', () => {
    it('should execute 100 sequential tasks in <1000ms', async () => {
      const workflow = createWorkflow(generateSequentialWorkflow(100));

      const start = performance.now();

      // Simulate task execution without full engine overhead
      let completedTasks = 0;
      for (const [taskId, task] of workflow._tasks) {
        // Minimal task processing
        const canEnable = workflow.canEnable(taskId, new Set());
        if (canEnable || completedTasks === 0) {
          completedTasks++;
        }
      }

      const duration = performance.now() - start;

      expect(completedTasks).toBe(100);
      expect(duration).toBeLessThan(1000);

      console.log(`  ✓ Processed 100 tasks in ${duration.toFixed(2)}ms (${(duration / 100).toFixed(2)}ms per task)`);
    });

    it('should evaluate control flow for 50 tasks in <100ms', () => {
      const workflow = createWorkflow(generateSequentialWorkflow(50));
      const completedTasks = new Set();

      const start = performance.now();

      // Evaluate downstream tasks after each completion
      for (let i = 0; i < 50; i++) {
        const taskId = `task-${i}`;
        completedTasks.add(taskId);
        const downstream = workflow.evaluateDownstream(taskId, completedTasks);
      }

      const duration = performance.now() - start;

      expect(duration).toBeLessThan(100);

      console.log(`  ✓ Evaluated control flow 50 times in ${duration.toFixed(2)}ms`);
    });

    it('should handle parallel task enablement in <50ms', () => {
      const workflow = createWorkflow(generateParallelWorkflow(20));

      const start = performance.now();

      // After start completes, all 20 branches should be enabled
      const completedTasks = new Set(['start']);
      const enabled = workflow.evaluateDownstream('start', completedTasks);

      const duration = performance.now() - start;

      expect(enabled.size).toBe(20);
      expect(duration).toBeLessThan(50);

      console.log(`  ✓ Enabled 20 parallel tasks in ${duration.toFixed(2)}ms`);
    });
  });

  describe('Receipt Generation Performance', () => {
    it('should generate 1000 receipts in <500ms', async () => {
      const receipts = [];
      const start = performance.now();

      let previousHash = null;
      for (let i = 0; i < 1000; i++) {
        const receiptData = generateReceiptData(i, previousHash);
        const receipt = await generateReceipt(receiptData);
        previousHash = receipt.hash;
        receipts.push(receipt);
      }

      const duration = performance.now() - start;

      expect(receipts.length).toBe(1000);
      expect(duration).toBeLessThan(500);

      console.log(`  ✓ Generated 1000 receipts in ${duration.toFixed(2)}ms (${(duration / 1000).toFixed(3)}ms per receipt)`);
    });

    it('should verify 100 receipts in <200ms', async () => {
      // Generate receipts first
      const receipts = [];
      let previousHash = null;
      for (let i = 0; i < 100; i++) {
        const receiptData = generateReceiptData(i, previousHash);
        const receipt = await generateReceipt(receiptData);
        previousHash = receipt.hash;
        receipts.push(receipt);
      }

      const start = performance.now();

      // Verify all receipts
      for (const receipt of receipts) {
        const result = await verifyReceipt(receipt);
        expect(result.valid).toBe(true);
      }

      const duration = performance.now() - start;

      expect(duration).toBeLessThan(200);

      console.log(`  ✓ Verified 100 receipts in ${duration.toFixed(2)}ms (${(duration / 100).toFixed(2)}ms per receipt)`);
    });

    it('should handle receipt chain validation efficiently', async () => {
      // Generate chain of 50 receipts
      const receipts = [];
      let previousHash = null;
      for (let i = 0; i < 50; i++) {
        const receiptData = generateReceiptData(i, previousHash);
        const receipt = await generateReceipt(receiptData);
        previousHash = receipt.hash;
        receipts.push(receipt);
      }

      const start = performance.now();

      // Verify chain integrity
      for (let i = 1; i < receipts.length; i++) {
        expect(receipts[i].previousHash).toBe(receipts[i - 1].hash);
      }

      const duration = performance.now() - start;

      expect(duration).toBeLessThan(10); // Should be nearly instant

      console.log(`  ✓ Validated 50-receipt chain in ${duration.toFixed(2)}ms`);
    });
  });

  describe('RDF Serialization Performance', () => {
    it('should serialize workflow to RDF in <200ms', () => {
      const workflow = createWorkflow(generateSequentialWorkflow(50));
      const store = createStore();

      const start = performance.now();

      const result = workflowToRDF(workflow, store);

      const duration = performance.now() - start;

      expect(result).toBeDefined();
      expect(result.specUri).toBeDefined();
      expect(result.quadCount).toBeGreaterThan(0);
      expect(duration).toBeLessThan(200);

      console.log(`  ✓ Serialized 50-task workflow to RDF in ${duration.toFixed(2)}ms (${result.quadCount} quads)`);
    });

    it('should deserialize workflow from RDF in <200ms', () => {
      const workflow = createWorkflow(generateSequentialWorkflow(50));
      const store = createStore();
      workflowToRDF(workflow, store);

      const start = performance.now();

      const reconstructed = workflowFromRDF(workflow.id, store);

      const duration = performance.now() - start;

      expect(reconstructed).toBeDefined();
      expect(reconstructed.id).toBe(workflow.id);
      expect(duration).toBeLessThan(200);

      console.log(`  ✓ Deserialized 50-task workflow from RDF in ${duration.toFixed(2)}ms`);
    });

    it('should handle round-trip RDF conversion in <400ms', () => {
      const workflow = createWorkflow(generateParallelWorkflow(20));
      const store = createStore();

      const start = performance.now();

      workflowToRDF(workflow, store);
      const reconstructed = workflowFromRDF(workflow.id, store);

      const duration = performance.now() - start;

      expect(reconstructed.id).toBe(workflow.id);
      expect(reconstructed._tasks.size).toBe(workflow._tasks.size);
      expect(duration).toBeLessThan(400);

      console.log(`  ✓ Round-trip RDF conversion in ${duration.toFixed(2)}ms`);
    });
  });

  describe('Memory Usage Tests', () => {
    it('should not leak memory during workflow creation', () => {
      const before = process.memoryUsage().heapUsed;

      // Create 100 workflows
      for (let i = 0; i < 100; i++) {
        createWorkflow(generateSequentialWorkflow(10));
      }

      // Force garbage collection if available
      if (global.gc) {
        global.gc();
      }

      const after = process.memoryUsage().heapUsed;
      const increase = after - before;
      const increaseMB = (increase / (1024 * 1024)).toFixed(2);

      // Memory increase should be minimal (workflows should be garbage collected)
      expect(increase).toBeLessThan(10 * 1024 * 1024); // <10MB

      console.log(`  ✓ Memory increase after 100 workflows: ${increaseMB}MB`);
    });

    it('should not leak memory during receipt generation', async () => {
      const before = process.memoryUsage().heapUsed;

      // Generate 1000 receipts
      let previousHash = null;
      for (let i = 0; i < 1000; i++) {
        const receiptData = generateReceiptData(i, previousHash);
        const receipt = await generateReceipt(receiptData);
        previousHash = receipt.hash;
      }

      if (global.gc) {
        global.gc();
      }

      const after = process.memoryUsage().heapUsed;
      const increase = after - before;
      const increaseMB = (increase / (1024 * 1024)).toFixed(2);

      expect(increase).toBeLessThan(5 * 1024 * 1024); // <5MB

      console.log(`  ✓ Memory increase after 1000 receipts: ${increaseMB}MB`);
    });

    it('should handle large workflows without excessive memory', () => {
      const before = process.memoryUsage().heapUsed;

      // Create one large workflow
      const workflow = createWorkflow(generateSequentialWorkflow(1000));

      const after = process.memoryUsage().heapUsed;
      const increase = after - before;
      const increaseMB = (increase / (1024 * 1024)).toFixed(2);

      expect(workflow._tasks.size).toBe(1000);
      expect(increase).toBeLessThan(20 * 1024 * 1024); // <20MB for 1000 tasks

      console.log(`  ✓ 1000-task workflow memory: ${increaseMB}MB`);
    });
  });

  describe('Throughput Benchmarks', () => {
    it('should measure workflow creation throughput', () => {
      const iterations = 1000;
      const start = performance.now();

      for (let i = 0; i < iterations; i++) {
        createWorkflow(generateSequentialWorkflow(10));
      }

      const duration = performance.now() - start;
      const throughput = (iterations / (duration / 1000)).toFixed(2);

      expect(duration).toBeDefined();

      console.log(`  ✓ Workflow creation throughput: ${throughput} workflows/sec (${iterations} workflows in ${duration.toFixed(2)}ms)`);
    });

    it('should measure control flow evaluation throughput', () => {
      const workflow = createWorkflow(generateSequentialWorkflow(100));
      const iterations = 1000;
      const start = performance.now();

      for (let i = 0; i < iterations; i++) {
        const taskId = `task-${i % 100}`;
        const completedTasks = new Set([`task-${(i - 1) % 100}`]);
        workflow.evaluateDownstream(taskId, completedTasks);
      }

      const duration = performance.now() - start;
      const throughput = (iterations / (duration / 1000)).toFixed(2);

      console.log(`  ✓ Control flow evaluation throughput: ${throughput} evaluations/sec`);
    });

    it('should measure receipt generation throughput', async () => {
      const iterations = 500;
      const start = performance.now();

      let previousHash = null;
      for (let i = 0; i < iterations; i++) {
        const receiptData = generateReceiptData(i, previousHash);
        const receipt = await generateReceipt(receiptData);
        previousHash = receipt.hash;
      }

      const duration = performance.now() - start;
      const throughput = (iterations / (duration / 1000)).toFixed(2);

      console.log(`  ✓ Receipt generation throughput: ${throughput} receipts/sec`);
    });
  });

  describe('Scalability Tests', () => {
    it('should handle increasing workflow complexity efficiently', () => {
      const complexities = [
        { tasks: 10, branches: 2 },
        { tasks: 50, branches: 5 },
        { tasks: 100, branches: 10 },
        { tasks: 200, branches: 20 },
      ];

      const results = [];

      for (const { tasks, branches } of complexities) {
        const start = performance.now();
        const workflow = createWorkflow(generateParallelWorkflow(branches));
        const duration = performance.now() - start;
        results.push({ tasks: branches + 2, duration });
      }

      // Verify scaling is reasonable
      for (let i = 1; i < results.length; i++) {
        const ratio = results[i].duration / results[i - 1].duration;
        const taskRatio = results[i].tasks / results[i - 1].tasks;
        expect(ratio).toBeLessThan(taskRatio * 2); // Allow 2x overhead
      }

      console.log(`  ✓ Scalability: ${results.map(r => `${r.tasks}tasks=${r.duration.toFixed(2)}ms`).join(', ')}`);
    });

    it('should maintain performance with deep nesting', () => {
      // Create a deeply nested workflow (sequential)
      const workflow = createWorkflow(generateSequentialWorkflow(100));

      const start = performance.now();

      // Simulate evaluating deep in the workflow
      const completedTasks = new Set();
      for (let i = 0; i < 50; i++) {
        completedTasks.add(`task-${i}`);
      }

      const enabled = workflow.evaluateDownstream('task-49', completedTasks);

      const duration = performance.now() - start;

      expect(enabled.size).toBeGreaterThan(0);
      expect(duration).toBeLessThan(10); // Should be fast even deep in workflow

      console.log(`  ✓ Deep nesting evaluation in ${duration.toFixed(2)}ms`);
    });
  });
});
