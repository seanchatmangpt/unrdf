/**
 * @file Engine Performance Benchmarks
 * @module benchmarks/core/engine-performance
 *
 * @description
 * Comprehensive benchmarks for YAWL engine operations:
 * - Engine initialization
 * - Case creation and management
 * - Task execution throughput
 * - Concurrent case handling
 */

import { suite, randomString, randomInt } from '../framework.mjs';
import { YawlEngine } from '../../packages/yawl/src/engine.mjs';
import { YawlWorkflow } from '../../packages/yawl/src/workflow.mjs';
import { KGCStore } from '@unrdf/kgc-4d';

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Create a simple sequential workflow
 * @param {number} taskCount - Number of tasks
 * @returns {YawlWorkflow} Workflow instance
 */
function createSequentialWorkflow(taskCount = 3) {
  const tasks = [];
  const flows = [];

  for (let i = 0; i < taskCount; i++) {
    tasks.push({
      id: `task_${i}`,
      name: `Task ${i}`,
      type: 'atomic',
      inputs: [],
      outputs: []
    });

    if (i < taskCount - 1) {
      flows.push({
        id: `flow_${i}`,
        from: `task_${i}`,
        to: `task_${i + 1}`
      });
    }
  }

  return new YawlWorkflow(
    `workflow_${randomString(8)}`,
    'Sequential Workflow',
    '1.0.0',
    tasks,
    flows,
    [],
    []
  );
}

/**
 * Create engine instance
 * @returns {Promise<YawlEngine>} Engine instance
 */
async function createEngine() {
  const store = new KGCStore();
  const engine = new YawlEngine(store);
  await engine.initialize();
  return engine;
}

// =============================================================================
// Benchmark Suite
// =============================================================================

export const engineBenchmarks = suite('YAWL Engine Performance', {
  'engine initialization': {
    fn: async () => {
      const store = new KGCStore();
      const engine = new YawlEngine(store);
      await engine.initialize();
      return engine;
    },
    iterations: 1000,
    warmup: 100
  },

  'register workflow': {
    setup: async () => {
      const engine = await createEngine();
      const workflow = createSequentialWorkflow(5);
      return { engine, workflow };
    },
    fn: async function() {
      return await this.engine.registerWorkflow(this.workflow);
    },
    iterations: 5000,
    warmup: 500
  },

  'create case': {
    setup: async () => {
      const engine = await createEngine();
      const workflow = createSequentialWorkflow(3);
      await engine.registerWorkflow(workflow);
      return { engine, workflowId: workflow.id };
    },
    fn: async function() {
      return await this.engine.createCase(this.workflowId, {});
    },
    iterations: 5000,
    warmup: 500
  },

  'enable task (simple)': {
    setup: async () => {
      const engine = await createEngine();
      const workflow = createSequentialWorkflow(3);
      await engine.registerWorkflow(workflow);
      const caseId = await engine.createCase(workflow.id, {});
      const tasks = await engine.getEnabledTasks(caseId);
      return { engine, caseId, taskId: tasks[0].id };
    },
    fn: async function() {
      await this.engine.startTask(this.caseId, this.taskId);
      return await this.engine.completeTask(this.caseId, this.taskId, {});
    },
    iterations: 3000,
    warmup: 300
  },

  'get case status': {
    setup: async () => {
      const engine = await createEngine();
      const workflow = createSequentialWorkflow(3);
      await engine.registerWorkflow(workflow);
      const caseId = await engine.createCase(workflow.id, {});
      return { engine, caseId };
    },
    fn: async function() {
      return await this.engine.getCaseStatus(this.caseId);
    },
    iterations: 10000,
    warmup: 1000
  },

  'get enabled tasks': {
    setup: async () => {
      const engine = await createEngine();
      const workflow = createSequentialWorkflow(5);
      await engine.registerWorkflow(workflow);
      const caseId = await engine.createCase(workflow.id, {});
      return { engine, caseId };
    },
    fn: async function() {
      return await this.engine.getEnabledTasks(this.caseId);
    },
    iterations: 10000,
    warmup: 1000
  },

  'complete simple workflow': {
    setup: async () => {
      const engine = await createEngine();
      const workflow = createSequentialWorkflow(3);
      await engine.registerWorkflow(workflow);
      return { engine, workflowId: workflow.id };
    },
    fn: async function() {
      const caseId = await this.engine.createCase(this.workflowId, {});

      // Execute all tasks sequentially
      let enabledTasks = await this.engine.getEnabledTasks(caseId);
      while (enabledTasks.length > 0) {
        const task = enabledTasks[0];
        await this.engine.startTask(caseId, task.id);
        await this.engine.completeTask(caseId, task.id, {});
        enabledTasks = await this.engine.getEnabledTasks(caseId);
      }

      return caseId;
    },
    iterations: 1000,
    warmup: 100
  }
});

// =============================================================================
// Runner
// =============================================================================

if (import.meta.url === `file://${process.argv[1]}`) {
  const result = await engineBenchmarks();
  const { formatDetailedReport } = await import('../framework.mjs');
  console.log('\n' + formatDetailedReport(result));
  process.exit(0);
}
