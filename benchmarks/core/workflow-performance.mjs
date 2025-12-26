/**
 * @file Workflow Performance Benchmarks
 * @module benchmarks/core/workflow-performance
 *
 * @description
 * Comprehensive benchmarks for YAWL workflow operations:
 * - Workflow creation speed
 * - Workflow validation
 * - Workflow serialization to RDF
 * - Workflow loading from storage
 */

import { suite, randomString, randomInt } from '../framework.mjs';
import { YawlWorkflow } from '../../packages/yawl/src/workflow.mjs';
import { dataFactory } from '@unrdf/oxigraph';

const { namedNode, literal } = dataFactory;

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Generate sample workflow definition
 * @param {number} taskCount - Number of tasks to generate
 * @returns {object} Workflow definition
 */
function generateWorkflowDefinition(taskCount = 5) {
  const tasks = [];
  const flows = [];

  for (let i = 0; i < taskCount; i++) {
    tasks.push({
      id: `task_${i}`,
      name: `Task ${i}`,
      type: i === 0 ? 'atomic' : (i === taskCount - 1 ? 'atomic' : 'composite'),
      split: i < taskCount - 1 ? 'AND' : undefined,
      join: i > 0 ? 'AND' : undefined,
      inputs: [`input_${i}`],
      outputs: [`output_${i}`]
    });

    if (i < taskCount - 1) {
      flows.push({
        id: `flow_${i}`,
        from: `task_${i}`,
        to: `task_${i + 1}`,
        condition: null
      });
    }
  }

  return {
    id: `workflow_${randomString(8)}`,
    name: `Benchmark Workflow ${randomInt(1, 1000)}`,
    version: '1.0.0',
    tasks,
    flows,
    inputParams: tasks[0].inputs.map(id => ({ id, type: 'string' })),
    outputParams: tasks[tasks.length - 1].outputs.map(id => ({ id, type: 'string' }))
  };
}

/**
 * Create workflow instance
 * @param {object} definition - Workflow definition
 * @returns {YawlWorkflow} Workflow instance
 */
function createWorkflowInstance(definition) {
  return new YawlWorkflow(
    definition.id,
    definition.name,
    definition.version,
    definition.tasks,
    definition.flows,
    definition.inputParams,
    definition.outputParams
  );
}

// =============================================================================
// Benchmark Suite
// =============================================================================

export const workflowBenchmarks = suite('YAWL Workflow Performance', {
  'create simple workflow (5 tasks)': {
    fn: () => {
      const def = generateWorkflowDefinition(5);
      return createWorkflowInstance(def);
    },
    iterations: 10000,
    warmup: 1000
  },

  'create medium workflow (20 tasks)': {
    fn: () => {
      const def = generateWorkflowDefinition(20);
      return createWorkflowInstance(def);
    },
    iterations: 5000,
    warmup: 500
  },

  'create large workflow (100 tasks)': {
    fn: () => {
      const def = generateWorkflowDefinition(100);
      return createWorkflowInstance(def);
    },
    iterations: 1000,
    warmup: 100
  },

  'validate workflow definition': {
    setup: () => {
      return { def: generateWorkflowDefinition(10) };
    },
    fn: function() {
      const workflow = createWorkflowInstance(this.def);
      return workflow.validate();
    },
    iterations: 5000,
    warmup: 500
  },

  'serialize workflow to RDF': {
    setup: () => {
      const def = generateWorkflowDefinition(10);
      return { workflow: createWorkflowInstance(def) };
    },
    fn: function() {
      return this.workflow.toRDF();
    },
    iterations: 1000,
    warmup: 100
  },

  'get workflow task by id': {
    setup: () => {
      const def = generateWorkflowDefinition(50);
      const workflow = createWorkflowInstance(def);
      return { workflow, taskId: 'task_25' };
    },
    fn: function() {
      return this.workflow.getTask(this.taskId);
    },
    iterations: 100000,
    warmup: 10000
  },

  'find workflows by condition': {
    setup: () => {
      const workflows = [];
      for (let i = 0; i < 100; i++) {
        const def = generateWorkflowDefinition(10);
        workflows.push(createWorkflowInstance(def));
      }
      return { workflows };
    },
    fn: function() {
      return this.workflows.filter(w => w.tasks.length > 5);
    },
    iterations: 10000,
    warmup: 1000
  }
});

// =============================================================================
// Runner
// =============================================================================

if (import.meta.url === `file://${process.argv[1]}`) {
  const result = await workflowBenchmarks();
  const { formatDetailedReport } = await import('../framework.mjs');
  console.log('\n' + formatDetailedReport(result));
  process.exit(0);
}
