#!/usr/bin/env node

/**
 * @fileoverview Graph-YAWL - Knowledge-engine + YAWL
 * @module @unrdf/microfw-4-graph-yawl
 *
 * Adversarial Innovation: AI pattern learning + workflow orchestration = self-optimizing workflows
 * Use Case: Workflows that learn and adapt from execution history
 */

// ============================================================================
// MOCK IMPLEMENTATIONS
// ============================================================================

// Knowledge engine
class KnowledgeEngine {
  constructor() {
    this.patterns = [];
  }

  async extractPatterns(executionData) {
    const patterns = [
      {
        task: executionData.task,
        avgDuration: executionData.duration,
        confidence: Math.random() * 0.3 + 0.7,
      },
    ];
    this.patterns.push(...patterns);
    return patterns;
  }

  async optimizeWorkflow(workflow) {
    // Reorder tasks based on learned patterns
    const optimized = { ...workflow };

    // Find slowest task based on patterns
    const taskTimes = new Map();
    for (const pattern of this.patterns) {
      taskTimes.set(pattern.task, pattern.avgDuration);
    }

    // Sort tasks by duration (parallel execution opportunity)
    optimized.tasks = workflow.tasks.sort((a, b) => {
      const aTime = taskTimes.get(a.id) || 0;
      const bTime = taskTimes.get(b.id) || 0;
      return bTime - aTime; // Longest first
    });

    return optimized;
  }
}

// YAWL workflow
class WorkflowBuilder {
  constructor() {
    this.tasks = [];
    this.flows = [];
  }

  task(id, type, handler) {
    this.tasks.push({ id, type, handler });
    return this;
  }

  flow(from, to) {
    this.flows.push({ from, to });
    return this;
  }

  build() {
    return {
      tasks: this.tasks,
      flows: this.flows,
    };
  }
}

class WorkflowEngine {
  async execute(workflow, context) {
    const results = [];
    const durations = [];

    for (const task of workflow.tasks) {
      const start = Date.now();
      const result = await task.handler(context);
      const duration = Date.now() - start;

      results.push({ task: task.id, result });
      durations.push({ task: task.id, duration });
    }

    return {
      status: 'Completed',
      results,
      durations,
    };
  }
}

// ============================================================================
// GRAPH-YAWL FRAMEWORK
// ============================================================================

/**
 * GraphYawlFramework - Self-optimizing workflows
 */
class GraphYawlFramework {
  constructor() {
    this.knowledge = new KnowledgeEngine();
    this.engine = new WorkflowEngine();
    this.workflows = new Map();
    this.executionHistory = [];
    this.stats = {
      workflowsExecuted: 0,
      patternsLearned: 0,
      optimizationsApplied: 0,
    };
  }

  /**
   * Define workflow
   */
  defineWorkflow(name, definition) {
    const builder = new WorkflowBuilder();
    definition(builder);
    const workflow = builder.build();

    this.workflows.set(name, workflow);
    console.log(`[Workflow] Defined: ${name} (${workflow.tasks.length} tasks)`);

    return workflow;
  }

  /**
   * Execute workflow with learning
   */
  async executeWithLearning(name, context = {}) {
    let workflow = this.workflows.get(name);
    if (!workflow) {
      throw new Error(`Workflow not found: ${name}`);
    }

    console.log(`\n[Execute] Running workflow: ${name}`);

    // Check if optimization available
    if (this.knowledge.patterns.length > 0) {
      console.log(`  [Optimize] Applying learned optimizations...`);
      workflow = await this.knowledge.optimizeWorkflow(workflow);
      this.stats.optimizationsApplied++;
    }

    // Execute workflow
    const execution = await this.engine.execute(workflow, context);

    // Learn from execution
    for (const duration of execution.durations) {
      const patterns = await this.knowledge.extractPatterns(duration);
      this.stats.patternsLearned += patterns.length;
    }

    // Record history
    this.executionHistory.push({
      workflow: name,
      timestamp: new Date().toISOString(),
      results: execution.results,
      durations: execution.durations,
    });

    this.stats.workflowsExecuted++;

    console.log(`  [Complete] ${execution.results.length} tasks executed`);

    return execution;
  }

  /**
   * Get learned patterns
   */
  getLearnedPatterns() {
    return this.knowledge.patterns.sort((a, b) => b.confidence - a.confidence);
  }

  /**
   * Get execution history
   */
  getHistory() {
    return this.executionHistory;
  }

  /**
   * Get statistics
   */
  getStats() {
    return {
      ...this.stats,
      workflows: this.workflows.size,
      patterns: this.knowledge.patterns.length,
      executions: this.executionHistory.length,
    };
  }
}

// ============================================================================
// DEMO
// ============================================================================

async function demo() {
  console.log('╔════════════════════════════════════════════════════════════╗');
  console.log('║ Graph-YAWL Framework Demo                                  ║');
  console.log('║ Knowledge-engine + YAWL = Self-optimizing workflows        ║');
  console.log('╚════════════════════════════════════════════════════════════╝\n');

  const framework = new GraphYawlFramework();

  // Define workflow
  framework.defineWorkflow('data-pipeline', (builder) => {
    builder
      .task('fetch', 'automated', async () => {
        await new Promise(r => setTimeout(r, 50));
        return 'Data fetched';
      })
      .task('process', 'automated', async () => {
        await new Promise(r => setTimeout(r, 100));
        return 'Data processed';
      })
      .task('validate', 'automated', async () => {
        await new Promise(r => setTimeout(r, 30));
        return 'Data validated';
      })
      .task('store', 'automated', async () => {
        await new Promise(r => setTimeout(r, 20));
        return 'Data stored';
      })
      .flow('fetch', 'process')
      .flow('process', 'validate')
      .flow('validate', 'store');
  });

  console.log('\n[Demo] Executing workflow (1st run - no optimizations)...');
  await framework.executeWithLearning('data-pipeline');

  console.log('\n[Demo] Executing workflow (2nd run - with learning)...');
  await framework.executeWithLearning('data-pipeline');

  console.log('\n[Demo] Executing workflow (3rd run - optimized)...');
  await framework.executeWithLearning('data-pipeline');

  console.log('\n[Learned] Top patterns:');
  const patterns = framework.getLearnedPatterns().slice(0, 5);
  patterns.forEach((p, i) => {
    console.log(`  ${i + 1}. Task ${p.task}: ~${p.avgDuration}ms (confidence: ${p.confidence.toFixed(3)})`);
  });

  console.log('\n[Stats] Final Statistics:');
  const stats = framework.getStats();
  Object.entries(stats).forEach(([key, value]) => {
    console.log(`  ${key}: ${value}`);
  });

  console.log('\n╔════════════════════════════════════════════════════════════╗');
  console.log('║ Adversarial Innovation:                                    ║');
  console.log('║ - Knowledge engine learns task execution patterns          ║');
  console.log('║ - YAWL provides workflow orchestration structure           ║');
  console.log('║ - Workflows self-optimize based on learned patterns        ║');
  console.log('║ - AI + orchestration = adaptive execution                  ║');
  console.log('╚════════════════════════════════════════════════════════════╝');
}

if (import.meta.url === `file://${process.argv[1]}`) {
  demo().catch(console.error);
}

export { GraphYawlFramework, demo };
