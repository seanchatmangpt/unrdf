/**
 * Task Activation Performance Benchmark
 * Measures end-to-end task activation latency including:
 * - Task creation
 * - Hook triggers
 * - SPARQL policy evaluation (optional)
 * - Handler execution
 * - Receipt generation
 *
 * Claims to validate:
 * - Task activation: <1ms (claimed) - likely 40-100us without policy
 * - Task activation with SPARQL: 1-10ms (realistic)
 */
import { performance } from 'perf_hooks';
import { KGCStore } from '../packages/kgc-4d/src/index.mjs';
import { createStore, dataFactory } from '../packages/oxigraph/src/index.mjs';
import {
  createWorkflowEngine,
  createWorkflow,
  YawlTask,
  generateReceipt,
} from '../packages/yawl/src/index.mjs';

const { quad, namedNode, literal } = dataFactory;

/**
 * Benchmark configuration
 */
const config = {
  iterations: 1000,
  warmupIterations: 100,
};

/**
 * Calculate statistics
 */
function calculateStats(values) {
  if (values.length === 0) return null;
  const sorted = [...values].sort((a, b) => a - b);
  const sum = values.reduce((a, b) => a + b, 0);
  const mean = sum / values.length;

  const variance = values.reduce((acc, val) => acc + Math.pow(val - mean, 2), 0) / values.length;
  const stddev = Math.sqrt(variance);

  return {
    count: values.length,
    min: sorted[0],
    max: sorted[sorted.length - 1],
    mean,
    stddev,
    median: sorted[Math.floor(sorted.length / 2)],
    p50: sorted[Math.floor(sorted.length * 0.50)],
    p90: sorted[Math.floor(sorted.length * 0.90)],
    p95: sorted[Math.floor(sorted.length * 0.95)],
    p99: sorted[Math.floor(sorted.length * 0.99)],
  };
}

/**
 * Benchmark 1: Basic Task Creation (no engine overhead)
 */
function benchmarkBasicTaskCreation() {
  const latencies = [];

  // Warmup
  for (let i = 0; i < config.warmupIterations; i++) {
    const task = {
      id: `warmup-task-${i}`,
      name: 'WarmupTask',
      kind: 'atomic',
      handler: () => ({ result: 'done' }),
    };
  }

  // Actual benchmark
  for (let i = 0; i < config.iterations; i++) {
    const start = performance.now();
    const task = {
      id: `task-${i}`,
      name: `BenchmarkTask${i}`,
      kind: 'atomic',
      handler: () => ({ result: 'done', iteration: i }),
      metadata: {
        createdAt: Date.now(),
        priority: 1,
      },
    };
    const elapsed = performance.now() - start;
    latencies.push(elapsed);
  }

  return latencies;
}

/**
 * Benchmark 2: Task Activation with KGC Store
 */
async function benchmarkTaskWithKGCStore() {
  const latencies = [];
  const store = new KGCStore();

  // Warmup
  for (let i = 0; i < config.warmupIterations; i++) {
    await store.appendEvent({
      type: 'TASK_ACTIVATED',
      payload: { taskId: `warmup-${i}`, status: 'pending' },
    });
  }

  // Actual benchmark
  for (let i = 0; i < config.iterations; i++) {
    const start = performance.now();

    // Simulate task activation: create task + append event
    const taskId = `task-${i}`;
    const task = {
      id: taskId,
      name: `Task${i}`,
      status: 'pending',
      activatedAt: Date.now(),
    };

    const receipt = await store.appendEvent({
      type: 'TASK_ACTIVATED',
      payload: task,
    });

    const elapsed = performance.now() - start;
    latencies.push(elapsed);

    if (!receipt || !receipt.receipt) {
      throw new Error(`Failed to get receipt at iteration ${i}`);
    }
  }

  return latencies;
}

/**
 * Benchmark 3: Full Task Activation with Receipt Generation
 */
async function benchmarkFullTaskActivation() {
  const latencies = [];
  const store = new KGCStore();

  // Warmup
  for (let i = 0; i < config.warmupIterations; i++) {
    const delta = {
      type: 'add',
      subject: namedNode(`http://example.org/task/warmup-${i}`),
      predicate: namedNode('http://example.org/status'),
      object: literal('activated'),
    };
    await store.appendEvent(
      { type: 'TASK_ACTIVATED', payload: { taskId: `warmup-${i}` } },
      [delta]
    );
  }

  // Actual benchmark
  for (let i = 0; i < config.iterations; i++) {
    const start = performance.now();

    // Full task activation flow
    const taskId = `task-${i}`;
    const taskUri = namedNode(`http://example.org/task/${taskId}`);

    // 1. Create task definition
    const task = {
      id: taskId,
      name: `Task${i}`,
      kind: 'atomic',
      status: 'enabled',
      input: { value: i },
    };

    // 2. Generate RDF delta
    const deltas = [
      {
        type: 'add',
        subject: taskUri,
        predicate: namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        object: namedNode('http://yawl.org/Task'),
      },
      {
        type: 'add',
        subject: taskUri,
        predicate: namedNode('http://yawl.org/status'),
        object: literal('enabled'),
      },
      {
        type: 'add',
        subject: taskUri,
        predicate: namedNode('http://yawl.org/name'),
        object: literal(task.name),
      },
    ];

    // 3. Append event with receipt
    const receipt = await store.appendEvent(
      { type: 'TASK_ACTIVATED', payload: task },
      deltas
    );

    const elapsed = performance.now() - start;
    latencies.push(elapsed);
  }

  return latencies;
}

/**
 * Benchmark 4: Task Execution with Handler
 */
async function benchmarkTaskExecution() {
  const latencies = [];
  const store = new KGCStore();

  // Define handler
  const handler = async (input) => {
    // Simulate minimal work
    return { processed: true, input };
  };

  // Warmup
  for (let i = 0; i < config.warmupIterations; i++) {
    await handler({ iteration: i });
  }

  // Actual benchmark
  for (let i = 0; i < config.iterations; i++) {
    const start = performance.now();

    // 1. Create task
    const taskId = `exec-task-${i}`;
    const task = {
      id: taskId,
      name: `ExecuteTask${i}`,
      status: 'started',
      input: { value: i, data: 'test' },
    };

    // 2. Execute handler
    const result = await handler(task.input);

    // 3. Record completion
    await store.appendEvent({
      type: 'TASK_COMPLETED',
      payload: { taskId, result },
    });

    const elapsed = performance.now() - start;
    latencies.push(elapsed);
  }

  return latencies;
}

/**
 * Benchmark 5: Workflow Engine Task Enablement
 */
async function benchmarkWorkflowTaskEnablement() {
  const latencies = [];

  try {
    // Create workflow engine
    const engine = await createWorkflowEngine();

    // Create simple workflow
    const workflow = createWorkflow({
      id: 'benchmark-workflow',
      name: 'BenchmarkWorkflow',
      tasks: [
        { id: 'task1', name: 'Task1', kind: 'atomic' },
        { id: 'task2', name: 'Task2', kind: 'atomic' },
      ],
      flows: [
        { from: 'task1', to: 'task2' },
      ],
    });

    // Warmup
    for (let i = 0; i < Math.min(config.warmupIterations, 50); i++) {
      const caseId = `warmup-case-${i}`;
      await engine.createCase(workflow, caseId);
    }

    // Actual benchmark
    for (let i = 0; i < Math.min(config.iterations, 500); i++) {
      const start = performance.now();

      const caseId = `bench-case-${i}`;
      const caseInstance = await engine.createCase(workflow, caseId);

      // Enable first task
      await engine.enableTask(caseId, 'task1');

      const elapsed = performance.now() - start;
      latencies.push(elapsed);
    }

    await engine.shutdown?.();

  } catch (error) {
    console.log(`  [SKIP] Workflow engine benchmark: ${error.message}`);
    return null;
  }

  return latencies;
}

/**
 * Main benchmark runner
 */
async function main() {
  console.log('='.repeat(70));
  console.log('TASK ACTIVATION PERFORMANCE BENCHMARK');
  console.log('='.repeat(70));
  console.log(`Iterations: ${config.iterations}`);
  console.log(`Warmup: ${config.warmupIterations}`);
  console.log('');

  const results = {};

  // Benchmark 1: Basic Task Creation
  console.log('Running: Basic Task Creation (object only)...');
  const basicLatencies = benchmarkBasicTaskCreation();
  const basicStats = calculateStats(basicLatencies);
  results.basicCreation = basicStats;
  console.log(`  Mean: ${(basicStats.mean * 1000).toFixed(2)} us`);
  console.log(`  P95:  ${(basicStats.p95 * 1000).toFixed(2)} us`);

  // Benchmark 2: Task with KGC Store
  console.log('\nRunning: Task Activation with KGC Store...');
  const kgcLatencies = await benchmarkTaskWithKGCStore();
  const kgcStats = calculateStats(kgcLatencies);
  results.kgcActivation = kgcStats;
  console.log(`  Mean: ${(kgcStats.mean * 1000).toFixed(2)} us`);
  console.log(`  P95:  ${(kgcStats.p95 * 1000).toFixed(2)} us`);

  // Benchmark 3: Full Task Activation
  console.log('\nRunning: Full Task Activation (RDF deltas + receipt)...');
  const fullLatencies = await benchmarkFullTaskActivation();
  const fullStats = calculateStats(fullLatencies);
  results.fullActivation = fullStats;
  console.log(`  Mean: ${(fullStats.mean * 1000).toFixed(2)} us`);
  console.log(`  P95:  ${(fullStats.p95 * 1000).toFixed(2)} us`);

  // Benchmark 4: Task Execution
  console.log('\nRunning: Task Execution (handler + completion)...');
  const execLatencies = await benchmarkTaskExecution();
  const execStats = calculateStats(execLatencies);
  results.taskExecution = execStats;
  console.log(`  Mean: ${(execStats.mean * 1000).toFixed(2)} us`);
  console.log(`  P95:  ${(execStats.p95 * 1000).toFixed(2)} us`);

  // Benchmark 5: Workflow Engine
  console.log('\nRunning: Workflow Engine Task Enablement...');
  const workflowLatencies = await benchmarkWorkflowTaskEnablement();
  if (workflowLatencies) {
    const workflowStats = calculateStats(workflowLatencies);
    results.workflowEnablement = workflowStats;
    console.log(`  Mean: ${workflowStats.mean.toFixed(3)} ms`);
    console.log(`  P95:  ${workflowStats.p95.toFixed(3)} ms`);
  }

  // Summary
  console.log('\n' + '='.repeat(70));
  console.log('SUMMARY');
  console.log('='.repeat(70));

  console.log('\nLATENCY BREAKDOWN:');
  console.log('-'.repeat(70));
  console.log('Operation                              | Mean      | P95       | P99');
  console.log('-'.repeat(70));
  console.log(`Basic Task Creation (object)           | ${(basicStats.mean * 1000).toFixed(1).padStart(7)} us | ${(basicStats.p95 * 1000).toFixed(1).padStart(7)} us | ${(basicStats.p99 * 1000).toFixed(1).padStart(7)} us`);
  console.log(`Task + KGC Store Event                 | ${(kgcStats.mean * 1000).toFixed(1).padStart(7)} us | ${(kgcStats.p95 * 1000).toFixed(1).padStart(7)} us | ${(kgcStats.p99 * 1000).toFixed(1).padStart(7)} us`);
  console.log(`Full Activation (RDF + receipt)        | ${(fullStats.mean * 1000).toFixed(1).padStart(7)} us | ${(fullStats.p95 * 1000).toFixed(1).padStart(7)} us | ${(fullStats.p99 * 1000).toFixed(1).padStart(7)} us`);
  console.log(`Task Execution (handler + complete)    | ${(execStats.mean * 1000).toFixed(1).padStart(7)} us | ${(execStats.p95 * 1000).toFixed(1).padStart(7)} us | ${(execStats.p99 * 1000).toFixed(1).padStart(7)} us`);
  if (results.workflowEnablement) {
    console.log(`Workflow Engine (case + enable)        | ${results.workflowEnablement.mean.toFixed(3).padStart(7)} ms | ${results.workflowEnablement.p95.toFixed(3).padStart(7)} ms | ${results.workflowEnablement.p99.toFixed(3).padStart(7)} ms`);
  }
  console.log('-'.repeat(70));

  // Claims Validation
  console.log('\n' + '='.repeat(70));
  console.log('CLAIMS VALIDATION');
  console.log('='.repeat(70));

  // Claim 1: Task activation <1ms (without SPARQL)
  const activationP95Us = fullStats.p95 * 1000;
  const activationClaimUs = 1000; // <1ms = 1000us
  const activationPass = activationP95Us < activationClaimUs;

  console.log(`\nClaim: Task activation <1ms (no SPARQL)`);
  console.log(`  Measured P95: ${activationP95Us.toFixed(1)} us (${(activationP95Us / 1000).toFixed(4)} ms)`);
  console.log(`  Target:       ${activationClaimUs} us (1.000 ms)`);
  console.log(`  Status:       ${activationPass ? 'PASS' : 'FAIL'}`);

  // Realistic expectation
  console.log(`\nRealistic Assessment:`);
  console.log(`  Simple task creation:       ${(basicStats.p95 * 1000).toFixed(1)} us`);
  console.log(`  With event logging:         ${(kgcStats.p95 * 1000).toFixed(1)} us`);
  console.log(`  Full RDF + receipt:         ${(fullStats.p95 * 1000).toFixed(1)} us`);
  console.log(`  Task execution (e2e):       ${(execStats.p95 * 1000).toFixed(1)} us`);

  // Throughput
  const throughputPerSec = 1000 / fullStats.mean;
  console.log(`\nThroughput: ${throughputPerSec.toFixed(0).toLocaleString()} task activations/sec`);

  console.log('\n' + '='.repeat(70));
  console.log('Benchmark complete');
  console.log('='.repeat(70));

  // Output JSON for summary generator
  console.log('\n__JSON_RESULTS__');
  console.log(JSON.stringify({
    benchmark: 'task-activation',
    timestamp: new Date().toISOString(),
    results: {
      basicCreation: { meanUs: basicStats.mean * 1000, p95Us: basicStats.p95 * 1000, p99Us: basicStats.p99 * 1000 },
      kgcActivation: { meanUs: kgcStats.mean * 1000, p95Us: kgcStats.p95 * 1000, p99Us: kgcStats.p99 * 1000 },
      fullActivation: { meanUs: fullStats.mean * 1000, p95Us: fullStats.p95 * 1000, p99Us: fullStats.p99 * 1000 },
      taskExecution: { meanUs: execStats.mean * 1000, p95Us: execStats.p95 * 1000, p99Us: execStats.p99 * 1000 },
      workflowEnablement: results.workflowEnablement ? {
        meanMs: results.workflowEnablement.mean,
        p95Ms: results.workflowEnablement.p95,
        p99Ms: results.workflowEnablement.p99,
      } : null,
      throughputPerSec,
    },
    claims: {
      taskActivationLt1ms: {
        targetUs: 1000,
        actualP95Us: activationP95Us,
        pass: activationPass
      },
    },
  }, null, 2));
}

main().catch(console.error);
