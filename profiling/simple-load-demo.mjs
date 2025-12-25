#!/usr/bin/env node
/**
 * ADVERSARIAL LOAD TEST DEMONSTRATION
 *
 * Demonstrates profiling methodology with PROOF using simple JavaScript objects
 * to show the framework works. Real YAWL/framework tests would follow same pattern.
 *
 * @module profiling/simple-load-demo
 */

import { performance } from 'node:perf_hooks';

// ============================================================================
// MEMORY PROFILING UTILITIES
// ============================================================================

function getMemoryUsageMB() {
  const usage = process.memoryUsage();
  return {
    rss: (usage.rss / 1024 / 1024).toFixed(2),
    heapTotal: (usage.heapTotal / 1024 / 1024).toFixed(2),
    heapUsed: (usage.heapUsed / 1024 / 1024).toFixed(2),
    external: (usage.external / 1024 / 1024).toFixed(2),
  };
}

function forceGC() {
  if (global.gc) {
    global.gc();
    return true;
  }
  return false;
}

function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

// ============================================================================
// MOCK WORKFLOW ENGINE (Simulates YAWL overhead)
// ============================================================================

class MockWorkflowEngine {
  constructor() {
    this.workflows = new Map();
    this.cases = new Map();
    this.eventLog = [];
  }

  registerWorkflow(workflow) {
    this.workflows.set(workflow.id, workflow);
  }

  async createCase(workflowId, data) {
    const caseId = `case-${Date.now()}-${Math.random()}`;
    const caseObj = {
      id: caseId,
      workflowId,
      data,
      status: 'active',
      createdAt: Date.now(),
      tasks: [],
      events: [],
    };

    this.cases.set(caseId, caseObj);
    this.eventLog.push({ type: 'case_created', caseId, timestamp: Date.now() });

    return caseObj;
  }
}

class MockWorkflow {
  constructor({ id, name }) {
    this.id = id;
    this.name = name;
    this.tasks = new Map();
    this.flows = [];
  }

  addTask(task) {
    this.tasks.set(task.id, task);
  }

  addFlow(flow) {
    this.flows.push(flow);
  }

  setStart(taskId) {
    this.startTask = taskId;
  }

  setEnd(taskIds) {
    this.endTasks = taskIds;
  }
}

// ============================================================================
// BASELINE MEASUREMENT
// ============================================================================

async function measureBaseline() {
  console.log('\n=== BASELINE MEASUREMENT ===');

  forceGC();
  await sleep(100);

  const baseline = getMemoryUsageMB();
  console.log('Baseline Memory (MB):', JSON.stringify(baseline, null, 2));

  return baseline;
}

// ============================================================================
// LOAD TEST: 1000 OPERATIONS
// ============================================================================

async function runLoadTest(iterations = 1000) {
  console.log(`\n=== LOAD TEST: ${iterations} ITERATIONS ===`);

  const engine = new MockWorkflowEngine();
  const memorySnapshots = [];
  const operationTimes = [];

  const beforeLoad = getMemoryUsageMB();
  console.log('Before Load (MB):', JSON.stringify(beforeLoad, null, 2));

  const startTime = performance.now();

  // Pre-register workflow
  const templateWorkflow = new MockWorkflow({
    id: 'workflow-template',
    name: 'Test Workflow Template',
  });

  templateWorkflow.addTask({ id: 'task-1', name: 'Task 1' });
  templateWorkflow.addTask({ id: 'task-2', name: 'Task 2' });
  templateWorkflow.addFlow({ from: 'task-1', to: 'task-2' });
  templateWorkflow.setStart('task-1');
  templateWorkflow.setEnd(['task-2']);

  engine.registerWorkflow(templateWorkflow);

  for (let i = 0; i < iterations; i++) {
    const opStart = performance.now();

    await engine.createCase(templateWorkflow.id, { iteration: i });

    const opEnd = performance.now();
    operationTimes.push(opEnd - opStart);

    if (i % 100 === 0) {
      memorySnapshots.push({
        iteration: i,
        memory: getMemoryUsageMB(),
        timestamp: performance.now() - startTime,
      });
    }
  }

  const endTime = performance.now();
  const totalTime = endTime - startTime;

  const afterLoad = getMemoryUsageMB();
  console.log('After Load (MB):', JSON.stringify(afterLoad, null, 2));

  const avgOpTime = operationTimes.reduce((sum, t) => sum + t, 0) / operationTimes.length;
  const throughput = (iterations / totalTime) * 1000;

  console.log(`\nLoad Test Results:`);
  console.log(`  Total Time: ${totalTime.toFixed(2)} ms`);
  console.log(`  Average Operation Time: ${avgOpTime.toFixed(2)} ms`);
  console.log(`  Throughput: ${throughput.toFixed(2)} ops/sec`);
  console.log(`  Memory Growth: ${(parseFloat(afterLoad.heapUsed) - parseFloat(beforeLoad.heapUsed)).toFixed(2)} MB`);

  return {
    beforeLoad,
    afterLoad,
    memorySnapshots,
    operationTimes,
    totalTime,
    avgOpTime,
    throughput,
  };
}

// ============================================================================
// MEMORY LEAK DETECTION
// ============================================================================

async function detectMemoryLeaks(loadTestResults) {
  console.log('\n=== MEMORY LEAK DETECTION ===');

  const beforeGC = getMemoryUsageMB();
  console.log('Before GC (MB):', JSON.stringify(beforeGC, null, 2));

  for (let i = 0; i < 5; i++) {
    forceGC();
    await sleep(50);
  }

  const afterGC = getMemoryUsageMB();
  console.log('After GC (MB):', JSON.stringify(afterGC, null, 2));

  const heapRetained = parseFloat(afterGC.heapUsed) - parseFloat(loadTestResults.beforeLoad.heapUsed);
  const leakThreshold = 50;
  const hasLeak = heapRetained > leakThreshold;

  console.log(`\nLeak Detection:`);
  console.log(`  Heap Retained After GC: ${heapRetained.toFixed(2)} MB`);
  console.log(`  Leak Threshold: ${leakThreshold} MB`);
  console.log(`  Memory Leak Detected: ${hasLeak ? 'YES ⚠️' : 'NO ✅'}`);

  return {
    beforeGC,
    afterGC,
    heapRetained,
    hasLeak,
  };
}

// ============================================================================
// CONCURRENT LOAD TEST
// ============================================================================

async function runConcurrentTest(parallelOps = 10, opsPerWorker = 100) {
  console.log(`\n=== CONCURRENT TEST: ${parallelOps} parallel workers ===`);

  const engine = new MockWorkflowEngine();
  const beforeConcurrent = getMemoryUsageMB();

  for (let workerId = 0; workerId < parallelOps; workerId++) {
    const workflow = new MockWorkflow({
      id: `concurrent-template-${workerId}`,
      name: `Concurrent Workflow Template ${workerId}`,
    });

    workflow.addTask({ id: `task-${workerId}`, name: `Task ${workerId}` });
    workflow.setStart(`task-${workerId}`);
    workflow.setEnd([`task-${workerId}`]);

    engine.registerWorkflow(workflow);
  }

  const startTime = performance.now();

  const workers = Array.from({ length: parallelOps }, async (_, workerId) => {
    const workerTimes = [];

    for (let i = 0; i < opsPerWorker; i++) {
      const opStart = performance.now();

      await engine.createCase(`concurrent-template-${workerId}`, { workerId, iteration: i });

      workerTimes.push(performance.now() - opStart);
    }

    return {
      workerId,
      times: workerTimes,
      avgTime: workerTimes.reduce((sum, t) => sum + t, 0) / workerTimes.length,
    };
  });

  const results = await Promise.all(workers);
  const endTime = performance.now();
  const totalTime = endTime - startTime;

  const afterConcurrent = getMemoryUsageMB();

  const allTimes = results.flatMap(r => r.times);
  const avgTime = allTimes.reduce((sum, t) => sum + t, 0) / allTimes.length;
  const totalOps = parallelOps * opsPerWorker;
  const throughput = (totalOps / totalTime) * 1000;

  console.log(`\nConcurrent Test Results:`);
  console.log(`  Total Operations: ${totalOps}`);
  console.log(`  Total Time: ${totalTime.toFixed(2)} ms`);
  console.log(`  Average Operation Time: ${avgTime.toFixed(2)} ms`);
  console.log(`  Throughput: ${throughput.toFixed(2)} ops/sec`);
  console.log(`  Memory Growth: ${(parseFloat(afterConcurrent.heapUsed) - parseFloat(beforeConcurrent.heapUsed)).toFixed(2)} MB`);

  return {
    beforeConcurrent,
    afterConcurrent,
    results,
    totalTime,
    avgTime,
    throughput,
  };
}

// ============================================================================
// MAIN PROFILING EXECUTION
// ============================================================================

async function main() {
  console.log('╔════════════════════════════════════════════════════════════════╗');
  console.log('║  ADVERSARIAL LOAD TEST DEMONSTRATION                           ║');
  console.log('║  Memory & Performance Profiling Framework                     ║');
  console.log('╚════════════════════════════════════════════════════════════════╝');

  if (!global.gc) {
    console.warn('\n⚠️  WARNING: --expose-gc not enabled. Memory leak detection limited.');
    console.warn('   Run with: node --expose-gc profiling/simple-load-demo.mjs\n');
  }

  try {
    const baseline = await measureBaseline();
    const loadResults = await runLoadTest(1000);
    const leakResults = await detectMemoryLeaks(loadResults);
    const concurrentResults = await runConcurrentTest(10, 100);

    console.log('\n╔════════════════════════════════════════════════════════════════╗');
    console.log('║  FINAL PROFILING REPORT                                        ║');
    console.log('╚════════════════════════════════════════════════════════════════╝');

    console.log('\n📊 MEMORY USAGE SUMMARY (MB)');
    console.log('┌──────────────────────────────────────────────────────────────┐');
    console.log(`│ Baseline Heap:          ${baseline.heapUsed.padStart(10)} MB           │`);
    console.log(`│ After 1000 Ops:         ${loadResults.afterLoad.heapUsed.padStart(10)} MB           │`);
    console.log(`│ After GC:               ${leakResults.afterGC.heapUsed.padStart(10)} MB           │`);
    console.log(`│ Memory Growth (Load):   ${((parseFloat(loadResults.afterLoad.heapUsed) - parseFloat(baseline.heapUsed)).toFixed(2) + '').padStart(10)} MB           │`);
    console.log(`│ Retained After GC:      ${(leakResults.heapRetained.toFixed(2) + '').padStart(10)} MB           │`);
    console.log('└──────────────────────────────────────────────────────────────┘');

    console.log('\n⚡ PERFORMANCE SUMMARY');
    console.log('┌──────────────────────────────────────────────────────────────┐');
    console.log(`│ Sequential Throughput:  ${loadResults.throughput.toFixed(2).padStart(10)} ops/sec     │`);
    console.log(`│ Concurrent Throughput:  ${concurrentResults.throughput.toFixed(2).padStart(10)} ops/sec     │`);
    console.log(`│ Avg Sequential Op:      ${loadResults.avgOpTime.toFixed(2).padStart(10)} ms          │`);
    console.log(`│ Avg Concurrent Op:      ${concurrentResults.avgTime.toFixed(2).padStart(10)} ms          │`);
    console.log('└──────────────────────────────────────────────────────────────┘');

    console.log('\n🔍 VERDICT');
    console.log('┌──────────────────────────────────────────────────────────────┐');
    console.log(`│ Memory Leak:            ${leakResults.hasLeak ? 'YES ⚠️ '.padStart(15) : 'NO ✅'.padStart(15)}          │`);
    console.log(`│ Load Performance:       ${loadResults.throughput > 100 ? 'GOOD ✅'.padStart(15) : 'NEEDS WORK ⚠️'.padStart(15)}          │`);
    console.log(`│ Concurrent Performance: ${concurrentResults.throughput > 100 ? 'GOOD ✅'.padStart(15) : 'NEEDS WORK ⚠️'.padStart(15)}          │`);
    console.log('└──────────────────────────────────────────────────────────────┘');

    console.log('\n✅ Profiling demonstration completed successfully');
    console.log('\nNOTE: This demonstrates the profiling methodology.');
    console.log('Real YAWL/framework tests would follow identical pattern.');

  } catch (error) {
    console.error('\n❌ Profiling failed:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}

export { main, measureBaseline, runLoadTest, detectMemoryLeaks, runConcurrentTest };
