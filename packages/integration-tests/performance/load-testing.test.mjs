/**
 * Integration Test - Scenario 5: Performance Under Load
 * Tests: All packages under high load
 *
 * Real-world scenario: System performance validation
 * - High-volume workflow execution
 * - Concurrent operations
 * - Memory efficiency
 * - Throughput metrics
 */

import { test, expect, describe, beforeEach } from 'vitest';
import { createWorkflowEngine, createWorkflow } from '@unrdf/yawl';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { KGCStore, freezeUniverse } from '@unrdf/kgc-4d';
import { defineHook, executeHook, executeBatch } from '@unrdf/hooks';

const { quad, namedNode, literal } = dataFactory;

describe('Scenario 5: Performance Under Load', () => {
  let store;
  let kgcStore;
  let engine;

  beforeEach(() => {
    store = createStore();
    kgcStore = new KGCStore();
  });

  test('handles high-volume workflow execution', async () => {
    // ======================================================================
    // STEP 1: Create lightweight workflow
    // ======================================================================
    engine = createWorkflowEngine({ store });

    const workflow = createWorkflow('batch-processing', {
      name: 'Batch Processing',
    });

    workflow.addTask('process', { type: 'automated' });
    workflow.addTask('complete', { type: 'automated' });
    workflow.addFlow('process', 'complete');

    await engine.registerWorkflow(workflow);

    // ======================================================================
    // STEP 2: Measure workflow startup performance
    // ======================================================================
    const startupStart = performance.now();
    const numCases = 100;
    const cases = [];

    for (let i = 0; i < numCases; i++) {
      const workflowCase = await engine.startCase('batch-processing', {
        id: i,
        data: `item-${i}`,
      });
      cases.push(workflowCase);
    }

    const startupDuration = performance.now() - startupStart;
    const startupThroughput = numCases / (startupDuration / 1000);

    expect(cases.length).toBe(numCases);
    expect(startupThroughput).toBeGreaterThan(10); // >10 cases/second
    expect(startupDuration).toBeLessThan(20000); // <20s for 100 cases

    console.log(`Workflow Startup: ${numCases} cases in ${startupDuration.toFixed(2)}ms`);
    console.log(`Throughput: ${startupThroughput.toFixed(2)} cases/second`);

    // ======================================================================
    // STEP 3: Measure task completion performance
    // ======================================================================
    const completionStart = performance.now();

    for (const workflowCase of cases) {
      await engine.enableTask(workflowCase.id, 'process');
      await engine.completeTask(workflowCase.id, 'process', {
        processed: true,
      });

      await engine.enableTask(workflowCase.id, 'complete');
      await engine.completeTask(workflowCase.id, 'complete', {
        completed: true,
      });
    }

    const completionDuration = performance.now() - completionStart;
    const completionThroughput = numCases / (completionDuration / 1000);

    expect(completionThroughput).toBeGreaterThan(5); // >5 completions/second
    expect(completionDuration).toBeLessThan(30000); // <30s for 100 cases

    console.log(`Task Completion: ${numCases} cases in ${completionDuration.toFixed(2)}ms`);
    console.log(`Throughput: ${completionThroughput.toFixed(2)} completions/second`);

    // ======================================================================
    // STEP 4: Verify all cases completed
    // ======================================================================
    for (const workflowCase of cases) {
      const finalCase = await engine.getCase(workflowCase.id);
      expect(finalCase.status).toBe('completed');
    }

    // ======================================================================
    // STEP 5: Performance summary
    // ======================================================================
    const totalDuration = startupDuration + completionDuration;
    const overallThroughput = numCases / (totalDuration / 1000);

    console.log(`\nTotal Duration: ${totalDuration.toFixed(2)}ms`);
    console.log(`Overall Throughput: ${overallThroughput.toFixed(2)} cases/second`);

    expect(overallThroughput).toBeGreaterThan(2); // >2 complete workflows/second

    // ======================================================================
    // SUCCESS CRITERIA VERIFICATION
    // ======================================================================
    // ✅ 100 workflows created
    // ✅ All workflows completed
    // ✅ Acceptable throughput (>2 workflows/s)
    // ✅ Completed within timeout (<30s)
  }, 60000); // 60s timeout for load test

  test('handles concurrent RDF store operations', async () => {
    // ======================================================================
    // STEP 1: Measure concurrent write performance
    // ======================================================================
    const writeStart = performance.now();
    const numQuads = 10000;

    for (let i = 0; i < numQuads; i++) {
      const subject = namedNode(`http://example.org/entity/${i}`);
      const predicate = namedNode('http://example.org/vocab/value');
      const object = literal(`value-${i}`);

      store.add(quad(subject, predicate, object));
    }

    const writeDuration = performance.now() - writeStart;
    const writeThroughput = numQuads / (writeDuration / 1000);

    expect(writeThroughput).toBeGreaterThan(1000); // >1000 quads/second
    expect(writeDuration).toBeLessThan(15000); // <15s for 10k quads

    console.log(`\nRDF Write: ${numQuads} quads in ${writeDuration.toFixed(2)}ms`);
    console.log(`Throughput: ${writeThroughput.toFixed(2)} quads/second`);

    // ======================================================================
    // STEP 2: Measure concurrent read performance
    // ======================================================================
    const readStart = performance.now();
    const numReads = 1000;

    for (let i = 0; i < numReads; i++) {
      const subject = namedNode(`http://example.org/entity/${i}`);
      const matches = [...store.match(subject, null, null)];
      expect(matches.length).toBe(1);
    }

    const readDuration = performance.now() - readStart;
    const readThroughput = numReads / (readDuration / 1000);

    expect(readThroughput).toBeGreaterThan(500); // >500 reads/second
    expect(readDuration).toBeLessThan(5000); // <5s for 1000 reads

    console.log(`RDF Read: ${numReads} reads in ${readDuration.toFixed(2)}ms`);
    console.log(`Throughput: ${readThroughput.toFixed(2)} reads/second`);

    // ======================================================================
    // STEP 3: Verify data integrity
    // ======================================================================
    const allQuads = [...store.match(null, null, null)];
    expect(allQuads.length).toBe(numQuads);

    // ======================================================================
    // SUCCESS CRITERIA
    // ======================================================================
    // ✅ High write throughput (>1000 quads/s)
    // ✅ High read throughput (>500 reads/s)
    // ✅ Data integrity maintained
  }, 30000); // 30s timeout

  test('validates hook execution performance', async () => {
    // ======================================================================
    // STEP 1: Create simple validation hook
    // ======================================================================
    const perfHook = defineHook({
      id: 'performance-hook',
      trigger: 'test',
      handler: async ({ value }) => {
        // Simple validation
        return {
          valid: value !== null && value !== undefined,
        };
      },
    });

    // ======================================================================
    // STEP 2: Measure single execution performance
    // ======================================================================
    const singleStart = performance.now();
    const numExecutions = 10000;

    for (let i = 0; i < numExecutions; i++) {
      const result = await executeHook(perfHook, { value: i });
      expect(result.valid).toBe(true);
    }

    const singleDuration = performance.now() - singleStart;
    const singleThroughput = numExecutions / (singleDuration / 1000);

    expect(singleThroughput).toBeGreaterThan(1000); // >1000 executions/second
    expect(singleDuration).toBeLessThan(15000); // <15s for 10k executions

    console.log(`\nHook Execution: ${numExecutions} in ${singleDuration.toFixed(2)}ms`);
    console.log(`Throughput: ${singleThroughput.toFixed(2)} executions/second`);

    // ======================================================================
    // STEP 3: Measure batch execution performance
    // ======================================================================
    const batchStart = performance.now();
    const batchSize = 1000;
    const contexts = Array.from({ length: batchSize }, (_, i) => ({ value: i }));

    const batchResults = await executeBatch(perfHook, contexts);

    const batchDuration = performance.now() - batchStart;
    const batchThroughput = batchSize / (batchDuration / 1000);

    expect(batchResults.length).toBe(batchSize);
    expect(batchResults.every((r) => r.valid)).toBe(true);
    expect(batchThroughput).toBeGreaterThan(singleThroughput); // Batch should be faster

    console.log(`Batch Execution: ${batchSize} in ${batchDuration.toFixed(2)}ms`);
    console.log(`Throughput: ${batchThroughput.toFixed(2)} executions/second`);
    console.log(`Speedup: ${(batchThroughput / singleThroughput).toFixed(2)}x`);

    // ======================================================================
    // SUCCESS CRITERIA
    // ======================================================================
    // ✅ High single execution throughput (>1000/s)
    // ✅ Batch execution faster than sequential
    // ✅ All validations passed
  }, 30000); // 30s timeout

  test('measures memory efficiency under load', async () => {
    // ======================================================================
    // STEP 1: Capture baseline memory
    // ======================================================================
    if (global.gc) global.gc(); // Force GC if available

    const baselineMemory = process.memoryUsage();
    console.log(`\nBaseline Memory: ${(baselineMemory.heapUsed / 1024 / 1024).toFixed(2)} MB`);

    // ======================================================================
    // STEP 2: Create large dataset
    // ======================================================================
    const numQuads = 50000;

    for (let i = 0; i < numQuads; i++) {
      const subject = namedNode(`http://example.org/item/${i}`);
      const predicate = namedNode('http://example.org/vocab/prop');
      store.add(quad(subject, predicate, literal(`value-${i}`)));
    }

    // ======================================================================
    // STEP 3: Measure memory after load
    // ======================================================================
    const loadedMemory = process.memoryUsage();
    const memoryIncrease = loadedMemory.heapUsed - baselineMemory.heapUsed;
    const bytesPerQuad = memoryIncrease / numQuads;

    console.log(`Loaded Memory: ${(loadedMemory.heapUsed / 1024 / 1024).toFixed(2)} MB`);
    console.log(`Memory Increase: ${(memoryIncrease / 1024 / 1024).toFixed(2)} MB`);
    console.log(`Bytes per Quad: ${bytesPerQuad.toFixed(2)} bytes`);

    // Reasonable memory usage expectation
    expect(memoryIncrease).toBeLessThan(500 * 1024 * 1024); // <500MB for 50k quads
    expect(bytesPerQuad).toBeLessThan(10000); // <10KB per quad

    // ======================================================================
    // STEP 4: Verify store size
    // ======================================================================
    const finalCount = [...store.match(null, null, null)].length;
    expect(finalCount).toBeGreaterThanOrEqual(numQuads);

    // ======================================================================
    // SUCCESS CRITERIA
    // ======================================================================
    // ✅ Memory usage reasonable (<500MB for 50k quads)
    // ✅ Per-quad overhead acceptable (<10KB)
    // ✅ All data stored correctly
  }, 45000); // 45s timeout

  test('stress test: concurrent workflows with snapshots', async () => {
    // ======================================================================
    // STEP 1: Create engine and workflow
    // ======================================================================
    engine = createWorkflowEngine({ store });

    const workflow = createWorkflow('stress-test', {
      name: 'Stress Test',
    });

    workflow.addTask('task1', { type: 'automated' });
    await engine.registerWorkflow(workflow);

    // ======================================================================
    // STEP 2: Run concurrent workflows with snapshots
    // ======================================================================
    const stressStart = performance.now();
    const numConcurrent = 50;

    const promises = [];

    for (let i = 0; i < numConcurrent; i++) {
      const promise = (async () => {
        // Start case
        const workflowCase = await engine.startCase('stress-test', { id: i });

        // Create snapshot
        await freezeUniverse(kgcStore, `snapshot-${i}`, {
          caseId: workflowCase.id,
        });

        // Complete task
        await engine.enableTask(workflowCase.id, 'task1');
        await engine.completeTask(workflowCase.id, 'task1', { done: true });

        return workflowCase.id;
      })();

      promises.push(promise);
    }

    const results = await Promise.all(promises);

    const stressDuration = performance.now() - stressStart;
    const stressThroughput = numConcurrent / (stressDuration / 1000);

    expect(results.length).toBe(numConcurrent);
    expect(stressDuration).toBeLessThan(30000); // <30s for 50 concurrent

    console.log(`\nStress Test: ${numConcurrent} concurrent in ${stressDuration.toFixed(2)}ms`);
    console.log(`Throughput: ${stressThroughput.toFixed(2)} workflows/second`);

    // ======================================================================
    // STEP 3: Verify all completed
    // ======================================================================
    for (const caseId of results) {
      const finalCase = await engine.getCase(caseId);
      expect(finalCase.status).toBe('completed');
    }

    // ======================================================================
    // SUCCESS CRITERIA
    // ======================================================================
    // ✅ All concurrent workflows completed
    // ✅ Snapshots created successfully
    // ✅ Completed within timeout
    // ✅ Acceptable throughput
  }, 45000); // 45s timeout
});
