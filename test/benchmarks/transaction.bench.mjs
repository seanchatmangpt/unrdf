/**
 * @fileoverview Transaction Performance Benchmark
 *
 * @description
 * Measures transaction commit performance to validate p99 < 5ms target.
 *
 * Performance Targets:
 * - Simple commit: p99 < 5ms
 * - With hooks: p99 < 10ms
 * - With receipts: p99 < 15ms
 * - Throughput: > 1000 tx/sec
 */

import { bench, describe } from 'vitest';
import { Store } from 'n3';
import { TransactionManager } from '../../src/knowledge-engine/transaction.mjs';
import { useTurtle } from '../../src/composables/use-turtle.mjs';
import { performance } from 'node:perf_hooks';
import { DataFactory } from 'n3';

const { quad, namedNode, literal } = DataFactory;

/**
 * Create test store with initial data
 * @returns {Promise<Store>} Store with test data
 */
async function createTestStore() {
  const turtle = await useTurtle();
  const store = new Store();

  const testData = `
    @prefix ex: <http://example.org/> .
    @prefix schema: <http://schema.org/> .

    ex:person1 a schema:Person ;
      schema:name "Alice" ;
      schema:age 30 .
  `;

  const quads = await turtle.parse(testData);
  store.addQuads(quads);

  return store;
}

/**
 * Create transaction delta
 * @param {number} size - Number of quads to add/remove
 * @returns {Object} Delta with added/removed quads
 */
function createDelta(size = 1) {
  const added = [];
  const removed = [];

  for (let i = 0; i < size; i++) {
    added.push(quad(
      namedNode(`http://example.org/person${i}`),
      namedNode('http://schema.org/name'),
      literal(`Person ${i}`)
    ));
  }

  return { added, removed };
}

describe('Transaction Performance', () => {
  // Benchmark simple transaction commit
  bench('Simple transaction commit (no hooks)', async () => {
    const store = await createTestStore();
    const manager = new TransactionManager({
      enableReceipts: false,
      afterHashOnly: true // Fast hashing
    });

    const delta = createDelta(1);
    await manager.applyTransaction(store, delta);
  }, {
    iterations: 1000,
    time: 5000,
    warmupIterations: 100
  });

  // Benchmark transaction with receipts
  bench('Transaction with receipts', async () => {
    const store = await createTestStore();
    const manager = new TransactionManager({
      enableReceipts: true,
      afterHashOnly: true
    });

    const delta = createDelta(1);
    await manager.applyTransaction(store, delta);
  }, {
    iterations: 500,
    time: 5000,
    warmupIterations: 50
  });

  // Benchmark transaction with hooks
  bench('Transaction with pre/post hooks', async () => {
    const store = await createTestStore();
    const manager = new TransactionManager({
      enableReceipts: false,
      afterHashOnly: true
    });

    // Add hooks
    manager.addHook({
      meta: { name: 'pre-hook' },
      when: 'pre',
      run: async () => ({ success: true })
    });

    manager.addHook({
      meta: { name: 'post-hook' },
      when: 'post',
      run: async () => ({ success: true })
    });

    const delta = createDelta(1);
    await manager.applyTransaction(store, delta);
  }, {
    iterations: 500,
    time: 5000,
    warmupIterations: 50
  });

  // Benchmark batch transaction
  bench('Batch transaction (10 quads)', async () => {
    const store = await createTestStore();
    const manager = new TransactionManager({
      enableReceipts: false,
      afterHashOnly: true
    });

    const delta = createDelta(10);
    await manager.applyTransaction(store, delta);
  }, {
    iterations: 500,
    time: 5000,
    warmupIterations: 50
  });

  // Benchmark batch transaction (100 quads)
  bench('Batch transaction (100 quads)', async () => {
    const store = await createTestStore();
    const manager = new TransactionManager({
      enableReceipts: false,
      afterHashOnly: true
    });

    const delta = createDelta(100);
    await manager.applyTransaction(store, delta);
  }, {
    iterations: 200,
    time: 5000,
    warmupIterations: 20
  });
});

describe('Transaction Performance Targets Validation', () => {
  bench('Validate p99 < 5ms for simple commits', async () => {
    const store = await createTestStore();
    const manager = new TransactionManager({
      enableReceipts: false,
      afterHashOnly: true
    });

    // Measure 100 transactions
    const durations = [];
    for (let i = 0; i < 100; i++) {
      const delta = createDelta(1);
      const start = performance.now();
      await manager.applyTransaction(store, delta);
      durations.push(performance.now() - start);
    }

    // Calculate percentiles
    const sorted = durations.sort((a, b) => a - b);
    const p50 = sorted[Math.floor(sorted.length * 0.50)];
    const p95 = sorted[Math.floor(sorted.length * 0.95)];
    const p99 = sorted[Math.floor(sorted.length * 0.99)];
    const mean = durations.reduce((sum, d) => sum + d, 0) / durations.length;

    console.log('\nTransaction Commit Performance:');
    console.log(`  Mean: ${mean.toFixed(3)}ms`);
    console.log(`  P50:  ${p50.toFixed(3)}ms`);
    console.log(`  P95:  ${p95.toFixed(3)}ms`);
    console.log(`  P99:  ${p99.toFixed(3)}ms`);
    console.log(`  Target: < 5ms`);
    console.log(`  Status: ${p99 < 5 ? '✅ PASS' : '❌ FAIL'}`);

    return mean;
  }, {
    iterations: 5,
    time: 10000
  });

  bench('Validate throughput > 1000 tx/sec', async () => {
    const store = await createTestStore();
    const manager = new TransactionManager({
      enableReceipts: false,
      afterHashOnly: true
    });

    // Measure transactions per second
    const startTime = performance.now();
    let txCount = 0;

    // Run for 5 seconds
    const runDuration = 5000;
    const endTime = startTime + runDuration;

    while (performance.now() < endTime) {
      const delta = createDelta(1);
      await manager.applyTransaction(store, delta);
      txCount++;
    }

    const actualDuration = performance.now() - startTime;
    const txPerSec = Math.round((txCount / actualDuration) * 1000);

    console.log('\nTransaction Throughput:');
    console.log(`  Transactions: ${txCount}`);
    console.log(`  Duration:     ${(actualDuration / 1000).toFixed(2)}s`);
    console.log(`  Rate:         ${txPerSec.toLocaleString()} tx/sec`);
    console.log(`  Target:       > 1,000 tx/sec`);
    console.log(`  Status:       ${txPerSec > 1000 ? '✅ PASS' : '❌ FAIL'}`);

    return txPerSec;
  }, {
    iterations: 3,
    time: 20000
  });
});

describe('Transaction Optimization Analysis', () => {
  bench('Compare afterHashOnly vs full canonicalization', async () => {
    const store = await createTestStore();
    const delta = createDelta(10);

    // Test 1: afterHashOnly (fast)
    const fastManager = new TransactionManager({
      enableReceipts: true,
      afterHashOnly: true
    });

    const fastDurations = [];
    for (let i = 0; i < 20; i++) {
      const start = performance.now();
      await fastManager.applyTransaction(store, delta);
      fastDurations.push(performance.now() - start);
    }

    // Test 2: Full canonicalization (slow but cryptographically secure)
    const slowManager = new TransactionManager({
      enableReceipts: true,
      afterHashOnly: false
    });

    const slowDurations = [];
    for (let i = 0; i < 20; i++) {
      const start = performance.now();
      await slowManager.applyTransaction(store, delta);
      slowDurations.push(performance.now() - start);
    }

    const fastMean = fastDurations.reduce((sum, d) => sum + d, 0) / fastDurations.length;
    const slowMean = slowDurations.reduce((sum, d) => sum + d, 0) / slowDurations.length;
    const speedup = slowMean / fastMean;

    console.log('\nCanonicalization Impact:');
    console.log(`  afterHashOnly:          ${fastMean.toFixed(2)}ms`);
    console.log(`  Full canonicalization:  ${slowMean.toFixed(2)}ms`);
    console.log(`  Speedup:                ${speedup.toFixed(2)}x`);
    console.log(`  Recommendation:         Use afterHashOnly for performance, full for security`);
  }, {
    iterations: 3,
    time: 30000
  });

  bench('Batch size impact on performance', async () => {
    const store = await createTestStore();
    const manager = new TransactionManager({
      enableReceipts: false,
      afterHashOnly: true
    });

    const batchSizes = [1, 10, 50, 100, 500];

    console.log('\nBatch Size Impact:');
    console.log('  Size  |  Mean (ms)  |  Per-Quad (µs)');
    console.log('  ------|-------------|---------------');

    for (const size of batchSizes) {
      const delta = createDelta(size);

      const durations = [];
      for (let i = 0; i < 20; i++) {
        const start = performance.now();
        await manager.applyTransaction(store, delta);
        durations.push(performance.now() - start);
      }

      const mean = durations.reduce((sum, d) => sum + d, 0) / durations.length;
      const perQuad = (mean * 1000) / size; // Convert to microseconds

      console.log(`  ${String(size).padEnd(5)} | ${mean.toFixed(2).padEnd(11)} | ${perQuad.toFixed(2)}`);
    }
  }, {
    iterations: 1,
    time: 30000
  });
});
