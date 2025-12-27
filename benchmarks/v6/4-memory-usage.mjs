#!/usr/bin/env node
/**
 * V6 Memory Usage Benchmark
 *
 * Measures: Memory delta for receipt chain
 * Target: <2% overhead (10MB state → <10.2MB with receipts)
 * Benchmark: Build 100-quad store, measure receipt storage
 * Report: Receipt size per quad
 *
 * @module benchmarks/v6/4-memory-usage
 */

import { createStore, dataFactory } from '../../packages/oxigraph/src/index.mjs';
import { createReceipt } from '../../packages/v6-core/src/receipts/index.mjs';

const { namedNode, literal, quad } = dataFactory;

// =============================================================================
// Benchmark Configuration
// =============================================================================

const CONFIG = {
  quadCount: 100,
  receiptChainLength: 100,
};

// =============================================================================
// Memory Utilities
// =============================================================================

/**
 * Get current memory usage
 * @returns {Object} Memory usage stats
 */
function getMemoryUsage() {
  const usage = process.memoryUsage();
  return {
    rss: usage.rss,
    heapTotal: usage.heapTotal,
    heapUsed: usage.heapUsed,
    external: usage.external,
  };
}

/**
 * Force garbage collection (requires --expose-gc flag)
 */
function forceGC() {
  if (global.gc) {
    global.gc();
  }
}

/**
 * Calculate memory delta
 * @param {Object} before - Before memory usage
 * @param {Object} after - After memory usage
 * @returns {Object} Memory delta
 */
function calculateMemoryDelta(before, after) {
  return {
    rss: after.rss - before.rss,
    heapTotal: after.heapTotal - before.heapTotal,
    heapUsed: after.heapUsed - before.heapUsed,
    external: after.external - before.external,
  };
}

// =============================================================================
// Data Generation
// =============================================================================

/**
 * Generate test quads
 * @param {number} count - Number of quads
 * @returns {Array<Object>} Array of quads
 */
function generateQuads(count) {
  const quads = [];
  for (let i = 0; i < count; i++) {
    const subject = namedNode(`http://example.org/entity${i}`);
    const predicate = namedNode('http://example.org/property');
    const object = literal(`Value ${i} - ${Math.random().toString(36).substring(7)}`);
    quads.push(quad(subject, predicate, object));
  }
  return quads;
}

// =============================================================================
// Benchmark Runner
// =============================================================================

/**
 * Measure baseline store memory
 * @returns {Promise<Object>} Memory measurement
 */
async function measureBaselineMemory() {
  console.log('\n[Memory Baseline] Measuring baseline store memory...');

  forceGC();
  await new Promise((resolve) => setTimeout(resolve, 100));
  const beforeMemory = getMemoryUsage();

  // Create store with quads
  const store = createStore();
  const quads = generateQuads(CONFIG.quadCount);
  quads.forEach((q) => store.add(q));

  forceGC();
  await new Promise((resolve) => setTimeout(resolve, 100));
  const afterMemory = getMemoryUsage();

  const delta = calculateMemoryDelta(beforeMemory, afterMemory);

  console.log(`  Quads: ${CONFIG.quadCount}`);
  console.log(`  Memory delta (heap): ${(delta.heapUsed / 1024).toFixed(2)} KB`);

  return {
    quadCount: CONFIG.quadCount,
    memoryDelta: delta,
    store,
  };
}

/**
 * Measure receipt chain memory
 * @returns {Promise<Object>} Memory measurement
 */
async function measureReceiptChainMemory() {
  console.log('\n[Memory Receipts] Measuring receipt chain memory...');

  forceGC();
  await new Promise((resolve) => setTimeout(resolve, 100));
  const beforeMemory = getMemoryUsage();

  // Create receipt chain
  const receipts = [];
  let previousReceipt = null;

  for (let i = 0; i < CONFIG.receiptChainLength; i++) {
    const receipt = await createReceipt(
      'execution',
      {
        eventType: 'TASK_COMPLETED',
        caseId: `case-${i}`,
        taskId: `task-${i}`,
        payload: { iteration: i },
      },
      previousReceipt
    );
    receipts.push(receipt);
    previousReceipt = receipt;
  }

  forceGC();
  await new Promise((resolve) => setTimeout(resolve, 100));
  const afterMemory = getMemoryUsage();

  const delta = calculateMemoryDelta(beforeMemory, afterMemory);

  console.log(`  Receipts: ${CONFIG.receiptChainLength}`);
  console.log(`  Memory delta (heap): ${(delta.heapUsed / 1024).toFixed(2)} KB`);

  return {
    receiptCount: CONFIG.receiptChainLength,
    memoryDelta: delta,
    receipts,
  };
}

/**
 * Measure combined store + receipts memory
 * @returns {Promise<Object>} Memory measurement
 */
async function measureCombinedMemory() {
  console.log('\n[Memory Combined] Measuring store + receipts memory...');

  forceGC();
  await new Promise((resolve) => setTimeout(resolve, 100));
  const beforeMemory = getMemoryUsage();

  // Create store with quads
  const store = createStore();
  const quads = generateQuads(CONFIG.quadCount);
  quads.forEach((q) => store.add(q));

  // Create receipt chain
  const receipts = [];
  let previousReceipt = null;

  for (let i = 0; i < CONFIG.receiptChainLength; i++) {
    const receipt = await createReceipt(
      'execution',
      {
        eventType: 'TASK_COMPLETED',
        caseId: `case-${i}`,
        taskId: `task-${i}`,
        payload: { iteration: i },
      },
      previousReceipt
    );
    receipts.push(receipt);
    previousReceipt = receipt;
  }

  forceGC();
  await new Promise((resolve) => setTimeout(resolve, 100));
  const afterMemory = getMemoryUsage();

  const delta = calculateMemoryDelta(beforeMemory, afterMemory);

  console.log(`  Quads: ${CONFIG.quadCount}`);
  console.log(`  Receipts: ${CONFIG.receiptChainLength}`);
  console.log(`  Memory delta (heap): ${(delta.heapUsed / 1024).toFixed(2)} KB`);

  return {
    quadCount: CONFIG.quadCount,
    receiptCount: CONFIG.receiptChainLength,
    memoryDelta: delta,
  };
}

// =============================================================================
// Main Benchmark
// =============================================================================

async function main() {
  console.log('='.repeat(80));
  console.log('V6 Memory Usage Benchmark');
  console.log('='.repeat(80));
  console.log(`Target: <2% overhead`);
  console.log(`Quads: ${CONFIG.quadCount}`);
  console.log(`Receipt chain: ${CONFIG.receiptChainLength}`);

  if (!global.gc) {
    console.warn(
      '\n⚠️  WARNING: Run with --expose-gc flag for accurate measurements'
    );
    console.warn('   Example: node --expose-gc benchmarks/v6/4-memory-usage.mjs\n');
  }

  // Run measurements
  const baseline = await measureBaselineMemory();
  const receiptChain = await measureReceiptChainMemory();
  const combined = await measureCombinedMemory();

  // Calculate overhead
  const baselineHeap = baseline.memoryDelta.heapUsed;
  const receiptHeap = receiptChain.memoryDelta.heapUsed;
  const combinedHeap = combined.memoryDelta.heapUsed;

  const receiptOverhead = combinedHeap - baselineHeap;
  const receiptOverheadPercent = (receiptOverhead / baselineHeap) * 100;

  const receiptPerQuad = receiptOverhead / CONFIG.quadCount;

  // Print results
  console.log('\n' + '='.repeat(80));
  console.log('MEMORY USAGE RESULTS');
  console.log('='.repeat(80));
  console.log(`Baseline (quads only):     ${(baselineHeap / 1024).toFixed(2)} KB`);
  console.log(`Receipt chain only:        ${(receiptHeap / 1024).toFixed(2)} KB`);
  console.log(`Combined (quads+receipts): ${(combinedHeap / 1024).toFixed(2)} KB`);

  console.log('\n' + '='.repeat(80));
  console.log('OVERHEAD ANALYSIS');
  console.log('='.repeat(80));
  console.log(`Receipt overhead:          ${(receiptOverhead / 1024).toFixed(2)} KB`);
  console.log(`Receipt overhead %:        ${receiptOverheadPercent.toFixed(2)}%`);
  console.log(`Receipt bytes per quad:    ${receiptPerQuad.toFixed(2)} bytes`);
  console.log(`Target:                    <2.00%`);
  console.log(`Status:                    ${receiptOverheadPercent < 2.0 ? '✅ PASS' : '❌ FAIL'}`);

  console.log('\n' + '='.repeat(80));
  console.log('MEMORY BREAKDOWN');
  console.log('='.repeat(80));
  console.log(`Heap per quad:             ${(baselineHeap / CONFIG.quadCount).toFixed(2)} bytes`);
  console.log(
    `Heap per receipt:          ${(receiptHeap / CONFIG.receiptChainLength).toFixed(2)} bytes`
  );

  // JSON output for aggregation
  console.log('\n__JSON_RESULTS__');
  const results = {
    benchmark: 'memory-usage',
    timestamp: new Date().toISOString(),
    config: CONFIG,
    results: {
      baseline: {
        quadCount: baseline.quadCount,
        heapUsedBytes: baseline.memoryDelta.heapUsed,
        heapUsedKB: baseline.memoryDelta.heapUsed / 1024,
      },
      receiptChain: {
        receiptCount: receiptChain.receiptCount,
        heapUsedBytes: receiptChain.memoryDelta.heapUsed,
        heapUsedKB: receiptChain.memoryDelta.heapUsed / 1024,
      },
      combined: {
        quadCount: combined.quadCount,
        receiptCount: combined.receiptCount,
        heapUsedBytes: combined.memoryDelta.heapUsed,
        heapUsedKB: combined.memoryDelta.heapUsed / 1024,
      },
      overhead: {
        absoluteBytes: receiptOverhead,
        absoluteKB: receiptOverhead / 1024,
        percentRelative: receiptOverheadPercent,
        bytesPerQuad: receiptPerQuad,
      },
    },
    target: {
      maxOverheadPercent: 2.0,
      pass: receiptOverheadPercent < 2.0,
    },
  };
  console.log(JSON.stringify(results, null, 2));

  // Exit with appropriate code
  process.exit(receiptOverheadPercent < 2.0 ? 0 : 1);
}

main().catch((error) => {
  console.error('Benchmark failed:', error);
  process.exit(1);
});
