#!/usr/bin/env node
/**
 * V6 Composition Latency Benchmark
 *
 * Measures: Time to compose 2-3 L5 modules
 * Target: <10% slowdown vs single module
 * Benchmark: oxigraph → kgc → workflow (3-hop composition)
 * Report: Time per hop
 *
 * @module benchmarks/v6/5-composition-latency
 */

import { performance } from 'node:perf_hooks';
import { createStore, dataFactory } from '../../packages/oxigraph/src/index.mjs';
import { createReceipt, verifyReceipt } from '../../packages/v6-core/src/receipts/index.mjs';
import { createDelta, DeltaGate } from '../../packages/v6-core/src/delta/index.mjs';

const { namedNode, literal, quad } = dataFactory;

// =============================================================================
// Benchmark Configuration
// =============================================================================

const CONFIG = {
  iterations: 100,
  warmupIterations: 10,
};

// =============================================================================
// Module Simulators
// =============================================================================

/**
 * L5 Module: Oxigraph Store Operations
 */
class OxigraphModule {
  constructor() {
    this.store = createStore();
  }

  async addQuad(subject, predicate, object) {
    const q = quad(
      namedNode(subject),
      namedNode(predicate),
      literal(object)
    );
    this.store.add(q);
    return q;
  }

  async query(sparql) {
    return this.store.query(sparql);
  }
}

/**
 * L5 Module: KGC Receipt Layer
 */
class KGCReceiptModule {
  constructor() {
    this.receipts = [];
  }

  async recordOperation(operation, data) {
    const receipt = await createReceipt('execution', {
      eventType: 'TASK_COMPLETED',
      caseId: `case-${Date.now()}`,
      taskId: operation,
      payload: data,
    });

    this.receipts.push(receipt);
    return receipt;
  }

  async verifyLastReceipt() {
    if (this.receipts.length === 0) return { valid: false };
    return verifyReceipt(this.receipts[this.receipts.length - 1]);
  }
}

/**
 * L5 Module: Workflow Delta Layer
 */
class WorkflowDeltaModule {
  constructor() {
    this.gate = new DeltaGate();
    this.deltas = [];
  }

  async proposeDelta(operation, subject, predicate, object) {
    const delta = createDelta(operation, subject, predicate, object, {
      package: '@unrdf/v6-benchmark',
      actor: 'composition-benchmark',
    });

    this.deltas.push(delta);
    return delta;
  }
}

// =============================================================================
// Composition Patterns
// =============================================================================

/**
 * Single module operation (baseline)
 * @returns {Promise<number>} Execution time in ms
 */
async function singleModuleOperation() {
  const start = performance.now();

  const oxigraph = new OxigraphModule();
  await oxigraph.addQuad(
    'http://example.org/entity1',
    'http://example.org/name',
    'Test Entity'
  );

  return performance.now() - start;
}

/**
 * Two-hop composition (oxigraph + KGC)
 * @returns {Promise<number>} Execution time in ms
 */
async function twoHopComposition() {
  const start = performance.now();

  // Hop 1: Oxigraph
  const oxigraph = new OxigraphModule();
  const q = await oxigraph.addQuad(
    'http://example.org/entity1',
    'http://example.org/name',
    'Test Entity'
  );

  // Hop 2: KGC Receipt
  const kgc = new KGCReceiptModule();
  await kgc.recordOperation('addQuad', { quad: q });

  return performance.now() - start;
}

/**
 * Three-hop composition (oxigraph + KGC + workflow)
 * @returns {Promise<number>} Execution time in ms
 */
async function threeHopComposition() {
  const start = performance.now();

  // Hop 1: Oxigraph
  const oxigraph = new OxigraphModule();
  const q = await oxigraph.addQuad(
    'http://example.org/entity1',
    'http://example.org/name',
    'Test Entity'
  );

  // Hop 2: KGC Receipt
  const kgc = new KGCReceiptModule();
  const receipt = await kgc.recordOperation('addQuad', { quad: q });

  // Hop 3: Workflow Delta
  const workflow = new WorkflowDeltaModule();
  await workflow.proposeDelta(
    'add',
    'http://example.org/entity1',
    'http://example.org/receiptId',
    receipt.id
  );

  return performance.now() - start;
}

// =============================================================================
// Benchmark Runner
// =============================================================================

/**
 * Run composition benchmark
 * @param {Function} compositionFn - Composition function to benchmark
 * @param {string} name - Benchmark name
 * @param {number} iterations - Number of iterations
 * @returns {Promise<Object>} Timing statistics
 */
async function runCompositionBenchmark(compositionFn, name, iterations) {
  console.log(`\n[${name}] Running ${iterations} iterations...`);

  // Warmup
  for (let i = 0; i < CONFIG.warmupIterations; i++) {
    await compositionFn();
  }

  // Actual benchmark
  const timings = [];
  for (let i = 0; i < iterations; i++) {
    const elapsed = await compositionFn();
    timings.push(elapsed);
  }

  const sorted = [...timings].sort((a, b) => a - b);
  const sum = timings.reduce((a, b) => a + b, 0);
  const mean = sum / timings.length;

  return {
    name,
    mean,
    median: sorted[Math.floor(sorted.length / 2)],
    p95: sorted[Math.floor(sorted.length * 0.95)],
    p99: sorted[Math.floor(sorted.length * 0.99)],
    min: sorted[0],
    max: sorted[sorted.length - 1],
  };
}

// =============================================================================
// Main Benchmark
// =============================================================================

async function main() {
  console.log('='.repeat(80));
  console.log('V6 Composition Latency Benchmark');
  console.log('='.repeat(80));
  console.log(`Target: <10% slowdown vs single module`);
  console.log(`Iterations: ${CONFIG.iterations}`);

  // Run benchmarks
  const singleModule = await runCompositionBenchmark(
    singleModuleOperation,
    'Single Module',
    CONFIG.iterations
  );

  const twoHop = await runCompositionBenchmark(
    twoHopComposition,
    'Two-Hop Composition',
    CONFIG.iterations
  );

  const threeHop = await runCompositionBenchmark(
    threeHopComposition,
    'Three-Hop Composition',
    CONFIG.iterations
  );

  // Calculate overhead
  const twoHopOverhead = ((twoHop.median - singleModule.median) / singleModule.median) * 100;
  const threeHopOverhead = ((threeHop.median - singleModule.median) / singleModule.median) * 100;

  const twoHopLatencyPerHop = (twoHop.median - singleModule.median) / 1; // 1 additional hop
  const threeHopLatencyPerHop = (threeHop.median - singleModule.median) / 2; // 2 additional hops

  // Print results
  console.log('\n' + '='.repeat(80));
  console.log('COMPOSITION LATENCY RESULTS');
  console.log('='.repeat(80));

  console.log('\nSINGLE MODULE (Baseline):');
  console.log(`  Median: ${singleModule.median.toFixed(4)} ms`);
  console.log(`  Mean:   ${singleModule.mean.toFixed(4)} ms`);
  console.log(`  P95:    ${singleModule.p95.toFixed(4)} ms`);
  console.log(`  P99:    ${singleModule.p99.toFixed(4)} ms`);

  console.log('\nTWO-HOP COMPOSITION (Oxigraph + KGC):');
  console.log(`  Median: ${twoHop.median.toFixed(4)} ms`);
  console.log(`  Mean:   ${twoHop.mean.toFixed(4)} ms`);
  console.log(`  P95:    ${twoHop.p95.toFixed(4)} ms`);
  console.log(`  P99:    ${twoHop.p99.toFixed(4)} ms`);
  console.log(`  Overhead: ${twoHopOverhead.toFixed(2)}%`);
  console.log(`  Latency per hop: ${twoHopLatencyPerHop.toFixed(4)} ms`);

  console.log('\nTHREE-HOP COMPOSITION (Oxigraph + KGC + Workflow):');
  console.log(`  Median: ${threeHop.median.toFixed(4)} ms`);
  console.log(`  Mean:   ${threeHop.mean.toFixed(4)} ms`);
  console.log(`  P95:    ${threeHop.p95.toFixed(4)} ms`);
  console.log(`  P99:    ${threeHop.p99.toFixed(4)} ms`);
  console.log(`  Overhead: ${threeHopOverhead.toFixed(2)}%`);
  console.log(`  Latency per hop: ${threeHopLatencyPerHop.toFixed(4)} ms`);

  console.log('\n' + '='.repeat(80));
  console.log('OVERHEAD ANALYSIS');
  console.log('='.repeat(80));
  console.log(`Target:                  <10.00%`);
  console.log(`Two-hop overhead:        ${twoHopOverhead.toFixed(2)}%`);
  console.log(`Three-hop overhead:      ${threeHopOverhead.toFixed(2)}%`);
  console.log(`Two-hop status:          ${twoHopOverhead < 10.0 ? '✅ PASS' : '❌ FAIL'}`);
  console.log(`Three-hop status:        ${threeHopOverhead < 10.0 ? '✅ PASS' : '❌ FAIL'}`);

  const allPass = twoHopOverhead < 10.0 && threeHopOverhead < 10.0;

  // JSON output for aggregation
  console.log('\n__JSON_RESULTS__');
  const results = {
    benchmark: 'composition-latency',
    timestamp: new Date().toISOString(),
    config: CONFIG,
    results: {
      singleModule: {
        median: singleModule.median,
        mean: singleModule.mean,
        p95: singleModule.p95,
        p99: singleModule.p99,
      },
      twoHop: {
        median: twoHop.median,
        mean: twoHop.mean,
        p95: twoHop.p95,
        p99: twoHop.p99,
        overheadPercent: twoHopOverhead,
        latencyPerHopMs: twoHopLatencyPerHop,
      },
      threeHop: {
        median: threeHop.median,
        mean: threeHop.mean,
        p95: threeHop.p95,
        p99: threeHop.p99,
        overheadPercent: threeHopOverhead,
        latencyPerHopMs: threeHopLatencyPerHop,
      },
    },
    target: {
      maxOverheadPercent: 10.0,
      pass: allPass,
    },
  };
  console.log(JSON.stringify(results, null, 2));

  // Exit with appropriate code
  process.exit(allPass ? 0 : 1);
}

main().catch((error) => {
  console.error('Benchmark failed:', error);
  process.exit(1);
});
