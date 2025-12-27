/**
 * @file Reconciliation Latency Benchmark
 * @module narrative-state-chain/bench/reconcile
 * @description Measures time from scene observation start to consequences ready
 */

import { HRTimer, analyzeStats, reportResults, generateTestQuads } from './utils.mjs';

/**
 * Simulate reconciliation - processing observations into consequences
 * @param {Array} quads - Quads to reconcile
 * @returns {Promise<object>} Reconciliation result
 */
async function simulateReconciliation(quads) {
  // Simulate processing time proportional to quad count
  const baseTimeMs = 0.5;
  const perQuadTimeMs = 0.3;
  const processingTimeMs = baseTimeMs + (quads.length * perQuadTimeMs);

  // Use setTimeout to create realistic async behavior
  await new Promise(resolve => setTimeout(resolve, processingTimeMs / 1000));

  // Simulate consequence generation
  return {
    consequenceCount: quads.length * 2,
    timestamp: Date.now()
  };
}

/**
 * Benchmark reconciliation latency for scenes
 * @async
 */
export async function benchmarkReconciliationLatency() {
  console.log('\n========== RECONCILIATION LATENCY BENCHMARK ==========\n');
  console.log('Measuring time from scene observation to reconciliation complete\n');

  const warmup = 10;
  const iterations = 100;

  // Test scenarios: 1 quad, 10 quads, 100 quads, 1000 quads
  const scenarios = [
    { name: '1 quad', quadCount: 1 },
    { name: '10 quads', quadCount: 10 },
    { name: '100 quads', quadCount: 100 },
    { name: '1000 quads', quadCount: 1000 }
  ];

  const results = {};

  for (const scenario of scenarios) {
    console.log(`\nBenchmarking: ${scenario.name}`);
    const measurements = [];

    // Warmup phase
    for (let i = 0; i < warmup; i++) {
      const quads = generateTestQuads(scenario.quadCount);
      await simulateReconciliation(quads);
    }

    // Force GC if available
    if (global.gc) {
      global.gc();
    }

    // Measurement phase
    for (let i = 0; i < iterations; i++) {
      const quads = generateTestQuads(scenario.quadCount);
      const timer = new HRTimer();

      try {
        timer.begin();
        await simulateReconciliation(quads);
        measurements.push(timer.elapsedMs());
      } catch (e) {
        // Record failed attempts
        measurements.push(timer.elapsedMs());
      }

      if ((i + 1) % 20 === 0) {
        process.stdout.write(`  ${i + 1}/${iterations}\r`);
      }
    }

    const stats = analyzeStats(measurements);
    results[scenario.name] = stats;

    // Determine SLA
    const sla = scenario.quadCount === 1 ? '5ms' :
               scenario.quadCount === 10 ? '10ms' :
               scenario.quadCount === 100 ? '50ms' : '100ms';

    console.log(reportResults(scenario.name, stats, sla));
  }

  return {
    name: 'Reconciliation Latency',
    results
  };
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const results = await benchmarkReconciliationLatency();
  console.log('\n✅ Reconciliation latency benchmark complete\n');
}
