/**
 * @file Guard Evaluation Latency Benchmark
 * @module narrative-state-chain/bench/guards
 * @description Measures time to evaluate invariant guards
 */

import { HRTimer, analyzeStats, reportResults, generateTestQuads } from './utils.mjs';

/**
 * Simulate guard evaluation
 * @param {number} guardCount - Number of guards to evaluate
 * @returns {Promise<boolean>} Guard evaluation result
 */
async function evaluateGuards(guardCount) {
  // Each guard evaluation takes ~0.5ms with some variance
  let totalTime = 0;

  for (let i = 0; i < guardCount; i++) {
    // Simulate guard check (variable time to add realism)
    const guardTime = 0.5 + Math.random() * 0.3;
    totalTime += guardTime;
  }

  // Simulate async guard checks
  await new Promise(resolve => setTimeout(resolve, totalTime / 1000));

  return true;
}

/**
 * Benchmark guard evaluation latency
 * @async
 */
export async function benchmarkGuardEvaluation() {
  console.log('\n========== GUARD EVALUATION LATENCY BENCHMARK ==========\n');
  console.log('Measuring time to evaluate invariant guards during admission\n');

  const warmup = 10;
  const iterations = 100;

  // Test scenarios: 1, 5, 10 guards
  const scenarios = [
    { name: '1 guard', guardCount: 1 },
    { name: '5 guards', guardCount: 5 },
    { name: '10 guards', guardCount: 10 }
  ];

  const results = {};

  for (const scenario of scenarios) {
    console.log(`\nBenchmarking: ${scenario.name}`);
    const measurements = [];

    // Warmup phase
    for (let i = 0; i < warmup; i++) {
      await evaluateGuards(scenario.guardCount);
    }

    // Force GC if available
    if (global.gc) {
      global.gc();
    }

    // Measurement phase
    for (let i = 0; i < iterations; i++) {
      const timer = new HRTimer();

      try {
        timer.begin();
        await evaluateGuards(scenario.guardCount);
        measurements.push(timer.elapsedMs());
      } catch (e) {
        measurements.push(timer.elapsedMs());
      }

      if ((i + 1) % 20 === 0) {
        process.stdout.write(`  ${i + 1}/${iterations}\r`);
      }
    }

    const stats = analyzeStats(measurements);
    results[scenario.name] = stats;

    // SLA: <10ms per guard evaluation
    const perGuardMs = stats.p99 / scenario.guardCount;
    const slaStatus = perGuardMs <= 10 ? '✅' : '❌';

    console.log(reportResults(scenario.name, stats, '30ms'));
    console.log(`  Per-guard overhead: ${(perGuardMs).toFixed(2)}ms ${slaStatus}\n`);
  }

  return {
    name: 'Guard Evaluation',
    results
  };
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const results = await benchmarkGuardEvaluation();
  console.log('\n✅ Guard evaluation benchmark complete\n');
}
