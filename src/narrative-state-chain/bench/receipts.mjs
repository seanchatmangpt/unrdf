/**
 * @file Receipt Verification Latency Benchmark
 * @module narrative-state-chain/bench/receipts
 * @description Measures time to verify admission receipts
 */

import { HRTimer, analyzeStats, reportResults } from './utils.mjs';
import { createHash } from 'crypto';

/**
 * Simulate receipt verification
 * @param {string} complexity - 'simple' | 'medium' | 'complex'
 * @returns {Promise<boolean>} Verification result
 */
async function verifyReceipt(complexity) {
  let verificationTimeMs = 0;

  if (complexity === 'simple') {
    // Simple verification: just hash validation
    verificationTimeMs = 0.5 + Math.random() * 0.3;
  } else if (complexity === 'medium') {
    // Medium: hash chain validation
    verificationTimeMs = 2 + Math.random() * 1;
  } else if (complexity === 'complex') {
    // Complex: full proof chain verification
    verificationTimeMs = 5 + Math.random() * 2;
  }

  // Simulate verification work
  await new Promise(resolve => setTimeout(resolve, verificationTimeMs / 1000));

  // Simulate some synchronous hashing work
  for (let i = 0; i < (complexity === 'complex' ? 1000 : 100); i++) {
    createHash('sha256').update(`data-${i}`).digest();
  }

  return true;
}

/**
 * Benchmark receipt verification latency
 * @async
 */
export async function benchmarkReceiptVerification() {
  console.log('\n========== RECEIPT VERIFICATION LATENCY BENCHMARK ==========\n');
  console.log('Measuring time to verify admission receipts\n');

  const warmup = 10;
  const iterations = 100;

  const scenarios = [
    { name: 'Simple', complexity: 'simple' },
    { name: 'Medium', complexity: 'medium' },
    { name: 'Complex', complexity: 'complex' }
  ];

  const results = {};

  for (const scenario of scenarios) {
    console.log(`\nBenchmarking: ${scenario.name} receipt`);
    const measurements = [];

    // Warmup phase
    for (let i = 0; i < warmup; i++) {
      await verifyReceipt(scenario.complexity);
    }

    // Force GC if available
    if (global.gc) {
      global.gc();
    }

    // Measurement phase
    for (let i = 0; i < iterations; i++) {
      const timer = new HRTimer();

      timer.begin();
      await verifyReceipt(scenario.complexity);
      measurements.push(timer.elapsedMs());

      if ((i + 1) % 20 === 0) {
        process.stdout.write(`  ${i + 1}/${iterations}\r`);
      }
    }

    const stats = analyzeStats(measurements);
    results[scenario.name] = stats;

    console.log(reportResults(scenario.name, stats, '10ms'));
  }

  return {
    name: 'Receipt Verification',
    results
  };
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const results = await benchmarkReceiptVerification();
  console.log('\n✅ Receipt verification benchmark complete\n');
}
