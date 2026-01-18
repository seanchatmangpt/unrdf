#!/usr/bin/env node
/**
 * @file Load test orchestrator - Runs all load tests
 * @module test/load-testing/run-all-load-tests
 * @description Orchestrates all load tests and generates comprehensive report
 */

import { startServer, stopServer } from './test-server.mjs';
import { runBaselineLoad } from './01-baseline-load.mjs';
import { runPeakLoad } from './02-peak-load.mjs';
import { runSustainedLoad } from './03-sustained-load.mjs';
import { runSpikeTest } from './04-spike-test.mjs';
import { runSoakTest } from './05-soak-test.mjs';
import { writeFile } from 'node:fs/promises';
import { join } from 'node:path';

/**
 * Run all load tests
 * @param {Object} options - Test options
 * @returns {Promise<Object>} All test results
 */
async function runAllLoadTests(options = {}) {
  const {
    port = 3000,
    sustainedDuration = 60, // 1 minute for quick test (30 min for full)
    soakDuration = 60,      // 1 minute for quick test (2 hours for full)
    skipLongTests = false   // Skip sustained and soak tests
  } = options;

  console.log('=== UNRDF Load Testing Suite ===\n');
  console.log(`Starting test server on port ${port}...`);

  // Start test server
  const server = await startServer(port);
  const url = `http://localhost:${port}`;

  // Wait for server to be ready
  await new Promise(resolve => setTimeout(resolve, 1000));

  const results = {
    startTime: new Date().toISOString(),
    config: options,
    tests: {}
  };

  try {
    // Test 1: Baseline Load
    console.log('\n' + '='.repeat(60));
    console.log('TEST 1/5: Baseline Load');
    console.log('='.repeat(60));
    results.tests.baseline = await runBaselineLoad(url);
    console.log(`✓ Baseline test completed`);

    // Test 2: Peak Load
    console.log('\n' + '='.repeat(60));
    console.log('TEST 2/5: Peak Load');
    console.log('='.repeat(60));
    results.tests.peak = await runPeakLoad(url);
    console.log(`✓ Peak load test completed`);

    // Test 3: Sustained Load (skip if requested)
    if (!skipLongTests) {
      console.log('\n' + '='.repeat(60));
      console.log('TEST 3/5: Sustained Load');
      console.log('='.repeat(60));
      results.tests.sustained = await runSustainedLoad(url, sustainedDuration);
      console.log(`✓ Sustained load test completed`);
    } else {
      console.log('\n[SKIPPED] Sustained load test');
      results.tests.sustained = { skipped: true };
    }

    // Test 4: Spike Test
    console.log('\n' + '='.repeat(60));
    console.log('TEST 4/5: Spike Test');
    console.log('='.repeat(60));
    results.tests.spike = await runSpikeTest(url);
    console.log(`✓ Spike test completed`);

    // Test 5: Soak Test (skip if requested)
    if (!skipLongTests) {
      console.log('\n' + '='.repeat(60));
      console.log('TEST 5/5: Soak Test');
      console.log('='.repeat(60));
      results.tests.soak = await runSoakTest(url, soakDuration);
      console.log(`✓ Soak test completed`);
    } else {
      console.log('\n[SKIPPED] Soak test');
      results.tests.soak = { skipped: true };
    }

    results.endTime = new Date().toISOString();
    results.totalDuration = new Date(results.endTime) - new Date(results.startTime);

  } catch (err) {
    console.error('\n❌ Test suite failed:', err);
    results.error = err.message;
    results.failed = true;
  } finally {
    // Stop test server
    console.log('\nStopping test server...');
    await stopServer(server);
  }

  return results;
}

/**
 * Main execution
 */
async function main() {
  const args = process.argv.slice(2);

  const options = {
    port: 3000,
    sustainedDuration: args.includes('--full') ? 1800 : 60,  // 30 min or 1 min
    soakDuration: args.includes('--full') ? 7200 : 60,       // 2 hours or 1 min
    skipLongTests: args.includes('--quick')                   // Skip long tests
  };

  console.log('Load Testing Options:');
  console.log(`  Mode: ${args.includes('--full') ? 'FULL' : args.includes('--quick') ? 'QUICK' : 'STANDARD'}`);
  console.log(`  Sustained duration: ${options.sustainedDuration}s`);
  console.log(`  Soak duration: ${options.soakDuration}s`);
  console.log(`  Skip long tests: ${options.skipLongTests}\n`);

  const results = await runAllLoadTests(options);

  // Save results to file
  const resultsDir = join(process.cwd(), 'test/load-testing/results');
  const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
  const resultsFile = join(resultsDir, `load-test-results-${timestamp}.json`);

  try {
    await writeFile(resultsFile, JSON.stringify(results, null, 2));
    console.log(`\n✓ Results saved to: ${resultsFile}`);
  } catch (err) {
    console.warn(`⚠ Could not save results file: ${err.message}`);
  }

  // Print summary
  console.log('\n' + '='.repeat(60));
  console.log('LOAD TEST SUMMARY');
  console.log('='.repeat(60));

  const allPassed = [];
  const allFailed = [];

  for (const [testName, result] of Object.entries(results.tests)) {
    if (result.skipped) {
      console.log(`${testName}: SKIPPED`);
      continue;
    }

    const passed = result.passed ? Object.values(result.passed).every(p => p) : false;
    const status = passed ? '✓ PASS' : '✗ FAIL';

    console.log(`\n${testName.toUpperCase()}: ${status}`);

    if (result.latency && result.latency.p95 !== undefined) {
      console.log(`  Latency P95: ${result.latency.p95.toFixed(2)}ms`);
      console.log(`  Latency P99: ${result.latency.p99.toFixed(2)}ms`);
    }
    if (result.throughput && result.throughput.requestsPerSecond !== undefined) {
      console.log(`  Throughput: ${result.throughput.requestsPerSecond.toFixed(2)} req/s`);
    }
    if (result.errors && result.errors.errorRate !== undefined) {
      console.log(`  Error rate: ${result.errors.errorRate.toFixed(4)}%`);
    }

    if (passed) {
      allPassed.push(testName);
    } else {
      allFailed.push(testName);
    }
  }

  console.log('\n' + '='.repeat(60));
  console.log(`Passed: ${allPassed.length} | Failed: ${allFailed.length}`);

  if (allFailed.length > 0) {
    console.log(`Failed tests: ${allFailed.join(', ')}`);
  }

  console.log('='.repeat(60));

  // Exit with appropriate code
  process.exit(allFailed.length > 0 ? 1 : 0);
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch((err) => {
    console.error('Fatal error:', err);
    process.exit(1);
  });
}

export { runAllLoadTests };
