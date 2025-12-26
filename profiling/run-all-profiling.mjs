#!/usr/bin/env node
/**
 * MASTER PROFILING RUNNER
 *
 * Executes all adversarial load tests and generates comprehensive report
 * with PROOF for all claims.
 *
 * @module profiling/run-all-profiling
 */

import { execSync } from 'node:child_process';
import { writeFileSync } from 'node:fs';
import { performance } from 'node:perf_hooks';

console.log('╔════════════════════════════════════════════════════════════════╗');
console.log('║  ADVERSARIAL PROFILING SUITE                                   ║');
console.log('║  Comprehensive Memory & Performance Testing                    ║');
console.log('╚════════════════════════════════════════════════════════════════╝\n');

const results = [];
const startTime = performance.now();

// ============================================================================
// TEST 1: YAWL Load Test
// ============================================================================

console.log('📋 Test 1/2: @unrdf/yawl Load Test');
console.log('─────────────────────────────────────────────────────────────────');

try {
  const yawlOutput = execSync(
    'node --expose-gc profiling/yawl-load-test.mjs',
    {
      cwd: '/home/user/unrdf',
      encoding: 'utf-8',
      timeout: 120000, // 2 minutes
    }
  );

  console.log(yawlOutput);

  results.push({
    test: 'YAWL Load Test',
    status: 'PASS',
    output: yawlOutput,
  });

} catch (error) {
  console.error('❌ YAWL Load Test FAILED');
  console.error(error.message);

  results.push({
    test: 'YAWL Load Test',
    status: 'FAIL',
    error: error.message,
    output: error.stdout || error.stderr,
  });
}

// ============================================================================
// TEST 2: Mega Framework Load Test
// ============================================================================

console.log('\n📋 Test 2/2: Mega Framework Load Test');
console.log('─────────────────────────────────────────────────────────────────');

try {
  const megaOutput = execSync(
    'node --expose-gc profiling/mega-framework-load-test.mjs',
    {
      cwd: '/home/user/unrdf',
      encoding: 'utf-8',
      timeout: 120000,
    }
  );

  console.log(megaOutput);

  results.push({
    test: 'Mega Framework Load Test',
    status: 'PASS',
    output: megaOutput,
  });

} catch (error) {
  console.error('❌ Mega Framework Load Test FAILED');
  console.error(error.message);

  results.push({
    test: 'Mega Framework Load Test',
    status: 'FAIL',
    error: error.message,
    output: error.stdout || error.stderr,
  });
}

// ============================================================================
// GENERATE FINAL REPORT
// ============================================================================

const endTime = performance.now();
const totalTime = endTime - startTime;

console.log('\n╔════════════════════════════════════════════════════════════════╗');
console.log('║  ADVERSARIAL PROFILING SUMMARY                                 ║');
console.log('╚════════════════════════════════════════════════════════════════╝\n');

const passCount = results.filter(r => r.status === 'PASS').length;
const failCount = results.filter(r => r.status === 'FAIL').length;

console.log('📊 Test Results:');
console.log(`   Total Tests: ${results.length}`);
console.log(`   Passed: ${passCount} ✅`);
console.log(`   Failed: ${failCount} ${failCount > 0 ? '❌' : ''}`);
console.log(`   Total Time: ${(totalTime / 1000).toFixed(2)}s\n`);

results.forEach((result, i) => {
  console.log(`${i + 1}. ${result.test}: ${result.status}`);
});

// Save detailed report
const report = {
  timestamp: new Date().toISOString(),
  totalTime: totalTime,
  results: results,
  summary: {
    total: results.length,
    passed: passCount,
    failed: failCount,
  },
};

writeFileSync(
  '/home/user/unrdf/profiling/profiling-report.json',
  JSON.stringify(report, null, 2)
);

console.log('\n📄 Detailed report saved to: profiling/profiling-report.json');

console.log('\n' + (failCount === 0 ? '✅' : '❌') + ` All profiling tests ${failCount === 0 ? 'completed successfully' : 'had failures'}`);

process.exit(failCount > 0 ? 1 : 0);
