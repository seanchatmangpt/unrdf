/**
 * @file L5 Maturity Test Suite Runner
 * @description Runs all L1-L5 tests and generates evidence report
 */

import { test, describe } from 'node:test';
import { execSync } from 'node:child_process';

describe('L5 Maturity Test Suite', () => {
  test('[META] Run all L1-L5 tests and report results', async () => {
    console.log('\n='.repeat(80));
    console.log('L5 MATURITY TEST SUITE - COMPREHENSIVE VALIDATION');
    console.log('='.repeat(80));
    console.log('\nTarget: v6 P0+P1 Modules');
    console.log('- @unrdf/v6-core (ΔGate, receipts, delta contracts)');
    console.log('- @unrdf/oxigraph (Store creation, SPARQL)');
    console.log('- @unrdf/kgc-4d (Freezing, hashing, determinism)');
    console.log('='.repeat(80));

    const tests = [
      { name: 'L1: Compiles & Runs', file: 'l1-compiles-runs.test.mjs' },
      { name: 'L2: Stable Contracts', file: 'l2-stable-contracts.test.mjs' },
      { name: 'L3: Determinism', file: 'l3-determinism.test.mjs' },
      { name: 'L4: Adversarial Safety', file: 'l4-adversarial-safety.test.mjs' },
      { name: 'L5: Full Composition', file: 'l5-composition.test.mjs' },
    ];

    const results = [];

    for (const testSuite of tests) {
      console.log(`\n${'='.repeat(80)}`);
      console.log(`Running: ${testSuite.name}`);
      console.log('='.repeat(80));

      try {
        const output = execSync(
          `node --test /home/user/unrdf/test/l5-maturity/${testSuite.file}`,
          {
            encoding: 'utf-8',
            timeout: 10000,
          }
        );

        console.log(output);
        results.push({ suite: testSuite.name, status: 'PASS', output });
      } catch (error) {
        console.error(`FAILED: ${testSuite.name}`);
        console.error(error.stdout || error.message);
        results.push({ suite: testSuite.name, status: 'FAIL', error: error.message });
      }
    }

    // Generate summary report
    console.log('\n' + '='.repeat(80));
    console.log('L5 MATURITY TEST SUMMARY');
    console.log('='.repeat(80));

    let passCount = 0;
    let failCount = 0;

    for (const result of results) {
      const status = result.status === 'PASS' ? '✅' : '❌';
      console.log(`${status} ${result.suite}: ${result.status}`);

      if (result.status === 'PASS') passCount++;
      else failCount++;
    }

    console.log('\n' + '='.repeat(80));
    console.log(`TOTAL: ${passCount}/${results.length} passed`);
    console.log('='.repeat(80));

    // Final assertion
    if (failCount > 0) {
      throw new Error(`${failCount} test suite(s) failed`);
    }
  });
});
