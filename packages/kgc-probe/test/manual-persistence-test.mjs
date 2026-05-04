#!/usr/bin/env node
/**
 * @file Manual Persistence Probe Test
 * @description Quick manual test to verify persistence probe works
 */

import { probePersistence } from '../src/probes/persistence.mjs';
import { promises as fs } from 'node:fs';
import { join } from 'node:path';
import { tmpdir } from 'node:os';

async function runTest() {
  console.log('Starting persistence probe test...\n');

  // Create test directory
  const testDir = join(tmpdir(), `kgc-probe-manual-test-${Date.now()}`);
  await fs.mkdir(testDir, { recursive: true });
  console.log(`Test directory: ${testDir}\n`);

  try {
    // Run probe
    const config = {
      out: testDir,
      timeout: 5000,
      maxQuota: 10 * 1024 * 1024, // 10 MB
    };

    console.log('Running persistence probe...');
    const startTime = Date.now();
    const observations = await probePersistence(config);
    const duration = Date.now() - startTime;

    console.log(`\nCompleted in ${duration}ms`);
    console.log(`Total observations: ${observations.length}\n`);

    // Group observations by category
    const byCategory = {};
    observations.forEach(obs => {
      if (!byCategory[obs.category]) {
        byCategory[obs.category] = [];
      }
      byCategory[obs.category].push(obs);
    });

    console.log('Observations by category:');
    Object.entries(byCategory).forEach(([category, obs]) => {
      console.log(`  ${category}: ${obs.length}`);
    });

    console.log('\nSample observations:');
    observations.slice(0, 5).forEach((obs, i) => {
      console.log(`\n[${i + 1}] ${obs.observation}`);
      console.log(`    Category: ${obs.category}`);
      console.log(`    Value: ${JSON.stringify(obs.value)}`);
      if (obs.guardDecision) {
        console.log(`    Guard: ${obs.guardDecision.allowed ? 'ALLOWED' : 'DENIED'} - ${obs.guardDecision.reason}`);
      }
    });

    // Verify key observations
    const hasWrite = observations.some(obs => obs.observation.includes('Write operation'));
    const hasRead = observations.some(obs => obs.observation.includes('Read operation'));
    const hasQuota = observations.some(obs => obs.category === 'quota');
    const hasStorage = observations.some(obs => obs.observation.includes('Storage type'));

    console.log('\n✅ Verification:');
    console.log(`  Write test: ${hasWrite ? 'PASS' : 'FAIL'}`);
    console.log(`  Read test: ${hasRead ? 'PASS' : 'FAIL'}`);
    console.log(`  Quota test: ${hasQuota ? 'PASS' : 'FAIL'}`);
    console.log(`  Storage detection: ${hasStorage ? 'PASS' : 'FAIL'}`);

    const allPassed = hasWrite && hasRead && hasQuota && hasStorage;
    console.log(`\n${allPassed ? '✅ ALL TESTS PASSED' : '❌ SOME TESTS FAILED'}`);

    // Cleanup
    await fs.rm(testDir, { recursive: true, force: true });
    console.log(`\nCleaned up test directory: ${testDir}`);

    process.exit(allPassed ? 0 : 1);
  } catch (error) {
    console.error('\n❌ Test failed:', error.message);
    console.error(error.stack);

    // Cleanup on error
    try {
      await fs.rm(testDir, { recursive: true, force: true });
    } catch (e) {
      // Ignore cleanup errors
    }

    process.exit(1);
  }
}

runTest();
