#!/usr/bin/env node
/**
 * Error Injection & Recovery Test
 * Validates system handles errors gracefully
 */

import { UniverseManager, createMorphism } from '../packages/kgc-multiverse/src/index.mjs';

let passedTests = 0;
let failedTests = 0;

function test(name, fn) {
  try {
    fn();
    console.log(`✅ ${name}`);
    passedTests++;
  } catch (err) {
    console.error(`❌ ${name}: ${err.message}`);
    failedTests++;
  }
}

console.log('=== Error Injection & Recovery Tests ===\n');

// Test 1: System doesn't crash on invalid inputs
test('System handles invalid inputs gracefully', () => {
  const manager = new UniverseManager();

  // Try various invalid inputs - system should not crash
  try { manager.createUniverse(null); } catch { /* expected */ }
  try { manager.createUniverse(undefined); } catch { /* expected */ }
  try { manager.createUniverse({}); } catch { /* expected */ }
  try { manager.createUniverse({ invalid: 'data' }); } catch { /* expected */ }

  // System should still be usable after errors
  const validId = manager.createUniverse({ name: 'Valid', description: 'Test' });
  if (!validId) throw new Error('Failed to create universe after errors');
});

// Test 2: Morphism error handling
test('Invalid morphism handled', () => {
  try {
    createMorphism({});
  } catch {
    // Expected - invalid morphism should fail gracefully
  }

  // System should still work
  const manager = new UniverseManager();
  const universeId = manager.createUniverse({ name: 'Post-Error', description: 'Test' });
  if (!universeId) throw new Error('System not recovered');
});

// Test 3: Multiple sequential recoveries
test('Multiple sequential recoveries', () => {
  const manager = new UniverseManager();
  const created = [];

  // Alternate between errors and valid calls
  for (let i = 0; i < 5; i++) {
    try { manager.createUniverse(null); } catch { /* expected */ }

    const universeId = manager.createUniverse({
      name: `Recovery-${i}`,
      description: `Recovery test ${i}`,
    });

    if (universeId) {
      created.push(universeId);
    }
  }

  // Should have created 5 universes successfully
  if (created.length !== 5) {
    throw new Error(`Expected 5 universes, got ${created.length}`);
  }
});

// Test 4: Concurrent errors don't crash
test('Concurrent errors handled', async () => {
  const manager = new UniverseManager();

  const promises = Array.from({ length: 10 }, (_, i) => {
    return Promise.resolve().then(() => {
      if (i % 2 === 0) {
        try {
          manager.createUniverse(null);
        } catch {
          // Expected
        }
      } else {
        manager.createUniverse({ name: `Test ${i}`, description: `Test universe ${i}` });
      }
    });
  });

  await Promise.all(promises);
});

// Test 5: Memory cleanup after errors
test('Memory cleanup after errors', () => {
  const manager = new UniverseManager();
  const before = process.memoryUsage().heapUsed;

  for (let i = 0; i < 100; i++) {
    try {
      manager.createUniverse(null);
    } catch {
      // Expected
    }
  }

  global.gc && global.gc();

  const after = process.memoryUsage().heapUsed;
  const growthMB = (after - before) / 1024 / 1024;

  if (growthMB > 10) {
    throw new Error(`Excessive memory growth: ${growthMB.toFixed(2)} MB`);
  }
});

console.log(`\n=== Summary ===`);
console.log(`Passed: ${passedTests}`);
console.log(`Failed: ${failedTests}`);

const result = {
  passed: passedTests,
  failed: failedTests,
  status: failedTests === 0 ? 'PASS' : 'FAIL',
};

console.log(JSON.stringify(result, null, 2));

if (failedTests > 0) {
  console.error(`❌ ${failedTests} error recovery tests failed`);
  process.exit(1);
}

console.log('✅ All error recovery tests passed');
process.exit(0);
