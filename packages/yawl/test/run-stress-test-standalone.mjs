#!/usr/bin/env node
/**
 * Standalone stress test runner
 * Runs stress tests without requiring vitest installation
 */

import {
  createResourceManager,
  createParticipant,
  createTool,
  createPolicyPack,
} from '../src/resources/index.mjs';

// Simple test framework
const results = {
  passed: 0,
  failed: 0,
  tests: [],
};

function expect(actual) {
  return {
    toBe(expected) {
      if (actual !== expected) {
        throw new Error(`Expected ${expected}, got ${actual}`);
      }
    },
    toBeGreaterThan(expected) {
      if (actual <= expected) {
        throw new Error(`Expected > ${expected}, got ${actual}`);
      }
    },
    toBeGreaterThanOrEqual(expected) {
      if (actual < expected) {
        throw new Error(`Expected >= ${expected}, got ${actual}`);
      }
    },
    toBeLessThanOrEqual(expected) {
      if (actual > expected) {
        throw new Error(`Expected <= ${expected}, got ${actual}`);
      }
    },
    toContain(expected) {
      if (!actual.includes(expected)) {
        throw new Error(`Expected to contain ${expected}, got ${actual}`);
      }
    },
    toBeDefined() {
      if (actual === undefined) {
        throw new Error('Expected to be defined');
      }
    },
    toBeNull() {
      if (actual !== null) {
        throw new Error(`Expected null, got ${actual}`);
      }
    },
    not: {
      toBe(expected) {
        if (actual === expected) {
          throw new Error(`Expected not ${expected}, but got ${actual}`);
        }
      },
      toBeNull() {
        if (actual === null) {
          throw new Error('Expected not null, but got null');
        }
      },
    },
  };
}

async function test(name, fn) {
  try {
    await fn();
    results.passed++;
    results.tests.push({ name, status: 'PASS' });
    console.log(`✅ ${name}`);
  } catch (error) {
    results.failed++;
    results.tests.push({ name, status: 'FAIL', error: error.message });
    console.log(`❌ ${name}`);
    console.log(`   Error: ${error.message}`);
  }
}

// Run stress tests
console.log('Running YAWL Resource Stress Tests...\n');

// Scenario 1: Over-Allocation
console.log('=== Scenario 1: Over-Allocation ===');

await test('should reject allocation when capacity=0', async () => {
  const manager = createResourceManager();
  const resource = createParticipant({ id: 'zero-capacity', capacity: 0 });
  manager.registerPolicyPack(createPolicyPack({
    id: 'zero-pack',
    resources: [resource],
  }));

  const workItem = { id: 'wi-1', taskId: 't', caseId: 'c' };

  let thrown = false;
  try {
    await manager.allocateResource(workItem, resource);
  } catch (error) {
    thrown = true;
    expect(error.message).toContain('Capacity exceeded');
  }

  if (!thrown) {
    throw new Error('Expected allocation to fail with capacity=0');
  }

  const status = manager.getCapacityStatus('zero-capacity');
  expect(status.max).toBe(0);
  expect(status.current).toBe(0);
});

await test('should handle 100 allocation attempts when capacity=2', async () => {
  const manager = createResourceManager();
  const resource = createParticipant({ id: 'limited', capacity: 2 });
  manager.registerPolicyPack(createPolicyPack({
    id: 'limited-pack',
    resources: [resource],
  }));

  let successCount = 0;
  let failureCount = 0;

  for (let i = 0; i < 100; i++) {
    try {
      await manager.allocateResource(
        { id: `wi-${i}`, taskId: 't', caseId: 'c' },
        resource
      );
      successCount++;
    } catch (error) {
      failureCount++;
    }
  }

  expect(successCount).toBe(2);
  expect(failureCount).toBe(98);
});

// Scenario 2: Concurrent Allocations
console.log('\n=== Scenario 2: Concurrent Allocations ===');

await test('should handle 100 concurrent allocations without double-allocation', async () => {
  const manager = createResourceManager();
  const resource = createParticipant({ id: 'concurrent', capacity: 10 });
  manager.registerPolicyPack(createPolicyPack({
    id: 'concurrent-pack',
    resources: [resource],
  }));

  const promises = [];
  for (let i = 0; i < 100; i++) {
    promises.push(
      manager.allocateResource(
        { id: `wi-${i}`, taskId: 't', caseId: 'c' },
        resource
      ).catch(error => ({ error: error.message }))
    );
  }

  const results = await Promise.all(promises);
  const successes = results.filter(r => !r.error);
  const failures = results.filter(r => r.error);

  expect(successes.length).toBe(10);
  expect(failures.length).toBe(90);
});

// Scenario 3: Capacity Boundaries
console.log('\n=== Scenario 3: Capacity Boundaries ===');

await test('should allocate exactly capacity resources', async () => {
  const manager = createResourceManager();
  const resource = createParticipant({ id: 'exact', capacity: 7 });
  manager.registerPolicyPack(createPolicyPack({
    id: 'exact-pack',
    resources: [resource],
  }));

  const receipts = [];
  for (let i = 0; i < 7; i++) {
    const receipt = await manager.allocateResource(
      { id: `wi-${i}`, taskId: 't', caseId: 'c' },
      resource
    );
    receipts.push(receipt);
  }

  expect(receipts.length).toBe(7);

  let thrown = false;
  try {
    await manager.allocateResource(
      { id: 'wi-8', taskId: 't', caseId: 'c' },
      resource
    );
  } catch (error) {
    thrown = true;
  }

  if (!thrown) {
    throw new Error('Expected 8th allocation to fail');
  }

  const status = manager.getCapacityStatus('exact');
  expect(status.current).toBe(7);
  expect(status.max).toBe(7);
  expect(status.available).toBe(0);
  expect(status.utilizationPercent).toBe(100);
});

await test('should handle capacity=-1 (unlimited) with 200+ allocations', async () => {
  const manager = createResourceManager();
  const resource = createTool({ id: 'unlimited', capacity: -1 });
  manager.registerPolicyPack(createPolicyPack({
    id: 'unlimited-pack',
    resources: [resource],
  }));

  const receipts = [];
  for (let i = 0; i < 200; i++) {
    const receipt = await manager.allocateResource(
      { id: `wi-${i}`, taskId: 't', caseId: 'c' },
      resource
    );
    receipts.push(receipt);
  }

  expect(receipts.length).toBe(200);

  const status = manager.getCapacityStatus('unlimited');
  expect(status.max).toBe(-1);
});

// Scenario 4: Pool Exhaustion
console.log('\n=== Scenario 4: Pool Exhaustion ===');

await test('should return null when pool exhausted', async () => {
  const manager = createResourceManager();
  const pool = manager.createResourcePool({
    id: 'exhausted-pool',
    resources: [
      createParticipant({ id: 'ep1', capacity: 1 }),
      createParticipant({ id: 'ep2', capacity: 1 }),
    ],
  });

  const receipt1 = await pool.allocateAny({ id: 'wi-1', taskId: 't', caseId: 'c' });
  const receipt2 = await pool.allocateAny({ id: 'wi-2', taskId: 't', caseId: 'c' });

  expect(receipt1).not.toBeNull();
  expect(receipt2).not.toBeNull();

  const availability = pool.getAvailability();
  expect(availability.available).toBe(false);
  expect(availability.availableCount).toBe(0);

  const receipt3 = await pool.allocateAny({ id: 'wi-3', taskId: 't', caseId: 'c' });
  expect(receipt3).toBeNull();
});

// Scenario 5: Rapid Allocation/Deallocation
console.log('\n=== Scenario 5: Rapid Allocation/Deallocation ===');

await test('should handle 200 rapid allocation/deallocation cycles', async () => {
  const manager = createResourceManager();
  const resource = createParticipant({ id: 'rapid', capacity: 1 });
  manager.registerPolicyPack(createPolicyPack({
    id: 'rapid-pack',
    resources: [resource],
  }));

  for (let i = 0; i < 200; i++) {
    const receipt = await manager.allocateResource(
      { id: `wi-${i}`, taskId: 't', caseId: 'c' },
      resource
    );
    expect(receipt).toBeDefined();

    const success = manager.deallocateResource(receipt.id);
    expect(success).toBe(true);
  }

  const status = manager.getCapacityStatus('rapid');
  expect(status.current).toBe(0);
  expect(status.available).toBe(1);
});

// Print summary
console.log('\n' + '='.repeat(50));
console.log('STRESS TEST SUMMARY');
console.log('='.repeat(50));
console.log(`✅ Passed: ${results.passed}`);
console.log(`❌ Failed: ${results.failed}`);
console.log(`📊 Total:  ${results.passed + results.failed}`);
console.log(`🎯 Success Rate: ${((results.passed / (results.passed + results.failed)) * 100).toFixed(1)}%`);

if (results.failed > 0) {
  console.log('\nFailed tests:');
  results.tests.filter(t => t.status === 'FAIL').forEach(t => {
    console.log(`  - ${t.name}`);
    console.log(`    ${t.error}`);
  });
  process.exit(1);
} else {
  console.log('\n🎉 All stress tests passed!');
  process.exit(0);
}
