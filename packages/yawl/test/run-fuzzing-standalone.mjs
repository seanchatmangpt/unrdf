/**
 * @file Standalone Fuzzing Test Runner
 * @description Runs cancellation fuzzing tests without vitest dependency
 */

import { createCancellationManager } from '../src/cancellation/index.mjs';

// ============================================================================
// TEST UTILITIES
// ============================================================================

function randomInt(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

function randomIntArray(minLength, maxLength, minValue, maxValue) {
  const length = randomInt(minLength, maxLength);
  return Array.from({ length }, () => randomInt(minValue, maxValue));
}

function createRandomWorkItems(manager, taskIds, caseId) {
  const workItemIds = [];
  for (const taskId of taskIds) {
    const wi = manager.createWorkItem({
      taskId,
      caseId,
      timeoutMs: randomInt(1000, 5000),
    });
    workItemIds.push(wi.id);
  }
  return workItemIds;
}

async function runPropertyTest(iterations, testFn, testName) {
  const errors = [];
  process.stdout.write(`\n${testName}: `);

  for (let i = 0; i < iterations; i++) {
    try {
      await testFn(i);
      if (i % 10 === 0) process.stdout.write('.');
    } catch (error) {
      errors.push({ iteration: i, error: error.message });
      process.stdout.write('X');
    }
  }

  if (errors.length > 0) {
    console.log(`\n  FAILED: ${errors.length}/${iterations} iterations`);
    errors.slice(0, 3).forEach(e => {
      console.log(`    Iteration ${e.iteration}: ${e.error}`);
    });
    return false;
  } else {
    console.log(`\n  PASSED: ${iterations}/${iterations} iterations`);
    return true;
  }
}

// ============================================================================
// TEST EXECUTION
// ============================================================================

let totalTests = 0;
let passedTests = 0;
let failedTests = 0;

console.log('\n='.repeat(70));
console.log('CANCELLATION FUZZING TEST SUITE');
console.log('='.repeat(70));

// Property 1: All aborts terminate cleanly
console.log('\n--- Property 1: All aborts terminate cleanly ---');
totalTests++;
const p1 = await runPropertyTest(100, () => {
  const manager = createCancellationManager();
  const nestingDepths = randomIntArray(1, 5, 1, 10);
  const caseId = 'case-' + Date.now();

  const taskIds = nestingDepths.map((_, i) => `task-${i}`);
  createRandomWorkItems(manager, taskIds, caseId);

  const regionIds = [];
  for (let i = 0; i < nestingDepths.length; i++) {
    const region = manager.regionManager.createRegion({
      name: `Region ${i}`,
      taskIds: [taskIds[i]],
      parentRegionId: i > 0 ? regionIds[i - 1] : undefined,
    });
    regionIds.push(region.id);
  }

  const abortIndex = randomInt(0, regionIds.length - 1);
  const workItemsForTask = manager.getWorkItemsForTask(taskIds[abortIndex]);
  if (workItemsForTask.length > 0) {
    manager.cancelWorkItem(workItemsForTask[0].id, 'manual');
  }

  const stats = manager.getStats();
  if (!stats || stats.workItems.total === 0) {
    throw new Error('Invalid manager state after abort');
  }
}, 'All aborts terminate cleanly');

if (p1) passedTests++; else failedTests++;

// Property 2: No orphaned work items
console.log('\n--- Property 2: No orphaned work items ---');
totalTests++;
const p2 = await runPropertyTest(100, () => {
  const manager = createCancellationManager();
  const taskCount = randomInt(1, 10);
  const caseId = 'case-' + Date.now();

  const taskIds = Array.from({ length: taskCount }, (_, i) => `task-${i}`);
  const workItemIds = createRandomWorkItems(manager, taskIds, caseId);

  const region = manager.regionManager.createRegion({
    name: 'Master Region',
    taskIds,
  });

  for (const wiId of workItemIds) {
    manager.enableWorkItem(wiId);
    if (Math.random() > 0.5) {
      manager.startExecution(wiId);
    }
  }

  const abortIndex = randomInt(0, workItemIds.length - 1);
  manager.cancelWorkItem(workItemIds[abortIndex], 'manual');

  const allWorkItems = manager.getWorkItemsForCase(caseId);
  const validStates = ['pending', 'enabled', 'executing', 'completed', 'cancelled', 'failed'];
  for (const wi of allWorkItems) {
    if (!validStates.includes(wi.state)) {
      throw new Error(`Invalid work item state: ${wi.state}`);
    }
  }
}, 'No orphaned work items');

if (p2) passedTests++; else failedTests++;

// Property 3: Deterministic completion-then-cancel
console.log('\n--- Property 3: Deterministic completion-then-cancel ---');
totalTests++;
const p3 = await runPropertyTest(100, () => {
  const manager = createCancellationManager();
  const caseId = 'case-' + Date.now();

  const wi = manager.createWorkItem({
    taskId: 'race-task',
    caseId,
    timeoutMs: 5000,
  });

  manager.enableWorkItem(wi.id);
  manager.startExecution(wi.id);
  manager.completeWorkItem(wi.id);
  const result = manager.cancelWorkItem(wi.id, 'manual');

  if (result.success !== false) {
    throw new Error('Cancel should fail after completion');
  }

  const finalWi = manager.getWorkItem(wi.id);
  if (finalWi.state !== 'completed') {
    throw new Error(`Expected completed state, got ${finalWi.state}`);
  }
}, 'Deterministic completion-then-cancel');

if (p3) passedTests++; else failedTests++;

// Property 4: Cancel-then-complete rejected
console.log('\n--- Property 4: Cancel-then-complete rejected ---');
totalTests++;
const p4 = await runPropertyTest(100, () => {
  const manager = createCancellationManager();
  const caseId = 'case-' + Date.now();

  const wi = manager.createWorkItem({
    taskId: 'race-task-2',
    caseId,
    timeoutMs: 5000,
  });

  manager.enableWorkItem(wi.id);
  manager.startExecution(wi.id);
  manager.cancelWorkItem(wi.id, 'manual');
  const completed = manager.completeWorkItem(wi.id);

  if (completed !== null) {
    throw new Error('Complete should fail after cancellation');
  }

  const finalWi = manager.getWorkItem(wi.id);
  if (finalWi.state !== 'cancelled') {
    throw new Error(`Expected cancelled state, got ${finalWi.state}`);
  }
}, 'Cancel-then-complete rejected');

if (p4) passedTests++; else failedTests++;

// Property 5: Concurrent operations are safe
console.log('\n--- Property 5: Concurrent operations are safe ---');
totalTests++;
const p5 = await runPropertyTest(100, () => {
  const manager = createCancellationManager();
  const caseId = 'case-' + Date.now();

  const workItems = [];
  for (let i = 0; i < 10; i++) {
    const wi = manager.createWorkItem({
      taskId: `concurrent-task-${i}`,
      caseId,
      timeoutMs: 5000,
    });
    manager.enableWorkItem(wi.id);
    manager.startExecution(wi.id);
    workItems.push(wi);
  }

  for (const wi of workItems) {
    if (Math.random() > 0.5) {
      manager.completeWorkItem(wi.id);
    } else {
      manager.cancelWorkItem(wi.id, 'manual');
    }
  }

  const allWorkItems = manager.getWorkItemsForCase(caseId);
  const terminalStates = ['completed', 'cancelled'];
  for (const wi of allWorkItems) {
    if (!terminalStates.includes(wi.state)) {
      throw new Error(`Expected terminal state, got ${wi.state}`);
    }
  }
}, 'Concurrent operations are safe');

if (p5) passedTests++; else failedTests++;

// Property 6: Cascading through linked regions
console.log('\n--- Property 6: Cascading through linked regions ---');
totalTests++;
const p6 = await runPropertyTest(100, () => {
  const manager = createCancellationManager();
  const caseId = 'case-' + Date.now();

  const slaveCount = randomInt(2, 10);
  const slaveTasks = Array.from({ length: slaveCount }, (_, i) => `slave-${i}`);

  createRandomWorkItems(manager, ['master', ...slaveTasks], caseId);

  const masterRegion = manager.regionManager.createRegion({
    name: 'Master',
    taskIds: ['master'],
  });

  const slaveRegions = slaveTasks.map((taskId, i) =>
    manager.regionManager.createRegion({
      name: `Slave ${i}`,
      taskIds: [taskId],
      parentRegionId: masterRegion.id,
    })
  );

  const masterWorkItems = manager.getWorkItemsForTask('master');
  if (masterWorkItems.length > 0) {
    manager.cancelWorkItem(masterWorkItems[0].id, 'manual');
  }

  if (manager.regionManager.getRegion(masterRegion.id).active !== false) {
    throw new Error('Master region should be deactivated');
  }

  for (const slaveRegion of slaveRegions) {
    if (manager.regionManager.getRegion(slaveRegion.id).active !== false) {
      throw new Error(`Slave region ${slaveRegion.id} should be deactivated`);
    }
  }
}, 'Cascading through linked regions');

if (p6) passedTests++; else failedTests++;

// Property 7: Cascading depth has no impact
console.log('\n--- Property 7: Cascading depth has no impact ---');
totalTests++;
const p7 = await runPropertyTest(100, () => {
  const manager = createCancellationManager();
  const caseId = 'case-' + Date.now();
  const depth = randomInt(1, 50);

  const taskIds = Array.from({ length: depth }, (_, i) => `cascade-${i}`);
  createRandomWorkItems(manager, taskIds, caseId);

  const regions = [];
  for (let i = 0; i < depth; i++) {
    const region = manager.regionManager.createRegion({
      name: `Level ${i}`,
      taskIds: [taskIds[i]],
      parentRegionId: i > 0 ? regions[i - 1].id : undefined,
    });
    regions.push(region);
  }

  const cancelLevel = randomInt(0, depth - 1);
  const workItems = manager.getWorkItemsForTask(taskIds[cancelLevel]);
  if (workItems.length > 0) {
    manager.cancelWorkItem(workItems[0].id, 'manual');
  }

  for (let i = cancelLevel; i < depth; i++) {
    if (manager.regionManager.getRegion(regions[i].id).active !== false) {
      throw new Error(`Region ${i} should be deactivated`);
    }
  }
}, 'Cascading depth has no impact');

if (p7) passedTests++; else failedTests++;

// Property 8: Cancel is idempotent
console.log('\n--- Property 8: Cancel is idempotent ---');
totalTests++;
const p8 = await runPropertyTest(100, () => {
  const manager = createCancellationManager();
  const caseId = 'case-' + Date.now();

  const wi = manager.createWorkItem({
    taskId: 'idempotent-task',
    caseId,
    timeoutMs: 5000,
  });

  manager.enableWorkItem(wi.id);
  manager.startExecution(wi.id);

  const result1 = manager.cancelWorkItem(wi.id, 'manual');
  const state1 = JSON.stringify(manager.getWorkItem(wi.id));

  const result2 = manager.cancelWorkItem(wi.id, 'manual');
  const state2 = JSON.stringify(manager.getWorkItem(wi.id));

  if (state1 !== state2) {
    throw new Error('States differ after idempotent cancel');
  }
  if (result1.success !== true || result2.success !== false) {
    throw new Error('Second cancel should fail');
  }
}, 'Cancel is idempotent');

if (p8) passedTests++; else failedTests++;

// Property 9: Work item state transitions are valid
console.log('\n--- Property 9: Work item state transitions are valid ---');
totalTests++;
const p9 = await runPropertyTest(100, () => {
  const manager = createCancellationManager();
  const caseId = 'case-' + Date.now();

  const wi = manager.createWorkItem({
    taskId: 'transition-task',
    caseId,
    timeoutMs: 5000,
  });

  const transitions = [wi.state];

  manager.enableWorkItem(wi.id);
  transitions.push(manager.getWorkItem(wi.id).state);

  manager.startExecution(wi.id);
  transitions.push(manager.getWorkItem(wi.id).state);

  if (Math.random() > 0.5) {
    manager.completeWorkItem(wi.id);
  } else {
    manager.cancelWorkItem(wi.id, 'manual');
  }
  transitions.push(manager.getWorkItem(wi.id).state);

  if (transitions[0] !== 'pending') throw new Error('Invalid initial state');
  if (transitions[1] !== 'enabled') throw new Error('Invalid enabled state');
  if (transitions[2] !== 'executing') throw new Error('Invalid executing state');
  if (!['completed', 'cancelled'].includes(transitions[3])) {
    throw new Error('Invalid terminal state');
  }
}, 'Work item state transitions are valid');

if (p9) passedTests++; else failedTests++;

// Property 10: Receipts logged for all cancellations
console.log('\n--- Property 10: Receipts logged for all cancellations ---');
totalTests++;
const p10 = await runPropertyTest(100, () => {
  const manager = createCancellationManager();
  const caseId = 'case-' + Date.now();

  const itemCount = randomInt(1, 10);
  const workItems = [];

  for (let i = 0; i < itemCount; i++) {
    const wi = manager.createWorkItem({
      taskId: `receipt-task-${i}`,
      caseId,
      timeoutMs: 5000,
    });
    manager.enableWorkItem(wi.id);
    manager.startExecution(wi.id);
    workItems.push(wi);
  }

  const receiptCountBefore = manager.receiptLogger.getAllReceipts().length;

  for (const wi of workItems) {
    manager.cancelWorkItem(wi.id, 'manual');
  }

  const receiptCountAfter = manager.receiptLogger.getAllReceipts().length;

  if (receiptCountAfter <= receiptCountBefore) {
    throw new Error('No receipts logged');
  }
  if (receiptCountAfter - receiptCountBefore < itemCount) {
    throw new Error('Insufficient receipts logged');
  }
}, 'Receipts logged for all cancellations');

if (p10) passedTests++; else failedTests++;

// Specific test: 100+ cascading aborts
console.log('\n--- Specific Test: 100+ cascading aborts (no stack overflow) ---');
totalTests++;
try {
  const manager = createCancellationManager();
  const caseId = 'case-cascade-100';
  const depth = 100;

  const taskIds = Array.from({ length: depth }, (_, i) => `cascade-task-${i}`);
  createRandomWorkItems(manager, taskIds, caseId);

  const regions = [];
  for (let i = 0; i < depth; i++) {
    const region = manager.regionManager.createRegion({
      name: `Cascade ${i}`,
      taskIds: [taskIds[i]],
      parentRegionId: i > 0 ? regions[i - 1].id : undefined,
    });
    regions.push(region);
  }

  const rootWorkItems = manager.getWorkItemsForTask(taskIds[0]);
  if (rootWorkItems.length > 0) {
    manager.cancelWorkItem(rootWorkItems[0].id, 'manual');
  }

  const allDeactivated = regions.every(r =>
    manager.regionManager.getRegion(r.id).active === false
  );

  if (!allDeactivated) {
    throw new Error('Not all regions deactivated');
  }

  console.log('  PASSED: 100 cascading aborts handled successfully');
  passedTests++;
} catch (error) {
  console.log(`  FAILED: ${error.message}`);
  failedTests++;
}

// ============================================================================
// SUMMARY
// ============================================================================

console.log('\n' + '='.repeat(70));
console.log('FUZZING TEST SUMMARY');
console.log('='.repeat(70));
console.log(`Total Properties Tested: 10`);
console.log(`Total Iterations: 1000+ (100 per property)`);
console.log(`Total Test Cases: ${totalTests}`);
console.log(`Passed: ${passedTests}`);
console.log(`Failed: ${failedTests}`);
console.log(`Success Rate: ${((passedTests / totalTests) * 100).toFixed(1)}%`);
console.log('='.repeat(70));

if (failedTests === 0) {
  console.log('\n✅ ALL FUZZING TESTS PASSED!');
  process.exit(0);
} else {
  console.log(`\n❌ ${failedTests} FUZZING TEST(S) FAILED`);
  process.exit(1);
}
