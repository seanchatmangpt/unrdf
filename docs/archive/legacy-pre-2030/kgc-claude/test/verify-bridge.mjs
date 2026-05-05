#!/usr/bin/env node
/**
 * Quick verification script for PolicyBridge
 * Can be run without vitest dependency
 */

import { join } from 'path';
import { fileURLToPath } from 'url';
import { dirname } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const projectRoot = join(__dirname, '../../..');

console.log('PolicyBridge Verification\n');
console.log('='.repeat(60));

let passed = 0;
let failed = 0;

function test(name, fn) {
  process.stdout.write(`${name} ... `);
  try {
    fn();
    console.log('✅ PASS');
    passed++;
  } catch (error) {
    console.log('❌ FAIL');
    console.log(`  Error: ${error.message}`);
    failed++;
  }
}

async function asyncTest(name, fn) {
  process.stdout.write(`${name} ... `);
  try {
    await fn();
    console.log('✅ PASS');
    passed++;
  } catch (error) {
    console.log('❌ FAIL');
    console.log(`  Error: ${error.message}`);
    failed++;
  }
}

// Import the module
const {
  loadPolicyPack,
  evaluateHookCondition,
  applyPolicy,
  isDenialReceipt,
  createPolicyBridge,
  HOOK_TO_WORKITEM_LIFECYCLE,
  WORKITEM_TO_HOOK_TRIGGERS,
} = await import('../src/PolicyBridge.mjs');

const { enqueueWorkItem } = await import('../src/async-workflow.mjs');

console.log('\n1. Module Import Tests');
console.log('-'.repeat(60));

test('loadPolicyPack is a function', () => {
  if (typeof loadPolicyPack !== 'function') {
    throw new Error('loadPolicyPack is not a function');
  }
});

test('evaluateHookCondition is a function', () => {
  if (typeof evaluateHookCondition !== 'function') {
    throw new Error('evaluateHookCondition is not a function');
  }
});

test('applyPolicy is a function', () => {
  if (typeof applyPolicy !== 'function') {
    throw new Error('applyPolicy is not a function');
  }
});

test('createPolicyBridge is a function', () => {
  if (typeof createPolicyBridge !== 'function') {
    throw new Error('createPolicyBridge is not a function');
  }
});

console.log('\n2. Lifecycle Mapping Tests');
console.log('-'.repeat(60));

test('HOOK_TO_WORKITEM_LIFECYCLE maps before-add to queued', () => {
  if (HOOK_TO_WORKITEM_LIFECYCLE['before-add'] !== 'queued') {
    throw new Error(`Expected 'queued', got '${HOOK_TO_WORKITEM_LIFECYCLE['before-add']}'`);
  }
});

test('HOOK_TO_WORKITEM_LIFECYCLE maps after-add to assigned', () => {
  if (HOOK_TO_WORKITEM_LIFECYCLE['after-add'] !== 'assigned') {
    throw new Error(`Expected 'assigned', got '${HOOK_TO_WORKITEM_LIFECYCLE['after-add']}'`);
  }
});

test('WORKITEM_TO_HOOK_TRIGGERS has queued status', () => {
  if (!WORKITEM_TO_HOOK_TRIGGERS['queued']) {
    throw new Error('queued status not found');
  }
  if (!WORKITEM_TO_HOOK_TRIGGERS['queued'].includes('before-add')) {
    throw new Error('queued does not include before-add');
  }
});

console.log('\n3. Policy Pack Loading Tests');
console.log('-'.repeat(60));

await asyncTest('Load policy pack from /policy-packs/default', async () => {
  const manifestPath = join(projectRoot, 'policy-packs/default/manifest.json');
  const pack = await loadPolicyPack(manifestPath, projectRoot);

  if (!pack) {
    throw new Error('Policy pack is null');
  }
  if (!pack.loaded) {
    throw new Error('Policy pack not loaded');
  }
  if (pack.manifest.meta.name !== 'default') {
    throw new Error(`Expected 'default', got '${pack.manifest.meta.name}'`);
  }
});

await asyncTest('Policy pack contains hooks', async () => {
  const manifestPath = join(projectRoot, 'policy-packs/default/manifest.json');
  const pack = await loadPolicyPack(manifestPath, projectRoot);
  const hooks = pack.getHooks();

  if (!Array.isArray(hooks)) {
    throw new Error('Hooks is not an array');
  }
  if (hooks.length === 0) {
    throw new Error('No hooks found');
  }
});

console.log('\n4. WorkItem Creation Tests');
console.log('-'.repeat(60));

test('Create a WorkItem', () => {
  const workItem = enqueueWorkItem({
    type: 'test_operation',
    payload: { test: 'data' },
  });

  if (!workItem.id) {
    throw new Error('WorkItem has no ID');
  }
  if (workItem.status !== 'queued') {
    throw new Error(`Expected status 'queued', got '${workItem.status}'`);
  }
});

console.log('\n5. PolicyBridge Factory Tests');
console.log('-'.repeat(60));

test('Create PolicyBridge instance', () => {
  const bridge = createPolicyBridge({ basePath: projectRoot });

  if (!bridge) {
    throw new Error('Bridge is null');
  }
  if (typeof bridge.loadPack !== 'function') {
    throw new Error('bridge.loadPack is not a function');
  }
  if (typeof bridge.evaluate !== 'function') {
    throw new Error('bridge.evaluate is not a function');
  }
});

await asyncTest('PolicyBridge caches policy packs', async () => {
  const bridge = createPolicyBridge({ basePath: projectRoot });

  const pack1 = await bridge.loadPack('policy-packs/default/manifest.json');
  const pack2 = await bridge.loadPack('policy-packs/default/manifest.json');

  if (pack1 !== pack2) {
    throw new Error('Policy packs are not cached (different instances)');
  }

  const stats = bridge.getCacheStats();
  if (stats.size !== 1) {
    throw new Error(`Expected cache size 1, got ${stats.size}`);
  }
});

console.log('\n6. Integration Tests');
console.log('-'.repeat(60));

await asyncTest('End-to-end: Load pack, create WorkItem, apply policy', async () => {
  const manifestPath = join(projectRoot, 'policy-packs/default/manifest.json');
  const pack = await loadPolicyPack(manifestPath, projectRoot);

  const workItem = enqueueWorkItem({
    type: 'test_operation',
    payload: { test: 'data' },
  });

  const result = await applyPolicy(workItem, pack);

  if (!result) {
    throw new Error('Result is null');
  }

  // Result should be either a WorkItem or DenialReceipt
  const isDenied = isDenialReceipt(result);
  if (!isDenied && !result.id) {
    throw new Error('Result is neither a DenialReceipt nor a WorkItem');
  }

  console.log(`\n  → Result: ${isDenied ? 'DENIED' : 'ADMITTED'}`);
  if (isDenied) {
    console.log(`  → Reason: ${result.reason || 'N/A'}`);
  }
});

console.log('\n' + '='.repeat(60));
console.log(`\nResults: ${passed} passed, ${failed} failed\n`);

if (failed > 0) {
  console.log('❌ Some tests failed\n');
  process.exit(1);
} else {
  console.log('✅ All tests passed\n');
  process.exit(0);
}
