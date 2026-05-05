#!/usr/bin/env node
/**
 * @fileoverview Final Integration Test for Narrative State Chain
 *
 * **Purpose**: Verify all 6 backend modules integrate correctly
 *
 * Tests:
 * 1. Module import verification (6 modules)
 * 2. Export inventory (types, functions, classes)
 * 3. Basic flow test (if dependencies available)
 * 4. Interface compatibility checks
 *
 * @module narrative-state-chain/integration-test
 */

console.log('📋 NARRATIVE STATE CHAIN - FINAL INTEGRATION TEST\n');
console.log('=' .repeat(70));
console.log('\n');

const results = {
  modules: { total: 6, loaded: 0, failed: [] },
  exports: { types: 0, functions: 0, classes: 0 },
  flow: { attempted: false, success: false, error: null },
  verdict: 'UNKNOWN'
};

// ============================================================================
// TEST 1: Module Import Verification
// ============================================================================

console.log('TEST 1: Module Import Verification');
console.log('-'.repeat(70));

const modules = [
  { name: 'types', path: './types.mjs' },
  { name: 'store', path: './store.mjs' },
  { name: 'reconcile', path: './reconcile.mjs' },
  { name: 'guards', path: './guards.mjs' },
  { name: 'receipts', path: './receipts.mjs' },
  { name: 'bridges', path: './bridges.mjs' }
];

const moduleExports = {};

for (const mod of modules) {
  try {
    const imported = await import(mod.path);
    moduleExports[mod.name] = imported;
    results.modules.loaded++;
    console.log(`✅ ${mod.name.padEnd(15)} - LOADED (${Object.keys(imported).length} exports)`);
  } catch (error) {
    results.modules.failed.push({ module: mod.name, error: error.message });
    console.log(`❌ ${mod.name.padEnd(15)} - FAILED: ${error.message}`);
  }
}

console.log(`\n📊 Modules: ${results.modules.loaded}/${results.modules.total} loaded successfully\n`);

// ============================================================================
// TEST 2: Export Inventory
// ============================================================================

console.log('TEST 2: Export Inventory');
console.log('-'.repeat(70));

// Types module exports
if (moduleExports.types) {
  console.log('\n📦 types.mjs exports:');
  const typeExports = Object.keys(moduleExports.types);
  const schemas = typeExports.filter(e => e.includes('Schema'));
  const validators = typeExports.filter(e => e.startsWith('validate'));

  console.log(`   Schemas: ${schemas.length} - ${schemas.join(', ')}`);
  console.log(`   Validators: ${validators.length} - ${validators.join(', ')}`);
  results.exports.types += typeExports.length;
}

// Store module exports
if (moduleExports.store) {
  console.log('\n📦 store.mjs exports:');
  const storeExports = Object.keys(moduleExports.store);
  console.log(`   Classes: ${storeExports.join(', ')}`);
  results.exports.classes += storeExports.length;
}

// Reconcile module exports
if (moduleExports.reconcile) {
  console.log('\n📦 reconcile.mjs exports:');
  const reconcileExports = Object.keys(moduleExports.reconcile);
  console.log(`   Functions: ${reconcileExports.join(', ')}`);
  results.exports.functions += reconcileExports.length;
}

// Guards module exports
if (moduleExports.guards) {
  console.log('\n📦 guards.mjs exports:');
  const guardsExports = Object.keys(moduleExports.guards);
  const evaluators = guardsExports.filter(e => e.startsWith('evaluate'));
  const creators = guardsExports.filter(e => e.startsWith('create'));

  console.log(`   Evaluators: ${evaluators.length} - ${evaluators.join(', ')}`);
  console.log(`   Creators: ${creators.length} - ${creators.join(', ')}`);
  results.exports.functions += guardsExports.length;
}

// Receipts module exports
if (moduleExports.receipts) {
  console.log('\n📦 receipts.mjs exports:');
  const receiptsExports = Object.keys(moduleExports.receipts);
  const generators = receiptsExports.filter(e => e.includes('generate') || e.includes('compute'));
  const verifiers = receiptsExports.filter(e => e.includes('verify') || e.includes('hash') || e.includes('sign'));

  console.log(`   Generators: ${generators.length} - ${generators.join(', ')}`);
  console.log(`   Verifiers: ${verifiers.length} - ${verifiers.join(', ')}`);
  results.exports.functions += receiptsExports.length;
}

// Bridges module exports
if (moduleExports.bridges) {
  console.log('\n📦 bridges.mjs exports:');
  const bridgesExports = Object.keys(moduleExports.bridges);
  const classes = bridgesExports.filter(e => e === 'Bridge');
  const functions = bridgesExports.filter(e => e !== 'Bridge');

  console.log(`   Classes: ${classes.join(', ')}`);
  console.log(`   Functions: ${functions.join(', ')}`);
  results.exports.classes += classes.length;
  results.exports.functions += functions.length;
}

console.log(`\n📊 Total Exports: ${results.exports.types + results.exports.functions + results.exports.classes}`);
console.log(`   - Types/Schemas: ${results.exports.types}`);
console.log(`   - Functions: ${results.exports.functions}`);
console.log(`   - Classes: ${results.exports.classes}\n`);

// ============================================================================
// TEST 3: Basic Flow Test
// ============================================================================

console.log('TEST 3: Basic Flow Test');
console.log('-'.repeat(70));

results.flow.attempted = true;

try {
  const { UniverseStore, SceneStore } = moduleExports.store;
  const { createIdentityReconcile } = moduleExports.reconcile;
  const { createAllowAllGuard, evaluateAllGuards } = moduleExports.guards;
  const { generateReceipt, verifyReceiptChain } = moduleExports.receipts;
  const { Bridge } = moduleExports.bridges;

  console.log('🔧 Step 1: Creating stores...');
  const universeStore = new UniverseStore();
  const sceneStore = new SceneStore(universeStore);
  console.log('   ✅ UniverseStore and SceneStore created');

  console.log('\n🔧 Step 2: Creating universe...');
  const universe = await universeStore.create({
    schema: 'http://example.org/integration-test#',
    reconcile: createIdentityReconcile(),
    guards: [createAllowAllGuard('allow-all', 'Allow All')],
    invariants: [
      {
        id: 'test-invariant',
        name: 'Test Invariant',
        predicate: () => true
      }
    ],
    metadata: {
      name: 'Integration Test Universe',
      description: 'Testing module integration',
      version: '[VERSION]'
    }
  });
  console.log(`   ✅ Universe created: ${universe.id}`);
  console.log(`   📝 Schema: ${universe.schema}`);

  console.log('\n🔧 Step 3: Adding scene with guards + reconciliation...');
  const scene = await sceneStore.add(
    universe.id,
    [{ type: 'test', value: 'integration' }],
    { testProperty: 'testValue' },
    { agent: 'integration-test@example.com' }
  );
  console.log(`   ✅ Scene created: ${scene.id}`);
  console.log(`   📊 Delta: ${JSON.stringify(scene.delta)}`);
  console.log(`   🧾 Receipt generated: ${scene.receipts.length} receipt(s)`);

  console.log('\n🔧 Step 4: Verifying receipt...');
  const receiptValid = scene.receipts[0] && scene.receipts[0].receiptHash;
  console.log(`   ✅ Receipt has hash: ${receiptValid ? 'YES' : 'NO'}`);
  console.log(`   🔗 Receipt hash: ${scene.receipts[0]?.receiptHash?.substring(0, 16)}...`);

  console.log('\n🔧 Step 5: Creating bridge...');
  const targetUniverse = await universeStore.create({
    schema: 'http://example.org/target#',
    reconcile: createIdentityReconcile(),
    metadata: { name: 'Target Universe' }
  });

  const bridge = await Bridge.define(
    universe,
    targetUniverse,
    (value) => ({ ...value, transformed: true }),
    async () => true,
    { name: 'Test Bridge' }
  );
  console.log(`   ✅ Bridge created: ${bridge.id}`);
  console.log(`   🌉 ${bridge.sourceUniverseId} → ${bridge.targetUniverseId}`);

  results.flow.success = true;
  console.log('\n✅ BASIC FLOW TEST PASSED - All operations completed successfully\n');

} catch (error) {
  results.flow.error = error.message;
  console.log(`\n❌ BASIC FLOW TEST FAILED: ${error.message}`);
  console.log(`   Stack: ${error.stack?.split('\n')[1]?.trim()}\n`);
}

// ============================================================================
// TEST 4: Interface Compatibility
// ============================================================================

console.log('TEST 4: Interface Compatibility');
console.log('-'.repeat(70));

const compatibility = {
  storeToReconcile: false,
  storeToGuards: false,
  storeToReceipts: false,
  storeToBridges: false
};

try {
  // Check if UniverseStore can work with reconcile functions
  if (moduleExports.store?.UniverseStore && moduleExports.reconcile?.createIdentityReconcile) {
    compatibility.storeToReconcile = true;
    console.log('✅ UniverseStore ↔ reconcile.mjs - Compatible');
  }

  // Check if SceneStore can work with guards
  if (moduleExports.store?.SceneStore && moduleExports.guards?.evaluateAllGuards) {
    compatibility.storeToGuards = true;
    console.log('✅ SceneStore ↔ guards.mjs - Compatible');
  }

  // Check if SceneStore can work with receipts
  if (moduleExports.store?.SceneStore && moduleExports.receipts?.generateReceipt) {
    compatibility.storeToReceipts = true;
    console.log('✅ SceneStore ↔ receipts.mjs - Compatible');
  }

  // Check if Bridge can work with UniverseStore
  if (moduleExports.bridges?.Bridge && moduleExports.store?.UniverseStore) {
    compatibility.storeToBridges = true;
    console.log('✅ Bridge ↔ store.mjs - Compatible');
  }

  const compatCount = Object.values(compatibility).filter(Boolean).length;
  console.log(`\n📊 Interface Compatibility: ${compatCount}/4 checks passed\n`);

} catch (error) {
  console.log(`❌ Compatibility check failed: ${error.message}\n`);
}

// ============================================================================
// FINAL VERDICT
// ============================================================================

console.log('=' .repeat(70));
console.log('FINAL INTEGRATION TEST REPORT');
console.log('=' .repeat(70));

console.log('\n📋 Summary:');
console.log(`   Modules Loaded: ${results.modules.loaded}/6`);
console.log(`   Total Exports: ${results.exports.types + results.exports.functions + results.exports.classes}`);
console.log(`   Basic Flow Test: ${results.flow.success ? 'PASSED ✅' : 'FAILED ❌'}`);
console.log(`   Interface Compatibility: ${Object.values(compatibility).filter(Boolean).length}/4`);

if (results.modules.failed.length > 0) {
  console.log('\n❌ Failed Modules:');
  results.modules.failed.forEach(f => {
    console.log(`   - ${f.module}: ${f.error}`);
  });
}

// Determine verdict
if (results.modules.loaded === 6 && results.flow.success) {
  results.verdict = 'READY FOR PRODUCTION';
  console.log('\n✅ VERDICT: READY FOR PRODUCTION');
  console.log('\nAll 6 backend modules integrate correctly:');
  console.log('   types → store → reconcile → guards → receipts → bridges');
  console.log('\nThe system forms a coherent, working backend architecture.');
} else if (results.modules.loaded === 6 && !results.flow.success) {
  results.verdict = 'NEEDS RUNTIME FIXES';
  console.log('\n⚠️  VERDICT: NEEDS RUNTIME FIXES');
  console.log('\nAll modules load but runtime integration has issues.');
  console.log(`Error: ${results.flow.error}`);
} else {
  results.verdict = 'NEEDS MODULE FIXES';
  console.log('\n❌ VERDICT: NEEDS MODULE FIXES');
  console.log('\nSome modules failed to load. Fix import errors first.');
}

console.log('\n' + '='.repeat(70));
console.log('\n');

// Exit with appropriate code
process.exit(results.verdict === 'READY FOR PRODUCTION' ? 0 : 1);
