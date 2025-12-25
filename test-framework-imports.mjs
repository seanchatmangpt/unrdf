#!/usr/bin/env node
/**
 * Runtime verification: Test framework imports/exports
 */

console.log('Testing framework imports/exports...\n');

// Test 1: Graph routing framework
console.log('[1] Testing microfw-9-graph-routing.mjs');
try {
  const { GraphAwareRouter, example } = await import('./microfw-9-graph-routing.mjs');
  console.log('  ✅ Import successful');
  console.log('  ✅ Exports: GraphAwareRouter =', typeof GraphAwareRouter);
  console.log('  ✅ Exports: example =', typeof example);

  // Test instantiation
  const router = new GraphAwareRouter();
  console.log('  ✅ Instantiation: GraphAwareRouter created');
  console.log('  ✅ Methods:', Object.getOwnPropertyNames(GraphAwareRouter.prototype).filter(m => m !== 'constructor').join(', '));
} catch (err) {
  console.log('  ❌ FAILED:', err.message);
}

// Test 2: Mega framework (with dependencies - expected to fail)
console.log('\n[2] Testing max-combo-10-mega-framework.mjs');
try {
  const { MegaFramework, runExample } = await import('./max-combo-10-mega-framework.mjs');
  console.log('  ✅ Import successful');
  console.log('  ✅ Exports: MegaFramework =', typeof MegaFramework);
  console.log('  ✅ Exports: runExample =', typeof runExample);
} catch (err) {
  console.log('  ❌ FAILED:', err.message);
  console.log('  ❌ Error code:', err.code);
}

// Test 3: Standalone mega framework
console.log('\n[3] Testing max-combo-10-mega-framework-standalone.mjs');
try {
  const { MegaFramework, runExample } = await import('./max-combo-10-mega-framework-standalone.mjs');
  console.log('  ✅ Import successful');
  console.log('  ✅ Exports: MegaFramework =', typeof MegaFramework);
  console.log('  ✅ Exports: runExample =', typeof runExample);

  // Test instantiation
  const framework = new MegaFramework();
  console.log('  ✅ Instantiation: MegaFramework created');
  console.log('  ✅ Properties:', Object.keys(framework).slice(0, 5).join(', '), '...');
} catch (err) {
  console.log('  ❌ FAILED:', err.message);
}

console.log('\n='.repeat(70));
console.log('Import/Export Verification Complete');
console.log('='.repeat(70));
