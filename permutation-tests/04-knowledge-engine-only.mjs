#!/usr/bin/env node
/**
 * PERMUTATION TEST 04: Knowledge Engine Only
 * Tests @unrdf/knowledge-engine in isolation (expected to FAIL - needs core)
 */

const TEST_NAME = '04-knowledge-engine-only';
const startTime = performance.now();

try {
  console.log(`\n🧪 ${TEST_NAME}: Testing @unrdf/knowledge-engine in isolation\n`);

  // === IMPORT TEST ===
  console.log('📦 Importing @unrdf/knowledge-engine...');
  const { reason, canonicalize } = await import('../packages/knowledge-engine/src/index.mjs');
  console.log('   ✅ Imports successful');

  // === REASON TEST (without store - will likely fail) ===
  console.log('\n🧠 Testing reasoning...');
  const result = await reason([], 'test-rules.n3');
  console.log(`   ✅ Reasoning executed: ${result.length} inferred quads`);

  // === SUCCESS (if we get here) ===
  const elapsed = (performance.now() - startTime).toFixed(2);
  console.log(`\n✅ ${TEST_NAME}: PASS (${elapsed}ms)\n`);
  console.log('   ⚠️  Note: Knowledge engine works but needs core for full functionality');
  process.exit(0);

} catch (error) {
  const elapsed = (performance.now() - startTime).toFixed(2);
  console.error(`\n❌ ${TEST_NAME}: FAIL (${elapsed}ms)`);
  console.error(`   Error: ${error.message}`);

  if (error.message.includes('@unrdf/core') || error.message.includes('Cannot find')) {
    console.error('   💡 Expected failure: Knowledge engine requires @unrdf/core');
  }

  console.error(`   Stack: ${error.stack}\n`);
  process.exit(1);
}
