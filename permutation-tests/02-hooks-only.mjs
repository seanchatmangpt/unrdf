#!/usr/bin/env node
/**
 * PERMUTATION TEST 02: Hooks Only
 * Tests @unrdf/hooks in isolation (expected to FAIL - needs core)
 */

const TEST_NAME = '02-hooks-only';
const startTime = performance.now();

try {
  console.log(`\n🧪 ${TEST_NAME}: Testing @unrdf/hooks in isolation\n`);

  // === IMPORT TEST ===
  console.log('📦 Importing @unrdf/hooks...');
  const { defineHook, executeHook } = await import('../packages/hooks/src/index.mjs');
  console.log('   ✅ Imports successful');

  // === DEFINE HOOK TEST ===
  console.log('\n🪝 Defining validation hook...');
  const hook = defineHook({
    name: 'test-hook',
    trigger: 'before-add',
    async validate(quad) {
      return quad.subject.termType === 'NamedNode';
    },
  });
  console.log('   ✅ Hook defined');

  // === EXECUTE HOOK TEST (without store - will likely fail) ===
  console.log('\n▶️  Executing hook...');
  const { dataFactory } = await import('../packages/oxigraph/src/index.mjs');
  const testQuad = {
    subject: dataFactory.namedNode('http://example.org/test'),
    predicate: dataFactory.namedNode('http://example.org/prop'),
    object: dataFactory.literal('value'),
  };

  const result = await executeHook(hook, testQuad);
  console.log(`   ✅ Hook executed: ${JSON.stringify(result)}`);

  // === SUCCESS (if we get here) ===
  const elapsed = (performance.now() - startTime).toFixed(2);
  console.log(`\n✅ ${TEST_NAME}: PASS (${elapsed}ms)\n`);
  console.log('   ⚠️  Note: Hooks work in isolation but need core for full functionality');
  process.exit(0);

} catch (error) {
  const elapsed = (performance.now() - startTime).toFixed(2);
  console.error(`\n❌ ${TEST_NAME}: FAIL (${elapsed}ms)`);
  console.error(`   Error: ${error.message}`);

  if (error.message.includes('@unrdf/core') || error.message.includes('Cannot find')) {
    console.error('   💡 Expected failure: Hooks require @unrdf/core');
  }

  console.error(`   Stack: ${error.stack}\n`);
  process.exit(1);
}
