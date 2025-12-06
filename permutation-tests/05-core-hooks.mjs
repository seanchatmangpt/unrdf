#!/usr/bin/env node
/**
 * PERMUTATION TEST 05: Core + Hooks
 * Tests @unrdf/core + @unrdf/hooks integration
 */

const TEST_NAME = '05-core-hooks';
const startTime = performance.now();

try {
  console.log(`\n🧪 ${TEST_NAME}: Testing @unrdf/core + @unrdf/hooks integration\n`);

  // === IMPORT TEST ===
  console.log('📦 Importing packages...');
  const { createStore, dataFactory } = await import('@unrdf/oxigraph');
  const { executeQuerySync } = await import('@unrdf/core');
  const { defineHook, executeHook } = await import('@unrdf/hooks');
  console.log('   ✅ Imports successful');

  // === CREATE STORE ===
  console.log('\n🏪 Creating store...');
  const store = createStore();
  console.log('   ✅ Store created');

  // === DEFINE HOOK ===
  console.log('\n🪝 Defining validation hook...');
  const hook = defineHook({
    name: 'validate-subject',
    trigger: 'before-add',
    async validate(quad) {
      return quad.subject.termType === 'NamedNode';
    },
  });
  console.log('   ✅ Hook defined');

  // === TEST HOOK EXECUTION ===
  console.log('\n▶️  Testing hook validation...');
  const validQuad = {
    subject: dataFactory.namedNode('http://example.org/Alice'),
    predicate: dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'),
    object: dataFactory.literal('Alice'),
  };

  const result = await executeHook(hook, validQuad);
  console.log(`   ✅ Hook validated quad: ${JSON.stringify(result)}`);

  // === ADD TO STORE ===
  console.log('\n➕ Adding quad to store...');
  store.insert(validQuad);
  console.log('   ✅ Quad added to store');

  // === QUERY ===
  console.log('\n🔍 Querying store...');
  const results = executeQuerySync(store, `
    SELECT ?name WHERE {
      ?s <http://xmlns.com/foaf/0.1/name> ?name
    }
  `);

  if (results.length === 1 && results[0].name.value === 'Alice') {
    console.log('   ✅ Query returned correct result');
  } else {
    throw new Error(`Query failed: ${JSON.stringify(results)}`);
  }

  // === SUCCESS ===
  const elapsed = (performance.now() - startTime).toFixed(2);
  console.log(`\n✅ ${TEST_NAME}: PASS (${elapsed}ms)\n`);
  console.log('   💡 Insight: Core + Hooks work together seamlessly');
  process.exit(0);

} catch (error) {
  const elapsed = (performance.now() - startTime).toFixed(2);
  console.error(`\n❌ ${TEST_NAME}: FAIL (${elapsed}ms)`);
  console.error(`   Error: ${error.message}`);
  console.error(`   Stack: ${error.stack}\n`);
  process.exit(1);
}
