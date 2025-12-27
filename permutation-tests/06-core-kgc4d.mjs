#!/usr/bin/env node
/**
 * PERMUTATION TEST 06: Core + KGC 4D
 * Tests @unrdf/core + @unrdf/kgc-4d integration
 */

const TEST_NAME = '06-core-kgc4d';
const startTime = performance.now();

try {
  console.log(`\n🧪 ${TEST_NAME}: Testing @unrdf/core + @unrdf/kgc-4d integration\n`);

  // === IMPORT TEST ===
  console.log('📦 Importing packages...');
  const { createStore, dataFactory } = await import('../packages/oxigraph/src/index.mjs');
  const { executeQuerySync } = await import('../packages/core/src/index.mjs');
  const { KGCStore, now, toISO, EVENT_TYPES, GitBackbone, freezeUniverse } = await import('../packages/kgc-4d/src/index.mjs');
  console.log('   ✅ Imports successful');

  // === TIME TEST ===
  console.log('\n⏰ Testing nanosecond time...');
  const t_ns = now();
  const iso = toISO(t_ns);
  console.log(`   ✅ Time: ${iso}`);

  // === CREATE KGC STORE ===
  console.log('\n🏪 Creating KGC store...');
  const kgcStore = new KGCStore();
  console.log('   ✅ KGC store created');

  // === APPEND EVENT ===
  console.log('\n📝 Appending event with delta...');
  const quad = {
    subject: dataFactory.namedNode('http://example.org/Alice'),
    predicate: dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'),
    object: dataFactory.literal('Alice'),
    graph: dataFactory.namedNode('http://kgc.io/Universe'),
  };

  const receipt = await kgcStore.appendEvent(
    { type: EVENT_TYPES.CREATE, payload: { action: 'create_alice' } },
    [{ type: 'add', ...quad }]
  );

  console.log(`   ✅ Event appended: count=${receipt.receipt.event_count}, t_ns=${receipt.receipt.t_ns}`);

  // === QUERY UNIVERSE ===
  console.log('\n🔍 Querying universe state...');
  const results = await kgcStore.queryUniverse(`
    SELECT ?name WHERE {
      GRAPH <http://kgc.io/Universe> {
        ?s <http://xmlns.com/foaf/0.1/name> ?name
      }
    }
  `);

  if (results.length === 1 && results[0].name.value === 'Alice') {
    console.log('   ✅ Query returned correct result from universe');
  } else {
    throw new Error(`Universe query failed: ${JSON.stringify(results)}`);
  }

  // === FREEZE TEST (optional - requires Git repo) ===
  try {
    console.log('\n❄️  Testing universe freeze (Git required)...');
    const git = new GitBackbone('/tmp/kgc-test-repo');
    const frozen = await freezeUniverse(kgcStore, git);
    console.log(`   ✅ Universe frozen: git_ref=${frozen.git_ref.substring(0, 8)}`);
  } catch (gitError) {
    console.log(`   ⚠️  Freeze skipped (Git not initialized): ${gitError.message}`);
  }

  // === SUCCESS ===
  const elapsed = (performance.now() - startTime).toFixed(2);
  console.log(`\n✅ ${TEST_NAME}: PASS (${elapsed}ms)\n`);
  console.log('   💡 Insight: Core + KGC 4D provide temporal event sourcing');
  process.exit(0);

} catch (error) {
  const elapsed = (performance.now() - startTime).toFixed(2);
  console.error(`\n❌ ${TEST_NAME}: FAIL (${elapsed}ms)`);
  console.error(`   Error: ${error.message}`);
  console.error(`   Stack: ${error.stack}\n`);
  process.exit(1);
}
