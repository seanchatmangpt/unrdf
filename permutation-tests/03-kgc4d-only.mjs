#!/usr/bin/env node
/**
 * PERMUTATION TEST 03: KGC 4D Only
 * Tests @unrdf/kgc-4d in isolation (expected to FAIL - needs core + oxigraph)
 */

const TEST_NAME = '03-kgc4d-only';
const startTime = performance.now();

try {
  console.log(`\n🧪 ${TEST_NAME}: Testing @unrdf/kgc-4d in isolation\n`);

  // === IMPORT TEST ===
  console.log('📦 Importing @unrdf/kgc-4d...');
  const { KGCStore, now, toISO, EVENT_TYPES } = await import('../packages/kgc-4d/src/index.mjs');
  console.log('   ✅ Imports successful');

  // === TIME TEST ===
  console.log('\n⏰ Testing nanosecond time...');
  const t_ns = now();
  const iso = toISO(t_ns);
  console.log(`   ✅ Current time: ${iso}`);

  // === STORE TEST ===
  console.log('\n🏪 Creating KGC store...');
  const store = new KGCStore();
  console.log('   ✅ KGC store created');

  // === EVENT TEST ===
  console.log('\n📝 Appending event...');
  const receipt = await store.appendEvent(
    { type: EVENT_TYPES.CREATE, payload: { test: 'data' } },
    []
  );
  console.log(`   ✅ Event appended: ${receipt.receipt.event_count} events`);

  // === SUCCESS ===
  const elapsed = (performance.now() - startTime).toFixed(2);
  console.log(`\n✅ ${TEST_NAME}: PASS (${elapsed}ms)\n`);
  console.log('   ⚠️  Note: KGC 4D works in isolation (has bundled deps)');
  process.exit(0);

} catch (error) {
  const elapsed = (performance.now() - startTime).toFixed(2);
  console.error(`\n❌ ${TEST_NAME}: FAIL (${elapsed}ms)`);
  console.error(`   Error: ${error.message}`);

  if (error.message.includes('@unrdf/core') || error.message.includes('@unrdf/oxigraph')) {
    console.error('   💡 Expected failure: KGC 4D requires @unrdf/core and @unrdf/oxigraph');
  }

  console.error(`   Stack: ${error.stack}\n`);
  process.exit(1);
}
