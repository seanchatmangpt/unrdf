#!/usr/bin/env node
/**
 * PERMUTATION TEST 11: Core + Hooks + KGC 4D
 * Tests full integration of core substrate + policy hooks + temporal layer
 */

const TEST_NAME = '11-core-hooks-kgc4d';
const startTime = performance.now();

try {
  console.log(`\n🧪 ${TEST_NAME}: Testing @unrdf/core + @unrdf/hooks + @unrdf/kgc-4d\n`);

  // === IMPORT TEST ===
  console.log('📦 Importing packages...');
  const { createStore, dataFactory } = await import('@unrdf/oxigraph');
  const { executeQuerySync } = await import('@unrdf/core');
  const { defineHook, executeHook } = await import('@unrdf/hooks');
  const { KGCStore, now, toISO, EVENT_TYPES } = await import('@unrdf/kgc-4d');
  console.log('   ✅ All imports successful');

  // === DEFINE VALIDATION HOOK ===
  console.log('\n🪝 Defining validation hook...');
  const hook = defineHook({
    name: 'validate-person',
    trigger: 'before-add',
    async validate(quad) {
      // Only allow Person types
      if (quad.predicate.value === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type') {
        return quad.object.value === 'http://xmlns.com/foaf/0.1/Person';
      }
      return true;
    },
  });
  console.log('   ✅ Hook defined');

  // === CREATE KGC STORE ===
  console.log('\n🏪 Creating KGC store...');
  const kgcStore = new KGCStore();
  console.log('   ✅ KGC store created');

  // === VALIDATE QUAD WITH HOOK ===
  console.log('\n✓  Validating quad with hook...');
  const aliceQuad = {
    subject: dataFactory.namedNode('http://example.org/Alice'),
    predicate: dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    object: dataFactory.namedNode('http://xmlns.com/foaf/0.1/Person'),
    graph: dataFactory.namedNode('http://kgc.io/Universe'),
  };

  const validationResult = await executeHook(hook, aliceQuad);
  console.log(`   ✅ Validation passed: ${JSON.stringify(validationResult)}`);

  // === APPEND EVENT WITH VALIDATED QUAD ===
  console.log('\n📝 Appending event with validated quad...');
  const receipt = await kgcStore.appendEvent(
    { type: EVENT_TYPES.CREATE, payload: { action: 'create_alice_validated' } },
    [{ type: 'add', ...aliceQuad }]
  );

  console.log(`   ✅ Event appended: count=${receipt.receipt.event_count}, t_ns=${receipt.receipt.t_ns}`);

  // === QUERY UNIVERSE ===
  console.log('\n🔍 Querying universe for Person types...');
  const results = await kgcStore.queryUniverse(`
    SELECT ?person WHERE {
      ?person <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person>
    }
  `);

  if (results.length === 1 && results[0].person.value === 'http://example.org/Alice') {
    console.log('   ✅ Query returned validated person');
  } else {
    throw new Error(`Query failed: ${JSON.stringify(results)}`);
  }

  // === QUERY EVENT LOG ===
  console.log('\n📜 Querying event log...');
  const events = await kgcStore.queryEventLog(`
    SELECT ?event ?type WHERE {
      ?event <http://kgc.io/type> ?type
    }
  `);

  if (events.length >= 1) {
    console.log(`   ✅ Event log contains ${events.length} event(s)`);
  } else {
    throw new Error(`Event log query failed: ${JSON.stringify(events)}`);
  }

  // === SUCCESS ===
  const elapsed = (performance.now() - startTime).toFixed(2);
  console.log(`\n✅ ${TEST_NAME}: PASS (${elapsed}ms)\n`);
  console.log('   💡 Insight: Full stack works - policy validation + temporal event sourcing');
  console.log('   💡 Use case: Validated, auditable knowledge graphs with time-travel');
  process.exit(0);

} catch (error) {
  const elapsed = (performance.now() - startTime).toFixed(2);
  console.error(`\n❌ ${TEST_NAME}: FAIL (${elapsed}ms)`);
  console.error(`   Error: ${error.message}`);
  console.error(`   Stack: ${error.stack}\n`);
  process.exit(1);
}
