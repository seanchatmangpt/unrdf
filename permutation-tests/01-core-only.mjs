#!/usr/bin/env node
/**
 * PERMUTATION TEST 01: Core Only
 * Tests @unrdf/core in isolation without any other packages
 */

const TEST_NAME = '01-core-only';
const startTime = performance.now();

try {
  console.log(`\n🧪 ${TEST_NAME}: Testing @unrdf/core in isolation\n`);

  // === IMPORT TEST ===
  console.log('📦 Importing @unrdf/core...');
  const { createStore, dataFactory } = await import('@unrdf/oxigraph');
  const { executeQuerySync } = await import('@unrdf/core');
  console.log('   ✅ Imports successful');

  // === CREATE STORE TEST ===
  console.log('\n🏪 Creating RDF store...');
  const store = createStore();
  console.log('   ✅ Store created');

  // === ADD QUAD TEST ===
  console.log('\n➕ Adding RDF quad...');
  const { namedNode, literal } = dataFactory;
  const quad = {
    subject: namedNode('http://example.org/Alice'),
    predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
    object: literal('Alice'),
    graph: namedNode('http://example.org/'),
  };
  store.insert(quad);
  console.log('   ✅ Quad added');

  // === QUERY TEST ===
  console.log('\n🔍 Executing SPARQL query...');
  const results = executeQuerySync(store, `
    SELECT ?name WHERE {
      GRAPH <http://example.org/> {
        ?s <http://xmlns.com/foaf/0.1/name> ?name
      }
    }
  `);

  if (results.length === 1 && results[0].name.value === 'Alice') {
    console.log('   ✅ Query returned correct result');
  } else {
    throw new Error(`Query failed: expected 1 result with name="Alice", got ${JSON.stringify(results)}`);
  }

  // === COUNT QUADS TEST ===
  console.log('\n📊 Counting quads...');
  const count = store.size;
  if (count === 1) {
    console.log(`   ✅ Store contains ${count} quad(s)`);
  } else {
    throw new Error(`Expected 1 quad, got ${count}`);
  }

  // === SUCCESS ===
  const elapsed = (performance.now() - startTime).toFixed(2);
  console.log(`\n✅ ${TEST_NAME}: PASS (${elapsed}ms)\n`);
  process.exit(0);

} catch (error) {
  const elapsed = (performance.now() - startTime).toFixed(2);
  console.error(`\n❌ ${TEST_NAME}: FAIL (${elapsed}ms)`);
  console.error(`   Error: ${error.message}`);
  console.error(`   Stack: ${error.stack}\n`);
  process.exit(1);
}
