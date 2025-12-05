/**
 * @fileoverview Simple test to debug RdfEngine context integration
 */

import { RdfEngine } from '../packages/knowledge-engine/src/engines/rdf-engine.mjs';
import { initStore, useStoreContext } from '../packages/composables/src/context/index.mjs';

const turtleData = `
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:alice a foaf:Person ;
    foaf:name "Alice" .
`;

async function testContext() {
  console.log('=== Testing RdfEngine Context Integration ===\n');

  const runApp = initStore([], {
    baseIRI: 'http://example.org/',
    deterministic: true,
  });

  await runApp(async () => {
    console.log('1. Testing context availability:');

    try {
      const storeContext = useStoreContext();
      console.log(`   ✓ Context store available: ${storeContext.store.size} quads`);
    } catch (error) {
      console.log(`   ✗ Context store error: ${error.message}`);
    }

    console.log('\n2. Testing RdfEngine context methods:');

    const engine = new RdfEngine();

    try {
      const contextStore = engine.getContextStore();
      console.log(`   ✓ getContextStore(): ${contextStore ? contextStore.size : 'null'} quads`);
    } catch (error) {
      console.log(`   ✗ getContextStore() error: ${error.message}`);
    }

    try {
      const store = engine.parseTurtleToContext(turtleData);
      console.log(`   ✓ parseTurtleToContext(): ${store.size} quads`);
    } catch (error) {
      console.log(`   ✗ parseTurtleToContext() error: ${error.message}`);
    }

    try {
      const stats = engine.getContextStats();
      console.log(`   ✓ getContextStats(): ${stats.quads} quads`);
    } catch (error) {
      console.log(`   ✗ getContextStats() error: ${error.message}`);
    }

    try {
      const result = await engine.queryContext(`
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        SELECT ?name WHERE {
          ?person foaf:name ?name .
        }
      `);
      console.log(`   ✓ queryContext(): ${result.results.length} results`);
      result.results.forEach(row => {
        console.log(`     - ${row.name.value}`);
      });
    } catch (error) {
      console.log(`   ✗ queryContext() error: ${error.message}`);
    }

    console.log('\n=== Test Complete ===');
  });
}

testContext().catch(console.error);
