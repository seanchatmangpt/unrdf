/**
 * @fileoverview Test async context loss during JSON-LD operations
 */

import { RdfEngine } from '../packages/knowledge-engine/src/engines/rdf-engine.mjs';
import { initStore, useStoreContext } from '../packages/composables/src/context/index.mjs';

const jsonldData = {
  '@context': {
    ex: 'http://example.org/',
    foaf: 'http://xmlns.com/foaf/0.1/',
  },
  '@id': 'ex:charlie',
  '@type': 'foaf:Person',
  'foaf:name': 'Charlie',
};

async function testAsyncJsonLd() {
  console.log('=== Testing Async JSON-LD Context Loss ===\n');

  const runApp = initStore([], {
    baseIRI: 'http://example.org/',
    deterministic: true,
  });

  await runApp(async () => {
    console.log('1. Before JSON-LD operation:');
    try {
      const storeContext = useStoreContext();
      console.log(`   ✓ Context available: ${storeContext.store.size} quads`);
    } catch (error) {
      console.log(`   ✗ Context error: ${error.message}`);
    }

    console.log('\n2. Testing RdfEngine context methods:');
    const engine = new RdfEngine();

    try {
      const storeContext = engine.getStoreContext();
      console.log(`   ✓ getStoreContext(): ${storeContext ? 'available' : 'null'}`);
    } catch (error) {
      console.log(`   ✗ getStoreContext() error: ${error.message}`);
    }

    console.log('\n3. During JSON-LD operation:');
    try {
      await engine.fromJSONLDToContext(jsonldData);
      console.log(`   ✓ fromJSONLDToContext() completed`);
    } catch (error) {
      console.log(`   ✗ fromJSONLDToContext() error: ${error.message}`);
    }

    console.log('\n4. After JSON-LD operation:');
    try {
      const storeContext = useStoreContext();
      console.log(`   ✓ Context still available: ${storeContext.store.size} quads`);
    } catch (error) {
      console.log(`   ✗ Context lost: ${error.message}`);
    }

    try {
      const storeContext = engine.getStoreContext();
      console.log(`   ✓ Engine context still available: ${storeContext ? 'yes' : 'no'}`);
    } catch (error) {
      console.log(`   ✗ Engine context lost: ${error.message}`);
    }

    console.log('\n=== Test Complete ===');
  });
}

testAsyncJsonLd().catch(console.error);
