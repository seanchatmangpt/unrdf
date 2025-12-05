/**
 * @fileoverview Test async context preservation
 */

import { initStore, useStoreContext } from '../packages/composables/src/context/index.mjs';

async function testAsyncContext() {
  console.log('=== Testing Async Context Preservation ===\n');

  const runApp = initStore([], {
    baseIRI: 'http://example.org/',
    deterministic: true,
  });

  await runApp(async () => {
    console.log('1. Before async operation:');
    try {
      const storeContext = useStoreContext();
      console.log(`   ✓ Context available: ${storeContext.store.size} quads`);
    } catch (error) {
      console.log(`   ✗ Context error: ${error.message}`);
    }

    console.log('\n2. During async operation:');

    // Simulate an async operation
    await new Promise(resolve => setTimeout(resolve, 10));

    try {
      const storeContext = useStoreContext();
      console.log(`   ✓ Context still available: ${storeContext.store.size} quads`);
    } catch (error) {
      console.log(`   ✗ Context lost: ${error.message}`);
    }

    console.log('\n3. After async operation:');

    // Another async operation
    await new Promise(resolve => setTimeout(resolve, 10));

    try {
      const storeContext = useStoreContext();
      console.log(`   ✓ Context still available: ${storeContext.store.size} quads`);
    } catch (error) {
      console.log(`   ✗ Context lost: ${error.message}`);
    }

    console.log('\n=== Test Complete ===');
  });
}

testAsyncContext().catch(console.error);
