/**
 * @unrdf/browser - Basic Example
 *
 * Demonstrates browser-based RDF storage with IndexedDB.
 */

import {
  createBrowserRDFStore,
  isBrowserEnvironment,
  getStorageQuota,
  formatStorageSize,
  checkStorageQuota,
} from '../src/index.mjs';

/**
 * Main example function
 */
async function main() {
  console.log('=== @unrdf/browser Example ===\n');

  // Check environment
  console.log('1. Environment Detection');
  const isBrowser = isBrowserEnvironment();
  console.log(`   Running in browser: ${isBrowser}`);

  // Create browser-optimized store
  console.log('\n2. Creating Browser RDF Store');
  const store = await createBrowserRDFStore({
    dbName: 'example-rdf',
    storeName: 'quads',
    persistent: true,
  });
  console.log('   Store created successfully');

  // Check storage quota
  console.log('\n3. Storage Quota Information');
  const quota = await getStorageQuota();
  console.log(`   Available: ${formatStorageSize(quota.available)}`);
  console.log(`   Used: ${formatStorageSize(quota.used)}`);
  console.log(`   Total: ${formatStorageSize(quota.quota)}`);

  const quotaInfo = await checkStorageQuota();
  console.log(`   Usage: ${quotaInfo.percentUsed.toFixed(2)}%`);

  // Example quad operations (if IndexedDB store)
  if (store.isOpen) {
    console.log('\n4. IndexedDB Store Operations');
    console.log('   Store is persistent and ready for quad operations');
    console.log(`   Database: ${store.dbName}`);
    console.log(`   Object Store: ${store.storeName}`);
  } else {
    console.log('\n4. Using Memory Store');
    console.log('   IndexedDB not available, using in-memory store');
  }

  console.log('\n=== Example Complete ===');
}

// Run example
if (typeof window !== 'undefined') {
  // Browser environment
  main().catch(console.error);
} else {
  // Node.js environment
  console.log('This example is designed for browser environments.');
  console.log('Run in a browser with module support or use a bundler.');
}
