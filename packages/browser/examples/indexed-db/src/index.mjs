/**
 * @file IndexedDB persistence example for @unrdf/browser
 * @module @unrdf/example-indexed-db
 */

import { IndexedDBStore } from '@unrdf/browser';
import { quad, namedNode, literal } from '@unrdf/core';

/**
 * Configuration for IndexedDB database
 */
const DB_CONFIG = {
  name: 'unrdf-example',
  version: 1,
  storeName: 'quads'
};

/**
 * Create and initialize IndexedDB store
 *
 * @returns {Promise<IndexedDBStore>} Initialized store instance
 */
export async function createIndexedDBStore() {
  const store = new IndexedDBStore(DB_CONFIG);
  await store.open();
  return store;
}

/**
 * Add sample RDF data to IndexedDB
 *
 * @param {IndexedDBStore} store - Store instance
 * @returns {Promise<void>}
 */
export async function addSampleData(store) {
  const quads = [
    quad(
      namedNode('http://example.org/user/1'),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal('Alice')
    ),
    quad(
      namedNode('http://example.org/user/1'),
      namedNode('http://xmlns.com/foaf/0.1/email'),
      literal('alice@example.org')
    ),
    quad(
      namedNode('http://example.org/user/2'),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal('Bob')
    )
  ];

  for (const q of quads) {
    await store.add(q);
  }

  console.log(`Added ${quads.length} quads to IndexedDB`);
}

/**
 * Query stored quads by pattern
 *
 * @param {IndexedDBStore} store - Store instance
 * @param {object} pattern - Query pattern
 * @returns {Promise<Array>} Matching quads
 */
export async function queryQuads(store, pattern = {}) {
  const results = await store.match(
    pattern.subject,
    pattern.predicate,
    pattern.object,
    pattern.graph
  );

  console.log(`Found ${results.length} matching quads`);
  return results;
}

/**
 * Persist data and verify across sessions
 *
 * @param {IndexedDBStore} store - Store instance
 * @returns {Promise<boolean>} True if data persisted correctly
 */
export async function verifyPersistence(store) {
  const beforeClose = await store.size();
  await store.close();

  // Reopen connection
  await store.open();
  const afterReopen = await store.size();

  const persisted = beforeClose === afterReopen;
  console.log(`Persistence check: ${persisted ? 'PASS' : 'FAIL'}`);
  console.log(`Before close: ${beforeClose} quads, After reopen: ${afterReopen} quads`);

  return persisted;
}

/**
 * Check storage quota and usage
 *
 * @returns {Promise<object>} Storage quota information
 */
export async function checkStorageQuota() {
  if (!navigator.storage || !navigator.storage.estimate) {
    console.warn('Storage API not available');
    return { supported: false };
  }

  const estimate = await navigator.storage.estimate();
  const usageInMB = (estimate.usage / (1024 * 1024)).toFixed(2);
  const quotaInMB = (estimate.quota / (1024 * 1024)).toFixed(2);
  const percentUsed = ((estimate.usage / estimate.quota) * 100).toFixed(2);

  const info = {
    supported: true,
    usage: estimate.usage,
    quota: estimate.quota,
    usageInMB,
    quotaInMB,
    percentUsed
  };

  console.log('Storage Quota:');
  console.log(`  Usage: ${usageInMB} MB / ${quotaInMB} MB (${percentUsed}%)`);

  return info;
}

/**
 * Delete all data from store
 *
 * @param {IndexedDBStore} store - Store instance
 * @returns {Promise<void>}
 */
export async function clearAllData(store) {
  await store.clear();
  console.log('All data cleared from IndexedDB');
}

/**
 * Main example execution
 */
export async function runExample() {
  console.log('=== IndexedDB Persistence Example ===\n');

  // Create store
  const store = await createIndexedDBStore();
  console.log('✓ IndexedDB store created\n');

  // Check initial storage
  await checkStorageQuota();
  console.log();

  // Add sample data
  await addSampleData(store);
  console.log();

  // Query all quads
  const allQuads = await queryQuads(store);
  console.log('All quads:', allQuads.map(q => q.toString()).join('\n'));
  console.log();

  // Query specific pattern
  const nameQuads = await queryQuads(store, {
    predicate: namedNode('http://xmlns.com/foaf/0.1/name')
  });
  console.log('Name quads:', nameQuads.map(q => q.toString()).join('\n'));
  console.log();

  // Verify persistence
  await verifyPersistence(store);
  console.log();

  // Check storage after operations
  await checkStorageQuota();
  console.log();

  // Cleanup
  await clearAllData(store);
  await store.close();
  console.log('✓ Example complete');
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runExample().catch(console.error);
}
