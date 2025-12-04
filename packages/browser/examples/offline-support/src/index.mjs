/**
 * @file Offline support example for @unrdf/browser
 * @module @unrdf/example-offline-support
 */

import { IndexedDBStore } from '@unrdf/browser';
import { quad, namedNode, literal } from '@unrdf/core';

/**
 * Offline queue for pending operations
 */
class OfflineQueue {
  constructor() {
    this.queue = [];
  }

  /**
   * Add operation to queue
   *
   * @param {string} type - Operation type (add, remove, update)
   * @param {object} data - Operation data
   */
  enqueue(type, data) {
    this.queue.push({
      type,
      data,
      timestamp: Date.now()
    });
    console.log(`Queued ${type} operation (queue size: ${this.queue.length})`);
  }

  /**
   * Get all queued operations
   *
   * @returns {Array} Queued operations
   */
  getAll() {
    return [...this.queue];
  }

  /**
   * Clear all operations
   */
  clear() {
    const count = this.queue.length;
    this.queue = [];
    console.log(`Cleared ${count} queued operations`);
  }

  /**
   * Get queue size
   *
   * @returns {number} Number of queued operations
   */
  size() {
    return this.queue.length;
  }
}

/**
 * Offline-capable RDF store
 */
export class OfflineRDFStore {
  /**
   * @param {object} config - Configuration options
   */
  constructor(config = {}) {
    this.store = new IndexedDBStore(config);
    this.queue = new OfflineQueue();
    this.isOnline = navigator.onLine;
    this.setupNetworkListeners();
  }

  /**
   * Setup network status listeners
   */
  setupNetworkListeners() {
    window.addEventListener('online', () => {
      console.log('Network: ONLINE');
      this.isOnline = true;
      this.syncQueuedOperations();
    });

    window.addEventListener('offline', () => {
      console.log('Network: OFFLINE');
      this.isOnline = false;
    });
  }

  /**
   * Open store connection
   *
   * @returns {Promise<void>}
   */
  async open() {
    await this.store.open();
    console.log('Store opened');
  }

  /**
   * Close store connection
   *
   * @returns {Promise<void>}
   */
  async close() {
    await this.store.close();
    console.log('Store closed');
  }

  /**
   * Add quad to store (with offline support)
   *
   * @param {object} quad - RDF quad to add
   * @returns {Promise<void>}
   */
  async add(quad) {
    if (this.isOnline) {
      await this.store.add(quad);
      console.log('Added quad (online)');
    } else {
      this.queue.enqueue('add', quad);
      console.log('Queued add operation (offline)');
    }
  }

  /**
   * Remove quad from store (with offline support)
   *
   * @param {object} quad - RDF quad to remove
   * @returns {Promise<void>}
   */
  async remove(quad) {
    if (this.isOnline) {
      await this.store.remove(quad);
      console.log('Removed quad (online)');
    } else {
      this.queue.enqueue('remove', quad);
      console.log('Queued remove operation (offline)');
    }
  }

  /**
   * Query quads (always available)
   *
   * @param {object} pattern - Query pattern
   * @returns {Promise<Array>} Matching quads
   */
  async match(pattern = {}) {
    const results = await this.store.match(
      pattern.subject,
      pattern.predicate,
      pattern.object,
      pattern.graph
    );
    console.log(`Found ${results.length} quads (cached)`);
    return results;
  }

  /**
   * Sync queued operations to store
   *
   * @returns {Promise<object>} Sync results
   */
  async syncQueuedOperations() {
    const operations = this.queue.getAll();

    if (operations.length === 0) {
      console.log('No operations to sync');
      return { synced: 0, failed: 0 };
    }

    console.log(`Syncing ${operations.length} queued operations...`);

    let synced = 0;
    let failed = 0;

    for (const op of operations) {
      try {
        if (op.type === 'add') {
          await this.store.add(op.data);
        } else if (op.type === 'remove') {
          await this.store.remove(op.data);
        }
        synced++;
      } catch (err) {
        console.error(`Failed to sync ${op.type} operation:`, err);
        failed++;
      }
    }

    this.queue.clear();

    const results = { synced, failed };
    console.log(`Sync complete: ${synced} synced, ${failed} failed`);
    return results;
  }

  /**
   * Get current network status
   *
   * @returns {boolean} True if online
   */
  getNetworkStatus() {
    return this.isOnline;
  }

  /**
   * Get queue size
   *
   * @returns {number} Number of queued operations
   */
  getQueueSize() {
    return this.queue.size();
  }

  /**
   * Clear all cached data
   *
   * @returns {Promise<void>}
   */
  async clearCache() {
    await this.store.clear();
    this.queue.clear();
    console.log('Cache cleared');
  }
}

/**
 * Setup offline support for RDF data
 *
 * @param {object} config - Configuration options
 * @returns {Promise<OfflineRDFStore>} Offline-capable store
 */
export async function setupOfflineSupport(config = {}) {
  const store = new OfflineRDFStore({
    name: 'unrdf-offline',
    version: 1,
    storeName: 'quads',
    ...config
  });

  await store.open();
  return store;
}

/**
 * Cache RDF data for offline use
 *
 * @param {OfflineRDFStore} store - Offline store instance
 * @param {Array} quads - Quads to cache
 * @returns {Promise<void>}
 */
export async function cacheRDFData(store, quads) {
  console.log(`Caching ${quads.length} quads for offline use...`);

  for (const q of quads) {
    await store.add(q);
  }

  console.log('Data cached successfully');
}

/**
 * Handle network detection
 *
 * @param {OfflineRDFStore} store - Offline store instance
 * @returns {object} Network status and queue info
 */
export function handleNetworkDetection(store) {
  const isOnline = store.getNetworkStatus();
  const queueSize = store.getQueueSize();

  const status = {
    online: isOnline,
    queueSize,
    message: isOnline
      ? `Online - ${queueSize} operations pending sync`
      : `Offline - ${queueSize} operations queued`
  };

  console.log(status.message);
  return status;
}

/**
 * Main example execution
 */
export async function runExample() {
  console.log('=== Offline Support Example ===\n');

  // Setup offline store
  const store = await setupOfflineSupport();
  console.log('✓ Offline store initialized\n');

  // Check initial network status
  handleNetworkDetection(store);
  console.log();

  // Cache sample data
  const sampleQuads = [
    quad(
      namedNode('http://example.org/user/1'),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal('Alice')
    ),
    quad(
      namedNode('http://example.org/user/2'),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal('Bob')
    )
  ];

  await cacheRDFData(store, sampleQuads);
  console.log();

  // Simulate offline operation
  console.log('Simulating offline mode...');
  store.isOnline = false;

  await store.add(quad(
    namedNode('http://example.org/user/3'),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal('Charlie')
  ));

  handleNetworkDetection(store);
  console.log();

  // Simulate coming back online
  console.log('Simulating online mode...');
  store.isOnline = true;
  await store.syncQueuedOperations();
  console.log();

  // Query all cached data
  const allQuads = await store.match();
  console.log('Cached quads:', allQuads.length);
  console.log();

  // Cleanup
  await store.close();
  console.log('✓ Example complete');
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runExample().catch(console.error);
}
