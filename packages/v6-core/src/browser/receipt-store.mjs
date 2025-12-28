/**
 * Browser Receipt Store - IndexedDB-backed receipt persistence
 *
 * Provides IndexedDB storage for v6 receipts with:
 * - Receipt CRUD operations
 * - Merkle tree persistence
 * - Chain verification
 * - Proof generation/verification
 *
 * @module @unrdf/v6-core/browser/receipt-store
 */

/* global indexedDB, IDBKeyRange */

import { verifyBaseReceipt } from '../receipts/base-receipt.mjs';
import { buildMerkleTree, getProofPath, verifyInclusion } from '../receipts/merkle/tree.mjs';

// =============================================================================
// Constants
// =============================================================================

const DB_NAME = 'unrdf-v6-receipts';
const DB_VERSION = 1;
const RECEIPT_STORE = 'receipts';
const MERKLE_STORE = 'merkle-trees';

// =============================================================================
// IndexedDB Helpers
// =============================================================================

/**
 * Open IndexedDB database
 *
 * @param {string} [dbName] - Database name
 * @returns {Promise<IDBDatabase>}
 * @private
 */
async function openDB(dbName = DB_NAME) {
  return new Promise((resolve, reject) => {
    const request = indexedDB.open(dbName, DB_VERSION);

    request.onerror = () => reject(request.error);
    request.onsuccess = () => resolve(request.result);

    request.onupgradeneeded = (event) => {
      const db = event.target.result;

      // Receipt store
      if (!db.objectStoreNames.contains(RECEIPT_STORE)) {
        const receiptStore = db.createObjectStore(RECEIPT_STORE, { keyPath: 'id' });
        receiptStore.createIndex('receiptType', 'receiptType', { unique: false });
        receiptStore.createIndex('timestamp', 't_ns', { unique: false });
        receiptStore.createIndex('receiptHash', 'receiptHash', { unique: true });
      }

      // Merkle tree store
      if (!db.objectStoreNames.contains(MERKLE_STORE)) {
        const merkleStore = db.createObjectStore(MERKLE_STORE, { keyPath: 'id' });
        merkleStore.createIndex('root', 'root', { unique: true });
        merkleStore.createIndex('timestamp', 'timestamp', { unique: false });
      }
    };
  });
}

/**
 * Get object from store
 *
 * @param {IDBDatabase} db - Database
 * @param {string} storeName - Store name
 * @param {string} key - Object key
 * @returns {Promise<any>}
 * @private
 */
async function getObject(db, storeName, key) {
  return new Promise((resolve, reject) => {
    const tx = db.transaction(storeName, 'readonly');
    const store = tx.objectStore(storeName);
    const request = store.get(key);

    request.onerror = () => reject(request.error);
    request.onsuccess = () => resolve(request.result);
  });
}

/**
 * Put object in store
 *
 * @param {IDBDatabase} db - Database
 * @param {string} storeName - Store name
 * @param {any} object - Object to store
 * @returns {Promise<void>}
 * @private
 */
async function putObject(db, storeName, object) {
  return new Promise((resolve, reject) => {
    const tx = db.transaction(storeName, 'readwrite');
    const store = tx.objectStore(storeName);
    const request = store.put(object);

    request.onerror = () => reject(request.error);
    request.onsuccess = () => resolve();
  });
}

/**
 * Get all objects from store
 *
 * @param {IDBDatabase} db - Database
 * @param {string} storeName - Store name
 * @param {Object} [options] - Query options
 * @returns {Promise<Array>}
 * @private
 */
async function getAllObjects(db, storeName, options = {}) {
  return new Promise((resolve, reject) => {
    const tx = db.transaction(storeName, 'readonly');
    const store = tx.objectStore(storeName);

    let request;
    if (options.index && options.range) {
      const index = store.index(options.index);
      request = index.getAll(options.range);
    } else {
      request = store.getAll();
    }

    request.onerror = () => reject(request.error);
    request.onsuccess = () => resolve(request.result);
  });
}

// =============================================================================
// Browser Receipt Store Class
// =============================================================================

/**
 * Browser Receipt Store
 *
 * IndexedDB-backed storage for v6 receipts.
 *
 * @class
 * @example
 * const store = new BrowserReceiptStore({ dbName: 'my-app-receipts' });
 * await store.init();
 *
 * await store.saveReceipt(receipt);
 * const receipt = await store.getReceipt(receiptId);
 * const allReceipts = await store.getAllReceipts();
 */
export class BrowserReceiptStore {
  /**
   * @param {Object} [options] - Store options
   * @param {string} [options.dbName] - Database name
   */
  constructor(options = {}) {
    this.dbName = options.dbName || DB_NAME;
    this.db = null;
  }

  /**
   * Initialize store (open database)
   *
   * @returns {Promise<void>}
   */
  async init() {
    this.db = await openDB(this.dbName);
  }

  /**
   * Close store
   *
   * @returns {Promise<void>}
   */
  async close() {
    if (this.db) {
      this.db.close();
      this.db = null;
    }
  }

  /**
   * Save receipt
   *
   * @param {Object} receipt - Receipt to save
   * @returns {Promise<void>}
   */
  async saveReceipt(receipt) {
    if (!this.db) {
      throw new Error('Store not initialized. Call init() first.');
    }

    // Verify receipt before saving
    const verification = await verifyBaseReceipt(receipt);
    if (!verification.valid) {
      throw new Error(`Invalid receipt: ${verification.error}`);
    }

    // Serialize bigints to strings for IndexedDB
    const serialized = {
      ...receipt,
      t_ns: receipt.t_ns.toString(),
    };

    await putObject(this.db, RECEIPT_STORE, serialized);
  }

  /**
   * Get receipt by ID
   *
   * @param {string} id - Receipt ID
   * @returns {Promise<Object|null>}
   */
  async getReceipt(id) {
    if (!this.db) {
      throw new Error('Store not initialized. Call init() first.');
    }

    const receipt = await getObject(this.db, RECEIPT_STORE, id);

    if (!receipt) {
      return null;
    }

    // Deserialize bigints from strings
    return {
      ...receipt,
      t_ns: BigInt(receipt.t_ns),
    };
  }

  /**
   * Get all receipts
   *
   * @param {Object} [options] - Query options
   * @param {string} [options.receiptType] - Filter by receipt type
   * @returns {Promise<Array>}
   */
  async getAllReceipts(options = {}) {
    if (!this.db) {
      throw new Error('Store not initialized. Call init() first.');
    }

    const queryOptions = {};
    if (options.receiptType) {
      queryOptions.index = 'receiptType';
      queryOptions.range = IDBKeyRange.only(options.receiptType);
    }

    const receipts = await getAllObjects(this.db, RECEIPT_STORE, queryOptions);

    // Deserialize bigints
    return receipts.map((r) => ({
      ...r,
      t_ns: BigInt(r.t_ns),
    }));
  }

  /**
   * Get receipt by hash
   *
   * @param {string} hash - Receipt hash
   * @returns {Promise<Object|null>}
   */
  async getReceiptByHash(hash) {
    if (!this.db) {
      throw new Error('Store not initialized. Call init() first.');
    }

    return new Promise((resolve, reject) => {
      const tx = this.db.transaction(RECEIPT_STORE, 'readonly');
      const store = tx.objectStore(RECEIPT_STORE);
      const index = store.index('receiptHash');
      const request = index.get(hash);

      request.onerror = () => reject(request.error);
      request.onsuccess = () => {
        const receipt = request.result;
        if (!receipt) {
          resolve(null);
        } else {
          resolve({
            ...receipt,
            t_ns: BigInt(receipt.t_ns),
          });
        }
      };
    });
  }

  /**
   * Build and save merkle tree
   *
   * @param {Array<Object>} receipts - Receipts to include
   * @param {Object} [metadata] - Additional metadata
   * @returns {Promise<Object>} Tree structure
   */
  async buildAndSaveMerkleTree(receipts, metadata = {}) {
    if (!this.db) {
      throw new Error('Store not initialized. Call init() first.');
    }

    // Add hash field to receipts for merkle tree
    const receiptsWithHash = receipts.map((r) => ({
      ...r,
      hash: r.receiptHash,
    }));

    const tree = await buildMerkleTree(receiptsWithHash);

    // Save tree
    const treeRecord = {
      id: `tree-${Date.now()}`,
      root: tree.root,
      leafCount: tree.leafCount,
      depth: tree.depth,
      leaves: tree.leaves,
      levels: tree.levels,
      timestamp: new Date().toISOString(),
      receiptIds: receipts.map((r) => r.id),
      ...metadata,
    };

    await putObject(this.db, MERKLE_STORE, treeRecord);

    return tree;
  }

  /**
   * Get merkle tree by root
   *
   * @param {string} root - Merkle root
   * @returns {Promise<Object|null>}
   */
  async getMerkleTreeByRoot(root) {
    if (!this.db) {
      throw new Error('Store not initialized. Call init() first.');
    }

    return new Promise((resolve, reject) => {
      const tx = this.db.transaction(MERKLE_STORE, 'readonly');
      const store = tx.objectStore(MERKLE_STORE);
      const index = store.index('root');
      const request = index.get(root);

      request.onerror = () => reject(request.error);
      request.onsuccess = () => resolve(request.result);
    });
  }

  /**
   * Generate proof for receipt
   *
   * @param {string} receiptId - Receipt ID
   * @param {Object} tree - Merkle tree structure
   * @returns {Promise<Object>} Merkle proof
   */
  async generateProof(receiptId, tree) {
    if (!this.db) {
      throw new Error('Store not initialized. Call init() first.');
    }

    // Get receipts for tree
    const treeRecord = await getObject(this.db, MERKLE_STORE, tree.id);
    if (!treeRecord) {
      throw new Error(`Merkle tree ${tree.id} not found`);
    }

    const receipts = await Promise.all(
      treeRecord.receiptIds.map((id) => this.getReceipt(id))
    );

    // Add hash field
    const receiptsWithHash = receipts.map((r) => ({
      ...r,
      hash: r.receiptHash,
    }));

    return getProofPath(tree, receiptId, receiptsWithHash);
  }

  /**
   * Verify receipt inclusion in merkle tree
   *
   * @param {string} root - Merkle root
   * @param {Object} receipt - Receipt to verify
   * @param {Object} proof - Merkle proof
   * @returns {Promise<boolean>}
   */
  async verifyReceiptInclusion(root, receipt, proof) {
    const receiptWithHash = {
      ...receipt,
      hash: receipt.receiptHash,
    };

    return verifyInclusion(root, receiptWithHash, proof);
  }

  /**
   * Get storage statistics
   *
   * @returns {Promise<Object>}
   */
  async getStats() {
    if (!this.db) {
      throw new Error('Store not initialized. Call init() first.');
    }

    const receipts = await getAllObjects(this.db, RECEIPT_STORE);
    const trees = await getAllObjects(this.db, MERKLE_STORE);

    return {
      receiptCount: receipts.length,
      merkleTreeCount: trees.length,
      receiptTypes: receipts.reduce((acc, r) => {
        acc[r.receiptType] = (acc[r.receiptType] || 0) + 1;
        return acc;
      }, {}),
    };
  }

  /**
   * Clear all data
   *
   * @returns {Promise<void>}
   */
  async clear() {
    if (!this.db) {
      throw new Error('Store not initialized. Call init() first.');
    }

    await new Promise((resolve, reject) => {
      const tx = this.db.transaction([RECEIPT_STORE, MERKLE_STORE], 'readwrite');
      tx.objectStore(RECEIPT_STORE).clear();
      tx.objectStore(MERKLE_STORE).clear();
      tx.onerror = () => reject(tx.error);
      tx.oncomplete = () => resolve();
    });
  }
}

/**
 * Create browser receipt store
 *
 * @param {Object} [options] - Store options
 * @returns {BrowserReceiptStore}
 */
export function createBrowserReceiptStore(options = {}) {
  return new BrowserReceiptStore(options);
}
