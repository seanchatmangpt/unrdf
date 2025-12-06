/**
 * @fileoverview Browser-compatible lockchain writer using IndexedDB
 *
 * Implements cryptographic audit trails in browser environment:
 * - Stores commits in IndexedDB instead of file system
 * - Uses Web Crypto API for hashing
 * - Maintains same security guarantees as Node.js version
 *
 * @module browser/browser-lockchain-writer
 */

/* global indexedDB */

import { IndexedDBFileSystem } from './indexeddb-fs.mjs';
import { _crypto } from './browser-shim.mjs';

const LOCKCHAIN_DB = 'unrdf-lockchain';
const COMMITS_STORE = 'commits';
const METADATA_STORE = 'metadata';

/**
 * Browser-compatible lockchain writer
 *
 * @example
 * const writer = new BrowserLockchainWriter();
 * await writer.init();
 *
 * await writer.recordChange({
 *   type: 'add',
 *   data: turtleData,
 *   author: 'alice@example.org',
 *   message: 'Added new entities',
 * });
 */
export class BrowserLockchainWriter {
  /**
   *
   */
  constructor(options = {}) {
    this.db = null;
    this.fs = new IndexedDBFileSystem();
    this.branch = options.branch || 'main';
    this.initialized = false;
  }

  /**
   * Initialize lockchain database
   * @returns {Promise<void>}
   */
  async init() {
    if (this.initialized) return;

    return new Promise((resolve, reject) => {
      const request = indexedDB.open(LOCKCHAIN_DB, 1);

      request.onerror = () => reject(request.error);
      request.onsuccess = () => {
        this.db = request.result;
        this.initialized = true;
        resolve();
      };

      request.onupgradeneeded = event => {
        const db = event.target.result;

        // Commits store
        if (!db.objectStoreNames.contains(COMMITS_STORE)) {
          const commitsStore = db.createObjectStore(COMMITS_STORE, {
            keyPath: 'hash',
          });
          commitsStore.createIndex('timestamp', 'timestamp', { unique: false });
          commitsStore.createIndex('author', 'author', { unique: false });
          commitsStore.createIndex('parent', 'parentHash', { unique: false });
        }

        // Metadata store (HEAD, branches, etc)
        if (!db.objectStoreNames.contains(METADATA_STORE)) {
          db.createObjectStore(METADATA_STORE, { keyPath: 'key' });
        }
      };
    });
  }

  /**
   * Ensure database is initialized
   * @private
   */
  async ensureInit() {
    if (!this.initialized) {
      await this.init();
    }
  }

  /**
   * Compute SHA-256 hash of data
   * @private
   * @param {string} data - Data to hash
   * @returns {Promise<string>} Hex hash
   */
  async hash(data) {
    const encoder = new TextEncoder();
    const dataBytes = encoder.encode(data);
    const hashBuffer = await globalThis.crypto.subtle.digest('SHA-256', dataBytes);
    const hashArray = new Uint8Array(hashBuffer);

    return Array.from(hashArray)
      .map(b => b.toString(16).padStart(2, '0'))
      .join('');
  }

  /**
   * Get current HEAD commit hash
   * @private
   * @returns {Promise<string|null>} HEAD commit hash
   */
  async getHead() {
    await this.ensureInit();

    return new Promise((resolve, reject) => {
      const transaction = this.db.transaction([METADATA_STORE], 'readonly');
      const request = transaction.objectStore(METADATA_STORE).get(`HEAD:${this.branch}`);

      request.onsuccess = () => {
        const record = request.result;
        resolve(record ? record.value : null);
      };

      request.onerror = () => reject(request.error);
    });
  }

  /**
   * Update HEAD to point to new commit
   * @private
   * @param {string} commitHash - New commit hash
   * @returns {Promise<void>}
   */
  async updateHead(commitHash) {
    await this.ensureInit();

    return new Promise((resolve, reject) => {
      const transaction = this.db.transaction([METADATA_STORE], 'readwrite');
      const store = transaction.objectStore(METADATA_STORE);

      store.put({
        key: `HEAD:${this.branch}`,
        value: commitHash,
        timestamp: Date.now(),
      });

      transaction.oncomplete = () => resolve();
      transaction.onerror = () => reject(transaction.error);
    });
  }

  /**
   * Record change to lockchain
   * @param {Object} change - Change record
   * @param {string} change.type - Change type (add, remove, modify)
   * @param {string} change.data - Changed data
   * @param {string} [change.author] - Author identity
   * @param {string} [change.message] - Commit message
   * @returns {Promise<string>} Commit hash
   */
  async recordChange(change) {
    await this.ensureInit();

    const timestamp = Date.now();
    const parentHash = await this.getHead();

    // Create commit object
    const commit = {
      type: change.type,
      data: change.data,
      author: change.author || 'anonymous',
      message: change.message || `${change.type} operation`,
      timestamp,
      parentHash,
      branch: this.branch,
    };

    // Compute commit hash
    const commitString = JSON.stringify(commit);
    const commitHash = await this.hash(commitString);

    commit.hash = commitHash;

    // Store commit
    await this.storeCommit(commit);

    // Update HEAD
    await this.updateHead(commitHash);

    return commitHash;
  }

  /**
   * Store commit in IndexedDB
   * @private
   * @param {Object} commit - Commit object
   * @returns {Promise<void>}
   */
  async storeCommit(commit) {
    await this.ensureInit();

    return new Promise((resolve, reject) => {
      const transaction = this.db.transaction([COMMITS_STORE], 'readwrite');
      transaction.objectStore(COMMITS_STORE).put(commit);

      transaction.oncomplete = () => resolve();
      transaction.onerror = () => reject(transaction.error);
    });
  }

  /**
   * Get commit by hash
   * @param {string} commitHash - Commit hash
   * @returns {Promise<Object|null>} Commit object
   */
  async getCommit(commitHash) {
    await this.ensureInit();

    return new Promise((resolve, reject) => {
      const transaction = this.db.transaction([COMMITS_STORE], 'readonly');
      const request = transaction.objectStore(COMMITS_STORE).get(commitHash);

      request.onsuccess = () => resolve(request.result || null);
      request.onerror = () => reject(request.error);
    });
  }

  /**
   * Get commit history
   * @param {number} [limit=50] - Maximum commits to retrieve
   * @returns {Promise<Array<Object>>} Commit history
   */
  async getHistory(limit = 50) {
    await this.ensureInit();

    return new Promise((resolve, reject) => {
      const transaction = this.db.transaction([COMMITS_STORE], 'readonly');
      const store = transaction.objectStore(COMMITS_STORE);
      const index = store.index('timestamp');
      const request = index.openCursor(null, 'prev'); // Descending order

      const commits = [];
      let count = 0;

      request.onsuccess = event => {
        const cursor = event.target.result;

        if (cursor && count < limit) {
          commits.push(cursor.value);
          count++;
          cursor.continue();
        } else {
          resolve(commits);
        }
      };

      request.onerror = () => reject(request.error);
    });
  }

  /**
   * Verify commit chain integrity
   * @param {string} [fromHash] - Starting commit hash (defaults to HEAD)
   * @returns {Promise<Object>} Verification result
   */
  async verifyChain(fromHash = null) {
    await this.ensureInit();

    const startHash = fromHash || (await this.getHead());
    if (!startHash) {
      return {
        valid: true,
        message: 'Empty chain',
        commits: 0,
      };
    }

    let currentHash = startHash;
    let commits = 0;
    const errors = [];

    while (currentHash) {
      const commit = await this.getCommit(currentHash);

      if (!commit) {
        errors.push(`Missing commit: ${currentHash}`);
        break;
      }

      // Verify commit hash
      const commitCopy = { ...commit };
      delete commitCopy.hash;
      const expectedHash = await this.hash(JSON.stringify(commitCopy));

      if (expectedHash !== commit.hash) {
        errors.push(`Invalid hash for commit ${commit.hash}`);
      }

      commits++;
      currentHash = commit.parentHash;

      // Prevent infinite loops
      if (commits > 10000) {
        errors.push('Chain too long (possible cycle)');
        break;
      }
    }

    return {
      valid: errors.length === 0,
      message: errors.length > 0 ? errors.join(', ') : 'Chain is valid',
      commits,
      errors,
    };
  }

  /**
   * Get commits by author
   * @param {string} author - Author identity
   * @returns {Promise<Array<Object>>} Commits by author
   */
  async getCommitsByAuthor(author) {
    await this.ensureInit();

    return new Promise((resolve, reject) => {
      const transaction = this.db.transaction([COMMITS_STORE], 'readonly');
      const index = transaction.objectStore(COMMITS_STORE).index('author');
      const request = index.getAll(author);

      request.onsuccess = () => resolve(request.result);
      request.onerror = () => reject(request.error);
    });
  }

  /**
   * Export lockchain as JSON
   * @returns {Promise<Object>} Lockchain data
   */
  async export() {
    await this.ensureInit();

    const commits = await this.getHistory(Infinity);
    const head = await this.getHead();

    return {
      version: '1.0',
      branch: this.branch,
      head,
      commits,
      exportedAt: new Date().toISOString(),
    };
  }

  /**
   * Import lockchain from JSON
   * @param {Object} data - Lockchain data
   * @returns {Promise<void>}
   */
  async import(data) {
    await this.ensureInit();

    return new Promise((resolve, reject) => {
      const transaction = this.db.transaction([COMMITS_STORE, METADATA_STORE], 'readwrite');
      const commitsStore = transaction.objectStore(COMMITS_STORE);
      const metaStore = transaction.objectStore(METADATA_STORE);

      // Import commits
      for (const commit of data.commits) {
        commitsStore.put(commit);
      }

      // Import HEAD
      if (data.head) {
        metaStore.put({
          key: `HEAD:${data.branch || this.branch}`,
          value: data.head,
          timestamp: Date.now(),
        });
      }

      transaction.oncomplete = () => resolve();
      transaction.onerror = () => reject(transaction.error);
    });
  }

  /**
   * Clear all lockchain data
   * @returns {Promise<void>}
   */
  async clear() {
    await this.ensureInit();

    return new Promise((resolve, reject) => {
      const transaction = this.db.transaction([COMMITS_STORE, METADATA_STORE], 'readwrite');

      transaction.objectStore(COMMITS_STORE).clear();
      transaction.objectStore(METADATA_STORE).clear();

      transaction.oncomplete = () => resolve();
      transaction.onerror = () => reject(transaction.error);
    });
  }

  /**
   * Close database connection
   */
  close() {
    if (this.db) {
      this.db.close();
      this.db = null;
      this.initialized = false;
    }
  }
}

/**
 * Create browser lockchain writer
 * @param {Object} [options] - Options
 * @returns {Promise<BrowserLockchainWriter>} Lockchain writer
 */
export async function createBrowserLockchainWriter(options) {
  const writer = new BrowserLockchainWriter(options);
  await writer.init();
  return writer;
}
