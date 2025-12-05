/**
 * @file Browser-compatible Lockchain Writer
 * @module lockchain-writer-browser
 *
 * @description
 * Browser-compatible version of the lockchain writer that stores entries in memory
 * or browser storage instead of Git commits. Provides cryptographic integrity
 * verification without Git dependencies.
 */

/* global localStorage, sessionStorage */

import { randomUUID, createHash, _fs } from './browser-shims.mjs';
import { sha3_256 } from '@noble/hashes/sha3.js';
import { _blake3 } from '@noble/hashes/blake3.js';
import { utf8ToBytes, bytesToHex } from '@noble/hashes/utils.js';
import { z } from 'zod';

/**
 * Schema for lockchain entry
 */
const LockchainEntrySchema = z.object({
  id: z.string().uuid(),
  timestamp: z.number(),
  receipt: z.any(), // Transaction receipt
  signature: z.object({
    algorithm: z.string(),
    value: z.string(),
    publicKey: z.string().optional(),
  }),
  previousHash: z.string().optional().nullable(),
  merkleRoot: z.string().optional(),
  storageKey: z.string().optional(), // Browser storage key instead of git commit
  storageRef: z.string().optional(),
});

/**
 * Schema for lockchain configuration
 */
const LockchainConfigSchema = z.object({
  storageType: z.enum(['memory', 'localStorage', 'sessionStorage', 'indexedDB']).default('memory'),
  algorithm: z.enum(['sha256', 'sha3-256', 'blake3']).default('sha3-256'),
  batchSize: z.number().int().positive().default(10),
  enableMerkle: z.boolean().default(true),
  enablePersistence: z.boolean().default(true),
  storagePrefix: z.string().default('lockchain'),
  maxAgeMs: z.number().int().positive().optional(), // Time to live
});

/**
 * Browser-compatible Lockchain Writer
 */
export class BrowserLockchainWriter {
  /**
   *
   */
  constructor(config = {}) {
    const validatedConfig = LockchainConfigSchema.parse({
      ...config,
      // Override Git-specific options for browser
      enableGitAnchoring: false,
      gitRepo: undefined,
    });
    this.config = validatedConfig;

    // Initialize storage
    this.entries = [];
    this.lastHash = null;
    this.processingBatches = new Set();

    // Browser storage API
    this.storage = this._initStorage();

    // Load existing entries
    this._loadEntries().catch(err => {
      console.warn('Failed to load entries during initialization:', err.message);
    });
  }

  /**
   * Initialize browser storage according to config
   * @private
   */
  _initStorage() {
    switch (this.config.storageType) {
      case 'localStorage':
        return {
          get: key => localStorage.getItem(`${this.config.storagePrefix}_${key}`),
          set: (key, value) => localStorage.setItem(`${this.config.storagePrefix}_${key}`, value),
          remove: key => localStorage.removeItem(`${this.config.storagePrefix}_${key}`),
        };
      case 'sessionStorage':
        return {
          get: key => sessionStorage.getItem(`${this.config.storagePrefix}_${key}`),
          set: (key, value) => sessionStorage.setItem(`${this.config.storagePrefix}_${key}`, value),
          remove: key => sessionStorage.removeItem(`${this.config.storagePrefix}_${key}`),
        };
      case 'indexedDB':
        // Simplified IndexedDB implementation would go here
        console.warn('IndexedDB storage not yet implemented, falling back to memory');
        return this._initMemoryStorage();
      default:
        return this._initMemoryStorage();
    }
  }

  /**
   * Initialize memory-based storage
   * @private
   */
  _initMemoryStorage() {
    const memoryStorage = new Map();
    return {
      get: key => memoryStorage.get(key),
      set: (key, value) => memoryStorage.set(key, value),
      remove: key => memoryStorage.delete(key),
    };
  }

  /**
   * Load existing entries from storage
   * @private
   */
  async _loadEntries() {
    try {
      const entriesKey = `${this.config.storagePrefix}_entries`;
      const entriesData = this.storage.get(entriesKey);

      if (entriesData) {
        this.entries = JSON.parse(entriesData);

        // Restore last hash
        const lastEntry = this.entries[this.entries.length - 1];
        if (lastEntry) {
          this.lastHash = await this._calculateEntryHash(lastEntry);
        }
      }
    } catch (error) {
      console.warn('Failed to load existing entries:', error.message);
      this.entries = [];
      this.lastHash = null;
    }
  }

  /**
   * Save entries to storage
   * @private
   */
  async _saveEntries() {
    try {
      const entriesKey = `${this.config.storagePrefix}_entries`;
      this.storage.set(entriesKey, JSON.stringify(this.entries));
    } catch (error) {
      console.warn('Failed to save entries:', error.message);
    }
  }

  /**
   * Write a receipt to the lockchain
   * @param {Object} receipt - Transaction receipt
   * @param {Object} [options] - Write options
   * @returns {Promise<Object>} Lockchain entry
   */
  async writeReceipt(receipt, options = {}) {
    const entryId = randomUUID();
    const timestamp = Date.now();

    // Create lockchain entry
    const entry = {
      id: entryId,
      timestamp,
      receipt: this._serializeReceipt(receipt),
      signature: await this._signEntry(receipt, entryId, timestamp),
      previousHash: this.lastHash || null,
      merkleRoot: options.merkleRoot,
    };

    // Validate entry
    const validatedEntry = LockchainEntrySchema.parse(entry);

    // Store entry
    await this._storeEntry(validatedEntry);

    // Update hash chain
    this.lastHash = await this._calculateEntryHash(validatedEntry);

    // Persist to storage
    await this._saveEntries();

    return validatedEntry;
  }

  /**
   * Write multiple receipts in a batch
   * @param {Array} receipts - Array of transaction receipts
   * @param {Object} [options] - Batch options
   * @returns {Promise<Array>} Array of lockchain entries
   */
  async writeReceiptBatch(receipts, _options = {}) {
    const batchId = `batch_${randomUUID().slice(0, 8)}`;
    const validatedReceipts = receipts.map(r => (typeof r === 'string' ? JSON.parse(r) : r));

    const batchPromise = this._processBatch(validatedReceipts, batchId);
    this.processingBatches.add(batchId);

    try {
      const entries = await batchPromise;
      this.processingBatches.delete(batchId);
      return entries;
    } catch (error) {
      this.processingBatches.delete(batchId);
      throw error;
    }
  }

  /**
   * Process a batch of receipts
   * @private
   */
  async _processBatch(receipts, batchId) {
    const entries = [];
    const merkleRoot = this.config.enableMerkle ? await this._calculateMerkleRoot(receipts) : null;

    for (let i = 0; i < receipts.length; i++) {
      const receipt = receipts[i];
      const entry = await this.writeReceipt(receipt, {
        batchId,
        batchIndex: i,
        merkleRoot,
      });
      entries.push(entry);
    }

    return entries;
  }

  /**
   * Calculate Merkle root from receipts
   * @private
   */
  async _calculateMerkleRoot(receipts) {
    const hashes = await Promise.all(
      receipts.map(async receipt => {
        const serialized = this._serializeReceipt(receipt);
        return bytesToHex(sha3_256(utf8ToBytes(JSON.stringify(serialized))));
      })
    );

    // Simple binary tree merkle calculation
    let current = hashes;
    while (current.length > 1) {
      const next = [];
      for (let i = 0; i < current.length; i += 2) {
        const left = current[i];
        const right = current[i + 1] || current[i]; // Pad with self if odd
        const combined = left + right;
        next.push(bytesToHex(sha3_256(utf8ToBytes(combined))));
      }
      current = next;
    }

    return current[0];
  }

  /**
   * Store a single entry
   * @private
   */
  async _storeEntry(entry) {
    // Store individual entry
    const storageKey = `entry_${entry.id}`;
    this.storage.set(storageKey, JSON.stringify(entry));

    // Update entries array
    this.entries.push({
      ...entry,
      storageKey,
    });
  }

  /**
   * Serialize receipt for storage
   * @private
   */
  _serializeReceipt(receipt) {
    if (typeof receipt === 'string') {
      return JSON.parse(receipt);
    }
    return receipt;
  }

  /**
   * Sign an entry
   * @private
   */
  async _signEntry(receipt, entryId, timestamp) {
    const hash = createHash(this.config.algorithm);
    const signature = hash.update(
      JSON.stringify({
        receipt,
        entryId,
        timestamp,
        previousHash: this.lastHash,
      })
    );

    return {
      algorithm: this.config.algorithm,
      value: await signature.digest('hex'),
    };
  }

  /**
   * Calculate hash for an entry
   * @private
   */
  async _calculateEntryHash(entry) {
    const hash = createHash(this.config.algorithm);
    return await hash.update(JSON.stringify(entry)).digest('hex');
  }

  /**
   * Verify lockchain integrity
   * @returns {Promise<Object>} Verification result
   */
  async verifyIntegrity() {
    const results = [];
    let previousHash = null;

    for (let i = 0; i < this.entries.length; i++) {
      const entry = this.entries[i];
      const expectedHash = await this._calculateEntryHash(entry);

      const isValid =
        expectedHash === entry.previousHash &&
        (previousHash === null || entry.previousHash === previousHash);

      results.push({
        index: i,
        entryId: entry.id,
        valid: isValid,
        hash: expectedHash,
      });

      if (!isValid) {
        break; // Chain is broken
      }

      previousHash = expectedHash;
    }

    const isValid = results.every(r => r.valid);

    return {
      valid: isValid,
      totalEntries: this.entries.length,
      results,
      verificationTimestamp: Date.now(),
    };
  }

  /**
   * Get lockchain statistics
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      totalEntries: this.entries.length,
      processingBatches: this.processingBatches.size,
      config: this.config,
      lastHash: this.lastHash,
      storageType: this.config.storageType,
      oldestEntry: this.entries.length > 0 ? this.entries[0].timestamp : null,
      newestEntry: this.entries.length > 0 ? this.entries[this.entries.length - 1].timestamp : null,
    };
  }

  /**
   * Clear all entries (use with caution)
   */
  async clear() {
    this.entries = [];
    this.lastHash = null;
    this.processingBatches.clear();

    if (this.config.enablePersistence) {
      // Clear persisted entries
      this.entries.forEach(entry => {
        if (entry.storageKey) {
          this.storage.remove(entry.storageKey);
        }
      });

      const entriesKey = `${this.config.storagePrefix}_entries`;
      this.storage.remove(entriesKey);
    }
  }
}

/**
 * Create a browser-compatible lockchain writer
 * @param {Object} [config] - Lockchain configuration
 * @returns {BrowserLockchainWriter} New writer instance
 */
export function createBrowserLockchainWriter(config = {}) {
  return new BrowserLockchainWriter(config);
}

// Export for compatibility
export { BrowserLockchainWriter as LockchainWriter };

export default BrowserLockchainWriter;
