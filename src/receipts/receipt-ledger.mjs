/**
 * @fileoverview Receipt Ledger - Append-only ledger for receipt storage
 *
 * **Purpose**: Persistent, tamper-evident storage for receipts
 * - Append-only semantics (no updates, no deletes)
 * - Chain verification via beforeHash linkage
 * - Package-scoped ledgers for isolation
 * - Storage abstraction (file system, database, memory)
 *
 * **Design Principles**:
 * 1. Immutability - once written, receipts cannot be modified
 * 2. Verifiability - chain integrity can be cryptographically verified
 * 3. Isolation - each package maintains its own ledger
 * 4. Durability - receipts survive process restarts
 *
 * @module receipts/receipt-ledger
 */

import { blake3 } from 'hash-wasm';
import {
  UniversalReceiptSchema,
  computeReceiptHash,
  verifyReceiptHash,
} from './receipt-standard.mjs';
import { computeMerkleRoot } from './merkle-root.mjs';
import { writeFileSync, readFileSync, existsSync, mkdirSync, appendFileSync } from 'fs';
import { join, dirname } from 'path';

// ============================================================================
// Storage Backends
// ============================================================================

/**
 * Memory storage backend - for testing and ephemeral use
 */
export class MemoryStorage {
  constructor() {
    /** @type {Map<string, object[]>} Package -> receipts */
    this._ledgers = new Map();
  }

  /**
   * Append receipt to package ledger
   * @param {string} pkg - Package name
   * @param {object} receipt - Receipt to append
   */
  append(pkg, receipt) {
    if (!this._ledgers.has(pkg)) {
      this._ledgers.set(pkg, []);
    }
    this._ledgers.get(pkg).push(receipt);
  }

  /**
   * Read all receipts for package
   * @param {string} pkg - Package name
   * @returns {object[]} Receipts
   */
  read(pkg) {
    return this._ledgers.get(pkg) || [];
  }

  /**
   * Get all packages
   * @returns {string[]}
   */
  getPackages() {
    return Array.from(this._ledgers.keys());
  }

  /**
   * Get total receipt count
   * @returns {number}
   */
  getTotalCount() {
    let total = 0;
    for (const receipts of this._ledgers.values()) {
      total += receipts.length;
    }
    return total;
  }

  /**
   * Clear all data
   */
  clear() {
    this._ledgers.clear();
  }
}

/**
 * File system storage backend - for persistent storage
 *
 * Structure:
 *   {baseDir}/
 *     {package}/
 *       ledger.ndjson  (newline-delimited JSON)
 *       index.json     (metadata + chain head)
 */
export class FileStorage {
  /**
   * @param {string} baseDir - Base directory for ledger files
   */
  constructor(baseDir) {
    this._baseDir = baseDir;
    this._ensureDir(baseDir);
  }

  /**
   * Ensure directory exists
   * @param {string} dir
   * @private
   */
  _ensureDir(dir) {
    if (!existsSync(dir)) {
      mkdirSync(dir, { recursive: true });
    }
  }

  /**
   * Get paths for package
   * @param {string} pkg
   * @returns {{ ledgerPath: string, indexPath: string }}
   * @private
   */
  _getPaths(pkg) {
    const safePackage = pkg.replace(/[^a-zA-Z0-9-_@]/g, '_');
    const pkgDir = join(this._baseDir, safePackage);
    this._ensureDir(pkgDir);
    return {
      ledgerPath: join(pkgDir, 'ledger.ndjson'),
      indexPath: join(pkgDir, 'index.json'),
    };
  }

  /**
   * Append receipt to package ledger
   * @param {string} pkg - Package name
   * @param {object} receipt - Receipt to append
   */
  append(pkg, receipt) {
    const { ledgerPath, indexPath } = this._getPaths(pkg);

    // Append to NDJSON file
    const line = JSON.stringify(receipt) + '\n';
    appendFileSync(ledgerPath, line, 'utf-8');

    // Update index
    const index = this._readIndex(indexPath);
    index.count = (index.count || 0) + 1;
    index.headHash = receipt.receiptHash;
    index.lastUpdated = new Date().toISOString();
    writeFileSync(indexPath, JSON.stringify(index, null, 2), 'utf-8');
  }

  /**
   * Read all receipts for package
   * @param {string} pkg - Package name
   * @returns {object[]} Receipts
   */
  read(pkg) {
    const { ledgerPath } = this._getPaths(pkg);

    if (!existsSync(ledgerPath)) {
      return [];
    }

    const content = readFileSync(ledgerPath, 'utf-8');
    const lines = content.trim().split('\n').filter(Boolean);
    return lines.map(line => JSON.parse(line));
  }

  /**
   * Read index file
   * @param {string} indexPath
   * @returns {object}
   * @private
   */
  _readIndex(indexPath) {
    if (!existsSync(indexPath)) {
      return { count: 0, headHash: null, created: new Date().toISOString() };
    }
    return JSON.parse(readFileSync(indexPath, 'utf-8'));
  }

  /**
   * Get all packages
   * @returns {string[]}
   */
  getPackages() {
    if (!existsSync(this._baseDir)) {
      return [];
    }
    const { readdirSync, statSync } = require('fs');
    const entries = readdirSync(this._baseDir);
    return entries.filter(e => statSync(join(this._baseDir, e)).isDirectory());
  }

  /**
   * Get total receipt count
   * @returns {number}
   */
  getTotalCount() {
    let total = 0;
    for (const pkg of this.getPackages()) {
      const { indexPath } = this._getPaths(pkg);
      const index = this._readIndex(indexPath);
      total += index.count || 0;
    }
    return total;
  }
}

// ============================================================================
// Receipt Ledger
// ============================================================================

/**
 * Receipt Ledger - manages receipt chains for packages
 *
 * @example
 * const ledger = new ReceiptLedger({ storage: new MemoryStorage() });
 * await ledger.append('@unrdf/core', receipt);
 * const chain = await ledger.getChain('@unrdf/core');
 */
export class ReceiptLedger {
  /**
   * Create a new ledger
   *
   * @param {object} options - Ledger options
   * @param {MemoryStorage|FileStorage} [options.storage] - Storage backend
   */
  constructor(options = {}) {
    this._storage = options.storage || new MemoryStorage();
    this._chainHeads = new Map(); // Package -> last receipt hash
  }

  /**
   * Append a receipt to the ledger
   *
   * **Validation**:
   * 1. Receipt must have valid schema
   * 2. Receipt hash must match computed hash
   * 3. beforeHash must match chain head (if chain exists)
   *
   * @param {string} pkg - Package name
   * @param {object} receipt - Receipt to append
   * @returns {Promise<{ success: boolean, error?: string }>}
   */
  async append(pkg, receipt) {
    // Validate schema
    const schemaResult = UniversalReceiptSchema.safeParse(receipt);
    if (!schemaResult.success) {
      return {
        success: false,
        error: `Invalid receipt schema: ${schemaResult.error.message}`,
      };
    }

    // Verify hash
    const isValidHash = await verifyReceiptHash(receipt);
    if (!isValidHash) {
      return {
        success: false,
        error: 'Receipt hash verification failed',
      };
    }

    // Verify chain linkage
    const chainHead = this._chainHeads.get(pkg);
    if (chainHead !== undefined && receipt.beforeHash !== chainHead) {
      return {
        success: false,
        error: `Chain linkage broken: expected beforeHash=${chainHead}, got ${receipt.beforeHash}`,
      };
    }

    // First receipt in chain must have null beforeHash
    if (chainHead === undefined && receipt.beforeHash !== null) {
      return {
        success: false,
        error: `First receipt in chain must have beforeHash=null, got ${receipt.beforeHash}`,
      };
    }

    // Append to storage
    this._storage.append(pkg, receipt);

    // Update chain head
    this._chainHeads.set(pkg, receipt.receiptHash);

    return { success: true };
  }

  /**
   * Get all receipts for a package
   *
   * @param {string} pkg - Package name
   * @returns {object[]} Receipts in chronological order
   */
  getChain(pkg) {
    return this._storage.read(pkg);
  }

  /**
   * Get latest receipt for a package
   *
   * @param {string} pkg - Package name
   * @returns {object|undefined}
   */
  getLatest(pkg) {
    const chain = this.getChain(pkg);
    return chain[chain.length - 1];
  }

  /**
   * Get receipt by hash
   *
   * @param {string} pkg - Package name
   * @param {string} hash - Receipt hash
   * @returns {object|undefined}
   */
  getByHash(pkg, hash) {
    const chain = this.getChain(pkg);
    return chain.find(r => r.receiptHash === hash);
  }

  /**
   * Verify chain integrity for a package
   *
   * **Checks**:
   * 1. All receipt hashes are valid
   * 2. Chain linkage is correct (beforeHash -> receiptHash)
   * 3. Epochs are monotonically increasing
   *
   * @param {string} pkg - Package name
   * @returns {Promise<{ valid: boolean, errors: string[], checked: number }>}
   */
  async verifyChain(pkg) {
    const chain = this.getChain(pkg);
    const errors = [];

    for (let i = 0; i < chain.length; i++) {
      const receipt = chain[i];

      // Verify hash
      const isValidHash = await verifyReceiptHash(receipt);
      if (!isValidHash) {
        errors.push(`Receipt ${i} (${receipt.epoch}): invalid hash`);
      }

      // Verify chain linkage
      if (i === 0) {
        if (receipt.beforeHash !== null) {
          errors.push(`Receipt ${i}: first receipt must have beforeHash=null`);
        }
      } else {
        const prevReceipt = chain[i - 1];
        if (receipt.beforeHash !== prevReceipt.receiptHash) {
          errors.push(
            `Receipt ${i}: broken chain (expected ${prevReceipt.receiptHash}, got ${receipt.beforeHash})`
          );
        }
      }

      // Verify epoch ordering
      if (i > 0) {
        const prevEpoch = chain[i - 1].epoch;
        if (receipt.epoch <= prevEpoch) {
          errors.push(`Receipt ${i}: epoch not increasing (${prevEpoch} >= ${receipt.epoch})`);
        }
      }
    }

    return {
      valid: errors.length === 0,
      errors,
      checked: chain.length,
    };
  }

  /**
   * Verify all chains
   *
   * @returns {Promise<Map<string, { valid: boolean, errors: string[] }>>}
   */
  async verifyAllChains() {
    const results = new Map();

    for (const pkg of this.getPackages()) {
      const result = await this.verifyChain(pkg);
      results.set(pkg, result);
    }

    return results;
  }

  /**
   * Compute Merkle root for package chain
   *
   * @param {string} pkg - Package name
   * @returns {Promise<string|null>} Merkle root or null if empty
   */
  async computeChainMerkle(pkg) {
    const chain = this.getChain(pkg);

    if (chain.length === 0) {
      return null;
    }

    const hashes = chain.map(r => r.receiptHash);
    return computeMerkleRoot(hashes);
  }

  /**
   * Get all packages with receipts
   *
   * @returns {string[]}
   */
  getPackages() {
    return this._storage.getPackages();
  }

  /**
   * Get chain statistics for a package
   *
   * @param {string} pkg - Package name
   * @returns {object}
   */
  getChainStats(pkg) {
    const chain = this.getChain(pkg);

    if (chain.length === 0) {
      return {
        count: 0,
        firstEpoch: null,
        lastEpoch: null,
        decisions: {},
        types: {},
      };
    }

    const decisions = {};
    const types = {};

    for (const receipt of chain) {
      decisions[receipt.decision] = (decisions[receipt.decision] || 0) + 1;
      types[receipt.type] = (types[receipt.type] || 0) + 1;
    }

    return {
      count: chain.length,
      firstEpoch: chain[0].epoch,
      lastEpoch: chain[chain.length - 1].epoch,
      headHash: chain[chain.length - 1].receiptHash,
      decisions,
      types,
    };
  }

  /**
   * Get global ledger statistics
   *
   * @returns {object}
   */
  getGlobalStats() {
    const packages = this.getPackages();
    let totalReceipts = 0;
    const packageStats = {};

    for (const pkg of packages) {
      const stats = this.getChainStats(pkg);
      packageStats[pkg] = stats;
      totalReceipts += stats.count;
    }

    return {
      packageCount: packages.length,
      totalReceipts,
      packages: packageStats,
    };
  }

  /**
   * Export package chain to JSON-LD
   *
   * @param {string} pkg - Package name
   * @returns {object} JSON-LD representation
   */
  toJSONLD(pkg) {
    const chain = this.getChain(pkg);

    return {
      '@context': {
        unrdf: 'https://unrdf.org/receipts#',
        rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
        xsd: 'http://www.w3.org/2001/XMLSchema#',
      },
      '@type': 'unrdf:ReceiptLedger',
      'unrdf:package': pkg,
      'unrdf:count': chain.length,
      'unrdf:receipts': chain.map(r => ({
        '@id': r.id,
        '@type': `unrdf:${r.type.charAt(0).toUpperCase() + r.type.slice(1)}Receipt`,
        'unrdf:epoch': r.epoch,
        'unrdf:decision': r.decision,
        'unrdf:receiptHash': r.receiptHash,
      })),
    };
  }

  /**
   * Export entire ledger to JSON
   *
   * @returns {object}
   */
  exportAll() {
    const packages = {};

    for (const pkg of this.getPackages()) {
      packages[pkg] = this.getChain(pkg);
    }

    return {
      version: '1.0.0',
      exportedAt: new Date().toISOString(),
      packages,
    };
  }

  /**
   * Import ledger from JSON export
   *
   * @param {object} data - Exported ledger data
   * @returns {Promise<{ imported: number, errors: string[] }>}
   */
  async importAll(data) {
    let imported = 0;
    const errors = [];

    for (const [pkg, receipts] of Object.entries(data.packages)) {
      for (const receipt of receipts) {
        const result = await this.append(pkg, receipt);
        if (result.success) {
          imported++;
        } else {
          errors.push(`${pkg}/${receipt.id}: ${result.error}`);
        }
      }
    }

    return { imported, errors };
  }

  /**
   * Initialize chain heads from storage
   * (Call this after loading a FileStorage with existing data)
   */
  initializeChainHeads() {
    for (const pkg of this._storage.getPackages()) {
      const chain = this._storage.read(pkg);
      if (chain.length > 0) {
        const lastReceipt = chain[chain.length - 1];
        this._chainHeads.set(pkg, lastReceipt.receiptHash);
      }
    }
  }
}

// ============================================================================
// Ledger Factory
// ============================================================================

/**
 * Create a memory-backed ledger (for testing)
 *
 * @returns {ReceiptLedger}
 */
export function createMemoryLedger() {
  return new ReceiptLedger({ storage: new MemoryStorage() });
}

/**
 * Create a file-backed ledger (for production)
 *
 * @param {string} baseDir - Base directory for ledger files
 * @returns {ReceiptLedger}
 */
export function createFileLedger(baseDir) {
  const ledger = new ReceiptLedger({ storage: new FileStorage(baseDir) });
  ledger.initializeChainHeads();
  return ledger;
}

// ============================================================================
// Chain Snapshot
// ============================================================================

/**
 * Create a cryptographic snapshot of chain state
 *
 * @param {ReceiptLedger} ledger - Ledger instance
 * @param {string} pkg - Package name
 * @returns {Promise<object>} Snapshot with merkle proofs
 */
export async function createChainSnapshot(ledger, pkg) {
  const chain = ledger.getChain(pkg);

  if (chain.length === 0) {
    return {
      package: pkg,
      count: 0,
      merkleRoot: null,
      timestamp: new Date().toISOString(),
    };
  }

  const hashes = chain.map(r => r.receiptHash);
  const merkleRoot = await computeMerkleRoot(hashes);

  return {
    package: pkg,
    count: chain.length,
    headHash: chain[chain.length - 1].receiptHash,
    merkleRoot,
    firstEpoch: chain[0].epoch,
    lastEpoch: chain[chain.length - 1].epoch,
    timestamp: new Date().toISOString(),
  };
}

/**
 * Verify a chain snapshot against current state
 *
 * @param {ReceiptLedger} ledger - Ledger instance
 * @param {object} snapshot - Snapshot to verify
 * @returns {Promise<{ valid: boolean, error?: string }>}
 */
export async function verifyChainSnapshot(ledger, snapshot) {
  const chain = ledger.getChain(snapshot.package);

  // Verify count
  if (chain.length !== snapshot.count) {
    return {
      valid: false,
      error: `Count mismatch: expected ${snapshot.count}, got ${chain.length}`,
    };
  }

  if (chain.length === 0 && snapshot.merkleRoot === null) {
    return { valid: true };
  }

  // Verify head hash
  if (chain[chain.length - 1].receiptHash !== snapshot.headHash) {
    return {
      valid: false,
      error: 'Head hash mismatch',
    };
  }

  // Verify merkle root
  const hashes = chain.map(r => r.receiptHash);
  const computedRoot = await computeMerkleRoot(hashes);

  if (computedRoot !== snapshot.merkleRoot) {
    return {
      valid: false,
      error: 'Merkle root mismatch',
    };
  }

  return { valid: true };
}
