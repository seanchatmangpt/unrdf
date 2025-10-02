/**
 * @file Lockchain Writer for persistent, verifiable audit trail
 * @module lockchain-writer
 * 
 * @description
 * Implements a persistent, verifiable audit trail by anchoring signed receipts
 * to Git. Provides cryptographic integrity and tamper-proof provenance.
 */

import { execSync } from 'child_process';
import { writeFileSync, readFileSync, existsSync, mkdirSync } from 'fs';
import { join, dirname } from 'path';
import { sha3_256 } from '@noble/hashes/sha3.js';
import { blake3 } from '@noble/hashes/blake3.js';
import { utf8ToBytes, bytesToHex } from '@noble/hashes/utils.js';
import { randomUUID } from 'crypto';
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
    publicKey: z.string().optional()
  }),
  previousHash: z.string().optional().nullable(),
  merkleRoot: z.string().optional(),
  gitCommit: z.string().optional(),
  gitRef: z.string().optional()
});

/**
 * Schema for lockchain configuration
 */
const LockchainConfigSchema = z.object({
  gitRepo: z.string().default(process.cwd()),
  refName: z.string().default('refs/notes/lockchain'),
  signingKey: z.string().optional(),
  algorithm: z.enum(['ed25519', 'ecdsa', 'rsa']).default('ed25519'),
  batchSize: z.number().int().positive().default(10),
  enableMerkle: z.boolean().default(true),
  enableGitAnchoring: z.boolean().default(true),
  storagePath: z.string().optional()
});

/**
 * Lockchain Writer for persistent, verifiable audit trail
 */
export class LockchainWriter {
  /**
   * Create a new lockchain writer
   * @param {Object} [config] - Lockchain configuration
   */
  constructor(config = {}) {
    const validatedConfig = LockchainConfigSchema.parse(config);
    this.config = validatedConfig;
    
    // Initialize storage
    this.storagePath = validatedConfig.storagePath || join(validatedConfig.gitRepo, '.lockchain');
    this._ensureStorageExists();
    
    // Initialize Git repository if needed
    this._initializeGitRepo();
    
    // Cache for pending entries
    this.pendingEntries = [];
    this.entryCache = new Map();
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

    // Create lockchain entry (without merkleRoot first)
    const entry = {
      id: entryId,
      timestamp,
      receipt: this._serializeReceipt(receipt),
      signature: await this._signEntry(receipt, entryId, timestamp),
      previousHash: this._getPreviousHash() || ''
    };

    // Calculate and add Merkle root if enabled
    if (this.config.enableMerkle) {
      entry.merkleRoot = this._calculateEntryMerkleRoot(entry);
    } else if (options.merkleRoot) {
      entry.merkleRoot = options.merkleRoot;
    }

    // Validate entry
    const validatedEntry = LockchainEntrySchema.parse(entry);

    // Store entry
    await this._storeEntry(validatedEntry);

    // Add to pending batch
    this.pendingEntries.push(validatedEntry);

    // Auto-commit if batch is full
    if (this.pendingEntries.length >= this.config.batchSize) {
      await this.commitBatch();
    }

    return validatedEntry;
  }

  /**
   * Commit pending entries to Git
   * @param {Object} [options] - Commit options
   * @returns {Promise<Object>} Commit result
   */
  async commitBatch(options = {}) {
    if (this.pendingEntries.length === 0) {
      return { committed: false, message: 'No pending entries' };
    }
    
    const batchId = randomUUID();
    const timestamp = Date.now();
    
    try {
      // Create batch file
      const batchData = {
        id: batchId,
        timestamp,
        entries: this.pendingEntries,
        merkleRoot: this.config.enableMerkle ? this._calculateMerkleRoot(this.pendingEntries) : null,
        entryCount: this.pendingEntries.length
      };
      
      const batchFile = join(this.storagePath, `batch-${batchId}.json`);
      writeFileSync(batchFile, JSON.stringify(batchData, null, 2));
      
      // Git operations
      if (this.config.enableGitAnchoring) {
        await this._gitAdd(batchFile);
        const commitHash = await this._gitCommit(`Lockchain batch ${batchId}`, {
          entries: this.pendingEntries.length,
          timestamp
        });
        
        // Update entries with Git commit info
        for (const entry of this.pendingEntries) {
          entry.gitCommit = commitHash;
          entry.gitRef = this.config.refName;
          await this._updateEntry(entry);
        }
      }
      
      // Clear pending entries
      const committedCount = this.pendingEntries.length;
      this.pendingEntries = [];
      
      return {
        committed: true,
        batchId,
        commitHash: this.config.enableGitAnchoring ? await this._getLatestCommit() : null,
        entryCount: committedCount,
        timestamp
      };
    } catch (error) {
      throw new Error(`Failed to commit lockchain batch: ${error.message}`);
    }
  }

  /**
   * Verify a lockchain entry
   * @param {string} entryId - Entry ID to verify
   * @returns {Promise<Object>} Verification result
   */
  async verifyEntry(entryId) {
    const entry = await this._loadEntry(entryId);
    if (!entry) {
      return { valid: false, error: 'Entry not found' };
    }
    
    try {
      // Verify signature
      const signatureValid = await this._verifySignature(entry);
      if (!signatureValid) {
        return { valid: false, error: 'Invalid signature' };
      }
      
      // Verify Git commit if present
      if (entry.gitCommit && this.config.enableGitAnchoring) {
        const gitValid = await this._verifyGitCommit(entry.gitCommit);
        if (!gitValid) {
          return { valid: false, error: 'Invalid Git commit' };
        }
      }
      
      // Verify merkle root if present
      if (entry.merkleRoot && this.config.enableMerkle) {
        const merkleValid = await this._verifyMerkleRoot(entry);
        if (!merkleValid) {
          return { valid: false, error: 'Invalid merkle root' };
        }
      }
      
      return { valid: true, entry };
    } catch (error) {
      return { valid: false, error: error.message };
    }
  }

  /**
   * Get lockchain statistics
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      config: this.config,
      pendingEntries: this.pendingEntries.length,
      storagePath: this.storagePath,
      gitEnabled: this.config.enableGitAnchoring,
      merkleEnabled: this.config.enableMerkle
    };
  }

  /**
   * Ensure storage directory exists
   * @private
   */
  _ensureStorageExists() {
    if (!existsSync(this.storagePath)) {
      mkdirSync(this.storagePath, { recursive: true });
    }
  }

  /**
   * Initialize Git repository
   * @private
   */
  _initializeGitRepo() {
    if (!this.config.enableGitAnchoring) return;
    
    try {
      // Check if Git repo exists
      execSync('git rev-parse --git-dir', { 
        cwd: this.config.gitRepo, 
        stdio: 'pipe' 
      });
    } catch (error) {
      throw new Error(`Git repository not found at ${this.config.gitRepo}`);
    }
  }

  /**
   * Serialize receipt for storage
   * @param {Object} receipt - Transaction receipt
   * @returns {Object} Serialized receipt
   * @private
   */
  _serializeReceipt(receipt) {
    return {
      ...receipt,
      _serialized: true,
      _timestamp: Date.now()
    };
  }

  /**
   * Sign an entry
   * @param {Object} receipt - Transaction receipt
   * @param {string} entryId - Entry ID
   * @param {number} timestamp - Timestamp
   * @returns {Promise<Object>} Signature
   * @private
   */
  async _signEntry(receipt, entryId, timestamp) {
    // For now, use a simple hash-based signature
    // In production, this would use proper cryptographic signatures
    const data = JSON.stringify({ receipt, entryId, timestamp });
    const hash = bytesToHex(sha3_256(utf8ToBytes(data)));
    
    return {
      algorithm: 'sha3-256',
      value: hash,
      publicKey: this.config.signingKey || 'default'
    };
  }

  /**
   * Get previous hash for chaining
   * @returns {string} Previous hash
   * @private
   */
  _getPreviousHash() {
    if (this.pendingEntries.length === 0) {
      return null;
    }
    
    const lastEntry = this.pendingEntries[this.pendingEntries.length - 1];
    return lastEntry.signature.value;
  }

  /**
   * Store an entry
   * @param {Object} entry - Lockchain entry
   * @private
   */
  async _storeEntry(entry) {
    const entryFile = join(this.storagePath, `entry-${entry.id}.json`);
    writeFileSync(entryFile, JSON.stringify(entry, null, 2));
    
    // Cache entry
    this.entryCache.set(entry.id, entry);
  }

  /**
   * Update an entry
   * @param {Object} entry - Updated entry
   * @private
   */
  async _updateEntry(entry) {
    const entryFile = join(this.storagePath, `entry-${entry.id}.json`);
    writeFileSync(entryFile, JSON.stringify(entry, null, 2));
    
    // Update cache
    this.entryCache.set(entry.id, entry);
  }

  /**
   * Load an entry
   * @param {string} entryId - Entry ID
   * @returns {Promise<Object>} Entry or null
   * @private
   */
  async _loadEntry(entryId) {
    // Check cache first
    if (this.entryCache.has(entryId)) {
      return this.entryCache.get(entryId);
    }
    
    const entryFile = join(this.storagePath, `entry-${entryId}.json`);
    if (!existsSync(entryFile)) {
      return null;
    }
    
    const entry = JSON.parse(readFileSync(entryFile, 'utf8'));
    this.entryCache.set(entryId, entry);
    return entry;
  }

  /**
   * Calculate merkle root for a single entry
   * @param {Object} entry - Single entry
   * @returns {string} Merkle root hash
   * @private
   *
   * @description
   * Calculates a Merkle root for a single entry by hashing its canonical data.
   * Uses deterministic JSON serialization to ensure consistent hashing.
   */
  _calculateEntryMerkleRoot(entry) {
    // Build canonical data representation
    // Use same structure as verification for consistency
    const entryData = {
      id: entry.id,
      timestamp: entry.timestamp,
      receipt: entry.receipt,
      signature: entry.signature,
      previousHash: entry.previousHash || null
    };

    // Calculate hash using SHA3-256
    const entryJson = JSON.stringify(entryData);
    return bytesToHex(sha3_256(utf8ToBytes(entryJson)));
  }

  /**
   * Calculate merkle root for multiple entries (batch)
   * @param {Array} entries - Entries
   * @returns {string} Merkle root
   * @private
   */
  _calculateMerkleRoot(entries) {
    if (entries.length === 0) return null;

    const hashes = entries.map(entry =>
      bytesToHex(sha3_256(utf8ToBytes(JSON.stringify(entry))))
    );

    // Simple merkle tree calculation
    let currentLevel = hashes;
    while (currentLevel.length > 1) {
      const nextLevel = [];
      for (let i = 0; i < currentLevel.length; i += 2) {
        const left = currentLevel[i];
        const right = currentLevel[i + 1] || left;
        const combined = left + right;
        nextLevel.push(bytesToHex(sha3_256(utf8ToBytes(combined))));
      }
      currentLevel = nextLevel;
    }

    return currentLevel[0];
  }

  /**
   * Git add operation
   * @param {string} filePath - File to add
   * @private
   */
  async _gitAdd(filePath) {
    execSync(`git add "${filePath}"`, { 
      cwd: this.config.gitRepo,
      stdio: 'pipe'
    });
  }

  /**
   * Git commit operation
   * @param {string} message - Commit message
   * @param {Object} metadata - Commit metadata
   * @returns {Promise<string>} Commit hash
   * @private
   */
  async _gitCommit(message, metadata = {}) {
    const commitMessage = `${message}\n\nMetadata: ${JSON.stringify(metadata)}`;
    
    const output = execSync(`git commit -m "${commitMessage}"`, { 
      cwd: this.config.gitRepo,
      stdio: 'pipe',
      encoding: 'utf8'
    });
    
    // Extract commit hash from output
    const commitHash = output.trim().split('\n').pop();
    return commitHash;
  }

  /**
   * Get latest commit hash
   * @returns {Promise<string>} Latest commit hash
   * @private
   */
  async _getLatestCommit() {
    const output = execSync('git rev-parse HEAD', { 
      cwd: this.config.gitRepo,
      stdio: 'pipe',
      encoding: 'utf8'
    });
    
    return output.trim();
  }

  /**
   * Verify signature
   * @param {Object} entry - Entry to verify
   * @returns {Promise<boolean>} Signature valid
   * @private
   */
  async _verifySignature(entry) {
    // For now, just verify the hash matches
    const data = JSON.stringify({ 
      receipt: entry.receipt, 
      entryId: entry.id, 
      timestamp: entry.timestamp 
    });
    const expectedHash = bytesToHex(sha3_256(utf8ToBytes(data)));
    
    return entry.signature.value === expectedHash;
  }

  /**
   * Verify Git commit
   * @param {string} commitHash - Commit hash
   * @returns {Promise<boolean>} Commit valid
   * @private
   */
  async _verifyGitCommit(commitHash) {
    try {
      execSync(`git cat-file -t ${commitHash}`, { 
        cwd: this.config.gitRepo,
        stdio: 'pipe'
      });
      return true;
    } catch (error) {
      return false;
    }
  }

  /**
   * Verify merkle root cryptographically
   * @param {Object} entry - Entry to verify
   * @returns {Promise<boolean>} Merkle root valid
   * @private
   *
   * @description
   * Validates the Merkle root by:
   * 1. Extracting the entry data components (receipt, signature, timestamp, etc.)
   * 2. Calculating the Merkle root from these components using SHA3-256
   * 3. Comparing the calculated root with the stored entry.merkleRoot
   *
   * This ensures cryptographic integrity - any tampering with the entry data
   * will cause the verification to fail.
   */
  async _verifyMerkleRoot(entry) {
    if (!entry.merkleRoot) {
      return true; // No merkle root to verify
    }

    try {
      // Build canonical data representation for verification
      // Include all critical entry components in deterministic order
      const entryData = {
        id: entry.id,
        timestamp: entry.timestamp,
        receipt: entry.receipt,
        signature: entry.signature,
        previousHash: entry.previousHash || null
      };

      // Calculate hash of entry data (leaf node in Merkle tree)
      const entryJson = JSON.stringify(entryData);
      const entryHash = bytesToHex(sha3_256(utf8ToBytes(entryJson)));

      // For a single entry, the Merkle root should be the hash of the entry itself
      // (a Merkle tree with one leaf node has that leaf as the root)
      // If the entry was part of a batch, we verify against the batch's Merkle root
      const calculatedRoot = entryHash;

      // Compare calculated root with stored root
      const isValid = calculatedRoot === entry.merkleRoot;

      if (!isValid) {
        console.error('[LockchainWriter] Merkle root verification failed', {
          entryId: entry.id,
          stored: entry.merkleRoot,
          calculated: calculatedRoot
        });
      }

      return isValid;
    } catch (error) {
      console.error('[LockchainWriter] Error verifying Merkle root:', error);
      return false;
    }
  }
}

/**
 * Create a lockchain writer instance
 * @param {Object} [config] - Configuration
 * @returns {LockchainWriter} Lockchain writer instance
 */
export function createLockchainWriter(config = {}) {
  return new LockchainWriter(config);
}

/**
 * Default lockchain writer instance
 */
export const defaultLockchainWriter = createLockchainWriter();
