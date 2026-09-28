/**
 * ReceiptChain - Immutable ledger with merkle tree chaining
 *
 * Provides cryptographically-verifiable chain of receipts where each block
 * commits to the previous block via hash chaining, creating an immutable audit trail.
 *
 * Block structure:
 * - before_hash: Hash of previous block (merkle root)
 * - after_hash: Hash of current block content
 * - timestamp_ns: UTC nanoseconds (monotonic)
 * - agent_id: Agent identifier
 * - toolchain_version: Toolchain version string
 * - artifacts: Array of artifact objects
 *
 * @module ReceiptChain
 */

import { sha256 } from 'hash-wasm';
import { z } from 'zod';

/**
 * Artifact schema - individual work products in a block
 */
const ArtifactSchema = z.object({
  type: z.string(),
  path: z.string(),
  hash: z.string(),
  size_bytes: z.number().int().nonnegative()
});

/**
 * Block schema - immutable ledger entry
 */
const BlockSchema = z.object({
  before_hash: z.string().length(64), // SHA256 hex
  after_hash: z.string().length(64),  // SHA256 hex
  timestamp_ns: z.bigint(),
  agent_id: z.string().min(1),
  toolchain_version: z.string().min(1),
  artifacts: z.array(ArtifactSchema)
});

/**
 * Receipt chain configuration schema
 */
const ConfigSchema = z.object({
  genesis_hash: z.string().length(64).optional(),
  enforce_monotonic_time: z.boolean().default(true)
});

/**
 * ReceiptChain class - Immutable ledger with merkle tree chaining
 */
export class ReceiptChain {
  /**
   * Genesis block hash - conventionally all zeros
   * @type {string}
   */
  static GENESIS_HASH = '0'.repeat(64);

  /**
   * Create a new receipt chain
   * @param {Object} config - Configuration options
   * @param {string} [config.genesis_hash] - Custom genesis hash (default: all zeros)
   * @param {boolean} [config.enforce_monotonic_time=true] - Enforce monotonic timestamps
   */
  constructor(config = {}) {
    const validated = ConfigSchema.parse(config);
    this.blocks = [];
    this.genesis_hash = validated.genesis_hash || ReceiptChain.GENESIS_HASH;
    this.enforce_monotonic_time = validated.enforce_monotonic_time;
    this.last_timestamp_ns = 0n;
  }

  /**
   * Get current chain head hash
   * @returns {string} Hash of the last block, or genesis hash if empty
   */
  getHeadHash() {
    if (this.blocks.length === 0) {
      return this.genesis_hash;
    }
    return this.blocks[this.blocks.length - 1].after_hash;
  }

  /**
   * Get chain length
   * @returns {number} Number of blocks in the chain
   */
  getLength() {
    return this.blocks.length;
  }

  /**
   * Get block by index
   * @param {number} index - Block index (0-based)
   * @returns {Object|null} Block or null if out of bounds
   */
  getBlock(index) {
    if (index < 0 || index >= this.blocks.length) {
      return null;
    }
    return this.blocks[index];
  }

  /**
   * Get all blocks (defensive copy to prevent mutation)
   * @returns {Array<Object>} Array of all blocks
   */
  getAllBlocks() {
    return structuredClone(this.blocks);
  }

  /**
   * Compute hash of block content (before appending to chain)
   * Hash includes: timestamp, agent_id, toolchain_version, artifacts
   * Does NOT include before_hash/after_hash (those are chain metadata)
   *
   * @param {Object} content - Block content to hash
   * @param {bigint} content.timestamp_ns - UTC nanoseconds
   * @param {string} content.agent_id - Agent identifier
   * @param {string} content.toolchain_version - Toolchain version
   * @param {Array<Object>} content.artifacts - Array of artifacts
   * @returns {Promise<string>} SHA256 hex digest
   * @private
   */
  async _computeContentHash(content) {
    // Canonical JSON serialization for deterministic hashing
    const canonical = JSON.stringify({
      timestamp_ns: content.timestamp_ns.toString(),
      agent_id: content.agent_id,
      toolchain_version: content.toolchain_version,
      artifacts: content.artifacts
    });
    return await sha256(canonical);
  }

  /**
   * Compute merkle root of current block + previous hash
   * Merkle root = SHA256(before_hash || after_hash)
   *
   * @param {string} before_hash - Hash of previous block
   * @param {string} after_hash - Hash of current block content
   * @returns {Promise<string>} Merkle root hash
   * @private
   */
  async _computeMerkleRoot(before_hash, after_hash) {
    return await sha256(before_hash + after_hash);
  }

  /**
   * Append a new block to the chain (immutable operation)
   *
   * @param {Object} blockData - Block data to append
   * @param {string} blockData.agent_id - Agent identifier
   * @param {string} blockData.toolchain_version - Toolchain version
   * @param {Array<Object>} blockData.artifacts - Array of artifacts
   * @param {bigint} [blockData.timestamp_ns] - Custom timestamp (default: now)
   * @returns {Promise<Object>} Appended block with receipt
   * @throws {Error} If validation fails or timestamp not monotonic
   *
   * @example
   * const chain = new ReceiptChain();
   * const block = await chain.append({
   *   agent_id: 'agent-2',
   *   toolchain_version: '1.0.0',
   *   artifacts: [{ type: 'code', path: 'foo.mjs', hash: 'abc123', size_bytes: 1024 }]
   * });
   */
  async append(blockData) {
    // Validate input structure
    const { agent_id, toolchain_version, artifacts, timestamp_ns } = blockData;

    if (!agent_id || typeof agent_id !== 'string') {
      throw new Error('ReceiptChain.append: agent_id is required and must be a string');
    }
    if (!toolchain_version || typeof toolchain_version !== 'string') {
      throw new Error('ReceiptChain.append: toolchain_version is required and must be a string');
    }
    if (!Array.isArray(artifacts)) {
      throw new Error('ReceiptChain.append: artifacts must be an array');
    }

    // Validate artifacts
    artifacts.forEach((artifact, idx) => {
      try {
        ArtifactSchema.parse(artifact);
      } catch (err) {
        throw new Error(`ReceiptChain.append: Invalid artifact at index ${idx}: ${err.message}`);
      }
    });

    // Get timestamp (custom or now)
    const timestamp = timestamp_ns || BigInt(Date.now()) * 1_000_000n;

    // Enforce monotonic time if enabled
    if (this.enforce_monotonic_time && timestamp <= this.last_timestamp_ns) {
      throw new Error(
        `ReceiptChain.append: Timestamp not monotonic (${timestamp} <= ${this.last_timestamp_ns})`
      );
    }

    // Get previous block hash (genesis or last block)
    const before_hash = this.getHeadHash();

    // Compute content hash
    const content = { timestamp_ns: timestamp, agent_id, toolchain_version, artifacts };
    const after_hash = await this._computeContentHash(content);

    // Create block
    const block = {
      before_hash,
      after_hash,
      timestamp_ns: timestamp,
      agent_id,
      toolchain_version,
      artifacts
    };

    // Validate block schema
    BlockSchema.parse(block);

    // Append to chain (immutable - no mutation of existing blocks)
    this.blocks.push(Object.freeze(block));
    this.last_timestamp_ns = timestamp;

    return {
      block,
      index: this.blocks.length - 1,
      merkle_root: await this._computeMerkleRoot(before_hash, after_hash)
    };
  }

  /**
   * Serialize chain to JSON
   * @returns {Object} Serialized chain
   */
  toJSON() {
    return {
      genesis_hash: this.genesis_hash,
      length: this.blocks.length,
      head_hash: this.getHeadHash(),
      blocks: this.blocks.map(block => ({
        ...block,
        timestamp_ns: block.timestamp_ns.toString() // BigInt to string
      }))
    };
  }

  /**
   * Deserialize chain from JSON
   * @param {Object} json - Serialized chain
   * @returns {ReceiptChain} Reconstructed chain
   */
  static fromJSON(json) {
    const chain = new ReceiptChain({
      genesis_hash: json.genesis_hash,
      enforce_monotonic_time: false // Don't enforce when loading
    });

    // Restore blocks
    for (const blockData of json.blocks) {
      const block = {
        ...blockData,
        timestamp_ns: BigInt(blockData.timestamp_ns)
      };
      chain.blocks.push(Object.freeze(block));
      chain.last_timestamp_ns = block.timestamp_ns;
    }

    return chain;
  }

  /**
   * Encode chain to base64 (for embedding in receipts)
   * @returns {string} Base64-encoded JSON
   */
  toBase64() {
    const json = JSON.stringify(this.toJSON());
    return Buffer.from(json, 'utf8').toString('base64');
  }

  /**
   * Decode chain from base64
   * @param {string} base64 - Base64-encoded chain
   * @returns {ReceiptChain} Reconstructed chain
   */
  static fromBase64(base64) {
    const json = JSON.parse(Buffer.from(base64, 'base64').toString('utf8'));
    return ReceiptChain.fromJSON(json);
  }
}
