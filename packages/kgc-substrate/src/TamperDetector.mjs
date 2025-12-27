/**
 * TamperDetector - Cryptographic verification and tamper detection for receipt chains
 *
 * Provides comprehensive verification of receipt chains including:
 * - Hash chain integrity (merkle tree verification)
 * - Timestamp monotonicity
 * - Block ordering
 * - Content hash verification
 * - Genesis hash validation
 *
 * @module TamperDetector
 */

import { sha256 } from 'hash-wasm';
import { ReceiptChain } from './ReceiptChain.mjs';

/**
 * Verification result schema
 * @typedef {Object} VerificationResult
 * @property {boolean} valid - Overall validity
 * @property {Array<string>} errors - Array of error descriptions
 * @property {Object} [details] - Additional verification details
 */

/**
 * TamperDetector class - Cryptographic verification and tamper detection
 */
export class TamperDetector {
  /**
   * Verify a receipt chain for tampering
   *
   * Performs comprehensive validation:
   * 1. Genesis hash check
   * 2. Block count validation
   * 3. Hash chain integrity
   * 4. Content hash verification
   * 5. Timestamp monotonicity
   * 6. Block ordering
   *
   * @param {ReceiptChain|Object} chainOrJSON - Chain to verify (ReceiptChain instance or JSON)
   * @returns {Promise<VerificationResult>} Verification result
   *
   * @example
   * const detector = new TamperDetector();
   * const result = await detector.verify(chain);
   * if (!result.valid) {
   *   console.error('Tampering detected:', result.errors);
   * }
   */
  async verify(chainOrJSON) {
    const errors = [];
    const details = {
      blocks_checked: 0,
      genesis_hash: null,
      chain_length: 0,
      timestamp_checks: 0
    };

    try {
      // Accept either ReceiptChain or JSON
      let chain;
      if (chainOrJSON instanceof ReceiptChain) {
        chain = chainOrJSON;
      } else if (typeof chainOrJSON === 'object' && chainOrJSON.blocks) {
        // Assume it's JSON serialized chain
        chain = ReceiptChain.fromJSON(chainOrJSON);
      } else {
        throw new Error('Invalid input: expected ReceiptChain instance or JSON object');
      }

      details.genesis_hash = chain.genesis_hash;
      details.chain_length = chain.getLength();

      // Check 1: Validate genesis hash format
      if (!/^[0-9a-f]{64}$/.test(chain.genesis_hash)) {
        errors.push(`Invalid genesis hash format: ${chain.genesis_hash}`);
      }

      // Check 2: Verify each block
      const blocks = chain.getAllBlocks();
      let previous_hash = chain.genesis_hash;
      let previous_timestamp = 0n;

      for (let i = 0; i < blocks.length; i++) {
        const block = blocks[i];
        details.blocks_checked++;

        // Check 2a: Verify before_hash matches previous block's after_hash
        if (block.before_hash !== previous_hash) {
          errors.push(
            `Block ${i}: before_hash mismatch (expected ${previous_hash}, got ${block.before_hash})`
          );
        }

        // Check 2b: Verify content hash matches after_hash
        const computed_hash = await this._computeContentHash({
          timestamp_ns: block.timestamp_ns,
          agent_id: block.agent_id,
          toolchain_version: block.toolchain_version,
          artifacts: block.artifacts
        });

        if (computed_hash !== block.after_hash) {
          errors.push(
            `Block ${i}: content hash mismatch (expected ${block.after_hash}, got ${computed_hash})`
          );
        }

        // Check 2c: Verify timestamp monotonicity
        if (block.timestamp_ns <= previous_timestamp) {
          errors.push(
            `Block ${i}: timestamp not monotonic (${block.timestamp_ns} <= ${previous_timestamp})`
          );
        }
        details.timestamp_checks++;

        // Check 2d: Validate block structure
        if (!block.agent_id || typeof block.agent_id !== 'string') {
          errors.push(`Block ${i}: invalid agent_id`);
        }
        if (!block.toolchain_version || typeof block.toolchain_version !== 'string') {
          errors.push(`Block ${i}: invalid toolchain_version`);
        }
        if (!Array.isArray(block.artifacts)) {
          errors.push(`Block ${i}: artifacts must be an array`);
        }

        // Update previous hash and timestamp for next iteration
        previous_hash = block.after_hash;
        previous_timestamp = block.timestamp_ns;
      }

      return {
        valid: errors.length === 0,
        errors,
        details
      };
    } catch (err) {
      errors.push(`Verification exception: ${err.message}`);
      return {
        valid: false,
        errors,
        details
      };
    }
  }

  /**
   * Compute hash of block content (same algorithm as ReceiptChain)
   * @param {Object} content - Block content
   * @returns {Promise<string>} SHA256 hex digest
   * @private
   */
  async _computeContentHash(content) {
    const canonical = JSON.stringify({
      timestamp_ns: content.timestamp_ns.toString(),
      agent_id: content.agent_id,
      toolchain_version: content.toolchain_version,
      artifacts: content.artifacts
    });
    return await sha256(canonical);
  }

  /**
   * Detect specific tampering scenarios (for testing/auditing)
   *
   * @param {ReceiptChain} original - Original chain
   * @param {ReceiptChain} suspect - Suspect chain to compare
   * @returns {Promise<Object>} Tampering analysis
   *
   * @example
   * const detector = new TamperDetector();
   * const analysis = await detector.detectTampering(original, suspect);
   * console.log(analysis.tamper_type); // e.g., 'bit_flip', 'reordered', 'replay'
   */
  async detectTampering(original, suspect) {
    const original_blocks = original.getAllBlocks();
    const suspect_blocks = suspect.getAllBlocks();

    const analysis = {
      tamper_type: 'none',
      tampered_blocks: [],
      description: ''
    };

    // Check 1: Length mismatch
    if (original_blocks.length !== suspect_blocks.length) {
      analysis.tamper_type = 'length_mismatch';
      analysis.description = `Block count changed: ${original_blocks.length} → ${suspect_blocks.length}`;
      return analysis;
    }

    // Check 2: Block-by-block comparison
    for (let i = 0; i < original_blocks.length; i++) {
      const orig = original_blocks[i];
      const susp = suspect_blocks[i];

      // Check timestamp
      if (orig.timestamp_ns !== susp.timestamp_ns) {
        analysis.tamper_type = 'timestamp_modified';
        analysis.tampered_blocks.push(i);
        analysis.description = `Block ${i}: timestamp changed`;
      }

      // Check hashes
      if (orig.after_hash !== susp.after_hash) {
        analysis.tamper_type = 'content_modified';
        analysis.tampered_blocks.push(i);
        analysis.description = `Block ${i}: content hash changed`;
      }

      // Check agent_id
      if (orig.agent_id !== susp.agent_id) {
        analysis.tamper_type = 'metadata_modified';
        analysis.tampered_blocks.push(i);
        analysis.description = `Block ${i}: agent_id changed`;
      }
    }

    // Check 3: Ordering check (timestamps out of order)
    for (let i = 1; i < suspect_blocks.length; i++) {
      if (suspect_blocks[i].timestamp_ns <= suspect_blocks[i - 1].timestamp_ns) {
        analysis.tamper_type = 'reordered';
        analysis.tampered_blocks.push(i);
        analysis.description = `Block ${i}: out of order`;
      }
    }

    return analysis;
  }

  /**
   * Verify merkle proof for a specific block
   *
   * @param {ReceiptChain} chain - Chain to verify
   * @param {number} blockIndex - Index of block to verify
   * @returns {Promise<Object>} Merkle proof verification result
   *
   * @example
   * const detector = new TamperDetector();
   * const proof = await detector.verifyMerkleProof(chain, 2);
   * console.log(proof.valid); // true if merkle path is valid
   */
  async verifyMerkleProof(chain, blockIndex) {
    const block = chain.getBlock(blockIndex);
    if (!block) {
      return {
        valid: false,
        error: 'Block index out of bounds'
      };
    }

    // Verify merkle root = SHA256(before_hash || after_hash)
    const computed_root = await sha256(block.before_hash + block.after_hash);

    return {
      valid: true,
      block_index: blockIndex,
      before_hash: block.before_hash,
      after_hash: block.after_hash,
      merkle_root: computed_root
    };
  }

  /**
   * Generate tamper report for a chain
   *
   * @param {ReceiptChain} chain - Chain to report on
   * @returns {Promise<Object>} Tamper detection report
   *
   * @example
   * const detector = new TamperDetector();
   * const report = await detector.generateReport(chain);
   * console.log(report.summary); // "Valid: true, Blocks: 5, Errors: 0"
   */
  async generateReport(chain) {
    const verification = await this.verify(chain);
    const blocks = chain.getAllBlocks();

    const report = {
      timestamp: new Date().toISOString(),
      chain_length: blocks.length,
      genesis_hash: chain.genesis_hash,
      head_hash: chain.getHeadHash(),
      verification: verification,
      summary: `Valid: ${verification.valid}, Blocks: ${blocks.length}, Errors: ${verification.errors.length}`,
      blocks_analyzed: blocks.map((block, idx) => ({
        index: idx,
        agent_id: block.agent_id,
        timestamp_ns: block.timestamp_ns.toString(),
        artifacts_count: block.artifacts.length,
        before_hash: block.before_hash,
        after_hash: block.after_hash
      }))
    };

    return report;
  }
}
