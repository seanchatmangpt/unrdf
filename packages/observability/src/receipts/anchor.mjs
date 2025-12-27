/**
 * Receipt Anchoring - External timestamping and verification
 *
 * Provides anchoring to external systems:
 * - Blockchain (Ethereum, Bitcoin, etc.)
 * - Git repositories (commit SHA)
 * - Timestamp authorities (RFC 3161)
 *
 * @module @unrdf/observability/receipts/anchor
 */

import { AnchorSchema } from './receipt-schema.mjs';

/**
 * ReceiptAnchorer - Anchor Merkle roots to external systems
 *
 * @example
 * const anchorer = new ReceiptAnchorer();
 * const anchor = await anchorer.anchorToGit(merkleRoot, commitSha, repo);
 */
export class ReceiptAnchorer {
  /**
   * Anchor Merkle root to Git repository
   *
   * @param {string} merkleRoot - Merkle root hash to anchor
   * @param {string} commitSha - Git commit SHA
   * @param {string} repository - Repository identifier
   * @returns {Promise<Object>} Anchor proof
   */
  async anchorToGit(merkleRoot, commitSha, repository) {
    const anchor = {
      merkleRoot,
      anchorType: 'git',
      anchorData: {
        commitSha,
        repository,
      },
      timestamp: new Date().toISOString(),
    };

    return AnchorSchema.parse(anchor);
  }

  /**
   * Anchor Merkle root to blockchain
   *
   * @param {string} merkleRoot - Merkle root hash to anchor
   * @param {string} txHash - Transaction hash
   * @param {number} blockNumber - Block number
   * @param {string} network - Network name (e.g., 'ethereum', 'bitcoin')
   * @returns {Promise<Object>} Anchor proof
   */
  async anchorToBlockchain(merkleRoot, txHash, blockNumber, network) {
    const anchor = {
      merkleRoot,
      anchorType: 'blockchain',
      anchorData: {
        txHash,
        blockNumber,
        network,
      },
      timestamp: new Date().toISOString(),
    };

    return AnchorSchema.parse(anchor);
  }

  /**
   * Anchor Merkle root to timestamp authority
   *
   * @param {string} merkleRoot - Merkle root hash to anchor
   * @param {string} timestampToken - RFC 3161 timestamp token
   * @param {string} authority - Timestamp authority identifier
   * @returns {Promise<Object>} Anchor proof
   */
  async anchorToTimestampService(merkleRoot, timestampToken, authority) {
    const anchor = {
      merkleRoot,
      anchorType: 'timestamp-service',
      anchorData: {
        timestampToken,
        authority,
      },
      timestamp: new Date().toISOString(),
    };

    return AnchorSchema.parse(anchor);
  }

  /**
   * Verify an anchor proof
   *
   * @param {Object} anchor - Anchor to verify
   * @returns {Promise<Object>} Verification result
   */
  async verifyAnchor(anchor) {
    try {
      AnchorSchema.parse(anchor);

      // Basic validation - actual verification would query external systems
      const errors = [];

      if (anchor.anchorType === 'git') {
        if (!anchor.anchorData.commitSha || !anchor.anchorData.repository) {
          errors.push('Git anchor missing commitSha or repository');
        }
      } else if (anchor.anchorType === 'blockchain') {
        if (!anchor.anchorData.txHash || !anchor.anchorData.network) {
          errors.push('Blockchain anchor missing txHash or network');
        }
      } else if (anchor.anchorType === 'timestamp-service') {
        if (!anchor.anchorData.timestampToken || !anchor.anchorData.authority) {
          errors.push('Timestamp anchor missing token or authority');
        }
      }

      return {
        valid: errors.length === 0,
        errors,
        anchorType: anchor.anchorType,
        timestamp: anchor.timestamp,
      };
    } catch (err) {
      return {
        valid: false,
        errors: ['Anchor validation failed: ' + err.message],
      };
    }
  }

  /**
   * Export anchor for external verification
   *
   * @param {Object} anchor - Anchor to export
   * @returns {string} Base64-encoded anchor
   */
  exportAnchor(anchor) {
    const json = JSON.stringify(anchor);
    return Buffer.from(json, 'utf8').toString('base64');
  }

  /**
   * Import anchor from external source
   *
   * @param {string} base64Anchor - Base64-encoded anchor
   * @returns {Object} Anchor object
   */
  importAnchor(base64Anchor) {
    const json = Buffer.from(base64Anchor, 'base64').toString('utf8');
    const anchor = JSON.parse(json);
    return AnchorSchema.parse(anchor);
  }
}

export default ReceiptAnchorer;
