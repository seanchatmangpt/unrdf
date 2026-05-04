/**
 * Receipt Anchorer - Blockchain anchoring for cryptographic receipts
 *
 * Provides on-chain anchoring of YAWL receipt hashes for immutable audit trails.
 * Supports individual anchoring and batch Merkle tree anchoring.
 *
 * @module @unrdf/blockchain/anchoring
 */

import { ethers } from 'ethers';
import { z } from 'zod';
import { sha256 } from '@noble/hashes/sha256';
import { bytesToHex } from '@noble/hashes/utils';

// =============================================================================
// Schemas
// =============================================================================

/**
 * Anchor result schema
 */
const AnchorResultSchema = z.object({
  /** Transaction hash */
  txHash: z.string(),
  /** Block number */
  blockNumber: z.number(),
  /** Gas used */
  gasUsed: z.bigint(),
  /** Gas price in wei */
  gasPrice: z.bigint(),
  /** Total cost in ETH */
  costETH: z.string(),
  /** Receipt hash that was anchored */
  receiptHash: z.string(),
  /** Timestamp of anchoring */
  timestamp: z.number(),
});

/**
 * Verification result schema
 */
const VerificationResultSchema = z.object({
  /** Whether the receipt is anchored on-chain */
  isAnchored: z.boolean(),
  /** Block number where anchored (if found) */
  blockNumber: z.number().optional(),
  /** Transaction hash (if found) */
  txHash: z.string().optional(),
  /** Timestamp (if found) */
  timestamp: z.number().optional(),
});

// =============================================================================
// Receipt Anchorer
// =============================================================================

/**
 * BlockchainAnchor - Anchors receipt hashes to Ethereum blockchain
 *
 * @class
 * @example
 * ```javascript
 * const anchorer = new ReceiptAnchorer({
 *   provider: 'http://localhost:8545',
 *   privateKey: process.env.PRIVATE_KEY,
 *   contractAddress: '0x...'
 * });
 *
 * const receipt = generateReceipt(...);
 * const result = await anchorer.anchorReceipt(receipt);
 * console.log(`Anchored at block ${result.blockNumber}`);
 * ```
 */
export class ReceiptAnchorer {
  /**
   * @param {Object} config - Configuration
   * @param {string} config.provider - Ethereum provider URL
   * @param {string} config.privateKey - Private key for signing transactions
   * @param {string} config.contractAddress - WorkflowVerifier contract address
   */
  constructor({ provider, privateKey, contractAddress }) {
    this.provider = new ethers.JsonRpcProvider(provider);
    this.wallet = new ethers.Wallet(privateKey, this.provider);
    this.contractAddress = contractAddress;

    // WorkflowVerifier ABI (minimal for anchoring)
    const abi = [
      'function anchorReceipt(bytes32 receiptHash) external returns (bool)',
      'function anchorBatch(bytes32[] memory receiptHashes) external returns (bool)',
      'function anchorMerkleRoot(bytes32 merkleRoot, uint256 receiptCount) external returns (bool)',
      'function verifyReceipt(bytes32 receiptHash) external view returns (bool, uint256, bytes32)',
      'event ReceiptAnchored(bytes32 indexed receiptHash, uint256 blockNumber, uint256 timestamp)',
      'event BatchAnchored(bytes32[] receiptHashes, uint256 blockNumber, uint256 timestamp)',
      'event MerkleRootAnchored(bytes32 indexed merkleRoot, uint256 receiptCount, uint256 blockNumber)',
    ];

    this.contract = new ethers.Contract(contractAddress, abi, this.wallet);
  }

  /**
   * Anchor a single receipt hash to blockchain
   *
   * @param {Object} receipt - YAWL receipt object
   * @returns {Promise<Object>} Anchor result with transaction details
   */
  async anchorReceipt(receipt) {
    // Extract or compute receipt hash
    const receiptHash = receipt.hash || this._computeReceiptHash(receipt);

    // Convert hash to bytes32
    const bytes32Hash = this._toBytes32(receiptHash);

    // Send transaction
    const tx = await this.contract.anchorReceipt(bytes32Hash);
    const txReceipt = await tx.wait();

    // Calculate cost
    const gasUsed = txReceipt.gasUsed;
    const gasPrice = tx.gasPrice;
    const costWei = gasUsed * gasPrice;
    const costETH = ethers.formatEther(costWei);

    const result = {
      txHash: txReceipt.hash,
      blockNumber: txReceipt.blockNumber,
      gasUsed,
      gasPrice,
      costETH,
      receiptHash,
      timestamp: Math.floor(Date.now() / 1000),
    };

    return AnchorResultSchema.parse(result);
  }

  /**
   * Anchor multiple receipts in a single transaction (batch mode)
   *
   * @param {Array<Object>} receipts - Array of YAWL receipt objects
   * @returns {Promise<Object>} Anchor result with batch details
   */
  async anchorBatch(receipts) {
    // Extract/compute all receipt hashes
    const receiptHashes = receipts.map(r =>
      this._toBytes32(r.hash || this._computeReceiptHash(r))
    );

    // Send batch transaction
    const tx = await this.contract.anchorBatch(receiptHashes);
    const txReceipt = await tx.wait();

    // Calculate cost
    const gasUsed = txReceipt.gasUsed;
    const gasPrice = tx.gasPrice;
    const costWei = gasUsed * gasPrice;
    const costETH = ethers.formatEther(costWei);

    return {
      txHash: txReceipt.hash,
      blockNumber: txReceipt.blockNumber,
      gasUsed,
      gasPrice,
      costETH,
      receiptCount: receipts.length,
      receiptHashes: receiptHashes.map(h => h),
      timestamp: Math.floor(Date.now() / 1000),
    };
  }

  /**
   * Anchor a Merkle root (most gas-efficient for large batches)
   *
   * @param {string} merkleRoot - Merkle tree root hash
   * @param {number} receiptCount - Number of receipts in the tree
   * @returns {Promise<Object>} Anchor result
   */
  async anchorMerkleRoot(merkleRoot, receiptCount) {
    const bytes32Root = this._toBytes32(merkleRoot);

    const tx = await this.contract.anchorMerkleRoot(bytes32Root, receiptCount);
    const txReceipt = await tx.wait();

    const gasUsed = txReceipt.gasUsed;
    const gasPrice = tx.gasPrice;
    const costWei = gasUsed * gasPrice;
    const costETH = ethers.formatEther(costWei);

    return {
      txHash: txReceipt.hash,
      blockNumber: txReceipt.blockNumber,
      gasUsed,
      gasPrice,
      costETH,
      merkleRoot,
      receiptCount,
      timestamp: Math.floor(Date.now() / 1000),
    };
  }

  /**
   * Verify if a receipt is anchored on-chain
   *
   * @param {Object} receipt - YAWL receipt object
   * @returns {Promise<Object>} Verification result
   */
  async verifyReceipt(receipt) {
    const receiptHash = receipt.hash || this._computeReceiptHash(receipt);
    const bytes32Hash = this._toBytes32(receiptHash);

    const [isAnchored, blockNumber, txHash] = await this.contract.verifyReceipt(bytes32Hash);

    const result = {
      isAnchored,
      ...(isAnchored && {
        blockNumber: Number(blockNumber),
        txHash,
      }),
    };

    return VerificationResultSchema.parse(result);
  }

  /**
   * Get gas cost estimate for anchoring operations
   *
   * @param {string} operation - 'single', 'batch', or 'merkle'
   * @param {number} count - Number of receipts (for batch/merkle)
   * @returns {Promise<Object>} Gas estimate details
   */
  async estimateGas(operation, count = 1) {
    const gasPrice = await this.provider.getFeeData();
    const baseGasPrice = gasPrice.gasPrice;

    let estimatedGas;
    switch (operation) {
      case 'single':
        estimatedGas = 50000n; // ~50k gas for single anchor
        break;
      case 'batch':
        estimatedGas = 30000n + BigInt(count) * 20000n; // 30k base + 20k per receipt
        break;
      case 'merkle':
        estimatedGas = 60000n; // Fixed ~60k for Merkle root
        break;
      default:
        throw new Error(`Unknown operation: ${operation}`);
    }

    const costWei = estimatedGas * baseGasPrice;
    const costETH = ethers.formatEther(costWei);

    return {
      operation,
      count,
      estimatedGas: estimatedGas.toString(),
      gasPrice: baseGasPrice.toString(),
      costWei: costWei.toString(),
      costETH,
    };
  }

  /**
   * Compute SHA256 hash of receipt (fallback if not BLAKE3)
   *
   * @private
   * @param {Object} receipt - Receipt object
   * @returns {string} Hex hash
   */
  _computeReceiptHash(receipt) {
    const data = JSON.stringify(receipt);
    const hash = sha256(new TextEncoder().encode(data));
    return bytesToHex(hash);
  }

  /**
   * Convert hex hash to bytes32 format for Solidity
   *
   * @private
   * @param {string} hash - Hex hash string
   * @returns {string} bytes32 formatted hash
   */
  _toBytes32(hash) {
    // Remove 0x prefix if present
    const cleanHash = hash.startsWith('0x') ? hash : `0x${hash}`;
    // Ensure 32 bytes (64 hex chars + 0x)
    if (cleanHash.length !== 66) {
      throw new Error(`Invalid hash length: ${cleanHash.length}, expected 66 (0x + 64 hex chars)`);
    }
    return cleanHash;
  }
}

// =============================================================================
// Exports
// =============================================================================

export default ReceiptAnchorer;
export { AnchorResultSchema, VerificationResultSchema };
